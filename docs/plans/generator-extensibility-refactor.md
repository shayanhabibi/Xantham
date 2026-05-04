# Generator Extensibility — Wholesale Refactor Plan

## Goal

Replace the ad-hoc `Interceptors` fields on `GeneratorContext.Customisation` with a single, uniform hook model that:

- Lets consumers intercept and rewrite **type-definition rendering** and **type-ref rendering** at well-defined pipeline stages.
- Composes multiple consumers deterministically.
- Costs one branch and zero allocations on the no-extension path.
- Carries enough ambient context (position, owner, render mode) that a handler can disambiguate heritage refs, type args, return types, etc.

Non-goal: preserving the existing four bespoke interceptor fields. They are subsumed by the new model.

## Breaking changes

This is a wholesale break of the public `Customisation` / `Interceptors` API. Every downstream consumer of `Interceptors.IgnorePathRender`, `Interceptors.Paths.*`, `Interceptors.ResolvedTypePrelude`, `Interceptors.AnchoredRender`, and the kind-keyed `pipe*` helpers must migrate to the new slot-based API. In-repo consumers known to break:

- `src/Xantham.Generator/Generator/Render.fs` (the live customisation, not just an example).
- `src/Xantham.Fable/Program.fs` (audit during migration).
- `tests/Xantham.Generator.Tests/**` and `tests/Xantham.Fable.Tests/**` (audit during migration).

The migration is mechanical but not optional. There is no compatibility shim.

## Pipeline

The generator has seven natural transformation boundaries. Every hook attaches to one of them:

| Stage              | Input                                    | Output                                   | Skippable | Replaces today                  |
|--------------------|------------------------------------------|------------------------------------------|-----------|---------------------------------|
| `PathResolution`   | `TypePath`                               | `TypePath`                               | yes       | `Paths.*`, `IgnorePathRender`   |
| `TypeRefBuild`     | `TypeReference`                          | `TypeRefRender`                          | no        | (new — covers encoder policies) |
| `TypeRefEmit`      | `TypeRefRender`                          | `WidgetBuilder<Type>`                    | no        | (new)                           |
| `RenderScopeBuild` | `RenderScope`                            | `RenderScope`                            | yes       | `ResolvedTypePrelude` (scope-level) |
| `TypeDefBuild`     | `Concrete.*Render` (per shape)           | `Concrete.*Render`                       | yes       | `ResolvedTypePrelude` (payload-level) |
| `TypeDefEmit`      | `WidgetBuilder<TypeDefn>`                | `WidgetBuilder<TypeDefn>`                | no        | (new)                           |
| `Anchored`         | `Anchored.*` (per shape)                 | `Anchored.*`                             | yes       | `AnchoredRender`                |

Build = AST-stage transformation (cache-friendly, idempotent).
Emit = render-stage transformation (wraps the produced widget — attributes, annotations).

`TypeDefBuild` and `Anchored` are split into per-shape slots (see the `Customisation` record under "Core types") so handlers don't have to match-and-ignore irrelevant cases. `TypeDefEmit` is intentionally **not** split: by the time we have a `WidgetBuilder<TypeDefn>` the shape distinction has been erased, so a single slot is the natural type.

**Member-level interception** (rename, skip, decorate methods/properties/fields) is not a separate slot. Members are part of `Concrete.TypeLikeRender`; consumers register a `TypeDefBuildClass` handler that returns a `Replace` with the transformed member list. Same for record fields and enum cases via their respective `TypeDefBuild*` slots.

`RenderScopeBuild` and `TypeDefBuild*` divide today's `ResolvedTypePrelude` along its real seams: scope-shell concerns (anchor, render mode, prelude metadata) go to `RenderScopeBuild`; typed payload concerns go to the matching `TypeDefBuild*`. Today's hook conflated both.

## Core types

Two slot variants — one that admits `Skip`, one that doesn't — so the type system enforces the table above. F# disambiguates the case names by type, so both DUs use plain `Pass` / `Replace` / `Skip`:

```fsharp
type HookResult<'T> =
    | Pass              // identity; chain continues with the running value unchanged
    | Replace of 'T     // swap the value; subsequent handlers see 'T

type SkippableHookResult<'T> =
    | Pass
    | Replace of 'T
    | Skip              // drop this node entirely (export elision, member skip, ...)

type Hook<'T>          = GeneratorContext -> RenderContext -> 'T -> HookResult<'T>
type SkippableHook<'T> = GeneratorContext -> RenderContext -> 'T -> SkippableHookResult<'T>

[<Struct>]
type RenderContext = {
    Position : RenderPosition       // a DU sum: TypeRef positions ∪ Path positions, see below
    Owner    : ResolvedType voption
    Render   : RenderMode           // the existing render-mode discriminator (RefOnly | Concrete | Transient), propagated from the active RenderScope
    Stage    : RenderStage          // tag, debug only
}

and RenderPosition =
    | RefPos of TypeRefPosition
    | PathPos of PathPosition
    | NotApplicable                 // for Emit/Anchored slots where position is meaningless

and TypeRefPosition =
    | Standalone
    | InheritanceRef
    | TypeArg
    | TupleElement
    | UnionMember
    | FunctionParameter
    | FunctionReturn
    | AliasTarget
    | MemberType

and PathPosition =
    | TopLevelType                  // PathResolution: a top-level type path
    | MemberPath                    // PathResolution: a member's anchor path
    | VariablePath                  // PathResolution: a top-level variable
    | FunctionPath                  // PathResolution: a top-level function

and RenderStage =
    | PathResolution | TypeRefBuild | TypeRefEmit
    | RenderScopeBuild | TypeDefBuild | TypeDefEmit | Anchored
```

`Owner` semantics:

- `PathResolution` with `PathPos TopLevelType` ⇒ `ValueSome <the type itself>` (the resolved type for which the path is being computed).
- `PathResolution` with `PathPos MemberPath` ⇒ `ValueSome <containing type>`.
- `PathResolution` with `PathPos VariablePath` / `PathPos FunctionPath` ⇒ `ValueNone`.
- `TypeRef*` slots ⇒ `ValueSome` of the surrounding type when the ref is rendered inside a member/heritage/return position; `ValueNone` for free-standing refs.
- `RenderScopeBuild` / `TypeDefBuild*` / `Anchored*` ⇒ `ValueSome <the type the scope/payload describes>`.

```fsharp
type HookSlot<'T>          = { Handlers : Hook<'T> list;          HasAny : bool }
type SkippableHookSlot<'T> = { Handlers : SkippableHook<'T> list; HasAny : bool }

type Customisation = {
    PathResolution         : SkippableHookSlot<TypePath>

    TypeRefBuild           : HookSlot<TypeRefRender>
    TypeRefEmit            : HookSlot<WidgetBuilder<Type>>

    RenderScopeBuild       : SkippableHookSlot<RenderScope>

    TypeDefBuildClass      : SkippableHookSlot<Concrete.TypeLikeRender>
    TypeDefBuildAlias      : SkippableHookSlot<Concrete.TypeAliasRender>
    TypeDefBuildEnum       : SkippableHookSlot<Concrete.LiteralUnionRender<int>>
    TypeDefBuildStringUnion: SkippableHookSlot<Concrete.LiteralUnionRender<TsLiteral>>
    TypeDefEmit            : HookSlot<WidgetBuilder<TypeDefn>>

    AnchoredRef            : SkippableHookSlot<Anchored.TypeRefRender>
    AnchoredScope          : SkippableHookSlot<Anchored.RenderScope>
}
```

`PathResolution` carries kind via `PathPosition` plus `Owner` for the containing type. This replaces the six `pipe*` helpers without growing the slot table.

## Interpreter

`run` is `inline` only as far as the `HasAny` check; the recursive walker is a module-level `let rec` so the JIT sees a normal tail-recursive function rather than an inlined-but-escaping closure.

Tokens are the boxed handler reference plus a small slot tag (so `Customisation.remove` knows which slot to rebuild). Comparison is `Object.ReferenceEquals` on the boxed function. No counter, no global state:

```fsharp
type SlotId =
    | PathResolutionSlot
    | TypeRefBuildSlot | TypeRefEmitSlot
    | RenderScopeBuildSlot
    | TypeDefBuildClassSlot | TypeDefBuildAliasSlot
    | TypeDefBuildEnumSlot  | TypeDefBuildStringUnionSlot
    | TypeDefEmitSlot
    | AnchoredRefSlot | AnchoredScopeSlot

type HandlerToken = HandlerToken of SlotId * obj

type HookSlot<'T>          = { Handlers : Hook<'T> list;          HasAny : bool }
type SkippableHookSlot<'T> = { Handlers : SkippableHook<'T> list; HasAny : bool }

module Hook =
    let ofMap (f: 'T -> 'T) : Hook<'T> = fun _ _ v -> Replace (f v)

module SkippableHook =
    let ofMap (f: 'T -> 'T) : SkippableHook<'T> =
        fun _ _ v -> SkippableHookResult.Replace (f v)
    // Convenience for predicate-driven Skip:
    let ofPredicate (shouldSkip: 'T -> bool) : SkippableHook<'T> =
        fun _ _ v -> if shouldSkip v then SkippableHookResult.Skip else SkippableHookResult.Pass

module HookSlot =
    let empty<'T> : HookSlot<'T> = { Handlers = []; HasAny = false }

    // Chainable: most consumers don't need a token.
    let add (h: Hook<'T>) (slot: HookSlot<'T>) : HookSlot<'T> =
        { Handlers = h :: slot.Handlers; HasAny = true }

    // Tracked: returns the slot plus the boxed handler reference for use as a token.
    // The `Customisation.add{Slot}Tracked` wrapper is responsible for tagging the
    // boxed reference with its SlotId before exposing a `HandlerToken`.
    let addTracked (h: Hook<'T>) (slot: HookSlot<'T>) : HookSlot<'T> * obj =
        { Handlers = h :: slot.Handlers; HasAny = true }, box h

    let remove (token: obj) (slot: HookSlot<'T>) : HookSlot<'T> =
        let h' = slot.Handlers |> List.filter (fun h -> not (System.Object.ReferenceEquals(box h, token)))
        { Handlers = h'; HasAny = not (List.isEmpty h') }

    let clear<'T> : HookSlot<'T> -> HookSlot<'T> = fun _ -> empty<'T>

    let rec private loop result current handlers ctx rctx =
        match handlers with
        | [] -> result
        | h :: rest ->
            match h ctx rctx current with
            | Pass      -> loop result current rest ctx rctx
            | Replace v -> loop (Replace v) v rest ctx rctx

    let inline run slot ctx rctx value =
        if not slot.HasAny then Pass
        else loop Pass value slot.Handlers ctx rctx

module SkippableHookSlot =
    let empty<'T> : SkippableHookSlot<'T> = { Handlers = []; HasAny = false }

    let add (h: SkippableHook<'T>) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> =
        { Handlers = h :: slot.Handlers; HasAny = true }

    let addTracked (h: SkippableHook<'T>) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> * obj =
        { Handlers = h :: slot.Handlers; HasAny = true }, box h

    let remove (token: obj) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> =
        let h' = slot.Handlers |> List.filter (fun h -> not (System.Object.ReferenceEquals(box h, token)))
        { Handlers = h'; HasAny = not (List.isEmpty h') }

    let clear<'T> : SkippableHookSlot<'T> -> SkippableHookSlot<'T> = fun _ -> empty<'T>

    let rec private loop result current handlers ctx rctx =
        match handlers with
        | [] -> result
        | h :: rest ->
            match h ctx rctx current with
            | SkippableHookResult.Pass      -> loop result current rest ctx rctx
            | SkippableHookResult.Replace v -> loop (SkippableHookResult.Replace v) v rest ctx rctx
            | SkippableHookResult.Skip      -> SkippableHookResult.Skip

    let inline run slot ctx rctx value =
        if not slot.HasAny then SkippableHookResult.Pass
        else loop SkippableHookResult.Pass value slot.Handlers ctx rctx
```

`Customisation` exposes both `add{Slot}` (chainable, returns `Customisation`) and `add{Slot}Tracked` (returns `Customisation * HandlerToken` where the token wraps the slot tag). `Customisation.remove : HandlerToken -> Customisation -> Customisation` matches on the slot tag and delegates to `HookSlot.remove` / `SkippableHookSlot.remove` on that field. Most call sites use the chainable form and never touch tokens.

### Truth table

| h₁ result      | h₂ result      | h₃ result      | Final         | `current` after | Notes                               |
|----------------|----------------|----------------|---------------|-----------------|-------------------------------------|
| `Pass`         | `Pass`         | —              | `Pass`        | original `v`    |                                     |
| `Pass`         | `Replace v₂`   | —              | `Replace v₂`  | `v₂`            |                                     |
| `Replace v₁`   | `Pass`         | —              | `Replace v₁`  | `v₁`            | `Pass` forwards `current`, no reset |
| `Replace v₁`   | `Replace v₂`   | —              | `Replace v₂`  | `v₂`            | h₂ saw `v₁`                         |
| `Replace v₁`   | `Pass`         | `Replace v₃`   | `Replace v₃`  | `v₃`            | h₃ saw `v₁`                         |
| `Replace v₁`   | `Skip`         | —              | `Skip`        | (elide)         | `v₁` discarded                      |
| `Skip`         | (anything)     | —              | `Skip`        | (chain stops)   | no "un-skip"                        |

### Ordering

`add` prepends, so **registration order is reverse of execution order**:

```
register a; register b; register c
Handlers = [c; b; a]
Execution: c → b → a
```

The latest-registered handler runs first and sees the original input. Earlier handlers see whatever the later ones produced. This matches "latest registration wraps everything underneath" — useful when an application registers a base policy at startup and tests layer overrides on top.

`HookSlot.add` is chainable (no token). `HookSlot.addTracked` returns the slot plus the boxed handler reference; `Customisation.add{Slot}Tracked` wraps that with a `SlotId` to produce a `HandlerToken`. `HookSlot.clear` resets a slot to empty.

## Consumer usage

End-to-end registration looks like this:

```fsharp
let customisation =
    Customisation.Default
    |> Customisation.addTypeRefBuild encoderInvariantPolicy   // Hook<TypeRefRender>
    |> Customisation.addPathResolution renameMembers          // SkippableHook<TypePath>
    |> Customisation.addTypeDefBuildClass injectAttribute     // SkippableHook<TypeLikeRender>

// For dynamic reconfiguration (tests, REPL):
let customisation', token =
    customisation |> Customisation.addAnchoredScopeTracked debugProbe
// ... later:
let customisation'' = Customisation.remove token customisation'
```

## Call-site pattern

Non-skippable slot:

```fsharp
let rctx = {
    Position = RefPos TypeArg
    Owner    = ValueSome owner
    Render   = scope.Render
    Stage    = TypeRefBuild
}
let render =
    match HookSlot.run ctx.Customisation.TypeRefBuild ctx rctx render with
    | Pass      -> render
    | Replace v -> v
```

Skippable slot:

```fsharp
match SkippableHookSlot.run ctx.Customisation.PathResolution ctx rctx path with
| SkippableHookResult.Pass      -> emit path
| SkippableHookResult.Replace p -> emit p
| SkippableHookResult.Skip      -> () // elide
```

## Performance

- **Per-slot `HasAny`**: a single `bool` field per slot. The no-handler path is one branch and zero allocations.
- **Struct `RenderContext`**: passed by value or `inref` — no heap allocation per node.
- **Inlining**: `run` inlines the `HasAny` check; the loop is module-level `let rec` so the JIT applies normal tail-call optimisation rather than relying on closure inlining.
- **Cache ordering**: `Anchored*` runs *before* `AnchorRenders` write. `RenderScopeBuild` and `TypeDefBuild*` run before `PreludeRenders` write. Today's ordering preserved.
- **SRTP smart constructors** in `Types/RenderScope.Prelude.fs` stay inline at the leaves — hooks attach at the renderer boundaries, never inside leaf allocators.
- **Lifecycle**: registration is expected to happen once at startup. Each `add{Slot}` allocates a new `Customisation` record (small, ~10 fields). Dynamic re-registration via `addTracked`/`remove` is supported but not the optimised path; budget accordingly.

### Cache invalidation policy

The interpreter always emits `Replace v` when any handler returns `Replace`. The reference-equality short-circuit lives at the **call site** that owns the cache, not in the interpreter:

```fsharp
match HookSlot.run slot ctx rctx value with
| Pass                                                   -> useCached value
| Replace v when System.Object.ReferenceEquals(v, value) -> useCached value
| Replace v                                              -> writeCacheEntry v; useFresh v
```

This relies on `TypeRefRender` / `*Render` records being reference types (F# default). If any of them are ever marked `[<Struct>]`, the call site must switch to structural equality. Documented in "Risks".

**Anchored cache keys are computed from the post-hook output**, not the pre-hook input. Two handlers producing the same anchored form for different inputs will hit the cache; a handler producing different anchored forms for the same input will miss. This matches today's behaviour for `AnchoredRender`.

### Determinism contract

A handler's output must be a pure function of `(ctx, rctx, input)`. Two handlers producing different output for the same `(ResolvedType, RenderContext)` would corrupt `PreludeRenders` / `AnchorRenders`.

Optional debug-mode enforcement: when a cached entry is hit, re-run the chain on the original input and assert reference (or structural) equality against the cached output. Cheap insurance, off in release.

## Migration map

| Today                                  | Replacement                                                                       |
|----------------------------------------|-----------------------------------------------------------------------------------|
| `Interceptors.IgnorePathRender`        | `PathResolution` handler returning `Skip`                                         |
| `Interceptors.Paths.TypePaths`         | `PathResolution` handler returning `Replace path'`; reads `rctx.Owner` for kind   |
| `Interceptors.Paths.MemberPaths`       | `PathResolution` handler with `Position = PathPos MemberPath`                     |
| `Interceptors.Paths.{Variable,Function}` | `PathResolution` handler with `Position = PathPos VariablePath` / `FunctionPath` |
| `Interceptors.ResolvedTypePrelude` (scope-level mutation) | `RenderScopeBuild` handler                                       |
| `Interceptors.ResolvedTypePrelude` (payload mutation)     | The matching `TypeDefBuild*` slot for the shape                  |
| `Interceptors.AnchoredRender`          | `AnchoredRef` or `AnchoredScope` per target shape                                 |
| (none — encoder policy tests)          | `TypeRefBuild` handler keyed on `Position`                                        |
| (none — attribute injection)           | `TypeDefEmit` handler                                                             |

The kind-keyed `pipeInterface`/`pipeClass`/`pipeEnum`/`pipeTypeAlias`/`pipeVariable`/`pipeFunction` helpers in `TypeRefRender.Paths.fs` collapse into a single `PathResolution` chain that branches on `rctx.Owner.Kind` and `rctx.Position` (`PathPos MemberPath`/`PathPos VariablePath`/`PathPos FunctionPath`).

## Alternatives considered

- **Single `TypeDefBuild` slot over a `TypeRenderTarget` DU.** Rejected — every handler would have to match four cases. The per-shape slot table costs four field names and gains type-safe handlers and per-shape `HasAny`. Trade-off: a handler that wants to apply uniformly to all shapes must register on all four slots. Tolerable; that handler is rare and registration is a one-line per slot.
- **Splitting `TypeDefEmit` per shape.** Rejected — by emit time the value is already a `WidgetBuilder<TypeDefn>`; the shape distinction has been erased.
- **`Pass | Replace | Skip` in one DU for every slot.** Rejected — `Skip` is meaningless for `TypeRefEmit` / `TypeDefEmit` (every node must produce a widget). Two slot variants encode this in the type system rather than as a runtime check.
- **One `RenderPosition` DU without partitioning.** Rejected — TypeRef positions and Path positions never co-occur. Splitting via `RefPos` / `PathPos` keeps `RenderContext` honest about which slot it belongs to.
- **Pre/post hook pairs.** Rejected — `Replace (wrap input)` already expresses "wrap after default render", and pre/post would double the slot count.
- **Conflating scope and payload mutation under one `TypeDefBuild` slot.** Rejected — the existing `ResolvedTypePrelude` does exactly this and the seams are visible. `RenderScopeBuild` (scope) and `TypeDefBuild*` (payload) are the natural split.

## File-level changes

1. **`src/Xantham.Generator/Types/Generator.fs`**
   - Remove `Interceptors`, `InterceptorPath`, `InterceptorPathDispatch` and the kind-keyed path fields.
   - Add `HookResult<'T>`, `SkippableHookResult<'T>`, `Hook<'T>`, `SkippableHook<'T>`, `HookSlot<'T>`, `SkippableHookSlot<'T>`, `RenderContext`, `RenderPosition`, `TypeRefPosition`, `PathPosition`, `RenderStage`, `HandlerToken`.
   - Replace `Customisation` with the per-shape slot record. `Customisation.Default` = all `(_HookSlot).empty`.
   - Add chainable `Customisation.add{Slot}` helpers (return `Customisation`) and tracked `Customisation.add{Slot}Tracked` helpers (return `Customisation * HandlerToken`) for every slot: `PathResolution`, `TypeRefBuild`, `TypeRefEmit`, `RenderScopeBuild`, `TypeDefBuildClass`, `TypeDefBuildAlias`, `TypeDefBuildEnum`, `TypeDefBuildStringUnion`, `TypeDefEmit`, `AnchoredRef`, `AnchoredScope`. Plus `Customisation.remove : HandlerToken -> Customisation -> Customisation` that finds the slot by token and rebuilds it.

2. **`src/Xantham.Generator/Generator/HookSlot.fs`** *(new)*
   - `Hook.ofMap`, `HookSlot.{empty, add, remove, clear, run}`, `SkippableHookSlot.{empty, add, remove, clear, run}`.

3. **`src/Xantham.Generator/Generator/TypeRefRender.Paths.fs`**
   - Delete `Path.Interceptors.shouldIgnoreRender`/`shouldIgnoreExport` and the `pipe*` family.
   - Replace each former call site with `SkippableHookSlot.run ctx.Customisation.PathResolution ...`. `Skip` triggers the existing elision path that `IgnorePathRender` used.
   - Thread `RenderContext` (compute `Owner`/`Position` from caller; member-path callers set `Owner` to the containing `ResolvedType`; top-level type paths set `Owner` to the type itself).

4. **`src/Xantham.Generator/Generator/TypeRefRender.Render.fs`**
   - Wrap `Implementation.renderAtom` / `renderMolecule` / `render` with `TypeRefBuild` invocation at entry; wrap the final `WidgetBuilder<Type>` with `TypeRefEmit`.
   - Mirror in the `Anchored` sub-module (uses `AnchoredRef`).
   - Thread `RenderContext` through recursion: `RefPos TypeArg` for prefix args, `RefPos UnionMember`/`RefPos TupleElement` for unions/tuples, `RefPos FunctionParameter`/`RefPos FunctionReturn` inside Function arms, `RefPos InheritanceRef` from `renderInheritance`, `Render = scope.Render`.

5. **`src/Xantham.Generator/Generator/Render.TypeShapes.fs` / `Render.TypeAlias.fs` / `Render.Enum.fs`**
   - After producing each `*Render`, run the matching `TypeDefBuild*` slot. `Skip` ⇒ skip the export.
   - Replace direct calls to former `Paths.pipe*` with the unified `PathResolution` already wired in step 3.

6. **`src/Xantham.Generator/Generator/TypeRender.Render.fs`**
   - In `TypeLikeRender.renderClass` / `renderRecord` / `renderTag` / `renderAnonymousRecord` and `TypeAliasRender.renderTypeAlias`, after producing the `WidgetBuilder<TypeDefn>`, run `TypeDefEmit`.

7. **`src/Xantham.Generator/Generator/RenderScope.Prelude.fs`**
   - Replace the `Customisation.Interceptors.ResolvedTypePrelude` invocation in `addOrReplaceScope` with `RenderScopeBuild` (scope-level), then dispatch to the matching `TypeDefBuild*` slot for the payload. Cache write happens *after* both hooks resolve (today's ordering preserved). The cache invalidation policy from "Performance" applies at this site.

8. **`src/Xantham.Generator/Types/Generator.fs` — `Anchored.addOrReplace`**
   - Replace `Customisation.Interceptors.AnchoredRender` with `AnchoredRef` / `AnchoredScope` per target shape. `Skip` removes the entry from anchored emission.

9. **`src/Xantham.Generator/Generator/Render.fs`** *(required migration, not example)*
   - Migrate the live customisation from `Interceptors.*` to `Customisation.add*` builders. Add a worked example registering an encoder-invariant policy as a `TypeRefBuild` handler keyed on `rctx.Position`.

10. **`src/Xantham.Fable/Program.fs` and tests** *(audit)*
    - Search for any `Interceptors.` reference and migrate. Confirm zero remaining references before merging.

## Tests

Create `tests/baselines/` (does not currently exist) and, before any code changes, **capture a baseline** of `Xantham.Fable/output.json` from a clean checkout of the commit agreed-upon as golden (e.g. `master` HEAD). Store at `tests/baselines/output.json`. The working-tree copy is dirty and not a valid baseline.

- **`tests/Xantham.Generator.Tests/Tests/TypeRefRender.EncoderInvariant*.fs`** — flesh out the three scaffolds against `TypeRefBuild` handlers (Invariant / Achievable / NotAchievable). Assert AST output: `Promise<>` vs `Promise`, `option<…>` wrapping for nullable, erased `U2` for union policies.

- **Composition** — register two `TypeRefBuild` handlers; assert latest-registered runs first and the earlier handler sees the later one's output via `Replace`.

- **Three-handler chain** — register `Replace v₁`, `Pass`, `Replace v₃`; assert final = `Replace v₃` and h₃'s input = `v₁` (regression test for the truth table).

- **`Skip` semantics** — register a `PathResolution` handler returning `Skip` for a known export; assert it disappears from the output. Replicate the current `IgnorePathRender` test cases against the new mechanism.

- **`Skip` short-circuit** — register two handlers where the first returns `Skip`; assert the second never runs.

- **Position context** — register a `TypeRefBuild` handler that asserts `Position = RefPos InheritanceRef` only when invoked from `renderInheritance`; assert it never fires elsewhere.

- **`Owner` for member paths** — register a `PathResolution` handler with `Position = PathPos MemberPath`; assert `Owner` is the containing `ResolvedType`, not the member's type.

- **`Owner` for top-level type paths** — register a `PathResolution` handler with `Position = PathPos TopLevelType`; assert `Owner` is the resolved type itself.

- **`Render` mode threading** — register a `TypeRefBuild` handler; assert `rctx.Render` matches the active `RenderScope.Render` at the call site.

- **`HookSlot.remove` / `clear`** — register, run, remove via token, run again; assert the removed handler is no longer invoked.

- **Cache invalidation phys-eq** — register a `TypeDefBuild*` handler that returns `Replace input`; assert the cache is **not** invalidated (reference-equality short-circuit).

- **Zero-cost no-op** — with `Customisation.Default`, instrument `HookSlot.run` and `SkippableHookSlot.run` with a counter and assert zero invocations of the loop body across a full generation of `Xantham.Fable/output.json`.

- **Snapshot regression** — byte-identical output against `tests/baselines/output.json` when `Customisation.Default` is in effect.

## Risks

- **Nullable invariant**: `TypeRefRender.Nullable` is computed at the molecule level for `Prefix`. A `TypeRefBuild` handler that swaps `Prefix → Atom` must restore the nullability bit. Document; consider a debug-build assertion.
- **Cache coherence**: `RenderScopeBuild` / `TypeDefBuild*` / `Anchored*` outputs reach `PreludeRenders` / `AnchorRenders`. Two handlers producing different output for the same input would corrupt the cache. Document the determinism contract; consider the optional debug-mode re-run check.
- **Recursive context propagation**: every recursive call inside `Implementation.renderMolecule` needs an explicit `RenderContext` argument. Mechanical but touches every recursion site — easy to miss one and end up with a stale `Position` or `Render` mode.
- **Reference-equality assumption**: the phys-eq fast-path at cache-write sites assumes `TypeRefRender` / `*Render` records are reference types. If any of them is later marked `[<Struct>]`, the call site needs structural equality.
- **Inline tail-call**: `HookSlot.run` is `inline` but delegates to a module-level `let rec loop`. Verify the JIT actually emits a tail call; if not, the loop may need to be rewritten as a `while` over an explicit cursor.

## Cost summary

- 1 file rewritten for the surface (`Types/Generator.fs`).
- 1 new file (`Generator/HookSlot.fs`).
- 7 wiring files (`TypeRefRender.Paths.fs`, `TypeRefRender.Render.fs`, `Render.TypeShapes.fs`, `Render.TypeAlias.fs`, `Render.Enum.fs`, `TypeRender.Render.fs`, `RenderScope.Prelude.fs`).
- 1 new test directory (`tests/baselines/`) with the captured baseline.
- All call sites that previously used `Interceptors.*` migrate mechanically to `HookSlot.run` / `SkippableHookSlot.run`.
- Runtime cost on the no-handler path: one `bool` branch per stage entry, zero allocations.

## Implementation progress

### Decisions locked during execution

1. **Step 2 (`Generator/HookSlot.fs`) is superseded.** F# strict file ordering required folding into `Types/Generator.fs`; no separate file is created.
2. **`TypeDefEmit` slot type:** corrected from `WidgetBuilder<Type>` (typo) to `WidgetBuilder<TypeDefn>`.
3. **`RenderContext.Render` is `RenderMode voption`**, not `RenderMode`. Path resolution legitimately fires before any `RenderScope` is entered, so it sets `Render = ValueNone`. All later `Build`/`Emit`/`Anchored` callers set `ValueSome scope.Render`.
4. **`PathResolution` slot is split into two slots** to honour the `TypePath` vs `MemberPath` divide that `fromVariable`/`fromFunction` produce: `PathResolutionType: SkippableHookSlot<TypePath>` and `PathResolutionMember: SkippableHookSlot<MemberPath>`. The plan's single-slot table is wrong on that row; the migration map's `MemberPaths` row maps to `PathResolutionMember`.
5. **Skip is resolved once at the dispatcher level** (`RenderScope.Anchored.fs`). Renderers receive the resolved path as a parameter and stop calling former `pipe*` helpers themselves. Renderer signatures change accordingly. *(Pending — applies in step 5/8.)*
6. **`Path.tryResolveExport`** is the single dispatcher entry point. Returns `AnchorPath voption`. `ValueNone` ⇒ elide the export. Modules currently bypass resolution to preserve legacy `pipeExport` parity.

### Foundation

- [x] Step 1 — `Types/Generator.fs` (committed in `6c361eb`).
- [x] Step 2 — superseded (see decision #1).
- [x] `TypeDefEmit` slot type fix.
- [x] `RenderContext.Render` made `RenderMode voption`.
- [x] `PathResolution` slot split into `PathResolutionType` + `PathResolutionMember`; `SlotId` cases, `Customisation.Default`, chainable `add*`, tracked `add*Tracked`, and `remove` dispatcher all updated.

### Step 3 — `TypeRefRender.Paths.fs`

- [x] Removed `module Interceptors` (deleted `shouldIgnoreRender`, `shouldIgnoreExport`, `pipeInterface`, `pipeClass`, `pipeEnum`, `pipeTypeAlias`, `pipeVariable`, `pipeFunction`, `pipeExport`).
- [x] Added `Path.resolveType` / `Path.resolveMember` returning `SkippableHookResult<…>`.
- [x] Added `Path.tryResolveTypePath` / `Path.tryResolveMemberPath` (voption convenience).
- [x] Added `Path.tryResolveExport ctx export : AnchorPath voption` (single dispatcher entry).
- [ ] Caller migration is **deferred to steps 5, 7, 8** — see "Pending downstream migration" below.

### Pending downstream migration (build is currently red)

Removing `Path.Interceptors.*` and `Customisation.Interceptors.*` left these call sites broken. Each must migrate before the build is green:

| File | Line | What to do |
|------|------|-----------|
| `Types/Generator.fs Anchored.addOrReplace` | 339 | **Step 8** — replace `Customisation.Interceptors.AnchoredRender ctx key value` with a `Choice`-arm dispatch: `AnchoredRef` / `AnchoredScope` slot. |
| `Generator/Render.Enum.fs` | 13, 53 | **Step 5** — drop `Path.Interceptors.pipeEnum`; accept resolved `TypePath` as a parameter instead. Caller in dispatcher passes the value from `Path.tryResolveExport`. |
| `Generator/Render.TypeAlias.fs` | 22 | **Step 5** — same pattern; drop `Path.Interceptors.pipeTypeAlias`. |
| `Generator/RenderScope.Prelude.fs` | 31 | **Step 7** — split `ResolvedTypePrelude` invocation into `RenderScopeBuild` + matching `TypeDefBuild*`. |
| `Generator/RenderScope.Prelude.fs` | 91, 108, 292, 438 | **Step 5** — drop `Path.Interceptors.pipe{Interface,Class,Enum,TypeAlias}`; accept resolved path from caller. |
| `Generator/RenderScope.Anchored.fs` | 502, 525, 555, 574, 593, 626 | **Step 8 / dispatcher** — replace `Interceptors.shouldIgnoreRender` short-circuit with `Path.tryResolveExport` / appropriate `tryResolveType` / `tryResolveMember` returning `ValueNone` ⇒ skip the export. Plumb the resolved `AnchorPath` into the renderer call. |
| `Generator/Render.fs` | 25–44 | **Step 9** — migrate the live `Customisation.Interceptors.*` block to the new `Customisation.add*` builders. Add encoder-invariant worked example. |
| `docs/Xantham.Generator/{pipeline.fsx, paths.md, generator-context.md}` | various | Doc-only — defer until code is green. |

### Remaining work

- [ ] Step 4 — `TypeRefRender.Render.fs`: wrap with `TypeRefBuild`/`TypeRefEmit`; mirror in `Anchored` for `AnchoredRef`; thread `RenderContext` through every recursion site (TypeArg / UnionMember / TupleElement / FunctionParameter|Return / InheritanceRef).
- [ ] Step 5 — `Render.TypeShapes.fs` / `Render.TypeAlias.fs` / `Render.Enum.fs`: accept resolved path as parameter; invoke matching `TypeDefBuild*` after each `*Render`.
- [ ] Step 6 — `TypeRender.Render.fs`: invoke `TypeDefEmit` after each `WidgetBuilder<TypeDefn>`.
- [ ] Step 7 — `RenderScope.Prelude.fs:31`: replace prelude interceptor with `RenderScopeBuild` then payload `TypeDefBuild*`; phys-eq cache invalidation.
- [ ] Step 8 — `Types/Generator.fs Anchored.addOrReplace`: replace `Interceptors.AnchoredRender` with `AnchoredRef`/`AnchoredScope`.
- [ ] Step 9 — `Generator/Render.fs`: migrate live customisation; add encoder-invariant worked example.
- [ ] Step 10 — Audit `Xantham.Fable/Program.fs` and `tests/**` for stale `Interceptors.` references.
- [ ] Final — `dotnet build`, capture `tests/baselines/output.json`, run new tests.
