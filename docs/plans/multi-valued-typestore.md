# Multi-valued TypeStore — design and decision path

> Author: Houston (with Claude). For Shayan's forensic review when he's back online.
> Branch: `verify-cloudflare-sdk-pipeline`, post-`empty-string-collapse` commit.
> Starting baseline: dyn 6, wt 337, agents 1,314 = **1,657 errors**.

## What problem this addresses

Across the Cloudflare verify pipeline, the largest remaining FS0039 buckets
all have the same structural shape:

- `'Code' ×42` in agents — Zod `RawIssue<ZodIssueCustom>`-like shapes where
  `code: "custom"` and other Zod types' `type: "custom"` share the interned
  `ResolvedType.Literal (String "custom")`.
- `'GetWithMetadata' ×28` in workers-types — `KVNamespace.getWithMetadata`
  overloads' synthetic `Partial<KVNamespaceGetOptions<undefined>>` options
  type, shared with `KVNamespace.get` overloads' synthetic of the same shape.
- `'CAPTURING_PHASE' ×10`, `'BUBBLING_PHASE' ×10`, `'AT_TARGET' ×10` in
  workers-types — `Event`-like static instance properties typed as
  `0 | 1 | 2 | 3` int literals, where the same `Literal (Int 3)` is referenced
  by many `*_ERR` constants on DOMException-like siblings.
- 'Type' ×14, 'Optin' ×30, 'Optout' ×22, '_parse' ×16, '_Lit85'-style
  buckets — all variants of the same pattern.

In every case, one interned `ResolvedType` is reached from multiple
property positions across one or more parents. The TS encoder's
`[<ReferenceEquality>]` interning makes them share identity. The generator's
property-name-derived path naming produces distinct
reference paths for each property position. But the **body emission runs
once** at the first property's path, leaving subsequent properties' references
dangling.

## Where the dedup happens (forensic trace)

Trace point 1 — **Property.render** (`src/Xantham.Generator/Generator/Render.Member.fs:297-298`):

```fsharp
let scopeStore =
    prop.Name |> RenderScopeStore.appendNameToPathContext scopeStore
```

Each property creates a child scope with `PathContext` augmented by its
property name. This is the *correct* design for naming each property's
own synthetic types after the property (e.g. anonymous TypeLiterals).

Trace point 2 — **prerender's Literal arm**
(`src/Xantham.Generator/Generator/RenderScope.Prelude.fs:348-364`):

```fsharp
| ResolvedType.Literal tsLiteral ->
    let path = TransientTypePath.Anchored
    let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
    let rootPath =
        TransientPath.toTransientModulePath scope.PathContext
        |> TransientTypePath.graft
    let childScope = RenderScopeStore.create()
    {
        RenderScope.Type = resolvedType
        Root = rootPath |> TypeLikePath.create |> ValueSome
        TypeRef = ref
        ...
    }
    |> addOrReplaceScope ctx resolvedType
```

On *first* call for a given `Literal` rt, `rootPath` is computed from the
caller's `scope.PathContext` (which ends with the first property's name).
This rootPath becomes the cached `RenderScope.Root`. The TypeRef is
`TransientPath_(Anchored)` — bare, no path baked in.

Trace point 3 — **createTransientPath** (`src/Xantham.Generator/Types/RenderScope.Prelude.fs:295-311`):

```fsharp
let createTransientPath (scope: RenderScopeStore) (resolvedType: ResolvedType) (path: TransientTypePath) =
    match path with
    | TransientTypePath.Anchored ->
        TransientPath.toTransientModulePath scope.PathContext
        |> TransientTypePath.graft
    | ...
    |> fun path ->
        scope.TypeStore.TryAdd(resolvedType, path)
        |> ignore
    Unsafe.createTransientPath path
```

`scope.TypeStore.TryAdd(resolvedType, path)` is the *first-wins* write.
The TypeStore is `Dictionary<ResolvedType, TransientTypePath>`. Multiple
property scopes call this for the same rt with different `path`s;
only the first survives. **This is the critical dedup site.**

Trace point 4 — **prerender cache-hit branch**
(`src/Xantham.Generator/Generator/RenderScope.Prelude.fs:72-78`):

```fsharp
if cachedRenderValue.IsSome then
    ...
    match cachedRenderValue.Value with
    | { Root = ValueSome (TypeLikePath.Transient path); TypeRef = ref } ->
        scope
        |> RenderScopeStore.tryAdd resolvedType path
        remap ref
```

On *subsequent* calls, the cache returns the same RenderScope. The
TypeRef returned (`ref`) is the bare-Anchored one from first call.
`RenderScopeStore.tryAdd` re-routes through `createTransientPath` (trace
point 3) with the cached path as input, which re-computes a fresh path
from the *current* scope.PathContext — and TryAdds first-wins into the
shared TypeStore. The recomputed path is dropped.

Trace point 5 — **anchor pass Transient arm**
(`src/Xantham.Generator/Generator/RenderScope.Anchored.fs:491-502`):

```fsharp
| { Root = ValueSome (TypeLikePath.Transient path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
    let path = TransientTypePath.anchor anchorPath path
    let render = Render.Transient.anchor ctx (AnchorPath.create path) renderTuple
    anchors
    |> Dictionary.tryAdd path render
    transientChildren.TypeStore.Keys
    |> Seq.iter (anchor ctx visited anchors (AnchorPath.create path))
```

The `path` used for emission comes from the cached `RenderScope.Root`
(first call's rootPath). `transientChildren.TypeStore.Keys` only iterates
keys — TypeStore values are unused at this iteration. One body emits at
the first call's path.

Trace point 6 — **reference resolution** at `anchorTypedNameRender`
(`src/Xantham.Generator/Generator/RenderScope.Anchored.fs:95-107`):

```fsharp
let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Transient.TypedNameRender) =
    let anchorPath =
        typedName.Metadata.Path
        |> anchorMetadataPath ctx anchorPath
    ...
    Type = typedName.Type |> TypeRefRender.anchorAndLocalise anchorPath
```

Each property's anchorPath is derived from its `metadata.Path` (set in
Property.render to `TransientMemberPath.AnchoredAndMoored prop.Name`).
The TypeRef (bare-Anchored) is anchored against this per-property
anchorPath. For property `code`, this resolves to `<parent>.Code`.
For property `type`, it resolves to `<parent>.Type`. Each property's
reference points to *its own* path — but only one of those paths has
an actual emitted body (trace point 5).

## Decision: option A — multi-valued TypeStore + body-cloning anchor pass

### Why not the alternatives

**Bake-in (tried, regressed +655).** Change the cached TypeRef from
bare `Anchored` to baked-in `rootPath`. Empirically reverts the
multi-path resolution back to a *single* canonical path. But the
`rootPath` is computed from first-call's `scope.PathContext` — a
chain like `Moored(Moored(Anchored, "_Lit161"), "Type")`. When
anchored against a deeper property's anchorPath, `toAnchored` segments
concatenate with the anchor's pathTrace, producing doubled paths like
`_Lit161.code._Lit161.Type`. Wrong for nearly every reference.
Reverted.

**Property-side decoupled naming.** Make Property.render append a
*value-derived* name to PathContext for Literal-typed properties
instead of the property name. This makes the COMPANION's emission
location value-derived — but the REFERENCE side still uses the
property's anchorPath which derives from `metadata.Path` (property
name). Mismatch unless `metadata.Path` is also changed — but that
renames the F# property (e.g. `abstract custom:` instead of
`abstract code:`), which is wrong. To fix only the type-side without
the property-side requires plumbing a second name through.

**Add Literal to SyntheticPathAssignment.** Tried previously
(`docs/plans/post-pr2-progress.md` documents the revert). Assigning
each Literal an absolute path under the export's home creates new
outer-scope buckets (`_LitN`) that don't match downstream reference
sites which still resolve via the transient-pathway. Without
reworking the per-parent referencing side too, the assignment is
half-implemented.

### Option A's mechanism

Make the TypeStore multi-valued, so each `(parent-scope, rt)` pair
accumulates *all* paths the rt was reached from within that scope. The
anchor pass then iterates each path and emits a body at each.

```
Before: TypeStore: Dictionary<ResolvedType, TransientTypePath>
After:  TypeStore: Dictionary<ResolvedType, HashSet<TransientTypePath>>
```

- **Writes** (`createTransientPath`, propagation): accumulate path into set.
- **Reads** (anchor pass): iterate KeyValues; for each rt's path set, emit body once per path.
- **Cycle guard**: extend visited tracker to `(rt, anchorPath, path)` so
  multi-path emission of the same `(rt, anchor)` pair isn't blocked,
  while genuine cycles (same rt referenced from inside its own body at
  same path) still terminate.

The cached `RenderScope` in `ctx.PreludeRenderScopes` stays
single-valued; we don't change its shape. The Root's path becomes
informational only — the actual emission location comes from
TypeStore per-parent. `Render.Transient.anchor` already accepts an
anchorPath, so cloning the emission per path is a matter of calling
it N times with N different anchors.

### Files touched (estimate)

1. `src/Xantham.Generator/Types/RenderScope.Prelude.fs` — `TypeStore`
   type definition; `createTransientPath`; `tryAdd`; `RenderScopeStore.create`.
2. `src/Xantham.Generator/Generator/RenderScope.Prelude.fs` — 4 sites
   doing `scope.TypeStore.TryAdd(rt, Anchored)` (placeholder synthetic
   writes); 2 sites doing TransientChildren propagation.
3. `src/Xantham.Generator/Generator/RenderScope.Anchored.fs` — the
   anchor pass: `anchor` function signature, `anchorPreludeAnchorScope`
   arms, `anchorPreludeExportScope` outer iteration.

Total surface: ~3 files, ~150-300 LOC change estimate.

### Risk model

**Likely wins (in order of confidence):**
- `'Code'`, `'GetWithMetadata'`, `'CAPTURING_PHASE'`, `'BUBBLING_PHASE'`,
  `'AT_TARGET'`, `'Type'`, `'Optin'`, `'Optout'` — all exact shape.
- Total ~200-300 FS0039 reductions estimated.

**Risk: over-emission causing F# name collisions.** If two paths
within the same TypeStore set anchor to the same concrete TypePath
(via different transient paths normalizing identically), `Dictionary.tryAdd`
on `anchors` keeps the first; the duplicate emission is discarded.
Should be a no-op in that case — but a duplicate-warning code path
would help diagnose if it happens unexpectedly.

**Risk: cycle prevention regression.** The previous attempt at
keying visited by just `ResolvedType` was too aggressive; current
`(rt, anchorPath)` works. Adding `path` as third key shouldn't
regress cycle detection — the dimensions extend monotonically. But
needs explicit test.

**Risk: TransientChildren propagation semantics.** Lines 786-800 of
`RenderScope.Prelude.fs` copy child TypeStore entries up to outer
scope. With multi-valued, this needs to *union* sets (not just TryAdd).
The intent is "every child synthetic seen by the inner scope must be
findable by the outer scope's anchor pass". Multi-valued union
preserves that.

### Empirical validation

Run after each step:
- `dotnet build src/Xantham.Generator/...` — compiler must accept
  the change.
- `dotnet run -- --colours 0` in `tests/Xantham.Generator.Tests/` —
  178 generator tests must still pass.
- `./scripts/regen-all.sh && ./scripts/wrap-all.sh && ./scripts/verify-all.sh counts` —
  measure SDK error totals; baseline = 1,657.

If at any point error total *grows*, the change is wrong; revert and
re-examine.

## Outcome (empirical)

### Final state (Literal-only multi-emission)

| | Baseline | Multi-emission (Literal only) | Δ |
|---|---:|---:|---:|
| dynamic-workflows | 6 | 6 | 0 |
| workers-types | 337 | 285 | **−52** |
| agents | 1,314 | 1,341 | +27 |
| **Total** | **1,657** | **1,632** | **−25** |

- agents generation timing: 20s (matches baseline)
- workers-types generation timing: 25s (matches baseline)
- dynamic-workflows generation timing: <1s (matches baseline)
- 178/178 generator tests pass

### Categorical wins (FS0039 buckets cleared/reduced)

- `'CAPTURING_PHASE' ×10` → 0 (cleared)
- `'BUBBLING_PHASE' ×10` → 0 (cleared)
- `'AT_TARGET' ×10` → 0 (cleared)
- `'Stream' ×10` → 0 (cleared)
- `'Code' ×42` (agents) → 32 (−10)
- `'Optin' ×30` (agents) → 22 (−8)
- `'Type' ×14` (workers-types) → 8 (−6)
- `'KVNamespace.GetWithMetadata' ×28` (workers-types) → 16 (−12) — partial; remaining are TypeLiteral synthetics

### Newly-exposed buckets (predicted inflation)

Multi-emission cleared conflating issues, surfacing previously-hidden
errors in agents:
- `'SuperRefine' ×12`, `'Prefault' ×12`, `'Pipe' ×12`,
  `'Overwrite' ×12`, `'Catch' ×12` — Zod V4 ZodType method parameter
  brand types. These reference `Brand.<Method>.<Param>` paths that
  the generator never emitted. Separate fix needed.

### Investigation findings during build

1. **DeepPartialInternal hang.** Adding the multi-valued anchor pass
   initially caused agents to time out at 60s. File-based diagnostic
   logging (added to `/tmp/xantham-debug.log`) identified the
   pathological export: Zod V3's `DeepPartialInternal` — a recursive
   `DeepPartial<T extends ZodTypeAny>` type alias whose body unfolds
   through conditional types. Switching the Transient arm to use
   the cached Root path (instead of per-call `currentPath`) for
   non-Literal types — and using `currentPath` only for Literals —
   restored baseline timing.

2. **Bug introduced during refactor.** Original `createTransientPath`
   returned `Unsafe.createTransientPath path` using the OUTER function
   parameter at function-body level. Initial refactor inadvertently
   piped the lambda-shadowed inner (computed) path. That single-line
   semantic shift accounted for ~760 of the regression observed
   before the fix.

3. **Doubled-path bug for cache-hit's tryAdd.** `createTransientPath`
   on cache-hit RECOMPUTES the transient path using current
   `scope.PathContext` combined with the cached path's leaf name.
   For baseline (first-wins TryAdd) this didn't matter because the
   recomputed path was discarded. With multi-valued accumulation it
   gets stored, producing paths like `<placeholder>.CertRevoked.CertPresented`
   (the doubled leaf). Multi-emission then renders as
   `module rec CertRevoked { type CertPresented }` instead of
   `type CertRevoked = ...`. Fix: for Literal rts, ignore the input
   path's leaf and compute fresh from current `scope.PathContext`
   (treat as if input were `Anchored`).

4. **TypeLiteral extension blocked.** Extending multi-emission to
   `ResolvedType.TypeLiteral` (to clear `KVNamespace.GetWithMetadata.Options`-
   style remaining cases) immediately re-triggered the 60s+ agents
   hang. TypeLiteral bodies have member-level recursion that
   compounds across per-path emission, especially through
   `DeepPartialInternal`-reachable paths. Reverted to Literal-only.

### Visited tracker design

`HashSet<ResolvedType * AnchorPath * TransientTypePath>` — 3-tuple.

For multi-emission to work without breaking cycle detection:
- Literal rts: third tuple element is the actual per-reference
  `currentPath` — distinct paths produce distinct visited keys,
  allowing one emission per path within an (rt, anchor) parent.
- All other rts: third element is the sentinel `TransientTypePath.Anchored` —
  all calls for the same (rt, anchor) produce the same visited key,
  blocking duplicates exactly as baseline 2-tuple did. No
  per-path explosion for heavy types.

### Files changed

- `src/Xantham.Generator/Types/RenderScope.Prelude.fs` — `TypeStore`
  type to `Dictionary<ResolvedType, HashSet<TransientTypePath>>`;
  `addTypeStorePath` helper; `createTransientPath` Literal-branch
  with leaf-strip; `RenderScopeStore.tryAdd` adjusted accordingly.
- `src/Xantham.Generator/Generator/RenderScope.Prelude.fs` — 4
  placeholder write sites updated; 2 propagation sites updated
  (first-path-only at both inside-toposort merge and after-toposort
  propagation).
- `src/Xantham.Generator/Generator/RenderScope.Anchored.fs` —
  `anchor`/`anchorPreludeAnchorScope`/`anchorPreludeExportScope`
  threaded `currentPath`; visited tracker 3-tuple with sentinel for
  non-Literal; per-path iteration for Literal rts at the Concrete /
  Anchored-synthetic / Transient arms and at the export-level.

### Remaining work (future)

- `'SuperRefine'`/`'Prefault'`/`'Pipe'`/`'Overwrite'`/`'Catch'` and
  similar Zod V4 method-parameter brand types: separate generation
  fix needed (not addressable via multi-emission). **Forensic
  finding (investigated and notated for Shayan, not fixed in this
  pass):** the references come from Zod V4's
  `$ZodBranded<T, Brand extends string | number | symbol>`
  (`node_modules/zod/v4/core/core.d.ts:25`). The second typar is
  literally named `Brand`. When this alias's body is unfolded
  through `core.$ZodBranded<this, T>` in the `brand()` method's
  return type, the resulting synthetic gets named `Brand` (from the
  typar) and references to it inside its own body resolve as
  `Brand.<member>` — but the synthesised member types (`_input`,
  `_output`, `_def`, `Type`, `Def`, etc.) are emitted as SIBLINGS
  of the `Brand` type rather than INSIDE a `Brand` module. The
  multi-emission cleanup (this branch) makes the `Brand.X.Y` pattern
  visible by clearing the conflating buckets that were masking it;
  the categorical fix is a separate investigation in encoder/typar-
  rendering territory.
- `'KVNamespace.GetWithMetadata.Options'`-style `TypeLiteral`
  synthetics shared across method overloads: needs either an
  expansion of `SyntheticPathAssignment` to handle method-overload
  conflicts, or a memoised `Render.Transient.anchor` that allows
  per-path emission without re-walking the body (would avoid the
  DeepPartialInternal recursion blowup).
