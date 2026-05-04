---
title: GeneratorContext
category: Xantham.Generator
categoryindex: 3
index: 4
---

# `GeneratorContext`

`GeneratorContext` is the mutable scratch space that every render pass
threads through. It carries four caches, an in-flight set, and a
customisation record. The dictionary type — `Dictionary<,>` or
`ConcurrentDictionary<,>` — is selected at compile time by the
`CONCURRENT_DICT` constant; this is enabled by default in
`Xantham.Generator.fsproj` so test runners that exercise the generator from
multiple threads do not race.

## Shape

```fsharp
type GeneratorContext = {
    TypeAliasRemap   : DictionaryImpl<ResolvedType, TypeRefRender>
    PreludeGetTypeRef: PreludeGetTypeRefFunc
    PreludeRenders   : DictionaryImpl<ResolvedType, RenderScope>
    AnchorRenders    : DictionaryImpl<Choice<ResolvedType, ResolvedExport>,
                                      Choice<Anchored.TypeRefRender, Anchored.RenderScope>>
    InFlight         : HashSet<ResolvedType>
    Customisation    : Customisation
}
```

* **`TypeAliasRemap`** — populated by `prerenderTypeAliases`. Maps an alias
  type to the resolved target's `TypeRefRender`, so subsequent passes can
  short-circuit "alias of an alias of an interface" without re-walking.
* **`PreludeGetTypeRef`** — strategy function called when an
  unresolved/transient `ResolvedType` needs a `TypeRefRender` *during* the
  prelude pass. Set once at construction.
* **`PreludeRenders`** — keyed by `ResolvedType`; holds the prelude scope
  (transient form) for everything seen during prelude.
* **`AnchorRenders`** — keyed by `Choice<ResolvedType, ResolvedExport>`;
  holds the anchored output. The two halves of the choice cover anonymous
  structural types (`ResolvedType`) and exported declarations
  (`ResolvedExport`) without collisions.
* **`InFlight`** — guard set used to break recursion in the prelude pass.
  `GeneratorContext.Prelude.canFlight` returns true on first entry, false
  on re-entry; passes use it to fall back to a `RefOnly` render rather than
  loop forever on a cyclic type.

## Construction

The `Customisation` record dispatches through hook slots at seven pipeline
stages. Each slot holds a list of handlers that are invoked left-to-right
(latest-registered runs first). Handlers are registered using chainable
`add*` methods:

```fsharp
let ctx =
    GeneratorContext.EmptyWithCustomisation (fun c ->
        c
        |> Customisation.addRenderScopeBuild myRenderScopeBuildHook
        |> Customisation.addPathResolutionType myTypePathHook
        |> Customisation.addPathResolutionMember myMemberPathHook)
```

The `EmptyWithCustomisation` helper wires up a default `PreludeGetTypeRef`,
accepts a function that composes hooks into `Customisation.Default`,
and returns the context. `GeneratorContext.Create(preludeFn, ?customisation)`
is the lower-level form that takes the prelude function and optional
customisation explicitly — used by tests that want to inject their own
resolution strategy.

## Hook slots and types

```fsharp
type HookSlot<'T> = {
    Handlers : Hook<'T> list
    HasAny   : bool
}

type SkippableHookSlot<'T> = {
    Handlers : SkippableHook<'T> list
    HasAny   : bool
}

type Hook<'T> = GeneratorContext -> RenderContext -> 'T -> HookResult<'T>

type SkippableHook<'T> = GeneratorContext -> RenderContext -> 'T -> SkippableHookResult<'T>

type HookResult<'T> = | Pass | Replace of 'T

type SkippableHookResult<'T> = | Pass | Replace of 'T | Skip
```

A `HookSlot` is a non-empty list wrapper carrying:
- `Handlers` — accumulated hooks in registration order (latest-registered first).
- `HasAny` — fast path; true iff the list is non-empty (avoids a check every call site).

`HookSlot.run slot ctx rctx value` invokes all handlers left-to-right:
- `Pass` → move to the next handler.
- `Replace v'` → update the value and move to the next handler.
- On the final handler, return the accumulated value.

`SkippableHookSlot.run` additionally respects `Skip`:
- `Skip` → immediately return `Skip` without invoking downstream handlers.

## Operations

The `GeneratorContext` module is partitioned by which dictionary you are
acting on:

```fsharp
GeneratorContext.Prelude.tryGet    ctx key
GeneratorContext.Prelude.addOrReplace ctx key scope
GeneratorContext.Prelude.canFlight ctx resolvedType         // false on re-entry
GeneratorContext.Prelude.addTypeAliasRemap ctx key value

GeneratorContext.Anchored.tryGet           ctx (Choice1Of2 t | Choice2Of2 e)
GeneratorContext.Anchored.tryGetResolvedType  ctx t
GeneratorContext.Anchored.tryGetResolvedExport ctx e
GeneratorContext.Anchored.addOrReplace     ctx key value
GeneratorContext.Anchored.addResolvedType  ctx t value
GeneratorContext.Anchored.addResolvedExport ctx e value
```

The `Anchored.addOrReplace` path dispatches through `AnchoredRef` /
`AnchoredScope` hook slots on every write — this is the last opportunity
to rewrite a render before it lands in the emit-time store. Return `Skip`
to prevent the entry from being stored.

## Customisation record

```fsharp
type Customisation = {
    PathResolutionType    : SkippableHookSlot<TypePath>
    PathResolutionMember  : SkippableHookSlot<MemberPath>
    TypeRefBuild          : HookSlot<TypeRefRender>
    TypeRefEmit           : HookSlot<WidgetBuilder<Type>>
    RenderScopeBuild      : SkippableHookSlot<RenderScope>
    TypeDefBuildClass     : SkippableHookSlot<Concrete.TypeLikeRender>
    TypeDefBuildAlias     : SkippableHookSlot<Concrete.TypeAliasRender>
    TypeDefBuildEnum      : SkippableHookSlot<Concrete.LiteralUnionRender<int>>
    TypeDefBuildStringUnion : SkippableHookSlot<Concrete.LiteralUnionRender<TsLiteral>>
    TypeDefEmit           : HookSlot<WidgetBuilder<TypeDefn>>
    AnchoredRef           : SkippableHookSlot<Anchored.TypeRefRender>
    AnchoredScope         : SkippableHookSlot<Anchored.RenderScope>
}
```

Each field is a hook slot. Register handlers using:

```fsharp
Customisation.addPathResolutionType hook customisation         // prepends hook
Customisation.addPathResolutionTypeTracked hook customisation  // returns (customisation, HandlerToken)
Customisation.addRenderScopeBuild hook customisation           // and so on for each slot
```

All `add*` methods are chainable and return the updated `Customisation`.
`add*Tracked` variants additionally return a `HandlerToken` that can be
passed to `Customisation.remove token customisation` to deregister a handler
later.

### Registration order and composition

Handlers register via `add*` which prepends to the slot's handler list.
When `HookSlot.run` or `SkippableHookSlot.run` is invoked:

1. Iterate handlers left-to-right (latest-registered runs first).
2. Apply the handler to the current value.
3. If the handler returns `Replace v'`, continue with `v'`.
4. If a `SkippableHook` returns `Skip`, stop immediately and return `Skip`.
5. Return the final value.

This means later-registered handlers see the mutations of earlier-registered
ones and can shape behavior without coordination.

`Customisation.Default` provides empty slot lists for every hook;
`GeneratorContext.EmptyWithCustomisation (fun c -> …)` is the recommended
entry point.

## Worked example

```fsharp
let renderScopeBuildHook: SkippableHook<RenderScope> =
    fun ctx rctx scope ->
        match rctx.Owner with
        | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
            SkippableHookResult.Replace { scope with Render = ValueSome (Render.RefOnly scope.TypeRef) }
        | _ -> SkippableHookResult.Pass

let pruneTypescriptTypeHook: SkippableHook<TypePath> =
    fun ctx rctx path ->
        match rctx.Owner with
        | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
            SkippableHookResult.Replace
                (TypePath.pruneParent
                    (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") path)
        | _ -> SkippableHookResult.Pass

let ctx =
    GeneratorContext.EmptyWithCustomisation (fun c ->
        c
        |> Customisation.addRenderScopeBuild renderScopeBuildHook
        |> Customisation.addPathResolutionType pruneTypescriptTypeHook)
```

This is the core configuration in `Generator/Render.fs`. It says:

1. For every lib-ES type, render only the reference (mark `Render = RefOnly`
   in the scope) — do not emit a definition.
2. For every lib-ES path, strip the synthetic `Typescript` parent module so
   the result reads like `MyTypes.Foo` rather than `Typescript.MyTypes.Foo`.

Both hooks dispatch on `rctx.Owner` to distinguish lib-ES types from
user-defined ones. No special interceptor for the path source is needed here;
the `Path` module's `fromX` builders already filter sources during path
construction.
