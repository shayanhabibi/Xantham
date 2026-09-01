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

```fsharp
let ctx =
    GeneratorContext.EmptyWithCustomisation (fun c ->
        { c with
            Customisation.Interceptors.IgnorePathRender.Source = …
            Customisation.Interceptors.Paths.TypePaths        = …
            Customisation.Interceptors.ResolvedTypePrelude    = …
        })
```

The `EmptyWithCustomisation` helper wires up a default
`PreludeGetTypeRef` and lets the caller mutate the customisation record
through a function. `GeneratorContext.Create(preludeFn, ?customisation)`
is the lower-level form that takes the prelude function explicitly — used
by tests that want to inject their own resolution strategy.

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

The `Anchored.addOrReplace` path runs the
`Customisation.Interceptors.AnchoredRender` interceptor on every write —
this is the last opportunity to rewrite a render before it lands in the
emit-time store.

## Customisation surface

```fsharp
type InterceptorIgnorePathRender = {
    Source        : ArenaInterner.QualifiedNamePart -> bool
    QualifiedName : QualifiedName -> bool
}

type InterceptorPaths = {
    TypePaths    : GeneratorContext -> Choice<Interface, EnumType, Class, TypeAlias> -> TypePath -> TypePath
    MemberPaths  : GeneratorContext -> Choice<Variable, Function> -> MemberPath -> MemberPath
}

type Interceptors = {
    IgnorePathRender    : InterceptorIgnorePathRender
    Paths               : InterceptorPaths
    ResolvedTypePrelude : GeneratorContext -> ResolvedType -> RenderScope -> RenderScope
    AnchoredRender      : GeneratorContext -> Choice<ResolvedType, ResolvedExport>
                           -> Choice<Anchored.TypeRefRender, Anchored.RenderScope>
                           -> Choice<Anchored.TypeRefRender, Anchored.RenderScope>
}

type Customisation = { Interceptors: Interceptors }
```

* **`IgnorePathRender.Source`** — return `true` to drop a qualified-name
  source segment before it becomes part of a path. Use this to filter out
  `babel`, `typescript`, or other infrastructure markers.
* **`IgnorePathRender.QualifiedName`** — same idea, but applied to the
  fully parsed `QualifiedName`.
* **`Paths.TypePaths` / `Paths.MemberPaths`** — rewrite an absolute path
  before it is stored. Typically used with `TypePath.pruneParent` to remove
  injected prefixes.
* **`ResolvedTypePrelude`** — last-mile mutation of a prelude
  `RenderScope`. The reference generator uses it to force every lib-ES type
  into `Render.RefOnly`, so that standard-library types are *referenced*
  but never *defined* in the output.
* **`AnchoredRender`** — same idea at the anchor level.

`Customisation.Default` provides identity functions for every hook;
`Customisation.Create fn` is the recommended entry point because it
preserves any hooks the runtime adds in the future.

## Worked example

```fsharp
let ctx =
    GeneratorContext.EmptyWithCustomisation (fun c ->
        { c with
            Customisation.Interceptors.IgnorePathRender.Source = function
                | QualifiedNamePart.Normal text
                | QualifiedNamePart.Abnormal(text, _) ->
                    text.Contains("typescript", StringComparison.OrdinalIgnoreCase)

            Customisation.Interceptors.ResolvedTypePrelude = fun _ -> function
                | ResolvedType.Interface { IsLibEs = true }
                | ResolvedType.Class     { IsLibEs = true }
                | ResolvedType.Enum      { IsLibEs = true } ->
                    fun scope -> { scope with Render = Render.RefOnly scope.TypeRef }
                | _ -> id

            Customisation.Interceptors.Paths.TypePaths = fun _ typ p ->
                match typ with
                | Choice1Of4 { IsLibEs = true }
                | Choice2Of4 { IsLibEs = true }
                | Choice3Of4 { IsLibEs = true }
                | Choice4Of4 { IsLibEs = true } ->
                    TypePath.pruneParent
                        (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") p
                | _ -> p })
```

This is the exact configuration in `Generator/Render.fs`. It says:

1. Drop the `typescript` source qualifier when computing module paths.
2. For every lib-ES type, do not emit a definition — emit only the
   reference.
3. For every lib-ES path, strip the synthetic `Typescript` parent module so
   the result reads like `MyTypes.Foo` rather than `Typescript.MyTypes.Foo`.
