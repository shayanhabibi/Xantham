---
title: Source Metadata Migration
category: Xantham.Decoder
categoryindex: 1
index: 6
---

# Migrating from `Source : QualifiedNamePart option` to `Source` DU + `Metadata`

This guide is the canonical reference for consumers of `Xantham.Decoder`
(generators, analysers, tooling) moving from the old "source = optional
qualified name part" model to the new `Metadata` / `Source` model. It
covers:

1. What changed and why.
2. The new types, exactly as you'll see them in `ArenaInterner`.
3. A mechanical mapping from old patterns to new ones.
4. The richer information now available to a generator, and how to reach it.

> [!IMPORTANT]
> `IsLibEs : bool` on `Module`, `Interface`, `Class`, `EnumType`, `Variable`,
> `TypeAlias`, and `Function` is now marked `[<System.Obsolete>]`. New code
> should pattern match against `Source` instead. `IsLibEs` will be removed
> in a future release.

## 1. What changed

### Before

Every named resolved declaration carried two pieces of provenance:

* `IsLibEs : bool` — true when the declaration came from a `lib.es*.d.ts`
  standard library file.
* `Source : QualifiedNamePart option` — the originating module path
  (when known), rendered as a single qualified-name part.

This was enough to render a Fable binding, but it answered no useful
questions beyond it:

* "Which package does this come from?" — couldn't say.
* "Which submodule (.d.ts file) inside that package?" — couldn't say.
* "Is this declaration a public export or an internal helper?" — couldn't say.
* "Through which re-export sites is it reachable?" — couldn't say.

### After

The encoder now stamps a `Xantham.Metadata` record on every declaration,
and the interner resolves that into a discriminated `Source` value that
distinguishes three provenance cases and exposes the full package /
submodule / export graph behind them.

```fsharp
// in Xantham (wire / common types)
[<Struct>]
type Metadata = { Source : Xantham.Source }

[<Struct>]
type Xantham.Source =
    | LibEs           of fileName: string
    | PackageInternal of subModuleId: SubModuleId
    | Package         of exportCollection: ExportCollection
```

The decoder resolves this into the interner-local `Source` DU, lifting
`SubModuleId` / `ExportPoint` references into lazy resolved records:

```fsharp
// in Xantham.Decoder.Types (Arena.Interner.fs)
and Source =
    | LibEs           of fileName: string
    | PackageInternal of LazySubModule
    | Package         of ResolvedExportCollection

and ResolvedExportCollection = {
    Canonical : ResolvedExportPoint
    Aliases   : ResolvedExportPoint list
}

and [<ReferenceEquality>] ResolvedExportPoint = {
    Name      : string
    SubModule : LazySubModule
}
```

`LazySubModule` and `LazyPackage` break the package⇄submodule cycle the
same way `LazyResolvedType` breaks structural cycles — following the
reference forces the lazy and returns the underlying record.

## 2. The new surface, in one place

### `Source` cases

| Case | Payload | Meaning |
|------|---------|---------|
| `LibEs fileName`               | `string` (e.g. `"lib.es2015.core.d.ts"`) | Declaration originates in a TypeScript standard library file. |
| `PackageInternal subModule`    | `LazySubModule`                          | Declaration is reachable through a dependency package but is **not** part of its public surface. |
| `Package collection`           | `ResolvedExportCollection`               | Declaration is publicly exported. `Canonical` is the originating export site; `Aliases` lists every re-export site (barrel files, default re-exports, etc.). |

### Lazy resolved records

```fsharp
and LazyPackage      = LazyContainer<PackageId, ResolvedPackage>
and LazySubModule    = LazyContainer<SubModuleId, ResolvedSubModule>
and LazyExportPoint  = LazyContainer<Xantham.ExportPoint, ResolvedExportPoint>

and [<ReferenceEquality>] ResolvedPackage = {
    Name       : string
    Version    : string
    Json       : Export voption        // package.json export, if any
    SubModules : LazySubModule list
    Entry      : LazySubModule list    // entry submodules (typically "main"/"types")
}

and [<ReferenceEquality>] ResolvedSubModule = {
    Name         : string
    Path         : string              // resolved .d.ts path
    Package      : LazyPackage
    Dependees    : LazySubModule list  // submodules that depend on this one
    Dependencies : LazySubModule list  // submodules this one depends on
    Exports      : LazyResolvedExport list
}
```

Force a lazy with `.Value` or with the existing `(|Resolve|)` active
pattern used elsewhere in the interner.

### `ArenaInterner` additions

`ArenaInterner` gained five new entry points and three new caches:

```fsharp
type ArenaInterner = {
    // existing
    ResolveType        : TypeKey -> ResolvedType
    ResolveExport      : TypeKey -> Result<ResolvedExport, ResolvedType>
    ResolvedTypes      : IDictionary<TypeKey, ResolvedType>
    ResolvedExports    : IDictionary<TypeKey, ResolvedExport>
    Graph              : Lazy<Graph>

    // new
    ResolvePackage     : PackageId           -> ResolvedPackage
    ResolveSubModule   : SubModuleId         -> ResolvedSubModule
    ResolveExportPoint : Xantham.ExportPoint -> ResolvedExportPoint
    ResolvedPackages     : IDictionary<PackageId,           ResolvedPackage>
    ResolvedSubModules   : IDictionary<SubModuleId,         ResolvedSubModule>
    ResolvedExportPoints : IDictionary<Xantham.ExportPoint, ResolvedExportPoint>

    // ExportMap is now keyed by Xantham.Source, not by string path
    ExportMap          : Map<Xantham.Source, LazyResolvedExport>
}
```

### `DecodedResult` additions

`Decoder.read` now returns a `DecodedResult` that exposes the same graph
in unresolved form for consumers that don't need the interner:

```fsharp
type SourceExportMap     = Map<SubModuleId voption, Set<TypeKey>>
type SourceDependencyMap = Map<SubModuleId, SubModuleRelation list>
type SubModuleMap        = Map<SubModuleId, SubModule>
type PackageMap          = Map<PackageId, Package>

type DecodedResult = {
    TypeMap             : TypeMap
    ExportTypeMap       : ExportTypeMap
    ExportMap           : SourceExportMap         // was Map<string, Set<TypeKey>>
    SourceDependencyMap : SourceDependencyMap     // new — submodule dependency edges
    SourceDependeeMap   : SourceDependencyMap     // new — reverse edges
    SubModuleMap        : SubModuleMap            // new
    PackageMap          : PackageMap              // new
    TopLevelExports     : TypeKey list
    LibEsExports        : TypeKey list
}
```

`ExportMap`'s key change is the only breaking change in `DecodedResult`:
`ValueNone` represents the default-library bucket; `ValueSome subModuleId`
identifies a specific user/package submodule.

## 3. Mechanical migration map

Every old pattern has a direct replacement. Where the explore agent found
the old patterns still in tree (`src/Xantham.Generator`), the migration is
literally a search-and-replace plus one additional `match` arm.

### 3.1 "Is this from the TS standard library?"

```fsharp
// Before
| ResolvedType.Interface { IsLibEs = true }
| ResolvedType.Class     { IsLibEs = true }
| ResolvedType.Enum      { IsLibEs = true } -> ...

// After
| ResolvedType.Interface { Source = Source.LibEs _ }
| ResolvedType.Class     { Source = Source.LibEs _ }
| ResolvedType.Enum      { Source = Source.LibEs _ } -> ...
```

If you need the file name (e.g. to distinguish `lib.es5` from
`lib.dom.iterable`), bind it: `Source = Source.LibEs fileName`.

### 3.2 "Where did this come from?" — rendering a source comment

```fsharp
// Before
match decl.Source with
| Some part -> renderPart part
| None      -> ()

// After
match decl.Source with
| Source.LibEs fileName ->
    renderLibSource fileName
| Source.PackageInternal (Resolve subModule) ->
    renderInternalSource subModule.Package.Value subModule
| Source.Package collection ->
    renderPublicSource collection.Canonical
```

The old `None` case has no exact analogue, because every declaration
now has *some* provenance. The closest equivalent is `Source.LibEs _`
(the typescript default lib used to surface as `None`).

### 3.3 `enumCase.Source : QualifiedNamePart option` (Generator/Render.Transient.fs)

`EnumCase` and `TypeQuery` no longer carry a `Source` field at all —
they inherit their provenance from the containing `EnumType` / referenced
declaration. The literal call sites in
`Generator/Render.Transient.fs:25,107,342,348`,
`Generator/Render.TypeAlias.fs:231`, etc. need to be rewritten to:

```fsharp
// Before
Source = enumCase.Source |> Option.toValueOption

// After: walk up to the EnumType for provenance
let enumType =
    match interner.ResolveType containingKey with
    | ResolvedType.Enum e -> e
    | _ -> failwithf "expected enum for %A" containingKey
let source = enumType.Source |> Render.sourceToRenderSource
```

(See §4 for the suggested `sourceToRenderSource` helper.)

### 3.4 Treating the `Render*` `Source` field

Where renderers already store a `Source` on their own `Render*` types
(`RenderScope.Anchored.fs:43,57,81,102`), the line

```fsharp
Source = enumUnion.Metadata.Source
```

is unaffected at the field level (renderers still hold `Xantham.Source`),
but the value flowing in is now always populated. Any code that branched
on `Option.isNone` should be re-examined — there is no "no source"
state any more.

## 4. New information available to generators

This is the point of the migration: the old representation answered one
question (lib-es or not, plus a string hint). The new representation
answers a lot more, and the interner makes all of it reachable in O(1)
or one lazy force.

### 4.1 Distinguish public exports from internal reachability

```fsharp
let categorise (resolved: ResolvedExport) =
    let src =
        match resolved with
        | ResolvedExport.Variable  v -> v.Source
        | ResolvedExport.Interface i -> i.Source
        | ResolvedExport.Class     c -> c.Source
        | ResolvedExport.TypeAlias t -> t.Source
        | ResolvedExport.Enum      e -> e.Source
        | ResolvedExport.Module    m -> m.Source
        | ResolvedExport.Function  f -> f.Source
    match src with
    | Source.LibEs _           -> Category.StandardLibrary
    | Source.PackageInternal _ -> Category.PackageInternal
    | Source.Package _         -> Category.PublicExport
```

A generator can now skip `PackageInternal` declarations entirely (they
aren't part of any package's public API), or emit them into a separate
internal namespace.

### 4.2 Reach the owning package and submodule

```fsharp
let packageOf (src: Source) : ResolvedPackage option =
    match src with
    | Source.LibEs _               -> None
    | Source.PackageInternal sm    -> Some sm.Value.Package.Value
    | Source.Package coll          -> Some coll.Canonical.SubModule.Value.Package.Value
```

From a `ResolvedPackage` you get `Name`, `Version`, `Entry`, every
submodule, and the optional `package.json` export. This is the natural
keying for "one F# project / namespace per npm package" generators.

### 4.3 Resolve re-export aliases

```fsharp
// Render "originally exported as X from foo/index.d.ts, also re-exported
// as Y from foo/legacy.d.ts and as Z from foo/default.d.ts"
let renderExportPath (src: Source) =
    match src with
    | Source.Package { Canonical = c; Aliases = aliases } ->
        printfn $"canonical: {c.Name} @ {c.SubModule.Value.Path}"
        for a in aliases do
            printfn $"  alias: {a.Name} @ {a.SubModule.Value.Path}"
    | _ -> ()
```

`Aliases` is the bridge between barrel files and the canonical export.
Generators that want to honour the `export { Foo as Bar } from "./x"`
shape now have direct access to every alias instead of inferring it from
duplicate emission.

### 4.4 Walk the submodule dependency graph

```fsharp
let directDependencies (sm: ResolvedSubModule) : ResolvedSubModule list =
    sm.Dependencies |> List.map (_.Value)

let directDependees (sm: ResolvedSubModule) : ResolvedSubModule list =
    sm.Dependees |> List.map (_.Value)
```

These are the resolved-graph equivalents of `DecodedResult.SourceDependencyMap`
/ `SourceDependeeMap` and let a generator emit per-submodule files in
topological order without re-deriving the graph.

### 4.5 Iterate every public export of a package

```fsharp
let exportsOf (pkg: ResolvedPackage) : ResolvedExport seq = seq {
    for lazySubModule in pkg.SubModules do
        let sm = lazySubModule.Value
        for lazyExport in sm.Exports do
            yield lazyExport.Value
}
```

### 4.6 Look an export up by source bucket

`ArenaInterner.ExportMap` is now keyed by `Xantham.Source`. That makes
"give me every public export and only the public ones" a one-liner:

```fsharp
let publicExports =
    interner.ExportMap
    |> Map.toSeq
    |> Seq.choose (function
        | Xantham.Source.Package _, lazyExport -> Some lazyExport.Value
        | _ -> None)
```

### 4.7 Suggested helper: convert `Source` to a renderable shape

If your `Render*` types still hold a `QualifiedNamePart option`,
introduce a single conversion at the boundary:

```fsharp
module Render =
    let sourceToRenderSource : Source -> RenderSource voption = function
        | Source.LibEs fileName ->
            RenderSource.LibEs fileName |> ValueSome
        | Source.PackageInternal sm ->
            RenderSource.Internal {
                Package   = sm.Value.Package.Value.Name
                SubModule = sm.Value.Path
            } |> ValueSome
        | Source.Package coll ->
            RenderSource.Public {
                Package   = coll.Canonical.SubModule.Value.Package.Value.Name
                SubModule = coll.Canonical.SubModule.Value.Path
                ExportAs  = coll.Canonical.Name
                Aliases   =
                    coll.Aliases
                    |> List.map (fun a -> a.Name, a.SubModule.Value.Path)
            } |> ValueSome
```

This lets the renderer keep its own simplified DU while still surfacing
the new information when it matters.

## 5. Migration checklist

For each consumer module:

- [ ] Replace every `IsLibEs = true` match with `Source = Source.LibEs _`.
- [ ] Replace every read of `decl.Source` (where the old type was
      `QualifiedNamePart option`) with a pattern match on the new
      three-case `Source` DU.
- [ ] Drop reads of `enumCase.Source` / `typeQuery.Source` — those fields
      are gone; pull provenance from the containing `EnumType` or the
      referenced declaration via the interner.
- [ ] If you key any cache by source path string, switch the key to
      `Xantham.Source` (or `SubModuleId` / `PackageId` as appropriate).
- [ ] Decide whether to skip, surface, or namespace `PackageInternal`
      declarations.
- [ ] Decide whether to emit re-export aliases as their own bindings or
      collapse them to a single canonical binding.

After this pass, the obsolete `IsLibEs` field can be removed from the
interner without breaking any consumer.
