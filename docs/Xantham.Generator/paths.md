---
title: Path System
category: Xantham.Generator
categoryindex: 3
index: 2
---

# Path System

`Types/NamePath.fs` defines a type-safe hierarchy of names that the rest of
the generator threads everywhere a type, member, parameter, or module needs
to be addressed. The hierarchy has three levels:

1. **Anchored** — absolute paths rooted at a real `ModulePath`.
2. **Transient** — partial paths that resolve relative to an anchor.
3. **Umbrella** — `Path`, `TypeLikePath`, `MemberLikePath`, etc. — DUs that
   accept either anchored or transient values.

## Anchored paths

```fsharp
type ModulePath    = { Parent: ModulePath voption; Name: Name<Case.pascal>; Depth: int }
type TypePath      = { Parent: ModulePath; Name: Name<Case.pascal> }
type MemberPath    = { Parent: TypePath | ModulePath; Name: Name<Case.camel> }
type ParameterPath = { Parent: MemberPath; Name: Name<Case.camel>; Index: int }
type TypeParamPath = { Parent: TypePath | MemberPath | ParameterPath; Name: Name<Case.typar> }

type AnchorPath = TypeParam | Parameter | Member | Type | Module
```

`AnchorPath` is the canonical root used when resolving a transient path.
Every concrete addressable thing in the source graph corresponds to one of
its five cases, and every transient path is interpreted by tracing back to a
parent module via `AnchorPath.traceToParentModule`.

`Name<Case.*>` is the case-tagged identifier type from `Xantham.Decoder`.
`Name.Case.valueOrSource` retrieves the original spelling;
`Name.Case.valueOrModified` retrieves the F#-safe spelling. The path system
does not pick between them — that decision happens at render time.

## Transient paths

```fsharp
type TransientModulePath    = Anchored | Moored of parent * name | AnchoredAndMoored of name
type TransientTypePath      = Anchored | Moored of TransientModulePath * name | AnchoredAndMoored of name
type TransientMemberPath    = Anchored | Moored of TransientTypePath * name | AnchoredAndMoored of name
type TransientParameterPath = Anchored | Moored of TransientMemberPath * name | AnchoredAndMoored of name
```

* **`Anchored`** — "I am exactly the anchor."
* **`AnchoredAndMoored name`** — "I am one segment under the anchor."
* **`Moored(parent, name)`** — recursive build-up; the transient root may
  itself be `Anchored` or `AnchoredAndMoored`.

Resolution is a fold: `TransientXxxPath.toAnchored` walks down to a flat
list of `Name<_>` segments, then `TransientXxxPath.anchor anchorPath`
splices that list under the anchor's traced module + path prefix.

`TransientPath` is the umbrella over the four transient cases (note: there
is no `TransientPath.Module` — modules never appear as transient leaves on
their own).

`TransientPath.Helpers.removeCommonRoots` strips a shared prefix between two
transient paths — used during localisation to avoid printing redundant
qualifiers when two cross-references live in the same module.

## Umbrella DUs

Where a function may legitimately receive either an anchored or a transient
form, the public surface is one of:

```fsharp
TypeLikePath, MemberLikePath, ParameterLikePath, TypeParamLikePath, ModuleLikePath
```

Each has a `SRTPHelper` so call sites can pass either form without ceremony:

```fsharp
TypeLikePath.create somePath        // dispatches on TypePath | TransientTypePath
ParameterLikePath.createWithName n p // works for both anchored and transient parents
```

## Relative paths between two anchors

`Path.getRelativePath target from` answers *"how should `target` be
referenced from `from`?"*. It compares the two module traces, finds the
last common segment, and returns the suffix of `target.Parent` plus
`target.Name`. This is what produces `Foo.Bar.MyType` instead of
`Root.Foo.Bar.MyType` when both ends share `Root`.

`anchored.TypeRefAtom.localise` uses the same logic on a per-atom basis when
lowering a `TypeRefRender` for a specific anchor.

## The `pathCe` builder

`PathBuilder` (aliased as `let pathCe = PathBuilder()`) is a computation
expression that turns path construction into something readable in tests and
generator setup. The convention:

* `_module "Name"`, `_type "Name"`, `_member "Name"`, `_parameter "Name"`,
  `_typar "Name"` build **anchored** segments.
* `_module_ "Name"`, `_type_ "Name"`, `_member_ "Name"`, `_parameter_ "Name"`
  build **transient** segments.
* The bare custom operations (`_module_`, `_type_`, …) with no string yield
  `Anchored`.
* `asAnchorPath`, `asTransientPath`, `asPath` cast the final value into an
  umbrella DU.

```fsharp
let myType =
    pathCe {
        _module "Root"
        _module "Foo"
        _type "Bar"
    }

let transient =
    pathCe {
        _module_ "Foo"
        _type_ "Bar"
    }

let anchored =
    pathCe {
        _module "Root"
        _type "Bar"
        asAnchorPath
    }
```

## Path hooks: `PathResolutionType` and `PathResolutionMember`

Path construction happens at two stages:

1. **`Path.tryResolveTypePath`** — Takes a raw `TypePath` from one of the
   builders (`Path.fromInterface`, `Path.fromEnum`, etc.), dispatches through
   the `PathResolutionType` hook slot, and returns `ValueSome resolved \| ValueNone`
   (if the hook returned `Skip`). Called for interface, class, enum, and type
   alias definitions.

2. **`Path.tryResolveMemberPath`** — Takes a raw `MemberPath`, dispatches
   through the `PathResolutionMember` hook slot, and returns the same options.
   Called for member paths (properties, methods, variables, functions).

Both hooks receive a `RenderContext` that includes:
- `Owner` — the `ResolvedType` or `ResolvedExport` containing the path, allowing
  the handler to match on kind (interface vs class vs enum) or other metadata.
- `Position` — `RenderPosition.PathPos (PathPosition.TopLevelType \| MemberPath \| VariablePath \| FunctionPath)`,
  letting the handler disambiguate.
- `Stage` — always `RenderStage.PathResolution`.

A hook can:
- Return `SkippableHookResult.Pass` — leave the path unchanged.
- Return `SkippableHookResult.Replace path'` — use the rewritten path.
- Return `SkippableHookResult.Skip` — elide the type/member (it will not be
  rendered).

### Mutating and pruning

`ModulePath.pruneParent predicate` walks up the chain and drops the first
parent that matches the predicate, replacing it with a name-only root.
`TypePath.pruneParent` and `MemberPath.pruneParent` lift this onto the
nested forms.

The reference generator uses `PathResolutionType` and `PathResolutionMember`
hooks to strip the synthetic `Typescript` module from `IsLibEs = true`
declarations:

```fsharp
let pruneTypescriptParentType: SkippableHook<TypePath> =
    fun ctx rctx path ->
        match rctx.Owner with
        | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
        | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
            SkippableHookResult.Replace
                (TypePath.pruneParent
                    (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") path)
        | _ -> SkippableHookResult.Pass

// Register:
GeneratorContext.EmptyWithCustomisation
    (Customisation.addPathResolutionType pruneTypescriptParentType)
```

`ModulePath.mutateChain` is the general escape hatch — it applies a function
at every link of the parent chain and is used when a customisation needs to
rewrite an entire module trace at once.
