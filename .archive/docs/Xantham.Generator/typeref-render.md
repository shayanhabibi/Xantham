---
title: TypeRef and Render
category: Xantham.Generator
categoryindex: 3
index: 3
---

# TypeRef and Render

The render layer is split into a *reference* model (how to mention a type)
and a *definition* model (how to declare a type). Almost everything the
generator produces is a `TypeRefRender` first, lowered to a Fabulous.AST
`WidgetBuilder<Type>` only at emit time.

## The three-layer reference model

```fsharp
TypeRefAtom =                     // leaf
    | Widget of WidgetBuilder<Type>
    | Intrinsic of string
    | ConcretePath of TypePath
    | TransientPath of TransientTypePath

TypeRefMolecule =                 // composite
    | Tuple of TypeRefRender list
    | Union of TypeRefRender list                     // erased U2/U3 union
    | Function of params: TypeRefRender list * ret: TypeRefRender
    | Prefix of prefix: TypeRefRender * args: TypeRefRender list

TypeRefKind = Atom of TypeRefAtom | Molecule of TypeRefMolecule

TypeRefRender = { Kind: TypeRefKind; Nullable: bool }
```

* **Atom** — a leaf. A literal Fabulous widget, an intrinsic ident
  (`string`, `obj`, …), an absolute `TypePath`, or a transient path that
  still needs to be resolved against an anchor.
* **Molecule** — built from other `TypeRefRender` values. `Tuple` and
  `Union` are flat lists; `Function` is a parameter list plus return type;
  `Prefix` represents generic application (`Foo<A,B>` → `Prefix(Foo, [A;B])`).
* **`Nullable`** — a render-time flag. Setting it true wraps the lowered
  type in `option`. Stored on the atom for atoms; stored once on the
  molecule for composites.

The cases of `TypeRefAtom` and `TypeRefMolecule` are private — call sites go
through smart constructors on `RenderScopeStore` (or via the SRTP `create`
dispatch described below). This is what guarantees that any transient path
mentioned in a render is actually registered with the active scope.

## The render layer

```fsharp
Render =
    | RefOnly of TypeRefRender                     // cross-reference only
    | Concrete of Concrete.Render
    | Transient of Transient.Render
```

* **`RefOnly`** — the type is mentioned somewhere else but does not need to
  be defined here.
* **`Concrete`** — the type has an absolute path; rendering produces a full
  `TypeRender` node (interface, class, alias, enum, …).
* **`Transient`** — the type is local to its parent (e.g. an inline type
  literal); rendering produces an in-place definition under the parent's
  module.

`Render.SRTPHelper.Create` overloads on `(TypeRefRender, _)` so call sites
can write `Render.create typeRef render` regardless of whether `render` is a
`Concrete.TypeRender`, `Transient.TypeRender`, eager value, or a thunk.

## RenderScope

A `RenderScope` ties a `ResolvedType` to its rendered form:

```fsharp
type RenderScope<'RootPathType, 'RenderType> = {
    Type: ResolvedType
    Root: 'RootPathType
    TypeRef: TypeRefRender
    Render: 'RenderType
    TransientChildren: RenderScopeStore voption
}
```

There are two specialisations:

* `Transient.RenderScope` — `Root: TransientTypePath`, lives inside another
  type or alias.
* `RenderScope` (the unsuffixed one) — `Root: TypeLikePath voption`, the
  prelude/anchor cache form.

`RenderScopeStore` is the per-scope registry of transient paths discovered
during a render. Every transient `TypeRefAtom.createTransientPath` call
grafts the scope's `PathContext` over the new path and records the result
in `TypeStore` keyed by `ResolvedType`. This is what later lets the anchor
pass walk the tree and emit each transient under its proper concrete root.

## Anchoring and localisation

Once a `Concrete.RenderScope` (or anchored render) is decided for a
declaration, every transient `TypeRefRender` it contains is rewritten:

* **`Anchored.TypeRefRender.anchor anchorPath ref`** — replaces every
  `TransientPath` atom with a concrete `TypePath` rooted under
  `anchorPath`'s traced module.
* **`Anchored.TypeRefRender.localise anchorPath ref`** — turns the now-
  concrete paths into the shortest `LongIdent` that still resolves from the
  anchor's location, using `Path.getRelativePath`.
* **`anchorAndLocalise`** — the obvious composition.

After both passes the render contains only `Widget` atoms and lowering with
`TypeRefRender.render` is a straightforward fold into `Ast.LongIdent`,
`Ast.Tuple`, `erasedUnion`, `Ast.Funs`, and `Ast.AppPrefix`.

## SRTP smart constructors

`RenderScopeStore.TypeRef.create` and `RenderScopeStore.TypeRefRender.create`
dispatch on the value type so call sites do not have to choose a constructor
explicitly. The same `create` works for any of:

```text
TypeRefAtom            // already a leaf
TypeRefMolecule        // already a composite
TypeRefKind            // either of the above, pre-tagged
TypeRefRender          // existing render — only Nullable is overwritten
TypePath               // → ConcretePath atom
TransientTypePath      // → TransientPath atom (registered with scope)
WidgetBuilder<Type>    // → Widget atom
string                 // → Intrinsic atom
TypeRefRender list     // → Union molecule
TypeRefRender array    // → Tuple molecule
TypeRefRender list * TypeRefRender   // → Function molecule
TypeRefRender * TypeRefRender list   // → Prefix molecule
```

This is intentionally aggressive — generators chain `|> srtpFunc` on the
back of arbitrary expressions and the compiler picks the right overload from
the value's static type.

## Per-shape render records

The records under the `Concrete` and `Transient` modules describe full
declarations rather than references:

| Record | What it represents |
|--------|--------------------|
| `TypeParameterRender` | A single `<T>` slot, optional constraint, optional default. |
| `TypedNameRender` | A named, typed thing — property, variable, constructor argument. Carries `RenderTraits` (Optional, ParamArray, Static, Readable, Writable, Literal, JS\*, EmitSelf, Inline, StringBuilder). |
| `FunctionLikeSignature` / `FunctionLikeRender` | A function with one or many overloaded signatures. |
| `LiteralCaseRender` / `LiteralUnionRender` | An enum case or string literal union. |
| `TypeLikeRender` | An interface / class shape — type parameters, inheritance, members, functions, constructors. |
| `TypeAliasRender` | An alias — either a `TypeAliasRenderRef` (shallow rename), a re-emitted `TypeLikeRender`, a `LiteralUnionRender`, or a function alias. |
| `TypeRender` | Top-level union of `TypeDefn | TypeAlias | StringUnion | EnumUnion | Function | Variable`. |

The two flavours (`Concrete` and `Transient`) share their generic types and
differ only in the `'TypeName` parameter — `Concrete` requires a name,
`Transient` makes it `voption` because anonymous inline types may not have
one.

## RenderMetadata

Every render record above carries a `RenderMetadata`:

```fsharp
[<Struct>]
type RenderMetadata = {
    Path: Path
    Original: Path
    Source: ArenaInterner.QualifiedNamePart voption
    FullyQualifiedName: ArenaInterner.QualifiedNamePart list voption
}
```

`Path` is the post-customisation path (after interceptor pruning),
`Original` is the path as it was first computed. `Source` and
`FullyQualifiedName` come from the decoder and feed `[<Import>]` /
`[<ImportMember>]` attributes — see `tryRenderMetadataImport` in
`Render.Collection.fs` for how they are consumed.
