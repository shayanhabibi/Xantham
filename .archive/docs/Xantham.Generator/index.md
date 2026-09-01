---
title: Overview
category: Xantham.Generator
categoryindex: 3
index: 1
---

# Xantham.Generator

`Xantham.Generator` is the rendering library that sits on top of
[`Xantham.Decoder`](../Xantham.Decoder/index.html). The decoder hands you a
lazy resolved object graph (`ArenaInterner`); the generator turns that graph
into Fabulous.AST `WidgetBuilder<_>` values that ultimately become an F#
syntax tree.

The library does not pick a generation strategy for you — it provides the
machinery (paths, type-ref model, render scopes, customisation hooks) on
which a generator is built. `src/Xantham.Generator/Generator/Render.fs` is
the reference entry point and shows how the pieces compose.

## What lives where

| Layer | Files | Purpose |
|-------|-------|---------|
| **Path system** | `Types/NamePath.fs` | Type-safe absolute (anchored) and relative (transient) paths. Three-level hierarchy: anchored → transient → umbrella. See [Paths](paths.html). |
| **TypeRef / Render model** | `Types/RenderScope.Prelude.fs`, `Types/RenderScope.Anchored.fs` | The `TypeRefAtom` / `TypeRefMolecule` / `TypeRefRender` triad — leaf, composite, nullable wrapper. `Render` distinguishes a cross-reference from a full in-place definition. See [TypeRef and Render](typeref-render.html). |
| **Generator context** | `Types/Generator.fs` | `GeneratorContext` mutable cache (`PreludeRenders`, `AnchorRenders`, `TypeAliasRemap`, `InFlight`) plus the `Customisation` / `Interceptors` records. See [GeneratorContext](generator-context.html). |
| **Path construction** | `Generator/TypeRefRender.Paths.fs` | Builds `ModulePath` / `TypePath` / `MemberPath` from `QualifiedName` + `Source` for each `ResolvedType` / `ResolvedExport`. |
| **TypeRef rendering** | `Generator/TypeRefRender.Render.fs`, `Generator/TypeRender.Render.fs` | Lowers a `TypeRefRender` (or a full `TypeRender`) into a `WidgetBuilder<Type>` / `WidgetBuilder<TypeDefn>` etc. |
| **Categorization** | `Generator/ResolvedType.Categorization.fs` | Splits the flat `ResolvedType` DU into kind buckets (`ResolvedTypeOther`, `ResolvedTypeEnumLike`, `ResolvedTypeLiteralLike`, `ResolvedTypePrimitiveLike`) for ergonomic dispatch. |
| **Render passes** | `Generator/Render.*.fs` | Per-shape lowerings — type aliases, parameters, members, type shapes, enums, transients. |
| **Module assembly** | `Generator/Render.Collection.fs` | Walks the anchored render store and groups everything by module path into a `RootModule` tree ready to be emitted. |
| **Entry** | `Generator/Render.fs` | Reads the JSON, builds an `ArenaInterner`, drives prelude + export passes, collects modules, and prints the resulting `Oak`. |

## Pipeline at a glance

1. **Decode** — `Decoder.Runtime.create file` produces a `XanthamTree`; `tree.GetArenaInterner()` yields the resolved graph.
2. **Build a `GeneratorContext`** — `GeneratorContext.EmptyWithCustomisation` injects user interceptors (path pruning, source filtering, prelude shaping, anchored-render rewriting).
3. **Prelude pass** — `ArenaInterner.prerenderTypeAliases` walks alias chains and seeds `TypeAliasRemap`.
4. **Export pass** — `ArenaInterner.processExports` iterates exported declarations, anchors transient renders against concrete `TypePath` / `MemberPath` roots, and stores the result in `AnchorRenders`.
5. **Collect** — `RootModule.collectModules` re-organises the flat anchor store into a nested `Module` tree keyed by module path.
6. **Emit** — `renderRoot` lowers the tree into Fabulous.AST nodes; `Gen.mkOak >> Gen.run` produces the F# source string.

## Key types in one breath

* `TypeRefAtom = Widget | Intrinsic | ConcretePath | TransientPath`
* `TypeRefMolecule = Tuple | Union | Function | Prefix`
* `TypeRefRender = { Kind: Atom | Molecule; Nullable: bool }` — `Nullable` wraps the rendered type in `option`
* `Render = RefOnly of TypeRefRender | Concrete of … | Transient of …` — cross-reference vs full in-place render
* `RenderScope<'Root,'Render> = { Type; Root; TypeRef; Render; TransientChildren }` — keyed by `ResolvedType`, cached on `GeneratorContext`

## Where to go next

* **[Paths](paths.html)** — anchored / transient hierarchy, the `pathCe` builder, and how transient paths get resolved against an anchor.
* **[TypeRef and Render](typeref-render.html)** — atom / molecule / render layering and the SRTP smart constructors.
* **[GeneratorContext](generator-context.html)** — the mutable cache, customisation surface, and interceptor types.
* **[`pipeline.fsx`](pipeline.html)** — annotated end-to-end walk of `Generator/Render.fs`.
