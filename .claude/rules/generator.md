---
paths:
    - src/Xantham.Generator/**
---

### Xantham.Generator

#### Path System (`Types/NamePath.fs`)

Three-level hierarchy: **anchored** (absolute) → **transient** (relative) → **path** (umbrella).

Anchored types: `ModulePath`, `TypePath { Parent: ModulePath; Name: pascal }`, `MemberPath { Parent: Type|Module; Name: camel }`, `ParameterPath { Parent: MemberPath; Index: int }`, `TypeParamPath { Parent: Type|Member|Parameter }`. `AnchorPath` is the canonical root for transient resolution.

Transient types: `TransientModulePath = Anchored | Moored of TransientModulePath * Name<pascal>`. `TransientXxxPath.anchor` converts to anchored by prepending anchor's module trace. `TransientPath.getRelativePath` strips common module prefix.

#### TypeRef/Render System (`Types/TypeRefRender.fs`)

Three-layer model:
- **`TypeRefAtom`** (leaf): `Widget` (Fabulous.AST node) | `AnchorPath of TypePath` | `TransientPath of TransientTypePath`
- **`TypeRefMolecule`** (composite): `Tuple` | `Union` (erased `U2<A,B>`) | `Function (params * ret)` | `Prefix` (generic application)
- **`TypeRefRender`**: `{ Kind: Atom|Molecule; Nullable: bool }` — `Nullable` wraps in `option`

`TypeRender = RefOnly of TypeRefRender | Render of TypeRefRender * TypeRender` — cross-reference vs full in-place render.

Key functions: `TypeRefRender.anchor` resolves transient→concrete; `localisePaths` shortens paths sharing a module prefix; `simpleRender` → `WidgetBuilder<Type>`.

SRTP smart constructors (`TypeRefRender.create`, `Render.createRefOnly`, `Render.create`) dispatch on value type — no need to specify the constructor.

`GeneratorContext` (`Types/Generator.fs`): mutable render cache with `TypeRefRenders`, `ExportRefRenders`, `TypeRenders`, `ExportRenders` dictionaries keyed by `ResolvedType`/`ResolvedExport`.
