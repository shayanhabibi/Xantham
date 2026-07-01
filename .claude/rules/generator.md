---
paths:
    - src/Xantham.Generator/**
---

### Xantham.Generator

#### Toolchain pin — DO NOT bump Fantomas naively (see `docs/toolchain-fantomas-fabulous-ast.md`)
Output is formatted via **Fabulous.AST 2.0.0-pre06 -> Fantomas.Core 7.0.1** (transitive, and every
Fabulous.AST release incl. pre07/pre08 hard-pins `Fantomas.Core [7.0.1]`). Fantomas 7.0.1 has an
offside/doc-comment layout bug (multi-line generic tupled params; `<deprecated>`/`<br/>` blocks in a
large `module rec`) fixed only in **Fantomas 8.0.0-alpha** — which is **unreachable**: overriding
Fantomas ABI-crashes (`BindingNode..ctor`), and pre08 crashes on `FSharp.Core` resolution. This
accounts for a residual ~34 FS errors (~24 strict-indentation-only, ~10 doc-comment parser cascades)
that do NOT isolate. `<br/>` is load-bearing (do not strip). Revisit only when a Fabulous.AST release
ships against Fantomas ≥ 8. Full breadcrumb + verified dead-ends: `docs/toolchain-fantomas-fabulous-ast.md`.

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
