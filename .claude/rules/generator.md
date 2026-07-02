---
paths:
    - src/Xantham.Generator/**
---

### Xantham.Generator

#### Emission model (the ONLY artifact)
The generator emits PARTITIONED UNITS (`--recipe <toml> --out-dir <dir>`, Emission.fs): one
compilation unit per recipe lib in publish order under `namespace rec Fidelity.CloudEdge`, gated by
`scripts/partition-gate.sh` (+ golden/arity over the concatenated unit sources). There is NO other
output mode — the legacy stdout monolith was retired 2026-07-03 (a kept monolith is only a
counter-example; borrowing invites drift). Judge L1 by the partition gate.

#### Toolchain pin — DO NOT bump Fantomas (see `docs/toolchain-fantomas-fabulous-ast.md`)
Fabulous.AST 2.0.0-pre06 hard-pins Fantomas.Core [7.0.1]; every upgrade path is a verified dead end
(ABI/FSharp.Core crashes) — AND upgrading is unnecessary. The historical "~34-error floor" was two
defects, both resolved 2026-07-03: the `<deprecated>`/`<br/>` doc-comment cascade was OURS
(unsanitized `\r` in upstream JSDoc; fixed by CR normalization in `normalizeDocString`,
TypeRender.Render.fs), and Fantomas's real long-annotation wrapping bug is avoided by wide-format
emission (`Gen.runWith { Default with MaxLineLength = 100000 }`, Emission.fs). Do not reintroduce
default-width formatting for generated units.

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
