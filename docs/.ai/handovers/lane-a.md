# Lane A handover — 0.1.0 wave (#75, #74, #73)

Branch `wave/lane-a`, merged into `develop` at `3ddaf95`.

## #75 single-case string enums (665f5fe, b720019)
- `Shape/Spec.fs` `reservedCaseNames`; `Shape/Overloads.fs` `literalDecl` returns the decl with an optional `LU002`; `Render.fs` `renderStringEnum` omits `RequireQualifiedAccess` for one non-reserved case.
- Lab `single-case-enum-lab` (overload-literal arms `fast`/`slow`, reserved `ok`/`error`, multi-case `Level`). `classify-literal-unions` never mints a single-case enum (`literals.Length < 2`), so LU002 is raised in `dedupe-overloads`.
- Goldens moved: workers-types, animejs, export-layout-lab, literal-overload-lab — attribute line only. RunGate consumer sites changed from `Exports.Left.Left` to `Exports.Left` (FS0812 otherwise).

## #74 autoOpenExports (42717d3)
- `Model.fs` field + loader + `Default`, `Schema.fs` row, `xantham.schema.json` regenerated, flag threaded through `renderModule`/`renderNamespace`/`renderCompilerLib` to the single `FsExports` render site. Lab `auto-open-exports-lab`. Flag off: no golden moved.

## #73 statics under an alias path (9bb679f, 6d4b73f)
- `Shape/Classes.fs`: statics grouped by declaration name (via `DeclNames`) and signature; kept occurrence = the class's own binding specifier (`bindingOf`), else first harvested; `SC010` per dropped member with the dropped specifier.
- Harvest cannot tell declaring path from alias (same SymbolId/Order; ambient modules harvested alphabetically), so "declaring module wins" was replaced by "class's own specifier wins".
- Lab `static-reexport-lab` (SC010 = 1). Pre-existing bug, out of scope: two distinct same-named classes in different ambient modules — `SA.AbbreviationNameTaken` drops the first, survivor widens to `obj`:
  ```typescript
  declare module "a" { export class Certificate { constructor(); static exportChallenge(spkac: string): string; } }
  declare module "a/other" { export class Certificate { constructor(); static exportChallenge(spkac: number): number; } }
  ```
