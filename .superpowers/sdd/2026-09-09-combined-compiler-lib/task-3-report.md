# Task 3 report: route compiler libraries through configured modules

## Change

- `Pipeline.groupModulesForScope` creates `CompilerLibLayout` once and uses its fully qualified ES/DOM modules for compiler-library group ownership.
- Other `PackageId` module naming remains on the existing `Naming.groupModule` route.
- The scripthost lab now uses `Fable.Core.TS`, `Ecma`, and `Browser`, with DOM auto-open only.
- The compact golden replaces the stale `groups/TypeScript.Lib.fs` with `groups/Fable.Core.TS.fs`.

## TDD evidence

The new ownership theory was run before the pipeline implementation and failed as intended:

```text
expected Fable.Core.TS.Ecma / Fable.Core.TS.Browser
actual   TypeScript.Lib.Es / TypeScript.Lib.Dom
```

After the minimal pipeline routing change, the same focused ownership filter passed with two cases. The fixture assertion then passed, proving the configured root, DOM child policy, and `Fable.Core.TS.Browser.*` cross-family references.

## Golden inspection

- File path: `groups/Fable.Core.TS.fs`.
- Header: `module rec Fable.Core.TS`.
- Children: `module Ecma =` and `[<AutoOpen>] module Browser =`.
- Manifest counts remain exact 3, ergonomic 12, widened 1, escape 4.

## Verification

- `rtk dotnet fsi build.fsx -- test --quick --update --no-run-gate --filter lib-ship-lab` — passed; regenerated only this fixture's files.
- `rtk dotnet fsi build.fsx -- test --quick --no-run-gate --filter lib-ship-lab` — passed, 2 generator tests.
- `rtk dotnet build tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj` — passed, 0 errors and 0 warnings.
- Fresh FsLangMCP project checks for Generator and Generator.Tests — clean, 0 errors and 0 warnings.
- `rtk git diff --check` — clean.

## Commit

`feat(generator): route compiler libs through configured modules`
