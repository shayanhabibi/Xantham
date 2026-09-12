# Lane B handover — 0.1.0 wave (#67, #66; #68 cancelled)

Branch `wave/lane-b`, merged into `develop` at `db72863`'s parent merge.

## #67 OS-agnostic manifest paths (4473467)
- `Render.fs` `sourceFile`: `node_modules/@typescript/typescript-<platform>-<arch>/` rewrites to `node_modules/typescript/`. Test in `Pipeline.test.fs` on `lib-ship-lab` symbols. Goldens `lib-ship-lab` and `compiler-lib-ownership-lab` `symbols.jsonl` moved in the `file` field only (142 lines each).

## #66 declaration-catalog suite (5369a21, db72863)
- Six bindings guarded by `Tsc.locate __SOURCE_DIRECTORY__` with the pipeline suites' skip message; consumer project gained the `Xantham.Fable.Core.TS` reference the export-provenance test already used.
- `tools/workspace.fsx` now exports `XANTHAM_TSGO_EXE` from any checkout (was worktree-only), which is what let the suite run in the main checkout.
- Real defect found: 22/38 cases error `declaration catalog: Exports has no stable declaration or parent role` across the lists `declaration catalog`, `callable signatures`, `source closure`, `anonymous literal unions`. Those four stay disabled behind a TODO; `private nullable aliases` and `generic nullable aliases` run (16 cases).

## #68 solid-js ordering on Linux
- Recon cancelled by the user before any finding. The two `skiptest` lines in `Pipeline.test.fs` remain.

## Environment note
- Under `dotnet test`, nested `dotnet build` calls inside tests stalled with idle MSBuild nodes holding the test host's stdout (3/3 on lane-b). `build.fsx` now runs the Expecto executables directly and the nested builds carry `global.json` (the temp dir was outside it, making the 11.0 RC SDK eligible). Full gate: ~2 min, no stall.
