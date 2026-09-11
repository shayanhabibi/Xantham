# Export module Task 1 status

The acceptance fixture is `tests/fixtures/export-layout-lab`, whose package name and runtime
specifier are both `layout-lab`; it is a global script containing the entry ambient module,
`layout-lab/strict`, `layout-lab/aliases`, and the global `sharedFlag`. Its focused Pipeline
regression is registered after `Pipeline.test.fs` under these Expecto selectors:

- `export module layout regression/values are nested under their public module owners`
- `export module layout regression/same-owner overloads and incompatible candidates all remain callable`
- `export module layout regression/type-only aliases do not create runtime members`

The old-layout baseline generated one root `Exports` container. It retained only the entry
`check` and `echo`, retained both `mode` declarations in the same container (an F# collision),
and recorded `DO004` drops for the strict `check`, strict `echo`, and the return-only `pick`
candidate. The focused regression therefore checks owner-qualified containers, exact import
selectors/specifiers, one occurrence per owner, two legal `convert` overloads, distinct F# names
for both `pick` and collapsed-literal `dispatch`, and exclusion of `typeOnlyCheck`.
The independent compile-gate baseline (`rtk dotnet build
tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj --no-restore`) fails with
FS0438 at `golden/layout-lab/LayoutLab.fs` lines 31 and 41: both ambient owners emitted
`get_mode` into `LayoutLab.Exports`.

The local Node input is `node_modules/@types/node` version `22.20.2`. The generated project
`src/Xantham.Fable.Node` uses `xantham.json` with `{ "lib": [ "node" ] }`; generation uses the
repository compiler pin `typescript` `7.1.0-dev.20260902.1`. The existing generated binding shows
the minimal owner collision `AssertionError.captureStackTrace` imported from both `assert` and
`assert/strict` (also from their `node:` aliases) in one root value surface. Regenerating and
compiling Node remain Task 6 work; the deterministic lab is the independent acceptance input.

Fresh verification after the shared model migration:

- `fslangmcp check`, explicitly scoped to `tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj`: clean, zero errors and warnings.
- `rtk dotnet build tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore`: succeeded, zero errors and warnings.
- `rtk dotnet run --project tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-build -- --filter-test-list "export module layout regression"`: intended red, three tests discovered; type-only passed, owner layout failed because only `Exports` was emitted, and overload retention failed because only one `pick` import survived. No compiler skip or fixture-discovery failure occurred.

The fixture does not encode impossible TypeScript declaration combinations; property/method
synthetic-model coverage remains assigned to the final collision tests now that the shared model
contract is frozen.
