# Declaration entry and ownership

Branch: `agents/cloudflare-sdk-bindings`, based on `master` at
`22c0cb2ee7bec6003de516af8e8737414608f8bf`.

Modern SDKs expose separate declaration inputs for public subpaths and environments. Xantham
now accepts an explicit package-relative `entry`, with independent `runtime` and F# `module`
names. Bootstrap and CLI validation reject invalid inputs before starting the compiler. Default
lookup stays at the package root; it no longer selects an arbitrary exported subpath. The schema,
usage guide and architecture record describe the contract.

Shipped declarations now keep their defining group's name and ownership when an application
aliases the same checker type. Transitive core types retain their own provenance, and synthesized
nested declarations follow their parent group and compiler-library family. Secondary aliases
retain the owner of their own export. The hook regression was reproduced with its generated
interface absent from `DeclNames`; two tests failed before the placement fix and passed after it.

A clean core producer can own the synthetic global object after a complete program-source check.
Application declarations and referenced augmentations keep that global object application-owned.
The ES5 ownership profile has a separate compile-gate project because independent compiler-lib
profiles define the same F# modules. Both profiles continue to compile with their consumers.

The combined `esnext`/`dom` producer supersedes the retained split-library snapshots. The solution
now includes the maintained projects, while the old files remain intact. Formatting excludes
generated snapshots, retained experiments and nested worktrees; cleaning visits solution-project
`bin` directories. The [baseline drift record](baseline-drift-2026-09-06.md) documents the three
pre-existing test failures and their bounded corrections. No finding codes were added.

## Validation

The final `dotnet build Xantham.slnx` completed with zero warnings/errors. The full
`dotnet fsi build.fsx -- test` gate passed 600 generator tests, 90 wire tests and 323 Fable/Node
runtime checks. `XANTHAM_REQUIRE_TSC=1` enabled live coverage with the pinned compiler supplied
through `XANTHAM_TSGO_EXE`. The wire executable-layout test intentionally skips under that override;
the generator suite skipped none. The run gate retains its two existing Fable type-test warnings.

The hook correction's narrow run passed 17 ownership and related live fixture tests. An independent
probe also verified the shipped hook and application-owned dotted alias against the rebuilt DLL.

Fresh combined-core output contains 73,424 lines and no producer-module references; it compiles
for `netstandard2.1` and `net8.0` with zero warnings/errors. Workers regeneration against the final
generator preserves its source hash and tier counts: exact 237, ergonomic 1,057, widened 382,
escape 111. The sibling consumer builds without warnings/errors and passes all 13 BAREWire byte
boundary checks through Fable 5.13.0 and Node 25.1.0. BAREWire was clean at
`14e46f6d4023b630c4d4d0f6c773cea019f4d9bc`.

Logs, input hashes, baseline reproductions and the working-tree recovery record are retained at
`/home/hhh/.local/state/clef-dimensional-rescue/checkpoints/2026-09-06-xantham-fsharp-cloudedge/`.

## Consumer and remaining scope

The sibling `FSharp.CloudEdge` directory owns Cloudflare package pins, entry configurations and
the SDK inventory. Its initial Workers target uses `@cloudflare/workers-types` 5.20260901.1,
TypeScript 7.1.0-dev.20260902.1 and Fable.Core 5.2.0. Cloudflare-specific orchestration stays there;
these generator changes use vendor-neutral lab fixtures.

The consumer's BAREWire regression exercises actual generated `Body.bytes()` bindings through
Fable and Node Fetch, including offset views, malformed frames and exact payload consumption.
That check establishes the byte boundary for the initial target. Full SDK generation, compatible
peer profiles and Workerd integration remain further consumer work.

An application-enriched DOM scope can still introduce a core-to-application dependency; the
ownership lab retains its reproduction. General augmentation handling and arbitrary group closure
are unresolved. Explicit entry selection is also separate from conditional npm resolution: a
rootless value entry needs its public `runtime` import configured. NuGet packing/publishing and
cross-host declaration-order stability are outside the checks in this slice.
