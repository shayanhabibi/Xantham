# TypeScript 7.1.0 beta and Fickl handover

Updated 2026-10-22 after resuming from the computer restart. Implementation and local
validation are complete. The user explicitly added migration to Fickl to this PR's scope,
then requested committing and opening the PR against the repository's integration branch.

## Working location

- Worktree: `C:/Users/shaya/RiderProjects/Xantham/.claude/worktrees/ts-7.1.0-beta`.
- Branch: `chore/typescript-7.1.0-beta`; migration checkpoint `929f0a10`, based on `594f1041`.
- Prior commits: `5a6fe62c` (Wire), `ed690106` (generator), `9c1737b9` (migration),
  `929f0a10` (formatting).
- Main checkout remains on the unrelated `feat/tagged-inline-interface` branch; preserve its
  `.serena/project.yml` edit. This beta branch uses Fable.Core 5.2.0, not that branch's fork.
- PR target: `develop`, following `.github/branch-workflow.md`. Retire this handover into
  the phase record on merge. GitHub CI remains the integration check.

## Follow-up fixes

TypeScript 7.1 changed filesystem callbacks to tagged JSON responses. Bare Boolean replies
terminated the server before the mailbox disposal test could reach its read callback.
`VirtualFileSystem.callbacks` now emits value, missing and useOS replies, and acknowledges
writes with a value tag. The contract comes from the installed compiler's
`dist/api/fsCallbacks.js`; all six existing adapters and documentation were updated.
Five updated encoding tests failed before the production change; all nine virtual-filesystem
tests and eleven mailbox tests passed afterward.

The Infinity mailbox test now checks supported non-finite types alongside ordinary batched
results. Pure batch tests retain rejected-batch replay coverage. Two more stale expectations
were corrected: node-handle parsing derives Parameter's ordinal from the generated enum;
session coverage puts createSnapshot on Sessionless and updateSnapshot on Session.

Fickl 0.5.1 (Fable 5.23.0) replaces the Fable 5.17.2 tool in the root and standalone support
repro manifests. Build/runtime commands, Partas gates, build checks and current pin notes use
Fickl. Historical compiler measurements retain their original versions. Fable.Core stays 5.2.0.

## Verification

- `rtk dotnet fsi build.fsx -- test --run-gate`: solution build passed; all 1,489 generator
  tests passed. It then found the two stale Wire expectations described above, so that
  invocation stopped before the runtime gates.
- After those test fixes, the complete rebuilt Wire executable passed 105/105 tests with
  zero skips. Log: `tests/.scratch/beta-wire-all-20261022.log`.
- With Fickl pinned and restored,
  `rtk proxy dotnet fsi build.fsx -- test --quick --filter session --run-gate` passed:
  11 selected Wire tests, all 564 runtime checks, Partas JSX acceptance, direct getters/setters
  from source and DLL, and the Partas Vitest check.
  Log: `tests/.scratch/beta-fickl-gates-20261022.log`.
- `rtk proxy node tools/verify-build.mjs` passed all pipeline checks, including the Fickl
  command. Log: `tests/.scratch/beta-build-checks-20261022.log`.
- Standalone support-helper repro restored and compiled with its Fickl pin; all five Node
  tests passed.
- Fantomas check passed for build.fsx and Library.fs; the changed test files are intentionally
  ignored by the repository's formatter configuration. `git diff --check` passed.
- Read-only subagent review found no blocking issues. It identified the standalone repro pin,
  which was subsequently migrated.

The generator suite was not repeated after changes confined to Wire test expectations and
JavaScript compiler tooling. Runtime gates were rerun with the new compiler. The main Fickl
compilation phase reported 2,185 ms versus the earlier Fable run's 7,071 ms; entire filtered
runtime pipelines were 81,592 ms versus 234,857 ms. These are observations, not controlled
benchmarks.

Existing warnings include the site's FSharp.Compiler.Service/FSharp.Core NU1608 constraint,
generated reserved-identifier warnings on a clean build, the two deliberate Fable workaround
type-test warnings, and an ES-module inference warning for export-layout-lab.

## Operational notes

Prefix commands with `rtk`; use `rtk proxy` when live log output is useful. The normal RTK
dotnet filter buffers output until exit. Scratch logs belong under `tests/.scratch/`.
After a Fable/Fickl run, run `rtk dotnet restore Xantham.slnx -v q` before a quick solution
build: compilation can leave the support project's assets restored for netstandard2.1 only.

F# MCP tools were unavailable after the restart. Before restart, set_project against the beta
Wire test project and check returned clean. Prefer those tools if available again; this
session used direct relevant reads and actual builds/tests.

The worktree owns its TypeScript 7.1.0-beta install. The build exports that compiler as
XANTHAM_TSGO_EXE with XANTHAM_REQUIRE_TSC=1. The compiler package revision is
`3e9236361fa97a9e6c685f4833ac457ae2d8c50a`, binary AST protocol 9.

For earlier migration detail, read the TypeScript 7.1.0-beta phase record in
`docs/.ai/plans/generator-architecture.md`. Claude's original history is
`C:/Users/shaya/.claude/projects/C--Users-shaya-RiderProjects-Xantham/acc391c2-f6b6-48bb-8ca0-623d19bac11d.jsonl`;
its final substantive message is dated 2026-10-21T06:16:53.659Z, followed by the detailed
compaction summary. Extract text blocks rather than dumping tool metadata.
