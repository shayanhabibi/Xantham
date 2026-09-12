# AGENTS.md

Xantham is a TypeScript-to-F# bindings generator, mid-rebuild on top of `Xantham.TypeScript.Wire`
— a .NET client for TypeScript 7's own `tsc --api` compiler server that decodes its binary AST
into a typed layer. Generated bindings target **Fable 5.x only**: if a mapping needs something
Fable 5 provides, use it; a lossy mapping is lossy because of F# or a missing binding, never
because of the Fable version.

Consumer documentation lives under `docs/`; AI/agent notes live under `docs/.ai/`.

## Rules files

- `.claude/rules/build.md` — `build.fsx`, `tools/*.fsx`: the stage/input DSL, worktrees borrowing
  the main checkout's compiler install.
- `.claude/rules/comments.md` — repo-wide: the doc-comment style contract.
- `.claude/rules/generator-fixtures.md` — the generator, its tests, `tests/fixtures/`:
  small-lab-first workflow, the fast test loop, which generator lists are append-only.
- `.claude/rules/style.md` — `**/*.fs`: Fantomas formatting defaults and exceptions.
- `.claude/rules/upstream.md` — `tools/tsc-ast/**`: vendoring upstream TypeScript sources.

## Compile-gate dependency pin

A compile gate is only evidence if it compiles against what a consumer will: `Fable.Core` 5.2.0
(`src/Xantham.Fable.Core`, `tests/Xantham.Generator.CompileGate`), `Xantham.Fable.Core.TS`
(generated from the root compiler pin, referenced by every consumer compile gate), and the
`typescript` 7.x pin in `package.json`, which supplies both the producer's `lib.*.d.ts` and the
live `tsc --api` server.

## Project structure

- `src/Xantham.TypeScript.Wire` — the compiler client: API surface, binary AST reader, typed
  node layer, batching mailbox, virtual filesystem. Published to NuGet.
- `src/Xantham.Generator` — Harvest → Resolve → Shape → Render, one linear pass per tier
  sequenced by `Pipeline.fs`; emits a binding file plus a `manifest.json` of per-symbol findings.
- `src/Xantham.Cli` — the `xantham` .NET tool; `generate` shells over `Pipeline.run`, `schema`
  emits `xantham.schema.json`.
- `src/Xantham.Fable.Core` / `src/Xantham.Fable.Core.TS` — hand-written support library, and the
  generated ECMAScript/DOM bindings consumers open alongside it.
- `tests/Xantham.Generator.Tests` — Expecto suite including the golden corpus under `golden/`
  (`XANTHAM_UPDATE_GOLDEN=1` rewrites it — read the diff before committing it).
- `tests/Xantham.Generator.CompileGate` / `tests/Xantham.Generator.RunGate` — not test projects:
  compile the goldens, resp. Fable-run behavioural checks against a subset, on every build.
- `tests/fixtures` — real `.d.ts` packages the generator runs against, pinned in `pins.json` and
  installed by `tools/xantham-fixtures.fsx`.
- `tools/tsc-ast`, `tools/proto-gen`, `tools/session-gen` — vendor upstream sources and emit the
  AST/enum, protocol and session F# layers.
- `build.fsx` — the build pipeline (Partas.Build).

## Key commands

- `dotnet build Xantham.slnx` — build. `dotnet test` — run the Expecto suite.
- `dotnet fsi build.fsx -- <build|test|generate|docs|pack|publish|bump>` — full pipeline.
- `dotnet fsi build.fsx -- generate [--only ast|proto|session|compiler-lib|schema] [--sync]` —
  routes to `tools/generate-wire.fsx`; `--sync` re-vendors upstream sources first (network).
- `dotnet run --project src/Xantham.Cli -- generate <package-dir> [-o <dir>] [--config <path>]` —
  generate a binding outside the test harness.
- `dotnet fsi tools/xantham-fixtures.fsx -- init` — install `tests/fixtures/` from `pins.json`;
  also runs as a stage of `build.fsx -- test|pack|publish`.

An agent worktree under `.claude/worktrees/` carries tracked files only; `build.fsx` borrows the
main checkout's compiler install rather than reinstalling (`.claude/rules/build.md`), while
fixtures are expected to be downloaded fresh there (`.claude/rules/generator-fixtures.md`).

## F# semantics — use `fslangmcp`, not grep

The repo ships an `fslangmcp` MCP server (`.mcp.json`) loaded against `Xantham.slnx`. **Answer
semantic questions about F# code with it; reach for grep only for prose, JSON, `.mts` tooling and
other non-F# files.** Over 26k lines of `*.generated.fs` sit in `src/`, so textual search
over-matches badly — short names like `decode` or `is` recur in thousands of generated members.

- `find` — definitions and cross-project use sites. **Run `check` first**: when the workspace does
  not type-check, `find` can return `outcome="not_found"` with `coverage.complete: true` — a
  confidently wrong negative. Never trust a `find` taken while `check` reports errors.
- `check` — fresh whole-workspace type-check verdict with structured diagnostics.
- `fcs_refactor_impact` — run before changing any public signature: blast radius, whether the
  symbol is public API, covering tests.
- `fcs_tests_for_symbol` — tests by direct call site; an empty result means "not called by name",
  not "untested" (much of the suite exercises the library through the wire).
- `fcs_public_api`, `fcs_nuget_types`/`fcs_nuget_members`/`fcs_referenced_symbols` — public surface
  diffing and inspecting referenced assemblies without unpacking packages.

Pass `projectPath` explicitly on `fcs_*` calls when several agents run at once. `fcs_dead_code` is
dominated by generated-file internals on the Wire project — treat it as candidates, not a list.

## Architecture notes

- Nothing hand-transcribed is generated: facts transcribed by a person are catalogued in
  `docs/wire-hand-written.md`, with how each was derived and how to update it.
- The generator is nano-passes over accumulating per-tier records in linear lists — source order
  is execution order. `docs/.ai/plans/generator-architecture.md` carries the phase decisions
  (O1–O7); `generator-type-mapping.md` carries the per-construct mapping. Update the relevant
  phase record in the same commit as a behaviour change.
- Findings are graded `Exact | Ergonomic | Widened | Escape` per symbol into `manifest.json`, so a
  lossy mapping says so. Each is a case in a union per pass in `Findings.fs`; a manifest key (e.g.
  `SI001`) is the union's prefix plus case position, so **unions are append-only** —
  `Findings.test.fs` snapshots the table.
- `AST` is read in place out of one blob; `Node<'Tag>` is a struct over a blob index, accessed
  through the typed layer, never raw indices — see `docs/wire-navigation.md` for the map.
- `tests/Xantham.Generator.CompileGate` is deliberately not an Expecto test: it is a plain project,
  so it runs on every build rather than only under `dotnet test`.

## TypeScript 7 compiler sources — read before researching the compiler

**`microsoft/typescript-go` is dead. Never clone it, never cite it.** The Go compiler merged into
`microsoft/TypeScript@main`, the only source of truth. Translate every historical "tsgo" /
"native-preview" name to its **TypeScript 7+** equivalent: repo `microsoft/typescript-go` →
`microsoft/TypeScript`; npm `@typescript/native-preview[-<rid>]` → `typescript` (7.x) /
`@typescript/typescript-<rid>`; executable `tsgo` → `tsc`; `_scripts/`, `internal/`,
`_packages/native-preview/`, `_tools/gen-proto` → `tools/scripts/tsc/`, `tsc/internal/`,
`packages/typescript/`, `tools/gen-proto`.

Ground truth, in order of preference: (1) `node_modules/typescript` at the repository root — what
our code runs against; `dist/` wins over upstream `main` when they disagree; (2)
`tools/tsc-ast/upstream/`, vendored at the pinned, checksummed commit (`sync tsc-ast --check`
reports drift); (3) `gh api repos/microsoft/TypeScript/contents/<path>` for the rest.

Known drift: the binary AST `ProtocolVersion` is **8** (`Ast` in `Library.fs`) — a note quoting
version 7 predates the merge. Kind ordinals are positional in `ast.json`, not the JavaScript
compiler API's — never hand-write one; `SyntaxKind` in `Ast.generated.fs` is diffed against
upstream's `syntaxKind.enum.ts` on every generator run.
