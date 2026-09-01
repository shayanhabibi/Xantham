# AGENTS.md

## Project Overview

* Purpose: TypeScript-to-F# bindings generator
* Mid-rebuild sitting on top of `Xantham.TypeScript.Wire`
  * .NET client for TypeScript 7 compiler's own API server (`tsc --api`)
  * Generated typed layer reading binary AST
* Coordinated build pipelines through `build.fsx`

## Project Structure

- `src/Xantham.TypeScript.Wire` — the only live source project. Generated API surface, binary AST
  reader, typed node layer, batching mailbox, virtual filesystem. Published to NuGet.
- `tests/Xantham.TypeScript.Wire.Tests` — Expecto suite. No `package.json` of its own: the live
  tests call `Tsc.locate`, which walks parents and finds the root `node_modules`.
- `tests/Test.fsx` — scratch script driving Wire against `tests/fixtures/` via the packed nupkg
  in `bin/`.
- `tools/tsc-ast` — vendors upstream compiler sources and emits the AST/enum F# layers.
- `tools/proto-gen` — emits the protocol F# layers from the `typescript` package's shipped schema.
- `build.fsx` — the current build pipeline (Partas.Build).
- `package.json` — root manifest, tooling only. The single pin of the `typescript` 7.x compiler,
  used both as generation input and as the live `tsc --api` server. Nothing else pins it.
- `.archive/` — dead weight. Never cite, build, or grep it as current. Ask before entering.

## Key Commands

- `dotnet build Xantham.slnx` — build. `dotnet test` — run the Expecto suite.
- `dotnet fsi build.fsx -- <build|test|generate|docs|pack|publish|bump>` — the full pipeline; the
  commands that need the compiler run `npm install` at the repository root first.
- `dotnet fsi build.fsx -- generate [--only ast|proto] [--sync]` — installs the root `typescript`
  pin, then routes to `tools/generate-wire.fsx` for both generated layers with repository defaults.
  `--sync` re-vendors the upstream sources first (network).
- `dotnet fsi tools/generate-wire.fsx sync tsc-ast [--check]` — vendor (or verify) the upstream
  sources pinned in `tools/tsc-ast/upstream.json` into `tools/tsc-ast/upstream/`, against the
  per-file digests in `upstream.lock.json`. The lock is committed; the tree is not.
- `dotnet fsi tools/generate-wire.fsx generate ast` — emit `Enums.generated.fs` (the compiler's
  flag and kind enums), `Ast.generated.fs` (`SyntaxKind`, guards, node-alias guards, `Slot`
  numbers), `AstNode.generated.fs` (named child and data accessors) and `Typed.generated.fs`
  (tags, `Node<'Tag>`, typed accessors, views) into `src/Xantham.TypeScript.Wire/`, from the
  vendored `ast.json` and `enums/`.
- `dotnet fsi tools/generate-wire.fsx generate proto` — emit the `Proto*.generated.fs` files.

## Working in an agent worktree

A worktree under `.claude/worktrees/` has tracked files only — no `node_modules`, no
`tools/tsc-ast/upstream/`. Do not run `npm install` to compensate:

- `build.fsx` and `tools/generate-wire.fsx` borrow the main checkout's install and export
  `XANTHAM_TSGO_EXE`, so the live suite and both generators work unchanged. See
  `.claude/rules/build.md`.
- They also set `XANTHAM_REQUIRE_TSC=1`, which turns a skipped live suite into a failure. **A run
  reporting `native tsc not found - live tests skipped` in a worktree is a broken run, not a
  pass.** Running `dotnet test` directly bypasses this, so set it yourself, or go through
  `dotnet fsi build.fsx -- test`.
- `tools/tsc-ast/upstream/` is vendored per checkout and on demand; `generate ast` tells you the
  command. Never copy it between worktrees. See `.claude/rules/upstream.md`.

## Git — local only, no pushing, no PRs

**This repo iterates locally. Agents do not touch a remote.** This overrides any default harness
guidance about pushing finished work or opening pull requests. `origin` and `speakez` are for
releases the human publishes, not for iteration.

- **Never `git push`, never `gh pr create`.** No branch pushes, no draft PRs, no PR comments, no
  `git fetch`/`pull` unless explicitly asked. Pushing to `speakez` notifies watchers and burns CI
  on states nobody asked to see.
- **Integrate by merging local branches.** Merge the feature branch into the local target branch
  in this checkout. There is no remote round trip and nothing to wait on.
- **Never merge into `master` and never force-push anything.** Merge into the local integration
  branch you were pointed at; if you weren't given one, stop and ask which branch to land on.

### Worktree branches survive; worktrees do not

A worktree under `.claude/worktrees/` shares this checkout's object database and refs. **A commit
made on a branch inside a worktree is already in the main checkout's history the moment it is
made** — deleting or pruning the worktree does not lose it, as long as the branch ref survives.
So:

- Commit before finishing a worktree job. That commit is the handoff; there is no push to back
  it up.
- Do not delete the branch when tearing down a worktree.
- Uncommitted work in a worktree is the one thing that *is* lost. Do not leave it dirty.

### Squash-merge commit messages carry the review

With no PR there is no diff view, no review thread, and no description field — the commit message
is the entire record of the change. A squash-merge message must be written to be read months
later by someone who never saw the branch:

- Subject line in the existing `type(scope): summary` style.
- Then a body covering **why the change was made**, what approach was chosen and what was
  rejected, anything surprising in the implementation, and any follow-up left undone.
- Reference issue numbers (`XANTHAM-nn`, `#nn`) where they apply.
- A one-line body is fine for a genuinely trivial change. It is not fine for a refactor, a
  behaviour change, or anything that took more than one branch to get right.

### Lifting the ban

`git push` and the PR-creating `gh` commands are denied in `.claude/settings.json`. When the human
wants something published they either run it themselves or remove that deny entry. An agent does
not edit the deny list to unblock itself.

## F# semantics — use `fslangmcp`, not grep

The repo ships an `fslangmcp` MCP server (`.mcp.json`, FsLangMCP 0.16.0 over FSAC +
FSharp.Compiler.Service). It loads `Xantham.slnx`, so it sees exactly the two projects the
solution references — `.archive/` is invisible to it, which is the behaviour this repo wants.
Requires the `fslangmcp`, `fsautocomplete` and `fantomas` global tools; `fslangmcp
--bootstrap-tools` installs the pinned set.

**Answer semantic questions about F# code with it. Reach for grep only for prose, JSON, `.mts`
tooling and other non-F# files.** Over 26k lines of `*.generated.fs` sit in `src/`, so textual
search over-matches badly: short binding names like `decode` or `is` recur in thousands of
generated members, and `find` resolves the real symbol instead.

- `find` — definitions and cross-project use sites, each tagged `definition`/`reference` with a
  coverage block. **Run `check` first.** When the workspace does not type-check, `find` returns
  `outcome="not_found"` *with* `coverage.complete: true` — a confidently wrong negative, not the
  indeterminate answer the coverage contract implies. Verified: `find "VirtualFileSystem"` matches
  on a clean tree and reports not_found on the same tree with unrelated compile errors. Never
  conclude "no usages" from a `find` taken while `check` says `errors`.
- `check` — fresh whole-workspace type-check verdict (`clean`/`errors`) with structured
  diagnostics, no build artifacts. Roughly 8s. An incremental `dotnet build Xantham.slnx` is
  about as fast, so prefer `check` for the structured diagnostics, not for speed.
- `fcs_refactor_impact` — run this *before* changing any public signature. Returns blast radius,
  whether the symbol is public API (i.e. a breaking change), covering tests, and a verify list.
- `fcs_tests_for_symbol` — which tests cover a symbol, resolved to the enclosing Expecto
  `testCase` name. Only finds direct call sites; much of the suite exercises the library
  indirectly through the wire, so an empty result means "not called by name", not "untested".
- `fcs_public_api` — stable-ordered public surface, for diffing API before/after a change.
- `fcs_nuget_types` / `fcs_nuget_members` / `fcs_referenced_symbols` — inspect referenced
  assemblies without unpacking packages.

Pass `projectPath` explicitly on `fcs_*` calls when several agents run at once; caches are keyed
per resolved `.fsproj`. `fcs_dead_code` on the Wire project is dominated by generated-file
internals — treat its output as candidates to filter, not a work list.

Serena's symbol tools (`find_symbol`, `find_referencing_symbols`) do **not** work on this repo —
its language server fails on `.archive/` and on `Library.fs`. Use `fslangmcp` instead.

## Architecture Notes

- Nothing is hand-transcribed that can be generated. Facts that must be transcribed are
  catalogued in `docs/wire-hand-written.md` with how each was derived and how to update it.
- The AST is read in place out of the blob; `Node<'Tag>` is a struct over a blob index.
- Expecto for .NET tests.

## TypeScript 7 compiler sources — READ BEFORE RESEARCHING THE COMPILER

**`microsoft/typescript-go` is dead. Never clone it, never cite it, never `gh api` it.**
The Go compiler was merged into `microsoft/TypeScript@main`, and that repository is the only
valid source of truth. The old checkout of it survives only as dead weight under
`.archive/scratch/tmp/tsgo-native`; it is not ground truth, must not be cited, and must not be
recreated anywhere live.

Read every historical mention of "tsgo", "native", or "native-preview" as **TypeScript 7+**:

| Historical name | Current name |
| --- | --- |
| repo `microsoft/typescript-go` | `microsoft/TypeScript`, branch `main` |
| npm `@typescript/native-preview` | npm `typescript` (7.x) |
| npm `@typescript/native-preview-<rid>` | npm `@typescript/typescript-<rid>` |
| executable `tsgo` / `tsgo.exe` | `tsc` / `tsc.exe` |
| Go module `github.com/microsoft/typescript-go` | `github.com/microsoft/TypeScript/tsc` |

Paths moved in the merge. Translate before looking anything up:

| Old path | Current path |
| --- | --- |
| `_scripts/` | `tools/scripts/tsc/` |
| `internal/` | `tsc/internal/` |
| `_packages/native-preview/` | `packages/typescript/` |
| `_tools/gen-proto` | `tools/gen-proto` |

Ground truth, in order of preference:
1. The installed npm package — `node_modules/typescript` at the repository root (currently
   `7.1.0-dev.20260830.1`). This is what our code actually runs against; `dist/` wins over
   upstream `main` whenever the two disagree, because upstream is always ahead of the release.
2. `tools/tsc-ast/upstream/` — vendored at a pinned commit and checksummed in
   `upstream.lock.json`, mirroring the upstream directory layout. Read these rather than
   fetching; `sync tsc-ast --check` says whether the pin has moved. Four directories:
   `tools/scripts/tsc/` (the AST schema `ast.json` and its generators),
   `tsc/internal/api/encoder/encoder.go` (the writer, and the only statement anywhere of the
   binary format — every section is documented at `:72-200`),
   `packages/typescript/src/api/node/node.infrastructure.ts` (the reader's masks), and
   `packages/typescript/src/enums/` (the flag and kind enums, which upstream generates from its
   own Go and commits, plus `syntaxKind.enum.ts` vendored as an oracle on our derived ordinals).
3. `gh api repos/microsoft/TypeScript/contents/<path>` for anything neither shipped in `dist/`
   nor vendored.

`dist/` is not bundled or obfuscated: it is a plain `tsc` build of `packages/typescript/src/`.
Files named `*.generated.*` are emitted from `tools/scripts/tsc/ast.json` (via `schema.ts`) or by
`tools/gen-proto`; treat `ast.json` as the schema of record for AST work rather than scraping
`.d.ts` files.

Known drift to watch: the binary AST `ProtocolVersion` is **8** (`Ast` in
`src/Xantham.TypeScript.Wire/Library.fs` matches). Any note quoting version 7 predates the merge
and is stale.

The AST is consumed through the **typed layer** in `Typed.generated.fs`, not through raw node
indexes. A node is a `Node<'Tag>` - a struct over the blob and an index - and the tags are
generated from `ast.json`: one per node type, one per node alias (`Expression`, `Statement`),
one per token instantiation (`QuestionToken`), plus `AnyNode` for a slot the schema does not
narrow. Tags inherit each other exactly when one's kinds are a subset of the other's, so
`'Tag :> Expression` is the compile-time form of `AstKind.isExpression`. Narrow with the views in
`Patterns` (`[<return: Struct>]` partial active patterns, measured at zero allocation), widen with
`<Alias>.ofNode` - there is one per alias because F# rejects a constraint whose right-hand side is
a type variable. `Slot` and `AstNode` are `internal`; the deliberate escape hatch is `Node.index`,
`Node.file`, `Node.ofIndex` and `Node.retag`. See `docs/wire-navigation.md`.

Child slots are the bit positions of a node's `Children` mask. They come from
`generate-encoder.ts`'s `childProps` — `members` filtered by `isChild() && !noTS && !noGo`, in
declaration order. The `noTS`/`noGo` part is load-bearing: seven function-like nodes declare a
`FullSignature` child mid-list that the encoder skips, so filtering on `isChild()` alone shifts
every slot after it. Use the typed accessors, or `AstNode.<Node>.<child>` / `Slot.<Node>.<Child>`
inside the assembly - never a literal. Slot numbers carry the `astSlot` measure and record offsets
carry `byteOffset`, so the two cannot be swapped.

The `data` word is three things at once, so read it through `Ast`/`AstNode` rather than masking
it by hand. Bits 30-31 pick the shape; bits 24-29 are `commonData` regardless (`Ast.commonData`),
packed as bools one bit each in member order followed by `SyntaxKind` unions at `ceil(log2 n)`
bits, index-encoded so an optional union's `0` means absent — `AstNode.<Node>.<member>` knows the
layout. The low bits are either a child mask, a string index, or an offset to an extended-data
record whose field offsets are hardcoded in `generate-encoder.ts`, not in the schema; those live
hand-written in `Library.fs` behind `Ast.text`/`rawText`/`tokenFlags`/`templateFlags`, keyed by
the generated `AstKind.hasStringText`/`hasExtendedText`. Note literal text is cooked, not source
spelling: `0x2a` reads back as `42`.

`SourceFile`'s extended record is the exception to "`ast.json` is the schema of record": it is
nineteen words of file-level metadata that the schema does not describe at all, stated only in
the format table in `encoder.go`'s header comment. The generator parses that table into
`SourceFileRecord`, so the offsets are generated rather than typed — strictly, because the
authority is prose: a reworded, reordered or resized field, or a table that disagrees with the
`appendUint32s` call below it, fails the run. Read the record through the file-level accessors on
`Ast` (`sourceText`, `fileName`, `path`, `imports`, `referencedFiles`, …), which take no node
index because a blob holds exactly one `SourceFile`.

Eight of those words are byte offsets into the blob's **structured data** section, which is
msgpack. `Msgpack.Reader` in `Library.fs` reads the eleven tags the writer emits and fails on
anything else. An offset of `0xFFFFFFFF` means absent, and an empty collection is written that
way rather than as an empty array, so the two are indistinguishable. Six fields
(`spanMap`, `supplementalSourceFileNames`, `canonicalSourceFileName`, `contentMapper`,
`virtualFileName`, `diagnosticDirectives`) hang off `ContentMapperSourceFileInfo` and exist only
for virtual files, so an ordinary file reads them as absent — including `diagnosticDirectives`,
which is *not* where `@ts-ignore`/`@ts-expect-error` comments go.

Kind ordinals are **not** the JavaScript compiler API's. They are positional in `ast.json`, so
they move whenever a kind is inserted upstream — never hand-write one. `SyntaxKind` in
`Ast.generated.fs` is the only source of truth, and the generator diffs all 351 of them against
upstream's published `syntaxKind.enum.ts` on every run.

Flags are named too. `Enums.generated.fs` carries 20 enums over 448 members, generated from
upstream's own `packages/typescript/src/enums`: `NodeFlags` and `TokenFlags` on the syntax side,
`SymbolFlags`, `TypeFlags`, `ObjectFlags`, `CheckFlags`, `SignatureFlags` and `ElementFlags` on
the checker's responses, and the small ones the `SourceFile` record uses (`ScriptKind`,
`LanguageVariant`, `SpanMap*`). The schema types all of them as bare `number`, so which response
field carries which enum is an explicit table in `tools/proto-gen/generate.mjs`.

Each enum is emitted in two halves: the bits upstream defines are enum cases, and the 122 members
it builds by combining them are `[<Literal>]`s in a companion module of the same name, since an
enum case may not name another case of its own enum. Callers see one prefix either way. The
combining expressions are re-parsed and fully re-parenthesised rather than copied, because F# puts
`|||` and `&&&` at one precedence where TypeScript does not; a test reads all 122 back out of the
built assembly and checks them against the generator's own evaluator.

Facts in this pipeline that are transcribed rather than derived — the per-kind extended-record
offsets, the msgpack tag subset, that field-to-enum table, and ten more — are catalogued in
`docs/wire-hand-written.md`, each with how it was derived and how to update it.
