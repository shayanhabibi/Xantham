# AGENTS.md

## Project Overview

Xantham is a TypeScript-to-F# bindings generator. It is mid-rebuild: the whole project now sits on
top of `Xantham.TypeScript.Wire`, a .NET client for the TypeScript 7 compiler's own API server
(`tsc --api`). Wire speaks the compiler's msgpack protocol and reads its binary AST in place
through a generated typed layer.

The previous design — a Fable extractor crawling the TypeScript 5 JavaScript compiler API, a
common JSON schema as the hand-off point, and a .NET decoder plus generator — has been retired.

## `.archive/` holds obsolete work only — do not read it as current

**Everything under `.archive/` is dead. Do not cite it, build it, grep it for how the project
works today, or copy patterns out of it without first confirming they still apply.** It is kept
because the obstacles it documents are worth consulting deliberately, not because any of it is
live. Read `.archive/README.md` for the inventory.

Archived: `Xantham.Common`, `Xantham.Fable`, `Xantham.Fable.Core`, `Xantham.Fable.Utils`,
`Xantham.Decoder`, `Xantham.Generator`, `Xantham.TypeScript`, `Xantham.Mocha`, their tests and
docs, the superseded plans (including the `tsgo-*` route documents), the old FAKE/EasyBuild
pipeline (`Build.fsproj`, `ci/`), the npm package manifest and `index.js`, the retired
`.claude/rules/`, and untracked scratch under `.archive/scratch/`.

Two archived traps in particular: `.archive/docs/plans/tsgo-fsharp-client.md` and
`tsgo-native-route.md` predate the compiler merge and use stale names throughout — translate via
the table below first. `.archive/scratch/tmp/tsgo-native` is a checkout of the dead
`microsoft/typescript-go` repository and is not ground truth for anything.

## Project Structure

- `src/Xantham.TypeScript.Wire` — the only live source project. Generated API surface, binary AST
  reader, typed node layer, batching mailbox, virtual filesystem. Published to NuGet.
- `tests/Xantham.TypeScript.Wire.Tests` — Expecto suite. Has its own `package.json` /
  `node_modules` pinning the `typescript` 7.x package used as ground truth and as the live server.
- `tests/Test.fsx` — scratch script driving Wire against `tests/fixtures/` via the packed nupkg
  in `bin/`.
- `tools/tsc-ast` — vendors upstream compiler sources and emits the AST/enum F# layers.
- `tools/proto-gen` — emits the protocol F# layers from the `typescript` package's shipped schema.
- `build.fsx` — the current build pipeline (Partas.Build). There is no root `package.json`.

## Key Commands

- `dotnet build Xantham.slnx` — build. `dotnet test` — run the Expecto suite.
- `dotnet fsi build.fsx -- <build|test|docs|pack|publish|bump>` — the full pipeline; `test` runs
  `npm install` in the Wire test project first.
- `dotnet fsi tools/generate-wire.fsx sync tsc-ast [--check]` — vendor (or verify) the upstream
  sources pinned in `tools/tsc-ast/upstream.json` into `tools/tsc-ast/upstream/`, against the
  per-file digests in `upstream.lock.json`. The lock is committed; the tree is not.
- `dotnet fsi tools/generate-wire.fsx generate ast` — emit `Enums.generated.fs` (the compiler's
  flag and kind enums), `Ast.generated.fs` (`SyntaxKind`, guards, node-alias guards, `Slot`
  numbers), `AstNode.generated.fs` (named child and data accessors) and `Typed.generated.fs`
  (tags, `Node<'Tag>`, typed accessors, views) into `src/Xantham.TypeScript.Wire/`, from the
  vendored `ast.json` and `enums/`.
- `dotnet fsi tools/generate-wire.fsx generate proto` — emit the `Proto*.generated.fs` files.

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
1. The installed npm package — `tests/Xantham.TypeScript.Wire.Tests/node_modules/typescript` (currently
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
