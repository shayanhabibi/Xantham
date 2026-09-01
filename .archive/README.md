# .archive — obsolete work only

**Nothing in this directory is live. Do not read it to answer questions about how Xantham
works today, do not cite it, do not build it, and do not copy patterns out of it without
first confirming they still apply.**

Xantham is being rebuilt around `src/Xantham.TypeScript.Wire`, which talks to the TypeScript 7
compiler's own API server (`tsc --api`) and reads its binary AST directly. The previous design —
a Fable extractor crawling the TypeScript 5 JavaScript compiler API, a JSON schema hand-off, and a
.NET decoder plus generator — has been retired wholesale. Its code, docs, plans and build system
are parked here rather than deleted, because the obstacles they document are still worth
consulting deliberately.

Treat this directory the way you would treat `git log`: a record, not a source of truth.

## What is here

| Path | What it was |
| --- | --- |
| `src/Xantham.Common` | Shared discriminated-union schema (`Common.Types.fs`) — the extractor/generator contract. |
| `src/Xantham.Fable`, `src/Xantham.Fable.Core`, `src/Xantham.Fable.Utils` | TypeScript extractor compiled to JS via Fable, and its bindings stubs. |
| `src/Xantham.Decoder` | .NET library decoding the JSON schema into F# structures. |
| `src/Xantham.Generator` | Rendering library — path system, type-ref model, F# AST helpers. |
| `src/Xantham.TypeScript`, `src/Xantham.Mocha` | Earlier TypeScript bindings and Mocha test-harness bindings. |
| `tests/` | `Xantham.Fable.Tests`, `Xantham.Generator.Tests`, `Xantham.EndToEnd`, and the standalone Fable `.fsx` test scripts. |
| `docs/` | `Overview/`, and the per-module docs for Decoder, Fable and Generator. |
| `docs/plans/` | Superseded plans, including the `tsgo-*` route documents that predate the merge of the Go compiler into `microsoft/TypeScript`. |
| `build/` | The old FAKE/EasyBuild pipeline (`Build.fsproj`, `ci/`), the `regressions_test.yml` workflow, the npm package manifest and `index.js` CLI entry point. |
| `claude-rules/` | Agent rules for the retired modules, formerly `.claude/rules/`. |
| `scratch/` | Probe checkouts, build output, logs and other untracked debris. Git-ignored; delete freely. |

## Two traps in particular

`docs/plans/tsgo-fsharp-client.md` and `docs/plans/tsgo-native-route.md` were written while
`microsoft/typescript-go` was still a separate repository. That repository is dead and the names
in those documents are all stale — see the translation table in the root `AGENTS.md` before acting
on anything they say.

`scratch/tmp/tsgo-native` is a checkout of that dead repository. It is not ground truth for the
compiler and must not be treated as such; the current sources are vendored under
`tools/tsc-ast/upstream/`.
