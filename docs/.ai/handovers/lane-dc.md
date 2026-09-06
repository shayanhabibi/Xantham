# Lane DC — `NoInfer<T>`, behind a `resolveNoInfer` toggle

Wave fifteen item 4, branch `worktree-gen-wave15-dc`, forked from
`worktree-generator-wave-fifteen` at `a9f2aeb`.

## What changed

TypeScript's `NoInfer<T>` idiom blocks inference through a parameter or property but denotes
exactly `T`. No compiler ships the name — a package that wants it declares its own, the way
`solid-js` does (`type NoInfer<T> = [T][T extends any ? 0 : never]`), and the checker expands
every reference to a plain indexed access before the shaper ever sees a name. The generator had
no handling for this: the idiom landed on `obj` at every site. The support package
(`src/Xantham.Fable.Core/Library.fs`) already carries `type NoInfer<'T> = 'T`, unused until now.

Two behaviors, selected by a new `GeneratorConfig.ResolveNoInfer` field (default `false`):

- **Default (`false`)**: a reference renders as `JS.NoInfer<'T>`, resolving through the support
  package's abbreviation. The generated file still shows the name the `.d.ts` declared.
- **Toggled on (`true`)**: a reference resolves to its bare operand at the mapping site,
  dropping the name entirely.

## Mechanism

The checker expands `NoInfer<T>` to an indexed access before any flag the shaper reads names it
as anything else — the existing `IndexedAccess` branch in `Resolve.fs`'s `deriveFacts` returned
only the object/index type pair, discarding the alias identity the reference was written
through. Recovering that identity is the entire fix:

- **`src/Xantham.Generator/Model.fs`** — `TypeFacts` gained `AliasIdentity: (string * int) option`,
  populated only on the `IndexedAccess` branch, distinct from the existing `AliasTypeArguments`
  (which serves declaration sites, not reference sites, and generalizing it would have touched
  unrelated declarations across the corpus — see "Rejected" below).
- **`src/Xantham.Generator/Resolve.fs`** — the `IndexedAccess` branch now also calls
  `getAliasSymbolOfType`/`getAliasTypeArgumentsOfType`, and records `Some(symbol.Name,
  argument.Id)` when the alias carries exactly one type argument.
- **`src/Xantham.Generator/Shape/Spec.fs`** — `indexedAccessRef` matches `facts.AliasIdentity`
  against `Naming.SupportBindings.shadows` (the same name-collision table that already
  routes `Record`/`ReadonlyRecord`/`PropertyRecord`/`keyof`/`typekeyof` references through the
  support package). A match on `"NoInfer"` resolves per `ctx.Config.ResolveNoInfer`: either the
  bare operand, or `FsApp(Naming.SupportBindings.qualify "NoInfer", [ operand ])`.
- Detection is **name-based, not shape-based** — the `noinfer-lab` fixture's `FirstOf<T> =
  T[0]` is an unrelated indexed access with `AliasIdentity = Some("FirstOf", _)`, which does not
  match `shadows`, and still widens to `obj` with a `TR020` finding, confirming the carve-out
  does not swallow ordinary indexed-access sites.
- `FreeTypeParams.fs` needed no change: it already discovers an `FsTypeVar` nested inside the
  new `FsApp("JS.NoInfer", [...])` wrapper. This is what let solid-js's `createMemo`/`on`
  gain a previously-invisible `'Prev` type parameter — erased to `obj` before, now correctly
  free — with zero changes to that file.
- `Shape/Arity.fs`'s repair pass needed no change either: the fix intercepts resolution before
  an `FsApp("NoInfer", ...)` reference is ever constructed, so the pass never sees an
  unqualified name to repair.

## Config plumbing

- `GeneratorConfig.ResolveNoInfer: bool`, default `false`.
- `GeneratorConfig.loadFile` reads `"resolveNoInfer"` from `xantham.json` via a new `boolField`
  helper.
- `src/Xantham.Cli/Schema.fs` gained the matching `"resolveNoInfer": { "type": "boolean" }`
  entry; `xantham.schema.json` was regenerated via the CLI and diffed — only that one property
  was added.

## Fixtures

- **`tests/fixtures/noinfer-lab/`** — new, hand-authored. `xantham.json` sets
  `"resolveNoInfer": true` (no real-world package's `.d.ts` carries that setting, and
  `solid-js` alone can't exercise the toggled path). `index.d.ts` declares the same
  `NoInfer<T>` idiom `solid-js` uses, a `widen<T>(seed, guard: NoInfer<T>)` function, and an
  `Options<T> { value: NoInfer<T> }` interface, plus the `FirstOf<T>`/`first<T>` negative case
  above.
- **`tests/Xantham.Generator.Tests/Pipeline.test.fs`** — `noInferLab` fixture binding and a
  `fixtureTests "noinfer-lab"` block with two cases: the toggled resolution reads `'T` directly
  (no `TR020` on `value`), and the unrelated indexed access still widens with `TR020` recorded
  against `FirstOf`.
- **`tests/Xantham.Generator.Tests/golden/{solid-js,noinfer-lab}/`** — regenerated via
  `XANTHAM_UPDATE_GOLDEN=1`; diffs read in full before committing.

## Numbers

**`solid-js`** (default setting, no `xantham.json`) — `manifest.json` counts, before → after:
exact 19 → 19, ergonomic 89 → 93 (**+4**), widened 66 → 62 (**−4**), escape 18 → 18. Gross
`TR020` findings: 22 → 3. The 3 remaining are unrelated indexed-access sites
(`SplitProps.Result.Item.Item.[]`, `For.Props.Children(item)`,
`Index.Props.Children(item)()`), confirmed by inspecting `symbols.jsonl` directly. `TR045`
(the conditional-type mechanism) is unaffected: 20 → 20 — the plan's original estimate that
~19 `TR045` sites were also NoInfer-driven did not hold up; that mechanism is untouched by this
change, and the real reduction lands entirely on `TR020`.

**`noinfer-lab`** (toggled setting) — exact 1, ergonomic 1, widened 3, escape 0; gross `TR020`
2 (both against `FirstOf`, the negative case: the type alias and the function that returns it).

**Whole corpus** (derived from the two fixtures' before/after, since nothing else moved):
exact 236 → 237 (+1, `noinfer-lab`'s own declarations), ergonomic 1051 → 1056 (+5: +4
`solid-js`, +1 `noinfer-lab`), widened 385 → 384 (−1: −4 `solid-js`, +3 `noinfer-lab`), escape
111 → 111 (unchanged). No new `Findings.fs` case was needed — resolving `NoInfer<T>` to `T`
loses no information, it recovers information the shaper was discarding.

## Gating

- `dotnet fsi build.fsx -- schema` / `dotnet run --project src/Xantham.Cli -- schema -o
  xantham.schema.json`: regenerated, diff is exactly the one new `resolveNoInfer` property.
- `dotnet fsi build.fsx -- test` (full, no `--quick`, no `--filter`, run gate on): format stage
  reformatted `Resolve.fs` on the first run (fantomas), clean on the re-run; **526/526**
  generator tests, **90/90** wire tests (1 skipped by design), run gate green (`Build
  succeeded`, `Xantham.Generator.CompileGate` compiled — its wildcard `<Compile>` include picks
  up `noinfer-lab`/`solid-js` goldens automatically, no manual line needed). Ran twice; both
  green.
- `mcp__fslangmcp__check` (project scope, `Xantham.Generator`): clean, 0 errors, 0 warnings.
- `mcp__fslangmcp__fcs_refactor_impact` (`kind=signature`) on both changed public records:
  `GeneratorConfig` — 58 sites across 11 files, single project, no cross-project surface
  detected from this workspace scope (the CLI's use of it was verified instead by the full
  gate, which builds `Xantham.Cli` and regenerates the schema successfully).
  `TypeFacts` — 455 sites across 13 files, single project, not on the public API surface.

## Rejected approach

Generalizing `AliasTypeArguments` capture in the `IndexedAccess` branch (rather than adding the
dedicated `AliasIdentity` field) would have also fixed arity loss on unrelated declarations
across the corpus — `array-shape-lab.TupleOf`, and `type-fest`'s `ArrayLength`, `ArrayValues`,
`ExtractRestElement`, `StringLength`, `TaggedUnion`, `GetTagMetadata`, `UnionLength`, `ValueOf`
(confirmed via a `jq` scan of every golden's `TR020` findings under `pass=="shape-aliases"`).
That is a real, separately-scoped fix and was left alone here on purpose — this lane's brief was
`NoInfer` only, not arity recovery for indexed-access aliases in general.

## Left undone

- Two known, accepted residuals, neither chased: `solid-js`'s own local `NoInfer` alias
  declaration (as opposed to its reference sites) still collapses to a non-generic
  `type NoInfer = JS.NoInfer<obj>` abbreviation — the declaration side of the idiom is a
  separate mechanism (`declParamIds`/`freeParamsOf` reading `AliasTypeArguments`) from the
  reference side this lane touched, and is out of scope. `noinfer-lab`'s own `NoInfer` alias
  carries the same residual (visible as a `TR013 TypeParameterOutOfScope` finding on that one
  declaration).
- The arity-loss fix for unrelated indexed-access aliases described above (rejected approach)
  is a legitimate follow-up, scoped wider than this item.
- Corpus-wide `TR020`/`TR045` numbers above are derived from the two fixtures that actually
  moved (`solid-js`, `noinfer-lab`), not from a fresh `git diff` against every golden in the
  corpus — no other fixture's `manifest.json`/`symbols.jsonl` changed at all, confirmed via
  `git status`.

## Standing gap noted, not fixed

`~/.claude/CLAUDE.md` prefixes every git/build/test/file-reading command with `rtk`, as
instructed. Piping `rtk`'s output through a second filter (e.g. `rtk dotnet fsi build.fsx --
test 2>&1 | rtk grep ...`) intermittently produced empty capture files under
`run_in_background`, even after the command reported `exit code 0` — worth a report upstream if
this recurs; worked around here by reading the unfiltered background output directly.
