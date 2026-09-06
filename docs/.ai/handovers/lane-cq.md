# Lane CQ - a CLI-only warning for a shadowed global type library

## The detection rule, in one sentence

`Grouping.classify` reads only a symbol's *first* declaration handle, so a global symbol whose
first declaration is the compiler's default DOM lib is filed away as `CompilerLib` even when a
later handle in the same merged declaration list sits under the entry package's own directory;
`Harvest.harvestGlobals` now counts exactly those symbols (`HarvestModel.ShadowedByLib`), and
`Xantham.Cli.Program.libShadowWarning` prints one line to stderr when that count reaches 20 and
no `lib` override is configured.

## Mechanism

`Grouping.declaresUnderPackage` (new, `src/Xantham.Generator/Model.fs`) scans a symbol's *entire*
`Declarations` array (not just the first handle `classify` reads) for one whose file path sits
under the package directory. `harvestGlobals` (`src/Xantham.Generator/Harvest.fs`) counts global
symbols where `classify <> EntryPackage` (excluded from `Exports`, so silently lost) yet
`declaresUnderPackage` is true (the package still contributed a declaration to that same merged
symbol) - the exact declaration-merging loss lane CI described. The count is computed from data
`harvestGlobals` already fetches: no new wire round-trip. It is always zero for module-based
packages, since `harvestGlobals` never runs once `harvestExports` has populated `Exports` - this
is what scopes the diagnostic to global-type-library-shaped packages without a package name list.

The count is threaded, read-only, through `HarvestModel -> RenderModel -> RunReport`
(`ShadowedByLib: int`, `Pipeline.toRender`/`Pipeline.run`) into `Program.fs`'s `emit`, which
prints the warning to `err` alongside the rest of the run summary - so it is already gated by the
existing `if not options.Quiet` block and needs no separate suppression path.

## Threshold: 20, absolute, not proportional

Chosen absolute rather than proportional because `Exports` is precisely the complement of the
shadowed set - "shadowed / total exports" isn't a well-defined ratio from harvest data alone (the
denominator the shadowed symbols would need is exactly what got lost). 20 sits well under the
observed `@cloudflare/workers-types` count and comfortably above what an intentional
one-or-two-interface global augmentation (a legitimate, common TypeScript pattern) would ever
reach. This cannot distinguish "one package augments `Array.prototype` on purpose" from "a
distinct type happens to share one DOM name" at low counts - it is a volume heuristic, not a
proof of any specific symbol's origin, and is not intended to fire on every case lane CI's
concern describes, only on the large-volume shape that made `@cloudflare/workers-types` lose most
of its own output.

## Verified

- `@cloudflare/workers-types` (`tests/fixtures/@cloudflare/workers-types/node_modules/@cloudflare/workers-types`,
  no `--config`): fires - `ShadowedByLib = 53`, warning printed to stderr, exit 0.
- `animejs`, `solid-js`, `type-fest` (same npm-installed fixtures, no `--config`): silent -
  `grep "default-lib declaration"` finds nothing on stderr for any of the three.
- New hand fixture `tests/fixtures/dom-shadow-lab/` (20 global interfaces, each reusing a DOM lib
  name and adding one member via ordinary declaration merging): fires, `ShadowedByLib >= 20`.
- `globals-lab` (existing hand fixture, global script, no DOM-name collisions) and `lab` (existing
  module-based fixture): silent.
- `--quiet` against `dom-shadow-lab`: stderr is empty (warning suppressed along with the rest of
  the summary).
- `--config` pointing at `{ "lib": ["esnext"] }` against `dom-shadow-lab`: silent (no default lib
  loaded, nothing shadowed) - the remedy the warning names actually works.

My count (53) is lower than lane CI's ~154-class estimate for `@cloudflare/workers-types`; the
two are different units (my count is global *symbols* whose declaration is lost, lane CI's was a
line-count-driven estimate of classes), not a contradiction - both point at the same mechanism and
both clear the threshold by a wide margin.

## Note - a small, flagged edit outside `src/Xantham.Cli/`

The brief scopes this lane to `src/Xantham.Cli/` and permits a minimal, flagged accessor addition
elsewhere if genuinely needed. `ShadowedByLib` could not be computed from anything the CLI already
had, so I added the field to three generator records and one pure function:

- `src/Xantham.Generator/Model.fs`: `HarvestModel.ShadowedByLib: int`, `RenderModel.ShadowedByLib: int`,
  `RunReport.ShadowedByLib: int`, and `Grouping.declaresUnderPackage`.
- `src/Xantham.Generator/Harvest.fs`: `harvestGlobals` computes and threads the count.
- `src/Xantham.Generator/Pipeline.fs`: `toRender` and `run` carry the count forward.

No generator emission changed - the new field is populated, read, and threaded, never branched on
inside the generator. No default changed (`Lib`'s default is untouched). `src/Xantham.Generator/Findings.fs`
was not touched, and nothing under `src/Xantham.Generator/Shape/` was touched.

Adding the field broke the build in two **test** files that construct `HarvestModel`/`RenderModel`
by record literal rather than through `.Empty`/`with`: `tests/Xantham.Generator.Tests/Shape.test.fs`
(20 sites) and `tests/Xantham.Generator.Tests/Render.test.fs` (1 site). These are test files, not
under `src/Xantham.Generator/Shape/`, so fixing them (adding `ShadowedByLib = 0`, or wiring the
`baseModel` field) was in scope and necessary to keep the tree building; no assertion in either
file changed.

## Gate

Fast loop first (`--quick --update --no-run-gate --filter "dom-shadow-lab"`, then `--filter
"generator cli"`), both green, before the full gate:

```
dotnet fsi build.fsx -- test
```

- Generator tests: 498 passed, 0 failed, 0 skipped (490 baseline + 8 new: 2 for the
  `dom-shadow-lab` pipeline fixture, plus 6 CLI-level cases: fires on `dom-shadow-lab`, silent on
  `globals-lab`, silent on `lab`, silent under `--quiet`, silent under a `--config` override, and
  the fixture's own pre-existing determinism test).
- Wire tests: 90 passed, 0 failed, 1 skipped (unchanged - platform without `tsc`).
- Run gate: 309 checks passed (unchanged). Exit 0.

`dotnet fsi build.fsx -- findings`: summed across every fixture, exact/ergonomic/widened/escape
moved from 535/1603/797/200 to 535/1603/797/**201**. The single extra `escape` is `HG003`
(`NothingHarvested`) fired by the new `dom-shadow-lab` fixture itself, whose entire point is that
every declared symbol merges away - summing every fixture *except* `dom-shadow-lab` reproduces
535/1603/797/200 exactly. No existing fixture's findings moved.

## Diff

```
git diff --stat 5c827c4
 src/Xantham.Cli/Program.fs                              | +warning + threshold + emit wiring
 src/Xantham.Generator/Model.fs                          | +ShadowedByLib fields, +declaresUnderPackage
 src/Xantham.Generator/Harvest.fs                        | +shadow count in harvestGlobals
 src/Xantham.Generator/Pipeline.fs                       | +field threading in toRender/run
 tests/Xantham.Generator.Tests/Cli.test.fs               | +6 CLI-level tests
 tests/Xantham.Generator.Tests/Pipeline.test.fs          | +1 fixture registration
 tests/Xantham.Generator.Tests/Render.test.fs            | +1 field on baseModel
 tests/Xantham.Generator.Tests/Shape.test.fs             | +field on 20 literal HarvestModel sites
 tests/fixtures/dom-shadow-lab/{package.json,index.d.ts} | new hand fixture
 tests/Xantham.Generator.Tests/golden/dom-shadow-lab/*   | new golden (12-line binding + manifest + symbols.jsonl)
```

Branch: `worktree-gen-wave14-cq`, forked at `5c827c4`. Committing at the head of this branch.

## Anything unexplained

Nothing outstanding. The one open judgement call is the threshold value (20) itself, covered
above - it is a volume cutoff, not a certainty, and the handover flags that explicitly rather than
presenting it as a proof.
