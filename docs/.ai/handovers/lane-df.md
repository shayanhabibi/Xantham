---
category: Generator
audience: managing agent
title: Lane DF — the compiler-lib group ships end to end, and the real path does not match the workaround
---

# Lane DF — wave fifteen item 1f

Branch `worktree-gen-wave15-df`, forked from `worktree-generator-wave-fifteen` (dispatched after
lane DD). No `.scratch/` left in the tree; every scratch package used below was created under
this worktree's `.scratch/` (to reach `node_modules` through the ordinary parent walk) and
deleted before this commit.

## What changed

`Harvest.harvestGlobals` (`src/Xantham.Generator/Harvest.fs`) admitted only symbols whose
`Grouping.classify` returned `EntryPackage`. Widened to a shared `admits` predicate: `origin =
EntryPackage || (origin = CompilerLib && GeneratorConfig.disposition ctx.Config CompilerLib =
Ship)`, used at all three filter sites in that pass — `ours` (what gets harvested), `shadowedByLib`
(now excludes symbols the widened `ours` already admits, so a shipped name is no longer double-
counted as "lost"), and the `namespaces` filter feeding `namespacesAmong`. `harvestExports` (the
module-based path) is untouched — out of scope per the dispatch note, and not exercised by an
ambient global-script entry like the one this changes.

`Model.fs`'s `HarvestModel.ShadowedByLib` doc comment gained one clause: the count excludes names
the compiler-lib group ships, since those are harvested rather than shadowed once `"typescript/lib":
"ship"` is set. No field, case, or finding changed shape; `Grouping.classify` itself is untouched,
per the brief.

No new finding case needed — this is a filter widening, not a new diagnostic. `FindingCodes.table`
and `Findings.test.fs` are untouched.

## Fixture: `tests/fixtures/lib-ship-lab`

Hand-authored, tracked. `index.d.ts` is a bare global script — no import, no export, no
declaration of its own. `xantham.json` sets `"lib": ["scripthost"]` (compiler's default lib set
replaced with just that one file, 320 lines, plain interfaces and `declare var`s) and `"groups":
{"typescript/lib": "ship"}`. Registered in `Pipeline.test.fs` right after `lib-reference-lab`,
via `handFixture`/`handConfig`. Golden: `exact 3 ergonomic 12 widened 1 escape 4`, 26 files
(`groups/TypeScript.Lib.fs` plus the — otherwise-empty — entry module). One test beyond the
golden-match test, asserting the group file renders real scripthost interfaces
(`ActiveXObject`, `TextStreamReader`) and that `HG003`/`NothingHarvested` never fires.

Before this fixture existed, pointing the same configuration at the CLI directly (`.scratch/`,
not a fixture) produced exactly this: `ScripthostCheck.fs` (the empty entry module) plus
`groups/TypeScript.Lib.fs` with real scripthost interfaces, `exact 3 ergonomic 12 widened 1
escape 4` — confirmed once by hand before the fixture was written, byte-for-byte what the fixture
now pins.

## Tier movement, gross, both directions

**Zero outside the new fixture.** `git status`/`git diff --stat` after `--update` touch only
`tests/Xantham.Generator.Tests/golden/lib-ship-lab/**` (new) and `tests/fixtures/lib-ship-lab/**`
(new); no existing golden changed. `admits` is stricter than a blanket widening — it only
changes behaviour when `GeneratorConfig.disposition ctx.Config CompilerLib = Ship`, and no
existing fixture sets that, so every fixture's `ours`/`shadowedByLib`/`namespaces` computation
takes the exact same branch it did before. `lib-ship-lab` is new, so its golden is an
introduction (`exact 3 / ergonomic 12 / widened 1 / escape 4` over what the manifest counts as 20
symbols across passes), not a movement.

## The DOM measurement: the real path does **not** agree with lane DA's workaround

Asked to run `"lib": ["dom"]` (uncommitted) and compare against DA's synthetic-package numbers
(`exact 1204, ergonomic 1545, widened 297, escape 526`, 3,572 symbols, 333,937 lines, both runs
under a minute). Built the analogous scratch package **through the real path this lane
implements** — trivial entry, `"lib": ["dom"]` (or, second attempt, `"lib": []` plus `///
<reference lib="dom" />`, matching DA's own directive shape more closely), `"groups": {
"typescript/lib": "ship" }` — and it does **not** behave the same way DA's copy-into-package
workaround did:

- First attempt (`"lib": ["dom"]`): resident memory climbed 2.0 GB → 3.65 GB → 6.3 GB over roughly
  ten minutes, accelerating rather than plateauing, no output written. Killed at 6.3 GB rather
  than let it run further on a machine shared with other lanes.
- Second attempt (`"lib": []` plus the reference directive, closer to DA's own recipe minus the
  physical copy): killed by an external 100-second timeout, no output written either.

**This directly answers the wave's question, and the answer is no, they do not agree.** DA's
workaround (copy `lib.dom.d.ts`'s text bodily into the package directory so it classifies
`EntryPackage` outright) finishes in under a minute; pointing the *same* declarations at the real
compiler-lib path this lane just taught `harvest-globals` to admit does not finish in ten minutes
and multiple gigabytes, on either of the two ways a user would plausibly set it up. This lane did
not diagnose which pass is the bottleneck — that is out of scope for the time available and risks
the shared machine further, the same caution lane DA raised about the ECMAScript half. Every
other tried input to date (`lib-ship-lab`'s `scripthost`, and DD's `dom-shadow-lab`/
`lib-reference-lab`) is trivially fast, so this is specifically a **DOM-scale** cost, not
something this fixture's own golden can be expected to surface.

**Flagging rather than chasing further, per the fixtures rule.** No DOM output was committed, no
`.scratch/` artifact survives this branch. Whoever picks up item 1c (shipping DOM for real) should
treat DA's 333,937-line, four-tier-count measurement as *aspirational* — a bound on what the
output should look like once the real path is made to scale, not a number this lane's own
implementation reproduces today. The next lane needs to profile which tier the real Ship path
diverges from the copy workaround in (Harvest's own symbol listing is a single RPC either way;
the divergence is downstream of that, in Resolve or Shape, and unmeasured here).

## Gate

`dotnet build Xantham.slnx -c Release` — compile gate included, 0 warnings, 0 errors (includes
`lib-ship-lab`'s golden compiling against `Fable.Core`/`Fable.Browser.*`).

`XANTHAM_UPDATE_GOLDEN=1 dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj
-c Release` — 532 passed, 0 failed (was 529 after lane DD; +2 for `lib-ship-lab`'s two tests, +1
elsewhere already present before this lane). Read the diff above before this commit.

`XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test` (no `--quick`, foreground, run twice — once
to confirm, once after Fantomas reformatted `Harvest.fs`'s wrapped lines): restore/clean/format
ok, `initialise fixtures` ok, test ok (Wire 90 passed/1 skipped — platform-specific, not a
missing-compiler skip; Generator 532 passed/0 failed), run gate 323 checks passed. Exit code 0
throughout both runs. Tree is clean of anything except the paths this lane changed.

## Provenance

Compiler: this worktree's own `node_modules` (installed by `initialise fixtures`), `typescript`
pinned per root `package.json`. Source touched: `src/Xantham.Generator/Harvest.fs`,
`src/Xantham.Generator/Model.fs` (doc comment only),
`tests/Xantham.Generator.Tests/Pipeline.test.fs`, `tests/fixtures/lib-ship-lab/**`,
`tests/Xantham.Generator.Tests/golden/lib-ship-lab/**`. `Grouping.classify` untouched, per brief.
DOM measurement reproduced by copying this handover's `.scratch/` recipe (trivial entry,
`"lib": ["dom"]` or `"lib": []` plus a `dom` reference directive, `"groups": { "typescript/lib":
"ship" }`) into a fresh package under a worktree's own `.scratch/`; nothing from it is committed.
