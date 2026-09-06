---
category: Generator
audience: managing agent
title: Lane DD — HG003 says what it discarded, not just that it found nothing
---

# Lane DD — wave fifteen item 1a's diagnostic half

Branch `worktree-gen-wave15-dd`, forked from `worktree-generator-wave-fifteen`. Fixes lane DA's
finding: an entry `.d.ts` reaching content only through `/// <reference lib="X" />` harvests
zero declarations, exits 0, and the only signal is `HG003` (`HG.NothingHarvested`) reading
"declares neither a module nor any ambient global - nothing harvested" — true, but reads as
"this package is empty" rather than "everything it points at lives somewhere the harvester
does not follow" (`Grouping.classify`, `Model.fs:1275`, deliberately out of scope here).

## What changed

`HarvestGlobals.NothingHarvested` gained two fields: `entryFile: string * inScope: int *
elsewhere: string` (was `entryFile: string` alone). `Harvest.harvestGlobals` already computed
`ours` (the empty in-package subset) and, separately, `shadowedByLib` for the CLI's own
`libShadowWarning`; neither carried a count of what got discarded or where it went. A new
private `Harvest.elsewhere` groups the same in-scope symbol array by `Grouping.classify` (no
change to that function or its rules), formats "N in {group}" per group, most populous first,
and joins them. The trigger site (`Harvest.fs`, where `harvested` and `findings` are both
empty) now passes `symbols.Length` and `elsewhere ctx.PackageDir symbols` alongside the existing
`underPackage`-formatted entry file path.

New message (`Findings.fs`): `"{entryFile} declares neither a module nor any ambient global -
{inScope} in-scope symbol(s) resolve to {elsewhere} rather than this package, nothing
harvested"`. A genuinely empty `inScope = 0` (should the checker ever return nothing at all)
keeps the original short message rather than printing a description of nothing.

Example, from the new fixture: `index.d.ts declares neither a module nor any ambient global -
2190 in-scope symbol(s) resolve to 2189 in typescript/lib, 1 in unclassified declarations
rather than this package, nothing harvested`.

`FindingCodes.table`'s `HG003` row is untouched (name and code are keyed by case name, not
payload shape); `Findings.test.fs`'s snapshot pins names/codes, not payloads, so it needed no
change.

## Exit code: kept at 0, deliberately

Did not touch `Xantham.Cli`'s exit codes. Reasoning: `dom-shadow-lab` is an existing, gated,
legitimate fixture that hits this exact code path today — a global-script package whose every
declaration is a declaration-merge addition to an existing DOM type has nothing new of its own
to export, and zero declarations is the *correct* output for that package, not a failure.
Making "zero declarations" trip `Exit.Failed` would break that fixture's own contract, not just
the buggy `lib="X"` case lane DA found. The bug here was never "the exit code lies" — it was
"the diagnostic is uninformative enough that a user cannot tell the difference between 'my
package is genuinely empty' and 'my package's declarations are ninety-nine percent DOM lib
noise'." The message change above is what closes that gap; exit-code semantics stay keyed on
"did the generator run to completion and write its manifest" (`docs/generator-usage.md`,
`AGENTS.md`), unchanged.

## Fixture: `tests/fixtures/lib-reference-lab`

Hand-authored, tracked. `index.d.ts` carries nothing but a doc comment and `/// <reference
lib="dom" />`. `xantham.json` sets `"lib": []` so the DOM names in scope come from the
directive alone, not an implicit default-lib load — an isolated reproduction of lane DA's
`lib.dom.d.ts`-as-entry recon, at fixture scale instead of 45k lines. Registered in
`Pipeline.test.fs` right after `lib-lab`, via `handFixture`/`handConfig` (its own `xantham.json`
is the configuration, not a copy of it in the test). One test case beyond the golden-match
test, asserting the message names both the count and the `typescript/lib` group.

## Tier movement, gross, both directions

**Zero.** `dom-shadow-lab`'s manifest (`{"exact":0,"ergonomic":0,"widened":0,"escape":1}`) is
byte-identical before and after — `git diff --stat` on its golden directory touches exactly one
file, `symbols.jsonl`, one line changed (the `HG003` finding's `fields` and `message` grew
richer; its `key`, `name`, `tier` did not move). No symbol changed tier anywhere in the corpus.
`lib-reference-lab` is new, so it has no prior baseline to move against — its golden is the
tier-1 introduction (`escape 1`), not a movement.

No other fixture's golden changed. `HG003`/`NothingHarvested` was raised by exactly two
fixtures before this change (`dom-shadow-lab`, and now `lib-reference-lab`); grepped the whole
golden corpus to confirm.

## Gate

`XANTHAM_UPDATE_GOLDEN=1 dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj
-c Release` — 529 passed, 0 failed, goldens above are what it wrote. Read the diff before this
commit (above).

Then `XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test` (no `--quick`), run in the foreground
to completion: `restore` ok, `clean` ok, `format` ok (reformatted `Harvest.fs` — whitespace only,
diffed to confirm), `initialise fixtures` ok, `test` ok (Wire 90 passed/1 skipped — a
platform-specific package skip, not a missing-compiler skip; Generator 529 passed/0 failed), run
gate 323 checks passed. Exit code 0 throughout. Tree is clean of anything except the paths this
lane changed.

## Provenance

Compiler: this worktree's own `node_modules` install (`npm install` ran as part of `initialise
fixtures`), `typescript` pinned per root `package.json`. No source file outside
`src/Xantham.Generator/Harvest.fs`, `src/Xantham.Generator/Findings.fs`,
`tests/Xantham.Generator.Tests/Pipeline.test.fs`, `tests/fixtures/lib-reference-lab/**`, and
`tests/Xantham.Generator.Tests/golden/{lib-reference-lab,dom-shadow-lab}/**` touched.
`Grouping.classify` untouched, per brief; `Harvest.elsewhere` only reads its result.
