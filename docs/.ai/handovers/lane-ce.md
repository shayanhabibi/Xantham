---
category: Generator
audience: managing agent
title: Handover - lane CE, wave thirteen batch two (TR058, cluster E recovery)
---

# Lane CE - uninhabited-intersection reduction lands as `TR058`, 7 of 9 sites recovered

Wave thirteen, batch two, branch `worktree-gen-wave13-ce`, forked at `522450b` (which
pre-declared `TR.UninhabitedIntersectionReduced property: string`, code `TR058`, ergonomic,
for this lane alone).

## What was built

- **`src/Xantham.Generator/Shape/Spec.fs`** - a new `uninhabitedOperand` helper, beside
  `reducedOperand`, and one new branch in `intersectionRef` that consults it before falling
  through to the `isPureCallback` / `IntersectionOverNonObject` fallback.

  `uninhabitedOperand` only fires inside the existing `facts.Members.IsEmpty &&
  facts.IndexInfos.IsEmpty` condition (the one that used to mean "non-object operand,
  widen"). It requires positive evidence that the operands were object types that
  collided, not merely that the aggregate is empty:

  - every operand resolved from `operandsOf` carries `TypeFlags.Object`,
  - at least one operand carries members (ruling out the genuine non-object case - a type
    parameter operand, an array, a primitive - which stays on the old fallback and keeps
    reporting `TR018`/`TR019` exactly as before),
  - some member name is shared across operands where one operand types it `Null` or
    `Undefined` and exactly one other operand does not.

  Where all three hold, the finding maps to the surviving (non-nullable) operand's type and
  raises `TR058` naming the collided property, instead of widening to `obj` under `TR018`.

  This is deliberately narrower than "no members present": a same-property, non-unit-typed
  collision (`{ then: string }` against a `then(): void` method) does **not** empty the
  aggregate - TypeScript keeps the object's members and the loss lands on the individual
  member's own nested facts instead, which the pre-existing per-member `IntersectionNotDeclared`
  path already covers unchanged. Verified with the lab's `Chained` negative (below).

- **`src/Xantham.Generator/Findings.fs`** - reworded `UninhabitedIntersectionReduced`'s message
  only (case name, tier, code untouched, per the constraint): `'{property}' collides across the
  intersection's operands and TypeScript reduces the whole type to never; the operand that does
  not mark '{property}' nullable is the type`. No `FindingCodes.table` edit; `Findings.test.fs`'s
  `TR.UninhabitedIntersectionReduced TR058 ergonomic` snapshot line was already present from the
  pre-declaration commit and needed no change.

- **`tests/fixtures/uninhabited-intersection-lab/`** - new hand-authored, tracked fixture (own
  `package.json`, `index.d.ts`). One class (`Timer`) whose own method collides with a `{ then:
  null }` marker on itself; the same reduction spelled as an alias plus a use of it (`Named` /
  `Reduced` / `reduced`); three negatives that each differ from `Timer` in exactly one place -
  no name collision (`Ticking`), collided name not a member of the other operand (`Player`), and
  a non-unit-typed collision (`Chained`).

- **`tests/Xantham.Generator.Tests/Pipeline.test.fs`** - `fixtureTests "uninhabited-intersection-lab"
  (handFixture "uninhabited-intersection-lab")` registered, three `testCase`s: the positive
  mapping and finding payload, the alias form, and the three negatives producing zero `TR058`.
  No run-gate check added - this is a shape/finding-tier change, not new runtime behaviour, so
  the compile gate (which already compiles every committed golden, including this lab's) is the
  correct and sufficient gate.

## Verdict: 7 of 9 sites recovered, 2 explained and left as `TR018`

Lane CC's nine owners were: `Timer.then`, `Timeline.then`, `JSAnimation.then`,
`WAAPIAnimation.then`, `CallbackArgument.then`, `CallbackArgument.Head.then`,
`CallbackArgument.Head.Parent.then`, `CallbackArgument.Head.Prev.then`,
`CallbackArgument.Head2.then`. Measured after regenerating the corpus:

| site | before | after |
| --- | --- | --- |
| `Timer.then(callback)(self)` | TR018 | **TR058** |
| `Timeline.then(callback)(self)` | TR018 | **TR058** |
| `JSAnimation.then(callback)(self)` | TR018 | **TR058** |
| `WAAPIAnimation.then(callback)(self)` | TR018 | **TR058** |
| `CallbackArgument.Head.then(callback)(self)` | TR018 | **TR058** (self: `JSAnimation`) |
| `CallbackArgument.Head.Prev.then(callback)(self)` | TR018 | **TR058** (self: `Timeline`) |
| `CallbackArgument.Head2.then(callback)(self)` | TR018 | **TR058** (self: `Timer`) |
| `CallbackArgument.then(callback)(self)` | TR018 | TR018 (unchanged) |
| `CallbackArgument.Head.Parent.then(callback)(self)` | TR018 | TR018 (unchanged) |

**Cause of the 2 residuals, confirmed by reading `animejs`'s own `.d.ts`:**
`export type CallbackArgument = Timer & JSAnimation & Timeline;`. When this alias intersects
with `{ then: null }`, TypeScript flattens the nested intersection into one flat operand list -
`[Timer, JSAnimation, Timeline, { then: null }]` - four operands, not two. My detection's
partition step requires *exactly one* non-nullable operand sharing the collided name (mirroring
`reducedOperand`'s own "exactly one remaining operand" shape); here there are three
(`Timer`, `JSAnimation`, `Timeline` each declare `then`), so the group has one nullable operand
and three survivors and the pattern match declines, correctly falling through to the unchanged
`TR018` fallback rather than guessing which of the three (or their conjunction) to report.

Recovering these two would mean synthesizing a reference to the *intersection of the three
survivors* - which happens to be exactly what the `CallbackArgument` alias itself denotes, but
there is no operand-level trace of that alias identity left after the checker's own flattening,
only the raw three class ids. Matching a synthesized survivor set back to a declared alias by
structural identity felt like real scope creep for this lane (risk of misattributing a
coincidental structural match), and the lab fixture that proves the two-operand case does not
model this three-operand alias-through-flatten shape. Reporting this rather than chasing it, per
the fixtures rule: **the residual 2 sites are exactly-explained, left as `TR018`, not chased.**

## Confirmation the other `TR018` sites are undisturbed

Corpus-wide `TR018` per fixture, before (committed goldens at `522450b`, matches the batch
baseline exactly) vs. after (regenerated with `dotnet fsi build.fsx -- test --quick --update
--no-run-gate`, no filter, over every fixture):

| fixture | before | after |
| --- | ---: | ---: |
| `@cloudflare/workers-types` | 20 | 20 |
| `animejs` | 13 | 6 |
| `brand-lab` | 1 | 1 |
| `hoist-conditional-lab` | 3 | 3 |
| `intersection-empty-lab` | 1 | 1 |
| `intersection-lab` | 1 | 1 |
| `solid-js` | 12 | 12 |
| `type-fest` | 8 | 8 |
| `uninhabited-intersection-lab` (new) | - | 1 (the `Chained` negative, unchanged mechanism) |
| **corpus total (pre-existing fixtures)** | **59** | **52** |

Every fixture except `animejs` is bit-for-bit identical in `TR018` count. `git status` and `git
diff --stat` (below) confirm no golden other than `animejs`'s three files and the new lab's
changed at all - `@cloudflare/workers-types`, `solid-js`, `type-fest` and every `*-lab` besides
the new one are untouched in the working tree. That is the strongest available confirmation: the
other 73 sites recon/lane CC attributed to cause A/B1/B2 were not touched because their goldens
did not regenerate differently.

`TR058` corpus-wide: 0 -> 10 (7 `animejs`, 3 `uninhabited-intersection-lab`).

## Tier and finding-count deltas

Summed across every `golden/**/manifest.json` (53 manifests, was 52 - the new lab):

| tier | before | after | delta |
| --- | ---: | ---: | ---: |
| exact | 530 | 530 | 0 |
| ergonomic | 1633 | 1636 | +3 |
| widened | 776 | 777 | +1 |
| escape | 195 | 201 | +6 |

The entire delta is the new lab fixture's own manifest counts (`exact 0, ergonomic 3, widened 1,
escape 6`) - confirmed by summing every other manifest before/after and finding them identical.
`animejs`'s own per-symbol tier counts (`exact 79, ergonomic 107, widened 18, escape 54`) are
*unchanged* despite the 7-site `TR018`->`TR058` swap: every one of those seven `.then` sites
already carried an `escape`-tier `TR.AnyToObj` finding (the callback parameter itself is
`any`-typed), so the symbol's own worst-tier bucket was already `escape` before and after: the
swap only moved which *finding key* explains the loss, not which tier bucket the symbol falls
in. `animejs`'s per-pass breakdown in `manifest.json` (`shape-interfaces`) does show the
movement directly: `ergonomic 1418 -> 1425` (+7), `widened 179 -> 172` (-7).

Total findings corpus-wide: 16,870 -> 16,918 (+48, exactly the new lab fixture's own finding
count: `TR008 10, SP001 8, SP002 7, MB001 6, TR032 6, SI003 3, SY004 3, TR058 3, TR014 1, TR018
1` = 48). `TR020 69, TR036 9, TR023 136, TR037 15, DO001 5, RT001 3` - all unchanged from the
wave-thirteen baseline.

## Run gate and generator test counts

- Generator suite: 477 passed (baseline 472, +5 = `fixtureTests` always contributes its own
  `"<fixture> generates the committed goldens"` and `"<fixture> generation is deterministic run
  to run"` cases before the `extra` list, so the new block's 3 `testCase`s land as 2 + 3 = 5).
- Wire suite: 90 passed, 1 skipped (unchanged from baseline).
- Run gate: 300 checks passed (unchanged from baseline - no new check added, per the rule that a
  shape/finding change needing only the compile gate should not add a run-gate check).
- `dotnet build Xantham.slnx`: 0 errors (one pre-existing `FS0025` incomplete-match warning in
  `Shape.test.fs:1553`, present before this branch, untouched).
- `dotnet fsi build.fsx -- test` (full, ungated flags): green end to end, including `npm
  install`/fixture initialisation, all Expecto suites, and the Fable run gate.

## `git diff --stat`

```
 src/Xantham.Generator/Findings.fs                           |  2 +-
 src/Xantham.Generator/Shape/Spec.fs                         | 76 +++++++++++++++++-----
 tests/Xantham.Generator.Tests/Pipeline.test.fs               | 57 ++++++++++++++++
 tests/Xantham.Generator.Tests/golden/animejs/Animejs.fs      | 14 ++--
 tests/Xantham.Generator.Tests/golden/animejs/manifest.json   |  4 +-
 tests/Xantham.Generator.Tests/golden/animejs/symbols.jsonl   | 14 ++--
 6 files changed, 133 insertions(+), 34 deletions(-)
```

Plus two new, untracked-until-this-commit paths: `tests/fixtures/uninhabited-intersection-lab/`
(hand-authored fixture) and `tests/Xantham.Generator.Tests/golden/uninhabited-intersection-lab/`
(its golden, generated by `--update`). No golden outside `animejs` and the new lab changed at
all - `@cloudflare/workers-types`, `solid-js`, `type-fest` and every other `*-lab` are
byte-identical to `522450b`.

## What I could not explain

Nothing outside the accounted-for residual above. The `animejs` `Animejs.fs` diff is exactly the
seven `self: obj -> self: <ClassName>` substitutions expected; nothing moved in any other
fixture; every count reconciles to either "the new lab fixture" or "the seven recovered sites",
with no unattributed remainder.

## For the next lane touching `TR018`/cluster E

The two residual `CallbackArgument`-through-alias sites are a genuine third sub-case of the
uninhabited-intersection family (three-or-more-operand collision via a flattened alias), not
covered by this lane's lab or predicate. If a future lane wants to close them, the shape to
solve is: given a nullable-marked operand and *n > 1* surviving operands that together equal an
already-declared alias's own operand set, map to that alias rather than to a single operand.
