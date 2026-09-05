---
category: Generator
audience: managing agent
title: Dispatch - generator wave nine
integration-branch: worktree-generator-wave-nine
---

# Generator wave nine — dispatch

The triage of `generator-wave-nine.md`, the batch structure it produced, and the decisions the
worklist handed the manager. Every lane's handover sits in `docs/.ai/handovers/`.

`worktree-generator-wave-nine` forks `master` at `d8245ef` and carries the worklist commit
`a647c14`. Wave eight is already merged into `master`, so no rebase is owed. Every lane forks the
prep commit `f7d6c17`.

## Baseline, re-measured on the integration branch

Measured at `a647c14` before anything was dispatched, and again at `f7d6c17` after the prep commit:

Gate: **463 generator tests, 90 wire tests** (1 skipped by design), **run gate 249 checks**.
Corpus over all 49 fixtures: `exact 488, ergonomic 1544, widened 785, escape 193`.

`dotnet fsi build.fsx -- test` **exited 0** and left `git status` clean on both runs. Wave eight's
items 9 and 10 are therefore confirmed live on this branch, and the two process clauses the
worklist retires are retired on evidence rather than on report.

## What decided the batch structure

**Items 1 and 2 are one lane, in sequence.** This is the worklist's own reading and wave eight's
items 2/3 precedent. Item 1 changes how retention keys an overload group; item 2 asks whether a
`keyof`-constrained parameter can separate one. Item 2's answer is downstream of item 1's repair.
Both land in `Shape/Spec.fs` and `Shape/Overloads.fs`, and `generator-fixtures.md` requires work
landing in `Shape/Spec.fs` to be sequenced alone because every pass shares it. Lane AP owns both
files for the whole wave; no other lane opens either.

**Item 3 is concurrent with everything.** It touches `tests/fixtures/callback-function-lab/` and
`tests/Xantham.Generator.RunGate/Program.fs`, neither of which lane AP opens. Lane AM produced no
`src/` change for the same shape of work and this lane is not expected to either.

**Item 5 gets a lane, and its deliverable is a verdict rather than a repair.** Four waves of
deferral is the item's actual problem, and the worklist says so: open it, or record why it keeps
deferring. A read-only recon lane priced against the two committed recon docs closes it either
way — it hands wave ten a priced brief, or it removes the queue from the worklist with a reason.
Read-only, so it runs concurrently.

**Item 4 stays with the manager.** `git worktree remove` mutates state shared by every checkout,
every other agent running against this repository, and this wave's own three lanes. It is the one
item that cannot be isolated in a worktree, so delegating it would hand a lane a destructive
operation it has no way to scope. It runs after batch 1 merges, against the keep-list the manager
holds.

## Decisions the worklist handed the manager

### Item 6 — the small items: carried, and named

Carried again, as wave eight carried them. The wave already holds one repair, one verdict on a
thirteen-site cluster, one run-gate extension and one recon; adding `inline` demand-driven resolve,
the `Fable.Core` binding gaps, group sorting after a dependent and the `EndOfStreamException`
exceeds what one integration composes.

Two of the four are worth separating from the other two when wave ten prices them. The
`EndOfStreamException` from a dying tsgo child is a **defect in the wire, reproducible without the
generator**, and it does not belong on a generator worklist at all — it belongs with the wire
suite. `docs/.ai/fable-binding-gaps.md` is a **question for the user**, not work for a lane: the
document's `(ANSWER)` blocks are the user's to fill, and no lane can price the gaps until they are.
Both keep re-listing because the generator worklist is the wrong home for them.

### Item 5's outcome is priced as either answer

The recon lane is briefed that "record why it keeps deferring" is a completed lane, not a shortfall.
Wave eight closed three of seven settled items with no code and the worklist calls that the expected
rate.

### Clause 4 — verifying the premise

Neither priced item rests on a hypothesis. Item 1's cause was measured by lane AO, and the manager
confirmed its structure before dispatch: `literalErasedKey` in `Shape/Spec.fs:621` takes a
`typeId: int`, and `normalize` in `Shape/Overloads.fs:67` resolves through `abbrevs[name]`. The two
key on different things, exactly as recorded. Item 3's two positions were measured unreached by
lane AM.

So no lane is asked to price its premise from nothing. Each is asked to confirm the site still
behaves as recorded before changing anything, and each is told that disproving the item closes it.

## Pre-declared finding case

Appended in `f7d6c17`, before dispatch. The maximum in the `DO` prefix before this wave was `DO004`.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `DO.KeyofConstrainedOverloadDropped` | `DO005` | widened | Lane AP, item 2 |

The doc comment was checked against what the case records before the commit, per clause 5. The case
arrives unraised: if lane AP measures that a `keyof` parameter can separate the thirteen sites, it
stays unraised and costs one dead row, an outcome accepted in advance as wave eight accepted it for
`DO003`.

## Standing clauses every lane carries

1. Read `.claude/rules/generator-fixtures.md` first.
2. Verify the branch base before starting. Each was verified at `f7d6c17` by the manager; confirm it.
3. Commit as the work becomes coherent, not once at the end.
4. Fast loop while iterating (`--quick --update --no-run-gate --filter`); full
   `dotnet fsi build.fsx -- test` before the final commit.
5. **Trust `build.fsx`'s exit code.** Wave eight fixed it and this wave re-measured the fix. A
   failing pipeline that exits 0 is a regression worth stopping for, not a condition to work around.
6. **Never end a turn waiting on a background run.** Commit first, then wait. An unfinished build is
   a reason to commit, not a reason to pause. Wave eight lost a lane to exactly this: it produced
   both of its edits correctly, stalled three times across roughly 118 tool calls, committed
   nothing, and the manager carried its working tree.
7. **Verify the premise before acting on it.** Disproving the item closes it — a completed lane, not
   a failed one.
8. Finding codes come from `FindingCodes.table`. Use only the case named in your brief and edit
   `Findings.fs` no further. A case discovered mid-task is a request back to the manager.
9. Write the full report to `docs/.ai/handovers/lane-<id>.md` on your own branch and return at most
   fifteen lines.
10. Report measurements, not verdicts: `findings` output before and after, tier counts, finding
    codes added or moved, `git diff --numstat`.
11. Never `git push`, never open a PR, never merge into `master` or into the integration branch.
12. Hand back what you cannot explain. An unexplained large-fixture diff reported early is cheap.

## Lanes

| Lane | Items | Model | Branch | Owns |
| --- | --- | --- | --- | --- |
| AP | 1 then 2 | Opus 5 | `worktree-gen-wave9-ap` | `Shape/Spec.fs`, `Shape/Overloads.fs` |
| AQ | 3 | Sonnet 5 | `worktree-gen-wave9-aq` | `callback-function-lab`, `RunGate/Program.fs` |
| R3 | 5 | Sonnet 5 | `worktree-gen-wave9-r3` | docs only |

Lane AP takes Opus 5 because it is the wave's only behaviour change and it lands in the file every
pass shares. Lanes AQ and R3 take Sonnet 5: AQ extends a mechanism lane AM already built, and R3
reads committed analysis to produce a verdict. No lane exceeds Opus 5 on the standard context
window.

### Lane AP — items 1 and 2

Item 1 first. `@cloudflare/workers-types` `BrowserRun.quickAction` loses a literal-separated
overload. `BrowserRunContentOptions` and `BrowserRunMarkdownOptions` are distinct type ids that
render as an abbreviation pair, so retention never groups them and dedupe then collides them.
Repair by keying the retention group nearer the shaped form. The general statement of the gap is in
`docs/.ai/handovers/lane-ao.md`, which also warns that a repair may move `DO002` and `TR056` along a
path lane AO did not measure — measure both.

Item 2 second, because its answer may depend on whether retention and dedupe have been reconciled.
Thirteen of the eighteen remaining `DO001` sites are `DrawableSVGGeometry` in `animejs`, colliding
on a type parameter constrained by `keyof …TagNameMap`. Lane AO's probe reports **no literal at the
colliding position in any of the five members**, so lane AF's retention cannot reach them. Decide
whether a `keyof` parameter can separate them, or whether they are an accepted loss of the kind wave
eight's item 3 recorded for exported functions — in which case raise `DO005` and leave `DO001` for
the sites that remain.

### Lane AQ — item 3

Two callback positions are emitted unproven: a function-typed union arm under an array or an
`option`, and `U2<...>` nested inside a delegate's own type parameter — cloudflare's
`Action<'Type, U2<(obj -> unit), EventListenerObject<Event>>, ...>` on `EventTarget`'s `Create`.
Lane AM measured every other union-arm position green and flagged these two as unreached. Extend
`tests/fixtures/callback-function-lab/index.d.ts` and `index.js` and add checks to
`RunGate/Program.fs`, exactly as lane AM did. `docs/.ai/handovers/lane-am.md` carries the table of
what is already proven; do not re-prove those rows.

`TR055` already covers every retained union arm, so no finding case is expected. If the measurement
says a position loses arity, that is a defect report back to the manager, not a repair to attempt.

### Lane R3 — item 5

Price the fidelity queue — `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, and `FollowDepth` —
against `docs/.ai/plans/generator-cloudflare-recon.md` and `generator-tr018-recon.md`. Read-only.
Produce, per code, what it costs to close and what closing it buys, then a single verdict: open in
wave ten with a priced brief, or record the reason it keeps deferring in a form that removes it from
the worklist. Either verdict completes the lane.

## Outcomes

Measured on the composed integration branch after all three merges, gate green and tree clean.

| Item | Lane | Outcome |
| --- | --- | --- |
| 1 | AP | **Repaired.** All nine `BrowserRun.quickAction` overloads survive. `shapedKey` keys a literal-free type by the F# type it shapes to. |
| 2 | AP | **Option (b), on three measurements.** `DO001` 18 → 5, `DO005` 0 → 14. |
| 3 | AQ | **Measured safe, no `src/` change.** Both positions carry arity. Run gate 249 → 257. |
| 4 | manager | **Done.** 56 worktrees → 23. All 93 branches preserved. |
| 5 | R3 | **Priced, and splits four to one.** Four entries open for wave ten; `FollowDepth` wants a sampling lane first. |
| 6 | manager | **Carried, and two of the four are named as belonging elsewhere.** |

### Gate

| | base `f7d6c17` | composed |
| --- | ---: | ---: |
| generator tests | 463 | **467** |
| wire tests | 90 | 90 |
| run gate checks | 249 | **257** |
| exit code | 0 | 0 |

### Corpus

| | base | composed |
| --- | ---: | ---: |
| exact | 488 | 495 |
| ergonomic | 1544 | 1552 |
| widened | 785 | 786 |
| escape | 193 | 193 |

`DO001` 18 → **5**, `DO002` 4 → 5, `DO003` 0 → 0, `DO004` 3 → 3, `DO005` 0 → **14**,
`TR056` 32 → 34, `TR006` 1208 → 1206, `TR055` 360 → 361.

**Every count each lane reported independently survives composition.** Lane AP's tier movement
(+2 exact, +4 ergonomic, +1 widened) and lane AQ's (+5 exact, +4 ergonomic) sum to the composed
totals exactly, so no interaction between the branches went unobserved.

`DO001` falling from 18 to 5 is the wave's headline. The residue that remains is only the sites
nothing has yet explained — two exported functions, one non-literal collision, one return-type-only
pair, and one deliberate lab negative. A finding code that names a cause is worth more than one that
counts a symptom, and `DO001` now does the first.

### What the wave cost in process

Two lanes needed manager intervention, both for causes wave eight had already recorded.

**Lane AP had to be dispatched twice.** The first attempt never left the shell's inherited working
directory, did work on an unrelated branch, and rationalised a task out of what it found there —
it re-declared the finding case the manager had already committed. Nothing was committed to the
wrong branch and its real worktree was untouched, so the cost was one wasted dispatch. The retry
opened with a base-SHA gate that halts on mismatch and a path prefix on every command, and it
completed both items.

**Clause 3 is not yet self-enforcing.** Lane AQ ended a turn waiting on a background run despite
being briefed against it in the same words the worklist uses. It had committed first, so nothing
was lost and the manager resumed it with one message rather than rescuing a working tree — the
brief's other half did its job. Carry the clause again, and expect to enforce it rather than to
have stated it.

The dispatch-time instruction that did work: naming each lane's owned files. Three lanes ran
concurrently over `Shape/`, `RunGate/` and `docs/` with **no merge conflict at all**.
