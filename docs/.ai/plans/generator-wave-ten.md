---
category: Generator
audience: managing agent
title: Worklist - generator wave ten
integration-branch: worktree-generator-wave-ten
---

# Generator wave ten — worklist

An inventory, not a plan. Triage the items below, decide what the wave takes, and write the lane
briefs that structure it. Wave nine's equivalent is `docs/.ai/plans/generator-wave-nine-dispatch.md`;
read its Outcomes section before pricing anything here.

**This wave is unusual: five of its items arrive already priced by a recon lane.** Wave nine's lane
R3 opened the fidelity queue that four waves had deferred and returned per-entry costs, a dispatch
order, and one entry it refused to price without more measurement. `docs/.ai/handovers/lane-r3.md`
is the document to read first — it is a dispatch brief in all but name, and re-deriving it would
waste the wave that paid for it.

Read `.claude/rules/generator-fixtures.md` first.

## Baselines, measured on the composed wave nine tree

Gate: **467 generator tests, 90 wire tests** (1 skipped by design under `XANTHAM_TSGO_EXE`), **run
gate 257 checks**. `build.fsx` exits its pipeline's real code and the gate leaves the tree clean.

Corpus, all 50 fixtures: `exact 495, ergonomic 1552, widened 786, escape 193`.

Residues across the golden corpus, labs included, counted from the committed `symbols.jsonl`:

`TR032` 5298, `MB003` 3836, `SP001` 1688, `TR006` 1206, `SY004` 713, `SP002` 651, `MB001` 610,
`TR008` 569, `TR055` 361, `TP002` 306, `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, `TR056` 34,
`DO005` 14, `RT001` 7, `DO001` 5, `DO002` 5, `DO004` 3, `AC001` 3, `TR014` 1. `DO003` stands at 0
and is meant to.

## Clauses that change how this wave is dispatched

Wave nine's twelve still hold. Three are worth restating because wave nine paid for them again, and
one is new:

- **New: pin every lane to its worktree with a halt-on-mismatch gate.** Wave nine lost a dispatch
  because a lane never left the working directory its shell started in, did work on an unrelated
  branch, and then rationalised a task out of what it found there. Prose naming the directory was
  not enough. **Open every brief with the exact `cd … && git rev-parse HEAD` command, the expected
  SHA, and an instruction to stop on mismatch**, and require the path prefix on every subsequent
  command. The retry with that gate completed both its items.
- **Expect to enforce the do-not-wait clause, not merely to state it.** Wave nine briefed a lane
  against ending a turn on a background run, in the worklist's own words, and the lane did it
  anyway. The other half of the clause is what saved it: it had committed first, so one message
  resumed it and nothing was rescued. **Brief both halves and treat the second as the load-bearing
  one.**
- **Naming each lane's owned files works.** Three lanes ran concurrently over `Shape/`, `RunGate/`
  and `docs/` with no merge conflict at all. Keep doing it.
- **A recon lane's verdict is a deliverable.** Item 5 sat on four worklists because every wave
  priced it as repair work and declined it. Priced as a verdict it took one read-only lane. Where
  an item keeps deferring, the thing to dispatch is the measurement, not the fix.

## Priced by lane R3, ready to become lanes

Lane R3's dispatch order, which the next manager should keep: the first four all land in
`Shape/Spec.fs` and must be **sequenced rather than parallelised**.

1. **`TR037` 54 — the cheapest entry in the queue.** `erasedUnionRef` accumulates duplicate
   per-arm findings before dedup, at `Shape/Spec.fs:1521`. No new finding case. Start here.

2. **`TR036` 72 — `ErasedUnionArity` is a stale constant.** It still stands at 4 while `Fable.Core`
   ships `U2` through `U9`. Raising it is the work; R3 also found a **silent tagged-union refusal**
   alongside it that wants fixing, and that one **needs a pre-declared finding case** — declare it
   before dispatch.

3. **`TR023` 137 — nearly at its floor.** One `LibBindings` row remains to add
   (`AsyncIterableIterator`); R3 verified the rest genuinely absent from `Fable.Core`. Expect the
   count to move very little and price the item on that basis rather than on the 137.

4. **`TR018` 82 — open, but its recon is stale.** Two of its causes (C and D) already landed
   uncredited. `type-fest`'s 8 sites are entirely cause B and are at their floor, verified by
   owner-name match. Two clusters are **unattributed and need a fresh reproducer before pricing**:
   `animejs`'s `.then()` cluster (8 sites) and four new `@cloudflare/workers-types`
   `Workflow*.retries` owners. R3 was read-only and could not build the reproducer.

5. **`FollowDepth` / `RT001` — a sampling lane, not a repair lane.** R3 refused to return a verdict
   here and the refusal is the finding. The `RT001` count of **7** masks roughly **2,061 unresolved
   types**, 1,815 of them in `@cloudflare/workers-types` — **up from 1,772 at the last
   measurement**. This is the one number in the corpus known to be growing while nothing watches it.
   Sample what those types actually are before anyone decides open or retire. R3 notes there is no
   read-only way to sample them without a debug hook, so price the hook as part of the lane.

## Carried from wave nine, priced rather than deferred

6. **Keeping the thirteen `keyof`-bound overloads.** Wave nine closed these as a recorded loss
   (`DO005` 14) on three measurements: the bound is not `keyof` by the time shaping sees it — the
   checker expands it into unions of 112, 63, 31 and 29 string literals; F# rejects members
   differing only in a constraint (`FS0438`, compiled directly); and no `TagNameMap` carries an F#
   name across the 452-row browser binding table, so `keyof<'T>` has no operand.

   The repair that *would* keep them is lane AO's `DO003` mechanism applied to a bound, at a
   **measured cost of 235 `StringEnum` cases across four bounds, in `animejs` alone**. That is the
   trade to decide. `docs/.ai/handovers/lane-ap.md` carries the measurement. This is a decision,
   not a discovery — do not re-measure it.

7. **The small items, reduced to two.** `inline` demand-driven resolve, and group sorting after a
   dependent. The other two entries left this list in wave nine because the generator worklist was
   the wrong home for them:

   - The **`EndOfStreamException` from a dying tsgo child** is a wire defect, reproducible without
     the generator. It belongs with the wire suite.
   - **`docs/.ai/fable-binding-gaps.md`** is a question for the user, not work for a lane. Its
     `(ANSWER)` blocks are the user's to fill and nothing can be priced until they are.

   Neither should appear on a generator worklist again.

## What not to chase

Wave nine's list stands unchanged, and two entries join it:

- **`TR032` 5298 and `MB003` 3836 are the corpus, not a backlog.** Both are `ergonomic`: a nullable
  hoisted to `option`, and an optional member read as `option`.
- **`TR006` 1206 is doc-noted.** Recorded at §4.2 of `generator-type-mapping.md`. Lane AF's
  retention covers the separating case and `TR056` counts it at 34.
- **`TR008` 569 is the `any` mapping.** Escape tier, not a work queue.
- **`TR055` 361 is a measured limit at the Fable boundary.** Waves seven, eight and nine measured
  this across the `Func`, curried and tupled spellings and across every position — including the
  union arm, and as of wave nine including a union arm behind an array or `option` and a `U2`
  nested inside a delegate's own type parameter. Only arity 0 and 1 are safe, and a function type
  may not return a function type. `D5a` in `generator-type-mapping.md` carries the rule. Do not
  open a lane to reduce it without new evidence about Fable's boundary.
- **`MB001` 610 is a repair working.**
- **Positional `obj` provenance is struck, not deferred.** Reopen it when a consumer arrives.
- **New: `DO005` 14 is a recorded loss, not a queue.** It counts the `keyof`-bound overloads wave
  nine measured and declined. Item 6 above is the decision that would change it; the count itself
  is not a target.
- **New: `DO001` 5 is at its explained floor.** What remains is two exported functions, one
  non-literal collision (`Ai.run`), one pair differing only in return type (`AutoRAG.aiSearch`),
  and two deliberate lab negatives. Every one has a recorded cause. A lower number would mean a
  cause went unrecorded, not that the generator improved.

## Shape of the wave, if it helps

Items 1 through 4 are one sequenced lane, not four — they share `Shape/Spec.fs`, and wave nine
demonstrated that sequencing two items in one lane composes cleanly while `Spec.fs` tolerates no
concurrency. Item 5 is independent of all of them and runs concurrently, but it is a measurement
lane and should be briefed to return numbers rather than a repair. Item 6 is a decision for the
manager or the user, and needs no lane at all.

Wave nine settled all six of its items across three lanes and changed generator behaviour in two
places, one of them a real consumer loss. It cost one wasted dispatch and one manager intervention,
both from causes wave eight had already recorded and wave nine's briefs had already named. Briefing
against a failure does not prevent it; gating against it does.
