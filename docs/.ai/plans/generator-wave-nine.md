---
category: Generator
audience: managing agent
title: Worklist - generator wave nine
integration-branch: worktree-generator-wave-nine
---

# Generator wave nine — worklist

An inventory, not a plan. Triage the items below, decide what the wave takes, and write the lane
briefs that structure it. Wave eight's equivalent is
`docs/.ai/plans/generator-wave-eight-dispatch.md`; read its Outcomes section before pricing
anything here, because three items below are its leavings and each carries the measurement that
produced it.

`worktree-generator-wave-nine` branches `master` at `d8245ef`. **Wave eight is already merged into
`master`** — verified in the reflog as a fast-forward onto an identical tree — so no rebase is
owed.

Read `.claude/rules/generator-fixtures.md` first.

## Baselines, measured on `master` at `d8245ef`

Gate: **463 generator tests, 90 wire tests** (1 skipped by design under `XANTHAM_TSGO_EXE`), **run
gate 249 checks**.

Corpus, all 49 fixtures: `exact 488, ergonomic 1544, widened 785, escape 193`.
`@cloudflare/workers-types`: `exact 236, ergonomic 1090, widened 382, escape 110`.

Residues across the golden corpus, labs included, counted from the committed `symbols.jsonl`:

`TR032` 5297, `MB003` 3835, `SP001` 1681, `TR006` 1208, `SY004` 713, `SP002` 650, `MB001` 610,
`TR008` 569, `TR055` 360, `TP002` 304, `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, `TR056` 32,
`DO001` 18, `SY005` 7, `RT001` 7, `DO002` 4, `DO004` 3, `AC001` 3, `TR014` 1. `DO003` and `TR057`
stand at 0 and are meant to.

## Five clauses that change how this wave is dispatched

Wave eight's own four still hold — finding codes come from `FindingCodes.table`, a lane writes its
full report to `docs/.ai/handovers/<lane>.md` and returns at most fifteen lines, gate at batch
boundaries, verify each branch base before dispatch. Five more were paid for:

- **`build.fsx` now exits with its pipeline's real code.** The advice every lane carried since wave
  six — read the summary line, not the exit code — is **retired**. Brief lanes to trust the exit
  code. A failing pipeline that exits 0 again is a regression worth stopping for, not a condition
  to work around.
- **The gate leaves the tree clean.** `git status` reads a lane's true footprint now, and
  `git merge` runs without a cleaning step. A full run that leaves ~47 F# files modified with a
  byte-empty content diff means `.editorconfig`'s `end_of_line = lf` has regressed.
- **A lane must never end its turn waiting on a background run.** Wave eight lost a lane exactly
  this way: it produced both of its edits correctly, then stalled three times across roughly 118
  tool calls waiting for a build to notify it, committed nothing, and the manager had to commit its
  working tree and carry out the verification. **Brief every lane to commit before it waits, and to
  read an unfinished build as a reason to commit rather than a reason to pause.**
- **Verify a worklist premise before acting on it.** Two of wave eight's five priced items were
  written on premises that measurement disproved — a bare `x: null` does record a fact, and the
  literal-union collision does not occur in this corpus. Both premises came from real wave-seven
  observations and both were wrong about what followed from them. Price the measurement first and
  the change second, and say in the brief that disproving the item closes it.
- **Spot-check the doc comment on every pre-declared case.** Two of the three cases pre-declared in
  wave eight carried comments asserting the opposite of what the case records. A lane reads that
  comment before it raises the case, so the error arrives where it does the most damage.

## Priced, ready to become lanes

1. **`BrowserRun.quickAction` loses a literal-separated overload, and the cause is measured.** This
   is wave eight's one newly found defect and the highest-value item here, because it is a real
   consumer loss rather than a recorded one. Retention groups signatures by type id through
   `literalErasedKey`; dedupe compares normalised F# signatures through `normalize`. The two
   disagree: `BrowserRunContentOptions` and `BrowserRunMarkdownOptions` are distinct type ids that
   render as an abbreviation pair, so retention never groups them and dedupe then collides them.
   Repair belongs in `Shape/Spec.fs`. Detail in `docs/.ai/handovers/lane-ao.md`.

2. **Thirteen of the eighteen remaining `DO001` sites are one cluster.** `DrawableSVGGeometry` in
   `animejs` drops thirteen overloads to a `keyof` type parameter. Never priced. Decide whether a
   `keyof` parameter can separate them, or whether they are an accepted loss of the kind wave
   eight's item 3 recorded for exported functions — in which case they want a finding of their own
   rather than `DO001`. The full split of the eighteen is in `docs/.ai/handovers/lane-ao.md`.

3. **Two callback positions are emitted unproven.** A function-typed union arm under an array or an
   `option`, and `U2<...>` nested inside a delegate's type parameter. Lane AM measured every other
   union-arm position green and flagged these two as unreached. Small, and the shape of the work is
   already built: extend `callback-function-lab` and the run gate exactly as lane AM did. Detail in
   `docs/.ai/handovers/lane-am.md`.

## Process, worth a lane of its own

4. **Prune the stale agent worktrees.** `git worktree list` carries 56 entries, most from waves two
   through seven. A branch survives its worktree's removal and is the record; the directory is
   not. **Remove the worktrees and keep every branch** — `AGENTS.md` is explicit that
   the branch is the handoff. Confirm no worktree holds uncommitted work before removing it, since
   that is the one thing removal loses.

## Carried from earlier waves

5. **The fidelity queue** — `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, and `FollowDepth`.
   `generator-cloudflare-recon.md` and `generator-tr018-recon.md` carry the analysis. Untouched for
   four waves: either open it, or record why it keeps deferring so it stops being re-listed.

6. **The small items** — `inline` demand-driven resolve, the `Fable.Core` binding gaps in
   `docs/.ai/fable-binding-gaps.md`, group sorting after a dependent, and the
   `EndOfStreamException` from a dying tsgo child.

## What not to chase

- **`TR032` 5297 and `MB003` 3835 are the corpus, not a backlog.** Both are `ergonomic`: a nullable
  hoisted to `option`, and an optional member read as `option`. They are the mapping working at the
  scale the corpus provides.
- **`TR006` 1208 is doc-noted.** A string literal widens to `string` wherever it does not separate
  an overload; the decision is recorded at §4.2 of `generator-type-mapping.md` and the finding's
  own message cites it. Lane AF's retention covers the separating case and `TR056` counts it at 32.
- **`TR008` 569 is the `any` mapping.** Established in `generator-cloudflare-recon.md` and
  unchanged since. Escape tier, not a work queue.
- **`TR055` 360 is a measured limit at the Fable boundary.** It marks the callbacks that keep a
  delegate because an F# function type is unsafe there. Waves seven and eight measured this across
  the `Func`, curried and tupled spellings and across every position including the union arm: only
  arity 0 and 1 are safe, and a function type may not return a function type. Do not open a lane to
  reduce `TR055` without new evidence about Fable's boundary. `D5a` in `generator-type-mapping.md`
  carries the rule.
- **`MB001` 610 is a repair working.** It fired nowhere before wave seven taught the wire to carry
  parameter optionality.
- **Positional `obj` provenance is struck, not deferred.** Waves five, six and seven deferred it and
  wave eight struck it: the mechanism exists as lane AG's `NodeHandle`, and no consumer joins
  checker facts to body operations. Reopen it when a consumer arrives, not before.

## Shape of the wave, if it helps

Items 1 and 2 are the same subject matter and both land in `Shape/Spec.fs` and
`Shape/Overloads.fs`, so they belong to one lane in sequence rather than to two lanes — the
structure wave eight used for its own items 2 and 3, which worked. Item 1 goes first: item 2's
answer may depend on whether retention and dedupe have been reconciled.

Item 3 touches only the run-gate lab and its checks, so it runs concurrently with anything. Item 4
touches no source at all.

Wave eight settled seven of thirteen items across five lanes and changed generator behaviour in one
place. Three items closed with no code because measurement disproved the item. That is the expected
rate rather than a shortfall, and a wave briefed to produce verdicts instead of measurements will
not reach it.
