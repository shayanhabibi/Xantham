---
category: Generator
audience: managing agent
title: Worklist - generator wave eight
integration-branch: worktree-generator-wave-eight
---

# Generator wave eight — worklist

An inventory, not a plan. Triage the items below, decide which the wave takes, and write the lane
briefs and batch structure from that. Wave seven's equivalent is
`docs/.ai/plans/generator-wave-seven-dispatch.md`; read its Outcomes section before pricing
anything here, because four of the items below are its leavings and it recorded what it measured.

`worktree-generator-wave-eight` branches from `master` at `ce59075`. **Wave seven is already
merged into master** — no rebase is owed, unlike the position wave seven inherited.

Read `.claude/rules/generator-fixtures.md` first.

## Baselines, measured on `master` at `ce59075`

Corpus, all 49 fixtures: `exact 479, ergonomic 1540, widened 783, escape 193`.
`@cloudflare/workers-types` `exact 236, ergonomic 1090, widened 382, escape 110`.
Gate: 460 generator tests, 90 wire tests (1 skipped by design under `XANTHAM_TSGO_EXE`), run gate
230 checks.

Residues across the golden corpus, labs included: `TR008` 569, `TR055` 357, `TR023` 137, `TR018`
82, `TR036` 72, `TR037` 54, `DO001` 19, `SY005` 7, `AC001` 3. `MB001` 610 and rising with the
corpus, which is correct rather than a queue.

## Four clauses from wave seven that change how a wave is dispatched

Wave seven's own four still hold — finding codes come from `FindingCodes.table`, a lane writes its
full report to `docs/.ai/handovers/<lane>.md` and returns at most fifteen lines, gate at batch
boundaries, verify every branch base before dispatch. Four more were paid for:

- **Do not trust a worklist's claim that two items are independent. Check the file.** Wave seven's
  worklist placed item 4 as the one change sharing no file with the rest. It shared `signatureRef`
  with the wave's largest lane, and four of six items resolved through `Shape/Spec.fs`. One grep
  before dispatch reshaped the whole wave.
- **`build.fsx` exits 0 when its pipeline fails.** It prints `Error: Pipeline is failed because
  the result is not indicating as successful` and exits successfully. Wave seven reported a broken
  tree as green off the exit code and had to retract it. Read the summary lines — `Passed!`,
  `run gate: N checks passed` — and grep for the failure line. **See item 9: this is worth fixing
  rather than documenting again.**
- **`Pipeline.test.fs` collisions can be reported as two regions.** Two lanes appending a fixture
  block produced two conflict hunks, and dropping every marker interleaved the branches — one
  lane's block was cut after its first `testCase` and the other's spliced into the gap. It reached
  `FS0747`. Resolve by taking one side whole and re-inserting the other's block whole, not by
  deleting markers.
- **A lane that measures and refuses is a successful lane.** Wave seven's lane AE was briefed to
  convert 1,245 members, measured that the conversion breaks at run time, converted nothing, and
  that refusal was the most valuable thing in the wave. Brief lanes so that refusal is an
  available outcome, and say so in the brief.

## Priced, ready to become lanes

1. **A function type inside `U2<...>` is emitted and unproven.** Wave seven's lane AK converted
   callbacks at arity 0 and 1 to F# function types. One such type appears inside an erased union
   arm in the `@cloudflare/workers-types` golden and compiles, but no run-gate check targets a
   union arm, so nothing measures what crosses the boundary there. The lane that landed the
   conversion flagged it as the one form it emitted without proving. Small: extend
   `callback-function-lab` and the probes. Detail in `docs/.ai/handovers/lane-ak.md`.

2. **An anonymous union of literals at a distinguishing position still collides.** Lane AF retains
   a *single* string literal where it separates an overload set. No single literal type stands for
   `"a" | "b"`, so an overload distinguished by an anonymous union of literals still dedupes away.
   Pinned as the negative by `literal-overload-lab`'s `Choice`. Closing it means synthesizing a
   StringEnum over the arm set at that position, which is the design lane AF considered and
   rejected for the single-literal case — read its reasoning first, because it does not transfer.

3. **Export-function overloads are uncovered by lane AF's fix.** `DO001` stands at 19; the two
   remaining `animejs` sites are `$` and `mapRange`, and neither is literal-separated, so the
   retention rule does not reach them. Needs a different separator or an accepted and recorded
   loss.

4. **A retained literal derived from a URL makes a long declaration name.**
   `"http://www.w3.org/1999/xhtml"` becomes `DrawableSVGGeometry.HttpWwwW3Org1999Xhtml` in the
   `animejs` golden. Deterministic and compiling, so this is a naming judgment rather than a
   defect: cap the derived name, hash the tail, or leave it. Handed back by lane AF explicitly for
   a manager to call.

5. **A bare `x: null` records no absence fact.** It is not a union, never reaches `unionRef`, and
   widens to `obj`; `TR033` does not fire. The five absence spellings D1 names all record
   something and this one does not. Pinned as the negative in `absence-alphabet-lab`. The question
   is whether a bare absence should carry an absence finding at all — decide before writing code,
   because the answer may be that the current behaviour is right.

## Process, worth a lane of its own

9. **Make `build.fsx` exit non-zero when its pipeline fails.** Every clause above about reading
   summary lines exists because the exit code lies. `master` has just moved to Partas.Build alpha
   3 (`ce59075`), so check whether the behaviour survived the upgrade before pricing the fix. This
   is a small change to the repository's own tooling that removes a recurring class of false
   green, and wave seven demonstrated the failure rather than hypothesising it.

10. **`build.fsx -- test` runs fantomas, which rewrites every F# file's line endings on Windows.**
    After a full run `git status` reports every F# file as modified with an empty content diff, so
    it cannot be used to read a lane's footprint, and `git merge` refuses to run until the tree is
    cleaned. Compare with `git diff --ignore-cr-at-eol`. Either pin fantomas' output to the
    `.gitattributes` policy or stop running it inside the test pipeline.

## Carried from earlier waves

11. **The fidelity queue** — `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, `alignOperands`, the
    `FollowDepth` cutoff. Priced in `generator-cloudflare-recon.md` and `generator-tr018-recon.md`.
    Carried through waves five, six and seven without being taken; either take a slice of it or
    strike it from the list, because a queue nobody schedules is not a queue.

12. **Wave five's open items** — `inline` demand-driven resolve, the `Fable.Core` binding gaps in
    `docs/.ai/fable-binding-gaps.md`, group sorting after a dependent, the `EndOfStreamException`
    from a dying tsgo child.

13. **Positional `obj` provenance in findings.** Deferred in waves five, six and seven, each time
    for the same reason: the per-symbol table already addresses every declaration site uniquely,
    and no consumer joins checker facts to body operations. If a consumer arrives, the field is a
    Wire node handle Harvest already holds — and lane AG built `NodeHandle` in wave seven, so the
    mechanism now exists. Reprice it against that, or strike it.

## What not to chase

- **`TR055` at 357 is a record, not a queue.** It marks callbacks that keep their delegate form
  because an F# function type is unsafe there. Wave seven measured three spellings — `Func`,
  curried, tupled — across every position; curried throws at arity 2 and above, tupled reads its
  arguments as `undefined` and does not throw, and only arity 0 and 1 are safe. Do not open a lane
  to reduce `TR055` without new evidence about Fable's boundary. `D5a` in
  `generator-type-mapping.md` carries the measurement.
- **`TR008` at 569 is the `any` mapping and the escape tier is not a work queue.** Established in
  `generator-cloudflare-recon.md` and unchanged since.
- **`MB001` at 610 is the repair working.** It fired nowhere before wave seven because the wire
  discarded the fact. A high count is the finding doing its job.

## Shape of the wave, if it helps

Items 1 and 5 are small and touch files nothing else needs — 1 is the run gate and a lab, 5 is
`unionRef` plus a decision. Items 2 and 3 are the same lane's subject matter and both land in
`Shape/Spec.fs` and `Shape/Overloads.fs`, so they belong together or in sequence, never
concurrently. Item 4 is a naming call that should be answered before item 2 lands, since item 2
mints more derived names. Items 9 and 10 touch only the build script and compose with anything.

Check `Shape/Spec.fs` ownership before believing any of that.
