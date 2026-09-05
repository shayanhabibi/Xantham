---
category: Generator
audience: managing agent
title: Worklist - generator wave seven
integration-branch: worktree-generator-wave-seven
---

# Generator wave seven — worklist

This is an inventory, not a plan. Wave six's plan priced four lanes against a consumer's edit
before dispatch; the equivalent for this wave has not been written. Triage the items below, decide
which the wave takes, and write the lane briefs and batch structure from that.

`worktree-generator-wave-seven` currently branches from wave six's tip, which has not landed on
master. **Rebase onto master once wave six merges**, or the first golden regeneration will
disagree with whatever else lands in between.

Read `.claude/rules/generator-fixtures.md` first. Four clauses changed during wave six and they
change how a wave is dispatched:

- Finding codes come from `FindingCodes.table`, not declaration position. Appending renumbers
  nothing, and a new case costs three edits.
- A lane writes its full report to `docs/.ai/handovers/<lane>.md` and returns at most fifteen
  lines. A wave of long reports exhausts the managing agent before the last lane is dispatched.
- Gate at batch boundaries, not per lane.
- Verify each agent's branch base before it starts, and tell every lane to commit as work becomes
  coherent. Three of six lanes in wave six forked off the wrong commit; two lost a turn to an
  interruption while carrying a whole change uncommitted.

## Baselines, measured on wave six's tip

Corpus `exact 464, ergonomic 1506, widened 773, escape 191`.
`@cloudflare/workers-types` `exact 234, ergonomic 1089, widened 381, escape 110`.
Gate: 419 generator tests, 85 wire tests, run gate 160 checks.

## Priced, ready to become lanes

1. **`Func<_,_>` becomes `FSharpFunc<_,_>`.** Every callback the generator emits obliges a consumer
   to wrap an ordinary curried F# function in `System.Func`. The plan asked that this be priced
   against the delegate count *after* wave six, because lane AA stopped optional hooks being
   delegates at all. That count is **1,245**: cloudflare 559, animejs 515, solid-js 169,
   type-fest 2. The largest consumer-facing item on the list.
2. **`KVNamespace.get` binds only the first overload.** `arrayBuffer`, `stream` and `json` are out
   of reach from F#. Reconnoitred in `generator-cloudflare-recon.md` §3.2. Same family as wave
   six's four reports.
3. **Six `Name2` residues are namespaced declarations** — `DurableObject2`,
   `WorkflowDurationLabel2`, `WorkflowSleepDuration2`, `ScriptVersion2`, `TailEvent2`,
   `TracePreviewInfo2` — which nesting under a namespace module would resolve. Lane AD answered
   the question with a number and deliberately made no change. Total residue stands at 124, the
   rest being overload parameters and union arms with no owner to nest under. Detail in
   `docs/.ai/handovers/lane-ad.md`.

## Defects found during wave six, none chased

4. **`MB001` never fires on an optional parameter.** The wire does not flag optionality on
   parameter symbols, so `p.Optional` is only ever true for expanded tuple-rest elements and
   `isOptionalParam` infers optionality back from the hoist. `f(x?: T)` and `f(x: T | undefined)`
   are therefore one signature and the `?` is unrecoverable at parameter positions. Lane AC pinned
   it as a test rather than repairing it. **This is a wire change, not a Shape change** — price it
   as one.
5. **`TR033` carries no absence alphabet.** A union of absences alone collapses to `unit` with no
   payload, so it is a sixth absence site wave six's `TR032` alphabet does not reach. Giving it
   flags is a payload change to a second case, which wave six's dispatch forbade.
6. **A member key that is not a legal .NET type name emits `FS0883`.** `"@cf/meta"` renders as
   ``type `Registry@cf/meta` ``. Pre-existing, byte-identical under both naming schemes, and
   unexercised by the corpus, so nothing gates it today. Reproducer in
   `docs/.ai/handovers/lane-ad.md`.

## One question that is not a code change

7. **Which discovery mode `workerd` performs.** Wave six's lane AA emits each optional hook on an
   entrypoint class as an opt-in interface, and that form works if the platform reads hooks by
   access and does not if anything in the path enumerates own keys. Lane Z established the F# and
   Fable half: the form is discoverable by access and by `in`, invisible to `Object.keys` and
   `hasOwnProperty`, and Fable emits the members unmangled. The platform half is outside this
   repository and remains **assumed**. Settling it needs a workerd source reading or a deployed
   probe, not a generator change. If it comes back the wrong way, lane AA's form must be revisited
   and no generator change repairs it.

## Hygiene

8. **Fold the run-gate probes into lab goldens.** `tests/Xantham.Generator.RunGate/Probes.fs` is
   hand-written F# mirroring what lanes AA and AD then made the generator emit. Its own header says
   it should shrink once those landed. Both have landed.

## Carried from earlier waves

9. **The fidelity queue** — `TR018`'s 77, `TR023`'s remaining 38 (wave six took it from 46),
   `TR036`/`TR037`, `alignOperands`, the `FollowDepth` cutoff. Priced in
   `generator-cloudflare-recon.md` and `generator-tr018-recon.md`.
10. **Wave five's open items** — `inline` and demand-driven resolve, the `Fable.Core` binding gaps
    (`docs/.ai/fable-binding-gaps.md`), group sorting after a dependent, and the
    `EndOfStreamException` from a dying tsgo child.
11. **Positional `obj` provenance in findings.** Needed only for a consumer joining checker facts
    to body operations; the per-symbol table already addresses every declaration site uniquely. If
    a finding grows a field for it, the right field is a Wire node handle Harvest already holds,
    not `file:line`, which drifts.

## Shape of the wave, if it helps

Items 1 and 2 are consumer-facing and independent of each other. Item 4 is the only wire change on
the list and shares no file with the rest. Item 8 is small and touches only the run gate, so it
composes with anything. Item 7 blocks no code but should be answered before another wave builds on
lane AA's form. Items 3, 5 and 6 are each small enough to ride alongside a larger lane rather than
take one.
