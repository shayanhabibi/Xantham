---
category: Generator
audience: managing agent
title: Dispatch - generator wave eleven
integration-branch: worktree-generator-wave-eleven
---

# Generator wave eleven — the frontier, settled

One item, two lanes, both recon. The user authorised a follow-up on wave ten's closing
recommendation — normalising utility-type instantiation on target and arguments — with the
instruction to attack the frontier with precision. The recommendation was wrong, and finding
out cost two lanes.

`worktree-generator-wave-eleven` forks wave ten at `3b7cacf`. Wave ten is complete, gated and
publishable; nothing here touches its packaging or its documentation.

## The question, as it stood

`RT001` 7 hides 1,815 type ids per walk that `resolveTypeTable`'s `FollowDepth = 12` cutoff
declines to follow, in `@cloudflare/workers-types`. Four waves carried this as the generator's
one correctness risk — members widening to `obj` unseen — without ever measuring it.

Wave ten's lane R4 measured the frontier's composition and found the count flat across
`FollowDepth` 12, 16 and 20. It read the frontier as generic utility types instantiated many
ways, and recommended normalising on target and arguments.

## What the lanes found

### Lane AU — the recommendation is wrong

Provenance, measured by tagging every discovery channel. Of the 1,815 stranded ids, 90.9% come
from `deriveStructure`: member types 35.5%, call-signature parameters 32.1%, returns 15.2%,
type parameters 12.6%. **The `target` channel strands nothing.** The tuple-target precedent at
`Resolve.fs:436` prices no repair here.

Dedup is priced out rather than in. The 1,815 collapse to 156 distinct shapes, but only because
a `TypeResponse` carries no type arguments — `Array<string>` and `Array<Response>` are the same
shape on the wire. Merging them would merge different types. Keying on target and arguments
reaches 261 of the 1,815; the other 1,554 carry no `Target` at all.

The stranded types are generic containers, not utility types: target 65 is `Array`, then
`ReadonlyArray`, `ArrayIterator`, `ConcatArray`, `Promise`, `ReadableStream`, `WritableStream`.

The walk does not converge. New ids per generation run 1198, 2091, 1286, 647, 1352, 1228, 968,
1494, 1465, 1409, 1954, 1941, 1773, with no decay, and hand 1,815 to the cutoff. The stranded
set is a **steady state** — the cutoff strands a set of the same size wherever it falls, which
is why wave ten measured it flat rather than falling.

**Two carried numbers corrected.** R4's 2,371 counted three superimposed walks — an npm
fixture's suite runs three, and checker ids are arrival-ordered, so they do not identify types
across walks. The per-walk figure is 1,815, which is lane R3's original number: R3 was right and
the reconciliation lands in its favour.

### Lane AV — the risk is unrealized, not closed

`Shape/Spec.fs:923` widens to `obj` and raises `TR002` where a rendered reference reads a
stranded id, `TR003` where the id is absent from the table entirely. **Both read 0 across all
50 fixtures**, which would retire the item.

The zero is real, and thinner than it looks.

`TR002` is a live code, not a dead one: it first fires at `FollowDepth 10` with a count of 1,
reaches 74 at depth 6 and 4,349 at depth 0. So the renderer does reach types the cutoff can
strand — it simply does not reach them at 12.

| fixture | renderer's deepest reach | margin to `FollowDepth 12` |
| --- | ---: | ---: |
| `@cloudflare/workers-types` | 11 | **1** |
| `solid-js` | 9 | 4 |
| `animejs` | 7 | 5 |
| `type-fest` | 6 | 6 |

`TR003` retires: `resolveTypeTable`'s own invariant puts every referenced id into `Types` or
`NotFollowed`, so the depth cutoff cannot raise it. It guards against a walk bug and should be
kept for that.

`TR002` does not retire. It is 0 today on a one-generation margin in the one package that
matters, driven by a single deep site — a nested synthesized-anonymous chain under
`Ai_Cf_Deepgram_Nova_3_Output…Alternatives.Item.words` — rather than by the steady-state
stranded set.

## What this means

**The frontier's size was never the risk, and normalising it was never the repair.** The size
is a steady state that no cutoff choice changes. The risk is the *margin* between the
renderer's deepest reach and the cutoff, and that margin is one generation on
`@cloudflare/workers-types`.

So `FollowDepth` is the lever after all — not to shrink the stranded set, which is what wave
ten tested and correctly rejected, but to buy margin. Raising it to 20 takes cloudflare's
margin from 1 generation to 9. Lane AU established that raising it strands a set of the same
size, so the change buys headroom without moving the loss.

**It is not free.** Changing `FollowDepth` moves goldens — `RT001`'s payload carries the
constant — so the change regenerates all 50 fixtures and has to be measured, not assumed.
Wall-clock across 12, 16 and 20 was noise-dominated in R4's measurement, so the cost is
probably small, but "probably" is not a measurement.

**Recommended as a decision, not dispatched.** It moves output across the whole corpus at the
moment wave ten's alpha is ready to publish, and today's realised loss is zero. Sequencing it
after the alpha costs nothing; sequencing it before costs a regeneration and a re-review of
what moved. That is the user's call.

## Instrumentation kept

`Resolve.fs` carries lane AU's per-walk `Trace`, gated behind `XANTHAM_FRONTIER_DUMP` and inert
when unset. It writes `<dump>.<tag>.jsonl` and `<dump>.<tag>.prov` per walk, recording each id's
generation, whether the cutoff stranded it, and which channels discovered it. Every measurement
above is reproducible from it. The `channel` wrapper returns its input unchanged when no trace
is running, so the walk's semantics are untouched.

Keeping it is the point: this number grew for four waves with nothing watching it, and the
margin — the number that actually matters — now has a way to be re-measured whenever a rung is
added.

## Gate and corpus

Unchanged, as designed. Both lanes were measurement lanes; a moved count would have been the
defect.

| | wave ten `3b7cacf` | composed |
| --- | ---: | ---: |
| generator tests | 467 | 467 |
| wire tests | 90 | 90 |
| run gate checks | 257 | 257 |
| exact / ergonomic / widened / escape | 495 / 1552 / 786 / 193 | 495 / 1552 / 786 / 193 |
| `RT001` | 7 | 7 |
| exit code | 0 | 0 |

## For the next worklist

1. **Raise `FollowDepth`** to restore margin. Priced above; needs a regeneration and a
   measurement of what moves, not a constant change alone.
2. **Record the margin, not the frontier count.** `RT001`'s 7 says nothing a reader can act on.
   The number worth watching is the gap between the renderer's deepest reach and the cutoff,
   per fixture, and it should be measured whenever a rung is added.
3. **The fidelity queue**, deferred from wave ten: `TR037` 54, `TR036` 72, `TR023` 137,
   `TR018` 82, and group emission ordering. Lane R3's dispatch order in
   `docs/.ai/handovers/lane-r3.md` still stands and needs no re-derivation.
