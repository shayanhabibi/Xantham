---
category: Generator
audience: managing agent
title: Handover - lane R3, wave nine (price fidelity queue recon)
---

# Lane R3 — price fidelity queue verdict

Read-only. Base `f7d6c17` (wave nine prep), branch `worktree-gen-wave9-r3`. No files under
`src/` or `tests/` touched.

## Verdict: split. Open TR023/TR036/TR037/TR018; FollowDepth needs its own recon lane first.

The queue's first four entries are already fully attributed by
`docs/.ai/plans/generator-cloudflare-recon.md` and `docs/.ai/plans/generator-tr018-recon.md`.
Both documents remain accurate: I checked the mechanisms they cite against the current tree and
found the specific bugs they describe still present, byte for byte, in `Shape/Spec.fs`. Four
waves of "untouched" is real, not a stale claim.

`FollowDepth` (`RT001`) has never been measured — the cloudflare recon says so explicitly ("wave
three flagged 1,772 truncated types on fixture, thin headroom. Nothing measures it.") — and I
found the same is still true, except worse: the frontier `count` on `@cloudflare/workers-types`
is now **1,815**, not 1,772. That is not a small item and I am not settling it here; see §5.

## 1. `TR023` — 137 sites. Open it.

`Shape/Spec.fs:819`'s `objectRef` fallback (`NotAmongGeneratedDeclarations`) is unchanged since
the recon. Two of the recon's two `Model.LibBindings` rows are **already half-landed**: `"Error"`
maps to `("exn", 0, Some "...")` at `Model.fs:372` — that recovery shipped in some prior wave,
uncredited to this item. `"AsyncIterableIterator"` is still absent from the table (only
`AsyncIterable`, `AsyncIterator`, `AsyncGenerator`, `IteratorResult` are there, `Model.fs:388-394`).

**What's left to dispatch (Lane Q5 remainder):**
- Add `"AsyncIterableIterator", ("JS.AsyncIterable", 1, Some "...")` to `Model.LibBindings`
  (`Model.fs:353`). One row, loss note, ~1 site fixture / a few corpus-wide.
- The remainder (`Iterable`, `IterableIterator`, `BigUint64Array`, `WebAssembly.Module`) is
  **mapping declining to lie** — verified absent from `Fable.Core` 5.2.0 by the recon's own
  `fslangmcp check` probe. Not a lane. This is the bulk of the 137.
- No new finding case needed.

## 2. `TR036` — 72 sites. Open it.

`ErasedUnionArity` is still `4` at `Shape/Spec.fs:356` (moved from the recon's cited line but
unchanged in value). `Fable.Core` 5.2.0 ships `U2..U9`; the cap is half what's available.

**Two lanes ready:**
- **Raise `ErasedUnionArity` 4 → 9.** Touches the constant, two doc comments (`Render.fs`,
  `Model.fs`), and the literal `UnionTooWide(5, 4)` pin in `Findings.test.fs`. Pure constant
  change, recovers the majority of the arity-only slice (9 of 17 sites on cloudflare per the
  recon; re-measure corpus-wide after).
- **`taggedUnionShape` (`Shape/Spec.fs`) still refuses silently** when two arms share a tag value
  (`List.distinct values = values` gate) — confirmed still present. Needs (a) a raised finding on
  the refusal path instead of silence, and (b) a merge path for arms sharing a tag. Recovers ~7
  corpus-wide (recon §5.2/5.3).
- No new finding case needed for the arity raise; the tagged-union refusal fix should raise
  something on its currently-silent path — **manager should pre-declare that case** if this lane
  is dispatched, since it's a new observable outcome, not a rename.

## 3. `TR037` — 54 sites. Open it.

`erasedUnionRef` (`Shape/Spec.fs:1521`) still accumulates `refFindings` for **every** arm before
`List.distinct` collapses them (`findings <- findings @ refFindings` inside the per-arm map,
confirmed unchanged). This is the recon's Lane Q1 and it has not landed.

**One lane:** deduplicate by `(owner, rendered message)`, not by owner alone (`BodyInit`-shaped
owners carry several genuinely distinct messages that must all survive). Cheapest item in the
whole queue — a predicate change in one function, no golden line moves, only finding counts drop.
Recovers the bulk of `TR037` (mis-keyed duplicates, not real losses) and a chunk of `DO001` for
free as the same predicate. No new finding case needed.

## 4. `TR018` — 82 sites. Open it, but re-measure first — the recon's site list is stale.

The recon's causes C and D are **already landed**: `intersectionRef` (`Shape/Spec.fs:1010`) has
an `isPureCallback facts && facts.IndexInfos.IsEmpty` branch feeding `delegateRef` with a new
`IntersectionCallableFlattened` finding (cause C), and a `reducedOperand` branch handling `X & {}`
(cause D). Neither is in the recon text as landed — they were done by some other, unrecorded
lane.

I grepped current owners for `TR018` per fixture (via `symbols.jsonl`, grep only, no full read):

- `type-fest`'s remaining 8 sites are **exactly** the recon's cause-B list —
  `Opaque, InvariantOf, Tagged, SetOptional, SetRequired, SetReadonly, Except, FixedLengthArray`.
  Byte-for-byte match. This fixture's `TR018` is **fully cause B (type-parameter operand,
  designed refusal)** — nothing left to chase there.
- `@cloudflare/workers-types`'s remaining 21 are mostly cause-B shaped (`XOR`, `Fetcher`,
  `DurableObjectStub`, `RpcStub`, loopback stubs — all generic-over-type-param intersections) plus
  a cluster the original recon never saw: four `Workflow*.Config.retries` owners. New surface,
  unattributed.
- `animejs`'s remaining 33 are `LayoutAnimationParams`/`AutoLayoutParams` `.delay`/`.duration`
  (24, the *non-identical-union* residue of cause A — the identical-union slice the recon
  described as cheap is gone, so cause C/D landed took the easy half) plus 8
  `*.then(callback)(self)` sites that look cause-C-shaped but survived the fix — worth a fresh
  6-line reproducer before assuming they're the same mechanism.
- `solid-js`'s remaining 14 match the recon's cause-B family (`ParentProps`, `VoidProps`,
  `FlowProps`, `Component<...>` wrappers) closely enough to be that cause, not re-derived here.

**Recommendation:** dispatch a short remeasurement (not a full recon rewrite) against current
`TR018` sites before writing an implementation lane — cause B is confirmed unrecoverable for at
least `type-fest` and most of the rest, but the animejs `.then()` cluster and the four cloudflare
`Workflow*.retries` owners are new territory the existing document doesn't cover. Everything else
in the 82 is very likely floor (cause B). No new finding case needed for cause B (nothing to
raise); the `.then()` cluster may need one if it turns out distinct from `IntersectionCallableFlattened`.

## 5. `FollowDepth` / `RT001` — 7 sites reported, ~2,061 hidden. Do not open as-is; needs a recon lane first.

`RT001` (`ResolveTypeTable.FrontierNotResolved`) fires **once per fixture**, batched, on a
synthetic `<type-table>` symbol — so "7 sites" corpus-wide is the count of *fixtures that hit the
cutoff*, not the count of affected types. The finding's own `count` field, read per fixture from
`symbols.jsonl` (grep only):

| fixture | count stuck at depth 12 |
|---|---:|
| `@cloudflare/workers-types` | **1,815** |
| `solid-js` | 217 |
| `hoist-conditional-lab` | 14 |
| `chain-lab` | 11 |
| `setter-lab` | 2 |
| `array-shape-lab` | 1 |
| `type-fest` | 1 |

Corpus total ≈ 2,061 unresolved type instantiations, up from the cloudflare recon's own
observation of 1,772 at the time it was written — the number has grown, not shrunk, over four
waves of silence. `FollowDepth = 12` is documented at `Resolve.fs:10-13` as bounding "runaway
utility-type expansion, not recursion" — a deliberate cycle-safe cutoff, not a bug — but nothing
has ever sampled what's actually still growing in `@cloudflare/workers-types` at generation 12, or
whether raising the constant (cheap, like `ErasedUnionArity`) converges or just moves the wall.

This is **not** a Fable-boundary block like `TR055` — it's an internal resolve-tier constant, nothing
stops raising it and re-measuring. But I have no attribution for what the 1,815 types *are*, and
guessing would be exactly the "whole-job analysis" mistake `generator-fixtures.md` warns against.

**Ask for wave ten:** a small dedicated lane samples a handful of the `@cloudflare/workers-types`
frontier's generation-12 ids (`ctx`/`model` instrumentation or a debug dump — I don't have a cheap
way to do this read-only) to determine whether this is one recursive utility type ballooning, or
many independent ones, before pricing a constant raise or a smarter cutoff. Until that sample
exists, `FollowDepth` should stay on the worklist rather than being retired — the count moving the
wrong way over four waves is itself evidence it isn't settled the way `TR032`/`MB003` are.

## Dispatch order if the manager opens the queue

1. **Lane Q1 (`TR037` dedup)** — cheapest, `Shape/Spec.fs` `erasedUnionRef` only, no case needed.
2. **`ErasedUnionArity` 4→9 (`TR036`)** — pure constant, touches `Spec.fs`, `Render.fs`, `Model.fs`
   doc comments, `Findings.test.fs` literal. No case needed.
3. **`LibBindings` `AsyncIterableIterator` row (`TR023`)** — one row, `Model.fs`. No case needed.
4. **Tagged-union refusal fix (`TR036` remainder)** — `Shape/TaggedUnions.fs` /
   `Shape/Spec.fs:195-240`. Needs a pre-declared case for the now-loud refusal path.
5. **`TR018` remeasurement** — small recon pass, not an implementation lane yet; scope is the
   animejs `.then()` cluster and the four cloudflare `Workflow*.retries` owners only, since the
   rest of the 82 is very likely cause B (floor).

All four owned files (`Shape/Spec.fs`, `Model.fs`, `Shape/TaggedUnions.fs`, `Render.fs`) are
outside `Shape/Interfaces.fs` etc., so they don't collide with any lane working a single pass file
— but 1 and 2 and the `TR018` remeasurement all touch `Shape/Spec.fs`, so sequence them rather
than running concurrently, per this rule's own guidance on `Shape/Spec.fs` being shared.

## What I could not settle from aggregates

- The animejs `.then(callback)(self)` `TR018` cluster and the cloudflare `Workflow*.retries`
  cluster need a real reproducer built against a lab fixture before anyone can price them; I did
  not build one (read-only lane).
- `FollowDepth`'s 1,815 stuck types in `@cloudflare/workers-types` have no per-type attribution;
  answering "what are they" needs either a debug hook in `resolveTypeTable`'s walk or a targeted
  script over the compiler session outside the golden pipeline. I did not have a read-only way to
  get this without loading the fixture's full `symbols.jsonl`, which the rule forbids and which
  would not have answered the question anyway (the frontier ids never resolve, so they carry no
  `TypeFacts` and no symbol name to grep for).
