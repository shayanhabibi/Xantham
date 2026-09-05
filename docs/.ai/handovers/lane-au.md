---
category: Generator
audience: managing agent
title: Lane AU - frontier provenance
---

# Lane AU - frontier provenance

Branch `worktree-gen-wave11-au`, based on `3b7cacf`. Recon only; no repair, no finding codes added,
no goldens moved. Full measurement in `docs/.ai/plans/generator-frontier-provenance.md`.

## What changed in the tree

`src/Xantham.Generator/Resolve.fs` only. The frontier hook now carries a per-walk `Trace` - the
channels that discovered each type id, and the generation each id reached the frontier at - and
writes `<dump>.<tag>.jsonl` beside `<dump>.<tag>.prov`. `Trace.start` returns `None` while
`XANTHAM_FRONTIER_DUMP` is unset, which leaves `channel` and `dumpFrontier` inert.

Default path proven unchanged: `dotnet fsi build.fsx -- findings` before and after diffs empty.
`exact 495, ergonomic 1552, widened 786, escape 193`, `RT001` 7, both sides.

## Correction to lane R4

An npm fixture's suite runs **three** walks over the package, two of them concurrently, and checker
ids are assigned in arrival order, so ids from one walk do not identify types in another. R4's dump
appended all three. Every absolute count in `generator-followdepth-recon.md` is three walks
superimposed: 5,445 raw lines is 3 x 1,815; 783 targeted entries is 3 x 261; 348 on target 65 is
3 x 116; 22 distinct targets is 12 per walk plus the five per walk whose ids drift. The flag
proportions R4 published are correct and reproduce on one walk.

## Answers

- **Q1.** The firehose is `deriveStructure`, at 90.9% of the 1,815 stuck ids: `member-type` 645
  (35.5%), `call-signature-parameter` 583 (32.1%), `call-signature-return` 276 (15.2%),
  `call-signature-type-parameter` 229 (12.6%). Then `union-members` 62, `type-arguments` 53,
  `indexed-access` 50, `index-info` 9. **`target` produced zero stuck ids** - it stops discovering
  anything new at generation 3 - and so did `base-types`, `alias-type-arguments`,
  `intersection-members`, `constraint`, `default`, `conditional-branch` and every
  `construct-signature-*` channel. The tuple-target precedent at `Resolve.fs:436` prices nothing
  here.
- **Q2.** The 116 entries on target 65 are **one** shape - identical field for field once `Id` is
  dropped. The whole frontier collapses 1,815 to 156. Dedup by shape is not available: a
  `TypeResponse` carries no type arguments, so `Array<string>` and `Array<Response>` are the same
  shape and collapsing them substitutes one for the other. Keying on "target plus arguments" costs
  one `getTypeArguments` round trip per `Reference` entry (261 of 1,815) and answers nothing for
  the other 1,554, which carry no `Target`.
- **Q3.** Target 65 is **`Array`**, `lib.es5.d.ts`. Then `ReadonlyArray` 25, `ArrayIterator` 24, an
  anonymous tuple target 21, `ConcatArray` 12, and `Promise`, `AsyncIterableIterator`,
  `ReadableStream`, `WritableStream`, `ReadableStreamDefaultReader`, `ReadableWritablePair`,
  `WritableStreamDefaultWriter` at 9 each. Generic containers, not utility types.
- **Q4.** Every stuck id is first discovered at generation 13, by construction. The useful profile
  is the generation curve, and it is flat: 1,198 / 2,091 / 1,286 / 647 / 1,352 / 1,228 / 968 /
  1,494 / 1,465 / 1,409 / 1,954 / 1,941 / 1,773, then 1,815 stranded. The walk expands as fast at
  the boundary as in the middle. The frontier is a steady state, which is why wave ten saw the
  count flatten rather than fall - the cutoff strands the same-sized set wherever it falls, so
  moving it carries neither gain nor risk. The channel mix settles by generation 10 and the
  channels absent from the stuck set are all early-walk phenomena.

## Hypotheses disproved

The `target` firehose hypothesis, and cheap dedup by shape. Both closed.

## Reproducing

```
XANTHAM_FRONTIER_DUMP=<prefix> dotnet fsi build.fsx -- test --quick --no-run-gate --filter "cloudflare"
```

Three `<prefix>.<tag>.jsonl` / `<prefix>.<tag>.prov` pairs land, one per walk; they agreed on every
count above. `.prov` lines are tab separated: `ID <id> <generation> <stuck 0|1> <channel=count;...>`
and `TARGET <targetId> <entries> <resolvedId> <name> <declaration>`.
