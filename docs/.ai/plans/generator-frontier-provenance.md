---
category: Generator
audience: managing agent
title: Frontier provenance recon
---

# Frontier provenance recon

Measurement only. No repair. Four questions, answered from an instrumented run of
`@cloudflare/workers-types`; a fifth answer arrived unasked and corrects the numbers in
`generator-followdepth-recon.md`.

## Correction: lane R4 measured three walks at once

The Expecto suite for one npm fixture starts **three walks** over the same package - one for
`generates the committed goldens`, two for `generation is deterministic run to run` - and Expecto
runs the two test cases in parallel. `Resolve.fs` already states that a checker id is assigned in
the order answers arrive, so an id identifies a different type in each walk.

Lane R4's dump appended all three walks to one file. Every absolute count in
`generator-followdepth-recon.md` is therefore three walks superimposed, and the distinct-target
count is inflated further by the ids that drift between walks. The proportions survive; the counts
do not.

| quantity | R4 reported | one walk |
| --- | --- | --- |
| raw frontier lines | 5,445 | 1,815 |
| unique type ids | 2,371 | 1,815 |
| entries carrying `Target` | 783 | 261 |
| distinct `Target` ids | 22 | 12 |
| entries whose `Target` is 65 | 348 | 116 |

`5,445 = 3 x 1,815`, `783 = 3 x 261`, `348 = 3 x 116`. The 12 distinct targets per walk split into
seven whose ids are stable across walks (65, 79, 393, 451, 601, 627, 631) and five whose ids drift;
three walks of five drifting ids plus seven stable ones is R4's 22.

The flag distribution R4 published is unaffected - 74.5% `Object`, 12.6% `TypeParameter`, 9.9%
`Union`, 2.2% `Conditional`, 0.8% `IndexedAccess`, and 77.8% of the `Object` entries anonymous and
instantiated - and reproduces exactly on a single walk (1,353 / 229 / 179 / 40 / 14, and 1,052 of
1,353).

## Method

`Resolve.fs` carries a per-walk `Trace`: the channels that discovered each type id, and the
generation each id reached the frontier at. `channel` wraps each discovery site in `deriveFacts`
and `deriveStructure` and hands its argument back unchanged. At the cutoff, `dumpFrontier` writes
`<dump>.<tag>.jsonl` (the frontier responses) and `<dump>.<tag>.prov` (an `ID` line per type id the
walk reached, and a `TARGET` line per distinct `Target` on the frontier naming its declaration).
`XANTHAM_FRONTIER_DUMP` unset leaves `Trace.start` returning `None` and every hook inert.

Default path proven unchanged: `dotnet fsi build.fsx -- findings` before and after the change
diffs empty - `exact 495, ergonomic 1552, widened 786, escape 193`, `RT001` 7 in both.

All three walks of the instrumented run produced identical channel counts and identical shape
counts, so every number below is one walk and is reproducible.

## Q1 - Provenance: the firehose is members and call signatures

1,815 stuck ids, tagged by the channel that discovered them (an id discovered on two channels is
counted on both):

| channel | stuck ids | share |
| --- | --- | --- |
| `member-type` | 645 | 35.5% |
| `call-signature-parameter` | 583 | 32.1% |
| `call-signature-return` | 276 | 15.2% |
| `call-signature-type-parameter` | 229 | 12.6% |
| `union-members` | 62 | 3.4% |
| `type-arguments` | 53 | 2.9% |
| `indexed-access` | 50 | 2.8% |
| `index-info` | 9 | 0.5% |

`deriveStructure` accounts for **90.9%** of the frontier (1,650 of 1,815 once the 52 ids on two
signature channels are counted once).

**`target` produced zero stuck ids.** So did `base-types`, `alias-type-arguments`,
`intersection-members`, `constraint`, `default`, `conditional-branch`, `index-operand` and all
three `construct-signature-*` channels. The `target` channel tags 445 discoveries over the whole
walk, of which 42 are first sightings, 40 of them in generation 0; from generation 3 it discovers
nothing new at all.

The hypothesis the dispatch carried - that `target` is the firehose and the tuple precedent at
`Resolve.fs:436` is the shape of the repair - **is disproved**. Excluding `target` from `discovered`
would remove nothing from the frontier.

Crossing channel with the frontier's own populations sharpens it. The dominant population, the
1,052 anonymous-instantiated object types that are 77.8% of the `Object` entries:

| channel | count |
| --- | --- |
| `member-type` | 615 |
| `call-signature-parameter` | 400 |
| `union-members` | 37 |

The 261 `Reference` entries - the `Array<T>` family the target concentration is made of - arrive
the same way: `call-signature-return` 158, `call-signature-parameter` 42, both 12, `union-members`
25, `type-arguments` 24. All 229 type parameters arrive on `call-signature-type-parameter`.

## Q2 - Structural distinctness: total collapse, and it means nothing

The 116 entries whose `Target` is 65 carry **one** distinct `TypeResponse` between them once `Id`
is dropped. Not "collapse heavily" - identical, byte for byte:

```json
{"flags":1048576,"isThisType":false,"isTupleType":false,"objectFlags":4,"symbol":1150,"target":65}
```

The whole 1,815-entry frontier collapses to **156** distinct shapes, 57 of which occur once. The
largest is 176 anonymous unions that are all the same four fields.

This is the answer to "is dedup by shape available cheap", and it is **no**. A `TypeResponse` is a
handle, not a structure. It carries no type arguments, no union members and no properties, so
`Array<string>` and `Array<Response>` are the same shape at this point and collapsing them
substitutes one for the other. The 116 are 116 genuinely different array element types.

The dispatch's constraint holds and decides it: type arguments arrive from `getTypeArguments`,
issued only while deriving a type, so a frontier type has never had its arguments fetched. Any
normalisation keyed on "target plus arguments" must first spend one `getTypeArguments` round trip
per frontier entry - 261 of them for the `Reference` entries, and nothing at all for the other
1,554, which carry no `Target` to key on.

## Q3 - Target 65 is `Array`

One `getTargetOfType` through a carrying reference, then `getSymbolOfType`. The ids on `Target` are
response fields rather than registered handles, so the target is re-requested through the reference
before anything else names it.

| `Target` | entries | name | declared in |
| --- | --- | --- | --- |
| 65 | 116 | `Array` | `lib.es5.d.ts` |
| 79 | 25 | `ReadonlyArray` | `lib.es5.d.ts` |
| 7638 | 24 | `ArrayIterator` | `lib.es2015.iterable.d.ts` |
| 4534 | 21 | *(anonymous, tuple)* | - |
| 7761 | 12 | `ConcatArray` | `lib.es5.d.ts` |
| 3245 | 9 | `Promise` | `lib.es5.d.ts` |
| 6751 | 9 | `AsyncIterableIterator` | `lib.es2018.asynciterable.d.ts` |
| 393 | 9 | `WritableStream` | `@cloudflare/workers-types/index.d.ts` |
| 451 | 9 | `ReadableStream` | `@cloudflare/workers-types/index.d.ts` |
| 601 | 9 | `ReadableStreamDefaultReader` | `@cloudflare/workers-types/index.d.ts` |
| 627 | 9 | `ReadableWritablePair` | `@cloudflare/workers-types/index.d.ts` |
| 631 | 9 | `WritableStreamDefaultWriter` | `@cloudflare/workers-types/index.d.ts` |

The concentration is not a utility type. It is the array family, the iterator family and `Promise`
out of the compiler lib, plus the five entry-package stream generics. R4's reading - "one or a few
utility types instantiated at many argument combinations" - was the right shape of answer about the
wrong types: these are ordinary generic containers, and each of the 116 `Array` references stands
for a different element type.

## Q4 - Depth profile: the walk never converges

Ids first reaching the frontier, per generation:

| gen | ids | gen | ids | gen | ids |
| --- | --- | --- | --- | --- | --- |
| 0 | 1,198 | 5 | 1,228 | 10 | 1,954 |
| 1 | 2,091 | 6 | 968 | 11 | 1,941 |
| 2 | 1,286 | 7 | 1,494 | 12 | 1,773 |
| 3 | 647 | 8 | 1,465 | 13 | **1,815 (stuck)** |
| 4 | 1,352 | 9 | 1,409 | | |

20,621 ids reached in thirteen generations. Every stuck id is first discovered at generation 13, by
construction: an id in an earlier frontier is derived in that generation and never re-entered.

The generation curve is flat. Generation 12 derives 1,773 types and hands 1,815 to the cutoff - the
walk is expanding as fast at the boundary as it was at generation 4, and that is what wave ten's
result means. Raising `FollowDepth` recovers nothing not because the frontier is emptying but
because the frontier is a **steady state**: each generation consumes ~1,800 types and produces
~1,800 more, so the cutoff strands the same-sized set wherever it falls.

The channel mix reaches that steady state at about generation 10 and does not move afterwards.
The channels absent from the stuck set are all early-walk phenomena: `target` stops discovering at
generation 3, `base-types` and `alias-type-arguments` at generation 4, `construct-signature-*` and
`intersection-members` shortly after. From generation 10 on, the walk is `member-type`,
`call-signature-parameter`, `call-signature-return` and `call-signature-type-parameter`, in that
order, and nothing else of consequence.

So there is no generation at which moving the cutoff changes what is stuck, and no risk in moving
it either - the set is the same size and made of the same channels at 12, at 16 and at 20.

## What this prices

Nothing on the `target` channel, and nothing keyed on shape. The frontier is produced by
`deriveStructure` following the members and call signatures of types it has already reached, and
1,815 of the types it reaches at the boundary are ordinary members and parameters of ordinary
declarations. A repair has to decide either that some of those members need not be followed, or
that the walk should stop expanding on some criterion other than depth. Both are outside this
lane.

One measurement a follow-up should take before either: how many of the 20,621 ids the walk reaches
are semantically the same type re-minted under a fresh id. `TypeResponse` cannot answer it - the
Q2 collapse shows the handle carries too little - so it needs `getTypeArguments` on the frontier's
261 `Reference` entries and a symbol round trip on the rest.
