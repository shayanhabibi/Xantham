---
category: Generator
audience: managing agent
title: FollowDepth frontier recon
---

# FollowDepth frontier recon

Sampling only. No repair, no verdict on whether `FollowDepth` should change.

## Method

`resolveTypeTable`'s cutoff branch (`Resolve.fs:746`, inside `walk`) already holds the full
`TypeResponse` of every type it declines to follow. A new `dumpFrontier` function
(`Resolve.fs:14`) writes each cutoff-branch `TypeResponse`, one per line, to the path named by
`XANTHAM_FRONTIER_DUMP` via `ProtoJson.serialize` (the same serializer the wire protocol already
uses for this record). The variable is unset by default, so `dumpFrontier` is a no-op and the
default path is unchanged: corpus tier counts stayed `exact 495, ergonomic 1552, widened 786,
escape 193` and `RT001` stayed 7, measured with `dotnet fsi build.fsx -- findings` before and
after.

Each measurement below reran `dotnet fsi build.fsx -- test --quick --no-run-gate --filter
"cloudflare"` with `XANTHAM_FRONTIER_DUMP` pointed at a scratch file and `FollowDepth` edited to
the value under test, then reverted to 12 before the final gate and commit. The golden comparison
fails at 16 and 20 since the committed golden was generated at `FollowDepth 12` - expected, and
irrelevant to the measurement, which reads the dump rather than the test result.

## Frontier counts

| `FollowDepth` | raw frontier lines | unique type ids | `test` stage wall-clock |
| --- | --- | --- | --- |
| 12 | 5,445 | 2,371 | 15.9s / 21.4s (two runs) |
| 16 | 5,544 (+99) | 2,392 (+21) | 28.9s |
| 20 | 5,544 (+0) | 2,397 (+5) | 23.9s |

Unique-id count converges: +21 unique ids from 12→16, +5 from 16→20, 0 additional raw lines from
16→20. Wall-clock time does not track depth in this range - 21.4s, 28.9s, 23.9s across 12/16/20 -
noise between runs (shared machine, other stages sharing the process) dominates whatever per-depth
cost exists. The frontier reruns from scratch every walk generation regardless of cutoff, so a
raised cutoff adds a bounded number of extra generations rather than compounding cost.

The unique-id count (2,371 at depth 12) is lower than the 1,815-plus estimate the dispatch brief
carried forward from lane R3's read; that estimate predates this dump and cannot be reconciled
further without re-deriving it, since checker ids are per-run and the earlier number was never
measured this way.

## Flag distribution (FollowDepth 12, `@cloudflare/workers-types`, 5,445 frontier entries)

`TypeFlags`:

| flag | count | share |
| --- | --- | --- |
| `Object` (1048576) | 4,059 | 74.5% |
| `TypeParameter` (524288) | 687 | 12.6% |
| `Union` (134217728) | 537 | 9.9% |
| `Conditional` (67108864) | 120 | 2.2% |
| `IndexedAccess` (33554432) | 42 | 0.8% |

`ObjectFlags`, restricted to the 4,059 entries flagged `Object`:

| flags | count | share of Object entries |
| --- | --- | --- |
| `Anonymous \| Instantiated \| CouldContainTypeVariablesComputed \| CouldContainTypeVariables` | 3,156 | 77.8% |
| `MembersResolved \| Anonymous \| Instantiated \| CouldContainTypeVariablesComputed \| CouldContainTypeVariables` | 75 | 1.8% |
| `Mapped \| Instantiated \| CouldContainTypeVariablesComputed \| CouldContainTypeVariables` | 45 | 1.1% |
| `Reference` | 783 | 19.3% |

The dominant object-flag combination (77.8% of `Object`-flagged frontier entries) is an anonymous,
instantiated type carrying an unresolved type variable - the signature of a generic utility type
applied to arguments (`Pick<T, K>`, `Partial<T>`, and similar), not a hand-declared interface.

## Target concentration

`Target` is present on exactly the 783 `Reference`-flagged entries (the `ObjectType`/
`TypeReference` this instance points at), spread across only 22 distinct target ids. The top ids:

| `Target` id | count |
| --- | --- |
| 65 | 348 |
| 79 | 75 |
| 393, 451, 601, 627, 631 | 27 each |
| 7552, 7566, 7654 | 24 each |

44% of targeted entries (348/783) point at a single target id. These are TypeScript checker ids,
assigned in encounter order within one compiler session - they are not stable across runs and
cannot be resolved to declaration names from this dump alone; doing so would need a further wire
round trip (a symbol/declaration lookup keyed by these ids) that was out of scope for a sampling
pass. Named sample for a later lane to chase: target id 65 is the single largest concentration
point in `@cloudflare/workers-types` at `FollowDepth 12`, worth a targeted `getSymbolAtLocation` or
equivalent lookup before spending more on this frontier.

## Reading

The concentration (44% of targeted entries on one id, 74.5% of all entries carrying `Object` with
77.8% of those the same instantiated-anonymous-with-unresolved-type-variable pattern) is more
consistent with one or a few utility types instantiated at many argument combinations than with
many independent named types individually widening.

Convergence between 16 and 20 (+5 unique ids, +0 raw lines) against the larger jump from 12 to 16
(+21 unique ids, +99 raw lines) suggests the count is settling rather than open-ended, though the
three points sampled are not enough to say where it flattens exactly.
