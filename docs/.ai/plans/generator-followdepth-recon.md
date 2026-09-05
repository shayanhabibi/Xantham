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

## Lane AV: are TR002/TR003 structurally dead, and what is the real margin

Lane AU's provenance measurement (`generator-frontier-provenance.md`) established that the walk
never converges and that stranded ids are ordinary generic containers, not utility types - but it
left the correctness question open: `TR002` (`TR.TypeNotResolved`) and `TR003`
(`TR.MissingFromTypeTable`) both read zero across the 50-fixture corpus at the committed
`FollowDepth 12`. This section makes them fire and measures the renderer's real reach against the
cutoff, rather than reasoning about the frontier.

### Method

`FollowDepth` (`Resolve.fs:60`) was edited in place, corpus regenerated with `dotnet fsi
build.fsx -- test --quick --update --no-run-gate --filter "<fixture>"`, and `dotnet fsi
build.fsx -- findings --key TR002` / `--key TR003` read after each regeneration. `FollowDepth` was
restored to 12 and the full corpus regenerated before the final commit; `git status` reported no
diff and `dotnet fsi build.fsx -- findings` reproduced the exact tier and `RT001` counts recorded
by lane AU, byte for byte.

### Q1 - `TR002` fires; `TR003` does not, at any depth tested

`TR002` is not structurally dead. Lowering `FollowDepth` on `@cloudflare/workers-types` makes it
fire starting at `FollowDepth 10` (count 1), and it climbs sharply as the cutoff drops further:

| `FollowDepth` | `TR002` count (cloudflare) |
| --- | --- |
| 12 (committed) | 0 |
| 11 | 0 |
| 10 | 1 |
| 9 | 1 |
| 8 | 5 |
| 7 | 17 |
| 6 | 74 |
| 4 | 170 |
| 2 | 491 |
| 1 | 1,862 |
| 0 | 4,349 |

`TR003` fired **nowhere**, on any fixture, at any `FollowDepth` from 12 down to 0 (0 means the walk
follows only the seed generation). `Resolve.fs`'s own header comment states the tier's invariant:
"every type id a `TypeFacts` refers to is in the table or recorded in `NotFollowed` with its
reason" - and `NotFollowed` is populated unconditionally for everything the cutoff strands
(`Resolve.fs:918`), so a type id a shaping pass looks up is always in one map or the other by
construction. `TR003` exists for the case that invariant fails to hold (a walk bug that leaves an
id neither resolved nor recorded), not for anything `FollowDepth` controls. It reads as dead
against every input this lane could vary; retiring it is a call for the managing agent, since a
lane cannot prove a negative for every possible walk bug, only that depth is not the lever.

### Q2 - Renderer's real reach, per fixture

The same sweep against `animejs`, `solid-js` and `type-fest` (`npmFixture` targets in
`Pipeline.test.fs`) locates the renderer's deepest actual reference by generation - the point
where dropping the cutoff one generation further starts stranding something a rendered type
reference actually needs:

| fixture | first `TR002` fires at `FollowDepth` | renderer's deepest reach (generation) | margin to `FollowDepth 12` |
| --- | --- | --- | --- |
| `@cloudflare/workers-types` | 10 | 11 | **1** |
| `solid-js` | 8 | 9 | 4 |
| `animejs` | 6 | 7 | 5 |
| `type-fest` | 5 | 6 | 6 |

(Deepest reach = the `FollowDepth` that first strands something needed, plus one - stranding
happens at `depth > FollowDepth`, so a cutoff of 10 stranding a generation-11 reference means the
renderer's deepest touch is generation 11.)

The single `@cloudflare/workers-types` site that fires at `FollowDepth 10` is
`Ai_Cf_Deepgram_Nova_3_Output.Results.Channels.Item.Alternatives.Item.words` - a member several
levels into a synthesized anonymous-object chain inside the Workers AI response bindings
(array-of-array-of-object nesting from the `Ai` namespace's model output types), not a generic
container. It is one site.

### Q3 - Margin is not uniform, and cloudflare is the tight one

The margin is **1 generation** for `@cloudflare/workers-types` at the committed `FollowDepth 12` -
not the 6-generation margin the dispatch brief hypothesized from `type-fest`'s figure. `type-fest`,
`animejs` and `solid-js` all carry margins of 4-6 generations; `@cloudflare/workers-types` is the
outlier, and it is the fixture the corpus already flags as the stranding-heaviest one. The margin
is thin because of one deeply-nested AI response type, not because the frontier's steady-state
stranding (lane AU's finding) is close to being touched - the 1,815 steady-state stranded ids
(`Array`, `ReadonlyArray`, `Promise`, etc.) are a different, unrelated set from the one site this
lane found at the boundary. Lowering `FollowDepth` by even 2 (to 10) is enough to widen a real,
rendered member in the committed corpus; raising it further than 12 buys no more correctness (the
frontier is in steady state per lane AU) but the current value is not comfortably clear of the
renderer's real use either.

### What this retires and what it does not

- **`TR003` retires as dead code** - not provably unreachable in general, but unreachable through
  the one lever (`FollowDepth`) this risk was ever about, at every value tested.
- **`TR002` does not retire.** It is live, has a real (if currently empty) firing corpus, and the
  corpus's margin to it is 1 generation on the fixture that matters most. The 1,815-member risk
  carried for four waves was answered "zero, and correctly zero" by lane AU's frontier work; this
  lane shows the *renderer's* separate margin is thin enough that the risk is not closed, only
  currently unrealized.
- No code changed under `Shape/`, `build.fsx`, or `README.md`. `Resolve.fs`'s only durable change
  is the restored `FollowDepth = 12` (the edits made while sweeping were reverted before commit).
