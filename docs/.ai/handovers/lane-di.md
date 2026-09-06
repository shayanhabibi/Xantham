# Lane DI — bounding the frontier's width, and what it costs `lib.dom`

Wave fifteen, lane DI. **Process note first:** the dispatch initially arrived without its task
text (a bare measurement table only), and the first search for a working tree landed in the
wrong worktree entirely. Work resumed in the correct location,
`.claude/worktrees/gen-wave15-di` on `worktree-gen-wave15-di`, once the coordinator's follow-up
supplied the missing task and the correction. Recorded here per the branch-discipline convention
lane CN's handover established.

## 1. The rule this implements

`Resolve.fs`'s breadth-first walk already bounds recursion depth (`FollowDepth = 20`): past
generation 20 the remaining frontier is recorded as not followed, one aggregate finding
(`RT.FrontierNotResolved`) rather than one per type. Depth alone does not bound the walk's
**width**: a generic method whose return type applies its own enclosing interface to a fresh
method-level type parameter — `Foo<T>.map<U>(...): Foo<U>`, the `Array<T>` shape — mints a
distinct instantiation id every generation, and each of those instantiations rediscovers the same
members again under its own fresh parameter. Width doubles generation over generation at any
depth, independent of the depth cutoff, which is why `@cloudflare/workers-types`-scale corpora
stayed bounded while a hand-built pathological shape (`.probe/half2`, `.probe/full`) and `lib.dom`
did not.

The fix, `FollowWidth = 4096` (`src/Xantham.Generator/Resolve.fs`): once one generation of the
frontier exceeds this many distinct types, the **whole generation** is recorded as not followed
(`RT.FrontierTooWide`, `RT003`) rather than derived, mirroring the depth cutoff's shape exactly —
one aggregate finding, a `notFollowed` map entry per deferred type, the walk halts along that
branch rather than widening it type by type.

## 2. The bug this lane found in its own first draft — this is the load-bearing discovery

The first implementation did **not** defer the whole generation. It sorted the frontier by
`TypeResponse.Id` and admitted the first `FollowWidth` types, deferring only the tail past the
cutoff (`List.splitAt FollowWidth fresh`). This is the design the task description asked for
("copying the mechanism" read, at first, as "cap the count, keep the rest") and it produced
believable numbers — until the new `frontier-width-lab` fixture's own committed-golden and
run-to-run-determinism tests started failing, the second on a plain re-run against a golden the
first run had *just* written moments earlier.

The cause is already on record two lines above the depth cutoff in the same file, in a comment
that predates this lane: **a checker id is assigned in the order answers arrived**, which is why
the depth cutoff's own finding keys on nothing but a count, never a type's id. Admitting "the
first N by id" silently promoted that arrival-order id from an internal lookup key (harmless -
the same logical type gets a different id in a different run, and nothing downstream reads the
number) to a **selection criterion that changes which types resolve and which widen** - and that
selection is exactly as unstable as the id it sorts on. Two independent runs of the identical
input frontier-width-lab fixture produced two different admitted subsets, hence two different
rendered outputs, hence a failing determinism test and a failing golden match against a golden
written from a third such run.

The fix is not a better sort key; the frontier's own top-of-function comment already explained why
one isn't needed for the depth cutoff (order never affected which types got processed, only in
what order they were logged). It affects correctness only where a **subset** gets chosen, so the
subset choice was removed rather than repaired: `List.splitAt` is gone; a generation past
`FollowWidth` is deferred **entirely**, the same all-or-nothing shape the depth cutoff already
uses, admitting nothing to derive nothing that depends on an id's arrival order. `Resolve.fs`'s
sort-by-id on the frontier remains, but now purely for a readable trace, documented as such.

## 3. What this costs — reported at the corrected, deterministic number, not the withdrawn one

The all-or-nothing design is considerably more conservative than the withdrawn partial-admission
one: once a generation crosses the line, none of it resolves, where the withdrawn design would
have resolved the first 4096 of it and only deferred the remainder. This lane's first (wrong)
pass reported gentler numbers for `.probe/half2`/`.probe/full`/`lib.dom` on the strength of that
withdrawn design; the honest numbers below are all from the corrected, verified-deterministic
version (each pair confirmed identical across two independent process runs).

| Package | Frontier expansions | Table size (resolved) | Widened by RT003 |
|---|---|---|---|
| `.probe/half2` (unbounded, no cutoff) | 39,908 | 28,644 | n/a |
| `.probe/half2` (bounded) | 14,308 | 9,956 | 4,352 (one generation) |
| `.probe/full` (unbounded, no cutoff) | 62,932 | 44,500 | n/a |
| `.probe/full` (bounded) | 15,700 | 11,092 | 4,608 (one generation) |

The bounded walk halts far earlier than the withdrawn design's partial-admission would have,
because deferring the whole oversized generation stops it from spawning a further, larger
generation next round — the withdrawn design's admitted 4096 would have kept doubling for one
more round before the cutoff caught up. Fewer total expansions, fewer resolved, all in one
aggregate `RT003` finding per package (one triggering generation, not several).

## 4. `lib.dom` — completes, but the honest trade is severe

Config: `{"groups": {"typescript/lib": "ship"}, "lib": ["dom"]}`. Prior state: killed at 171s,
6,025MB. Under the bounded, corrected walk: **completes in ~11–16s, ~614MB peak**, monitored via
`Get-CimInstance Win32_Process` process-tree tracking with a 300s hard external bound (never hit).
Confirmed identical across three independent runs (`frontier-expansions=12999 table-size=2781
max-width=10218`, byte-identical each time).

The width cutoff fires exactly once, at a generation of **10,218** candidate types — over double
`FollowWidth`, and the whole generation is deferred. `RT001` (the depth cutoff) never fires; the
width cutoff catches the runaway growth long before depth 20. Widening arithmetic, summed
directly off `symbols.jsonl` (internally consistent: table size + widened = total expansions):

| | Count | Share of 12,999 total |
|---|---|---|
| Table size (resolved) | 2,781 | 21.4% |
| Widened by RT003 (width cutoff) | 10,218 | 78.6% |
| Widened by RT001 (depth cutoff) | 0 | 0% |

**This is materially worse than what a partial-admission design would have reported** (this
lane's withdrawn first draft would have put the widened share near 41%, not 79%) — the price of
removing the subset selection that made the walk nondeterministic is that a package whose first
runaway generation is this wide (10,218, over double the 4,096 cutoff) loses that whole generation
rather than the ~6,122 excess past 4,096 a (nondeterministic) partial cut would have kept. Raising
`FollowWidth` does not rescue this: the growth this shape produces is exponential generation over
generation (`.probe/half2`'s own raw pre-cap widths go 2,816 → 4,352 between two consecutive
generations), so a materially higher limit only delays the same wall by one more doubling, at the
cost of exactly the unbounded memory/time blowup this lane exists to bound. `FollowWidth = 4096`
is kept at its measured-headroom value (§5), not raised to flatter `lib.dom`'s number.

A more surgical admission rule — one that picks *which* types in an oversized generation to keep,
using something more stable than a checker-assigned id (e.g. the generation's own deterministic
discovery order, which does not depend on request-arrival timing the way a checker id does) —
would very likely recover much of this loss for `lib.dom` without reintroducing the
nondeterminism this lane found. That is future work, not attempted here: the coordinator's brief
noted four separate memoization-key attempts across two prior lanes, three of which corrupted a
golden through exactly this class of ordering assumption, and this lane's own first draft repeated
the mistake in miniature. The safe, verified mechanism — copy the depth cutoff's all-or-nothing
shape exactly — is what is committed; a subset rule is not, until it can be verified as
rigorously as this one now is.

## 5. `FollowWidth`'s derivation

The corpus's widest generation seen live sits at 2,091 (`@cloudflare/workers-types`, generation
10, measured via `XANTHAM_RESOLVE_COUNTERS`). `FollowWidth = 4096` carries roughly double that as
headroom. The full committed corpus's goldens show **zero diff** under this value
(`XANTHAM_UPDATE_GOLDEN=1`, 535/535 passed, `git status` on `tests/Xantham.Generator.Tests/golden`
shows nothing but the new fixture below) — the cutoff never fires for any committed fixture today,
consistent with 4,096 sitting well above the observed real-corpus maximum.

## 6. `frontier-width-lab` — the reproducer fixture

`tests/fixtures/frontier-width-lab/index.d.ts`: a `Frontier<T>` interface whose `map<U>`,
`reduce<U>`, `reduceRight<U>` methods return the interface applied to a fresh method-level type
parameter — the smallest shape that outgrows `FollowWidth` in a few seconds, standing in for the
`lib.dom` measurement only a live run against the real library reproduces. Registered via
`fixtureTests` in `tests/Xantham.Generator.Tests/Pipeline.test.fs`, plus one custom `testCase`
("a generation past the width cutoff is not resolved, RT003") asserting at least one `RT003`
finding exists and every such finding names the same limit, 4096. All three pass, confirmed
stable across repeated runs both with and without `XANTHAM_UPDATE_GOLDEN=1` — the determinism this
lane's first draft could not deliver.

## 7. Verification

- `frontier-width-lab`'s two baseline tests (golden match, run-to-run determinism) plus its one
  custom `RT003` assertion: all pass, confirmed stable across two independent process runs each.
- Full suite with golden regeneration (`XANTHAM_UPDATE_GOLDEN=1`): 535/535 passed; `git status` on
  the golden directory shows only the new `frontier-width-lab` entry — zero diff against every
  other committed fixture.
- `.probe/half2` and `.probe/dom` (`lib.dom`) each re-run twice independently: identical
  `frontier-expansions`/`table-size`/`max-width` counters both times.
- `lib.dom` monitored under a 300s hard external time bound with process-tree memory tracking
  (`Get-CimInstance Win32_Process`): completes in ~11–16s at ~614MB peak, well inside the bound
  that previously killed the same package at 171s/6,025MB.
- Final gate, foreground, `XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test`: 90 wire tests
  (1 skipped, pre-existing tsc-executable skip, unrelated), 535 generator tests, run gate 323
  checks, all stages green, exit 0.
- `.probe/` deleted before this commit; no leftover `dotnet`/`tsc` processes tied to this
  worktree's path (checked via `Get-CimInstance Win32_Process`).

Code commit: see this lane's commit on `worktree-gen-wave15-di`.
