# Lane CM — pricing admitting `Intersection`-flagged arms into `isObjectMember`

Wave fourteen, branch `worktree-gen-wave14-cm`, forked at `8d3a4fa`. Read-only lane: no
behaviour change lands. Everything below came from a local, uncommitted flip of `isObjectMember`
in `Shape/Spec.fs` (`flag TypeFlags.Object m` widened to `flag TypeFlags.Object m || flag
TypeFlags.Intersection m`), regenerating goldens, and reverting with `git checkout -- .` before
this file was written. `git status` is clean; nothing under `src/` or `tests/` is committed.

## Method

A file-based probe (`lock`-guarded, written to a path outside the repo) counted every call to
`taggedUnionShape` where at least one union arm carries `TypeFlags.Intersection` with populated
members — i.e. every site that `isObjectMember` rejects today but the widened test would admit.
Expecto swallows `Console.Out`/`Error` per passing test, so `eprintfn` reported nothing until the
probe wrote to a file directly. Runs were filtered per fixture (`--filter <name>`) so that
per-package type ids, which restart at each package boundary, could not be double-counted.

## 1. Corpus-wide site count

62 unique type ids in `@cloudflare/workers-types`, 11 in `animejs`, 2 in the hand-written
`shared-tag-lab` (`Log`, `OnEvent` — both already pinned as negatives). Zero in `ansi-regex`,
`solid-js`, `type-fest`, and every other fixture. **75 sites corpus-wide reach the guard with an
Intersection-flagged arm today; 73 of them are in real npm corpus, entirely inside two packages.**

## 2. Outcome split, and 4. collateral

Flipping the flag and regenerating: only `@cloudflare/workers-types` and `shared-tag-lab` moved
— **`animejs`'s 11 reachable sites produced zero diff.** They reach the widened guard but fail
`isTaggedCaseData` (no arm-uniform string-literal candidate), so they stay `Untagged`; this is
the collateral check for item 4, and it came back clean — no shape changed there. No other
fixture moved at all.

In `@cloudflare/workers-types` the corpus-wide finding deltas (net, `findings` before/after) are:

| key | before | after | delta |
| --- | ---: | ---: | ---: |
| `DT002` (`DT.TaggedUnion`) | 24 | 26 | +2 |
| `DT003` (`DT.TagValueShared`) | 1 | 2 | +1 |
| `DT004` (`DT.ArmsMergedOnSharedTag`) | 1 | 3 | +2 |
| `SY004` | 726 | 728 | +2 |
| `TR008` (`TR.AnyToObj`) | 574 | 575 | +1 |
| `TR032` | 5131 | 5143 | +12 |
| `TR036` (`TR.UnionTooWide`) | 10 | 6 | -4 |

Tiers, whole corpus: exact 535→535, ergonomic 1603→1607 (+4), widened 797→794 (-3), escape
200→201 (+1). Total findings 17,040 → 17,056 (+16). The one `DT003` is `shared-tag-lab.Log`
itself (bare, no other discriminating arms) — a candidate that is newly *considered* and then
refused, exactly the three-way split the item asked for: **62 sites reach the guard, one of them
(the standalone `Log` shape) is a candidate but refused (`DT003`), the `@cloudflare` corpus
instance folds cleanly (`DT004`), and the remaining reached sites in `animejs` are not candidates
at all.**

The escape count's +1 is explained, not mysterious: the newly-minted DU case
`ExportedHandlerTailStreamHandler.Result.Item.Event.Item` carries a nested `any`-typed member
(`DiagnosticChannel.message`, `TR.AnyToObj`) elsewhere in its own case set, and a symbol's tier is
the worst of its findings — so the clean fold still reports as `escape` overall. `git diff` on
the golden and manifest confirms this line-for-line; no unexplained collateral.

## 3. Does `TailStream.EventType` actually fold? — yes, at all three sites

`ExportedHandlerTailStreamHandler`'s constructor, `ExportedHandler.tailStream`, and
`WorkerEntrypoint.ITailStreamHandler.tailStream` all move from `obj` (`U3<JS.Promise<U2<(TailEvent<obj> ->
…)>>, …>`) to a real ten-case discriminated union
(`ExportedHandlerTailStreamHandler.Result.Item.Event.Item`), with the two `Log`-intersection
halves folded into one `Log of level: string` case (`DT004`) and the other nine arms
discriminating cleanly. This is confirmed directly in the regenerated golden diff (all three call
sites), and `shared-tag-lab.OnEvent` — the reduced reproducer — folds identically. **This is the
strongest possible outcome: the sites do not merely reach the pass and get refused a second time,
they fold into working discriminated unions**, and `dotnet build Xantham.slnx` compiled the
regenerated `@cloudflare` golden clean (0 errors) under the flipped flag.

## Gating under the experimental flip

`dotnet fsi build.fsx -- test --quick --update --no-run-gate`: 488/490 passed. The two failures
are both `shared-tag-lab` assertions that pin *today's* behaviour as a negative (`Log` expected
`[]` findings, `OnEvent` expected to stay `obj`) — they fail because the flip is working as
intended, not because anything broke. No compiler errors; `dotnet build Xantham.slnx` (full
compile gate) succeeded with 0 errors, 1 pre-existing warning unrelated to this change.

## What the experiment did not establish

- Whether the two now-renamed downstream symbols in `@cloudflare` (`Item`→`Item2`,
  `Item2`→`Item3`, from a numbering shift caused by the new case sitting earlier in member
  order) are a naming-stability concern for consumers pinning those names across a version bump.
  Not chased further per the "hand back what you cannot explain" rule — flagged here.
- Whether `animejs`'s 11 non-candidate sites would ever become candidates under some other
  admitted shape; not investigated beyond confirming today's flip leaves them untouched.
- Behaviour beyond the six fixtures actually reached (this repo's fixture set only, not the
  wider npm ecosystem).

## Recommendation: take it in a later batch

The blast radius is small (2 real packages, 73 sites, one corpus-wide fold, one corpus-wide
refusal, no collateral in non-candidate sites) and the outcome is exactly the one wave thirteen's
`DT003`/`DT004` machinery was built to report loudly rather than drop silently. The motivating
site — `TailStream.EventType`'s three call positions — recovers a real discriminated union and
compiles clean. The one-line guard change (`isObjectMember`) is the entire diff; no new finding
case is needed. Recommend taking it as a small, single-file, well-measured item in the next
batch, gated on this handover's numbers being re-verified against the branch it lands on (another
lane's `Shape/` work this wave may shift the baseline).
