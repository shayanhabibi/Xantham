---
category: Generator
audience: managing agent
title: Dispatch - generator wave thirteen
integration-branch: worktree-generator-wave-thirteen
---

# Generator wave thirteen — the fidelity queue, opened

Wave twelve's closing worklist put the fidelity queue first and left it priced. This wave spends
that price. Every mechanism below was re-verified against the current tree before dispatch: the
lines lane R3 cites in `docs/.ai/handovers/lane-r3.md` are still present, and the two recon
documents it rests on — `generator-cloudflare-recon.md` and `generator-tr018-recon.md` — still
describe what the code does.

`worktree-generator-wave-thirteen` forks `master` at `ffacc3d`, which is wave twelve merged. The
alpha packaging, the `FollowDepth` constant wave eleven settled at 20, and the delegate
declarations wave twelve landed are all untouched.

## Lanes

Three lanes in batch one, one held for batch two. Lane R3's dispatch order is followed, with its
first three entries collapsed into a single lane: all three land in `erasedUnionRef` or in the
constant that function reads, and two agents inside one function is a merge chosen rather than
suffered.

### Lane CA — the cheap three

Branch `worktree-gen-wave13-ca`, forked from this integration branch. Three changes, three
commits, each measured on its own.

1. **Deduplicate `erasedUnionRef`'s findings by *(owner, rendered message)*.** The function
   accumulates every arm's findings inside the per-arm map while `List.distinct` collapses the
   arms themselves, so a union whose fourteen arms all map to `string` reports fourteen identical
   findings against one rendered type. Keying on the owner alone is wrong: `BodyInit`-shaped
   owners carry several genuinely distinct messages that must all survive. Emits no golden line;
   the whole change is manifest-visible.
2. **`ErasedUnionArity` 4 → 9.** `Fable.Core` 5.2.0 ships `U2`–`U9`, and `Render.fs` already
   builds the type name from the arity, so the renderer needs no work. This reopens D4's recorded
   decision, which is the change's real cost.
3. **`Model.LibBindings` gains `AsyncIterableIterator` → `JS.AsyncIterable`,** with a loss note.
   The table already carries `AsyncIterable`, `AsyncIterator` and `AsyncGenerator`; this is the
   family's missing fourth name. `Error` → `exn` landed in an earlier wave and needs nothing.

Owns `Shape/Spec.fs` (`erasedUnionRef` and `ErasedUnionArity`), `Model.fs`, the doc comments in
`Render.fs`, the `UnionTooWide(5, 4)` literal in `Findings.test.fs`, and D4's record in
`docs/.ai/plans/generator-type-mapping.md`. No new finding case.

### Lane CB — a tagged union refused on a shared tag says so

Branch `worktree-gen-wave13-cb`, forked from this integration branch. `taggedUnionShape` requires
every arm's tag value to be distinct and returns `None` where two collide. `DT.ArmNotPlainData` is
raised only on the success path, so the refusal is silent: the union falls through to
`erasedUnionRef` and the manifest carries no `DT` finding for it under any name.

Two outcomes are acceptable, in this order of preference: merge the arms sharing a value into one
case carrying the members they agree on, or refuse loudly. **Refusing silently is what the lane
exists to end.** A DU case per arm where two arms share a tag is not legal F#, so the merge has to
be real or not attempted.

`taggedUnionShape` has exactly one call site, `Shape/TaggedUnions.fs:34`, which is the seam: it
can return a refusal reason rather than a bare `None`, and the pass raises the finding.

Owns `Shape/TaggedUnions.fs` and `taggedUnionShape` (`Shape/Spec.fs:195-240`).

**Out of scope, deliberately: recon §5.3, the inline discriminated union at member position.**
`detectTaggedUnions` iterates `model.DeclNames`, so a union written inline has no name to be
offered under. Reaching it means minting one through `Anonymous.claim` inside a pass that names
nothing today — a wave-sized change for one recorded site. It stays on the worklist.

### Lane CC — remeasure `TR018` before anyone implements against it

Branch `worktree-gen-wave13-cc`. Read-only. Lane R3 found the `TR018` recon's site list stale: its
causes C and D landed in an earlier wave and took the cheap half of the count with them. Two
clusters in the residue are territory the document never covered.

- **`animejs`'s eight `*.then(callback)(self)` sites** look cause-C-shaped but survived the
  cause-C fix. A fresh reproducer decides whether they share that mechanism.
- **`@cloudflare/workers-types`'s four `Workflow*.Config.retries` owners**, which the original
  recon never saw.

The remainder is very likely cause B — an intersection generic over a type parameter — which is a
floor rather than a backlog, and `type-fest`'s eight are confirmed unrecoverable. The lane's
deliverable is a verdict per cluster with a reproducer under ten lines for anything it calls
recoverable, written to `docs/.ai/handovers/lane-cc.md`. It opens no implementation lane itself.

Owns nothing under `src/` or `tests/`.

### Held for batch two — Lane CD, indexed access over a concrete operand

`TR020` 69, recon lane Q3, `indexedAccessRef`. Sequenced after CA: an event map with more than
nine entries currently lands on `obj` through `TR036`, so once the cap is 9 those sites move
between keys rather than down. Measuring Q3 before CA merges would price it wrong.

## Ownership map

| File | Lane |
| --- | --- |
| `Shape/Spec.fs` — `erasedUnionRef`, `ErasedUnionArity` | CA |
| `Shape/Spec.fs` — `taggedUnionShape` | CB |
| `Shape/TaggedUnions.fs` | CB |
| `Model.fs`, `Render.fs`, `Findings.test.fs` | CA |
| `Findings.fs` | neither — pre-declared below |

CA and CB share `Shape/Spec.fs` and are dispatched in parallel regardless: the two regions sit
about 1,400 lines apart and merge by hunk. This is the one exception to "give each agent a
different pass file" this wave takes, recorded so the merge is expected rather than discovered.

## Pre-declared finding cases

Appended on the integration branch before dispatch, so lane CB never has to come back for one. A
case that turns out unused costs one dead row.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `DT.TagValueShared of tag: string * value: string` | `DT003` | ergonomic | Lane CB |
| `DT.ArmsMergedOnSharedTag of tag: string * value: string` | `DT004` | ergonomic | Lane CB |

Both are graded ergonomic to match `DT001`, which describes the same outcome: the union survives,
as an erased union rather than a discriminated one. Where the fallback then exceeds the arity cap,
`TR036` records that loss separately and keeps the two apart.

## What every lane owes back

Fifteen lines to the managing agent; the full report to `docs/.ai/handovers/lane-<id>.md` on the
lane's own branch. Counts, not verdicts:

- `dotnet fsi build.fsx -- findings` before and after, and the per-key delta for every code that
  moved — not only the targeted one.
- Tier counts, run-gate check count, generator and wire test counts.
- `git diff --stat` over the regenerated goldens.
- Branch and commit SHA.
- Anything a large fixture did that the lab fixture does not account for. **Report it and stop**
  rather than chasing it.

Lane CA additionally owes the **arity histogram of the sites `UnionTooWide` widens today** — how
many are arity 5, 6, 7, 8, 9, and how many above 9 — measured before the constant moves. Raising
the cap reopens a decision that was recorded with a reason, and the histogram is what lets the
threshold be chosen on evidence rather than on the largest number `Fable.Core` happens to ship.

## Gate and corpus

Baseline, measured on this branch at the fork, reproducing wave twelve's recorded numbers exactly:

| | wave twelve `ffacc3d` | composed |
| --- | ---: | ---: |
| generator tests | 469 | |
| wire tests | 90 | |
| run gate checks | 297 | |
| exact | 513 | |
| ergonomic | 1616 | |
| widened | 790 | |
| escape | 195 | |
| total findings | 17,928 | |
| `TR018` | 82 | |
| `TR020` | 69 | |
| `TR023` | 137 | |
| `TR036` | 72 | |
| `TR037` | 54 | |
| `DO001` | 5 | |
| `DT001` / `DT002` | 2 / 14 | |
| `RT001` | 3 | |
| exit code | 0 | |

`DO001` is 5 corpus-wide rather than the 14 the cloudflare recon predicted lane Q1 would recover,
so that half of the dedup has already landed under another wave. `TR037` 54 is the whole of lane
CA's first target.
