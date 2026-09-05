---
category: Generator
audience: managing agent
title: Lane AX - FollowDepth raised to 20
---

# Lane AX - FollowDepth raised to 20

Branch `worktree-gen-wave11-ax`, based on `8966e7f`. Full measurement appended to
`docs/.ai/plans/generator-followdepth-recon.md` as its "Lane AX" section; lanes AU, AV and R4's
sections are untouched apart from one corrected table cell.

## What changed in the tree

- `src/Xantham.Generator/Resolve.fs` - `FollowDepth` 12 → 20, plus four doc-comment lines stating
  the headroom the value now carries. The `XANTHAM_FRONTIER_DUMP` instrumentation is unchanged and
  still inert with the variable unset.
- Eleven regenerated golden files under `tests/Xantham.Generator.Tests/golden/`: seven
  `symbols.jsonl` and four `manifest.json`. All eleven carry `RT001` movement and nothing else.
- `docs/.ai/plans/generator-followdepth-recon.md` - lane AX section, and the `solid-js` margin cell
  in lane AV's Q2 table corrected from 4 to 3.

No generated binding file moved. `Shape/`, `build.fsx`, `README.md`, `docs/generator-usage.md` and
every packaging file are untouched.

## Counts, before and after

| | before (`FollowDepth 12`) | after (`FollowDepth 20`) |
| --- | --- | --- |
| exact / ergonomic / widened / escape | 495 / 1552 / 786 / 193 | 495 / 1552 / **782** / 193 |
| `RT001` | 7 | **3** |
| `TR002` | 0 | 0 |
| `TR003` | 0 | 0 |
| generator tests | 467 | 467 |
| wire tests | 90 | 90 |
| run gate checks | 257 | 257 |
| `dotnet fsi build.fsx -- test` | 0 | 0 |

`widened` −4 and `RT001` −4 are one movement, not two. `RT.FrontierNotResolved` is a
`[<Widened>]` case; `array-shape-lab`, `setter-lab`, `solid-js` and `type-fest` each exhaust their
frontier once the cutoff passes 12, so each drops the single `RT001` it carried. Nothing else in
the corpus moved.

## Why 20

`@cloudflare/workers-types` is the tightest fixture: `TR002 = 1` at `FollowDepth 10`, `TR002 = 0`
at 11, so the renderer's deepest reach is generation 11 (re-measured this lane, not inherited).
20 − 11 = 9 generations of headroom, one over the eight the brief asked for.

The reach does not grow with the cutoff. Corpus output is byte-identical at 16, 19, 20 and 24 -
same tiers, same `RT001` count, same frontier sizes - so opening the walk does not give the
renderer anything deeper to reference. 16 clears only 5 generations; 19 clears exactly 8 and
measured no cheaper than 20; 24 is 20's output for four more generations of walk.

Wall-clock, full `generator e2e` regenerate-and-check: 12 → 185s, 16 → 188s, 19 → 160s,
20 → 249s, 24 → 261s. The spread is run-to-run noise on this machine and does not separate the
candidates; the full gate at 20 runs in 2m45s.

## Discrepancy resolved

Lane AV's `solid-js` row read reach 9, margin 4. The reach is right and the margin was not:
`TR002 = 1` at `FollowDepth 8`, `TR002 = 0` at 9, so reach is 9 and the margin to 12 is 3. The cell
is corrected in the recon document.

## Frontier and lab reaches

`@cloudflare/workers-types`'s stranded set settles at 1,848 ids by cutoff 16 and holds there
through 24 (1,815 at 12). The four labs that cross the cutoff are nowhere near it: `chain-lab` and
`hoist-conditional-lab` reach generation 3, `setter-lab` 4, `array-shape-lab` 1. `TR003` fired on
no fixture at any value from 0 to 24.

## Nothing unexplained

Every moved line is an `RT001` payload or the tier row it feeds. No count moved that this lane
cannot name.
