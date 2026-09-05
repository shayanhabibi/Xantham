---
category: Generator
audience: managing agent
title: Lane R4 handover - FollowDepth frontier sample
---

# Lane R4 — FollowDepth frontier sample

Branch `worktree-gen-wave10-r4`, forked `cb6256b`. Sampling lane: numbers only, no repair, no
verdict on `FollowDepth`.

## What changed

`src/Xantham.Generator/Resolve.fs` only (+16/-0 lines, `git diff --numstat`):

- Added `dumpFrontier` (line 14), a no-op unless `XANTHAM_FRONTIER_DUMP` is set. When set, it
  appends each `TypeResponse` the depth cutoff declines to follow, one per line, JSON-serialised
  with `ProtoJson.serialize` (the record already ships `JsonPropertyName` attributes, no new
  serializer needed).
- One call site: `resolveTypeTable`'s cutoff branch (`Resolve.fs:746`), before the existing
  `notFollowed` map is built.

`FollowDepth` (`Resolve.fs:25`) is committed at `12`, unchanged from base.

## Default path proof

`dotnet fsi build.fsx -- findings`, summed over all fixtures, before and after:
`exact 495, ergonomic 1552, widened 786, escape 193` in both. `RT001` count: `7` in both.
`dotnet fsi build.fsx -- test`: exit 0, both before and after.

## Frontier measurements (`@cloudflare/workers-types`, filter `"cloudflare"`)

| `FollowDepth` | raw frontier lines | unique type ids | `test` stage wall-clock |
| --- | --- | --- | --- |
| 12 | 5,445 | 2,371 | 15.9s / 21.4s |
| 16 | 5,544 | 2,392 | 28.9s |
| 20 | 5,544 | 2,397 | 23.9s |

Unique-id growth shrinks (+21 then +5); wall-clock does not track depth in this range (noise
between runs dominates). Full detail, flag/objectFlags/target distributions, and the reading on
utility-type concentration are in `docs/.ai/plans/generator-followdepth-recon.md`.

## Could not explain / left for a later lane

- Target id 65 accounts for 348 of 783 `Target`-carrying entries (44%) at `FollowDepth 12`.
  Checker ids are per-run, so this cannot be resolved to a declaration name without a further
  wire round trip - out of scope for this sampling pass, named as the starting point for whoever
  chases it.
- The dispatch brief's carried-forward estimate of ~1,815 unresolved types in this fixture is
  higher than the 2,371 unique ids measured here at the *same* `FollowDepth 12` - not a
  contradiction (this dump measures something the earlier estimate never directly counted), but
  the two numbers cannot be reconciled against each other since the earlier one was never
  produced by this method.

Final commit: `0738744`.
