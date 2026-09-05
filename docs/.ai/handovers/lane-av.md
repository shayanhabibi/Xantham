---
category: Generator
audience: managing agent
title: Lane AV - FollowDepth vs. TR002/TR003
---

# Lane AV - FollowDepth vs. TR002/TR003

Branch `worktree-gen-wave11-av`, based on `b2551e7`. Measurement only. Full analysis appended to
`docs/.ai/plans/generator-followdepth-recon.md` (new section, lane R4's untouched).

## What changed in the tree

Nothing durable. `Resolve.fs`'s `FollowDepth` was swept through 12, 11, 10, 9, 8, 7, 6, 5, 4, 2, 1,
0 to find where `TR002`/`TR003` fire, then restored to 12. `git status` is clean and `dotnet fsi
build.fsx -- findings` reproduces `exact 495, ergonomic 1552, widened 786, escape 193`, `RT001` 7,
byte for byte against pre-lane output.

## Answers

- **`TR003` never fired**, on any fixture, at any `FollowDepth` from 12 down to 0. `Resolve.fs`'s
  own invariant ("every type id a `TypeFacts` refers to is in the table or recorded in
  `NotFollowed`") guarantees `NotFollowed` catches everything the cutoff strands, so `TR003` fires
  only if that invariant breaks - not something `FollowDepth` controls. Reads as dead code;
  retiring it is the managing agent's call, since a lane can't prove no walk bug exists.
- **`TR002` fires and is live.** First fire on `@cloudflare/workers-types`: `FollowDepth 10`,
  count 1 (one site, a member several levels into a synthesized anonymous-object chain in the
  Workers AI response bindings). Climbs to 74 at `FollowDepth 6`, 4,349 at `FollowDepth 0`.
- **Renderer's real reach, per fixture** (deepest generation a rendered reference actually
  touches, found by sweeping each fixture's own `FollowDepth`):

  | fixture | deepest reach | margin to committed `FollowDepth 12` |
  | --- | --- | --- |
  | `@cloudflare/workers-types` | 11 | **1** |
  | `solid-js` | 9 | 4 |
  | `animejs` | 7 | 5 |
  | `type-fest` | 6 | 6 |

  Margin is not uniform and cloudflare is the tight one - 1 generation, not the ~6 the brief's
  `type-fest` figure suggested. Dropping `FollowDepth` by 2 already widens a real member in the
  committed corpus.

## What this retires

`TR003` retires as unreachable via `FollowDepth`. `TR002` does **not** retire - it is correctly
zero today but the margin protecting that zero is thin (1 generation on cloudflare), so the
four-wave correctness risk is answered "currently unrealized," not "closed."

## Proof default path unchanged

`dotnet fsi build.fsx -- findings`: `exact 495, ergonomic 1552, widened 786, escape 193`, `RT001`
7, identical before/after. `git status` clean; `FollowDepth = 12` restored.

## Final commit

See branch `worktree-gen-wave11-av` HEAD.

## Could not explain

Nothing outstanding.
