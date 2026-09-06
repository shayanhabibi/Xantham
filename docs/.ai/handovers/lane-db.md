# Lane DB — TR036 closed: erased unions past U9 render inline, footer-declared

Wave fifteen, item 3. Behaviour change, staged on `worktree-gen-wave15-db` (not yet
merged to the wave-fifteen integration branch).

## 1. What changed

`ErasedUnionArity` (`src/Xantham.Generator/Shape/Spec.fs`) capped shaped erased unions at
9 arms; anything wider widened to `obj` and raised `TR036` (`TR.UnionTooWide`). The cap is
lifted at the Shape tier — `FsErasedUnion` now carries any arm count, uncapped. `TR036`'s
finding case stays in the manifest (`src/Xantham.Generator/Findings.fs`, append-only) but
the code path that raised it is gone; it is dormant, not removed.

Fable.Core 5.2.0 ships `U2`–`U9` only, each an `[<Erase>]` union with one
`op_ErasedCast` overload per arm enabling `!^`. An arm count above 9 has nowhere to
resolve at the Render tier, so Render.fs now:

1. Walks every `FsDecl` a file emits (`erasedArities`, mirroring the recursive-descent
   shape of `Shape/Arity.fs`'s private `mapRef`/`mapDeclRefs`, reimplemented locally
   since those are private to a different module) and collects every `FsErasedUnion` arm
   count actually reachable from the file's own declarations, including inside
   `FsOption`/`FsArray`/`FsTuple`/`FsDelegate`/`FsFunc`/`FsApp`/`FsBranded` and across
   interface members, statics, constructors, delegate types, phantom carriers, measure
   primitives, tagged-union case fields, and export members.
2. Keeps only counts above `ErasedUnionArity` (`footerArities`), deduplicated, ascending.
3. Renders one `[<Erase>] type U{n}<'t1,...,'tn>` per required arity, each carrying the
   same `Case{i}`/`op_ErasedCast` shape Fable.Core's own `U9` uses (verified via
   `fcs_nuget_members` against `Fable.Core`), and appends the block to the bottom of the
   generated file as `footer`.

`footer` is per-file, per-package: a file needing arity 12 gets one `U12` type declared
once at its own bottom; a file needing nothing past 9 gets no footer, and its rendered
bytes are unchanged (`footer = ""` reproduces the prior bare-string tail exactly, so
~52 fixtures in the corpus that never touch this path have zero diff). This was the
directed approach — the alternative, a shared `Xantham.Fable.Core` support package
shipping `U10`–`U100` globally, was already rejected in wave thirteen item D4 and is
reaffirmed rejected here, because bulk `op_ErasedCast` overloads living in a shared
package would burden every `!^` cast site for every consumer of the package, not just
the ones needing arity above 9.

## 2. Files

- `src/Xantham.Generator/Shape/Spec.fs` — removed the `arms.Length <= ErasedUnionArity`
  widening branch; every non-empty, non-single, non-`obj`-arm union now shapes to
  `FsErasedUnion`. `ErasedUnionArity`'s doc comment now describes it as the render-tier
  boundary between Fable.Core's own `U`-types and file-declared footer types, not "the
  widest union D4 allows".
- `src/Xantham.Generator/Model.fs` — `FsErasedUnion`'s doc comment updated: arities above
  9 resolve against a `U<n>` declared in the file's own footer.
- `src/Xantham.Generator/Render.fs` — added `erasedArities`, `footerArities`,
  `renderErasedUnionArity`, and the `footer` binding inside `renderModule`; updated the
  `FsErasedUnion` render-arm comment.
- `tests/Xantham.Generator.Tests/Shape.test.fs` — the test that asserted a 10-arm union
  widens to `obj` now asserts it shapes to a 10-arm `FsErasedUnion` with no finding.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` — the indexed-access-lab test that
  asserted a value union past the cap re-keys as loss (`TR036`) now asserts it renders
  `U12<...>` and resolves with no `TR020`/`TR036` finding.
- Golden regeneration (`XANTHAM_UPDATE_GOLDEN=1`, via `build.fsx -- test`) touched the
  three fixtures that actually contain a union wider than 9 arms: `indexed-access-lab`,
  `animejs`, `@cloudflare/workers-types` (`.fs`, `manifest.json`, `symbols.jsonl` each).

## 3. Before/after — corpus-wide TR036

Six `TR036` findings existed before this lane, across three fixtures. All six are gone;
`dotnet fsi build.fsx -- findings --key TR036` now returns zero matches everywhere in
the corpus.

| Fixture | Owner symbol | Arms |
|---|---|---|
| animejs | `DurationKeyframes.Item.[]` | 10 |
| animejs | `Revertible` | 10 |
| animejs | `AnimationParams.[]` | 14 |
| animejs | `WAAPIAnimationParams.[]` | 14 |
| @cloudflare/workers-types | `TraceItem.event` | 10 |
| indexed-access-lab | `onWide(handler)(event)` | 12 |

Manifest tier deltas (gross, per fixture):

| Fixture | Tier | Before | After |
|---|---|---|---|
| indexed-access-lab | ergonomic | 25 | 26 |
| indexed-access-lab | widened | 3 | 2 |
| animejs | exact | 79 | 80 |
| animejs | ergonomic | 101 | 101 |
| animejs | widened | 17 | 16 |
| animejs | escape | 54 | 54 |
| @cloudflare/workers-types | ergonomic | 1056 | 1057 |
| @cloudflare/workers-types | widened | 384 | 383 |

## 4. Per-symbol attribution (gross, both directions)

- `indexed-access-lab`'s arity-12 union: `widened` → `ergonomic`. Direct target of the
  fix.
- `@cloudflare/workers-types`'s `TraceItem.event` (arity 10): `widened` → `ergonomic`.
  Direct target.
- animejs's `Revertible` symbol: `widened` → `exact`. **Not a directly targeted symbol —
  a side effect.** `TR036` was `Revertible`'s only non-exact finding; once it stopped
  firing, the symbol had nothing left keeping it out of `exact`.
- animejs's `DurationKeyframes.Item`, `AnimationParams`, `WAAPIAnimationParams`: **did
  not change tier**, despite each carrying a resolved `TR036` before this lane. Each
  carries at least one other finding of equal-or-worse severity already, so removing
  `TR036` from their finding sets left their tier unchanged. No symbol moved backward;
  no other symbol moved unintentionally.

Net tier deltas above are entirely explained by these five symbols; no unrelated symbol
in any of the three fixtures shifted tier as a result of this change.

## 5. Rejected

The shared `Xantham.Fable.Core` `U10`–`U100` support-package approach (wave thirteen
item D4) was considered and rejected again here, per direction — see §1. No other
`TR0xx` finding key's behavior was touched; the fix is confined to `TR036`.

## 6. Verification

- `dotnet fsi build.fsx -- test` (no `--quick`, no `--no-run-gate`, no filter),
  `XANTHAM_REQUIRE_TSC=1`, run in true foreground twice: format ("47 files unchanged"),
  522/522 generator tests, 90/90 wire tests (1 skipped — the documented
  `XANTHAM_TSGO_EXE` design skip, not a "native tsc not found" failure), goldens
  regenerated clean, run gate 323 checks, exit 0 both times.
- `dotnet fsi build.fsx -- findings --key TR036`: zero results corpus-wide.
- `git status --short`: only the 14 files listed in §2 are modified; no stray or
  untracked files.

## 7. Left undone

- Merge into the wave-fifteen integration branch — out of scope for this lane; the
  coordinator owns that step.
- No other TR0xx behavior was touched, per the brief's explicit scope limit.
