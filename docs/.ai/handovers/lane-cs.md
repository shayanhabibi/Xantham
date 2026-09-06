# Lane CS — admitting `Intersection`-flagged arms into `isObjectMember`

Wave fourteen, branch `worktree-gen-wave14-cs`, forked at `5c827c4`. Lane CM measured this
change with a local, reverted flip; this lane made it real, updated the standing negatives it
turned into positives, and added the missing unit coverage.

## What changed

`isObjectMember` in `src/Xantham.Generator/Shape/Spec.fs` (inside `taggedUnionShape`) admits an
arm flagged `TypeFlags.Intersection` the same way it already admits one flagged `Object`,
provided its members are populated:

```fsharp
let isObjectMember (m: TypeFacts) =
    (flag TypeFlags.Object m || flag TypeFlags.Intersection m) && not m.Members.IsEmpty
```

The checker distributes an intersection over a union into arms carrying `Intersection` rather
than `Object`; their members are already resolved, so the old guard rejected candidates it had
full information to judge. One line, single file, exactly the diff CM's experiment predicted.

## Fixtures touched

- **`tests/fixtures/shared-tag-lab/`** — no new fixture (not needed): `Log` (the standing
  `TailStream.EventType`-shaped negative) and `OnEvent` (the inline-union analogue) both flip
  from negatives to positives under this change, so their pinned assertions in
  `tests/Xantham.Generator.Tests/Pipeline.test.fs` were rewritten rather than left wrong:
  - `Log`: was asserted to raise no `detect-tagged-unions` finding at all and stay
    `U2<Log2, Log3>`. Now asserted to raise `DT003` (`discriminated by 'type', but two arms
    carry 'log'; left as an erased union`) — the same refusal `Onset`'s flat spelling gets —
    and **still** stays `U2<Log2, Log3>` (refused, not folded; `Log` alone has no third
    discriminating arm).
  - `OnEvent`: was asserted to widen to `(obj -> unit)` with no `detect-tagged-unions` finding.
    Now asserted to read `(OnEvent.Event -> unit)`, a ten-case DU with
    `| [<CompiledName("log")>] Log of level: string` as its tenth case, and keys
    `[ "DT002"; "DT004"; "SY004" ]` — discriminated, folded once, named under its owner.
- **`tests/Xantham.Generator.Tests/Shape.test.fs`** — added
  `detect-tagged-unions admits an arm the checker flagged Intersection`, a pure per-pass unit
  test: two `TypeFlags.Intersection` arms with populated, tag-uniform members discriminate
  identically to two `TypeFlags.Object` arms, isolating the guard change from the fixture.

Both edits are additive/corrective to existing assertions, not net-new fixtures — no new lab
was needed, per the constraint (`shared-tag-lab` already held the reproducer).

## Measurements

`git diff --stat` (full):

```
src/Xantham.Generator/Shape/Spec.fs                                          |  5 ++-
tests/Xantham.Generator.Tests/Pipeline.test.fs                               | 41 ++++++++++-----
tests/Xantham.Generator.Tests/Shape.test.fs                                  | 35 ++++++++++++
.../golden/@cloudflare/workers-types/Cloudflare.WorkersTypes.fs              | 41 ++++++++++-----
.../golden/@cloudflare/workers-types/manifest.json                          | 27 +++++-----
.../golden/@cloudflare/workers-types/symbols.jsonl                          | 13 ++---
.../golden/shared-tag-lab/SharedTagLab.fs                                   | 16 +++++-
.../golden/shared-tag-lab/manifest.json                                     | 19 ++-----
.../golden/shared-tag-lab/symbols.jsonl                                     |  5 +-
9 files changed, 134 insertions(+), 68 deletions(-)
```

Only `@cloudflare/workers-types` and `shared-tag-lab` moved. `animejs` produced **zero diff**,
confirming CM's finding that its 11 reachable sites fail `isTaggedCaseData` (no uniform
string-literal candidate) and stay `Untagged`. No other fixture in the corpus moved at all.

**Tiers, whole corpus** (baseline → after, both fork numbers this lane re-verified, not
reused): exact 535 → 535, ergonomic 1603 → 1607 (**+4**), widened 797 → 794 (**-3**), escape
200 → 201 (**+1**). Per-fixture: `@cloudflare` ergonomic +2 / widened -2 / escape +1;
`shared-tag-lab` ergonomic +2 / widened -1 (exact unchanged in both).

**Finding keys, whole corpus** (before → after): `DT002` 24 → 26 (**+2**), `DT003` 1 → 2
(**+1**), `DT004` 1 → 3 (**+2**). No new finding case was needed or added — `DT003`/`DT004`
already existed from wave thirteen, exactly as briefed.

**Gross tier attribution** (declarations that changed, not netted):

- `@cloudflare/workers-types`: `ExportedHandlerTailStreamHandler` and `ExportedHandler` move
  widened → ergonomic (their `tailStream` signature no longer reads a widened `obj` callback
  parameter). A folded DU case's payload path
  (`ExportedHandlerTailStreamHandler.Result.Item.Event.Item`, post-rename — see below) lands at
  **escape**, carrying a nested `TR.AnyToObj` on `DiagnosticChannel.message`; that one symbol
  is the entire corpus-wide escape delta (+1), and it costs an escape entry *because* the fold
  worked, not despite it — a symbol's tier is the worst of its own findings. One prior
  declaration at this same path (the pre-fold arm interface) is removed by the fold, which is
  why a like-for-like column count is not meaningful here: the fold **removes minted
  declarations**, so their findings reattach to whichever new declaration inherits the path.
- `shared-tag-lab`: `Log` moves into ergonomic (new `DT003` finding, itself unchanged shape).
  `OnEvent`/`OnEvent.Event` moves out of widened, since the callback parameter no longer reads
  `obj`; the new `OnEvent.Event` DU sits at ergonomic (`DT004` fold cost), not exact, matching
  `Terminal`'s existing fold precedent.

## Gating

- `dotnet build Xantham.slnx`: 0 errors, 0 warnings on `src/`, one pre-existing `FS0025`
  warning in `Shape.test.fs` (unrelated incomplete-match, not touched by this change).
- `dotnet fsi build.fsx -- test` (full, run gate on): **491/491** generator tests, **90/90**
  wire tests (1 skipped by design), run gate **309** checks, exit 0.
- Both rewritten `shared-tag-lab` assertions verified failing on the pre-change guard and
  passing on the post-change one (checked individually before the full run).

## `TailStream.EventType`: folds as CM predicted

Confirmed at all three call sites (`ExportedHandlerTailStreamHandler`'s constructor,
`ExportedHandler.tailStream`, `WorkerEntrypoint.ITailStreamHandler.tailStream`): the callback
parameter reads a real ten-case DU (`ExportedHandlerTailStreamHandler.Result.Item.Event.Item`,
its `Event` module), with the `Log` intersection's two halves merged into one
`Log of level: string` case (`DT004`) and the other nine arms discriminating cleanly. This
compiles clean under `dotnet build Xantham.slnx`.

## The renaming question

Two declarations at `@cloudflare/workers-types`' `ExportedHandlerTailStreamHandler.Result.
Item.Event.*` path rename (`Item`→`Item2`, `Item2`→`Item3`) under the flip. Traced to source,
not left unexplained:

1. **Mechanism.** `Shape/Anonymous.fs`'s `nameAnonymous` walks the type graph in deterministic
   (source/traversal) order and calls `claim` on every type that `needsName`. Before this
   change, `becomesTaggedUnion` returned `false` for `TailStream.EventType` (its arms failed
   `isObjectMember`), so the union was never named and the walk never descended into it or its
   case payloads. After the change, `becomesTaggedUnion` returns `true`, the union gets a name,
   and the walk descends into its (now-named) case fields — new `claim` calls that did not
   exist before, landing earlier in the walk than declarations that already occupied the path
   `...Event.Item`. `claim`'s collision rule (`Seq.initInfinite (fun i -> $"{wanted}{i + 2}")`)
   hands out the first free suffix at a path, in call order — so a new claim inserted earlier
   at that path bumps every later claim at the same path by one slot.
2. **Deterministic, not a one-off cost or an unstable ordering.** The shift is a function of
   walk order, which is itself a function of the corpus content — same input always produces
   the same rename. It is the same mechanism every other change in this generator already goes
   through: inserting any new named declaration earlier in the walk already renumbers every
   later occupant of a colliding path, for any pass, not just this one. This is not new
   instability introduced by admitting `Intersection` arms; it is the pre-existing path-based
   naming scheme reacting normally to a graph that now has two more nodes in it.
3. **`Shape/Ordering.fs` is not the right place to pin this.** `Ordering.fs` fixes the
   **render** order of already-named declarations (source order, name as tiebreak) — it runs
   *after* naming, on `model.Decls`. The rename happens *during* naming, in `Anonymous.fs`'s
   `claim`, before `Ordering.fs` ever sees the declarations. Pinning render order does not
   touch the suffix a name gets. If this class of rename is judged worth stabilising, the fix
   belongs in `claim`'s collision policy (e.g., keying on something more stable than call
   order), not in `Ordering.fs` — and that is a design change wider than this lane's brief, so
   left unbuilt and flagged rather than attempted here.

## What this lane could not fully resolve

- Whether the `Item`/`Item2`/`Item3` rename is a compatibility break worth costing to a
  consumer pinning those names across a version bump is a product judgement, not a mechanism
  question — the mechanism above is deterministic and explained, but whether it matters is for
  the managing agent/user.
- `animejs`'s 11 non-candidate sites were reconfirmed unreached (zero diff) but not
  investigated further, per CM's original scope note — unchanged from CM's handover.

## Summary for the managing agent (≤15 lines)

- Tier movement, whole corpus: exact 535→535, ergonomic 1603→1607 (+4), widened 797→794 (-3),
  escape 200→201 (+1). Only `@cloudflare/workers-types` (+2/-2/+1) and `shared-tag-lab`
  (+2/-1/0) moved; `animejs` and every other fixture: zero diff.
- `DT002` 24→26 (+2), `DT003` 1→2 (+1), `DT004` 1→3 (+2). No new finding case added.
- `TailStream.EventType` folds at all three call sites as CM predicted; `dotnet build
  Xantham.slnx` compiles the regenerated golden with 0 errors.
- Renaming verdict: deterministic consequence of the naming walk visiting two new nodes ahead
  of `Item`/`Item2` at a colliding path (`Anonymous.fs`'s `claim`), not an ordering bug;
  `Shape/Ordering.fs` runs after naming and is not the fix point. Stabilising it (if wanted) is
  a `claim`-collision-policy redesign, out of this lane's scope.
- Full gate: 491 generator tests, 90 wire tests (1 skipped), run gate 309 checks, exit 0.
- Branch `worktree-gen-wave14-cs`, forked `5c827c4`, this handover committed on top.
- Nothing unexplained; escape's +1 is the one symbol CM already named
  (`ExportedHandlerTailStreamHandler.Result.Item.Event.Item`, nested `TR.AnyToObj`).
