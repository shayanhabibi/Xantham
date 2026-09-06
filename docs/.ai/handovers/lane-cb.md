# Lane CB - a tagged union refused on a shared tag says so

Wave thirteen, branch `worktree-gen-wave13-cb`, forked at `a008490`.

`taggedUnionShape` required every arm's tag value to be distinct and returned a bare `None`
where two collided. `DT.ArmNotPlainData` is raised only on the success path, so the refusal was
silent: the union fell through to `erasedUnionRef` and the manifest carried no `DT` finding for
it under any name. A reader could not tell the union had ever been considered.

Both outcomes the dispatch priced are landed. Arms sharing a tag value fold into one case
carrying the members they agree on, and where the fold leaves a single case the union is refused
under the discriminant that came closest.

## What was built

- **`Shape/Spec.fs`** - `taggedUnionShape` returns a `TaggedShape` union rather than an option:

  ```fsharp
  type internal TaggedShape =
      | Discriminated of tag: string * cases: (TypeFacts * string) list * folded: string list
      | TagCollides of tag: string * value: string
      | Untagged
  ```

  Every property of the first arm that carries a string literal on *every* arm is collected as a
  candidate before any decision is taken. A candidate whose values are all distinct wins, in
  member order - so a candidate two arms collide on leaves a later one free to discriminate
  cleanly, which is the `Signal` case in the lab. Where every candidate collides, the first one
  folds: arms are grouped by tag value in first-occurrence order, and `foldedArm` builds one case
  per group from the members every arm in it declares under the same name, type id and
  optionality. Two or more groups is a `Discriminated` carrying the folded values; one group is
  `TagCollides`, which names that candidate rather than an arbitrary property.

  `foldedArm` also collects the group's call and construct signatures onto the folded facts, so a
  folded arm that is not plain data still reaches `DT.ArmNotPlainData` at the call site.

- **`Shape/TaggedUnions.fs`** - the three-way match at the single call site. `TagCollides` raises
  `DT.TagValueShared(tag, value)` (`DT003`) and returns `None`, leaving the erased-union fallback
  exactly as it was. `Discriminated` raises one `DT.ArmsMergedOnSharedTag(tag, value)` (`DT004`)
  per folded value, immediately before the existing `DT.TaggedUnion`.

- **`tests/fixtures/shared-tag-lab/`** - hand-authored, tracked, six declarations:

  | Declaration | What it pins |
  | --- | --- |
  | `Distinct` | regression guard: uniformly tagged, values distinct, still a DU on `kind` |
  | `Signal` | `channel` collides, `verb` discriminates; the DU is on `verb`, nothing reported |
  | `Terminal` | two arms share `state: "done"`, a third discriminates; folds to two cases, `DT004` |
  | `Onset` | both arms carry `type: "log"`; the fold leaves one case, `DT003`, stays `U2` |
  | `Log` | the `TailStream` intersection form - the negative, see below |
  | `Loose` | `kind` is a bare string on one arm, so no candidate is uniform: no `DT` finding |

  Registered in `Pipeline.test.fs` with one extra test asserting all six.

- **Run gate** - `SharedTagLab.fs` linked, `foldedTaggedCases ()` added. Three checks. The fold is
  the first thing in the corpus to produce a DU case with *no* field, so what it claims is that a
  payload-free folded case still erases to `{"state":"done"}` rather than to a bare string or a
  Fable tag object. It does, and it matches itself back.

  The lab declares only types, so it needs no `index.js` and `register.mjs` needs no edit.

## Findings

No new case. Both codes were pre-declared on the integration branch and both are used:

| Case | Code | Tier | Where it fires |
| --- | --- | --- | --- |
| `DT.TagValueShared` | `DT003` | ergonomic | `shared-tag-lab.Onset` |
| `DT.ArmsMergedOnSharedTag` | `DT004` | ergonomic | `shared-tag-lab.Terminal` |

## Measurements

`dotnet fsi build.fsx -- findings`, before and after. **Every key that moved moved by exactly the
new lab's own contribution; no existing fixture's golden changed.**

| | before (`a008490`) | after |
| --- | ---: | ---: |
| generator tests | 469 | 472 |
| wire tests | 90 | 90 |
| run-gate checks | 297 | 300 |
| exact | 513 | 516 |
| ergonomic | 1616 | 1619 |
| widened | 790 | 804 |
| escape | 195 | 195 |
| total findings | 17,928 | 17,972 |
| `DT001` | 2 | 2 |
| `DT002` | 14 | 17 |
| `DT003` | - | 1 |
| `DT004` | - | 1 |
| `TR036` | 72 | 72 |
| `TR018` / `TR020` / `TR023` / `TR037` | 82 / 69 / 137 / 54 | unchanged |
| `DO001` / `RT001` | 5 / 3 | unchanged |

Per-key delta, whole corpus: `DT002` +3, `DT003` +1, `DT004` +1, `TR006` +17, `SP001` +14,
`MB003` +2, `SI003` +2, `TR032` +2, `TR040` +2. Those nine sum to 44, which is
`shared-tag-lab`'s entire manifest. Nothing else moved.

`git diff --stat` over the existing goldens: **zero lines**. The only golden the branch adds is
`golden/shared-tag-lab/`. `dotnet build Xantham.slnx` succeeds; `dotnet fsi build.fsx -- test`
exits 0 with the run gate on.

## The corpus has no named instance of the defect

`DT003` and `DT004` fire nowhere outside the lab. That is a measurement rather than an accident
of the fixture set, and it has one explanation per half.

### 1. `detectTaggedUnions` iterates `model.DeclNames`

The recorded real-world site is `@cloudflare/workers-types`' `TailStream.EventType`, and the
manifest records it as `ExportedHandlerTailStreamHandler()(event)` - an *inline* union at
callback-parameter position, widened by `TR.UnionTooWide` with `arms: 11, cap: 4`. A union
written inline has no name to be offered under, so it never reaches this pass at all. That is
recon 5.3, which the dispatch put out of scope for this lane, and it stays the gating obstacle:
the fold cannot rescue `TailStream.EventType` until 5.3 lands.

### 2. An intersection over a union distributes into arms flagged `Intersection`

The `Log` declaration in the lab is the reproducer verbatim:

```ts
export type Log = { readonly type: "log"; readonly level: string } & (
    | { readonly message: object; readonly truncated?: false }
    | { readonly message: string; readonly truncated: true }
);
```

The checker normalises this to a union of two intersections. `taggedUnionShape` guards arms on
`isObjectMember` - `flag TypeFlags.Object m && not m.Members.IsEmpty` - and an intersection type
carries `TypeFlags.Intersection`, not `TypeFlags.Object`, so the union is `Untagged` and is never
considered a tagged one to refuse. Its members *are* populated (the resolve tier flattens them:
`Log2` renders as an interface with all four), so the arms are readable; the flag test alone
rejects them.

`Onset` in the lab is the same pair written flat, and it is exactly what `DT003` reports on. So
the fold and the refusal are proven on the shape the checker produces; what is missing is
admitting that shape in the spelling the author wrote.

**This is left on the worklist rather than taken.** Widening `isObjectMember` to admit
intersections changes which unions across the whole corpus become discriminated - a measurement
and a decision the dispatch did not price - and it is subordinate to 5.3 for the site that
motivated the wave.

## Worklist

- **Recon 5.3, the inline union at member position** (`model.DeclNames` / `Anonymous.claim`) -
  unchanged in priority, and now with a second reason: it is what stands between the fold and
  `TailStream.EventType`.
- **Admit intersection-flagged arms in `isObjectMember`.** One guard plus a corpus measurement.
  `shared-tag-lab.Log` is the standing reproducer and the lab already asserts today's behaviour,
  so the change is a single golden diff away from being priced.
- A folded case drops the members its arms disagree on, so constructing one writes an object
  TypeScript would reject as incomplete. `DT004` records that per union. If that record is judged
  too weak, the alternative is to keep the disagreeing members and widen them, which needs a
  finding case this lane did not have.
