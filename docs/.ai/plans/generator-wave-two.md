# Generator wave two — dispatch plan

For the managing agent. Wave one landed constructor objects, inherited bases, class statics and
the `>>:` parse fix, and cost more at integration than it should have. The tooling fixes since
(`.claude/rules/generator-fixtures.md`) remove most of that cost; this plan spends what remains on
the work the corpus says is worth doing.

Read `.claude/rules/generator-fixtures.md` first — the "For the managing agent" section is the
protocol this plan assumes.

## Where the corpus stands

15,359 findings over 17 goldens; 436 exact / 1,324 ergonomic / 738 widened / 198 escape.

The three largest keys are **not** defects and no agent should be sent at them: `TR032` (5,265,
null-union members hoisted to option), `MB003` (3,938, optional member reads as option) and
`SP001` (1,369, ParamObject Create synthesized) are the mapping working as designed. `TR008` (544,
`any` maps to `obj`) is inherent to `any`.

What is left, in order of size:

| Key | Count | Tier | What it says |
|---|---|---|---|
| `TR006` | 1,215 | widened | string literal type widened to `string` (doc-noted, §4.2) |
| `TR023` | 727 | widened | the named type is not among the generated declarations |
| `TR018` | 232 | widened | intersection over a non-object operand has no members to flatten |
| `TR014` | 227 | widened | `Conditional` type flags not mapped |

`TR006` is doc-noted at §4.2 and may be intentional; **settle that before treating it as work.**

## Before dispatch

Ordered, all on the integration branch, all before any agent starts.

1. **Land the recon.** `docs/plans/generator-three-rung.md` exists only on the unmerged branch
   `gen-three-recon` (`399719d`). Three of this wave's four lanes are scoped by it, so merge it to
   the integration branch first or the agents cannot read their own brief.
2. **Rebuild the two lost reproducers.** The recon reports building `nominal-lab` and `chain-lab`
   and quotes both, but neither was committed — `git ls-files` on that branch shows only the eight
   pre-existing labs. The quoted `.d.ts` in §9 is the whole content, so this is transcription, not
   rediscovery. Commit them as `tests/fixtures/nominal-lab/` and `tests/fixtures/chain-lab/`; the
   `*lab` pattern tracks and gates them with no further edit.
3. **Pre-declare every finding case**, in one commit, with `Findings.test.fs` extended in the same
   commit. Proposed below per lane; the names are yours to finalise, but the *count and owner* must
   be fixed before dispatch. This is the one append point no pattern removes.
4. **Assign the lab names** in each brief, so two agents cannot both write `union-lab`.

## Lanes

`Shape/` is one file per pass, so lane assignment is file assignment. **Two lanes want
`Shape/Spec.fs`** — that is the shared reading machinery every pass is written against, and it
cannot be split across concurrent agents. They are sequenced, not parallel.

### Batch 1 — three agents, disjoint files

**Lane A — the instantiation runaway (recon blockers 1 and 2).**
`three`'s polymorphic-`this`-in-an-intersection return mints a strictly larger anonymous type per
application: 518 declarations, 369,116 lines, names 1,689 characters long, stopped only by the
depth cutoff at 12. Blocker 2 is downstream of it — `TP001` fires only where the cutoff already
fired, and its message interpolates a checker-assigned type id, which is what makes the manifest
non-deterministic. Fix the runaway and re-measure before treating blocker 2 as separate work; if it
survives, the fix is the one already applied to the frontier finding — name the owner, or aggregate
onto `<type-table>` the way `RT001` does, rather than putting a checker id in a rendered string.

- Owns `Shape/Anonymous.fs`. Lab: `chain-lab`.
- Findings to pre-declare: expect one on the hoist refusing to recurse, and possibly one
  aggregate replacing `TP001`'s per-site message.
- Done: `chain-lab`'s 11 lines produce a bounded number of declarations, the same twice; corpus
  goldens unchanged or the change explained.

**Lane D — the runtime import specifier (recon blocker 5).**
737 imports across the corpus point at a package with no runtime, which blocks every `@types/*`
rung rather than `three` specifically. Needs a `GeneratorConfig` key — `RuntimePackage: string
option`, defaulting to the package name with a `@types/` prefix stripped — and a test that a
types-only package renders its runtime specifier.

- Owns the config type and `Shape/ExportNames.fs`. Lab: `types-only-lab` (new).
- Purely additive; the recon calls it the cheapest of the five.
- Done: a types-only fixture renders imports against the runtime package, and the existing
  `@types/*` goldens move exactly where that predicts.

**Lane F — `TR023` reconnaissance. No code change.**
727 sites say a named type is not among the generated declarations, and nobody has established
why. This is the largest addressable widening in the corpus and it is not yet a task — it is a
question. Produce the same artefact the `three` recon produced: the distinct causes behind the
count, each reduced to the smallest `.d.ts` that reproduces it, with a recommendation on which are
worth fixing.

- Read-only. Writes one document under `docs/plans/`. No findings to pre-declare.
- Done: the 727 are attributed to a small number of named causes with reproducers.

### Batch 2 — after batch 1 merges, one agent at a time in `Shape/Spec.fs`

**Lane C — structural `extends` becomes nominal `:>` (recon blocker 3).**
328 `FS0001` and one `FS0043` on the `three` rung. A TypeScript `extends` constraint is structural;
rendering it as an F# `:>` makes it nominal, so an argument that satisfies the constraint
structurally but not nominally fails to compile. This is the blocker that decides whether generated
constraints are usable at all, so it goes first.

- Owns `Shape/Spec.fs` (type-parameter mapping) and `Render.fs`. Lab: `nominal-lab`.
- Findings to pre-declare: one for a constraint dropped rather than rendered nominally.
- Done: `nominal-lab` compiles; the corpus `FS0001` count is zero and the tier movement is
  reported.

**Lane E — `Conditional` type mapping (`TR014`, 227).**
The last well-defined unmapped type flag. Scoped, previously queued, and unblocked once C has
landed.

- Owns `Shape/Spec.fs` (`typeRef`). Lab: `conditional-lab` (new).
- Findings to pre-declare: `TR014` narrows; expect one new case for conditionals that still have
  no F# form.
- Done: `TR014` falls, what replaces it is named, and no golden moves unexplained.

## Sequencing

```
pre-dispatch  →  [ A | D | F ]  →  merge  →  C  →  E  →  merge
```

Batch 1's three lanes touch `Shape/Anonymous.fs`, the config plus `Shape/ExportNames.fs`, and
nothing. Batch 2's two both rewrite parts of `Shape/Spec.fs` and run one after the other.

## Deliberately not in this wave

- **`@types/three` as a golden rung.** The recon's recommendation stands: four independent reasons,
  any one sufficient. Reconsider only once blockers 1–4 are closed and a re-run reports a bounded
  rendered file and a deterministic manifest.
- **`TR032`, `MB003`, `SP001`, `TR008`.** Working as designed; see above.
- **`TR018`** (232, intersection over a non-object operand). Real, but smaller than everything in
  batch 2 and not yet reduced to a cause. Fold it into lane F's brief if that recon finishes early.
