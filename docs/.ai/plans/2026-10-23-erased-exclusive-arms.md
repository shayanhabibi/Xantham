# Erased exclusive-arm unions

> Execute inline with `superpowers:executing-plans`; review the completed slice independently.

**Goal:** Preserve callable and constructable exclusive arms as typed cases of an erased DU.

**Spec:** The user's October 23 D2 decision: cases accept existing typed arms, preserve identity,
retain each arm's members, expose shared `Invoke` only when every arm agrees, and support normal
matching only where TypeScript facts provide a reliable discriminator. This supersedes Task B's
blanket refusal of callable arms in the October 15 exclusive-fold plan.

**Architecture:** Keep the existing plain-data fold. Introduce a separate erased-object-union
declaration with one typed payload and a predicate per case. Detect it before interface folding,
retain its arm declarations, and carry its references through naming, generics, ordering,
catalogues and rendering. A callable or constructable union that cannot be safely separated
retains its individual arms through the existing union fallback.

**Tech stack:** TypeScript 7.1 beta facts, F#, Fickl/Fickl.Core 0.5.4.

## Constraints and review focus

- Construction and extraction preserve the original value and its prototype; no tag or copy.
- An optional `never` property can hold `undefined`: property presence alone is insufficient.
- Every generated predicate must positively identify its case, including case testers; an
  exhaustive match's last branch is not evidence that the last predicate works.
- Shared calls require source-signature agreement, not just equal widened F# spellings.
- Retain generic arguments and declarations referenced only by payloads or shared calls.
- Preserve source-named arms; deterministic case names must avoid collisions with `Invoke`.
- Goldens are regenerated through the build script. Measure corpus changes; stop on unexplained
  output. No Core.TS/Node regeneration without evidence they contain affected shapes.

## Slice 1: released compiler prerequisite

- [x] Add consumer runtime checks for matching, shared calls, identity, constructors and single evaluation.
- [x] Observe failure under Fickl 0.5.3 (3 of 601 run-gate checks failed).
- [x] Pin compiler, support dependency, customization validator and standalone repro to 0.5.4.
- [x] Run the complete test pipeline with the runtime and Partas gates; review and commit.
  PR #165 merged as `4bcfad547e3721398de360257ace2ec82694ea31`; all CI checks passed.

## Slice 2: generated erased unions

Files: `Model.fs`, `Shape/Spec.fs`, new `Shape/ErasedObjectUnions.fs`, the Shape pass list,
`Shape/Anonymous.fs`, `Shape/Interfaces.fs`, `Shape/FreeTypeParams.fs`, `Shape/Aliases.fs`,
`Shape/Arity.fs`, `Shape/Ordering.fs`, `Shape/Orphans.fs`, `Shape/Coverage.fs`,
`DeclarationCatalog.fs`, `Render.fs` and their tests.

- [x] Add a surgical `erased-exclusive-lab` fixture: one callable arm, shared and different calls,
  constructor arms, explicit undefined on an absent-only property, ambiguous optional arms,
  generic payloads, source-named arms and escaped property names. Register pipeline, compile and
  runtime coverage. Observe the generator regression before implementing the model/pass.
- [x] Introduce `FsErasedObjectUnionDecl` and its case payloads/tests/shared calls. Run semantic
  impact analysis before changing the public model. Extend every declaration traversal.
- [x] Add a conservative eligibility/discriminator helper in `Shape/Spec.fs`. Required own
  properties whose types exclude undefined can distinguish optional-never alternatives via a
  defined-value test; ambiguous arms retain the existing fallback. Literal-only discrimination
  remains a separate increment. Keep callable/constructable arms out of the plain-data fold.
- [x] Preserve arm names and generic parameters; shape the DU before aliases. Expose matching
  shared call signatures as `Invoke`, with each arm's original call/construct/index members intact.
- [x] Render `[<Erase>]` and `Fickl.Core.CaseTest`, typed case payloads and emitted shared calls.
  Append and pin a finding describing this representation; retain existing finding codes.
- [x] Run targeted lab/generic/catalogue tests, regenerate goldens, compare findings and read
  aggregate diffs. Run the full test and runtime pipelines. Update architecture/mapping records.
- [ ] Review, commit and submit above slice 1 using `gh stack`; monitor only the active slice.

## Execution notes

Catalogue API version remains 8: the new canonical declaration shape changes the affected
per-declaration API hash, which catalogue reuse always compares. Earlier interface/alias
representations therefore fail authentication individually; no new serialized catalogue field
requires a global contract rejection or Core.TS regeneration.

The first corpus regeneration changes only `tagged-arm-lab` among existing fixtures. Its `T1`
now retains the callable arm's `Invoke`; exact/ergonomic/widened/escape counts move from
16/88/5/0 to 17/89/5/0 as the two arm declarations survive. No existing large golden changes.

Validation: full `test --run-gate` passed 1,520 generator tests and 105 wire tests (one ignored
in each suite), 617 runtime checks, and the Partas gate. After the final intersection safeguard
and catalogue consumer compilation assertion, `test --quick --update --filter erased` passed
all nine tests in both write and check phases. Corpus regeneration passed all 140 golden tests.
Independent spec and standards reviews found overload collision and generic alpha-equivalence
gaps; both are fixed and covered. API hashes authenticate catalogue reuse without a contract bump.

The prerequisite's earlier version-probe timeout passed in isolation and all its CI checks
passed. The generator slice's full run had no failures or timeouts.
