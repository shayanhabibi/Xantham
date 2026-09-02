---
audience: managing agent
integration-branch: worktree-generator-wave-three
---

# Generator wave three — dispatch plan

Wave two closed at `c54e210` (`docs/plans/generator-architecture.md`, "Wave two, closed"). It left
five open items. This plan spends the wave on the three that a lane can close, and sequences the
rest behind the measurement that prices them.

Read `.claude/rules/generator-fixtures.md` first — the "For the managing agent" section is the
protocol this plan assumes.

## Where the corpus stands

21 fixtures. The four largest keys are settled and no agent is sent at them: `TR032` (5,184,
null-union members hoisted to option), `MB003` (3,849, optional member reads option), `SP001`
(1,378, `ParamObject` `Create` synthesized) and `TR008` (544, `any` maps to `obj`) are the mapping
working as designed; `TR006` (1,212, string literal widened to `string`) was reviewed at the close
of wave two and accepted as intent.

What is left, in order of size:

| Key | Count | Tier | What it says | Lane |
|---|---|---|---|---|
| `TR023` | 727 | widened | named type not among generated declarations | H |
| `TR018` | 232 | widened | intersection over a non-object operand has no members to flatten | deferred — re-measure after H |

`TR023` is the only large key with a completed reconnaissance behind it
(`docs/plans/generator-tr023-recon.md`): 727 sites, three named causes, reproducers built.
`TR018` is deliberately not dispatched — the recon's §9 records a concrete reason to think part of
it is the same defect as `TR023`'s cause 1 wearing a different case, so pricing it before lane H
lands would price work that is about to move.

## Before dispatch

Done on the integration branch, in one commit, before any agent starts:

1. **Finding cases pre-declared.** `TR047 ObjectWithoutMembers`, `TR048
   ArrayIntersectionMembersDropped` (lane H) and `RA006 AliasKeptAsPhantom` (lane I), with
   `Findings.test.fs` extended in the same commit. Keys are positional, so this is the one append
   point no pattern removes. Agents raise these and edit `Findings.fs` no further.
2. **Lab names assigned in the briefs** — `array-shape-lab` (H), `phantom-arity-lab` (I). Lane G
   writes no fixture.

## Batch 1 — three agents, disjoint files

### Lane G — is `@types/three` enrollable now? **No code change.**

The wave-two payoff nobody has measured. `docs/plans/generator-three-rung.md` §10 held the rung
back on four reasons and gated reconsideration on blockers 1–5 closing. All five have since been
addressed — 1 and 2 by wave two's lane A, 3 by lane C, 4 by the colon-spacing fix, 5 by lane D —
and no re-run has happened, so whether the output is bounded and deterministic is untested.

- Read-only on `src/`. Appends one section to `docs/plans/generator-three-rung.md`.
- Reuses Appendix A's methods so the numbers compare against the baseline directly.
- Done: the four §10 criteria measured at this HEAD (rendered lines, byte-determinism across two
  runs, compiler errors, gate compile time), the counts for `TP007` and `SY002` on `three`, and a
  recommendation to enrol or to name what remains.

### Lane H — `TR023` cause 1A plus the cause-3 reporting fix

`docs/plans/generator-tr023-recon.md` §8. `arrayElement` matches on the symbol name alone and
misses every real-world spelling of an array; a structural recognizer recovers 509 sites and
~2,164 golden lines. The cause-3 reporting fix rides with it, because leaving it makes the post-1A
count harder to read.

- Owns `Shape/Spec.fs` and `arrayElement`'s call sites. Lab: `array-shape-lab`.
- Findings: raises `TR047`, and `TR048` where an intersection collapse drops members.
- Done: `TR023` at or near 182, the negative pinned by a lab, golden churn reviewed in aggregate.

### Lane I — the alias the arity pass drops rather than keeps

Wave two's lane E measured and handed back: `type-fest`'s `ExcludeStrict` / `ExtractStrict` move
Widened → Escape because a resolved right side uses fewer parameters than its head, and `RA001`
drops an alias it used to keep as a phantom. The repair belongs to the arity pass and moves 9
pre-existing sites, which is why lane E left it.

- Owns `Shape/Arity.fs`. Lab: `phantom-arity-lab`.
- Findings: raises `RA006`.
- Done: `ExcludeStrict` / `ExtractStrict` back off Escape, the 9 pre-existing sites accounted for
  one by one, `RA002` moving with `RA001`.

```
pre-dispatch  →  [ G | H | I ]  →  merge  →  batch 2
```

Lane G writes no source. Lane H owns `Shape/Spec.fs`, lane I owns `Shape/Arity.fs`. Both regenerate
goldens; a golden conflict is resolved by regenerating over the composed tree, never by hand.

## Batch 1 — landed

| lane | result |
|---|---|
| G | `@types/three` still refused: 1 of 4 criteria passes. Compiles clean (0 errors, was 666), but 482,801 lines, non-deterministic, 3 m 47 s gate. Blockers 1 and 2 unmoved; the cause is reduced to 12 lines. |
| H | `TR023` 727 → 182, the predicted number. `TR047` 27, `TR048` 14. `TR018` fell 232 → 194 as a side effect. |
| I | Escape 200 → 190. `RA001` 11 → 1, `AC001` 13 → 3, `RA006` 11. Lane E's handback closed. |

Corpus 15,278 → 14,618 findings over 23 fixtures. Each lane's counts survived composition
exactly, so the branches composed rather than interacted.

## Batch 2 — three agents, disjoint files

### Lane J — an alias body that intersects a conditional is not hoisted

Lane G's reproducer, and the reason wave two's lane A did not carry to `three`: an alias whose
body intersects a conditional type is not recognised as a hoistable instantiation, so `SY001`
fires 4 times on a control and zero times on the reproducer. This is blocker 1, which decides the
rung.

- Owns `Shape/Anonymous.fs`. Lab: `hoist-conditional-lab`.
- Findings: `SY003` where one operand of an intersection resists hoisting.
- Done: the reproducer bounds at the control's declaration count, and `three` re-measured.

### Lane K — the `Shape/Spec.fs` queue, in one lane because they share a file

Three items, ordered. The first is a live compile-gate defect that lane I had to guard around:

1. `aliasTypeParams` appends every call signature's type parameter without deduplicating, so
   `solid-js`'s `Setter` renders `Setter<'T, 'U, 'U, 'U, 'U>` — FS0037. Raises `TP009`.
2. `Setter` then renders `Action<obj[]>` through §4.12's empty-tuple rest widening where §4.11's
   call is what it wants. Wave two's second handback.
3. `TR044`'s substitution matches `FsNamed` / `FsApp` / `FsObj` / `FsTypeVar` only. A primitive or
   tuple argument against a still-written constraint falls through to a faithful reference and is
   `FS0001`. Nothing in the corpus does this, so the compile gate is an absence, not a closed
   hole. A lab settles it either way.

- Owns `Shape/Spec.fs`. Labs: `setter-lab` (1 and 2), `constraint-arg-lab` (3).

### Lane L — `TR023` lane 1B, and `TR018` re-priced

The remaining 86 cause-1 sites: extend `isAnonymousShape` so a symbol whose flags say *member*
rather than *type declaration* resolves structurally regardless of its group. The recon requires
1A first, which has landed, and warns that 1B deepens the type table on lib-heavy rungs and
converts some sites into a different finding rather than into exact.

- Owns `Resolve.fs`. Lab: `member-shape-lab`.
- Also re-measures `TR018`, now 194 after lane H took 38 of it, and says whether what remains is a
  distinct defect or the same one.

```
[ J | K | L ]  →  merge  →  wave record
```

## Deliberately not in this wave

- **`TP007` and `SY002`, which fire zero on the corpus.** Both were declared for blockers measured
  on `@types/three`, which is not enrolled, so a dead case is not established until lane G reports.
  Deleting them renumbers every case after them, and the renumbering is the expensive half.
- **`TR023` cause 2** (95 sites). 84 are correct behaviour. The recoverable 11 are four table rows
  in `Model.fs` and belong as a rider on some other lane.
- **`TR032`, `MB003`, `SP001`, `TR008`, `TR006`.** Settled, above.
