---
category: Generator
audience: managing agent
title: Dispatch - generator wave fifteen
integration-branch: worktree-generator-wave-fifteen
---

# Generator wave fifteen — the shipped compiler-lib package

`worktree-generator-wave-fifteen` forks `master` `fd2a10b`, wave fourteen composed and closed. The
worklist is `docs/.ai/plans/generator-wave-fifteen.md`; this document carries the triage verdict
over it and the record of what each lane settled.

## Baseline, measured on the integration branch at item zero

Item zero is `2e2e9b2`; the worklist is `a9f2aeb`. `master` was red at the fork, so the baseline is
recorded after the fix rather than at the fork itself.

| | at item zero |
| --- | ---: |
| exact | 543 |
| ergonomic | 1623 |
| widened | 793 |
| escape | 206 |
| symbols | 3,165 |
| total findings | 17,121 |
| generator tests | 522 |
| run gate checks | 323 |

Identical to wave fourteen's close in every tier column: item zero changed spelling only.

Targeted keys: `TR024` 147, `TR023` 136, `TR020` 53, `TR036` 6.

**`TR024` is 147, not the 70 the utility-types document enumerated.** That document listed 70
against named `Fable.Core` destinations — `ArrayBufferView` 63 of them. The other 77 are
unattributed, distributed `@cloudflare/workers-types` 118, `type-fest` 21, `lib-lab` 7, `animejs`
1 across the key as a whole. Attribution is owed before item 2 is priced.

## Triage verdict

| Worklist item | Verdict | Lane | Batch |
| --- | --- | --- | --- |
| 0 — support package shadowed into `Fable.Core.JS` | Taken, landed before dispatch by the managing agent. Every lane forks from it. | — | — |
| 1a — recon, `typescript/lib` as an entry package | Taken **as recon only**. Read-only; gates 1b–1f. | DA | one |
| 3 — erased-union arity above nine, emitted inline | Taken. Directed, unpriced, proof obligation waived. | DB | one |
| 4 — `NoInfer` toggle in `xantham.json` | Taken. | DC | one |
| 1b — ECMAScript half into `Xantham.Fable.Core` | Deferred to batch two, behind DA. | — | two |
| 1c — DOM half into `src/Xantham.Fable.Browser` | Deferred to batch two, behind DA. | — | two |
| 1d — `tools/browser-gen` rewritten | Deferred to batch two, behind DA and 1c. | — | two |
| 1e — `SupportBindings` at the whole shadowed surface | Deferred to batch two; may force the reserved-name redesign item zero declined. | — | two |
| 1f — `typescript/lib` off `Widen` | Deferred to batch two, behind DA. | — | two |
| 2 — bound name at lower arity than TypeScript gives | Not a lane. Stated as sharing an implementation with 1b and 1c; carried as a property of those. | — | two |

Batch one takes the one item that gates the wave and the two that are independent of it. **1a is
read-only, so it writes nothing DB or DC could collide with**, and DB and DC touch disjoint files
— `Shape/Spec.fs` and `Render.fs` against `Model.fs` and the resolve path.

## Why 1a is alone at the front

The generator has never been run with the compiler lib as an entry package. `lib.dom.d.ts` is an
order of magnitude larger than any fixture, its declarations are mutually recursive across files,
and the `Lib` configuration exists to *exclude* these names rather than ship them. Emitted size,
whether grouping yields a usable DOM/ECMAScript split, whether the naming walk terminates and is
stable across two runs, and what a run costs are all unknown. Every estimate in 1b–1f is a guess
until they are measured, and the DOM/ECMAScript split in particular is the wave's plan rather than
a measured fact.

## Batch one

Three lanes, all on `sonnet`.

| Lane | Item | Kind |
| --- | --- | --- |
| DA | 1a | read-only measurement |
| DB | 3 | implementation |
| DC | 4 | implementation |

### Outcomes

To be recorded as each lane lands.

### Process notes

**Lane DB stopped mid-flight on its first dispatch.** It reported completion having written no
commit, left 14 files modified in its worktree, and gated on `--quick --update --no-run-gate` —
which its brief forbade in terms, because `--quick` skips `format` and `--no-run-gate` removes one
of the two things that would show an emitted `U<n>` is well-formed. It had also parked itself
waiting on a background Monitor task rather than reading a foreground run.

This is wave fourteen's lane CH and lane CN failure repeating across a wave boundary, which puts
it beyond a lane-local surprise: **a brief saying "never `--quick`" does not stop a lane using it,
and a completion notification is not evidence of completion.** Both checks belong to the managing
agent at merge. The lane was re-sent with the correction rather than re-briefed from scratch.
