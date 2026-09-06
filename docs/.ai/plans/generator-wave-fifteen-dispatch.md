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

Lane DA is still running; DB and DC are composed and gated on the integration branch.

| | at item zero | batch one |
| --- | ---: | ---: |
| exact | 543 | 545 |
| ergonomic | 1623 | 1630 |
| widened | 793 | **789** |
| escape | 206 | 206 |
| symbols | 3,165 | 3,170 |
| total findings | 17,121 | 17,107 |
| generator tests | 522 | 526 |
| run gate checks | 323 | 323 |
| `TR036` | 6 | **0** |
| `TR020` | 53 | **36** |

**Composition found nothing.** Regenerating the goldens on the merged tree
produced an empty diff, so the two lanes are independent in fact and not only by
inspection - unlike wave thirteen, where five widened symbols existed on neither
branch alone.

**Every tier movement in the batch is an improvement.** Reconciled per symbol
against `a9f2aeb` rather than from the lanes' own arithmetic:

| Symbol | Movement | Lane |
| --- | --- | --- |
| `@cloudflare/workers-types` `TraceItem` | widened to ergonomic | DB |
| `indexed-access-lab` `onWide` | widened to ergonomic | DB |
| `animejs` `Revertible` | widened to **exact** | DB |
| `solid-js` `createEffect` | widened to ergonomic | DC |
| `solid-js` `createMemo` | widened to ergonomic | DC |
| `solid-js` `createComputed` | widened to ergonomic | DC |
| `solid-js` `createRenderEffect` | widened to ergonomic | DC |

Nothing moved the wrong way and nothing was removed. The five added symbols are
`noinfer-lab`, DC's new fixture: 3 widened, 1 ergonomic, 1 exact.

**DB reported its three and DC reported none.** DC gave its result as a finding
count - `solid-js`'s `TR020` 22 to 3 - which is a different quantity from tier
movement, and its four symbols were established here rather than by the lane. The
clause asks for tier movement per symbol; a finding-key delta does not satisfy it,
and this is a third distinct way of under-reporting after wave thirteen's netting
and wave fourteen's fixture-scoped claim.

### What each lane settled

| Lane | Outcome |
| --- | --- |
| DB | **`TR036` is closed at 6 of 6.** A file needing arity above nine declares that `U<n>` at its footer, in `Fable.Core`'s own shape. Per-file rather than shared, so no cost reaches a consumer that does not need it. |
| DC | **`NoInfer` reaches a type.** Default emits `JS.NoInfer<'T>`; `resolveNoInfer` resolves to the bare operand. `solid-js`'s `TR020` falls 22 to 3. |

### A worklist estimate corrected

The worklist priced item 4 as driving "about 20 of `solid-js`'s `TR045` sites and
about 19 of its 22 `TR020` sites". The `TR020` half held - 22 to 3. **The `TR045`
half was wrong: `TR045` does not move at all.** The idiom's deferred conditional
is not what those findings record. The estimate came from reading the
utility-types document rather than from measurement, and is corrected here so it
is not carried into a later wave as a residue believed to be reachable.

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

---

## Batch two

Re-planned on lane DA's measurement. The worklist ordered 1b before 1c; DA inverted that, so the
DOM half goes first and the ECMAScript half waits on a diagnosis.

| Lane | Item | Kind | Model |
| --- | --- | --- | --- |
| DD | the silent empty harvest | implementation | sonnet |
| DE | why the ECMAScript lib does not complete | read-only diagnosis | sonnet |
| DF | `CompilerLib` carries a `Ship` disposition, DOM end to end | implementation | to dispatch after DD |

DF waits for DD rather than running beside it: both edit `Harvest.fs`, and DD's subject is what
happens when harvest finds nothing while DF changes what harvest finds. The collision is textual
rather than semantic, and sequencing costs less than composing it.

### The design 1f implements, established before briefing it

Read off the tree rather than left to the lane, because wave fourteen showed a brief that asks for
analysis first gets analysis and no change:

- `PackageId.CompilerLib` exists, and `Grouping.classify` already returns it. It recognises the
  default libs three ways, because the compiler serves them three ways: the platform package
  (`node_modules/@typescript/typescript-<rid>/lib/lib.*.d.ts`), `typescript/lib`, and `bundled:`
  pseudo-paths.
- The group already has its `xantham.json` key (`typescript/lib`, `Model.fs:245`) and its module
  name (`Naming.CompilerLibModule`, `"TypeScript.Lib"`, `Model.fs:313`).
- **The shape tier already branches on the disposition.** `Shape/Classes.fs:56` and
  `Shape/Spec.fs:1742` both consult `GeneratorConfig.disposition ctx.Config CompilerLib = Ship`
  and behave differently when it is set.

So the disposition is plumbed from configuration through to shaping, and setting it changes
behaviour today. **What is missing is the harvest tier.** `Harvest.harvestGlobals` keeps symbols
whose classification equals `EntryPackage` and nothing else, so a run with `typescript/lib` set to
`ship` still harvests no compiler-lib declaration and the shape tier's branch never has anything
to act on.

That is the gap 1f closes, and it is narrower than lane DA's workaround implied. Entry-package
impersonation was necessary only because harvest admits one group; it is not the shape of the fix.

### The ECMAScript surface is one surface, at the modern lib level

Decided with the user during batch two, and it settles the packaging question the worklist left
open.

`lib.esnext.d.ts` references `es2025`, which chains down through every year to `es5`, and each
year's feature files reopen the interfaces the earlier years declared. The libs describe a runtime
that keeps backward compatibility, so a later year augments rather than replaces: **the modern
`Array` is all eight of its declarations merged.** Targeting only the modern lib is therefore the
full transitive closure, not a subset of it.

Two consequences, in opposite directions.

**It settles the shipping layout.** One ECMAScript surface, not a package per lib year. This is
also the only layout F# can express: F# cannot reopen an interface, so a symbol merged across lib
levels has to be emitted once at its fully merged shape. A split putting `Array`'s es5
declarations in one package and its es2015 additions in another is not expressible; a split
putting all of `Array` in one package and all of `Intl` in another is. Lane DE's lib-level
bisection is a diagnostic instrument and must not be read as a candidate layout.

**It does not reduce merge depth, so it does not avoid what lane DA hit.** "Modern only" is
precisely DA's failing input. If the cost scales with declarations merged per symbol, it is paid
in full at `esnext`. What the decision does buy is that the fix is needed once, for one target
surface, rather than per lib level.

### Lane DD, landed

`HG.NothingHarvested` carries the in-scope count and where those symbols went, grouped by
`Grouping.classify` and ordered by population, so an entry whose declarations are classified away
names `typescript/lib` as their destination instead of reporting only that it found nothing.

Exit code stays 0, and the reasoning is worth keeping: `dom-shadow-lab` is an existing gated
fixture that reaches this path correctly, since a global-script package whose declarations are
pure declaration-merge additions has nothing of its own to export. Failing on zero declarations
would break a correct case to catch a confusing one. The defect was diagnostic quality.

No tier movement in either direction, verified per symbol at merge.

