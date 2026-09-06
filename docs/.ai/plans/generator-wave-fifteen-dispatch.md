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

### The costly shape, narrowed past lane DE

Measured by the managing agent on an idle machine, after lane DG twice failed to act on DE's
statement of it. DE's reproducer split in half by which return types its methods carry, each half
the same length:

| Input | Methods | Time |
| --- | ---: | ---: |
| returns `Foo<T>` and `this` only | 5 | **1.0s** |
| returns `Foo<U>` and `Foo<S>`, fresh parameters | 5 | **7.9s** |
| both together | 10 | **12.9s** |

**Self-reference is not what costs.** An interface whose methods return itself at its own
parameter resolves in a second. The cost is a generic method whose return type applies the
enclosing interface to *that method's own fresh type parameter* - `map<U>(...): Foo<U>`,
`filter<S>(...): Foo<S>`. Five such methods cost eight times what five ordinary self-returns cost,
and the two halves together cost more than their sum.

That is the shape `Array<T>` carries in `lib.es5.d.ts` - `map<U>`, `filter<S>`, `reduce<U>`,
`flat` - and it is why the ECMAScript libs never finish.

Wall-clock numbers in this wave are only comparable when taken on an idle machine. DE measured
the full reproducer at 45.8s under three concurrent lanes; the same input is 12.9s idle. **A lane
given a wall-clock target on this box has no stable signal**, which is what sent DG to an
output-correctness metric it could measure instead. Later lanes get a deterministic count.

### The reproducer is not added to the corpus yet

At 12.9s it would cost a tenth of the whole suite's runtime to gate a bug that is still open. It
lands as a regression fixture once it is fast, and not before.

### Lane DG, failed twice, and what it established anyway

Two dispatches, no change to `Resolve.fs` either time, nothing committed. The second reported
against a duplicate-`Foo2` premise that was never in its brief and does not exist in the tree.

Two negative results are worth keeping, because both were paid for:

- **Keying frontier memoization on the checker's raw `Target` is unsafe.** It collides across
  unrelated named declarations - `hoist-conditional-lab`'s `CondNode` and `DirectNode`,
  `solid-js`'s `Computation`, `@cloudflare/workers-types`'s `File`, a `chain-lab` case - splicing
  one declaration's facts onto another and corrupting generic parameter lists. This rules out the
  obvious dedup.
- A safe structural key has to carry the declaration's own identity - the `SymbolName`/`Origin`
  pair the emitter already uses to decide whether two responses are the same named declaration -
  **and** the type arguments applied at the reference. Collapsing to either alone is what breaks.

---

## Wave fifteen, closed

Final tip gated with `XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test`: **535 generator tests,
90 wire (1 skipped by design), run gate 323 checks, exit 0**, tree clean.

| | at item zero | closed |
| --- | ---: | ---: |
| exact | 543 | 549 |
| ergonomic | 1623 | 1647 |
| widened | 793 | **791** |
| escape | 206 | 212 |
| symbols | 3,165 | 3,199 |
| generator tests | 522 | 535 |
| `TR036` | 6 | **0** |
| `TR020` | 53 | **36** |

Reconciled per symbol against `a9f2aeb`. **Seven symbols moved and every one improved; nothing
regressed and nothing was removed.** The thirty-four added symbols and the whole of the escape
column's rise belong to four new lab fixtures - `noinfer-lab`, `lib-reference-lab`, `lib-ship-lab`
and `frontier-width-lab` - which did not exist when the wave opened. No existing fixture's escape
count moved.

### What the wave was for, and what it returned

It was opened to publish Fable bindings for the compiler's own lib: the DOM half as
`Xantham.Fable.Browser`, the ECMAScript half into `Xantham.Fable.Core`. **Neither exists.**

Seven items shipped, none of them that one:

| | Item |
| --- | --- |
| item zero | the support package shadows into `Fable.Core.JS`; generated output carries no Xantham-named token |
| 3 | erased-union arity above nine, declared per file at the footer; `TR036` closed at 6 of 6 |
| 4 | `NoInfer` reaches a type behind `resolveNoInfer`; `TR020` 53 to 36 |
| DD | `HG003` names what it discarded rather than only that it found nothing |
| 1f | the compiler-lib group is harvested when its disposition ships, gated by `lib-ship-lab` |
| DH | frontier expansions and table size counted behind `XANTHAM_RESOLVE_COUNTERS` |
| DI | the frontier's width is bounded; the generator no longer exhausts memory |

Items 1b, 1c, 1d and 2 did not start. `TR023` 136 and `TR024` 147 are untouched, and
`fable-binding-gaps.md` is still unanswered.

### Why they did not start

One defect, found by this wave and not fixed by it. **A generic method whose return type applies
its enclosing interface to that method's own fresh type parameter costs the resolve tier
disproportionately** - 24 frontier expansions against 39,908 for inputs of the same length.
`Array<T>` has that shape, so the ECMAScript libs never complete, and `lib.dom` reached 6GB before
lane DI bounded the width.

DI's bound ends the crash and is not a fix: `lib.dom` completes by widening 78.6% of its frontier
to `obj`. A DOM binding four fifths widened is not a DOM binding.

**Six approaches were tried. Five changed the frontier's identity and all five moved output**;
three died on `hoist-conditional-lab`. The sixth, the width bound, is the one that shipped, and it
is a safety valve. The identity the frontier needs is not one the checker's responses carry, and
finding it is a design problem rather than a lane brief.

### Wave sixteen opens on this

1. **The resolve tier's identity model** - how a type reference is keyed when its arguments are
   open. Written design first. Every attempt in this wave is catalogued in
   `docs/.ai/handovers/wave-fifteen-management.md` so the seventh is not a repeat. The nearest
   lead: DI's withdrawn partial admission would have widened about 41% rather than 78.6%, so a
   stable partial cut ordered by declaration identity rather than by a transient checker id is
   worth roughly half the loss.
2. **Then** items 1b, 1c, 1d and 2, which are blocked behind it and were fully briefed here.
3. **Wave fourteen's six carried items**, deferred twice now and listed in the worklist.
