---
category: Generator
audience: managing agent
title: Plan - generator wave five
integration-branch: worktree-generator-wave-five
---

# Generator wave five — the multi-package story

Wave four closed at `6d470e7` (`docs/plans/generator-architecture.md`, "Wave four, closed"). The
corpus stands at **30 fixtures**. This wave changes subject: **no lane is priced on finding
counts**. Wave two, three and four each bought their scope from the finding aggregate, and that
aggregate describes a single package generated in isolation. What it cannot describe is the thing
O7 was designed for and the thing no run has ever done — **more than one package**.

Read `.claude/rules/generator-fixtures.md` first — the "For managing agent" section is the
protocol this plan assumes.

## What O7 promised and what exists

`docs/plans/generator-architecture.md` §7, decision O7, classifies every type's origin into a
*group* and gives each group a disposition. Its status line reads: `ship`, `reference` and
`widen` are implemented; `map` and `inline` are not. Read against the source, that status is
generous in one place and exact in the others:

| disposition | promised | in the tree |
|---|---|---|
| `ship` | group emitted as its own module (its own package) | **the entry group only.** A dependency configured `ship` resolves fully (`Resolve.fs:423`) and is then emitted nowhere. One run writes one module. |
| `reference` | `FsNamed` into the group's templated module | implemented (`Shape/Spec.fs:864`). Exercised by four unit tests over a synthetic config; **no fixture, golden or gate has ever generated one package against another.** |
| `map` | redirected to an existing package | not built. The two pinned tables (`Naming.LibBindings`, `Naming.BrowserBindings`) redirect *below* the disposition, hardcoded to `CompilerLib` in `Shape.libBinding`; any other group has no way to be redirected. |
| `inline` | folded into the entry group, demand-driven | not built; needs demand-driven resolve. |
| `widen` | `obj` + finding | implemented, and the default for every non-entry group. |

The multi-package story rests on three claims, of which **one is untested and two are
unimplemented**:

1. *Order stops mattering* — "generate B against already-generated A" and "generate B first" emit
   identical source, because a `reference` group templates exactly the names a real `ship` run of
   that group produces. Nothing compares a templated name against a shipped one.
2. *A group can be shipped* — beyond the entry package. It cannot.
3. *A group can be redirected* to a binding somebody already wrote by hand. Only the two lib
   tables can, and only for the compiler lib.

Those are the three lanes.

## Not in this wave

- **No finding reduction.** `TR018`'s remaining 77, `TR023` cause 2, the cloudflare causes behind
  `TR036`/`TR037`, `alignOperands` and the `FollowDepth` cutoff all stand where wave four left
  them. A lane may report a count that moved as a side effect; **no lane may take a count as its
  objective**, and a lane that finds itself tuning one has drifted.
- **`@types/three`.** Still unenrolled, still not re-measured, still not a reason for anything.
- **The `Fable.Core` binding gaps** (`docs/fable-binding-gaps.md`), which the wave-four record
  named as wave five's first lane. They are hand-written bindings in `src/Xantham.Fable.Core`,
  independent of everything here, and they are **priced behind batch 1** rather than run beside
  it: `map` is the mechanism by which a generated package points at those bindings, and writing
  them before the mechanism exists fixes their shape prematurely.

## Before dispatch — done, in one commit

**Finding cases pre-declared.** Codes are positional. Five cases are appended and
`Findings.test.fs` updated in the same commit. Each lane **raises the cases named below and edits
`Findings.fs` no further**.

| key | case | tier | lane |
|---|---|---|---|
| `TR052` | `TypeReference.AnonymousInMappedGroup` | widened | R |
| `TR053` | `TypeReference.MappedNameArityMismatch of name: string * given: int` | widened | R |
| `GE001` | `EmitGroups.GroupShipped of group: string * declarations: int` | exact | S |
| `GE002` | `EmitGroups.ShippedGroupWithoutDeclarations of group: string` | widened | S |
| `GE003` | `EmitGroups.GroupModuleCollision of group: string * moduleName: string` | escape | S |

`EmitGroups` is a new union with prefix `GE` and no bound pass, appended last to
`FindingCatalogue.unions`. `GE003` is pre-declared because the collision is findable on paper:
`Naming.packageModule` derives the module from the *runtime* package, so `@types/three` and
`three` both template `Three`, and one run shipping both writes two modules of one name.

## Batch 1 — three lanes, disjoint files

```
[ R | S | T ]  →  merge  →  batch 2
```

Lane R owns `Model.fs` and `Shape/Spec.fs`. Lane S owns `Render.fs` and `Pipeline.fs`. Lane T
writes no `src/`. `Shape/Spec.fs` is the file every pass shares, which is why exactly one lane
holds it and why the other two request a `Model.fs` edit rather than make one.

### Lane R — `map`: a group redirected to a binding somebody already wrote

`Shape.libBinding` intercepts a compiler-lib name and rewrites it to `Fable.Core.JS.*` or
`Browser.Types.*` before the disposition applies. It is hardcoded to `CompilerLib`, so a package
whose `@types/node` types should read `Fable.Node.*`, or whose dependency has a hand-written
binding, has no way to say so. Lift the interception into a `Map` disposition.

- Owns `Model.fs` and `Shape/Spec.fs`. Lab: `group-map-lab`.
- **The destination carries arity.** This is the whole safety argument, and O7 records it: the
  compiler's lib drifts, and a redirection that guessed an arity emits code that does not
  compile. A mapped name applied at an arity the destination does not take raises `TR053` and
  widens, rather than emitting the application.
- The config spelling is the lane's to design, under one constraint: it extends to a per-name
  table (an F# name differing from the TypeScript name, plus its arity) **without a breaking
  change** to the group-level form. `Naming.LibBindings` is that table's shape.
- The two existing tables keep working exactly as they do. Whether the compiler lib becomes an
  ordinary `map` group is the lane's call to make and report; it is not required.
- Findings: `TR052`, `TR053`.
- Done: `group-map-lab` pins a mapped group, a mapped generic at correct arity, and both negatives
  (an unmapped name in a mapped group, an arity the destination does not take).

### Lane S — a shipped group is emitted, beyond the entry package

One run writes one module (`Render.fs:558`). Make it write one file per `ship` group, each
`module rec` over `Naming.groupModule`, so a package and a dependency it needs generate together.
`Pipeline.fs` already carries `rendered.Files` as a list that has only ever held one element.

- Owns `Render.fs` and `Pipeline.fs`. Lab: `multi-ship-lab` — a two-package fixture (a lab with a
  hand-authored `node_modules/<dep>` carrying its own `package.json` and `.d.ts`), the dependency
  configured `ship`.
- The resolve tier already resolves a `ship` dependency in full. **The missing work is emission**;
  a lane that finds itself in `Resolve.fs` should stop and report.
- Cross-group references already template through `Naming.groupModule`, so a reference from the
  entry module into the shipped dependency module should need no new machinery. Report it if it
  does.
- Findings: `GE001`, `GE002`, `GE003`.
- Done: two modules from one run, each compiling under the gate, the entry naming the dependency's
  types; the collision case (`GE003`) provoked by a lab rather than argued.

### Lane T — the contract that order does not matter, proven

O7's first consequence is that generation order stops mattering. Four unit tests assert that a
`Reference` group renders `Module.Name`; nothing asserts that **`Module.Name` is what a `ship` run
of that group declares**. That is the claim every independently generated package rests on, and it
is the cheapest one in the wave to break by accident.

- Writes no `src/`. Owns a new `tests/Xantham.Generator.Tests/MultiPackage.test.fs`, its fixture,
  and the compile-gate policy below. Lab: `cross-package-lab`.
- The test to build: generate the dependency standalone (it is its own entry package, so it ships
  today), generate the entry with the dependency configured `reference`, and assert that **every
  name the entry templates is declared by the dependency's own golden, at the same arity**. A name
  templated into a module declaring nothing of that name is the failure this lane exists to catch.
- Second half: the same entry package generated with and without the dependency present produces
  identical source, byte for byte.
- **Settle the recorded open question.** O7 leaves open what the compile gate means for output
  whose `reference` groups are shipped nowhere: gate only closed configurations, or synthesize
  stub assemblies from the templated identities. The doc says "simplest first: gate closed
  configurations only." Implement that unless the fixture shows it wrong, and record which.
- Findings: none expected. A finding this lane needs is a request back to the manager.
- Done: the contract holds or it does not, stated as a test rather than a paragraph.

#### Lane T result — the contract does not hold

`cross-package-lab` is two hand-authored packages side by side under one `node_modules`:
`cross-package-dep` declares an interface (`Widget`), a generic at arity 1 (`Box<T>`) and an
object alias (`WidgetPair`); `cross-package-lab` reads all three. Each half is registered in
`Pipeline.test.fs` as its own entry package, and `MultiPackage.test.fs` generates the entry a
second time with the dependency configured `reference`.

Two of the three shapes break, and the repairs belong to `Shape/Spec.fs` and `Resolve.fs`, which
this lane does not own:

1. **A templated generic loses its arguments.** `Shape/Spec.fs`'s `Reference` arm renders
   `FsNamed $"{groupModule}.{typeName}", []`, discarding `facts.TypeArguments`, so `Box<string>`
   and `Box<Widget>` both come out as a bare `CrossPackageDep.Box`. The dependency's own golden
   declares `type Box<'T>`. Three sites, `error FS0033: The type 'CrossPackageDep.Box<_>' expects
   1 type argument(s) but is given 0`, and no finding is raised. `instantiationOf` is what would
   normally re-apply the arguments, and it looks the target up in `DeclNames` - which a
   `reference` group is never in, since identity-only resolution declares nothing.
2. **A referenced object alias is copied instead of named.** `WidgetPair` is a type alias over an
   object literal, so the type the checker hands back carries the symbol `__type` and
   `Resolve.fs`'s `isAnonymousShape` is true; the O7 shortcut is skipped and the dependency's
   shape is re-derived into the entry package as `PanelPair`. Two packages then declare the same
   TypeScript type as two unrelated F# interfaces. This one compiles, which is what makes it the
   more expensive of the two. The alias symbol is on `aliasSymbol` rather than on `symbol`, so
   the "a mapped type has no name to defer to" reasoning `isAnonymousShape` is written for does
   not describe this case.

`Widget` - a plain interface - templates and resolves correctly, which is the whole of what the
four existing unit tests and the `ansi-regex` `typescript/lib` case cover.

Order-independence holds: the entry and the dependency are byte-identical whichever is generated
first, and the entry is byte-identical whether or not the dependency has been generated before
it. Generation reads no earlier output, and `MultiPackage.test.fs` now asserts it in both orders.

#### The compile-gate policy for open configurations

**Gate closed configurations only - closure read over the gated corpus, not over the run.** A
golden generated against a `reference` group joins the compile gate when some other gated golden
ships the module it templates into; `cross-package-dep` and `cross-package-lab` are gated
together for exactly that reason, which makes the F# compiler rather than a string comparison the
judge of whether the templated name is the shipped one. A golden whose templated module nothing
ships carries the `.open.fs` suffix and is excluded by one `Exclude` attribute in
`Xantham.Generator.CompileGate.fsproj`.

The entry's `reference` rendering is committed as
`golden/cross-package-lab/CrossPackageLab.reference.open.fs` and is excluded today - not because
its group is unshipped, but because of break 1 above. Renaming it into the gate is the acceptance
test for the fix.

Stub synthesis is refused. A stub is written from the templated identity, so it agrees with the
template by construction, and both breaks above would compile clean against one.

## Batch 1 — landed

| lane | result |
|---|---|
| R | `map` is a real disposition. Configured as a per-name table, because arity cannot be inferred from the site and a group-level rule has nowhere to carry it. The pinned lib tables are **not** folded in — they fire under `reference` too, and one group has one disposition, so `map` cannot mean "redirected *and* templated"; the two compose instead, pinned tables first and configured table for the rest. `TR053` 1 (the lab's own negative), `TR052` 0. |
| S | A shipped group is written as its own module under `groups/`, one file per `ship` group. A name crossing the boundary is spelled exactly as `reference` templates it, so a shipped group and a referenced one are interchangeable at the use site. The module-name collision is real and pinned: `dep-lab` and `dep_lab` both derive `DepLab`, the entry claims first, the loser keeps its declarations in the entry module and says so. `GE001`, `GE002`, `GE003` one each. |
| T | **The contract does not hold.** Two breaks, neither repaired — both are `src/` files this lane does not own. Order-independence itself holds byte for byte. Gate policy settled: closed configurations, corpus as the unit of closure. |

Corpus 31 → 35 fixtures. **Every pre-existing fixture is byte-unchanged**: regenerating over the
composed tree moved no golden and no manifest, so each lane's counts survived composition exactly
and no interaction appeared. That is the whole aggregate story of this wave, as designed — `map`
is inert until configured, `ship` emission is inert until a group is configured `ship`, and lane T
wrote no `src/`.

Gates over the composed tree: 337 generator tests passed, 2 skipped, 85 wire tests passed, compile
gate built, run gate 98 checks passed.

### The two breaks, which are what this wave bought

1. **A templated generic loses its type arguments.** `Shape/Spec.fs` renders a `reference` name as
   `FsNamed "{module}.{name}", []`, discarding `facts.TypeArguments`. `Box<string>` reads
   `Dep.Box` against a dependency declaring `Box<'T>`: `FS0033` at every site, and **no finding
   raised**. `instantiationOf` is what would re-apply them, and it resolves the target through
   `model.DeclNames`, which identity-only resolution never fills. Repair belongs in `Shape/Spec.fs`
   beside lane R's work. `golden/cross-package-lab/CrossPackageLab.reference.open.fs` is committed
   and excluded from the gate because of this; renaming it into the gate is the acceptance test.
2. **A referenced object alias is copied rather than named.** An alias over an object literal
   carries `__type` on `symbol` rather than `aliasSymbol`, so `isAnonymousShape` is true, the O7
   shortcut is skipped, and the dependency's shape is re-derived into the entry package under a
   second name. One TypeScript type becomes two unrelated F# interfaces. **This one compiles**,
   which makes it the more expensive of the two. Disposition-independent — it happens under `widen`
   as well. Repair belongs in `Resolve.fs`.

### Two constraints found on the way

- **`Grouping.classify` cannot see a nested dependency.** The entry-package check runs before the
  `node_modules` check, so a dependency installed *under* the entry package — npm's nested layout,
  which is what a version conflict produces — classifies as `EntryPackage` and is shipped into it.
  Both multi-package labs are laid out side by side to avoid it.
- **`TR052` and its twin `TR021` are unreachable from a live fixture.** `Resolve.fs` skips the
  identity-only shortcut for anonymous shapes, so an anonymous shape from a non-entry group
  resolves fully and is declared; it never reaches the disposition match. Both stand at 0 across
  the corpus, pinned by synthetic unit tests. A future `inline` is the first thing likely to
  construct one.

## Batch 2 — priced by batch 1, not before

1. **The two breaks above**, in that order. Break 1 is bounded and has an acceptance test already
   committed. Break 2 is unbounded until somebody reads what `aliasSymbol` costs, and it is the
   one that ships wrong output silently.
2. **`inline` and demand-driven resolve.** The named prerequisite for shipping large groups.
2. **The compiler lib as a real shipped package.** O7's default flips from `widen` to `reference`
   "once the shipped compiler-lib package exists". Lanes R and S together are what make it
   buildable.
3. **The `Fable.Core` binding gaps** (`docs/fable-binding-gaps.md`, four entries, the user's
   disposition recorded in the wave-four record): hand-rolled or generated into
   `src/Xantham.Fable.Core`, shadowing `Fable.Core.JS`, PR upstream once the generator works.
   Shaped by whatever `map` turns out to be.
4. **The CLI** (`xantham generate <package-dir>`), deferred since phase C, plus the JSON Schema
   generated from the config record — both grow in value once `groups` is a key anyone tunes, and
   the CLI is how a package *family* gets generated at all.

## Batch 2 - dispatched

Four lanes, off `fbe0c13`, which pre-declares the case keys so four unions can be appended to
without racing for a position. `inline` is held back: it stays unpriced until lane W reports what
shipping a large group costs, and lane W owes that number whether or not its fix lands. The
`Fable.Core` gaps wait on `map` proving itself in use.

| Lane | Objective | Owns | Keys | Fixture |
| --- | --- | --- | --- | --- |
| U | Break 1: a referenced generic carries its type arguments | `Shape/Spec.fs`, the compile gate | `TR054`, `TR055` | `cross-package-lab` (existing) |
| V | Break 2: a referenced alias is named rather than copied | `Resolve.fs` | `RT003` | `alias-copy-lab` |
| W | The compiler lib ships; a nested dependency classifies by nesting | `Model.fs` (`Grouping`), config | `GE004` | `nested-dep-lab` |
| X | `xantham generate <package-dir>`, and a schema for `groups` | `src/Xantham.Generator.Cli/`, `build.fsx`, additive `Pipeline.fs` | none | reuses `*lab` |

`Shape/Spec.fs` goes to exactly one lane, as it did in batch 1. A lane that needs a case it was
not given reports it; appending one races the other three.

Lane U's acceptance test is the rename of
`golden/cross-package-lab/CrossPackageLab.reference.open.fs` into the compile gate. Lane X's is
byte-identical output between the CLI path and the harness path.

## Batch 2 - landed

Four lanes composed. Both contract breaks are repaired, so lane T's two `ptestCase` are live
tests and the suite runs with **nothing skipped**. Corpus 35 -> 37 fixtures. Every pre-existing
golden is byte-unchanged except `cross-package-lab`, which is where both repairs land - one line
carries both: `abstract pair: CrossPackageDep.WidgetPair` beside
`abstract boxed: CrossPackageDep.Box<string>`.

| Lane | Landed | Measured cost |
| --- | --- | --- |
| U | A referenced generic carries the arguments the site applies | `Shape/Spec.fs`, one 20-line helper |
| V | A referenced alias reads under the name its dependency ships | 6,806 extra `getAliasSymbolOfType` calls, inside the noise band |
| W | A nested dependency classifies by nesting; the compiler lib measured and refused | `Grouping.classify` ordering |
| X | `xantham generate`, `xantham schema`, five exit codes | new `src/Xantham.Cli`, no change to `Pipeline.fs` |

### What batch 2 bought beyond the code

**The compiler lib will not ship, and O7's default stays `widen`.** Recorded in full under O7 in
`generator-architecture.md`. The short form: the cost is resolution rather than emission, one
`HTMLElement` reference exhausts a 12.9 GB heap, and the corpus names only 61 distinct
compiler-lib types against the ~20,000 a group walk reaches. `reference` is free (442 ms against
`widen`'s 443 ms). This is the number that prices `inline`, and it prices it as necessary rather
than optional.

**Break 2 was silent, not rare.** One declaration duplicated across the whole corpus, because 34
of 35 fixtures were single-package. A duplicated declaration compiles, so no gate would have
caught it at any corpus size. The lesson is about what the corpus can and cannot see, rather
than about aliases.

**Three pre-declared keys were retired unraised.** `TR055`, `RT003` and `GE004` were reserved
before dispatch so four lanes could append without racing. Each lane then measured its own case
unreachable - `TR055` because control reaches that arm only after both `DeclNames` lookups miss,
`RT003` because TypeScript attaches an alias symbol to every alias body the corpus reaches, and
`GE004` because the classification reorder makes the condition unconstructable. All three were
the last case of their union, so retiring them left every other key at its position. Reserving
keys before dispatch remains right; reserving them is a guess, and a guess that measures out
gets retired rather than fed a contrived fixture.

## Batch 3

**Landed: a package family names itself.** `namespace` in `xantham.json` names the entry package
and every group `groups` lists, so an SDK publishing `@cloudedge/sdk` and `@cloudedge/agents`
reads as `FSharp.CloudEdge` and `FSharp.CloudEdge.Agents` with one key per member. Three call
sites carry the whole naming surface (`Pipeline.moduleName`, `Pipeline.groupModules`,
`Shape/Spec.fs`'s reference arm), so nothing inside generation moved: **zero golden churn**,
371 tests. `GE004` records each group named this way, since the name asserts something about a
run performed elsewhere.

### Still open

1. **`inline` and demand-driven resolve.** Now priced, and priced as the prerequisite it was
   named as. It has to resolve what a package references rather than what its group contains,
   and its scoping has to cover `Unclassified` shapes: one `Date` under `esnext` put 37
   anonymous lib declarations into a consumer's own module.
2. **The `Fable.Core` binding gaps** (`docs/fable-binding-gaps.md`, four entries). `map` is now
   built and in use, so the shape these take is settled.
3. **Publishing the CLI.** `build.fsx -- pack` filters `srcProjects` to names ending `Wire`, so
   `Xantham.Cli` is packable and never packed. A release decision.
4. **Group emission ordering.** `groups/*.fs` compiles in filesystem order, which happens to be
   correct for `nested-dep-lab`. A shipped group sorting after its dependent breaks the gate
   with no rule to point at.
5. **A dying tsgo child reports `EndOfStreamException` at `Msgpack.readFrame`** with no
   indication the child is gone. Cost lane W one run to diagnose.

## What every lane reports

Per `.claude/rules/generator-fixtures.md`: the finding codes raised, the fixture counts before and
after, and — this wave specifically — **whether the count movement was incidental**. A lane whose
objective was a mechanism and whose report is a finding delta has answered a question nobody
asked.
