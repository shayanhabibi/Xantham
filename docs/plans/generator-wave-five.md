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

## Batch 2 — priced by batch 1, not before

1. **`inline` and demand-driven resolve.** The named prerequisite for shipping large groups. Lane
   S measures what a shipped group costs in lines; that number decides whether this is a lane or a
   wave.
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

## What every lane reports

Per `.claude/rules/generator-fixtures.md`: the finding codes raised, the fixture counts before and
after, and — this wave specifically — **whether the count movement was incidental**. A lane whose
objective was a mechanism and whose report is a finding delta has answered a question nobody
asked.
