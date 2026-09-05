# Lane AD — a nested module replaces the numeric suffix

Wave six, batch 2. Branch `worktree-agent-a7376c8f5a2728e7f`, commit `05e02c9`.

## What changed

An anonymous shape is named for the position it is reached through, and that name is now a
module path rather than a concatenation. `Widget`'s `options` member reads `Widget.Options`,
which `render-source` writes as `type Options` inside `module Widget`. Two owners with an
`options` member are two declarations; neither takes a suffix.

Five files carry it:

| File | Change |
| --- | --- |
| `src/Xantham.Generator/Model.fs` | `Naming.nestable`: the plain identifier shape, which is what opens a module. |
| `src/Xantham.Generator/Shape/Spec.fs` | `nestUnder owner segment`: dots the two names where both ends are `nestable`, concatenates otherwise. |
| `src/Xantham.Generator/Shape/Anonymous.fs` | `into` calls `nestUnder`; `claim` raises `SY.NameNestedUnderOwner` for any name carrying a dot. |
| `src/Xantham.Generator/Shape/ConstructorObjects.fs` | Same descent; `pascalSegment` is applied to the source-name branches only, since it splits on dots. |
| `src/Xantham.Generator/Render.fs` | `nestingOf`/`underLeaf`/`nestedBlocks` emit the module tree; `ownerIn` attributes a finding to the longest declared prefix of its symbol. |

`Shape/Spec.fs` took one addition (`nestUnder`, 12 lines at the top). Batch 1's `absenceOf` and
`isNullish` were not touched. `Spec.fs`'s own `$"{name}{i + 2}"` (now line 1882, `uniqueCaseNames`)
was left alone — see the residue section.

### The reference side needed nothing

`Render.printTypeIn` already routed `FsNamed`/`FsApp`/`FsBranded` through `qualified`, which
splits on dots and escapes each segment, because O7's `reference` disposition writes another
group's names dotted. A nested name reuses that path unchanged. Cross-group qualification
(`foreignTo`) also composes: an owner map entry becomes `OtherModule.Widget.Options`.

### Emission

`nestedBlocks` walks the declaration list once. A declaration with no modules left to enter is
rendered at the current indent; a module opens at the position of the first declaration that
reaches into it and absorbs every later one. Order within a level is the order
`order-declarations` fixed, so the diff against the previous corpus reads as a reordering of
whole declarations rather than a reshuffle.

`underLeaf` rewrites a declaration's `Name` to its leaf before the existing render functions see
it, so `declHead`, `declRef`, the ParamObject `Create` return type and `renderPhantom`'s case
name all resolve within the module. `[<Import>]`/`[<Global>]` attributes read their JavaScript
name off `FsBinding`, not off `decl.Name`, so nothing needed to change there.

### Findings

`ownerIn` replaces the old `ownerOf` inside `symbolTiers` only; `ownerOf` survives as its
fallback. Without it every finding under `Widget.Options.retry` would have been cut at the first
dot and folded into the `Widget` row, and `Widget.Options` would have shown as `exact` with no
findings at all.

## F# legality

Lane Z's probe table is confirmed by the corpus. Beyond it, this lane probed:

| Case | Result |
| --- | --- |
| `type X` and `module X` in one file, no `CompilationRepresentation` | compiles (F# 4.1 auto-suffix) |
| Two levels (`Widget.Options.Retry`), cross-references both directions, under `module rec` | compiles |
| `type Handle<'T> = Func<'T> * Handle.Item<'T>` — an abbreviation naming its own companion module | compiles (`generics-lab`) |
| StringEnum inside two nested modules | compiles, and erases correctly (run gate) |
| `` module ``type`` ``, `` module ``2fa`` ``, `` module ``Has Space`` `` | compile |
| `` module ``Dollar$`` ``, `` module ``@cf/meta`` `` | `FS0883` |
| `` type ``Dollar$`` ``, `` type ``Slash/Name`` `` | `FS0883` — the same rule, so there is **no** name that is a legal type and an illegal module |

The last row corrected the guard's original rationale. `Naming.nestable` is deliberately
*stricter* than F# requires (it refuses `Has Space` and `2fa`, which F# would accept as module
names): where it refuses, the name concatenates exactly as it did before this lane, so a key
outside the identifier shape sees no behaviour change at all.

### Shadowing

A nested module can shadow a top-level name: inside `module Widget`, a bare reference to a
top-level `type Options` would resolve to `Widget.Options`. Nominal typing turns that into a
compile error rather than a silent wrong binding, and the compile gate is green across 2,807
declarations, so it does not occur in the corpus today. It is not *prevented* — a future corpus
could hit it, and the symptom would be an `FS0001`/`FS0039` in a nested declaration. The fix, if
it ever fires, is to qualify the reference with the file's module name; the hazard set is
computable from the declaration names alone (a reference whose head segment names a child of an
enclosing nested scope).

## Emitted names, before and after

```
- type BasicImageTransformationsGravityCoordinatesMode =        + module BasicImageTransformationsGravityCoordinates =
- type ReadableStreamDefaultReaderReadResultItem<'R> =          +     module Read =
- type ReadableStreamDefaultReaderReadResultItem2 =             +         module Result =
                                                                +             type Item<'R> = ...
                                                                +             type Item2 = ...
- type LabelTarget =                                            + module Label = / type Target =
- type EachProps<'T, 'U> =                                      + module Each = / type Props<'T, 'U> =
- type SlimBase =                                               + module Slim = / type Base =
- type DraftPanel =                                             + module Draft = / type Panel =
- type MakeOptions =                                            + Make.Options
- NodeExtensionsToVarResult (SY001 symbol)                      + NodeExtensions.ToVar.Result
```

640 module headers are new across the corpus.

## Residue — the numeric suffix that stays

Measured over every `symbols.jsonl` in the corpus (2,810 declarations before, 2,807 after), a
name counts as residue when it is `Stem` + a number ≥ 2 **and** `Stem` is also a declared symbol.

| | Before | After |
| --- | --- | --- |
| Numeric-suffix declarations | **129** | **124** |
| …of which dotted (nested, still colliding) | 0 | 48 |
| …of which flat (no owner to nest under) | 129 | 76 |

All 129 before-names vanished as spellings. 53 were path-derived and became nested; 48 of those
53 still collide at their new path, so the gross fall is 129 → 124. The composition is the real
result, and it splits three ways:

1. **Several shapes at one position — 48, dotted.** Two causes, both genuine:
   - *Overloads.* `WorkflowStep.do` has four overloads, each with a `rollbackOptions` parameter
     of a different type, so all four reach `WorkflowStep.Do.RollbackOptions`. Same for
     `CreateResource.Options2..4`, `CreateResource.Fetcher.Info2..4`, `Ai.Run.Options2..3`,
     `Show.Props2`, `Match.Props2`.
   - *Union arms.* `R2Object.range` is a union of three object shapes, and `walk` gives a union's
     members their owner's path. `R2Object.Range2`, `R2Object.Range3`, and `Choice.Either2` in
     the lab.

   Neither has a position to separate it by. TypeScript itself distinguishes overloads only by
   order, so any scheme here reintroduces a number under a different spelling. Recorded as
   deliberate.

2. **Two exports of one name — 76, flat.** `name-exports` names two declarations that own each
   other nothing, and the brief directed keeping the suffix. Broken down by where the source
   declares them (brace-tracked over `@cloudflare/workers-types/index.d.ts`):

   | | Count |
   | --- | --- |
   | Stem declared at global scope **and** inside a `declare namespace` | **6** |
   | Stem declared only at global scope (a plain redeclaration) | 66 |
   | Stem from another package in the corpus | 4 |

   The six are `DurableObject2`, `WorkflowDurationLabel2`, `WorkflowSleepDuration2` (all
   `CloudflareWorkersModule`) and `ScriptVersion2`, `TailEvent2`, `TracePreviewInfo2` (all
   `TailStream`). **That is the answer to the lane's open question**: nesting a namespaced
   declaration under a module named for its namespace would resolve 6 of 124, and would rename
   `DurableObject2` to `CloudflareWorkersModule.DurableObject`. No change made — the number is
   the deliverable.

3. **DU case names — untouched.** `Spec.uniqueCaseNames` is the fourth site the plan listed. Its
   names are the cases of one discriminated union, which are already nested inside the union
   type; there is no further owner. Left as a numeric suffix.

`ConstructorObjects.claim` keeps its suffix for the same reason: it names `{stem}Constructor`,
and two constructor objects with one stem have no position between them.

## Findings

`SY.NameNestedUnderOwner` (`SY004`, `[<Ergonomic>]`, payload `nestedAs`) fires once per name
carrying a dot. Corpus total **701**:

| Fixture | SY004 |
| --- | --- |
| `@cloudflare/workers-types` | 550 |
| `solid-js` | 54 |
| `type-fest` | 50 |
| `animejs` | 28 |
| `nested-name-lab` | 9 |
| `generics-lab` | 3 |
| seven other labs, one each | 7 |

`@cloudflare/workers-types` alone accounts for 550 of the 701; its tier counts moved from
exact 137 / ergonomic 1171 / widened 381 / escape 110.

No other finding code was added, removed or reordered. `Findings.fs` carries only the manager's
pre-declared case; its tier attribute and its `Findings.test.fs` snapshot line are unchanged.

### Tier counts, whole corpus

| | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| Before | 472 | 1469 | 772 | 191 |
| After | **333** | **1611** | 771 | 191 |

139 symbols moved exact → ergonomic, all of them from `SY004`. One symbol moved out of
`widened`, from `ownerIn` attributing a finding to a nested declaration that previously rolled
up to its owner.

**The tier is worth a second opinion.** `Ergonomic` is defined as "meaning preserved, spelling
made idiomatic", which describes nesting exactly, and the manager pre-declared it. Against it:
`SY.InstantiationNamedOnce` and `DT.TaggedUnion` are `[<Exact>]` and are also pure reports of a
naming decision with nothing lost, and a nested name accepts and rejects exactly what the source
did. Flipping to `[<Exact>]` is one attribute plus one snapshot line, and would return the 139
symbols to `exact`. Left as dispatched.

## Golden churn, in aggregate

40 files, +16,719 / −16,038 (net +681 lines, which is the module headers and the indentation).
Reviewed as counts plus bounded samples per `.claude/rules/generator-fixtures.md`; no golden was
paged through.

| Fixture | .fs lines changed |
| --- | --- |
| `@cloudflare/workers-types` | 24,943 |
| `type-fest` | 2,942 |
| `animejs` | 2,310 |
| `solid-js` | 871 |
| labs | 11–43 each |

Every changed hunk sampled was one of two forms: a flat `type FooBarBaz =` becoming
`module Foo = / module Bar = / type Baz =`, or a reference `FooBarBaz` becoming `Foo.Bar.Baz`.

## Keyword and identifier escaping

Two rules, and they are separate:

- **`Render.ident`** — backticks a name that is an F# keyword or outside the identifier shape.
  Unchanged, and it now runs on module names too (`nestedBlocks` writes `module {ident head} =`).
  A module segment is never a keyword in practice: every segment comes through
  `Naming.pascalSegment`, which capitalises, and every F# keyword is lower-case.
- **`Naming.nestable`** — decides whether a segment opens a module at all. Strictly the
  identifier shape, so `beta channel` and `@cf/meta` concatenate into their owner's name and
  the result is one backticked type, spelled exactly as it was before this lane.

`Render.qualified` splits a name on dots and escapes each segment separately, which is what
makes `` `RegistryBeta channel` `` and `Widget.Options.Retry` both come out right.

## Lab fixture and gates

`tests/fixtures/nested-name-lab/` — `index.d.ts`, `index.js`, `package.json`; golden at
`tests/Xantham.Generator.Tests/golden/nested-name-lab/`. Registered in `Pipeline.test.fs` with
seven cases, and linked into the run gate (`.fsproj` plus `nestedNames ()` in `Program.fs`).

It pins: two owners of one member name; two levels of nesting; a nested shape referring across to
another owner's nested shape; a StringEnum three modules deep; a function export nesting under a
module with no type beside it; a key outside the identifier shape concatenating; and the union-arm
residue (`Choice.Either2`).

Run gate additions prove the nesting is an F# spelling and nothing else: a `ParamObject Create`
two modules deep is still the bare object literal `{"attempts":3,"backoff":"linear"}`; a nested
StringEnum crosses the boundary as its `CompiledName` string; a JavaScript-built object reads
back through a nested declaration; and an `[<Import>]` binds under a nested parameter type.

Gates: `dotnet build Xantham.slnx` green; `dotnet fsi build.fsx -- test` green — 85 + 409 tests,
**148 run gate checks passed**.

### Consumer-visible churn inside the repository

`tests/Xantham.Generator.RunGate/Program.fs` had to change
`PhaseBLab.ConfigureSettings.Create` to `PhaseBLab.Configure.Settings.Create`. That is the
lane's whole point arriving at a consumer, and it is the shape of the break every downstream
binding consumer will see.

Six pipeline/shape tests asserted the old concatenated spellings and were updated
(`LabelTarget`, `EachProps`, `SlimBase`, `HandleItem`, `MakeOptions`, the `SY001` symbol lists).
`alias-copy-lab`'s `declarationsIn` helper read declarations off lines starting with `type ` and
so missed every nested one; it now tracks module nesting by indentation and returns qualified
names.

## Unprobed and unreachable

Lane Z named three constructs it had not probed inside a nested module. Their status:

| Construct | Status |
| --- | --- |
| StringEnum | **Probed and proved.** `Widget.Options.Retry.Backoff` in the lab compiles, and the run gate reads `"linear"` back across the boundary. |
| Tagged union | **Structurally unreachable.** `detect-tagged-unions` fires on a union that `synthesize-anonymous` named, and `needsName` names a union only when `isLiteralUnion` holds — every member a literal. An object union at an anonymous position never gets a name of its own; its *arms* are named and the union renders `U2<Panel.State, Panel.State2>`. A tagged union therefore only ever forms from an exported alias, which `name-exports` names flat. Confirmed in the lab: `state: { kind: "idle" } \| { kind: "busy"; since: number }` produced two nested interfaces and an erased union, not a `TypeScriptTaggedUnion`. F# legality of a tagged union inside a nested module is **still unprobed**, because nothing can put one there. |
| `[<Import>]`-attributed *class* | **Structurally unreachable, same shape of reason.** An entrypoint class comes from a `declare class` export and is named by `name-exports`, which produces flat names only. An anonymous position never yields a class. Also unprobed for the same reason. |

An `[<Import>]`-carrying *interface* inside a nested module is reached and proved: a constructor
object or a statics-carrying interface nested under an owner takes `bindingAttribute` exactly as
a top-level one does, and the corpus compiles.

## Handed back — one latent defect, not this lane's

A member key that is not a legal .NET type name produces an illegal *type* name today, with or
without this lane's change. The first draft of the lab used `@cf/meta` and the compile gate
rejected the golden:

```ts
export interface Registry {
    "@cf/meta": { model: string };
}
```

emits `type ``Registry@cf/meta`` =` → `FS0883: Invalid namespace, module, type or union case
name`. The concatenated name this lane produces for such a key is byte-identical to the one the
previous scheme produced, so nothing regressed; the construct is simply not exercised anywhere
in the corpus and was never gated. The compile gate's own `.fsproj` comment cites
`@cf/meta/llama-3` as a real key on a real object, so it is reachable in principle. The lab uses
`"beta channel"` instead, which is the same class of key with a legal outcome. Fixing it means
sanitising a synthesized name's characters, which changes names outside this lane's brief.
