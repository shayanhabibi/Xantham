---
category: Generator
title: Plan - Architecture
index: 1
---

# Generator architecture — a nano-pass pipeline over the Wire

Companion to `generator-type-mapping.md` (the *what*); this document plans the *how*: the
structure of the generator that turns a TypeScript package into F# bindings, built for
testability first. Like the mapping document, it is an iteration artifact — decisions land
here as they are made, and everything else is marked open.

## 1. The organizing idea: nano-passes as sequenced async operations

The nano-pass compiler tradition builds a compiler from many tiny passes, each doing one
conceptual transformation with a well-defined input and output, each testable in isolation.
Its usual cost — dozens of near-identical IR definitions and traversal boilerplate — mostly
evaporates here because of one observation: **a pass does not have to be a traversal. It is
an async operation sequenced after the previous one.**

Concretely: a pass is a named `Context -> 'Model -> Async<'Model>` step over its tier's
model type (O1). Some passes walk
declarations; some fire one batched Wire request and fold the answers into a table; some are
pure rewrites lifted into `Async`; some only *check* and emit findings. `Async` is the
uniform shape because roughly half the passes talk to the compiler through the mailbox
(`TscMailbox` batches overlapping calls into one `batchRequests` round trip, so a pass that
asks 500 small questions is one wire exchange), and uniformity is what makes the pipeline a
plain fold:

```fsharp
type PassOutcome<'Model> =
    | Advanced of 'Model
    | Degraded of 'Model * Finding list   // pass applied partially; findings say where
type Pass<'Model> = { Name: string; Run: Context -> 'Model -> Async<PassOutcome<'Model>> }

// per tier, the pipeline is a plain fold over a list; tiers join via total transitions
let shapePasses : Pass<ShapeModel> list = [ ... ]
```

What "nano" buys us, in this repo's terms:

- **Unit testability.** Each pass is exercised with a hand-built (or snapshot-loaded) model
  and asserted on its output model, without running the passes before it.
- **The fidelity report falls out.** `Finding`s (tier annotations, drops, degradations from
  `generator-type-mapping.md` §1/§5) are per-pass outputs, so "which pass widened this and
  why" is answerable by construction.
- **Decision isolation.** Each D-decision in the mapping doc becomes, where possible, *one*
  pass (D4 union classification, D3 ParamObject synthesis, D6 alias naming…), so revisiting
  a decision is editing one pass and its tests, not spelunking a monolith.
- **Determinism is enforceable per pass.** Same input model ⇒ same output model, testable
  pass-by-pass rather than only end-to-end.

What we deliberately do *not* take from the tradition: a distinct IR type per pass. Instead,
a small number of **tiers** (§3), with nano-passes being endomorphisms *within* a tier and
tier transitions being total functions — the F# compiler's exhaustiveness checking on the
transition is our no-silent-drops guarantee at the type level.

## 2. Pipeline overview

```
package entry (.d.ts, package.json, tsconfig)  +  generator config
        │
        ▼
  ┌ Session bootstrap ┐   Tsc.locate → TscChannel/TscMailbox → Session<Async>
  │                   │   createProgram over the package + virtual tsconfig
  └───────┬───────────┘
          ▼
  Tier 1: HARVEST      symbols, declarations, module tree, docs      (Wire-heavy)
          ▼
  Tier 2: RESOLVE      type table keyed by TypeResponse.Id           (Wire-heavy)
          ▼
  Tier 3: SHAPE        F#-shaped declarations (mapping doc applied)  (mostly pure)
          ▼
  Tier 4: RENDER       F# source text + fidelity manifest            (pure)
          ▼
  output directory: *.fs files, manifest.json, report
```

**Decided (O1): accumulating records.** Each tier has its own model type whose fields
*include the earlier tiers' artifacts as required fields* — `ShapeModel` carries the harvest
tables and the type table alongside the growing `FsDecl` set, with no `option` wrapping.
Within a tier, passes are uniform endomorphisms on that tier's model; between tiers, a total
transition function constructs the next record from the previous. Reading an artifact that
does not exist yet is therefore a compile error (the field is not on your tier's type), the
accumulation is by composition rather than copying, and the pass list stays homogeneous per
tier. The cost is four model types instead of one, which the four-tier structure justifies.

## 3. Tiers and their invariants

### Tier 1 — Harvest (what the author exported)

Wire-driven inventory, no mapping decisions at all.

- **Artifacts:** module tree (files → namespaces → exports), symbol table
  (`SymbolResponse` + declaration node handles), doc table (`getDocumentationComment`,
  `getJsDocTags`), source-order index (for deterministic output ordering).
- **Invariant:** every export of the entry module (transitively through re-exports, via
  `getAliasedSymbol` to origin) appears exactly once; declaration merging is already done
  because we harvest *symbols*, not declaration nodes.

- **Value exports retain their path provenance.** The flattened module export list includes
  symbols reached only through `export type *`. Harvest records `HasValueExport` from the
  compiler's direct export tables and typed import/export nodes, following only value-capable
  paths to the same declaring symbol. Type-only classes retain their instance shapes and
  `typeof` aliases retain the value-type facts, while class constructors/statics and function
  imports require a value-export path. Barrel cycles do not cache a provisional absence.

- **Ambient export-equals variable roots retain their module value.** When the checker's
  flattened module export list is empty, Harvest follows its direct `export=` assignment
  to a declaration-backed value and records a `default` import under that ambient origin.
  Namespace/class export tables retain their existing named imports. Typed declaration
  ancestry distinguishes a module-local target from a genuine global or `declare global`
  member; only wholly local targets are excluded from global exports.

- **An ambient declaration in a `.d.ts` module is exported without the keyword**, and the
  harvest is right to take it. `declare const secret: number` beside an `export declare
  function` looks local and is not: `getExportsOfModule` returns it, and `import { secret }`
  from the module type-checks with no diagnostic. Verified against stock TypeScript 5.9.3 as
  well as the 7.x dev compiler the wire drives, because it reads like over-collection and the
  "fix" - filtering the harvest to declarations carrying an `export` modifier - would quietly
  delete real bindings. Written down here so it is not fixed twice.

### Tier 2 — Resolve (what the checker says everything is)

- **Artifacts:** type table `Map<TypeId, TypeFacts>` where `TypeFacts` wraps `TypeResponse`
  plus the derived facts passes repeatedly need (members via `getPropertiesOfType`,
  signatures, index infos, base types, constraints, alias identity). Built breadth-first
  from the harvested symbols' types, batched per generation of the frontier, memoized on
  `Id` — this is also the cycle-detection boundary.
- **Invariant:** the type table is closed — every `TypeId` referenced by a `TypeFacts` is
  itself in the table (or recorded as deliberately-not-followed, e.g. beyond the
  utility-type depth cutoff, D9).

### Tier 3 — Shape (the mapping document, executed)

The nano-pass heart. Pure except where a pass asks the checker a clarifying question
(e.g. `isTypeAssignableTo` during tagged-union detection). Each pass reads the type table
and rewrites the growing set of `FsDecl`s / `FsTypeRef`s. Draft pass catalogue, roughly in
order — each row cites its mapping-doc section:

| Pass | Does | Ref |
|---|---|---|
| `hoist-nullability` | strip `null`/`undefined` union members → `option` flag (per-symbol overrides applied) | §4.3, D1 |
| `classify-literal-unions` | StringEnum / enum / CompiledValue mixed cases | §4.2, D12 |
| `detect-tagged-unions` | common literal tag ⇒ `TypeScriptTaggedUnion` shape | §4.5, D4 |
| `detect-erased-object-unions` | callable/constructable exclusive arms ⇒ typed erased cases with defined-value predicates; shared calls require equivalent source signatures | §4.5, October 23 D2 |
| `classify-unions-by-position` | input: overloads/erased ctors; output: tagged/test-helpers; `U_n ≤ 4` | §4.5, D4 |
| `detect-brands` | primitive ∩ phantom-object ⇒ measure-tagged primitive | §4.6 |
| `flatten-intersections` | apparent-members flattening + interface inheritance where nominal | §4.6 |
| `resolve-enums` | `getConstantValue`, const-enum inlining | §4.7 |
| `shape-callbacks` | delegates by default | §4.8, D5 |
| `expand-literal-overloads` | `on("click", …)` ⇒ `onClick` under `[<Emit>]`; landed inside `shape-interfaces`, unconditional (W5) | §4.8 |
| `shape-tuples` | fixed/option-tail/rest-carrier | §4.12, D7 |
| `split-keyof-regimes` | closed ⇒ literal union; open ⇒ support-package idioms | §4.10 |
| `name-expanded-aliases` | `Partial<Options>` ⇒ `OptionsPartial` | §4.10, D6 |
| `hash-cons-anonymous` | synthetic interfaces for inline literals, by `TypeId` | §4.4 |
| `synthesize-paramobjects` | ParamObject ctors for plain-data interfaces; parameter-position literals flattened | §4.4, D3 |
| `default-type-args` | same-name-lower-arity abbreviations | §4.9 |
| `shape-classes` | instance interface + statics/`Exports` split | §4.4 |
| `assign-names` | NamePath synthesis, collision resolution, keyword escaping, CompiledName | §4.14 |
| `order-declarations` | topological sort, `and`-groups for cycles, module layout | — |
| `drop-orphan-delegates` | a delegate whose name no declaration reads | §4.8, D5 |
| `audit-coverage` | every harvested export reached a shaped decl or an explicit Finding | §5 |

Passes are *conceptually* categorized this way even when, mechanically, two adjacent pure
rewrites could fuse — do not fuse them; the seam is the test point.

- **Invariant (tier exit):** the shaped model contains only `FsDecl` constructs the renderer
  has cases for, and `audit-coverage` has accounted for every export.
  A namespace is represented through its harvested descendants, including declarations under
  `Globals` or a subpath module. If all harvested children are absent, the namespace and its
  missing children each receive `AC001`; an empty type-only namespace remains excused.
  `synthesize-anonymous` records position-derived names in `ShapeModel.MintedNames` at their
  claim sites. Orphan pruning uses that provenance for unused declarations under folded-arm
  roots; source-written namespace aliases and export roots remain outside that set. Alias
  identity alone does not establish whether a declaration was generated from a position.
  Reference-only type facts retain the declaration's namespace ancestry, so library and
  dependency references use the producer's nested spelling. When an object is named through
  its alias, the reference carries every alias application argument, including concrete types;
  the parameter-only list used to bind declaration variables is insufficient for a reference.

### Tier 4 — Render

- F# declaration IR → source text. **Decided (O2): direct text emission** — a small
  indent-aware printer owned by the generator, no Fantomas/Fabulous.AST dependency. The
  decisive argument is golden-file stability: golden diffs are this plan's review surface,
  and a formatter dependency makes them churn on formatter upgrades for reasons unrelated
  to the generator. Output is almost entirely declarations (interfaces, DUs, abbreviations,
  `jsNative` stubs, tiny inline bodies), which is the easy 95% of F# printing; the compile
  gate (§5) absorbs the correctness risk on every fixture.
- Also renders `manifest.json` — the fidelity report: per-symbol tier, declaring file, findings
  with their catalogue key (`SI001`), pass, qualified symbol and message — the run report
  (counts of Exact/Ergonomic/Widened/Escape) and a per-pass tally of findings by tier.
- **Invariant:** byte-identical output for identical input (model ordering already fixed by
  `order-declarations`; renderer adds nothing nondeterministic).

## 4. Concurrency and batching rules

- Within a pass, fan out per-symbol/per-type wire queries freely — the mailbox coalesces
  them — but **fold results in a sorted order, never arrival order**. Determinism outranks
  latency.
- Between passes, strictly sequential. **Decided (O3): linear lists, no pass DAG.** The
  per-tier source order *is* the documentation of what runs when; tier boundaries are the
  only hard ordering constraints and O1's accumulating records make those compile-checked.
  The parallelism a DAG would buy is weak here — latency lives in Wire round trips, which
  the mailbox already batches *within* a pass — and per-pass unit tests construct their
  input models directly, so nothing needs a scheduler to run subsets. If a within-tier
  ordering bug ever bites, the patch is a debug-build well-formedness assertion between
  passes, not a scheduler.
- The Wire session (snapshot, project, program) lives in `Context`, created once per run.
  Passes never create programs; a pass that needs a throwaway program (e.g. future
  verify-by-compile checks) gets a factory in `Context` so tests can stub it.

## 5. Testing strategy

Layered, mirroring the tiers:

1. **Per-pass unit tests** (the nano-pass payoff): construct a small model — builders will
   matter here; invest in a terse model-construction DSL early — run one pass, assert the
   output model and findings. No Wire, no fixtures, milliseconds.
2. **Tier tests against live tsc:** harvest/resolve over the small fixtures (`ansi-regex`
   is one declaration; `animejs` mid-size), asserting a snapshot. This repo's culture is
   live-compiler tests (`XANTHAM_REQUIRE_TSC=1` makes skips loud), and the Wire is fast
   enough; no mock server. **Decided (O5): snapshots are golden-prints** — a purpose-built
   human-readable textual projection of the model (exports outline, resolved types and
   relationships, shaped declarations with fidelity tiers), not serialized model JSON. The
   printer shows what is behaviorally meaningful, so internal model refactors don't rewrite
   the corpus, and the diff a pass change produces is legible. It pays rent twice more: as
   the between-pass debugging dump, and as documentation of what each tier promises.
   Golden-prints are not machine-round-trippable, and nothing needs them to be — per-pass
   tests build models through the construction DSL; a test that someday wants a big
   recorded model gets one bespoke JSON dump, not a corpus format.
3. **End-to-end golden files:** fixture package → generated `.fs` + manifest, committed and
   diffed. A golden diff is the review surface for any pass change. An npm rung's install is
   untracked, so `tests/fixtures/pins.json` records the version each golden was generated
   against and the suite reports a mismatch as fixture drift *instead of* the golden diff:
   a package that moved and a pass that regressed otherwise produce the same diff. Rungs
   that exercise a feature rather than a package are hand-authored and tracked whole
   (`tests/fixtures/lab`, `globals-lab`, `keyof-lab`, `brand-lab`, `lib-lab`), so they need no
   install and no pin. Everything else is installed on demand — `build.fsx -- test` runs the
   fixtures stage, in an agent worktree as much as in the main checkout.
4. **Compile gate:** generated output for the golden fixtures is compiled (F# type-check;
   Fable compile once the support package exists) in the test suite. Bindings that do not
   compile are not bindings. It is an ordinary project rather than a test
   (`tests/Xantham.Generator.CompileGate`), so it runs on every build; it carries the same
   `Fable.Core` pin as `src/Xantham.Fable.Core` and references the whole `Fable.Browser.*`
   family, which is what the goldens may cite. The Fable *run* gate
   (`tests/Xantham.Generator.RunGate`, landed 2026-09-02) is the behavioural half: Fable
   compiles the linked goldens and a program of checks, and node runs it against hand-written
   JavaScript runtimes of the same fixtures (`tests/fixtures/<lab>/index.js`), so `[<Global>]`,
   `[<EmitConstructor>]`, `TypeScriptTaggedUnion`, `ParamObject` and `[<Import>]` are proven
   by what the erasure did rather than by what type-checked. It runs from `build.fsx -- test`
   after the suites; it grows by linking another golden and adding checks.
5. **Pipeline properties:** run-twice determinism; `audit-coverage` findings empty for
   fixtures we declare fully-supported; manifest tier counts monotonic (a PR that turns
   Exacts into Escapes must say so).
6. **Litmus ladder (D9 calibration):** `ansi-regex` → `animejs` → `@cloudflare/workers-types`
   (already the dogfood in `tests/Test.fsx`) → `solid-js` → `type-fest` → `@types/three` →
   `typescript`. Each rung gains a CI-checked "no silent drops" claim before the next is
   attempted; `type-fest` is expected to *stay* partially Widened — it exists to tune the
   cutoff, not to be conquered.

## 6. Project layout and phasing

New projects (names step around the archive, which is invisible to the solution anyway):

- `src/Xantham.Generator` — the pipeline, passes, model, renderer. .NET, references Wire.
- `src/Xantham.Cli` — the `xantham` command. A shell over `Pipeline.run` and `Schema.json`,
  packed as a .NET tool.
- `src/Xantham.Fable.Core` — revived support package (erased `keyof`/`typekeyof`/
  `PropertyRecord`, brand helpers). Fable library, no dependency on the generator.
- `tests/Xantham.Generator.Tests` — Expecto, same conventions as the Wire suite.
- **Decided (O4):** configuration is a JSON file next to the target `package.json`
  (`xantham.json`), deserialized with `System.Text.Json` (JSONC-tolerant via
  `ReadCommentHandling.Skip` — comments matter in per-symbol override lists), validated by
  a **JSON Schema generated from the config record itself** so the file self-documents and
  editors check it. The generator core is a library function taking the config record; the
  CLI (`xantham generate <package-dir> [-o <out>] [--config <path>]`) and the scratch
  harness are both thin shells over it. Keys so far:
  `module`, `groups` (O7), `lib` - the compiler's option as `tsconfig.json` spells it,
  for a global type library that replaces the DOM rather than extending it (phase E) - and
  `runtime`, the npm package the generated `[<Import(…)>]` attributes name, which defaults to
  the package name with DefinitelyTyped's `@types/` naming undone (phase E, wave two lane D;
  `generator-three-rung.md` §8).

  *Status (2026-09-03, wave five batch 2):* both halves are built. `xantham generate
  <package-dir> [-o <dir>] [--config <path>] [--quiet]` writes the binding, the shipped
  groups and `manifest.json`, listing the written paths on standard output and the tier and
  finding-key counts on standard error; it exits 0 generated, 1 usage, 2 no package, 3
  configuration refused, 4 generation failed. `xantham.schema.json` at the repository root is
  emitted by `xantham schema` from `GeneratorConfig`, `GroupDisposition` and `MappedName` by
  reflection — the property set, their JSON types and the disposition vocabulary come from the
  F# declarations, the key spellings and descriptions from a table in `src/Xantham.Cli/Schema.fs`
  that emission refuses to skip. `dotnet fsi build.fsx -- generate --only schema` regenerates
  it, and `Cli.test.fs` fails when the committed text and the record disagree.

  *Extended (2026-09-06):* `entry` selects one existing TypeScript file relative to the
  package directory, independently of the `runtime` import and F# `module` name. The default
  lookup checks root declarations; a package exposing only named subpaths requires an explicit
  input. Conditional environment selection remains the caller's responsibility. Bootstrap,
  CLI, schema and `entry-selection-lab` tests cover selection and refusal paths, including
  `.d.mts` and `.d.cts`. See [entry selection](../../../site/content/xantham-cli/guide/usage.md).

  Two things `load` still leaves to a caller. It is addressed by *directory*, so `--config`
  pointing at a file under another name reads it through a staged copy; a `loadFile` taking
  the path would remove that. And it signals every refusal with `failwith`, so the CLI
  separates "configuration refused" from "generation failed" by catching around the call
  rather than by the exception's type.

Phases — each ends with the compile gate green on its fixtures:

- **A — walking skeleton.** Bootstrap, harvest, resolve, minimal shape (interfaces,
  functions, primitives, `option`), render, manifest. End-to-end on `ansi-regex`.
  *Everything after this is adding passes to a working pipeline.*
  **Landed (2026-09-01):** `src/Xantham.Generator` + `tests/Xantham.Generator.Tests`
  (per-pass units, live e2e goldens, run-twice determinism) +
  `tests/Xantham.Generator.CompileGate` (goldens compiled against Fable.Core on every
  build). Two things the fixture taught that the plan should carry forward: expanding
  one external type (`RegExp`) transitively reaches most of `lib.d.ts` — which forced
  the package-boundary grouping now recorded as O7 (external groups resolve to identity
  only unless their disposition says otherwise) — and the wire flags neither optional
  parameters nor declared `readonly` on symbols (`?` optionality is derived from the
  hoisted `undefined`; readonly comes from `isReadonlySymbol`).
- **B — the common 90%.** Literal unions (D12), enums, ParamObject synthesis (D3),
  callbacks (D5), classes/statics, naming pass hardening. Fixture: `animejs`.
  **Landed (2026-09-01):** the shape tier grew to eleven passes (`name-exports`,
  `synthesize-anonymous`, `classify-literal-unions`, `shape-callbacks`,
  `shape-interfaces`, `shape-aliases`, `shape-classes`, `shape-exports`,
  `synthesize-paramobjects`, `order-declarations`, `audit-coverage`); `DeclNames` is
  keyed by *type id* so exported and synthesized declarations share one naming table.
  Empty shipped interfaces retain the `obj` ABI and `TR047` after alias shaping suppresses
  their own name. Ownership uses an exact checker type id in the selected harvested type
  exports, with both declaration groups shipped and the type symbol marked as an interface.
  Qualified and renamed occurrences therefore retain the empty-object loss rather than
  `TR023`; an unselected type, a widened or referenced group, and a private nominal instance
  keep their existing paths. `census-empty-owned-interface-lab` pins the diagnostic correction.
  A hand-authored tracked fixture (`tests/fixtures/lab`) pins each feature under the
  live compiler alongside the `animejs` rung. Decisions the fixtures forced, recorded
  here rather than re-derived later:
  - *Arrays* are `'T[]` (undecided in the mapping doc until now); the element resolves
    for every group disposition because `Array<T>`'s type arguments are followed even
    in identity-only groups.
  - *Tuples* read as arrays until D7's pass (phase C) - without this, a tuple alias
    flattens into an interface carrying the whole `Array.prototype`.
  - *Symbol-keyed members* (`__@iterator@1469`) are dropped with a finding: they are
    unrepresentable, and the checker id embedded in the name breaks run-to-run
    determinism (the first determinism failure the e2e property caught). *Superseded for
    well-known symbols by "Well-known symbol members (2026-10-13, W4 #135)".*
  - *`true | false` inside bigger unions* reads back as `bool` - the checker re-expands
    `boolean` in unions, so `boolean | undefined` is a two-literal union at the wire.
  - *Member-position unions resolve to declared aliases by member set*: literal types
    are interned, so `"ms" | "s" | undefined` at a member matches the exported
    `TimeUnit` union by id set instead of synthesizing a twin.
  - *Twin declared unions abbreviate toward the smallest type id only*: when two
    exported unions share a member set (`animejs`'s `TimelinePosition` /
    `ScrollThresholdValue`), matching each to the other produces `type A = B` /
    `type B = A` - fsc reports a plain two-type cycle as FS0953, but a *generic
    instantiation* over the cycle (`Func<obj[], TimelinePosition>`) never terminates.
    `shape-aliases` therefore hides every same-set twin with a larger id before
    resolving an alias's right side: chains strictly decrease, the smallest twin
    widens structurally, and cycles are impossible by construction.
  - *Class statics*: constructors become `[<EmitConstructor>]` members on `Exports`;
    the constructor-object type itself is never declared. Static members beyond the
    constructor are findings until a fixture needs them (`animejs` has none).
  - *ParamObject Create* is capped (24 parameters): wider is unusable and measurably
    quadratic for the F# typechecker - the uncapped `animejs` golden took the compile
    gate from seconds to many minutes. Create statics also moved the gate's TFM to
    net8.0 (static interface members need default-interface-member runtime support;
    Fable erases them).
- **C — unions and generics.** Position-aware unions (D4), tagged-union detection, tuples
  (D7), generics/constraints/default-args. Fixture: `@cloudflare/workers-types`.
  **Landed (2026-09-01):** tuples, erased and tagged unions, and declaration-level
  generics on the `lab` and `animejs` rungs, then the `@cloudflare/workers-types` rung
  itself - 1387 declarations, compile gate green. What the fixtures settled:
  - *Tuple element flags live on the type's target*, not on the reference the checker
    hands back (the reference reports `elementFlags: null`). The target is read for its
    flags and then dropped rather than followed - it is the generic tuple type, so
    deriving it drags all of `Array.prototype` in again per distinct tuple shape.
  - *Optional tuple components need no work of their own*: `[number, number?]` arrives
    as `number` and `number | undefined`, so D1's nullability hoist produces
    `float * float option` for free.
  - *Rest and variadic tuples widen to an array* with a finding. §4.12's erased carrier
    is deferred until a fixture asks for it.
  - *Erased unions* are Fable's `U2`-`U4`; four is the cap, because past that the
    consumer is doing runtime tests the type no longer helps them write. Arms are
    deduplicated *after* mapping (`boolean` re-expands inside unions, several string
    literals all widen to `string`), a union collapsing to one arm *is* that arm, and
    any `obj` arm collapses the whole union to `obj`. `U_n` already satisfies D4's
    position preference - `U2.Case1 x` constructs on input, and the DU matches on
    output - so only overload expansion at input positions is still deferred. Designed in
    `2026-09-22-union-arm-overloads.md` as the `expand-union-arms` pass, running after
    `dedupe-overloads` and shipping disabled.
  - *Fable's tagged-union erasure carries the arm's own properties as named DU fields*,
    not the arm type as a single payload. Verified against Fable 5.13 rather than
    recalled: `Circle(radius = 2.0)` emits `{ kind: "circle", radius: 2 }`, `None` in an
    optional field omits the key exactly as TypeScript optional properties do, and
    backtick escaping is transparent (`` ``type`` `` reaches JS as `type`). The
    single-payload form emits `{ kind: "circle", Item: x }`, which no TypeScript
    signature accepts. The discriminant is written by Fable from the case's
    `CompiledName`, so it must *not* also be a field. Cases are capped at twelve fields:
    a DU binds fields positionally, and past a dozen every `match` is a wall of
    wildcards that the erased union over arm interfaces reads better than.
  - *A type parameter's name costs a round trip*: `TypeFacts.SymbolName` is `None` for
    type parameters, but `getSymbolOfType` returns the `T`. Constraints and defaults come
    from `getConstraintOfTypeParameter` / `getDefaultFromTypeParameter`.
  - *In-scope type variables live on the shape model* (`ShapeModel.TypeVars`), not in
    `typeRef`'s arguments: scope is a property of *where* a reference is written, so the
    pass binds it once around a declaration and every nested `typeRef` inherits it. A
    parameter of some other declaration has no name to write here and widens to `obj`.
  - *A generic alias hangs its parameters off the alias, not the type.* The function type
    behind `type Mapper<T> = (t: T) => T` reports no parameters of its own; they are only
    reachable through `getAliasTypeArgumentsOfType`, filtered to the arguments that
    actually are type parameters (an instantiated alias reports concrete ones).
    `getLocalTypeParametersOfType` is not an alternative - it panics the checker on
    anonymous types.
  - *A generic declaration named at a reference position re-applies its parameters*: F#
    has no bare `Box`, so the self-reference in `map(next: T): Box<T>` and the `Create`
    return type both carry `<'T>`.
  - *Instantiations are never named or re-declared.* The checker substitutes members
    eagerly, so `Box<string>` would read perfectly well as a structure of its own;
    naming it would declare the expansion a second time under a made-up name and lose
    the tie to the generic. `synthesize-anonymous` skips them and `shape-interfaces`
    leaves them to `shape-aliases`, which writes `type StringBox = Box<string>`.
  - *A constraint survives only if it maps to a named type.* `extends string` and
    `extends keyof T` are dropped with a finding rather than approximated: F# has no
    form for them and the nearest one would reject code TypeScript accepts.
  - *Rank-2 function types hoist onto the alias* with a finding - F# has no rank-2 form.
  - *A package can declare no module at all.* `@cloudflare/workers-types` is a global
    type library: `getSymbolOfSourceFile` returns nothing, and every name lives in the
    global scope. `harvest-globals` reads them from `getSymbolsInScope` at position 0 of
    the entry file and filters the three thousand names that come back to the ones the
    package itself declares, by the same O7 placement the resolve tier groups types with.
    It runs only when `harvest-exports` found nothing - a package that has a module may
    *also* augment global scope, and folding those in would emit names it does not
    export. A value harvested this way binds with `[<Global(name)>]` rather than
    `[<Import>]`: it is already on `globalThis`. **Unverified:** only the F# compile is
    gated; that `[<Global>]` emits the right JavaScript is a Fable-run claim, and the
    Fable gate arrives with the support package in phase D.
  - *An ambient module declaration is dropped with an escape.* `declare module
    "cloudflare:email"` is a global-scope symbol whose name *is* its quoted specifier;
    `` ``"cloudflare:email"`` `` is FS0883, not a type name. Its members are importable
    from that specifier, which needs a nested module with imports of its own - until
    that exists, dropping it loudly beats emitting a name F# cannot write.
  - *Three repairs have to run after every shaping pass*, because they fix what the others
    produce (`repair-arity`, between `order-declarations` and `audit-coverage`): a
    generic abbreviation whose target widened away its parameters is FS0035, so the
    declaration goes and its references widen; a generic declaration named bare at a
    reference position is FS0033, so that position widens - §4.9's rule for an
    out-of-scope type *variable*, one level up at the declaration head. A settable
    property of type `unit` is FS0252 and is demoted to read-only in the same pass: a
    `never`-typed brand holds no value, so it also stops being a `Create` parameter.
    `drop-orphan-delegates` runs last of the three, after `resolve-export-collisions`:
    `synthesize-anonymous` mints a delegate name while the positions that read the callback
    are still being decided, and a position settled later - a lifecycle hook rendered as a
    handler interface - writes the parameters out in full instead. Running once every
    declaration is built replaces that prediction with the finished declaration set, so the
    delegate goes under `DD001` rather than reaching the golden unread.
  - *Parallel fan-out is not free of observable order.* Asking for a declared type is
    what *creates* it in the checker, and a type alias stamps its name on what it
    creates, so `type A = X & Y; type B = X & Y` race: whichever is asked for first owns
    the intersection and the other aliases it or widens. Under `Async.Parallel` that
    order came from the thread pool and the same package generated two different files
    (the second determinism failure the e2e property caught, and the reason the seed
    resolution in `resolve-export-types` is sequential - it costs nothing measurable).
  - *The checker hands back escaped symbol names.* A member whose name begins with two
    underscores arrives with a third prepended, so that a real `__html` cannot collide
    with the internal names the checker invents (`__type`, `__call`). Emitting the
    escaped form names a key the object does not carry, so `Naming.memberName` undoes it
    - after the internal-name test, since the escaping is the only thing telling the two
    apart.
  - *A global library redeclares DOM names.* `Response`, `Request` and friends are
    declared by `lib.dom.d.ts` too, so O7 places them in the compiler-lib group and the
    default `Widen` disposition takes them to `obj`; a `Reference` disposition templates
    them as `TypeScript.Lib.Response` instead. This is the grouping working as designed,
    but it is why six workers-types aliases (`PagesFunction`, `ExportedHandlerFetch-
    Handler`, ...) lose their whole shape - the phase D work on group dispositions is
    what improves them, not more shaping.
  - Still deferred, and findings say so at every site: instantiations of generic
    *aliases* written as applications (they re-expand inline today; instantiations of
    generic *interfaces* have been applications since phase E's first step), and default
    type arguments, whose §4.9 wording needs revisiting
    because F# cannot overload a type name by arity.
- **D — the erased-idiom zone.** Revive `Xantham.Fable.Core`; keyof regimes, mapped/
  conditional handling, alias naming (D6), brand detection. Fixtures: `solid-js`, `type-fest`.
  Two items phase C deferred here rather than to E: the group dispositions that would give
  the six widened `workers-types` aliases their shape back, and the Fable *run* gate that
  turns `[<Global>]` and the tagged-union erasure from compile-checked claims into
  behavioural ones.
  **Support package landed (2026-09-01):** `src/Xantham.Fable.Core` is revived from the
  archive near-verbatim per the mapping document's §7, and the compile gate now references
  it so the goldens and the idioms they may cite are proven to compile together. What the
  revival settled:
  - *The archive source needed one edit.* It was written against `Fable.Core` 5.0.0-beta.4
    and type-checks unchanged against **5.2.0**, which the support package and the compile gate
    are both pinned to - the gate moved up from 4.5.0 to meet it. One Fable.Core across the
    gate and generated output is the invariant; a version seam between them is not allowed,
    which is why the pin is stated in both projects rather than floated. The one edit came
    from the run gate: `KeyOf.access` and `TypeKeyOf.access` were `let access = item`, a value
    binding over an `inline` function, which fsc accepts and Fable refuses ("is not supported")
    - the compile gate could never have seen it. They are inline functions now.
  - *The package multi-targets `netstandard2.1;net8.0`.* netstandard2.1 is the Fable library
    convention; net8.0 exists only so the gate — held at net8.0 by phase B's `Create` static
    interface members — can reference the package without a downgrade.
  - *Brands render to units of measure* (`src/Xantham.Fable.Core/Brand.fs`, new work - the
    archive had no brand helpers, D11 having dropped them until the generator existed). A
    measure is precisely a compile-time-only nominal distinction over a shared runtime
    representation, which is what a TypeScript intersection brand is, and Fable erases it to
    nothing. Numeric brands need no support at all - `float<Millis>` is an ordinary measure
    application. Non-numeric primitives go through `[<MeasureAnnotatedAbbreviation>]`, the
    mechanism FSharp.UMX is built on, which carries a measure on `string`/`bool`/`char`.
    Verified rather than assumed: the brand is enforced in *both* directions (a
    `string<UserId>` is not a `string<OrderId>`, and a raw `string` is neither), and the
    abbreviation does **not** shadow the primitive - an application with no measure argument
    still resolves to `string`. That last property is what makes it safe to put the
    abbreviation in scope over generated code, and it is gated rather than assumed:
    `tests/Xantham.Generator.CompileGate/BrandIdioms.fs` compiles plain and branded
    primitives side by side under `open Xantham.Fable.Core`.
  **Rungs landed since (each verified end to end and committed on its own):**
  - *Keyof regimes (2026-09-01).* The shape tier emits `keyof<'T>` and `typekeyof<'T,'U>`,
    so generated output references the support package for the first time.
  - *Phantoms (2026-09-02).* A declaration whose right-hand side is a type-level computation
    keeps its name and arity as `[<Erase>] type X<'T> = private X__ of obj`, rather than
    vanishing into an escape. `keyof-lab` went from two escapes to none and `workers-types`
    from 41 to 38, recovering `DurableObjectClass<'T>`, `Fetcher<'T,'Reserved>`,
    `D1Result<'T>` and friends.
  - *Brands (2026-09-02).* Detection in the checker's output, structural rather than by
    name, so a branding intersection emits `[<Measure>] type UserId` and its uses read
    `string<UserId>`. Generated files now `open Xantham.Fable.Core`; `tests/fixtures/brand-
    lab` pins the idiom and its negatives under the live compiler. D11 closes.
  - *The compiler-lib disposition (2026-09-02).* O7's compiler-lib group widened to `obj`
    for want of a shipped binding; for the ECMAScript half of `lib.d.ts` that binding is
    `Fable.Core.JS`, which every generated file already opens. `Naming.LibBindings` is the
    pinned table, keyed by name and carrying each binding's arity, because TypeScript's lib
    drifts (it made `Uint8Array` generic in a parameter Fable's abbreviation lacks) and a
    mapping that guessed would emit code that does not compile. On `workers-types` this took
    widened findings from 528 to 451 and removed 39 "type parameter is erased" findings, a
    generic whose only use of `'T` was inside a `Promise` now carrying it.
  - *The DOM half of the compiler lib (2026-09-02).* The other half of the same group, and the
    dependency decision phase C deferred. Generated bindings now take the `Fable.Browser.*`
    family and a DOM name resolves to `Browser.Types.*` instead of widening. The family is
    close to a universal implicit dependency of Fable libraries already, which is what makes
    the added dependency cheaper than what the widening was destroying. What the work settled:
    - *The table is generated, not transcribed.* `tools/browser-gen/generate.fsx`
      (`build.fsx -- generate browser`) reflects over the `Browser.Types` namespace of all 23
      pinned `Fable.Browser.*` assemblies and intersects it with the names the pinned
      compiler's own `lib.*.d.ts` declares. 439 entries — well past what stays correct by hand
      — and it re-derives whenever either pin moves.
    - *Arity is part of the key here, rather than a property of the name.* `LibBindings` carries
      one arity per name and declines to bind below it; the DOM family binds several names at
      two arities (`CustomEvent` and `CustomEvent<'T>` are both real), so the lookup takes the
      largest arity the reference can fill. `LibBindings` stays authoritative wherever both
      tables have a name: every generated file already opens `Fable.Core.JS`, so preferring it
      keeps the shorter spelling.
    - *Ambiguity is resolved when the table is generated, not when a reference is emitted.* Two
      packages of the family each export a `Browser.Types.Range`, and F# resolves the name
      before the arity, so no qualification picks one. Such names are dropped from the table
      and widen honestly at the site. `Range` is the only one affected today.
    - *The table's own proof is a generated compile gate.*
      `tests/Xantham.Generator.CompileGate/BrowserBindings.fs` abbreviates all 439 entries
      (`type private T1_CustomEvent<'T1> = Browser.Types.CustomEvent<'T1>`), so a table
      claiming a name or an arity the pinned packages do not have fails the build rather than
      surfacing as a golden diff. Abbreviations rather than values, because they prove name
      *and* arity without instantiating a parameter that may be constrained.
    - What it bought: `animejs` went from 88 widened symbols to 83 and lost 82 widening
      findings — `HTMLElement`, `NodeList` and `SVGElement` are most of them — and with those
      `obj` arms gone, 15 unions that had collapsed to `obj` are erased unions again.
      `workers-types` lost 53, mostly `EventTarget` and `WebSocket`. What is still `obj` is now
      a statement about coverage rather than about dependencies: `fetch`'s types (`Response`,
      `Request`) live in `Fable.Fetch`, a different family, and remain widened.
  - *The D9 calibration rungs (2026-09-02).* `type-fest` (5.9.0, 249 exports → 270
    declarations) and `solid-js` (1.9.15, 119 exports → 119 declarations) are goldens and gate
    entries. Neither claims zero escapes; each pins that the pipeline survives the package,
    generates it deterministically, and owns every drop. What landing them cost:
    - *The compiler's API server refuses a response it cannot encode.* `type-fest` declares
      `PositiveInfinity = 1e999`, whose literal type carries `+Inf` in `value`, and Go's JSON
      encoder cannot write that - the server answers with an error frame instead of the type
      (an upstream bug at the pinned compiler; the channel survives it). Two things had to
      change. The mailbox's batch is marshalled in one piece, so one such result refused every
      request travelling with it - verified live, with the same requests answering singly - and
      `TscMailbox` now replays a refused batch member by member, so only the guilty request
      fails (`tests/Xantham.TypeScript.Wire.Tests/fixtures/infinity.ts` pins it). Above that,
      the resolve tier's `attempt` turns a `TsGoError` on one request into a reason: an export
      whose type the server would not hand over is an `Escape` finding on the export, and a type
      whose derivation was refused joins `NotFollowed` with its reason, which the shape tier
      already reads. The refusal is restated in fixed words rather than quoted - the encoder says
      "cannot marshal" on one run and "unable to marshal" on the next for the same value, which
      was the third determinism failure the e2e property caught.
    - *`typeof globalThis` groups with the compiler lib.* The checker's symbol for the global
      scope declares nothing anywhere, so by path it was unclassified and shipped: one interface
      of 700 members, a third of the file, for `type GlobalThis = typeof globalThis`. It is now
      identity-only and widens with a finding that names the scope.
    - *F# optional parameters are a tail.* `undefined` in a parameter's type is admitted anywhere
      (`createResource(source: S | undefined, fetcher, options?)`) and read as optional, which is
      FS1212 ahead of a required parameter. Only the trailing run gets the `?`; a parameter ahead
      of a required one stays required, of `option` type. The gate caught this on a *static*
      member; two abstract members in the `animejs` and `workers-types` goldens had the same
      shape and fsc had accepted it silently, so those goldens changed too.
    - What the rungs measure, to tune against: `type-fest` is 183 phantoms and 172 "Conditional
      not mapped" widenings out of 220 - the utility-type surface is type-level computation
      almost entirely, and the phantom-erasure cutoff D9 wanted calibrating is the question of
      which of those two forms each alias should take. `solid-js` is dominated by method-level
      generics ("type parameter is not in scope here", 53) and callable-and-properties hybrids
      (19), both already on the deferred list.
  - *The Fable run gate (2026-09-02).* `tests/Xantham.Generator.RunGate` links the
    `globals-lab` and `lab` goldens, and `build.fsx -- test` compiles it with Fable and runs it
    under node against `tests/fixtures/{globals-lab,lab}/index.js` - hand-written runtimes of
    the same declarations, kept deliberately small and checkable (a timer that records its
    calls). A node preload (`register.mjs`) installs the ambient globals and resolves the
    fixture package names to those runtimes, so the gate needs no install. Thirty checks
    cover the claims phase C and D made on paper: `[<Global>]` reaches `globalThis`,
    `[<Global; EmitConstructor>]` produces an `instanceof` the global class, a tagged-union
    case is the tagged object JavaScript reads and a JavaScript-built one matches the F# case
    with its fields, `Create` is the bare literal with omitted optionals absent, imports and
    overloads land on the module's exports, an options-object callback is invoked. What the
    first run found:
    - *Rest parameters on abstract members did not spread.* The render tier read a rest tail
      as a plain array on abstract members on the belief that attribute syntax is unavailable
      in a slot signature, so `tween(...values)` arrived as `[[1, 2, 3]]`. F# admits
      `[<ParamArray>]` there, fsc compiles it, and Fable spreads it; the `lab`,
      `workers-types` (eight members) goldens changed accordingly.
    - *The support package did not compile under Fable* (the `access` aliases, above).
    - *Fable's up-to-date check missed a changed linked golden*, so the stage passes
      `--noCache`.
    - Two things to know rather than fix: `[<Global("x")>]` emits the bare identifier, so a
      caller's local named `x` shadows it (`const registry = registry`); and Fable compiles a
      `float[]` literal to a `Float64Array`, which `JSON.stringify` prints as an object - the
      gate spreads or reads such arrays element-wise, and a binding that hands a `float[]` to
      a library expecting `Array.isArray` is a consumer-side `--typedArrays false` matter.
  **Still ahead in D:** nothing named; the `three` and `typescript` rungs stay in E.
- **E — hardening.** Dedup/naming at scale, fidelity-manifest UX, determinism under the
  full litmus ladder, `@types/three` and `typescript` rungs.
  - *Type parameters in scope where they are read (2026-09-02).* The largest single
    finding the D9 rungs measured - "type parameter is not in scope here", 62 sites in
    `solid-js` - was not method-level generics after all (those were already spelled);
    it was three ways a parameter's declaration and its use came apart, and
    `tests/fixtures/generics-lab` pins each:
    - *A generic declaration reached only through instantiations.* `Ready<T>` is not
      exported; `Resource<T> = Ready<T> | ...` reaches `Ready<T_Resource>`, a reference
      whose target is the declaration. The resolve tier never followed the target, so the
      shape tier named the instantiation and its members read a parameter that was not
      theirs; a second instantiation became `Ready2`, and `workers-types` carried ten
      copies of `TailEvent`. The target is now followed for entry-group references, named
      ahead of the instantiation, and every instantiation is written as an application -
      `TailEvent<'Event>` once, `SqlStorageCursorNextResult<'T>` once.
    - *A generic union alias.* The union branch of derivation never asked for alias
      type arguments, so `type Ref<T> = T | ((value: T) => void)` reached shaping with no
      scope and rendered `obj`. It asks now, as the object and conditional branches
      already did. Since 2026-10-24 an application of such an alias is written over its
      arguments (`erasedAliasApplication`, `union-alias-application-lab`): `FunctionMaybe<Live>`
      rather than `U2<string, (unit -> Live)>`, the declared form found by alias symbol and the
      site matched arm for arm; an argument rendering as an erased union keeps the flattened
      form. `solid-js` widened 327 -> 169 and `TR006` 6,348 -> 1,347;
      `ResourceSource<'S>` and `GenericValue<'T>` are now written by name.
    - *An anonymous object type hoisted out of a generic scope.* `each<T, U>(props: {
      items: T[]; render: (item: T) => U })` names `EachProps` for the parameter, and
      `EachProps` binds nothing of its own. A new pass (`bind-free-type-params`) walks
      each hoisted declaration for the type parameters it reads without binding - a
      signature's own stay inside it, another named declaration's stop at its arguments
      - and records them as `DeclParams`; the declaration is then written over them
      (`EachProps<'T, 'U>`) and every reference applies them back, in scope. First-use
      order, so the arity is deterministic.
    Measured: `solid-js` widened 97 → 67 and the finding 62 → 4 (all four on one rank-2
    callback, `untrack?: <V>(fn: () => V) => V`, which F# cannot spell); `type-fest` 16 →
    11 (`ReadonlyTuple`'s mapped body and `KeysOfUnion`, both conditional territory).
    What the compile gate had to prove: a generic interface with `[<ParamObject>]`
    `Create` over `'T`, and an application of a hoisted declaration from inside a generic
    `Exports` member - both compile under Fable.Core 5.2.0.
  - *Object intersections flattened (2026-09-02).* The largest widening left across every
    rung - "intersection of object types has no F# form yet", 290 sites over the four npm
    fixtures - was §4.6's first bullet, still unimplemented. The resolve tier now asks the
    checker for the properties, signatures and index signatures of the intersection *itself*
    when every operand is an object (`getPropertiesOfType` hands them over flattened, a
    property both operands declare typed as the intersection of its two types), and the shape
    tier names and declares such an intersection exactly as it does a hoisted anonymous
    object: `type NamedTimed = Named & Timed` is one interface with both member sets, a
    parameter-position `Named & { id: number }` hoists as `LabelTarget`, a generic alias
    `WithValue<T> = Named & { value: T }` binds `'T` on the declaration, and the reference
    names it. The is-a relation §4.6 said would be lost is kept where F# can state it: an
    operand this run declares as an interface is `inherit`ed - F# admits both the diamond
    (`inherit Loud; inherit Pitched` sharing `volume`) and the redeclaration, verified before
    relying on either - so the intersection upcasts to its operands. Members are still
    declared in full rather than left to the inherited operand, which is what keeps `Create`
    and the member list exact when an operand is not an interface (a lib type, a callable, an
    anonymous shape folded in). What stays widened, and now says which case it is: an
    intersection with a type-parameter or primitive operand has no members to read until
    instantiated (`T & { id: number }`, 84 sites in `workers-types`), and a brand is still a
    measure (D11; `brand-lab`'s two object-intersection negatives now flatten, its
    primitive-with-a-real-member negative still widens). The naming walk also learned to
    descend into an index signature's value - `Record<string, A & B>` reached its intersection
    nowhere else - which closed the last two "not declared by this run" sites. Pinned by
    `tests/fixtures/intersection-lab` and two per-pass unit tests.
    Measured: flattened `workers-types` 80, `animejs` 21, `solid-js` 8, `type-fest` 3.
    `animejs` widened 83 → 37 (exact 46 → 73); `workers-types` exact 197 → 232. The two big
    goldens grew - `animejs` 2.7k → 5.6k lines, `workers-types` 19k → 26k - because an operand
    that is a lib type or a class instance is copied member by member rather than referenced;
    that is the honest representation until a `Reference` disposition gives those operands a
    name to inherit. Not done: instantiations of a *generic* intersection alias still
    re-expand under a suffixed name (`CreateResourceOptions2..4` in `solid-js`), the same
    deferred item as generic alias applications generally.
  - *A global type library generated without the DOM (2026-09-02).* `@cloudflare/workers-types`
    replaces `lib.dom.d.ts` rather than extending it - its README prescribes
    `"lib": ["esnext"]` - and the rung had been generated with the compiler's default lib
    loaded. Every name the package shares with the DOM (`Headers`, `Response`, `Event`,
    `Crypto`, `EventTarget`: most of its 154 classes) merged with the lib's declaration, was
    grouped as the compiler lib by its first declaration's path, and never reached the harvest.
    `audit-coverage` cannot see the drop of something never harvested, so the rung's "no silent
    drops" claim was true of two thirds of the package. `xantham.json` now carries `lib`, as
    `tsconfig.json` spells it, the bootstrap hands it to `createProgram`, and the rung is
    configured `["esnext"]`. What generating it that way found, each pinned in `generics-lab`
    and by the gate:
    - *Index-only anonymous objects were never named.* `Record<string, boolean>` has no members
      and one index signature, and `synthesize-anonymous` asked for members; and the mapped
      type's own node sits in `lib.es5.d.ts`, so it inherited the lib's `widen` disposition
      although the resolve tier reads it by content (D6). Both conditions are lifted: an
      anonymous shape is the entry package's whatever file its node is in. The largest
      "`__type` is not among the generated declarations" finding fell from 208 to 31 sites in
      `workers-types` (what is left is `typeof Request` constructor objects, and `{}`), and
      `animejs`, `type-fest`, `solid-js` and `globals-lab` each gained indexer interfaces for
      members that had been `obj`. The naming walk had to stop descending into an array's
      members and skip symbol-keyed members: lib `Array<T>`'s `[Symbol.unscopables]` is an
      anonymous object too, and the walk named eleven copies of it after a session-specific id.
    - *A `when` clause between two parameters.* `PagesPluginFunction<Env, Params, Data extends
      Record<string, unknown>, PluginArgs>` rendered `<'Env, 'Params, 'Data when 'Data :> ...,
      'PluginArgs>`, a syntax error no earlier golden had reached because a constrained
      parameter had always been last. The head is parameters first, then one clause with the
      constraints joined by `and`.
    - *Constraints bind applications too.* `EventListenerObject<'Event when 'Event :> Event>`
      is the declaration; `EventListenerObject<obj>` (an indexed access over an out-of-scope
      map, widened) and `EventListenerObject<'R>` (a `typekeyof` result variable, unconstrained)
      are FS0001 at the reference. An argument that widened to `obj`, or is a variable not bound
      with the stated constraint, is written as the constraint itself - the substitution an
      out-of-scope parameter already gets - with a finding. 20 sites.
    - *No `?` ahead of `[<ParamArray>]`.* `setTimeout(callback, msDelay?, ...args)` is FS1212:
      the rest parameter is required and last, so a signature with one has no optional tail at
      all, and `msDelay` stays required, of `option` type.
    - *A finding keyed by a checker id is not deterministic.* Without the DOM,
      `ReadableStream`'s method-level generics re-instantiate themselves every generation
      (`pipeThrough<T>` returns `ReadableStream<T>`), and the frontier the depth cutoff records
      was 261 types at every cutoff tried (12, 24, 48) - the runaway the cutoff exists for, and
      nothing the shape tier renders, since an instantiation reads only its arguments. But each
      was a finding on `type#<id>`, and ids are assigned in the order answers arrive, so the
      manifest differed run to run. The frontier is one counted finding on `<type-table>` now;
      a reference that reads a cut-off type still widens under its owner's name.
    - *The fixture installer never applied the pins.* `xantham-fixtures.fsx` wrote `"*"` into
      each fixture's manifest and installed whatever npm had that morning, so a fresh
      `build.fsx -- test` on the day a rung published (`workers-types` 5.20260902.1) failed
      the drift guard rather than the goldens. A pinned rung is installed as `name@version
      --save-exact` now; unpinned fixtures still float.
    Measured: `workers-types` 1515 → 1714 symbols (the classes and what they reach), exact
    232 → 260, widened 454 → 384, escape 44 → 107 (`any` on the newly harvested classes, owned
    as such), golden 26k → 30k lines. Not done, and now visible: `typeof X` constructor objects
    as member types (31 sites, the whole `ServiceWorkerGlobalScope` constructor table); 81
    static class members (`DOMException.INDEX_SIZE_ERR`) still awaiting statics emission; and
    the one `Record<string, unknown>` shared by many members is named after the first path
    that reached it (`RequestInitCfPropertiesBase`) - hash-consing by id names a lib mapped
    type as if it were the package's own literal, and an alias-aware name (`Record` and its
    arguments) would read better.
  - *Class statics emitted (2026-09-02).* The 81 sites the entry above left visible - a
    finding whose own message said statics emission awaited a fixture. A class's statics live
    on the constructor object, not on the prototype, so each one binds individually through a
    dotted selector off whatever binds the class: `[<Import("Counter.MAX", "statics-lab")>]`,
    `[<Global("Gadget.SPEED")>]`, `default.MAX` for a default export. They are emitted as
    static members of the class's own instance interface rather than on `Exports`, so the
    consumer spells `Counter.MAX` exactly as TypeScript does. A static is the same thing an
    export is - a bound value on a JavaScript object - so it reuses `FsExportMember` and one
    renderer serves both. `tests/fixtures/statics-lab` pins the behaviour; what it taught:
    - *Fable compiles an assignment to an imported static as a call.* `Timer.tick <- 3.0`
      came out `Timer.tick(3)`. Read off the emitted JavaScript, not recalled. So a settable
      static is emitted get-only, with a finding (`SC003`); the setter route (`with get` /
      `and set`, which is also the only way past FS0554 on a `get, set` static with a body)
      was abandoned once the emit was read.
    - *F# admits a static beside an instance member of the same name only when both are
      methods.* Property over property is FS0441, method over property FS0434, and a static
      property under an abstract method *declares* cleanly but is FS3214 at every use. The one
      arrangement that survives is the one the wild has (`Response.json` is both). The other
      three are dropped, and `SC002` now says why instead of saying it awaits a fixture.
    - *A declaration carrying a static is no longer inferred as an interface.* FS0054/FS0365
      at the type, not at the member. `[<Interface>]` is emitted whenever a declaration has
      statics or a synthesized `Create`.
    - *A class with no instance members has no type to hang statics on* - F# has no
      free-standing static member - so those are dropped with `SC004`.
    - *JavaScript inherits statics down the constructor chain, and the checker agrees.* It
      reports the base's statics as the subclass's own, so `Doubling.MAX` is emitted again;
      the run gate proves it reads 100 rather than `undefined`.
    Measured: `workers-types` `SC002` 81 → 0 and the pass's widened 85 → 6, for 214 lines of
    statics in the golden (`DOMException`'s 25 legacy `*_ERR` codes, `AbortSignal.abort`,
    `EventSource.from`, `URL.parse`/`URL.canParse`); four symbols' worst loss improved with
    it (widened 384 → 380, ergonomic 963 → 967, escape held at 107), and the statics' own
    `any` and `null` shapes are owned where they land.
    Both gates grew: `statics-lab` is linked into the compile gate and the run gate, and
    `globals-lab`'s `Gadget` gained a static so the `[<Global>]` dotted selector is run-gated
    beside the imported one (30 → 40 checks). Not done, and now visible: `DOMException.isError`
    is inherited from the lib's `ErrorConstructor`, whose member type is a named declaration
    the generator never emits, so it lands as `static member isError: obj` (`TR023` plus a
    misleading `SC003`) rather than as a method; and `synthesize-paramobjects` still offers a
    `[<ParamObject>]` `Create` on a class whose instance type is all properties (`Box`), which
    cannot build a class instance - both predate this pass and are separate behaviour changes.
  - *The type flags the shape tier refused (2026-09-02).* `TR014` was one finding standing in
    for six unrelated constructs, and its message was a flag name - the single largest thing
    the manifest could say about a symbol without saying anything. Aggregating every golden
    manifest by that key gave the survey: 350 sites, of which `TemplateLiteral` 60,
    `NonPrimitive` 42, `BigInt` 13, `UniqueESSymbol` 5, `ESSymbol` 3, `Conditional` 227. Five
    of the six are now mapped, each with a finding that names the construct and what it cost;
    `Conditional` is untouched and keeps `TR014` to itself. `tests/fixtures/flags-lab` pins
    every mapping and its negatives, and is the readable half of the evidence. What it settled:
    - *A template literal is a string at runtime, so `string` is the whole of the mapping.*
      The pattern is what is lost, and F# has no form for it; `obj` lost the pattern *and*
      the type. The same goes for an intrinsic string mapping (`Uppercase<string>`), which
      says separately that the transform is gone (`TR037`, `TR038`, §4.11).
    - *The two template-literal negatives already worked and had to keep working.* A closed
      template literal over finite unions (`` `on${Capitalize<Mode>}` ``) is expanded by the
      checker into its literal union and takes the StringEnum path - Exact, never reaching the
      widening. One over a type parameter (`` `x-${T}` ``) stays the erased phantom of D9,
      because `string` binds no variable and would lose the arity. Both are in the lab.
    - *Fable 5 compiles F# `bigint` to the native JavaScript `BigInt`, so the mapping is Exact
      and reports nothing at all.* Read off the emitted `fable-out/Program.js`, not recalled:
      `2I` is `fromInt32(2)` and `9007199254740993I` is `fromInt64(9007199254740993n)`, both
      from `fable-library-js.5.0.0/BigInt.js`, whose `fromInt32(n)` is `BigInt(n)`. The run
      gate calls a fixture runtime that `TypeError`s on any argument whose `typeof` is not
      `"bigint"`, sums with `+` over `0n`, and reads `typeof` back through `emitJsExpr`. A
      `bigint` *literal* type keeps its own widening (`TR039`), as its string and number
      counterparts do.
    - *`obj` was already the right answer for `object`; only the report was wrong.* `object`
      is TypeScript for "not a primitive" and F# has nothing narrower, so the mapping stands -
      but it is still a widening, and `TR040` now says which one: `obj` admits exactly the
      primitives `object` was written to exclude. A finding that reports a flag name reports
      nothing; a finding that reports the wrong tier overstates the damage.
    - *Nothing binds a symbol.* Fable.Core 5.2.0 declares no `JS.Symbol` - checked by
      reflecting over the shipped assembly, since §4.1 aspires to one. So `symbol` and
      `unique symbol` still widen to `obj`, with `TR041` and `TR042` naming the missing
      binding and, for the unique one, the identity there is no F# form for. These are the two
      findings to revisit if a binding ever ships.
      *Superseded (W1, 2026-10-24):* the binding is the compiler lib's own `Symbol`
      interface, which Core.TS generates. Resolve follows the checker's apparent type of a
      `symbol` or `unique symbol` into `TypeFacts.SymbolApparentType`, and `typeRef` writes the
      name that interface takes in the run (`Fable.Core.TS.Es.Symbol` under `widen`, the local
      declaration under `ship`) with the ergonomic `TR069`. `TR041` remains the fallback where
      the interface has no name; `TR042` now reports only the lost identity.
    - *Mapping a flag unblocked the unions it was poisoning.* `number | bigint` was `obj`
      across `workers-types` because one arm was unmapped; it is `U2<float, bigint>` now, and
      `WorkflowSleepDuration` (`` `${number} ${WorkflowDurationLabel}` | number ``) is
      `U2<float, string>`. Fourteen `TR035` "union with an obj arm" findings went away with
      them - and one union that had been collapsing distinct arms into a single `obj`
      (`Attribute.value`) now has more than four and reports `TR036` instead, which is the
      honest answer rather than a regression.
    Measured, aggregated over every manifest rather than read: `TR014` 350 → 227 sites, and
    every one that remains is `Conditional` (type-fest 172, solid-js 49, workers-types 3,
    animejs 2, keyof-lab 1). Per rung: `workers-types` 110 → 3, `type-fest` 178 → 172,
    `solid-js` 54 → 49, `animejs` 3 → 2, `keyof-lab` 2 → 1, `lib-lab` 2 → 0, `brand-lab`
    1 → 0. In their place, `TR037` 60, `TR040` 42, `TR042` 5, `TR041` 3 - and 13 `BigInt`
    sites that now report nothing, because nothing is lost. `workers-types` tiers moved
    widened 380 → 375 and ergonomic 967 → 972 (five symbols whose only widening had been a
    `bigint`), exact 260 and escape 107 both held; `TR035` 36 → 22, `TR036` 16 → 17. The
    golden diff is 10 files, 391 insertions and 573 deletions, none of it in a package the
    change did not target. All three gates green: `dotnet build Xantham.slnx` clean,
    177 + 85 Expecto tests, and the run gate 40 → 49 checks with `flags-lab` linked into both
    gates. Not done: `Conditional`, the 227 sites above and now the whole of `TR014` - §4.11
    treats a conditional type as deferred, and it is a shape question, not a flag one.

- **Phase E — a member's type-parameter head is spaced off its colon (`parse-lab`).**
    A method-level type parameter whose constraint is itself a generic application renders a
    head ending `>>`, and F# lexes `>>:` as one token: the member's colon is swallowed and the
    file fails to parse with `FS0010`. Because a parse error stops the compiler before it
    reports anything else, one such member takes the whole compile gate with it — which is what
    `@types/three` hits at `Raycaster.intersectObject`, and what stopped that rung's recon from
    seeing any other diagnostic.
    The fix is `Render.memberColon`: a head ending `>>` is followed by ` :`, everything else
    keeps the tight `:`. The narrowness is deliberate and was measured at the lexer rather than
    assumed — `m<'T>:`, and `m<'T when 'T :> Ev>:` with a constraint that is a bare name, both
    parse, so only a nested constraint needs the space. Consequence: **no golden written before
    this moves** (`git diff --numstat` over the whole corpus: zero files), and the corpus
    contained no `>>:` site to begin with, because such a file could never have passed the gate
    to be committed.
    Evidence is `tests/fixtures/parse-lab/`: two members whose heads end `>>` (one constraint,
    and two joined with `and`) against three negatives that must keep the tight colon. It is
    linked into the compile gate, which is the gate that has an opinion here; the run gate is
    not, since nothing about this reaches runtime. Three new tests — one on the renderer
    against a hand-built model, two on the rung — plus the standing guard that no rendered
    source anywhere contains `>>:`. All three gates green: `dotnet build Xantham.slnx` clean,
    182 + 85 Expecto tests, run gate 49 checks. Found by the `@types/three` recon
    (`docs/.ai/plans/generator-three-rung.md` §9, blocker 4); it is not `three`-specific, which is
    why the lab landed regardless of whether that rung ever does.
  - *Constructor objects declared (2026-09-02).* The 31 `__type` sites the harvest entry left
    visible, and the `DOMException.isError` finding the statics entry left visible, are the same
    construct seen twice: `typeof X` at a member position is a *constructor object* - the static
    side of a class, an object carrying `prototype`, the statics and the construct signatures -
    and F# has no first-class type for it. It reached `object-ref` with no `DeclNames` entry and
    fell out as `obj` under `TR023`, whose message printed the checker's symbol name: `__type`
    for `declare const Widget: { new (...): Widget }`, and, worse, the *class's own name* for
    `typeof AbortController`, which reads as though a declaration the run does emit had gone
    missing. It had not; `AbortController` is declared, and `typeof AbortController` is a
    different type.
    A constructor object is now declared as its own interface, `<Stem>Constructor`, whose
    construct signatures are `[<EmitConstructor>] abstract Create` members (`$0` is the object
    the member is read off, so `scope.Gauge.Create 3.0` news the constructor the property holds)
    and whose properties are the class's statics; `prototype` is the instance side, declared
    separately, and is dropped. Three options were on the table and this is the first:
    - *Declare the constructor object (taken).* The only one that keeps the construct
      constructible. `[<EmitConstructor>]` on an abstract member is exactly the JavaScript this
      is - `new` on a value - and the run gate proves it: `scope.Gauge.Create 3.0` is
      `instanceof Gauge` and its `size` reads back.
    - *Point the reference at the class's own declaration, where the statics already live
      (rejected).* The member would be typed as an *instance* where a constructor stands, so
      `new` is unreachable and `scope.Gauge.UNIT` resolves against the wrong side. Statics
      emission binds each static through a dotted selector off the class's own binding, which
      is a different value from the one this member holds.
    - *Widen honestly with a better message (rejected as the primary answer, kept as the
      fallback).* `TR043` names the class the constructor object constructs rather than
      `__type`, and fires when the object's own declaration is not in scope.
    Naming is a new pure pass, `name-constructor-objects`, rather than a relaxation of
    `synthesize-anonymous` - whose `needsName` had explicitly refused any type with construct
    signatures. The distinction is that naming is driven from *reference positions*: a class
    that nothing ever `typeof`s keeps its static side on its own declaration, where
    `shape-classes` puts it. Naming every constructor object the harvest can see would have
    added 154 unreferenced `XConstructor` interfaces to `workers-types`. The name comes from
    the export it is the value of, else its own non-synthetic symbol name, else the instance
    type its first construct signature returns, suffixed `Constructor`.
    `DOMException.isError` is the same fault at the static side: it is inherited from the lib's
    `ErrorConstructor`, which this run resolves identity-only, so it has no call signatures to
    shape a method from - and the property branch then reported `SC003` ("settable") because a
    method symbol is not `ReadOnly`. `SC005` says what actually happened and names the group
    whose resolution lost the signatures.
    What it taught:
    - *A constructor interface is not plain data.* `synthesize-paramobjects` would otherwise
      offer it a `[<ParamObject>]` `Create`, colliding with the construct signatures' own.
    - *An interface of construct signatures only used to be `obj`.* `shape-interfaces` declared
      nothing for a type with no members and no indexers; `ParcelFactory` in the lab is that
      shape, and it now carries two `Create` overloads, one of them generic.
    Measured: `workers-types` `TR023` 229 -> 148 (-81; of those, the `__type` sites 31 -> 20),
    `SC003` 1 -> 0, `SC005` 0 -> 1, `SI004` 0 -> 83, exact 260 -> 261, ergonomic 967 -> 1031,
    widened 380 -> 388, escape 107 -> 117; `animejs` `TR023` 103 -> 101 with ergonomic 76 -> 80
    and widened 34 -> 32; `type-fest` `TR023` 355 -> 351 with widened 219 -> 215; `solid-js` and
    every existing lab unchanged. The increases are the 83 new declarations bringing their own
    members into shaping scope - their `any` statics land as `TR008`, their optional members as
    `TR032` - which is the loss being owned where it happens rather than hidden behind one `obj`.
    `tests/fixtures/ctor-lab` pins the construct and its negatives (a class nothing `typeof`s,
    `typeof` over a plain value) and is linked into both gates; the run gate grew 40 -> 49 checks.
    Not done, and now visible: `TR043` fires nowhere in the corpus today, because every
    constructor object a reference reaches is one this run may declare - it is the honest answer
    held in reserve for a `Reference`-disposition compiler-lib constructor object, and only the
    unit test exercises it. And a class's statics are now written twice wherever both an
    `XConstructor` and the class itself exist - once as `Exports`-bound dotted selectors, once as
    that interface's members - shaped from two different views of the same static side, and the
    two are not cross-checked against each other.
  - *Reached ambient class sides (2026-09-13).* Harvest retains runtime ambient class export
    metadata separately from public exports, using the existing resolver for quoted modules and
    `export =` namespace bodies. Resolve reads constructor signatures only for reached shipped
    classes and follows their parameter/type-parameter/return dependencies. The common class-side
    view feeds optional-hook naming, interface inheritance admission and entrypoint shaping;
    dependency recovery adds no public constructor/static occurrences. Source-package imports
    remain ordinary bindings alongside the existing runtime/public-input check. Catalog source
    and API authentication remain strict. `dependency-entrypoint-lab` compiles an authenticated
    generic subclass with an inline constructor argument and optional hook, rejects a wrong
    constructor argument, and covers ordinary modules, type-only exports and a package's own
    quoted public module. Its Fable checks exercise the ambient constructor and hook presence.
  - *Interface bases emitted as classes (2026-09-13).* Classes removes an interface's
    inheritance edge when the accepted entrypoint map emits its target as an abstract class.
    This precedes inherited-member deduplication, preserving the complete member surface and
    ordinary constructor exports. Existing SI006 records the omitted nominal relation;
    ordinary interface bases retain their inheritance. `catalog-class-inheritance-lab` covers
    explicit and inherited constructors, an interface extending the class, ordinary upcasts,
    and typed inherited members. Ambient bases remain subclassable; ordinary derived SDK
    classes retain their existing interface-and-constructor representation.
  - *Explicit class contracts (2026-09-13).* Resolve records `implements` types separately
    from `BaseTypes`, using typed class heritage nodes and the compiler's type-node query.
    Reachability, free parameters and catalog source closure include these contracts.
    `shape-interfaces` sends regular class contracts through its existing named-interface and
    cycle guards; catalog ownership is authenticated and redirected at the existing later
    catalog stage. Entrypoint classes, and contracts whose target is an entrypoint class, retain
    the class constructor representation and report the unrepresented contract as `SI002`.
    Optional lifecycle hooks and generic constraint inference retain their existing base-type
    decisions. `class-implements-lab` covers fixed and constrained generic contracts plus a
    structurally matching class without `implements`; `class-implements-entrypoint-lab` covers
    subclassing, an optional hook, `Error` inheritance and a class-shaped contract target.
  - *Declared bases inherited (2026-09-02).* §4.4's heritage rule, executed: a base an
    `extends` clause names becomes an F# `inherit` beside the members that were already
    flattened in, so the derived type upcasts to it. `SI002` - "base has no F# name at this
    position" - was firing 77 times in `workers-types`, the largest remaining structural loss
    once intersections were settled, and it fired undifferentiated: a base that widened away, a
    base a shipped package binds, and a base this run itself declares all read the same. They
    are three cases now. §4.6's intersection operands and §4.4's bases were already the same
    question - "is this a type the declaration *is*, and can F# say so" - so they go through one
    gate: inheritable exactly when the reference resolves to a name this run declares as an
    interface. Members are still declared in full beside the `inherit`, which is what keeps
    `Create` and the member list exact when a sibling base is not inheritable.
    `tests/fixtures/inherit-lab` pins it - the redeclaration, the narrowed member, the diamond,
    a generic base at a variable and at a fixed argument, a class base, and the three negatives.
    What it taught:
    - *An F# name is not a type id, so the cycle guard is not optional.* TypeScript admits no
      cyclic heritage, but two ids hash-consed onto one name, or one declaration reached twice,
      can close a loop the source never wrote, and F# rejects it outright (FS0954). The edges
      are accumulated into a name graph in declaration order and an edge the graph can already
      walk back from is refused (`SI007`), which is enough to keep the whole graph acyclic. It
      fires nowhere in the corpus; the unit test is synthetic, and that is the point.
    - *The arity repair can unname a base after the edge is drawn.* A base whose reference
      widens to `obj` would render `inherit obj`, which is FS0887 - `obj` is not an interface
      type. The widening owns the loss already, so `repair-arity` drops any `inherit` left
      holding something that is not a named application rather than emitting a file that
      cannot compile.
    - *F# subtyping is nominal where TypeScript's is structural.* `EventTarget<'EventMap when
      'EventMap :> EventCurrentTargetItem>` had never been *applied* by a generated file
      before; the first `inherit EventTarget<WorkerGlobalScopeEventMap>` was FS0001, because
      the event map satisfies the bound by having its members and not by naming it. An
      argument that reaches its bound through neither bases nor intersection operands is
      written as the bound instead (`TR044`), the same substitution an argument that widened
      to `obj` already gets. Two sites, both in `workers-types`.
    - *Wave two, lane C: and a bound the run can see is unsatisfiable is not written.* The
      substitution above is the right answer when the head's constraint is real, and the wrong
      one when it is not: `class Geometry<Attributes extends Wide = Narrow>` renders a bound
      that rejects its own default, so every bare `Geometry` was substituted up to
      `Geometry<Wide>` - 328 sites on the `@types/three` rung, all of them
      `BufferGeometry<NormalBufferAttributes, …>`. `inherit` cannot reach them, because the two
      types are `Record<string, …>` aliases with no declared base. The head drops the bound
      instead (`TP008`, Ergonomic) and `TR044` reads the same predicate, so it falls silent on
      exactly the constraints the head no longer writes and the resolved argument survives.
      `TP008` 0 → 33, `TR044` unmoved at 2; `tests/fixtures/nominal-lab` pins it.
    Measured: `workers-types` `SI002` 77 → 20 with `SI005` (an emitted is-a relation, Exact)
    at 57, `TR044` 0 → 2, `TR024` 105 → 114 (a base reference now goes through `typeRef`, so
    its type arguments raise lib-binding findings), exact 260 → 261 and ergonomic 967 → 966,
    golden 30580 → 30637 lines and `inherit` lines 107 → 164. `animejs` `SI002` 4 → 0,
    `solid-js` 7 → 1, `statics-lab` 1 → 0; `type-fest` moved by a message rewording alone. The
    count reads oddly because `SI002` used to fire once per *declaration* and now fires once
    per *base*: `solid-js`'s 7 became 8 findings across 7 declarations. Both gates grew:
    `inherit-lab` is linked into the compile gate and the run gate (40 → 49 checks), where the
    upcast is proved to be the identical object rather than only to type-check. Not done, and
    now visible: the 20 `SI002` left in `workers-types` are all `extends Error`, whose base is
    the compiler lib's and which nothing shipped binds - a `JS.Error` binding would take them
    all; `SI006` names `JS.Promise` in the lab and fires nowhere on an npm rung, so the choice
    to refuse lib bindings an `inherit` costs nothing measurable yet; and the two `TR044` sites
    state the is-a relation at a coarser instantiation than TypeScript's
    (`inherit EventTarget<EventCurrentTargetItem>`, not `<WorkerGlobalScopeEventMap>`), which
    is honest but is the one place the emitted relation is weaker than the source's.

- **Phase E — a conditional type is mapped by the branch the checker can prove (`conditional-lab`).**
  `TR014 TypeFlagsNotMapped` was 227 sites, every one of them a conditional reaching the flag
  dispatch and falling through to `obj` under a name that described the pass rather than the
  type. It is now zero, and the sites are split by what the run can establish.

  Where the branch is decided — the head's bound settles the condition
  (`isTypeAssignableTo(check, extends)`), or one branch is `never` — the branch is mapped
  through the ordinary `typeRef` path, so nesting costs nothing and the branch carries its own
  findings. That is `TR046 ConditionalResolvedToBranch` (Ergonomic), 61 sites. Where two
  inhabited branches share no F# form, the reference stays `obj` under `TR045
  ConditionalTypeDeferred` (Widened), 187 sites. `ConditionalFacts` on `TypeFacts` carries the
  branch taken, and the resolver frontier admits that branch alone.

  Rejected: dropping an `undefined` or `void` branch, which narrows rather than widens, since
  `never` is the only uninhabited one; and unioning both branches, which reads as precision
  where the deferral is the honest answer.

  `isTypeAssignableTo` reads a type parameter through its bound, and the reading is
  directional. With a parameter as the *target*, `undefined extends T ? [] : [value: T]`
  resolved true through `T`'s `unknown` bound and `Ai.run`'s model name rendered `unit`. A
  target-is-`TypeParameter` guard, plus refusing a `never` true branch, returns those 3 sites
  to `TR045` and leaves `@cloudflare/workers-types` byte-identical. 248 conditional findings
  against 227 old sites: 21 are nested conditionals sitting behind a branch nothing previously
  reached. `type-fest` 11955 -> 11857 lines, widened 215 -> 203, ergonomic 59 -> 69.

  Two interactions were measured and left: `ExcludeStrict` and `ExtractStrict` in `type-fest`
  move Widened -> Escape, because a resolved right side uses fewer parameters than its head and
  `RA001` drops an alias it used to keep as a phantom - the repair belongs to the arity pass,
  where it would move 9 pre-existing sites; and `solid-js`'s `Setter` renders `Action<obj[]>`
  where it rendered `Func<obj, obj>`, the `obj[]` being section 4.12's widening of an
  empty-tuple rest parameter rather than section 4.11's call.

- **Phase E — a DefinitelyTyped package is named for the library it describes.**
  `Naming.packageModule` takes its name from the runtime package, so `@types/three` binds as
  `Three` and `@types/babel__core` as `Babel.Core`. `@types/` is a TypeScript-side convention for
  attaching declarations to a library that shipped without them; on the F# side the declarations
  and the library are one binding, so the scope segment separates a thing from itself. The
  derivation is lane D's `derivedRuntimePackage`, which already undoes DefinitelyTyped's
  `scope__name` folding — reusing it keeps `Babel.Core` a namespace, where stripping the prefix
  literally would give `BabelCore`. It sits inside `packageModule` rather than at either call
  site, so `Pipeline.moduleName` and `Naming.groupModule` stay on one name and a `reference`
  group templates the module a real `ship` run of that package writes.

  This amends O7's pinned naming contract, and it is breaking for every binding generated from a
  `@types/*` package. One golden moved: `types-only-lab` renders `module rec TypesOnlyLab` with
  its manifest reading `"module": "TypesOnlyLab"`, and the file renamed with it — git recorded a
  pure rename, so the binding is otherwise identical. Provenance is untouched, the header still
  recording `@types/types-only-lab`; `runtime` in `xantham.json` still overrides the import
  specifier and leaves the module name alone; and a real scope keeps its segment, with
  `@cloudflare/workers-types` staying `Cloudflare.WorkersTypes`.

**Wave two, closed (2026-09-02).** Five lanes over `docs/.ai/plans/generator-wave-two.md`, dispatched
as three concurrent lanes on disjoint pass files and then two sequenced through `Shape/Spec.fs`.
Every finding case the wave needed was appended in one pre-dispatch commit, so no branch edited
`Findings.fs` and the positional-renumbering conflict did not arise; two case wordings that did
not match the condition an agent ended up guarding were corrected on the integration branch
rather than in the lane. The corpus stands at 21 fixtures. `TR014` closed to zero, and the three
largest keys (`TR032`, `MB003`, `SP001`) are working as designed and were left alone. `TR006`
(1,212, a string literal type widened to `string`) was reviewed and accepted as intent for now,
so it is unmoved by design rather than by omission.

The recon that scoped the wave had run against a commit predating `c592092`, so blocker 3's 328
`FS0001` were already unreachable when lane C opened it - but they were being paid for by
widening every argument at those sites, which is the finding that mattered and would have been
missed by taking the recon's count on trust. `@types/three` stays out of the corpus; the four
reasons in `generator-three-rung.md` for holding it back are unchanged by this wave.

**Wave three, closed (2026-09-02).** Seven lanes over `docs/.ai/plans/generator-wave-three.md`, in two batches of concurrent lanes on disjoint pass files. The corpus stands at 28 fixtures and 14,803 findings, from 21 and 15,278. `TR023` fell 727 to 143 and its cause 1 is closed: every site remaining names a type rather than a member. Escape fell 200 to 192.

The wave was scoped by a measurement rather than by a plan. Lane G re-ran the `@types/three` harness against wave two's HEAD and found one of four criteria met - the 128 MB file compiles clean where it had 666 errors - and the instantiation runaway unmoved to the digit. It reduced the reason to twelve lines, and lane J closed it: `three` renders 112,054 lines against 482,805, the runaway falls from 70.6% of the file to 0.1%, `TR002` 5,698 to 4, `TP001` 162 to 2. The rung is still refused, now on size alone at 112k against ~50k, and `TR019` 0 to 4,461 is the price a memberless phantom declaration charges. Blocker 1 had been open since the rung was first reconnoitred.

Three things the wave learned that outlive it. `TR044`'s absence was not evidence of a closed hole: lane K's lab compiled to three `FS0001`, so a primitive, tuple or array argument against a written constraint had been falling through all along. `TR018` is a distinct defect rather than `TR023`'s residue - it moved by zero under lane L, refuting the hypothesis in the `TR023` recon's §9 - and is re-priced in `docs/.ai/plans/generator-tr018-recon.md` at 194 sites over four causes. And `RA001` had been masking a malformed type-parameter list, which is what a drop does: it hides the thing it drops.

`TP007` is constructed nowhere in `src/` and is retained rather than retired, because removing it renumbers `TP008`, which is quoted by four source files and by measurements two plan documents record. The case says so.

The measurement handed up rather than acted on: `RT001` says `@cloudflare/workers-types` now truncates 1,772 types at `FollowDepth` 12 against 261 before, and a five-symbol lab from another lane crossed the cutoff during composition. No output is wrong today. Raising the cutoff is a corpus-wide behaviour change and belongs to a wave that measures it deliberately.

**Wave four, closed (2026-09-03).** Four lanes over `docs/.ai/plans/generator-wave-four.md`, in one
batch on disjoint pass files. The corpus stands at 30 fixtures. `TR018` fell 197 to 77 and
`@cloudflare/workers-types` - now the litmus test in place of `@types/three` - reads exact 231,
ergonomic 1,061, widened 365, escape 115, from 374 widened.

The wave was scoped by the user's dispositions rather than by the corpus. Four of the six losses
in `docs/.ai/fable5-workarounds.md` are closed as unresolvable and carry no work in any wave. Two
carried work and both landed, so **§3 and §6 of that document are now closed**. Deprioritising
`@types/three` cost the previous wave-four list its second item: `alignOperands` stands at 45 on
`three` against 1 on cloudflare, so with the rung out of scope the lane had no justification and
was not dispatched.

Three things the wave learned that outlive it. **A settable static was never blocked by Fable's
compilation model** - `Shape/Classes.fs` asserted that an assignment to an imported static
compiles to a call, which is true of a per-member `[<Import("Budget.limit", …)>]` and false of a
type-level `[<Import("Budget", …)>]` carrying a bare `get`/`set` member. The loss was the
placement of the attribute, and the run gate now writes through the binding and reads the value
back off the JavaScript object. **A recon's prediction was low by a third**: `TR018` was priced at
112 and landed at 77, because reducing `X & {}` inside a union also removes the union-and-union
intersections booked to a different cause. And **`TR035` is a cascade rather than a population** -
it fell 88 to 27 while `TR036` rose 35 to 72, because arms that used to collapse into one `obj`
now survive as distinct types and meet D4's cap of four instead.

`docs/fable-binding-gaps.md` is new, and is a fourth cause of loss beside the three
`fable5-workarounds.md` sorts into: the mapping is expressible and **`Fable.Core` ships no name to
bind to**. Four entries, all answered - the synchronous iteration protocol (28 sites; the async
half ships and the sync half does not), `symbol` (11), `BigUint64Array` (1), and a bound name that
drops arity (132). The user's disposition on all four is to hand-roll or generate the bindings in
`src/Xantham.Fable.Core`, shadowing the `Fable.Core.JS` namespace so the names resolve there, and
to raise a PR upstream once the generator is working. That is a wave-five lane.

The measurement handed up rather than acted on, for the second wave running: `RT001` and the
`FollowDepth` cutoff. `docs/.ai/plans/generator-cloudflare-recon.md` adds three more - a finding-count
inflation of x14 in `erasedUnionRef` that makes `TR037`'s 56 into 4 real owners, an
`ErasedUnionArity` cap of 4 that `Fable.Core`'s `U2`-`U9` does not require, and `KVNamespace.get`
binding only its first overload, which makes the arrayBuffer, stream and json forms unreachable
from F# on the litmus package.

## 7. Decisions (2026-09-01)

All six original open questions were resolved in review; each is also inlined at its
section above.

- **O1 — accumulating records.** Per-tier model types that include earlier tiers' artifacts
  as required fields; passes are per-tier endomorphisms, tier transitions are total
  constructors. Compile-time "read before written" safety without option-unwrapping or
  model copying (§2).
- **O2 — direct text emission.** Generator-owned printer, no formatter dependency; golden
  stability over delegated style; compile gate absorbs the correctness risk (§3, Tier 4).
- **O3 — linear pass lists.** No DAG; source order documents execution order; debug-build
  well-formedness assertions are the escape hatch if ordering bugs appear (§4).
- **O4 — JSON config with generated schema.** `xantham.json` beside the target
  `package.json`, JSONC-tolerant, JSON Schema generated from the config record; generator
  core is a library function, CLI a thin shell over it (§6). *Built 2026-09-03, wave five
  batch 2:* `src/Xantham.Cli` and `xantham.schema.json`.
- **O5 — golden-print snapshots.** Human-readable textual projection of the model as the
  tier-test corpus and the between-pass debug dump; no machine round-tripping required
  anywhere in the strategy (§5).
- **O6 — `tests/Test.fsx` retires after phase A.** It remains the ad-hoc live-compiler
  probe until the walking skeleton runs end to end, then is deleted (not archived — its
  lessons are already recorded in `wire-remaining-work.md` phase 1).
- **O7 — resolution groups by package boundary (2026-09-01).** The resolve tier
  classifies every type's origin into a *group* — the entry package, the compiler's
  `lib.*.d.ts`, or a dependency, from its declaration's file path — and a per-group
  **disposition** decides how deep resolution follows and what a reference to the
  group's types renders as:

  | Disposition | Resolve tier | Shape tier |
  |---|---|---|
  | `ship` | full member resolution | group emitted as its own module (its own package) |
  | `reference` | identity only (name, arity, type args) | `FsNamed` into the group's templated module |
  | `map` | identity only | redirected to an existing package (`Fable.Browser.*`, BCL/Fable-native types) |
  | `inline` *(future)* | demand-driven full | folded into the entry group, scoped to what is actually referenced |
  | `widen` | identity only | `obj` + finding |

  The entry package (and unplaceable anonymous types) is always `ship`. Every other
  group defaults to `widen` **until the shipped compiler-lib package exists**, at which
  point the default flips to `reference` — a `reference` emission is Exact, no finding.
  Groups are addressed in `xantham.json` under `"groups"` by npm name, the compiler lib
  as `"typescript/lib"`.

  *Status (2026-09-03, wave five batch 1):* `ship`, `reference`, `map` and `widen` are
  implemented; `inline` is not. `map` is configured per group as a per-name table
  (`{ "map": { "Buffer": "Node.Buffer.Buffer", "Readable": { "name": "...", "arity": 1 } } }`),
  because the arity a destination takes cannot be inferred from the site and a group-level
  rule has nowhere to carry it. A shipped group is now written as its own module under
  `groups/`, so a run emits one file per `ship` group rather than one file. The compiler lib is the one group where a `map` already happens in practice, and it
  happens *below* the disposition rather than through it — the group is still configured
  `widen`, and `Shape.libBinding` intercepts any name the pinned tables bind
  (`Naming.LibBindings` → `Fable.Core.JS.*`, `Naming.BrowserBindings` → `Browser.Types.*`)
  before the widening applies. Two tables of known-good names are not a configurable
  redirection of a whole group. `map` (wave five, lane R) is that redirection, and the
  tables are why its destination carries arity rather than a name alone.

  Two consequences are the point of the design. First, generation order stops
  mattering: a `reference` group templates exactly the names a real `ship` run of that
  group produces, so "generate B against already-generated A" and "generate B first"
  emit identical source. Second, **naming is a contract**: `Naming.groupModule`
  (`@scope/pkg` → `Scope.Pkg`, `@types/pkg` → the library's own name, compiler lib →
  `TypeScript.Lib`) is pinned and versioned because independently generated packages must
  agree on every templated name — while
  remembering that source-level agreement is not identity; the compiled *assembly*
  (NuGet version discipline against the `typescript` npm pin, as Wire already
  practices) is what unifies a type across packages. Demand-driven resolve (a
  prerequisite for `inline` and for shipping large groups) is phase B+.

  **The first consequence holds as of wave five batch 2**, and
  `tests/Xantham.Generator.Tests/MultiPackage.test.fs` asserts it rather than claiming it.
  Both breaks lane T found are repaired. A templated *generic* now carries the arguments the
  site applies (lane U, `Shape/Spec.fs`): `Box<string>` reads `Dep.Box<string>` where it read
  `Dep.Box` and failed `FS0033` in silence. A referenced *alias over an object literal* now
  reads under the name the dependency ships, taken from its alias symbol (lane V,
  `Resolve.fs`); previously the symbol `__type` sat on `symbol` rather than `aliasSymbol`, so
  `isAnonymousShape` was true, the group shortcut was skipped, and the dependency's shape was
  re-derived into the entry package under a second name — one TypeScript type declared as two
  unrelated F# interfaces, which compiles. Lane V measured that break's reach before repairing
  it: one declaration duplicated across the whole corpus, because 34 of 35 fixtures were
  single-package. Silent, rather than rare.

  Arity remains unverifiable from inside a single run. A `reference` site applies its type
  arguments against a group resolved by identity only, so `TR054` records the applied count
  as an escape on otherwise correct output.

  *Amended (wave five, batch 3): the naming contract takes a configured namespace.* A package
  family publishes under one F# namespace, and `namespace` in `xantham.json` is where it says
  so: the entry package is named `<namespace>.<Leaf>`, each group `groups` names takes the same
  form, and `module` names the family's root outright. `@cloudedge/agents` under
  `FSharp.CloudEdge` reads `FSharp.CloudEdge.Agents` from either side, so one key per member
  and one `module` in the root is the whole configuration. Membership is what `groups` lists
  rather than what an npm scope implies, which lets a family span scopes and lets an unscoped
  family exist. A dependency the configuration leaves unnamed keeps its derived module.

  This moves part of the naming contract from pinned derivation to agreement between two
  configuration files, which is the failure mode lane T spent a wave finding. `GE004` is the
  guard: one escape per group a run names from its namespace, recording that the name is an
  assertion about a run performed elsewhere. A run configuring no namespace raises none and
  emits exactly what it emitted before.

  *Recursive package output (2026-10-09).* `recursiveGroups: true` places the entry and
  shipped dependency modules in the renderer's existing `namespace rec` bundle at
  `groups/<namespace>.fs`. Configuration requires an explicit namespace and immediate
  child module names; compiler-library placement retains its own layout. The default
  keeps separate module files. This is an O7 placement choice: catalog identities,
  qualified names and source ownership retain their existing contracts. The
  `recursive-groups-lab` gates a two-package declaration cycle through the normal golden
  corpus and compile gate. Entry and ambient export blocks retain their declared entry
  owner; a dependency type sharing a public subpath's name cannot redirect those exports.

  *Settled (wave five lane W): the compiler-lib group will not ship, and O7's default stays
  `widen`.* Shipping `TypeScript.Lib` needs no code beyond lane S's group emission, and it
  does not terminate for a realistic entry package: one `EventTarget` reference resolves
  22,931 compiler-lib type entries to emit 6 declarations, and one `HTMLElement` exhausts a
  12.9 GB heap. The cost is resolution rather than emission — `ship` derives every member,
  base type and type argument transitively, while the shape tier declares 5 to 49 of the
  100-175 names that survive. The corpus as a whole names 61 distinct compiler-lib types.
  Two further blocks stand behind the cost. There is no canonical `TypeScript.Lib` for a
  `reference` to template against: `Date` alone yields 2, 5 or 49 declarations according to
  the `lib` setting, so the producer the contract assumes does not exist. And shipping the lib
  inflates the *consumer's* module, since anonymous lib shapes are entry-package by D6 — one
  `Date` under `esnext` took an entry module from 55 lines to 346. `reference` itself is free:
  442 ms against `widen`'s 443 ms on the same fixture. These numbers price `inline`, which
  has to resolve the 61 names the corpus references rather than the 20,000 a group walk
  reaches, and has to scope `Unclassified` shapes as well as named group members.

  *Updated (2026-09-06):* the later combined compiler-library producer supersedes the
  shipping restriction above. Its `Es` and `Dom` modules share `namespace rec TypeScript.Lib`.
  The clean `esnext`/`dom` profile compiles for `netstandard2.1` and `net8.0`; the retained split
  snapshots are outside the solution. The input pins and measurements were recorded in a
  packaging handover retired on 2026-09-22.

  Declaration placement now follows the defining export, with synthesized nested declarations
  following their parent. A secondary alias keeps its own export's owner. These distinctions
  prevent an application alias from renaming or relocating a shared compiler type. A complete
  program-source check permits reusable `GlobalThis` only when compiler libraries are accompanied
  by empty sources or empty exports. Application declarations and referenced augmentations keep
  that global object in the application. Ownership tests cover these cases; the independent ES5
  golden profile compiles with its consumer in a separate gate project.

  This establishes the clean producer and the tested ownership cases. An application-enriched
  DOM scope can still introduce a dependency from core to application; its reproduction remains
  in `compiler-lib-ownership-lab`. General augmentation handling and closure across arbitrary
  separately generated groups remain further work.

  *Settled (wave five lane T):* the compile gate takes closed configurations only, with the
  **corpus** as the unit of closure rather than the run. `cross-package-lab` and
  `cross-package-dep` are gated together, which makes the F# compiler rather than a string
  comparison the judge of whether a templated name is the shipped one. A golden whose
  templated module nothing ships carries the `.open.fs` suffix and stays out. Stub synthesis
  is refused: a stub written from the templated identity agrees with the template by
  construction, and both breaks above would compile clean against one.

Declaration catalogs (2026-09-06) add an opt-in stage between Shape and Render. Resolve retains
actual-symbol declaration handles and concrete alias arguments; the catalog records stable
package-relative handles, instantiated shape identities, source closure hashes, compiler/profile
keys, generator binary identity, package manifest hashes, and canonical F# APIs including method
constraints. Bound generic arguments are normalized by position, and source closure excludes
contextual argument bounds while retaining the declaration's own source. The stage rewrites `FsTypeRef` occurrences and
replaces repeated public declarations with aliases while retaining entry-specific value imports.
Catalog dependencies form an ordered owner DAG, including inherited producer entries. The
`declaration-identity-lab` producer/adapter/next consumer and catalog-conflict tests exercise the
contract, including class constructor values, nested package versions, and incompatible generic
specializations. Generic alias applications retain their declaration owner; transparent aliases
normalize to their underlying F# type for API comparison. Callable identities include union,
intersection, tuple and recursive references. Source closure retains declaration dependencies
and excludes unrelated export use sites. The regression suite checks each case through producer
and consumer compilation. Anonymous generic result members reuse their declaration only under
a complete substitution, preserving caller bounds and repeated or reordered arguments.
In catalog mode this includes original anonymous declarations retained by Resolve whose only
reachable occurrence is an application. The canonical declaration binds its original free
parameters; applied method parameters and their constraints remain at the method use site.
Declaration-less, alias-less intersections with a compiler-authenticated TypeLiteral constituent
retain their ordered constituent contracts and declaration handles independently of reachable
parents (2026-10-25). Constituents authenticate directly before parent-role inference; complete
applied contracts preserve generic arguments and distinct written literals preserve source
identity. Incomplete or recursive constituent evidence retains the existing fallback. Source
ownership retains the earlier canonical snapshot. The owner-intersection lab compares producer
and consumer identities with different reachable parents and compiles their shared member API.
Identity compatibility advances to 8; API and inference policy are unchanged by this correction.
Anonymous unions of actual source-declared interface/class arms also use directly authenticated
constituent contracts and normalized declaration handles before parent roles (2026-10-25).
Sorted arm identity keys make this a union-of-nominal-contracts equivalence: repeated anonymous
occurrences and arm order share identity, while independent nominal sources, named aliases and
applied arguments retain their distinctions. Every direct arm must have complete source and
contract evidence; primitive or unavailable/cyclic arms retain the earlier fallback. Existing
source and F# API authentication rejects stale sources and incompatible union representations.
This is part of identity compatibility 8 and is covered by the census owner-union lab.
Catalog bound naming preserves source object aliases reached only through declaration or
signature type parameters. Resolve authenticates the bound's TypeLiteral and alias declaration
handles and requires exact checker-declared type equality before recovering its source name.
Anonymous naming follows source-named constraints/defaults; inline structural constraints retain
their existing policy. The bound-alias naming lab compares producer/consumer APIs and compiles
valid and invalid consumers. Catalogue source, API and constraint guards remain unchanged.
Opaque dependency aliases retain their argument owners and applications in identity, including
the source dependencies of concrete defaults, even when member facts are intentionally widened.
Anonymous literal unions use their literal values for identity; named aliases and enum members
retain their declaration anchors. Nullable literal unions retain the compiler's nonnullable alias
relation, including its source dependencies, so optional properties reuse the correct named
type across entries. Remaining SDK API and constraint mismatches, and cross-profile
ownership, are still explicitly refused.

Rendering regressions from the SDK corpus (2026-09-06) preserve `_` identifiers and qualify
root type references shadowed by nested generated declarations. Bound parameters that collide
with FSharp.Core pattern constructors use distinct F# names (`GE005`), while object construction
preserves JavaScript property names and optional-field omission. The underscore, inherited-name
and pattern-parameter labs provide compiler checks; the four added runtime checks pass in the
327-check Fable gate. The affected Puppeteer, Actors, Sandbox, Sandbox Preview and RealtimeKit UI
root outputs compile; the refreshed dependency profiles and full subentry set still require
their own generation and acceptance checks.

Watch items rather than open questions: the debug assertion pass (O3) and the bespoke
JSON model dump (O5) are named escape hatches, built only when their triggering need
appears.

## Compiler-library artifact (2026-09-09)

The compiler library is now a single combined generated artifact rather than a family of
separately generated library inputs. One recursive source, `tools/fable-core-ts-input/entry.d.ts`,
is the intentionally narrow traversal root: it covers the compiler's selected `lib.*.d.ts`
surface without reintroducing the non-terminating per-library shipping walk that O7 rejected.
The source uses the constrained `compilerLib` layout configuration to place the result under the
public `Fable.Core.TS` root, with its `Es` and `Dom` child modules. Only ES is auto-opened;
`Fable.Core.TS.Dom` remains explicit to avoid making browser globals ambient in every consumer.

Regeneration is an explicit `build.fsx -- generate --only compiler-lib` stage, not a default
generated-layer stage, because this checked-in consumer artifact is large and should change only
when deliberately refreshed. The initial output is package version `0.1.0-alpha.1`, manifest
schema version 1, with tier counts exact 544, ergonomic 1824, widened 624, and escape 658. The
manifest is the fidelity record for the generated binding; those counts are not a claim that a
future TypeScript compiler pin will reproduce the same shape.

## DOM consumer bindings (2026-09-10)

Generated consumer files now open `Fable.Core.TS.Dom` and reference DOM declarations
from `Xantham.Fable.Core.TS`. The Browser reflection generator, lookup table, generated
Browser gate, package references, and generation command have been removed. This supersedes
the earlier Browser-family decision recorded above. Core.TS generation omits the consumer
open so the producer remains bootstrappable.

DOM references preserve the compiler's type arguments. Resolution retains target parameters
and base types without walking DOM members, allowing the existing constrained-argument repair
to handle widened indexed accesses such as `NodeListOf<obj>` and report the loss. The compile
gate checks the resulting references against Core.TS itself. Package-owned declarations keep
their identity, and explicitly shipped compiler libraries still use their configured modules.

The `ConstrainFunction` investigation found that its current SRTP helper cannot replace all
nominal `JS.Function` constraints: generic interfaces and abstract methods fail FS0670, while
an annotated inline function specializes to an F# function. The generator retains its existing
function constraints. Reproduction details are in `../probes/constrain-function/README.md`.

## ECMAScript consumer bindings (2026-10-13, W3 #134)

Under the default `widen` disposition of `typescript/lib`, a compiler-lib ECMAScript declaration
that `Naming.LibBindings` does not bind is written as `Fable.Core.TS.Es.{name}` with all of its
checker type arguments, as the DOM half above already is. The table still answers first
for its three remaining rows (below). Checker placeholders (`__type`) and `globalThis` keep the
existing widening. Namespace members retain their namespace path
(`Intl.NumberFormatOptions` → `Fable.Core.TS.Es.Intl.NumberFormatOptions`).

- **`widen` only.** A `map` disposition keeps its own table (group-map-lab maps `RegExp` to
  `System.Text.RegularExpressions.Regex`), and `reference` keeps the O7 template
  (`TypeScript.Lib.Es.RegExp`).
- **Nominal metadata.** Resolve reads the target parameters and base types of these declarations
  (not their members), as it does for DOM, so the constrained-argument repair applies to
  `Fable.Core.TS.Es` references. Names the table binds are excluded from that read.
- **No catalog contract bump.** Ship producers (Core.TS and every catalog lab) emit an unchanged
  ABI, and a consumer under `widen` loads no catalogue. Bumping `CatalogCompatibility.current`
  would reject Core.TS's committed catalogue for no change in its content.
- **Why not catalog reuse.** Node lists no `declarationReferences`, and a catalogue is accepted
  only under an equal inference profile. Making Node a catalogue consumer means switching it to
  `lib: [esnext, dom]` with `typescript/lib: ship`, which changes its compiler program and is a
  product decision (`docs/.ai/footguns.md`, "Declaration catalogs").
- **lib-lab.** `each(values: Iterable<string>)` was pinned as `obj`. It is now
  `Fable.Core.TS.Es.Iterable<string, obj, obj>`, Core.TS's JS-iterable interface and not
  `seq<'T>`, so the reason for that pin still holds.
- **Known gaps.** The `any` defaults of `Iterable`/`IterableIterator` (`TReturn`, `TNext`) render
  as `obj` and raise TR008, graded escape until W1. A name Core.TS renamed on collision
  (`Iterator2`) is reachable only under the name it kept; the compile gate cannot detect a
  same-arity wrong target.

Measured on 2026-10-13:

| Run | TR023 | TR035 | TR008 | Tiers (exact / ergonomic / widened / escape) |
| --- | --- | --- | --- | --- |
| es-reuse-lab (new) | 0 | 0 | 2 | 8 / 0 / 0 / 1 |
| lib-lab | 1 → 0 | 0 | 0 → 2 | widened 2 → 1, escape 0 → 1 |
| `@cloudflare/workers-types` | 37 → 9 | 11 → 6 | 168 → 218 | 412 / 1067 → 1068 / 374 → 361 / 111 → 123 |
| `solid-js` | 6 → 0 | 35 → 35 | 78 → 78 | 69 / 146 → 147 / 85 → 84 / 42 |
| `Xantham.Fable.Node` | 737 → 148 | 1725 → 1270 | 2668 → 2735 | 1247 → 1263 / 1772 → 1858 / 768 → 656 / 345 → 353 |

In workers-types, TP006 5 → 3 and SP004 37 → 39: `SqlStorageCursor.raw<U>` and
`SyncKvStorage.list<T>` now return `IterableIterator<'U>`, so the type parameter is used rather
than dropped. Its remaining TR023 are `Module` (8) and `Env` (1), outside the compiler lib. Node's
remaining TR023 come from undici and web-global names (`Headers`, `FormData`, `MessageEvent`,
`SpecIterableIterator`) and module specifiers.

Node was regenerated under the T4 override: the selected goldens hold 43 of W3's ~780 TR023 hits
and none of `SharedArrayBuffer`, `ArrayLike`, `ArrayIterator`, `AsyncGeneratorFunction` or
`RegExp`; Node holds the rest. Node's "before" figures come from a regeneration at the parent
commit, not the committed 2026-10-06 output, which differs by 60 `symbols.jsonl` lines of
earlier generator drift.

### Fable.Core `JS.*` bindings retired (2026-10-13, W3 #134)

Generated bindings no longer depend on Fable.Core's `JS.*` bindings for standard-library types.
`Naming.LibBindings` keeps three rows: `Error` → `exn` (Fable compiles `System.Exception` to a
JavaScript `Error`), and `Promise`/`PromiseLike` → `JS.Promise<'T>` (lead decision: Fable.Promise's
`promise { }` and `Async.AwaitPromise`/`StartAsPromise` are written against `JS.Promise`;
`PromiseLike` keeps its TR025 loss so a `JS.Promise` passes where one is expected). Every other
former row (`Map`, `ReadonlyMap`, `WeakMap`, `Set`, `ReadonlySet`, `WeakSet`, `Date`, `Function`,
`Object`, `Math`, `JSON`, `Console`, `PropertyDescriptor`, `ArrayBuffer`, `ArrayBufferView`,
`DataView`, the typed arrays, `AsyncIterable`, `AsyncIterator`, `AsyncGenerator`,
`IteratorResult`, `AsyncIterableIterator`) is written as its `Fable.Core.TS.Es` declaration with
all of its checker arguments (`Uint8Array<'TArrayBuffer>`), so `U2<Es.ArrayBuffer,
Es.SharedArrayBuffer>` is Core.TS's `ArrayBufferLike`.

- **Premise.** Fable 5.17.2 special-cases the `Fable.Core.JS.` namespace as a whole (global
  resolution, no replacement, unmangled members) and `JS.Map`/`JS.Set` only for TypeScript-output
  annotations; no `JS.*` type gets an F#-visible conversion (`docs/.ai/footguns.md`, "Fable
  consumption of bindings").
- **Every non-ship disposition.** The former rows were consulted under `reference`, `map` and
  `widen` alike, so `LibBindings.isPinnedToCoreTs` keeps them bound under all three, now to
  Core.TS, ahead of the O7 template and a `map` table, as DOM names already are. Every generated
  consumer file opens `Fable.Core.TS.Dom`, so no non-ship disposition runs without Core.TS.
  Other ECMAScript names still follow their disposition (`widen` → Core.TS, `reference` →
  `TypeScript.Lib.Es.*`, `map` → table or widen).
- **TP010.** The dropped `Function` bound is matched as `Fable.Core.TS.Es.Function`
  (`LibBindings.FunctionBinding`); the message no longer names `JS.Function`.
- **Ship mode.** A `ship` run binds `Promise` and `PromiseLike` too (`LibBindings.bindsWhenShipped`,
  checked in `Spec.typeRefOnPath` ahead of the run's own declaration), so Core.TS's members return
  `JS.Promise` and unify with consumers and Fable.Promise; `PromiseLike` records TR025 there as
  well. Core.TS still declares `Es.Promise`, `Es.PromiseLike` and `PromiseConstructor`: the
  declarations keep their catalogue entries (the `core-ts package` test asserts `Es.Promise` at
  arity 1), and the `Promise` global's `resolve`/`all`/`race`/`reject` now return `JS.Promise`.
  Only a declaration's own `Create` still returns it. The emitted ABI of a shipped compiler lib
  changed, so `CatalogCompatibility.current` ApiVersion is 6 → 7 and Core.TS's catalogue is
  regenerated. compiler-lib-ownership-lab (ES5, `ship`) is the lab.

Measured on 2026-10-13 (es-reuse-lab is the lab, extended with `Promise`, `PromiseLike`, `Map`,
`Set`, `Date`, `ArrayBuffer`, `Uint8Array<ArrayBuffer>` and `Error`):

| Run | TR024 | TR025 | TR008 | Tiers (exact / ergonomic / widened / escape) |
| --- | --- | --- | --- | --- |
| es-reuse-lab | 0 | 0 → 2 (new `PromiseLike`, `Error` sites) | 2 | 8 / 1 / 0 / 0 → 11 / 3 / 0 / 0 |
| lib-lab | 7 → 0 | 3 → 1 | 2 → 6 | 12 / 8 / 1 / 0 → 15 / 5 / 1 / 0 |
| `@cloudflare/workers-types` | 120 → 0 | 10 → 8 | 218 → 222 | 412 / 1161 / 390 / 1 → 419 / 1154 / 390 / 1 |
| `solid-js` | 0 | 2 | 78 | unchanged (69 / 180 / 92 / 1); 26 lines respelled |
| `Xantham.Fable.Node` | 255 → 0 | 1434 → 1431 | 2877 → 2895 | 1263 / 2032 / 830 / 5 → 1270 / 2025 / 830 / 5 |
| `Xantham.Fable.Core.TS` (ship) | 0 | 0 → 51 | 1681 | unchanged (2073 / 2203 / 785 / 5); 483 `JS.Promise` references |

TR023 and TR035 are unchanged in every row. The TR008 rise is the `any` defaults of the
async-iteration and `IteratorResult` declarations, now applied at their real arity rather than
truncated away. Node's residual `JS.*` spellings are `JS.Promise` (1006) and the support package's
`JS.Record` (30); it held 1469 `JS.ArrayBuffer`, 195 `JS.Function` and 112 `JS.AsyncIterable`
before.

## Generator identity measures (2026-09-11)

The generator keeps distinct measures for compiler symbol, type and node IDs, and for
opaque strings carried through its records: source symbol names, declaration handles,
declaration files, package names, directory paths and JavaScript import specifiers.
Import specifiers are distinct from npm package identities because they also include
ambient modules and public subpaths. Wire protocol records and JSON text remain boundary
representations; the generator raises values when constructing its internal model.

Raise with `value * uom<role>` and lower with `value / uom<role>`. Composite measures
can lose only the relevant dimension: a declaration file becomes a file path through
`file / uom<node>`. The operators support both strings and integer IDs and preserve the
underlying value. The former tag/untag/retag helpers are removed.

Measures describe a value's role, not a prohibition on transforming it. Lower once
inside a naming, path-normalization or rendering boundary. Generated F# names, source
fragments, diagnostic messages and joined header text remain ordinary strings; do not
raise prose merely to satisfy an identity-bearing parameter. This changes internal F#
signatures, not the generated binding or manifest format.

## Export companion modules (2026-09-11)

Ordering allows public export containers to nest under companion modules of existing
types. In workers-types, `type Cloudflare` therefore coexists with one `module Cloudflare`
containing `Email.Exports`, `Workers.Exports`, and `Workflows.Exports`. Reserving every
type name as a module path had unnecessarily allocated a separate hashed parent for
each ambient owner. Type names and identities stay unchanged; normalized owner-path
collisions and `Exports` type-leaf reservations still use the existing allocator.

Verification: the installed workers-types regression failed before the change and
passes afterward; all nine path-allocation tests pass. FCS probes accept companion
modules for interfaces, aliases, measures, unions, and delegates inside a recursive
root module. Regenerated workers-types with `lib: ["esnext"]` compiles against Core
and Core.TS with zero errors and 98 identifier warnings; the committed CompileGate
also builds with zero errors (one FSharp.Core version warning). This is a focused
layout correction; the remaining export-collision and
runtime acceptance tasks in the export-module plan are still pending.

## Export collision resolution (2026-09-12)

Source-identical function overloads retain TypeScript's first applicable return before this
pass (2026-10-25, `census-source-overloads-lab`). `shape-exports` compares compiler parameter
type identities and optionality on nongeneric, non-rest function declarations with source facts
and no explicit receiver. A later identical input signature is unreachable under TypeScript's
source-order overload selection, so it contributes no return alternative. Resolve records
`HasThisParameter` from the compiler signature; Shape remains pure. Filtering retains each
surviving member's original expanded `SignatureOrdinal`, including gaps, for provenance.
Distinct TypeScript inputs that merely compile to the same F# parameter types keep the late
collision policy below. Generic, rest, receiver-bearing and declarationless signatures keep
that policy as well.

The late `resolve-export-collisions` pass keeps every candidate an owner's value exports
produce. Under one exported name: repeated occurrences of one declaration consolidate
(`DO006`, Exact); different declarations mapping to one F# signature, return included,
consolidate (`DO009`, Ergonomic); candidates with one compiled parameter signature and
different returns become one member returning the erased union of every return, in harvest
order (`DO008`, Widened); an ambiguous call form or a member-kind conflict takes a numbered
`<name>_OverloadN` name that skips every original member and accessor (`DO007`, Ergonomic).
`DO004` stays defined and unused. The dispatch brief's original `pick_Overload2` contract
was corrected to the union: two F# names for one JavaScript function with identical
parameters give the call site nothing to choose on.

Export functions share the interface members' literal-overload retention: a literal
parameter that separates an exported overload set keeps its single-case `StringEnum`
type under the container (`Exports.Left`), and a signature returning one of those literals
keeps it at the return position too.

Findings raised for an export member carry its container-qualified name
(`Exports.check()`, `Strict.Exports.check(value)`), identical to its manifest row, so the
row's tier follows its own findings. The `entry`, `global` and `ambient:<specifier>`
pseudo-symbols are retired, which is why every manifest's `exact` count fell by the
number of pseudo-rows it carried. Container names are allocated once
(`ExportLayout.containersFor`) and read by shaping, literal retention and ordering alike;
`ShapeModel.RuntimePackage` carries the specifier that allocation needs.

Migration impact: consumers of a return-only overload pair now call one member and match
on `U2`/`U3` rather than choosing a member; `dispatch`-style literal overloads gain typed
arguments; the `layout-lab` fixture merged into `export-layout-lab` with a runtime and run
gate checks. Known consequence: a constructor's own call signature and the one it inherits
(`EvalErrorConstructor` over `ErrorConstructor`) now union to `U2<EvalError, Error>` where
the old pass dropped the inherited one.

Verification: 689 Expecto tests, the solution compile gate over every golden, and the run
gate's 443 checks pass. Findings moved `DO004` 8 -> 0, `DO008` 0 -> 8, `DO007` 0 -> 1
(`error-class-lab` `Mishap(?message)` beside `Mishap(?message, ?options)`), `TR056` +4.

## 0.1.0 release wave (2026-09-12)

Single-case string enums render without `RequireQualifiedAccess` (#75). The attribute stays
when the single case is a reserved F# name (`Ok`, `Error`, `Some`, `None`, `ValueSome`,
`ValueNone`; `Shape.Spec.reservedCaseNames`), recorded as `LU002` (Ergonomic) at the mint site
in `dedupe-overloads`, the only pass that mints one. Consumer impact: a
single-case enum is written `Exports.Left`, and the previous `Exports.Left.Left` form is a
compile error (FS0812). Multi-case enums and tagged unions are unchanged.

`autoOpenExports` (#74), a `xantham.json` boolean, default `false`, places `[<AutoOpen>]`
before `[<Erase>]` on every generated `Exports` type, root and nested. With the flag off the
output is byte-identical to before.

A class reachable through an `export * from` alias path (`declare module "node:crypto"
{ export * from "crypto" }`) emits each static member once (#73). Static occurrences group by
declaration identity; the kept occurrence uses the specifier the class's
own binding uses, or the first harvested path when the class carries none; each dropped
occurrence records `SC010` (Exact) with its specifier on `Class.member`. The type keeps its
single declaration site. Two distinct same-named classes in different ambient modules are not
this case and remain broken by `SA.AbbreviationNameTaken` (recorded, out of scope).

Manifest `file` paths for compiler-library declarations read `node_modules/typescript/lib/...`
on every platform (#67). The declaration-catalog suite runs behind the `Tsc.locate` guard for
its two nullable-alias lists; four lists stay disabled on one defect, `Exports has no stable
declaration or parent role`, 22/38 cases (#66). `build.fsx -- test` runs the Expecto
executables directly, and the nested consumer builds in the catalog and export-provenance
suites carry the repository `global.json`.

Verification: 711 Expecto tests, the compile gate, and the run gate pass. Findings moved
`LU002` 0 -> 2 (`single-case-enum-lab`), `SC010` 0 -> 1 (`static-reexport-lab`); four
goldens moved by the attribute line only. Regenerating `@types/node` compiles with zero
errors (previously 50x FS0438): 1677 exact / 1924 ergonomic / 867 widened / 356 escape,
`SC010` x1495, 289,053 lines. The Node project stays out of the solution (#71 open).

## Public subpaths and shallowest-path type homes (2026-09-12)

One run enumerates every `./` key of the package.json `exports` map (`Bootstrap.publicPaths`)
and harvests each as an owner, `FromAmbientModule "<runtime>/<key>"`, so the export-layout
allocator renders `pkg/client` as `Client` with its own `Exports`. A key with `*` records
`HG008`; a key naming a non-TypeScript target is skipped silently, and a key naming a missing
or absent declaration file records `HG009`; a key resolving outside the package fails the run.
`subpaths` in `xantham.json` restricts the set; `entry` disables enumeration.

Type declarations home under the shallowest owner path among the exports sharing the
declared type, ties broken by ordinal owner specifier, then harvest order
(`ExportNames.declarationExports`). The cross-shipped-group preference still applies first.
A deeper owner's export of the same type emits nothing (`Aliases`). Ambient modules follow
the same rule. Values are unchanged: one `Exports` per owner.

Catalog rows already carried dotted nested names since the specifier-module change, so
`declarations.json` schema stays at version 1.

Verification: 730 + 90 Expecto tests, compile gate, run gate. Findings moved:
`HG008` 0 -> 1, `HG009` 0 -> 1 (`subpath-lab`); `RA007` 0 -> 1 (`solid-js`, `Store.SetStoreFunction`);
`SA001` 8 -> 8 and `NE001` 0 -> 0, unchanged. Goldens moved: `type-fest` (+`Globals` module for
`./globals`), `animejs` (new subpath modules and imports), `solid-js` (new subpath modules,
`JSX` moving to the root, and a duplicate `DOMElement` abbreviation dropping), `@cloudflare/workers-types`
(three ambient-module types moving to the root), `static-reexport-lab` (a root abbreviation
dropping), `subpath-lab` (new).

## Catalog and mixed public-input follow-up (2026-09-13)

The follow-up on 2026-09-13 keeps global declarations when a selected public input is a global
script alongside module inputs. `harvest-globals` reads the global script's scope and merges
its declarations and namespaces with the public module exports. Module-only runs retain the
existing policy for incidental global augmentations. `mixed-subpaths-lab` checks both entry
orders and executes the global and imported functions through Fable.

Catalog ownership covers reusable type declarations. `FsExports` containers remain local to
each generation, with their signature references redirected to catalog owners. Public aliases
and class constructor helpers use the qualified declaration names selected by `name-exports`;
constructor selection uses the source symbol and export owner. The restored catalog suites
create disposable fixtures under the test project's `obj/catalog-fixtures`, within compiler
and SDK discovery scope. Consumer probes cover nested type aliases, local subpath imports,
and constructors and static members on a reused nested class.

`catalog-subpath-lab` covers anonymous function-result declarations under a public subpath
and two names exported for the same interface. Catalog parent-role lookup uses the export's
qualified module path, including the Pascal-cased function parent of generated result types.
`shape-aliases` places secondary names under that same export path. The consumer gate reuses
the result union through a second catalog owner and type-checks the nested interface alias.

Validation: 811 generator and 90 Wire tests pass in both the regeneration and check phases;
the compile gate and 455 Fable runtime checks pass. Workers' `SA003` count falls by one as
the root retention alias and the three module-scoped Workflow aliases retain separate names.
Workers' tiers change from 410/1068/369/111 to 412/1068/368/111 (exact/ergonomic/widened/escape).

## Augmented libraries and exclusive factories (2026-09-13)

`resolve-type-table` follows non-compiler declarations when a standard-library symbol is
augmented, preferring entry sources and then deterministic dependency source order. Local
augmentations retain their generic parameters and members; dependency augmentations use the
configured group disposition. Function values resolve their callable signatures by content.
`augmented-dom-lab` and its compiled consumer cover generic Request/RequestInit augmentations,
`typeof fetch`, and unchanged mapping of an ordinary DOM Response.

The exclusive-arm fold preserves separate union arms when the proposed factories have
identical parameter types and optionality. F# parameter names do not distinguish overload
signatures. `exclusive-signature-lab` exercises the URL/HTML case and calls each arm's factory
through Fable to verify the emitted property.

Validation: 816 generator tests and 90 Wire tests pass in both phases, with the compile gate
and 457 Fable checks passing. The Workers golden adds its merged `Disposable` declaration
and explicit `HyperdriveDynamic` inheritance. Its symbol-keyed dispose member remains a
reported mapping loss (`MB.SymbolKeyedMemberDropped` +1); base flattening becomes inheritance.
The existing Agents browser entry compiles in the external corpus probe. Shipping its Workers
dependency separately still exposes a group-to-root reference cycle and remains an open
library-stratification issue.

## Explicit public-input partitions (2026-09-13)

`GeneratorConfig.PublicInputs` maps concrete public export keys to declaration files within
one package. Bootstrap uses exactly that map as its compiler roots, with `.` included only
when selected. This supports caller-expanded wildcard exports, selected conditional variants,
and independent browser/Workers programs without changing automatic export enumeration.
`entry` and `subpaths` are mutually exclusive with this mode. Empty maps, duplicate JSON keys,
non-concrete import keys and invalid declaration paths fail before generation.

The public-inputs lab selects two runtime exports sharing a declaration file while the
manifest root is unavailable. Fable checks both runtime imports with the same generated Card
type. Config and bootstrap checks cover selection conflicts and invalid inputs. FCS impact
analysis before adding the field found 92 uses in nine generator files; CLI/schema and consumer
compatibility are also checked by the build and regression gates.

Validation: 843 generator tests, 90 Wire tests, the compile gate and 459 Fable runtime
checks pass. Existing golden bindings and finding counts are unchanged; the new lab has
2 exact and 1 ergonomic symbols. Catalog consumer builds now disable shared build servers
and use one MSBuild worker after two nested build workers exited during an earlier run.

### Contextual constructor bounds and unresolved declaration arguments

Constructor identities include the constraints on their declaration and captured type
parameters. An exported `Factory<T extends Base>` and a use under `T extends Derived` receive
distinct identities; both remain reusable through a producer catalog. The four-line
catalog-constructor-bounds lab reproduces the previous collision.

An unresolved declaration argument contributes a scalar key only for supported intrinsic or
literal flags. Enum, union and other unexpanded arguments require a resolved identity or a
parent role. Their flags alone do not identify a type. The readonly-enums lab imports two
dependency enums and applies `Readonly` to each; their generated enum types remain distinct
and reusable through a catalog.

Catalog compile consumers use the test assembly's configuration and disable dependency
rebuilds. The test project builds Core.TS as a prerequisite. This keeps parallel consumer
builds from rewriting a shared reference assembly while another compiler reads it.

Validation: 849 generator tests and 90 Wire tests pass during regeneration and independent
checking; the compile gate and 459 Fable runtime checks pass. Existing golden bindings and
finding counts are unchanged. Constructor bounds: 1 exact / 4 ergonomic. Readonly enums:
2 exact / 2 ergonomic. Both labs have zero widened and escape symbols.

### Public SDK classes and ambient runtime entrypoints

Entrypoint classification compares an export's import specifier against the exact selected
public-input map. A package subpath retains the same class representation as its root.
Genuine ambient runtime modules still receive the entrypoint class and optional-hook rules.
The distinction applies consistently in interface, anonymous-type and class shaping.

The subpath-classes lab has a two-class Error hierarchy under a public subpath. It previously
emitted an interface inheriting an imported abstract class (FS0887). The corrected output
compiles, and Fable constructs the real JavaScript subclass and reads its own and inherited
members. Existing binding text is unchanged. Anime.js loses three inappropriate SC008
entrypoint-refusal findings (JSAnimation, Timeline, Timer); symbol tiers are unchanged.

Validation: 851 generator tests, 90 Wire tests, the compile gate and 461 Fable runtime
checks pass. A fresh FCS check reports no errors or warnings.


### Installed package ownership and nested module manifests

Catalog sources belong to their installed package root, including scoped and nested
`node_modules` installations. A module manifest inside that package may declare its own
resolution settings without declaring an independent package version. Source fingerprints
include both the owner manifest and every intervening module manifest, with paths relative
to the owner. An installed owner still requires its name and version.

The package-submanifest lab reproduces a dependency subpath with a named, unversioned module
manifest. Catalog checks cover scoped installations, nested dependencies, producer reuse,
stale nested metadata rejection and rejection of an unversioned package owner. Existing
golden bindings and finding counts are unchanged; the new lab retains one reported opaque
dependency alias. Further Firebase integration validation is deferred from the first-party
Cloudflare delivery; its general package-ownership defect is covered by this regression.

Validation: 854 generator tests, 90 Wire tests, the compile gate and 461 Fable runtime
checks pass in the full regeneration and independent-check pipeline.


### Parent identities through index signatures

Catalog parent roles include index-signature keys and values, using the checker-provided
index order. Synthesized callbacks reached through a mapped index signature receive a
stable role through that index and any intervening union. Catalog source closure already
traversed these dependencies; identity propagation now follows the same index edges.

The three-line indexed-callback lab reduces the Containers SDK failure to a mapped handler
collection. A second generated library reuses its catalog, and a compiled consumer reads
and invokes the producer's callback type. Its conditional context remains a reported
mapping loss (0 exact / 1 ergonomic / 3 widened / 0 escape). Existing golden bindings and
finding counts are unchanged.

Validation: 857 generator tests and 90 Wire tests pass during regeneration and independent
checking; the compile gate and 461 Fable runtime checks pass. A fresh FCS check is clean.

### Intrinsic arguments and named primitive aliases

Structural declaration identities normalize intrinsic argument types by their flags and
literal values before consulting declaration handles. Named primitive aliases retain their
own catalog declarations. Enum literals retain declaration identity and are excluded from
the intrinsic fallback.

The primitive-argument-identity lab has an ambient dependency with named string, number and
boolean aliases and an interface containing an anonymous nested object. A consumer reaches
the interface without reaching those aliases. The producer and consumer must agree on the
nested object's identity and preserve the same typed API. The regression compiles a consumer
that reads each nested member and uses the producer's primitive aliases.

The failure originates in the checker's interned primitive types: an exported alias can add
declaration handles to the same primitive type ID used by an unrelated structural member.
Intrinsic arguments share canonical keys across named alias owners. The intrinsic mask
excludes unique symbols, template literals and string mappings, whose semantic identities
require more than flags and a literal value.

Existing golden bindings and finding counts are unchanged. The new lab has 1 exact /
2 ergonomic / 0 widened / 1 escape symbol; the escape is GE004's dependency-module naming
provenance, and every nested member retains its primitive type.

Validation: 860 generator tests and 90 Wire tests pass during regeneration and independent
checking; the compile gate and 461 Fable runtime checks pass. A fresh FCS check is clean.

### Compiler-library literal aliases used by shipped groups

O7 placement assigns an unexported compiler-library literal alias to its unique shipped
dependency consumer when the compiler library itself is not shipped. The finished F#
declaration references determine that consumer. Root declarations reuse the same alias
from the dependency module. Explicit exports, shipped compiler-library declarations and
aliases consumed by several dependency groups retain their existing ownership.

The grouped-dom-aliases lab contains an exported function over `Request` and one ambient
dependency adding `Request.cf`. Catalog generation recovers named DOM literal aliases,
including `RequestCache`, which previously remained in the root file while the merged
interface belonged to the shipped dependency. That produced references in both file
directions and FS0039 when compiling groups first. A catalog producer/adapter consumer
passes the dependency-owned Request through both exports and reads its typed cache value.
The ordinary golden and compile consumer also retain the augmentation's string property.
This change addresses compiler-library literal-alias placement, not arbitrary package
cycles or compatibility across inference profiles.

Validation: 866 generator tests, 90 Wire tests, the compile gate and 461 Fable runtime
checks pass. Existing golden bindings and finding counts are unchanged. The new lab and
the before/after catalog probe each retain 7 exact / 0 ergonomic / 0 widened / 2 escape
symbols; only the catalog probe's literal-alias module ownership changes.

Combined validation with intrinsic-argument normalization: 869 generator tests, 90 Wire
tests, the compile gate and 461 Fable runtime checks pass. Existing golden bindings and
finding counts remain unchanged; the new labs retain their independently measured counts.

### Canonical declaration sources for transparent aliases

A declaration's source closure follows the sources selected for each canonical type
identity. Transparent alias applications retain their source fingerprints in catalog inputs;
they contribute declaration sources when the alias itself defines the identity. Intrinsic
arguments share their provenance across named primitive aliases.

The record-alias-source-identity lab gives separate packages independent aliases of
`Record<string, unknown>` and `Record<string, Model>`. The aliases share their applied
canonical declarations while preserving public F# names and typed consumer access. The
model's declaration remains part of its record's source closure. Changes to a reachable
alias input or to the applied model still invalidate the producer catalog through the
existing source authentication checks.

Validation: 872 generator tests and 90 Wire tests pass in both regeneration and check
phases, the compile gate passes, and 461 Fable runtime checks pass. All 95 pre-existing
golden directories, including their finding counts, remain byte-identical. The new lab
has two widened declarations: unknown values use `obj`, and the separately owned model
is opaque when the root fixture is generated alone. Fresh generator FCS checking reports
zero errors and warnings.

### Nullable aliases shared across catalog owners

Resolve recovers the canonical declaration behind a nullable nongeneric object union in
catalog mode, including a tagged union reached only through an optional member. Generic
alias applications retain their argument handling. Shape references to a named nullable
heterogeneous alias preserve the option layer already carried by that alias. Literal unions
still use a nullable reference to their generated enum, whose cases carry only the values.

The catalog-alias-api lab exposes a nullable value alias, a tagged payload union and an
optional tagged choice through a dependency. A consumer reaches the option object through
`Parameters<Model["generate"]>[0]`. Its typed F# consumer passes producer-owned options and
payloads directly to the consumer SDK and reads a JSON payload with exactly one option layer.
The catalog API, source, arity and constraint guards remain unchanged.

Validation and measured golden changes are recorded in
`docs/.ai/footguns.md`.
### Source closure precedes anonymous parent ownership

Source closure consults the canonical type identities before parent-role inference
assigns names to anonymous checker types. The checker shares types such as
`string[] | undefined` across independent properties. A chosen parent gives that
anonymous type an output identity, but cannot make the parent's package a dependency
of every other occurrence of the type. The canonical identity map is retained
separately for closure; the completed map still supplies output declaration identities
and their own parent sources.

The anonymous-parent-source lab gives shared and consumer interfaces independent
optional string-array properties. A typed consumer passes the shared interface across
the catalog boundary and reads its optional strings. A changed shared input still
invalidates the catalog. This also reproduces the Workers CacheContext closure gaining
Containers' `deniedHosts` parent through CachePurgeOptions.

Validation: 875 generator tests and 90 Wire tests pass in both phases, the compile
gate passes, and 461 Fable checks pass. All 97 existing golden trees are byte-identical.
The new lab has one exact and two ergonomic symbols, with no widening or escape.
Fresh FCS checking reports zero errors and warnings.


### Recursive alias boundaries and NonNullable payloads

Catalog-mode Resolve records a nongeneric union alias's source name only when the checker
reports the same declared type for its alias symbol. A transformed `NonNullable<T>` therefore
keeps its own nullability instead of borrowing the name of nullable `T`. Shape names shipped
canonical union aliases even when they are reachable only through other declarations.
Recursive aliases then reach the same named boundary in producer and consumer programs.

Union member-set matching reuses a heterogeneous alias only when the candidate is
non-nullable. Nullable literal unions remain reusable through their value-only enums.
This preserves the difference between `T` and `NonNullable<T>` while keeping enum reuse.
The existing widening at an unnamed recursive alias cycle remains reported as TR001.

The catalog-recursive-json lab contains `Value = string | number | null | Value[]` and a
tagged payload whose result is `NonNullable<Value>`. A separately generated consumer imports
only the payload. The shared payload has no outer option, while its recursive array values
retain the canonical nullable Value. A typed F# consumer constructs and reuses both forms.
Actual pinned AI SDK diagnostic generation/compilation and the full regression measurements
are recorded in `docs/.ai/footguns.md`.

### Inline literal unions across catalog programs

Shape does not promote an inline literal union to an unrelated named alias by matching
its member values when producing or consuming a declaration catalog. The named alias
may be reachable only in the producer program; borrowing its name would change the
shared declaration's F# API in the consumer. Inline unions instead retain their own
structural enum identity. Explicit alias references and recovered non-nullable alias
wrappers keep their named owners, source closures, and distinct declaration identities.
Nonliteral unions retain the existing member-set matching behavior.

The literal-alias-identity lab places an inline optional `"a" | "b"` property beside
named aliases in both literal orders and a nominal TypeScript enum. A separate consumer
uses the inline values in reverse order. Its typed F# consumer shares the producer's
property enum; the two named aliases and the nominal enum keep independent catalog
identities, and a changed dependency input still invalidates the catalog. Catalog
source, API, arity, and constraint checks remain unchanged. Measurements and rejected
alternatives are recorded in `docs/.ai/footguns.md`.

### Erased unions containing emitted object aliases

The late `normalize-obj-unions` Shape pass applies the existing TR035 rule after local
abbreviations and generic phantoms have been emitted. A union containing a nongeneric
local alias whose target resolves to `obj` becomes `obj`, just like a direct object arm.
Alias traversal stops at cycles and does not expand external references, named interface
contracts, or generic phantoms. Option and container wrappers retain their structure.
The existing declaration-reference traversal keeps properties, constructor helpers,
aliases, and export signatures consistent before catalog API authentication.

The empty-union-alias lab reproduces a producer exporting an empty interface and a
consumer reaching it only through `Value.event`. The producer formerly retained an
erased union over its named `obj` alias while the consumer widened a direct object arm.
The typed catalog regression checks shared values and setters, alias chains, retained
callable/indexed/inherited/generic contracts, the TR035 finding, and stale-input rejection.
Measurements are recorded in `docs/.ai/footguns.md`.

### Generic empty declarations reached through dependencies

Anonymous naming retains a shipped, explicitly named generic empty declaration. The
generic target is visited before its applications, so existing alias shaping emits its
phantom once and references retain their type arguments. Empty nongeneric objects keep
their existing object representation; external group disposition and constructor,
callback, array, and index-signature rules remain in force.

The generic-marker-catalog lab passes Options<string> and Marker<string> across a
producer/consumer catalog boundary, rejects Marker<int> at the string marker setter,
and retains stale-input rejection. It reproduces the Workers FacetStartupOptions class
property losing DurableObjectClass<T> in a consumer program. The producer's phantom
contract remains unchanged; no catalog compatibility guard is relaxed. Full measurements
are recorded in `docs/.ai/footguns.md`.

## Generator review fixes (2026-09-24)

- **Overload keys are structural.** `dedupe-overloads` keys a signature by the
  `signatureKey` tuple itself. The earlier `.ToString()` key printed three list elements and
  an ellipsis, so overloads that differ past their third parameter collapsed as `DO001`.
  `overload-arity-lab` pins the separation for methods and `Invoke`, and the negative (a
  fourth parameter that widens to one F# type). `solid-js`: `Store.SetStoreFunction.Invoke`
  regains 4 overloads, `DO001` 4 -> 0. `animejs`: `Utils.mapRange` regains its 4-parameter
  overload, `DO001` 16 -> 15.
- **Catalog hashes print canonically.** The constraint and API fields of a declaration
  catalog hash a canonical printer (`DeclarationCatalog.Canonical`): every case and list
  element is printed at any length. `sprintf "%A"` stopped at 100 list elements, so two
  interfaces differing in their 101st member hashed alike. The hash format changed with the
  generator assembly, which already invalidates every earlier catalog.
- **Union-arm and export collisions are call-level.** `expand-union-arms` declines a member
  (`UA004`) and `resolve-export-collisions` renames one (`AmbiguousCall`) when two signatures
  share a compiled parameter signature, or when one opens the other with an omissible tail and
  some call leaves an optional unsupplied in both, or in neither
  (`CompiledSignature.ambiguousCall`). An optional parameter compares by its rendered `?name: T`
  type. `(x)` beside `(x, ?y)` expands and keeps its name, since F# prefers the candidate with no
  unsupplied optional; `(x, ?y)` beside `(x, ?y, ?z)` declines, and `(u, ?q, ?s)` beside
  `(u, q, ?s)` renames the second. A later candidate whose return alone differs from a renamed
  member merges into it (`DO008`), so `@types/node`'s four `url.parse` overloads emit
  `parse(u)`, `parse(u, ?q, ?s)` and `parse_Overload2(u, q, ?s)`. `dedupe-overloads` keys on
  `CompiledSignature.parameterKey`, generic arity included, so `M<'A>(x)` and `M(x)` both
  survive.
- **Findings carry no checker ids.** `RT002` reports under `<type-table>`, `TR003` has no
  payload, and `TP001` carries the type parameter's position. Checker ids are assigned in
  answer order, so a manifest quoting one differed run to run.
- **`$` in declaration and type-parameter names.** An exported type whose name spells a
  character FS0883 rejects under backticks declares as its identifier shape (`$Shape` ->
  `Shape`), yielding to a verbatim name at the same path it collides with (`$Taken` ->
  `Taken2`, in either harvest order). A type variable writes each non-identifier character as
  `_` (`$T` -> `'_T`, a lone `$` -> `'_T`), suffixed where it collides with a variable in scope
  (`<$T, _T>` -> `<'_T2, '_T>`). Both report `SY005` with the written name; imports keep the
  JavaScript name. `dollar-name-lab` pins each position.

## Nullable catalogue identities (2026-10-08)

Nullable structural catalogue identities retain the compiler-recovered non-nullable alias
selected by shaping. Equal literal values remain distinct when their F# references name
different aliases. `catalog-mapped-options-lab` covers named versus anonymous locale options
through `Pick`; the catalogue compatibility guard continues to reject conflicting APIs.

## Portable catalogue compatibility (2026-10-09)

Declaration catalogue emission uses schema 2 with an internal compatibility sidecar; the public
Catalog and Context records retain their shapes. Contract, identity, API, inference and
customization versions begin at 1 and compare exactly. The selected compiler's verified
TypeScript release, full gitHead and AST protocol authenticate package toolchains across
platform binaries. Unidentified executables retain exact compiler-hash authentication, and
schema 1 retains exact compiler/generator hashes. Schema 2 keeps those hashes as provenance.

Bootstrap captures the actual launched executable in a weak context association. Pipeline caches
producer discovery once across preliminary and final customization authentication.
Compiler discovery retains verified logical wrapper pairing for symlinked platform directories,
with physical executable identity taking precedence for file links into another install. A reference
is decoded once for catalogue, compatibility and variant metadata; existing declaration and
ownership checks follow compatibility validation. The catalog-portability lab compiles a
producer, adapter and downstream consumer, including accepted schema-1 input and differing
portable provenance hashes. Contract negatives reject before output; customization retains
variants. Local focused results: declaration catalogues 58/58, portable integration 19/19,
customization 22/22. Cross-platform artifact exchange evidence is recorded after CI runs.

## Brotli catalogue file transport (2026-10-09)

CatalogTransport owns synchronous stream decoding, strict Brotli completion and the inclusive
134,217,728-byte decoded limit. DeclarationCatalog consumes its JsonDocument through the
existing schema, compatibility, source, ownership, API, generic and variant checks.
Case-insensitive .br suffixes select compression; other paths retain JSON semantics.
GeneratorConfig keeps its emission boolean and adds explicit output compression. Pipeline.run
writes declarations.json.br when selected and reports its actual name; RenderModel.Files
retains JSON text. Existing alternate-format output is preserved. Transport does not change
the inference profile or compatibility versions. Opposing Windows/Linux exchange retains both
payloads; package discovery and NuGet descriptors remain later work.

# Easy Nits 

To include in scope when a phases implementation/attempt ends up being small/quick.

- Pass names should be keyed by a unique prefix.
  - This has been done to some level, but does not cover all stages. Changing the current implementation of prefixed stages is ok if required.
  - Stage prefixes should be unique for that stage
  - Tests should fail quickly if a pass name & prefix is not unique.
  - Add this as a prefix to the key of a finding as such: `XX:TY001`
  - This will allow grepping a stage by `XX:`and a finding by `:XX000`

## Generator customization (2026-10-06)

The supported extension seam projects immutable semantic facts after ordinary shaping. Ordered F# extensions return structured attribute edits and independent marker/property companions. Pipeline.generateWith and runWith preserve the ordinary entry points with empty registration. The renderer receives an explicit annotation plan; no callback runs inside printing. Companion properties reuse resolved F# type references and preserve readonly access. Referenced source declarations retain semantic selection even when ordinary output omits them.

Explicit interface replacement preserves declaration identity while selecting resolved property
contracts and base references. Heritage consumers of a changed contract are rejected.
Customization findings use the CU prefix; source mapping findings follow companion properties.
Manifests record ordered extension identities, sorted configuration, and companion source hashes.
Catalog variants retain the authenticated original API alongside the customized API hash and
extension profile. Empty registration retains the legacy manifest and catalog bytes.
Variants with a changed declaration contract are rejected for reference reuse. Raw non-generic
interface fragments require `generateValidatedWith` and a configured .NET validation compiler;
their declared export witnesses and the complete generated output compile before returning.
Raw fragments cannot produce declaration catalogs.
Catalog reference ownership is authenticated before callbacks; referenced targets are immutable.
Companions render after catalog redirects so their property types name the compiled producer API.
Variant metadata is an optional JSON sidecar field; the existing public Catalog record is preserved.
Customization finalization invariant: semantic member targets include materialized exports
and methods, while property companion selection stays property-only. Attribute type arguments
use authenticated producer names; both interface and concrete entrypoint renderers consume the
same annotations. Replacement signatures reject unbound type variables, and companion namespaces
cannot enter occupied generated modules/types.

## Catalog composition (2026-10-09)

Schema 2 compatibility advances `identityVersion` to 2 for the source-closure correction
and `apiVersion` to 2 for the emitted generic-constraint change. Producers from the
preceding contract must be regenerated; their catalogs reject before substitution.
Contract policy, inference-profile semantics and customization rules are unchanged, so
their versions remain 1. This deliberate boundary avoids relying on a declaration-specific
mismatch to identify an older producer. Catalog transport and the compiler source contract
remain unchanged.

The catalog's transitive source closure uses canonical entries with intrinsic type declarations.
Export-only handles still anchor their own public aliases, but an exported indexed projection
cannot add its consumer file to an upstream interface's source set through a compiler-interned
anonymous type. `catalog-source-projection-lab` covers scalar, array, nullable, tuple and nested
object projections, preserves explicit named-alias sources, and rejects source/API/input changes.
Named-versus-anonymous tagged-union API drift remains a separate migration blocker.

Binding families select a shared anonymous-literal owner through the producer catalog graph.
Generate the selected owner first, then regenerate each dependent producer against its catalog.
The dependent producer exposes the selected enum in its public F# API and records the owner
dependency. Consumers may list the coordinated catalogs in either order. Independent producers
with different F# owners for one structural identity remain incompatible; consumer ordering does
not select an owner. Named aliases retain their declaration identities at equal literal values.
`literal-union-ownership-lab` covers both independent rejection and coordinated typed field use,
including source/API invalidation and distinct named aliases.

Nominal generic constraints remain present when a TypeScript type parameter defaults to `never`.
`Shape.Spec.constraintProvenNominal` preserves that declaration bound; concrete applications
retain the existing argument check and `TR044` fallback when the F# argument cannot satisfy it.
Applied bounds retain their full F# reference, including generic arguments, at both declaration
and argument sites. Constraint equivalence compares emitted references instead of checker IDs;
two checker instantiations of the same bound must not force different widening. Arguments such
as `any` or `never` widen to the complete bound through the existing findings. Pure index
signatures remain outside nominal constraint mapping.
Other structurally incompatible defaults retain the existing `TP008` policy.
`catalog-generic-defaults-lab` covers declaration and callback-result agreement, bare default
applications, generic-bound arguments, nominal subclasses, recursive bounds and invalid concrete
arguments. Catalog constraint authentication remains unchanged.


## Early union projections (2026-10-09)

The customization owner now exposes a separate `ResolvedSnapshot` after Resolve and before
Shape. It projects exported declaration handles, finite union arms and source fingerprints;
checker type IDs never serve as public identity. Equal literal sets retain separate named
source selections. Snapshot nonces seal both selected sources and returned companion plans.
This does not move catalog ownership or change the ordinary shape passes.

`ProjectionExtension` is additive to the existing `GeneratorExtension` record.
`generateProjectedWith` / `runProjectedWith` take a required validation compiler, early
projections and late customizations. Apply validates identities across both phases, current
snapshot seals, paths and exported names. Compile checks all output and type witnesses before
write. Projection provenance lives only in the manifest; raw catalog variant policy remains
owned by existing late customization. Empty early registration preserves existing output.
`CU006` reports the arbitrary-source projection boundary as Escape, retaining raw findings.

The opt-in `Xantham.Generator.Myriad` project owns emission via the published Myriad.Core 1.1.0
`IMyriadGenerator` source contract. Xantham owns resolution and source validation. Its first
adapter emits ordinary DUs, strict JavaScript codecs and the scalable two-case
`Decoded|Invalid` active pattern for selected string literal sets with number/null/undefined.
Existing naming helpers allocate cases; reserved payload/absence cases win collisions.
No signature replacement or overlapping-union provenance inference is attempted. Generic,
boolean, numeric-literal, broad-string, object and unresolved shapes fail closed.

`projection-lab`, early contract tests, compiler rejection controls and companion goldens
exercise the seam. The existing Fable RunGate executes null/undefined matching, non-finite
numbers, hostile strings, equal/overlapping sets and the explicit raw echo boundary.

### Operation contracts (2026-10-09)

The same early snapshot now resolves selected instance-method parameter occurrences lazily,
while the compiler session is alive. `Resolved.shape` carries bounded primitive, array, record
and union facts; `Resolved.operation` carries argument order, optionality and optional selected
field metadata. Resolve owns typed declaration keys and defining-source closure reads.
Semantics owns validation and occurrence fingerprints. Missing facts, unsupported source forms,
generic/overloaded calls, recursive/indexed/callable payloads and symbolic keys fail closed.

`ProjectionCompanion.forOperation` adds a sealed receiver assertion. Pipeline verifies that
assertion against the final qualified Shape/catalog mapping before compilation. The Myriad
operation emitter consumes the preserved shape to generate closed input DUs and records, private
encoders and public typed methods. Remaining parameters and results are inferred from the real
SDK method call. Raw binding types and catalog ownership remain intact.

Optional record properties preserve omission separately from present undefined, with null as
its own value case. Required literal fields are automatic discriminators. Actual declaration
keys and safe own-property writes preserve hostile string keys. `Operations.createShared`
provides explicit coordination: the first selection owns the input types and later operations
alias them after exact shape equality. Per-operation source authentication remains independent.

The operation matrix extends `projection-lab` and the existing contract, compile and Fable gates.
The real integration target is CloudEdge's Agents 0.27/Pi 1.1/Chord 1.1 bundle. Its closed generated
dependency graph uses opt-in `recursiveGroups` placement. The configured namespace must be
the immediate parent of every ordinary shipped module; compiler-library modules retain their
separate owner. The existing namespace renderer owns the single-file recursive representation.

## Generic tagged unions (2026-10-09)

A discriminated union declares the type parameters its arms read (§4.5(2), §4.9). `FsTaggedUnionDecl.TypeParameters` holds the alias's own parameters, then those `bind-free-type-params` records for a union hoisted out of a generic scope; `detect-tagged-unions` shapes the case fields under that scope, so a `T` payload reads `'T` rather than `obj` with `TR013`. `Spec.isTaggedDeclaration` states the conditions `detect-tagged-unions` applies inline; the naming walk and the reference read it. `unionRef` applies the parameters at every reference: an alias application its recovered arguments, any other tagged declaration its own and free parameters. Literal-union aliases with a phantom parameter stay non-generic string enums.

`synthesize-anonymous` recognises an application of a generic tagged alias the way it recognises one over an intersection. The form is the smallest id carrying the alias symbol whose arguments are its own distinct type parameters. An application must correspond to that form arm for arm - the same arm, a reference to the same declaration, or an inline arm under the same tag value and field names - and carry its arguments in the type table. Otherwise the union is hoisted on its own, generic over what it reads. `schedule<T>(): Scheduled<T>` therefore reads `Scheduled<'T>` instead of minting `Scheduler.Schedule.Result`. The walk's alias-form clause reads only a union form for a union, since an intersection alias distributed over a union argument keeps its symbol on the union. Arity, alias re-exports, orphan reads, rendering, foreign qualification and the declaration catalog carry the parameters; catalog `apiVersion` advances to 3 because tagged-union arity and canonical API change. Identity keys are unchanged: union identities bind only declared alias parameters, and applications stay outside the catalog's name map.

A nullable position hides its application: `x?: Job<string>` reaches the shape tier as the application's arms beside `undefined`. Resolve records the application from `getNonNullableType`, which returns it through the union's origin (`TypeFacts.NonNullableApplication`, with its arguments followed into the table) for a union keeping two or more members, one an object. The naming walk registers the application ahead of the union and leaves the union's arms unnamed, and `unionRef` writes it under `option`: `poll(): Job<T> | undefined` reads `Job<'T> option`, and `MaybeJob<T> = Job<T> | null` abbreviates `Job<'T> option` rather than an erased union over arm copies. A parameter no arm reads leaves the arms shared by every application, so a member-set match lands on the declared form from outside its scope; `unionRef` then takes the arguments of the site's own application (`marker?: Marker<string>` reads `Marker<string> option`) and declines to the erased union where the site carries none, never applying an out-of-scope parameter as `obj`. `Maybe<T> | undefined` over an alias that already includes `null` leaves no single application in the origin and still depends on the member-set match.

`synthesize-paramobjects` carries a generic method into `Create` with its type parameters bound once per call, which the interface does not promise; `SP004 MethodTypeParametersBoundAtCreate` (Widened) now reports each such method. Refusing the `Create` instead is a user decision.

`generic-tag-lab` pins the shape matrix (two parameters, array and promise payloads, a constraint, a phantom, recursion, interface arms, an intersection distributed over arms with a default, nullable and optional positions including a phantom argument and an application no other member reaches, a re-export, a re-alias, a transformed subset and a phantom literal union) and a producer/adapter catalog run reading `Job<string> option` across packages; `shared-tag-lab` pins the `Scheduled<T>` reducer. Committed corpus: `TR013` and `TP006` unchanged (no golden held a generic tagged union); labs before/after `TR013` 27 to 0, `TP006` 1 to 0, and the 27 `obj` tokens the generic payloads produced to 0. The nullable-application fix removes the lab's four unreferenced arm copies (`Queue.Last`, `Queue.Last2`, `MaybeJob2`, `MaybeJob3`), and a phantom argument beside `undefined` reads `Marker<string>` where it read `Marker<obj>` with `TR013`. `SP004` reads 63 across the committed goldens (`workers-types` 37) and 37 in `Xantham.Fable.Core.TS`; `solid-js`'s 8 wait on a regeneration where its paths case-fold (`caseFoldSensitive`).

## Operation declaration-name selection (2026-10-10)

Before-Shape operation selection matches method and selected-field names recovered from typed
declarations. Leading double underscores remain JavaScript keys: `__method` and `__proto__`
select their declarations, while checker-escaped aliases reject. Required sibling checks compare
symbol identity. Final declaration-name, receiver, source-closure and snapshot authentication
remain in place. The resolved-operation regression covers whole parameters, selected fields,
authenticated companion plans and escaped-name rejection.

The review follow-up also regenerates the outstanding Windows `solid-js` findings: `SP004`
increases from 0 to 8 across `Universal.RendererOptions` and `Universal.Renderer`. Binding text
is unchanged; one symbol moves from Ergonomic to Widened. The original PR head reproduces the
stale findings failure before this regeneration.

Validation: the unfiltered Windows `build.fsx -- test --run-gate --ci` passed 1,271 Generator
tests and 99 Wire tests (one existing ignored case in each suite), 528 Fable checks and the real
Partas plugin gate. The operation regression was observed failing before the lookup fix.

## The checker's error type (2026-10-09)

`TypeFlags.Any` covers four checker intrinsic names, and `any` covers a written or omitted annotation, a shorthand ambient module and diagnosed circular return, accessor or import-alias annotations, so `TR008` is not proof of authorship. A circular type alias is the error type. `typeRef` reads `TypeResponse.IntrinsicName`: `any` keeps `TR008`; `error` and `unresolved` (a reference the program leaves unresolved or invalid) raise `TR063 ErrorTypeToObj` with the name the checker keeps; `intrinsic` (the body of `Uppercase` and its siblings) raises `TR064 IntrinsicMarkerToObj`. An Any without an intrinsic name is not claimed as a written `any`. `TR063` and `TR064` are Escape; `TR008` is Ergonomic (W1); the keys separate them. Resolve records the name from the error type's alias symbol, qualified through its unresolved parents (`NodeJS.Timeout`); a union, an intersection or an optional position the checker reduced over an error constituent carries none. A second export abbreviating the shared intrinsic marker (`Lowercase = Uppercase`) reports the same `TR064`.

`unresolved-any-lab` pins an unresolved import, an undeclared name, a qualified name, an element position and a reduced union beside a written `any`, an omitted annotation and `unknown`. Committed corpus: `TR008` 841 to 830, `TR063` 0 to 17 (animejs 12: six members typed by the absent `three`, three sites typed by `InstanceParent`, which reduces to the nameless error type, two `NodeJS` references and one bare `TargetAdapterEntry`; the lab 5), `TR064` 0 to 5 (`compiler-lib-ownership-lab`'s `Uppercase`, `Lowercase`, `Capitalize`, `Uncapitalize` and `NoInfer`) and 0 to 5 in `Xantham.Fable.Core.TS`. Binding text is unchanged outside the lab.

## Faithful any grades ergonomic (2026-10-13, W1 #132)

`TR.AnyToObj` (`TR008`) is Ergonomic; its `FindingCodes.table` key and message are unchanged. Escape holds stated-type losses: on the `typeRef` path, `TR063 ErrorTypeToObj`, `TR064 IntrinsicMarkerToObj`, `TR003 MissingFromTypeTable` and `TR054 ReferencedArityUnconfirmed`. A symbol's tier is the worst of its findings, so a row with `TR008` beside a `TR063` stays escape.

The case is regraded whole, not split. `typeRef` receives a type id and an owner, and every source of the intrinsic `any` (a written `any`, an omitted annotation, `type X = any`, a shorthand ambient module, a checker circular-annotation fallback) is the same singleton id with identical `TypeFacts`. A split keyed on reference sites would need per-file semantic diagnostics mapped to declarations and threaded into `typeRef`, and would still misgrade nested positions such as `Array<any>`. No golden carries a circularity signal.

Exception: a diagnosed circular return, accessor or import-alias annotation (TS2577, TS2303) reaches `typeRef` as the intrinsic `any` and reads `TR008` at Ergonomic. These occur only in a broken declaration file, which reports the diagnostic. No lab probe pins them.

`unresolved-any-lab` adds `Faithful` (a written `any` and an omitted annotation as the interface's only findings) and `listen(listener: (...args: any[]) => void)`, rendered `listener: (obj[] -> unit)`. Its test pins every `TR008` at Ergonomic (`Exports.listen(listener)(args)` among them), every `TR063` at Escape, the `Exports.listen` and `Faithful` rows at Ergonomic and the `Probe` row at Escape. `Findings.test.fs` and the `shape typeRef` unit pin the new tier.

Measured on the two selected goldens (symbol tiers, exact / ergonomic / widened / escape):

| Golden | Before | After | `TR008` at escape |
| --- | --- | --- | --- |
| `animejs` | 129 / 129 / 14 / 79 | 129 / 189 / 27 / 6 | 392 to 0 |
| `@cloudflare/workers-types` | 412 / 1067 / 374 / 111 | 412 / 1144 / 407 / 1 | 168 to 0 |

46 regraded rows land at Widened through another finding on the same symbol, which moves the widened baseline later workstreams measure against. The two `animejs` rows still escape with `TR008` (`TextSplitter`, `Adapters.Three.ThreeAdapter.RegisterPropertyResolver.Resolver`) each carry a `TR063`. Finding counts by key are unchanged.

Binding text is byte-identical: outside the lab's own additions no golden `.fs` changed. Thirteen lab goldens moved tier only in `manifest.json` and `symbols.jsonl`: `compiler-lib-ownership-lab`, `uninhabited-intersection-lab`, `lib-ship-lab`, `catalog-library-entry-lab`, `catalog-subpath-lab`, `augmented-dom-lab`, `catalog-generic-defaults-lab`, `grouped-dom-aliases-lab`, `frontier-width-lab`, `inherit-lab`, `literal-overload-lab`, `member-shape-lab` and `optional-param-lab`. `solid-js`, `Xantham.Fable.Core.TS` and `Xantham.Fable.Node` were not regenerated (T4, T6); their reports still grade `TR008` Escape until integration.

## Selected callback any returns as unit (2026-10-13, W2 #133)

`anyReturnAsUnit` in `xantham.json` is one .NET regular expression, matched case-sensitively and culture-invariantly against a TypeScript property or value name; it is unanchored unless the pattern anchors itself. An empty string, a non-string and an invalid expression fail at load. Where it matches, the `any` return of a callback typed directly at that property or value is written `unit`, under `TR.AnyReturnAsUnit` (`TR065`, Ergonomic) in place of `TR008`. Omitted, output is unchanged.

Scope is owner equality. The four member and value sites (`shapeMembers`' property branch, class statics in `Shape/Classes.fs`, the folded exclusive-arm `buildParam`/`buildMember` in `Shape/Interfaces.fs`, value exports in `Shape/Exports.fs`) shape through `Spec.selectAnyReturn`, which sets `ShapeModel.AnyReturnOwner` to the member's owner, or clears it for an unmatched name. `delegateRef` writes `unit` only when its own owner equals that field and the return is the intrinsic `any` (`Spec.isSourceAny`). A nullable or erased-union position passes the owner through unchanged, so `((ev) => any) | null` and an arm of `string | ((ev) => any)` are selected. These keep `obj`:

- method returns and method parameters (`onclose(ev): any`);
- a callback nested in a selected callback (`Upload.onregister(resolve)()`);
- a named callback alias, a declaration shared by every reference (`Handler`);
- an anonymous callback of two or more arguments, which `synthesize-anonymous` declares as a named delegate;
- tagged-union case fields and hook interfaces;
- the error type (`TR063`) and every stated return.

The pattern is part of `DeclarationCatalog.profile` when set, beside `resolveNoInfer`: a catalog consumer whose pattern differs from its producer's fails the inference-profile check before any declaration is reused. A profile without the key hashes as it did before the key existed, so catalogs whose producer sets no pattern keep their profile.

`handler-return-lab` (`^on`) pins `TR065` at Ergonomic on `Upload.onprogress()`, `Upload.onready()`, `Upload.onmixed()` (the callback arm of `((ev) => any) | string`) and `Exports.onload()`; `TR008` on `Handler()`, `Upload.Onpair()` (the delegate hoisted from the two-argument `onpair`), `Upload.onclose()`, `Upload.onregister(resolve)()`, `Upload.onstate` and `Upload.transform()`; and the rendered `onprogress`, `onready`, `onmixed`, `Onpair`, `onload`, `Create` and `Handler` lines. `catalog library ambient authentication` pins that a consumer with a different pattern fails the inference profile. `shape typeRef` pins the owner check and `selectAnyReturn`; `generator config` pins the loader.

The selected goldens configure no pattern and are unchanged: `animejs` and `@cloudflare/workers-types` regenerated byte-identical. Generated into scratch with `{"anyReturnAsUnit":"^on"}`, `animejs` moves 257 findings from `TR008` to `TR065` (392 to 135 `TR008`), every one an `on*` property in `shape-interfaces` (`onComplete` 22, `onUpdate` 20, `onBegin`/`onLoop`/`onPause`/`onBeforeUpdate` 16 each). Symbol tiers are unchanged (129 / 189 / 27 / 6), because both codes are Ergonomic after W1.

W2.2: `tools/fable-core-ts-input/xantham.json` selects `^on`, and `Xantham.Fable.Core.TS` is regenerated under the T4 override. The selected goldens configure no pattern, so they cannot show W2.2. `catalogue-inputs.json` hashes the generation configuration (`generationConfigurationSha256`), so editing it alone fails the pack-time `prepare` check until Core.TS is regenerated. The binding diff is 411 lines, every one an `abstract on*` handler moving from `-> obj)` to `-> unit)`; no other line moved. `TR008` falls from 17,659 to 1,672 and `TR065` rises to 15,987 (15,863 in `shape-interfaces`, 124 global `on*` values in `shape-exports`, which the binding writes as members of `Fable.Core.TS.Es.GlobalThis`). `declarations.json` changes 75 `api` hashes, each a declaration that carries `on*` handlers (`GlobalEventHandlers`, `Window`, `XMLHttpRequest`, `Fable.Core.TS.Es.GlobalThis`), plus the generator hash and the inference profile, which now records `^on`; identities and owners are unchanged. Symbol tiers move from 2,073 / 1,750 / 578 / 662 to 2,073 / 2,125 / 860 / 5; that movement is W1's `TR008` regrade, applied to Core.TS here for the first time, and no finding count changes outside `TR008` and `TR065`. `build.fsx -- generate --only compiler-lib` deletes the tracked `symbols.jsonl`; it is restored from a CLI run with the same compiler, whose binding, manifest and catalog are byte-identical.

A catalog consumer of Core.TS must now set `anyReturnAsUnit` to `^on`, or it fails the inference-profile check. Both in-repo consumers (`catalog library identity`, `core-ts package`) load the Core.TS configuration and inherit the pattern. A consumer that assigns `fun e -> box …` to a handler no longer compiles, because the handler now returns `unit`; the packages are pre-1.0, so this ships as a minor change (`feat(core-ts)`). `Xantham.Fable.Node` is not regenerated: it declares its own `on*` handlers and inherits none of Core.TS's.

## TypeScript library catalogue identity (2026-10-09)

Catalogue sources for installed TypeScript `lib/lib.*.d.ts` files use logical package
ownership `typescript` across the wrapper and recognized platform distributions. Canonical
root metadata hashes name, exact version and normalized gitHead, while source bytes and
intervening manifests retain authentication. Library metadata must agree with the recognized
compiler release; binary fallback retains exact executable authentication and permits missing
gitHead. Ordinary package manifests and non-library files retain their previous hashes.
Normalized source keys feed declaration handles, identities and canonical API references.
Identity contract 3 requires regeneration of platform-coloured producers; API hashing,
inference, customization, policy and transport versions remain unchanged. Core.TS emission
is compared against the complete committed semantic catalogue on each CI OS, and a compiled
consumer plus targeted mutations cover reuse and authentication.

Full Core.TS reuse is covered with a compiler-only `export {}` program under the matching
ES/DOM profile; its generated aliases compile against the original binding. An ordinary
package probe containing `export function accept(value: Map<string, HTMLElement>):
Map<string, HTMLElement>;` under the same shipping profile is still rejected for
`AudioWorkletNodeOptions` API mismatch. The OS-identity correction preserves that guard;
entry-dependent API compatibility remains a separate investigation.

Diagnosis (2026-10-10): `harvestGlobals` returns early when module exports are
non-empty and there is no global entry file, even when `typescript/lib` is `ship`.
The empty Core.TS producer therefore seeds all library declarations, including generic
`Record`, while the exported-function consumer only reaches instantiated library types.
The consumer emits `AudioWorkletNodeOptions.parameterData` as a bare `Record<string, float>`
application without emitting the generic `Record` declaration. Canonical API hashing
therefore retains the literal name `Record` instead of the producer's declaration identity.
Captured API texts are otherwise identical; the producer hash is `471214c5bcb9bc1ba777792d18f4bc9149969076b3b419cbab7ac4d6dcf9650c`,
the consumer hash is `9316f56c62f5670d77a863edc99076431fd54f63527bcf9d1ca553fa1c5b9534`.
A controlled experiment bypassing only that early return for compiler-library shipping
restored `Record` and allowed the original exported-function consumer to reuse the complete
Core.TS catalogue with all authentication enabled. The experiment was reverted after diagnosis;
the production fix needs harvest regression coverage and full gates, not relaxed API checks.

Validation of the OS identity wave: the full `build.fsx -- test --run-gate` pipeline passed,
including the Generator and Wire suites, Fable run gate and customization Partas gate.
The 28 new library identity/reuse cases cover all eight package names, complete Core.TS
semantic re-emission, compiled aliases and reader rejection paths. Five Node portability
and provenance checks passed. All 254 checked golden/Core.TS binding and manifest hashes
remained unchanged. Independent review found no defects; actual Linux execution remains
for CI, while the Windows regression authenticates the Linux-origin source-byte baseline.

## Compiler-library entry invariance and computed keys (2026-10-10)

Compiler-library `ship` runs harvest globals even when the entry module exports values or
types. Module-only inputs query an actual default-library source's global scope: querying
the entry scope duplicated module exports as globals and let local names hide library names.
Explicit global public inputs retain their own scope. The small scripthost harvest lab
compares the complete library declaration set across entry forms and checks that Reference,
Widen and Map continue to withhold library globals. Programs with `no-default-lib` retain
module exports without reharvesting them as globals.

Drilling the Core.TS matrix exposed a second false mismatch in `String`: structural identities
hashed escaped computed-member names such as `__@match@1085`. The numeric suffix is a
compiler-session allocation, so even fresh compiler-only producers could differ. Computed
member keys now authenticate normalized declaration handles; ordinary names have a disjoint
encoding. Structural member hashing and inferred parent roles share this key. Missing computed
declarations do not fall back to a session ID. Identity contract advances from 3 to 4; the
Customization and policy contracts remain unchanged. Intrinsic checker
types no longer borrow aggregate export handles or inferred parent roles: their named aliases
retain their own declaration handles. The identity bump covers both corrections.

Pure index-signature references use explicit `JS.Record` / `JS.ReadonlyRecord` support names.
Previously a bare `Record` could bind to the shipped library's phantom or a local declaration;
arity repair qualified it only for some consumers, causing different emitted APIs. Inference
contract advances to 2 and API contract to 3 for this emitted ABI correction. Core.TS and dependent producers require
regeneration; source and API authentication remain strict.

The entry matrix covers empty module, function, value, interface, namespace, default, re-export,
unrelated export, primitive aliases, local library-name/ReadonlyRecord collisions, type-only
global and mixed public entries against a shared ES5/worker/scripthost catalogue.
Each entry authenticates and compiles reused TextStreamReader/TextStreamWriter aliases.
The worker library supplies a declaration-backed path to the synthesized global object;
scripthost alone cannot catalogue that object. A single full Core.TS smoke retains semantic
re-emission, Map/HTMLElement and AudioWorkletNodeOptions reuse, and Record/ReadonlyRecord
collision checks. A global function adds a genuine `typeof globalThis` member: its changed
GlobalThis API must still reject reuse of the unaugmented catalogue. This is distinct from
the false entry-dependent hashes.

The small producer is generated lazily once per test process. Every consumer still starts a
fresh compiler session, and rejection tests mutate their own catalogue copies. The Expecto
runner owns the producer's scratch-directory lifetime, including cleanup after failures;
producer-generation failures clean up before propagating. This removes 21 repeated setup runs
without dropping entry cases or sharing mutable consumer state.

Reference transport, structure and compatibility checks now run immediately after bootstrap,
before Harvest/Resolve/Shape, using the same cached compiler identity as final authentication.
The later catalogue pass still checks source closures, F# APIs, owners and variants. This
changes rejection order for incompatible inputs without accepting additional catalogues or
changing contract versions. CI Expecto commands emit per-test timings so future repeated
large-profile traversals can be identified in the job log.

Runtime follow-up validation: the unfiltered Windows CI-mode `build.fsx -- test --run-gate`
passed 1,258 Generator tests, 99 Wire tests (one existing ignored case per suite), 468 Fable
checks and the Partas gate. With the shared producer, the Generator suite took 7m11s and the
complete pipeline 9m21s. The first CI run before producer sharing reduced the Test step from
41m58s to 24m51s; the Generator suite fell from 37m45s to 19m41s on that Linux runner.
All tracked goldens and Core.TS artefacts stayed unchanged. A full ESNext/DOM input with an
incompatible compiler-release reference rejected in 0.55s before declaration traversal.

Validation: the full Windows `build.fsx -- test --run-gate` pipeline passed: 1,249 Generator
tests, 99 Wire tests (one existing ignored case in each suite), 468 Fable checks and the real
Partas customization gate. This wave adds 24 regression cases, including private/equal-valued
literal alias reuse. All 118 golden-generation cases and five Node provenance/portability
checks passed. Independent review's literal-alias and emitted-ABI probes were addressed.
The 14 changed existing binding files differ only in explicit support-record qualification;
all existing findings manifests and compiler input bytes are unchanged. Three new binding/
manifest hashes cover the scripthost lab. Actual Linux execution remains for CI.

### Combined portable-library and projection catalogue contract

Integrating the independent portable library and early projection changes uses IdentityVersion 5, ApiVersion 4 and InferenceVersion 2. Both branches used API version 3 for different emitted contracts; taking the maximum would accept a pre-integration producer. The combined version rejects both old producers before traversal. Producer closure dependencies authenticate against actual consumer inputs; the shared-input test selects ES5 explicitly rather than relying on package sort order, and separately rejects a producer-only source. Global augmentation is rejected by the final GlobalThis API authentication before output is written.

Regenerating Core.TS adds 776 catalogue declarations relative to the portable-library branch:
774 literal-overload support declarations now retained in shipped compiler libraries, and
the two intersection aliases `DecoratorMetadataObject.Base` and `ElementTagNameMap.Base`.
The full-library smoke regenerates the catalogue and compiles reused consumer types; these
additions follow the projection branch's overload and anonymous-intersection rules.

## Core.TS catalogue package distribution (2026-10-11)

Compiler-library regeneration now emits the committed schema-2 declaration catalogue in its
normal input configuration. Core.TS packing invokes `tools/package-core-ts-catalog.fsx` before
NuGet metadata generation, producing `xantham/declarations.json.br` and `xantham/catalogue.json`
under `obj/xantham/`. The descriptor follows the actual MSBuild package version and records
package identity, the existing `FableCoreTsInput` owner, ordered owners, compatibility contracts
and hashes of the payload, compiled assembly, binding sources and generation configuration.
Packaging preserves all declaration identities and generator compatibility contracts.

The compiler-lib stage writes `catalogue-inputs.json` after moving the generated binding into
place. Packing rejects catalogue/source/configuration hashes, producer entry/manifest hashes
and the root TypeScript pin that differ from this record.
Tracked text hashes normalize CRLF to LF. The Core.TS Build target records binding inputs and
the compiled assembly hash beside that assembly; packing also rejects a missing or stale build
record, including `--no-build` with stale output. Descriptor packageVersion uses PackageVersion
so standard NuGet overrides retain the correct package identity.

The package regression restores into a disposable isolated NuGet cache and compiles a generated
consumer with PackageReferences only. It verifies HTMLInputElement/FileList arity 0 and Promise
arity 1, native qualified names and ownership, and descriptor payload/assembly hashes. Setting
`XANTHAM_CORE_TS_PACKAGE` to a downloaded nupkg exercises the same consumer against a published
artifact without repacking the checkout. The existing full-library smoke still authenticates
the regenerated semantic catalogue against the committed producer.

## Delivery contract recovery (2026-10-09)

Resolve retains source contracts when an unrelated generic expansion exceeds the frontier
width. Admission uses source declaration identity and the existing type table's canonical
nominal contract edges; it never queries a second recursively expanded structure for admission.
Nested application arguments survive, while application members retain their ordinary
identity/structural distinction. Width-deferred types can resume when a later owner proves
they belong to its contract. The depth limit remains in force. Low-width compile regressions
cover reordered promotion, nested conditional/default intersections and a late source owner,
with a negative union-payload compile.

DeclarationCatalog retains constructor parameter bounds in indexed alias fallback keys,
preserving equivalent alpha-renamed constructors across producer/consumer generation.
Shape's default-export naming rejects SourceFile absolute paths; secondary-alias anchoring
uses the same sanitized identifier as Shape. Identity/API/inference contracts advance to
6/5/3; policy and customization versions remain 1. Regenerate Core.TS and dependent producers
together; old contracts reject before traversal.

Audit excluded the proposed extension of bound hashing to every structural alias. It broke
both JSON and Brotli consumer compilation for `Config<T>` when a caller constrained `T`:
the alias declaration itself is unconstrained. Constructor-only fallback hashing passes those
existing reuse cases. The separately reproduced `Direct<T,U> = Flatten<T,U>` versus
`Infer<T extends Schema,U> = Flatten<T["output"],U>` collision remains deferred; resolving it
must distinguish declared bounds from caller bounds. The original patch is preserved in the
integration recovery stash. The deferred reproducer is:

```typescript
type Flatten<T, U = string> = { formErrors: U[]; fieldErrors: { [P in keyof T]?: U[] } };
interface Schema { output: unknown }
export type Direct<T, U = string> = Flatten<T, U>;
export type Infer<T extends Schema, U = string> = Flatten<T["output"], U>;
```

The old literal-union ownership audit is also preserved in the recovery stash, but its
coordinated-producer solution and tracked lab already exist on the remote branch.

Validation on the updated remote branch: `build.fsx -- test --no-format --run-gate` passed
1,341 Generator tests and 99 Wire tests, with one existing ignored case in each suite,
528 Fable runtime checks and the real Partas plugin JSX gate. The Linux `solid-js` golden
comparison remains the documented case-folding skip; its determinism check is retained.
Targeted Fantomas checks passed. Committed golden bindings and findings are unchanged.
Core.TS regeneration in catalogue mode preserves all 4,062 declaration identities, APIs,
constraints and source sets, and the binding is byte-identical; only compatibility metadata
and producer provenance change in its committed catalogue.

## Well-known symbol members (2026-10-13, W4 #135)

A member keyed by a well-known symbol is emitted. Resolve records the symbol on
`ResolvedMember.WellKnownSymbol` from the declaration's computed name `[Symbol.<name>]`, and
only where the checker resolves `Symbol.<name>` to a `SymbolConstructor` property declared by a
`lib.*.d.ts` file. The escaped name `__@iterator@<id>` also spells a package's own `declare
const iterator: unique symbol`, so the name alone does not identify one; and a package's
`declare global` augmentation (`Symbol.observable` in `solid-js` and `type-fest`) names a key
that is `undefined` at runtime without a polyfill.

- `shape-interfaces` writes `[Symbol.iterator]` as an `inherit seq<element>` beside an
  `FsSymbolKeyed` member. The element is the `value` of the first `next` result arm whose `done`
  is absent or admits `false`, or else the iterator's first type argument: a lib iterator reaches
  Shape without members under O7, and every lib iterator takes its element first.
- Every other well-known key is an `FsSymbolKeyed` member named `Symbol<Name>` and rendered
  under `[<Emit("$0[Symbol.<name>]")>]`: a call appends `()` or `($1...)`, and a writable property
  carries per-accessor get and set Emits.
- `settle-symbol-members` runs after `drop-inherited`, bases first. A declaration keeps its own
  `seq` only where no base iterates; at the base's element it inherits the base's alone. F#
  admits one `seq` instantiation per interface (FS3360, `NodeListOf<'TNode>` over
  `NodeList`), so a narrowed element drops its own `seq` and keeps the iterator as an Emit
  member. `MB008` reports a kept `seq`, `MB009` each Emit member.
- The findings raised while shaping a `[Symbol.iterator]` wait on `ShapeModel.IteratorFindings`:
  `settle-symbol-members` reports the element's with a kept `seq` and the member's with a kept
  member, stamped with its own pass name. A removed member leaves no finding behind.
- Bases the settle pass reads are the run's own declarations. A lib base is never inherited,
  catalogue run or not: its members are flattened in (`SI002`), so the declaration's own `seq`
  is the only one. A catalogue run re-shapes every reused declaration, so a reused base is among
  the run's declarations.
- `MB002` remains for a key naming the package's own `unique symbol`, a key declared solely
  by a package's augmentation, an optional member, a member whose `Symbol<Name>` is already declared
  by another member, a class static, a member of an `[<AbstractClass>]` entrypoint (a new
  abstract slot obliges every consumer subclass to implement it), and a narrowed iterator whose
  member `drop-inherited` removed. Its message text is unchanged.
- A declaration carrying a required symbol member or a `seq` inherit, its own or a base's,
  carries no `Create` (`SP003`, "a required well-known symbol member has no key a ParamObject
  literal can write"): the object `Create` builds would lack the key the type promises.
  `synthesize-paramobjects` runs before the settle pass, so inherited members are still present.
- `CatalogCompatibility.ApiVersion` is 6: the canonical API text gains a `symbol` node and lib
  declarations' emitted ABI changes. A consumer rejects an earlier Core.TS catalogue.
- No page under `site/content` documents member-key mapping, so the consumer docs are
  unchanged.

Lab `symbol-member-lab`: `Disposable` reads `SymbolDispose`, `Tagged` reads
`SymbolToStringTag`, `Bag`/`Box<T>`/`Values` inherit `seq<string>`/`seq<'T>`/`seq<U2<string,
float>>`, `LabelledBag` inherits `Bag` alone, and `Strings` (narrowed through a package
iterator) inherits `Values` and keeps `SymbolIterator` under Emit. `Convertible` carries a
writable `SymbolToStringTag` (get and set Emits) and `SymbolToPrimitive: ?hint: string ->
string`; `Narrowed` extends lib `Iterable<string | number>` and inherits `seq<string>` alone.
`MB002` remains on `Keyed.__@key`, on `Impostor.__@iterator` (a unique symbol spelled
`iterator`) and on `Observed.__@observable` (an augmented `SymbolConstructor`). Every
declaration with a symbol member or `seq` reports `SP003` and carries no `Create`, and no
`[Symbol.iterator](...)` finding survives a removed member. The RunGate calls `SymbolDispose`,
reads `SymbolToStringTag`, iterates `bag` with `Seq.toList`, writes `convertible`'s
`SymbolToStringTag`, and calls `SymbolToPrimitive` with and without its argument.

Measured on W2 (`anyReturnAsUnit`, with W1 merged):

| Golden | MB002 | MB008 | MB009 | exact / ergonomic / widened / escape |
| --- | --- | --- | --- | --- |
| `@cloudflare/workers-types` | 10 → 0 | 0 → 5 | 0 → 2 | 412/1144/407/1 → 412/1147/404/1 |
| `type-fest` | 2 → 2 | 0 | 0 | 26/64/205/2 → 26/64/205/2 |
| Core.TS (T4) | 202 → 0 | 0 → 61 | 0 → 76 | 2073/2125/860/5 → 2073/2203/785/5 |

In `workers-types`, `TR023` is unchanged (37): the iterator member of the five iterables that
inherit `seq` (`EventCounts`, `FormData`, `Headers`, `SqlStorageCursor`, `URLSearchParams`) and
of `R2Bucket.Get.Options.OnlyIf`/`OnlyIf2`, which read through `Headers`, is removed with its
findings. `HyperdriveDynamic` reads `SymbolDispose` through `Disposable`. Those eight lose their
`Create` (`SP001` 1305 → 1298 against the first W4 commit, `SP003` +7, with `SP002`/`SP004`/`TR055`
falling with the dropped parameters). `ReadableStream.[Symbol.asyncIterator]` keeps its Emit
member, so its `TR024`/`TR025`/`TR032`/`MB001` stand. `type-fest`'s
`EmptyObject.__@emptyObjectSymbol` is a unique symbol and `ObservableLike.__@observable` an
augmented key; both stay `MB002`.

The pull-request regeneration (`test --goldens`) moves `inherit-lab` and `member-shape-lab`
`Deferred`, which gain `SymbolToStringTag` from `Promise` (`MB002` 1 → 0 each) and so carry
no `Create`. `solid-js` `Observable.__@observable` stays `MB002`, and `Observable` keeps its
`Create`.

Core.TS was regenerated under override T4: a consumer re-shapes every reused Core.TS
declaration and authenticates it against the committed `declarations.json` API hash
(`catalog library identity` failed with 117 changed entries before the regeneration), and
W4 with the ApiVersion bump changes that hash. It was regenerated again for the `Create`
refusal: 31 Core.TS interfaces (`Promise`, `Map`, `Set`, `Headers` and others) carried a
`Create` building an object without the symbol key the type promises. Node was not
regenerated: it compiles against the regenerated Core.TS. Core.TS's three new escape symbols
(`SegmentIterator` and two `[Symbol.replace]` replacer delegates) are newly reached
declarations whose escape is `TR008`.

### Augmented well-known keys (2026-10-24, W3)

`@types/node` runs under `lib: ["node"]`, which loads no `esnext.disposable`, so its
`compatibility/disposable.d.ts` augmentation is the program's only declaration of
`Symbol.dispose` and `Symbol.asyncDispose`, and both read as `MB002`. Resolve keeps the
program's own lib declaration as the first proof. A key declared only outside the program's lib
falls back to a second program, one per connection and created on first use: lib `esnext`,
rooted at the lib file that declares the program's global `Symbol`. The key is well-known when
that program's `Symbol` has the property declared by a `lib.*.d.ts` file. The fallback applies
only to a key whose receiver resolves to the program's global `Symbol` and whose symbol is the
property `getPropertyOfType` returns on the global `SymbolConstructor`; a namespace-local
`Symbol` keyed `[Symbol.asyncDispose]` stays `MB002`. `Symbol.observable` has no such
declaration and stays `MB002`. The program needs a root file: with none,
`resolveName` returns no global `Symbol`.

Lab `disposable-augmentation-lab` (`lib: ["es2020"]`): the global `Disposable` reads
`SymbolDispose` and `Stream` reads `SymbolAsyncDispose` (`MB009`), `Handle` inherits
`Disposable` and loses its `Create` (`SP003`), and `MB002` remains on `Keyed.__@own` and
`Stream.__@observable`, and `Shadowed.Resource.__@asyncDispose` (a namespace-local `Symbol`).
Node, measured against its regeneration at the W1 generator:
`MB002` 161 → 92 (`dispose` 12 and `asyncDispose` 57 to 0; `captureRejectionSymbol` 89,
`kOptions` 2 and `custom` 1 unchanged), `MB009` 30 → 43, `SP003` 290 → 293, `SP001` 1057 → 1054,
`SP002` 546 → 536, tiers ergonomic/widened 2184/593 → 2192/585, 13 Emit members for the two keys, and no symbol is
worse. Under lib `esnext` every well-known key has a program lib declaration, so Core.TS
keeps its `MB002`/`MB008`/`MB009` counts (2/61/75), and every test golden other than the lab
is unchanged. Emitted ABI changes for a producer whose lib omits a key that a package augments
onto `SymbolConstructor`: the declaration gains a required Emit member and can lose `Create`
(`SP003`), so a catalog from an earlier producer of that shape disagrees with the current
reading. Core.TS, the only shipped catalog, is not such a producer (lib `esnext`).
`CatalogCompatibility.ApiVersion` 10, raised once for the fidelity stack in W1, rejects such
producers.

## Literal overloads as methods (2026-10-13, W5 #136)

An overload keyed by a string literal is a method of its own. The draft pass
`expand-literal-overloads` (§3) landed inside `shape-interfaces`, in `Spec.shapeMembers`,
rather than as a pass of its own: `typeRef` raises `TR006` while the signature is shaped, and a
finding cannot be withdrawn (`docs/.ai/footguns.md`). It is unconditional.

- **Which members.** `Spec.isLiteralKeyed`: two or more call signatures, at least one carrying a
  literal key, on a method or on a required property holding a pure callback whose signatures
  `callSignaturesSeparable` tells apart. An intersection's member is such a property
  (`Element & { setAttribute(name: "draw", ...): void }`), unless `shapeMembers` reads it at a
  type its operands agree on, where it stays unkeyed. A symbol-keyed name is
  excluded. A literal key (`Spec.literalKeys`) is a required, non-rest parameter typed by one
  string literal that is not an enum member and holds no `$`, `{` or `}` (Emit macro syntax).
  Exported functions, class statics, optional callbacks, single signatures, unions of literals
  and literals nested in a parameter type (`Options<"text">`) keep their mapping.
- **Names.** The keyed overload is `FsLiteralKeyed(target, literals, FsMethod inner)`: the
  literal parameters are removed and the inner name is the JavaScript key plus each literal's
  `Naming.literalSuffix` (`onClose`, `getElementsByTagNameNSHttpWwwW3Org1999Xhtml`). A name the
  declaration's members already hold (inherited ones included) takes `on_Close`, then
  `on_Close2`. Overloads sharing a key and literals share a method (`onData` twice). The
  overloads without a literal stay under the TypeScript name, so `on(event: string, ...)` is
  kept.
- **Emit.** `Render.literalCallTemplate` writes each literal, single-quoted, before the F#
  argument at its index; from the first optional or rest parameter at or after the last literal
  the arguments spread as `$k...`: `$0.on('close', $1)`, `$0.read($1, 'text')`,
  `$0.log('warn', $1...)`. A key that is no JavaScript identifier is indexed
  (`$0['my-on'](...)`). On an `[<AbstractClass>]` entrypoint the method is a concrete
  `[<Emit>] member _.onClose(...) = jsNative`, so a derived class has no slot to fill.
- **Docs.** Resolve reads each signature declaration of a set of two or more into
  `ResolvedSignature.Source`: the declaration on one line (comments outside string literals
  removed) and the description of the JSDoc above it. The method's doc is that description,
  then a ```` ```ts ```` block of the signature (`<code lang="ts">`); an overload without JSDoc
  carries the signature block alone, because the member's doc merges every overload's JSDoc. The
  overloads that stay keep the member's doc.
- **Listeners.** A listener of two or more parameters keeps its D5a delegate
  (`onConnectionAttempt: listener: Socket.On.Listener`), departing from W5.3's curried form
  (probe 6). The keyed method's parameters are shaped by the same `shapeSignature` as every other
  signature, and D5a measured a curried callback of arity 2 or more failing in return and
  read-back positions (`generator-type-mapping.md`, D5).
- **Findings.** `MB.LiteralOverloadAsMethod` (`MB010`, Ergonomic) once per method, at
  `{owner}.{key}`, with the literal and the name; the inner signature's findings are owned by
  `{owner}.{name}`. A literal-keyed member refuses `Create` with the overloaded-method reason
  (`SP003`), and `dedupe-overloads` drops a repeated `FsLiteralKeyed` signature as `DO001`.
- **Retention.** `literalOverloadSets` reads the same `isLiteralKeyed`/`literalKeys` and offers
  only a keyed member's unkeyed signatures, so retention (`TR056`/`DO002`) serves nested
  literals, optional callbacks and exported functions. `KVNamespace.get(key, "text")` is
  `getText(key)` where it read `get(key, KVNamespace.Text)`.
- `CatalogCompatibility.ApiVersion` is 7 → 8: the canonical API text gains a `literal` node, and
  lib declarations' emitted ABI changes.

Lab `literal-method-lab`: `Socket` (an `Emitter` with `on("end")`) carries `onClose`, the
three `() => void` listeners that collided before (`onConnect`, `onReady`, `onEnd`),
`onConnectionAttempt` through its delegate, `onceClose`, `emitClose` and `emitConnect`, and keeps
the generic `on`, `once` and `emit`. `Dialog` has an `onClose` property, so its literal method is
`on_Close`; `onKeyDown` reads `"key-down"`. `Store.readText(key)` writes the literal after the
argument, `Logger.logWarn(?message)` and `logInfo(...parts)` spread their tails, and `Drawable`
(`Element & {...}`) reads `setAttributeDraw`. `Solo.tag` keeps `TR006` and `Choice.pick` its
literal unions. No `TR006` or `DO001` names a keyed member. The RunGate calls `onClose`,
`emitClose`, `onConnect`/`emitConnect`, `onConnectionAttempt` with the generic `emit`, the generic
`on`, `onEnd` (declared by `Socket` and `Emitter` both), `on_Close`, `readText`, the generic
`read`, `logWarn` with and without its argument, and `logInfo("a", "b")`, which arrives spread.

`literal-overload-lab` moves with it: `Store.read`'s `kind` overloads read `readText`,
`readJson` and `readBytes`, `Mixed.send` reads `sendNow`/`sendLater`, and `TR056` keeps
`Exports.emit(kind)` and `Store.read(options)`.

Measured on the two selected goldens:

| Golden | TR006 | DO001 | TR056 | DO002 | MB010 | exact symbols | lines |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `@cloudflare/workers-types` | 228 → 221 | 2 → 2 | 26 → 12 | 3 → 2 | 0 → 17 | 412 → 410 | 26,006 → 25,952 |
| `animejs` | 21 → 20 | 1 → 0 | 3 → 0 | 1 → 0 | 0 → 4 | 129 → 126 | 5,167 → 5,177 |

The other tiers are unchanged; the exact symbols that go are the retained single-case
StringEnums. In `workers-types`, `BrowserRun.quickAction` and `KVNamespace.get`/`getWithMetadata`
read as methods, and `DO001` stays at `Ai.run` (a `keyof` bound) and `AutoRAG.aiSearch`
(object-typed parameters), which no literal keys. In `animejs`,
`DrawableSVGGeometry.setAttribute(name: "draw", ...)` reads `setAttributeDraw` and the three
namespace literals of `getElementsByTagNameNS` read as methods.

Core.TS and Node were generated into scratch, not committed (T4), and both compile there:

| Binding | TR006 | DO001 | TR056 | DO002 | MB010 | exact symbols |
| --- | --- | --- | --- | --- | --- | --- |
| Core.TS | 94 → 89 | 10 → 10 | 786 → 0 | 161 → 1 | 0 → 784 | 2,073 → 1,302 |
| Node | 5,063 → 673 | 1,904 → 27 | 546 → 8 | 87 → 1 | 0 → 4,843 | 1,263 → 768 |

Node also loses its one `LU002` (`WorkerThreads.Worker.Error`, a retained literal now
`onError`). Both need regeneration at integration: Core.TS's member surfaces and API hashes
change (`createEvent("AnimationEvent")` is `createEventAnimationEvent()`) and ApiVersion 8
rejects the committed catalogue, so `catalog library identity` and `core-ts package` fail until
it is regenerated; Node carries the `TR006`/`DO001` reduction.

## Literal members (2026-10-13, W6 #137)

A literal member is a property that `shapeMembers` reads as required and typed by one string
or numeric literal, readonly or writable, where the literal is not an enum member or a union
(`Spec.literalMemberValue`). A writable single-literal property can hold only that value, so it
is a literal member too (lead amendment, 2026-10-14). Optional literal members, boolean literals,
enum members, symbol-keyed members, methods and the exclusive-arm fold's members keep their earlier
mapping. The member carries its value on `FsPropertyMember.Literal`, and its findings change
from `TR007`/`TR006` (Widened) to `MB.LiteralMemberAsConstant` (`MB011`, Ergonomic).

- **Instance (W6.2).** The interface still declares the primitive (`abstract AT_TARGET:
  float`), with `/// <remarks>TypeScript type: <c>2</c></remarks>` after its own docs.
- **Module (W6.1).** `renderBody` follows each interface with a block of `[<Literal>]` values
  for the literal members it declares after `drop-inherited`: a numeric literal is a `float`
  (`2.0`), a string literal a `string`. The block sits at `<declaration path>`, so
  `nestedBlocks` opens `module <leaf> =` at whichever of the block and the declaration's nested
  declarations comes first and merges the rest into it: one module per name. A subtype's module
  holds its own literal members only; its inherited ones are read through the base's module.
  Three names stay out of the module: `Create`, a static's name, and a name that opens a module
  nested under the declaration (`<declaration>.<name>.*`). A declaration rendered from
  customization source gets no module.
- **Create (W6.3).** `synthesize-paramobjects` runs before `drop-inherited`, so a subtype's
  members still include inherited literal members. They bind no parameter and do not count
  against the 24-parameter budget; `FsCreateOverload.Literals` carries them in member order, and
  `SP.LiteralMembersWrittenByCreate` (`SP005`, Exact) names them. A declaration of literal
  members alone gets a parameterless `Create`. The Emit forms are
  `[<ParamObject; Emit("({ kind: 'method', ...$0 })")>]` with parameters,
  `[<Emit("({ NONE: 0, DONE: 1 })")>]` without, and `({kind: 'method', "None": $0})` where a
  pattern-case parameter is renamed. Without literals, every form is unchanged.
- **Escaping (W6.4).** A key is bare where it matches `[A-Za-z_][A-Za-z0-9_]*`, otherwise a
  string. `Render.jsString` writes a single-quoted string with `\\`, `\'`, `\n`, `\r`, `\t`,
  `\u2028`, `\u2029` and `\xHH` for other control characters, and writes `$`, `{` and `}` as
  `\x24`, `\x7B` and `\x7D`, which Fable's Emit macro would otherwise read.
- **Catalog.** The canonical declaration API text hashes a property as name, readonly and type,
  and leaves out `Create`. Neither `Literal` nor `FsCreateOverload.Literals` joins it, so W6
  needs no `CatalogCompatibility.ApiVersion` bump of its own (W5 took it to 8).
- **Expression access.** In an expression, the fully qualified
  `LiteralMemberLab.PhaseEvent.AT_TARGET` resolves to the interface's instance property
  (FS0806); a pattern resolves it to the literal.
  After `open LiteralMemberLab`, the unqualified `PhaseEvent.AT_TARGET` resolves to the literal
  in both positions. A module abbreviation reads the value under qualified access
  (`docs/.ai/footguns.md`).

Lab `literal-member-lab`: `PhaseEvent` (methods beside `NONE`/`CAPTURING_PHASE`/`AT_TARGET`/
`BUBBLING_PHASE`) gets `module PhaseEvent` and a `Create` writing the four constants;
`PointerPhaseEvent` gets no module and a `Create` writing the inherited four; `MethodSignal`,
`Tagged<T>` and `Phases` (literals alone, parameterless `Create`) carry their discriminant or
constants; `WritableSignal`'s writable `kind: "writable"` is a literal member like a readonly
one; `Quoted` pins the escaped marker, `-1` and `0.5`. `MB011` stands on all sixteen
literal members (the inherited four of `PointerPhaseEvent` included), `SP005` on the seven
`Create`s. In `NotLiterals`, the optional writable `mode` keeps `TR006`, the optional readonly
`optional` keeps `TR007`, `level: Level.High` keeps `TR004`, `flag: true` is `bool`, and all
four stay `Create` parameters under `Emit("$0")`. The RunGate matches `event.eventPhase` against
`PhaseEvent.AT_TARGET` (compiled to `event.eventPhase === 2`, with no `AT_TARGET` read), runs a
statement-position `Create`, reads the inherited constants off `PointerPhaseEvent.Create`, the
discriminants of `MethodSignal`, `WritableSignal` and `Tagged<string>`, the JSON of `Phases.Create()`, and the
escaped marker through a module abbreviation.

Measured at integration on `integ/w6` (2026-10-14), over W5, with the lead amendment:

| Golden | `TR007` | `TR006` | `MB011` | `SP005` | exact / ergonomic / widened / escape |
| --- | --- | --- | --- | --- | --- |
| `solid-js` | 54 → 0 | 6 → 1 | 0 → 59 | 0 → 5 | 69/180/92/1 → 69/188/84/1 |
| `animejs` | 18 → 0 | 20 → 20 | 0 → 18 | 0 → 0 | 126/190/26/6 → 126/190/26/6 |
| `@cloudflare/workers-types` | 0 → 0 | 221 → 77 | 0 → 144 | 0 → 121 | 417/1154/390/1 → 417/1259/285/1 |

The readonly-only branch measured 54 `MB011` on `solid-js`, every one a DOM `Node` constant
flattened into `H.JsxDevRuntime.Jsx.Result` and `Result.Item`, which gain `module Result =` and a
merged `module Item =`; the other five are writable discriminants. Every `animejs` hit is on
`DrawableSVGGeometry`, whose existing module now opens at its constants, so its nested string
enums move up beside them; it carries an index signature and so no `Create`.

Required string discriminants, readonly or writable, move to `MB011`, with each `Create`
dropping the `kind` parameter and writing it in the Emit. The readonly-only branch moved
`workers-types` `TR006` 228 → 196; the writable discriminants take it to 77. In the labs,
`TR006` falls in `generic-tag-lab` 23 → 0, `shared-tag-lab` 31 → 2,
`compiler-lib-ownership-lab` 6 → 0 (the `ClassDecoratorContext` family),
`catalog-alias-api-lab` 4 → 0, `projection-lab` 4 → 2, `literal-overload-lab` 4 → 3, and 2 or
1 → 0 in `nominal-lab`, `instantiation-operand-lab`, `catalog-recursive-json-lab`, `lab`,
`customization-lab`, `flags-lab`, `inherit-lab` and `subpath-lab`.

Core.TS and Node are regenerated under the T4 override and compile against each other:

| Binding | `TR007` | `MB011` | `TR006` | `SP005` | `SP001` / `SP003` | tiers |
| --- | --- | --- | --- | --- | --- | --- |
| Core.TS | 6,494 → 0 | 0 → 6,515 | 89 → 68 | 0 → 138 | 1,098/519 → 1,124/493 | 1302/2203/785/5 → 1302/2397/591/5 |
| Node | 86 → 0 | 0 → 182 | 673 → 577 | 0 → 70 | 1,120/215 → 1,122/213 | 775/2025/829/5 → 775/2094/760/5 |

In Core.TS, `Event.Create` writes `NONE`, `CAPTURING_PHASE`, `AT_TARGET` and `BUBBLING_PHASE`
rather than taking them, and 26 interfaces refused for the parameter budget (`UIEvent`,
`ProgressEvent` and `DOMException` among them) now carry a `Create`, raising `SP002` 884 → 988
and `TR055` 678 → 704 with their method parameters. The catalogue's API hashes are unchanged.

## Type parameters in scope (2026-10-13, W7 #138)

A hoisted declaration binds the owner's type parameters it reads, a namespace function is a
method, and a method type parameter keeps a constraint that inherits its use site's bound.

- **W7.1.** `freeTypeParams` reads a named declaration's type arguments in both of its forms.
  Written inside `interface Grid<T>`, `Grid<T>` is the declared form (`ownArguments`
  non-empty), and its arguments are the enclosing declaration's parameters; the walk used to
  skip them, so `BigInt64Array.Every.Predicate` reached `array: BigInt64Array<TArrayBuffer>`
  with nothing bound and widened it (`TR012`/`TR013`). The walk also follows a shadowed
  `AliasIdentity` operand (`NoInfer<T>`), the operand `indexedAccessRef` renders, so solid-js
  `CreateResource.Options2<'T>`/`Options4<'S, 'T>` bind `'T`. `bind-free-type-params` admits a
  hoisted anonymous type from a widened group, the rule `synthesize-anonymous` names it under:
  Node's typed-array delegates sit in `lib.*.d.ts` files.
- **W7.2.** No new code: `typeParamsOf` already writes only an interface or class constraint,
  so a delegate binding a union-constrained parameter (`'TArrayBuffer`, `ArrayBufferLike`) drops
  it with `TP002` as its owner does.
- **W7.3.** `ResolvedMember.ResolvesToFunction` records a `function` declaration, read through
  `getAliasedSymbol` for a re-exported namespace member (`export { f } from`, an `Alias`
  symbol). `Spec.emitsAsMethod` is a method symbol, or a required function whose type is a
  callback alone; a function+namespace merge keeps the property form. It decides in
  `Spec.shapeMembers` (namespace objects, `globalThis`), `Classes.shapeStatic` (a class's merged
  namespace) and `Anonymous.signatureShaped` (no delegate is minted for a method's type).
  TypeScript rejects assignment to a function declaration (TS2630), so the setter the property
  form carried was not part of the contract, and no finding records it. A `const` of a
  function type stays a property. `Customization/Semantics.fs` classifies members through
  `Spec.emitsAsMethodWith`, so a customization sees a namespace function as the method it
  renders (a property replacement fails with `customization/property-required`).
- **W7.4.** `appliedRefTo` accepts a type variable whose written constraint is the bound or
  inherits it (`satisfiesNominally`): `querySelectorAll<'E when 'E :> Element>: selectors:
  string -> NodeListOf<'E>`. A structural-only bound still widens with `TR027`.
- Codes taken: none.

Lab `type-scope-lab`: `Grid.Each.Visit<'T when 'T :> Shape>` and `Store.Each.Visit<'B>` (union
bound dropped, `TP002` on `Store` and the delegate); `Ns.identity<'T>`, `Ns.measure<'T when 'T
:> Shape>` and the re-exported `Ns.relabel` are methods, `Ns.version` and `Ns.format` (a generic
function-typed `const`) stay properties, as does `Ns.tagged` (a function+namespace merge) while
its namespace's `strip` is a method; `Registry.find<'T when 'T :> Shape>` is a static method;
`Container.queryAll<'E when 'E :> TaggedEntry>` returns `EntryList<'E>`; `Create.Options<'T>`
reads `JS.NoInfer<'T>`. Remaining: `TR013` on `NoInfer` (the alias declaration) and
`Ns.format(value)`, `TR027`/`TP006` on `Container.queryLike` (structural bound). The
`compiler-lib-ownership-lab` entry gains `declare function pick<T>`: `GlobalThis` writes every
global function as a method (`pick<'T>: items: 'T[] -> 'T`, `parseInt: string: string * ?radix:
float -> float`). Lab `lab` `Utils.clamp` is a method, its `Clamp` delegate is gone, and the
RunGate calls `utils.clamp(5.0, 0.0, 1.0)`; `test --run-gate` passes its 535 Fable checks.

| Golden | TR012 | TR013 | TR027 | TP006 | TP002 | exact / ergonomic / widened / escape |
| --- | --- | --- | --- | --- | --- | --- |
| `animejs` | 2 → 0 | 3 → 0 | 1 → 0 | 1 → 0 | 26 → 28 | 129/190/26/6 → 129/188/26/6 |
| `solid-js` | 0 | 23 → 15 | 0 | 15 → 15 | unchanged | 69/180/92/1 → 69/182/90/1 |
| Core.TS (scratch) | 190 → 0 | 6 → 2 | 152 → 0 | 154 → 2 | 3307 → 3499 | 2073/2203/785/5 → 2073/2346/632/5 |
| Node (scratch) | 102 → 0 | 250 → 19 | 5 → 0 | 452 → 454 | 374 → 538 | 1263/2032/830/5 → 1263/2136/678/5 |

In `animejs`, the `Utils`, `Easings`, `Svg`, `Text` and `Waapi` namespace functions become
methods (`TR055` 70 → 62, `SY006` 25 → 23, `MB001` 328 → 361 as optional parameters become
`?` arguments, `SP002` 34 → 48 as `Create` takes the methods as functions). `solid-js` moves only
through `NoInfer`: the 8 `CreateResource.Options2/4` sites. Its remaining `TR013` are rank-2
callback parameters, `Store<T> = T`, the `NoInfer` alias declarations and `Store.DEV.isWrappable`
(a property typed `typeof isWrappable`), and its `TP006` are conditional-erased parameters; none
is a W7 construct. The spec's acceptance line ("In solid-js and animejs, TR012, TR013, TR027 and
TP006 fall") is met for `animejs` only: in `solid-js`, `TP006` stays 15 and `TR012`/`TR027` start at
0; this gap is raised with the lead rather than amended. Node's remaining `TR013` are `Assert.Assert`/`AssertStrict` (members of a
`Pick<typeof assert, …>` mapped type, not function symbols), `AsyncLocalStorage.snapshot` and
`Stream.PipelineDestination`. In Node, functions of a function+namespace merge's namespace
(`Mkdtemp.__promisify__`) become overloaded methods, so `DO001` +6 settles overloads equal after
widening and `SP003` +3 refuses their `Create`; `Util.Types.isMap`/`isSet` gain `TP006`.

The pull-request regeneration (`test --goldens`) also moves `@cloudflare/workers-types`
(`WebAssembly.instantiate`/`validate` become methods, `SY006` 16 → 15, `SP002` 500 → 502;
`WorkflowEntrypoint.Run.Event<'T>` binds its owner's `'T`, `TP002` 95 → 96) and `type-fest`
(`ObservableLike.Subscribe.Observer<'ValueType>`, `TR013` 2 → 1).

Core.TS and Node are regenerated at integration on `integ/w7` (2026-10-14), over W5 and W6,
under the T4 override. Every W7 count equals the scratch measurement above; the tiers move from
1302/2397/591/5 to 1302/2540/438/5 in Core.TS and from 775/2094/760/5 to 775/2198/608/5 in Node.

## Key types for tag maps and value-of aliases (2026-10-13, W8 #139)

A key over a declared map and a value-of access are written with `Xantham.Fable.Core`'s key
types (§4.10 of `generator-type-mapping.md`).

- The checker resolves `keyof M` over a declared `M` to `M`'s key literals, so the bound of
  `K extends keyof HTMLElementTagNameMap` reaches Shape as a literal union. For a type parameter
  whose bound is a union or a string literal, Resolve reads the declaration's constraint node and,
  where it is a `keyof` type operator, records `getTypeFromTypeNode` of its operand as
  `TypeFacts.KeyOfOperand`. `keyCandidates` takes that operand ahead of an `Index` bound.
- `KeyBinding` carries the operand as an `FsTypeRef` (the in-scope `'T`, or the map's written
  reference) and `TypedKeyOf` the operand's id and the result's constraint. `indexedAccessRef`
  matches the access to its binding by operand id.
- `shapeSignature` partitions keys whose operand is out of scope: an operand that is not a type
  parameter and whose reference is an `FsNamed` or `FsApp` with no Widened or Escape finding is a
  map. The key reads `typekeyof<M,'E>` with `'E` at `M[K]` when a parameter or return reaches the
  access, `keyof<M>` otherwise (`TP.KeyOverDeclaredMap`, `TP011`, Ergonomic). Any other operand
  keeps the ordinary type parameter and its `TP002`. The result variable is `'E` (`E1`, `E2`, ... where
  taken), the spec's W8.1 spelling; a key over an in-scope operand keeps `'R` (`Spec.resultName`).
- `accessBound` gives `'E` the constraint of the generic parameter the access is applied at
  (`statedConstraintOf`, lifted out of `appliedRefTo`), where every member value of `M` satisfies
  it nominally. `appliedRefTo` passes a result variable bound with that constraint without
  `TR027`. Otherwise `'E` stays unconstrained and the argument widens to the constraint (`TR027`).
- `valueOfOperand` recognises `T[keyof T]`, and `T[K]` where `K`'s constraint and default are both
  `keyof T`. Over an in-scope `T`, after `indexedAccessValues` declines, the access reads
  `proptypelock<'T>` (`TR.ValueOfAsPropTypeLock`, `TR066`, Ergonomic). A key with no default
  stays `TR020`. `shape-aliases` binds the operand of such an alias body, which carries no alias
  arguments: `type ValueOf<'ObjectType> = proptypelock<'ObjectType>`.
- `proptypelock` joins `Naming.SupportBindings`, so a package declaring the name reaches the
  support package's as `JS.proptypelock`.
- Each map is a distinct F# parameter type, so a key-set overload set (`createElement` over
  `HTMLElementTagNameMap` and `HTMLElementDeprecatedTagNameMap`) survives `dedupe-overloads`
  rather than dropping under `DO005`. A call naming one of several key overloads states the map:
  `typekeyof<HTMLElementTagNameMap, _> _.canvas`.

Lab `tag-map-lab`: `Doc.createElement` reads `typekeyof<TagMap, 'E>` and
`typekeyof<DeprecatedTagMap, 'E>` beside the `string` overload; `getElementsByTagName<'E when 'E
:> Elem>` returns `ElemList<'E>` over `TagMap` and `SvgTagMap`; `createSvg` has the single key
overload; `hasTag` reads `keyof<TagMap>`; `findLoose`, over a map whose value does not inherit
`Elem`, keeps `'E` unconstrained and writes `ElemList<Elem>` under `TR027`. `ValueOf`,
`ValuesOf` and `valuesOf` read `proptypelock`; `Picked<T, K extends keyof T>` stays `obj`
(`TR020`). No `DO005`, `DO001` or `TR044`; `TP011` on the five `Doc` methods. The RunGate calls
each method and reads the key back: `createElement("canvas")`, `createElement("marquee")`,
`createSvg("circle")`, `getElementsByTagName("div")` and `hasTag("div")`. `map key binding` and
`shape typeRef` pin the binding, the constraint and its nominal guard, and the value-of forms.

`keyof-overload-lab` keeps both `find` overloads (`DO005` 1 → 0; `DO001` stays on `pick`).
`keyof-lab` `values` reads `proptypelock<'T>[]`. `indexed-access-lab` `addEventListener`,
`lastEvent` and `Registrar.on` read `typekeyof<WorkerEventMap, 'E>`, and `runModel`, whose access
is nested, reads `keyof<ModelMap>`; the fixture gains `anyWide(WideEventMap[keyof
WideEventMap])` to keep the footer-declared `U12` coverage `onWide` carried.

Measured goldens (finding counts):

| Golden | TR020 | TR044 | DO005 | TP002 | TR027 | TP011 | TR066 | SA002 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `type-fest` (selected) | 8 → 7 | 0 | 0 | 144 | 0 | 0 | 0 → 1 | 170 |
| `solid-js` (selected) | 70 | 0 | 0 | 157 | 0 | 0 | 0 | 10 |
| `@cloudflare/workers-types` (T6) | 5 | 8 → 6 | 0 | 95 → 93 | 2 | 0 → 2 | 0 | — |
| `animejs` (T6) | 17 → 0 | 0 | 13 → 0 | 26 → 6 | 1 → 9 | 0 → 20 | 0 | — |
| Core.TS (scratch, T4) | 2 | 1,209 → 5 | 17 → 0 | 3,307 → 137 | 152 | 0 → 3,170 | 0 | — |

`solid-js` is byte-identical: its `Store` accesses are bounded by its own conditional `KeyOf`
alias and `T[number]`, neither a `keyof` over a declared type. type-fest's 170 `SA002` are
type-level computations with no Core counterpart. `workers-types`' `addEventListener` reads
`<'E when 'E :> Event>` over `WorkerGlobalScopeEventMap`. `animejs` flattens DOM element members
in: its `getElementsByTagName` and `querySelectorAll` overloads cannot prove the lib map's values
inherit `Element`/`Node`, so `'E` stays unconstrained and the collection argument widens
(`TR027` +8). Symbol tiers: `type-fest` 26/65/204/2 → 26/66/203/2, `workers-types` ergonomic
1,161 → 1,163 and widened 390 → 388.

Core.TS was generated into scratch only (T4) and compiled there: 193 `typekeyof` members, among
them two `createElement` key overloads and four each on `querySelector` and `getElementsByTagName`, and 83
`addEventListener<'E>`. At integration on `integ/w8` (2026-10-14), over W5–W7, Core.TS and Node are
regenerated under the T4 override: Core.TS's TP011 0 → 3,170, TP002 3,499 → 329, TR044 1,209 → 5
and DO005 17 → 0 (tiers 1302/2540/438/5 → 1302/2688/290/5); `CatalogCompatibility.ApiVersion`
stays 8, the wave's single bump (W5). Node's findings and tiers do not move; 1,416 doc lines go,
as an overload, a literal-keyed method (W5) included, carries only its own JSDoc's tags.

### Review follow-up (2026-10-14)

- **`PropertyRecord`.** Resolve reads a member-less mapped type's `MappedType` node into
  `TypeFacts.MappedRecord`: the operand of a `keyof` constraint and the value node, where the
  mapping has no modifiers, no `as` clause and a value that does not mention the key. With the
  operand in scope, `objectRef` writes `PropertyRecord<'T, V>` (`propertyRecordRef`,
  `TR.MappedAsPropertyRecord`, `TR067`, Ergonomic). A value that reads the key (`{ [P in keyof
  T]: T[P] }`) stays a phantom (`SA002`).
- **Catalogued maps.** A map reached from the compiler lib or a dependency derives no members.
  Resolve reads the property types of `KeyOfOperand` into `TypeFacts.KeyOfValues`, and
  `accessBound` decides the bound over those where the operand has no members.
- **Overload tags.** `getJsDocTags` merges every overload's tags under the one symbol. Resolve
  reads each overload's own leading JSDoc into `ResolvedSignature.OwnDoc` (only where a type has
  several signatures); `signatureTags` keeps the tags that JSDoc writes, and an undocumented
  overload keeps the shared tags other than `@deprecated`. Applied to interface methods,
  symbol-keyed methods, class statics and exported functions.

`tag-map-lab` gains `Stringified` (`PropertyRecord<'T, string>`), its negative `Mirrored`
(phantom), and `Doc.libElements` over `HTMLElementTagNameMap` (`<'E when 'E :>
Fable.Core.TS.Dom.Element>`, returning `HTMLCollectionOf<'E>`); only the `DeprecatedTagMap`
overload of `createElement` carries `@deprecated`.

| Golden | SA002 | TR067 | TR027 | `@deprecated` lines |
| --- | --- | --- | --- | --- |
| `type-fest` (selected) | 170 → 169 | 0 → 1 | 0 | — |
| `solid-js` (selected) | 10 | 0 | 0 | unchanged (byte-identical) |
| `keyof-lab` (T6) | 4 → 3 (`Flags<'T>`) | 0 → 1 | — | — |
| `animejs` (T6) | — | 0 | 9 → 1 | 22 → 10 |
| Core.TS (scratch, T4) | — | 0 | 152 | 714 (committed) → 540 |

`compiler-lib-ownership-lab` drops two `@param` lines from `Date.toLocaleString: unit -> string`,
whose own declaration documents no parameters. The W8 acceptance line (SA002, TR020, TR044 and
DO005 falling in `type-fest` and `solid-js`) remains unmet in `solid-js` and is with the lead.

## Conditionals at a bounded check type (2026-10-13, W9 #140, experiment)

`resolveConditionalConstraints` in `xantham.json` (boolean, default `false`, experimental)
writes a conditional deferred over a bounded type parameter as its value at that bound.
`InstanceType<Request>` with `Request extends typeof IncomingMessage` reads `IncomingMessage`,
under `TR.ConditionalResolvedToConstraint` (`TR068`, Widened) in place of `TR045`. Unset,
output is unchanged. `src/Xantham.Fable.Node/xantham.json` sets it (#151); Core.TS does not. The full experiment record, with every measurement, is
[`2026-10-13-w9-early-resolve-findings.md`](2026-10-13-w9-early-resolve-findings.md).

- Resolve's conditional derivation records `ConditionalFacts.Constraint` from
  `getBaseConstraintOfType`. It does so when no branch is taken, or the taken branch is an
  `infer` variable (a type parameter declared under an `InferType` node), and the peeled
  check type is a type parameter that
  `getConstraintOfTypeParameter` bounds. The constraint joins the frontier through the
  `conditional-constraint` channel and the catalogue's dependency edges.
- An alias's own declaration (the alias's declared type equals the conditional) stays
  deferred. So does a constraint that is the conditional itself, either branch, `any`,
  `unknown`, `never`, a conditional, or a union other than `boolean`. Over a union bound the
  distributive constraint is the per-member branch union, and without a distributive
  constraint the checker's answer is the union of both branches. §4.11 rejects both.
- `Spec.conditionalRef` tries the taken branch, then the constraint. A constraint that maps to
  `obj` reports `TR045`.
- The flag is part of `DeclarationCatalog.profile` when set, and only then.

W9.1's premise does not hold as written. A concrete application (`InstanceType<typeof
IncomingMessage>`) is resolved by the checker before Resolve reads it. All 130 `InstanceType`
`TR045` findings in Node are applications over a bounded parameter. The read therefore sits in
Resolve, not in a separate pass before Resolve.

`early-resolve-lab` (flag on) pins `TR068` at `RequestListener(req)`,
`Server(requestListener)(req)`, `Server.on(listener)(req)`,
`Exports.createServer(listener)(req)`, `Exports.instance(value)` (a taken `infer` branch) and
`Exports.flag(value)` (a bound whose value is `boolean`, written `bool`), plus their rendered
lines; the last two stay `TR045` with the flag off. It also pins `TR045` at `Divergent`,
`Instance` (an alias's own body), `Exports.divergent(value)` (unbounded),
`Exports.returns(value)` (bound instantiates to `any`), `Exports.either(value)` (union bound)
and `Exports.same(value)` (a bound whose value is one of its own branches). The three `TR018` intersections over `InstanceType<Response>` are pinned unchanged,
and `ConcreteListener` carries no finding. Its baseline testcase pins every `InstanceType` site
at `TR045` with the flag off. `shape typeRef`, `generator config` and `catalog library ambient
authentication` pin the rung, the loader and the profile.

The selected goldens are unchanged: `type-fest` and `solid-js` regenerated byte-identical.
Measured in scratch with the flag on:

- `type-fest`: `TR045` 154 → 152 (two inline conditionals inside `Replace` and
  `StringRepeat`), with an identical binding.
- `solid-js`: unchanged, because every conditional is over an unbounded parameter.
- `@types/node`: `TR045` 224 → 93 and `TR068` 160 (159 `InstanceType` over 90 symbols, the
  surplus being nested applications). 101 binding lines changed, and the binding compiles.
- Core.TS: unchanged.

Tiers, table size and wall time did not move. Core.TS and Node are not regenerated, since
neither configuration sets the flag. Enabling it for Node is the recommendation, left to
integration.

## Tagged-union arms (2026-10-15, #158)

Both folds compare members with `Spec.sameShape`: one id, or plain anonymous objects, unions and
applications of one target that pair structurally. The checker gives every inline object literal
its own id, so the earlier id comparison dropped `owner: { name: string }` from a tagged fold and
refused the exclusive-arm fold over a shared inline `meta`. An exclusive-folded union's arm
declarations nest under the folded interface (`Choice.Meta`).

`detect-tagged-unions` writes the alias's JSDoc on the DU and each arm member's JSDoc, tags and
literal value on its case field; render writes them as one `<param>` per field, empty where a
field has no text (FS3390 rejects a case documenting some fields). A case with an optional or a
literal field has a `Create<Case>` static member, the arm interface's `Create` moved onto the DU:
an Emit writing the tag and literal fields, the remaining fields bound as parameters (`DT005`);
where `Create<Case>` is a case of the same union, the case has none (`DT006`). A same-named
member is FS0023. An anonymous arm's own declarations nest under `<Union>.Cases.<Case>`
(`synthesize-anonymous` reads the case names from `Spec.taggedCaseNames`, the source
`detect-tagged-unions` names cases from); a submodule spelled like the case itself is
unreachable from expressions (FS0039), and a union with a case named `Cases` keeps the arm-name
path. `drop-orphan-delegates` then prunes each unread interface named for an
anonymous arm the DU carries whole (`DD002`); an arm of a folded case with a field the case
omits stays declared (`DT007`), and such a case has `Create<Case>` only where its fields alone
build one of its arms. It then prunes every unread generator-minted declaration under
an arm root - a DU's `Cases` module, a dropped arm's path, an exclusive-folded interface -
transitively (`DD003`); named arms, erased fallbacks, arms read elsewhere and author-written
declarations under an arm root (a namespace member merged by name, `T1.Extra`) stay. The
catalogue API hash is unchanged: no `CatalogCompatibility.current` bump. A tagged union's API
hash covers exactly its tag, cases and field types; docs and `Create<Case>` members sit outside
it. Catalogue identity keys come from roles, declaration handles and
arguments, and a declaration's F# name enters a key only through the `generated:` fallback for a
declaration without a type identity. The declarations nested under `<Union>.Cases.<Case>` all
carry `type` or `literal-union` roles, so the rename leaves `identityVersion` unchanged. A newer
producer owns fewer declarations, as orphan delegates already do, and a consumer takes every
name from the catalogue of the producer it read. Core.TS regenerated with an unchanged semantic
catalogue.

Measured over the corpus (findings against the pre-wave baseline): `DD002` 2 in
`@cloudflare/workers-types`, 2 in `catalog-alias-api-lab`, 2 in `catalog-recursive-json-lab`,
19 in `generic-tag-lab`, 10 in `shared-tag-lab` and 20 in `tagged-arm-lab`; `DD003` 2 and
`DT006` 1, both in `tagged-arm-lab`; `DT005` 20 in `@cloudflare/workers-types`, 1 in
`catalog-alias-api-lab`, 1 in `projection-lab`, 4 in `shared-tag-lab` and 7 in `tagged-arm-lab`;
`DT007` 1 in `@cloudflare/workers-types` (`TailStream` `Event.Item.Log`, whose `CreateLog` built
no arm), 2 in `shared-tag-lab` and 5 in `tagged-arm-lab`. Ergonomic
tiers fall by the dropped nested arms. `catalog-recursive-json-lab`'s shipped group held only its
two arm interfaces and is no longer written (`GE001` → `GE002`). Node drops 13 anonymous
`TestEvent` arms (`DD002` 13) and gains 3 `Create<Case>` members; Core.TS's binding is unchanged.

## Shared tag values as case tests (2026-10-20, D1, #158)

Supersedes the fold above. Every arm of a tagged union is a case of its own, and arms sharing a
tag value carry a `[<CaseTest>]` from the Fable fork's `Fable.Core`
(`tools/fable-fork/fork.json`): Fable matches the tag, then the test. `Spec.separateArms` picks one
test form per shared value, in TypeScript's narrowing order: a second literal discriminant
(`$0.ok === true`; an optional arm also matches an absent property), a required member no other
arm of the value declares (`'value' in $0`, with at most one arm matching by lacking the others'),
or a member's `typeof` class. Arms no form separates leave the union an erased union (`DT003`,
reworded); a union whose arms all share one value is a DU once they separate (`Onset`, workers
`TailStream` `Log`). Each separated value is reported once (`DT008`). `DT004` retires (row kept);
`DT007` is removed before release. Case names keep `uniqueCaseNames` (`Done`, `Done2`).

The `typeof` strategy requires positive evidence for one runtime class. TypeScript `object`
and structural object types without call signatures still admit callable values, so neither
proves `typeof === "object"`. They retain the erased-union fallback unless another strategy
separates the arms. `tagged-arm-lab` pins both refusals alongside the string/number separation.

A required boolean-literal case field is now a literal field, written by `Create<Case>` like a
string or number one, since it can be what separates two arms (`Task.CreateDone2` writes
`ok: false`). Interface `Create` keeps its string and number literals only. An arm interface
drops only where its case carries it whole (`Spec.caseCarriesArmWhole`): a symbol-keyed member or
an index signature keeps it declared (I4). A delegate read only by dropped declarations other than
delegates grades `DD003`, not `DD001`. `Spec.sameShape` compares unions by mutual inclusion, so
the exclusive-arm fold no longer depends on arm order (I1).

Measured over the goldens against the #158 head: `DT004` 8 → 0 and `DT007` 8 → 0 (1 each in
`@cloudflare/workers-types`); `DT008` 0 → 12 (`shared-tag-lab` 4, `tagged-arm-lab` 6,
`@cloudflare/workers-types` 2); `DT003` 2 → 1 (`Onset` and `Log` separate; the new `Mirror`
negative refuses); `DT002` 56 → 62; `DT005` 33 → 48; `DD002` 55 → 76 as every separated arm drops.
`catalog-recursive-json-lab` ships its group again (`GE001`, one declaration, `Payload`).
Core.TS's binding and Node are unchanged.

## Arm interfaces carried whole (2026-10-20, InlineInterface)

A case whose arm has a declared name, or whose arm carries more than its case's fields (a method,
an index signature), is a case of one `[<InlineInterface>]` field, the arm's interface, from the
Fable fork's `Fable.Core` (`tools/fable-fork/fork.json`): `Ping of Ping` compiles as the `Ping`
value with the tag set, and a field read returns the union value itself (`DT009`). An anonymous
plain-data arm keeps its fields. `Spec.caseInlinesArm` excludes an arm with a call or construct
signature and a class instance (`Spec.isClassInstance`: an ambient class, a class symbol, an
interface merged with a variable such as a DOM host interface, or an instantiation of one):
the fork constructs an inlined case by copying the value onto its prototype, which loses a
function's callability and a host object's internal state. Such a union stays an erased union
(`DT001`). `TypeFacts.SymbolFlags` carries the flags of the type's own symbol for that test. A
named arm sharing its tag value takes its case name from the arm (`Done`, `Aborted`), not
`uniqueCaseNames`' numbered tag. A tag-only named arm is no longer a bare case: it carries its
interface (`Queue of QueueInfo`). Only Fable validates an `[<InlineInterface>]` case
(`footguns.md`); the projection companions, catalogue consumers and width-admission scratch
projects now pin the fork's `Fable.Core`, checked by `fable-fork.fsx -- check`.

Measured over the goldens against `3b40e89a`: `DT009` 0 → 135 (`@cloudflare/workers-types` 75,
`shared-tag-lab` 25, `compiler-lib-ownership-lab` 11, `inline-arm-lab` 9, `tagged-arm-lab` 5, 2
each in `generic-tag-lab`, `instantiation-operand-lab`, `nominal-lab`, `phase-b-lab`,
`projection-lab`); `DT005` 37 → 16 (workers 24 → 5) as inlined cases need no `Create<Case>`;
workers `TR032` 3565 → 3529 and tiers 417/1257/285/1 → 427/1251/282/1. `compiler-lib-ownership-lab`'s
`ClassMemberDecoratorContext` and `DecoratorContext` become DUs (`DT001` 2 → 0, `DT002` 0 → 2);
their `unknown` arguments now report `TR009` per case (20 → 36), the same widening. Core.TS
regenerated: the two decorator contexts become DUs and `PromiseSettledResult` carries
`PromiseFulfilledResult<'T>` and `PromiseRejectedResult` whole, 13 inlined cases in all;
`FileSystemDirectoryHandle | FileSystemFileHandle` stays `U2`. The changed goldens and Core.TS
compile under Fable.

## Exclusive-arm fold arms and signatures, value-side names (2026-10-15, #158 follow-up)

`shape-interfaces` removes only an exclusive fold's anonymous arms (`Spec.isAnonymousArm`) from
`DeclNames`. A source-named arm stays declared, under the name `name-exports` gives every
namespace member at this point (`Contested.Left` beside a top-level `Left`, bare `Right`
otherwise). The fold no longer leaves references to an undeclared arm (FS0039), and no longer
widens a parameter typed by one (TR023). `exclusive-arms-lab` declares `QueryArm`, `MessagesArm`,
`ImageArm` and `SnapshotArm` again; no other golden moved. The namespace-member harvest plan
(`2026-10-15-namespace-member-harvest.md`, R1) later nests every namespace member, which re-pins
pair four to `Contested.Right`.

The exclusive fold also requires every arm to agree on a shared member's readonly; otherwise the
union stays `U2`, so the folded member's setter cannot depend on which arm comes first.

Where the bare name is taken, `synthesize-anonymous` names an enum's value side `<Stem>Enum`, a
namespace merged with a type `<Stem>Namespace`, and a variable merged with a type `<Stem>Value`
(`SY007`), instead of the first free number. The number had also counted arms that
`drop-orphan-delegates` later prunes (`Shape4` beside a DU whose `Shape2`/`Shape3` were
dropped). The shapes an enum's or namespace's values own nest under the source namespace
(`nestBases[value] = root`): `Point.Defaults`, or `Point.Anchor2` where the namespace type
`Point.Anchor` holds the name. Renamed in the goldens: `case-sanitise-lab` `KindEnum`,
`CollideEnum`, `PlainEnum`; `literal-member-lab` `LevelEnum`. This change leaves Core.TS and Node
unchanged.

`shape-exports` binds a callable value export as a method only where its value type carries
call signatures alone. A function merged with a namespace holding values, or a callable `const`
carrying properties, binds as that hybrid object (`static member stamp: Stamp`) and is called
through `Invoke`. Its value side is role-named like any merged value side (`MergedNamespace`,
`SY007`; Task D's rule now excludes classes only). `synthesize-paramobjects` refuses `Create` on
any declaration whose type is callable (SP003 "the declaration is callable…"), including one
whose call is the SI001 loss: no object literal is callable. Node: ~76 function+namespace
exports move to the property form, and 64 callable hybrids lose `Create`; Core.TS loses 2.

## Catalogue identity of an alias over an instantiated mapped type (2026-10-20)

An instantiated mapped type with unresolved members (`{ [P in keyof T]?: U[] }`) has no
catalogue identity of its own: its only key was its declaration, which every instantiation shares,
so aliases applying one object alias to different arguments (`ZodFlattenedError<T, U>`,
`inferFlattenedErrors<T, U> = $ZodFlattenedError<output<T>, U>`) claimed one identity. The
catalogue then redirected the later aliases to the first, writing `inferFlattenedErrors<'T, 'U> =
ZodFlattenedError<'T, 'U>`, or refused a pair whose constraints differ (zod's `zod-v4` owner in
FSharp.CloudEdge). Such an alias is now keyed by its own declaration. A mapped type `MappedRecord`
describes keeps its structural key. `IdentityVersion` 6 → 7: Core.TS's `Es.Omit` identity moves;
its binding is unchanged. `catalog-mapped-alias-lab` pins the three aliases.

## TypeScript 7.1.0-beta (2026-10-21)

The compiler pin moves to `typescript` 7.1.0-beta (`3e9236361fa9`), binary AST protocol 9. Two
checker changes reach the generator.

A symbol is a reference: a file-owned symbol (a declaration's, a module's and its exports') carries
its file and an id unique within it, a snapshot-owned one a project and an id offset by 2^48.
Every symbol-keyed table keys by `SymbolKey`, the owning file's node id and an `int64` id, and every
symbol request sends `SymbolResponse.Reference`. The ids are handles the server assigns as symbols
are first requested, so two sessions over one program number them differently; no output depends
on them, and tests comparing models across sessions normalise `SourceSymbolId`.

A conditional's check type is a type distinct from the declaration's parameter it was written as,
sharing that parameter's symbol. `Spec.typeVarOf` binds a type parameter by type id, then by
symbol, for a reference and for an operand read off another type's facts (`keyof`, value-of and
property-record operands). Without it `Proven<T>` and `Inhabited<T>` deferred to an erased phantom
and type-fest's `RequiredKeysOf`/`WritableKeysOf` lost their `keyof<'Type>`; with it both labs and
type-fest match their 7.0 goldens.

The server now encodes a non-finite numeric literal type: type-fest's `PositiveInfinity` and
`NegativeInfinity` widen to `float` (`TR007`) where 7.0 refused the response (`RE001`, `AC001`).

Measured over the goldens against `origin/master`: type-fest `RE001` 2 → 0, `AC001` 2 → 0, `TR007`
0 → 2, tiers 27/66/202/2 → 27/66/204/0. The rest is the 7.1 lib: `Error.isError(error: unknown)`,
inherited as a static by every `Error` subclass (`TR009` +1 in `ambient-module-lab` and
`orphan-callback-lab`, +3 in `error-class-lab`), and `setHTML`/`setHTMLUnsafe` options (animejs
`MB001` and `TR032` +2). Core.TS gains the 7.1 lib's declarations (Web Serial, WebTransport,
`JSON.rawJSON`, GPU usage flags) and its catalogue records the new compiler in every entry; Node is
unchanged.

Validation follow-up (2026-10-22): filesystem callbacks use the 7.1 tagged replies from
`dist/api/fsCallbacks.js`: value, missing and useOS. The old Boolean response terminated the
server before the mailbox disposal test reached its read callback. Callback encoding and
live virtual-file tests now cover the revised protocol.

The JavaScript compiler tool is Fickl 0.5.1 (Fable 5.23.0), pinned in the root and standalone
support-helper manifests. The build and Partas gates invoke `dotnet fickl`; Fable.Core remains
5.2.0. All 1,489 generator tests and 105 Wire tests passed across the validation runs. The new
compiler passed 564 runtime checks, Partas JSX and source/DLL acceptance, its Vitest check,
and all five standalone support-helper Node tests. Build-command verification also passed.

## Tagged-arm integration with TypeScript beta and Fickl (2026-10-22)

Merged develop after PR #162 into tagged-inline-interface. Symbol parents retain the beta
SymbolKey identity alongside the arm symbol flags used to reject class and host instances.
Core.TS, Node and fixture goldens are regenerated from the composed generator and compiler pin.

The user directed removal of the private Fable fork. Runtime compilation uses Fickl 0.5.1;
all Fable.Core references use released 5.5.0, which supplies CaseTest. With explicit user
approval, Xantham.Fable.Core temporarily carries the upstream InlineInterface attribute source
in the Fickl.Core namespace until the user publishes Fickl.Core. Render emits the qualified
attribute name. The private fork build script, pin and publication blocker are removed.

This integration preserves the existing arm-admission rules and copying behavior. The proposed
identity-preserving construction and subsequent frontier work remain separate decisions.

Compared with the tagged-inline-interface parent, Core.TS tiers (exact/ergonomic/widened/escape)
move from 1319/2676/277/5 to 1341/2725/291/5 with the beta library additions. Regenerating Node
with the branch's existing InlineInterface support moves 815/2158/595/5 to 817/2156/595/5;
DNS records and network-interface unions now carry their named interface arms. Type-fest moves
27/66/202/2 to 27/66/204/0 through the beta's non-finite numeric literal support. Other fixture
tier counts are unchanged. All 139 golden-writing tests passed.

PR #162 was squash-merged into develop as `40a70f03`. Its final Wire follow-up widens the
Unknown, Undefined and Arguments well-known-symbol responses to int64, matching the 7.1
protocol; all 106 Wire tests and all 15 CI checks passed before that merge. The temporary beta
handover is retired here.

Integration validation: the full `build.fsx -- test --run-gate` pipeline passed, with 1,512
generator tests, 590 runtime checks, Partas JSX and source/DLL consumption, and its Vitest
check. The standalone Wire suite passed all 106 tests with no skips. Build-command checks
also passed. These gates used Fickl 0.5.1. During validation, Fickl 0.5.2 appeared on NuGet,
but its Windows platform package was not yet restorable. Fickl.Core 0.5.2 contains
Fable.Core.dll without InlineInterface; the temporary source remains necessary.

Follow-up: Fickl 0.5.3 and its Windows platform package restore from NuGet. Fickl.Core 0.5.3
ships Fickl.Core.dll, InlineInterface and the compiler's F# sources, with a dependency on public
Fable.Core 5.5.0. The support project now depends on this package and the temporary attribute
source is removed. Root and standalone compiler manifests use 0.5.3; generated attribute
names and arm-construction semantics are unchanged.

The restored 0.5.3 packages passed the solution compile gates, 11 selected session tests,
590 runtime checks, Partas JSX and source/DLL consumption, and Partas Vitest. The support
NuGet package was packed and inspected: Fickl.Core is a transitive dependency for both target
frameworks and the temporary source is absent. The unchanged generator suite was not repeated
after this dependency-only follow-up.

## PR #160 resync with develop (2026-10-22)

Merged develop through `74308014`, retaining the tagged-arm and shared-tag CaseTest behavior.
The compiler tool is Fickl 0.5.3 and Fable.Core is public 5.5.0, which supplies CaseTest.
The private fork pin, build helper and publication blocker are removed. InlineInterface belongs
to the later branch and is not introduced into this PR. Core.TS, Node and fixture goldens are
regenerated against this branch and TypeScript 7.1.0-beta.

Core.TS exact/ergonomic/widened/escape counts move 1302/2688/290/5 to 1324/2737/304/5;
Node remains 775/2185/608/5 with no generated binding changes. Type-fest moves 27/66/202/2
to 27/66/204/0 through the beta's non-finite literal support. Other fixture tier counts are
unchanged. All 136 golden-writing tests passed. The official `gh stack` extension tracks the
dependent PRs, and validation workflows now include feature-base PRs. All 48 CI-policy and
golden-tooling tests passed.

Full validation passed: 1,497 generator tests and 105 Wire tests (one ignored in each suite),
579 runtime checks, and the Partas JSX, source/DLL and Vitest gates.
## PR #161 resync onto #160 (2026-10-22)

The official gh stack extension restacks this slice onto #160 at 3f860134.
It inherits TypeScript 7.1.0-beta, public Fable.Core 5.5.0 and Fickl 0.5.3.
The source-named arms, readonly agreement, role-named value sides and callable hybrids
are retained. Generated catalogue conflicts are resolved by regeneration.

Against the previous #161 head, Core.TS tiers move 1318/2677/277/5 to 1340/2726/291/5
through the beta library additions. Node stays 815/2158/595/5 with identical bindings.
Type-fest moves 27/66/202/2 to 27/66/204/0; other fixture tier counts are unchanged.
All 137 golden-writing tests passed without further fixture changes.

Full validation passed: 1,504 generator tests and 105 Wire tests (one ignored in each suite),
586 runtime checks, and the Partas JSX, source/DLL and Vitest gates. Resync reviews found
no introduced spec or standards issues.


## InlineInterface stack sync (2026-10-22)

Final review (2026-10-23): customization validation projects explicitly restore Fickl.Core
0.5.3 alongside Fable.Core 5.5.0. Their raw support-assembly references do not restore transitive
NuGet dependencies, and generated InlineInterface cases require the attribute assembly. The
projection-companion compile tests reproduce the missing-reference failure and gate the fix.

Merged the resynced #161 to retain the earlier integration merge resolutions.
Source, bindings, compiler pins and build inputs are unchanged from bc537272; the update
adds the stack workflow policy and validation coverage for feature-base pull requests.
Each slice is validated in turn; upper-layer CI does not gate assessment of a lower slice.

Core.TS and Node regeneration reproduced their binding code and tier counts. Core.TS
metadata refreshes the generator binary fingerprint and one session-assigned symbol id in
a Date finding. All 31 selected workflow, golden-provenance and catalogue-portability tests
passed. The unchanged generator/runtime suites were not repeated for this workflow-only sync.

## Typed erased exclusive arms (2026-10-23)

The approved D2 representation carries existing typed arms in an erased DU, preserving callable,
constructor and object identity. `detect-erased-object-unions` follows tagged-union detection,
before the data-interface fold. A required property that provably excludes `undefined`
distinguishes an arm only when all other arms explicitly declare it optional-never/undefined.
`Fickl.Core.CaseTest` emits a defined-value predicate; property presence alone is insufficient.
An intersection proves this only through a constituent that excludes undefined. Literal-only
callable discrimination remains a separate increment. Ambiguous callable/constructable unions
retain the ordinary union fallback and cannot enter the plain-data fold.

Each payload retains its arm declaration. Shared `Invoke` members require matching source
signatures, including alpha-equivalent generic constraints; the usual overload pass removes
collisions after shaping. Naming recovers generic alias applications, and declaration traversals
retain payload and call references through arity repair, ordering, orphan pruning and catalogue
qualification. `EO001` records the representation. The catalogue API remains version 8: affected
per-declaration hashes change and always authenticate reuse, so old interface/alias forms fail
individually without requiring global Core.TS regeneration.

The prerequisite #165 adopted released Fickl/Fickl.Core 0.5.4 and merged after all CI checks
passed. The generator change is #166. The surgical `erased-exclusive-lab` covers explicit
undefined, shared/different calls, constructors, generic applications and methods, escaped
property names, ambiguous fallback, identity, and a compiled catalogue consumer. Independent
reviews found overload and generic-equivalence gaps, which the final tests cover.

Full validation passed 1,520 generator and 105 Wire tests (one ignored in each suite), 617
runtime checks and the Partas gate. The final intersection and catalogue compilation checks
passed in all nine targeted tests, in both write and check phases. All 140 golden regeneration
tests passed. Among existing fixtures, only `tagged-arm-lab` changes: its `T1` retains the
previously dropped callable arm. Exact/ergonomic/widened/escape counts move 16/88/5/0 to
17/89/5/0 as both arm declarations survive. No existing large golden changes.

## Named generic callable references (2026-10-24)

`objectRef` enters the pure-callback fallback only when no existing named instantiation is
available. A named application follows `appliedRefTo`, retaining declaration constraints and
arity checks. This preserves delegate/interface identity and the source type arguments without
introducing a new callback representation. Array and tuple precedence and unnamed callbacks
retain their existing handling.

The generic-callable-reference lab proves transfers through a tuple, return and input, plus
consumer-created callbacks and a compiled catalogue consumer. The unit test first reproduced
the incorrect fresh delegate; the live lab reproduced a separate `Func` at every use.
Five focused tests and 620 runtime checks passed, along with the Partas gate. All 141 golden
regeneration tests passed. Independent spec and standards reviews found no issues.

Only SolidJS changes among existing goldens: `StoreReturn<'T>` carries `SetStoreFunction<'T>`
instead of `Action<obj, obj, obj, obj, obj, obj, obj, obj>`. Exact/ergonomic/widened/escape counts
move 70/189/82/1 to 71/189/81/1. TR013 falls 15→7, TR031 13→12, TR020 70→60, TR035 35→27,
and TR045 56→53 because the tuple no longer re-expands and widens the callable's generic
overloads. Animejs and every other existing golden remain unchanged.

Regenerated Core.TS stream interfaces also retain their named callback references; the
Transformer and UnderlyingSink catalogue API hashes change with their nominal delegate types.
The full gate passed 1,526 generator tests and 105 Wire tests (one ignored in each suite),
620 runtime checks and the Partas gate. The generated Core.TS diff received an additional
independent review with no actionable findings.

## Alias declaration binders (2026-10-24)

Resolve reads exported type-alias parameters from the typed declaration AST before walking
the type table. ExportTypeIds.AliasParameters retains their ordered IDs on the owning symbol;
parameter responses enter the ordinary resolver frontier. The checker may erase this binder
list from the alias body, so it must not be attached to a shared TypeFacts record. ShapeAliases
uses each alias export's binders, including secondary aliases of already-shaped declarations.
Merged value/type symbols select the actual TypeAliasDeclaration handle. Missing declaration
metadata reports the existing unresolved-facet finding.

Unused binders retain the existing phantom representation and its reported limitation, with
the known carrier preserved instead of needlessly becoming obj. The identity-alias lab covers
identity and NoInfer aliases, shared literal bodies in both declaration orders, an interface
carrier, merged declarations and reordered tuple binders. Three focused tests pass, including
direct source and catalogue-consumer compilation. Secondary phantom aliases use their export's
source order for module ownership, as abbreviations already do; their carrier cannot determine
where the alias belongs. Direct pass tests cover binder ownership and compiler-library placement.
All 142 golden regeneration tests pass. The full gate passes 1,533 generator tests and 105 Wire
tests (one ignored in each suite), 620 runtime checks and the Partas gate using released Fickl.
Core.TS was regenerated to reflect the restored alias arities and narrower phantom carriers;
its catalogue and source changes passed independent review. SolidJS moves from 71/189/81/1 to
72/190/79/1 exact/ergonomic/widened/escape. Type-fest's widened count rises by one because retaining
ValueOf's second parameter now exposes the existing phantom limitation instead of silently
discarding that parameter. Deferred computations remain lossy; this change preserves their
binders and known carriers without claiming to implement TypeScript's type-level evaluation.

## Invocation-owned callable parameters (Fickl 0.6.0)

A pure callable with one generic signature is an FsInterfaceDecl marked Callable. Its outer
parameters remain on the interface; signature parameters remain on Invoke. Anonymous callback
contracts follow the same route. Callable value exports remain properties, including typeof
aliases sharing a function declaration's checker ID; actual function declarations stay methods.
Render emits Fickl.Core.CallableInterface, and the catalogue records that representation.

Inherited member comparison substitutes applied base arguments and alpha-renames method binders
before comparison. Only identical resolved signatures collapse. Redundant callable bases are
removed only when another base reaches the exact same instantiated reference. This retains
subtype relations and avoids capturing an outer type parameter with a method parameter's name.

The permanent runtime gate adds 28 checks for construction, identity, repeated generic calls,
anonymous contracts, inherited assignability and binder collisions. All 13 Fickl 0.6.0 packages
are publicly available. A fresh public-only tool restore passes six callable fixture tests,
all 648 runtime checks and Partas; cached compiler packages match fresh nuget.org downloads.
The full public-feed .NET run passes 1,543 generator tests and 105 Wire tests, including fresh
Core.TS package consumers. All 143 golden regeneration tests pass. Core.TS and corpus changes
received independent review. The corrected callable-overload, setter and type-scope assertions
verify invocation-owned binders and first-class callable values.

## Namespace member contracts (2026-10-24)

Harvest includes type members exported by namespaces, including unreached declarations and
package-owned global namespaces. Each occurrence retains its declaring namespace separately
from the resolved target symbol. Declaration names and references use the namespace path;
position-derived names retain separate provenance for orphan pruning. Catalogue API contract
9 requires regenerated producers and consumers for these changed public names.

Single-signature callbacks that recur through abbreviation bodies use Fickl callable
interfaces. Their signature edges, union members and type arguments determine recursion;
ordinary object members terminate the traversal at a nominal boundary. Each reachable type
is visited once per classification. Source binders remain on the callable interface and its
recursive references. The nested-name lab covers recursive and generic callbacks, ordinary
callbacks and recursion through object members.

Compiled overload identity excludes ParamArray metadata. Overloads with identical compiled
parameter types share a .NET signature even when only one is a rest overload; deduplication
retains the first and reports the existing DO001 finding for the dropped contract.

Harvest deduplication includes the declaring namespace, preserving a same-named re-export
beside its root export. Distinct source type-alias symbols retain their declarations across
module owners even when the checker shares their type ID, including primitive aliases.

Shape computes canonical abbreviations and phantoms before materializing their re-exports.
The re-export parameter lookup includes declarations produced in that same pass. Re-exporting
the same source symbol applies its canonical type and forwards its binders; only a distinct
source alias reinterprets its own alias parameters. This keeps `GlobalThis.Array<'T>` typed
and makes `GlobalThis.Partial<'T>` the same F# type as the canonical `Partial<'T>`, rather
than a second erased wrapper. The compiler-library consumer gate pins both assignability
and generic arity.

Namespace traversal retains both the canonical source path and each public export path.
`HarvestedExport.NamespacePath` records occurrence ancestry, while `Namespaces` remains the
source-symbol map. Traversal uses ancestor symbols to stop cycles without suppressing a sibling
alias of the same namespace. Declaration selection prefers a canonical occurrence; other paths
emit type abbreviations forwarding generic parameters. The nested-name lab and its consumer
compile gate cover a root namespace alias, nested members, and two sibling aliases sharing the
same source types.

Width-cutoff protection includes the contract edges of explicit export seeds, not only class
and interface declarations. Exported functions and anonymous object aliases can otherwise lose
their options/member types when a newly harvested namespace fills the frontier. Applications
still contribute their arguments without recursively opening substituted members. The bounded
frontier regression covers exported generic function options alongside the generic-expansion
termination case. A generic alias's own declaration also carries alias arguments; Resolve
authenticates that declaration before deciding whether those arguments mark an application.
Substituted contracts are followed from source roots with declaration ancestry. A private
per-generation cache reads both the type symbol's and alias symbol's declaration handles;
catalogue identity fields are not populated until a later pass. Revisiting a declaration with
new arguments stops expansion on that path, while siblings keep independent paths. Anonymous
union/intersection edges preserve ancestry. Fresh generic method binders remain outside
application expansion. Traversal memoization reuses a path only when its ancestor set is no
more restrictive. The regression pins callback parameter identity and bounds recursive
callable trees through both returns and parameters; real-package findings are audited after
regeneration.

The contract walk's cost is linear in the table. Contracts are computed once per generation and
shared by the deferred-type retry and the width cutoff; declaration handles for the whole table
are prefetched in chunks of 128, and the traversal itself is a synchronous work-stack loop. The
canonical-seed-alias set is still computed before the first generation: deferring it to the first
oversized frontier reorders checker queries, and with them the checker type ids, which moved 16
Core.TS constructor objects (`WebAssembly.*Constructor`, `Intl.*Constructor`) under `GlobalThis`.
A nominal declaration restarts the ancestry and the depth,
so its contract is the same on every path and a diamond of interfaces is walked once per node.
Ancestry still stops an application from re-entering one of its own declarations; in addition,
an application whose arguments wrap (anything beyond type parameters, primitives, literals and
unions of those) consumes one unit of `ContractDepth` (1) when its members are followed, so
mutually wrapping generic aliases admit a bounded number of instantiations instead of one per
permutation of their declarations. Applications with plain arguments are bounded by the per-type
memo, which expands each type at most once per remaining depth. Computed literal arguments are
plain and unbounded in principle (`${S}k` spells out its path); measured on six to eight mutually
recursive aliases, each appending its own digit, the table grows polynomially (209 entries at six,
483 at eight), and a rule spending depth on literal arguments below an application left those
counts unchanged, so none is applied.
A generic call signature on an application keeps its parameters, return type and type
parameters. A failed declaration-handle or seed lookup fails the pass. The width-1 regressions
cover a 20-deep interface diamond chain, bounded by contract-walk visits per table entry through
`resolveTypeTableObserved`; six mutually wrapping interfaces and aliases; six aliases over
computed literal arguments; a callback below an application shared by two roots, in both root
orders; and `Mapper<T>`.

### TypeScript-private members (2026-10-24)

Resolve omits a TypeScript-private property: every declaration carries the `private` modifier or
a `#name` (`PrivateKeyword` in the declaration's modifiers, or a `PrivateIdentifier` name). A
leading underscore is never read; `protected` members stay. The omission is Exact and records no
finding. `TypeFacts.PrivateMembers` records that a type had one: such a type admits only its
class's instances, so it is declared even with no other member (`interface end`, never the TR047
`obj`), and `synthesize-paramobjects` refuses its `Create` (SP003). A `private constructor` sets
`ResolvedSignature.IsPrivate`; the construct signature stays, so the class keeps its class
identity and statics, and shape-classes and constructor objects emit no constructor for it. A
public getter over a private setter reads get-only. private-member-lab is the lab.

Measured against a base regeneration: private members account for no TR008 in Core.TS, Node or
the npm goldens (solid-js's private members are in seroval, which it does not emit). Core.TS moves
RawJSON and RawJSONInstance, whose `#isRawJson` brand read as `unknown` → `obj` (TR009 338 → 336,
SP001 1,179 → 1,177, SP003 504 → 506); the lib-ship and catalog-library-entry labs move
lib.scripthost's SafeArray and VarDate. Node loses fourteen `[<EmitConstructor>]` exports of private constructors (`Cipher`,
`Hash`, `Stats`, `REPLServer` and others). Catalogue identity keys do not move; RawJSON's `api`
hash in Core.TS's `declarations.json` does.

## Census batch 2 catalogue compatibility (2026-10-25)

Ambient export-equals value recovery adds imported value members, and source-identical overload selection preserves the first applicable return instead of unioning unreachable alternatives. Schema 2 advances API version 11 to 12 and inference version 4 to 5 so prior producers are rejected before reuse. Identity version 7, customization version 1 and contract version 1 remain unchanged: declaration identities, source authentication and selector semantics retain their existing rules. The Core.TS producer is regenerated with the new contract alongside the affected Node bindings.

Batch measurements: Node HG001 4 to 0 and DO008 30 to 26; non-lab widened/escape finding occurrences 6708 to 6698. The new process import adds one existing SE003 read-only-import finding while its writable global stays intact. Both tier audits report zero regressions and zero unexplained removals. All 154 golden write tests, 1634 generator tests, 105 Wire tests, 664 runtime checks and the Partas source/DLL consumer gate passed (one ignored test in each Expecto suite).

## Authenticated compiler NoInfer declarations (2026-10-25)

`resolve-alias-parameters` records the complete symbol identities of the selected compiler's
top-level `NoInfer<T> = intrinsic` declaration. Authentication requires default-library metadata,
the declaration handle's top-level AST node, the written name, an intrinsic body and one binder.
The identity set travels through Resolve into pure Shape; the shared intrinsic type id carries
no authentication. `shape-aliases` emits each authenticated public occurrence, including
`GlobalThis.NoInfer`, as `type NoInfer<'T> = JS.NoInfer<'T>` or `= 'T` under `resolveNoInfer`.
This precedes first-claimant intrinsic fallback and arity repair. Other intrinsic declarations
and user aliases retain their current mapping and findings. The support abbreviation remains
an identity type; this adds no F# inference barrier. Compiler substitution references still
widen through their existing path; indexed-access NoInfer references retain their toggle policy.

The census-noinfer-declaration lab checks actual compiler declaration ownership in both claimant
orders, both resolve settings, secondary public occurrences, sibling Uppercase fallback and
namespaced user aliases. Pure pass tests check distinct symbol identities sharing one intrinsic
type id. No finding code is introduced; the batch coordinator owns the API compatibility bump.

## Census batch 3 catalogue compatibility (2026-10-25)

Schema 2 advances identity version 7 to 8 for compiler-anchored anonymous intersection and nominal-union contracts, and API version 12 to 13 for authenticated compiler NoInfer declarations and exact-key member bindings. Old producers are rejected before declaration reuse. Inference version 5, customization version 1 and contract version 1 remain unchanged. The current-contract test pins this combination and separately rejects identity-7 and API-12 producers; regenerated catalogues and the full batch gate provide the integration evidence.

## Backtick string member keys (2026-10-25)

`shape-interfaces` allocates otherwise unspellable string member names with
`Naming.identifierName` and stable numeric suffixes. Original and inherited spellings,
indexers, construction/invocation members, well-known symbol members and literal overloads
share reservations. Each source key retains its overload family. `MB012` reports its exact
key and allocated F# spelling; the historical dropped-member code `MB007` remains reserved.

`FsStringKeyed` carries the original key through member substitution, overload settlement,
qualification and catalogue hashing. Literal overloads retain their original call target.
Every JavaScript key payload passes through `Naming.memberName` to undo the checker's
leading-underscore escape before rendering or hashing; allocation lookup identities remain
checker names. In particular, `___promisify__` targets the real `__promisify__` member.
Exclusive-arm folds allocate across their shared and exclusive members and carry the same
wrapper. `FsCreateOverload.StringKeys` preserves construction mappings after inherited
members have been removed from the declaration. Optional keys retain omission semantics.

Class statics use `ImportKeyed` to retain the constructor binding and exact member key.
Render emits bracket reads, writes and receiver calls through the imported constructor.
`Render.jsString` escapes JavaScript text and Emit macro markers in each key. Catalogue API
hashes include string member keys, keyed statics and constructor mappings. Ordinary member
spellings retain their mapping; literal overload allocation also reserves synthetic names.

An impossible-key method on an ambient entrypoint class retains `MB007`: its abstract
override contract needs separate exact-name override evidence. A concrete forwarding member
cannot replace that contract. The entrypoint negative fixture pins the retained loss.

### Resolve: canonical generic alias contract roots (2026-10-25)

The bounded source-contract walk includes canonical generic aliases discovered after the export
seeds. Eligibility requires actual source generic TypeAliasDeclaration nodes and the exact type
ID returned by getDeclaredTypeOfSymbol for the alias. Per-type authentication and per-symbol
declared-type queries are memoized. These finite declared forms restart contract traversal;
applications keep the existing ancestry and ContractDepth limits, including method applications
whose arguments happen to be type parameters. Width 32768 and the depth cutoff remain unchanged.

The Hono/Chanfana Resolve-only diagnostic fails before this change because the canonical
JSONRespondReturn intersection's TypedResponse operand is deferred, then passes with both
operands retained. Its body takes 44.60 seconds before and 48.52 seconds after; resolved table
size changes 149382 to 149402. The small width-one lab covers canonical/application distinction,
and existing source-method, diamond, mutual wrapping/computed-literal and shared-root cost tests
remain bounded. Catalogue source/API and applied-generic ownership guards are unchanged.

### Resolve: selected conditional source-contract closure (2026-10-26)

The protected source-contract walk follows an already selected Conditional.Branch edge under
its existing expand guard. It inherits the current declaration ancestry and remaining wrapping
depth and uses the existing per-type memoization. Neither unselected raw branch is admitted;
branch selection, width32768, depth policy and catalogue guards are unchanged. This restores the
inner conditional of HonoRequest.param, which the compiler selected identically in producer and
consumer but the consumer lost to frontier width pressure. The small nested-conditional lab is
a positive policy/cost guard; the source-authenticated real request snapshot supplies RED.

### Shape: dependent generic application constraints (2026-10-26)

`appliedRefTo` substitutes supplied arguments into a target declaration's dependent bounds
under disjoint comparison binders. Its dependency graph processes forward references before
their users, with one correction per argument. Complete emitted nominal bases establish
dependent subtyping; ordinary constraint checks and their existing losses remain active.

Final cyclic validation retains lawful nominal self and mutual applications. A cyclic
correction whose final bounds remain unproven widens the whole application with `TR074`.
The Zod reduction covers defaults, explicit arguments, open generic inheritance, methods,
nested reordered binders, forward correction and recursive positive/negative controls.
The initial generated consumer fails FS0001 before the fix; the corrected consumer compiles
and rejects a different nominal dependent contract. Full Zod and corpus evidence belongs to
the follow-up integration gate.

The follow-up catalogue contract advances API version 13 to 14 for the specialized dependent
constraints and cyclic-application fallback. Identity version 8, inference version 5,
customization version 1 and contract version 1 remain unchanged: source identity, inference
profile normalization and metadata interpretation retain their existing meanings. API-13
producers are rejected before traversal, and the complete owner family must be regenerated
with the same API-14 candidate.

The API-14 regeneration passes all 161 golden-writing tests. All 157 existing corpus/Core.TS/
Node manifests retain their tiers, symbol counts and per-symbol tiers against source6bc;
the new Zod lab is the sole added manifest. Core.TS changes only its catalogue compatibility
and producer fingerprint plus the matching input stamp. Both Core.TS and Node binding text
remain unchanged. Full latest CloudEdge consumer compilation and runtime gates follow this
artifact milestone.

### Shape: generic exclusive object unions (2026-10-26)

Exclusive-arm folds participate in the generic object-alias application path. A complete,
one-to-one arm correspondence requires compiler type identity or source declaration/symbol
identity with the same field names, optionality and readonly flags. Applications retain the
canonical alias and supplied arguments; the fold declares its own type parameters and shapes
properties and Create overloads under that scope. Independently named or extracted arm
contracts remain narrower declarations rather than aliases of the folded union.

The Workers reduction covers generic calls, both Create overloads, independent arm access and
negative cross-argument/arm consumers. This repairs missing synthetic arm references and erased
fold binders without adding a fallback or changing the exclusive-arm mapping policy. API14 is
the unpublished follow-up contract; every final producer must be regenerated from the same
candidate before consumer validation.

The full latest Workers declaration package (5.20261025.1) compiles after regeneration with
the captured CloudEdge configuration and Fable.Core 5.6.0. Its sole changed symbol tier is
WorkflowBatchCreateOptions, Widened to Ergonomic; the nested retention declaration moves from
the method's anonymous application path to the canonical alias. The mapped BooleanArm control
retains its boolean fields rather than being mistaken for Options<string>.

After this fix, proper Core.TS and Node regeneration succeeds and all 162 golden-writing
tests pass. Against source6bc, all 157 existing corpus/Core.TS/Node manifests retain their
counts and per-symbol tiers; the Zod and Workers reductions are the two added manifests.
Core.TS and Node binding text is unchanged. Core.TS refreshes only the producer fingerprint
and its matching catalogue input stamp, keeping the unpublished API14 family coherent.

### Catalogue: source namespace aliases (2026-10-26)

The source-export identity fallback qualifies each occurrence with both its public module
path and its namespace occurrence path, exactly as Shape qualifies an emitted alias. This
lets a nested generic mapped utility such as Zod's V3.Util.Omit authenticate its actual
TypeAliasDeclaration even when the unresolved mapped result has no reusable type identity.
Declaration order and source declaration handles remain required; no synthetic parent or
rendered target supplies identity. Same-shaped aliases in separate namespaces retain distinct
identities, while multiple public occurrences of one source alias reuse the same identity.
The namespace utility lab reproduces the original catalogue guard before the fix.

### Catalogue: overlapping source compositions (2026-10-26)

Source-intersection and source-union identities already contain the complete authenticated
handle set plus every constituent contract key. Two distinct compositions can share an operand
without representing inconsistent declarations of one nominal source type. On a complete-key
miss, the overlapping-handle merge guard therefore applies to ordinary declaration roles,
not pairs of source-composition identities of the same role. Only complete-key equality grants
producer reuse; handles, source authentication and subsequent API/arity/constraint checks are
unchanged. A composite/nominal role mismatch retains its existing treatment.

The composite handle lab reproduces two unrelated Omit intersections sharing the library's
mapped declaration, and also covers overlapping nominal unions. Their complete identities
and owners remain distinct. Nominal augmentation inside either composition must still fail.
This fixes conflict classification without changing stored identities, hashing or valid-match
semantics; identity8 and the unpublished API14 contract retain their meanings.

### Render: private phantom case identifiers (2026-10-26)

Phantom declarations retain the exact public type name, generic parameters, private carrier
and Erase attribute. Their implementation case uses a fixed uppercase Phantom_ prefix before
the complete original spelling. This is injective across lower/Upper spellings and legal when
the public alias begins lowercase, with an underscore, or is an escaped F# keyword. Changing
only that private implementation identifier preserves the canonical public API and erasure.
The phantom case lab compiles those aliases and a source alias already beginning Phantom_,
and rejects generic/nominal substitutions and external access to the private representation.

With the namespace and composite-identity corrections, exact latest Zod4.6.7 owner generation
succeeds against the captured Workers catalogue. The complete fresh Zod source and current
matching Workers source compile together against net8.0/Fable.Core5.6.0 with zero errors.
The phantom-only diff is 81 private case prefixes; all 4214 catalogue contracts (identity,
canonical API, arity and constraints) and manifest tiers remain unchanged. Final packaged
consumer-family validation and the consolidated runtime gate follow producer regeneration.

### Catalogue: authenticated transparent declaration occurrences (2026-10-26)

Canonical API normalization follows a type-only abbreviation to its actual local defining
declaration only when their complete source identities match, every binder forwards in order,
arity and canonical constraints agree, and the chain is acyclic. Unrelated source aliases,
transformed arguments and value bindings retain their own surfaces. Original and current
declaration/abbreviation maps remain separate when computing customization base APIs.

Within one identity group, an authenticated defining declaration precedes its forwarding
wrappers in canonical ownership selection; all unrelated ordering stays unchanged. Otherwise
same-depth lexical ordering can choose Mirror.Compute before Original.Compute and redirect the
definition back to its own alias. Harvested Type export occurrence paths are retained using
the exact shaping qualification, preserving the public Mirror.Compute alias after deduplication.

The mapped phantom namespace reexport lab reproduces Core.TS Omit's API conflict. It checks
the public alias with a bounded consumer build, alpha-renamed forwarding, transformed/partial/
permuted arguments, changed constraints, value bindings, unrelated structural/opaque targets,
cycles, carrier changes and customization base API isolation. This changes the canonical API
normal form for authenticated transparent occurrences within the unpublished API14 batch;
all final producer catalogues and packaged consumers require regeneration together.

Identity contract 9 rejects namespace aliases authenticated through generated parent roles.
The CoreTS audit accounts for all 65 removed GlobalThis catalogue rows as actual source aliases;
all emitted public type paths remain present. API contract 14 covers faithful alias surface normalization.
A stale identity-8/API-14 producer is rejected independently of its API version.

Source function parent-role closure: source-authenticated exported FunctionDeclaration sets
seed signature-role traversal only when complete structural identity is unavailable. Parent
roots remain outside canonical byType/byName maps, and child expansion cannot republish a
root. Existing call/construct/parameter/return role keys and full source/API guards remain.
The ProviderUtils destructured-parameter lab pins independent function/overload/position
identity, generic constraints, source mutation rejection and missing-anchor rejection.
This is part of the unpublished identity-9/API-14 producer refresh batch.

### Shape: source-annotated closed string aliases (2026-10-26)

TypeScript interns equal literal unions, so a shared TypeId or equal case set does not identify
a nominal F# string enum. Resolve records per-property evidence only for actual source
PropertySignature annotations: a direct, nongeneric TypeReference resolving to a closed
string alias, with the exact declared TypeId and complete resolved symbol declaration set.
Import renames resolve to their actual symbol. Direct string-literal unions carry separate
inline evidence; unsupported generic or nullable annotations retain their existing mapping.

Shape allocates a source enum for each authenticated alias occurrence, preserving transparent
type-only alias chains as abbreviations. Per-member shaping uses that source name; raw literal
occurrences retain a separate anonymous enum and their occurrence ownership. An unexported
alias (a compiler-lib global such as `RequestCache`) keeps the name its type already carries,
so its group placement is unchanged; a second alias of the same type claims a fresh name. Only the nominal
source alias receives its actual source origin/compiler family. A merged const value helper
is renamed when necessary to avoid the restored source type name, while its JS import stays
unchanged. Complete symbol handles and source bytes still guard catalogue reuse; no value-set
matching or global interned-TypeId ownership replacement is introduced.

The keyof enum owner lab checks producer/consumer context stability, equal-valued distinct
aliases, raw literals, transparent aliases, import renames and merged const value consumers.
Fresh real Zod generation passes, and ProviderUtils advances its IssueData guard to a distinct
ParseContext API mismatch. Full-family acceptance remains pending that separate reduction.

### Shape: catalogued generic callable interfaces (2026-10-26)

A function-shaped callback (D5) is written inline as an F# function unless something names it,
and an export names it. A generic callable interface (`$ZodErrorMap<T>`) therefore read as
`ZodErrorMap<'T>` in the package exporting it and as an expanded `obj -> ...` function in a
consumer reaching it through a member, so the catalogue API of every declaration referencing it
depended on export reach. In catalogue or reference mode, a named generic interface whose
content is call signatures is declared wherever it is reached; its applications are written as
applications of that declaration and keep their type arguments. Standalone generation keeps D5.
The callable interface owner lab checks the transitive standalone output and producer reuse.
ProviderUtils advances to a `ParsePayload` mismatch: the producer elects the exported classic
alias `IssueData` for `$ZodRawIssue<$ZodIssue>`, which the consumer never reaches.

### Declaration catalog: export-independent union identities (2026-10-26)

A union without declarations of its own is one interned checker type wherever it is reached,
so its key inside another type's key no longer depends on whether the program exports a name
for it: an unaliased literal union keys members by its values (`ZodParsedType` under
`OmitKeys<ZodInvalidTypeIssue, "path">`). Its own declaration row keeps the export's identity.
An unaliased union whose arms are anchored in their declarations, alias declarations or
intersection composition is keyed `union` by its sorted arm keys; an arm known only through an
export handle keeps the parent-role fallback. Two rows answer for those value keys in an
optional `equivalents` array (omitted when empty; older readers ignore it): the declared union
an unaliased union's member set elects (`namedUnionByMembers`, here Zod's classic
`IssueData`), and a declared literal union identified only by its export (`keyof typeof Kind`).
A row's own identity always wins over another row's equivalent.
A consumer whose own union matches an equivalent reuses the row: its references canonicalise to
the row's primary identity, and the alias's own file, which the consumer may never load, is the
only producer source allowed to be absent. Every loaded source still authenticates exactly,
and the alias row keeps its alias identity for consumers that name `IssueData` directly. The
identity change stays within the unpublished identity 9 batch; every producer regenerates.
Labs: alias-election-owner-lab (transitive and direct consumers), keyof-member-key-lab.

### Constraint satisfaction: alias-transparent bounds (2026-10-26)

`satisfiesNominally` treats an alias of an instantiation (`type AnySchema = Schema`) as the
instantiation it renders an abbreviation of: two type ids sharing a target other than themselves
and the same type arguments satisfy each other, in either direction. The checker interns the
alias as its own type, so before this a method bound written as the target
(`output<NewReturns extends $ZodType>`) failed against a parameter bound written as the alias
(`Returns extends $ZodFunctionOut`), and `TR` widened `$ZodFunction<Args, NewReturns>` to
`ZodFunction<'Args, ZodFunctionOut>`. A consumer that never declares the alias renders both bounds
as the same application and kept `'NewReturns`, so the catalogue API diverged (`ai` against Zod's
`V4.Core.ZodFunction`). Both now keep the argument. Lab: alias-bound-lab.

### Resolve: indexed-access bounds are structural (2026-10-26)

`T[K]` over a type parameter selects from the parameter's bound (`operandShape`), so the bound
is a type the shape tier reads for its structure. Resolve now registers it `Structural` from the
indexed-access branch, walking a chain of type parameter bounds to the first other type. An
instantiation bound (`I extends Internals`, which the checker applies as `Internals<unknown>`)
was otherwise derived as identity alone unless some union or intersection happened to read it,
so `I["def"]` widened to `obj` in one program and selected `Def` in another: Zod's own run kept
`_ZodType.def: ZodTypeDef`, while `ai` dropped `def`/`_def` and the reused API diverged. Both
now select the bound's member. Keeping the access generic rather than bound-typed is tracked in
issue #178 (inline SRTP accessor). Lab: catalog-indexed-bound-lab.

### Declaration catalog: default arguments of bare alias references (2026-10-26)

The checker can intern a bare reference to a generic alias as its own copy carrying the alias
and no arguments: `T extends Tool`, with `type Tool<I = any, O = any> = FunctionTool<I, O> |
DynamicTool<I, O>`, sits beside the ordinary `Tool<any, any>`. Which copy a later bare reference
resolves to depends on the program, so `ToolCallerDefinition`'s `bind` returned `Tool<any, any,
any>` in ProviderUtils and the argument-free copy in `ai`, and every anonymous arm keyed under it
diverged. `resolve-declaration-identities` now records the alias's defaults as
`DeclarationArguments` when the checker reports none. A bare reference to a generic alias is its
default application, so the defaults are recorded where every parameter has one and the type is
a closed application: not the alias's declared body, and free of the alias's own parameters
through four levels of members, type arguments and alias arguments. Matching the type against
the body under the defaults does not work: `Tool<any, any, any>` distributes its intersection
arms over member unions and arrives with eight arms against the body's four.
Only catalogue keys read `DeclarationArguments`; rendering is unchanged.
Lab: catalog-alias-defaults-lab.

Declarations that share an identity merge into one row when arity, bounds and API agree. Two
copies of one type can be resolved to different depths, so their source closures may differ
(`ai`'s `ToolChoice2` closed over 2 files, its use-site copy over 55); the row records the union
of the closures instead of refusing the merge.

### Declaration catalog: use-site bounds and hoisted intersection scope (2026-10-26)

A type parameter argument is keyed by its name, so an alias applied over a use site's own `M`
keyed exactly like the declaration over its `M`, whatever either bound. Where the bounds agree
or the use site only adds one, the copy merges into the declaration as an abbreviation; where
the use site's bound differs (`createAgentUIStream`'s `UI_MESSAGE extends UIMessage<META, never,
…>` against `UIMessageStreamOptions<UI_MESSAGE extends UIMessage>`), F# cannot abbreviate it,
Shape emits it separately, and the merge refused the shared identity. Resolve now records, per
alias argument, the bound of a type parameter whose bound differs by checker identity from the
alias's declared bound at that position (`DeclarationArgumentBounds`), and the catalogue keys it
beside the argument, structural keys included. Lab: catalog-use-site-bound-lab.

A hoisted anonymous intersection binds the parameters it reads from its scope (`DeclParams`)
positionally in its `source-intersection` key, with each parameter's bound, as a declared type
binds its own. Copies over equally bounded owner parameters merge; an owner whose parameter is
bounded differently (`DirectChatTransport`'s `UI_MESSAGE extends UIMessage<unknown, never, ...>`)
keeps its own copy, which F# needs for the different constraint. Without that
each owner's copy (`HttpChatTransport<UI_MESSAGE>`'s, `DefaultChatTransport<UI_MESSAGE>`'s)
keyed its constituents by its own `UI_MESSAGE` declaration, the copies of
`PrepareSendMessagesRequest`'s options object diverged, and the declarations naming them
disagreed on API. Lab: catalog-anonymous-owner-lab.

### Declaration catalog: reuse past the consumer's follow frontier (2026-10-27)

Resolve follows twenty generations from the seeds (`FollowDepth`); types beyond are left in
`NotFollowed`. A producer seeds its own declarations, while a consumer may reach the same
declaration twenty generations deep: `agents` reached Zod's `$ZodCheckStringFormatDef<Format>`
with `Format` past its frontier, rendered it without parameters, and failed the reuse check on
arity. The consumer's rendering of a reused declaration is replaced by the producer's row, and the
row's identity and authenticated sources already fix the declaration, so a reused declaration
whose own structure reaches a `NotFollowed` type, or which names a declaration that does, is
reused without the arity, bound and API comparison. A complete mismatch still fails.
Lab: catalog-generic-base-lab (a 21-link chain ending at a generic base).

A consumer export naming a producer's anonymous union reuses the producer's row through the
consumer row's equivalents: `agents` writes `type WorkflowStatus = InstanceStatus["status"]`
over Workers' anonymous status union, keys it by that export, and records the value key as an
equivalent; Workers keyed the union by those values (`InstanceStatus.Status`). Reuse now also
looks up the consumer row's equivalents among producer primary identities, so the export
abbreviates the producer row and `InstanceStatus` compares equal. Lab: catalog-consumer-alias-lab.

### Shape and catalogue: the shared empty object (2026-10-27)

Every bare `{}` resolves to one checker type. An alias resolving to it - Workers'
`type MainModule = GlobalProp<"mainModule", {}>` - claimed it through its export, so Workers
wrote every `logprobs?: {} | null` as `MainModule` and keyed it by `MainModule`'s handle, while
`aigatewayprovider`, which never loads `MainModule`, keyed the same `{}` as an empty object.
`isSharedEmptyObject` treats the type as intrinsic, as `string` is: `typeRefOnPath` renders it
`obj` with `ObjectWithoutMembers` before consulting `DeclNames`, the alias still declares
`type MainModule = obj`, and the catalogue keys it `empty-object` ahead of export and alias
handles. Accepted tier movement (user decision): five Workers AI symbols that reached `{}`
through `MainModule` move from ergonomic to widened; their F# type is unchanged.
Lab: catalog-empty-object-lab.
