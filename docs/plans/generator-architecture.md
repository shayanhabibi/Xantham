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
| `classify-unions-by-position` | input: overloads/erased ctors; output: tagged/test-helpers; `U_n ≤ 4` | §4.5, D4 |
| `detect-brands` | primitive ∩ phantom-object ⇒ measure-tagged primitive | §4.6 |
| `flatten-intersections` | apparent-members flattening + interface inheritance where nominal | §4.6 |
| `resolve-enums` | `getConstantValue`, const-enum inlining | §4.7 |
| `shape-callbacks` | delegates by default | §4.8, D5 |
| `expand-literal-overloads` | `on("click", …)` ⇒ `onClick` companions (config-gated) | §4.8 |
| `shape-tuples` | fixed/option-tail/rest-carrier | §4.12, D7 |
| `split-keyof-regimes` | closed ⇒ literal union; open ⇒ support-package idioms | §4.10 |
| `name-expanded-aliases` | `Partial<Options>` ⇒ `OptionsPartial` | §4.10, D6 |
| `hash-cons-anonymous` | synthetic interfaces for inline literals, by `TypeId` | §4.4 |
| `synthesize-paramobjects` | ParamObject ctors for plain-data interfaces; parameter-position literals flattened | §4.4, D3 |
| `default-type-args` | same-name-lower-arity abbreviations | §4.9 |
| `shape-classes` | instance interface + statics/`Exports` split | §4.4 |
| `assign-names` | NamePath synthesis, collision resolution, keyword escaping, CompiledName | §4.14 |
| `order-declarations` | topological sort, `and`-groups for cycles, module layout | — |
| `audit-coverage` | every harvested export reached a shaped decl or an explicit Finding | §5 |

Passes are *conceptually* categorized this way even when, mechanically, two adjacent pure
rewrites could fuse — do not fuse them; the seam is the test point.

- **Invariant (tier exit):** the shaped model contains only `FsDecl` constructs the renderer
  has cases for, and `audit-coverage` has accounted for every export.

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
    determinism (the first determinism failure the e2e property caught).
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
    output - so only overload expansion at input positions is still deferred.
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
  - *Two repairs have to run after every shaping pass*, because they fix what the others
    produce (`repair-arity`, between `order-declarations` and `audit-coverage`): a
    generic abbreviation whose target widened away its parameters is FS0035, so the
    declaration goes and its references widen; a generic declaration named bare at a
    reference position is FS0033, so that position widens - §4.9's rule for an
    out-of-scope type *variable*, one level up at the declaration head. A settable
    property of type `unit` is FS0252 and is demoted to read-only in the same pass: a
    `never`-typed brand holds no value, so it also stops being a `Create` parameter.
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
      already did.
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
    (`docs/plans/generator-three-rung.md` §9, blocker 4); it is not `three`-specific, which is
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

**Wave two, closed (2026-09-02).** Five lanes over `docs/plans/generator-wave-two.md`, dispatched
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

**Wave three, closed (2026-09-02).** Seven lanes over `docs/plans/generator-wave-three.md`, in two batches of concurrent lanes on disjoint pass files. The corpus stands at 28 fixtures and 14,803 findings, from 21 and 15,278. `TR023` fell 727 to 143 and its cause 1 is closed: every site remaining names a type rather than a member. Escape fell 200 to 192.

The wave was scoped by a measurement rather than by a plan. Lane G re-ran the `@types/three` harness against wave two's HEAD and found one of four criteria met - the 128 MB file compiles clean where it had 666 errors - and the instantiation runaway unmoved to the digit. It reduced the reason to twelve lines, and lane J closed it: `three` renders 112,054 lines against 482,805, the runaway falls from 70.6% of the file to 0.1%, `TR002` 5,698 to 4, `TP001` 162 to 2. The rung is still refused, now on size alone at 112k against ~50k, and `TR019` 0 to 4,461 is the price a memberless phantom declaration charges. Blocker 1 had been open since the rung was first reconnoitred.

Three things the wave learned that outlive it. `TR044`'s absence was not evidence of a closed hole: lane K's lab compiled to three `FS0001`, so a primitive, tuple or array argument against a written constraint had been falling through all along. `TR018` is a distinct defect rather than `TR023`'s residue - it moved by zero under lane L, refuting the hypothesis in the `TR023` recon's §9 - and is re-priced in `docs/plans/generator-tr018-recon.md` at 194 sites over four causes. And `RA001` had been masking a malformed type-parameter list, which is what a drop does: it hides the thing it drops.

`TP007` is constructed nowhere in `src/` and is retained rather than retired, because removing it renumbers `TP008`, which is quoted by four source files and by measurements two plan documents record. The case says so.

The measurement handed up rather than acted on: `RT001` says `@cloudflare/workers-types` now truncates 1,772 types at `FollowDepth` 12 against 261 before, and a five-symbol lab from another lane crossed the cutoff during composition. No output is wrong today. Raising the cutoff is a corpus-wide behaviour change and belongs to a wave that measures it deliberately.

**Wave four, closed (2026-09-03).** Four lanes over `docs/plans/generator-wave-four.md`, in one
batch on disjoint pass files. The corpus stands at 30 fixtures. `TR018` fell 197 to 77 and
`@cloudflare/workers-types` - now the litmus test in place of `@types/three` - reads exact 231,
ergonomic 1,061, widened 365, escape 115, from 374 widened.

The wave was scoped by the user's dispositions rather than by the corpus. Four of the six losses
in `docs/fable5-workarounds.md` are closed as unresolvable and carry no work in any wave. Two
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
`FollowDepth` cutoff. `docs/plans/generator-cloudflare-recon.md` adds three more - a finding-count
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
  | `map` *(future)* | identity only | redirected to an existing package (`Fable.Browser.*`, BCL/Fable-native types) |
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
  redirection of a whole group, so `map` stays future work; what they do establish is that
  the destination of such a redirection has to carry arity, not just a name.

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

  **The first consequence does not hold yet, and wave five lane T is where it is stated as a
  test** (`tests/Xantham.Generator.Tests/MultiPackage.test.fs`, two `ptestCase`). Two breaks
  stand between the contract and the claim. A templated *generic* is rendered by
  `Shape/Spec.fs` as a bare name with its type arguments discarded, so `Box<string>` reads
  `Dep.Box` against a dependency declaring `Box<'T>` and the reference fails `FS0033` with no
  finding raised; `instantiationOf` would re-apply them, and it looks the target up in
  `model.DeclNames`, which identity-only resolution never fills. And a referenced *alias over
  an object literal* carries the symbol `__type` on `symbol` rather than `aliasSymbol`, so
  `isAnonymousShape` is true, the group shortcut is skipped, and the dependency's shape is
  re-derived into the entry package under a second name — one TypeScript type declared as two
  unrelated F# interfaces, and that one compiles.

  *Settled (wave five lane T):* the compile gate takes closed configurations only, with the
  **corpus** as the unit of closure rather than the run. `cross-package-lab` and
  `cross-package-dep` are gated together, which makes the F# compiler rather than a string
  comparison the judge of whether a templated name is the shipped one. A golden whose
  templated module nothing ships carries the `.open.fs` suffix and stays out. Stub synthesis
  is refused: a stub written from the templated identity agrees with the template by
  construction, and both breaks above would compile clean against one.

Watch items rather than open questions: the debug assertion pass (O3) and the bespoke
JSON model dump (O5) are named escape hatches, built only when their triggering need
appears.

# Easy Nits 

To include in scope when a phases implementation/attempt ends up being small/quick.

- Pass names should be keyed by a unique prefix.
  - This has been done to some level, but does not cover all stages. Changing the current implementation of prefixed stages is ok if required.
  - Stage prefixes should be unique for that stage
  - Tests should fail quickly if a pass name & prefix is not unique.
  - Add this as a prefix to the key of a finding as such: `XX:TY001`
  - This will allow grepping a stage by `XX:`and a finding by `:XX000`