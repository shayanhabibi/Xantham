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
- Also renders `manifest.json` — the fidelity report: per-symbol tier, findings, pass
  provenance — and the run report (counts of Exact/Ergonomic/Widened/Escape).
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
   family, which is what the goldens may cite. The Fable *run* gate is still outstanding.
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
- `src/Xantham.Fable.Core` — revived support package (erased `keyof`/`typekeyof`/
  `PropertyRecord`, brand helpers). Fable library, no dependency on the generator.
- `tests/Xantham.Generator.Tests` — Expecto, same conventions as the Wire suite.
- **Decided (O4):** configuration is a JSON file next to the target `package.json`
  (`xantham.json`), deserialized with `System.Text.Json` (JSONC-tolerant via
  `ReadCommentHandling.Skip` — comments matter in per-symbol override lists), validated by
  a **JSON Schema generated from the config record itself** so the file self-documents and
  editors check it. The generator core is a library function taking the config record; the
  CLI (`xantham generate <package-dir> [-o <out>] [--config <path>]`) and the scratch
  harness are both thin shells over it, and the CLI is deferred to phase C.

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
  - Still deferred, and findings say so at every site: method- and function-level
    generics (F# can spell generic abstract members, but nothing needed it yet),
    instantiations of generic *aliases* written as applications (they re-expand
    inline today), and default type arguments, whose §4.9 wording needs revisiting
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
  - *The archive source needed no edits.* It was written against `Fable.Core` 5.0.0-beta.4
    and compiles unchanged against **5.2.0**, which the support package and the compile gate
    are both pinned to - the gate moved up from 4.5.0 to meet it. One Fable.Core across the
    gate and generated output is the invariant; a version seam between them is not allowed,
    which is why the pin is stated in both projects rather than floated.
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
  **Still ahead in D:** the Fable *run* gate, and the `type-fest` and `solid-js` rungs D9 wants
  for calibration.
- **E — hardening.** Dedup/naming at scale, fidelity-manifest UX, determinism under the
  full litmus ladder, `@types/three` and `typescript` rungs.

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
  core is a library function, CLI a thin shell deferred to phase C (§6).
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

  *Status (2026-09-02):* `ship`, `reference` and `widen` are implemented; `map` and `inline`
  are not. The compiler lib is the one group where a `map` already happens in practice, and it
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
  (`@scope/pkg` → `Scope.Pkg`, compiler lib → `TypeScript.Lib`) is pinned and versioned
  because independently generated packages must agree on every templated name — while
  remembering that source-level agreement is not identity; the compiled *assembly*
  (NuGet version discipline against the `typescript` npm pin, as Wire already
  practices) is what unifies a type across packages. Ship-group emission beyond the
  entry package, and demand-driven resolve (a prerequisite for `inline` and for
  shipping large groups), are phase B+.

  **Open:** what the compile gate means for output whose `reference` groups are not
  shipped anywhere yet — gate only closed configurations (every group `ship`/`map`/
  `inline`), or synthesize stub assemblies from the templated identities. Simplest
  first: gate closed configurations only.

Watch items rather than open questions: the debug assertion pass (O3) and the bespoke
JSON model dump (O5) are named escape hatches, built only when their triggering need
appears.
