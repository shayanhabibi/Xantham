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
   diffed. A golden diff is the review surface for any pass change.
4. **Compile gate:** generated output for the golden fixtures is compiled (F# type-check;
   Fable compile once the support package exists) in the test suite. Bindings that do not
   compile are not bindings.
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
- **C — unions and generics.** Position-aware unions (D4), tagged-union detection, tuples
  (D7), generics/constraints/default-args. Fixture: `@cloudflare/workers-types`.
- **D — the erased-idiom zone.** Revive `Xantham.Fable.Core`; keyof regimes, mapped/
  conditional handling, alias naming (D6), brand detection. Fixtures: `solid-js`, `type-fest`.
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
