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

Concretely: a pass is a named `Context -> Model -> Async<Model>` step. Some passes walk
declarations; some fire one batched Wire request and fold the answers into a table; some are
pure rewrites lifted into `Async`; some only *check* and emit findings. `Async` is the
uniform shape because roughly half the passes talk to the compiler through the mailbox
(`TscMailbox` batches overlapping calls into one `batchRequests` round trip, so a pass that
asks 500 small questions is one wire exchange), and uniformity is what makes the pipeline a
plain fold:

```fsharp
type PassOutcome =
    | Advanced of Model
    | Degraded of Model * Finding list   // pass applied partially; findings say where
type Pass = { Name: string; Run: Context -> Model -> Async<PassOutcome> }

let pipeline : Pass list = [ ... ]      // the whole generator, in order, in one place
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

The `Model` is a record with one slot per tier artifact; tiers fill their slot and later
passes read earlier slots. (Alternative considered: four separate model types threaded
through typed tier boundaries — stronger, noisier; see open question O1.)

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

- F# declaration IR → source text. Backend open (O2): Fantomas.Core / Fabulous.AST /
  direct text emission. Leaning direct text emission with golden-file tests (the archive's
  renderer worked this way; formatting churn from a formatter dependency is a real cost, and
  generated code needs stable diffs more than it needs configurable style).
- Also renders `manifest.json` — the fidelity report: per-symbol tier, findings, pass
  provenance — and the run report (counts of Exact/Ergonomic/Widened/Escape).
- **Invariant:** byte-identical output for identical input (model ordering already fixed by
  `order-declarations`; renderer adds nothing nondeterministic).

## 4. Concurrency and batching rules

- Within a pass, fan out per-symbol/per-type wire queries freely — the mailbox coalesces
  them — but **fold results in a sorted order, never arrival order**. Determinism outranks
  latency.
- Between passes, strictly sequential. The pipeline is a fold; there is no pass DAG until a
  measured need appears (O3).
- The Wire session (snapshot, project, program) lives in `Context`, created once per run.
  Passes never create programs; a pass that needs a throwaway program (e.g. future
  verify-by-compile checks) gets a factory in `Context` so tests can stub it.

## 5. Testing strategy

Layered, mirroring the tiers:

1. **Per-pass unit tests** (the nano-pass payoff): construct a small model — builders will
   matter here; invest in a terse model-construction DSL early — run one pass, assert the
   output model and findings. No Wire, no fixtures, milliseconds.
2. **Tier tests against live tsc:** harvest/resolve over the small fixtures (`ansi-regex`
   is one declaration; `animejs` mid-size), asserting the model snapshot. This repo's
   culture is live-compiler tests (`XANTHAM_REQUIRE_TSC=1` makes skips loud), and the Wire
   is fast enough; no mock server.
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
- CLI/packaging deferred until the pipeline generates something worth invoking (open O4).

Phases — each ends with the compile gate green on its fixtures:

- **A — walking skeleton.** Bootstrap, harvest, resolve, minimal shape (interfaces,
  functions, primitives, `option`), render, manifest. End-to-end on `ansi-regex`.
  *Everything after this is adding passes to a working pipeline.*
- **B — the common 90%.** Literal unions (D12), enums, ParamObject synthesis (D3),
  callbacks (D5), classes/statics, naming pass hardening. Fixture: `animejs`.
- **C — unions and generics.** Position-aware unions (D4), tagged-union detection, tuples
  (D7), generics/constraints/default-args. Fixture: `@cloudflare/workers-types`.
- **D — the erased-idiom zone.** Revive `Xantham.Fable.Core`; keyof regimes, mapped/
  conditional handling, alias naming (D6), brand detection. Fixtures: `solid-js`, `type-fest`.
- **E — hardening.** Dedup/naming at scale, fidelity-manifest UX, determinism under the
  full litmus ladder, `@types/three` and `typescript` rungs.

## 7. Open questions

- **O1 — model shape:** one `Model` record with per-tier slots (proposed) vs distinct typed
  models per tier boundary. The record is simpler and passes stay uniform; the typed
  boundary catches "read before written" at compile time. Start with the record + runtime
  slot assertions; revisit if slot misuse actually bites.
- **O2 — render backend:** direct text emission (leaning) vs Fantomas.Core vs Fabulous.AST.
  Decide in phase A with a spike on the skeleton's output.
- **O3 — pass sequencing:** linear list (proposed) vs dependency DAG. Linear until a
  measured need says otherwise.
- **O4 — CLI shape and config file format** (the mapping doc's §5 config surface): decide
  when phase B makes the config real; likely JSON next to the target `package.json`.
- **O5 — snapshot format for tier tests:** serialize the model (JSON?) vs golden-print it.
  Affects test ergonomics more than architecture.
- **O6 — where `tests/Test.fsx` goes:** it stays the scratch harness through phase A, then
  its role is absorbed by the generator's own CLI/e2e tests.
