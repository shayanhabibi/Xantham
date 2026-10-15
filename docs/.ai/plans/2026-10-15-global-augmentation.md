# Global Augmentation — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A module package's `declare global` block is the package's API, so it is bound as
TypeScript spells it. The package's own globals are declared and bound. Its additions to types
another group declares (`interface Window { extra: string }`, `namespace NodeJS { interface
ProcessEnv { X: string } }`) become F# extensions of the owner's existing binding. Today they are
copies of the owner declared as the package's own (pre-#158 problem **E**, Option 1).

**Architecture:** `harvest-globals` reads global scope at a default-library file in every run
that has no global public input. This removes the early return that skipped `declare global` in
module packages. It harvests the package's own globals as `FromGlobal` exports, and it records
every symbol another group declares first and the package declares again in a new
`HarvestModel.Augmentations` inventory. Resolve stops re-homing a merged standard-library type
onto the augmenting package, so the owner keeps its group and identity. A new
`resolve-augmentations` pass reads only what the package adds: members it declares first,
overloads and signatures its declarations write, the bases its `extends` clauses name, and the
owner's own arity. A new `shape-augmentations` pass writes one `FsAugmentation` per owner F# type.
Render emits these in an `[<AutoOpen>] module Augmentations` as optional type extensions. Every
member is reached through `[<Emit>]`, per accessor for a property. Three new findings report what
an extension cannot express: `SI009`, `SI010` and `SI011`.

**Tech Stack:** F# generator (`src/Xantham.Generator`), Expecto suites, Fable.Core 5.2.0 compile
gate, Fable 5.17.2 run gate, TypeScript 7.x `tsc --api` (live), TypeScript 6.0.3 JS API (census
script only).

**Spec:** `docs/.ai/plans/2026-10-15-pre158-findings.md` §E, Option 1 (§4), with its spike
(§1, artefacts in `spike/`), measures (§2), repro labs (§3: `lab/`, `lab-unreached/`,
`lab-mixed/`) and test plan (§5). Controller rulings of 2026-10-15 are recorded under **Rulings**
(C1–C9). The user's standing rule applies: *always choose the most faithful option to the TS
source; blast radius, back-compat and churn are measures to report, never reasons.* This plan runs
**after** `docs/.ai/plans/2026-10-15-exclusive-fold-and-value-side-fixes.md` (plan 1) and
`docs/.ai/plans/2026-10-15-namespace-member-harvest.md` (plan 2). Plan 2's interfaces used here:
- `Spec.namespacePath` / `Spec.underNamespace`;
- `HarvestModel.Namespaces` with dotted paths;
- `harvest-namespace-members`, whose R3 seed is narrowed to package-first global namespaces;
- `foreign-namespace-lab`;
- `CatalogCompatibility.ApiVersion = 9`.

## Global Constraints

- Generated bindings target Fable 5.x only (`Fable.Core` 5.2.0 compile gate, Fable 5.17.2 run gate).
- Tests run only via `dotnet fsi build.fsx -- test [--quick] [--update] [--filter <prefix>] [--run-gate]`; never `dotnet test` (it stalls on the suites' nested builds). The inner loop is `dotnet fsi build.fsx -- test --quick --update --filter "<suite>"`; the unfiltered `dotnet fsi build.fsx -- test` runs before every commit.
- Goldens change only via `--update` (or `--goldens`), never by hand, and every golden diff is read before commit: aggregate measures plus bounded samples for the large fixtures (`.claude/rules/generator-fixtures.md`). A large-fixture move the lab evidence does not explain is handed back.
- A new finding code is a new case in `Findings.fs` + a `FindingCodes.table` row (next unused number under its prefix) + a `Findings.test.fs` pin. This plan adds `SI009`, `SI010`, `SI011` (`SI001`–`SI008` taken, none retired) and retires none.
- The phase record in `docs/.ai/plans/generator-architecture.md` / `docs/.ai/plans/generator-type-mapping.md` is updated in the same commit as the behaviour change.
- Commits use conventional-commit subjects and end with these two lines, in this order:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
  ```
- Scratch files go under `tests/.scratch/`, never the system temp directory. Shell commands are prefixed with `rtk`. Never stage `.serena/project.yml`; stage paths explicitly, never `git add .`.
- Branch: from `feat/tagged-arm-158` (PR #160, unmerged), after plan 1's and plan 2's commits. The work uses `Spec.isAnonymousArm` and the DD002/DT007 machinery of #158, plus plan 2's `Spec.namespacePath`. Rebase onto `develop` once #160 merges. Suggested branch: `fix/global-augmentation`.
- Doc comments follow `.claude/rules/comments.md`; reviewer-only notes use `//FOR-REVIEW`.
- `src/**/*.fs` stays Fantomas-formatted (`build.fsx` formats unless `--no-format`); goldens stay unformatted.
- Release notes are the commits' `BREAKING CHANGE:` paragraphs. Only release commits edit a `CHANGELOG.md`. Migration notes go in each package's `README.md`.

## Stages

| Stage | Tasks | Commits | Boundary |
|---|---|---|---|
| 1 | 1, 2, 3 | 2 | Task 1 measures and spikes and commits nothing. Tasks 2–3 pin the two adjacent global-binding defects. |
| 2 | 4 | 2 | Namespace-qualified foreign names and `ApiVersion` 10. The Core.TS catalogue is regenerated in the same task, so no later commit fails the catalogue suites. |
| 3 | 5 | 1 | Harvest of a module package's global scope and the augmentation inventory. Owners are still re-homed here, so nothing is lost relative to today. |
| 4 | 6, 7, 8 | 1 (Task 8) | Owner identity and extensions. Removing the re-homing without the extensions would drop the package's members, so Tasks 6–7 hand Task 8 an uncommitted tree. |
| 5 | 9 | 3 | Core.TS check (expected no diff), Node regeneration as its own breaking commit, measures. |

## Measures (taken while planning, 2026-10-15, frozen CLI at `91742f98`)

Planning evidence, under `tests/.scratch/pre158/plan-global-augmentation/`. It was generated with
`dotnet tests/.scratch/xantham-cli/xantham.dll generate <lab> -o <out>`, which predates plans 1–2.
- **`lab/`** is Task 5's lab: `global-augmentation-lab` with its dependency `augmentation-owner-lab`.
  The binding is **17,598 lines**: 11 exact, 718 ergonomic, 5 widened, 0 escape.
  - Whole copies are declared as the package's own: `ArrayConstructor`, `DateConstructor`,
    `Document` (+ `module Document`), `Intl` (+ `module Intl`), `Map<'K,'V>`, `Window`.
  - `env()` returns `AugmentationOwnerLab.ProcessEnv`. That is the flat templated name; after
    plan 2 the owner's golden declares `NodeJS.ProcessEnv`, so this reference is FS0039 (Task 4).
  - `PkgAdded` is declared flat at the root, not under `NodeJS`.
  - `g`, `h`, `c`, `PkgUnreached`, `Array.last`, `ProcessEnv.X` and `extraFmt` are absent, with no
    finding.
- **`out-owner/`**: `augmentation-owner-lab` generated alone declares nothing. Its type-only
  `NodeJS` namespace is plan 2's to harvest.
- **`mixed-module-root/`**: `mixed-subpaths-lab` plus `declare var counter: number`, with the module
  entry first. The output is `module Globals =` → `[<Erase>]` `[<Global("globalThis")>]`
  `type Exports`, holding `[<Global("rootCall")>] static member rootCall` and a bare
  `static member counter with get/set`.
  `tests/.scratch/pre158/e-global-augmentation/out-mixed/` (global entry first) has the same
  attributes on the root `Exports`.
  **This contradicts findings §1's "bare static on non-global Exports"**: the frozen CLI writes the
  type-level `[<Global("globalThis")>]` in both entry orders. Spike (k) in Task 1 decides whether
  the form fails from a DLL.
- **`out-cl-ref/`**: `compiler-lib-ownership-lab` with `entry: referenced-augmentation.d.ts`.
  `DateConstructor` is declared in `groups/TypeScript.Lib.fs`, not in the entry, because the run
  ships the compiler lib. Its one `Invoke` is the lib's `(): string`. The package's `(): number`
  call signature has the same parameter list and is not separately declared; that is existing
  behaviour and out of scope here.
- `tests/Xantham.Generator.Tests/golden/dom-shadow-lab/DomShadowLab.fs` is **13 lines**. All 21
  `xanthamDomShadowLabMarker` additions to DOM interfaces are absent today.
- Corpus census (findings §2, `census-global.txt`):
  - golden module packages with `declare global`: solid-js (2 blocks: `var Solid$$`, `SymbolConstructor` +1), type-fest (`SymbolConstructor` +1), augmented-dom-lab (`Request<T>`/`RequestInit<T>`, both adding a type parameter), symbol-member-lab (`SymbolConstructor` +1);
  - Node (`@types/node` 22.20.2): 20 global-script merges with lib types, 14 of them adding `extends RelativeIndexable`; `ImportMeta` +5, `ErrorConstructor` +3, `SymbolConstructor` +2; Node.fs declares **16 lib names as its own types**;
  - non-golden `@types/node` fixture under DOM: 11 overloads added to foreign global functions, 67 `var`s redeclaring DOM `var`s.

## Spike results

*Filled by Task 1 Step 5 and committed with Task 2.* One row per form (a)–(n): the form, the
consumer's emitted JavaScript, and pass/fail. Tasks 6–8 rely only on rows marked pass.

## Rulings

Controller rulings (C) are restated with their consequence. Planner rulings (P) each carry
*decision — why — cost if wrong*.

- **C1 — Additions to a foreign type are Emit-per-accessor extension members in an
  `[<AutoOpen>]` module.**
  - Properties carry `[<Emit("$0.n")>]` on `get` and `[<Emit("$0.n = $1")>]` on `set`.
  - Methods carry `[<Emit("$0.n($1, …)")>]`, spelled by `Render.literalCallTemplate`.
  - An added call signature is an `[<Emit("$0($1...)")>]` `Invoke`, an added construct signature
    an `[<Emit("new $0($1...)")>]` `Create`, and an added index signature an `Item` with
    per-accessor `[<Emit("$0[$1]")>]` / `[<Emit("$0[$1] = $2")>]`.
  - A symbol key keeps `MB009`'s `$0[Symbol.<name>]`.
  - `[<Erase>]` is ruled out: spike §1 shows a compile error from a DLL and an unexported import
    from source.
  - The forms the spike did not cover are verified in Task 1 before any task uses them: the array
    target, the indexer, the constructor, `Invoke`, optional/rest arguments, overloads beside
    intrinsic members, and the `As<Base>` upcast.
- **C2 — Globals sit in the `[<Erase; Global("globalThis")>]` container with bare settable
  statics; a writable static never carries `[<Global(name)>]`.**
  - **P1 (container spelling):** the container is ExportLayout's existing `GlobalScope`
    container. In a run with an entry owner that is `module Globals` holding `[<Erase>]`
    `[<Global("globalThis")>]` `type Exports` (`ExportLayout.fs:85`; `export-layout-lab` and
    `mixed-subpaths-lab` render it today). In a global-only run it is the root `Exports`.
  - *Why:* this is the form the run gate already proves from source
    (`LayoutLab.Globals.Exports.sharedFlag <- true`, `RunGate/Program.fs:2065`). Renaming it to a
    root `type Globals` gains no fidelity, and it collides with animejs's exported `Globals`
    interface (`golden/animejs/Animejs.fs:241`).
  - *Cost if wrong:* if the controller meant a root `type Globals`, `ExportLayout.preferredPath`
    and the run-gate references change. Plan 2's "Handled by other plans" wording ("a
    `[<Erase; Global("globalThis")>] Globals` type") should then be read as this container.
  - **P2 (member forms):** a `var` is a bare settable static under the type-level attribute (today's
    `renderBound`). A `function`, `const`, `let` or class value keeps its member-level
    `[<Global(name)>]`.
  - *Why:* ECMAScript makes only `var` and `function` declarations properties of the global object.
    A bare static under `globalThis` would read `globalThis.c`, which is `undefined` for a lexical
    `const c`. Spike (k) verifies that a member-level `[<Global("c")>]` inside the `globalThis`
    type still emits the bare `c`.
  - *Cost if wrong:* if (k) shows the member attribute is ignored there, lexical globals need a
    container of their own. Task 1 hands back before any task relies on this.
- **C3 — Remove the re-homing at `Resolve.fs:1466-1481`: the owner keeps its identity.** This
  reverses the 2026-09-13 "Augmented libraries" ruling (`generator-architecture.md:1611-1631`):
  "local augmentations retain their generic parameters and members". That record is amended, and
  so is the false premise at `generator-architecture.md:409-411`: "folding those in would emit
  names it does not export". Task 5 amends the premise and Task 8 the record.
- **C4 — An added type parameter keeps the original type and widens.** `augmented-dom-lab`'s
  `Request<T = unknown>` stays `Fable.Core.TS.Dom.Request`.
  - Its `cf` is an extension typed `obj option`, and `SI011 SI.AugmentationTypeParameterWidened`
    (Widened) is raised on `Request` and on `RequestInit`.
  - A reference applying the argument (`Request<{ foo: string }>`) drops it with the existing
    `TypeReference.LibExtraTypeArgumentsDropped`.
  - Rejecting valid TypeScript programs would be the larger infidelity.
- **C5 — The two adjacent defects are tasks: Task 2 (the mixed-run `declare var` from a DLL) and
  Task 3 (a writable static's setter emitting a call).** Planning evidence (Measures) shows the
  generator writes neither failing form today.
  - Task 2 reproduces the exact frozen-CLI container from a DLL (spike k) and pins it with a lab
    and the run gate. It changes `Render` only if (k) fails.
  - Task 3 pins the invariant that holds today (`renderBound`, `Render.fs:844-855`;
    `hoistedBinding`, `:823-824`) with Render unit tests.
- **C6 — The three new codes take the next free numbers:** `SI009 SI.AugmentationAsExtension`
  (Ergonomic), `SI010 SI.AugmentationHeritageNotInherited` (Widened) and
  `SI011 SI.AugmentationTypeParameterWidened` (Widened).
- **C7 — `ApiVersion` 9 → 10** (Task 4). Two changes alter the F# names a catalogue's API text
  carries:
  - lib and referenced namespace members are written under their namespace path (Task 4);
  - owners are no longer copied (Task 8).

  Both land before the next release, so one bump covers them.
- **C8 — Node is a breaking regeneration in its own commit with a `BREAKING CHANGE:` footer.** It
  loses 16 copied lib types (Task 9).
- **C9 — The compiler-lib-ownership tests are re-read** (Task 8 Step 6). The lab ships the
  compiler lib, so under P3 its owners are declared whole by their own group. Its pins hold, and
  one is added.
- **P3 — What is an augmentation.** It is a symbol whose first declaration classifies to a group
  `G` other than the entry package, that has at least one declaration under the package directory,
  and for which `G`'s disposition is not `Ship`.
  - When `G` ships in this run, the run declares the merged owner whole in `G`'s module. This is
    the compiler-lib-ownership lab and Core.TS's own `lib.es2025.iterator.d.ts` `declare global`.
  - *Why:* a shipped group's declaration is this program's declaration, merged members included.
    No other binding of it exists to extend.
  - *Cost if wrong:* a shipped core group carries an entry package's additions. That is today's
    behaviour in that configuration, and its certificate (`Pipeline.fs:16-53`) already marks such
    a scope unverified.
- **P4 — What a package adds, read per declaration.**
  - Properties and methods whose *first* declaration is the package's, inherited ones included.
    This is how `Window extends PkgMixin` contributes `mixed`.
  - Overloads, call, construct and index signatures whose own declaration handle is the package's.
  - Bases named in `extends` clauses of the package's declarations, read from the AST like
    `resolveImplementedTypes` reads `implements` (`Resolve.fs:660-700`).
  - Type parameters beyond the count the owner's first non-package declaration binds.
  - A member the owner declares first and the package redeclares adds nothing: TypeScript requires
    the redeclaration's type to be identical. A `var` redeclaring a foreign `var` belongs to its
    owner and is not recorded.
  - *Why:* this is the exact set of declarations the package wrote.
  - *Cost if wrong:* a redeclared member with a narrower JSDoc loses the package's doc text.
- **P5 — A package overload of a foreign global function is reported `AC001`**, not bound. Census:
  11, in the non-golden `@types/node` fixture under DOM, 0 in any golden or shipped binding.
  - *Why:* `Exports` shapes a value's merged call signatures, all overloads. Binding only the
    package's needs a per-declaration filter on the value path, which no golden exercises.
  - *Cost if wrong:* those overloads stay unbound, loudly; listed under "Reported, not in this plan".
- **P6 — A foreign namespace's members.**
  - A member the package declares first is the package's own type, declared in the package's
    module of that path (`NodeJS.PkgAdded` in `module NodeJS`). The walked foreign namespace
    joins `HarvestModel.Namespaces`, so plan 2's `Spec.namespacePath` nests the member.
  - A member the owner declares first and the package declares again is an augmentation like any
    other (`NodeJS.ProcessEnv.X`).
  - A *value* the package declares first under a foreign namespace is reported `AC001` (census: 0).
  - *Why:* TypeScript spells the new type `NodeJS.PkgAdded`, and F# can spell it the same way
    without touching the owner's module.
  - *Cost if wrong:* a consumer opening both the owner's binding and this one resolves
    `NodeJS.X` across two modules; spike §1 verified that this resolves.
- **P7 — An added base is reached through an `As<Base>` upcast, `SI010` (Widened).** Each added
  base gets `[<Emit("$0")>] member _.As<Leaf>: <Base>`. Base members the package declares first
  are extension members under P4. Members a foreign added base declares are reached through the
  upcast.
  - *Why:* F# cannot add an interface to a type it does not declare. The upcast is the faithful
    is-a relation, and copying foreign members would duplicate an owner's API.
  - *Cost if wrong:* `w.AsBase.member` where TypeScript writes `w.member`, for foreign-base
    members only.
- **P8 — Unextendable owners are reported `AC001`.** An owner whose group widens, so that
  `typeRef` gives no named target, is reported `AC001 AuditCoverage.ExportNotRepresented` on the
  owner path. So is a value augmentation (P5, P6). `audit-coverage` raises it.
  - *Why:* AC001 is the corpus's "declared API with no F# representation" code, and the pass that
    owns it is the safety net.
  - *Cost if wrong:* one code serves exports and augmentations; the symbol (`Window`, not
    `Exports.x`) tells them apart.
- **P9 — Extensions are not catalogue declarations.** `DeclarationCatalog` skips `FsAugmentation`
  as it skips `FsExports`.
  - *Why:* no consumer reference names an extension, and `AutoOpen` delivers it with the module.
  - *Cost if wrong:* a consumer that re-augments the same owner emits its own extensions. Identical
    extensions from two opened modules compile (spike §1, `DUP`).
- **P10 — One `[<AutoOpen>] module Augmentations` at the end of the entry module.**
  - Owners with one F# target merge into one block: `Array` and `ReadonlyArray` are both `'T[]`.
    A member repeated by label and signature is kept once.
  - The module is named `Augmentations_` where the entry module already declares a root name
    `Augmentations`.
  - *Why:* one extension block per target avoids FS0438 duplicate members.
  - *Cost if wrong:* nothing a consumer writes depends on the module name, since it is `AutoOpen`.
- **P11 — A lib or referenced namespace member is written under its namespace path** (Task 4).
  This applies to `Fable.Core.TS.Es.Intl.NumberFormatOptions` and
  `AugmentationOwnerLab.NodeJS.ProcessEnv`.
  - Plan 2 nests namespace members in the run that declares them. `Spec.libBinding` and
    `Spec.referencedRef` still write `<Module>.<Name>` (`Spec.fs:2213-2220`, `:2305`), and the
    identity-only resolve branch records no parent (`Resolve.fs:1629-1640`). After plan 2's Core.TS
    regeneration, every reference to an `Intl`/`WebAssembly`/`Temporal` member from another binding
    is therefore FS0039. So is every extension head on one.
  - *Why:* TypeScript spells `Intl.NumberFormatOptions`, and the declaring binding now does too.
  - *Cost if wrong:* if plan 2 already landed an equivalent, Task 4 Step 1 detects it and the task
    reduces to the `ApiVersion` bump. **Reported to the controller as a plan 2 gap.**

## Review Focus

1. **A generic owner whose binding takes type parameters** (`Map<'K, 'V>`,
   `Uint8Array<'TArrayBuffer>`). The extension head must restate the owner's parameters as Core.TS
   declares them; a mismatch is FS0957. Pinned by the lab's `Map` and `Uint8Array` additions, the
   Task 7 consumer compile test, and the compile gate.
2. **Two owners written as one F# type** (`Array<T>` and `ReadonlyArray<T>`, both `'T[]`) adding
   the same member. Expected: one `last` in one `'T[]` block, not FS0438. Pinned in Task 6
   ("owners sharing an F# type share one extension").
3. **An added overload or signature beside an intrinsic member of the same name**
   (`Document.createElement`, `ArrayConstructor.Create`, `DateConstructor.Invoke`). A consumer must
   be able to call it. Pinned by spike (h) and by the Task 7 consumer compile test.
4. **A module package's module-local declaration** (`interface PkgMixin`, not exported). It must
   not be harvested as a global, and an export must not be bound twice. Pinned in Task 5 ("module
   locals and exports are not globals").
5. **An owner whose group ships in the run.** The compiler-lib-ownership lab and `nested-dep-lab`'s
   shipped groups keep the merged declaration in the owner's group and write no extension. Pinned
   in Task 8 Step 6.

## File map

| File | Change | Task |
|---|---|---|
| `tests/.scratch/global-augmentation/spike/**` | extended spike (scratch) | 1 |
| `tests/fixtures/mixed-subpaths-lab/{index.d.ts,index.js}` | `declare var counter` | 2 |
| `tests/Xantham.Generator.RunGate/Program.fs` | mixed-run counter check; global-augmentation checks | 2, 8 |
| `tests/Xantham.Generator.Tests/Render.test.fs` | settable-static invariant pins; extension rendering | 3, 7 |
| `src/Xantham.Generator/Model.fs` | `TypeFacts.NamespacePath`; `HarvestedAugmentation`; `HarvestModel.Augmentations`; `ResolvedAugmentation`; `ResolveModel`/`ShapeModel.AugmentationFacts`; `FsAugmentationDecl`; `FsDecl.FsAugmentation` | 4, 5, 6, 7 |
| `src/Xantham.Generator/Resolve.fs` | `namespacePathOf`; re-homing removed; `resolveSignaturesWhere`; `packageHeritage`; `resolveAugmentations`; `passes` | 4, 6 |
| `src/Xantham.Generator/Shape/Spec.fs` | namespace-qualified `libBinding`/`referencedRef`; `augmentedArity` | 4, 6 |
| `src/Xantham.Generator/CatalogCompatibility.fs:33`, `tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs` | `ApiVersion = 10` + pins | 4 |
| `src/Xantham.Fable.Core.TS/{declarations.json,catalogue-inputs.json,…}` | catalogue at API 10 | 4 |
| `src/Xantham.Generator/Harvest.fs` | global scope in module runs; augmentation inventory; foreign namespace walk | 5 |
| `tests/fixtures/global-augmentation-lab/**` (+ `node_modules/augmentation-owner-lab/**`) | new lab | 5 |
| `tests/Xantham.Generator.Tests/Pipeline.test.fs` | new lab blocks; `foreign-namespace-lab`, `dom-shadow-lab`, `augmented-dom-lab`, `compiler-lib-ownership-lab`, `mixed-subpaths-lab` pins | 2, 5, 6, 8 |
| `tests/Xantham.Generator.Tests/{Shape,CatalogLibraryEntry,CatalogMember}.test.fs`, `Build.fs` | record literals gain `Augmentations`/`AugmentationFacts`; Task 4 unit cases | 4, 5, 6 |
| `src/Xantham.Generator/Findings.fs`, `tests/Xantham.Generator.Tests/Findings.test.fs` | `SI009`–`SI011` | 7 |
| `src/Xantham.Generator/Shape/Anonymous.fs` | walk augmentation types | 7 |
| `src/Xantham.Generator/Shape/Augmentations.fs` (new), `Xantham.Generator.fsproj`, `Shape/Passes.fs` | `shape-augmentations` | 7 |
| `src/Xantham.Generator/Render.fs` | `renderExtensionMember`, `renderAugmentation`, `renderBody` | 7 |
| every `FsDecl` match site (list in Task 7 Step 7) | `FsAugmentation` arm | 7 |
| `src/Xantham.Generator/Shape/Coverage.fs` | augmentation audit | 8 |
| `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs:1371-1394` | augmented-DOM consumer rewritten | 8 |
| `tests/Xantham.Generator.RunGate/Xantham.Generator.RunGate.fsproj` | link the two new goldens | 8 |
| `docs/.ai/plans/generator-architecture.md`, `generator-type-mapping.md`, `docs/.ai/footguns.md` | premise, record amendment, phase record, §4.13, footguns | 5, 8 |
| `src/Xantham.Fable.Node/**` (incl. `README.md`) | regenerated, migration note | 9 |

---

### Task 1: Spike the unverified extension forms, take the baselines

Commits nothing. Its results fill this plan's **Spike results** section, which Task 2 commits.

**Files:**
- Create (scratch): `tests/.scratch/global-augmentation/spike/` (copied from `tests/.scratch/pre158/e-global-augmentation/spike/`)
- Create (scratch): `tests/.scratch/global-augmentation/{findings-before,census-before}.txt`

**Interfaces:**
- Produces: the pass/fail table that Tasks 2, 6, 7 and 8 read. A form marked fail stops the task that would use it, and that task hands back.

- [ ] **Step 1: Copy the spike**

```
rtk mkdir -p tests/.scratch/global-augmentation
cp -r tests/.scratch/pre158/e-global-augmentation/spike tests/.scratch/global-augmentation/spike
rm -rf tests/.scratch/global-augmentation/spike/Consumer/fable-out* tests/.scratch/global-augmentation/spike/*/bin tests/.scratch/global-augmentation/spike/*/obj
```

The empty `Directory.Build.props`/`.targets` it carries keep the repository's build settings out
(`.claude/rules/tests.md`).

- [ ] **Step 2: Replace `Foreign/Foreign.fs`**

```fsharp
module rec Foreign

open Fable.Core

[<Interface>]
type Window =
    abstract own: string with get, set

[<Interface>]
type Mixin =
    abstract mixed: bool

[<Interface>]
type Box<'T> =
    abstract value: 'T with get, set

[<Interface>]
type Bag =
    abstract size: float

/// A constructor object with intrinsic members named like the ones extensions add.
[<Interface>]
type Maker =
    [<EmitConstructor>]
    abstract Create: unit -> Window
    [<Emit("$0($1...)")>]
    abstract Invoke: unit -> string
    abstract go: x: float -> string

module NodeJS =
    [<Interface>]
    type ProcessEnv =
        [<EmitIndexer>]
        abstract Item: key: string -> string option with get, set

[<Erase>]
type Exports =
    [<Global("window")>]
    static member window: Window = jsNative
    [<Global("env")>]
    static member env: NodeJS.ProcessEnv = jsNative
    [<Global("box")>]
    static member box: Box<float> = jsNative
    [<Global("bag")>]
    static member bag: Bag = jsNative
    [<Global("maker")>]
    static member maker: Maker = jsNative
    [<Global("numbers")>]
    static member numbers: float[] = jsNative
```

- [ ] **Step 3: Append the forms under test to `SpikeLib/SpikeLib.fs`**

Append inside `module Augmentations` (after its `module NodeJS`, same indentation):

```fsharp
    // (a) array target, spelling A; spelling B under ARRAY_B
#if ARRAY_B
    type ``[]``<'T> with
#else
    type 'T ``[]`` with
#endif
        [<Emit("$0.last()")>]
        member _.last() : 'T option = jsNative

    // (b) generic owner, qualified head
    type Foreign.Box<'T> with
        member _.peek
            with [<Emit("$0.peek")>] get (): 'T = jsNative
            and [<Emit("$0.peek = $1")>] set (value: 'T): unit = jsNative

    // (c) indexer: per-accessor Emit; EmitIndexer under EMIT_INDEXER
    type Foreign.Bag with
#if EMIT_INDEXER
        [<EmitIndexer>]
        member _.Item
            with get (key: string): float = jsNative
            and set (key: string) (value: float): unit = jsNative
#else
        member _.Item
            with [<Emit("$0[$1]")>] get (key: string): float = jsNative
            and [<Emit("$0[$1] = $2")>] set (key: string) (value: float): unit = jsNative
#endif

    type Foreign.Maker with
        // (d) construct signature: Emit("new ..."); EmitConstructor under EMIT_CONSTRUCTOR
#if EMIT_CONSTRUCTOR
        [<EmitConstructor>]
#else
        [<Emit("new $0($1...)")>]
#endif
        member _.Create(marker: bool) : Foreign.Window = jsNative
        // (e) call signature
        [<Emit("$0($1...)")>]
        member _.Invoke(flag: bool) : string = jsNative
        // (h) overload beside an intrinsic method of the same name
        [<Emit("$0.go($1, $2)")>]
        member _.go(x: string, flag: bool) : string = jsNative
        // (f) optional and rest arguments
        [<Emit("$0.opt($1, $2...)")>]
        member _.opt(a: float, ?b: string) : string = jsNative
        [<Emit("$0.rest($1...)")>]
        member _.rest([<System.ParamArray>] values: float[]) : string = jsNative

    type Foreign.Window with
        // (g) symbol-keyed method
        [<Emit("$0[Symbol.dispose]()")>]
        member _.SymbolDispose() : unit = jsNative
        // (j) upcast to an added base
        [<Emit("$0")>]
        member _.AsMixin: Foreign.Mixin = jsNative
```

Append at the end of the file (top level of `module rec SpikeLib`):

```fsharp
/// (k) The frozen CLI's mixed-run global container, as `mixed-module-root` renders it, plus a
/// lexical global (l).
module MixedGlobals =
    [<Erase>]
    [<Global("globalThis")>]
    type Exports =
        [<Global("rootCall")>]
        static member rootCall (options: obj) : string = jsNative
        [<Global("c")>]
        static member c: string = jsNative
        static member counter
            with get (): float = jsNative
            and set (_: float): unit = jsNative
```

- [ ] **Step 4: Append the consumer and the runtime**

Append to `Consumer/Program.fs`, before its `JS.console.log` line:

```fsharp
let last = Exports.numbers.last ()
let peek = Exports.box.peek
Exports.box.peek <- 2.0
let item = Exports.bag.["k"]
Exports.bag.["k"] <- 3.0
let made = Exports.maker.Create true
let invoked = Exports.maker.Invoke true
let went = Exports.maker.go ("s", true)
let opt1 = Exports.maker.opt 1.0
let opt2 = Exports.maker.opt (1.0, "b")
let rested = Exports.maker.rest (1.0, 2.0)
Exports.window.SymbolDispose ()
let upcast = Exports.window.AsMixin.mixed
let counter = MixedGlobals.Exports.counter
MixedGlobals.Exports.counter <- 5.0
let rooted = MixedGlobals.Exports.rootCall null
let lexical = MixedGlobals.Exports.c
JS.console.log (last, peek, item, made, invoked, went, opt1, opt2, rested, upcast, counter, rooted, lexical)
```

Append to `globals.mjs`:

```js
globalThis.numbers = Object.assign([1, 2, 3], { last() { return this[this.length - 1]; } });
globalThis.box = { value: 1, peek: 1 };
globalThis.bag = { size: 0, k: 1 };
function maker(flag) { return new.target ? { own: "made:" + flag } : "called:" + flag; }
maker.go = (x, flag) => "go:" + x + flag;
maker.opt = (a, b) => "opt:" + a + (b ?? "");
maker.rest = (...values) => "rest:" + values.join(",");
globalThis.maker = maker;
Object.assign(globalThis.window, { mixed: true, [Symbol.dispose]() { globalThis.disposed = true; } });
globalThis.counter = 0;
globalThis.rootCall = () => "root";
globalThis.c = "c";
```

- [ ] **Step 5: Run each variant and record**

From `tests/.scratch/global-augmentation/spike/Consumer`:

```
dotnet build
dotnet fable . --noCache --exclude SpikeLib --exclude Foreign -o fable-out
node --import ../globals.mjs fable-out/Program.js
```

Repeat the three commands after rebuilding SpikeLib with each variant:
`dotnet build ../SpikeLib -p:DefineConstants=ARRAY_B`, then `EMIT_INDEXER`, then `EMIT_CONSTRUCTOR`.

`dotnet build` proves F# resolution. Fable `--exclude` consumes SpikeLib as a DLL. Expected
JavaScript in `fable-out/Program.js` (grep for each):

| Form | Expected |
|---|---|
| (a) `'T ``[]`` with` / B `` ``[]``<'T> with `` | `numbers.last()` (record which spelling compiles in .NET and in Fable) |
| (b) generic qualified head | `box.peek` and `box.peek = 2` |
| (c) indexer per-accessor / EmitIndexer | `bag["k"]` and `bag["k"] = 3` |
| (d) `Emit("new $0($1...)")` / EmitConstructor | `new maker(true)` |
| (e) `Invoke` | `maker(true)` |
| (f) optional / rest | `maker.opt(1)`, `maker.opt(1, "b")`, `maker.rest(1, 2)` |
| (g) symbol-keyed | `window[Symbol.dispose]()` |
| (h) overloads beside intrinsic `Create`/`Invoke`/`go` | the three calls above resolve to the *extension* (the .NET build succeeds and the JS shows the extension's template) |
| (i) qualified heads `type Foreign.X with` in `module rec` | compiles |
| (j) `As<Base>` | `window.mixed` |
| (k) frozen mixed container from a DLL | `globalThis.counter`, `globalThis.counter = 5`, `rootCall(null)` or `globalThis.rootCall(null)` |
| (l) member `[<Global("c")>]` inside the `globalThis` type | bare `c`; `globalThis.c` is a **fail** |
| (m) `member _.` self identifier | compiles (every form above uses it) |
| (n) `node` run | prints the values without a TypeError |

Write one row per form into this plan's **Spike results** section. Any fail:
- (a): if neither spelling works, hand back before Task 6. Array and ReadonlyArray additions have
  no form.
- (c): if only `EmitIndexer` passes, Task 7 Step 5 renders that form instead.
- (d): if only `EmitConstructor` passes, Task 7 Step 5 renders that form instead.
- (e), (f), (g), (h), (i) or (j): hand back with the Fable or FS error before Task 6.
- (k): Task 2 Step 4 applies.
- (l): hand back before Task 5.

- [ ] **Step 6: Baselines**

```
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
dotnet fsi build.fsx -- findings > tests/.scratch/global-augmentation/findings-before.txt
node tests/.scratch/pre158/e-global-augmentation/census-global.cjs "$PWD" "$TSJS" > tests/.scratch/global-augmentation/census-before.txt
rtk wc -l src/Xantham.Fable.Node/Node.fs tests/Xantham.Generator.Tests/golden/*/*.fs > tests/.scratch/global-augmentation/lines-before.txt
```

The census script's lib path names `typescript-win32-x64`; on Linux, replace it with
`typescript-linux-x64`. Expected: the totals in findings §2. A different count is written into
this plan's Measures in Task 8's commit.

---

### Task 2: A mixed-run mutable global is pinned from both entry orders

**Files:**
- Modify: `tests/fixtures/mixed-subpaths-lab/index.d.ts`, `tests/fixtures/mixed-subpaths-lab/index.js`
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs:5419-5440`
- Modify: `tests/Xantham.Generator.RunGate/Program.fs:2110-2119`
- Modify (only if spike (k) failed): `src/Xantham.Generator/Render.fs` (`renderBound`, `:844-855`)
- Modify: this plan (Spike results)

**Interfaces:**
- Consumes: spike (k).
- Produces: nothing later tasks call. The pinned container form is the one Task 5's module-run globals use.

- [ ] **Step 1: Add the global to the lab**

Append to `tests/fixtures/mixed-subpaths-lab/index.d.ts`:

```ts
/** A writable global beside a module subpath: assignment writes through to globalThis. */
declare var counter: number;
```

Append to `tests/fixtures/mixed-subpaths-lab/index.js`:

```js
globalThis.counter = 0;
```

- [ ] **Step 2: Write the failing pins**

In `Pipeline.test.fs`, in `"global root declarations survive module subpaths"` (`:5420`), after its
last `Expect`:

```fsharp
                      Expect.stringContains
                          source
                          "[<Erase>]\n[<Global(\"globalThis\")>]\ntype Exports ="
                          "a container holding a writable global binds through globalThis"
                      Expect.stringContains
                          source
                          "    static member counter\n        with get (): float = jsNative\n        and set (_: float): unit = jsNative"
                          "the writable global is a bare settable static"
                      Expect.isFalse (source.Contains "[<Global(\"counter\")>]") "and carries no binding of its own"
```

In `"a global subpath is harvested from its global scope beside a module root"` (`:5428`), after
its last `Expect`:

```fsharp
                          Expect.stringContains
                              source
                              "module Globals =\n    /// <summary>The package's value exports, each bound to its import.</summary>\n    [<Erase>]\n    [<Global(\"globalThis\")>]\n    type Exports ="
                              "beside a module root, the globals container binds through globalThis"
                          Expect.isFalse (source.Contains "[<Global(\"counter\")>]") "its writable global carries no binding of its own"
```

In `RunGate/Program.fs`, append to `mixedSubpaths ()`:

```fsharp
    equal "a mutable global beside a module subpath reads globalThis" 0. MixedSubpathsLab.Exports.counter
    MixedSubpathsLab.Exports.counter <- 5.
    check "and an assignment writes through to globalThis" (emitJsExpr () "globalThis.counter === 5")
```

- [ ] **Step 3: Run**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`, then
`dotnet fsi build.fsx -- test --quick --run-gate`.
Expected: PASS, from the Measures evidence (both orders already render the type-level attribute).
`git diff -- tests/Xantham.Generator.Tests/golden/mixed-subpaths-lab` shows `counter` added and
nothing else.

- [ ] **Step 4: Only if spike (k) failed**

A failure means Fable rejects a bare static of a DLL `[<Erase>]` container even under the
type-level attribute. Record the exact Fable message in the Spike results and hand back: no form
for a writable global from a DLL is verified, so this task changes no Render code without a new
ruling.

- [ ] **Step 5: Commit**

```
rtk git add tests/fixtures/mixed-subpaths-lab tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden/mixed-subpaths-lab tests/Xantham.Generator.RunGate/Program.fs docs/.ai/plans/2026-10-15-global-augmentation.md
rtk git commit -m "test(generator): pin a mixed-run mutable global from both entry orders" -m "Spike results for the global-augmentation extension forms recorded in the plan." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task 3: A writable static never carries a member-level binding

**Files:**
- Modify: `tests/Xantham.Generator.Tests/Render.test.fs` (after `"renamed export members retain every runtime target spelling"`, `:940-967`)
- Modify: `docs/.ai/footguns.md`

**Interfaces:**
- Consumes: `Render.test.fs` helpers `baseModel`, `exportContainer`, `exportValue`, `exportFunction`, `renderAll`.
- Produces: the invariant Task 5's module-run globals rely on.

- [ ] **Step 1: Write the pins**

```fsharp
        testCase "a writable global binds through its container, never through a member attribute" <| fun _ ->
            let container =
                exportContainer
                    "Exports"
                    GlobalScope
                    [ exportValue "g" (GlobalName "globalThis") true FsFloat
                      exportFunction "h" (GlobalName "h") [] FsUnit
                      exportValue "c" (GlobalName "c") false FsString ]

            let source = renderAll { baseModel with Decls = [ container ] } |> Map.find "TestPkg.fs"

            Expect.stringContains source "[<Erase>]\n[<Global(\"globalThis\")>]\ntype Exports =" "the container carries the object"
            Expect.stringContains source "    static member g\n        with get (): float = jsNative" "the writable global is bare"
            Expect.isFalse (source.Contains "[<Global(\"g\")>]") "a member attribute would compile the setter to the call g(v)"
            Expect.stringContains source "    [<Global(\"h\")>]\n    static member h" "a function keeps its own binding"
            Expect.stringContains source "    [<Global(\"c\")>]\n    static member c: string" "a lexical global keeps its own binding"

        testCase "a writable class static binds through its class" <| fun _ ->
            let counter =
                FsInterface
                    { Name = "Counter"; Docs = ""; Tags = []; Order = None; TypeParameters = []; Inherits = []
                      Members = []; Entrypoint = None; CreateOverloads = []
                      Statics =
                        [ exportValue "tick" (ImportNamed "Counter") true FsFloat
                          exportValue "MAX" (ImportNamed "Counter.MAX") false FsFloat ] }

            let source = renderAll { baseModel with Decls = [ counter ] } |> Map.find "TestPkg.fs"

            Expect.stringContains source "[<Import(\"Counter\", \"test-pkg\")>]\ntype Counter =" "the class carries the binding"
            Expect.isFalse (source.Contains "[<Import(\"Counter\", \"test-pkg\")>]\n    static member tick") "the settable static carries none"
            Expect.stringContains source "    [<Import(\"Counter.MAX\", \"test-pkg\")>]\n    static member MAX" "a get-only static keeps its dotted selector"
```

If `exportContainer`'s `GlobalScope` owner or the `FsInterface` field list differs, copy the field
list from `"nested exports qualify local shadows…"` (`Render.test.fs:968+`).

- [ ] **Step 2: Run**

Run: `dotnet fsi build.fsx -- test --quick --filter "render"` → PASS (`renderBound` writes no
attribute for a settable value, `Render.fs:844-855`; `hoistedBinding` puts it on the type,
`:823-824`). A FAIL is a real defect: hand it back with the rendered text.

- [ ] **Step 3: Footgun**

Append to `docs/.ai/footguns.md`, after "Compiler-library shipping must not depend on module exports":

```markdown
**A writable static binds through its declaring type.** Under a member-level `[<Global("g")>]` or
`[<Import>]`, Fable compiles the getter to `g` but the setter `X.g <- v` to the call `g(v)`, a
TypeError. `renderBound` writes no attribute on a settable member, and `hoistedBinding` puts the
binding on the type (`globalThis` for a global `var`, the class for a static).
`Render.test.fs` pins both.
```

- [ ] **Step 4: Commit**

```
rtk git add tests/Xantham.Generator.Tests/Render.test.fs docs/.ai/footguns.md
rtk git commit -m "test(generator): pin that a writable static binds through its declaring type" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task 4: Lib and referenced namespace members are written under their namespace; API 10

**Files:**
- Modify: `src/Xantham.Generator/Model.fs` (`TypeFacts`, `:1257-1333`; `TypeFacts.shallow`, `:1338-1370`)
- Modify: `src/Xantham.Generator/Resolve.fs` (new `namespacePathOf` above `resolveMember`, `:1018`; the `TypeFlags.Object` branch, `:1464-1640`; the full record at `:1700-1817`)
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (`libBinding`, `:2192-2252`; `referencedRef`, `:2293-2318`)
- Modify: `src/Xantham.Generator/CatalogCompatibility.fs:33`, `tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs:38-52`
- Test: `tests/Xantham.Generator.Tests/Shape.test.fs` (in `testList "shape passes"`, `:1511`)
- Modify: `src/Xantham.Fable.Core.TS/{declarations.json,catalogue-inputs.json}` (regenerated)

**Interfaces:**
- Consumes: plan 2's nested Core.TS (`Fable.Core.TS.Es.Intl.NumberFormatOptions`).
- Produces: `TypeFacts.NamespacePath: string list` (TypeScript namespaces enclosing the declaration, outermost first; `[]` at top level), and `CatalogCompatibility.current(...).ApiVersion = 10`.

- [ ] **Step 1: Check whether plan 2 landed the equivalent**

Run `rtk grep -n "NamespacePath\|namespacePathOf" src/Xantham.Generator` and read
`Spec.referencedRef`. If a lib or referenced type's name already carries its namespace path,
record so in this task's commit body. Then skip Steps 2–5 and 7 and keep Steps 6, 8 and 9 (the API
bump).

- [ ] **Step 2: Write the failing unit tests**

Append to `testList "shape passes"`:

```fsharp
        testCase "a lib namespace member is written under its namespace's Core.TS module" <| fun _ ->
            let options =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some((fun value -> value * uom<symbolName>) "NumberFormatOptions")
                    DeclFile = Some((fun value -> value * uom<declFile>) "/ts/lib/lib.es5.d.ts")
                    NamespacePath = [ "Intl" ] }

            let reference, _ = Spec.typeRef Build.context (Build.shapeModel (options :: Build.primitives)) None "Holder.options" 50<typeId>

            Expect.equal reference (FsNamed "Fable.Core.TS.Es.Intl.NumberFormatOptions") "TypeScript spells Intl.NumberFormatOptions"

        testCase "a referenced namespace member is written under its namespace in the group's module" <| fun _ ->
            let env =
                { Build.facts (Build.typeResponse 51 TypeFlags.Object) with
                    Origin = Dependency("augmentation-owner-lab" * uom<npmDependency>)
                    SymbolName = Some((fun value -> value * uom<symbolName>) "ProcessEnv")
                    NamespacePath = [ "NodeJS" ] }

            let ctx =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups = Map.ofList [ "augmentation-owner-lab" * uom<npmDependency>, Reference ] } }

            let reference, _ = Spec.typeRef ctx (Build.shapeModel (env :: Build.primitives)) None "Exports.env" 51<typeId>

            Expect.equal reference (FsNamed "AugmentationOwnerLab.NodeJS.ProcessEnv") "a ship run of the group nests it there"
```

- [ ] **Step 3: Run to verify they fail**

Run: `dotnet fsi build.fsx -- test --quick --filter "shape passes"`.
Expected: FAIL to compile: `NamespacePath` is not a field of `TypeFacts`.

- [ ] **Step 4: The field and its resolution**

`Model.fs`, in `TypeFacts` after `SymbolParent` (`:1276`):

```fsharp
        /// The TypeScript namespaces enclosing the declaration, outermost first: `["Intl"]` for
        /// `Intl.NumberFormatOptions`. Empty at a module's top level and for an anonymous type.
        NamespacePath: string list
```

In `TypeFacts.shallow`, after `SymbolParent = None`: `NamespacePath = []`.

`Resolve.fs`, directly above `resolveMember` (`:1018`):

```fsharp
/// The TypeScript namespaces enclosing a symbol's declaration, outermost first. A source file's
/// module symbol or an ambient module (`"node:fs"`) ends the path: neither is a namespace.
let rec private namespacePathOf (ctx: Context) (symbol: SymbolResponse voption) : Async<string list> =
    async {
        match symbol with
        | ValueSome current when ValueOption.isSome current.Parent ->
            let! parent = ctx.Session.getParentOfSymbol current.Id

            match parent with
            | ValueSome owner when
                hasAny SymbolFlags.Module owner.Flags && Naming.isWritableTypeName owner.Name
                ->
                let! outer = namespacePathOf ctx parent
                return outer @ [ owner.Name ]
            | _ -> return []
        | _ -> return []
    }
```

In the `has TypeFlags.Object` branch, directly after `let firstOrigin = …` (`:1466`), add
`let! namespacePath = namespacePathOf ctx symbol`. Set `NamespacePath = namespacePath` in the
identity-only return (`{ TypeFacts.shallow ty with Origin = origin … }`, `:1629`). Set it also in
the full record of the derived branch (the literal ending `AliasIdentity = None; UnresolvedName =
None`, `:1815-1816`). `rtk grep -n "AliasIdentity = None" src/Xantham.Generator` lists every full
`TypeFacts` literal. Each one that is not `shallow` gets `NamespacePath = []`, or `namespacePath`
where the branch has a symbol in scope. That includes `Customization/Semantics.fs:881` if it
constructs one.

- [ ] **Step 5: Qualify the lib and referenced names**

`Spec.fs`, directly above `let rec typeRef` (`:1715`):

```fsharp
/// `name` under the declaration's TypeScript namespaces, each segment as an F# module spells it:
/// `Intl.NumberFormatOptions`.
let internal namespaceQualified (facts: TypeFacts) (name: string) =
    (facts.NamespacePath |> List.map Naming.pascalSegment) @ [ name ] |> String.concat "."
```

In `libBinding`, replace the two `Some($"Fable.Core.TS.Dom.{name}", …)` /
`Some($"Fable.Core.TS.Es.{name}", …)` constructions (`:2213`, `:2220`) with
`$"Fable.Core.TS.Dom.{namespaceQualified facts name}"` and
`$"Fable.Core.TS.Es.{namespaceQualified facts name}"`. `Naming.LibBindings.tryFind name` (bare
lib names `Promise`, `PromiseLike`, `Error`) is unchanged.

In `referencedRef`, `let name = $"{moduleName}.{typeName}"` (`:2305`) becomes
`let name = $"{moduleName}.{namespaceQualified facts typeName}"`.

- [ ] **Step 6: Pin API 10**

`CatalogCompatibility.fs:33`: `ApiVersion = 10`.

In `CatalogCompatibility.test.fs`, the `"the current contract pins every version"` expectation becomes:

```fsharp
                (1, 6, 10, 3, 1)
                "identity 6 authenticates constructor fallback bounds and sanitized aliases; API 10 writes lib and referenced namespace members under their namespace and augmentations as extensions; inference 3 preserves source contracts at the width cutoff"
```

Add after it:

```fsharp
        testCase "the API contract rejects catalogues built before global augmentation" <| fun _ ->
            rejects "API version" (fun () ->
                validateContract 2 expected.Compiler expected.Generator "profile"
                    (Some { expected.Contract with ApiVersion = 9 }))
```

Plan 2's API 9 rejection case stays.

- [ ] **Step 7: Run the units, then the e2e suite**

Run: `dotnet fsi build.fsx -- test --quick --filter "shape passes"` → PASS, and
`dotnet fsi build.fsx -- test --quick --filter "catalog compatibility"` → PASS.
Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`, then
`rtk git diff --stat -- tests/Xantham.Generator.Tests/golden` and
`rtk grep -rln "Fable.Core.TS.Es.Intl\.\|Fable.Core.TS.Es.WebAssembly\.\|Fable.Core.TS.Es.Temporal\." tests/Xantham.Generator.Tests/golden`.
Expected: only references to lib or referenced namespace members move, each now qualified.
Report the moved goldens and their counts. Any other moved line is handed back.

- [ ] **Step 8: Regenerate the Core.TS catalogue**

Run: `dotnet fsi build.fsx -- generate --only compiler-lib`, then
`rtk git diff --stat -- src/Xantham.Fable.Core.TS`.
Expected: `declarations.json` (`"apiVersion": 10`) and `catalogue-inputs.json` change, and
`Fable.Core.TS.fs` does not. Core.TS ships the compiler lib and names its own references from
`DeclNames`, not `libBinding`. A diff in `Fable.Core.TS.fs` is handed back.

- [ ] **Step 9: Gate and commit**

Run: `dotnet build Xantham.slnx`, then `dotnet fsi build.fsx -- test` → PASS.

```
rtk git add src/Xantham.Generator/Model.fs src/Xantham.Generator/Resolve.fs src/Xantham.Generator/Shape/Spec.fs src/Xantham.Generator/CatalogCompatibility.fs tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs tests/Xantham.Generator.Tests/Shape.test.fs tests/Xantham.Generator.Tests/golden
rtk git status
rtk git commit -m "fix(generator)!: write a lib or referenced namespace member under its namespace" -m "<moved goldens and counts from Step 7, or 'plan 2 landed the qualification; API bump only'>" -m "BREAKING CHANGE: a binding's references to a standard-library or referenced namespace member carry the namespace (Fable.Core.TS.Es.Intl.NumberFormatOptions). Declaration catalogues built before this change (API 9) are rejected." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
rtk git add src/Xantham.Fable.Core.TS
rtk git commit -m "chore(core-ts)!: regenerate the declaration catalogue at API 10" -m "BREAKING CHANGE: the packaged catalogue is API version 10; generators before this release reject it. The F# surface is unchanged." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

Fill the first `-m` body from Step 7. `rtk git status` must show `.serena/project.yml` unstaged.

---

### Task 5: Harvest a module package's global scope and inventory its augmentations

**Files:**
- Create: `tests/fixtures/global-augmentation-lab/{package.json,xantham.json,index.d.ts,index.js}`
- Create: `tests/fixtures/global-augmentation-lab/node_modules/augmentation-owner-lab/{package.json,index.d.ts}`
- Modify: `src/Xantham.Generator/Model.fs` (`HarvestedAugmentation` after `HarvestedExport`, `:1124-1143`; `HarvestModel`, `:1145-1171`)
- Modify: `src/Xantham.Generator/Harvest.fs` (`harvestGlobals`, `:316-510`; header comment of the pass, `:313-315`)
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (new `[<Tests>]` list at the end; plan 2's `foreign-namespace-lab` block)
- Modify: every test record literal of `HarvestModel` (Step 5)
- Modify: `docs/.ai/plans/generator-architecture.md:409-411`, `docs/.ai/footguns.md` ("Compiler-library shipping must not depend on module exports")

**Interfaces:**
- Consumes: plan 2's `HarvestModel.Namespaces` (dotted) and `Spec.namespacePath`; `followAlias` (`Harvest.fs:51-59`); `Grouping.classify`/`classifyFile`/`declaresUnderPackage`/`declOrder`.
- Produces:
  - `type HarvestedAugmentation = { Owner: SymbolResponse; Path: string; Order: DeclOrder option }`
  - `HarvestModel.Augmentations: HarvestedAugmentation list`, sorted by `Path`, one per owner symbol
  - module-run `FromGlobal` exports for the package's own globals
  - `HarvestModel.Namespaces` entries for walked foreign namespaces (`"NodeJS"`)

- [ ] **Step 1: The lab**

`tests/fixtures/global-augmentation-lab/package.json`:

```json
{"name":"global-augmentation-lab","type":"module","types":"index.d.ts","dependencies":{"augmentation-owner-lab":"*"},"description":"Hand-authored fixture: a module package's `declare global` block. Its own globals are bound under Globals; its additions to types another group declares are extensions of the owner's binding (docs/.ai/plans/2026-10-15-global-augmentation.md). The dependency is referenced, and its golden is generated on its own. Tracked in git, no npm install."}
```

`tests/fixtures/global-augmentation-lab/xantham.json`:

```json
{
    // The DOM lib supplies Window and Document. The dependency is templated (`reference`) and
    // its own golden is generated standalone, so the compile gate decides whether the templated
    // NodeJS.ProcessEnv is the shipped one.
    "lib": ["esnext", "dom"],
    "types": [],
    "groups": { "augmentation-owner-lab": "reference" }
}
```

`tests/fixtures/global-augmentation-lab/node_modules/augmentation-owner-lab/package.json`:

```json
{"name":"augmentation-owner-lab","types":"index.d.ts","description":"The owner of `NodeJS` for global-augmentation-lab: a global script, as @types/node is."}
```

`tests/fixtures/global-augmentation-lab/node_modules/augmentation-owner-lab/index.d.ts`:

```ts
declare namespace NodeJS {
    interface ProcessEnv { [key: string]: string | undefined }
}
```

`tests/fixtures/global-augmentation-lab/index.d.ts`:

```ts
/// <reference types="augmentation-owner-lab" />

/** A base only this package's declaration of `Window` adds. Module-local: not a global. */
interface PkgMixin { mixed: boolean }

declare global {
    /** A writable global. */
    var g: number;
    /** A global function. */
    function h(): void;
    /** A lexical global: not a property of globalThis. */
    const c: string;
    /** Declared though no signature reads it. */
    interface PkgUnreached { a: number }
    interface PkgReached { b: number }
    /** Additions to the DOM's Window: a property, a symbol-keyed method and a base. */
    interface Window extends PkgMixin { extra: string; [Symbol.dispose](): void }
    /** A method and a construct signature added to a lib constructor object. */
    interface ArrayConstructor { extraStatic(): void; new (marker: boolean): string[] }
    /** Additions to the two lib types F# writes as one array type. */
    interface Array<T> { last(): T | undefined }
    interface ReadonlyArray<T> { last(): T | undefined }
    /** An addition to generic lib types F# writes as applications. */
    interface Map<K, V> { pkgSize(): number }
    interface Uint8Array<TArrayBuffer extends ArrayBufferLike = ArrayBufferLike> { pkgTag: string }
    /** A call signature added beside the lib's own. */
    interface DateConstructor { (flag: boolean): string }
    /** An added index signature. */
    interface ImportMeta { [key: string]: unknown }
    /** An overload added to a lib method. */
    interface Document { createElement(tagName: string, flag: boolean): HTMLDivElement }
    namespace NodeJS {
        /** An addition to a dependency's namespace member. */
        interface ProcessEnv { X: string }
        /** A type this package adds to a dependency's namespace. */
        interface PkgAdded { y: number }
    }
    namespace Intl {
        /** An addition to a lib namespace member. */
        interface NumberFormatOptions { extraFmt?: string }
    }
}

export declare function getWindow(): Window;
export declare function env(): NodeJS.ProcessEnv;
export declare function added(): NodeJS.PkgAdded;
export declare function reached(): PkgReached;
export declare function numbers(): number[];
```

`tests/fixtures/global-augmentation-lab/index.js`:

```js
// Runtime of global-augmentation-lab: the globals its `declare global` block declares, and the
// objects its functions hand back.
globalThis.g = 1;
globalThis.h = () => { globalThis.hCalled = true; };
const window = { extra: "extra0", mixed: true };
export const getWindow = () => window;
export const env = () => ({ X: "x0" });
export const added = () => ({ y: 1 });
export const reached = () => ({ b: 2 });
export const numbers = () => Object.assign([1, 2, 3], { last() { return this[this.length - 1]; } });
```

If the `reference types` directive does not resolve, `env` renders `obj` with a TR finding. In
that case replace the directive with `import "augmentation-owner-lab";` and say which form resolved
in the lab's description. The frozen CLI resolved the directive (Measures, `lab/`).

- [ ] **Step 2: Write the failing lab tests**

Append to `Pipeline.test.fs`:

```fsharp
[<Tests>]
let globalAugmentationTests =
    let labConfig = handConfig (handFixture "global-augmentation-lab")

    testList "global augmentation fixture" [
        // The dependency half, generated on its own as its own entry package: the compile gate
        // compiles it beside the lab's golden, which templates names into it.
        yield!
            fixtureTests
                "augmentation-owner-lab"
                (handFixture "global-augmentation-lab/node_modules/augmentation-owner-lab")
                GeneratorConfig.Default
                (fun _ -> [])

        yield!
            fixtureTests "global-augmentation-lab" (handFixture "global-augmentation-lab") labConfig (fun package ->
                let generate () = Async.RunSynchronously(Pipeline.generate labConfig package)
                let sourceOf (rendered: RenderModel) = rendered.Files |> List.head |> snd

                [ testCase "a module package's global values are bound under Globals" <| fun _ ->
                      let source = sourceOf (generate ())
                      let globals = moduleBodyOf source "Globals"

                      Expect.stringContains globals "[<Global(\"globalThis\")>]" "the container binds through globalThis"
                      Expect.stringContains globals "static member g\n" "the writable global is a bare settable static"
                      Expect.stringContains globals "[<Global(\"h\")>]" "the function keeps its own binding"
                      Expect.stringContains globals "[<Global(\"c\")>]" "the lexical global keeps its own binding"

                  testCase "its global types are declared whether or not a signature reads them" <| fun _ ->
                      let rendered = generate ()
                      let source = sourceOf rendered

                      Expect.stringContains source "type PkgUnreached =" "an unreached global interface"
                      Expect.stringContains source "type PkgReached =" "a reached one"
                      Expect.isEmpty (rendered.Findings |> List.filter (fun f -> f.Key = "AC001" && f.Symbol.StartsWith "Pkg")) "with nothing reported missing"

                  testCase "a type it adds to a dependency's namespace is declared in that namespace's module" <| fun _ ->
                      let source = sourceOf (generate ())
                      let nodeJs = moduleBodyOf source "NodeJS"

                      Expect.stringContains nodeJs "type PkgAdded =" "NodeJS.PkgAdded is the package's own"
                      Expect.isFalse (nodeJs.Contains "type ProcessEnv =") "the owner's member is not copied"
                      Expect.stringContains source "static member added () : NodeJS.PkgAdded" "its reader writes the nested name"

                  testCase "module locals and exports are not globals" <| fun _ ->
                      let globals = moduleBodyOf (sourceOf (generate ())) "Globals"

                      Expect.isFalse (globals.Contains "PkgMixin") "a module-local interface is not in global scope"
                      Expect.isFalse (globals.Contains "getWindow") "an export is bound once, by its import" ])
    ]
```

In plan 2's `foreign-namespace-lab` block, replace:
- `Expect.isFalse (source.Contains "module NodeJS =") "the dependency's namespace is not re-homed"`
  with `Expect.isFalse ((moduleBodyOf source "NodeJS").Contains "type ProcessEnv =") "the dependency's namespace member is not re-homed"`;
- `Expect.isFalse (source.Contains "type PkgAdded =") "the addition is an extension of the owner, not a new namespace"`
  with `Expect.stringContains (moduleBodyOf source "NodeJS") "type PkgAdded =" "a type this package adds to the namespace is its own, in the namespace's module"`.

Keep `Expect.isFalse (source.Contains "type ProcessEnv =") …` as it is.

- [ ] **Step 3: Run to verify they fail**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "global augmentation"`.
Expected FAIL: there is no `module Globals`, `PkgUnreached` is absent, `PkgAdded` is declared flat,
and the `foreign-namespace-lab` re-pins fail too
(`dotnet fsi build.fsx -- test --quick --filter "generator e2e"`).

- [ ] **Step 4: The model**

`Model.fs`, directly after `HarvestedExport`:

```fsharp
/// A type or function another group declares first and the entry package declares again
/// (`declare global { interface Window { extra: string } }`). The owner keeps its group; what the
/// package adds is written as an extension of the owner's binding (`shape-augmentations`).
type HarvestedAugmentation =
    {
        /// The merged symbol.
        Owner: SymbolResponse
        /// The owner's TypeScript spelling, enclosing namespaces included: `Window`,
        /// `NodeJS.ProcessEnv`, `Intl.NumberFormatOptions`.
        Path: string
        /// The source order of the package's first declaration of the owner.
        Order: DeclOrder option
    }
```

In `HarvestModel`, after `Namespaces`:

```fsharp
        /// The entry package's additions to types and functions another group declares first, by
        /// owner path. An owner whose group ships in this run is absent: the run declares it whole.
        Augmentations: HarvestedAugmentation list
```

`HarvestModel.Empty`: `Augmentations = []`. Amend the `Namespaces` doc to read: "The namespaces
the entry package declares or adds to, by symbol id, …" (keep plan 2's dotted-path sentence).

- [ ] **Step 5: Every `HarvestModel` literal**

Write `tests/.scratch/global-augmentation/augmentations-field.fsx`:

```fsharp
// Adds `Augmentations = []` beside every `ShadowedByLib = 0` of a HarvestModel record literal in
// the test project, asserting each file's count. Render.test.fs's literal is a RenderModel and is
// left alone.
open System.IO
open System.Text.RegularExpressions

let root = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "Xantham.Generator.Tests")

for name in [ "Shape.test.fs"; "CatalogLibraryEntry.test.fs"; "CatalogMember.test.fs" ] do
    let path = Path.Combine(root, name)
    let text = File.ReadAllText path
    let found = Regex.Matches(text, @"ShadowedByLib = 0(?!; Augmentations)").Count
    let updated = Regex.Replace(text, @"ShadowedByLib = 0(?!; Augmentations)", "ShadowedByLib = 0; Augmentations = []")
    if found = 0 then failwith $"{name}: no literal found"
    File.WriteAllText(path, updated)
    printfn "%s: %d" name found
```

Run `dotnet fsi tests/.scratch/global-augmentation/augmentations-field.fsx`. At `91742f98` the
counts are Shape.test.fs 27, CatalogLibraryEntry.test.fs 1, CatalogMember.test.fs 1. Plans 1–2 may
add literals; the script's output must equal `rtk grep -c "ShadowedByLib = 0" <file>` before the
run. `Harvest.fs:478` and `:502` set `ShadowedByLib` with `{ model with … }` and need nothing.

- [ ] **Step 6: Read global scope in every run without a global input**

In `harvestGlobals`, the library-file lookup (`Harvest.fs:338`) becomes
`if Option.isNone globalFile then`. Replace its comment with:

```fsharp
                            // Module locals shadow library names and are not globals, and a
                            // module's exports are not global values. A library file exposes the
                            // program's actual global scope: a module package's `declare global`
                            // block is read there.
```

Keep the early return's condition (`:364-369`) as it is. It now fires only when no default library
exists. Put this comment above it:

```fsharp
                    // A module run with no default library has no file whose scope is global
                    // scope alone, so its `declare global` declarations stay unread.
```

Add the program with no default library to "Reported, not in this plan" (already listed below).

Replace the pass doc comment (`:313-315`) with:

```fsharp
/// Harvests global scope: a global script's or a global library's declarations, a module
/// package's `declare global` declarations, and the compiler lib's when it ships. Values bind
/// with `[<Global>]`. A symbol another group declares first and this package declares again is
/// recorded in `Augmentations` instead.
```

- [ ] **Step 7: The inventory and the foreign namespace walk**

In `harvestGlobals`, after `namespaces` (`:416-419`), insert:

```fsharp
                        let isNamespace (symbol: SymbolResponse) =
                            hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name

                        let memberKinds =
                            SymbolFlags.Interface
                            ||| SymbolFlags.TypeAlias
                            ||| SymbolFlags.Class
                            ||| SymbolFlags.Enum

                        // Declared first by another group, declared again here, and that group
                        // does not ship in this run: a run that ships it declares the merged owner
                        // whole in the owner's own module.
                        let augments (symbol: SymbolResponse) =
                            let group = Grouping.classify ctx.PackageDir (ValueSome symbol)

                            not (admits group)
                            && GeneratorConfig.disposition ctx.Config group <> Ship
                            && Grouping.declaresUnderPackage ctx.PackageDir symbol

                        let underPackage (handle: string) =
                            match NodeHandle.parse handle with
                            | ValueSome parsed ->
                                Grouping.classifyFile ctx.PackageDir (parsed.Path * uom<filePath>) = EntryPackage
                            | ValueNone -> false

                        let augmentation (path: string) (symbol: SymbolResponse) =
                            {
                                Owner = symbol
                                Path = path
                                Order = symbol.Declarations |> ValueOption.map (Array.filter underPackage) |> Grouping.declOrder
                            }

                        let ownMember (name: string) (symbol: SymbolResponse) =
                            {
                                ExportName = name
                                Symbol = symbol
                                HasValueExport = false
                                Docs = ""
                                Tags = []
                                Origin = FromGlobal
                                Order = Grouping.declOrder symbol.Declarations
                            }

                        // A top-level owner: an interface or class (its additions are extensions),
                        // or a function (its added overloads, reported by audit-coverage).
                        let topLevel =
                            symbols
                            |> Array.filter (fun symbol ->
                                augments symbol
                                && hasAny (SymbolFlags.Interface ||| SymbolFlags.Class ||| SymbolFlags.Function) symbol.Flags)
                            |> Array.map (fun symbol -> augmentation symbol.Name symbol)
                            |> Array.toList

                        // A namespace another group declares first. A member this package declares
                        // first is the package's own, in the namespace's module path; a member the
                        // owner declares first is an augmentation like any other.
                        let rec walkForeign (path: string) (ns: SymbolResponse) =
                            async {
                                let! exports = ctx.Session.getExportsOfModule ns.Id

                                let! members =
                                    exports |> ValueOption.defaultValue [||] |> Array.map (followAlias ctx) |> Async.Parallel

                                let rec visit pending (own, added, named) =
                                    async {
                                        match pending with
                                        | [] -> return own, added, named
                                        | (name: string, symbol: SymbolResponse) :: rest ->
                                            let qualified = $"{path}.{name}"

                                            if Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage then
                                                if hasAny memberKinds symbol.Flags then
                                                    return! visit rest (own @ [ ownMember name symbol ], added, named)
                                                elif hasAny SymbolFlags.Value symbol.Flags then
                                                    return! visit rest (own, added @ [ augmentation qualified symbol ], named)
                                                else
                                                    return! visit rest (own, added, named)
                                            elif not (augments symbol) then
                                                return! visit rest (own, added, named)
                                            elif isNamespace symbol then
                                                let! innerOwn, innerAdded, innerNamed = walkForeign qualified symbol

                                                let added =
                                                    if hasAny (SymbolFlags.Interface ||| SymbolFlags.Class) symbol.Flags then
                                                        added @ [ augmentation qualified symbol ]
                                                    else
                                                        added

                                                return! visit rest (own @ innerOwn, added @ innerAdded, named @ innerNamed)
                                            elif hasAny (SymbolFlags.Interface ||| SymbolFlags.Class ||| SymbolFlags.Function) symbol.Flags then
                                                return! visit rest (own, added @ [ augmentation qualified symbol ], named)
                                            else
                                                return! visit rest (own, added, named)
                                    }

                                return!
                                    visit
                                        (members |> Array.sortBy fst |> Array.toList)
                                        ([], [], [ ns.SymbolId, path * uom<symbolName> ])
                            }

                        let! walked =
                            declared
                            |> Array.filter (fun symbol ->
                                isNamespace symbol && ValueOption.isNone symbol.Parent && augments symbol)
                            |> Array.sortBy _.Name
                            |> Array.map (fun ns -> walkForeign ns.Name ns)
                            |> Async.Sequential

                        let foreignOwn = walked |> Array.toList |> List.collect (fun (own, _, _) -> own)

                        let augmentations =
                            topLevel @ (walked |> Array.toList |> List.collect (fun (_, added, _) -> added))
                            |> List.distinctBy _.Owner.SymbolId
                            |> List.sortBy _.Path

                        let foreignNamespaces =
                            walked |> Array.toList |> List.collect (fun (_, _, named) -> named) |> Map.ofList
```

A foreign symbol that is both an interface and a namespace (`interface Intl` does not exist, but
an interface merged with a namespace may) is recorded once by `List.distinctBy`.

Wire the results in:
- In `harvested` (`:453-467`), after `for exports, _, _ in fromModules do yield! exports`, add
  `yield! foreignOwn`.
- The `NothingHarvested` condition (`:472`) gains `&& List.isEmpty augmentations`.
- In both returned models (`:476-479` and `:494-503`), set `Augmentations = augmentations`.
- In the second, `Namespaces` also takes the walked foreign namespaces, existing entries winning:

```fsharp
                                    Namespaces =
                                        let package =
                                            namespaces
                                            |> Map.fold (fun names id name -> Map.add id name names) model.Namespaces

                                        foreignNamespaces
                                        |> Map.fold
                                            (fun names id name ->
                                                if Map.containsKey id names then names else Map.add id name names)
                                            package
```

- [ ] **Step 8: Run**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "global augmentation"` → PASS.
Read `git diff -- tests/Xantham.Generator.Tests/golden/global-augmentation-lab` in full:
- `module Globals` holds `g`, `h`, `c`;
- `PkgUnreached` and `PkgReached` are declared at the root;
- `module NodeJS` holds `PkgAdded`;
- the copies (`Window`, `Document`, …) are still there: Task 6 removes them.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`. Expected golden moves:
- `solid-js`: `Solid$$` under `Globals` (`rtk grep -n "Solid" tests/Xantham.Generator.Tests/golden/solid-js/*.fs | head`);
- `foreign-namespace-lab`: `NodeJS.PkgAdded`;
- `mixed-subpaths-lab`: nothing.

Any other moved golden is reported with the moved declarations before committing.

- [ ] **Step 9: Documentation**

`generator-architecture.md:409-411`, replace "It runs only when `harvest-exports` found nothing - a
package that has a module may *also* augment global scope, and folding those in would emit names
it does not export." with:

```markdown
    It also reads a module package's global scope, at a default-library file, where module
    locals do not shadow it (2026-10-15): a `declare global` block is API the package contributes,
    so its own globals are harvested as global exports and its additions to other groups' types
    are recorded as augmentations (`HarvestModel.Augmentations`).
```

In `footguns.md`, "Compiler-library shipping must not depend on module exports": replace "For
module-only inputs, read global scope through a verified default-library source" with "For every
input with no global script, read global scope through a verified default-library source".

- [ ] **Step 10: Gate and commit**

Run: `dotnet build Xantham.slnx`, then `dotnet fsi build.fsx -- test` → PASS.

```
rtk git add src/Xantham.Generator/Model.fs src/Xantham.Generator/Harvest.fs tests/fixtures/global-augmentation-lab tests/Xantham.Generator.Tests docs/.ai/plans/generator-architecture.md docs/.ai/footguns.md
rtk git status
rtk git commit -m "fix(generator): harvest a module package's declare global declarations" -m "harvest-globals reads global scope at a default-library file in every run with no global input, so a module package's own globals are bound under Globals and its global types are declared. Additions to types another group declares are recorded in HarvestModel.Augmentations. <golden moves from Step 8>" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task 6: The owner keeps its identity; resolve what the package adds

Ends with the lab's extension tests **red** and the tree uncommitted (Stages): the members the
copies carried now reach no declaration until Task 7.

**Files:**
- Modify: `src/Xantham.Generator/Model.fs` (`ResolvedAugmentation` before `ResolveModel`, `:1380`; `ResolveModel`; `ShapeModel`)
- Modify: `src/Xantham.Generator/Resolve.fs` (`:1466-1481`; `resolveSignatures`, `:1074`; new `packageHeritage` and `resolveAugmentations` before `resolveDeclarationIdentities`, `:2545`; `passes`, `:2597-2598`)
- Modify: `src/Xantham.Generator/Pipeline.fs` (`toResolve`, `:80-85`; `toShape`, `:87-104`)
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (`augmentedArity`; `libBinding`; `referencedRef`)
- Modify: `tests/Xantham.Generator.Tests/Build.fs:99-119`
- Test: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`globalAugmentationTests`)

**Interfaces:**
- Consumes: `HarvestModel.Augmentations` (Task 5); `TypeFacts.NamespacePath` (Task 4); spike rows (a)–(j).
- Produces:
  - `type ResolvedAugmentation = { Augmentation: HarvestedAugmentation; OwnerTypeId: int<typeId>; OwnerArity: int; Members: ResolvedMember list; Overloads: (ResolvedMember * ResolvedSignature list) list; CallSignatures: ResolvedSignature list; ConstructSignatures: ResolvedSignature list; IndexInfos: ResolvedIndex list; Bases: int<typeId> list }`
  - `ResolveModel.AugmentationFacts: ResolvedAugmentation list`
  - `ShapeModel.AugmentationFacts: ResolvedAugmentation list`
  - `Resolve.resolveAugmentations: Pass<ResolveModel>`, `Name = "resolve-augmentations"`
  - `Spec.augmentedArity: ShapeModel -> TypeFacts -> int -> int`

- [ ] **Step 1: Write the failing extension tests**

Add to `globalAugmentationTests`' lab case list (move the closing `])` onto the last case):

```fsharp
                  testCase "an addition to a lib type extends its Core.TS binding" <| fun _ ->
                      let source = sourceOf (generate ())

                      Expect.isFalse (source.Contains "\ntype Window =") "the owner is not copied"
                      Expect.isFalse (source.Contains "module Window =") "nor its members' shapes"
                      Expect.stringContains source "type Fable.Core.TS.Dom.Window with" "the binding is extended"
                      Expect.stringContains source "[<Emit(\"$0.extra\")>] get (): string = jsNative" "a property reads per accessor"
                      Expect.stringContains source "[<Emit(\"$0.extra = $1\")>] set (value: string): unit = jsNative" "and writes per accessor"
                      Expect.stringContains source "static member getWindow () : Fable.Core.TS.Dom.Window" "the signature reads the owner"

                  testCase "every addition form is an emitted extension member" <| fun _ ->
                      let source = sourceOf (generate ())

                      for expected in
                          [ "type 'T ``[]`` with"
                            "[<Emit(\"$0.last()\")>]"
                            "type Fable.Core.TS.Es.Map<'K, 'V> with"
                            "[<Emit(\"$0.pkgSize()\")>]"
                            "type Fable.Core.TS.Es.Uint8Array<'TArrayBuffer> with"
                            "type Fable.Core.TS.Es.DateConstructor with"
                            "[<Emit(\"$0($1...)\")>]"
                            "member _.Invoke"
                            "type Fable.Core.TS.Es.ArrayConstructor with"
                            "[<Emit(\"new $0($1...)\")>]"
                            "member _.Create"
                            "[<Emit(\"$0.extraStatic()\")>]"
                            "type Fable.Core.TS.Es.ImportMeta with"
                            "[<Emit(\"$0[$1]\")>] get (key: string): obj = jsNative"
                            "[<Emit(\"$0[Symbol.dispose]()\")>]"
                            "member _.SymbolDispose"
                            "type Fable.Core.TS.Dom.Document with"
                            "[<Emit(\"$0.createElement($1, $2)\")>]" ] do
                          Expect.stringContains source expected expected

                  testCase "owners sharing an F# type share one extension" <| fun _ ->
                      let source = sourceOf (generate ())
                      let count (text: string) = source.Split(text).Length - 1

                      Expect.equal (count "type 'T ``[]`` with") 1 "Array and ReadonlyArray are both 'T[]"
                      Expect.equal (count "member _.last") 1 "and their identical addition is one member"

                  testCase "an addition to a namespace member extends the owner's nested name" <| fun _ ->
                      let source = sourceOf (generate ())

                      Expect.stringContains source "type AugmentationOwnerLab.NodeJS.ProcessEnv with" "a referenced group's member"
                      Expect.stringContains source "[<Emit(\"$0.X\")>] get (): string = jsNative" "reads its addition"
                      Expect.stringContains source "static member env () : AugmentationOwnerLab.NodeJS.ProcessEnv" "and its reader writes the nested name"
                      Expect.stringContains source "type Fable.Core.TS.Es.Intl.NumberFormatOptions with" "a lib namespace member"
                      Expect.stringContains source "[<Emit(\"$0.extraFmt\")>] get (): string option = jsNative" "reads its optional addition"

                  testCase "an added base is reached through an upcast" <| fun _ ->
                      let rendered = generate ()
                      let source = sourceOf rendered

                      Expect.stringContains source "[<Emit(\"$0\")>]" "the upcast emits the receiver"
                      Expect.stringContains source "member _.AsPkgMixin: PkgMixin = jsNative" "to the base the package names"
                      Expect.stringContains source "[<Emit(\"$0.mixed\")>] get (): bool = jsNative" "the base's own member is an extension too"
                      Expect.contains (rendered.Findings |> List.map (fun f -> f.Key, f.Symbol)) ("SI010", "Window") "the missing inherit is reported"

                  testCase "each extension member is reported, and nothing is missing" <| fun _ ->
                      let rendered = generate ()
                      let extensions = rendered.Findings |> List.filter (fun f -> f.Key = "SI009") |> List.map _.Symbol |> Set.ofList

                      for symbol in [ "Window.extra"; "Window.mixed"; "Array.last"; "Map.pkgSize"; "NodeJS.ProcessEnv.X"; "Intl.NumberFormatOptions.extraFmt"; "DateConstructor.Invoke"; "ArrayConstructor.Create"; "ImportMeta.Item"; "Document.createElement" ] do
                          Expect.isTrue (Set.contains symbol extensions) $"{symbol} is reported as an extension"

                      Expect.isEmpty (rendered.Findings |> List.filter (fun f -> f.Key = "AC001")) "every addition is represented"

                  testCase "the extensions open with the package" <| fun _ ->
                      Expect.stringContains (sourceOf (generate ())) "[<AutoOpen>]\nmodule Augmentations =" "one AutoOpen module"
```

If spike (a) chose spelling B, the first and the count assertions read `` "type ``[]``<'T> with" ``.
The same applies to (c) and (d): the asserted Emit strings follow the passing form.

- [ ] **Step 2: Run to verify they fail**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "global augmentation"`.
Expected: every new case fails, because the copies are still declared. Task 5's cases pass.

- [ ] **Step 3: The resolved model**

`Model.fs`, directly before `ResolveModel`:

```fsharp
/// What the entry package adds to a type another group declares (`HarvestedAugmentation`), read
/// off the merged owner.
type ResolvedAugmentation =
    {
        Augmentation: HarvestedAugmentation
        /// The owner's declared type, which reaches the table as identity like any type of its group.
        OwnerTypeId: int<typeId>
        /// The count of type parameters the owner's own declarations bind; the owner's parameters
        /// past it are bound only by the package's declarations.
        OwnerArity: int
        /// Properties and methods whose first declaration is the package's, inherited ones included.
        Members: ResolvedMember list
        /// Methods the owner declares first, each with the overloads only the package's
        /// declarations write.
        Overloads: (ResolvedMember * ResolvedSignature list) list
        CallSignatures: ResolvedSignature list
        ConstructSignatures: ResolvedSignature list
        IndexInfos: ResolvedIndex list
        /// The bases the package's declarations name in `extends`.
        Bases: int<typeId> list
    }
```

`ResolveModel` and `ShapeModel` each gain, after `NotFollowed`:

```fsharp
        /// The entry package's additions to types other groups declare, by owner path.
        AugmentationFacts: ResolvedAugmentation list
```

Set `AugmentationFacts = []` in `Pipeline.toResolve` (`:80-85`) and `Build.shapeModel`
(`Build.fs:99-119`), and `AugmentationFacts = resolve.AugmentationFacts` in `Pipeline.toShape`.
The build lists any other full literal as FS0764; give each `[]`.

- [ ] **Step 4: Remove the re-homing**

In `Resolve.fs`, the block from `let! symbol = ctx.Session.getSymbolOfType ty.Id` (`:1465`) through
`|> Option.defaultValue firstOrigin` (`:1481`), Task 4's `namespacePath` line included, becomes:

```fsharp
            let! symbol = ctx.Session.getSymbolOfType ty.Id
            // A merged type keeps the group of its first declaration: a package's additions to a
            // standard-library type are extensions of that type's binding (`resolve-augmentations`).
            let origin = Grouping.classify ctx.PackageDir symbol
            let! namespacePath = namespacePathOf ctx symbol
```

Then rename the remaining uses of `firstOrigin` in the branch to `origin`
(`rtk grep -n "firstOrigin" src/Xantham.Generator/Resolve.fs` lists them).

- [ ] **Step 5: Signatures filtered by declaration**

Rename `resolveSignatures` (`:1074`) to `resolveSignaturesWhere`, with a first parameter
`(keep: SignatureResponse -> bool)`. Filter right after `getSignaturesOfType`:

```fsharp
        let! signatures = ctx.Session.getSignaturesOfType (ty.Id, kind)
        let signatures = signatures |> Array.filter keep
```

Below it, keep every existing caller compiling:

```fsharp
let private resolveSignatures (ctx: Context) (trace: Trace option) (registers: Registers) (ty: TypeResponse) kind =
    resolveSignaturesWhere (fun _ -> true) ctx trace registers ty kind
```

- [ ] **Step 6: Heritage and arity from the package's declarations**

Directly before `resolveDeclarationIdentities` (`:2545`):

```fsharp
/// The `extends` types a package's declarations of `owner` write, and the count of type
/// parameters the owner's first other declaration binds (`None` where no such declaration is an
/// interface or class).
let private packageHeritage (ctx: Context) (owner: SymbolResponse) =
    async {
        let handles =
            owner.Declarations
            |> ValueOption.defaultValue [||]
            |> Array.choose (NodeHandle.parse >> ValueOption.toOption)

        let ours (handle: NodeHandle) =
            Grouping.classifyFile ctx.PackageDir (handle.Path * uom<filePath>) = EntryPackage

        let! nodes =
            handles
            |> Array.map (fun handle ->
                async {
                    let! source = sourceFile ctx handle.Path
                    return handle, source |> ValueOption.map (fun file -> Node.ofIndex<AnyNode> file handle.Index)
                })
            |> Async.Sequential

        let parametersOf (node: Node<AnyNode>) =
            match node.Kind with
            | SyntaxKind.InterfaceDeclaration ->
                Some(Node.retag<AnyNode, InterfaceDeclaration> node |> InterfaceDeclaration.typeParameters |> Seq.length)
            | SyntaxKind.ClassDeclaration ->
                Some(Node.retag<AnyNode, ClassDeclaration> node |> ClassDeclaration.typeParameters |> Seq.length)
            | _ -> None

        let extendsOf (node: Node<AnyNode>) =
            let clauses =
                match node.Kind with
                | SyntaxKind.InterfaceDeclaration -> Node.retag<AnyNode, InterfaceDeclaration> node |> InterfaceDeclaration.heritageClauses
                | SyntaxKind.ClassDeclaration -> Node.retag<AnyNode, ClassDeclaration> node |> ClassDeclaration.heritageClauses
                | _ -> Seq.empty

            clauses
            |> Seq.filter (fun clause -> HeritageClause.token clause = ValueSome SyntaxKind.ExtendsKeyword)
            |> Seq.collect HeritageClause.types

        let arity =
            nodes
            |> Array.tryPick (fun (handle, node) ->
                if ours handle then None else node |> ValueOption.toOption |> Option.bind parametersOf)

        let! bases =
            nodes
            |> Array.filter (fst >> ours)
            |> Array.collect (fun (handle, node) ->
                match node with
                | ValueSome node ->
                    extendsOf node
                    |> Seq.map (fun reference ->
                        ctx.Session.getTypeFromTypeNode (
                            NodeHandle.format
                                {
                                    Index = Node.index reference
                                    Kind = reference.Kind
                                    Path = handle.Path
                                }
                        ))
                    |> Seq.toArray
                | ValueNone -> [||])
            |> Async.Parallel

        return arity, Array.toList bases
    }
```

If `HeritageClause.types` yields `Node<ExpressionWithTypeArguments>`, the `NodeHandle.format`
record still takes `Node.index` and `.Kind` of it, as `resolveImplementedTypes` does
(`Resolve.fs:675-685`). Copy its exact spelling.

- [ ] **Step 7: `resolve-augmentations`**

Directly after `packageHeritage`:

```fsharp
/// What the entry package adds to each type another group declares, read off the merged owner:
/// members the package declares first, overloads and signatures its declarations write, bases
/// its `extends` clauses name, and the owner's own arity. Every type these read enters the
/// table for `resolve-type-table` to derive.
let resolveAugmentations: Pass<ResolveModel> =
    {
        Name = "resolve-augmentations"
        Run =
            fun ctx model ->
                async {
                    let registers = Registers.start ()

                    let underPackage (handle: string voption) =
                        handle
                        |> ValueOption.bind NodeHandle.parse
                        |> ValueOption.exists (fun parsed ->
                            Grouping.classifyFile ctx.PackageDir (parsed.Path * uom<filePath>) = EntryPackage)

                    let firstUnderPackage (symbol: SymbolResponse) =
                        Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage

                    let keep (signature: SignatureResponse) = underPackage signature.Declaration

                    let! resolved =
                        model.Harvest.Augmentations
                        |> List.filter (fun augmentation ->
                            hasAny (SymbolFlags.Interface ||| SymbolFlags.Class) augmentation.Owner.Flags)
                        |> List.map (fun augmentation ->
                            async {
                                let! owner = ctx.Session.getDeclaredTypeOfSymbol augmentation.Owner.Id
                                let! properties = ctx.Session.getPropertiesOfType owner.Id
                                let properties = properties |> ValueOption.defaultValue [||]

                                let! members =
                                    properties
                                    |> Array.filter firstUnderPackage
                                    |> Array.map (resolveMember ctx true)
                                    |> Async.Parallel

                                let! overloads =
                                    properties
                                    |> Array.filter (fun property ->
                                        property.Flags.HasFlag SymbolFlags.Method
                                        && not (firstUnderPackage property)
                                        && Grouping.declaresUnderPackage ctx.PackageDir property)
                                    |> Array.map (fun property ->
                                        async {
                                            let! resolvedMember, memberType = resolveMember ctx true property

                                            let! signatures =
                                                resolveSignaturesWhere keep ctx None registers memberType SignatureKind.Call

                                            return
                                                (resolvedMember, signatures |> Array.map fst |> Array.toList),
                                                memberType :: (signatures |> Array.toList |> List.collect snd)
                                        })
                                    |> Async.Parallel

                                let! calls = resolveSignaturesWhere keep ctx None registers owner SignatureKind.Call
                                let! constructs = resolveSignaturesWhere keep ctx None registers owner SignatureKind.Construct
                                let! indexInfos = ctx.Session.getIndexInfosOfType owner.Id

                                let indexInfos =
                                    indexInfos
                                    |> ValueOption.defaultValue [||]
                                    |> Array.filter (fun info -> underPackage info.Declaration)
                                    |> Array.toList

                                let! arity, bases = packageHeritage ctx augmentation.Owner

                                let fact =
                                    {
                                        Augmentation = augmentation
                                        OwnerTypeId = owner.TypeId
                                        OwnerArity =
                                            arity
                                            |> Option.defaultValue (owner.ParameterTypeIds |> ValueOption.map Array.length |> ValueOption.defaultValue 0)
                                        Members = members |> Array.map fst |> Array.toList
                                        Overloads = overloads |> Array.map fst |> Array.toList
                                        CallSignatures = calls |> Array.map fst |> Array.toList
                                        ConstructSignatures = constructs |> Array.map fst |> Array.toList
                                        IndexInfos =
                                            indexInfos
                                            |> List.map (fun info ->
                                                {
                                                    KeyTypeId = info.KeyType.TypeId
                                                    ValueTypeId = info.ValueType.TypeId
                                                    IsReadonly = info.IsReadonly = ValueSome true
                                                })
                                        Bases = bases |> List.map _.TypeId
                                    }

                                let discovered =
                                    [
                                        owner
                                        yield! members |> Array.map snd
                                        yield! overloads |> Array.collect (snd >> List.toArray)
                                        yield! calls |> Array.collect (snd >> List.toArray)
                                        yield! constructs |> Array.collect (snd >> List.toArray)
                                        for info in indexInfos do
                                            info.KeyType
                                            info.ValueType
                                        yield! bases
                                    ]

                                return fact, discovered
                            })
                        |> Async.Sequential

                    let types =
                        resolved
                        |> Array.collect (snd >> List.toArray)
                        |> Array.fold
                            (fun map ty ->
                                if Map.containsKey ty.TypeId map then
                                    map
                                else
                                    Map.add ty.TypeId (TypeFacts.shallow ty) map)
                            model.Types

                    return
                        Advanced
                            { model with
                                Types = types
                                AugmentationFacts = resolved |> Array.map fst |> Array.toList
                            }
                }
    }
```

`Async.Sequential`, like `resolve-export-types` (`:262-268`): asking for a declared type creates
it, and source order keeps generation deterministic.

`passes` (`:2597-2598`) becomes
`[ resolveExportTypes; resolveAugmentations; resolveTypeTable; resolveDeclarationIdentities ]`.

- [ ] **Step 8: References apply the owner's own arity**

`Spec.fs`, beside `namespaceQualified` (Task 4):

```fsharp
/// The arity a reference to `facts` applies: `arity`, capped at the owner's own where a package's
/// declarations bind further parameters (`interface Request<T = unknown>` over DOM's `Request`).
let internal augmentedArity (model: ShapeModel) (facts: TypeFacts) (arity: int) =
    let target = facts.Response.TargetTypeId |> ValueOption.defaultValue facts.Response.TypeId

    model.AugmentationFacts
    |> List.tryFind (fun fact -> fact.OwnerTypeId = target)
    |> Option.map (fun fact -> min arity fact.OwnerArity)
    |> Option.defaultValue arity
```

In `libBinding`, the Dom and Es constructions take `augmentedArity model facts arguments.Length`
in place of `arguments.Length`. Their existing `dropped` finding
(`LibExtraTypeArgumentsDropped`) then reports each reference that applies an added parameter.

In `referencedRef`, compute `let arity = augmentedArity model facts facts.TypeArguments.Length`
and match on `facts.TypeArguments |> List.truncate arity`. When
`facts.TypeArguments.Length > arity`, append
`Finding.make owner (TypeReference.LibExtraTypeArgumentsDropped(typeName, facts.TypeArguments.Length, name, arity))`
(the payload order `libBinding` uses, `:2243`).

- [ ] **Step 9: Run**

Run: `dotnet build src/Xantham.Generator`. Expected: it compiles. Then
`dotnet fsi build.fsx -- test --quick --update --filter "global augmentation"`.
Expected:
- Task 5's cases PASS;
- the owner-not-copied assertions PASS;
- the extension assertions still FAIL.

Read `git diff -- tests/Xantham.Generator.Tests/golden/global-augmentation-lab --stat`. The golden
shrinks from ~17,600 lines to a few hundred.

Do not commit. Hand Task 7 the working tree.

---

### Task 7: Findings, `shape-augmentations`, and the extension rendering

Ends with the lab green and the tree uncommitted. Task 8 commits.

**Files:**
- Modify: `src/Xantham.Generator/Findings.fs` (`ShapeInterfaces`, `:956-993`; table `:203-210`), `tests/Xantham.Generator.Tests/Findings.test.fs:167-174`
- Modify: `src/Xantham.Generator/Model.fs` (`FsAugmentationDecl` before `FsDecl`, `:1810`; `FsDecl.FsAugmentation`)
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs` (after the export loop, `:762-770`)
- Create: `src/Xantham.Generator/Shape/Augmentations.fs`; Modify: `src/Xantham.Generator/Xantham.Generator.fsproj:46`, `src/Xantham.Generator/Shape/Passes.fs`
- Modify: `src/Xantham.Generator/Render.fs` (new functions before `renderBody`, `:1699`; `renderBody`)
- Modify: every `FsDecl` match site (Step 7)
- Test: `tests/Xantham.Generator.Tests/Render.test.fs`, `tests/Xantham.Generator.Tests/Pipeline.test.fs`

**Interfaces:**
- Consumes: `ShapeModel.AugmentationFacts`, `Spec.augmentedArity` (Task 6); `Spec.shapeMembers` (`Spec.fs:3620`), `Spec.shapeSignature` (`:3155`), `Spec.typeParamsOf` (`:2770`), `Spec.declParamIds` (`:341`), `Spec.typeRef`, `Spec.signatureTags` (`:3145`), `Spec.nestUnder`; `Render.literalCallTemplate` (`Render.fs:677`).
- Produces:
  - `type FsAugmentationDecl = { Owners: string list; Target: FsTypeRef; TypeParameters: FsTypeParam list; Members: FsMember list; Bases: (string * FsTypeRef) list; Order: DeclOrder option }`
  - `FsDecl.FsAugmentation of FsAugmentationDecl`
  - `Augmentations.shapeAugmentations: Pass<ShapeModel>`, `Name = "shape-augmentations"`, after `shape-exports`
  - findings `ShapeInterfaces.AugmentationAsExtension of owner: string` (SI009), `AugmentationHeritageNotInherited of baseName: string` (SI010), `AugmentationTypeParameterWidened of parameter: string` (SI011)

- [ ] **Step 1: The findings**

Append to `ShapeInterfaces` (`Findings.fs`, after `BaseWouldCycle`):

```fsharp
    /// A member a package adds to a type another group declares, written as an F# extension member
    /// of the owner's binding and reached through `[<Emit>]`. An SRTP constraint resolves only
    /// intrinsic members (FS0001), an object expression or implementation supplies only abstract
    /// ones, and the owner's `Create` takes only the owner's own.
    | [<Ergonomic>] AugmentationAsExtension of owner: string
    /// A base only a package's declarations of a foreign type add (`interface Array<T> extends
    /// RelativeIndexable<T>`). The owner reaches it through an `As<Base>` extension emitting the
    /// receiver itself.
    | [<Widened>] AugmentationHeritageNotInherited of baseName: string
    /// A type parameter only a package's declarations of a foreign type bind (`interface
    /// Request<T = unknown>`). References apply the owner's own arity, and members reading the
    /// parameter read `obj`.
    | [<Widened>] AugmentationTypeParameterWidened of parameter: string
```

Message arms:

```fsharp
            | AugmentationAsExtension owner ->
                $"added to {owner}, which another group declares; written as an Emit extension member, which SRTP constraints, object expressions and {owner}'s Create leave out"
            | AugmentationHeritageNotInherited baseName ->
                $"base {baseName} is added by this package's declarations alone; F# adds an interface only to a type it declares, so As{baseName} upcasts to it"
            | AugmentationTypeParameterWidened parameter ->
                $"type parameter {parameter} is declared by this package's declarations alone; references apply the owner's own arity and its members read obj"
```

Table rows after `"SI.HybridCallSignaturesAsInvoke", "SI008"`:

```fsharp
            "SI.AugmentationAsExtension", "SI009"
            "SI.AugmentationHeritageNotInherited", "SI010"
            "SI.AugmentationTypeParameterWidened", "SI011"
```

`Findings.test.fs`, after `"SI.BaseWouldCycle SI007 ergonomic"` (`:174`):

```fsharp
                    "SI.AugmentationAsExtension SI009 ergonomic"
                    "SI.AugmentationHeritageNotInherited SI010 widened"
                    "SI.AugmentationTypeParameterWidened SI011 widened"
```

Run: `dotnet fsi build.fsx -- test --quick --filter "findings"` → PASS.

- [ ] **Step 2: The declaration**

`Model.fs`, before `FsDecl`:

```fsharp
/// A package's additions to types another group declares, written as one F# optional type
/// extension of the owners' binding (`type Fable.Core.TS.Dom.Window with`). Every member is
/// reached through `[<Emit>]`: Fable resolves an extension member of a referenced assembly only in
/// an emitted form.
type FsAugmentationDecl =
    {
        /// The owners' TypeScript spellings: one, or more where owners share an F# type
        /// (`Array` and `ReadonlyArray` are both `'T[]`).
        Owners: string list
        /// `FsNamed`, `FsApp` over `TypeParameters`, or `FsArray` of the one parameter.
        Target: FsTypeRef
        TypeParameters: FsTypeParam list
        Members: FsMember list
        /// `As<Base>` upcasts to the bases the package's declarations add, by member name.
        Bases: (string * FsTypeRef) list
        Order: DeclOrder option
    }
```

In `FsDecl`, after `FsExports`:

```fsharp
    /// A package's additions to a type another group declares.
    | FsAugmentation of FsAugmentationDecl
```

- [ ] **Step 3: Name the shapes the additions reach**

`Anonymous.fs`, after the export loop (`:762-770`), before the record is returned:

```fsharp
    // A package's additions to another group's type reach shapes no export does. Each is named
    // under the owner's path, as a member of the owner would be.
    for fact in model.AugmentationFacts do
        modulePrefix <- ""

        let root =
            fact.Augmentation.Path.Split '.' |> Array.map Naming.pascalSegment |> String.concat "."

        let order = fact.Augmentation.Order
        let under (segment: string) = nestUnder root (Naming.pascalSegment segment)

        let signature (segment: string) (s: ResolvedSignature) =
            for parameter in s.Parameters do
                walk (under segment) order parameter.TypeId

            walk (under segment) order s.ReturnTypeId

        for m in fact.Members do
            walk (under (Naming.memberName m.Symbol.Name)) order m.TypeId

        for m, signatures in fact.Overloads do
            for s in signatures do
                signature (Naming.memberName m.Symbol.Name) s

        for s in fact.CallSignatures do
            signature "Invoke" s

        for s in fact.ConstructSignatures do
            signature "Create" s

        for info in fact.IndexInfos do
            walk (under "Item") order info.ValueTypeId

        for baseId in fact.Bases do
            walk (under "Base") order baseId
```

If `modulePrefix` is not a `mutable` at this scope, read how the export loop sets it (`:763`) and
match that.

- [ ] **Step 4: The pass**

`src/Xantham.Generator/Shape/Augmentations.fs`:

```fsharp
/// `shape-augmentations`: a package's additions to types another group declares.
module Xantham.Generator.Shape.Augmentations

open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The name a finding reports an extension member under, after its owner's path.
let private memberLabel (m: FsMember) =
    match m with
    | FsProperty p -> p.Name
    | FsMethod m
    | FsLiteralKeyed(_, _, FsMethod m) -> m.Name
    | FsIndexer _ -> "Item"
    | FsConstructor _ -> "Create"
    | FsInvoke _ -> "Invoke"
    | FsSymbolKeyed(symbol, _) -> $"[Symbol.{symbol}]"
    | FsLiteralKeyed _ -> ""

/// What F# overload resolution distinguishes a member by: two members agreeing here are one.
let rec private signatureKey (m: FsMember) =
    match m with
    | FsProperty p -> [ p.Type ], None
    | FsMethod m
    | FsLiteralKeyed(_, _, FsMethod m) -> m.Parameters |> List.map _.Type, Some m.Return
    | FsIndexer i -> [ i.Key ], Some i.Value
    | FsConstructor c
    | FsInvoke c -> c.Parameters |> List.map _.Type, Some c.Return
    | FsSymbolKeyed(_, inner) -> signatureKey inner
    | FsLiteralKeyed _ -> [], None

/// The last segment of a named F# type, which an `As<leaf>` upcast is named by.
let private leafOf (reference: FsTypeRef) =
    match reference with
    | FsNamed name
    | FsApp(name, _) -> Some(name.Split '.' |> Array.last)
    | _ -> None

/// Each package addition to a type another group declares, as an optional type extension of the
/// owner's F# binding. Owners written as one F# type share one extension, each member once. An
/// owner whose binding widens has no extension, and `audit-coverage` reports it.
let shapeAugmentations: Pass<ShapeModel> =
    {
        Name = "shape-augmentations"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []
                    let emit finding = findings <- findings @ [ finding ]

                    let shaped =
                        model.AugmentationFacts
                        |> List.sortBy _.Augmentation.Path
                        |> List.choose (fun fact ->
                            let path = fact.Augmentation.Path

                            match Map.tryFind fact.OwnerTypeId model.Types with
                            | None -> None
                            | Some owner ->
                                let parameterIds = declParamIds owner
                                let arity = min fact.OwnerArity parameterIds.Length

                                let typeParameters, scope, parameterFindings =
                                    typeParamsOf ctx model path (List.truncate arity parameterIds)

                                let scoped = { model with TypeVars = scope }

                                // The head is the owner's own name. Each reference to the owner
                                // elsewhere reports its own findings.
                                match typeRef ctx scoped None path fact.OwnerTypeId with
                                | ((FsNamed _ | FsApp _ | FsArray(FsTypeVar _)) as target), _ ->
                                    findings <- findings @ parameterFindings

                                    for added in List.skip arity parameterIds do
                                        let name =
                                            Map.tryFind added model.Types
                                            |> Option.bind _.SymbolName
                                            |> Option.map (fun value -> value / uom<symbolName>)
                                            |> Option.defaultValue "T"

                                        emit (Finding.make path (ShapeInterfaces.AugmentationTypeParameterWidened name))

                                    let additions =
                                        { owner with
                                            Members = fact.Members
                                            IndexInfos = fact.IndexInfos
                                            CallSignatures = []
                                            ConstructSignatures = fact.ConstructSignatures
                                            BaseTypes = []
                                            IntersectionMembers = []
                                        }

                                    let members, memberFindings = shapeMembers ctx scoped Set.empty path additions
                                    findings <- findings @ memberFindings

                                    let shapeOne (site: string) (signature: ResolvedSignature) =
                                        let typeParameters, parameters, returns, signatureFindings =
                                            shapeSignature ctx scoped None site signature

                                        findings <- findings @ signatureFindings
                                        typeParameters, parameters, returns

                                    let overloads =
                                        fact.Overloads
                                        |> List.collect (fun (m, signatures) ->
                                            let name = Naming.memberName m.Symbol.Name

                                            signatures
                                            |> List.map (fun signature ->
                                                let typeParameters, parameters, returns =
                                                    shapeOne $"{path}.{name}" signature

                                                FsMethod
                                                    {
                                                        Name = name
                                                        Docs = m.Docs
                                                        Tags = signatureTags m.Tags signature
                                                        TypeParameters = typeParameters
                                                        Parameters = parameters
                                                        Return = returns
                                                    }))

                                    let invokes =
                                        fact.CallSignatures
                                        |> List.map (fun signature ->
                                            let typeParameters, parameters, returns = shapeOne $"{path}.Invoke" signature

                                            FsInvoke
                                                {
                                                    Docs = ""
                                                    Tags = []
                                                    TypeParameters = typeParameters
                                                    Parameters = parameters
                                                    Return = returns
                                                })

                                    let bases =
                                        fact.Bases
                                        |> List.choose (fun baseId ->
                                            let reference, baseFindings = typeRef ctx scoped None path baseId
                                            findings <- findings @ baseFindings

                                            let shown =
                                                leafOf reference
                                                |> Option.orElse (
                                                    Map.tryFind baseId model.Types
                                                    |> Option.bind _.SymbolName
                                                    |> Option.map (fun value -> value / uom<symbolName>)
                                                )
                                                |> Option.defaultValue "base"

                                            emit (Finding.make path (ShapeInterfaces.AugmentationHeritageNotInherited shown))
                                            leafOf reference |> Option.map (fun leaf -> $"As{leaf}", reference))

                                    let all = members @ overloads @ invokes

                                    for m in all do
                                        emit (Finding.make $"{path}.{memberLabel m}" (ShapeInterfaces.AugmentationAsExtension path))

                                    if all.IsEmpty && bases.IsEmpty then
                                        None
                                    else
                                        Some
                                            {
                                                Owners = [ path ]
                                                Target = target
                                                TypeParameters = typeParameters
                                                Members = all
                                                Bases = bases
                                                Order = fact.Augmentation.Order
                                            }
                                | _ -> None)

                    let merged =
                        shaped
                        |> List.groupBy _.Target
                        |> List.map (fun (_, group) ->
                            let first = List.head group

                            FsAugmentation
                                { first with
                                    Owners = group |> List.collect _.Owners
                                    Members =
                                        group
                                        |> List.collect _.Members
                                        |> List.distinctBy (fun m -> memberLabel m, signatureKey m)
                                    Bases = group |> List.collect _.Bases |> List.distinctBy fst
                                })

                    let model = { model with Decls = model.Decls @ merged }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
```

`Xantham.Generator.fsproj`, after `<Compile Include="Shape\Exports.fs" />` (`:46`):
`<Compile Include="Shape\Augmentations.fs" />`.
`Passes.fs`, after `Exports.shapeExports`: `Augmentations.shapeAugmentations`.

`shapeMembers` names a literal-keyed overload by the members `additions` carries. If it raises
SI/MB findings with symbol `path.member`, they stay. A `Create` that `ParamObjects` would
synthesize is not at stake here: `FsAugmentation` is not an `FsInterface`.

- [ ] **Step 5: Render the extension**

`Render.fs`, directly before `renderBody` (`:1699`):

```fsharp
/// The JavaScript property access `$0.<key>`, or `$0['<key>']` where the key is not an identifier.
let private emitReceiver (key: string) =
    if jsIdentifier.IsMatch key then
        $"$0.{key}"
    else
        $"$0[{jsQuote key}]"

/// A member of an F# optional type extension. Every member carries `[<Emit>]`: per accessor for
/// a property and an indexer, on the member for a method, a call, a construction and an upcast.
let rec private renderExtensionMember (m: FsMember) : string list =
    let emit (text: string) = $"[<Emit({stringLit text})>]"

    let property (name: string) (reference: FsTypeRef) (readOnly: bool) (key: string) docs tags =
        [
            yield! docLines "" docs tags
            yield $"member _.{ident name}"
            yield $"    with {emit key} get (): {printType reference} = jsNative"

            if not readOnly then
                yield $"    and {emit (key + " = $1")} set (value: {printType reference}): unit = jsNative"
        ]

    let call (template: string) (name: string) typeParameters parameters returns docs tags =
        [
            yield! docLines "" docs tags
            yield emit template
            let head = declHead name typeParameters
            yield $"member _.{head} {renderParamList parameters} : {printType returns} = jsNative"
        ]

    let spread (receiver: string) (parameters: FsParam list) =
        if parameters.IsEmpty then $"{receiver}()" else $"{receiver}($1...)"

    match m with
    | FsProperty p -> property p.Name p.Type p.ReadOnly (emitReceiver p.Name) p.Docs p.Tags
    | FsMethod m ->
        call (literalCallTemplate m.Name [] m.Parameters) m.Name m.TypeParameters m.Parameters m.Return m.Docs m.Tags
    | FsLiteralKeyed(target, literals, FsMethod m) ->
        call (literalCallTemplate target literals m.Parameters) m.Name m.TypeParameters m.Parameters m.Return m.Docs m.Tags
    | FsLiteralKeyed(_, _, inner) -> renderExtensionMember inner
    | FsSymbolKeyed(symbol, FsProperty p) -> property p.Name p.Type p.ReadOnly $"$0[Symbol.{symbol}]" p.Docs p.Tags
    | FsSymbolKeyed(symbol, FsMethod m) ->
        call (spread $"$0[Symbol.{symbol}]" m.Parameters) m.Name m.TypeParameters m.Parameters m.Return m.Docs m.Tags
    | FsSymbolKeyed(_, inner) -> renderExtensionMember inner
    | FsIndexer i ->
        [
            yield "member _.Item"
            yield $"    with {emit "$0[$1]"} get (key: {printType i.Key}): {printType i.Value} = jsNative"

            if not i.ReadOnly then
                yield $"    and {emit "$0[$1] = $2"} set (key: {printType i.Key}) (value: {printType i.Value}): unit = jsNative"
        ]
    | FsConstructor c -> call (spread "new $0" c.Parameters) "Create" c.TypeParameters c.Parameters c.Return c.Docs c.Tags
    | FsInvoke c -> call (spread "$0" c.Parameters) "Invoke" c.TypeParameters c.Parameters c.Return c.Docs c.Tags

/// One optional type extension: the owner's F# type and the members the package adds to it.
let private renderAugmentation (decl: FsAugmentationDecl) =
    let head =
        match decl.Target with
        | FsArray(FsTypeVar name) -> $"type '{Naming.typeVariable name} ``[]`` with"
        | FsApp(name, _) -> $"type {name}{(declHead "X" decl.TypeParameters).Substring 1} with"
        | target -> $"type {printType target} with"

    [
        yield head

        for m in decl.Members do
            yield! renderExtensionMember m |> List.map (indented "    ")

        for name, reference in decl.Bases do
            yield "    [<Emit(\"$0\")>]"
            yield $"    member _.{ident name}: {printType reference} = jsNative"
    ]
```

`declHead "X" …` yields `X<'K, 'V when …>`, so `.Substring 1` is the parameter list, constraints
included, which the extension head must restate (Review Focus 1).

Apply the spike's outcome:
- (a) spelling B: the array head is `` $"type ``[]``<'{Naming.typeVariable name}> with" ``.
- (c) only `EmitIndexer` passed: the indexer renders `[<EmitIndexer>]` above `member _.Item` with
  plain accessors.
- (d) only `EmitConstructor` passed: the constructor renders `[<EmitConstructor>]` in place of
  `emit (spread "new $0" …)`.

In `renderBody`, partition the extensions out of the nested body and append them after it:

```fsharp
    let augmentations =
        decls
        |> List.choose (function
            | FsAugmentation decl -> Some decl
            | _ -> None)

    let nested =
        decls
        |> List.filter (function
            | FsAugmentation _ -> false
            | _ -> true)
```

Build `body` from `nested` in place of `decls`. Keep returning the full `decls`: the footer's
erased arities read the extension members too. Then:

```fsharp
    let extensions =
        match augmentations with
        | [] -> []
        | augmentations ->
            let name =
                if names |> List.exists (fun declared -> declared = "Augmentations" || declared.StartsWith "Augmentations.") then
                    "Augmentations_"
                else
                    "Augmentations"

            [
                "[<AutoOpen>]"
                $"module {name} ="
                augmentations
                |> List.map (renderAugmentation >> List.map (indented "    ") >> String.concat "\n")
                |> String.concat "\n\n"
            ]
            |> List.map (indented indent)
```

The body becomes `body` followed by `"\n\n" + String.concat "\n" extensions` when `extensions` is
non-empty. `indented` skips empty lines (`:1304`). Apply it per line of the joined block if the
blank separator lines carry the indent.

- [ ] **Step 6: Render pins**

Append to `Render.test.fs`'s `testList "render"`:

```fsharp
        testCase "an augmentation renders as an AutoOpen module of emitted extension members" <| fun _ ->
            let decl =
                FsAugmentation
                    { Owners = [ "Window" ]
                      Target = FsNamed "Fable.Core.TS.Dom.Window"
                      TypeParameters = []
                      Members =
                        [ FsProperty { Name = "extra"; Docs = ""; Tags = []; ReadOnly = false; Type = FsString; Literal = None }
                          FsMethod { Name = "go"; Docs = ""; Tags = []; TypeParameters = []; Return = FsString
                                     Parameters = [ { Name = "x"; Type = FsFloat; Optional = false; Rest = false } ] }
                          FsIndexer { Key = FsString; Value = FsObj; ReadOnly = false } ]
                      Bases = [ "AsMixin", FsNamed "Mixin" ]
                      Order = None }

            let source = renderAll { baseModel with Decls = [ decl ] } |> Map.find "TestPkg.fs"

            Expect.stringContains source "[<AutoOpen>]\nmodule Augmentations =\n    type Fable.Core.TS.Dom.Window with" "one module of extensions"
            Expect.stringContains source "        member _.extra\n            with [<Emit(\"$0.extra\")>] get (): string = jsNative\n            and [<Emit(\"$0.extra = $1\")>] set (value: string): unit = jsNative" "a property per accessor"
            Expect.stringContains source "        [<Emit(\"$0.go($1)\")>]\n        member _.go (x: float) : string = jsNative" "a method"
            Expect.stringContains source "with [<Emit(\"$0[$1]\")>] get (key: string): obj = jsNative" "an indexer per accessor"
            Expect.stringContains source "        [<Emit(\"$0\")>]\n        member _.AsMixin: Mixin = jsNative" "an upcast"

        testCase "an array augmentation extends the array type" <| fun _ ->
            let decl =
                FsAugmentation
                    { Owners = [ "Array"; "ReadonlyArray" ]
                      Target = FsArray(FsTypeVar "T")
                      TypeParameters = [ { Name = "T"; Constraint = None } ]
                      Members = [ FsMethod { Name = "last"; Docs = ""; Tags = []; TypeParameters = []; Parameters = []; Return = FsOption(FsTypeVar "T") } ]
                      Bases = []
                      Order = None }

            let source = renderAll { baseModel with Decls = [ decl ] } |> Map.find "TestPkg.fs"

            Expect.stringContains source "    type 'T ``[]`` with\n        [<Emit(\"$0.last()\")>]\n        member _.last () : 'T option = jsNative" "the spike's array head"
```

The literal `` 'T ``[]`` `` follows spike (a). If `FsParam`/`FsTypeParam` carry further fields, the
compiler says so; copy them from the record definitions (`Model.fs:1469-1483`).

- [ ] **Step 7: Every `FsDecl` match**

`rtk grep -rn "| FsExports" src/Xantham.Generator` lists the sites below; the build reports
FS0025 at each explicit match without the new case. Give each an `FsAugmentation` arm:

| Site | Arm |
|---|---|
| `Customization/Semantics.fs:1136` | as `FsExports` is not: `| FsAugmentation _ -> None` (not customizable) |
| `DeclarationCatalog.fs:89`, `:331`, `:1113`, `:1622` | `FsExports`' arm (not a catalogued declaration, P9). At `:1113` that is `[]`. |
| `Pipeline.fs:246-247` | `| FsAugmentation _ -> EntryPackage` |
| `Render.fs:1283` (`declName`) | `| FsAugmentation _ -> ""` |
| `Render.fs:1302` (rename) | `| FsAugmentation decl -> FsAugmentation decl` |
| `Render.fs:1512` (`qualifyDecl`) | `FsAugmentation { decl with Target = qualifyRef foreign decl.Target; TypeParameters = qualifyTypeParams foreign decl.TypeParameters; Members = decl.Members |> List.map (qualifyMember foreign); Bases = decl.Bases |> List.map (fun (name, reference) -> name, qualifyRef foreign reference) }` |
| `Render.fs:1602` (`declErasedArities`) | `decl.Members |> List.collect ofMember` plus `decl.Bases |> List.collect (snd >> erasedArities)` |
| `Render.fs:1713` | `true` |
| `Render.fs:1809` | not reached (partitioned out in Step 5); `| FsAugmentation _ -> []` |
| `Render.fs:2135`, `:2217`, `:2404` | `FsExports`' arm |
| `Shape/Aliases.fs:68` | `[]` |
| `Shape/Arity.fs:128` (`mapDeclRefs`) | `FsAugmentation { a with TypeParameters = a.TypeParameters |> List.map typeParam; Members = a.Members |> List.map declMember; Bases = a.Bases |> List.map (fun (name, r) -> name, reference r) }` |
| `Shape/Arity.fs:161` | `None` |
| `Shape/Coverage.fs:28` | `[]` (Task 8 reads `Owners` separately) |
| `Shape/ExportCollisions.fs:262` | the decl unchanged |
| `Shape/Ordering.fs:30`, `:102` | `""` |
| `Shape/Ordering.fs:60` | `((3, "", ""), (System.Int32.MaxValue * uom<nodeId>)), String.concat "," decl.Owners` |
| `Shape/Ordering.fs:70`, `:92` | `FsExports`' arm, with the decl unchanged |
| `Shape/Orphans.fs:85` (names a declaration reads) | `typeParamNames a.TypeParameters @ List.collect memberNames a.Members @ List.collect (snd >> namesOf) a.Bases` |
| `Shape/Orphans.fs:101` (`declaredNames`) | `[]` |
| `Shape/Orphans.fs:187` | `None` |
| `Shape/UnionArms.fs:175` | the decl unchanged |

Read each site before editing; where its existing arms differ from this table's assumption, follow
the rule: walk and rewrite member references as for `FsInterface`, and name, catalogue or
customize as for `FsExports`. Run `dotnet build src/Xantham.Generator`; expected zero FS0025
naming `FsAugmentation`.

- [ ] **Step 8: The consumer compile test**

Add to `globalAugmentationTests`' lab cases:

```fsharp
                  testCase "a consumer reaches every extension form through the package's module" <| fun _ ->
                      use scratch = Scratch.directory "global-augmentation-consumer"
                      let owner = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default (Path.Combine(package, "node_modules", "augmentation-owner-lab")))

                      let write (rendered: RenderModel) =
                          rendered.Files
                          |> List.filter (fst >> fun name -> name.EndsWith ".fs")
                          |> List.map (fun (name, content) ->
                              File.WriteAllText(Path.Combine(scratch.Path, name), content)
                              name)

                      let sources = write owner @ write (generate ())

                      let consumer = """module GlobalAugmentationConsumer
open Fable.Core
open GlobalAugmentationLab

let extra (window: Fable.Core.TS.Dom.Window) : string = window.extra
let setExtra (window: Fable.Core.TS.Dom.Window) = window.extra <- "x"
let mixed (window: Fable.Core.TS.Dom.Window) : bool = window.mixed
let asBase (window: Fable.Core.TS.Dom.Window) : PkgMixin = window.AsPkgMixin
let dispose (window: Fable.Core.TS.Dom.Window) : unit = window.SymbolDispose()
let last (values: float[]) : float option = values.last ()
let size (map: Fable.Core.TS.Es.Map<string, float>) : float = map.pkgSize ()
let tag (bytes: Fable.Core.TS.Es.Uint8Array<_>) : string = bytes.pkgTag
let call (date: Fable.Core.TS.Es.DateConstructor) : string = date.Invoke true
let create (array: Fable.Core.TS.Es.ArrayConstructor) : string[] = array.Create true
let item (meta: Fable.Core.TS.Es.ImportMeta) : obj = meta.["key"]
let overload (document: Fable.Core.TS.Dom.Document) : Fable.Core.TS.Dom.HTMLDivElement = document.createElement ("div", true)
let env (value: AugmentationOwnerLab.NodeJS.ProcessEnv) : string = value.X
let fmt (options: Fable.Core.TS.Es.Intl.NumberFormatOptions) : string option = options.extraFmt
let added (value: NodeJS.PkgAdded) : float = value.y
let g () : float = Globals.Exports.g
"""

                      let code, output = DeclarationCatalogTests.compileConsumer scratch.Path sources consumer
                      Expect.equal code 0 output
```

Open `System.IO` at the top of `Pipeline.test.fs` if it is not already open (it is, `:5429` uses
`Path`). The member names in the consumer are the ones the Step 1 golden assertions pin. If a
rendered name differs, read the golden first.

- [ ] **Step 9: Run**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "global augmentation"` → PASS, and
`dotnet fsi build.fsx -- test --quick --filter "render"` → PASS.
Read the `global-augmentation-lab` golden diff in full.

Run: `dotnet build Xantham.slnx`. A compile-gate failure in `GlobalAugmentationLab.fs` belongs to
this task; reduce it to one lab declaration. Typical codes:
- FS0957: the head's parameters differ from the owner's;
- FS0438: a duplicate member;
- FS0039: an owner name Core.TS does not declare.

Do not commit. Hand Task 8 the working tree.

---

### Task 8: Audit, re-pins, documentation, goldens — the Stage 4 commit

**Files:**
- Modify: `src/Xantham.Generator/Shape/Coverage.fs` (after `missing`, `:73-85`)
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`dom-shadow-lab` `:698-705`; `augmented-dom-lab` `:6361-6363`; `compiler-lib-ownership-lab` `:2136-2146`)
- Modify: `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs:1371-1394`
- Modify: `tests/Xantham.Generator.RunGate/Xantham.Generator.RunGate.fsproj` (after `:53`), `tests/Xantham.Generator.RunGate/Program.fs`
- Modify: `tests/Xantham.Generator.Tests/golden/**` (regenerated)
- Modify: `docs/.ai/plans/generator-architecture.md` (`:1611-1631`; appended record), `docs/.ai/plans/generator-type-mapping.md` §4.13 (`:802-819`), `docs/.ai/footguns.md` (`:433-435` and a new entry)

**Interfaces:**
- Consumes: Tasks 5–7.
- Produces: the Stage 4 commit.

- [ ] **Step 1: Audit the augmentations**

In `Coverage.fs`, after `missing`:

```fsharp
                    // A package's addition to another group's type is represented by an extension
                    // of that type. An owner whose binding widens, a function overload (P5) and a
                    // value under a foreign namespace (P6) are reported here.
                    let extended =
                        model.Decls
                        |> List.collect (function
                            | FsAugmentation decl -> decl.Owners
                            | _ -> [])
                        |> Set.ofList

                    let adds (augmentation: HarvestedAugmentation) =
                        model.AugmentationFacts
                        |> List.tryFind (fun fact -> fact.Augmentation.Owner.SymbolId = augmentation.Owner.SymbolId)
                        |> Option.map (fun fact ->
                            not (
                                fact.Members.IsEmpty
                                && fact.Overloads.IsEmpty
                                && fact.CallSignatures.IsEmpty
                                && fact.ConstructSignatures.IsEmpty
                                && fact.IndexInfos.IsEmpty
                                && fact.Bases.IsEmpty
                            ))
                        |> Option.defaultValue true

                    let unextended =
                        model.Harvest.Augmentations
                        |> List.filter (fun augmentation -> adds augmentation && not (Set.contains augmentation.Path extended))
                        |> List.map (fun augmentation -> Finding.make augmentation.Path AuditCoverage.ExportNotRepresented)
```

Return `missing @ unextended` in place of `missing`. Amend the pass doc: "…every harvested export,
and every addition a package makes to another group's type, either appears in the declarations or
is the subject of a finding this pass adds…".

Shape unit case, in `testList "shape passes"` after plan 2's audit-coverage cases:

```fsharp
        testCase "audit-coverage reports an addition with no extension" <| fun _ ->
            let window = Build.symbol 330 "Window" SymbolFlags.Interface

            let model =
                { Build.shapeModel [] with
                    Harvest = { HarvestModel.Empty with Augmentations = [ { Owner = window; Path = "Window"; Order = None } ] } }

            let _, findings = Build.runPass Coverage.auditCoverage model

            Expect.equal (findings |> List.map (fun f -> f.Key, f.Symbol)) [ "AC001", "Window" ] "a widened owner's addition is missing"
```

- [ ] **Step 2: Re-pin the labs whose owners stopped being copied**

`dom-shadow-lab` (`:698`), add a case beside the `ShadowedByLib` one:

```fsharp
                  testCase "a global script's additions to DOM types are extensions of the DOM bindings" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Fable.Core.TS.Dom.Response with" "the owner is extended"
                      Expect.stringContains source "[<Emit(\"$0.xanthamDomShadowLabMarker\")>] get (): string option = jsNative" "with the addition"
                      Expect.equal (source.Split("type Fable.Core.TS.Dom.").Length - 1) 21 "one extension per interface the script adds to"
                      Expect.isFalse (rendered.Findings |> List.exists (fun f -> f.Key = "HG003")) "the run harvests something"
```

`augmented-dom-lab` (`:6361-6363`): replace `(fun _ -> [])` with:

```fsharp
                (fun package ->
                    [ testCase "an added type parameter keeps the DOM type and widens" <| fun _ ->
                          let rendered =
                              Async.RunSynchronously(
                                  Pipeline.generate
                                      { GeneratorConfig.Default with Lib = Some [ "esnext"; "dom" ]; Types = Some [] }
                                      package)

                          let source = rendered.Files |> List.head |> snd

                          Expect.isFalse (source.Contains "type Request<'T> =") "the owner is not copied"
                          Expect.stringContains source "type Fable.Core.TS.Dom.Request with" "the DOM type is extended"
                          Expect.stringContains source "[<Emit(\"$0.cf\")>] get (): obj option = jsNative" "the field reads obj"
                          Expect.stringContains source "static member request (value: Fable.Core.TS.Dom.Request, init: Fable.Core.TS.Dom.RequestInit)" "references apply the owner's arity"

                          for owner in [ "Request"; "RequestInit" ] do
                              Expect.contains (rendered.Findings |> List.map (fun f -> f.Key, f.Symbol)) ("SI011", owner) $"{owner}'s added T is reported" ])
```

`DeclarationCatalog.test.fs:1382-1391`, the consumer becomes:

```fsharp
                    let consumer = """module Identity.Consumer
open Fable.Core
open Identity.Root
let value (request: Fable.Core.TS.Dom.Request) : obj option = request.cf
let options (init: Fable.Core.TS.Dom.RequestInit) : obj option = init.cf
let plain () : Fable.Core.TS.Dom.Response = Identity.Root.Exports.plain()
let invoke (request: Fable.Core.TS.Dom.Request) (init: Fable.Core.TS.Dom.RequestInit) : JS.Promise<Fable.Core.TS.Dom.Response> =
    let fetch = Identity.Root.Exports.request(request, init)
    fetch.Invoke(U3.Case1 "https://example.invalid/", None)
"""
```

The test name stays. Its claim ("augmented DOM types … compile against Core.TS") now holds through
the extensions.

- [ ] **Step 3: The run gate**

`Xantham.Generator.RunGate.fsproj`, after the `mixed-subpaths-lab` link (`:53`):

```xml
        <Compile Include="..\Xantham.Generator.Tests\golden\augmentation-owner-lab\AugmentationOwnerLab.fs" Link="augmentation-owner-lab\AugmentationOwnerLab.fs" />
        <Compile Include="..\Xantham.Generator.Tests\golden\global-augmentation-lab\GlobalAugmentationLab.fs" Link="global-augmentation-lab\GlobalAugmentationLab.fs" />
```

`Program.fs`: add `open GlobalAugmentationLab.Augmentations` beside the file's other `open`
lines; it opens the extensions alone. Add before `main`:

```fsharp
/// A module package's globals bind through globalThis, and its additions to other groups'
/// types reach the owners' objects.
let private globalAugmentation () =
    equal "a writable global reads globalThis" 1. GlobalAugmentationLab.Globals.Exports.g
    GlobalAugmentationLab.Globals.Exports.g <- 4.
    check "and an assignment writes through to it" (emitJsExpr () "globalThis.g === 4")
    GlobalAugmentationLab.Globals.Exports.h ()
    check "a global function is called" (emitJsExpr () "globalThis.hCalled === true")

    let window = GlobalAugmentationLab.Exports.getWindow ()
    equal "an added property reads the owner's object" "extra0" window.extra
    window.extra <- "extra1"
    equal "and writes it" "extra1" window.extra
    equal "an added base's member reads through its extension" true window.mixed
    equal "an addition to a dependency's namespace member reads its object" "x0" (GlobalAugmentationLab.Exports.env ()).X
    equal "an addition to Array reaches the array" (Some 3.) ((GlobalAugmentationLab.Exports.numbers ()).last ())
```

In `main`, call `globalAugmentation ()` after `mixedSubpaths ()` (`:2363`).

- [ ] **Step 4: Regenerate, gate and measure**

```
dotnet fsi build.fsx -- test --update
dotnet build Xantham.slnx
dotnet fsi build.fsx -- findings > tests/.scratch/global-augmentation/findings-after.txt
diff tests/.scratch/global-augmentation/findings-before.txt tests/.scratch/global-augmentation/findings-after.txt
rtk git diff --stat -- tests/Xantham.Generator.Tests/golden
```

Expected moves:
- **solid-js:** `Fable.Core.TS.Es.SymbolConstructor with` + `observable` (SI009 +1).
- **type-fest** and **symbol-member-lab:** the `SymbolConstructor` extension (+1 each).
- **augmented-dom-lab:** ~280 copied lines become 2 extensions; SI011 +2,
  `LibExtraTypeArgumentsDropped` +2 or +4.
- **dom-shadow-lab:** 21 extensions; HG003 −1.
- **@cloudflare/workers-types:** the copied `Disposable` disappears. Its `[Symbol.dispose]` is
  lib.esnext's redeclared, so nothing is added.
- **global-augmentation-lab:** as Task 7.
- **SI009–SI011** rise only in these.
- **AC001:** unchanged, or rises only where Step 5 explains each case.

On a compile-gate failure, report the first several distinct error codes with one site each. Any
other moved golden is reported with a sample (`git diff -- <golden> | head -n 200`) before
committing.

- [ ] **Step 5: Read the diffs by sample; residuals**

For workers-types, solid-js and type-fest: `git diff -- <golden>/*.fs | head -n 200` and
`rtk grep -n "module Augmentations" -A20 <golden>/*.fs`. For each AC001 that is new in
`findings-after.txt`:
1. Write the owner's declaration into `tests/.scratch/global-augmentation/residual-<name>/`.
2. Generate it with `dotnet run --project src/Xantham.Cli -- generate <lab> -o <lab>/out`.
3. Decide whether the owner widens (P8, expected) or a pass drops the target.

A drop by a pass is handed back with the five-line reproducer.

- [ ] **Step 6: The compiler-lib-ownership tests, re-read (C9)**

Re-read `Pipeline.test.fs:2046-2146`. Each case holds under P3: the lab ships the compiler lib, so
no owner of it is an augmentation:
- `GlobalThis` placement is decided by the source certificate (`Pipeline.fs:16-53`), not by origin;
- `Date` stays in the core;
- the entry's global functions remain `GlobalThis` methods.

Add to `"compiler-lib ownership checks referenced augmentation files too"`, after its last
assertion:

```fsharp
                          (core.Contains "type DateConstructor =", entry.Contains "type DateConstructor =")
                          |> Flip.Expect.equal "a shipped owner keeps its augmented declaration in its own group" (true, false)

                          entry.Contains "module Augmentations"
                          |> Flip.Expect.equal "and the entry writes no extension of it" false
```

Measures already show `(true, false)` at the frozen CLI (`out-cl-ref/`). The pin holds that
under the re-homing's removal.

Read `nested-dep-lab`'s golden diff: its dependencies ship, so it must not move.

- [ ] **Step 7: Run the full suite**

Run: `dotnet fsi build.fsx -- test --run-gate` → PASS.

- [ ] **Step 8: Documentation**

`generator-architecture.md`, at the end of "Augmented libraries and exclusive factories
(2026-09-13)" (`:1611-1631`), after its first paragraph:

```markdown
**Superseded 2026-10-15** (Global augmentation, below). A standard-library or dependency type a
package augments keeps the group of its first declaration; the package's additions are
extensions of the owner's binding, not a copy of the owner. `augmented-dom-lab`'s `Request<T>`
is `Fable.Core.TS.Dom.Request` with an `obj option` `cf` extension (`SI011`), and Node no longer
declares 16 lib types of its own.
```

Append at the end of `generator-architecture.md`:

```markdown
## Global augmentation (2026-10-15)

A module package's `declare global` block is the package's API. `harvest-globals` reads global
scope at a default-library file in every run without a global input. The package's own globals
bind under `Globals.Exports`: a `var` is a bare settable static under `[<Global("globalThis")>]`,
and functions and lexical globals keep `[<Global(name)>]`. A symbol another group declares first
and the package declares again is an augmentation (`HarvestModel.Augmentations`), unless its group
ships in the run. Resolve no longer re-homes such an owner onto the package.
`resolve-augmentations` reads what the package adds, per declaration: members it declares first,
overloads and signatures its declarations write, `extends` bases and added type parameters.
`shape-augmentations` writes one `FsAugmentation` per owner F# type, rendered as an
`[<AutoOpen>] module Augmentations` of optional type extensions whose members are `[<Emit>]` (per
accessor for properties). `[<Erase>]` on an extension member fails from a DLL and from source
(spike, `e-global-augmentation/findings.md` §1). Findings:
- `SI009` per extension member (Ergonomic);
- `SI010` per added base, reached through `As<Base>` (Widened);
- `SI011` per added type parameter (Widened);
- `AC001` for an owner whose binding widens, a package overload of a foreign function, or a value
  under a foreign namespace.

A package-first type under a foreign namespace is declared in the package's module of that path
(`NodeJS.PkgAdded`). Lib and referenced namespace members are written under their namespace
(`Fable.Core.TS.Es.Intl.NumberFormatOptions`); `ApiVersion` is 10.
Measures: <findings diff summary, golden line counts before/after, Node and dom-shadow-lab counts>.
```

Fill the measures from Step 4 and Task 9.

`generator-type-mapping.md` §4.13, after the "Declaration merging" bullet:

```markdown
- **Global augmentation (2026-10-15).** A merge across groups (`declare global { interface
  Window { extra: string } }`, `interface Array<T> { last(): T | undefined }`, `namespace NodeJS
  { interface ProcessEnv { X: string } }`) keeps the owner's binding. The package's additions are
  F# optional type extensions in `[<AutoOpen>] module Augmentations`: a property is per-accessor
  `[<Emit("$0.x")>]`/`[<Emit("$0.x = $1")>]`, a method `[<Emit("$0.m($1, …)")>]`, a call signature
  `Invoke`, a construct signature `Create`, an index signature `Item`, and an added base an
  `[<Emit("$0")>] As<Base>`. `Array`/`ReadonlyArray` extend `'T[]`. An extension member is not
  abstract, so SRTP, object expressions and the owner's `Create` do not see it (`SI009`).
```

`footguns.md`, replace "**Array identity comes from the checker.** Global augmentation can classify
the built-in `Array<T>` under `@types/node`." with "**Array identity comes from the checker.** A
package's augmentation of `Array<T>` leaves it the compiler lib's (2026-10-15), but a local
interface named `Array<T>` still exists." Keep the rest of that entry. Append:

```markdown
**An extension member reaches JavaScript only through `[<Emit>]`.** Fable resolves an extension
member of a referenced assembly only in an emitted form: `[<Erase>]` or a bare `jsNative` is
"Cannot reference member from .dll reference", and from source `[<Erase>]` imports a function the
library never exports. A property needs the Emit on each accessor. An extension head restates the
owner's type parameters and constraints exactly (FS0957), and two blocks on one target are merged
(`shape-augmentations`), since `Array` and `ReadonlyArray` are both `'T[]`.
```

- [ ] **Step 9: Commit Stage 4**

```
rtk git add src/Xantham.Generator tests/fixtures/global-augmentation-lab tests/Xantham.Generator.Tests tests/Xantham.Generator.RunGate docs/.ai/plans/generator-architecture.md docs/.ai/plans/generator-type-mapping.md docs/.ai/footguns.md docs/.ai/plans/2026-10-15-global-augmentation.md
rtk git status
rtk git commit -m "fix(generator)!: write a package's additions to other groups' types as extensions" -m "<one paragraph: re-homing removed; resolve-augmentations; shape-augmentations; SI009-SI011; findings diff and golden line counts from Step 4>" -m "BREAKING CHANGE: a binding no longer declares its own copy of a standard-library or dependency type its package augments (Window, Request, Uint8Array, ...). Signatures read the owner's binding (Fable.Core.TS.Dom.Window), and the package's additions are extension members in the binding's AutoOpen Augmentations module. A type parameter only the package declares is dropped from references (SI011)." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

`.serena/project.yml` stays unstaged.

---

### Task 9: Core.TS unchanged; Node regenerated as a breaking change

**Files:**
- Modify: `src/Xantham.Fable.Node/{Node.fs,manifest.json,symbols.jsonl,README.md}` (regenerated + note)
- Modify: this plan's Measures

**Interfaces:**
- Consumes: Stage 4.
- Produces: two commits (Node, measures). Core.TS gets a commit only if Step 1 shows a diff.

- [ ] **Step 1: Core.TS**

Run: `dotnet fsi build.fsx -- generate --only compiler-lib`, then `rtk git status src/Xantham.Fable.Core.TS`.
Expected: no change. The run ships the compiler lib, its entry is `export {}`, and P3 records no
augmentation. A diff is handed back with its `--stat` and a 100-line sample.

- [ ] **Step 2: Regenerate and measure Node**

```
dotnet fsi build.fsx -- generate --only node-lib
git diff -U0 -- src/Xantham.Fable.Node/Node.fs | grep -E '^-(type|and) ' > tests/.scratch/global-augmentation/node-removed.txt
git diff -U0 -- src/Xantham.Fable.Node/Node.fs | grep -E '^\+(type|and) ' > tests/.scratch/global-augmentation/node-added.txt
rtk grep -n "module Augmentations" -A3 src/Xantham.Fable.Node/Node.fs
rtk grep -c "SI009\|SI010\|SI011" src/Xantham.Fable.Node/symbols.jsonl
rtk wc -l src/Xantham.Fable.Node/Node.fs
```

Expected:
- **Removed copies — the 16 lib names:** `Int8Array`, `Uint8Array`, `Uint8ClampedArray`,
  `Int16Array`, `Uint16Array`, `Int32Array`, `Uint32Array`, `Float32Array`, `Float64Array`,
  `BigInt64Array`, `BigUint64Array`, `IteratorObject`, `AsyncIteratorObject`, `Disposable`,
  `AsyncDisposable`, `ImportMeta`. List the exact set from `node-removed.txt`; a removed name
  outside it is handed back.
- **Extensions:**
  - `ImportMeta` (+5);
  - `ErrorConstructor` (+3), unless `ErrorConstructor` binds to a name this run widens. Then it is
    `AC001`; report which.
  - `SymbolConstructor` (+2);
  - `As` upcasts with SI010 on the 14 owners extending `RelativeIndexable`.
- **Line count:** record before/after.

- [ ] **Step 3: Gate Node**

Run: `dotnet build Xantham.slnx`, then `dotnet fsi build.fsx -- test` → PASS. A Node compile
failure is reduced to a lab declaration in `global-augmentation-lab` and fixed in the owning pass,
then the Stage 4 commit is amended by a follow-up `fix(generator):` commit. It is not fixed in
Node.fs.

- [ ] **Step 4: Node migration note and commit**

Add to `src/Xantham.Fable.Node/README.md`, after its first paragraph:

```markdown
## Standard-library types are Core.TS's

From the release after 0.4.0-beta.1, this binding no longer declares its own copies of the
standard-library types `@types/node` augments: the typed arrays, `IteratorObject`,
`AsyncIteratorObject`, `Disposable`, `AsyncDisposable` and `ImportMeta`. Signatures read the
`Fable.Core.TS.Es` types (`Fable.Core.TS.Es.Uint8Array<'TArrayBuffer>`), and `@types/node`'s
additions to them are extension members in `Node.Augmentations`, opened with `Node`. Replace
`Node.Uint8Array<_>` with `Fable.Core.TS.Es.Uint8Array<_>`; the full list is in the release notes.
```

```
rtk git add src/Xantham.Fable.Node
rtk git commit -m "feat(node)!: write @types/node's additions to standard-library types as extensions" -m "<removed names from node-removed.txt; extension counts; SI009/SI010/SI011 counts; Node.fs lines before/after>" -m "BREAKING CHANGE: Node no longer declares its own copies of 16 standard-library types (Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array, BigInt64Array, BigUint64Array, IteratorObject, AsyncIteratorObject, Disposable, AsyncDisposable, ImportMeta). Signatures read the Fable.Core.TS.Es types, and @types/node's additions to them are extension members in Node.Augmentations." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

The footer's name list matches `node-removed.txt`; correct it to the measured set.

- [ ] **Step 5: Record the measures**

Fill this plan's Measures with Task 8 Step 4's findings diff and Step 2's Node numbers, and fill
the architecture record's `Measures:` line.

```
rtk git add docs/.ai/plans/2026-10-15-global-augmentation.md docs/.ai/plans/generator-architecture.md
rtk git commit -m "docs(plans): record global-augmentation measures" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -m "Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

Push Stages 2–5 together.

---

## Reported, not in this plan

- **Module augmentation** (`declare module "vue" { interface ComponentCustomProperties { … } }`,
  100 blocks in the corpus: `@types/three` 84, `@types/lodash` 12, puppeteer 3,
  declaration-identity-lab 1). This augments another *module's* exports, not global scope, and
  needs its own reproducer and ruling.
- **A package overload of a foreign global function** (P5): `AC001`. Census: 11 in the non-golden
  `@types/node` fixture under DOM.
- **A value a package declares first under a foreign namespace** (P6): `AC001`. Census: 0.
- **A module run with no default library** (`lib: []`/`noLib`): no file's scope is global scope
  alone, so its `declare global` stays unread (Task 5 Step 6).
- **Plan 2 gap (P11):** lib and referenced namespace members are written flat by `libBinding` and
  `referencedRef`. After plan 2's Core.TS regeneration they are FS0039 in any consumer binding.
  Task 4 closes it if plan 2 has not.
- A shipped group's merged owner carries the entry's additions in that group's module (P3). This
  is today's behaviour and is certified as unverified scope.

## Self-review

- **Spec coverage** (findings §3 root causes, §4 Option 1, §5 test plan):
  - (1) the early return: Task 5 Step 6;
  - (2) `admits` and `ShadowedByLib`: Task 5 Step 7 (the inventory);
  - (3) the re-homing: Task 6 Step 4;
  - (4) owner members lost under a dependency group's disposition: P3, P8, Task 8 Step 1;
  - (5) the audit: Task 8 Step 1;
  - plan 2's R3 narrowing: consumed through P6 and the `foreign-namespace-lab` re-pin (Task 5 Step 2);
  - `SI009`–`SI011`: Task 7 Step 1;
  - computed keys through `MB009`: Task 7 Step 5 (`FsSymbolKeyed`);
  - duplicate extensions: P9, P10;
  - "a var redeclaring a foreign var belongs to its owner": P4;
  - catalogue and `ApiVersion`: P9, C7, Task 4;
  - the churn list: Task 8 Step 4 and Task 9;
  - the compiler-lib-ownership re-read: C9, Task 8 Step 6;
  - flattened lib-base members not double-reported (findings §4 risk): P4 reads only members the
    package declares first;
  - §5 test plan: Tasks 5–8, the run gate, and the mixed-run regression in Task 2;
  - spike-uncovered forms: Task 1;
  - both adjacent defects: Tasks 2–3.
- **Placeholders:** the angle-bracketed commit bodies and measures are filled from named steps'
  output. The Spike results section is filled by Task 1 Step 5.
- **Type consistency:** these names are spelled identically across tasks:
  - `HarvestedAugmentation { Owner; Path; Order }`, `HarvestModel.Augmentations`;
  - `ResolvedAugmentation` fields, `ResolveModel`/`ShapeModel.AugmentationFacts`;
  - `FsAugmentationDecl { Owners; Target; TypeParameters; Members; Bases; Order }`, `FsDecl.FsAugmentation`;
  - `resolveAugmentations` / `"resolve-augmentations"`, `shapeAugmentations` / `"shape-augmentations"`;
  - `Spec.augmentedArity`, `Spec.namespaceQualified`, `TypeFacts.NamespacePath`;
  - `ShapeInterfaces.AugmentationAsExtension`/`AugmentationHeritageNotInherited`/`AugmentationTypeParameterWidened`.
- **Review Focus:** each line names its pinning test.
