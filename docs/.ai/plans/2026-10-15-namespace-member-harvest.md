# Namespace Member Harvest — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every exported type member of a namespace is declared under its namespace, as
TypeScript spells it (`Pool.Idle`, `Intl.Collator`, `JSX.Element`, `NodeJS.Signals`), whether
or not a signature reaches it. A member that cannot be declared carries `AC001` instead of
vanishing (pre-#158 problem **A**, plus the flat rendering of reached namespace members, which
is the same defect).

**Architecture:** A new Harvest pass, `harvest-namespace-members`, walks every harvested
namespace export recursively (`getExportsOfModule`, aliases followed). It appends each
interface, alias, class and enum member as a `HarvestedExport` with `HasValueExport = false`
under its parent's `Origin`, and records every walked namespace in `HarvestModel.Namespaces`
under its dotted path. `harvest-globals` also harvests a package-global namespace of types alone
(`TailStream`, `NodeJS`). The pass also harvests the `declare global` namespaces a module package
declares first; a namespace another group declares first stays its owner's. `drop-orphan-delegates`
prunes only names `synthesize-anonymous` recorded as claimed from a derived position
(`ShapeModel.MintedNames`), so a harvested member can never be pruned as an orphan.
In the shape tier, one helper, `Spec.namespacePath`, gives every declaration written inside a
namespace that namespace's module path. `name-exports`, `synthesize-anonymous`, `shape-aliases`
and `name-constructor-objects` nest under it unconditionally, so contest no longer decides it.
Nesting under a namespace is the source spelling and raises no finding. `SY004` stays for
position-derived nesting. Because emitted names change, `CatalogCompatibility.ApiVersion`
goes 8 → 9, and Core.TS and Node are regenerated as a documented breaking change.

**Tech Stack:** F# generator (`src/Xantham.Generator`), Expecto suites, Fable.Core 5.2.0 compile
gate, Fable 5.17.2 run gate, TypeScript 7.x `tsc --api` (live) and the TypeScript 6.0.3 JS API
(census scripts only).

**Spec:** `docs/.ai/plans/2026-10-15-pre158-findings.md` §A (controller ruling: Option 1;
`AC001` covers residuals; amend `generator-architecture.md` §3 Tier 1 invariant), its repro lab
`tests/.scratch/pre158/a-unreached-ns/lab/index.d.ts`;
`docs/.ai/plans/2026-10-15-pre158-findings.md` §H.4 option 2 and §H.5 (R11);
`docs/.ai/plans/2026-10-15-pre158-findings.md` §E.4 first paragraph (R3's seed); and the
user's standing rule to choose the most faithful option, from their ruling of 2026-10-15:
*"A bug is a bug, and a blast radius from fixing the bug does not legitimise a representation that
is farther from the source."* Fidelity to the TypeScript source decides every ruling below. Blast
radius, back-compatibility and golden churn are measured, release-noted and reviewed, never
weighed. Companion plan, executed first: `docs/.ai/plans/2026-10-15-exclusive-fold-and-value-side-fixes.md`.

## Global Constraints

- Generated bindings target Fable 5.x only (`Fable.Core` 5.2.0 compile gate, Fable 5.17.2 run gate).
- Tests run only via `dotnet fsi build.fsx -- test [--quick] [--update] [--filter <prefix>] [--run-gate]`; never `dotnet test` (it stalls on the suites' nested builds).
- Goldens change only via `--update` (or `--goldens`), never by hand, and every golden diff is read before commit — aggregate measures plus bounded samples for the large fixtures (`.claude/rules/generator-fixtures.md`).
- A new finding code is a new case in `Findings.fs` + a `FindingCodes.table` row (next unused number under its prefix) + a `Findings.test.fs` pin. A retired case keeps its row. **This plan adds and retires no code** (R6).
- The phase record in `docs/.ai/plans/generator-architecture.md` / `generator-type-mapping.md` is updated in the same commit as the behaviour change.
- Commits use conventional-commit subjects and end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Scratch files go under `tests/.scratch/`, never the system temp directory. Shell commands are prefixed with `rtk`. Never stage `.serena/project.yml`.
- Branch: from `feat/tagged-arm-158` (PR #160, unmerged), after the companion plan's commits. The companion's `Spec.isAnonymousArm` fold restriction, `<Stem>Namespace`/`<Stem>Enum`/`<Stem>Value` value-side names (`SY007`) with value-owned shapes nested under the source namespace (`nestBases`, `Point.Defaults`), and the #158 DD002/DT007 machinery sit in the files this plan edits. Rebase onto `develop` once #160 merges.
- Doc comments follow `.claude/rules/comments.md`; reviewer-only notes use `//FOR-REVIEW`.
- `src/**/*.fs` stays Fantomas-formatted (`build.fsx` formats unless `--no-format`); goldens stay unformatted.
- Release notes are the commits' `BREAKING CHANGE:` paragraphs. ShipIt renders them into each package's `CHANGELOG.md` at `build.fsx -- bump`, and only release commits (`chore: prepare package release`) edit a `CHANGELOG.md`. The migration notes live in each package's `README.md`.

## Stages

| Stage | Tasks | Commits | Boundary |
|---|---|---|---|
| 1 | Task 1, Task 2 | 1 (Task 2) | Task 1 measures and commits nothing. Task 2 moves no golden (verified in its Step 5) and fixes the audit contract the harvest relies on. |
| 2 | Task 3, Task 4 | 1 | Nesting and harvest move every golden with a namespace, and a golden commit is preceded by reading it. Task 3 proves the behaviour on labs and unit tests and hands Task 4 an uncommitted tree. |
| 3 | Task 5 | 2 | Core.TS and Node are regenerated only on opt-in, each in its own commit (precedent `90c35bc1`/`dae1fe29`). Stage 3 is pushed together with Stage 2: Stage 2's `ApiVersion` 9 rejects the committed Core.TS catalogue (API 8) until Task 5 regenerates it. |

## Measures (taken while planning, 2026-10-15, at `91742f98`)

Census method: exported type members (interface, alias, class, enum) of exported or
package-global namespaces, recursively, checked against the committed binding.
- **absent**: no `type <Name>` declaration.
- **declared flat**: declared, and no `<Namespace>.<Name>` qualified text anywhere in the binding.
- **nested**: such text exists (an upper bound: doc text and position-minted paths also match).

An alias counts as present only under its own name, so a reached alias rendered under a
position-minted name (`JSX.Element` → `JSXElement`, `Ns.Kind` → `G.K`) counts as absent.

| Binding | Total | Absent | Declared flat | Nested (≤) |
|---|---|---|---|---|
| **Core.TS** `Fable.Core.TS.fs` | 169 | 28 (Intl 19, WebAssembly 8, Temporal 1) | **124** (Intl 56, Temporal 54, WebAssembly 14) | 17 |
| **Node** `Node.fs` (`@types/node` 22.20.2) | 391 | 63 (NodeJS 34, `crypto` webcrypto 9, `inspector` 18, `module` 2) | **153** (NodeJS 36, `crypto` webcrypto 32, `node:test` test 17, `inspector` 37 across 9 namespaces, `assert` strict 9, `stream` Stream 9, `module` Module 7, `events` EventEmitter 6) | 175 |
| `@cloudflare/workers-types` golden | 108 | 32 | 73 | 3 |
| `solid-js` golden | 226 | 225 | 1 | 0 |
| `type-fest` golden | 39 | 23 | 1 | 15 |
| labs: `ambient-module-lab` 2, `dependency-entrypoint-lab` 2, `tagged-arm-lab` 2, `catalog-class-inheritance-lab` 1 flat; `dollar-name-lab` 1, `nested-name-lab` 1 nested | | 0 | 7 | 2 |

**Renamed public types (lower bound): Core.TS 124, Node 153.** Every flat declaration moves under
its namespace, and every absent alias that is reached today drops its position-minted name.
Raw output: `tests/.scratch/pre158/plan-namespace-member-harvest/census-flat.txt` (per-member
list), `census-flat-fixtures.txt`. Task 1 re-runs both; Task 5 records the exact rename table
from the diff.

Frozen-CLI evidence (`tests/.scratch/pre158/plan-namespace-member-harvest/`):
- **`lab-nested/`**: the Task 3 lab before this change. `Idle`, `Deep`, `Lonely`, `Shapes.Node`,
  `Shapes.Exports`, `Extra`, `Fragment`, `Slot` and `Unread` are absent with zero `AC001`.
  `Reached`, `Conn`/`ConnConstructor`, `Element` and `Attributes` are declared flat at the root.
- **`lab-alias/`**: a reached namespace interface named `Exports` displaces the value container
  to `Exports_923fe53966c6`. A reached namespace alias is minted by position (`G.K`, `G.P`).
- **`lab-prim/`**: `type A = string; type B = string` declares `type A = string` and
  `type B = A`; readers write `string`.

## Rulings

- **R1 — Namespace type members always nest under their namespace.** This holds whether or not the
  name is contested, and for reached members as well as newly harvested ones. TypeScript spells
  them `Ns.Member`, and F# can spell them the same way. One helper, `Spec.namespacePath`, reads
  `Harvest.Namespaces`, and every pass that names a type from a symbol prefixes it:
  - `name-exports` (`ExportNames.fs:121-127`)
  - `synthesize-anonymous` (`Anonymous.fs:352-367` claim, `:764` export root)
  - `shape-aliases` (`Aliases.fs:118-122`)
  - `name-constructor-objects` (`ConstructorObjects.fs:66-82`, `:143`)

  The F# side is already proven:
  - `type X` beside `module X` in one `module rec` is legal F#, with an implicit `ModuleSuffix`,
    as `type Widget` + `module Widget` in the `nested-name-lab` golden shows.
  - A namespace's value side keeps its root type (`type Pool`) and its binding
    (`static member Pool: Pool`). Its types live in `module Pool`.
  - A reference in a type position (`Pool.Idle`) resolves to the module.

  *Cost if wrong:* none known on fidelity grounds. A DU case shares its spelling with a member
  module in expression positions (R10).
- **R2 — The value container's name follows from R1.** Every namespaced claimant now nests, so
  `WebAssembly.Exports` and `Shapes.Exports` cannot displace the container. A top-level
  `export interface Exports` keeps the source name, and the generator-invented container
  yields (`Exports_<digest>`, `ExportLayout.fs:197-211`), exactly as today. The source name
  outranks the generator's. No special case in `name-exports`.
- **R3 — A module package's `declare global { namespace X }` is harvested.** It is API that the
  package contributes to global scope; the earlier "stays reach-only" ruling dropped source API.
  `harvest-namespace-members` seeds from the global namespaces that are in scope at the entry
  file and **first declared under the package**: no parent symbol,
  `Grouping.classify … = EntryPackage` (which reads the first declaration), and not already
  harvested. Each is harvested as a `FromGlobal` export, so a mixed run renders it under
  `Globals.` and binds any value with `[<Global>]`.

  `Grouping.declaresUnderPackage` is the wrong test: it is true for a *foreign* namespace the
  package adds to (`NodeJS`, `Intl`, per `e-global-augmentation/findings.md` §4). With it, this
  plan would re-home `NodeJS` as the package's own, and `ships` would then silently drop the
  owner's members (`ProcessEnv`, `NumberFormatOptions`). A foreign namespace's members belong to
  their owner in this plan.

  The package's *additions* to a foreign namespace (`NodeJS.PkgAdded`, `ProcessEnv.X`) are written
  as extensions of the owner's types by plan 3,
  `docs/.ai/plans/2026-10-15-global-augmentation.md`. Plan 3 also owns a module package's
  non-namespace `declare global` declarations (`var`, `function`, `interface`), because they
  share its `Globals` and extension rendering. Pinned by `foreign-namespace-lab` (Task 3 Step 2b):
  `NodeJS` is not re-homed, and the package-first `PkgNs` is declared.
- **R4 — A nested namespace is audited through its members and its parent's value side.** A
  nested namespace enters `Namespaces` under its dotted path (`Pool.Inner`). Its types nest
  there and are each audited. A nested namespace's value is a member of its parent's value
  interface (`Pool.Inner: …`), which `shape-interfaces` shapes and reports like any member.
  Appending it to `Exports` would audit an artefact that F# expresses as that member, not a
  declaration of its own. *Cost if wrong:* if a nested value namespace whose parent value
  interface omits it exists, that gap is a `shape-interfaces` finding gap, not a harvest gap.
- **R5 — A namespace is represented by its module, that is, by its represented members.**
  `audit-coverage` (`Coverage.fs:44-81`) tests a namespace by name or name-prefix. Under `Globals.`
  or a subpath, `Globals.TailStream.X` neither equals nor starts with `TailStream.`. A namespace
  export is therefore represented when any harvested member it parents is represented
  (recursively). A namespace with harvested members, none represented, reports `AC001` beside each
  member's `AC001`: the module is missing as much as they are. The empty namespace whose members
  all omit `export` stays excused.
- **R6 — No finding code is added or retired.** `SY004 NameNestedUnderOwner` (`[<Exact>]`,
  `Findings.fs:932`, row `:200`) stops being raised for namespace nesting. The nesting is the
  source spelling, not a choice the generator made. It stays for position-derived nesting
  (`Widget.Options`), its only remaining use, which its current message ("anonymous shape named
  … under the declaration that owns it", `:950`) describes accurately. Residual drops carry
  `AC001`.
- **R7 — `CatalogCompatibility.ApiVersion` 8 → 9.** A consumer reusing a catalogued declaration
  compares its own `surface decl` with the producer's `Api`
  (`DeclarationCatalog.fs:1643-1662`). The surface's reference nodes carry F# names
  (`node "named" [ text name ]`, `:1243`). A new generator reading a Core.TS catalogue built at
  API 8 therefore computes `Intl.NumberFormatOptions` where the producer recorded
  `NumberFormatOptions`, and fails with a per-declaration "F# API mismatch". An old generator
  reading the new catalogue fails the same way. `footguns.md` §"Schema 2 compatibility" assigns
  emitted-API changes to `apiVersion` "when an earlier producer must be rejected". API 7
  (standard-library names) and API 8 (literal-keyed methods, `7b3b20a9`) are the precedent. The
  bump makes the rejection a contract error instead of a mismatch on the first declaration
  compared. Pin test in `CatalogCompatibility.test.fs`.
- **R8 — Members carry `HasValueExport = false`, and a class member's constructor nests with it.**
  A member's value binds through the namespace object: `Pool.Conn: ConnConstructor` sits on the
  value interface `Pool`. So the container gains no binding, and the class renders its instance
  side (arch §3 Tier 1, "Type-only classes retain their instance shapes"). The constructor object
  is named `Pool.ConnConstructor`, under the class's namespace (R1, `ConstructorObjects.fs`).
- **R9 — Reached aliases are written by their own names.** A namespace alias a signature reads
  is a declared export once harvested, so readers use `Ns.Kind`/`JSX.Element` instead of a
  position-minted `G.K`/`JSXElement`. Primitive aliases sharing one checker type abbreviate each
  other (`type B = A`, `lab-prim/`), the same as top-level aliases today. Task 5 counts the
  chains Node gains.
- **R10 — A namespace member spelled like a DU case of its merged type nests anyway.**
  `type Signal = Signal.Start | Signal.Stop` with interfaces `Signal.Start`/`Signal.Stop` declares
  DU `Signal` (cases `Start`, `Stop`) and `module Signal = type Start; type Stop`. Type positions
  resolve `Signal.Start` to the interface. In expressions, `Signal.Start` is the case (the #158
  spike's FS0039/FS1127), and construction goes through the case or `Signal.CreateStart`. That is
  the faithful reading: TypeScript's `Signal.Start` is a type, never a value. Pinned by the compile
  gate in Task 3.
- **R11 — `drop-orphan-delegates` prunes by provenance, recorded when a name is claimed**
  (`h-alias-application/findings.md` §4 option 2). Today `Orphans.mintedNames`
  (`Orphans.fs:166-174`) re-derives "minted" from `Spec.isAnonymousArm` (`Spec.fs:329-332`), a
  property of the type rather than of how its name arose. Two consequences:
  - It misses position-path copies that carry an alias symbol (`F1.W2`, `F1.W2.Value` stay
    declared and unread).
  - Any shape predicate wide enough to catch those also catches author-written aliases. H §2
    shows the ledger's `:94` candidate testing `F2.Box<T>` as minted. Once this plan nests
    harvested namespace members under arm roots (`FW.Box<T>` under an exclusive `FW`), that would
    be a false drop of source API.

  `nameAnonymous` instead records `MintedNames`: every name it claims from a **derived
  position path**, i.e. a member or argument path, a union-arm walk, or a case module. It never
  records a name claimed at an export root, nor a namespace-member root (this plan's export roots
  under `Spec.namespacePath`). `Orphans.prune` reads `model.MintedNames` in place of
  `mintedNames`. A name the author wrote can never be in the set, whatever its type looks like.

  The `retained` export guard (`Orphans.fs:222-230`) stays as a second line. `taggedArmNames`
  (`:113-148`) keeps `isAnonymousArm`: it selects tagged-arm *interfaces* for `DD002`, and a
  harvested namespace member is named, so never one of them.

  Writing alias applications as applications (H option 1, `Wrap<F1.W.Value>`) is the fidelity fix
  for the copies themselves. It is H's own work item and not this plan's; R11 only makes their
  residue prunable and the pruning sound. *Cost if wrong:* a derived claim missed by the
  recording leaves an unread copy declared, the same as today.

## Review Focus

1. **A namespace member a signature reads today** (`Shapes.Reached`, `Pool.Conn`,
   `Markup.Element`) is declared once, nested, and its readers write the nested name
   (`widen (r: Shapes.Reached)`). Its old flat declaration is gone. Pinned in Task 3, "a
   namespace member a signature reads is declared once, nested".
2. **A namespace merged with a class** (`Queue` + `namespace Queue { Item; Unread }`):
   `module Queue` holds `Item` and `Unread`, and the class keeps `type Queue` and its
   constructor. `push` reads `Queue.Item`. Pinned in Task 3, "a class merged with a namespace…".
3. **`export import A = B.C` inside a namespace** (`Pool.Depth = Inner.Deep`): `Pool.Inner.Deep`
   is declared once, and `Pool.Depth` is `type Depth = Pool.Inner.Deep` inside `module Pool`
   (`Aliases.fs:98-176`). Pinned in Task 3, first case.
4. **`declare global { namespace X }`** from a module package (`NestedNameLabGlobal.Unreached`) is
   declared under `Globals.NestedNameLabGlobal` (R3). A global library's type-only namespaces
   (`TailStream`, `Rpc`, `Cloudflare`, `NodeJS`) are declared under their own modules. A
   *foreign* namespace the package only adds to (`declare global { namespace NodeJS { interface
   PkgAdded {} } }` over a dependency's `NodeJS`) is not re-homed: `NodeJS` stays its owner's,
   and the addition is plan 3's extension. Pinned in Task 3 (`nested-name-lab`,
   `foreign-namespace-lab`) and by the census in Tasks 4–5.
5. **Two namespaces of one name at one owner path.** solid-js declares `JSX` in two files, and the
   value sides read `JSX` and `JSX2`. The second namespace's members must nest under the module
   its value side names (`JSX2.Element`), not as per-member suffixes (`JSX.Element2`). Task 4
   Step 3 checks this. A per-member suffix there is handed back with a two-file reproducer.

## File map

| File | Change | Task |
|---|---|---|
| `src/Xantham.Generator/Shape/Coverage.fs` | a namespace is represented through its members | 2 |
| `tests/Xantham.Generator.Tests/Shape.test.fs` | `audit-coverage` cases after `:5375-5390`; nesting cases `:1703-1840` updated, two added | 2, 3 |
| `src/Xantham.Generator/Harvest.fs` | `harvest-globals` harvests type-only global namespaces; `harvestNamespaceMembers`; `passes`; header and `:410-413` comments | 3 |
| `src/Xantham.Generator/Shape/Spec.fs` | `namespacePath`, `underNamespace` after `nestUnder` (`:50-56`) | 3 |
| `src/Xantham.Generator/Shape/ExportNames.fs` | unconditional nesting; `contested` and its SY004 removed; doc `:52-63` | 3 |
| `src/Xantham.Generator/Shape/Anonymous.fs` | `namespaceOf` via `namespacePath`; unconditional nesting; SY004 only for position paths; export root | 3 |
| `src/Xantham.Generator/Shape/Aliases.fs` | abbreviation names nest under the namespace | 3 |
| `src/Xantham.Generator/Shape/ConstructorObjects.fs` | constructor-object names nest under the namespace | 3 |
| `src/Xantham.Generator/Model.fs` | `HarvestModel.Namespaces` doc (`:1150-1153`); `ShapeModel.MintedNames` (after `AliasApplications`, `:1857`) | 3 |
| `src/Xantham.Generator/Pipeline.fs:96`, `tests/Xantham.Generator.Tests/Build.fs:111` | `MintedNames = Set.empty` in the two `ShapeModel` literals | 3 |
| `src/Xantham.Generator/Shape/Orphans.fs` | `prune` reads `model.MintedNames`; `mintedNames` (`:166-174`) deleted | 3 |
| `tests/fixtures/exclusive-arms-lab/index.d.ts` | `Wrap`/`F1` residue and the `FW` false-drop guard (H §5) | 3 |
| `tests/fixtures/foreign-namespace-lab/**` (new, with `node_modules/ns-owner-lab/`) | the foreign-namespace seed pin | 3 |
| `src/Xantham.Generator/CatalogCompatibility.fs:33`, `tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs:38-52` | `ApiVersion` 9 + pin | 3 |
| `tests/fixtures/nested-name-lab/index.d.ts` | namespace-member section | 3 |
| `tests/Xantham.Generator.Tests/Pipeline.test.fs` | six cases in the `nested-name-lab` block (`:3155-3248`); companion pins in `exclusive-arms-lab` / `value-side-lab` blocks | 3 |
| `tests/Xantham.Generator.Tests/golden/**` | regenerated | 3, 4 |
| `docs/.ai/plans/generator-architecture.md`, `generator-type-mapping.md`, `docs/.ai/footguns.md` | Tier 1 invariant, phase record, §4.13/§4.14 notes, footgun | 4 |
| `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**` (incl. `README.md`) | regenerated; migration notes | 5 |

---

### Task 1: Measure the shipped libraries, the fixtures and the baseline

Commits nothing. Re-runs the planning census so the executor's own numbers fill Task 5's rename
table, and records the findings baseline that Task 4 diffs against.

**Files:**
- Create (scratch): `tests/.scratch/namespace-member-harvest/census-flat.cjs`, `tests/.scratch/namespace-member-harvest/census-flat-fixtures.cjs`
- Read: `tools/fable-core-ts-input/xantham.json` (`lib: ["esnext","dom"]`, `typescript/lib: ship`), `src/Xantham.Fable.Node/xantham.json`, `build.fsx:513-563` (`generate compiler-lib` runs the CLI over `tools/fable-core-ts-input` into `src/Xantham.Fable.Core.TS`; `generate node-lib` generates the root checkout's `node_modules/@types/node` with `--config src/Xantham.Fable.Node/xantham.json`)

**Interfaces:**
- Produces: `tests/.scratch/namespace-member-harvest/{findings-before,census-libs-before,census-fixtures-before}.txt`, read by Tasks 4 and 5.

- [ ] **Step 1: Write the library census**

`tests/.scratch/namespace-member-harvest/census-flat.cjs` (on Linux, `typescript-linux-x64`):

```js
// Census of exported type members of namespaces the shipped libraries declare, classified as
// absent, declared flat (no `<Ns>.<Name>` qualified reference anywhere in the binding), or nested.
// usage: node census-flat.cjs <repoRoot> <path-to-typescript.js (JS API, 5.x/6.x)>
const path = require('path'), fs = require('fs');
const [,, root, tsPath] = process.argv;
const ts = require(tsPath);
const ts7lib = path.join(root, 'node_modules', '@typescript', 'typescript-win32-x64', 'lib');
const esc = s => s.replace(/[$]/g, '\\$');
const present = (text, name) => new RegExp('\\btype\\s+(``)?' + esc(name) + '\\d*\\b').test(text);
const nestedIn = (text, qual, name) => {
  const leaf = qual.split('.').pop().replace(/^"[^"]*":/, '');
  const L = leaf[0].toUpperCase() + leaf.slice(1);
  return new RegExp('\\b' + esc(L) + '\\.' + esc(name) + '\\b').test(text);
};
const kindOf = m => ts.SymbolFlags[m.flags & (ts.SymbolFlags.Interface|ts.SymbolFlags.TypeAlias|ts.SymbolFlags.Class|ts.SymbolFlags.Enum)] || String(m.flags);
function census(label, rootNames, options, isOurs, golden, ambientModules) {
  const host = ts.createCompilerHost(options);
  host.getDefaultLibLocation = () => ts7lib;
  host.getDefaultLibFileName = o => path.join(ts7lib, ts.getDefaultLibFileName(o));
  const prog = ts.createProgram(rootNames, options, host);
  const ck = prog.getTypeChecker();
  const sf = prog.getSourceFile(rootNames[0]);
  const resolve = s => s.flags & ts.SymbolFlags.Alias ? ck.getAliasedSymbol(s) : s;
  const ours = s => (s.declarations || []).some(d => isOurs(path.resolve(d.getSourceFile().fileName)));
  const seen = new Set(); const rows = [];
  const visit = (ns, qual) => {
    if (seen.has(ns)) return; seen.add(ns);
    for (const m0 of ck.getExportsOfModule(ns)) {
      const m = resolve(m0); const q = qual + '.' + m0.name;
      if (m.flags & ts.SymbolFlags.Type && !(m.flags & ts.SymbolFlags.TypeParameter))
        rows.push({ q, kind: kindOf(m), present: present(golden, m0.name), nested: nestedIn(golden, qual, m0.name) });
      if (m.flags & (ts.SymbolFlags.ValueModule|ts.SymbolFlags.NamespaceModule)) visit(m, q);
    }
  };
  const scope = ck.getSymbolsInScope(sf, ts.SymbolFlags.Module).filter(ours);
  for (const s of scope) {
    if (s.name.startsWith('"')) {
      if (!ambientModules) continue;
      for (const e0 of ck.getExportsOfModule(s)) {
        const e = resolve(e0);
        if (e.flags & (ts.SymbolFlags.ValueModule|ts.SymbolFlags.NamespaceModule) && !e.name.startsWith('"')) visit(e, s.name + ':' + e0.name);
      }
    } else visit(s, s.name);
  }
  const heads = rs => { const h = {}; for (const r of rs) { const k = r.q.split('.').slice(0, -1).join('.'); h[k] = (h[k] || 0) + 1; } return h; };
  const absent = rows.filter(r => !r.present), flat = rows.filter(r => r.present && !r.nested), nested = rows.filter(r => r.present && r.nested);
  console.log(`${label}: total ${rows.length}, absent ${absent.length}, declared flat ${flat.length}, nested ${nested.length}`);
  console.log(`  flat by namespace ${JSON.stringify(heads(flat))}`);
  console.log(`  absent by namespace ${JSON.stringify(heads(absent))}`);
  console.log(`  nested by namespace ${JSON.stringify(heads(nested))}`);
  for (const r of flat) console.log(`    flat ${r.q} (${r.kind})`);
}
{
  const dummy = path.join(root, 'tools', 'fable-core-ts-input', 'entry.d.ts');
  const golden = fs.readFileSync(path.join(root, 'src', 'Xantham.Fable.Core.TS', 'Fable.Core.TS.fs'), 'utf8');
  census('Core.TS (lib esnext+dom)', [dummy], { noEmit: true, lib: ['lib.esnext.d.ts', 'lib.dom.d.ts'], types: [] },
    f => f.startsWith(path.resolve(ts7lib)), golden, false);
}
{
  const dir = path.join(root, 'node_modules', '@types', 'node');
  const golden = fs.readFileSync(path.join(root, 'src', 'Xantham.Fable.Node', 'Node.fs'), 'utf8');
  census('Node (@types/node)', [path.join(dir, 'index.d.ts')], { noEmit: true, lib: ['lib.esnext.d.ts'], types: [] },
    f => f.startsWith(path.resolve(dir)), golden, true);
}
```

- [ ] **Step 2: Write the fixture census**

`tests/.scratch/namespace-member-harvest/census-flat-fixtures.cjs`:

```js
// Fixture-golden census: namespace type members absent / declared flat / nested.
// usage: node census-flat-fixtures.cjs <repoRoot> <path-to-typescript.js (JS API, 5.x/6.x)>
const path = require('path'), fs = require('fs');
const [,, root, tsPath] = process.argv;
const ts = require(tsPath);
const fx = path.join(root, 'tests', 'fixtures'), gd = path.join(root, 'tests', 'Xantham.Generator.Tests', 'golden');
const esc = s => s.replace(/[$]/g, '\\$');
const present = (text, name) => new RegExp('\\btype\\s+(``)?' + esc(name) + '\\d*\\b').test(text);
const nestedIn = (text, qual, name) => {
  const leaf = qual.split('.').pop();
  const L = leaf[0].toUpperCase() + leaf.slice(1);
  return new RegExp('\\b' + esc(L) + '\\.' + esc(name) + '\\b').test(text);
};
const gold = n => { const out = []; const walk = d => { for (const e of fs.readdirSync(d, {withFileTypes:true})) { const p = path.join(d, e.name); e.isDirectory() ? walk(p) : p.endsWith('.fs') && out.push(fs.readFileSync(p,'utf8')); } }; walk(n); return out.join('\n'); };
const pkgDir = name => {
  if (name === '@cloudflare') return path.join(fx, '@cloudflare','workers-types','node_modules','@cloudflare','workers-types');
  const nm = path.join(fx, name, 'node_modules', name);
  return fs.existsSync(path.join(nm,'package.json')) ? nm : path.join(fx, name);
};
for (const name of fs.readdirSync(gd)) {
  const dir = pkgDir(name); const pj = path.join(dir,'package.json');
  if (!fs.existsSync(pj)) continue;
  const p = JSON.parse(fs.readFileSync(pj,'utf8'));
  let entry = path.resolve(dir, p.types || p.typings || 'index.d.ts'); if (!fs.existsSync(entry)) continue;
  const golden = gold(path.join(gd, name));
  const prog = ts.createProgram([entry], { noEmit: true, skipLibCheck: true, types: [], lib: ['lib.esnext.d.ts'], moduleResolution: ts.ModuleResolutionKind.Bundler, module: ts.ModuleKind.ESNext });
  const ck = prog.getTypeChecker(); const sf = prog.getSourceFile(entry);
  const ours = s => (s.declarations||[]).some(d => path.resolve(d.getSourceFile().fileName).startsWith(path.resolve(dir)));
  const resolve = s => s.flags & ts.SymbolFlags.Alias ? ck.getAliasedSymbol(s) : s;
  const ms = ck.getSymbolAtLocation(sf);
  const tops = ms ? ck.getExportsOfModule(ms).map(resolve) : ck.getSymbolsInScope(sf, ts.SymbolFlags.Module|ts.SymbolFlags.Type|ts.SymbolFlags.Value).filter(ours);
  const seen = new Set(); const rows = [];
  const visit = (ns, qual) => {
    if (seen.has(ns)) return; seen.add(ns);
    for (const m0 of ck.getExportsOfModule(ns)) {
      const m = resolve(m0);
      if (m.flags & ts.SymbolFlags.Type && !(m.flags & ts.SymbolFlags.TypeParameter))
        rows.push({ q: qual + '.' + m0.name, present: present(golden, m0.name), nested: nestedIn(golden, qual, m0.name) });
      if (m.flags & (ts.SymbolFlags.ValueModule|ts.SymbolFlags.NamespaceModule)) visit(m, qual + '.' + m0.name);
    }
  };
  for (const t of tops) if (t.flags & (ts.SymbolFlags.ValueModule|ts.SymbolFlags.NamespaceModule) && !t.name.startsWith('"') && ours(t)) visit(t, t.name);
  if (!rows.length) continue;
  const absent = rows.filter(r => !r.present).length, flat = rows.filter(r => r.present && !r.nested).length, nested = rows.filter(r => r.present && r.nested).length;
  console.log(`${name}: total ${rows.length}, absent ${absent}, declared flat ${flat}, nested ${nested}`);
}
```

- [ ] **Step 3: Run both censuses and the findings baseline**

```
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
node tests/.scratch/namespace-member-harvest/census-flat.cjs "$PWD" "$TSJS" > tests/.scratch/namespace-member-harvest/census-libs-before.txt
node tests/.scratch/namespace-member-harvest/census-flat-fixtures.cjs "$PWD" "$TSJS" > tests/.scratch/namespace-member-harvest/census-fixtures-before.txt
dotnet fsi build.fsx -- findings > tests/.scratch/namespace-member-harvest/findings-before.txt
```

The `typescript.js` is the fixture's TypeScript 6.0.3 JS API; the root `typescript` 7.x ships no
JS `createProgram`. Expected first lines:
- `Core.TS (lib esnext+dom): total 169, absent 28, declared flat 124, nested 17`
- `Node (@types/node): total 391, absent 63, declared flat 153, nested 175`
- the fixture lines in the Measures table.

If the counts differ, write the new numbers into the Measures table in Task 4's commit.

---

### Task 2: `audit-coverage` represents a namespace through its members

**Files:**
- Modify: `src/Xantham.Generator/Shape/Coverage.fs:44-78`
- Test: `tests/Xantham.Generator.Tests/Shape.test.fs` (after `"audit-coverage reports an export nothing represented"`, `:5375-5390`; the enclosing list is `testList "shape passes"`, `:1511`)

**Interfaces:**
- Consumes: `Build.symbol`, `Build.export`, `Build.shapeModel`, `Build.runPass` (`tests/Xantham.Generator.Tests/Build.fs`); `SymbolResponse.Parent: int voption`, read by `ParentSymbolId` (`Measures.fs:133`).
- Produces: the audit contract Task 3 relies on. A namespace export is represented when a harvested member it parents is represented. A namespace whose harvested members are all unrepresented reports `AC001` beside them. An empty namespace stays excused.

- [ ] **Step 1: Write the failing tests**

After `"audit-coverage reports an export nothing represented"`:

```fsharp
        testCase "audit-coverage reads a namespace as represented by a nested member" <| fun _ ->
            let shapes = Build.symbol 310 "Shapes" SymbolFlags.NamespaceModule
            let lonely = { Build.symbol 311 "Lonely" SymbolFlags.Interface with Parent = ValueSome 310 }

            let declared =
                FsInterface
                    { Name = "Globals.Shapes.Lonely"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members = []
                      Entrypoint = None
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel [] with
                    Decls = [ declared ]
                    Harvest =
                        { HarvestModel.Empty with
                            Exports =
                                [ Build.export "Shapes" shapes
                                  { Build.export "Lonely" lonely with HasValueExport = false } ] } }

            let _, findings = Build.runPass Coverage.auditCoverage model
            Expect.isEmpty findings "the namespace's module is its member's path"

        testCase "audit-coverage reports a namespace whose members are all missing beside them" <| fun _ ->
            let shapes = Build.symbol 310 "Shapes" SymbolFlags.NamespaceModule
            let lonely = { Build.symbol 311 "Lonely" SymbolFlags.Interface with Parent = ValueSome 310 }

            let model =
                { Build.shapeModel [] with
                    Harvest =
                        { HarvestModel.Empty with
                            Exports =
                                [ Build.export "Shapes" shapes
                                  { Build.export "Lonely" lonely with HasValueExport = false } ] } }

            let _, findings = Build.runPass Coverage.auditCoverage model

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Escape, "Shapes"; Escape, "Lonely" ]
                "the module is missing as much as its member"

        testCase "audit-coverage excuses a namespace with no harvested member" <| fun _ ->
            let unwrappable = Build.symbol 320 "Unwrappable" SymbolFlags.NamespaceModule

            let model =
                { Build.shapeModel [] with
                    Harvest = { HarvestModel.Empty with Exports = [ Build.export "Unwrappable" unwrappable ] } }

            let _, findings = Build.runPass Coverage.auditCoverage model
            Expect.isEmpty findings "every member omits `export`, so there is nothing to represent"
```

If the `FsInterface` record has fields beyond those listed, copy the field list from the
`"repair-arity demotes a settable property…"` case directly above (`Shape.test.fs:5340-5363`).

- [ ] **Step 2: Run to verify the first fails**

Run: `dotnet fsi build.fsx -- test --quick --filter "shape passes"`
Expected: FAIL in the first case. Actual `[(Escape, "Shapes")]`: `Globals.Shapes.Lonely` neither
equals, starts with `Shapes.` nor ends with `.Shapes` (`Coverage.fs:48-55`). The other two pass.

- [ ] **Step 3: Implement**

In `Coverage.fs`, replace `represented` (`:48-55`) with a recursive form that also accepts a
represented member, and keep `opaqueNamespace` (`:57-71`) unchanged:

```fsharp
                    // The harvested members of each namespace export, by the namespace's symbol id.
                    let membersOf =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            export.Symbol.ParentSymbolId
                            |> ValueOption.toOption
                            |> Option.map (fun parent -> parent, export))
                        |> List.groupBy fst
                        |> List.map (fun (parent, members) -> parent, List.map snd members)
                        |> Map.ofList

                    // An export is represented by a declaration carrying its name, by a declaration
                    // nested under a module of its name, by a declaration whose final segment is its
                    // name, by the name `name-exports` assigned it, or - for a namespace - by any
                    // represented member, wherever its module path sits (`Globals.TailStream.X`).
                    let rec represented (export: HarvestedExport) =
                        let exported = name export

                        Set.contains exported generated
                        || Set.contains (qualifiedName export) generated
                        || generated
                           |> Set.exists (fun declared ->
                               declared.StartsWith(exported + ".") || declared.EndsWith("." + exported))
                        || (hasAny SymbolFlags.Module export.Symbol.Flags
                            && Map.tryFind export.Symbol.SymbolId membersOf
                               |> Option.exists (List.exists represented))
```

A member's parent is a different symbol from the member, and `ParentSymbolId` forms a tree, so the
recursion terminates.

- [ ] **Step 4: Run to verify all three pass**

Run: `dotnet fsi build.fsx -- test --quick --filter "shape passes"` → PASS.

- [ ] **Step 5: Prove no golden moves, then gate**

Run `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`, then
`rtk git status tests/Xantham.Generator.Tests/golden`.
Expected: no golden changed, since no real run harvests a namespace member yet. Any change is
handed back with the fixture name.
Run: `dotnet fsi build.fsx -- test` → PASS.

- [ ] **Step 6: Commit**

```
rtk git add src/Xantham.Generator/Shape/Coverage.fs tests/Xantham.Generator.Tests/Shape.test.fs
rtk git commit -m "fix(generator): a namespace is represented through its members" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Harvest namespace members and nest them, lab first

Ends with the lab and unit tests green and the working tree **uncommitted** (see Stages). Task 4
commits.

**Files:**
- Modify: `tests/fixtures/nested-name-lab/index.d.ts` (append)
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`nested-name-lab` block `:3155-3248`; companion pins, Step 10)
- Modify: `tests/Xantham.Generator.Tests/Shape.test.fs:1703-1840`
- Modify: `src/Xantham.Generator/Harvest.fs` (header `:1-3`; `harvestGlobals` `:410-466`; new pass above `harvestDocs` `:513`; `passes` `:603-610`)
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (after `nestUnder`, `:50-56`)
- Modify: `src/Xantham.Generator/Shape/ExportNames.fs:52-183`
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs:331-334`, `:352-389`, `:764`
- Modify: `src/Xantham.Generator/Shape/Aliases.fs:118-122`
- Modify: `src/Xantham.Generator/Shape/ConstructorObjects.fs:16-27`, `:66-76`, `:143`
- Modify: `src/Xantham.Generator/Model.fs:1150-1153` (`HarvestModel.Namespaces` doc), `:1856-1857` (new `ShapeModel.MintedNames`)
- Modify: `src/Xantham.Generator/Pipeline.fs:96`, `tests/Xantham.Generator.Tests/Build.fs:111` (`MintedNames = Set.empty`)
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs:296-303` (mutable state), `:580-606` (claim sites), `:762-777` (roots, result)
- Modify: `src/Xantham.Generator/Shape/Orphans.fs:166-174`, `:272`
- Modify: `tests/fixtures/exclusive-arms-lab/index.d.ts` (append), its `Pipeline.test.fs` block (`:5298`)
- Create: `tests/fixtures/foreign-namespace-lab/{package.json,index.d.ts}`, `tests/fixtures/foreign-namespace-lab/node_modules/ns-owner-lab/{package.json,index.d.ts}`, and its `fixtureTests` block in `Pipeline.test.fs`
- Modify: `src/Xantham.Generator/CatalogCompatibility.fs:33`, `tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs:38-52`

**Interfaces:**
- Consumes: Task 2's audit contract; `followAlias` (`Harvest.fs:51-59`); `Grouping.classify`, `Grouping.declOrder` (`Model.fs:1935+`); `GeneratorConfig.disposition`; `ctx.Session.getExportsOfModule : int -> Async<SymbolResponse[] voption>`; `ctx.Session.getSymbolsInScope`.
- Produces:
  - `Harvest.harvestNamespaceMembers: Pass<HarvestModel>`, `Name = "harvest-namespace-members"`, after `harvest-globals`.
  - `HarvestModel.Namespaces`: every walked namespace, dotted for nested ones (`"Pool.Inner"`).
  - `Spec.namespacePath: Map<int<symbolId>, string<symbolName>> -> int<symbolId> option -> string option` and `Spec.underNamespace: string option -> string -> string`.
  - Per member, a `HarvestedExport` with `HasValueExport = false` and the walked namespace's `Origin`.
  - `ShapeModel.MintedNames: Set<string>`, written by `synthesize-anonymous` and read by `drop-orphan-delegates`.
  - `CatalogCompatibility.current(...).ApiVersion = 9`.

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/nested-name-lab/index.d.ts`:

```ts

// ---------------------------------------------------------------------------
// An exported namespace's members are API whether or not a signature reaches
// them. Each is declared once, under its namespace, as TypeScript spells it.
// ---------------------------------------------------------------------------

/** A namespace with a value beside its types. */
export namespace Pool {
    /** Declared though no signature reads it. */
    export interface Idle { since: number }
    /** A class member: the namespace object reads its constructor. */
    export class Conn { close(): void }
    export function size(): number;
    /** A namespace inside a namespace. */
    export namespace Inner {
        /** Declared two namespaces deep. */
        export interface Deep { depth: number }
        /** Shares a name with the exported `Node` and with `Shapes.Node`. */
        export interface Node { inner: boolean }
    }
    /** A second name for a nested member: abbreviates the first. */
    export import Depth = Inner.Deep;
}

/** A namespace of types alone. */
export namespace Shapes {
    export interface Lonely { d: number }
    /** Read by `widen` as well as harvested: declared once. */
    export interface Reached { r: string }
    export interface Node { shape: string }
    /** Named like the value container, which keeps its name. */
    export interface Exports { e: number }
}

/** Reads a namespace member that is harvested too. */
export declare function widen(r: Shapes.Reached): void;

/** An alias merged with a namespace. */
export type Pick = { kind: "a" } | { kind: "b" };
export namespace Pick {
    export interface Extra { e: number }
}

/** A tagged union whose arms are its namespace's members. */
export type Signal = Signal.Start | Signal.Stop;
export namespace Signal {
    export interface Start { kind: "start"; at: number }
    export interface Stop { kind: "stop" }
}
export declare function send(signal: Signal): void;

/** Interfaces of one namespace that refer to each other, read by a signature and not. */
export namespace Markup {
    export interface Element { children: Element[]; attributes: Attributes }
    export interface Attributes { parent?: Element; ref?: (element: Element) => void }
    export interface Fragment { items: Fragment[]; slot?: Slot }
    export interface Slot { fill: Fragment }
}

/** Reads the DOM `Element` beside a namespace member of that name. */
export declare function mount(host: Element): Markup.Element;

/** A class merged with a namespace of types. */
export declare class Queue {
    push(item: Queue.Item): void;
}
export declare namespace Queue {
    interface Item { id: string }
    interface Unread { pending: number }
}

/** A global namespace this module package contributes. */
declare global {
    namespace NestedNameLabGlobal {
        interface Unreached { u: number }
    }
}
```

`index.js` is unchanged. The run gate calls only `backoffOf`, `defaultRetry` and `configure`
(`tests/Xantham.Generator.RunGate/Program.fs:273-307`), and `joinCluster` is already declared
without a runtime export.

- [ ] **Step 2: Write the failing lab tests**

In the `nested-name-lab` block, after `"a namespace separates two declarations of one name"`,
add the following cases. Move the block's closing `])` onto the last one.

```fsharp
                      testCase "an exported namespace's members are declared under it whether or not a signature reads them"
                      <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          let declaredIn (owner: string) (name: string) =
                              let body = source.Substring(source.IndexOf($"\nmodule {owner} ="))

                              body.Contains($"\n    type {name} =") || body.Contains($"\n    and {name} =")

                          for owner, name in
                              [ "Pool", "Idle"; "Shapes", "Lonely"; "Pick", "Extra"; "Markup", "Fragment"
                                "Markup", "Slot"; "Queue", "Unread" ] do
                              Expect.isTrue (declaredIn owner name) $"{owner}.{name} is declared inside module {owner}"

                          Expect.stringContains source "    module Inner =" "a nested namespace opens a module inside its parent's"
                          Expect.stringContains source "        type Deep =" "and declares its members there"
                          Expect.stringContains source "    type Depth = Pool.Inner.Deep\n" "a second name abbreviates the first, in its own namespace"

                          Expect.isEmpty
                              (rendered.Findings |> List.filter (fun f -> f.Key = "AC001") |> List.map _.Symbol)
                              "nothing is reported missing"

                      testCase "a namespace member nests without a finding" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          let namespaceNests =
                              [ "Pool.Idle"; "Pool.Inner.Deep"; "Pool.Inner.Node"; "Shapes.Node"; "Shapes.Exports"; "Cluster.Node" ]

                          for name in namespaceNests do
                              Expect.isFalse
                                  (rendered.Findings |> List.exists (fun f -> f.Symbol = name && f.Key = "SY004"))
                                  $"{name} is the source's own spelling, not a nesting the generator chose"

                          Expect.stringContains source "[<Erase>]\ntype Exports =" "the value container keeps its name"
                          Expect.isFalse (source.Contains "Exports_") "and takes no digest"
                          Expect.isFalse (source.Contains "Node2") "no Node takes a numeric suffix"

                      testCase "a namespace member a signature reads is declared once, nested" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          let count (text: string) = source.Split(text).Length - 1

                          for name in [ "Reached"; "Conn"; "Element"; "Attributes"; "Item" ] do
                              Expect.equal
                                  (count $"\n    type {name} =" + count $"\n    and {name} =")
                                  1
                                  $"{name} has one declaration, inside its namespace's module"

                              Expect.isFalse
                                  (source.Contains($"\ntype {name} =") || source.Contains($"\nand {name} ="))
                                  $"and none at the root"

                          Expect.stringContains source "static member widen (r: Shapes.Reached) : unit" "its reader writes the nested name"

                          Expect.stringContains
                              source
                              "static member mount (host: Fable.Core.TS.Dom.Element) : Markup.Element"
                              "the DOM name stays qualified beside the member of that name"

                          Expect.stringContains source "Pool.ConnConstructor" "a class member's constructor nests with it"
                          Expect.isFalse (source.Contains "static member Conn:") "and binds through its namespace object"
                          Expect.stringContains source "static member Pool: Pool" "the namespace's value side keeps its binding"

                      testCase "a class merged with a namespace declares the namespace's members under it" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains source "\ntype Queue =" "the class keeps its instance side"
                          Expect.stringContains source "\nmodule Queue =" "and its namespace opens a module beside it"
                          Expect.stringContains source "Queue.Item" "the class's member reads the nested name"
                          Expect.isFalse (source.Contains "static member Unread") "a type member binds nothing"

                      testCase "a tagged union's namespace arms nest beside its cases" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains source "TypeScriptTaggedUnion(\"kind\"" "the union is a DU"
                          Expect.stringContains source "\nmodule Signal =" "its namespace opens a module"
                          Expect.stringContains source "    type Start =" "holding the arm interfaces"

                      testCase "a module package's global namespace is declared under Globals" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains source "module NestedNameLabGlobal =" "the global namespace opens its module"
                          Expect.stringContains source "type Unreached =" "and declares its member"

                          Expect.isEmpty
                              (rendered.Findings |> List.filter (fun f -> f.Symbol.Contains "Unreached"))
                              "with nothing reported" ])
```

The existing `"two owners of one member name…"`, `"every nested name is reported under SY004"`
and `"a namespace separates two declarations of one name"` cases stay unchanged: their names are
position-derived or already nested (`Cluster.Node`).

- [ ] **Step 2b: The foreign-namespace pin and the pruning guards**

**Foreign namespace (R3).** Create `tests/fixtures/foreign-namespace-lab/`, laid out like
`nested-dep-lab`. Its dependency lives under the lab's own `node_modules/`, and the `*lab` name
is what `.gitignore` un-ignores.

`tests/fixtures/foreign-namespace-lab/package.json`:

```json
{"name":"foreign-namespace-lab","type":"module","types":"index.d.ts","description":"Hand-authored fixture: a module package's `declare global` namespaces. A namespace first declared by the package is its own and is declared under Globals; a namespace a dependency declares first stays its owner's, and the package's additions to it are global-augmentation extensions (docs/.ai/plans/2026-10-15-global-augmentation.md). Tracked in git, no npm install."}
```

`tests/fixtures/foreign-namespace-lab/node_modules/ns-owner-lab/package.json`:

```json
{"name":"ns-owner-lab","types":"index.d.ts"}
```

`tests/fixtures/foreign-namespace-lab/node_modules/ns-owner-lab/index.d.ts`:

```ts
// The owner of `NodeJS`: a global script, as `@types/node` is.
declare namespace NodeJS {
    interface ProcessEnv { [key: string]: string | undefined }
}
```

`tests/fixtures/foreign-namespace-lab/index.d.ts`:

```ts
/// <reference types="ns-owner-lab" />

declare global {
    /** A dependency declares `NodeJS` first; this package only adds to it. */
    namespace NodeJS {
        interface PkgAdded { y: number }
    }
    /** This package declares `PkgNs` first: it is the package's own. */
    namespace PkgNs {
        interface Inner { z: number }
    }
}

export declare function env(): NodeJS.ProcessEnv;
```

If the `reference types` directive does not resolve the dependency (`NodeJS.ProcessEnv` widens
with a TR finding), replace it with `import "ns-owner-lab";` and record which form resolved in
the lab's description.

In `Pipeline.test.fs`, register it after the `nested-name-lab` block:

```fsharp
        // A module package's global namespaces: its own are declared, a dependency's are not
        // re-homed (the package's additions to them are global-augmentation extensions).
        yield!
            fixtureTests
                "foreign-namespace-lab"
                (handFixture "foreign-namespace-lab")
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "a namespace a dependency declares first stays its owner's" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.isFalse (source.Contains "module NodeJS =") "the dependency's namespace is not re-homed"
                          Expect.isFalse (source.Contains "type ProcessEnv =") "nor its member copied as the package's"
                          Expect.isFalse (source.Contains "type PkgAdded =") "the addition is an extension of the owner, not a new namespace"

                          Expect.isFalse
                              (rendered.Findings |> List.exists (fun f -> f.Symbol.StartsWith "NodeJS" && f.Key = "AC001"))
                              "and the owner's namespace is not a harvested export to miss"

                      testCase "a namespace the package declares first is its own" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains (moduleBodyOf source "PkgNs") "type Inner =" "declared in its own module" ])
```

`PkgAdded` is absent here by design. Plan 3 writes it as an extension of the owner's `NodeJS`,
and replaces the third assertion in that plan's commit.

**Pruning guards (R11, H §5).** Append to `tests/fixtures/exclusive-arms-lab/index.d.ts`:

```ts

// A generic object alias applied inside both arms of an exclusive fold. Each arm's application
// is copied by position (`F1.W`, `F1.W2`) until alias applications are written as applications;
// the copy no declaration reads is dropped by provenance, the one read is kept.
export type Wrap<T> = { value: T };
export type F1 =
    | { w: Wrap<{ n: number }>; a: number; b?: never }
    | { w: Wrap<{ n: number }>; b: string; a?: never };

// The false-drop guard: an exclusive union merged with a namespace whose members are author-written
// aliases. `FW.Box<T>` and `FW.Applied` sit under the fold's root `FW.` and nothing reads them;
// they are source API, never pruned.
export type FW = { left: number; right?: never } | { right: string; left?: never };
export namespace FW {
    export type Box<T> = { boxed: T };
    export type Applied = Box<string>;
}
```

In the `exclusive-arms-lab` block's list, add:

```fsharp
                  testCase "an unread position copy under a fold is dropped by provenance" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains (moduleBodyOf source "F1") "type W =" "the copy the folded member reads stays"
                      Expect.isFalse (source.Contains "type W2") "the second arm's copy, read by nothing, is dropped"

                      Expect.isTrue
                          (rendered.Findings |> List.exists (fun f -> f.Symbol = "F1.W2" && f.Key = "DD003"))
                          "and the drop is reported"

                  testCase "a namespace member under a fold root is never pruned" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      let fw = moduleBodyOf source "FW"

                      Expect.stringContains fw "type Box<'T> =" "an author-written generic alias is declared"
                      Expect.stringContains fw "type Applied =" "and so is its application"

                      Expect.isFalse
                          (rendered.Findings |> List.exists (fun f -> f.Symbol.StartsWith "FW." && f.Key = "DD003"))
                          "no namespace member is dropped as an orphan"
```

These run RED in Step 5. `F1.W2` is kept today (it carries `AliasSymbolId = Wrap`, so
`isAnonymousArm` is false), and `FW.Box`/`FW.Applied` are absent until the harvest.

- [ ] **Step 3: Update and add the nesting unit tests**

In `Shape.test.fs`:
- `"synthesize-anonymous nests a namespaced type under the namespace, not a number"`
  (`:1703`): replace its last assertion (`Expect.contains … "SY004"`) with
  `Expect.isFalse (findings |> List.exists (fun f -> f.Key = "SY004")) "a namespace member's module is its source spelling"`.
- `"name-exports nests the namespaced claimant of a contested name"` (`:1776`): replace
  `Expect.equal (findings |> List.map _.Key) [ "SY004" ] "the nesting is reported once"` with
  `Expect.isEmpty findings "the source spelling raises nothing"`.
- Read `"name-exports suffixes the second declaration…"` (`:1813`) and
  `"synthesize-anonymous keeps the number where no namespace names the second"` (`:1741`). Their
  claimants carry no namespace, so they stay unchanged.

Add after `:1812`:

```fsharp
        testCase "name-exports nests a namespaced claimant nothing else contests" <| fun _ ->
            let lonely =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 401 "d" SymbolFlags.Property) 2 ] }

            let inNamespace = { Build.symbol 100 "Lonely" SymbolFlags.Interface with Parent = ValueSome 900 }

            let model =
                { Build.shapeModel (lonely :: Build.primitives) with
                    Harvest =
                        { HarvestModel.Empty with
                            Exports = [ { Build.export "Lonely" inNamespace with HasValueExport = false } ]
                            Namespaces =
                                Map.ofList [ 900<symbolId>, ((fun value -> value * uom<symbolName>) "Pool.Inner") ] }
                    ExportTypes = Map.ofList [ 100<symbolId>, { Declared = Some 40<typeId>; Value = None } ] }

            let named, findings = Build.runPass ExportNames.nameExports model

            Expect.equal (Map.tryFind 40<typeId> named.DeclNames) (Some "Pool.Inner.Lonely") "nested along the dotted path"
            Expect.isEmpty findings "and the source spelling raises nothing"

        testCase "synthesize-anonymous nests a reached namespaced type nothing else contests" <| fun _ ->
            let collator =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some ((fun value -> value * uom<symbolName>) "Collator")
                    SymbolParent = Some 900<symbolId>
                    Members = [ Build.resolvedMember (Build.symbol 402 "compare" SymbolFlags.Property) 2 ] }

            let holder =
                { Build.facts (Build.typeResponse 42 TypeFlags.Object) with
                    SymbolName = Some ((fun value -> value * uom<symbolName>) "Holder")
                    Members = [ Build.resolvedMember (Build.symbol 403 "collator" SymbolFlags.Property) 41 ] }

            let model =
                { Build.shapeModel (collator :: holder :: Build.primitives) with
                    Harvest =
                        { HarvestModel.Empty with
                            Exports = [ Build.export "holder" (Build.symbol 400 "holder" SymbolFlags.BlockScopedVariable) ]
                            Namespaces = Map.ofList [ 900<symbolId>, ((fun value -> value * uom<symbolName>) "Intl") ] }
                    ExportTypes = Map.ofList [ 400<symbolId>, { Declared = None; Value = Some 42<typeId> } ] }

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 41<typeId> named.DeclNames) (Some "Intl.Collator") "Core.TS's flat Collator, nested"
            Expect.isFalse (findings |> List.exists (fun f -> f.Key = "SY004")) "without a finding"
```

- [ ] **Step 4: Pin the API version**

In `CatalogCompatibility.test.fs`, change the `"the current contract pins every version"`
expectation (`:47-48`) to:

```fsharp
                (1, 6, 9, 3, 1)
                "identity 6 authenticates constructor fallback bounds and sanitized aliases; API 9 nests namespace members under their namespaces; inference 3 preserves source contracts at the width cutoff"
```

and add after it:

```fsharp
        testCase "the API contract rejects catalogues built before namespace members nested" <| fun _ ->
            rejects "API version" (fun () ->
                validateContract 2 expected.Compiler expected.Generator "profile"
                    (Some { expected.Contract with ApiVersion = 8 }))
```

- [ ] **Step 5: Run to verify they fail**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`, then
`dotnet fsi build.fsx -- test --quick --filter "shape passes"` and
`dotnet fsi build.fsx -- test --quick --filter "catalog compatibility"`.
Expected FAILs:
- every new lab case except `"a tagged union's namespace arms…"`'s first assertion
- `foreign-namespace-lab`'s `"…is its own"` (`PkgNs` absent today). Its `"…stays its owner's"` case passes today and is the pin that the seed narrowing keeps green.
- both new `exclusive-arms-lab` cases (`F1.W2` kept; `FW.Box`/`FW.Applied` absent)
- the two new unit cases, and the two edited ones (SY004 still raised)
- the version pin (8 ≠ 9)

- [ ] **Step 6: `Spec.namespacePath`**

In `Spec.fs`, directly after `nestUnder` (`:50-56`):

```fsharp
/// The F# module path of the namespace a declaration is written in, where `Namespaces` names it:
/// `Pool.Inner` for a member of `namespace Pool { namespace Inner { … } }`.
let internal namespacePath
    (namespaces: Map<int<symbolId>, string<symbolName>>)
    (parent: int<symbolId> option)
    : string option =
    parent
    |> Option.bind (fun id -> Map.tryFind id namespaces)
    |> Option.map (fun value -> (value / uom<symbolName>).Split '.' |> Array.map Naming.pascalSegment |> String.concat ".")

/// `name` inside the namespace module path, when it has one.
let internal underNamespace (path: string option) (name: string) =
    match path with
    | Some ns -> nestUnder ns name
    | None -> name
```

`Spec.fs` opens `Measure` (`:9`). If `uom` needs qualification there, read how `Spec.fs` already
strips a measure (`rtk grep -n "uom<" src/Xantham.Generator/Shape/Spec.fs`) and match it.

- [ ] **Step 7: Nest in every naming pass**

`ExportNames.fs`:
- Replace `namespaceOf` (`:80-86`) with
  `let namespaceOf (export: HarvestedExport) = namespacePath model.Harvest.Namespaces (export.Symbol.ParentSymbolId |> ValueOption.toOption)`.
- Delete `contested` (`:114-119`).
- Replace `wantedOf` (`:121-127`) with:

```fsharp
                    /// The full spelling a claimant prefers: its owner path, then its namespace's
                    /// module path, then its name.
                    let wantedOf preferred owner path =
                        path @ [ underNamespace owner preferred ] |> String.concat "."
```

- In `verbatim` (`:137-141`), drop the `|> fst`.
- In the fold (`:146-183`), make the binding `let wanted = wantedOf preferred owner path` and
  delete the `nestedUnderNamespace` `SY004` block (`:168-173`).

Rewrite the pass's doc comment (`:52-63`) so it reads:

```fsharp
/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type, `declarationExports` names it under the defining symbol across shipped groups, or
/// otherwise under the shallowest owner path (ordinal owner specifier, then harvest order,
/// breaking ties), and `shape-aliases` abbreviates the rest.
///
/// A declaration written inside a namespace is named inside that namespace's module path, as
/// TypeScript spells it: `TailStream.TailEvent`, `Pool.Inner.Deep`. Two exports of one name at
/// one path keep the numeric suffix `shape-anonymous` uses for the same clash.
```

`Anonymous.fs`:
- `namespaceOf` (`:331-334`) becomes
  `let namespaceOf (facts: TypeFacts) = namespacePath model.Harvest.Namespaces facts.SymbolParent`.
- In `claim` (`:361-364`), replace the `wanted` match with
  `let wanted = underPrefix (underNamespace owner admitted)`, and update its comment to say that a
  name written in a namespace nests under it.
- Guard the `SY004` block (`:385-387`) with `owner.IsNone &&`, so only a position-derived dot is
  reported.
- At the export root (`:764`), replace
  `let root = underPrefix (Naming.pascalSegment (fsName fallback export))` with:

```fsharp
        let root =
            underPrefix (
                underNamespace
                    (namespacePath model.Harvest.Namespaces (export.Symbol.ParentSymbolId |> ValueOption.toOption))
                    (Naming.pascalSegment (fsName fallback export))
            )
```

`Aliases.fs:118-122`: the abbreviation name becomes

```fsharp
                                let name =
                                    String.concat
                                        "."
                                        (exportPath export
                                         @ [ underNamespace
                                                 (namespacePath
                                                     model.Harvest.Namespaces
                                                     (export.Symbol.ParentSymbolId |> ValueOption.toOption))
                                                 (Naming.typeNameSegment (fsName fallback export)) ])
```

`ConstructorObjects.fs`:
- In `exportNames` (`:16-27`), store
  `underNamespace (namespacePath model.Harvest.Namespaces (export.Symbol.ParentSymbolId |> ValueOption.toOption)) (fsName fallback export)`.
- In `preferredName`'s `stem` (`:66-76`), Pascal-case each dotted segment rather than the whole
  string. When the stem comes from `facts.SymbolName`, prefix
  `namespacePath model.Harvest.Namespaces facts.SymbolParent`:

```fsharp
            let pascalPath (name: string) =
                name.Split '.' |> Array.map (Naming.pascalSegment >> Naming.typeNameSegment) |> String.concat "."

            let stem =
                Map.tryFind facts.Response.TypeId exportNames
                |> Option.map pascalPath
                |> Option.orElseWith (fun () ->
                    facts.SymbolName
                    |> Option.filter (isSyntheticName >> not)
                    |> Option.map (fun value ->
                        underNamespace
                            (namespacePath model.Harvest.Namespaces facts.SymbolParent)
                            (Naming.typeNameSegment (Naming.pascalSegment (value / uom<symbolName>)))))
                |> Option.orElseWith instanceName
                |> Option.defaultValue path
```

  `instanceName` returns a declared name, already dotted where it nests.
- At `:143`, apply the same `underNamespace (namespacePath …)` prefix to `path`.
- Replace the comment above `stem` with: "A stem is a declared F# name, dotted where it nests;
  each segment is Pascal-cased on its own."

Run `rtk grep -n "fsName" src/Xantham.Generator/Shape` and read each remaining type-naming use.
Value bindings (`Classes.fs:316`, `Exports.fs:59`, `Coverage.fs:31`, `Orphans.fs:244`) name values
and stay bare. Any other *type* name built from `fsName` gets the same prefix.

- [ ] **Step 7b: Record minted names by provenance (R11)**

`Model.fs`, in `ShapeModel` directly after `AliasApplications` (`:1856-1857`):

```fsharp
        /// The declaration names `synthesize-anonymous` claimed from a derived position path: a
        /// member, argument, union-arm or case-module position below an export. A name claimed
        /// at an export root, a namespace member's root included, is absent, so every name the
        /// author wrote is outside this set.
        MintedNames: Set<string>
```

Add `MintedNames = Set.empty` to the two `ShapeModel` literals, `Pipeline.fs:96` and
`tests/Xantham.Generator.Tests/Build.fs:111`, and to any further literal the compiler reports.

`Anonymous.fs`, `nameAnonymous`:
- Beside the other mutable state (`:296-303`), add `let mutable minted = Set.empty` and
  `let mutable exportRoots = Set.empty`.
- Compute every export's root before the walk loop (`:762-770`), using the same formula as the
  loop (Step 7's namespaced root, under that export's `modulePrefix`), and reset `modulePrefix`
  afterwards:

```fsharp
    for export in model.Harvest.Exports do
        modulePrefix <- pathOf export |> String.concat "."

        exportRoots <-
            Set.add
                (underPrefix (
                    underNamespace
                        (namespacePath model.Harvest.Namespaces (export.Symbol.ParentSymbolId |> ValueOption.toOption))
                        (Naming.pascalSegment (fsName fallback export))
                ))
                exportRoots

    modulePrefix <- ""
```

  `walk` reads `exportRoots` when it runs, so this loop sits after `walk`'s definition and before
  the walk loop. `walk` captures the mutable the same way it already captures `names` and
  `nestBases`.
- Add a helper beside `claim`:

```fsharp
    /// `claim` for a name taken from the position path it was reached at, recorded as minted
    /// unless the path is an export's own root.
    let claimDerived typeId order (path: string) =
        let unique = claim None path typeId order

        if not (Set.contains path exportRoots) then
            minted <- Set.add unique minted

        unique
```

- At `:582`, replace `claim None path declared order |> ignore` with
  `claimDerived declared order path |> ignore`.
- At `:589-606`, keep `owner`/`preferred` from the symbol branch, and route the path branch
  through `claimDerived`. The `match facts.SymbolName with … | _ -> path, None` binding gains a
  third component `derived` (`false` for the symbol branch, `true` for the path branch). Both
  `claim owner preferred typeId order` calls become
  `if derived then claimDerived typeId order preferred else claim owner preferred typeId order`.
- Any further `claim` call the companion plan added (its Task D role-name claim
  `claim None $"{root}{role}" value export.Order`) stays a plain `claim`. A role name is
  root-level and not derived.
- In the result record (`:773-777`), add `MintedNames = minted`.

`Orphans.fs`: delete `mintedNames` (`:166-174`) and its doc comment. At `:272`, pass
`model.MintedNames` in its place:

```fsharp
                        prune arms model.MintedNames (armRoots model arms) (exportNames ctx model) [] model.Decls
```

Update `prune`'s doc comment (`:188-192`): "unread generator-minted declaration" becomes
"unread declaration `synthesize-anonymous` named from a derived position
(`ShapeModel.MintedNames`)".

Run `rtk grep -n "mintedNames\|isAnonymousArm" src/Xantham.Generator/Shape/Orphans.fs`. Expected
output: only `taggedArmNames`' use (`:143`), which R11 keeps.

- [ ] **Step 8: Harvest type-only global namespaces and add the pass**

In `harvestGlobals`, replace the comment at `:410-413` with:

```fsharp
                        // A namespace of types alone is neither a type nor a value, so
                        // `TailStream` arrives under `Module` and nowhere else. It is harvested
                        // below as an export of its own, which `harvest-namespace-members` walks.
```

In the `harvested` list (`:453-467`), after the `for symbol in writable do` loop:

```fsharp
                                for symbol in declared do
                                    if
                                        admits (Grouping.classify ctx.PackageDir (ValueSome symbol))
                                        && Naming.isWritableTypeName symbol.Name
                                        && not (hasAny (SymbolFlags.Type ||| SymbolFlags.Value) symbol.Flags)
                                        && not (Set.contains symbol.Id moduleBodies)
                                    then
                                        {
                                            ExportName = symbol.Name
                                            Symbol = symbol
                                            HasValueExport = false
                                            Docs = ""
                                            Tags = []
                                            Origin = FromGlobal
                                            Order = Grouping.declOrder symbol.Declarations
                                        }
```

Directly above the `harvestDocs` doc comment (`:513`):

```fsharp
/// The exported interfaces, aliases, classes and enums of every harvested namespace, nested
/// namespaces included, each harvested under its namespace's origin. A member carries no value
/// export of its own: its value binds through the namespace object. Every walked namespace joins
/// `Namespaces`, a nested one under its dotted path (`Pool.Inner`). A module package's global
/// namespaces (`declare global { namespace X }`) are harvested as global exports first. Only
/// namespaces and members whose group ships are walked.
let harvestNamespaceMembers: Pass<HarvestModel> =
    {
        Name = "harvest-namespace-members"
        Run =
            fun ctx model ->
                async {
                    let ships (symbol: SymbolResponse) =
                        GeneratorConfig.disposition ctx.Config (Grouping.classify ctx.PackageDir (ValueSome symbol)) = Ship

                    let isNamespace (symbol: SymbolResponse) =
                        hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name

                    let memberKinds =
                        SymbolFlags.Interface
                        ||| SymbolFlags.TypeAlias
                        ||| SymbolFlags.Class
                        ||| SymbolFlags.Enum

                    // Global namespaces a module package declares: in scope at the entry file,
                    // parented by no module, and first declared under the package. A namespace a
                    // dependency or the compiler lib declares first stays its owner's, even where
                    // this package adds to it (`NodeJS`, `Intl`): `declaresUnderPackage` would
                    // re-home it, and `ships` would then drop the owner's members.
                    let harvestedIds = model.Exports |> List.map _.Symbol.SymbolId |> Set.ofList

                    let! inScope =
                        ctx.Session.getSymbolsInScope (
                            SymbolFlags.Module,
                            file = DocumentIdentifier.FileName(ctx.EntryFile / uom<declFile>),
                            position = 0
                        )

                    let packageGlobals =
                        inScope
                        |> Array.filter (fun symbol ->
                            isNamespace symbol
                            && ValueOption.isNone symbol.Parent
                            && Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage
                            && not (Set.contains symbol.SymbolId harvestedIds))
                        |> Array.sortBy _.Name
                        |> Array.map (fun symbol ->
                            {
                                ExportName = symbol.Name
                                Symbol = symbol
                                HasValueExport = hasAny SymbolFlags.Value symbol.Flags
                                Docs = ""
                                Tags = []
                                Origin = FromGlobal
                                Order = Grouping.declOrder symbol.Declarations
                            })
                        |> Array.toList

                    let rec walk visited (path: string) (origin: ExportOrigin) (ns: SymbolResponse) =
                        async {
                            let! exports = ctx.Session.getExportsOfModule ns.Id

                            let! resolved =
                                exports |> ValueOption.defaultValue [||] |> Array.map (followAlias ctx) |> Async.Parallel

                            let candidates =
                                resolved |> Array.sortBy fst |> Array.filter (snd >> ships) |> Array.toList

                            let members =
                                candidates
                                |> List.filter (fun (_, symbol) -> hasAny memberKinds symbol.Flags)
                                |> List.map (fun (name, symbol) ->
                                    {
                                        ExportName = name
                                        Symbol = symbol
                                        HasValueExport = false
                                        Docs = ""
                                        Tags = []
                                        Origin = origin
                                        Order = Grouping.declOrder symbol.Declarations
                                    })

                            let rec descend visited found named pending =
                                async {
                                    match pending with
                                    | [] -> return visited, found, named
                                    | (_, (symbol: SymbolResponse)) :: rest when
                                        isNamespace symbol && not (Set.contains symbol.SymbolId visited)
                                        ->
                                        let nested = $"{path}.{symbol.Name}"
                                        let! visited, inner, innerNamed = walk (Set.add symbol.SymbolId visited) nested origin symbol

                                        return!
                                            descend
                                                visited
                                                (found @ inner)
                                                (named @ [ symbol.SymbolId, nested * uom<symbolName> ] @ innerNamed)
                                                rest
                                    | _ :: rest -> return! descend visited found named rest
                                }

                            return! descend visited members [] candidates
                        }

                    let exports = model.Exports @ packageGlobals

                    let seeds =
                        exports
                        |> List.filter (fun export -> isNamespace export.Symbol && ships export.Symbol)
                        |> List.distinctBy _.Symbol.SymbolId

                    let pathOf (seed: HarvestedExport) =
                        Map.tryFind seed.Symbol.SymbolId model.Namespaces
                        |> Option.map (fun name -> name / uom<symbolName>)
                        |> Option.defaultValue seed.Symbol.Name

                    let! _, harvested, named =
                        seeds
                        |> List.fold
                            (fun state (seed: HarvestedExport) ->
                                async {
                                    let! visited, found, named = state
                                    let path = pathOf seed
                                    let! visited, inner, innerNamed = walk visited path seed.Origin seed.Symbol

                                    return
                                        visited,
                                        found @ inner,
                                        named @ [ seed.Symbol.SymbolId, path * uom<symbolName> ] @ innerNamed
                                })
                            (async.Return(seeds |> List.map _.Symbol.SymbolId |> Set.ofList, [], []))

                    return
                        Advanced
                            { model with
                                Exports =
                                    exports @ harvested
                                    |> List.distinctBy (fun export ->
                                        export.Origin, export.ExportName, export.Symbol.SymbolId)
                                Namespaces =
                                    named
                                    |> List.fold
                                        (fun names (id, name) ->
                                            if Map.containsKey id names then names else Map.add id name names)
                                        model.Namespaces
                            }
                }
    }
```

Seeds are recorded in `Namespaces` too. An ambient module's namespace export (Node's `"crypto"`
`webcrypto`) is otherwise absent from the map, so its members would find no module path.

`ValueOption.isNone symbol.Parent` is the global test, and it has two failure modes to check in
Step 9:
- A module-local namespace that reports no parent would appear here and be harvested twice (once
  under `FromModule`, once `FromGlobal`). Duplicate `Pool`/`Shapes` declarations would show it.
- A global namespace that reports a parent would never appear, and `NestedNameLabGlobal` would be
  absent.

On either symptom, read what the wire returns for `symbol.Parent` on both symbols before choosing
another test.

Add the pass to `passes` (`:603-610`):

```fsharp
        harvestExports
        harvestGlobals
        harvestNamespaceMembers
        harvestAmbientClasses
        harvestDocs
        orderExports
```

Replace the header invariant (`:1-3`) with:

```fsharp
/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; the tier's invariant is that every export of the entry module, and every
/// exported type member of a harvested namespace, appears exactly once, aliases followed to their
/// origin.
```

`Model.fs:1150-1153`, the `Namespaces` doc:

```fsharp
        /// The namespaces the entry package declares, by symbol id, under names an F# module
        /// can be spelled with; a nested namespace is spelled by its dotted path (`Pool.Inner`).
        /// Every declaration written inside one is named inside that module path.
```

`CatalogCompatibility.fs:33`: `ApiVersion = 9`.

- [ ] **Step 9: Run to verify the lab and units pass**

Run the three commands of Step 5. Expected: PASS. Read the `nested-name-lab` golden diff in full
(`git diff -- tests/Xantham.Generator.Tests/golden/nested-name-lab`):
- New modules `Pool` (with `Inner`), `Shapes`, `Pick`, `Signal`, `Markup`, `Queue`, and
  `Globals.NestedNameLabGlobal` (or `NestedNameLabGlobal` wherever `ExportLayout` homes a global
  in a mixed run; read which).
- `Reached`, `Conn`, `Element`, `Attributes` moved from the root into their modules.
- The `Cluster.Node` SY004 removed from `manifest.json`/`symbols.jsonl`.
- Zero AC001.

If an expected string differs, read the golden and the code path that produced it before
touching the assertion. Change an assertion only to what the rulings predict. Examples:
`and Fragment =` where `type Fragment =` was assumed, or `Globals.` nesting the global module.

Run: `dotnet build Xantham.slnx`. A failure in `NestedNameLab.fs` belongs to this task: reduce it
to the lab declaration and fix it here. Failures in large fixtures belong to Task 4.

- [ ] **Step 10: Re-pin the companion plan's tests to the nested spelling**

The companion plan lands first. Its Task C pins `Contested.Right` under today's naming, as the
bare `Right`; Task C itself states that this plan re-pins them. Under R1 they nest.

- **`exclusive-arms-lab` pair four** (companion Task C, Step 3's Pipeline case; `nested` is its
  `moduleBodyOf source "Contested"`, `holder` its `declarationBodyOf source "ContestedHolder"`):
  - `Expect.stringContains source "type Right =" "…bare spelling (re-pinned by the companion plan)"`
    becomes `Expect.stringContains nested "type Right =" "`Contested.Right` is declared in its namespace's module"`.
  - `Expect.stringContains holder "abstract right: Right with get, set" "and the bare one"` becomes
    `"abstract right: Contested.Right with get, set"` with the message "and so is the other arm".
  - `"abstract read: arm: Right -> Contested.Left"` becomes `"abstract read: arm: Contested.Right -> Contested.Left"`.
  - `"static member pickLeft (left: Contested.Left) : Right = jsNative"` becomes
    `"static member pickLeft (left: Contested.Left) : Contested.Right = jsNative"`.
  - The `Contested.Left` pins and the lab's own comment ("A top-level `Left` sits beside
    `Contested.Left`") stay as they are.
- **`value-side-lab`** (companion Task D): its value-side pins hold under always-nest unchanged:
  - `Point.Defaults` is a value-owned shape nesting under the source namespace through
    `nestBases[valueId] = root`. The export root of `Point` has no parent namespace, so this
    plan's root change (Step 7) leaves it `Point`.
  - The collision `Point.Anchor2` holds. `Point.Anchor` is a namespace interface: it is now
    harvested and named `Point.Anchor` by `name-exports`, ahead of every walk. That removes the
    walk-order dependence the companion flags (its Review Focus 5 and its closing note: `pinned`
    before `anchor`). The value-owned `anchor` shape then finds `Point.Anchor` taken and claims
    `Point.Anchor2` whatever the member order.
  - The lab's top-level `Anchor` is no longer what makes `Point.Anchor` nest. It nests
    unconditionally. The pins still hold, and that line of the lab comment is now redundant.
  - `TokenValue.Options` (`<Stem>Value`, `SY007`) holds. `Token` is a top-level variable merged
    with a type, with no parent namespace, and its value-owned shape stays position-derived under
    `TokenValue`.
  - The `SY004` the companion keeps for `Point.Defaults` (position-derived, raised by `claim` with
    no `owner`) is untouched: Step 7 guards only the `owner = Some` path.

  To verify, read the `value-side-lab` diff: expected only `Point.Anchor`'s `SY004` removed
  (namespace nesting, R6) and no rename. A moved `Point.Anchor2`, `Point.Defaults` or
  `TokenValue.Options` is handed back.
- **New interaction.** A harvested enum *member of a namespace* has a declared and a value type,
  so the companion's role naming applies to it. With Step 7's export root, its value side is
  claimed as `<Ns>.<Stem>Enum` (e.g. `Ns.ColorEnum`, `SY007`) inside the namespace's module, not at
  the root. No current lab has a namespace enum. If a golden shows a root-level `<Stem>Enum` for
  one, Step 7's root prefix was skipped.

Run `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"` → PASS.

Do not commit. Hand Task 4 the working tree.

---

### Task 4: Real-fixture goldens, documentation, and the Stage 2 commit

**Files:**
- Modify: `tests/Xantham.Generator.Tests/golden/**` (regenerated)
- Modify: `docs/.ai/plans/generator-architecture.md` (§3 Tier 1 invariant `:96-99`; phase record appended at the end)
- Modify: `docs/.ai/plans/generator-type-mapping.md` §4.13 (`:802-819`), §4.14 (`:845-851`)
- Modify: `docs/.ai/footguns.md`
- Modify (only if a compile-gate break reduces to a generator fix): the owning `src/Xantham.Generator/Shape/*.fs` file, plus a lab case in `tests/fixtures/nested-name-lab/index.d.ts`

**Interfaces:**
- Consumes: Task 3's working tree; Task 1's baselines.
- Produces: the Stage 2 commit.

- [ ] **Step 1: Regenerate and gate**

```
dotnet fsi build.fsx -- test --update
dotnet build Xantham.slnx
```

Expected: the compile gate passes. On failure, report the first several **distinct** error codes
with one site each. The likely ones are:
- FS0037, a duplicate definition in one module
- FS0039, a reference to a renamed type
- FS0953, a recursive abbreviation
- FS0023, a member name clashing with a union case

Reduce each to a namespace declaration added to `nested-name-lab` and fix it in the pass that
named the type. A break that is not a consequence of the nesting or the harvest is handed back.

Suites that authenticate the committed Core.TS catalogue fail on `API version` (8 vs 9) until
Task 5. Record which suites they are; every other failure is this task's.

- [ ] **Step 2: Measure**

```
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
dotnet fsi build.fsx -- findings > tests/.scratch/namespace-member-harvest/findings-after.txt
diff tests/.scratch/namespace-member-harvest/findings-before.txt tests/.scratch/namespace-member-harvest/findings-after.txt
node tests/.scratch/namespace-member-harvest/census-flat-fixtures.cjs "$PWD" "$TSJS" > tests/.scratch/namespace-member-harvest/census-fixtures-after.txt
rtk git diff --stat -- tests/Xantham.Generator.Tests/golden
```

Expected:
- **Census:** `absent 0, declared flat 0` for every fixture, except names carrying an explaining
  finding (list them).
- **SY004:** falls by the namespace nestings it used to report.
- **AC001:** rises only where Step 5 explains each case.
- `rtk grep -l "Exports_" tests/Xantham.Generator.Tests/golden` lists no golden that lacked it before.

- [ ] **Step 3: Read the diffs by sample**

For each of solid-js, workers-types and type-fest, read
`git diff -- <golden>/*.fs | head -n 200`, plus these targeted greps:
- **solid-js:**
  - `module JSX` holds the `JSX` members.
  - `JSXElement` readers now read `JSX.Element` (R9).
  - The second `JSX` namespace's members sit under the module its value side names (`JSX2.…`),
    not under per-member suffixes (Review Focus 5): `rtk grep -c "Element2\b" SolidJs.fs` before
    and after.
- **workers-types:**
  - `module WebAssembly` holds `Exports`, `Module`, `Memory`, `Table` and `Global`.
  - The root `[<Global("globalThis")>] type Exports` is unchanged.
  - `TailStream`, `Rpc` and `Cloudflare` members are nested.
  - `CloudflareWorkersModule.*` members are nested whether or not they are contested.
- **type-fest:**
  - `PackageJson.Exports` (alias) is declared beside the existing `module PackageJson.Exports`.
    `type Exports` and `module Exports` in one module are legal F#.
  - `TsConfigJson.CompilerOptions.JSX` sits two modules deep.
- **Labs:** `ambient-module-lab`, `dependency-entrypoint-lab`, `tagged-arm-lab` and
  `catalog-class-inheritance-lab` each move flat namespace members into modules. Read each diff
  in full.

Any other golden that moved: report which and why before committing.

- [ ] **Step 4: Run the full suite**

Run: `dotnet fsi build.fsx -- test --run-gate`
Expected: PASS except the catalogue-version suites recorded in Step 1. That includes the three
`unexplainedDrops` assertions (`Pipeline.test.fs:3831-3867`).

- [ ] **Step 5: Residuals**

If an `unexplainedDrops` assertion fails, each symbol it lists is a namespace member that only
`AC001` names. For each one:
1. Write the member into `tests/.scratch/namespace-member-harvest/residual-<name>/`.
2. Generate it with `dotnet run --project src/Xantham.Cli -- generate <lab> -o <lab>/out`.
3. Find the pass that drops it.

A pass that drops silently gets its existing finding at the drop site. A construct no pass can
declare is handed back with its five-line reproducer. The test is not weakened.

- [ ] **Step 6: Documentation**

`generator-architecture.md` §3 Tier 1: replace the **Invariant** bullet (`:96-99`) with:

```markdown
- **Invariant:** every export of the entry module (transitively through re-exports, via
  `getAliasedSymbol` to origin) appears exactly once, and so does every exported interface,
  alias, class and enum of a harvested namespace, nested namespaces and a module package's
  `declare global` namespaces included (`harvest-namespace-members`). A namespace member carries
  no value export: its value binds through the namespace object. Declaration merging is already
  done because we harvest *symbols*, not declaration nodes.
```

Append at the end of `generator-architecture.md`:

```markdown
## Namespace members (2026-10-15)

A namespace's type members are declared under its module path, as TypeScript spells them
(`Intl.Collator`, `JSX.Element`, `NodeJS.Signals`, `Pool.Inner.Deep`), whether or not a
signature reads them. Before, members were reach-only: an exported namespace's member no
signature read was never declared and raised nothing, and a reached member was declared flat
unless its name was contested. `harvest-namespace-members` walks every harvested namespace
export, every package-global namespace of types alone, and the `declare global` namespaces a
module package declares first (a namespace another group declares first stays its owner's), and
harvests each member under its namespace's origin with `HasValueExport = false`.
`Spec.namespacePath` gives every naming pass the module path. Nesting under a namespace raises no
finding; `SY004` reports position-derived nesting only. A namespace is represented by its
represented members. `drop-orphan-delegates` prunes by provenance: `synthesize-anonymous` records
`MintedNames`, the names it claims from a derived position, and no export or namespace-member
root is ever in it. `CatalogCompatibility.ApiVersion` is 9: a catalogue's API text carries the
nested names. Measures: <findings diff summary, golden line counts, census before/after>.
```

Fill `<findings diff summary, golden line counts, census before/after>` from Step 2.

`generator-type-mapping.md` §4.13, after the "Declaration merging" bullet:

```markdown
- **Landed, 2026-10-15.** "Nested `namespace` → nested module" holds for every namespace type
  member, contested or not, reached or not: `Pool.Inner.Deep`, `Intl.Collator`. A namespace alias
  a signature reads is written by its own name (`JSX.Element`, not a position-minted
  `JSXElement`). The namespace's value side keeps its root type and binding (`type Pool`,
  `static member Pool: Pool`) beside `module Pool`. A DU case spelled like a member module
  (`Signal.Start`) resolves to the case in expressions and to the interface in type positions.
```

`generator-type-mapping.md` §4.14: in the "Landed, wave seven lane AI (item 3)" bullet
(`:845-851`), append: "Superseded 2026-10-15 (§4.13): a namespaced declaration nests whether or
not its name is contested, and the nesting raises no `SY.NameNestedUnderOwner`."

`footguns.md`, beside `objectRef ordering`:

```markdown
- **Namespace members are harvested and nested, not reached and flattened.** A namespace member
  reaches the shape tier as a `HarvestedExport` (`harvest-namespace-members`), and every naming
  pass prefixes `Spec.namespacePath`. A naming pass that builds a type name from `fsName` without
  that prefix declares the member flat at its owner's root. A type-only global namespace arrives
  under `SymbolFlags.Module` alone, and `harvest-globals` harvests it explicitly.
```

Update this plan's Measures table if Task 1's numbers differed.

- [ ] **Step 7: Commit Stage 2**

```
rtk git add src/Xantham.Generator tests/fixtures/nested-name-lab tests/Xantham.Generator.Tests docs/.ai/plans/generator-architecture.md docs/.ai/plans/generator-type-mapping.md docs/.ai/footguns.md docs/.ai/plans/2026-10-15-namespace-member-harvest.md
rtk git status
rtk git commit -m "fix(generator)!: declare namespace members under their namespaces" -m "<one paragraph: harvest of unreached members; unconditional nesting; SY004 scope; ApiVersion 9; census before/after from Step 2>" -m "BREAKING CHANGE: a type written inside a TypeScript namespace is declared inside that namespace's F# module (Intl.Collator, JSX.Element), not at the binding root; bindings regenerated with this generator rename every such type. Catalogues built before this change (API 8) are rejected." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

`rtk git status` must show `.serena/project.yml` unstaged. Fill the second `-m` from Step 2.

---

### Task 5: Regenerate Core.TS and Node as a documented breaking change

**Files:**
- Modify: `src/Xantham.Fable.Core.TS/{Fable.Core.TS.fs,declarations.json,manifest.json,symbols.jsonl,catalogue-inputs.json}` (regenerated), `src/Xantham.Fable.Core.TS/README.md`
- Modify: `src/Xantham.Fable.Node/{Node.fs,manifest.json,symbols.jsonl}` (regenerated), `src/Xantham.Fable.Node/README.md`
- Modify: this plan's Measures section (rename table)

**Interfaces:**
- Consumes: Stage 2; `census-libs-before.txt`.
- Produces: two commits pushed with Stage 2.

- [ ] **Step 1: Regenerate Core.TS**

Run: `dotnet fsi build.fsx -- generate --only compiler-lib`
This runs the CLI over `tools/fable-core-ts-input`, moves `groups/Fable.Core.TS.fs`, formats it,
and stamps `catalogue-inputs.json` via `tools/package-core-ts-catalog.fsx -- stamp`
(`build.fsx:515-538`). `declarations.json` now records `apiVersion: 9`.

- [ ] **Step 2: Measure Core.TS and build the rename table**

```
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
node tests/.scratch/namespace-member-harvest/census-flat.cjs "$PWD" "$TSJS" > tests/.scratch/namespace-member-harvest/census-libs-after-core.txt
git diff -U0 -- src/Xantham.Fable.Core.TS/Fable.Core.TS.fs | grep -E '^-\s*(type|and) ' > tests/.scratch/namespace-member-harvest/core-ts-removed.txt
git diff -U0 -- src/Xantham.Fable.Core.TS/Fable.Core.TS.fs | grep -E '^\+\s*(type|and) ' > tests/.scratch/namespace-member-harvest/core-ts-added.txt
```

Expected: `Core.TS (lib esnext+dom): total 169, absent 0, declared flat 0`. Any remainder must be
explained by a finding in `src/Xantham.Fable.Core.TS/symbols.jsonl`; grep it, do not load it.

Pair each removed declaration with its replacement: a flat `Collator` becomes `Intl.Collator`,
and a position-minted alias name becomes the alias's own nested name. Write a rename table into
this plan's Measures section, one row per namespace: `namespace | renamed | newly declared | example`.
A removed declaration with no replacement is handed back.

- [ ] **Step 3: Gate Core.TS**

Run `dotnet build Xantham.slnx`, then `dotnet fsi build.fsx -- test`.
Expected: PASS, including the catalogue suites that failed on `API version` in Task 4 (the Core.TS
entry matrix, the compiler-lib ownership lab, the package consumer test). Any other failure is
handed back.

- [ ] **Step 4: Core.TS migration note and commit**

Add to `src/Xantham.Fable.Core.TS/README.md`, after its first paragraph:

```markdown
## Namespace members live in their namespace's module

From the release after 0.4.0-beta.1, a type TypeScript declares inside a namespace is declared
inside the F# module of that namespace: `Intl.Collator`, `Intl.NumberFormatOptions`,
`WebAssembly.Memory`, `Temporal.PlainDate`. Earlier releases declared them at the root of `Es`
(`Collator`). Replace the root name with the namespace-qualified one; the full list is in the
release notes. A declaration catalogue from an earlier release is rejected (API version 8); restore
the catalogue of the same package version you compile against.
```

```
rtk git add src/Xantham.Fable.Core.TS
rtk git commit -m "feat(core-ts)!: declare namespace members under their namespaces" -m "<rename table from Step 2: one line per namespace with counts, then one line per renamed type, old -> new>" -m "BREAKING CHANGE: Intl, WebAssembly and Temporal types moved from the Es root into their namespace modules (<n> renamed, <m> newly declared; e.g. Collator -> Intl.Collator). The packaged catalogue is API version 9." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

Fill `<…>` from Step 2's table.

- [ ] **Step 5: Regenerate, measure and gate Node**

```
dotnet fsi build.fsx -- generate --only node-lib
node tests/.scratch/namespace-member-harvest/census-flat.cjs "$PWD" "$TSJS" > tests/.scratch/namespace-member-harvest/census-libs-after-node.txt
git diff -U0 -- src/Xantham.Fable.Node/Node.fs | grep -E '^-\s*(type|and) ' > tests/.scratch/namespace-member-harvest/node-removed.txt
git diff -U0 -- src/Xantham.Fable.Node/Node.fs | grep -E '^\+\s*(type|and) ' > tests/.scratch/namespace-member-harvest/node-added.txt
grep -cE '^\+\s*type [A-Za-z.]+ = [A-Z][A-Za-z.]+$' tests/.scratch/namespace-member-harvest/node-added.txt
dotnet build Xantham.slnx
dotnet fsi build.fsx -- test
```

Expected:
- **Census:** `Node (@types/node): total 391, absent 0, declared flat 0`.
- **`grep -c`:** counts alias-to-alias abbreviations, i.e. R9's primitive chains such as
  `type RequestId = ScriptId`. Report the number.
- **Rename table:** same pairing and table as Step 2, added to the Measures section.

- [ ] **Step 6: Node migration note and commit**

Add to `src/Xantham.Fable.Node/README.md`, after its first paragraph:

```markdown
## Namespace members live in their namespace's module

From the release after 0.4.0-beta.1, a type `@types/node` declares inside a namespace is declared
inside that namespace's F# module: `NodeJS.Signals`, `NodeJS.ProcessEnv`, `Inspector.Runtime.ScriptId`,
`Crypto.Webcrypto.CryptoKey`. Earlier releases declared most of them at the binding root.
Replace the root name with the namespace-qualified one; the full list is in the release notes.
```

Check the module spellings in this note (`Inspector.Runtime`, `Crypto.Webcrypto`) against the
regenerated `Node.fs` and correct them to what it declares.

```
rtk git add src/Xantham.Fable.Node
rtk git commit -m "feat(node)!: declare namespace members under their namespaces" -m "<rename table from Step 5>" -m "BREAKING CHANGE: NodeJS, inspector, crypto webcrypto, node:test, stream, module, assert and events namespace types moved from the binding root into their namespace modules (<n> renamed, <m> newly declared; e.g. Signals -> NodeJS.Signals)." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

- [ ] **Step 7: Record the rename tables**

Commit the Measures section's two rename tables as a docs commit:

```
rtk git add docs/.ai/plans/2026-10-15-namespace-member-harvest.md
rtk git commit -m "docs(plans): record namespace-member rename measures" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

## Handled by other plans

- **Global augmentation: plan 3, `docs/.ai/plans/2026-10-15-global-augmentation.md`.** It owns:
  - a module package's non-namespace `declare global` declarations (`var`, `function`,
    `interface`), rendered as a `[<Erase; Global("globalThis")>] Globals` type;
  - a package's additions to a type or namespace another group declares first (`NodeJS.PkgAdded`,
    `ProcessEnv.X`, `Window.extra`), rendered as per-accessor `[<Emit>]` extensions of the owner.

  Today they are dropped because of `harvestGlobals`' early return (`Harvest.fs:364-369`). That
  return rests on the premise at `generator-architecture.md` ~`:409-411` (2026-09-13) that folding
  globals in "would emit names it does not export". The premise is false: a `declare global`
  declaration is API the package contributes (`e-global-augmentation/findings.md` §3). This plan
  narrows its own seed (R3) so that plan 3 finds foreign namespaces still with their owners.
- **Alias applications written as applications** (H option 1, `h-alias-application/findings.md`
  §4). This is the fidelity fix for position copies such as `F1.W`. This plan takes only H option
  2 (R11), which prunes their unread residue by provenance.

## Self-review

- **Spec coverage:**
  - Findings §"Root cause":
    - seeding: Task 3 Step 8
    - `resolve-export-types` seeds from `Harvest.Exports`, unchanged (`Resolve.fs:234-262`)
    - coverage: Task 2
    - `name-exports` nesting: Task 3 Step 7, R1
  - §"Option 1" risks:
    - `WebAssembly.Exports`: R2, Task 4 Step 3
    - `Cloudflare.Env`: Task 4 Step 3
    - JSX recursion: `Markup`
    - class instance side: R8, `Pool.Conn`
    - compile gates: Task 4 Step 1
    - catalogue hash: R7, Task 3 Step 4
    - Core.TS/Node: Task 5
  - §"Test": the lab and the unit pins.
  - §"Related" C/D: Task 3 Step 10.
  - Arch §3: Task 4 Step 6.
  - User ruling: R1–R11 restated on fidelity (R3 narrowed per E §4; R11 is H §4 option 2 with the H §5 guard); Task 5 as a breaking change with rename tables
    and migration notes.
- **Placeholders:** the angle-bracketed commit bodies and measures are filled from the named
  step's output, which does not exist before execution.
- **Type consistency:** `harvestNamespaceMembers`, `"harvest-namespace-members"`,
  `Spec.namespacePath`, `Spec.underNamespace`, `HarvestModel.Namespaces` (dotted) and
  `ApiVersion = 9` are spelled identically across tasks.
- **Review Focus:** each line names its pinning test or step.
