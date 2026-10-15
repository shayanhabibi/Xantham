# Generic Object-Alias Applications — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An application of a generic alias over an object literal is written as an application
of that alias (`Wrap<bool>`, `Box2<float>`, `Wrap<'T>`, `Keyed<Derived>`), not as a
per-position copy of its members (`C.X`, `B.X`, `D.X<'T>`, `F1.W`, `F1.W2`). An alias whose body
is such an application is an abbreviation of it (`type Applied = Wrap<string>`). This reverses
§4.9's deferred item "generic alias instantiates by re-expansion" for object aliases.

**Architecture:** `synthesize-anonymous` (`Shape/Anonymous.fs`) already writes applications of
generic *intersection* and *tagged-union* aliases through `aliasDeclarationForms` and
`aliasInstantiationOf`. This plan adds object aliases to both. Each alias's declared form is
identified through the checker: it is the instantiation target its applications carry, or an
export's declared type for an alias whose body applies another alias. An application's anonymous
arguments are named from the application's position (`Fold.W.Item`). References apply the alias's
own constraints (`appliedRefTo`). Three smaller changes follow on separate tasks: a referenced
group's alias applications carry their arguments; a re-aliasing alias (`type Applied =
Wrap<string>`) becomes an abbreviation through a new `ShapeModel.AliasBodies`; and the corpus,
Core.TS and Node are regenerated and measured.

**Tech Stack:** F# generator (`src/Xantham.Generator`), Expecto suites, Fable.Core 5.2.0 compile
gate, TypeScript 7.x `tsc --api` (live), TypeScript 6.0.3 JS API (census scripts only).

**Spec:** `docs/.ai/plans/2026-10-15-pre158-findings.md` §H.4 option 1 (controller
ruling), with its repro labs `tests/.scratch/pre158/h-alias-application/lab/` and `lab2/`, and
the user's standing rule: *always choose the option most faithful to the TypeScript source;
blast radius, back-compatibility and churn are measures to report, never reasons.* The planning
evidence (scratch labs, census scripts, frozen-CLI outputs) is under
`tests/.scratch/pre158/plan-alias-application/`.

Plan 4 of four. It runs after:
1. `docs/.ai/plans/2026-10-15-exclusive-fold-and-value-side-fixes.md` (plan 1: `isAnonymousArm`
   fold restriction, `SY007`);
2. `docs/.ai/plans/2026-10-15-namespace-member-harvest.md` (plan 2: namespace members harvested
   and nested under `Spec.namespacePath`; R11 `ShapeModel.MintedNames` provenance pruning in
   `Orphans.prune`; `CatalogCompatibility.ApiVersion` 8 → 9);
3. `docs/.ai/plans/2026-10-15-global-augmentation.md` (plan 3).

## Global Constraints

- Generated bindings target Fable 5.x only (`Fable.Core` 5.2.0 compile gate, Fable 5.17.2 run gate).
- Tests run only via `dotnet fsi build.fsx -- test [--quick] [--update] [--filter <prefix>] [--run-gate]`; never `dotnet test` (it stalls on the suites' nested builds). The inner loop is `dotnet fsi build.fsx -- test --quick --update --filter "<prefix>"`; run the unfiltered `dotnet fsi build.fsx -- test` before each task's commit. `--filter` is a hierarchy prefix (`generator e2e.alias-application-lab`, not `alias-application-lab`).
- Goldens change only via `--update` (or `--goldens`), never by hand, and every golden diff is read before commit — aggregate measures plus bounded samples for the large fixtures (`.claude/rules/generator-fixtures.md`).
- A new finding code is a new case in `Findings.fs` + a `FindingCodes.table` row (next unused number under its prefix) + a `Findings.test.fs` pin. This plan adds one: `SY008` (`SY.AliasApplicationCopied`). `SY001`–`SY006` exist and plan 1 adds `SY007`; if plan 3 took `SY008`, take the next unused number and use it everywhere this plan says `SY008`.
- Update the relevant phase record in `docs/.ai/plans/generator-architecture.md` / `generator-type-mapping.md` in the same commit as the behaviour change.
- Commits use conventional-commit subjects and end with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
  ```
- Scratch files under `tests/.scratch/`, never the system temp directory. Shell commands are prefixed with `rtk`. Never stage `.serena/project.yml`; stage paths explicitly, never `git add .`.
- Branch: from `feat/tagged-arm-158` (PR #160, not yet merged), after plans 1–3's commits, because the work uses `Spec.isAnonymousArm` and the DD002/DD003/DT007 machinery from #158 and plan 2's `MintedNames`. Rebase onto `develop` once #160 merges. Suggested branch: `fix/generic-alias-applications`.
- Doc comments follow `.claude/rules/comments.md`; reviewer-only notes use `//FOR-REVIEW`.
- `src/**/*.fs` stays Fantomas-formatted (`build.fsx` formats unless `--no-format`); goldens stay unformatted.
- Release notes are the commits' `BREAKING CHANGE:` paragraphs (ShipIt renders them at `build.fsx -- bump`); only release commits edit a `CHANGELOG.md`.
- Line numbers below are at `91742f98`. Plans 1–3 edit the same files, so locate each edit by the quoted anchor text, not by number.

## Stages

| Stage | Tasks | Commits | Boundary |
|---|---|---|---|
| 0 | Task 1 | 0 | Measurement on a scratch branch, discarded. Its numbers go into Task 5's commit. |
| 1 | Task 2, Task 3 | 1 | Applications, then abbreviations, one commit. Task 2 makes the catalogue's `Config<string>` an application while `TextConfig` is still a distinct interface, and the catalog consumer fails (FS0001) until Task 3 makes it the abbreviation. Task 2 proves its behaviour on the lab and unit tests and hands Task 3 an uncommitted tree. |
| 2 | Task 4 | 1 | Referenced-group arguments. |
| 3 | Task 5 | 1 | Corpus measures and records. |
| 4 | Task 6 | 0–2 | Core.TS and Node, regenerated on opt-in in their own commits only if their output or the catalogue contract moved (R10). |

## Measures (taken while planning, 2026-10-15, at `91742f98`)

Two independent methods, neither a text match on the generator's output. Both were run from the
main checkout; the goldens are the frozen CLI's output (`tests/.scratch/xantham-cli/xantham.dll`
reproduces `golden/type-scope-lab/TypeScopeLab.fs` byte for byte).

1. **Checker census** — `tests/.scratch/pre158/plan-alias-application/census/census-alias-applications.cjs`,
   TypeScript 6.0.3 JS API over every fixture package's own `.d.ts` files. It counts distinct
   checker instantiations (`getTypeFromTypeNode`, keyed by type id, the generator's own
   hash-consing key) of a generic type alias whose declared type is an anonymous object type. It
   splits **object** (properties or index signatures) from **callback** (call signatures only)
   and **intersection** aliases, the declaring package (own / foreign / lib), **open** (an argument
   is a type parameter) and **re-aliased** (the instantiation carries another alias's symbol:
   `type TextConfig = Config<string>`).
2. **Structural golden match** — `census/golden-copies.py` reads every committed binding of a
   package the census lists. It counts declarations whose member-name set equals a generic object
   alias's property set (from the checker) under a name other than the alias's own declaration.

| Binding | Object-alias instantiations (open) | Per-position copies in the binding | Re-aliased declarations | Callback-alias instantiations (R8, out of scope) |
|---|---|---|---|---|
| `@cloudflare/workers-types` | 22 (20) | **19** (`Cloudflare.Workers.WorkflowEntrypoint.Run.Event ~ WorkflowEvent`, `…WorkflowRollbackContext.Ctx ~ WorkflowStepContext`) | 0 | 21 |
| `solid-js` | 16 (13) | **17** (`CreateResource.Options`…`Options4 ~ ResourceOptions`) | 0 | 84 |
| `type-fest` | 7 (5) | **1** (`Globals.ObservableLike.Subscribe.Observer ~ Observer`) | 1 | 1 |
| `catalog-subpath-lab` | 4 (0) | **4** (`Chat.Create.Result.Item`…`Item4 ~ Tool`) | 0 | 0 |
| `declaration-identity-lab` | 7 (3) | **1** (`Holder.Config ~ Config`) | 3 (`TextConfig`, `LeftCallback`, `RightCallback`) | 0 |
| `catalog-generic-defaults-lab` | 1 (1) | **1** (`Middleware.Callback.Result ~ Result`) | 0 | 0 |
| `type-scope-lab` | 1 (1) | **1** (`Create.Options ~ Options`) | 0 | 0 |
| `animejs` | 5 (0) | 0 (all are intersection operands, flattened) | 0 | 25 |
| `globals-lab` | 1 (0) | 0 | 0 | 0 |
| **Core.TS** (`lib.*.d.ts`) | 0 — the compiler lib declares no generic object-literal alias (`Partial`/`Record`/`Pick` are mapped, `FlatArray` indexed) | 0 | 0 | 0 |
| **Node** (`@types/node`, root) | 0 | 0 | 0 | 18 |
| **Total (corpus goldens)** | **64** | **44** | **4** | 160 |

Intersection-alias applications already are applications; R3 renames only their anonymous
arguments (`WithName.Item` → `Holder.W.Item`). Every committed binding holds **0** names of the form
`<IntersectionAlias>.Item<n>`, so R3 moves no committed golden.

Not in the corpus, for scale: the installed but ungenerated `@types/lodash` holds 1,736 object-alias
instantiations, `agents` 160 and `@cloudflare/puppeteer` 101.

**Lab evidence (frozen CLI, `plan-alias-application/lab-final`, Task 2's fixture without its last
line, `nv`, which adds `Nv.X`):** 67 declarations today, 27 after this plan. Today the lab emits 45 copies (`B.X`, `C.X` shared
by `c`, `c2` and nothing else, `D.X<'T>`, `Pb.X`, `K1.X`, `K2.X<'U>`, `K3.X<'U>` with its
constraint silently dropped, `P.X`, `T.X`, `Tree.X`, `Chain.Next`, `Link.Next`, `Fold.W`,
`Fold.W2` and their `Value`s, `Tag.Cases.X.W`/`Y.W` and their `Value`s, `UseFW.X`, `Opt.O`, which
`either (x: U2<string, Opt.O>)` also reads, `Nested.X`, `Holder.W<'T>`, `Flags.X`). It also
emits a runaway: `type Nest<T> = { v: T; inner?: Nest<T[]> }` mints `Nest.Inner` nested ten deep
and `Nest.X.Inner` nine deep, ending in `obj` (`RT001` 1, `TR002` 3), the object-alias analogue
of the `this` runaway the intersection path guards. Cross-package (`lab-x`, reference group):
`abstract box: AliasDepLab.DepBox` is written **bare**, FS0033 against the dependency's
`DepBox<'T>` (R7).

Task 1 replaces this estimate with the measured corpus delta.

## Rulings

- **R1 — Object-alias applications are written as applications of the alias they spell.**
  `c(x: Wrap<boolean>)` reads `c (x: Wrap<bool>)`, `b(x: Box2<number>)` reads `b (x:
  Box2<float>)`, `d<T>(x: Wrap<T>)` reads `d<'T> (x: Wrap<'T>) : 'T`, and the folded `Fold.w` reads
  `Wrap<Fold.W.Item>`. A copy is a distinct F# nominal type: a `C.X` value is not a `Wrap<bool>`,
  where TypeScript's `Wrap<boolean>` is one type. *Cost if wrong:* a misidentified form writes an
  application of the wrong declaration; R2 makes the form checker-backed.
- **R2 — An object alias's declared form is identified through the checker, never by the
  smallest-id shape test.** It is (a) the `Target` of any instantiation, when that target carries an
  alias symbol and binds that alias's own parameters, or (b) an export's declared type
  (`ExportTypes.Declared`) of the same form. The `isAliasTaggedForm` reading (all arguments type
  parameters, smallest id) is unsound here: `Wrap<T>` written inside `d<T>` passes it too (findings
  §2). A re-aliasing alias (`type Box2<T> = Wrap<T>`) shares `Wrap`'s target, so rule (b) is its only
  form. A top-level alias of a `.d.ts` module is an export whether or not it is written `export`
  (`type PrivBox<T> = Wrap<T>` is declared today with nothing reading it, `lab-priv3`), so rule (b)
  covers it. *Cost if wrong:* a re-aliasing alias whose declared type no export carries (a
  non-exported namespace member) has no form. Its applications are then written as applications of
  the alias its body names (`Wrap<float>`): still an application, and type-equal to the alias once
  Task 3 makes it an abbreviation.
- **R3 — An application's anonymous arguments are named from the application's position.**
  `Fold.w: Wrap<{ n: number }>` names its argument `Fold.W.Item`, as a generic interface's
  instantiation already does (`G<{ n: number }>` at `Holder.g` → `Holder.G.Item`, `type SB = G<{ n:
  number }>` → `SB.Item`). Today the intersection path names them under the alias's module with a
  global counter (`WithName.Item`, `WithName.Item2`, lab `lab-ix`). That spelling lands outside an
  arm root, so plan 2's `MintedNames` prune cannot reach a dropped arm's argument. Applies to
  intersection applications too: one rule for every alias application. *Cost:* 0 committed names
  move (Measures).
- **R4 — Alias applications apply the alias's own constraints.** The `AliasApplications` branch
  of `objectRef` and of the intersection reference called `appliedRef` with no parameters, so no
  argument was checked. They now call `appliedRefTo` with the declared form's `declParamIds`, as
  `namedUnionRef` already does. `k3<U extends { id: string }>(x: Keyed<U>)` reads `(x: Keyed<Base>)`
  with `TR027` (the copy `K3.X<'U>` dropped the bound with no finding). *Cost:* a structural-only
  bound widens its argument to the bound, reported.
- **R5 — An object application whose arguments cannot be recovered stays a copy, reported as
  `SY008 AliasApplicationCopied` (Ergonomic).** The intersection path widens this case to `obj`
  (`SY002`) because its copy runs away. An object copy terminates, because its own members'
  applications are recognised one level down, and the copy is closer to the source than `obj`.
- **R6 — An alias whose body applies another generic object alias is an abbreviation of that
  application.** This covers `type Applied = Wrap<string>`, `type Box2<'T> = Wrap<'T>`,
  `type TextConfig = Config<string>` and `FW.Applied` → `type Applied = FW.Box<string>`. It mirrors
  `type SB = G<SB.Item>` for interfaces. TypeScript's alias is transparent, and an interface copy
  is not. `Applied.Create "x"` still resolves, through the abbreviation, to `Wrap<string>.Create`.
  Where the arguments cannot be recovered (a parameter read only in a method's own type-parameter
  constraint: `type LeftCallback = Callback<Left>` with `Callback<T> = { run<U extends T>(value: U):
  U }`), the declaration stays an interface with `SY008`.
- **R7 — A referenced group's alias application writes the alias's arguments.** A `reference`
  group's object alias is identity-only, named by its alias symbol (O7), and carries its
  arguments on `AliasTypeArguments`, not `TypeArguments`. So `referencedRef` wrote
  `AliasDepLab.DepBox` bare, which is FS0033 against the shipped `DepBox<'T>`. It now writes
  `AliasDepLab.DepBox<float>`.
- **R8 — Pure-callback generic aliases are out of scope.** `type Callback<T> = (self: T) => any`
  is declared `type Callback<'T> = ('T -> obj)`. Its applications expand to `(Timer -> obj)`, which
  F# treats as the same type, so nothing nominal is lost; the alias name is lost from the
  spelling. Measured at 160 sites and reported for its own plan.
- **R9 — A `ship`-group dependency's aliases are out of scope.** Under `"groups": { "dep": "ship" }`
  the dependency's alias shapes are declared by content into `groups/<Dep>.fs` (`AliasDepLab.F.X<'T>`,
  and the non-generic `DepPlain` copied as `Holder.Plain`). This is the naming of ship-group alias
  declarations (`Resolve.fs:1573-1588` gives a `ship` origin no `shapeName`), not applications, and it
  affects non-generic aliases equally. Reported with its reproducer below.
- **R10 — The catalogue `apiVersion` moves only if a released producer must be rejected.** Copies
  in a catalogued producer's API (`Holder.config: Holder.Config`) differ from what this generator
  emits (`Config<string>`), so a consumer comparing surfaces would fail on that producer. Plan 2
  bumps 8 → 9. If no package release has shipped between that bump and this plan's merge, the
  only API 9 producers are the repository's own Core.TS and Node catalogues, which hold no
  object-alias application (Measures). Those stay valid, and no bump is made. If a release has
  shipped, bump to 10 and regenerate both catalogues (Task 6).
- **R11 — The intersection and tagged-union form readings stay as they are.** Their heuristics are
  pinned by `hoist-conditional-lab`, `chain-lab` and `generic-tag-lab`. R2's checker-backed rule is
  added for object forms only, beside them.

## Review Focus

1. **An application in an optional position** (`interface Opt { o?: Wrap<number> }`) reads
   `abstract o: Wrap<float> option`, not `Opt.O option`. Pinned in Task 2, lab `Opt`.
2. **An application shared by unrelated sites** (`Opt.o` and `either(x: Wrap<number> | string)`
   hash-cons to one type id) reads `U2<string, Wrap<float>>` at the second site, not the first
   site's copy name. Pinned in Task 2, lab `either`.
3. **An application as another application's argument** (`Wrap<Wrap<number>>`) reads
   `Wrap<Wrap<float>>`, and an application of a generic declaration's own parameter
   (`interface Holder<T> { w: Wrap<T> }`) reads `Wrap<'T>` in scope. Pinned in Task 2, lab
   `nested`, `Holder`.
4. **A mapped alias over a literal key set** (`type Flags<T> = { [K in "a" | "b"]: T }`) is an
   object form with members, so `flags (x: Flags<float>)`. An index-signature-only alias (`Dict<T>`)
   still reads `JS.Record<string, float>` and a callback alias (`Cb<T>`) `(float -> unit)`. Pinned in
   Task 2, lab `flags`, `dict`, `cb`.
5. **An application whose argument is `never` or `unknown`** (`Tool<never, any>` in
   `catalog-subpath-lab`) is written with each argument's own mapping and compiles; the copy wrote
   `never` as a get-only `unit` member (`plan-alias-application/lab-nv`). Pinned in Task 2, lab `nv`,
   and by the compile gate. A form claimed under a position path rather than its alias's name would
   be written at every application (`F.X<float>`); if the corpus diff shows one, hand it back with a
   reproducer.

## File map

| File | Change | Task |
|---|---|---|
| `src/Xantham.Generator/Shape/Anonymous.fs` | object forms in `aliasDeclarationForms`; `isAliasObjectShape`, `isAliasObjectForm`, `targetApplicationOf`; object branch and declared id in `aliasInstantiationOf`; `needsName` object gate; walk: `SY008`, argument path (R3), alias bodies (Task 3) | 2, 3 |
| `src/Xantham.Generator/Shape/Spec.fs` | `objectRef` and flattened-intersection reference use `appliedRefTo` for alias applications (R4); `referencedRef` alias arguments (R7) | 2, 4 |
| `src/Xantham.Generator/Findings.fs`, `tests/Xantham.Generator.Tests/Findings.test.fs` | `SY008` | 2 |
| `src/Xantham.Generator/Model.fs`, `src/Xantham.Generator/Pipeline.fs`, `tests/Xantham.Generator.Tests/Build.fs` | `ShapeModel.AliasBodies` | 3 |
| `src/Xantham.Generator/Shape/Interfaces.fs` | `declaresInterface` excludes alias bodies | 3 |
| `src/Xantham.Generator/Shape/Aliases.fs` | abbreviation right side for an alias body | 3 |
| `tests/fixtures/alias-application-lab/{package.json,index.d.ts}` | new lab | 2 |
| `tests/fixtures/alias-copy-lab/node_modules/{alias-copy-dep-lab,alias-copy-lab}/index.d.ts` | generic alias across a reference group | 4 |
| `tests/Xantham.Generator.Tests/Pipeline.test.fs` | `declarationsIn` helper; `alias-application-lab` block; `alias-copy-lab` case | 2, 3, 4 |
| `tests/Xantham.Generator.Tests/Shape.test.fs` | `objectAliasModel` and four `synthesize-anonymous` cases | 2, 3 |
| `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs` | `sharedConsumer` lines | 2, 3 |
| `tests/Xantham.Generator.Tests/golden/**` | regenerated | 2–5 |
| `docs/.ai/plans/generator-type-mapping.md` (§4.6 `:385-388`, §4.9 `:571`), `generator-architecture.md` (`:454-458`, `:667-670`, new section at the end), `docs/.ai/footguns.md` (Compiler API) | records | 2–5 |
| `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**`, `src/Xantham.Generator/CatalogCompatibility.fs:33` | conditional (R10) | 6 |

---

### Task 1: Measure the copies the change removes (scratch branch, no commit)

**Files:**
- Read only. Scratch: `tests/.scratch/alias-application/`.
- Scripts (gitignored, main checkout): `C:/Users/shaya/RiderProjects/Xantham/tests/.scratch/pre158/plan-alias-application/census/{census-alias-applications.cjs,golden-copies.py,golden-diff.py}`.

**Interfaces:**
- Consumes: the tree after plans 1–3; Task 2 Steps 4–7 (the code this task applies temporarily).
- Produces: `tests/.scratch/alias-application/{census-before.txt,copies-before.txt,findings-before.txt,findings-after.txt,golden-diff.txt,stat.txt}`; the numbers for Task 5's Measures record.

- [ ] **Step 1: Copy the scripts and take the baseline**

```bash
rtk git switch -c measure/alias-applications
mkdir -p tests/.scratch/alias-application
cp C:/Users/shaya/RiderProjects/Xantham/tests/.scratch/pre158/plan-alias-application/census/census-alias-applications.cjs tests/.scratch/alias-application/
cp C:/Users/shaya/RiderProjects/Xantham/tests/.scratch/pre158/plan-alias-application/census/golden-copies.py tests/.scratch/alias-application/
cp C:/Users/shaya/RiderProjects/Xantham/tests/.scratch/pre158/plan-alias-application/census/golden-diff.py tests/.scratch/alias-application/
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
node tests/.scratch/alias-application/census-alias-applications.cjs "$PWD" "$TSJS" > tests/.scratch/alias-application/census-before.txt
python tests/.scratch/alias-application/golden-copies.py "$PWD" > tests/.scratch/alias-application/copies-before.txt
dotnet fsi build.fsx -- findings > tests/.scratch/alias-application/findings-before.txt
```

The census writes `alias-props.json` beside itself, and the two Python scripts read it from there.
Expected last line of `copies-before.txt` at `91742f98`: `TOTAL {'multi': 42, 'single': 2}`
(44 copies). Plans 1–3 may move it. Plan 2's harvest declares namespace aliases that a copy used
to stand in for. Record whatever it prints.

- [ ] **Step 2: Apply Task 2's code changes, without its tests**

Apply Task 2 Steps 4, 5, 6 and 7 exactly as written there (`Findings.fs` `SY008`; `Anonymous.fs`
forms, `aliasInstantiationOf`, `needsName`, walk; `Spec.fs` `objectRef` and intersection reference).
Do not add the lab or the tests.

- [ ] **Step 3: Regenerate every golden**

Run: `dotnet fsi build.fsx -- test --quick --update`
Expected: the write pass rewrites the goldens. The check pass may report failures in suites that pin
copy names (`declaration-identity-lab`'s `Holder.Config`). Those failures are what Task 2 and Task 5
fix, so ignore them here. If the check pass obstructs the write, run
`dotnet fsi build.fsx -- test --goldens` instead, which writes the same files and checks nothing.
If the compile gate (`dotnet build Xantham.slnx`) fails, record the first distinct error codes with
one site each in `tests/.scratch/alias-application/compile.txt`; Task 5 must clear them.

- [ ] **Step 4: Measure the delta**

```bash
python tests/.scratch/alias-application/golden-diff.py "$PWD" HEAD > tests/.scratch/alias-application/golden-diff.txt
dotnet fsi build.fsx -- findings > tests/.scratch/alias-application/findings-after.txt
rtk git diff --stat -- tests/Xantham.Generator.Tests/golden src/Xantham.Fable.Core.TS src/Xantham.Fable.Node > tests/.scratch/alias-application/stat.txt
diff tests/.scratch/alias-application/findings-before.txt tests/.scratch/alias-application/findings-after.txt
```

Read, per binding, from `golden-diff.txt`: removed copies, their nested declarations, other removed
declarations, and added declarations. From the findings diff, read:
- `SY001` added: each recognised application;
- `SY004` removed: copies were position-nested;
- `SY008`: residual copies;
- `DD003`: plan 2's prune of unread arm residue;
- `RT001`/`TR002`: the depth cutoff, which falls where a runaway copy chain stood;
- `TR026`/`TR027`/`TR044`: constraint repair, R4.

Expected, against the planning estimate: copies removed ≈ 44 and `SY001` added ≈ the census's
object-alias instantiations in golden fixtures (64), less the 5 `animejs` intersection operands.
The corpus binding is expected to have 0 added declarations.

- [ ] **Step 5: Hand back what the estimate does not explain**

Stop and report, with the binding, the declaration name and a five-line reproducer, if any of these
holds:
- a removed declaration is neither a copy nor nested under one;
- an added declaration is anything other than an argument renamed under R3 (`<path>.Item`);
- Core.TS's `.fs` moves.

- [ ] **Step 6: Discard the branch**

```bash
rtk git restore --staged --worktree -- src tests
rtk git switch -
rtk git branch -D measure/alias-applications
```

Keep `tests/.scratch/alias-application/` (gitignored); Task 5 copies its numbers into the record.

---

### Task 2: Object-alias applications written as applications

**Files:**
- Create: `tests/fixtures/alias-application-lab/package.json`, `tests/fixtures/alias-application-lab/index.d.ts`
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs:43-75` (forms), `:220-288` (`aliasInstantiationOf`), `:515-537` (`needsName` object branch), `:619-668` (walk)
- Modify: `src/Xantham.Generator/Shape/Spec.fs:1952-1963` (flattened-intersection reference), `:2081-2094` (`objectRef`)
- Modify: `src/Xantham.Generator/Findings.fs:197-202`, `:918-953`; `tests/Xantham.Generator.Tests/Findings.test.fs:161-166`
- Test: `tests/Xantham.Generator.Tests/Shape.test.fs` (after the case "synthesize-anonymous widens an alias whose argument only the conditional carried", `:1960-1971`); `tests/Xantham.Generator.Tests/Pipeline.test.fs` (helper after `moduleBodyOf`, `:83-97`; new block after the `exclusive-arms-lab` block's closing `)` and plan 1's `value-side-lab` block); `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs:64-82`
- Regenerate: `golden/alias-application-lab`, `golden/declaration-identity-lab`
- Docs: `docs/.ai/plans/generator-type-mapping.md`, `generator-architecture.md`, `docs/.ai/footguns.md`

**Interfaces:**
- Consumes: plan 2's `ShapeModel.MintedNames` (derived claims, read by `Orphans.prune`) and namespace-member exports (`FW.Box` is an export named `FW.Box`); `Spec.declParamIds`, `Spec.isPureCallback`, `Spec.isAnonymousIndexSignature`, `Spec.arrayElement`, `Spec.isTuple`, `Spec.nestUnder`, `Spec.appliedRefTo`.
- Produces:
  - `Anonymous.isAliasObjectShape: ShapeModel -> TypeFacts -> bool` (private);
  - `Anonymous.targetApplicationOf: ShapeModel -> Map<int<symbolId>, int<typeId>> -> TypeFacts -> (int<typeId> * int<typeId> list option) option` (private; Task 3 reads it);
  - `Anonymous.aliasInstantiationOf` now returns `(string * int<typeId> * int<typeId> list option) option` (name, declared form id, arguments);
  - `SynthesizeAnonymous.AliasApplicationCopied of alias: string` (`SY008`, Ergonomic);
  - lab `alias-application-lab`;
  - helper `declarationsIn: string -> string list` in `Pipeline.test.fs`.

- [ ] **Step 1: Write the lab fixture**

`tests/fixtures/alias-application-lab/package.json`:

```json
{ "name": "alias-application-lab", "version": "1.0.0", "types": "index.d.ts" }
```

`tests/fixtures/alias-application-lab/index.d.ts` (identical to
`tests/.scratch/pre158/plan-alias-application/fixture/index.d.ts`, which the Measures were taken on):

```ts
// Applications of a generic object alias, written as applications (§4.9).
export type Wrap<T> = { value: T };
export declare function c(x: Wrap<boolean>): void;
export declare function c2(x: Wrap<boolean>, y: Wrap<boolean>): Wrap<boolean>;
export declare function d<T>(x: Wrap<T>): T;

// An alias whose body applies another: an abbreviation of that application.
export type Box2<T> = Wrap<T>;
export type Applied = Wrap<string>;
export declare function b(x: Box2<number>): void;
type PrivBox<T> = Wrap<T>;
export declare function pb(x: PrivBox<number>): void;

// Constraints, defaults and an unused parameter on the alias.
export interface Base { id: string }
export interface Derived extends Base { extra: number }
export type Keyed<T extends Base> = { item: T };
export declare function k1(x: Keyed<Derived>): void;
export declare function k2<U extends Base>(x: Keyed<U>): void;
export declare function k3<U extends { id: string }>(x: Keyed<U>): void;
export type Pair<A, B = string> = { a: A; b: B };
export declare function p(x: Pair<number>): void;
export type Tagged<T> = { tag: string };
export declare function t(x: Tagged<number>): void;

// `this` as an argument, and recursion uniform and polymorphic.
export interface Chain { next: Wrap<this> }
export interface Link extends Chain { label: string }
export type Tree<T> = { value: T; children: Tree<T>[] };
export declare function tree(x: Tree<number>): void;
export type Nest<T> = { v: T; inner?: Nest<T[]> };
export declare function nest(x: Nest<number>): void;

// Applications inside an exclusive fold and inside tagged arms.
export type Fold = { w: Wrap<{ n: number }>; a: number; b?: never } | { w: Wrap<{ n: number }>; b: string; a?: never };
export type Tag = { kind: "x"; w: Wrap<{ m: string }> } | { kind: "y"; w: Wrap<{ m: string }> };

// A generic namespace alias under an exclusive fold of the same name.
export type FW = { a: number; b?: never } | { b: string; a?: never };
export namespace FW {
    export type Box<T> = { boxed: T };
    export type Applied = Box<string>;
}
export declare function useFW(x: FW.Box<number>): void;

// Negatives: an index-signature alias and a callback alias keep their mappings.
export type Dict<T> = { [key: string]: T };
export declare function dict(x: Dict<number>): void;
export type Cb<T> = (value: T) => void;
export declare function cb(x: Cb<number>): void;

// An optional position, a nested application, an own-parameter application, a mapped alias over
// literal keys, and a union with a primitive.
export interface Opt { o?: Wrap<number> }
export declare function nested(x: Wrap<Wrap<number>>): void;
export interface Holder<T> { w: Wrap<T> }
export type Flags<T> = { [K in "a" | "b"]: T };
export declare function flags(x: Flags<number>): void;
export declare function either(x: Wrap<number> | string): void;
export declare function nv(x: Pair<never, unknown>): void;
```

- [ ] **Step 2: Write the failing tests**

In `Pipeline.test.fs`, directly after `moduleBodyOf` (`:83-97`), add:

```fsharp
/// The names a rendered source declares, in emission order, each qualified by the modules it is
/// written inside.
let private declarationsIn (rendered: string) =
    let scope = ref []

    rendered.Replace("\r\n", "\n").Split('\n')
    |> Array.choose (fun line ->
        let body = line.TrimStart ' '
        let depth = (line.Length - body.Length) / 4

        if body.StartsWith "module " && body.TrimEnd().EndsWith "=" then
            scope.Value <- List.truncate depth scope.Value @ [ body.Substring(7).Split(' ').[0] ]
            None
        elif body.StartsWith "type " then
            let name = body.Substring(5).Split([| ' '; '<'; '(' |]).[0]
            Some(String.concat "." (List.truncate depth scope.Value @ [ name ]))
        else
            None)
    |> List.ofArray
```

After the `fixtureTests "exclusive-arms-lab"` block and plan 1's `value-side-lab` block, add:

```fsharp
        // Plan 4 (2026-10-15): an application of a generic object alias is written as an
        // application of it (§4.9), its anonymous arguments named from the application's
        // position, its arguments checked against the alias's own constraints.
        yield!
            fixtureTests "alias-application-lab" (handFixture "alias-application-lab") GeneratorConfig.Default (fun package ->
                let rendered () = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                let keyed (result: RenderModel) key =
                    result.Findings |> List.filter (fun f -> f.Key = key)

                let messagesFor (result: RenderModel) symbol key =
                    keyed result key |> List.filter (fun f -> f.Symbol = symbol) |> List.map _.Message

                [ testCase "an application of a generic object alias is written as an application" <| fun _ ->
                      let source = (rendered ()).Files |> List.head |> snd

                      for expected in
                          [ "static member c (x: Wrap<bool>) : unit"
                            "static member c2 (x: Wrap<bool>, y: Wrap<bool>) : Wrap<bool>"
                            "static member d<'T> (x: Wrap<'T>) : 'T"
                            "static member b (x: Box2<float>) : unit"
                            "static member pb (x: PrivBox<float>) : unit"
                            "static member nv (x: Pair<"
                            "static member p (x: Pair<float, string>) : unit"
                            "static member t (x: Tagged<float>) : unit"
                            "static member tree (x: Tree<float>) : unit"
                            "static member nest (x: Nest<float>) : unit"
                            "abstract inner: Nest<'T[]> option"
                            "abstract next: Wrap<Chain>"
                            "abstract next: Wrap<Link>"
                            "abstract o: Wrap<float> option"
                            "static member nested (x: Wrap<Wrap<float>>) : unit"
                            "abstract w: Wrap<'T>"
                            "static member flags (x: Flags<float>) : unit"
                            "static member either (x: U2<string, Wrap<float>>) : unit"
                            "static member dict (x: JS.Record<string, float>) : unit"
                            "static member cb (x: (float -> unit)) : unit" ] do
                          Expect.stringContains source expected expected

                  testCase "an application's arguments are checked against the alias's constraints" <| fun _ ->
                      let result = rendered ()
                      let source = result.Files |> List.head |> snd
                      Expect.stringContains source "static member k1 (x: Keyed<Derived>) : unit" "a subtype argument is applied as written"
                      Expect.stringContains source "(x: Keyed<'U>) : unit" "a variable bound by the constraint is applied as written"
                      Expect.stringContains source "(x: Keyed<Base>) : unit" "a structural-only bound is written as the constraint"
                      Expect.isNonEmpty (messagesFor result "Exports.k3" "TR027") "and the substitution is reported"

                  testCase "no per-position copy is declared, and no runaway" <| fun _ ->
                      let result = rendered ()
                      let source = result.Files |> List.head |> snd

                      Expect.equal
                          (declarationsIn source |> Set.ofList)
                          (Set.ofList
                              [ "Wrap"; "Box2"; "Applied"; "PrivBox"; "Base"; "Derived"; "Keyed"; "Pair"; "Tagged"
                                "Chain"; "Link"; "Tree"; "Nest"; "Fold"; "Tag"; "FW"; "Dict"; "Cb"; "Opt"; "Holder"
                                "Flags"; "Exports"; "Fold.W.Item"; "Tag.Cases.X.W.Item"; "Tag.Cases.Y.W.Item"
                                "FW.Box"; "FW.Applied" ])
                          "the lab declares its source names and the anonymous arguments, nothing else"

                      Expect.isEmpty (keyed result "RT001") "polymorphic recursion no longer reaches the depth cutoff"
                      Expect.isEmpty (keyed result "SY008") "every application's arguments are recovered"

                      Expect.equal
                          (messagesFor result "C.X" "SY001")
                          [ "anonymous shape is an instantiation of Wrap; written as an application" ]
                          "each application is reported once, under the position it used to be named for"

                  testCase "an application inside an exclusive fold or a tagged arm leaves no copy" <| fun _ ->
                      let result = rendered ()
                      let source = result.Files |> List.head |> snd
                      Expect.stringContains source "abstract w: Wrap<Fold.W.Item>" "the fold reads the application"
                      Expect.isFalse (source.Contains "type W2") "no second arm copy"
                      Expect.stringContains source "X of w: Wrap<Tag.Cases.X.W.Item>" "each case field reads the application"
                      Expect.stringContains source "Y of w: Wrap<Tag.Cases.Y.W.Item>" "under its own case's path"

                      Expect.equal
                          (messagesFor result "Fold.W.Item2" "DD003")
                          [ "declaration dropped: only an arm the union replaced read it" ]
                          "the folded-away arm's argument is pruned and reported"

                  testCase "a generic namespace alias under an exclusive fold is not false-dropped" <| fun _ ->
                      let result = rendered ()
                      let source = result.Files |> List.head |> snd
                      Expect.stringContains source "static member useFW (x: FW.Box<float>) : unit" "the namespace alias is applied"
                      Expect.stringContains (moduleBodyOf source "FW") "type Box<'T> =" "declared under its namespace"

                      Expect.isEmpty
                          (keyed result "DD003" |> List.filter (fun f -> f.Symbol.StartsWith "FW."))
                          "an author-written name under the fold's root is never pruned" ])
```

`RenderModel` (`Model.fs:1886`) is what `Pipeline.generate` returns (`Pipeline.fs:448-454`). Check the `Symbol` of `k3`'s `TR027` against the regenerated
golden's `symbols.jsonl` (`rtk grep TR027 tests/Xantham.Generator.Tests/golden/alias-application-lab/symbols.jsonl`)
and use the symbol it carries.

In `Shape.test.fs`, after the helper `conditionalAliasModel` (`:338-363`), add:

```fsharp
/// `type Wrap<T> = { value: T }` as its declared form `declaredId` over `T` (20), alias symbol
/// 100, named `Wrap`, and one application `appliedId` instantiated from it (target
/// `declaredId`), carrying `appliedArguments` and one `number` (2) member named `appliedMember`,
/// exported as the value `seed`.
let private objectAliasModel (declaredId: int) (appliedId: int) (appliedArguments: int[] voption) (appliedMember: string) =
    let table =
        [ typeParam 20 "T"
          typeParam 21 "U"
          { Build.facts
              { Build.typeResponse declaredId TypeFlags.Object with
                  AliasSymbol = ValueSome 100
                  AliasTypeArguments = ValueSome [| 20 |]
                  ObjectFlags = ValueSome ObjectFlags.Anonymous } with
              AliasTypeArguments = [ 20<typeId> ]
              Members = [ Build.resolvedMember (Build.symbol 500 "value" SymbolFlags.Property) 20 ] }
          { Build.facts
              { Build.typeResponse appliedId TypeFlags.Object with
                  AliasSymbol = ValueSome 100
                  AliasTypeArguments = appliedArguments
                  Target = ValueSome declaredId
                  ObjectFlags = ValueSome(ObjectFlags.Anonymous ||| ObjectFlags.Instantiated) } with
              Members = [ Build.resolvedMember (Build.symbol 501 appliedMember SymbolFlags.Property) 2 ] } ]

    { Build.shapeModel (table @ Build.primitives) with
        Harvest = { Exports = [ Build.export "seed" (Build.symbol 400 "seed" SymbolFlags.BlockScopedVariable) ]
                    AmbientClasses = []
                    Namespaces = Map.empty
                    ShadowedByLib = 0
                  }
        ExportTypes = Map.ofList [ 400<symbolId>, { Declared = None; Value = Some((fun value -> value * uom<typeId>) appliedId) } ]
        DeclNames = Map.ofList [ (fun value -> value * uom<typeId>) declaredId, "Wrap" ] }
```

Mirror the `Harvest` record literal exactly as `conditionalAliasModel` spells it after plans 1–3
(a field plans 2–3 add goes here too).

After the case "synthesize-anonymous widens an alias whose argument only the conditional carried", add:

```fsharp
        testCase "synthesize-anonymous writes an object alias application as an application" <| fun _ ->
            let model = objectAliasModel 50 60 (ValueSome [| 2 |]) "value"

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60<typeId> named.DeclNames) (Some "Wrap") "the application reads the alias's name"
            Expect.equal (Map.tryFind 60<typeId> named.DeclParams) (Some [ 2<typeId> ]) "applied to its argument"
            Expect.equal (Map.tryFind 60<typeId> named.AliasApplications) (Some 50<typeId>) "an application of the declared form"
            Expect.equal (findings |> List.map _.Key) [ "SY001" ] "recognition, not a copy"

        testCase "synthesize-anonymous takes an object alias's form from its applications' target" <| fun _ ->
            // `Wrap<U>` written inside a generic signature (60) is created before the declared
            // form's id (70) in this table and binds only type parameters, as the declared form
            // does. The target names the form.
            let model = objectAliasModel 70 60 (ValueSome [| 21 |]) "value"

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60<typeId> named.AliasApplications) (Some 70<typeId>) "the target is the form"
            Expect.equal (Map.tryFind 60<typeId> named.DeclParams) (Some [ 21<typeId> ]) "applied to the caller's parameter"

        testCase "synthesize-anonymous keeps an object alias application whose arguments are lost as a copy" <| fun _ ->
            // No alias arguments, and no member shared with the form to unify through.
            let model = objectAliasModel 50 60 ValueNone "other"

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60<typeId> named.AliasApplications) None "no application to write"
            Expect.isSome (Map.tryFind 60<typeId> named.DeclNames) "declared as a copy under its position"
            Expect.contains (findings |> List.map _.Key) "SY008" "and reported"
            Expect.isFalse (findings |> List.exists (fun f -> f.Key = "SY002")) "not widened to obj"
```

In `DeclarationCatalog.test.fs`, append to `sharedConsumer` (before its closing `"""`):

```fsharp
let holderConfig (holder: Identity.Root.Holder) : Identity.Root.Config<string> = holder.config
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `dotnet fsi build.fsx -- test --quick --filter "generator e2e.alias-application-lab"`
Expected: FAIL. `alias-application-lab` has no golden yet (the e2e golden case fails), and
"an application of a generic object alias is written as an application" fails on
`static member c (x: Wrap<bool>) : unit` (today `c (x: C.X)`).

Run: `dotnet fsi build.fsx -- test --quick --filter "shape"`
Expected: FAIL in "synthesize-anonymous writes an object alias application as an application"
(`DeclNames[60]` is `Some "Seed"`, the copy).

- [ ] **Step 4: `SY008`**

`Findings.fs`, `FindingCodes.table`, after plan 1's `"SY.ValueSideNamed", "SY007"` row:

```fsharp
            "SY.AliasApplicationCopied", "SY008"
```

`SynthesizeAnonymous` union, after plan 1's `ValueSideNamed` case:

```fsharp
    /// An application of a generic object alias whose arguments neither the checker's alias
    /// arguments nor the alias's members recover. The declaration carries the alias's members
    /// under its own name, and a value of it is not a value of the alias.
    | [<Ergonomic>] AliasApplicationCopied of alias: string
```

and in its `Message`:

```fsharp
            | AliasApplicationCopied alias ->
                $"an application of {alias} whose arguments could not be recovered; declared as a copy of its members"
```

`Findings.test.fs`, after plan 1's `SY007` line:

```fsharp
                    "SY.AliasApplicationCopied SY008 ergonomic"
```

- [ ] **Step 5: Object forms and the object branch of `aliasInstantiationOf` (`Anonymous.fs`)**

After `isAliasTaggedForm` (`:45-55`), add:

```fsharp
/// An object type a generic alias stands for and `shape-interfaces` declares: members, or an
/// index signature beside them. A callback, an array, a tuple, a constructor object and a pure
/// index signature belong to other mappings.
let private isAliasObjectShape (model: ShapeModel) (facts: TypeFacts) =
    flag TypeFlags.Object facts
    && not (isPureCallback facts)
    && (arrayElement model facts).IsNone
    && not (isTuple facts)
    && facts.ConstructSignatures.IsEmpty
    && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
    && not (isAnonymousIndexSignature model facts)

/// A generic alias over an object literal, in a form that binds parameters of its own: every
/// alias argument is a distinct type parameter (§4.9).
let private isAliasObjectForm (model: ShapeModel) (facts: TypeFacts) =
    isAliasObjectShape model facts
    && (match facts.Response.AliasArgumentTypeIds with
        | ValueSome arguments ->
            arguments.Length > 0
            && arguments.Length = (declParamIds facts).Length
            && arguments
               |> Array.forall (fun id -> Map.tryFind id model.Types |> Option.exists (flag TypeFlags.TypeParameter))
        | ValueNone -> false)
```

Replace `aliasDeclarationForms` (`:57-75`, doc comment included) with:

```fsharp
/// The declaration form of every generic alias over an intersection, a tagged union or an object
/// literal: alias symbol -> the type id of that form. An intersection or tagged form is the smallest
/// id carrying the alias symbol that binds parameters of its own. An object form is the
/// instantiation target its applications carry, or the declared type of an export.
let private aliasDeclarationForms (model: ShapeModel) : Map<int<Measure.symbolId>, int<Measure.typeId>> =
    let forms =
        model.Types
        |> Map.toList
        |> List.sortBy fst
        |> List.fold
            (fun forms (typeId, facts) ->
                match facts.Response.AliasSymbolId with
                | ValueSome alias when
                    not (Map.containsKey alias forms)
                    && ((isAliasIntersectionForm model facts && not (List.isEmpty (declParamIds facts)))
                        || isAliasTaggedForm model facts)
                    ->
                    Map.add alias typeId forms
                | _ -> forms)
            Map.empty

    //FOR-REVIEW the smallest-id reading is unsound for objects: `Wrap<T>` written in `d<T>` binds only type parameters too (h-alias-application/findings.md §2). A re-aliasing alias (`type Box2<T> = Wrap<T>`) shares Wrap's target, so its export's declared type is its only checker-backed form.
    let targets =
        model.Types
        |> Map.toList
        |> List.choose (fun (typeId, facts) ->
            facts.Response.TargetTypeId
            |> ValueOption.toOption
            |> Option.filter (fun target -> target <> typeId))

    let exported =
        model.ExportTypes |> Map.toList |> List.choose (snd >> _.Declared)

    (List.distinct targets |> List.sort) @ (List.distinct exported |> List.sort)
    |> List.fold
        (fun forms typeId ->
            match Map.tryFind typeId model.Types with
            | Some facts when isAliasObjectForm model facts ->
                match facts.Response.AliasSymbolId with
                | ValueSome alias when not (Map.containsKey alias forms) -> Map.add alias typeId forms
                | _ -> forms
            | _ -> forms)
        forms
```

After `unifyAlias` (ends `:218`), add:

```fsharp
/// The object-alias form a type instantiates without carrying that alias's symbol, with the
/// arguments recovered from its members: the declared type of `type Applied = Wrap<string>`, or an
/// application of a re-aliasing alias that has no form of its own. `None` for the arguments where the members do
/// not recover every parameter.
let private targetApplicationOf
    (model: ShapeModel)
    (forms: Map<int<Measure.symbolId>, int<Measure.typeId>>)
    (facts: TypeFacts)
    : (int<Measure.typeId> * int<Measure.typeId> list option) option =
    match facts.Response.AliasSymbolId, facts.Response.TargetTypeId with
    | ValueSome alias, ValueSome target when target <> facts.Response.TypeId && isAliasObjectShape model facts ->
        match Map.tryFind target model.Types with
        | Some declared ->
            match declared.Response.AliasSymbolId with
            | ValueSome inner when inner <> alias && Map.tryFind inner forms = Some target ->
                let parameters = declParamIds declared

                let arguments =
                    unifyAlias model (Set.ofList parameters) target facts.Response.TypeId
                    |> Option.filter (fun subst -> parameters |> List.forall (fun p -> Map.containsKey p subst))
                    |> Option.map (fun subst -> parameters |> List.map (fun p -> Map.find p subst))

                Some(target, arguments)
            | _ -> None
        | None -> None
    | _ -> None
```

Replace `aliasInstantiationOf` (`:220-288`, doc comment included) with:

```fsharp
/// What an anonymous shape is an application *of*, where this run already declares the alias it
/// came from: the declaration's name, its declared form's id, and the arguments to write it with
/// when they can be recovered (§4.9, and `docs/plans/generator-three-rung.md` §9 blocker 1).
///
/// A generic alias over an intersection or an object literal is erased by the checker:
/// `VarNode<T, this>` arrives as a bare intersection and `Wrap<boolean>` as an anonymous object,
/// neither a reference `instantiationOf` can read. The alias *symbol* survives on the response and
/// is the tie back. Compiler arguments are retained directly; otherwise matching operand and member
/// paths recover a complete, consistent substitution for the declaration parameters.
let internal aliasInstantiationOf
    (model: ShapeModel)
    (forms: Map<int<Measure.symbolId>, int<Measure.typeId>>)
    (facts: TypeFacts)
    : (string * int<Measure.typeId> * int<Measure.typeId> list option) option =
    let recovered (declared: int<Measure.typeId>) (declaredFacts: TypeFacts) =
        let parameters = declParamIds declaredFacts

        facts.Response.AliasArgumentTypeIds
        |> ValueOption.toOption
        |> Option.map Array.toList
        |> Option.filter (fun arguments ->
            arguments.Length = parameters.Length
            && arguments |> List.forall (fun argument -> Map.containsKey argument model.Types))
        |> Option.orElseWith (fun () ->
            unifyAlias model (Set.ofList parameters) declared facts.Response.TypeId
            |> Option.bind (fun subst ->
                // A parameter the body never mentions cannot be recovered, and an
                // application short of an argument is not writable F#.
                if parameters |> List.forall (fun p -> Map.containsKey p subst) then
                    Some(parameters |> List.map (fun p -> Map.find p subst))
                else
                    None))

    match facts.Response.AliasSymbolId with
    | ValueSome alias when isFlattenable model facts ->
        match Map.tryFind alias forms with
        | Some declared when declared <> facts.Response.TypeId ->
            match Map.tryFind declared model.DeclNames, Map.tryFind declared model.Types with
            | Some name, Some declaredFacts -> Some(name, declared, recovered declared declaredFacts)
            | _ -> None
        | _ -> None
    | ValueSome alias when isAliasObjectShape model facts ->
        match Map.tryFind alias forms with
        | Some declared when declared <> facts.Response.TypeId ->
            match Map.tryFind declared model.DeclNames, Map.tryFind declared model.Types with
            | Some name, Some declaredFacts when isAliasObjectForm model declaredFacts ->
                Some(name, declared, recovered declared declaredFacts)
            | _ -> None
        | Some _ -> None
        // A type this run names on its own, an export, is a declaration.
        | None when Map.containsKey facts.Response.TypeId model.DeclNames -> None
        | None ->
            targetApplicationOf model forms facts
            |> Option.bind (fun (declared, arguments) ->
                Map.tryFind declared model.DeclNames |> Option.map (fun name -> name, declared, arguments))
    | ValueSome alias when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
        // A union application writes the arguments its alias carries. An application whose
        // arguments are not all in the table is hoisted on its own.
        match Map.tryFind alias forms with
        | Some declared when declared <> facts.Response.TypeId ->
            match Map.tryFind declared model.DeclNames, Map.tryFind declared model.Types with
            | Some name, Some declaredFacts when
                flag TypeFlags.Union declaredFacts && armsCorrespond model declaredFacts facts
                ->
                let parameters = declParamIds declaredFacts

                facts.Response.AliasArgumentTypeIds
                |> ValueOption.toOption
                |> Option.map Array.toList
                |> Option.filter (fun arguments ->
                    arguments.Length = parameters.Length
                    && arguments |> List.forall (fun argument -> Map.containsKey argument model.Types))
                |> Option.map (fun arguments -> name, declared, Some arguments)
            | _ -> None
        | _ -> None
    | _ -> None
```

- [ ] **Step 6: `needsName` and the walk (`Anonymous.fs`)**

In `needsName`'s union branch (`:483-497`), the guard reads
`&& (aliasInstantiationOf { model with DeclNames = names } aliasForms facts).IsNone`. It keeps that
spelling (an option of a triple is still `.IsNone`). The intersection branch (`:538-545`) also
keeps it.

In the object branch (`:515-537`), after `&& (instantiationOf { model with DeclNames = names } facts).IsNone`, add:

```fsharp
            // An application of a generic object alias this run declares is written as that
            // application. One whose arguments are lost is declared as a copy (`SY008`).
            && (match aliasInstantiationOf { model with DeclNames = names } aliasForms facts with
                | Some(_, _, Some _) -> false
                | Some(_, _, None)
                | None -> true)
```

In the walk, replace the `aliasApplication` match (`:619-641`, from `// An erased alias application:`
through the `| None -> ()` that closes it) with:

```fsharp
                // An erased alias application: hash-consed onto the declaration it applies,
                // with the recovered arguments standing in for the parameters a hoisted
                // shape would have read free. The application map separates reference sites
                // from the declaration candidates in later shaping passes.
                let aliasApplication =
                    aliasInstantiationOf { model with DeclNames = names } aliasForms facts

                match aliasApplication with
                | Some(name, declared, Some arguments) ->
                    names <- Map.add typeId name names
                    declParams <- Map.add typeId arguments declParams
                    aliasApplications <- Map.add typeId declared aliasApplications

                    findings <-
                        findings
                        @ [ Finding.make path (SynthesizeAnonymous.InstantiationNamedOnce name) ]
                | Some(name, _, None) when flag TypeFlags.Object facts ->
                    //FOR-REVIEW an object copy terminates (its members' applications are recognised one level down), so it is kept rather than widened as SY002 does
                    let copied = Map.tryFind typeId names |> Option.defaultValue path

                    findings <-
                        findings
                        @ [ Finding.make copied (SynthesizeAnonymous.AliasApplicationCopied name) ]
                | Some(name, _, None) ->
                    // The alias is recognised but its arguments do not come back out of the
                    // operands, so there is no application to write. Widening here is the
                    // whole point: hoisting is what runs away.
                    findings <-
                        findings
                        @ [ Finding.make path (SynthesizeAnonymous.HoistArgumentsNotRecovered name) ]
                | None -> ()

                let writesApplication =
                    match aliasApplication with
                    | Some(_, _, Some _) -> true
                    | Some(_, _, None) -> not (flag TypeFlags.Object facts)
                    | None -> false
```

Then in the descent (`:654-668`), replace

```fsharp
                elif aliasApplication.IsSome then
                    // Same rule for an erased alias application, and the reason the runaway
                    // stops: the members are the declaration's, so descending into them is
                    // what mints the next strictly larger type.
                    match aliasApplication with
                    | Some(_, Some arguments) ->
                        for argument in arguments do
                            walk (into "Item") order argument
                    | _ -> ()
```

with

```fsharp
                elif writesApplication then
                    // Same rule for an erased alias application, and the reason the runaway
                    // stops: the members are the declaration's, so descending into them is
                    // what mints the next strictly larger type. An argument is named from the
                    // application's position, as an interface instantiation's is.
                    match aliasApplication with
                    | Some(_, _, Some arguments) ->
                        for argument in arguments do
                            walk (nestUnder path "Item") order argument
                    | _ -> ()
```

- [ ] **Step 7: Constraint repair at alias applications (`Spec.fs`, R4)**

In the flattened-intersection reference (`:1952-1963`), replace

```fsharp
                | arguments -> appliedRef ctx model self owner name arguments
            | _ ->
                match reducedOperand model facts with
```

with

```fsharp
                | arguments ->
                    match
                        Map.tryFind facts.Response.TypeId model.AliasApplications
                        |> Option.bind (fun declared -> Map.tryFind declared model.Types)
                    with
                    | Some declared -> appliedRefTo ctx model self owner name (declParamIds declared) arguments
                    | None -> appliedRef ctx model self owner name arguments
            | _ ->
                match reducedOperand model facts with
```

In `objectRef` (`:2081-2094`), replace

```fsharp
        | Some name ->
            let arguments =
                if Map.containsKey facts.Response.TypeId model.AliasApplications then
                    freeParamsOf model facts.Response.TypeId
                else
                    (ownArguments facts
                     @ declParamIds facts
                     @ freeParamsOf model facts.Response.TypeId)
                    |> List.distinct

            match arguments with
            | [] -> FsNamed name, []
            | arguments -> appliedRef ctx model self owner name arguments
```

with

```fsharp
        | Some name ->
            match Map.tryFind facts.Response.TypeId model.AliasApplications with
            | Some declared ->
                // An alias application checks each argument against the alias's own constraints.
                match freeParamsOf model facts.Response.TypeId with
                | [] -> FsNamed name, []
                | arguments ->
                    let parameters =
                        Map.tryFind declared model.Types |> Option.map declParamIds |> Option.defaultValue []

                    appliedRefTo ctx model self owner name parameters arguments
            | None ->
                match
                    (ownArguments facts
                     @ declParamIds facts
                     @ freeParamsOf model facts.Response.TypeId)
                    |> List.distinct
                with
                | [] -> FsNamed name, []
                | arguments -> appliedRef ctx model self owner name arguments
```

- [ ] **Step 8: Run the tests to verify they pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e.alias-application-lab"`
Expected: PASS. Read `golden/alias-application-lab/AliasApplicationLab.fs` in full (the frozen
CLI wrote 520 lines for this lab): every assertion above holds, and `Box2`, `Applied`, `PrivBox`,
`FW.Applied` are still interfaces (Task 3 changes them).

Run: `dotnet fsi build.fsx -- test --quick --filter "shape"` → PASS.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e.declaration-identity-lab"`
→ PASS. The golden reads `abstract config: Config<string>` and loses `module Holder`.

Run: `dotnet fsi build.fsx -- test --quick --filter "catalog"`
Expected: one FAIL, in "renamed exports and generic subpath types share producer identity": the
consumer's `text (config: Identity.Root.TextConfig) : Identity.Root.TextConfig =
Identity.Adapter.Exports.acceptText config` is FS0001. Today the catalogue hash-conses
`acceptText`'s `Config<string>` copy onto the structurally equal `TextConfig` (frozen CLI,
`plan-alias-application/catalog/root`: `abstract config: Identity.Root.TextConfig`). The
application is now `Config<string>`, and `TextConfig` is still a distinct interface. Task 3 makes
`TextConfig` the abbreviation `Config<string>`, which closes it. That is why Tasks 2 and 3 share one
commit. Every other catalog case passes, and `holderConfig` compiles.

Run: `dotnet build Xantham.slnx` → the compile gate compiles both goldens.

Do not run the unfiltered suite or commit yet: Task 3 continues on this tree.

- [ ] **Step 9: Records**

`docs/.ai/plans/generator-type-mapping.md` §4.9 (`:571`), append after the bullet ending "`NodeListOf<'TNode when 'TNode :> Node>` (W7.4, 2026-10-13)." (the constraints bullet):

```markdown
- An application of a generic alias over an object literal is written as an application of that
  alias: `c(x: Wrap<boolean>)` reads `c (x: Wrap<bool>)`, `d<T>(x: Wrap<T>)` reads `Wrap<'T>`, and
  `Wrap<{ n: number }>` at `Fold.w` reads `Wrap<Fold.W.Item>`, its anonymous argument named from the
  application's position. The alias's declared form is the instantiation target its applications
  carry, or an export's declared type for an alias whose body applies another. The arguments are
  checked against the alias's own constraints (`Keyed<Base>`, `TR027`). An application whose
  arguments cannot be recovered is declared as a copy of its members (`SY008`). A callback alias
  still expands to its F# function type (2026-10-15, plan 4).
```

§4.6 (`:385-388`), replace "and a member-level generic intersection alias instantiates by
re-expansion under a suffixed name like any generic alias (§4.9's deferred item)" with "and an
application of a generic intersection alias is written as an application of it (§4.9)".

`docs/.ai/plans/generator-architecture.md`, `:454-458`: replace "instantiations of generic
*aliases* written as applications (they re-expand inline today; instantiations of generic
*interfaces* have been applications since phase E's first step), and" with "and"; and `:667-670`:
replace "Not done: instantiations of a *generic* intersection alias still re-expand under a suffixed
name (`CreateResourceOptions2..4` in `solid-js`), the same deferred item as generic alias
applications generally." with "Generic intersection and object alias applications are written as
applications since 2026-10-15 (plan 4)." Append a section at the end:

```markdown
## Generic alias applications (2026-10-15, plan 4)

An application of a generic alias over an object literal is written as an application of that
alias (§4.9), where it was a per-position copy of the alias's members (`C.X`, `D.X<'T>`,
`Fold.W`/`Fold.W2`). `synthesize-anonymous` reads each object alias's declared form from the
checker: the `Target` its applications carry, or an export's declared type for an alias whose
body applies another (`type Box2<T> = Wrap<T>`). An application's anonymous arguments are named
from its position (`Fold.W.Item`), for intersection applications too, so an exclusive fold's or a
tagged arm's residue sits under its root for `drop-orphan-delegates`. `objectRef` and the
flattened-intersection reference check an application's arguments against the alias's own
constraints (`appliedRefTo`). An application whose arguments neither the alias arguments nor the
members recover is a copy reported as `SY008`. `tests/fixtures/alias-application-lab` pins the
forms, the negatives (`Dict<T>` → `JS.Record`, `Cb<T>` → function type) and the polymorphic
recursion `Nest<T[]>`, which minted ten nested copies before the depth cutoff.
```

`docs/.ai/footguns.md`, Compiler API, after "**Checker alias-symbol identity alone does not name a type.**":

```markdown
**An object alias's declared form is its applications' target, not the smallest id carrying its
symbol.** `Wrap<T>` written inside `d<T>` carries `Wrap`'s symbol and only type-parameter
arguments, exactly as `Wrap`'s declared form does, and the table can hold it first. Every
instantiation of an anonymous object literal carries the literal's declared type as its `Target`.
An alias whose body applies another (`type Box2<T> = Wrap<T>`) carries that other alias's target,
so its own declared form is its export's declared type, and its symbol is the only tie to
`Box2`.
```

- [ ] **Step 10: Hand the tree to Task 3**

Leave the change uncommitted. Task 3 adds the abbreviations, runs the unfiltered suite over both,
and commits them together.

---

### Task 3: An alias whose body applies another is an abbreviation

**Files:**
- Modify: `src/Xantham.Generator/Model.fs:1856-1857` (`ShapeModel`), `src/Xantham.Generator/Pipeline.fs:88-100` (`toShape`), `tests/Xantham.Generator.Tests/Build.fs:99-115` (`shapeModel`)
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs` (walk; `nameAnonymous` state and result)
- Modify: `src/Xantham.Generator/Shape/Interfaces.fs:13-32` (`declaresInterface`)
- Modify: `src/Xantham.Generator/Shape/Aliases.fs:233-237` (`remainingDecls` right side)
- Test: `Pipeline.test.fs` (`alias-application-lab` block), `Shape.test.fs`, `DeclarationCatalog.test.fs` (`sharedConsumer`)
- Regenerate: `golden/alias-application-lab`, `golden/declaration-identity-lab`, `golden/type-fest` (1 re-aliased declaration, Measures)
- Docs: §4.9 bullet, plan 4 section

**Interfaces:**
- Consumes: Task 2's uncommitted tree: `targetApplicationOf`, the object forms, and the walk's
  `aliasApplication`/`writesApplication` bindings.
- Produces: `ShapeModel.AliasBodies: Map<int<typeId>, int<typeId> * int<typeId> list>`
  (declared type of a re-aliasing alias -> the applied alias's declared form and the arguments).

- [ ] **Step 1: Write the failing tests**

Append to the `alias-application-lab` list:

```fsharp
                  testCase "an alias whose body applies another is an abbreviation of the application" <| fun _ ->
                      let source = (rendered ()).Files |> List.head |> snd
                      Expect.stringContains source "type Box2<'T> = Wrap<'T>" "a generic re-aliasing alias"
                      Expect.stringContains source "type Applied = Wrap<string>" "a closed one"
                      Expect.stringContains source "type PrivBox<'T> = Wrap<'T>" "one written without `export`"
                      Expect.stringContains (moduleBodyOf source "FW") "type Applied = FW.Box<string>" "one inside a namespace"
                      Expect.isFalse (source.Replace("\r\n", "\n").Contains "type Applied =\n") "no interface copy remains"
```

In `DeclarationCatalog.test.fs`, append to `sharedConsumer`:

```fsharp
let textIsConfig (config: Identity.Root.TextConfig) : Identity.Root.Config<string> = config
let makeText () : Identity.Root.TextConfig = Identity.Root.TextConfig.Create "text"
```

In `Shape.test.fs`, after the three Task 2 cases:

```fsharp
        testCase "synthesize-anonymous records an alias whose body applies another as its body" <| fun _ ->
            // `type Applied = Wrap<string>`: the declared type 80 carries its own alias symbol
            // (200), no alias arguments, and `Wrap`'s form (50) as its target.
            let model = objectAliasModel 50 60 (ValueSome [| 2 |]) "value"

            let applied =
                { Build.facts
                    { Build.typeResponse 80 TypeFlags.Object with
                        AliasSymbol = ValueSome 200
                        Target = ValueSome 50
                        ObjectFlags = ValueSome(ObjectFlags.Anonymous ||| ObjectFlags.Instantiated) } with
                    Members = [ Build.resolvedMember (Build.symbol 502 "value" SymbolFlags.Property) 1 ] }

            let model =
                { model with
                    Types = Map.add 80<typeId> applied model.Types
                    DeclNames = Map.add 80<typeId> "Applied" model.DeclNames
                    Harvest = { model.Harvest with Exports = model.Harvest.Exports @ [ Build.export "Applied" (Build.symbol 401 "Applied" SymbolFlags.TypeAlias) ] }
                    ExportTypes = Map.add 401<symbolId> { Declared = Some 80<typeId>; Value = None } model.ExportTypes }

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 80<typeId> named.AliasBodies) (Some(50<typeId>, [ 1<typeId> ])) "Wrap applied to string"
            Expect.equal (Map.tryFind 80<typeId> named.DeclNames) (Some "Applied") "the alias keeps its own name"
```

Run: `dotnet fsi build.fsx -- test --quick --filter "generator e2e.alias-application-lab"` →
FAIL on `type Box2<'T> = Wrap<'T>`. The `shape` filter fails to compile (`AliasBodies` is not a
field yet). That counts as the red state for the unit test.

- [ ] **Step 2: `ShapeModel.AliasBodies`**

`Model.fs`, after `AliasApplications: Map<int<typeId>, int<typeId>>`:

```fsharp
        /// Declared type of an alias whose body applies another generic object alias (`type
        /// Applied = Wrap<string>`) -> that alias's declared form and the arguments it is applied
        /// with. `shape-aliases` declares it as an abbreviation of the application.
        AliasBodies: Map<int<typeId>, int<typeId> * int<typeId> list>
```

`Pipeline.fs` `toShape` and `Build.fs` `shapeModel`, after `AliasApplications = Map.empty`:

```fsharp
        AliasBodies = Map.empty
```

- [ ] **Step 3: Record bodies in the walk (`Anonymous.fs`)**

In `nameAnonymous`, after `let mutable aliasApplications = model.AliasApplications`:

```fsharp
    let mutable aliasBodies = model.AliasBodies
```

In the walk, directly after the `let writesApplication = …` binding Task 2 added:

```fsharp
                // A declaration whose body applies another generic object alias is an abbreviation
                // of that application, and reads only its arguments.
                let aliasBody =
                    match aliasApplication with
                    | None when Map.containsKey typeId names ->
                        match targetApplicationOf { model with DeclNames = names } aliasForms facts with
                        | Some(declared, Some arguments) -> Some(declared, arguments)
                        | Some(declared, None) ->
                            let alias = Map.tryFind declared names |> Option.defaultValue path

                            findings <-
                                findings
                                @ [ Finding.make names[typeId] (SynthesizeAnonymous.AliasApplicationCopied alias) ]

                            None
                        | None -> None
                    | _ -> None

                aliasBody |> Option.iter (fun body -> aliasBodies <- Map.add typeId body aliasBodies)
```

In the descent, after the `elif writesApplication then … | _ -> ()` branch, add:

```fsharp
                elif aliasBody.IsSome then
                    for argument in snd aliasBody.Value do
                        walk (into "Item") order argument
```

In the result record, after `AliasApplications = aliasApplications`:

```fsharp
        AliasBodies = aliasBodies
```

- [ ] **Step 4: Declare it as an abbreviation**

`Interfaces.fs` `declaresInterface`, after `&& (instantiationOf model facts).IsNone)`, inside the
same parenthesis (before `|| isFlattenable model facts`), the last conjunct becomes:

```fsharp
     && (instantiationOf model facts).IsNone
     // An alias whose body applies another is `shape-aliases`' abbreviation of that application.
     && not (Map.containsKey facts.Response.TypeId model.AliasBodies))
```

`Aliases.fs` `remainingDecls`, replace

```fsharp
                                                | _ -> typeRefIgnoringSelf ctx scoped name facts
```

with

```fsharp
                                                | _ ->
                                                    match Map.tryFind facts.Response.TypeId model.AliasBodies with
                                                    | Some(declared, arguments) when Map.containsKey declared model.DeclNames ->
                                                        let parameters =
                                                            Map.tryFind declared model.Types
                                                            |> Option.map declParamIds
                                                            |> Option.defaultValue []

                                                        appliedRefTo ctx scoped None name model.DeclNames[declared] parameters arguments
                                                    | _ -> typeRefIgnoringSelf ctx scoped name facts
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e.alias-application-lab"` → PASS.
Read the golden: `Box2`, `Applied`, `PrivBox` and `FW.Applied` are one-line abbreviations with no
`Create` and no nested module. The Task 2 declaration-set assertion still holds (the names are the
same).

Run: `dotnet fsi build.fsx -- test --quick --filter "shape"` → PASS.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e.declaration-identity-lab"`
and `--filter "catalog"` → PASS, including the case Task 2 left failing. The golden reads `type
TextConfig = Config<string>`.
`LeftCallback`/`RightCallback` stay interfaces with `SY008`: `Callback<T>` reads `T` only in
`run<U extends T>`'s constraint, which `unifyAlias` does not pair. The catalog consumer compiles
`textIsConfig` and `makeText` (`TextConfig.Create` resolves through the abbreviation) and still
rejects `invalid` with FS0001.

Run: `dotnet fsi build.fsx -- test` (unfiltered) and `dotnet build Xantham.slnx` → PASS. Every
golden that held a copy moves, and `type-fest` also moves by its one re-aliased declaration. Read the
moved goldens with `golden-diff.py` (Task 1 Step 4's command) and samples (`rtk git diff --
<golden> | head -n 200`). Commit them with this change; Task 5 reads the corpus diff in aggregate.
If a golden moves in a way Task 1 did not predict, stop and report it (Task 1 Step 5).

- [ ] **Step 6: Record and commit Tasks 2 and 3 together**

Append to the §4.9 bullet Task 2 added: "An alias whose body applies another generic object alias
is an abbreviation of that application (`type Applied = Wrap<string>`, `type Box2<'T> =
Wrap<'T>`), as `type SB = G<SB.Item>` is for an interface; where its arguments cannot be recovered
it stays an interface with `SY008`." Append the same sentence to the plan 4 section of
`generator-architecture.md`, naming `ShapeModel.AliasBodies`.

```bash
rtk git add src/Xantham.Generator/Model.fs src/Xantham.Generator/Pipeline.fs src/Xantham.Generator/Shape/Anonymous.fs src/Xantham.Generator/Shape/Spec.fs src/Xantham.Generator/Shape/Interfaces.fs src/Xantham.Generator/Shape/Aliases.fs src/Xantham.Generator/Findings.fs tests/Xantham.Generator.Tests/Findings.test.fs tests/Xantham.Generator.Tests/Build.fs tests/Xantham.Generator.Tests/Shape.test.fs tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs tests/fixtures/alias-application-lab tests/Xantham.Generator.Tests/golden docs/.ai/plans/generator-type-mapping.md docs/.ai/plans/generator-architecture.md docs/.ai/footguns.md
rtk git commit -m "$(cat <<'EOF'
fix(generator): write generic object-alias applications as applications

An application of a generic alias over an object literal is written as an
application of the alias (Wrap<bool>, Box2<float>, Wrap<'T>) instead of a
per-position copy of its members. Anonymous arguments are named from the
application's position, and arguments are checked against the alias's
constraints. Unrecoverable arguments keep a copy, reported as SY008.
`type Applied = Wrap<string>` and `type Box2<T> = Wrap<T>` are abbreviations
of the application their body writes, as `type SB = G<...>` is for a generic
interface.

BREAKING CHANGE: generated bindings no longer declare per-position copies of
generic object aliases (`C.X`, `D.X<'T>`, `Holder.Config`,
`CreateResource.Options2`); a parameter, member or result that read one now
reads the alias's application (`Wrap<bool>`, `Config<string>`,
`ResourceOptions<'T, bool>`). An argument that violates the alias's constraint
is written as the constraint (`Keyed<Base>`, TR027). An alias whose body
applies another is an F# abbreviation (`type TextConfig = Config<string>`),
type-equal to the application; `TextConfig.Create` resolves through it, and
code that relied on it being a distinct interface no longer type-checks.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
EOF
)"
```

---

### Task 4: A referenced group's alias application carries its arguments

**Files:**
- Modify: `src/Xantham.Generator/Shape/Spec.fs:2293-2318` (`referencedRef`)
- Modify: `tests/fixtures/alias-copy-lab/node_modules/alias-copy-dep-lab/index.d.ts`, `tests/fixtures/alias-copy-lab/node_modules/alias-copy-lab/index.d.ts`
- Test: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`alias-copy-lab` block, after the case "a referenced callback alias is named rather than expanded")
- Regenerate: `golden/alias-copy-lab`, `golden/alias-copy-dep-lab`
- Docs: the plan 4 section of `generator-architecture.md`

**Interfaces:**
- Consumes: Task 2 (an entry-group application is already an application; this task is the
  identity-only path for a `reference` group).
- Produces: nothing new by name.

- [ ] **Step 1: Extend the lab**

Append to `alias-copy-dep-lab/index.d.ts`:

```ts
/** A generic alias over an object literal: the dependency ships `WidgetBox<'T>`. */
export type WidgetBox<T> = {
    held: T;
};
```

In `alias-copy-lab/index.d.ts`, change the import to
`import { Formatter, Widget, WidgetBox, WidgetPair } from "alias-copy-dep-lab";`, add a member
`boxed: WidgetBox<Widget>;` to `Panel` after `format: Formatter;`, and append:

```ts
/** An application of the dependency's generic alias over this signature's own parameter. */
export declare function unbox<T>(box: WidgetBox<T>): T;
```

- [ ] **Step 2: Write the failing test**

In the `alias-copy-lab` block, after the case "a referenced callback alias is named rather than
expanded" and before the closing `])`, add:

```fsharp
                      testCase "a referenced generic alias is applied to its arguments" <| fun _ ->
                          let rendered = source ()

                          Expect.stringContains
                              rendered
                              "abstract boxed: AliasCopyDepLab.WidgetBox<AliasCopyDepLab.Widget>"
                              "the alias's arguments, not a bare name of the wrong arity"

                          Expect.stringContains
                              rendered
                              "unbox<'T> (box: AliasCopyDepLab.WidgetBox<'T>) : 'T"
                              "an open application keeps the caller's parameter"
```

and change the first case's
`[ "Panel"; "PanelPair"; "Exports"; "Draft.Panel" ]` expectation only if the regenerated source
declares something else. It must not: the new member and function add no declaration.

Run: `dotnet fsi build.fsx -- test --quick --filter "generator e2e.alias-copy-lab"`
Expected: FAIL on `abstract boxed: AliasCopyDepLab.WidgetBox<AliasCopyDepLab.Widget>`. Today it
reads `abstract boxed: AliasCopyDepLab.WidgetBox`, the bare name (`plan-alias-application/out-x-ref`).

- [ ] **Step 3: Implement (`Spec.fs`, `referencedRef`)**

Replace

```fsharp
    match facts.TypeArguments with
    | [] -> FsNamed name, []
    | arguments ->
        let reference, findings = appliedRef ctx model self owner name arguments
```

with

```fsharp
    // A referenced alias over an object literal is named by its alias symbol, and its arguments
    // are the alias's.
    let arguments =
        match facts.TypeArguments with
        | [] -> facts.AliasTypeArguments
        | arguments -> arguments

    match arguments with
    | [] -> FsNamed name, []
    | arguments ->
        let reference, findings = appliedRef ctx model self owner name arguments
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e.alias-copy"`
Expected: PASS. `golden/alias-copy-dep-lab` declares `type WidgetBox<'T>`. `golden/alias-copy-lab`
reads both strings and carries `ReferencedArityUnconfirmed` (`TR` code per `Findings.fs`) for each
application. `dotnet build Xantham.slnx` compiles both goldens together, so the compile gate checks
the arity.

Run: `dotnet fsi build.fsx -- test` (unfiltered) → PASS, and no other golden moves (a `reference`
group with a generic object alias exists nowhere else in the corpus; if one moves, read it and
record it in the commit body).

- [ ] **Step 5: Record and commit**

Append to the plan 4 section of `generator-architecture.md`:

```markdown
A `reference` group's object alias is identity-only and named by its alias symbol (O7). Its
arguments are the alias's (`AliasTypeArguments`), so `referencedRef` applies them:
`AliasCopyDepLab.WidgetBox<AliasCopyDepLab.Widget>`, where the bare name was FS0033 against the
shipped `WidgetBox<'T>` (`alias-copy-lab`).
```

```bash
rtk git add src/Xantham.Generator/Shape/Spec.fs tests/fixtures/alias-copy-lab tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden/alias-copy-lab tests/Xantham.Generator.Tests/golden/alias-copy-dep-lab docs/.ai/plans/generator-architecture.md
rtk git commit -m "$(cat <<'EOF'
fix(generator): apply a referenced group's alias arguments

A referenced group's generic object alias is named by its alias symbol and
carries its arguments on the alias, so the reference was written bare
(FS0033 against the shipped declaration). It is now applied:
AliasCopyDepLab.WidgetBox<AliasCopyDepLab.Widget>.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
EOF
)"
```

---

### Task 5: Corpus measures and records

**Files:**
- Read: every moved golden, in aggregate.
- Modify: the plan 4 section of `docs/.ai/plans/generator-architecture.md`; this plan's Measures table.

**Interfaces:**
- Consumes: Tasks 2–4 committed; Task 1's `tests/.scratch/alias-application/` baseline.
- Produces: the measured record.

- [ ] **Step 1: Re-measure against the baseline**

```bash
TSJS="$PWD/tests/fixtures/@cloudflare/worker-bundler/node_modules/typescript/lib/typescript.js"
node tests/.scratch/alias-application/census-alias-applications.cjs "$PWD" "$TSJS" > tests/.scratch/alias-application/census-after.txt
python tests/.scratch/alias-application/golden-copies.py "$PWD" > tests/.scratch/alias-application/copies-after.txt
python tests/.scratch/alias-application/golden-diff.py "$PWD" <commit before Task 2> > tests/.scratch/alias-application/golden-diff-final.txt
dotnet fsi build.fsx -- findings > tests/.scratch/alias-application/findings-final.txt
rtk git diff --stat <commit before Task 2> -- tests/Xantham.Generator.Tests/golden
```

Expected: the `copies-after.txt` total falls to the `SY008` residual, and `golden-diff-final.txt`
agrees with Task 1 Step 4's counts. Every `SY008` site is listed by
`dotnet fsi build.fsx -- findings --key SY008`. Each one is either `declaration-identity-lab`'s two
callbacks or carries a one-line reason in the record.

- [ ] **Step 2: Gate**

Run: `dotnet build Xantham.slnx` and `dotnet fsi build.fsx -- test --run-gate`
Expected: PASS. On a compile failure, report the first distinct error codes with one site each.

- [ ] **Step 3: Record**

Append to the plan 4 section of `generator-architecture.md` a "Measured" paragraph:
- per binding, copies removed and their nested declarations;
- `SY001` added, `SY004` removed, `SY008` total, `DD003` delta, `RT001`/`TR002` delta and
  `TR026`/`TR027`/`TR044` added;
- golden line counts of `@cloudflare/workers-types`, `solid-js` and `type-fest`, before and after;
- and the planning estimate beside it (44 copies, 64 instantiations).

Update this plan's Measures table with the measured column.

- [ ] **Step 4: Commit**

```bash
rtk git add docs/.ai/plans/generator-architecture.md docs/.ai/plans/2026-10-15-generic-alias-applications.md
rtk git commit -m "$(cat <<'EOF'
docs(plans): record generic alias application measures

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
EOF
)"
```

---

### Task 6: Core.TS, Node and the catalogue contract (R10)

**Files:**
- Conditional: `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**`, `src/Xantham.Generator/CatalogCompatibility.fs:33`, `tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs`

**Interfaces:**
- Consumes: Tasks 2–5.
- Produces: regenerated libraries only where they moved.

- [ ] **Step 1: Decide the bump**

```bash
rtk git log --oneline --grep "chore: prepare package release" <plan 2's ApiVersion 9 commit>..HEAD
```

If this lists a release, set `ApiVersion = 10` in `CatalogCompatibility.fs:33`, and in
`CatalogCompatibility.test.fs` update the pin plan 2 set to 9 to 10. Otherwise leave it at 9 and
say so in the Task 5 record ("API 9 is unreleased; no released producer carries an object-alias
copy").

- [ ] **Step 2: Regenerate on scratch and compare**

```bash
dotnet fsi build.fsx -- generate --only compiler-lib
dotnet fsi build.fsx -- generate --only node-lib
rtk git diff --stat -- src/Xantham.Fable.Core.TS src/Xantham.Fable.Node
```

Expected from the Measures: no `.fs` change in either. Neither the compiler lib nor `@types/node`
holds a generic object-alias application, and R3 renames none of their names. A change under R4
(an intersection application's argument now constraint-checked) is possible in Node's 10
intersection applications. Read any hunk; anything else is a hand-back.

- [ ] **Step 3: Commit what moved**

If Step 1 bumped, or Step 2 moved a `.fs`, commit Core.TS and Node each in its own commit (precedent
`90c35bc1`/`dae1fe29`). Each commit carries a `BREAKING CHANGE:` paragraph naming the moved
declarations. A migration note goes in that package's `README.md` when a public name changed.
Otherwise revert the regeneration (`rtk git restore -- src/Xantham.Fable.Core.TS
src/Xantham.Fable.Node`) and commit nothing.

```bash
rtk git add src/Xantham.Fable.Core.TS src/Xantham.Generator/CatalogCompatibility.fs tests/Xantham.Generator.Tests/CatalogCompatibility.test.fs
rtk git commit -m "$(cat <<'EOF'
chore(bindings): regenerate Core.TS for generic alias applications

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
EOF
)"
rtk git add src/Xantham.Fable.Node
rtk git commit -m "$(cat <<'EOF'
chore(bindings): regenerate Node for generic alias applications

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
EOF
)"
```

---

## Reported, not in this plan

- **Callback-alias applications (R8).** `type Callback<T> = (self: T) => any` applied as
  `Callback<Timer>` reads `(Timer -> obj)`. F# treats that as the same type, but the alias name is
  lost from the spelling. 160 sites in the corpus goldens and Node (Measures).
- **Ship-group dependency aliases (R9).** Reproducer (`plan-alias-application/lab-x`, `--config
  xantham-ship.json`):

  ```ts
  // node_modules/alias-dep-lab/index.d.ts
  export type DepBox<T> = { held: T };
  export type DepPlain = { p: number };
  // index.d.ts, groups { "alias-dep-lab": "ship" }
  import { DepBox, DepPlain } from "alias-dep-lab";
  export interface Holder { box: DepBox<number>; plain: DepPlain }
  export declare function f<T>(x: DepBox<T>): T;
  ```

  The output is `abstract box: Holder.Box`, `abstract plain: Holder.Plain`, and `f<'T> (x:
  AliasDepLab.F.X<'T>)` with `groups/AliasDepLab.fs` declaring only `module F = type X<'T>`. Neither
  `DepBox` nor `DepPlain` is declared under its own name.
- **Unification through a method's type-parameter constraint.** `Callback<T> = { run<U extends
  T>(value: U): U }` re-aliased as `LeftCallback = Callback<Left>` keeps an interface copy with
  `SY008`, because `unifyAlias` pairs members and operands, not signature constraints.

## Self-review

1. **Spec coverage.** Findings §4 option 1 is covered by Task 2: object forms, `aliasInstantiationOf`
   and `needsName` for `flag Object`, `Wrap<…>`/`Box2<float>`/`Wrap<'T>`, `F1.W`/`F1.W2` never minted,
   and the second arm's argument prunable. §5's test plan is covered: Task 2's `Fold`, `FW` and `Box2`
   cases, and the false-drop guard. Task 2 also covers the brief's risk list: arity and constraints
   (`Pair<float, string>`, `Tagged<float>`, `Keyed<Base>`/TR027); `this` (`Wrap<Chain>`, `Wrap<Link>`);
   recursion (`Tree<float>`, `Nest<'T[]>`, no `RT001`); exclusive fold and tagged arm (`Fold.W.Item`,
   `Tag.Cases.X.W.Item`, `DD003` on `Fold.W.Item2`); the namespace alias (`FW.Box<float>`, no `DD003`
   under `FW.`). Task 4 covers another package; Task 2's `holderConfig` and Task 3's `textIsConfig`
   cover the catalogue. Core.TS has no generic object alias, and Task 6 confirms it. Findings §4
   option 2 (provenance pruning) is plan 2's R11, consumed here. The `:94` predicate (option 3) is
   not used.
2. **Placeholders.** Two values are read off at execution, and both name where they come from:
   `<commit before Task 2>` and `<plan 2's ApiVersion 9 commit>`. The `TR027` symbol carries an
   instruction to check it against the regenerated `symbols.jsonl`.
3. **Type consistency.** `aliasInstantiationOf` returns `(string * int<typeId> * int<typeId> list
   option) option` in Task 2's definition, its `needsName` and walk matches, and Task 3's
   `aliasBody`. `targetApplicationOf` returns `(int<typeId> * int<typeId> list option) option` in
   Task 2 and Task 3. `AliasBodies: Map<int<typeId>, int<typeId> * int<typeId> list>` matches its
   reads in `Interfaces.fs`, `Aliases.fs` and the Shape test.
4. **Review Focus.** Each of the five items has a lab line in Task 2.
