# 0.1.0 release wave: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use `superpowers:subagent-driven-development`
> to implement this plan task by task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the open items on PR #76 (issues #75, #74, #73, #67, #66) and bound #68 and #71,
so `develop` can merge to `master` as 0.1.0.

**Architecture:** Two lanes, each a worktree branched from `develop`. Lane A carries the three
generator features in sequence through the renderer and `Shape/Classes.fs`. Lane B carries the
two simple test-infrastructure nits, then the capped #68 recon after lane A's goldens are
composed. The coordinator pre-declares every finding case in one commit before either lane
branches, merges by regenerating goldens, and ends with a Node regeneration as a measurement.

**Tech Stack:** F# / .NET 10, Expecto, Fable 5.x (`Fable.Core` 5.2.0), TypeScript 7 `tsc --api`,
`build.fsx` (Partas.Build), `fslangmcp` for F# semantics.

**Spec:** This document is the specification. Decisions were settled with the user on
2026-09-12 and are recorded verbatim under *Decisions*.

## Decisions

- Node (`@types/node`, #71) is a stretch goal, not a release blocker. The wave ends with a Node
  regeneration reported as a measurement; `src/Xantham.Fable.Node` stays commented out of
  `Xantham.slnx`.
- Workers are hosted Sonnet 5 subagents, one correction attempt each, then escalate to the
  coordinator. No local inference.
- Lane order: **A** = #75 → #74 → #73. **B** = #67 → #66, then #68 recon last.
- Integration branch is `develop`. Nothing lands on `master` until PR #76 merges.
- #73 covers the re-export alias case only (`declare module "node:x" { export * from "x" }`).
  The type is never relocated or duplicated; only its statics are deduplicated.
- #74's key is `autoOpenExports`, boolean, default `false`.
- #75 uses a constant reserved-case list; tagged unions keep `RequireQualifiedAccess`.
- The coordinator may write to GitHub where natural (issue bodies, PR checklist), with any
  substantial text trimmed to essentials and the full text returned to the user at the end.
- One planning document (this one). Lane reports go to `docs/.ai/handovers/<lane>.md`; each
  lane returns the coordinator at most fifteen lines.

## Global constraints

- Fable 5.x only. `Fable.Core` 5.2.0. TypeScript 7.x pin from `package.json`.
- Follow `AGENTS.md`, `.claude/rules/generator-fixtures.md`, `.claude/rules/comments.md`,
  `.claude/rules/style.md` (Fantomas 8.0.0-beta-001 defaults; Expecto files exempt).
- Every finding case is append-only: union case, `FindingCodes.table` row, and the
  `Findings.test.fs` snapshot line. Codes are pre-declared in Task 0; a lane never invents one.
- Never open a large golden or `symbols.jsonl`; grep it. Report measures: `findings` output
  before and after, tier counts, distinct compiler error codes with one site each.
- Fast loop: `dotnet fsi build.fsx -- test --quick --update --filter "<suite>"`. Full gate before
  every commit that is handed back: `dotnet fsi build.fsx -- test`.
- Every lab fixture is named `<feature>-lab` under `tests/fixtures/`, and registered with one
  `fixtureTests "<name>-lab" (handFixture "<name>-lab") ...` block in `Pipeline.test.fs`.
- All shell commands through `rtk`.
- Each worker commits as soon as its work is coherent, and reports: commit SHA, owned files,
  exact test commands and results, counts that moved, anything unexplained.

## Worker context budget

Assignment text under 2,000 tokens. Load only the task section below plus the named source
anchors. Use `fslangmcp` (`check` first, then `find`, `fcs_file_outline`, `fcs_refactor_impact`)
for F# questions; textual search for Markdown, JSON and `.d.ts`. Do not load Node.fs, any npm
golden, or Wire generated sources.

---

## Task 0: Pre-declare finding cases (coordinator, on `develop`)

**Files:**
- Modify: `src/Xantham.Generator/Findings.fs` — `ClassifyLiteralUnions` union (line ~747),
  `ShapeClasses` union (line ~887), `FindingCodes.table` rows near `"LU.NonStringLiteralCase"`
  (line ~173) and `"SC.EntrypointClassInheritsExn"` (line ~203).
- Modify: `tests/Xantham.Generator.Tests/Findings.test.fs` snapshot (pattern at line ~167:
  `"SC.EntrypointClassInheritsExn SC009 ergonomic"`).

**Produces:**
- `ClassifyLiteralUnions.QualifiedAccessKept of caseName: string` — `[<Ergonomic>]`, code
  `LU002`. Message: `$"single-case string enum keeps RequireQualifiedAccess: case {caseName} is a reserved F# name"`.
- `ShapeClasses.StaticAliasPathCollapsed of specifier: string` — `[<Exact>]`, code `SC010`.
  Message: `$"static also exported from {specifier}; one member emitted under the declaring module's specifier"`.

- [ ] **Step 1: Append the two union cases** with doc comments in the contract style of
  `comments.md` (what the finding asserts, not why the code is right).
- [ ] **Step 2: Append the two `FindingCodes.table` rows** `"LU.QualifiedAccessKept", "LU002"`
  and `"SC.StaticAliasPathCollapsed", "SC010"`.
- [ ] **Step 3: Append the two snapshot lines** in `Findings.test.fs`:
  `"LU.QualifiedAccessKept LU002 ergonomic"` and `"SC.StaticAliasPathCollapsed SC010 exact"`.
- [ ] **Step 4: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "findings"` → PASS.
- [ ] **Step 5: Commit** on `develop`:
  `chore(findings): pre-declare LU002 and SC010 for the 0.1.0 wave`.
- [ ] **Step 6: Create worktrees** `.claude/worktrees/lane-a` and `.claude/worktrees/lane-b`
  from `develop` at that commit; verify with `rtk git -C <wt> merge-base --is-ancestor develop HEAD`.

---

## Lane A

### Task A1: #75 — single-case string enums drop `RequireQualifiedAccess`

**Files:**
- Create: `tests/fixtures/single-case-enum-lab/package.json`, `index.d.ts`, `index.js`
- Modify: `src/Xantham.Generator/Render.fs` — `renderStringEnum` (line ~797 yields
  `"[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"`).
- Modify: `src/Xantham.Generator/Shape/LiteralUnions.fs` — raise `LU.QualifiedAccessKept` where
  a single-case enum is classified. If the single-case enums that reach the renderer are minted
  elsewhere (overload literal splitting in `Shape/Overloads.fs`, `DO002`), raise it there instead
  and say so in the report.
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — append a `fixtureTests` block.
- Test: `tests/Xantham.Generator.Tests/Render.test.fs` if a string-enum unit exists there;
  otherwise the Pipeline block is the test.

**Interfaces:**
- Consumes: `FsStringEnumDecl.Cases: FsUnionCase list` (`Model.fs:1297`);
  `ClassifyLiteralUnions.QualifiedAccessKept` (Task 0).
- Produces: `Render.reservedCaseNames : Set<string>` =
  `set [ "Ok"; "Error"; "Some"; "None"; "ValueSome"; "ValueNone" ]`.

Contract: a string enum with exactly one case renders without `RequireQualifiedAccess` unless
the case's F# identifier is in `reservedCaseNames`. Multi-case enums are unchanged. Tagged
unions (`Render.fs:824`) are unchanged.

- [ ] **Step 1: Write the lab.** `package.json`:
  `{"name":"single-case-enum-lab","version":"1.0.0","type":"module","main":"index.js","types":"index.d.ts"}`.
  `index.d.ts`:

  ```typescript
  // Single-case string enums. Two sources: a one-literal alias, and overloads split
  // by a literal parameter (each arm mints a one-case enum).
  export type Mode = "strict";
  export type Outcome = "Ok";
  export type Level = "low" | "high";
  export function pick(kind: "fast"): number;
  export function pick(kind: "slow"): string;
  export function run(mode: Mode): Outcome;
  export function level(value: Level): Level;
  ```

  `index.js`: `export function pick(kind) { return kind === "fast" ? 1 : "s"; } export function run() { return "Ok"; } export function level(v) { return v; }`.
  First generate once with the fast loop and grep the golden for `StringEnum`. Keep only the
  declarations that actually mint a single-case enum; delete the rest from the lab and record
  which forms did not.
- [ ] **Step 2: Register and write the failing assertions** in `Pipeline.test.fs`:

  ```fsharp
  fixtureTests "single-case-enum-lab" (handFixture "single-case-enum-lab") GeneratorConfig.Default (fun package -> [
      testCase "a single-case string enum is not RequireQualifiedAccess" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
          let source = rendered.Files |> List.head |> snd
          Expect.stringContains source "[<StringEnum(CaseRules.None)>]\ntype Mode =" "single case drops RQA"
          Expect.stringContains source "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]\ntype Level =" "multi case keeps RQA"
          Expect.stringContains source "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]\ntype Outcome =" "reserved case keeps RQA"
      testCase "a reserved single case records LU002" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
          let symbols = rendered.Files |> List.find (fst >> (=) "symbols.jsonl") |> snd
          Expect.stringContains symbols "\"key\":\"LU002\"" "finding recorded" ])
  ```

  Adjust type names to whatever Step 1 showed the lab mints.
- [ ] **Step 3: Run** `rtk dotnet fsi build.fsx -- test --quick --update --filter "single-case-enum-lab"` → FAIL on the first assertion.
- [ ] **Step 4: Implement** `reservedCaseNames` and the branch in `renderStringEnum`; raise
  `QualifiedAccessKept` for the reserved case.
- [ ] **Step 5: Run the fast loop** → PASS. Then `rtk dotnet fsi build.fsx -- findings` and
  `rtk dotnet fsi build.fsx -- test --update`; record which npm goldens moved (`git diff --stat`)
  and grep one moved hunk to confirm it is only the attribute line.
- [ ] **Step 6: Full gate** `rtk dotnet fsi build.fsx -- test` → PASS. Commit:
  `feat(render): drop RequireQualifiedAccess on single-case string enums (#75)`.

### Task A2: #74 — `autoOpenExports` config toggle

**Files:**
- Create: `tests/fixtures/auto-open-exports-lab/package.json`, `index.d.ts`, `index.js`
- Modify: `src/Xantham.Generator/Model.fs` — `GeneratorConfig` record (field with
  `[<Description>]`, `Default`, loader `boolField "autoOpenExports" false` near line 431).
  Follow the `AutoOpenEs` precedent (lines 71–82, 98, 147, 417).
- Modify: `src/Xantham.Cli/Schema.fs` — row near line 95:
  `"AutoOpenExports", ("autoOpenExports", "Mark every generated `Exports` type [<AutoOpen>], so a package's value exports resolve unqualified. Defaults false.")`.
- Modify: `src/Xantham.Generator/Render.fs` — `renderExports` (line ~908) yields `"[<AutoOpen>]"`
  before `"[<Erase>]"` when the flag is set. Thread the flag from `GeneratorConfig` through
  the render model the same way `AutoOpenEs` reaches `CompilerLibLayout` (`Model.fs:147`).
- Modify: `xantham.schema.json` — regenerate with
  `rtk dotnet run --project src/Xantham.Cli -- schema`.
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — append a `fixtureTests` block.

**Interfaces:**
- Produces: `GeneratorConfig.AutoOpenExports: bool`, default `false`.

Contract: with the flag on, every `type Exports` (root and every nested module's) carries
`[<AutoOpen>]`. `[<AutoOpen>]` on a type is legal F# (verified 2026-09-12 in fsi: an unqualified
static-member call resolves). With the flag off, output is byte-identical to before.

- [ ] **Step 1: Write the lab.** `index.d.ts`:

  ```typescript
  export function greet(name: string): string;
  export const version: string;
  ```

  `index.js`: `export function greet(n) { return "hi " + n; } export const version = "1";`.
- [ ] **Step 2: Register with the flag on**, and assert both settings:

  ```fsharp
  fixtureTests "auto-open-exports-lab" (handFixture "auto-open-exports-lab")
      { GeneratorConfig.Default with AutoOpenExports = true } (fun package -> [
      testCase "autoOpenExports marks the Exports type" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate { GeneratorConfig.Default with AutoOpenExports = true } package)
          let source = rendered.Files |> List.head |> snd
          Expect.stringContains source "[<AutoOpen>]\n[<Erase>]\ntype Exports =" "attribute precedes Erase"
      testCase "default leaves Exports qualified" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
          let source = rendered.Files |> List.head |> snd
          Expect.isFalse (source.Contains "[<AutoOpen>]") "no AutoOpen by default" ])
  ```

  Check how the existing block on `resolveNoInfer` (`Pipeline.test.fs:~491`) passes a non-default
  config and mirror it.
- [ ] **Step 3: Run the fast loop** → FAIL (field does not exist).
- [ ] **Step 4: Implement** the field, loader, schema row, and render branch. Regenerate
  `xantham.schema.json`.
- [ ] **Step 5: Fast loop** → PASS. `rtk dotnet fsi build.fsx -- test --update`; assert
  `git diff --stat` shows only the new lab's golden, `xantham.schema.json`, and source.
- [ ] **Step 6: Full gate** → PASS. Commit:
  `feat(config): autoOpenExports marks generated Exports types AutoOpen (#74)`.

### Task A3: #73 — statics re-exported under a second ambient path

**Files:**
- Create: `tests/fixtures/static-reexport-lab/package.json`, `index.d.ts`, `index.js`,
  `ambient-modules.json` (model: `tests/fixtures/export-layout-lab/`).
- Modify: `src/Xantham.Generator/Shape/Classes.fs` — `shapeStatic` and the `statics` map
  (lines ~87–140, ~389). Every `HarvestedExport` occurrence of a class currently appends its
  statics; two export paths for one declaration append twice.
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — append a `fixtureTests` block.

**Interfaces:**
- Consumes: `ShapeClasses.StaticAliasPathCollapsed specifier` (Task 0, `SC010`);
  `bindingOf` (`Shape/Spec.fs:19`), `staticBinding` (`Shape/Classes.fs:34`).

Contract: for one class declaration reached through several export paths, each static is
emitted once. The emitted `Import` specifier is the class's declaring ambient module; when no
export path equals the declaring module, the first path in source order. Each collapsed path
raises `SC010` with owner `Class.member` and the collapsed specifier. Instance members are
unaffected. Two *different* class declarations with the same name in different modules are
not this task; the lab carries one as a negative that must already pass, and if it does not,
the worker reports the failure with the five-line reproducer and removes it from the lab.

- [ ] **Step 1: Write the lab.** `package.json`:
  `{"name":"static-reexport-lab","version":"1.0.0","main":"index.js","types":"index.d.ts"}`.
  `index.d.ts`:

  ```typescript
  // Global script: ambient modules are harvested together.
  declare module "static-reexport-lab" {
      export class Certificate {
          constructor();
          verify(spkac: string): boolean;
          static exportChallenge(spkac: string): string;
      }
  }
  declare module "node:static-reexport-lab" {
      export * from "static-reexport-lab";
  }
  // Negative: a distinct declaration of the same name in another module keeps its own static.
  declare module "static-reexport-lab/other" {
      export class Certificate {
          constructor();
          static exportChallenge(spkac: number): number;
      }
  }
  ```

  `ambient-modules.json`: `{"static-reexport-lab/other":"other.js"}`. `index.js` exports a
  `Certificate` class with a static `exportChallenge`; `other.js` likewise. The `node:` path
  has no runtime file; this lab is compile-gated only, not run-gated.
- [ ] **Step 2: Register and write the failing assertions**:

  ```fsharp
  fixtureTests "static-reexport-lab" (handFixture "static-reexport-lab") GeneratorConfig.Default (fun package -> [
      testCase "a re-exported class emits each static once" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
          let source = rendered.Files |> List.head |> snd
          let hits = System.Text.RegularExpressions.Regex.Matches(source, "static member exportChallenge\\(spkac: string\\)").Count
          Expect.equal hits 1 "one static for the string overload"
          Expect.stringContains source "[<Import(\"Certificate.exportChallenge\", \"static-reexport-lab\")>]" "declaring module wins"
          Expect.isFalse (source.Contains "[<Import(\"Certificate.exportChallenge\", \"node:static-reexport-lab\")>]") "alias path collapsed"
      testCase "the collapsed path records SC010" <| fun _ ->
          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
          let symbols = rendered.Files |> List.find (fst >> (=) "symbols.jsonl") |> snd
          Expect.stringContains symbols "\"key\":\"SC010\"" "finding recorded"
          Expect.stringContains symbols "node:static-reexport-lab" "names the collapsed specifier" ])
  ```

- [ ] **Step 3: Fast loop** → FAIL with two statics (the compile gate would fail with a
  duplicate-member error; the assertion fails first).
- [ ] **Step 4: Implement** in `Classes.fs`: key statics by the class's declaration identity
  (symbol / `TypeId`, not name), group the export occurrences per declaration, pick the winning
  occurrence by the contract above, emit `SC010` for each other occurrence. Use
  `fcs_refactor_impact` on `shapeStatic` before editing.
- [ ] **Step 5: Fast loop** → PASS. `rtk dotnet fsi build.fsx -- findings --key SC010` before
  and after; `rtk dotnet fsi build.fsx -- test --update`; report `git diff --stat` and confirm
  no npm golden lost a static (`findings --key SC002` unchanged).
- [ ] **Step 6: Full gate** → PASS. Commit:
  `fix(shape): emit re-exported class statics once under the declaring module (#73)`.

---

## Lane B

### Task B1: #67 — platform-neutral compiler-lib paths in `symbols.jsonl`

**Files:**
- Modify: `src/Xantham.Generator/Render.fs` — `sourceFile` (line ~1874). After the
  `/node_modules/` branch, rewrite a leading `@typescript/typescript-<rid>/` to `typescript/`.
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — add one assertion to the existing
  `lib-ship-lab` block (or `compiler-lib-ownership-lab`; both goldens carry the rid path today).
- Regenerate: `tests/Xantham.Generator.Tests/golden/lib-ship-lab/symbols.jsonl`,
  `.../compiler-lib-ownership-lab/symbols.jsonl`.

Contract: `file` in `symbols.jsonl` never contains a runtime identifier. A bundled lib file
reports as `node_modules/typescript/lib/<name>.d.ts` on every platform.

- [ ] **Step 1: Write the failing assertion** in the `lib-ship-lab` block:

  ```fsharp
  testCase "symbols report compiler libs without a platform rid" <| fun _ ->
      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
      let symbols = rendered.Files |> List.find (fst >> (=) "symbols.jsonl") |> snd
      Expect.isFalse (symbols.Contains "@typescript/typescript-") "rid stripped"
      Expect.stringContains symbols "node_modules/typescript/lib/" "neutral path"
  ```

- [ ] **Step 2: Fast loop** `--filter "lib-ship-lab"` → FAIL.
- [ ] **Step 3: Implement** in `sourceFile`:

  ```fsharp
  let neutral (relative: string) =
      System.Text.RegularExpressions.Regex.Replace(relative, "^@typescript/typescript-[a-z0-9]+-[a-z0-9]+/", "typescript/")
  ```

  applied to the `node_modules`-relative result so the value becomes
  `node_modules/typescript/lib/...`.
- [ ] **Step 4: Fast loop with `--update`** → PASS; `git diff --stat` shows the two
  `symbols.jsonl` goldens and the source.
- [ ] **Step 5: Full gate** → PASS. Commit:
  `fix(render): report compiler-lib files under a platform-neutral path (#67)`.

### Task B2: #66 — re-enable the declaration-catalog suite behind the compiler guard

**Files:**
- Modify: `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs` — line ~83 has
  `// TODO - unbrick tests` and `// [<Tests>]` above `let tests =`.

Contract: the suite runs whenever `Tsc.locate __SOURCE_DIRECTORY__` finds a compiler, and skips
with the same message the pipeline suites use otherwise (`Pipeline.test.fs:~259`). The suite's
spawned `dotnet build` of a temp consumer relies on `Fable.Core` 5.2.0 being restorable, which
the compile gate already guarantees on any run that built first.

- [ ] **Step 1: Restore** `[<Tests>]` and wrap:

  ```fsharp
  [<Tests>]
  let tests =
      match Tsc.locate __SOURCE_DIRECTORY__ with
      | None ->
          testList "declaration catalog" [
              testCase "skipped - no compiler" <| fun _ ->
                  skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
      | Some _ -> testList "declaration catalog" [ (* existing cases unchanged *) ]
  ```

  Keep the existing case list verbatim; only the guard is new. Check `Tsc.locate`'s exact
  signature with `fslangmcp find` first.
- [ ] **Step 2: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "declaration catalog"`
  → all cases PASS (or a real failure, which is reported with the first distinct error and
  the task stops; do not fix catalog behaviour in this task).
- [ ] **Step 3: Full gate** → PASS. Commit:
  `test(catalog): re-enable the declaration-catalog suite behind the compiler guard (#66)`.

### Task B3: #68 — bounded recon of the solid-js ordering drift on Linux

Runs after lane A is composed onto `develop`. **Hard cap: one worker session, at most 40 tool
calls, no edits outside the single site identified.** If the cap is reached without a single
site, the deliverable is the reproducer note, not a fix.

**Files:**
- Read: `tests/Xantham.Generator.Tests/Pipeline.test.fs:283–293` (the two `skiptest` lines),
  `src/Xantham.Generator/Shape/Ordering.fs`, `src/Xantham.Generator/Harvest.fs`.
- Possibly modify: exactly one of the above, plus the two `skiptest` lines.
- Write: `docs/.ai/handovers/lane-b-solid.md`.

Procedure:
- [ ] **Step 1:** `rtk gh run list --workflow <ci> --limit 5` and pull the last failing
  solid-js diff from the run log (`rtk gh run view <id> --log-failed | grep -A3 solid-js | head -60`).
  If no log carries a diff, note that and continue.
- [ ] **Step 2:** From the diff or by inspection, decide which of these it is: (a) source-file
  order from the compiler program varies by filesystem enumeration and `Harvest` keeps it;
  (b) a string sort using culture comparison; (c) a hash-ordered collection
  (`HashSet`, `Dictionary`, `Seq.distinct` over a non-ordinal key) whose iteration order is
  consumed. Use `fslangmcp find` for `Seq.distinct`, `HashSet`, `dict`, `String.Compare`,
  `StringComparer.CurrentCulture` sites in `Xantham.Generator`.
- [ ] **Step 3:** If exactly one site explains it, fix it with an ordinal sort at that site,
  remove both `skiptest` lines, run `rtk dotnet fsi build.fsx -- test --update` and confirm the
  solid-js golden is byte-identical to the committed one on Windows (`git diff --stat` empty for
  it). Commit: `fix(generator): order <what> ordinally so solid-js is stable across platforms (#68)`.
- [ ] **Step 4:** Otherwise write the handover with the candidate sites and the evidence, leave
  the skips in place, and return.

---

## Coordinator: integration

- [ ] **I1:** Merge lane B's B1 and B2 into `develop` first (they are independent of lane A).
- [ ] **I2:** Merge lane A (A1 → A2 → A3) into `develop`. Resolve any golden or manifest
  conflict by taking either side and running `rtk dotnet fsi build.fsx -- test --update` once.
- [ ] **I3:** Full gate on the composed tree: `rtk dotnet fsi build.fsx -- test --run-gate`.
  Compose the measurements: every count each lane reported must survive. One that does not is
  an interaction between lanes and is the coordinator's to find.
- [ ] **I4:** Dispatch B3 against the composed `develop`.
- [ ] **I5 (measurement, not gate):** regenerate Node:
  `rtk dotnet run --project src/Xantham.Cli -- generate src/Xantham.Fable.Node --config src/Xantham.Fable.Node/xantham.json -o src/Xantham.Fable.Node`,
  then a one-off compile attempt with the project temporarily in a scratch solution or via
  `dotnet build src/Xantham.Fable.Node/Xantham.Fable.Node.fsproj`. Report the distinct error
  codes with one site each, and the manifest tier deltas. Revert any solution edit.
- [ ] **I6:** Update `docs/.ai/plans/generator-architecture.md` phase records for #73 and #75
  behaviour changes in the same commit as the integration merge, per `AGENTS.md`.
- [ ] **I7:** GitHub: post the #73 reproducer as its body; post the Node error-code summary
  on #71; tick the PR #76 checklist boxes for landed issues. Trim to essentials; carry the full
  text into the final report for the user.
- [ ] **I8:** Final report to the user: integration commit, tests run, counts moved per issue,
  Node measurement, anything unexplained, and the untrimmed GitHub text.
