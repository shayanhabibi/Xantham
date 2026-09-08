# Combined Compiler-Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the pinned TypeScript `esnext` and `dom` declaration closure as one generated, compilable `Fable.Core.TS.fs` binding, with independently configurable ES and DOM child modules.

**Architecture:** Keep ordinary group rendering unchanged. Add one `CompilerLibLayout` value that turns optional `compilerLib` configuration into a dotted root module plus two validated one-segment children. Route compiler-library groups through a dedicated one-file `module rec` renderer; it uses full qualified names internally and applies `[<AutoOpen>]` only as requested. A new library project commits and compiles the generated full artifact.

**Tech Stack:** F#/.NET 10, Fable.Core 5.2.0, Expecto, FsLangMCP, TypeScript 7.x, Partas.Build.

**Spec:** `docs/superpowers/specs/2026-09-09-combined-compiler-lib-design.md`

## Global Constraints

- Target Fable 5.x only; pin Fable.Core at 5.2.0 everywhere the artifact is compiled, and target `net8.0` because generated `ParamObject` static interface members require default-interface-member runtime support.
- Preserve ordinary package and dependency module/namespace behavior exactly.
- `compilerLib.module` may be dotted; `esModule` and `domModule` are one valid F# identifier each and must differ.
- Defaults remain `TypeScript.Lib`, `Es`, `Dom`, and both auto-open flags `false`.
- The shipped artifact uses `Fable.Core.TS`, `Es`, `Dom`, `autoOpenEs: true`, and `autoOpenDom: false`.
- Never inspect or diff the generated full binding in chat. Use targeted FsLangMCP checks, file sizes, hashes, build results, and manifest counts instead.
- Before changing an existing public F# signature, run FsLangMCP `fcs_refactor_impact` against that exact symbol and project. Before using semantic `find` negatives, run a scoped `check` for the same project.

---

## File structure

- `src/Xantham.Generator/Model.fs` — config record, JSONC parsing, layout defaults and validation helpers.
- `src/Xantham.Generator/Pipeline.fs` — compiler-library family placement and canonical fully qualified module names.
- `src/Xantham.Generator/Render.fs` — dedicated combined compiler-library file renderer; ordinary namespace rendering remains intact.
- `tests/Xantham.Generator.Tests/Render.test.fs` — byte-level rendering tests using small hand-built models.
- `tests/Xantham.Generator.Tests/Ownership.test.fs` — family-placement and canonical-reference planning tests.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` — JSON config parsing and the small `lib-ship-lab` end-to-end assertion.
- `tests/fixtures/lib-ship-lab/xantham.json` and its golden — compact end-to-end regression fixture; it remains scripthost-only.
- `tools/fable-core-ts-input/` — tracked, minimal package input (`package.json`, `entry.d.ts`, `xantham.json`) driving the full compiler-library artifact.
- `src/Xantham.Fable.Core.TS/` — first-class package project, generated `Fable.Core.TS.fs`, manifest metadata, README, and project file.
- `Xantham.slnx`, `build.fsx`, `xantham.schema.json`, `docs/generator-usage.md`, `docs/.ai/plans/generator-architecture.md` — solution/build/schema/consumer documentation and phase record.

### Task 1: Add the compiler-library layout configuration

**Files:**

- Modify: `src/Xantham.Generator/Model.fs:56-364`
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs:312-427`
- Modify: `docs/generator-usage.md:66-101`
- Modify: `xantham.schema.json` (regenerated, never hand-edited)

**Interfaces:**

- Produces: `CompilerLibLayout.ofConfig : GeneratorConfig -> CompilerLibLayout`.
- Produces: `CompilerLibLayout.RootModule`, `EsModule`, `DomModule`, `AutoOpenEs`, `AutoOpenDom`, `EsQualifiedModule`, and `DomQualifiedModule`.
- Consumed by: `Pipeline.groupModulesForScope` and the dedicated compiler-library renderer.

- [ ] **Step 1: Add failing parser/default tests**

  Add compact cases beside `configTests`; write each JSONC string to the existing temporary-config helper and assert the effective layout:

  ```fsharp
  testCase "compilerLib defaults preserve the established layout" <| fun _ ->
      let config = GeneratorConfig.loadFile configWithoutCompilerLib
      let layout = CompilerLibLayout.ofConfig config
      Expect.equal layout.RootModule "TypeScript.Lib" "root"
      Expect.equal layout.EsModule "Es" "ES child"
      Expect.equal layout.DomModule "Dom" "DOM child"
      Expect.isFalse layout.AutoOpenEs "ES stays explicit by default"
      Expect.isFalse layout.AutoOpenDom "DOM stays explicit by default"

  testCase "compilerLib rejects a dotted family child" <| fun _ ->
      Expect.throwsT<Exception>
          (fun () -> GeneratorConfig.loadFile configWithEsModuleCoreEs |> ignore)
          "family children are one module segment"
  ```

  Cover an omitted object, partial overrides, dotted root, both flags independently, non-object/non-string/non-boolean values, blank values, invalid child identifiers, and equal children.

- [ ] **Step 2: Run the focused test project to confirm the red state**

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "compilerLib"`

  Expected: compile failure because `CompilerLibLayout` and `compilerLib` parsing do not exist.

- [ ] **Step 3: Implement the small layout module and JSONC field**

  In `Model.fs`, add records close to `GeneratorConfig` rather than changing render types:

  ```fsharp
  type CompilerLibConfig =
      { Module: string option
        EsModule: string option
        DomModule: string option
        AutoOpenEs: bool
        AutoOpenDom: bool }

  type CompilerLibLayout =
      { RootModule: string
        EsModule: string
        DomModule: string
        AutoOpenEs: bool
        AutoOpenDom: bool }
  ```

  Add `CompilerLib: CompilerLibConfig` to `GeneratorConfig` with the fully defaulted record in `GeneratorConfig.Default`. Parse only the optional `compilerLib` object in `loadFile`; reject invalid supplied values with `xantham.json: compilerLib...` messages. Put all fallback and validation in `CompilerLibLayout.ofConfig`, including `EsQualifiedModule = RootModule + "." + EsModule` and its DOM counterpart. Reuse/expose the existing identifier-shape validation rather than introduce a second regex.

- [ ] **Step 4: Regenerate schema and add consumer documentation**

  Add the approved configuration example and explain that child names cannot be dotted and auto-open affects consumers only. Regenerate the schema rather than editing it:

  Run: `rtk dotnet run --project src/Xantham.Cli -- schema -o xantham.schema.json`

  Expected: `compilerLib` is an object with the five documented properties in the emitted schema.

- [ ] **Step 5: Run focused verification**

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "compilerLib"`

  Expected: PASS; every default, override, and refusal case is covered.

- [ ] **Step 6: Commit**

  ```bash
  rtk git add src/Xantham.Generator/Model.fs tests/Xantham.Generator.Tests/Pipeline.test.fs docs/generator-usage.md xantham.schema.json
  rtk git commit -m "feat(generator): configure compiler-lib layout"
  ```

### Task 2: Render compiler libraries as one recursive module file

**Files:**

- Modify: `src/Xantham.Generator/Render.fs:796-814,1251-1367`
- Modify: `tests/Xantham.Generator.Tests/Render.test.fs`

**Interfaces:**

- Consumes: planned compiler groups with a common root and one-segment `Es`/`Dom` leaves.
- Produces: one `groups/<root>.fs` headed by `module rec <root>`, containing the two child modules.
- Preserves: `renderNamespace` and `renderModule` behavior for all non-compiler groups.

- [ ] **Step 1: Add a failing renderer contract test**

  Create two small `Render.GroupModule` values whose declarations reference each other; invoke the new compiler-layout rendering entry point and assert the exact structural fragments:

  ```fsharp
  Expect.stringContains source "module rec Fable.Core.TS" "single recursive root"
  Expect.stringContains source "[<AutoOpen>]\nmodule Es =" "only ES auto-opens"
  Expect.stringContains source "module Dom =" "DOM stays scoped"
  Expect.stringContains source "Fable.Core.TS.Dom.DomType" "ES uses canonical DOM name"
  Expect.stringContains source "Fable.Core.TS.Es.EsType" "DOM uses canonical ES name"
  Expect.equal (files |> List.map fst) [ "groups/Fable.Core.TS.fs" ] "one source file"
  ```

- [ ] **Step 2: Run the renderer test to confirm it fails**

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "combined compiler-lib renderer"`

  Expected: FAIL because all namespaced groups currently use `namespace rec` and no compiler-specific renderer exists.

- [ ] **Step 3: Introduce the private compiler-library rendering branch**

  Keep `GroupModule.Module: string`; add only the minimal discriminator needed to identify the two compiler groups (for example, a `CompilerLibFamily: string option` field populated only by Pipeline). Implement a private renderer equivalent to:

  ```fsharp
  let private renderCompilerLib root groups foreignTo =
      let child group =
          let attribute = if group.AutoOpen then "[<AutoOpen>]\n" else ""
          let body, _ = renderBody group (foreignTo group) "    "
          attribute + $"module {ident group.Leaf} =\n" + body
      String.concat "\n" (fileHeader sources $"module rec {root}" @ [ groups |> List.map child |> String.concat "\n\n" ])
  ```

  Use actual layout data rather than hard-coded names. Aggregate erased-union footers once for the file. Route only the two compiler-family groups through this branch; leave ordinary namespaces unchanged.

- [ ] **Step 4: Run renderer and whole generator test projects**

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "render"`

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore`

  Expected: PASS; existing byte-for-byte renderer tests demonstrate no regression outside the new branch.

- [ ] **Step 5: Commit**

  ```bash
  rtk git add src/Xantham.Generator/Render.fs tests/Xantham.Generator.Tests/Render.test.fs
  rtk git commit -m "feat(generator): co-locate compiler-lib families"
  ```

### Task 3: Route ownership and references through the configured layout

**Files:**

- Modify: `src/Xantham.Generator/Pipeline.fs:194-301`
- Modify: `tests/Xantham.Generator.Tests/Ownership.test.fs:92-194`
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs:1660-1756`
- Modify: `tests/fixtures/lib-ship-lab/xantham.json`
- Regenerate: `tests/Xantham.Generator.Tests/golden/lib-ship-lab/groups/TypeScript.Lib.fs`

**Interfaces:**

- Consumes: `CompilerLibLayout` and `Grouping.libFamily` values.
- Produces: ES and DOM planned groups with canonical modules `<root>.<child>`, shared output root, and independent auto-open flags.
- Preserves: declaration ownership and source-order family routing.

- [ ] **Step 1: Add failing placement tests**

  Extend `hookPlacement` with a custom config and assert both family names:

  ```fsharp
  hookPlacement "Fable.Core.TS.Dom" CompilerLib "/compiler/lib.dom.d.ts"
  |> fun (_, actual) -> Expect.equal actual.["Station"] "Fable.Core.TS.Dom" "DOM uses configured child"
  ```

  Add the ES counterpart for `/compiler/lib.esnext.d.ts`; assert generated cross-family type references retain the full configured root rather than the old fixed `TypeScript.Lib` spelling.

- [ ] **Step 2: Run ownership tests to confirm the red state**

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "compiler-lib ownership"`

  Expected: FAIL because `Pipeline.groupModulesForScope` still calls `Naming.compilerLibFamilyModule`.

- [ ] **Step 3: Replace fixed compiler-library naming at the pipeline seam**

  In `groupModulesForScope`, compute `let layout = CompilerLibLayout.ofConfig ctx.Config` once. For `CompilerLib`, set each group's full module to `layout.EsQualifiedModule` or `layout.DomQualifiedModule`, attach the common renderer root/family/auto-open metadata, and keep the existing `Grouping.libFamily` placement decision. Do not change `Naming.groupModule` for any other `PackageId`.

- [ ] **Step 4: Make the compact end-to-end fixture exercise configuration**

  Update `lib-ship-lab/xantham.json` with non-default root and children plus one true and one false auto-open flag. Regenerate only this golden:

  Run: `rtk dotnet fsi build.fsx -- test --quick --update --no-run-gate --filter lib-ship-lab`

  Expected: exactly its one compiler-group source changes; inspect only the changed file path, header, child-module lines, and manifest counts—not the full contents.

- [ ] **Step 5: Add assertions and verify the compact gate**

  Assert one `groups/Fable.Core.TS.fs` output, `module rec Fable.Core.TS`, the requested child modules, independent auto-open attribute placement, and no legacy fixed root. Then run:

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore --filter "lib-ship-lab|compiler-lib ownership"`

  Run: `rtk dotnet build tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj --no-restore -v q`

  Expected: PASS; the compact golden remains type-checkable in the existing compile gate.

- [ ] **Step 6: Commit**

  ```bash
  rtk git add src/Xantham.Generator/Pipeline.fs tests/Xantham.Generator.Tests/Ownership.test.fs tests/Xantham.Generator.Tests/Pipeline.test.fs tests/fixtures/lib-ship-lab/xantham.json tests/Xantham.Generator.Tests/golden/lib-ship-lab
  rtk git commit -m "feat(generator): route compiler libs through configured modules"
  ```

### Task 4: Add the first-class `Fable.Core.TS` artifact and regeneration stage

**Files:**

- Create: `tools/fable-core-ts-input/package.json`
- Create: `tools/fable-core-ts-input/entry.d.ts`
- Create: `tools/fable-core-ts-input/xantham.json`
- Create: `src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj`
- Create: `src/Xantham.Fable.Core.TS/README.md`
- Create: `src/Xantham.Fable.Core.TS/Fable.Core.TS.fs` (generated)
- Create: `src/Xantham.Fable.Core.TS/manifest.json` and `symbols.jsonl` (generated metadata)
- Modify: `Xantham.slnx:11-24`
- Modify: `build.fsx:57-62,104-118,263-310,448-468`

**Interfaces:**

- Consumes: the root `node_modules/typescript` 7.x pin and the tracked minimal input package.
- Produces: the committed package `Xantham.Fable.Core.TS` and its single generated F# binding file.
- Produces: `dotnet fsi build.fsx -- generate --only compiler-lib` as the explicit regeneration command.

- [ ] **Step 1: Add the project and build-routing tests before generation**

  Add a test that parses `tools/fable-core-ts-input/xantham.json` and asserts the release layout:

  ```fsharp
  let layout = CompilerLibLayout.ofConfig (GeneratorConfig.loadFile artifactConfig)
  Expect.equal layout.RootModule "Fable.Core.TS" "published root"
  Expect.isTrue layout.AutoOpenEs "ES is ergonomic at the package root"
  Expect.isFalse layout.AutoOpenDom "DOM remains explicit"
  ```

  Add a build-script-level test only if `build.fsx` already has executable command-routing coverage; otherwise prove the command in Step 5 and record it in the build script's comments.

- [ ] **Step 2: Create the minimal generator input and package project**

  `tools/fable-core-ts-input/package.json` must be a valid local package; `entry.d.ts` must contain only `export {}`. Configure:

  ```jsonc
  {
    "entry": "entry.d.ts",
    "lib": ["esnext", "dom"],
    "groups": { "typescript/lib": "ship" },
    "compilerLib": {
      "module": "Fable.Core.TS",
      "esModule": "Es",
      "domModule": "Dom",
      "autoOpenEs": true,
      "autoOpenDom": false
    }
  }
  ```

  Make the project mirror `Xantham.Fable.Core`'s package metadata but target `net8.0` only: generated `ParamObject` static interface members with bodies cannot compile for `netstandard2.1`. Use these project-specific elements:

  ```xml
  <PackageId>Xantham.Fable.Core.TS</PackageId>
  <Compile Include="Fable.Core.TS.fs" />
  <PackageReference Include="Fable.Core" Version="5.2.0" />
  <ProjectReference Include="..\Xantham.Fable.Core\Xantham.Fable.Core.fsproj" />
  ```

  Document regeneration, the TypeScript pin, `open Fable.Core.TS`, the auto-opened ES surface, and explicit `Fable.Core.TS.Dom` access.

- [ ] **Step 3: Add solution, packaging, and generation integration**

  Add the project to `Xantham.slnx`. Add `Xantham.Fable.Core.TS` to `Spec.publishable` so normal pack/publish selection includes the first-class artifact. Extend the existing `generateOnly` accepted values and help text with `compiler-lib`; add a stage that executes exactly:

  ```fsharp
  run "dotnet run --project src/Xantham.Cli -- generate tools/fable-core-ts-input -o src/Xantham.Fable.Core.TS --config tools/fable-core-ts-input/xantham.json"
  ```

  Keep it opt-in (`--only compiler-lib`) so ordinary wire generation does not unexpectedly regenerate the very large source file.

- [ ] **Step 4: Generate once and inspect bounded evidence**

  Run: `rtk dotnet fsi build.fsx -- generate --only compiler-lib`

  Expected: `src/Xantham.Fable.Core.TS/Fable.Core.TS.fs`, `manifest.json`, and `symbols.jsonl` are written. Check only:

  ```powershell
  Get-Item src/Xantham.Fable.Core.TS/Fable.Core.TS.fs | Select-Object Length
  Get-Content src/Xantham.Fable.Core.TS/Fable.Core.TS.fs -TotalCount 12
  Get-Content src/Xantham.Fable.Core.TS/manifest.json -TotalCount 30
  ```

  Confirm the header is `module rec Fable.Core.TS`, the ES child bears `[<AutoOpen>]`, the DOM child does not, and manifest counts are present. Do not open or grep the whole generated file.

- [ ] **Step 5: Prove determinism and artifact compilation**

  Run the same generation command again, then:

  Run: `rtk git diff --exit-code -- src/Xantham.Fable.Core.TS`

  Run: `rtk dotnet build src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj --no-restore -c Release -v q`

  Expected: empty diff after the second generation and a successful package compilation.

- [ ] **Step 6: Commit**

  ```bash
  rtk git add tools/fable-core-ts-input src/Xantham.Fable.Core.TS Xantham.slnx build.fsx tests/Xantham.Generator.Tests/Pipeline.test.fs
  rtk git commit -m "feat(fable-core-ts): ship generated TypeScript libraries"
  ```

### Task 5: Close the documentation, architecture record, and full verification loop

**Files:**

- Modify: `docs/generator-usage.md`
- Modify: `docs/.ai/plans/generator-architecture.md`
- Modify: `docs/superpowers/specs/2026-09-09-combined-compiler-lib-design.md` only if implementation exposes a design contradiction

**Interfaces:**

- Produces: a documented, reproducible artifact-generation workflow and phase record.
- Verifies: source determinism, F# compilation, full generator suite, and solution compilation.

- [ ] **Step 1: Update the architecture phase record**

  Add a dated entry that records: why the ES/DOM libraries share one recursive source file; the constrained configuration surface; the published `Fable.Core.TS` root; why only ES is auto-opened; the explicit rather than default regeneration stage; and the artifact's manifest counts/version used for the initial generation.

- [ ] **Step 2: Complete consumer documentation**

  Add one concise usage example:

  ```fsharp
  open Fable.Core.TS

  let later : Promise<string> = promise { return "done" }
  let element : Fable.Core.TS.Dom.Element = unbox null
  ```

  State that `Es` is auto-opened by this package configuration while `Dom` deliberately is not.

- [ ] **Step 3: Run semantic and project-level verification**

  First run scoped FsLangMCP checks for `Xantham.Generator.fsproj`, `Xantham.Generator.Tests.fsproj`, and `Xantham.Fable.Core.TS.fsproj`. Then run:

  Run: `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --no-restore -c Release`

  Run: `rtk dotnet build Xantham.slnx --no-restore -c Release -v q`

  Expected: all commands pass. If the generated project creates an F# compiler scalability failure, capture only the diagnostic summary, retain the one-file contract, and investigate with FsLangMCP before altering output semantics.

- [ ] **Step 4: Inspect final changes without reading generated corpus text**

  Run: `rtk git status --short`

  Run: `rtk git diff --stat`

  Run: `rtk git diff --check`

  Expected: only intended source, configuration, project, documentation, and generated artifact files are modified; no whitespace errors.

- [ ] **Step 5: Commit**

  ```bash
  rtk git add docs/generator-usage.md docs/.ai/plans/generator-architecture.md
  rtk git commit -m "docs(generator): record compiler-lib artifact generation"
  ```
