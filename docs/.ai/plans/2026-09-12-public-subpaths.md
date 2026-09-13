# Public subpaths and shallowest-path type homes: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use `superpowers:subagent-driven-development`
> to implement this plan task by task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One generation run enumerates every public subpath of an ESM package's `exports`
map, renders each subpath as a nested module with its own `Exports` type, and homes every
type declaration under the shallowest public path that exports it.

**Architecture:** Bootstrap reads the `exports` map into a list of public paths and creates
the program over all of their declaration files. Harvest runs the existing entry-module
harvest once per public path, tagging subpath exports with the existing
`FromAmbientModule` origin so `Shape/ExportLayout.fs` already turns `pkg/client` into
`Client`. The type-home decision in `Shape/ExportNames.declarationExports` changes from
"first in harvest order" to "shallowest owner path, ordinal tie-break", and applies to
ambient modules too. Nothing else in Shape or Render changes.

**Tech Stack:** F# / .NET 10, Expecto, Fable 5.x (`Fable.Core` 5.2.0), TypeScript 7 pinned
in `package.json`, `build.fsx` (Partas.Build), `fslangmcp` for F# semantics.

**Spec:** the decisions below, settled with the user on 2026-09-12 by grilling. This plan
is the specification; there is no separate spec document.

## Decisions (verbatim outcome of the grilling session)

1. A "public path" is a key of the package.json `exports` map only. A package without an
   `exports` map has one public path, the root.
2. One run, one `module rec` file, one nested module per subpath. `entry` in `xantham.json`
   remains a strict single-path override exactly as today.
3. Depth is the segment count of the public specifier after the package name: `pkg` is 0,
   `pkg/client` is 1. Equal depth ties break by ordinal sort of the full specifier.
4. The shallow rule relocates **types only**. Values keep one `Exports` per owner, each
   importing from its own specifier, exactly as the export-layout plan already specifies.
5. A deeper path that also exports a relocated type emits **nothing** for it. No
   abbreviation, no finding; the manifest `file` column already shows the origin.
6. A relocated type takes the name the shallowest path exports it under.
7. A subpath's declaration file comes from its conditions in the order `types`, `import`,
   `default`, matching the existing root lookup. Wildcard keys (`./features/*`) are skipped
   with a finding.
8. Ambient modules use the same shallow rule. The `@types/node` `node:X`/`X` collapse stays
   as an alias-normalisation pre-step.
9. Subpath owners reuse `FromAmbientModule "pkg/client"`; no new origin case.
10. A type no public path exports stays at the root.
11. A package whose `exports` has subpaths but no `"."` key runs and produces a root module
    with no root `Exports`.
12. Two keys resolving to one declaration file: types home under the ordinal-first key,
    values appear under both.
13. Enumeration is automatic whenever an `exports` map has one or more `./` keys. An
    optional `subpaths` allowlist in `xantham.json` restricts it. No boolean toggle.
14. Findings: wildcard key skipped and key without a declaration file are both Escape. A key
    resolving outside the package directory is a hard failure. Relocation gets no finding.
15. Declaration catalogs: `FSharpName` already carries the dotted nested path since commit
    4d2fede (`Shape/ExportNames.fs` writes `path @ [preferred]`). **No schema change.**
    The catalog `SchemaVersion` stays 1.
16. An ambient `declare module "pkg/client"` and an `exports` key `./client` are the same
    owner and merge into one module.
17. Lands on `develop` after the alpha merge; no compatibility shims.

## Global constraints

- Fable 5.x only. `Fable.Core` 5.2.0. TypeScript 7.x pin from `package.json`.
- Follow `AGENTS.md`, `.claude/rules/generator-fixtures.md`, `.claude/rules/comments.md`,
  `.claude/rules/style.md` (Fantomas 8.0.0-beta-001 defaults; Expecto files exempt).
- Every finding case is append-only: union case, `FindingCodes.table` row,
  `Findings.test.fs` snapshot line. Codes are pre-declared in Task 1; no other task invents one.
- Never open a large golden or `symbols.jsonl`; grep it. Report measures as `findings`
  output before and after, tier counts, and distinct compiler error codes with one site each.
- Fast loop: `rtk dotnet fsi build.fsx -- test --quick --update --filter "<suite>"`.
  Full gate before every commit handed back: `rtk dotnet fsi build.fsx -- test`.
- Every lab fixture is named `<feature>-lab` under `tests/fixtures/`, registered by one
  `fixtureTests "<name>-lab" (handFixture "<name>-lab") ...` block in `Pipeline.test.fs`.
- All shell commands through `rtk`. Use `fslangmcp` (`check` first, then `find`,
  `fcs_file_outline`, `fcs_refactor_impact`) for F# questions.
- Each worker commits as soon as work is coherent and reports: commit SHA, owned files,
  exact test commands and results, counts moved, anything unexplained. Under 400 words.
- Worker context budget: assignment text under 2,000 tokens. Load only the task section
  below plus the named source anchors.

## Module-path rules (authoritative from commit cab5922)

Earlier commits cite "rules 4/5" and "rule 13" from a list that was never written down. This
list is reconstructed from `Shape/ExportLayout.fs`, `Shape/ExportNames.fs`,
`Shape/Ordering.fs`, `Pipeline.moduleSpecifiers`, and the `ambient-module-lab` and
`export-layout-lab` tests. Rules 14 and 15 are new in this plan.

1. A binding is one `module rec <Module>`. The root module name is `module` from
   `xantham.json`, else derived from the package name.
2. Every harvested export has one owner: `EntryModule` for the entry file's module symbol
   and for an ambient specifier equal to the runtime package; `GlobalScope` for ambient
   globals; `AmbientModule specifier` otherwise. (`ExportLayout.ownerOf`)
3. The entry owner's path is empty. Globals are at the root unless an entry owner also
   exists, in which case they are under `Globals`. (`ExportLayout.preferredPath`)
4. An ambient specifier beginning with the runtime package followed by `/` uses the
   remaining segments; any other specifier uses its full path. Splitting: strip a `node:`
   prefix, strip a leading `@`, split on `/` and `:`, strip a `.js`/`.mjs`/`.d.ts` extension
   from each segment, drop a trailing `index` segment, PascalCase each segment
   (`worker_threads` is `WorkerThreads`, `react-dom/client` is `ReactDom.Client`).
5. For `@types/node`, `node:X` and bare `X` are one module imported as `node:X`; when the
   two spellings' export sets disagree, `HG007` is raised. (`Harvest.collapseNodeAliases`)
6. Two owners whose normalised paths collide from different raw spellings (`foo-bar` and
   `foo_bar`, `foo/bar` and `foo:bar`) are separated at the first ambiguous segment by
   `_` plus the first 12 lowercase hex characters of SHA-256 over the owner key; the digest
   extends until unique. Owner keys are `entry`, `global`, `ambient:<specifier>`.
   (`ExportLayout.allocate`)
7. Type declaration names are not reserved as module paths, so a type and a companion
   module share a name. The `Exports` leaf is reserved in every container scope; a root
   `Exports` that collides with a declaration becomes `Exports_<digest>`.
   (`ExportLayout.containerName`)
8. Values render as members of the owner's `Exports` type. A value exported by two owners
   appears in both `Exports` types, each importing from its own specifier.
9. A type declaration renders under its defining export's owner path. **Before this plan:**
   the defining export is the first in harvest order, except that a defining symbol in
   another shipped group wins over an alias. **After this plan:** rule 15.
10. A TypeScript `namespace` nests as a module under the owner path. A contested type name
    nests under its namespace and records `SY004`.
11. Anonymous shapes nest under the member they were read from, beneath the declaring
    type's path.
12. Cross-module references are qualified relative to the root module. Root declarations
    precede specifier modules; specifier modules order by path.
13. Each specifier module opens with a one-line summary carrying the specifier it binds.
    (`Pipeline.moduleSpecifiers`, `Render.renderSources`)
14. **New.** Each `./`-prefixed key of the package.json `exports` map is a public path whose
    owner is `AmbientModule "<runtime>/<key without ./>"`. The `"."` key, or the manifest
    root when the map has no `./` keys, is the entry owner. Keys with `*` are skipped
    (`HG008`); keys whose conditions yield no declaration file are skipped (`HG009`).
15. **New.** When several exports share one declared type, the defining export is the one
    whose owner path is shortest; ties break by ordinal comparison of the owner's
    specifier, then harvest order. The cross-shipped-group preference of rule 9 still
    applies first. Other exports of the same type under a different owner emit nothing.

## File map

| File | Change |
|---|---|
| `src/Xantham.Generator/Findings.fs` | Append `HG008`, `HG009` cases and table rows |
| `tests/Xantham.Generator.Tests/Findings.test.fs` | Append two snapshot lines |
| `src/Xantham.Generator/Model.fs` | `PublicPath` record; `Context.PublicPaths`, `Context.SkippedPaths`; `GeneratorConfig.Subpaths` + loader |
| `src/Xantham.Generator/Bootstrap.fs` | `publicPaths`; `start` creates the program over every path |
| `src/Xantham.Cli/Schema.fs`, `xantham.schema.json` | `subpaths` row |
| `tests/Xantham.Generator.Tests/Bootstrap.test.fs` | Enumeration theories |
| `src/Xantham.Generator/Harvest.fs` | `harvestExports` loops public paths |
| `src/Xantham.Generator/Shape/ExportLayout.fs` | `depthOf`, `ownerSpecifier` |
| `src/Xantham.Generator/Shape/ExportNames.fs` | Shallow defining-export selection |
| `src/Xantham.Generator/Shape/Aliases.fs` | Cross-owner secondary exports emit nothing |
| `tests/fixtures/subpath-lab/` | New lab |
| `tests/Xantham.Generator.Tests/Pipeline.test.fs` | `subpath-lab` block |
| `tests/Xantham.Generator.RunGate/*` | Runtime probes for `subpath-lab` |
| `docs/generator-usage.md`, `docs/.ai/plans/generator-architecture.md` | Docs |

Every pure test fabricates a `Context`; grep `Pipeline.test.fs` for `Session =` to see how
existing tests do it before touching `Context`.

---

## Task 1: pre-declare findings HG008 and HG009

**Files:**
- Modify: `src/Xantham.Generator/Findings.fs` (`HarvestGlobals` union at line ~694;
  `FindingCodes.table` rows at line ~170, after `"HG.AmbientModuleAliasDivergent", "HG007"`)
- Modify: `tests/Xantham.Generator.Tests/Findings.test.fs` (snapshot list at line ~134)

**Interfaces:**
- Produces: `HarvestGlobals.SubpathWildcardSkipped of key: string` (Escape, `HG008`);
  `HarvestGlobals.SubpathWithoutDeclarations of key: string` (Escape, `HG009`).

- [ ] **Step 1: Append snapshot lines** in `Findings.test.fs` directly after
  `"HG.AmbientModuleAliasDivergent HG007 widened"`:

```fsharp
                    "HG.SubpathWildcardSkipped HG008 escape"
                    "HG.SubpathWithoutDeclarations HG009 escape"
```

- [ ] **Step 2: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "findings"` → FAIL
  (two lines missing from the produced table).

- [ ] **Step 3: Append union cases** after `AmbientModuleAliasDivergent` in `HarvestGlobals`,
  with contract-style doc comments:

```fsharp
    /// A public subpath key containing `*`. No module is generated for it.
    | [<Escape>] SubpathWildcardSkipped of key: string
    /// A public subpath key whose conditions supply no declaration file. No module is
    /// generated for it.
    | [<Escape>] SubpathWithoutDeclarations of key: string
```

  and message arms after the `AmbientModuleAliasDivergent` arm:

```fsharp
            | SubpathWildcardSkipped key ->
                $"exports key \"{key}\" skipped - a wildcard names no subpath an import can resolve"
            | SubpathWithoutDeclarations key ->
                $"exports key \"{key}\" skipped - its conditions supply no declaration file"
```

- [ ] **Step 4: Append table rows** after `"HG.AmbientModuleAliasDivergent", "HG007"`:

```fsharp
            "HG.SubpathWildcardSkipped", "HG008"
            "HG.SubpathWithoutDeclarations", "HG009"
```

- [ ] **Step 5: Run** the findings filter again → PASS.
- [ ] **Step 6: Commit** on the feature branch:
  `chore(findings): pre-declare HG008 HG009 for public subpaths`.

---

## Task 2: enumerate public paths in Bootstrap

**Files:**
- Modify: `src/Xantham.Generator/Model.fs` — `GeneratorConfig` record (line ~195),
  `Default` (line ~240), loader (`declarationReferences` parsing at line ~380 is the model),
  `Context` (line ~773).
- Modify: `src/Xantham.Generator/Bootstrap.fs` — new `publicPaths`, `start` (line ~131).
- Modify: `src/Xantham.Cli/Schema.fs` — `configKeys` map near the `AutoOpenExports` row.
- Regenerate: `xantham.schema.json` via `rtk dotnet run --project src/Xantham.Cli -- schema`.
- Test: `tests/Xantham.Generator.Tests/Bootstrap.test.fs`.

**Interfaces:**
- Produces in `Model.fs`:

```fsharp
/// One public import surface of the package: an `exports` map key and the declaration
/// file its conditions select. `Key` is `"."` for the root.
type PublicPath =
    {
        Key: string
        File: string<declFile>
    }
```

  `Context` gains two fields after `EntryFile`:

```fsharp
        /// Every public path the run generates, root first, then subpaths in ordinal key
        /// order. A configured `entry` yields the root alone.
        PublicPaths: PublicPath list
        /// `exports` keys the run skipped, with the finding each raises.
        SkippedPaths: (string * HarvestGlobals) list
```

  `GeneratorConfig` gains `Subpaths: string list option` (JSON key `subpaths`), default
  `None`. Each string is an `exports` key as written, e.g. `"./client"`.

- Produces in `Bootstrap.fs`:

```fsharp
/// The public paths a run generates. With `entry` configured, the root alone over that
/// file. Otherwise the manifest root (`entryFile`) plus every `./` key of the `exports`
/// map, filtered by `Subpaths` when configured. Skipped keys are returned with their
/// finding. A root-less map yields no root path.
val publicPaths : config: GeneratorConfig -> packageDir: string -> PublicPath list * (string * HarvestGlobals) list
```

  `EntryFile` stays and equals the root path's file when a root exists, else the first
  subpath's file (the program needs one file for `getSymbolsInScope`; `harvest-globals`
  reads it).

- [ ] **Step 1: Write failing theories** in `Bootstrap.test.fs`. Add a helper beside
  `selection` that writes the manifest plus every declaration file named, then calls
  `Bootstrap.publicPaths GeneratorConfig.Default package`:

```fsharp
let private enumeration (files: string list) (manifest: string) =
    let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
    Directory.CreateDirectory package |> ignore

    try
        File.WriteAllText(Path.Combine(package, "package.json"), manifest)

        for file in files do
            let path = Path.Combine(package, file)
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, "export declare const value: number;")

        let paths, skipped = Bootstrap.publicPaths GeneratorConfig.Default package

        paths
        |> List.map (fun p -> p.Key, Path.GetRelativePath(package, p.File / uom<declFile>).Replace('\\', '/')),
        skipped |> List.map fst
    finally
        Directory.Delete(package, true)
```

  and the theory:

```fsharp
        testTheory "public paths" [
            """{ "types": "index.d.ts" }""", [ "index.d.ts" ], ([ ".", "index.d.ts" ], [])
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./client": { "types": "./client.d.ts" } } }""",
                [ "root.d.ts"; "client.d.ts" ], ([ ".", "root.d.ts"; "./client", "client.d.ts" ], [])
            """{ "exports": { "./b": { "types": "./b.d.ts" }, "./a": { "import": { "types": "./a.d.ts" } } } }""",
                [ "a.d.ts"; "b.d.ts" ], ([ "./a", "a.d.ts"; "./b", "b.d.ts" ], [])
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./features/*": { "types": "./f/*.d.ts" } } }""",
                [ "root.d.ts" ], ([ ".", "root.d.ts" ], [ "./features/*" ])
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./js": { "default": "./js.js" } } }""",
                [ "root.d.ts" ], ([ ".", "root.d.ts" ], [ "./js" ])
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./client/index.js": { "types": "./client/index.d.ts" } } }""",
                [ "root.d.ts"; "client/index.d.ts" ], ([ ".", "root.d.ts"; "./client/index.js", "client/index.d.ts" ], [])
        ] <| fun (manifest, files, expected) ->
            Expect.equal (enumeration files manifest) expected "root first, subpaths ordinal, skipped keys listed"

        testCase "a configured entry is the root alone" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory(Path.Combine(package, "dist")) |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { ".": { "types": "./dist/root.d.ts" }, "./adapter": { "types": "./dist/adapter.d.ts" } } }""")
                File.WriteAllText(Path.Combine(package, "dist", "root.d.ts"), "export declare const r: number;")
                File.WriteAllText(Path.Combine(package, "dist", "adapter.d.ts"), "export declare const a: number;")
                let config = { GeneratorConfig.Default with Entry = Some "dist/adapter.d.ts" }
                let paths, skipped = Bootstrap.publicPaths config package
                Expect.equal (paths |> List.map _.Key) [ "." ] "entry is the only path"
                Expect.isEmpty skipped "nothing skipped"
            finally
                Directory.Delete(package, true)

        testCase "a subpaths allowlist restricts enumeration and rejects unknown keys" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory package |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { ".": { "types": "./root.d.ts" }, "./a": { "types": "./a.d.ts" }, "./b": { "types": "./b.d.ts" } } }""")
                for f in [ "root.d.ts"; "a.d.ts"; "b.d.ts" ] do
                    File.WriteAllText(Path.Combine(package, f), "export declare const v: number;")
                let config = { GeneratorConfig.Default with Subpaths = Some [ "./b" ] }
                let paths, _ = Bootstrap.publicPaths config package
                Expect.equal (paths |> List.map _.Key) [ "."; "./b" ] "root plus the allowlisted key"
                let bad = { GeneratorConfig.Default with Subpaths = Some [ "./missing" ] }
                Expect.throwsC (fun () -> Bootstrap.publicPaths bad package |> ignore) (fun e ->
                    Expect.stringContains e.Message "./missing" "the unknown key is named")
            finally
                Directory.Delete(package, true)

        testCase "a root-less map yields subpaths and no root" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory package |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { "./a": { "types": "./a.d.ts" } } }""")
                File.WriteAllText(Path.Combine(package, "a.d.ts"), "export declare const v: number;")
                let paths, _ = Bootstrap.publicPaths GeneratorConfig.Default package
                Expect.equal (paths |> List.map _.Key) [ "./a" ] "no root path"
            finally
                Directory.Delete(package, true)
```

  Note that the existing theory "a package without a root requires an explicit entry" tests
  `Bootstrap.entryFile`, which keeps refusing. `publicPaths` catches that refusal only when
  the map has at least one `./` key; a map with no keys at all still fails through
  `entryFile`.

- [ ] **Step 2: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "generator declaration entry"`
  → FAIL (`publicPaths`, `Subpaths` undefined).

- [ ] **Step 3: Add `PublicPath`, `Context` fields, `Subpaths`** in `Model.fs`. `PublicPath`
  goes directly above `Context`. Loader arm, placed after `declarationReferences`:

```fsharp
            let subpaths =
                match doc.RootElement.TryGetProperty "subpaths" with
                | true, value when value.ValueKind = JsonValueKind.Array ->
                    value.EnumerateArray()
                    |> Seq.map (fun item ->
                        if item.ValueKind <> JsonValueKind.String || not (item.GetString().StartsWith "./") then
                            failwith "xantham.json: subpaths must be an array of exports keys beginning with ./"

                        item.GetString())
                    |> Seq.toList
                    |> Some
                | true, _ -> failwith "xantham.json: subpaths must be an array of exports keys beginning with ./"
                | _ -> None
```

  Field with description, after `AutoOpenExports`:

```fsharp
        /// The `exports` keys generated as nested modules. `None` generates every
        /// non-wildcard `./` key.
        [<Description("The package.json exports keys generated as nested modules, each written as in the map \
        (\"./client\"). Omitted, every non-wildcard ./ key is generated. A key absent from the map fails generation.")>]
        Subpaths: string list option
```

- [ ] **Step 4: Implement `publicPaths`** in `Bootstrap.fs` after `resolveEntryFile`. The
  root comes from `resolveEntryFile` when the map has a usable root, reusing the existing
  `findTypes` walk (lift it out of `entryFile` into a private `declarationOf (el: JsonElement) : string option` that returns the `types` string found under `types`, then `import`, then `default`, then any other non-`.` condition, in that order):

```fsharp
/// The public paths a run generates. See `Context.PublicPaths`.
let publicPaths (config: GeneratorConfig) (packageDir: string) : PublicPath list * (string * HarvestGlobals) list =
    let packageDir = Path.GetFullPath packageDir

    let inside (relative: string) =
        let path = Path.GetFullPath(Path.Combine(packageDir, relative))
        let rel = Path.GetRelativePath(packageDir, path)

        if rel = ".." || rel.StartsWith(".." + string Path.DirectorySeparatorChar) then
            failwith $"package.json: exports entry {relative} leaves the package directory"

        path

    match config.Entry with
    | Some _ ->
        [ { Key = "."; File = resolveEntryFile config packageDir * uom<declFile> } ], []
    | None ->
        let keys =
            readManifest packageDir (fun root ->
                match root.TryGetProperty "exports" with
                | true, exports when exports.ValueKind = JsonValueKind.Object ->
                    exports.EnumerateObject()
                    |> Seq.filter (fun p -> p.Name.StartsWith "./")
                    |> Seq.map (fun p -> p.Name, declarationOf p.Value)
                    |> Seq.toList
                    |> Some
                | _ -> None)
            |> Option.defaultValue []

        let hasRoot =
            readManifest packageDir (fun root ->
                match root.TryGetProperty "exports" with
                | true, exports when exports.ValueKind = JsonValueKind.Object ->
                    match exports.TryGetProperty "." with
                    | true, r -> Some(r.ValueKind <> JsonValueKind.Null)
                    | _ -> Some(List.isEmpty keys)
                | _ -> Some true)
            |> Option.defaultValue true

        let selected =
            match config.Subpaths with
            | None -> keys
            | Some wanted ->
                for key in wanted do
                    if not (keys |> List.exists (fun (k, _) -> k = key)) then
                        failwith $"xantham.json: subpaths names \"{key}\", which the exports map does not declare"

                keys |> List.filter (fun (k, _) -> List.contains k wanted)

        let skipped, taken =
            selected
            |> List.sortBy (fun (key, _) -> key)
            |> List.fold
                (fun (skipped, taken) (key, declared) ->
                    if key.Contains '*' then
                        skipped @ [ key, HarvestGlobals.SubpathWildcardSkipped key ], taken
                    else
                        match declared with
                        | None -> skipped @ [ key, HarvestGlobals.SubpathWithoutDeclarations key ], taken
                        | Some file ->
                            let path = inside file

                            if not (File.Exists path) then
                                failwith $"package.json: exports key \"{key}\" names a missing declaration file {file}"

                            skipped, taken @ [ { Key = key; File = path * uom<declFile> } ])
                ([], [])

        let root =
            if hasRoot then
                [ { Key = "."; File = resolveEntryFile config packageDir * uom<declFile> } ]
            else
                []

        root @ taken, skipped
```

  Sorting uses `List.sortBy` on the key string, which is ordinal in .NET (`String.CompareOrdinal`
  is what F# structural comparison of strings uses). Keep it that way; do not use
  `String.Compare`.

- [ ] **Step 5: Thread through `start`.** Replace `let entry = resolveEntryFile config packageDir`
  with:

```fsharp
        let paths, skipped = publicPaths config packageDir

        let entry =
            match paths with
            | [] -> failwith $"package at {packageDir} exposes no public path - set \"entry\" in xantham.json"
            | first :: _ -> first.File / uom<declFile>
```

  and `rootFiles = paths |> List.map (fun p -> DocumentIdentifier.FileName(p.File / uom<declFile>)) |> List.toArray`.
  Fill `PublicPaths = paths` and `SkippedPaths = skipped` in the returned `Context`.

- [ ] **Step 6: Schema row** in `Schema.fs` after `AutoOpenExports`:

```fsharp
            "Subpaths",
            ("subpaths",
             "The package.json exports keys generated as nested modules, each written as in the map (\"./client\"). \
          Omitted, every non-wildcard ./ key is generated. A key absent from the map fails generation.")
```

  Regenerate `xantham.schema.json`: `rtk dotnet run --project src/Xantham.Cli -- schema`.

- [ ] **Step 7: Fix every `Context` construction** the compiler reports. Use
  `fslangmcp fcs_refactor_impact` on `Context` first. Tests fabricate `Context` in
  `Pipeline.test.fs`, `Shape.test.fs`, `Resolve.test.fs`, `Ownership.test.fs` and others;
  add `PublicPaths = [ { Key = "."; File = <the EntryFile value already given> } ]` and
  `SkippedPaths = []` to each.

- [ ] **Step 8: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "generator declaration entry"` → PASS.
  Then `rtk dotnet fsi build.fsx -- test --quick --filter "cli"` → PASS (schema text).
- [ ] **Step 9: Full gate** `rtk dotnet fsi build.fsx -- test` → PASS, no golden moved
  (`rtk git diff --stat` shows only source, tests, schema).
- [ ] **Step 10: Commit**: `feat(bootstrap): enumerate public exports subpaths as run inputs`.

---

## Task 3: harvest every public path, and the `subpath-lab` fixture

**Files:**
- Create: `tests/fixtures/subpath-lab/package.json`, `index.d.ts`, `index.js`,
  `client.d.ts`, `client.js`, `deep.d.ts`, `deep.js`, `shared.d.ts`, `alias.d.ts`,
  `alias.js`, `ambient-modules.json`.
- Modify: `src/Xantham.Generator/Harvest.fs` — `harvestExports` (line ~54).
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — append a `fixtureTests` block.

**Interfaces:**
- Consumes: `Context.PublicPaths`, `Context.SkippedPaths`,
  `GeneratorConfig.runtimePackage : GeneratorConfig -> string<npmDependency> -> string<importSpecifier>` (Model.fs ~487).
- Produces: `HarvestedExport` values whose `Origin` is `FromModule` for key `"."` and
  `FromAmbientModule "<runtime>/<key after ./>"` otherwise. Harvest order within the model:
  root exports, then each subpath in `PublicPaths` order, then `harvest-order` sorts
  everything by source position as today.

- [ ] **Step 1: Write the lab.** `package.json`:

```json
{
  "name": "subpath-lab",
  "version": "1.0.0",
  "type": "module",
  "exports": {
    ".": { "types": "./index.d.ts", "default": "./index.js" },
    "./client": { "types": "./client.d.ts", "default": "./client.js" },
    "./client/deep": { "types": "./deep.d.ts", "default": "./deep.js" },
    "./alias": { "types": "./alias.d.ts", "default": "./alias.js" },
    "./mirror": { "types": "./alias.d.ts", "default": "./alias.js" },
    "./features/*": { "types": "./features/*.d.ts" },
    "./untyped": { "default": "./index.js" },
    "./legacy/index.js": { "types": "./client.d.ts", "default": "./client.js" }
  }
}
```

  `shared.d.ts` (not a public path):

```typescript
export interface Payload { value: string; }
export interface Internal { hidden: number; }
export declare class Session { constructor(); readonly kind: "session"; }
```

  `index.d.ts` (root, depth 0):

```typescript
export { Payload, Session as RootSession } from "./shared.js";
export declare function describe(payload: Payload): string;
```

  `client.d.ts` (depth 1):

```typescript
import { Internal } from "./shared.js";
export { Payload, Session } from "./shared.js";
export interface ClientOptions { retries: number; }
export declare function connect(options: ClientOptions): Internal;
export declare function describe(payload: Payload): string;
```

  `deep.d.ts` (depth 2):

```typescript
export { ClientOptions } from "./client.js";
export interface DeepOnly { level: 2; }
export declare function depth(): number;
```

  `alias.d.ts` (depth 1, exported under two keys):

```typescript
export interface AliasShape { name: string; }
export declare function whoami(): string;
```

  `index.js`:

```javascript
export function describe(payload) { return `root:${payload.value}`; }
```

  `client.js`:

```javascript
export function connect(options) { return { hidden: options.retries }; }
export function describe(payload) { return `client:${payload.value}`; }
```

  `deep.js`:

```javascript
export function depth() { return 2; }
```

  `alias.js`:

```javascript
export function whoami() { return "alias"; }
```

  `ambient-modules.json` (RunGate resolves subpath specifiers through this file):

```json
{
  "subpath-lab/client": "client.js",
  "subpath-lab/client/deep": "deep.js",
  "subpath-lab/alias": "alias.js",
  "subpath-lab/mirror": "alias.js",
  "subpath-lab/legacy/index.js": "client.js"
}
```

- [ ] **Step 2: Register assertions** in `Pipeline.test.fs` (append near the
  `export-layout-lab` block). Task 3 asserts module structure only; Task 4 adds the
  shallow-rule cases to the same block.

```fsharp
        yield!
            fixtureTests "subpath-lab" (handFixture "subpath-lab") GeneratorConfig.Default (fun package ->
                [ testCase "each public subpath is a nested module with its own Exports" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "[<Import(\"describe\", \"subpath-lab\")>]" "root value imports the root"
                      Expect.stringContains source "[<Import(\"describe\", \"subpath-lab/client\")>]" "client value imports the subpath"
                      Expect.stringContains source "[<Import(\"depth\", \"subpath-lab/client/deep\")>]" "deep value imports its subpath"
                      Expect.stringContains source "module Client =" "client module"
                      Expect.stringContains source "module Deep =" "deep module nests under client"
                      Expect.stringContains source "module Legacy =" "trailing index.js key strips to Legacy"

                  testCase "two keys over one file both carry the value surface" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "[<Import(\"whoami\", \"subpath-lab/alias\")>]" "alias key"
                      Expect.stringContains source "[<Import(\"whoami\", \"subpath-lab/mirror\")>]" "mirror key"

                  testCase "wildcard and untyped keys are skipped with findings" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let symbols = rendered.Files |> List.find (fst >> (=) "symbols.jsonl") |> snd
                      Expect.stringContains symbols "\"key\":\"HG008\"" "wildcard skipped"
                      Expect.stringContains symbols "\"key\":\"HG009\"" "untyped skipped"
                      let source = rendered.Files |> List.head |> snd
                      Expect.isFalse (source.Contains "module Features") "no wildcard module"
                      Expect.isFalse (source.Contains "module Untyped") "no untyped module" ])
```

- [ ] **Step 3: Run** `rtk dotnet fsi build.fsx -- test --quick --update --filter "subpath-lab"`
  → FAIL (only the root is harvested).

- [ ] **Step 4: Implement** in `Harvest.harvestExports`. Extract the body that today runs
  over `ctx.EntryFile` into a private function
  `harvestPublicPath (ctx: Context) (origin: ExportOrigin) (file: string<declFile>) : Async<HarvestedExport list * SymbolResponse list>`
  returning the harvested exports and the resolved origin symbols (for `namespacesAmong`).
  The pass then runs:

```fsharp
                    let runtime = GeneratorConfig.runtimePackage ctx.Config ctx.PackageName / uom<importSpecifier>

                    let originOf (path: PublicPath) =
                        if path.Key = "." then
                            FromModule
                        else
                            FromAmbientModule($"{runtime}/{path.Key.Substring 2}" * uom<importSpecifier>)

                    let! perPath =
                        ctx.PublicPaths
                        |> List.map (fun path -> harvestPublicPath ctx (originOf path) path.File)
                        |> Async.Sequential

                    let harvested = perPath |> Array.toList |> List.collect fst
                    let resolvedSymbols = perPath |> Array.toList |> List.collect snd

                    let skipped =
                        ctx.SkippedPaths |> List.map (fun (key, finding) -> Finding.make key finding)
```

  `harvestPublicPath` returns `[], []` when `getSymbolOfSourceFile` is `ValueNone` for that
  file (a global-script subpath contributes nothing; `harvest-globals` still runs off
  `EntryFile` when the whole model is empty, unchanged).

  In-scope namespaces: ask `getSymbolsInScope (SymbolFlags.Module, file, 0)` for every
  path's file and union the results before `namespacesAmong`.

  Return `Degraded(model, skipped)` when `skipped` is nonempty, else `Advanced model`.

- [ ] **Step 5: Run** the fast loop → PASS. Inspect the new golden with grep only:
  `rtk grep -n "^module\|^    module\|Import(" tests/Xantham.Generator.Tests/golden/subpath-lab/SubpathLab.fs`.
- [ ] **Step 6: Full gate** → PASS. `rtk git diff --stat` must show only the new golden and
  sources. Report `rtk dotnet fsi build.fsx -- findings --key HG008` and `--key HG009`.
- [ ] **Step 7: Commit**: `feat(harvest): harvest every public exports subpath as an owner`.

---

## Task 4: shallowest-path type homes

**Files:**
- Modify: `src/Xantham.Generator/Shape/ExportLayout.fs` — after `preferredPath` (line ~95).
- Modify: `src/Xantham.Generator/Shape/ExportNames.fs` — `declarationExports` (line 8).
- Modify: `src/Xantham.Generator/Shape/Aliases.fs` — `aliasDecls` (line ~107).
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` — extend the `subpath-lab` block.
- Test: `tests/Xantham.Generator.Tests/ExportLayoutPaths.test.fs`.

**Interfaces:**
- Produces in `ExportLayout`:

```fsharp
/// The nesting depth of an owner's preferred path: 0 for the entry module and globals.
val depthOf : runtimePackage: string<importSpecifier> -> owner: ExportOwner -> int
/// The ordinal tie-break key of an owner: its specifier, or `""` for the entry module and
/// globals.
val ownerSpecifier : owner: ExportOwner -> string
```

- [ ] **Step 1: Pure tests** in `ExportLayoutPaths.test.fs`:

```fsharp
          testCase "depth counts preferred-path segments and the entry is shallowest" <| fun _ ->
              let depth = ExportLayout.depthOf (runtime "pkg")
              Expect.equal (depth EntryModule) 0 "entry"
              Expect.equal (depth GlobalScope) 0 "global"
              Expect.equal (depth (ambient "pkg/client")) 1 "child"
              Expect.equal (depth (ambient "pkg/client/deep")) 2 "grandchild"
              Expect.equal (depth (ambient "pkg/client/index.js")) 1 "trailing index"
              Expect.equal (depth (ambient "node:stream/web")) 2 "unrelated"

          testCase "owner specifiers order ordinally" <| fun _ ->
              let owners = [ ambient "pkg/mirror"; ambient "pkg/alias" ]
              let sorted = owners |> List.sortBy ExportLayout.ownerSpecifier
              Expect.equal sorted [ ambient "pkg/alias"; ambient "pkg/mirror" ] "ordinal"
```

- [ ] **Step 2: Run** `rtk dotnet fsi build.fsx -- test --quick --filter "Export layout paths"` → FAIL.

- [ ] **Step 3: Implement** in `ExportLayout.fs`:

```fsharp
let depthOf (runtimePackage: string<importSpecifier>) (owner: ExportOwner) : int =
    match owner with
    | EntryModule
    | GlobalScope -> 0
    | AmbientModule _ -> rawPreferredPath runtimePackage true owner |> List.length

let ownerSpecifier (owner: ExportOwner) : string =
    match owner with
    | EntryModule
    | GlobalScope -> ""
    | AmbientModule specifier -> specifier / uom<importSpecifier>
```

- [ ] **Step 4: Run** the pure filter → PASS.

- [ ] **Step 5: Pipeline assertions** appended inside the `subpath-lab` block:

```fsharp
                  testCase "a type exported from root and a subpath is declared at the root" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "\ntype Payload =" "root declaration"
                      Expect.isFalse (source.Contains "    type Payload =") "no nested redeclaration"
                      Expect.isFalse (source.Contains "type Payload = Payload") "no abbreviation under the subpath"

                  testCase "the shallowest path's exported name wins" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "\ntype RootSession" "root exports Session as RootSession"
                      Expect.isFalse (source.Contains "    type Session") "client does not redeclare it"

                  testCase "a type exported by a subpath alone nests under it" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "    type ClientOptions" "ClientOptions declared under Client"
                      Expect.stringContains source "abstract level: " "DeepOnly declared"
                      Expect.stringContains source "(options: ClientOptions)" "client signature reads its own module"

                  testCase "an unexported shared type stays at the root" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      Expect.stringContains source "\ntype Internal =" "root"

                  testCase "equal-depth keys over one file home types under the ordinal-first key" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      let aliasAt = source.IndexOf "module Alias ="
                      let shapeAt = source.IndexOf "type AliasShape"
                      let mirrorAt = source.IndexOf "module Mirror ="
                      Expect.isTrue (aliasAt >= 0 && shapeAt > aliasAt && (mirrorAt < 0 || shapeAt < mirrorAt)) "AliasShape under Alias, not Mirror"
```

- [ ] **Step 6: Run** the `subpath-lab` fast loop → FAIL on the root-declaration cases.

- [ ] **Step 7: Implement** the defining-export choice in `ExportNames.declarationExports`.
  Keep the cross-group `tryFind` exactly as it is; replace `|> Option.defaultValue first` with:

```fsharp
            |> Option.defaultValue (
                exports
                |> List.indexed
                |> List.minBy (fun (index, export) ->
                    let owner = ExportLayout.ownerOf model.RuntimePackage export.Origin
                    ExportLayout.depthOf model.RuntimePackage owner, ExportLayout.ownerSpecifier owner, index)
                |> snd
            )
```

  `ExportLayout` compiles before `ExportNames` in the fsproj, so the reference is legal.

- [ ] **Step 8: Cross-owner secondary exports emit nothing.** In `Aliases.fs` `aliasDecls`,
  before the `match Map.tryFind export.Symbol.SymbolId model.ExportTypes ...`, add:

```fsharp
                                let definingOwner typeId =
                                    Map.tryFind typeId definingExports
                                    |> Option.map (fun defining -> ExportLayout.ownerOf model.RuntimePackage defining.Origin)

                                let owner = ExportLayout.ownerOf model.RuntimePackage export.Origin
```

  and add a first arm to the inner `match Map.tryFind typeId model.DeclNames with`:

```fsharp
                                    | Some _ when definingOwner typeId <> Some owner -> None
```

  This is rule 15's last sentence. The `AbbreviationNameTaken` arm below it still fires for
  same-owner clashes.

- [ ] **Step 9: Run** the `subpath-lab` fast loop → PASS. Then
  `rtk dotnet fsi build.fsx -- test --update` and review `rtk git diff --stat`. Expected
  movers: `subpath-lab` (new cases) and possibly `ambient-module-lab`, `static-reexport-lab`,
  `export-layout-lab`, `@cloudflare/workers-types` where a type was previously homed by
  harvest order under a deeper ambient path. For every moved golden, grep one moved hunk
  and confirm it is a type moving to a shallower module or an abbreviation disappearing.
  Any other kind of change is a stop-and-report.
- [ ] **Step 10: Full gate** → PASS. Report `findings` tier counts before and after for each
  moved golden (`rtk dotnet fsi build.fsx -- findings` output, first line per fixture) and
  `SA001`/`NE001` counts.
- [ ] **Step 11: Commit**: `feat(shape): home each declared type under its shallowest public path`.

---

## Task 5: runtime gate for `subpath-lab`

**Files:**
- Modify: `tests/Xantham.Generator.RunGate/Xantham.Generator.RunGate.fsproj` — add a
  `Compile Include` beside line 48 (`export-layout-lab`) for
  `..\Xantham.Generator.Tests\golden\subpath-lab\SubpathLab.fs`.
- Modify: `tests/Xantham.Generator.RunGate/Program.fs` — add probes beside the
  `LayoutLab` block (line ~1967).

**Interfaces:**
- Consumes: the `subpath-lab` golden from Task 4; `register.mjs` already resolves the
  specifiers listed in `ambient-modules.json`.

- [ ] **Step 1: Link the golden** in the RunGate fsproj.
- [ ] **Step 2: Add probes** after the `LayoutLab` probes, using the same `equal` helper:

```fsharp
    equal "the root describe reaches the root runtime" "root:x" (SubpathLab.Exports.describe (jsOptions<SubpathLab.Payload> (fun p -> p.value <- "x")))
    equal "the client describe reaches the client runtime" "client:x" (SubpathLab.Client.Exports.describe (jsOptions<SubpathLab.Payload> (fun p -> p.value <- "x")))
    equal "the deep subpath reaches its runtime" 2. (SubpathLab.Client.Deep.Exports.depth ())
    equal "alias and mirror both reach the shared runtime" ("alias", "alias") (SubpathLab.Alias.Exports.whoami (), SubpathLab.Mirror.Exports.whoami ())
    equal "a legacy index.js key reaches the client runtime" "client:y" (SubpathLab.Legacy.Exports.describe (jsOptions<SubpathLab.Payload> (fun p -> p.value <- "y")))
    let connected = SubpathLab.Client.Exports.connect (jsOptions<SubpathLab.Client.ClientOptions> (fun o -> o.retries <- 3.))
    equal "a root-homed type flows through a subpath signature" 3. connected.hidden
```

  If `Payload` is rendered as an interface without a settable `value`, construct it as
  `unbox<SubpathLab.Payload> (createObj [ "value" ==> "x" ])` instead; grep
  `Probes.fs` for the helper the existing probes use and follow it.

- [ ] **Step 3: Run** `rtk dotnet fsi build.fsx -- test --run-gate` → PASS. If Fable
  compilation fails, report the first distinct error code and site; do not edit generator
  sources in this task.
- [ ] **Step 4: Commit**: `test(run-gate): exercise subpath-lab across every public path`.

---

## Task 6: documentation and phase record

**Files:**
- Modify: `docs/generator-usage.md` — "Select a declaration entry" (line ~160) and
  "Where a type lives in the binding" (line ~250).
- Modify: `site/content/xantham-cli/guide/usage.md` — mirror the same edits (commit
  4d2fede edited both files together; keep them identical in these sections).
- Modify: `docs/.ai/plans/generator-architecture.md` — append a phase record after
  "0.1.0 release wave (2026-09-12)".
- Modify: `docs/.ai/plans/2026-09-12-public-subpaths.md` — tick the rules list as
  authoritative: replace "reconstructed 2026-09-12" with "authoritative from <commit>".

- [ ] **Step 1: Usage doc.** In "Select a declaration entry" replace the sentence beginning
  "Generate other public entries with separate configurations" and the one after it with:

```markdown
Every `./` key of the `exports` map is generated in the same run as a nested module named
for the key's segments: `"./client"` becomes `Client`, `"./client/deep"` becomes
`Client.Deep`, and a trailing `index` segment or `.js`/`.mjs`/`.d.ts` extension is dropped.
Each key's declaration file comes from its `types`, `import` or `default` condition, in that
order. A wildcard key or a key without a declaration file is skipped and reported in the
manifest. `subpaths` restricts generation to the listed keys. Setting `entry` disables
enumeration and generates the one file it names.
```

  In "Where a type lives in the binding", add two table rows after the subpath row:

```markdown
| An `exports` map subpath (`"./client"`) | One nested module per `/` segment: `Pkg.Client.Exports.connect`. |
| A type exported from several public paths | The shallowest path; ties resolve by ordinal specifier order. Deeper paths do not redeclare or abbreviate it. |
```

- [ ] **Step 2: Architecture phase record.** Append:

```markdown
## Public subpaths and shallowest-path type homes (2026-09-12)

One run enumerates every `./` key of the package.json `exports` map (`Bootstrap.publicPaths`)
and harvests each as an owner, `FromAmbientModule "<runtime>/<key>"`, so the export-layout
allocator renders `pkg/client` as `Client` with its own `Exports`. A key with `*` records
`HG008`; a key whose conditions supply no declaration file records `HG009`; a key resolving
outside the package fails the run. `subpaths` in `xantham.json` restricts the set; `entry`
disables enumeration.

Type declarations home under the shallowest owner path among the exports sharing the
declared type, ties broken by ordinal owner specifier, then harvest order
(`ExportNames.declarationExports`). The cross-shipped-group preference still applies first.
A deeper owner's export of the same type emits nothing (`Aliases`). Ambient modules follow
the same rule. Values are unchanged: one `Exports` per owner.

Catalog rows already carried dotted nested names since the specifier-module change, so
`declarations.json` schema stays at version 1.

Verification: <tests count> Expecto tests, compile gate, run gate. Findings moved:
`HG008` 0 -> 1, `HG009` 0 -> 1 (`subpath-lab`); <list any SA001/NE001 deltas and the goldens
that moved, one line each>.
```

  Fill the angle-bracket placeholders from the Task 4 and Task 5 reports before committing.

- [ ] **Step 3: Commit**: `docs: public subpath enumeration and shallowest-path type homes`.

---

## Coordinator: integration

- [ ] **I1:** Tasks run in sequence on one feature branch `feat/public-subpaths` from
  `develop`; each task depends on the previous one's commit. No parallel lanes.
- [ ] **I2:** After Task 4, the coordinator reads every moved golden's `git diff --stat`
  line and the grep evidence before allowing Task 5.
- [ ] **I3:** Full gate on the composed branch: `rtk dotnet fsi build.fsx -- test --run-gate`.
- [ ] **I4:** Regenerate `@types/node` once as a measurement (not a gate):
  `rtk dotnet run --project src/Xantham.Cli -- generate --config src/Xantham.Fable.Node/xantham.json`
  then `rtk dotnet build src/Xantham.Fable.Node/Xantham.Fable.Node.fsproj`. Report distinct
  error codes with one site each and manifest tier deltas against the counts in the 0.1.0
  phase record (1677 / 1924 / 867 / 356). Revert the regenerated Node files unless the
  user asks to keep them.
- [ ] **I5:** Merge into `develop` with a substantive message. Do not touch `master`.

## Acceptance

Required: `subpath-lab` fails before Task 3 and passes after Task 4; each `./` key yields a
nested module with its own `Exports` importing from its exact specifier; a type shared by
root and subpath is declared once at the root under the root's exported name; a type
exported by one subpath nests under it; an unexported shared type stays at the root; two
keys over one file home types under the ordinal-first key and carry values under both;
wildcard and untyped keys record `HG008`/`HG009` and produce no module; a configured
`entry` still generates a single path; no golden moves except by a type relocating
shallower or an abbreviation disappearing; compile gate and run gate pass.

## Follow-up work (recorded 2026-09-13, after the merge)

Landed complete; these are the items reviews raised and the wave deliberately left.

- **Lib-type aliases keep a subpath home.** `solid-js` declares `type DOMElement = Element`
  in `types/jsx.d.ts`, which no public path exports. It renders as
  `JsxDevRuntime.DOMElement` and root signatures reference it qualified, against decision 10.
  The redundant root abbreviation is gone, and the output compiles. The naming site was not
  located inside `Shape/Anonymous.fs`, `Shape/Aliases.fs` or `Resolve.fs`; start from the
  rendered `Docs` string of the declaration and trace backwards.
- **`RA007` drops rather than renames.** A declaration head that repeats a type-parameter
  name loses the declaration. Renaming the duplicate parameters retains it.
- **The opaque-namespace coverage exemption is broad.** It admits any Module-flagged export
  with no Type flag, no value export and no harvested member. Gating on
  `getExportsOfModule` returning empty is tighter.
- **`Shape/Anonymous.fs` resets the module prefix for symbol-named types only.** A
  path-derived member of a root-homed type keeps the referencing owner's prefix. No fixture
  reaches this.
- **`subpaths` is ignored when `entry` is set**, silently. The JSON schema's `items` carries
  no `^\./` pattern although the loader rejects other strings.
- **Harvest issues one full-scope `getSymbolsInScope` per public path.** Several keys
  resolving to one file repeat the query.
- **Root-less maps, two keys over one module path, and `subpaths`** are covered by unit
  tests over `Bootstrap.publicPaths` rather than end to end.
- **`@types/node` was not regenerated** as a measurement for this wave.
