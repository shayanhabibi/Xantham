/// The `xantham` command against the same fixtures the golden corpus is generated from.
///
/// The acceptance test is byte identity: what `xantham generate` writes to disk equals what
/// `Pipeline.generate` hands the harness, file for file. The command is invoked in process,
/// over string writers, so the bytes compared here are the bytes the process writes.
module Xantham.Generator.Tests.CliTests

open System
open System.IO
open System.Text
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator
open Xantham.Generator.Measure

let private root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let private required =
    match Environment.GetEnvironmentVariable "XANTHAM_REQUIRE_TSC" with
    | null
    | ""
    | "0"
    | "false" -> false
    | _ -> true

/// Runs the command in a scratch output directory, handing the test the exit code, the two
/// streams and the directory.
let private invoke (args: string list) (test: int * string * string * string -> 'T) : 'T =
    let outDir =
        Path.Combine(Path.GetTempPath(), "xantham-cli-" + Guid.NewGuid().ToString "N")

    let out = new StringWriter()
    let err = new StringWriter()

    try
        let code = Xantham.Cli.Program.run out err (Array.ofList (args @ [ "-o"; outDir ]))
        test (code, out.ToString(), err.ToString(), outDir)
    finally
        if Directory.Exists outDir then
            Directory.Delete(outDir, true)

let private refusesConfig (message: string) (settings: string) =
    let configDir = Path.Combine(Path.GetTempPath(), "xantham-entry-config-" + Guid.NewGuid().ToString "N")
    Directory.CreateDirectory configDir |> ignore

    try
        let config = Path.Combine(configDir, "selection.json")
        File.WriteAllText(config, "{ " + settings + " }")

        invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "entry-selection-lab"); "--config"; config ]
        <| fun (code, _, err, outDir) ->
            (3, true, false), (code, err.Contains message, Directory.Exists outDir)
    finally
        Directory.Delete(configDir, true)

/// Every file the command wrote, against what the pipeline renders for the same package and
/// configuration.
let private matchesPipeline (fixture: string) =
    let package = Path.Combine(root, "tests", "fixtures", fixture)
    let config = GeneratorConfig.load package

    invoke [ "generate"; package ]
    <| fun (code, out, _, outDir) ->
        Expect.equal code 0 "the command reports success"

        let rendered = Async.RunSynchronously(Pipeline.generate config package)

        let written =
            Directory.GetFiles(outDir, "*", SearchOption.AllDirectories)
            |> Array.map (fun path -> Path.GetRelativePath(outDir, path).Replace('\\', '/'))
            |> Array.sort

        Expect.equal written (rendered.Files |> List.map fst |> List.sort |> Array.ofList) "the same files"

        for name, content in rendered.Files do
            let path = Path.Combine(outDir, name.Replace('/', Path.DirectorySeparatorChar))

            Expect.equal
                (File.ReadAllBytes path)
                (Encoding.UTF8.GetBytes content)
                $"{fixture}/{name} written byte for byte as the pipeline renders it"

        for name, _ in rendered.Files do
            Expect.stringContains out (Path.GetFileName name) "the written path is on standard output"

[<Tests>]
let schemaTests =
    let committed = Path.Combine(root, "xantham.schema.json")

    testList "generator cli schema" [
        // The schema is emitted from `GeneratorConfig`, `GroupDisposition` and `MappedName`,
        // so a key added to the record with no entry in `Schema.fs`'s table fails here rather
        // than shipping a schema that rejects the key the loader reads.
        testCase "the committed schema is what the config record emits" <| fun _ ->
            let emitted = Xantham.Cli.Schema.json ()
            let text = File.ReadAllText(committed).Replace("\r\n", "\n")

            if emitted <> text then
                failtest
                    "xantham.schema.json is not what Schema.json() emits. Regenerate it with \
                     `dotnet fsi build.fsx -- generate --only schema`."

        testCase "the schema command binds and writes its optional output path" <| fun _ ->
            let destination = Path.Combine(Path.GetTempPath(), "xantham-schema-" + Guid.NewGuid().ToString "N")
            let out = new StringWriter()
            let err = new StringWriter()

            try
                let code = Xantham.Cli.Program.run out err [| "schema"; "-o"; destination |]

                Expect.equal code 0 "the command accepts its optional path"
                Expect.isEmpty (err.ToString()) "a successful write has no diagnostics"
                Expect.equal (File.ReadAllText destination) (Xantham.Cli.Schema.json ()) "the requested file is the emitted schema"
            finally
                if File.Exists destination then
                    File.Delete destination

        testCase "every disposition the schema offers is one the loader accepts" <| fun _ ->
            use doc = Text.Json.JsonDocument.Parse(File.ReadAllText committed)

            let offered =
                doc
                    .RootElement.GetProperty("$defs")
                    .GetProperty("disposition")
                    .GetProperty("oneOf")
                    .EnumerateArray()
                |> Seq.collect (fun form ->
                    match form.TryGetProperty "enum" with
                    | true, values -> values.EnumerateArray() |> Seq.map _.GetString()
                    | _ -> Seq.empty)
                |> Seq.toList

            Expect.isNonEmpty offered "the schema offers at least one disposition"

            let dir =
                Path.Combine(Path.GetTempPath(), "xantham-schema-" + Guid.NewGuid().ToString "N")

            Directory.CreateDirectory dir |> ignore

            try
                for name in offered do
                    File.WriteAllText(Path.Combine(dir, "xantham.json"), $"""{{ "groups": {{ "dep": "{name}" }} }}""")

                    let config = GeneratorConfig.load dir
                    Expect.isTrue (Map.containsKey ("dep" * uom<npmDependency>) config.Groups) $"the loader accepts '{name}'"
            finally
                Directory.Delete(dir, true)
    ]

[<Tests>]
let commandTests =
    testList "generator cli" [
        let inline (=!>) entry message = entry, message

        // TODO - FSharp.SystemCommandLine parsing causes earlier failure due to non existent file.
        //          So these all fail correctly, but not for the reasons we originally prepared the tests for.
        // testTheory "invalid declaration inputs are configuration errors" [
        //     "null" =!> "entry must be a string"
        //     "42" =!> "entry must be a string"
        //     "true" =!> "entry must be a string"
        //     "[]" =!> "entry must be a string"
        //     "{}" =!> "entry must be a string"
        //     "\"\"" =!> "entry must be a nonempty relative path"
        //     "\" \"" =!> "entry must be a nonempty relative path"
        //     (Text.Json.JsonSerializer.Serialize(Path.GetTempPath())) =!> "entry must be a relative path"
        //     "\"../outside.d.ts\"" =!> "entry must stay within the package directory"
        //     "\"package.json\"" =!> "entry must name a TypeScript file"
        //     "\"dist/missing.d.ts\"" =!> "entry file does not exist"
        // ] <| fun (entry, message) ->
        //     refusesConfig message ("\"entry\": " + entry)
        //     ||> Flip.Expect.equal "selection fails before generation writes output"
        //
        // testTheory "invalid runtime imports are configuration errors" [
        //     "null" =!> "runtime must be a string"
        //     "42" =!> "runtime must be a string"
        //     "true" =!> "runtime must be a string"
        //     "[]" =!> "runtime must be a string"
        //     "{}" =!> "runtime must be a string"
        //     "\"\"" =!> "runtime must be a nonempty string"
        //     "\" \"" =!> "runtime must be a nonempty string"
        // ] <| fun (runtime, message) ->
        //     refusesConfig message ("\"entry\": \"dist/adapter.d.ts\", \"runtime\": " + runtime)
        //     ||> Flip.Expect.equal "an invalid import cannot fall back to the package root"
        //
        // testTheory "invalid ambient type packages are configuration errors" [
        //     "null" =!> "types must be an array of nonempty strings"
        //     "42" =!> "types must be an array of nonempty strings"
        //     "\"provider\"" =!> "types must be an array of nonempty strings"
        //     "[42]" =!> "types must be an array of nonempty strings"
        //     "[\"\"]" =!> "types must be an array of nonempty strings"
        //     "[\" \"]" =!> "types must be an array of nonempty strings"
        // ] <| fun (types, message) ->
        //     refusesConfig message ("\"types\": " + types)
        //     ||> Flip.Expect.equal "an invalid ambient selection cannot fall back to automatic discovery"

        testCase "a path with no directory is refused" <| fun _ ->
            invoke [ "generate"; Path.Combine(root, "no-such-package") ]
            <| fun (code, _, err, _) ->
                Expect.equal code 2 "no package"
                Expect.stringContains err "no directory at" "the path is named"

        testCase "a directory with no package manifest is refused" <| fun _ ->
            invoke [ "generate"; Path.Combine(root, "tests", "fixtures") ]
            <| fun (code, _, err, _) ->
                Expect.equal code 2 "no package"
                Expect.stringContains err "package.json" "the missing file is named"

        testCase "an unknown option is a usage error" <| fun _ ->
            invoke [ "generate"; "--bogus" ]
            <| fun (code, _, err, _) ->
                Expect.equal code 2 "usage"

        testCase "an unknown command is a usage error" <| fun _ ->
            invoke [ "compile" ] <| fun (code, _, _, _) -> Expect.equal code 1 "usage"

        // TODO - needs to be updated for new CLI
        // testCase "a refused xantham.json exits before generation" <| fun _ ->
        //     let package =
        //         Path.Combine(Path.GetTempPath(), "xantham-cli-cfg-" + Guid.NewGuid().ToString "N")
        //
        //     Directory.CreateDirectory package |> ignore
        //
        //     try
        //         File.WriteAllText(Path.Combine(package, "package.json"), """{ "name": "cfg", "types": "index.d.ts" }""")
        //         File.WriteAllText(Path.Combine(package, "index.d.ts"), "export declare const one: number;\n")
        //         File.WriteAllText(Path.Combine(package, "xantham.json"), """{ "groups": { "dep": "nonsense" } }""")
        //
        //         invoke [ "generate"; package ]
        //         <| fun (code, _, err, _) ->
        //             Expect.equal code 3 "configuration refused"
        //             Expect.stringContains err "unknown disposition" "the loader's own refusal is reported"
        //     finally
        //         Directory.Delete(package, true)
    ]

[<Tests>]
let generationTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "generator cli generation" [
            testCase "cli generation skipped - no compiler" <| fun _ ->
                if required then
                    failtest "XANTHAM_REQUIRE_TSC is set and no tsc was found"
                else
                    skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE"
        ]
    | Some _ ->
        testList "generator cli generation" [
            let inline (==>) entry runtime = entry, runtime

            testTheory "an explicit declaration opens a package with only named subpaths" [
                "adapter.d.mts" ==> "adapter.mjs"
                "adapter.d.cts" ==> "adapter.cjs"
            ] <| fun (entry, runtime) ->
                let package = Path.Combine(root, "tests", "fixtures", "entry-selection-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory package |> ignore

                try
                    File.WriteAllText(Path.Combine(package, "package.json"),
                        $$"""{ "name": "subpath-only", "exports": { "./adapter": { "types": "./{{entry}}", "default": "./{{runtime}}" } } }""")
                    File.WriteAllText(Path.Combine(package, entry), "export declare function adapt(value: string): string;")
                    File.WriteAllText(Path.Combine(package, "xantham.json"),
                        $$"""{ "entry": "{{entry}}", "runtime": "subpath-only/adapter", "module": "SubpathAdapter" }""")

                    invoke [ "generate"; package ]
                    <| fun (code, _, _, outDir) ->
                        code |> Flip.Expect.equal "an explicit input bypasses root discovery" 0
                        File.ReadAllText(Path.Combine(outDir, "SubpathAdapter.fs"))
                        |> fun source -> source.Contains "Import(\"adapt\", \"subpath-only/adapter\")"
                        |> Flip.Expect.equal "the emitted import addresses the selected public subpath" true
                finally
                    Directory.Delete(package, true)

            testCase "an external config selects a declaration and its public runtime import" <| fun _ ->
                let package = Path.Combine(root, "tests", "fixtures", "entry-selection-lab")
                let configDir = Path.Combine(Path.GetTempPath(), "xantham-entry-config-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory configDir |> ignore

                try
                    let config = Path.Combine(configDir, "adapter.json")
                    File.Copy(Path.Combine(package, "xantham.json"), config)

                    invoke [ "generate"; package; "--config"; config ]
                    <| fun (code, _, _, outDir) ->
                        code |> Flip.Expect.equal "the configured declaration exists under the package" 0
                        let source = Directory.GetFiles(outDir, "*.fs") |> Array.map File.ReadAllText |> String.concat "\n"
                        source.Contains "module rec EntrySelectionLab.Adapter"
                        |> Flip.Expect.equal "module controls the F# name" true
                        source.Contains "adapterValue"
                        |> Flip.Expect.equal "entry selects the adapter declaration" true
                        source.Contains "rootValue"
                        |> Flip.Expect.equal "the package root is a separate generation input" false
                        source.Contains "Import(\"adapterValue\", \"entry-selection-lab/adapter\")"
                        |> Flip.Expect.equal "runtime preserves the public JavaScript subpath" true
                finally
                    Directory.Delete(configDir, true)

            testCase "the command writes what the pipeline renders, for a package with no configuration" <| fun _ ->
                matchesPipeline "lab"

            // `group-map-lab` carries an `xantham.json`, so this is also the discovery test:
            // the pipeline is handed `GeneratorConfig.load`, and the command is handed nothing
            // but the package directory.
            testCase "the command writes what the pipeline renders, for a configured package" <| fun _ ->
                matchesPipeline "group-map-lab"

            testCase "the configuration beside the package is the one the command runs with" <| fun _ ->
                let package = Path.Combine(root, "tests", "fixtures", "group-map-lab")
                let configured = Async.RunSynchronously(Pipeline.generate (GeneratorConfig.load package) package)
                let bare = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                Expect.notEqual
                    (bare.Files |> List.map snd)
                    (configured.Files |> List.map snd)
                    "the fixture's own xantham.json changes what is rendered, so byte identity above is \
                     evidence the command read it"

            testCase "a shipped group is written under groups/" <| fun _ ->
                let package =
                    Path.Combine(root, "tests", "fixtures", "multi-ship-lab", "node_modules", "multi-ship-lab")

                invoke [ "generate"; package ]
                <| fun (code, out, _, outDir) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.isTrue (Directory.Exists(Path.Combine(outDir, "groups"))) "the groups directory"
                    Expect.stringContains out "DepLab.fs" "the shipped group is on standard output"

            testCase "the findings summary reports the manifest's tiers and keys" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "lab") ]
                <| fun (code, _, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.stringContains err "exact " "the tier legend"
                    Expect.stringContains err "ergonomic " "the tier legend"
                    Expect.stringContains err "widened " "the tier legend"
                    Expect.stringContains err "escape " "the tier legend"

            testCase "--quiet leaves the file list alone on standard output" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "lab"); "--quiet" ]
                <| fun (code, out, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.equal err "" "nothing on standard error"
                    Expect.stringContains out "PhaseBLab.fs" "the binding is still listed"

            testCase "a package whose own declarations shadow the default lib is warned about" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "dom-shadow-lab") ]
                <| fun (code, _, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.stringContains err "share a name with a default-lib declaration" "the warning names the mechanism"
                    Expect.stringContains err "lib" "the warning names the remedy"

            testCase "an ordinary global-script package is not warned about" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "globals-lab") ]
                <| fun (code, _, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.isFalse (err.Contains "default-lib declaration") "none of its names collide with the default lib"

            testCase "an ordinary module-based package is not warned about" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "lab") ]
                <| fun (code, _, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.isFalse (err.Contains "default-lib declaration") "it harvests through exports, not globals"

            testCase "--quiet suppresses the warning along with the rest of standard error" <| fun _ ->
                invoke [ "generate"; Path.Combine(root, "tests", "fixtures", "dom-shadow-lab"); "--quiet" ]
                <| fun (code, _, err, _) ->
                    Expect.equal code 0 "the command reports success"
                    Expect.equal err "" "nothing on standard error, warning included"

            testCase "configuring `lib` is the remedy the warning names, and it silences the warning" <| fun _ ->
                let config =
                    Path.Combine(Path.GetTempPath(), "xantham-cli-cfg-" + Guid.NewGuid().ToString "N")

                Directory.CreateDirectory config |> ignore

                try
                    File.WriteAllText(Path.Combine(config, "xantham.json"), """{ "lib": ["esnext"] }""")

                    invoke
                        [ "generate"
                          Path.Combine(root, "tests", "fixtures", "dom-shadow-lab")
                          "--config"
                          config ]
                    <| fun (code, _, err, _) ->
                        Expect.equal code 0 "the command reports success"
                        Expect.isFalse (err.Contains "default-lib declaration") "no default lib loaded, nothing shadowed"
                finally
                    Directory.Delete(config, true)
        ]
