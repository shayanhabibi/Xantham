#r "nuget: Partas.Build, 0.3.0"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Str"
#r "nuget: Fake.IO.FileSystem"

#load "tools/workspace.fsx"

open Partas.Build
open Partas.TypeProvider.BuildHelper
open Fake.IO
open Fake.IO.Globbing.Operators

type Repo = BuildHelperProvider<__SOURCE_DIRECTORY__, capabilityFullOverride=true>

module Spec =
    let projects = Repo.Project.AllProjects()
    let srcProjects = projects |> List.filter _.RelativePath.StartsWith("src")
    let testProjects = projects |> List.filter _.RelativePath.StartsWith("test")

module Options =
    let quick =
        Input.option<bool> "--quick"
        |> Input.alias "-q"
        |> Input.description "Skip setup steps, such as installing dependencies"

    let config =
        Baked.Input.DotNet.configString
        |> InputSpec.ofInput
        |> InputSpec.map (Option.defaultValue "Release")

    let projects =
        Spec.srcProjects
        |> List.map _.Name
        |> Baked.Input.Project.target
        |> Input.def (Spec.srcProjects |> List.filter _.Name.EndsWith("Wire") |> List.map _.Name)
        |> Input.customParser (fun res ->
            res.Tokens
            |> Seq.map (fun token ->
                Spec.srcProjects
                |> List.find _.Name.Equals(token.Value, System.StringComparison.OrdinalIgnoreCase)
                |> _.RelativePath)
            |> Seq.toList)
        |> InputSpec.ofInput
        |> InputSpec.map (fun projects ->
            Spec.srcProjects
            |> List.filter (_.RelativePath >> List.contains >> fun fn -> fn projects)
            |> function
                | [] -> Spec.srcProjects |> List.filter _.Name.EndsWith("Wire")
                | projects -> projects)

    let watch =
        Input.option<bool> "--watch"
        |> Input.alias "-w"
        |> Input.description "Run in watch mode."
        |> Input.def false

    let skipTests =
        Input.option<bool> "--skip-tests" |> Input.description "Skip running tests"

    let generateOnly =
        Input.option<string> "--only"
        |> Input.description
            "Limit generation to one layer: ast | proto | session | browser | schema. All five by default."
        |> Input.def ""

    /// The generator's inner loop, in three flags. An agent iterating on a pass runs
    /// `test --quick --update --no-run-gate` until the Expecto suite is green, then drops all
    /// three for the full gate before it commits. Each flag removes a step that is real safety
    /// on the way out and pure latency on the way in.
    let updateGoldens =
        Input.option<bool> "--update"
        |> Input.alias "-u"
        |> Input.description
            "Regenerate the golden corpus before asserting against it. Runs the suite twice: once writing, once checking."
        |> Input.def false

    let testFilter =
        Input.option<string> "--filter"
        |> Input.description
            "Run only tests whose name matches, e.g. --filter \"generator e2e\". Every test by default."
        |> Input.def ""

    let skipRunGate =
        Input.option<bool> "--no-run-gate"
        |> Input.description
            "Skip the Fable run gate, much the slowest step. The compile gate and the Expecto suites still run."
        |> Input.def false

    /// `findings` reads the manifests, which is the only part of a large fixture worth reading:
    /// `symbols.jsonl` runs to thousands of lines, its aggregate is a page.
    let findingsFixture =
        Input.option<string> "--fixture"
        |> Input.description "Aggregate one fixture's manifest rather than every one."
        |> Input.def ""

    let findingsKey =
        Input.option<string> "--key"
        |> Input.description
            "Report only this finding, named or coded, e.g. --key TR023 or --key TR.NotAmongGeneratedDeclarations."
        |> Input.def ""

    let syncUpstream =
        Input.option<bool> "--sync"
        |> Input.description
            "Re-vendor the upstream compiler sources before generating. Hits the network, so it is off by default; bump the pin in tools/tsc-ast/upstream.json first."
        |> Input.def false

module Stages =
    let restore =
        input {
            let! quick = Options.quick

            return
                stage "restore" {
                    quiet
                    when' (not quick)
                    run (cmd $"dotnet restore {Repo.Project.SolutionFile} -v q")
                    run "dotnet tool restore -v q"
                }

        }

    let format =
        input {
            let! quick = Options.quick

            return
                stage "format" {
                    workingDir Repo.FileSystem.``.``
                    when' (not quick)
                    run "dotnet fantomas ."
                }
        }


    let clean =
        input {
            let! quick = Options.quick

            return
                stage "clean" {
                    when' (not quick)
                    run (fun _ -> !!"**/**/bin" -- "bin" |> Shell.cleanDirs)
                }
        }

    let build (projects: Internal.InputSpec<string list>) =
        input {
            let! projects = projects
            and! config = Options.config

            return
                stage "build" {
                    quiet
                    when' (List.isEmpty projects |> not)

                    if projects.Length > 1 then
                        for project in projects do
                            stage $"build-{project}" { run (cmd $"dotnet build {project} -c {config} -v q") }
                    else
                        stage $"build-{projects[0]}" { run (cmd $"dotnet build {projects[0]} -c {config} -v q") }
                }
        }

    let docs =
        input {
            let! watch = Options.watch

            return
                stage "docs" {
                    if watch then
                        stage "watch" { run "dotnet fsdocs watch --eval" }
                    else
                        stage "build" { run "dotnet fsdocs build --eval --clean" }

                }
        }

    /// Installs the repository-level `typescript` pin from the root `package.json`. Generation reads
    /// the shipped schema out of that package, and `Tsc.locate` walks parent directories, so the
    /// same install also serves as the live `tsc --api` server for anything run under the repo.
    ///
    /// An agent worktree has no `node_modules` of its own, so it borrows the main checkout's
    /// install instead of downloading the pin a second time - `Workspace.ensureTsc` exports it as
    /// `XANTHAM_TSGO_EXE` for every later stage, and there is then nothing left to install.
    let deps =
        input {
            let! quick = Options.quick
            let borrowed = Workspace.ensureTsc __SOURCE_DIRECTORY__

            return
                stage "npm install" {
                    quiet
                    when' (not quick && borrowed.IsNone)
                    workingDir Repo.FileSystem.``.``
                    run "npm install"
                }
        }

    let fixtures =
        input {
            let! quick = Options.quick

            return
                stage "initialise fixtures" {
                    quiet
                    when' (not quick)
                    run "dotnet fsi tools/xantham-fixtures.fsx -- init"
                }
        }

    /// Routes to `tools/generate-wire.fsx`, which owns the per-layer options. Everything it needs
    /// already defaults to the repository layout, so the stages pass no arguments.
    let generate =
        input {
            let! only = Options.generateOnly
            and! sync = Options.syncUpstream

            return
                stage "generate" {
                    workingDir Repo.FileSystem.``.``

                    stage "sync tsc-ast" {
                        when' sync
                        run "dotnet fsi tools/generate-wire.fsx -- sync tsc-ast"
                    }
                    // Named rather than excluded, so a third layer does not turn `--only` into a list of
                    // everything it is not.
                    let wanted layer = only = "" || only = layer

                    stage "generate ast" {
                        when' (wanted "ast")
                        run "dotnet fsi tools/generate-wire.fsx -- generate ast"
                    }

                    stage "generate proto" {
                        when' (wanted "proto")
                        run "dotnet fsi tools/generate-wire.fsx -- generate proto"
                    }
                    // After proto: it reads the same schema, but the file it emits compiles against the
                    // surface proto emits.
                    stage "generate session" {
                        when' (wanted "session")
                        run "dotnet fsi tools/generate-wire.fsx -- generate session"
                    }
                    // The generator's own table rather than a wire layer, and it reads a NuGet family
                    // instead of the vendored sources - so it needs neither `sync` nor the others.
                    stage "generate browser" {
                        when' (wanted "browser")
                        run "dotnet fsi tools/generate-wire.fsx -- generate browser"
                    }
                    // The `xantham.json` schema, emitted from the config record by the CLI that
                    // ships it - so a key added to `GeneratorConfig` reaches an editor by
                    // rerunning this rather than by a second hand edit.
                    stage "generate schema" {
                        when' (wanted "schema")
                        run "dotnet run --project src/Xantham.Cli -- schema -o xantham.schema.json"
                    }
                }
        }

    /// Compose with `deps`: the live tests resolve the compiler with `Tsc.locate`, which walks
    /// parents from the test project, so the root install is what they find. Solution-driven so
    /// a new test project is in the run the moment the solution references it.
    ///
    /// The explicit build is not redundant with `dotnet test`. `dotnet test` on a solution
    /// builds the test projects and what they reference, and nothing references the compile
    /// gate - so without this the gate silently sat out every `build.fsx -- test` run and the
    /// goldens went unchecked. Building the solution first is what makes "bindings that do not
    /// compile are not bindings" true of this command.
    let test =
        input {
            let! skipTests = Options.skipTests
            and! config = Options.config
            and! update = Options.updateGoldens
            and! filter = Options.testFilter
            and! skipRunGate = Options.skipRunGate

            // `cmd` quotes each interpolation hole as one argument, so the flag and its value
            // have to be part of the format string rather than a pre-baked `" --filter ..."`
            // hole - that arrives as a single argument and MSBuild rejects it as one switch.
            let suite =
                if System.String.IsNullOrWhiteSpace filter then
                    cmd $"dotnet test {Repo.Project.SolutionFile} -c {config} --no-build"
                else
                    cmd $"dotnet test {Repo.Project.SolutionFile} -c {config} --no-build --filter {filter}"

            return
                stage "test" {
                    when' (not skipTests)
                    run (cmd $"dotnet build {Repo.Project.SolutionFile} -c {config} -v q")

                    // Regeneration is a *separate* run of the same suite, not a mode of the
                    // checking one: with `XANTHAM_UPDATE_GOLDEN` set the e2e tests write the
                    // goldens, so asserting in that same pass would only assert that a file
                    // equals what was just written to it. Child processes inherit the variable.
                    stage "regenerate goldens" {
                        when' update

                        run (fun _ ->
                            System.Environment.SetEnvironmentVariable("XANTHAM_UPDATE_GOLDEN", "1")
                            Ok())

                        run suite

                        run (fun _ ->
                            System.Environment.SetEnvironmentVariable("XANTHAM_UPDATE_GOLDEN", null)
                            Ok())
                    }

                    run suite
                    // The Fable *run* gate (§5 of the architecture plan): the linked goldens compiled
                    // by Fable and executed under node against the fixtures' JavaScript runtimes.
                    // `--noCache` because Fable's up-to-date check missed a changed linked golden once,
                    // and a gate that skips its compile is not a gate.
                    stage "run gate" {
                        when' (not skipRunGate)
                        workingDir "tests/Xantham.Generator.RunGate"

                        run
                            "dotnet fable . -o fable-out --noCache --run node --import ./register.mjs fable-out/Program.js"
                    }
                }
        }

    /// The aggregate of every manifest, which is what a large fixture is *for*: `TR023 148` is
    /// the measurement a change is justified by, and reading the thousands of lines of
    /// `symbols.jsonl` it was computed from tells you strictly less.
    let findings =
        input {
            let! fixture = Options.findingsFixture
            and! key = Options.findingsKey

            return
                stage "findings" {
                    run (fun _ ->
                        let golden =
                            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "tests", "Xantham.Generator.Tests", "golden")

                        let manifests =
                            System.IO.Directory.GetFiles(
                                golden,
                                "manifest.json",
                                System.IO.SearchOption.AllDirectories
                            )
                            |> Array.filter (fun path ->
                                System.String.IsNullOrWhiteSpace fixture
                                || path.Replace('\\', '/').Contains $"/{fixture}/")
                            |> Array.sort

                        if Array.isEmpty manifests then
                            Error $"no golden manifest matches {fixture}"
                        else
                            for path in manifests do
                                use doc = System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText path)
                                let root = doc.RootElement
                                let tiers = root.GetProperty "counts"
                                let tier (name: string) = tiers.GetProperty(name).GetInt32()

                                let symbols =
                                    System.IO.Path.Combine(System.IO.Path.GetDirectoryName path, "symbols.jsonl")

                                let counts =
                                    if not (System.IO.File.Exists symbols) then
                                        []
                                    else
                                        System.IO.File.ReadLines symbols
                                        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
                                        |> Seq.collect (fun line ->
                                            use symbol = System.Text.Json.JsonDocument.Parse line

                                            match symbol.RootElement.TryGetProperty "findings" with
                                            | true, findings ->
                                                findings.EnumerateArray()
                                                |> Seq.map (fun finding ->
                                                    finding.GetProperty("name").GetString(),
                                                    finding.GetProperty("key").GetString())
                                                |> Seq.toList
                                            | _ -> [])
                                        |> Seq.filter (fun (name, code) ->
                                            System.String.IsNullOrWhiteSpace key || name = key || code = key)
                                        |> Seq.map snd
                                        |> Seq.countBy id
                                        |> Seq.sortByDescending snd
                                        |> Seq.toList

                                let package = root.GetProperty("package").GetString()
                                let exact = tier "exact"
                                let ergonomic = tier "ergonomic"
                                let widened = tier "widened"
                                let escape = tier "escape"

                                printfn ""
                                printfn $"{package}"

                                printfn $"  exact {exact}  ergonomic {ergonomic}  widened {widened}  escape {escape}"

                                for found, count in counts do
                                    printfn $"  {found} {count}"

                            Ok())
                }
        }

    let pack =
        input {
            let! projects = Options.projects
            and! config = Options.config

            return
                stage "pack" {
                    quiet

                    for project in projects do
                        stage $"pack-{project.Name}" {
                            run (cmd $"dotnet pack {project.Path} -c {config} --no-build --no-restore -v q -o bin")
                        }
                }
        }

    let publish =
        input {
            let! apiKey = Baked.Input.NuGet.apiKeyOrEnv
            let path = "bin/*.nupkg"

            return
                stage "publish" {
                    workingDir Repo.FileSystem.``.``
                    when' apiKey.IsSome
                    failIfIgnored

                    run
                        $"dotnet nuget push {path} -k {apiKey.Value} -s https://api.nuget.org/v3/index.json --skip-duplicate"
                }
        }

rootCommand fsi.CommandLineArgs[1..] {
    workingDir Repo.FileSystem.``.``

    command "format" {
        Stages.restore
        Stages.format
    }

    command "bump" {
        Baked.Pipelines.bumpArgument
            (Spec.srcProjects |> List.map _.RelativePath)
            (Options.projects |> InputSpec.map (List.map _.RelativePath))
    }

    command "build" {
        Stages.restore
        Stages.clean
        Stages.format
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
    }

    command "generate" {
        Stages.deps
        Stages.generate
    }

    command "docs" {
        Stages.restore
        Stages.clean
        Stages.format
        Stages.docs
    }

    command "publish" {
        Stages.restore
        Stages.clean
        Stages.format
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
        Stages.deps
        Stages.fixtures
        Stages.test
        Stages.pack
        Stages.publish
    }

    command "test" {
        Stages.restore
        Stages.clean
        Stages.format
        Stages.deps
        Stages.fixtures
        Stages.test
    }

    command "findings" { Stages.findings }

    command "pack" {
        Stages.restore
        Stages.clean
        Stages.format
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
        Stages.deps
        Stages.fixtures
        Stages.test
        Stages.pack
    }
}
