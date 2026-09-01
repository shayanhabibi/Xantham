#r "nuget: Partas.Build, 0.3.0"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Str"
#r "nuget: Fake.IO.FileSystem"

#load "tools/workspace.fsx"

open Partas.Build
open Partas.TypeProvider.BuildHelper
open Fake.IO
open Fake.IO.Globbing.Operators

type Repo = BuildHelperProvider<__SOURCE_DIRECTORY__, capabilityFullOverride = true>

module Spec =
    let projects = Repo.Project.AllProjects()
    let srcProjects =
        projects
        |> List.filter _.RelativePath.StartsWith("src")
    let testProjects =
        projects
        |> List.filter _.RelativePath.StartsWith("test")

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
                |> _.RelativePath
                )
            |> Seq.toList
            )
        |> InputSpec.ofInput
        |> InputSpec.map (fun projects ->
            Spec.srcProjects
            |> List.filter (_.RelativePath >> List.contains >> fun fn -> fn projects)
            |> function
                | [] -> Spec.srcProjects |> List.filter _.Name.EndsWith("Wire")
                | projects -> projects
            )
    let watch =
        Input.option<bool> "--watch"
        |> Input.alias "-w"
        |> Input.description "Run in watch mode."
        |> Input.def false
    let skipTests =
        Input.option<bool> "--skip-tests"
        |> Input.description "Skip running tests"
    let generateOnly =
        Input.option<string> "--only"
        |> Input.description "Limit generation to one layer: ast | proto | session | browser. All four by default."
        |> Input.def ""
    let syncUpstream =
        Input.option<bool> "--sync"
        |> Input.description "Re-vendor the upstream compiler sources before generating. Hits the network, so it is off by default; bump the pin in tools/tsc-ast/upstream.json first."
        |> Input.def false

module Stages =
    let restore = input {
        let! quick = Options.quick
        return stage "restore" {
            quiet
            when' (not quick)
            run (cmd $"dotnet restore {Repo.Project.SolutionFile} -v q")
            run "dotnet tool restore -v q"
        }
        
    }
    let clean = input {
        let! quick = Options.quick
        return stage "clean" {
            when' (not quick)
            run (fun _ -> !! "**/**/bin" -- "bin" |> Shell.cleanDirs)
        }
    }
    let build (projects: Internal.InputSpec<string list>) = input {
        let! projects = projects
        and! config = Options.config
        return stage "build" {
            quiet
            when' (List.isEmpty projects |> not)
            if projects.Length > 1 then
                for project in projects do
                stage $"build-{project}" {
                    run (cmd $"dotnet build {project} -c {config} -v q")
                }
            else
                stage $"build-{projects[0]}" {
                    run (cmd $"dotnet build {projects[0]} -c {config} -v q")
                }
        }
    }
    let docs = input {
        let! watch = Options.watch
        return stage "docs" {
            if watch then
                stage "watch" {
                    run "dotnet fsdocs watch --eval"
                }
            else
                stage "build" {
                    run "dotnet fsdocs build --eval --clean"
                }
                
        }
    }
    
    /// Installs the repository-level `typescript` pin from the root `package.json`. Generation reads
    /// the shipped schema out of that package, and `Tsc.locate` walks parent directories, so the
    /// same install also serves as the live `tsc --api` server for anything run under the repo.
    ///
    /// An agent worktree has no `node_modules` of its own, so it borrows the main checkout's
    /// install instead of downloading the pin a second time - `Workspace.ensureTsc` exports it as
    /// `XANTHAM_TSGO_EXE` for every later stage, and there is then nothing left to install.
    let deps = input {
        let! quick = Options.quick
        let borrowed = Workspace.ensureTsc __SOURCE_DIRECTORY__
        return stage "npm install" {
            quiet
            when' (not quick && borrowed.IsNone)
            workingDir Repo.FileSystem.``.``
            run "npm install"
        }
    }
    
    let fixtures = input {
        let! quick = Options.quick
        return stage "initialise fixtures" {
            quiet
            when' (not quick)
            run "dotnet fsi tools/xantham-fixtures.fsx -- init" 
        }
    }

    /// Routes to `tools/generate-wire.fsx`, which owns the per-layer options. Everything it needs
    /// already defaults to the repository layout, so the stages pass no arguments.
    let generate = input {
        let! only = Options.generateOnly
        and! sync = Options.syncUpstream
        return stage "generate" {
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
    let test = input {
        let! skipTests = Options.skipTests
        and! config = Options.config
        return stage "test" {
            when' (not skipTests)
            run (cmd $"dotnet build {Repo.Project.SolutionFile} -c {config} -v q")
            run (cmd $"dotnet test {Repo.Project.SolutionFile} -c {config} --no-build")
        }
    }
    
    let pack = input {
        let! projects = Options.projects
        and! config = Options.config
        return stage "pack" {
            quiet
            for project in projects do
            stage $"pack-{project.Name}" {
                run (cmd $"dotnet pack {project.Path} -c {config} --no-build --no-restore -v q -o bin")
            }
        }
    }
    
    let publish = input {
        let! apiKey = Baked.Input.NuGet.apiKeyOrEnv
        let path = "bin/*.nupkg"
        return stage "publish" {
            workingDir Repo.FileSystem.``.``
            when' apiKey.IsSome
            failIfIgnored
            run $"dotnet nuget push {path} -k {apiKey.Value} -s https://api.nuget.org/v3/index.json --skip-duplicate"
        }
    }

rootCommand fsi.CommandLineArgs[1..] {
    workingDir Repo.FileSystem.``.``
    command "bump" {
        Baked.Pipelines.bumpArgument (Spec.srcProjects |> List.map _.RelativePath) (Options.projects |> InputSpec.map (List.map _.RelativePath))
    }
    command "build" {
        Stages.restore
        Stages.clean
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
    }
    command "generate" {
        Stages.deps
        Stages.generate
    }
    command "docs" {
        Stages.restore
        Stages.clean
        Stages.docs
    }
    command "publish" {
        Stages.restore
        Stages.clean
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
        Stages.deps
        Stages.fixtures
        Stages.test
    }
    command "pack" {
        Stages.restore
        Stages.clean
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
        Stages.deps
        Stages.fixtures
        Stages.test
        Stages.pack
    }
}