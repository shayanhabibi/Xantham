#r "nuget: Partas.Build, 0.3.0"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Str"
#r "nuget: Fake.IO.FileSystem"

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
    
    let generate = stage "generate" {
        echo "NULL OP"
    }
    
    let test = input {
        let! skipTests = Options.skipTests
        and! config = Options.config
        and! quick = Options.quick
        return stage "test" {
            when' (not skipTests)
            stage "npm install" {
                when' (not quick)
                workingDir Repo.Project.``Xantham.TypeScript.Wire.Tests``.Directory
                run "npm install"
            }
            run (cmd $"dotnet test {Repo.Project.``Xantham.TypeScript.Wire.Tests``.Path} -c {config}")
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
    command "docs" {
        Stages.restore
        Stages.clean
        Stages.docs
    }
    command "publish" {
        Stages.restore
        Stages.clean
        Stages.build (Options.projects |> InputSpec.map (List.map _.RelativePath))
        Stages.test
        Stages.pack
        Stages.publish
    }
    command "test" {
        Stages.restore
        Stages.clean
        Stages.test
    }
}