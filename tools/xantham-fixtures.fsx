#r "nuget: Str"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Partas.Build, 0.3.0"
#load "workspace.fsx"

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Partas.Build
open Partas.TypeProvider.BuildHelper
open Str

[<Literal>]
let __REPOSITORY_DIRECTORY__ = __SOURCE_DIRECTORY__ + "/.."

type Repo = BuildHelperProvider<__REPOSITORY_DIRECTORY__,
"
    tests/
        fixtures/
",
capabilityFullOverride = true>
let fixturesDir = Repo.VirtualFileSystem.tests.fixtures.ToString()
// let fixturesDir =
//     let root = Repo.FileSystem.ToString()
//     if Workspace.isLinkedWorktree root |> not
//     then root
//     else Workspace.mainCheckout root |> Option.defaultValue root
//     |> fun root ->
//         Path.Combine(root, "tests", "fixtures")
        

type Fixture = Fixture of string

type FixtureGroup = FixtureGroup of Fixture list

let fixtures = [
    Fixture "@cloudflare/workers-types"
    Fixture "@types/node"
    Fixture "@types/semver"
    Fixture "@types/lodash"
    Fixture "@types/d3"
    Fixture "@types/three"
    Fixture "typescript"
    Fixture "@cloudflare/ai-chat"
    Fixture "@cloudflare/dynamic-workflows"
    Fixture "@cloudflare/sandbox"
    Fixture "@cloudflare/shell"
    Fixture "@cloudflare/think"
    Fixture "@cloudflare/voice"
    Fixture "@cloudflare/worker-bundler"
    Fixture "@cloudflare/puppeteer"
    Fixture "@cloudflare/containers"
    Fixture "@cloudflare/workers-types"
    Fixture "solid-js"
    Fixture "ansi-regex"
    Fixture "type-fest"
    Fixture "animejs"
    Fixture "agents"
]
let fixtureGroups =
    fixtures
    |> List.groupBy (fun (Fixture fixture) ->
        match fixture.Split('/', 1) with
        | [| _ |] -> None
        | [| group; _ |] -> Some group
        | _ -> failwith "invalid split"
        )

module Options =
    let cleanInstall =
        Input.option<bool> "--clean-install"
        |> Input.alias "--ci"
        |> Input.description "Run npm install with --clean-install"
    let output =
        Input.option<string> "--output"
        |> Input.alias "-o"
        |> Input.description "Output directory for fixture folders"
        |> Input.def fixturesDir

module Stage =
    open Partas.Build.Internal
    let initialiseFixture (fixture: InputSpec<Fixture>)= input {
        let! (Fixture fixture) = fixture
        and! output = Options.output
        and! cleanInstall = Options.cleanInstall
        let fixturePath =
            fixture
            |> Str.splitChar '/'
            |> Array.toList
            |> function
            | [] -> failwith "empty fixture path"
            | [ head ] -> Path.Combine(output, head)
            | head :: tail :: _ -> Path.Combine(output, head, tail)
            
        return stage $"initialise {fixture}" {
            workingDir output
            stage "dir sanity" {
                when' (not <| Directory.Exists fixturePath)
                run (fun _ ->
                    if Directory.CreateDirectory(fixturePath).Exists then Ok() else
                    Error $"failed to create directory {fixturePath}"
                    )
            }
            stage "package-json" {
                when' (not <| File.Exists (Path.Combine(fixturePath, "package.json")))
                run (fun _ ->
                    let doc = JsonObject()
                    doc["name"] <- fixture
                    doc["type"] <- "module"
                    doc["dependencies"] <-
                        let ele = JsonObject()
                        ele[fixture] <- "*"
                        ele
                    use file = File.OpenWrite(Path.Combine(fixturePath, "package.json"))
                    use writer = new Utf8JsonWriter(file)
                    doc.WriteTo(writer)
                    )
            }
            if cleanInstall then
                stage "npm clean install" {
                    workingDir fixturePath
                    run "npm install --no-audit --no-fund --no-package-lock --clean-install"
                }
            else
                stage "npm install" {
                    workingDir fixturePath
                    run "npm install --no-audit --no-fund --no-package-lock"
                }
        }
    }
        
rootCommand fsi.CommandLineArgs[1..] {
    command "init" {
        stage "parallel/initialise fixtures" {
            parallel' 4
            for fixture in fixtures do
            Stage.initialiseFixture (InputSpec.ret fixture)
        }
    }
}
