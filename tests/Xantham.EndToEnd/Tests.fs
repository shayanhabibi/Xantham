module Tests

open Fake.Core
open Fake.IO
open Expecto
open Fake.JavaScript
open System.IO

let mutable buildDriver = true
let mutable runXantham = false
let mutable buildXantham = false


let createTestsForFixture (fixture: TestFixture) =
    let ensureProcessResult (fmt: obj -> string) =
        fun (result: ProcessResult<Result<string, string>>) ->
            Expect.equal result.ExitCode 0 (fmt result)
            result.Result
        >> Flip.Expect.isOk "Received error output"
    [
        testCase "setup" <| fun _ ->
            TestFixture.setup fixture
        testCase "encode" <| fun _ ->
            if not runXantham && File.exists fixture.Json && not buildXantham then skiptest "Skipping encode"
            RepoRoot.``.``
            |> node [ "index.js"; fixture.TypeDefTarget |> Option.defaultValue fixture.TypeDefinitionFile; "-o"; fixture.Json ]
            |> ensureProcessResult (sprintf "Failed encoding: %A")
        testCase "decode" <| fun _ ->
            dotnet [
                "run"
                "--no-build"
                "--"
                fixture.Json; fixture.FSharp
            ] Xantham.Driver.``.``
            |> ensureProcessResult (sprintf "Failed decoding: %A")
            File.Move(fixture.FSharp, fixture.VerifyTarget, true)
        testCase "generate" <| fun _ ->
            CreateProcess.fromRawCommand "dotnet" [
                "build"
                fixture.VerifyProject
                "--no-incremental"
                "/p:OtherFlags=\"--maxerrors:10000\""
            ]
            |> CreateProcess.withWorkingDirectory (Path.getDirectory fixture.VerifyProject)
            |> CreateProcess.redirectOutput
            |> Proc.run
            |> function
                | { Result = result; ExitCode = exitCode } ->
                    result.Output + result.Error
                    |> String.convertTextToWindowsLineBreaks
                    |> String.splitStr String.WindowsLineBreaks
                    |> List.choose (fun line ->
                        if line.Contains "error FS" |> not then None else
                        line.Substring(line.IndexOf("error ") + "error ".Length, "FS0000".Length)
                        |> Some
                        )
                    |> List.countBy id
                    |> List.sortByDescending snd
                    |> function
                        | [] -> ()
                        | errors ->
                            errors
                            |> List.unzip
                            ||> fun labels counts ->
                                [ counts |> List.sum |> sprintf "Total Errors: %i" ]
                                |> List.append (List.map2 (sprintf "    %s - %i") labels counts)
                            |> String.concat "\n"
                            |> failtestNoStackf "Error histogram:\n%s" 
                    Expect.equal exitCode 0 "Expected no errors"
    ]
    |> testList fixture.Name
let fixtures =
    [
        let agentsRoot = RepoRoot.tests.``Xantham.Decoder.Tests``.fixtures.agents.``.``
        {
            Name = "agents"
            Json = VirtualRoot.output.``agents.json``
            FSharp = VirtualRoot.output.``agents.fs``
            VerifyTarget = VirtualRoot.verify.``agents.wrapped.fs``
            VerifyProject = VirtualRoot.verify.``Verify.Agents.fsproj``
            TypeDefRoot = agentsRoot
            TypeDefinitionFile =
                "node_modules/agents/dist/index.d.ts"
                |> Path.combine agentsRoot
            TypeDefTarget = None
        }
        let dynamicWorkflowsRoot = RepoRoot.tests.``Xantham.Decoder.Tests``.fixtures.``dynamic-workflows``.``.``
        {
            Name = "dynamic-workflows"
            Json = VirtualRoot.output.``dynamic-workflows.json``
            FSharp = VirtualRoot.output.``dynamic-workflows.fs``
            VerifyTarget = VirtualRoot.verify.``dynamic-workflows.wrapped.fs``
            VerifyProject = VirtualRoot.verify.``Verify.DynamicWorkflows.fsproj``
            TypeDefRoot = dynamicWorkflowsRoot
            TypeDefinitionFile =
                "node_modules/@cloudflare/dynamic-workflows/dist/index.d.ts"
                |> Path.combine dynamicWorkflowsRoot
            TypeDefTarget = None
        }
        let workersTypesRoot = RepoRoot.tests.``Xantham.Decoder.Tests``.fixtures.``workers-types``.``.``
        {
            Name = "workers-types"
            Json = VirtualRoot.output.``workers-types.json``
            FSharp = VirtualRoot.output.``workers-types.fs``
            VerifyTarget = VirtualRoot.verify.``workersTypes.wrapped.fs``
            VerifyProject = VirtualRoot.verify.``Verify.WorkersTypes.fsproj``
            TypeDefRoot = workersTypesRoot
            TypeDefinitionFile =
                "node_modules/@cloudflare/workers-types/index.d.ts"
                |> Path.combine workersTypesRoot
            TypeDefTarget = Some "@cloudflare/workers-types"
        }
    ]

let setupDriverTests =
    [
        testCase "Build Driver" <| fun _ ->
            if not buildDriver then skiptest "Skipping rebuilding the driver."
            dotnetx [ "restore"; Xantham.Driver.``Xantham.EndToEnd.Driver.fsproj`` ] Xantham.Driver.``.``
            dotnet [ "build"; Xantham.Driver.``Xantham.EndToEnd.Driver.fsproj``; "--no-incremental" ] Xantham.Driver.``.``
            |> fun result ->
                Expect.equal result.ExitCode 0 $"Failed build: {result}"
                result.Result
            |> Flip.Expect.isOk "Received error output"
        testCase "Build Xantham" <| fun _ ->
            if not buildXantham then skiptest "Skipping building Xantham.Fable"
            Npm.setDir RepoRoot.``.``
            |> Npm.run "build"
    ]

[<Tests>]
let tests =
    if buildXantham || buildDriver then
        fixtures
        |> List.map (fun fixture -> createTestsForFixture fixture |> testSequencedGroup "build")
        |> List.append setupDriverTests
        |> testList "EndToEnd"
        |> testSequencedGroup "build"
    else
        fixtures
        |> List.map (fun fixture -> createTestsForFixture fixture |> testSequencedGroup fixture.Name)
        |> testList "EndToEnd"