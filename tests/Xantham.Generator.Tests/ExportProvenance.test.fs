module Xantham.Generator.Tests.ExportProvenanceTests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Expecto
open Xantham.Generator
open Xantham.TypeScript.Wire

let private fixture = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "type-only-export-lab"))

let private generate entry =
    Pipeline.generate { GeneratorConfig.load fixture with Entry = Some entry } fixture
    |> Async.RunSynchronously

let private sourceOf (rendered: RenderModel) = rendered.Files |> List.find (fst >> fun name -> name = "TypeOnlyExportLab.fs") |> snd

let private importedNames (source: string) =
    Regex.Matches(source, "Import\\(\"([^\"]+)\", \"type-only-export-lab\"\\)")
    |> Seq.map (fun matched -> matched.Groups[1].Value)
    |> Seq.distinct |> Seq.sort |> Seq.toList

let private run executable directory arguments =
    let start = ProcessStartInfo(executable)
    start.WorkingDirectory <- directory
    for arg in arguments do start.ArgumentList.Add arg
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    child.WaitForExit()
    child.ExitCode, output.Result + errors.Result

let private compile directory (source: string) (consumer: string) =
    File.WriteAllText(Path.Combine(directory, "Binding.fs"), source)
    File.WriteAllText(Path.Combine(directory, "Consumer.fs"), consumer)
    let coreTs = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Xantham.Fable.Core.TS", "Xantham.Fable.Core.TS.fsproj") |> Path.GetFullPath
    File.WriteAllText(Path.Combine(directory, "Consumer.fsproj"), $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup>
<ItemGroup><Compile Include="Binding.fs"/><Compile Include="Consumer.fs"/></ItemGroup>
<ItemGroup><PackageReference Include="Fable.Core" Version="5.2.0"/></ItemGroup>
<ItemGroup><ProjectReference Include="{coreTs}"/></ItemGroup>
</Project>""")
    run "dotnet" directory [ "build"; "Consumer.fsproj"; "--verbosity"; "quiet" ]

let private consumer = """module Consumer
open TypeOnlyExportLab
let message: Message = Payload.Create "message"
let client: Client = Exports.visible message
let send (value: Client) (payload: Payload): Message = value.send payload
let keepFunctionType (callback: HiddenFunction): HiddenFunction = callback
"""

[<Tests>]
let tests =
    testList "export provenance" [
        let inline (==>) input expected = input, expected
        testCase "the compiler accepts types and rejects type-only values" <| fun _ ->
            let compiler = Tsc.locate fixture |> Option.defaultWith (fun () -> failtest "native TypeScript compiler is required")
            let arguments = [
                "--noEmit"; "--ignoreConfig"; "--target"; "esnext"; "--module"; "nodenext"
                yield! Directory.GetFiles(fixture, "*.d.*") |> Array.sort
                "consumer.ts"
            ]
            let code, output = run compiler fixture arguments
            code |> Flip.Expect.equal output 0

        testTheory "only value paths import their exports" [
            "index.d.ts" ==> [ "visible" ]
            "named.d.ts" ==> [ "visible" ]
            "forwarded.d.ts" ==> [ "visible" ]
            "named-forwarded.d.ts" ==> [ "visible" ]
            "cycle.d.ts" ==> [ "visible" ]
            "local.d.ts" ==> [ "visible" ]
            "restored.d.ts" ==> [ "Constructor"; "Constructor.create"; "recovered"; "visible" ]
            "default-type.d.ts" ==> []
            "default-value.d.ts" ==> [ "default"; "default.create" ]
            "equals-type.d.cts" ==> []
            "equals-value.d.cts" ==> [ "create"; "prototype" ]
            "synthetic-default.d.ts" ==> [ "SDK" ]
        ] <| fun (entry, expected) ->
            generate entry |> sourceOf |> importedNames |> Flip.Expect.equal entry expected

        testCase "type shapes remain usable without runtime exports" <| fun _ ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-type-only-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let rendered = generate "index.d.ts"
                rendered.Findings
                |> List.exists (fun finding -> finding.Symbol = "hidden" && finding.Kind :? AuditCoverage)
                |> Flip.Expect.equal "a type-only function has no direct type or runtime name to emit" false
                let source = sourceOf rendered
                let code, output = compile directory source consumer
                code |> Flip.Expect.equal output 0
                for expression in [ "Exports.hidden message"; "Exports.Client message"; "Client.create message" ] do
                    let code, output = compile directory source (consumer + "\nlet invalid = " + expression + "\n")
                    (code <> 0 && output.Contains "FS0039") |> Flip.Expect.equal output true
            finally Directory.Delete(directory, true)
    ]
