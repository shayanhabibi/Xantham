#load "workspace.fsx"
#r "../src/Xantham.TypeScript.Wire/bin/Release/net10.0/Xantham.TypeScript.Wire.dll"
#r "../src/Xantham.Generator/bin/Release/net10.0/Xantham.Generator.dll"

open System
open System.Diagnostics
open System.IO
open System.Security
open System.Security.Cryptography
open System.Text.Json
open Xantham.Generator
open Xantham.TypeScript.Wire

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let fixture = Path.Combine(root, "tests", "fixtures", "catalog-portability-lab")
let scratchRoot = Path.Combine(root, "tests", ".scratch")
let options = JsonSerializerOptions(WriteIndented = true)

let fail message =
    failwith $"catalog portability: {message}"

let scratchPath path =
    let path = Path.GetFullPath path
    let relative = Path.GetRelativePath(scratchRoot, path)

    if
        relative = "."
        || Path.IsPathRooted relative
        || relative = ".."
        || relative.StartsWith(".." + string Path.DirectorySeparatorChar)
    then
        fail "outputs must be beneath tests/.scratch"

    path

let freshPath path =
    let path = scratchPath path

    if Directory.Exists path || File.Exists path then
        fail $"output already exists: {path}"

    path

let hashFile path =
    File.ReadAllBytes path |> SHA256.HashData |> Convert.ToHexStringLower

let sourceHashes () =
    Directory.EnumerateFiles(fixture, "*", SearchOption.AllDirectories)
    |> Seq.map (fun file -> Path.GetRelativePath(fixture, file).Replace('\\', '/'), hashFile file)
    |> Map.ofSeq

let readHashes path =
    JsonSerializer.Deserialize<Map<string, string>>(File.ReadAllText path)

let writeHashes path values =
    File.WriteAllText(path, JsonSerializer.Serialize(values, options))

let requireFile artifact name =
    let file = Path.Combine(artifact, name)

    if not (File.Exists file) then
        fail $"missing artifact file: {name}"

    file

let payloadFiles = [ "Portability.Root.fs"; "declarations.json" ]

let requirePortable catalog =
    use document = JsonDocument.Parse(File.ReadAllText catalog)

    let metadata =
        document.RootElement.GetProperty("compatibility").GetProperty("compiler")

    if metadata.GetProperty("kind").GetString() <> "typescript-package" then
        fail "the exchange gate requires a verified TypeScript package toolchain"

let produce artifact =
    let artifact = freshPath artifact

    let config =
        { GeneratorConfig.load fixture with
            ModuleName = Some "Portability.Root"
        }

    let dependency =
        Path.Combine(fixture, "node_modules", "catalog-portability-owner-lab")

    Pipeline.run config dependency artifact |> Async.RunSynchronously |> ignore
    requirePortable (requireFile artifact "declarations.json")
    writeHashes (Path.Combine(artifact, "sources.json")) (sourceHashes ())

    let payload =
        payloadFiles
        |> List.map (fun name -> name, hashFile (requireFile artifact name))
        |> Map.ofList

    writeHashes (Path.Combine(artifact, "payload.json")) payload
    printfn "catalog portability: producer artifact written"

let compile directory files =
    File.Copy(Path.Combine(root, "global.json"), Path.Combine(directory, "global.json"))

    for name in [ "Directory.Build.props"; "Directory.Build.targets" ] do
        File.WriteAllText(Path.Combine(directory, name), "<Project />")

    File.WriteAllText(
        Path.Combine(directory, "Consumer.fs"),
        "module Portability.Consumer\nlet accept (value: Portability.Root.Box<string>) : Portability.Root.Box<string> = Portability.Adapter.Exports.accept value\nlet read (value: Portability.Root.Box<string>) : string = value.value\n"
    )

    let includes =
        files
        |> List.map (fun file -> $"<Compile Include=\"{SecurityElement.Escape file}\" />")
        |> String.concat ""

    let support =
        Path.Combine(root, "src", "Xantham.Fable.Core.TS", "Xantham.Fable.Core.TS.fsproj")
        |> SecurityElement.Escape

    File.WriteAllText(
        Path.Combine(directory, "Consumer.fsproj"),
        $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup>
<ItemGroup>{includes}<Compile Include="Consumer.fs" /></ItemGroup>
<ItemGroup><PackageReference Include="Fable.Core" Version="5.2.0" /><ProjectReference Include="{support}" /></ItemGroup>
</Project>"""
    )

    let start =
        ProcessStartInfo(
            "dotnet",
            WorkingDirectory = directory,
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        )

    for argument in
        [
            "build"
            "Consumer.fsproj"
            "--disable-build-servers"
            "-m:1"
            "-p:BuildProjectReferences=false"
            "--configuration"
            "Release"
            "--verbosity"
            "quiet"
        ] do
        start.ArgumentList.Add argument

    use child = Process.Start start
    let stdout = child.StandardOutput.ReadToEndAsync()
    let stderr = child.StandardError.ReadToEndAsync()

    if not (child.WaitForExit 120000) then
        child.Kill(true)
        child.WaitForExit()
        fail "consumer compile timed out"

    let output = stdout.Result + stderr.Result

    if child.ExitCode <> 0 then
        fail $"consumer compile failed: {output}"

    printfn "catalog portability: consumer compiled"

let consume artifact directory =
    let artifact = scratchPath artifact
    let directory = freshPath directory
    let sources = requireFile artifact "sources.json" |> readHashes

    if sources <> sourceHashes () then
        fail "fixture source hash mismatch"

    let payload =
        payloadFiles
        |> List.map (fun name -> name, hashFile (requireFile artifact name))
        |> Map.ofList

    if payload <> (requireFile artifact "payload.json" |> readHashes) then
        fail "artifact payload hash mismatch"

    let catalog = requireFile artifact "declarations.json"
    requirePortable catalog

    let config =
        { GeneratorConfig.load fixture with
            DeclarationReferences = [ catalog ]
        }

    let generated = Path.Combine(directory, "adapter")
    let result = Pipeline.run config fixture generated |> Async.RunSynchronously

    let files =
        result.OutputFiles
        |> List.filter (fun name -> name.EndsWith ".fs")
        |> List.map (fun name -> Path.Combine(generated, name))

    compile directory (requireFile artifact "Portability.Root.fs" :: files)

match fsi.CommandLineArgs |> Array.skip 1 |> Array.toList with
| [ "produce"; artifact ] ->
    Workspace.ensureTsc root |> ignore

    if Tsc.locate root |> Option.isNone then
        fail "compiler unavailable; install the pinned npm dependencies"

    produce artifact
| [ "consume"; artifact; directory ] ->
    Workspace.ensureTsc root |> ignore

    if Tsc.locate root |> Option.isNone then
        fail "compiler unavailable; install the pinned npm dependencies"

    consume artifact directory
| _ ->
    fail "usage: dotnet fsi tools/catalog-portability.fsx -- produce <artifactDir> | consume <artifactDir> <scratchDir>"
