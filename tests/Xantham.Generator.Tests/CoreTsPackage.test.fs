module Xantham.Generator.Tests.CoreTsPackageTests

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text.Json
open System.Xml.Linq
open Expecto
open Xantham.Generator

let private repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let private project = Path.Combine(repository, "src", "Xantham.Fable.Core.TS", "Xantham.Fable.Core.TS.fsproj")

let private runDotnet directory arguments =
    let start = ProcessStartInfo("dotnet", WorkingDirectory = directory)
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    for argument in arguments do start.ArgumentList.Add argument
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    child.WaitForExit()
    child.ExitCode, output.Result + errors.Result

let private dotnet directory arguments =
    let code, output = runDotnet directory arguments
    Expect.equal code 0 output

[<Tests>]
let tests = testSequenced <| testList "core-ts package" [
    testCase "packing rejects stale bindings and stale compiled assemblies" <| fun () ->
        use scratch = Scratch.directory "core-ts-stale-package"
        let original = Path.GetDirectoryName project
        for file in ["Xantham.Fable.Core.TS.fsproj"; "Fable.Core.TS.fs"; "Fable.Core.TS.Extensions.fs"; "declarations.json"; "catalogue-inputs.json"] do
            File.Copy(Path.Combine(original, file), Path.Combine(scratch.Path, file))
        let binding = Path.Combine(scratch.Path, "Fable.Core.TS.fs")
        File.AppendAllText(binding, "\n// stale binding\n")
        let assembly = Path.Combine(scratch.Path, "Xantham.Fable.Core.TS.dll")
        let script = Path.Combine(repository, "tools", "package-core-ts-catalog.fsx")
        let prepare = ["fsi"; script; "--"; "prepare"; scratch.Path; assembly; "1.0.0"]
        let code, output = runDotnet repository prepare
        Expect.notEqual code 0 "packing refuses changed binding sources"
        Expect.stringContains output "Regenerate with build.fsx" "packing gives a regeneration remedy"
        File.Copy(Path.Combine(original, "Fable.Core.TS.fs"), binding, true)
        let built = Path.Combine(original, "bin", "Release", "net8.0", "Xantham.Fable.Core.TS.dll")
        File.Copy(built, assembly)
        File.Copy(built + ".catalogue-build.json", assembly + ".catalogue-build.json")
        File.AppendAllText(assembly, "stale")
        let code, output = runDotnet repository prepare
        Expect.notEqual code 0 "packing refuses a different compiled assembly"
        Expect.stringContains output "Build Core.TS" "packing gives a build remedy"

    testCase "restored package supplies authenticated DOM and generic identities" <| fun () ->
        use scratch = Scratch.directory "core-ts-package"
        let feed = Path.Combine(scratch.Path, "feed")
        let package =
            match Environment.GetEnvironmentVariable "XANTHAM_CORE_TS_PACKAGE" with
            | null | "" ->
                let version = (XDocument.Load(project).Descendants(XName.Get "Version") |> Seq.exactlyOne |> _.Value) + "-catalogue-test"
                let support = Path.Combine(repository, "src", "Xantham.Fable.Core", "Xantham.Fable.Core.fsproj")
                dotnet repository ["pack"; support; "-c"; "Release"; "--no-build"; "--no-restore"; "-p:PackageVersion=" + version; "-o"; feed; "-v"; "q"]
                dotnet repository ["pack"; project; "-c"; "Release"; "--no-build"; "--no-restore"; "-p:PackageVersion=" + version; "-o"; feed; "-v"; "q"]
                Path.Combine(feed, "Xantham.Fable.Core.TS." + version + ".nupkg")
            | published ->
                Directory.CreateDirectory feed |> ignore
                let local = Path.Combine(feed, Path.GetFileName published)
                File.Copy(published, local)
                local
        use archive = ZipFile.OpenRead package
        let specification = archive.Entries |> Seq.find (fun entry -> entry.FullName.EndsWith ".nuspec")
        use specificationStream = specification.Open()
        let metadata = XDocument.Load specificationStream
        let version = metadata.Descendants() |> Seq.find (fun element -> element.Name.LocalName = "version") |> _.Value
        for payload in ["xantham/declarations.json.br"; "xantham/catalogue.json"] do
            Expect.isNotNull (archive.GetEntry payload) ("the package distributes " + payload)

        let consumer = Path.Combine(scratch.Path, "consumer")
        Directory.CreateDirectory consumer |> ignore
        for file in ["Directory.Build.props"; "Directory.Build.targets"] do
            File.WriteAllText(Path.Combine(consumer, file), "<Project />")
        File.Copy(Path.Combine(repository, "global.json"), Path.Combine(consumer, "global.json"))
        File.WriteAllText(Path.Combine(consumer, "NuGet.Config"),
            $"""<configuration><packageSources><clear/>
<add key="local" value="{feed}"/>
<add key="xantham" value="https://nuget.cloudsmith.io/shayanhabibi/xantham/v3/index.json"/>
<add key="nuget" value="https://api.nuget.org/v3/index.json"/>
</packageSources></configuration>""")
        let restored = Path.Combine(scratch.Path, "packages")
        let fsproj = Path.Combine(consumer, "Consumer.fsproj")
        File.WriteAllText(fsproj,
            $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net8.0</TargetFramework><NuGetAudit>false</NuGetAudit><RestorePackagesPath>{restored}</RestorePackagesPath></PropertyGroup>
<ItemGroup><PackageReference Include="Xantham.Fable.Core.TS" Version="{version}"/></ItemGroup>
<ItemGroup><Compile Include="Consumer.fs"/></ItemGroup></Project>""")
        File.WriteAllText(Path.Combine(consumer, "Consumer.fs"), "module Package.Consumer\n")
        dotnet consumer ["restore"; "Consumer.fsproj"; "-v"; "q"]
        let contents = Path.Combine(restored, "xantham.fable.core.ts", version, "xantham")
        let reference = Path.Combine(contents, "declarations.json.br")
        use compressed = File.OpenRead reference
        use decoder = new BrotliStream(compressed, CompressionMode.Decompress)
        use catalog = JsonDocument.Parse decoder
        let declarations = catalog.RootElement.GetProperty("declarations").EnumerateArray() |> Seq.toArray
        for name, arity in ["Fable.Core.TS.Dom.HTMLInputElement", 0; "Fable.Core.TS.Dom.FileList", 0; "Fable.Core.TS.Es.Promise", 1] do
            let entry = declarations |> Array.find (fun entry -> entry.GetProperty("fSharpName").GetString() = name)
            Expect.equal (entry.GetProperty("arity").GetInt32()) arity "the packaged type retains its generic arity"
            Expect.equal (entry.GetProperty("owner").GetString()) "FableCoreTsInput" "the authenticated producer owns the type"
        use descriptor = JsonDocument.Parse(File.ReadAllText(Path.Combine(contents, "catalogue.json")))
        Expect.equal (descriptor.RootElement.GetProperty("packageId").GetString()) "Xantham.Fable.Core.TS" "descriptor identifies the restored producer package"
        Expect.equal (descriptor.RootElement.GetProperty("packageVersion").GetString()) version "descriptor follows the packed version"
        Expect.equal (descriptor.RootElement.GetProperty("owner").GetString()) "FableCoreTsInput" "descriptor maps the authenticated owner"
        let digest path = File.ReadAllBytes path |> SHA256.HashData |> Convert.ToHexStringLower
        Expect.equal (descriptor.RootElement.GetProperty("catalogueSha256").GetString()) (digest reference) "descriptor authenticates the transported catalogue"
        let assembly = Path.Combine(restored, "xantham.fable.core.ts", version, "lib", "net8.0", "Xantham.Fable.Core.TS.dll")
        Expect.equal (descriptor.RootElement.GetProperty("assemblySha256").GetString()) (digest assembly) "descriptor binds the catalogue to the restored assembly"

        let input = Path.Combine(scratch.Path, "input")
        Directory.CreateDirectory input |> ignore
        File.WriteAllText(Path.Combine(input, "package.json"), """{"name":"core-ts-package-lab","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(input, "index.d.ts"),
            "export interface Selection { element: HTMLInputElement; files: FileList; ready: Promise<HTMLInputElement>; }\n")
        let config =
            { GeneratorConfig.load (Path.Combine(repository, "tools", "fable-core-ts-input")) with
                ModuleName = Some "Package.Binding"
                CompilerLib = CompilerLibConfig.Default
                DeclarationCatalog = true
                DeclarationReferences = [reference] }
        let generated = Path.Combine(consumer, "generated")
        let report = Pipeline.run config input generated |> Async.RunSynchronously
        let includes = report.OutputFiles |> List.filter _.EndsWith(".fs") |> List.map (fun file -> $"<Compile Include=\"generated/{file}\"/>") |> String.concat ""
        File.WriteAllText(fsproj, File.ReadAllText(fsproj).Replace("<Compile Include=\"Consumer.fs\"/>", includes + "<Compile Include=\"Consumer.fs\"/>"))
        File.WriteAllText(Path.Combine(consumer, "Consumer.fs"), """module Package.Consumer
let element (value: Package.Binding.Selection) : Fable.Core.TS.Dom.HTMLInputElement = value.element
let files (value: Package.Binding.Selection) : Fable.Core.TS.Dom.FileList = value.files
let ready (value: Package.Binding.Selection) : Fable.Core.JS.Promise<Fable.Core.TS.Dom.HTMLInputElement> = value.ready
        """)
        dotnet consumer ["build"; "Consumer.fsproj"; "--no-restore"; "--disable-build-servers"; "-m:1"; "-v"; "q"]
]
