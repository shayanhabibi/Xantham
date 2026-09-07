module Xantham.Generator.Tests.DeclarationCatalogTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator

let private fixture = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "declaration-identity-lab"))

let private configured (directory: string) name entry (references: string array) =
    let path = Path.Combine(directory, name + ".json")
    File.WriteAllText(path,
        JsonSerializer.Serialize {| ``module`` = "Identity." + name; entry = entry
                                    runtime = "declaration-identity-lab/" + name.ToLowerInvariant()
                                    lib = [| "esnext" |]; types = ([||] : string array)
                                    declarationCatalog = true; declarationReferences = references |})
    GeneratorConfig.loadFile path

let private compileConsumer directory sources (consumer: string) =
    File.WriteAllText(Path.Combine(directory, "Consumer.fs"), consumer)
    let includes = sources |> List.map (fun file -> $"<Compile Include=\"{file}\"/>") |> String.concat ""
    File.WriteAllText(Path.Combine(directory, "Consumer.fsproj"), $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup>
<ItemGroup>{includes}<Compile Include="Consumer.fs"/></ItemGroup>
<ItemGroup><PackageReference Include="Fable.Core" Version="5.2.0"/></ItemGroup>
</Project>""")
    let start = ProcessStartInfo("dotnet")
    start.WorkingDirectory <- directory
    start.ArgumentList.Add "build"
    start.ArgumentList.Add "Consumer.fsproj"
    start.ArgumentList.Add "--verbosity"
    start.ArgumentList.Add "quiet"
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    child.WaitForExit()
    child.ExitCode, output.Result + errors.Result

let private sharedConsumer = """module Identity.Consumer
let attach (client: Identity.Root.PublicClient) : Identity.Root.PublicClient =
    Identity.Adapter.Exports.attach client
let bind<'T when 'T :> Identity.Root.Item> (box: Identity.Root.PublicBox<'T>) : Identity.Root.PublicBox<'T> =
    Identity.Adapter.Exports.bind box
let alias (client: Identity.Adapter.Client) : Identity.Root.PublicClient = client
let follow (client: Identity.Root.PublicClient) : Identity.Root.PublicClient =
    Identity.Next.Exports.follow client
let text (config: Identity.Root.TextConfig) : Identity.Root.TextConfig =
    Identity.Adapter.Exports.acceptText config
let generic<'T> (config: Identity.Root.Config<'T>) : Identity.Root.Config<'T> =
    Identity.Adapter.Exports.acceptGeneric config
let constrained<'T> (config: Identity.Root.Config<'T>) : Identity.Root.Config<'T> =
    Identity.Adapter.Exports.acceptConstrained config
let constructor (ctor: Identity.Root.StringConstructor) =
    Identity.Adapter.Exports.acceptConstructor ctor
let right (callback: Identity.Adapter.RightCallback) (value: Identity.Root.Right) : Identity.Root.Right =
    callback.run value
"""

let private sharedSources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs"; "next/Identity.Next.fs" ]

let private writePackageFile directory file (text: string) =
    let path = Path.Combine(directory, file)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

let private staticConsumer = """module Identity.Consumer
let tag: string = Identity.Adapter.Client.tag
let createText () : Identity.Root.Client = Identity.Adapter.Client.create "client"
let createNumber () : Identity.Root.Client = Identity.Adapter.Client.create 2.0
let construct () : Identity.Root.Client = Identity.Adapter.Client.Create "client"
let generic () : string = Identity.Adapter.Client.identity "value"
let setCount () = Identity.Adapter.Client.count <- 3.0
let getCount () : float = Identity.Adapter.Client.count
let accept (client: Identity.Adapter.Client) = Identity.Root.Exports.``use`` client
"""

[<Tests>]
let tests =
    testList "declaration catalog" [
        testCase "renamed exports and generic subpath types share producer identity" <| fun _ ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-declaration-catalog-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let root = configured directory "Root" "index.d.ts" [||]
                let producer = Path.Combine(directory, "root")
                Pipeline.run root fixture producer |> Async.RunSynchronously |> ignore
                let catalog = Path.Combine(producer, "declarations.json")
                File.Exists catalog |> Flip.Expect.equal "producer emits its declaration catalog" true
                let adapter = configured directory "Adapter" "adapter.d.ts" [| catalog |]
                Pipeline.run adapter fixture (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let next = configured directory "Next" "next.d.ts" [| Path.GetRelativePath(fixture, Path.Combine(directory, "adapter", "declarations.json")) |]
                Pipeline.run next fixture (Path.Combine(directory, "next")) |> Async.RunSynchronously |> ignore
                use chained = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, "next", "declarations.json")))
                chained.RootElement.GetProperty("owners").EnumerateArray()
                |> Seq.map (fun owner -> owner.GetProperty("name").GetString())
                |> Seq.toList
                |> Flip.Expect.equal "inherited owners precede consumers" [ "Identity.Root"; "Identity.Adapter"; "Identity.Next" ]
                let exitCode, output = compileConsumer directory sharedSources sharedConsumer
                exitCode |> Flip.Expect.equal output 0
                let invalid = sharedConsumer + "\nlet invalid (callback: Identity.Adapter.RightCallback) (left: Identity.Root.Left) = callback.run left\n"
                let exitCode, output = compileConsumer directory sharedSources invalid
                (exitCode <> 0 && output.Contains "FS0001") |> Flip.Expect.equal output true
            finally
                Directory.Delete(directory, true)

        testCase "private producer ownership preserves a later class value export" <| fun _ ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-catalog-statics-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let root = configured directory "Root" "static-root.d.ts" [||]
                Pipeline.run root fixture (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                let reference = Path.Combine(directory, "root", "declarations.json")
                let adapter = configured directory "Adapter" "static-adapter.d.ts" [| reference |]
                Pipeline.run adapter fixture (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] staticConsumer
                code |> Flip.Expect.equal output 0
            finally Directory.Delete(directory, true)

        testCase "unchanged nested dependency versions remain separate inputs" <| fun _ ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-catalog-versions-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                writePackageFile directory "package.json" """{"name":"versions-lab","version":"1.0.0","type":"module"}"""
                writePackageFile directory "index.d.ts" """export { One } from "dep"; export { Two } from "owner";"""
                writePackageFile directory "adapter.d.ts" """import { One, Two } from "./index.js"; export function one(value: One): One; export function two(value: Two): Two;"""
                writePackageFile directory "node_modules/dep/package.json" """{"name":"dep","version":"1.0.0","types":"index.d.ts"}"""
                writePackageFile directory "node_modules/dep/index.d.ts" "export interface One { one: string; }"
                writePackageFile directory "node_modules/owner/package.json" """{"name":"owner","version":"1.0.0","types":"index.d.ts"}"""
                writePackageFile directory "node_modules/owner/index.d.ts" """export { Two } from "dep";"""
                writePackageFile directory "node_modules/owner/node_modules/dep/package.json" """{"name":"dep","version":"2.0.0","types":"index.d.ts"}"""
                writePackageFile directory "node_modules/owner/node_modules/dep/index.d.ts" "export interface Two { two: number; }"
                let groups = Map.ofList [ "dep", Ship; "owner", Ship ]
                let root = { configured directory "Root" "index.d.ts" [||] with Groups = groups }
                Pipeline.run root directory (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                let reference = Path.Combine(directory, "root", "declarations.json")
                let adapter = { configured directory "Adapter" "adapter.d.ts" [| reference |] with Groups = groups }
                Pipeline.run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                use catalog = JsonDocument.Parse(File.ReadAllText reference)
                catalog.RootElement.GetProperty("inputs").EnumerateArray()
                |> Seq.filter (fun source -> source.GetProperty("package").GetString() = "dep")
                |> Seq.map (fun source -> source.GetProperty("version").GetString())
                |> Seq.distinct |> Seq.sort |> Seq.toList
                |> Flip.Expect.equal "dependency versions keep separate provenance" [ "1.0.0"; "2.0.0" ]
            finally Directory.Delete(directory, true)

        let inline (==>) input expected = input, expected
        testTheory "only suppressed unresolved imports may certify a catalog" [
            "" ==> false
            "// @ts-ignore\n" ==> true
        ] <| fun (prefix, succeeds) ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-catalog-missing-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                writePackageFile directory "package.json" """{"name":"missing-lab","version":"1.0.0","type":"module"}"""
                writePackageFile directory "index.d.ts" (prefix + "import { Missing } from 'missing-catalog-provider'; export function use(value: Missing): void;")
                let config = { configured directory "Root" "index.d.ts" [||] with Types = None }
                let output = Path.Combine(directory, "output")
                let message = try Pipeline.run config directory output |> Async.RunSynchronously |> ignore; "" with error -> error.Message
                (Directory.Exists output, message.Contains "TS2307") |> Flip.Expect.equal message (succeeds, not succeeds)
            finally Directory.Delete(directory, true)

        let inline (=!>) mutation expected = mutation, expected
        testTheory "incompatible producers fail before output is written" [
            "compiler" =!> "different compiler"
            "generator" =!> "different generator"
            "inferenceProfile" =!> "different inference profile"
            "manifest" =!> "package manifest mismatch"
            "input" =!> "input source hash mismatch"
            "api" =!> "F# API mismatch"
            "source" =!> "source hash mismatch"
            "arity" =!> "arity mismatch"
            "constraints" =!> "constraint mismatch"
            "cycle" =!> "owner dependency cycle"
            "merged" =!> "declaration handle set"
        ] <| fun (mutation, expected) ->
            let directory = Path.Combine(Path.GetTempPath(), "xantham-catalog-conflict-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let root = configured directory "Root" "index.d.ts" [||]
                let producer = Path.Combine(directory, "root")
                Pipeline.run root fixture producer |> Async.RunSynchronously |> ignore
                let path = Path.Combine(producer, "declarations.json")
                let catalog = JsonNode.Parse(File.ReadAllText path)
                let declarations = catalog["declarations"].AsArray()
                let client = declarations |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.PublicClient")
                let box = declarations |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.PublicBox")
                match mutation with
                | "compiler" | "generator" | "inferenceProfile" -> catalog[mutation] <- JsonValue.Create "incompatible"
                | "manifest" -> (catalog["inputs"][0])["manifestSha256"] <- JsonValue.Create "changed"
                | "input" -> (catalog["inputs"][0])["sha256"] <- JsonValue.Create "changed"
                | "api" -> client["api"] <- JsonValue.Create "changed"
                | "source" -> (client["sources"][0])["sha256"] <- JsonValue.Create "changed"
                | "arity" ->
                    client["arity"] <- JsonValue.Create 1
                    client["constraints"] <- JsonArray(JsonValue.Create "None")
                | "constraints" -> box["constraints"][0] <- JsonValue.Create "None"
                | "cycle" -> (catalog["owners"][0])["dependencies"] <- JsonArray(JsonValue.Create "Identity.Root")
                | "merged" -> ()
                | _ -> failtest "unknown mutation"
                File.WriteAllText(path, catalog.ToJsonString())
                let entry = if mutation = "merged" then "augmented.d.ts" else "adapter.d.ts"
                let config = configured directory "Adapter" entry [| path |]
                let output = Path.Combine(directory, "adapter")
                let message =
                    try Pipeline.run config fixture output |> Async.RunSynchronously |> ignore; ""
                    with error -> error.Message
                (message.Contains expected, Directory.Exists output)
                |> Flip.Expect.equal message (true, false)
            finally
                Directory.Delete(directory, true)
    ]
