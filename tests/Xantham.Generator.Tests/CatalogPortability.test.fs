module Xantham.Generator.Tests.CatalogPortabilityTests

open System
open System.IO
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator
open Xantham.Generator.CatalogCompatibility

let private fixture = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "catalog-portability-lab"))

let private withProducer run =
    use scratch = Scratch.directory "catalog-portability"
    let package = Path.Combine(scratch.Path, "package")
    for file in Directory.EnumerateFiles(fixture, "*", SearchOption.AllDirectories) do
        let target = Path.Combine(package, Path.GetRelativePath(fixture, file))
        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
        File.Copy(file, target)
    let config = GeneratorConfig.load package
    let producer = { config with ModuleName = Some "Portability.Root" }
    let dependency = Path.Combine(package, "node_modules", "catalog-portability-owner-lab")
    let root = Path.Combine(scratch.Path, "root")
    Pipeline.run producer dependency root |> Async.RunSynchronously |> ignore
    let catalog = Path.Combine(root, "declarations.json")
    let consumer = { config with DeclarationReferences = [catalog] }
    run scratch.Path package dependency consumer catalog

let private edit path mutate =
    let document = JsonNode.Parse(File.ReadAllText path)
    mutate document
    File.WriteAllText(path, document.ToJsonString())

let private declaration (document: JsonNode) =
    document["declarations"].AsArray()
    |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>().EndsWith ".Box")

let private assertRejected part (mutate: JsonNode -> unit) =
    withProducer (fun directory package _ config catalog ->
        edit catalog mutate
        let output = Path.Combine(directory, "rejected")
        Expect.throwsC
            (fun () -> Pipeline.run config package output |> Async.RunSynchronously |> ignore)
            (fun error -> Expect.stringContains error.Message part "catalogue guard remains active")
        Expect.isFalse (Directory.Exists output) "rejected catalogue writes no consumer output")

[<Tests>]
let tests =
    testList "catalog portability" [
        testCase "compression record copy preserves inference authentication" <| fun _ ->
            withProducer (fun directory package _ config _ ->
                let compressedConsumer = { config with DeclarationCatalogCompression = CatalogCompression.Brotli }
                let report = Pipeline.run compressedConsumer package (Path.Combine(directory, "different-output-compression")) |> Async.RunSynchronously
                Expect.contains report.OutputFiles "declarations.json.br" "output format is independent of authenticated inference")
        testCase "equivalent portable toolchains generate a compiled consumer" <| fun _ ->
            withProducer (fun directory package _ config catalog ->
                edit catalog (fun document ->
                    document["compiler"] <- JsonValue.Create "other-platform-binary"
                    document["generator"] <- JsonValue.Create "equivalent-generator-build")
                Pipeline.run config package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let code, output =
                    DeclarationCatalogTests.compileConsumer directory
                        ["root/Portability.Root.fs"; "adapter/Portability.Adapter.fs"]
                        "module Portability.Consumer\nlet accept (value: Portability.Root.Box<string>) : Portability.Root.Box<string> = Portability.Adapter.Exports.accept value\nlet read (value: Portability.Root.Box<string>) : string = value.value\n"
                Expect.equal code 0 output)

        testCase "schema two records portable compiler and contract metadata" <| fun _ ->
            withProducer (fun _ _ _ _ catalog ->
                let document = JsonNode.Parse(File.ReadAllText catalog)
                Expect.equal (document["schemaVersion"].GetValue<int>()) 2 "new catalogue schema"
                let metadata = document["compatibility"]
                let compiler = metadata["compiler"]
                Expect.equal (compiler["kind"].GetValue<string>()) "typescript-package" "recognized installed compiler"
                Expect.equal (compiler["astProtocolVersion"].GetValue<int>()) 8 "binary AST protocol")

        testCase "legacy catalogue remains usable under exact fingerprints" <| fun _ ->
            withProducer (fun directory package _ config catalog ->
                edit catalog (fun document ->
                    document["schemaVersion"] <- JsonValue.Create 1
                    document.AsObject().Remove "compatibility" |> ignore)
                Pipeline.run config package (Path.Combine(directory, "legacy")) |> Async.RunSynchronously |> ignore)

        for key, part in ["compiler", "compiler"; "generator", "generator"] do
            testCase ("legacy rejects changed " + key) <| fun _ ->
                assertRejected part (fun document ->
                    document["schemaVersion"] <- JsonValue.Create 1
                    document.AsObject().Remove "compatibility" |> ignore
                    document[key] <- JsonValue.Create "different")

        for key, part in [
            "contractVersion", "contract version"
            "identityVersion", "identity version"
            "apiVersion", "API version"
            "inferenceVersion", "inference version"
            "customizationVersion", "customization version"
        ] do
            testCase ("pipeline rejects incompatible " + key) <| fun _ ->
                assertRejected part (fun document ->
                    let metadata = document["compatibility"]
                    metadata[key] <- JsonValue.Create 2)

        testCase "schema two cannot omit compatibility" <| fun _ ->
            assertRejected "compatibility" (fun document -> document.AsObject().Remove "compatibility" |> ignore)

        testCase "inference profile remains authenticated" <| fun _ ->
            assertRejected "inference profile" (fun document -> document["inferenceProfile"] <- JsonValue.Create "different")

        testCase "portable contract still authenticates FSharp API" <| fun _ ->
            assertRejected "F# API mismatch" (fun document -> (declaration document)["api"] <- JsonValue.Create "different")

        testCase "portable contract still authenticates arity" <| fun _ ->
            assertRejected "arity mismatch" (fun document ->
                let entry = declaration document
                entry["arity"] <- JsonValue.Create 2
                entry["constraints"] <- JsonNode.Parse "[\"none\",\"none\"]")

        testCase "portable contract still authenticates constraints" <| fun _ ->
            assertRejected "constraint mismatch" (fun document -> (declaration document)["constraints"] <- JsonNode.Parse "[\"different\"]")

        testCase "portable contract still authenticates source bytes" <| fun _ ->
            withProducer (fun directory package dependency config _ ->
                File.AppendAllText(Path.Combine(dependency, "index.d.ts"), "\n")
                Expect.throwsC
                    (fun () -> Pipeline.run config package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                    (fun error -> Expect.stringContains error.Message "input source hash mismatch" "shared input remains authenticated"))

        for legacy in [false; true] do
          testCase ("adapter catalogue retains authenticated owner dependency chain, legacy=" + string legacy) <| fun _ ->
            withProducer (fun directory package _ config catalog ->
                if legacy then
                    edit catalog (fun document ->
                        document["schemaVersion"] <- JsonValue.Create 1
                        document.AsObject().Remove "compatibility" |> ignore)
                let adapter = Path.Combine(directory, "adapter")
                Pipeline.run config package adapter |> Async.RunSynchronously |> ignore
                let chained =
                    { config with ModuleName = Some "Portability.Next"
                                  DeclarationReferences = [Path.Combine(adapter, "declarations.json")] }
                Pipeline.run chained package (Path.Combine(directory, "next")) |> Async.RunSynchronously |> ignore
                let code, output =
                    DeclarationCatalogTests.compileConsumer directory
                        ["root/Portability.Root.fs"; "adapter/Portability.Adapter.fs"; "next/Portability.Next.fs"]
                        "module Portability.Consumer\nlet accept (value: Portability.Root.Box<string>) : Portability.Root.Box<string> = Portability.Next.Exports.accept value\n"
                Expect.equal code 0 output)

        testCase "cached producer executes discovery once across authentication passes" <| fun _ ->
            let mutable runs = 0
            let operation = async {
                runs <- runs + 1
                return { Compiler = "compiler"; Generator = "generator"; InferenceProfile = "profile"
                         Contract = current (Binary 8u) } }
            let producer = DeclarationCatalog.cacheProducer operation
            [producer (); producer ()] |> Async.Parallel |> Async.RunSynchronously |> ignore
            Expect.equal runs 1 "preliminary and final consumers share one identity probe"
    ]
