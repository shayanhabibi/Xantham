module Xantham.Generator.Tests.CatalogPortabilityTests

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator
open Xantham.Generator.CatalogCompatibility

let private fixture = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "catalog-portability-lab"))

let private withProducer compression run =
    use scratch = Scratch.directory "catalog-portability"
    let package = Path.Combine(scratch.Path, "package")
    for file in Directory.EnumerateFiles(fixture, "*", SearchOption.AllDirectories) do
        let target = Path.Combine(package, Path.GetRelativePath(fixture, file))
        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
        File.Copy(file, target)
    let config = GeneratorConfig.load package
    let producer = { config with ModuleName = Some "Portability.Root"; DeclarationCatalogCompression = compression }
    let dependency = Path.Combine(package, "node_modules", "catalog-portability-owner-lab")
    let root = Path.Combine(scratch.Path, "root")
    Pipeline.run producer dependency root |> Async.RunSynchronously |> ignore
    let catalog = Path.Combine(root, if compression = CatalogCompression.Brotli then "declarations.json.br" else "declarations.json")
    let consumer = { config with DeclarationReferences = [catalog] }
    run scratch.Path package dependency consumer catalog

let private readText (path: string) =
    if path.EndsWith(".br") then
        use file = File.OpenRead path
        use decoder = new BrotliStream(file, CompressionMode.Decompress)
        use reader = new StreamReader(decoder, Encoding.UTF8)
        reader.ReadToEnd()
    else File.ReadAllText path

let private writeText (path: string) (text: string) =
    if path.EndsWith(".br") then
        use file = File.Create path
        use encoder = new BrotliStream(file, CompressionLevel.SmallestSize)
        let bytes = Encoding.UTF8.GetBytes text
        encoder.Write(bytes, 0, bytes.Length)
    else File.WriteAllText(path, text)

let private edit path mutate =
    let document = JsonNode.Parse(readText path)
    mutate document
    writeText path (document.ToJsonString())

let private declaration (document: JsonNode) =
    document["declarations"].AsArray()
    |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>().EndsWith ".Box")

let private assertRejected compression part (mutate: JsonNode -> unit) =
    withProducer compression (fun directory package _ config catalog ->
        edit catalog mutate
        let output = Path.Combine(directory, "rejected")
        Expect.throwsC
            (fun () -> Pipeline.run config package output |> Async.RunSynchronously |> ignore)
            (fun error -> Expect.stringContains error.Message part "catalogue guard remains active")
        Expect.isFalse (Directory.Exists output) "rejected catalogue writes no consumer output")

let private testsFor compression =
    let withProducer = withProducer compression
    let assertRejected = assertRejected compression
    testList ("catalog portability " + string compression) [
        for key in [ "schemaVersion"; "contractVersion"; "apiVersion" ] do
            testCase ("duplicate or malformed metadata " + key) <| fun () ->
                withProducer (fun directory package _ config catalog ->
                    let text = readText catalog
                    let document = JsonNode.Parse text
                    let metadata = if key = "schemaVersion" then document else document["compatibility"]
                    let value = metadata[key].ToJsonString()
                    let originalCompact = "\"" + key + "\":" + value
                    let duplicate = if key = "apiVersion" then "\"invalid\"" else value
                    let replacement = originalCompact + ",\"" + key + "\":" + duplicate
                    let compact = text.Replace(": ", ":").Replace(":\r\n", ":")
                    Expect.stringContains compact originalCompact "mutation targets real metadata"
                    writeText catalog (compact.Replace(originalCompact, replacement))
                    let output = Path.Combine(directory, "duplicate")
                    Expect.throws (fun () -> Pipeline.run config package output |> Async.RunSynchronously |> ignore) "metadata refused"
                    Expect.isFalse (Directory.Exists output) "metadata failure writes no output")
        testCase "later corrupt Brotli reference fails before all output" <| fun () ->
            withProducer (fun directory package _ config _ ->
                let corrupt = Path.Combine(directory, "corrupt.json.br")
                File.WriteAllBytes(corrupt, [|0uy|])
                let mixed = { config with DeclarationReferences = config.DeclarationReferences @ [corrupt] }
                let output = Path.Combine(directory, "rejected-later")
                Expect.throwsC
                    (fun () -> Pipeline.run mixed package output |> Async.RunSynchronously |> ignore)
                    (fun error -> Expect.stringContains error.Message corrupt "later reference identified")
                Expect.isFalse (Directory.Exists output) "all generation happens before writes"
                use reopened = new FileStream(corrupt, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
                ())
        testCase "switching format retains alternate file and reports current output" <| fun () ->
            withProducer (fun directory _ dependency config catalog ->
                let root = Path.GetDirectoryName catalog
                let original = File.ReadAllBytes catalog
                let other = if compression = CatalogCompression.Brotli then CatalogCompression.Uncompressed else CatalogCompression.Brotli
                let producer = { config with ModuleName = Some "Portability.Root"; DeclarationReferences = []; DeclarationCatalogCompression = other }
                let report = Pipeline.run producer dependency root |> Async.RunSynchronously
                let expected = if other = CatalogCompression.Brotli then "declarations.json.br" else "declarations.json"
                Expect.contains report.OutputFiles expected "selected format"
                Expect.isFalse (report.OutputFiles |> List.contains (Path.GetFileName catalog)) "alternate excluded from report"
                Expect.sequenceEqual (File.ReadAllBytes catalog) original "alternate preserved")
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
                let document = JsonNode.Parse(readText catalog)
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
                    metadata[key] <- JsonValue.Create(metadata[key].GetValue<int>() + 1))

        testCase "schema two cannot omit compatibility" <| fun _ ->
            assertRejected "compatibility" (fun document -> document.AsObject().Remove "compatibility" |> ignore)

        testCase "malformed nested compiler metadata rejects" <| fun _ ->
            assertRejected "compiler" (fun document -> document["compatibility"]["compiler"] <- JsonValue.Create 1)

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
                let adapterCompression = if compression = CatalogCompression.Brotli then CatalogCompression.Uncompressed else CatalogCompression.Brotli
                let adapterConfig = { config with DeclarationCatalogCompression = adapterCompression }
                Pipeline.run adapterConfig package adapter |> Async.RunSynchronously |> ignore
                let chained =
                    { config with ModuleName = Some "Portability.Next"
                                  DeclarationReferences = [Path.Combine(adapter, if adapterCompression = CatalogCompression.Brotli then "declarations.json.br" else "declarations.json")] }
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

[<Tests>]
let tests = testList "catalog portability formats" [testsFor CatalogCompression.Uncompressed; testsFor CatalogCompression.Brotli]
