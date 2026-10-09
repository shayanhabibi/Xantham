module Xantham.Generator.Tests.CatalogLibraryTests

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator

let private repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let private input = Path.Combine(repository, "tools", "fable-core-ts-input")
let private version = "7.1.0-dev.20260902.1"
let private revision = "43a90f4c105bc9db7cb7aa299beddafbabe1d23e"
let private compiler = CatalogCompatibility.TypeScriptPackage(version, revision, 8u)

let private manifest directory name release commit =
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(Path.Combine(directory, "package.json"),
        $"""{{"name":"{name}","version":"{release}","gitHead":"{commit}","os":["platform-only"],"cpu":["architecture-only"]}}""")

let private platforms = [
    "typescript"
    "@typescript/typescript-win32-x64"
    "@typescript/typescript-win32-arm64"
    "@typescript/typescript-linux-x64"
    "@typescript/typescript-linux-arm"
    "@typescript/typescript-linux-arm64"
    "@typescript/typescript-darwin-x64"
    "@typescript/typescript-darwin-arm64"
]

let private coreCatalog = Path.Combine(repository, "src", "Xantham.Fable.Core.TS", "declarations.json")

let private readerCatalog () =
    use document = JsonDocument.Parse(File.ReadAllText coreCatalog)
    let root = document.RootElement
    let result = JsonObject()
    for property in root.EnumerateObject() do
        if property.Name <> "declarations" then
            result[property.Name] <- JsonNode.Parse(property.Value.GetRawText())
    let declarations = JsonArray()
    for declaration in root.GetProperty("declarations").EnumerateArray() do
        if ["Fable.Core.TS.Dom.HTMLElement"; "Fable.Core.TS.Es.Map"] |> List.contains (declaration.GetProperty("fSharpName").GetString()) then
            declarations.Add(JsonNode.Parse(declaration.GetRawText()))
    result["declarations"] <- declarations
    result :> JsonNode

let private withConsumer run =
    use scratch = Scratch.directory "catalog-library-reader"
    let package = Path.Combine(scratch.Path, "package")
    Directory.CreateDirectory package |> ignore
    File.WriteAllText(Path.Combine(package, "package.json"), """{"name":"catalog-library-reader-lab","version":"1.0.0","types":"index.d.ts"}""")
    File.WriteAllText(Path.Combine(package, "index.d.ts"), "export function accept(value: Map<string, HTMLElement>): Map<string, HTMLElement>;\n")
    let config =
        { GeneratorConfig.load input with
            ModuleName = Some "Library.Consumer"
            CompilerLib = CompilerLibConfig.Default
            DeclarationCatalog = true
            DeclarationReferences = [coreCatalog] }
    run scratch.Path package config

[<Tests>]
let tests = testSequenced <| testList "catalog library identity" [
    for package in platforms do
        testCase ("canonical release identity for " + package) <| fun () ->
            use scratch = Scratch.directory "catalog-library-metadata"
            manifest scratch.Path package version (revision.ToUpperInvariant())
            let expected =
                $"""{{"gitHead":"{revision}","name":"typescript","version":"{version}"}}"""
                |> Encoding.UTF8.GetBytes |> SHA256.HashData |> Convert.ToHexStringLower
            Expect.equal (CatalogLibrary.tryIdentity compiler scratch.Path package version "lib/lib.es5.d.ts")
                (Some("typescript", expected)) "release metadata excludes OS and CPU while retaining exact revision"
    for package, file in [
        "@typescript/typescript-win32-x64", "lib/index.d.ts"
        "@typescript/typescript-win32-x64", "other/lib.es5.d.ts"
        "@typescript/typescript-win32-x64", "lib/nested/lib.es5.d.ts"
        "@typescript/typescript-impostor", "lib/lib.es5.d.ts"
        "ordinary-library", "lib/lib.es5.d.ts"
    ] do
        testCase ("ordinary source retains manifest identity " + package + "/" + file) <| fun () ->
            use scratch = Scratch.directory "catalog-library-ordinary"
            manifest scratch.Path package version revision
            Expect.isNone (CatalogLibrary.tryIdentity compiler scratch.Path package version file) "normal package manifests remain authenticated as bytes"
    for name, release, commit, part in [
        "typescript", "different", revision, "version"
        "typescript", version, String.replicate 40 "a", "revision"
        "typescript", version, "invalid", "gitHead"
        "wrong-package", version, revision, "name"
    ] do
        testCase ("library rejects incompatible metadata " + part) <| fun () ->
            use scratch = Scratch.directory "catalog-library-invalid"
            manifest scratch.Path name release commit
            Expect.throwsC
                (fun () -> CatalogLibrary.tryIdentity compiler scratch.Path "typescript" release "lib/lib.es5.d.ts" |> ignore)
                (fun error -> Expect.stringContains error.Message part "identity disagreement is explicit")
    testCase "binary fallback permits library metadata without gitHead" <| fun () ->
        use scratch = Scratch.directory "catalog-library-binary"
        File.WriteAllText(Path.Combine(scratch.Path, "package.json"), $"""{{"name":"typescript","version":"{version}"}}""")
        let expected =
            $"""{{"gitHead":null,"name":"typescript","version":"{version}"}}"""
            |> Encoding.UTF8.GetBytes |> SHA256.HashData |> Convert.ToHexStringLower
        Expect.equal (CatalogLibrary.tryIdentity (CatalogCompatibility.Binary 8u) scratch.Path "typescript" version "lib/lib.es5.d.ts")
            (Some("typescript", expected)) "binary authentication remains the fallback for absent release metadata"
        Expect.throws (fun () -> CatalogLibrary.tryIdentity compiler scratch.Path "typescript" version "lib/lib.es5.d.ts" |> ignore)
            "recognized compiler requires matching release metadata"
    for label, part, mutate in [
        "source bytes", "input source hash mismatch", fun (document: JsonNode) -> document["inputs"].AsArray() |> Seq.find (fun s -> s["file"].GetValue<string>() = "lib/lib.es5.d.ts") |> fun s -> s["sha256"] <- JsonValue.Create "changed"
        "release fingerprint", "package manifest mismatch", fun document -> document["inputs"].AsArray() |> Seq.find (fun s -> s["file"].GetValue<string>() = "lib/lib.es5.d.ts") |> fun s -> s["manifestSha256"] <- JsonValue.Create "changed"
        "API", "F# API mismatch", fun document -> document["declarations"].AsArray() |> Seq.find (fun d -> d["fSharpName"].GetValue<string>() = "Fable.Core.TS.Dom.HTMLElement") |> fun d -> d["api"] <- JsonValue.Create "changed"
        "library source identity", "source hash mismatch", fun document -> document["declarations"].AsArray() |> Seq.find (fun d -> d["fSharpName"].GetValue<string>() = "Fable.Core.TS.Dom.HTMLElement") |> fun d -> d["sources"][0]["package"] <- JsonValue.Create "other-library"
        "compiler release", "compiler version", fun document -> document["compatibility"]["compiler"]["version"] <- JsonValue.Create "other-version"
        "compiler revision", "compiler revision", fun document -> document["compatibility"]["compiler"]["revision"] <- JsonValue.Create (String.replicate 40 "a")
        "AST protocol", "AST protocol", fun document -> document["compatibility"]["compiler"]["astProtocolVersion"] <- JsonValue.Create 9
        "old platform identity contract", "identity version", fun document -> document["compatibility"]["identityVersion"] <- JsonValue.Create 2
    ] do
        testCase ("reader rejects changed " + label) <| fun () ->
            withConsumer (fun directory package config ->
                let document = readerCatalog ()
                mutate document
                let reference = Path.Combine(directory, "changed.json")
                File.WriteAllText(reference, document.ToJsonString())
                let output = Path.Combine(directory, "rejected")
                Expect.throwsC
                    (fun () -> Pipeline.run { config with DeclarationReferences = [reference] } package output |> Async.RunSynchronously |> ignore)
                    (fun error -> Expect.stringContains error.Message part "reader authentication remains active")
                Expect.isFalse (Directory.Exists output) "authentication precedes disk output")
    testCase "Core.TS library emissions use logical TypeScript ownership" <| fun () ->
        use scratch = Scratch.directory "catalog-library-emission"
        let config = { GeneratorConfig.load input with DeclarationCatalog = true }
        Pipeline.run config input scratch.Path |> Async.RunSynchronously |> ignore
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(scratch.Path, "declarations.json")))
        let catalog = document.RootElement
        let sources =
            [ yield! catalog.GetProperty("inputs").EnumerateArray()
              for declaration in catalog.GetProperty("declarations").EnumerateArray() do
                  yield! declaration.GetProperty("sources").EnumerateArray() ]
            |> List.filter (fun source -> source.GetProperty("file").GetString().StartsWith("lib/lib."))
        Expect.isNonEmpty sources "real compiler libraries were emitted"
        let es5 = sources |> List.find (fun source -> source.GetProperty("file").GetString() = "lib/lib.es5.d.ts")
        Expect.equal (es5.GetProperty("sha256").GetString())
            "6388847232654d7fcbed7fe89ea511a65ad1f5104b41e905c43f90910e1408b6"
            "Windows library bytes match the Linux-produced Core.TS catalogue at origin/develop"
        for source in sources do
            Expect.equal (source.GetProperty("package").GetString()) "typescript" "library ownership is independent of OS and CPU"
        for declaration in catalog.GetProperty("declarations").EnumerateArray() do
            for handle in declaration.GetProperty("handles").EnumerateArray() do
                Expect.isFalse (handle.GetString().Contains("@typescript/typescript-")) "canonical handles exclude platform distribution names"
        use committed = JsonDocument.Parse(File.ReadAllText coreCatalog)
        for property in catalog.EnumerateObject() do
            if not (["compiler"; "generator"] |> List.contains property.Name) then
                let expected = committed.RootElement.GetProperty property.Name
                if property.Name = "declarations" && not (JsonElement.DeepEquals(property.Value, expected)) then
                    let byName = expected.EnumerateArray() |> Seq.map (fun d -> d.GetProperty("fSharpName").GetString(), d) |> Map.ofSeq
                    let changes =
                        property.Value.EnumerateArray()
                        |> Seq.choose (fun actual ->
                            let name = actual.GetProperty("fSharpName").GetString()
                            match Map.tryFind name byName with
                            | Some old when JsonElement.DeepEquals(actual, old) -> None
                            | Some old ->
                                let fields = actual.EnumerateObject() |> Seq.filter (fun p -> not (JsonElement.DeepEquals(p.Value, old.GetProperty p.Name))) |> Seq.map _.Name |> String.concat ","
                                Some(name + ": " + fields + "; identity=" + actual.GetProperty("identity").GetString() + "; api=" + actual.GetProperty("api").GetString())
                            | None -> Some(name + ": new declaration"))
                        |> Seq.toList
                    failtest (sprintf "Core.TS declarations differ: %d changed entries; %A" changes.Length (List.truncate 5 changes))
                Expect.isTrue (JsonElement.DeepEquals(property.Value, expected))
                    ("Core.TS " + property.Name + " matches the checked-in catalogue on every OS")
]

[<Tests>]
let reuseTests = testSequenced <| testList "catalog library reuse" [
    testCase "compiler-only reader reuses the full Core.TS catalogue in a compiled consumer" <| fun () ->
        withConsumer (fun directory package config ->
            File.WriteAllText(Path.Combine(package, "index.d.ts"), "export {};\n")
            let output = Path.Combine(directory, "consumer")
            Pipeline.run config package output |> Async.RunSynchronously |> ignore
            use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "declarations.json")))
            let element = document.RootElement.GetProperty("declarations").EnumerateArray() |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Fable.Core.TS.Dom.HTMLElement")
            Expect.equal (element.GetProperty("owner").GetString()) "FableCoreTsInput" "library identity resolves to the accepted owner"
            let code, errors =
                DeclarationCatalogTests.compileConsumer directory ["consumer/groups/TypeScript.Lib.fs"; "consumer/Library.Consumer.fs"]
                    "module Library.Check\nlet accept (value: TypeScript.Lib.Es.Map<string, TypeScript.Lib.Dom.HTMLElement>) : Fable.Core.TS.Es.Map<string, Fable.Core.TS.Dom.HTMLElement> = value\n"
            Expect.equal code 0 errors)
]
