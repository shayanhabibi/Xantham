module Xantham.Generator.Tests.CatalogLibraryEntryTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator
open Xantham.Generator.Measure

let private repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

[<Tests>]
let tests = testSequenced <| testList "catalog library entry variants" [
    for label, entry in [
        "empty module", "export {};"
        "function", "export function accept(value: TextStreamReader): TextStreamReader;"
        "value", "export const options: TextStreamReader;"
        "interface", "export interface Consumer { options: TextStreamReader; }"
        "namespace", "export namespace Consumer { function accept(value: TextStreamReader): void; }"
        "default", "export default function accept(value: TextStreamReader): void;"
        "re-export", "export { accept } from './api';"
        "unrelated value", "export const enabled: boolean;"
        "intrinsic aliases", "export type Enabled = boolean; export type Label = string; export type Count = number; export const enabled: boolean; export const label: string; export const count: number;"
        "local library-name collision", "export interface TextStreamReader { local: string; }"
        "local readonly support-name collision", "export interface ReadonlyRecord { local: string; } export interface Dictionary { readonly [key: string]: number; } export function accept(value: TextStreamReader): void;"
        "global script", "interface ConsumerOptions { options: TextStreamReader; }"
        "mixed public paths", "export const enabled: boolean;"
    ] do
        testCase (label + " retains complete authenticated compiler library") <| fun () ->
          CatalogLibraryLab.withProducer (fun reference library ->
            use scratch = Scratch.directory "catalog-library-entry-lab"
            let package = Path.Combine(scratch.Path, "package")
            Directory.CreateDirectory package |> ignore
            File.WriteAllText(Path.Combine(package, "package.json"),
                """{"name":"catalog-library-entry-lab","version":"1.0.0","types":"index.d.ts"}""")
            File.WriteAllText(Path.Combine(package, "index.d.ts"), entry + "\n")
            File.WriteAllText(Path.Combine(package, "api.d.ts"),
                "export function accept(value: TextStreamReader): void;\n")
            File.WriteAllText(Path.Combine(package, "globals.d.ts"),
                "interface ConsumerOptions { options: TextStreamReader; }\n")
            let config =
                { CatalogLibraryLab.config with
                    ModuleName = Some "Library.Consumer"
                    CompilerLib = CompilerLibConfig.Default
                    DeclarationCatalog = true
                    PublicInputs =
                        if label = "mixed public paths" then Some(Map.ofList [".", "index.d.ts"; "./globals", "globals.d.ts"])
                        else None
                    DeclarationReferences = [reference] }
            let output = Path.Combine(scratch.Path, "consumer")
            Pipeline.run config package output |> Async.RunSynchronously |> ignore
            use actual = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "declarations.json")))
            use producer = JsonDocument.Parse(File.ReadAllText reference)
            let declarations = actual.RootElement.GetProperty("declarations").EnumerateArray() |> Seq.toArray
            for name in ["Library.Owner.Dom.TextStreamReader"; "Library.Owner.Dom.TextStreamWriter"] do
                let expected = producer.RootElement.GetProperty("declarations").EnumerateArray()
                               |> Seq.find (fun d -> d.GetProperty("fSharpName").GetString() = name)
                let reused = declarations |> Array.find (fun d -> d.GetProperty("fSharpName").GetString() = name)
                for field in ["identity"; "api"; "owner"] do
                    Expect.equal (reused.GetProperty(field).GetString()) (expected.GetProperty(field).GetString())
                        (name + " authenticates " + field + " independently of entry form")
            let readerAlias = if label = "local library-name collision" then "TextStreamReader2" else "TextStreamReader"
            let code, errors =
                DeclarationCatalogTests.compileConsumer scratch.Path
                    [library; "consumer/groups/TypeScript.Lib.fs"; "consumer/Library.Consumer.fs"]
                    ("module Library.Check\nlet accept (value: TypeScript.Lib.Dom." + readerAlias + ") : Library.Owner.Dom.TextStreamReader = value\nlet writer (value: TypeScript.Lib.Dom.TextStreamWriter) : Library.Owner.Dom.TextStreamWriter = value\n")
            Expect.equal code 0 errors)
]

[<Tests>]
let augmentationTests = testSequenced <| testList "catalog library ambient authentication" [
    testCase "global value augmentation cannot reuse an unchanged global-object catalogue" <| fun () ->
      CatalogLibraryLab.withProducer (fun reference _ ->
        use scratch = Scratch.directory "catalog-library-augmentation-lab"
        let package = Path.Combine(scratch.Path, "package")
        Directory.CreateDirectory package |> ignore
        File.WriteAllText(Path.Combine(package, "package.json"),
            """{"name":"catalog-library-augmentation-lab","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(package, "index.d.ts"),
            "declare function accept(value: TextStreamReader): void;")
        let config =
            { CatalogLibraryLab.config with
                CompilerLib = CompilerLibConfig.Default
                DeclarationCatalog = true
                DeclarationReferences = [reference] }
        let output = Path.Combine(scratch.Path, "rejected")
        Expect.throwsC
            (fun () -> Pipeline.run config package output |> Async.RunSynchronously |> ignore)
            (fun error -> Expect.stringContains error.Message "source hash mismatch" "changed globalThis closure remains authenticated")
        Expect.isFalse (Directory.Exists output) "authentication happens before writing output")
]

[<Tests>]
let harvestTests = testSequenced <| testList "catalog library harvest variants" [
    testCase "scripthost library surface is independent of module entry scope" <| fun () ->
        use scratch = Scratch.directory "catalog-library-harvest-lab"
        let fixture = Path.Combine(repository, "tests", "fixtures", "catalog-library-entry-lab")
        let package = Path.Combine(scratch.Path, "package")
        Directory.CreateDirectory package |> ignore
        File.Copy(Path.Combine(fixture, "package.json"), Path.Combine(package, "package.json"))
        let config = { GeneratorConfig.load fixture with DeclarationCatalog = false }
        let generate (configured: GeneratorConfig) (entry: string) =
            File.WriteAllText(Path.Combine(package, "index.d.ts"), entry)
            let mailbox, ctx = Bootstrap.start configured package |> Async.RunSynchronously
            use lifetime = mailbox
            let model, _ =
                Pipeline.runTier ctx Harvest.passes
                    { Exports = []; AmbientClasses = []; Namespaces = Map.empty; ShadowedByLib = 0 }
                |> Async.RunSynchronously
            let libraries =
                model.Exports
                |> List.filter (fun export -> Grouping.classify ctx.PackageDir (ValueSome export.Symbol) = CompilerLib
                                             && export.Symbol.DeclarationHandles |> ValueOption.exists (fun handles -> handles.Length > 0))
                |> List.map (fun export -> export.ExportName, export.Symbol.DeclarationHandles)
                |> Set.ofList
            let falseGlobals =
                model.Exports
                |> List.filter (fun export -> export.Origin = FromGlobal && Grouping.classify ctx.PackageDir (ValueSome export.Symbol) = EntryPackage)
            libraries, falseGlobals
        let expected, _ = generate config "export {};"
        Expect.isNonEmpty expected "the small library producer emits a real surface"
        for label, entry in [
            "function", File.ReadAllText(Path.Combine(fixture, "index.d.ts"))
            "value", "export const stream: TextStreamReader;"
            "interface", "export interface Consumer { stream: TextStreamReader; }"
            "namespace", "export namespace Consumer { function accept(value: TextStreamReader): void; }"
            "default", "export default function accept(value: TextStreamReader): void;"
            "unrelated", "export const enabled: boolean;"
            "shadow", "export interface TextStreamReader { local: string; }"
            "global-type", "interface Consumer { stream: TextStreamReader; }"
        ] do
            let actual, falseGlobals = generate config entry
            Expect.equal actual expected (label + " harvests every library declaration from the same source")
            if label <> "global-type" then
                Expect.isEmpty falseGlobals (label + " module exports are not harvested again as globals")
        for disposition in [Reference; Widen; Map Map.empty] do
            let configured = { config with Groups = Map.ofList ["typescript/lib" * uom<npmDependency>, disposition] }
            let actual, _ = generate configured "export const enabled: boolean;"
            Expect.isEmpty actual
                "non-shipping modes do not harvest library globals"
        let noLibraries = { config with Lib = Some [] }
        let actual, falseGlobals = generate noLibraries "/// <reference no-default-lib=\"true\"/>\nexport const enabled: boolean;"
        Expect.isEmpty actual "disabled libraries have no global library surface"
        Expect.isEmpty falseGlobals "missing libraries do not turn module exports into globals"
]

[<Tests>]
let intrinsicTests = testSequenced <| testList "catalog intrinsic declaration identity" [
    testCase "private and equal-valued public literal aliases retain authenticated reuse" <| fun () ->
        use scratch = Scratch.directory "catalog-intrinsic-alias-lab"
        File.WriteAllText(Path.Combine(scratch.Path, "package.json"),
            """{"name":"catalog-intrinsic-alias-lab","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(scratch.Path, "shared.d.ts"),
            "type Private = 'same'; type Twin = 'same'; export type First = 'same'; export type Second = 'same'; export type Count = 42; export interface Consumer { first: Private; second: Twin; count: Count; }\n")
        File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export * from './shared'; export const enabled: boolean;\n")
        File.WriteAllText(Path.Combine(scratch.Path, "adapter.d.ts"), "export * from './shared';\n")
        let configured moduleName entry references =
            { GeneratorConfig.Default with ModuleName = Some moduleName; Entry = Some entry
                                           Lib = Some ["es5"]; Types = Some []; DeclarationCatalog = true
                                           DeclarationReferences = references }
        let producer = Path.Combine(scratch.Path, "producer")
        Pipeline.run (configured "Intrinsic.Producer" "index.d.ts" []) scratch.Path producer |> Async.RunSynchronously |> ignore
        let reference = Path.Combine(producer, "declarations.json")
        use catalog = JsonDocument.Parse(File.ReadAllText reference)
        let aliases = catalog.RootElement.GetProperty("declarations").EnumerateArray()
                      |> Seq.filter (fun d -> List.contains (d.GetProperty("fSharpName").GetString()) ["Intrinsic.Producer.First"; "Intrinsic.Producer.Second"; "Intrinsic.Producer.Count"])
                      |> Seq.toList
        Expect.equal aliases.Length 3 "each exported literal alias has its own declaration"
        Expect.equal (aliases |> List.map (fun d -> d.GetProperty("identity").GetString()) |> List.distinct |> List.length) 3
            "equal literal values do not merge declaration identities"
        Pipeline.run (configured "Intrinsic.Adapter" "adapter.d.ts" [reference]) scratch.Path (Path.Combine(scratch.Path, "adapter"))
        |> Async.RunSynchronously |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer scratch.Path
                               ["producer/Intrinsic.Producer.fs"; "adapter/Intrinsic.Adapter.fs"]
                               "module Intrinsic.Check\nlet reuse (value: Intrinsic.Adapter.Consumer) : Intrinsic.Producer.Consumer = value\n"
        Expect.equal code 0 output
]
