module Xantham.Generator.Tests.CustomizationTests

open System.IO
open System
open System.Diagnostics
open Expecto
open Xantham.Generator
open Xantham.Generator.Customization

let private package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/customization-lab"))

let private supportAssembly root name =
    let configuration = DirectoryInfo(AppContext.BaseDirectory).Parent.Name
    Path.Combine(root, "src", name, "bin", configuration, "net8.0", name + ".dll")

let private snapshot path =
    async {
        let! mailbox, ctx = Bootstrap.start GeneratorConfig.Default path
        use _ = mailbox :> IDisposable
        let! harvest, hf = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
        let! resolve, rf = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
        let! shape, sf = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage ctx.Config ctx.PackageName) resolve)
        return! Semantics.project ctx shape (hf @ rf @ sf)
    } |> Async.RunSynchronously

let private requireType owner path model =
    Semantic.tryFind owner path model |> Option.defaultWith (fun () ->
        let candidates = Semantic.types model |> List.filter (fun t -> Semantic.path t model = path) |> List.map (fun t -> Semantic.package t model)
        failtestf "missing semantic type %s/%A; matching paths owned by %A" owner path candidates)

let private compileConsumer (generated: RenderModel) (consumer: string) =
    use scratch = Scratch.directory "customization-compile"
    let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../.."))
    File.Copy(Path.Combine(root, "global.json"), Path.Combine(scratch.Path, "global.json"))
    for name in ["Directory.Build.props"; "Directory.Build.targets"] do File.WriteAllText(Path.Combine(scratch.Path, name), "<Project />")
    let files = generated.Files |> List.filter (fst >> fun name -> name.EndsWith ".fs")
    for name, content in files do
        let path = Path.Combine(scratch.Path, name)
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllText(path, content)
    File.WriteAllText(Path.Combine(scratch.Path, "Consumer.fs"), consumer)
    let items = files |> List.map (fun (name, _) -> $"<Compile Include='{name}' />") |> String.concat ""
    let support = ["Xantham.Fable.Core"; "Xantham.Fable.Core.TS"] |> List.map (fun name ->
        let dll = supportAssembly root name
        $"<Reference Include='{name}'><HintPath>{dll}</HintPath></Reference>") |> String.concat ""
    File.WriteAllText(Path.Combine(scratch.Path, "Consumer.fsproj"), $"<Project Sdk='Microsoft.NET.Sdk'><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup>{items}<Compile Include='Consumer.fs' /><PackageReference Include='Fable.Core' Version='5.2.0' />{support}</ItemGroup></Project>")
    let start = ProcessStartInfo("dotnet", WorkingDirectory = scratch.Path, RedirectStandardOutput = true, RedirectStandardError = true)
    for arg in ["build"; "Consumer.fsproj"; "--nologo"; "-v:q"; "-nodeReuse:false"] do start.ArgumentList.Add arg
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    if not (child.WaitForExit 120000) then child.Kill(true); failtest "consumer build timed out"
    child.ExitCode, output.Result + errors.Result

[<Tests>]
let tests =
    testList "customization" [
        for includeExports in [false; true] do
          testCase (if includeExports then "exported member attributes are emitted" else "class entrypoint property edits are emitted") <| fun _ ->
            use scratch = Scratch.directory "customization-entrypoint"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"customization-entrypoint\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "declare module 'entrypoint:widget' { export abstract class Widget { constructor(); value: string; read(): string; } export function hello(): string; export const version: string; }")
            let extension =
                { Identity = { Id = "entrypoint"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let widget = requireType "customization-entrypoint" ["Widget"] model
                    let value = Semantic.properties widget model |> List.find (fun p -> Semantic.jsName p model = "value")
                    let target = Semantic.outputTargets value model |> List.head
                    let edits = Edits.empty |> Edits.addAttribute target (Attribute.create "System.Obsolete" [AttributeValue.String "entrypoint"]) |> Edits.replaceInterop target (Interop.property "renamed")
                    let edits = (if includeExports then ["hello"; "version"; "read"] else []) |> List.fold (fun edits name ->
                        let memberSource = Semantic.members model |> List.find (fun p -> Semantic.jsName p model = name)
                        Semantic.outputTargets memberSource model |> List.fold (fun edits target -> Edits.addAttribute target (Attribute.create "System.Obsolete" [AttributeValue.String name]) edits) edits) edits
                    Ok edits }
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default scratch.Path |> Async.RunSynchronously
            let source = generated.Files |> List.filter (fst >> fun n -> n.EndsWith ".fs") |> List.map snd |> String.concat "\n"
            Expect.stringContains source "AbstractClass" "exercise concrete entrypoint renderer"
            for expected in [ yield "Obsolete(\"entrypoint\")"; yield "renamed"; if includeExports then yield! ["Obsolete(\"hello\")"; "Obsolete(\"version\")"; "Obsolete(\"read\")"] ] do
                Expect.stringContains source expected "accepted edit appears in output"
            let code, errors = compileConsumer generated "module Consumer\nlet value (x: CustomizationEntrypoint.Entrypoint.Widget.Widget) = x.value\n"
            Expect.equal code 0 errors

        testCase "type-valued attributes qualify a shipped dependency" <| fun _ ->
            use scratch = Scratch.directory "customization-type-attribute"
            let dependency = Path.Combine(scratch.Path, "node_modules", "customization-dependency")
            Directory.CreateDirectory dependency |> ignore
            File.WriteAllText(Path.Combine(dependency, "package.json"), "{\"name\":\"customization-dependency\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(dependency, "index.d.ts"), "export interface Other { value: string }")
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"customization-typed\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "import type {Other} from 'customization-dependency'; export interface Target { other: Other; label: string }")
            let extension =
                { Identity = { Id = "typed"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let targetType = requireType "customization-typed" ["Target"] model
                    let properties = Semantic.properties targetType model
                    let other = properties |> List.find (fun p -> Semantic.jsName p model = "other")
                    let label = properties |> List.find (fun p -> Semantic.jsName p model = "label")
                    Ok (Semantic.outputTargets label model |> List.fold (fun edits target -> Edits.addAttribute target (Attribute.create "System.ComponentModel.TypeConverter" [AttributeValue.Type (Semantic.bindingType other model)]) edits) Edits.empty) }
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default scratch.Path |> Async.RunSynchronously
            let code, errors = compileConsumer generated "module Consumer\nlet label (x: CustomizationTyped.Target) = x.label\n"
            Expect.equal code 0 errors

        testCase "replacement rejects free type variables and companions reject module ancestors" <| fun _ ->
            let extension collision =
                { Identity = { Id = "invalid"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    if collision then Ok (Edits.empty |> Edits.emitCompanion (Companion.create "CustomizationLab" "Extra" input model))
                    else
                        let generic = requireType "customization-lab" ["Properties"] model
                        let properties = Semantic.properties generic model |> List.filter (fun p -> Semantic.jsName p model = "value")
                        Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) (Replacement.properties properties model)) }
            let results = [false; true] |> List.map (fun collision ->
                try Pipeline.generateWith [extension collision] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore; false
                with _ -> true)
            Expect.equal results [true; true] "free variable and occupied namespace both reject"

        testCase "ordinary interface contract remains abstract" <| fun _ ->
            let rendered = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
            let source = rendered.Files |> List.find (fst >> fun name -> name.EndsWith ".fs") |> snd
            Expect.stringContains source "abstract value: 'T with get, set" "generic property contract"
            Expect.stringContains source "abstract stamp: string\n" "readonly property contract"
            Expect.stringContains source "inherit Properties<string>" "generic substitution"

        testCase "registered attribute targets one property and empty registration is identical" <| fun _ ->
            let extension =
                { Identity = { Id = "example.attributes"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let property = Semantic.properties input model |> List.find (fun m -> Semantic.jsName m model = "disabled")
                    let attribute = Attribute.create "System.Obsolete" [AttributeValue.String "use \"enabled\""]
                    Ok (Semantic.outputTargets property model |> List.fold (fun edits target -> Edits.addAttribute target attribute edits) Edits.empty) }
            let baseline = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
            let empty = Pipeline.generateWith [] GeneratorConfig.Default package |> Async.RunSynchronously
            Expect.equal empty baseline "empty registration preserves all output and findings"
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            let source = generated.Files |> List.find (fst >> fun name -> name.EndsWith ".fs") |> snd
            Expect.stringContains source "[<System.Obsolete(\"use \\\"enabled\\\"\")>]\n    abstract disabled" "attribute on selected property"
            Expect.equal (source.Split("System.Obsolete").Length - 1) 1 "attribute appears once"

        testCase "extension failure leaves destination untouched" <| fun _ ->
            use scratch = Scratch.directory "customization-output"
            let sentinel = Path.Combine(scratch.Path, "sentinel.txt")
            File.WriteAllText(sentinel, "keep")
            let extension =
                { Identity = { Id = "example.failure"; Version = "1"; Configuration = Map.empty }
                  Transform = fun _ -> failwith "callback failed" }
            Expect.throws (fun () -> Pipeline.runWith [extension] GeneratorConfig.Default package scratch.Path |> Async.RunSynchronously |> ignore) "callback fails"
            Expect.equal (Directory.GetFiles scratch.Path |> Array.map Path.GetFileName) [|"sentinel.txt"|] "no partial output"
            Expect.equal (File.ReadAllText sentinel) "keep" "existing destination preserved"

        testCase "attribute coalescing preserves order and accessor replacement compiles" <| fun _ ->
            let extension =
                { Identity = { Id = "accessors"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let p = Semantic.properties input model |> List.find (fun p -> Semantic.jsName p model = "disabled")
                    let target = Semantic.outputTargets p model |> List.head
                    let first = Attribute.create "System.Obsolete" [AttributeValue.String "first"]
                    let second = Attribute.create "System.ComponentModel.Description" [AttributeValue.String "second"]
                    Ok (Edits.empty |> Edits.addAttribute target first |> Edits.addAttribute target first |> Edits.addAttribute target second |> Edits.replaceInterop target (Interop.property "enabled")) }
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            let source = generated.Files |> List.find (fst >> ((=) "CustomizationLab.fs")) |> snd
            Expect.equal (source.Split("System.Obsolete").Length - 1) 1 "identical attribute coalesces"
            Expect.isTrue (source.IndexOf("System.Obsolete") < source.IndexOf("System.ComponentModel.Description")) "attribute order"
            let code, errors = compileConsumer generated "module Consumer\nlet update (x: CustomizationLab.Input) = x.disabled <- true\n"
            Expect.equal code 0 errors

        testCase "a saved target from another source cannot mutate a same-named declaration" <| fun _ ->
            let model = snapshot package
            let target = requireType "customization-lab" ["Input"] model |> fun input -> Semantic.declarationTarget input model |> Option.get
            use scratch = Scratch.directory "customization-stale-target"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"other\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export interface Input { value: string }")
            let extension = {Identity = {Id = "saved-target"; Version = "1"; Configuration = Map.empty}; Transform = fun _ -> Ok (Edits.empty |> Edits.replaceDeclaration target Replacement.marker)}
            Expect.throws (fun () -> Pipeline.generateWith [extension] GeneratorConfig.Default scratch.Path |> Async.RunSynchronously |> ignore) "source identity authenticates target"

        testCase "companion properties preserve inherited types and readonly access" <| fun _ ->
            let extension =
                { Identity = { Id = "example.companions"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    Ok (Edits.empty |> Edits.emitCompanion (Companion.create "Example.Components" "InputProperties" input model |> Companion.directProperties)) }
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            let companion = generated.Files |> List.find (fst >> fun name -> name.Contains "InputProperties") |> snd
            Expect.stringContains companion "type InputProperties = interface end" "independent marker"
            Expect.stringContains companion "get (): string" "substituted generic type"
            Expect.stringContains companion "member _.stamp: string" "readonly getter"
            Expect.isFalse (companion.Contains "set (stamp") "readonly setter absent"
            Expect.stringContains companion "member _.title" "inherited property"
            let ordinary = generated.Files |> List.find (fst >> ((=) "CustomizationLab.fs")) |> snd
            Expect.stringContains ordinary "abstract disabled" "base binding retained"
            let exitCode, output = compileConsumer generated "module Consumer\nopen Example.Components\ntype Input() = interface InputProperties\nlet update (x: InputProperties) = x.value <- x.title\n"
            Expect.equal exitCode 0 output
            let negativeCode, negativeOutput = compileConsumer generated "module Consumer\nopen Example.Components\nlet update (x: InputProperties) = x.stamp <- \"bad\"\n"
            Expect.isTrue (negativeCode <> 0 && negativeOutput.Contains "FS0810") "readonly setter is rejected by compiler"

        testCase "overlapping interop and declaration replacements fail explicitly" <| fun _ ->
            let interop id =
                { Identity = { Id = id; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let property = Semantic.properties input model |> List.find (fun p -> Semantic.jsName p model = "disabled")
                    let target = Semantic.outputTargets property model |> List.head
                    Ok (Edits.empty |> Edits.replaceInterop target (Interop.property "enabled")) }
            Expect.throws (fun () -> Pipeline.generateWith [interop "first"; interop "second"] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "competing replacements"
            let replace id =
                { Identity = { Id = id; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) Replacement.marker) }
            Expect.throws (fun () -> Pipeline.generateWith [replace "first"; replace "second"] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "competing declaration replacement"
            Expect.throws (fun () -> Pipeline.generateWith [replace "first"; interop "second"] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "replacement conflicts with removed member edit"

        testCase "duplicate companions fail and explicit marker replacement preserves name" <| fun _ ->
            let extension id =
                { Identity = { Id = id; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    Ok (Edits.empty |> Edits.emitCompanion (Companion.create "Example.Components" "InputProperties" input model)) }
            Expect.throws (fun () -> Pipeline.generateWith [extension "one"; extension "two"] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "colliding companion names"
            let marker =
                { Identity = { Id = "marker"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) Replacement.marker) }
            let output = Pipeline.generateWith [marker] GeneratorConfig.Default package |> Async.RunSynchronously
            let source = output.Files |> List.find (fst >> ((=) "CustomizationLab.fs")) |> snd
            Expect.stringContains source "type Input =\n    interface end" "explicit replacement retains identity"
            let code, errors = compileConsumer output "module Consumer\ntype Input() = interface CustomizationLab.Input\n"
            Expect.equal code 0 errors

        testCase "replacement fidelity and deterministic provenance are recorded" <| fun _ ->
            let extension =
                { Identity = { Id = "marker"; Version = "1"; Configuration = Map.ofList ["b", "2"; "a", "1"] }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) Replacement.marker) }
            let first = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            let second = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            Expect.equal first.Files second.Files "stable fresh generation"
            Expect.isTrue (first.Findings |> List.exists (fun f -> f.Symbol = "Input" && f.Tier = Escape)) "replacement loses Exact claim"
            let manifest = first.Files |> List.find (fst >> ((=) "manifest.json")) |> snd
            Expect.stringContains manifest "customizations" "extension provenance"
            Expect.stringContains manifest "marker" "identity"

        testCase "structured replacement selects resolved property contracts" <| fun _ ->
            let extension =
                { Identity = { Id = "properties"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let members = Semantic.properties input model |> List.filter (fun p -> Semantic.jsName p model = "value")
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) (Replacement.properties members model)) }
            let generated = Pipeline.generateWith [extension] GeneratorConfig.Default package |> Async.RunSynchronously
            let code, errors = compileConsumer generated "module Consumer\ntype Input() = interface CustomizationLab.Input with member _.value with get() = \"ok\" and set (_: string) = ()\n"
            Expect.equal code 0 errors

        testCase "companion bases are ordered and ordinary names are protected" <| fun _ ->
            let extension collision =
                { Identity = { Id = "bases"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let parent = Companion.create "Example.Components" "BaseProperties" input model
                    let child = Companion.create "Example.Components" "InputProperties" input model |> Companion.withBases [Companion.reference parent model]
                    if collision then Ok (Edits.empty |> Edits.emitCompanion (Companion.create "CustomizationLab" "Input" input model))
                    else Ok (Edits.empty |> Edits.emitCompanion child |> Edits.emitCompanion parent) }
            let generated = Pipeline.generateWith [extension false] GeneratorConfig.Default package |> Async.RunSynchronously
            let code, errors = compileConsumer generated "module Consumer\ntype Input() = interface Example.Components.InputProperties\n"
            Expect.equal code 0 errors
            Expect.throws (fun () -> Pipeline.generateWith [extension true] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "ordinary declaration name collision"

        testCase "raw replacement requires compiler validation and rejects catalog production" <| fun _ ->
            use scratch = Scratch.directory "customization-raw"
            let extension source =
                { Identity = { Id = "raw"; Version = "1"; Configuration = Map.empty }
                  Transform = fun model ->
                    let input = requireType "customization-lab" ["Input"] model
                    let replacement = Replacement.raw source ["Input"] []
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget input model |> Option.get) replacement) }
            let valid = extension "[<Interface>]\ntype Input = interface end"
            Expect.throws (fun () -> Pipeline.generateWith [valid] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "raw requires compiler"
            let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../.."))
            let references = ["Xantham.Fable.Core.TS"; "Xantham.Fable.Core"] |> List.map (supportAssembly root)
            let compiler = Compiler.dotnet scratch.Path references
            let generated = Pipeline.generateValidatedWith compiler [valid] GeneratorConfig.Default package |> Async.RunSynchronously
            Expect.isTrue (generated.Files |> List.exists (fun (_, source) -> source.Contains "[<Interface>]\ntype Input = interface end")) "raw declaration replaces rendered source"
            let code, errors = compileConsumer generated "module Consumer\ntype Input() = interface CustomizationLab.Input\n"
            Expect.equal code 0 errors
            Expect.throws (fun () -> Pipeline.generateValidatedWith compiler [extension "type Input = ___DefinitelyMissingType"] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore) "invalid raw source fails compiler"
            Expect.throws (fun () -> Pipeline.generateValidatedWith compiler [valid] {GeneratorConfig.Default with DeclarationCatalog = true} package |> Async.RunSynchronously |> ignore) "raw catalog API is unauthenticated"

        testCase "customized producer catalog authenticates its original API and variant" <| fun _ ->
            use scratch = Scratch.directory "customization-catalog"
            let producer = Path.Combine(scratch.Path, "producer")
            let consumer = Path.Combine(scratch.Path, "consumer")
            Directory.CreateDirectory producer |> ignore
            Directory.CreateDirectory consumer |> ignore
            File.WriteAllText(Path.Combine(producer, "package.json"), "{\"name\":\"custom-shared\",\"version\":\"1.0.0\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(producer, "index.d.ts"), "export interface Model { value: string }")
            File.WriteAllText(Path.Combine(consumer, "package.json"), "{\"name\":\"custom-consumer\",\"version\":\"1.0.0\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(consumer, "index.d.ts"), "import { Model } from '../producer/index'; export declare function accept(value: Model): Model;")
            let extension version =
                { Identity = { Id = "attributes"; Version = version; Configuration = Map.empty }
                  Transform = fun model ->
                    let selected = Semantic.tryFind "custom-shared" ["Model"] model
                    let edits = match selected with
                                | None -> Edits.empty
                                | Some t -> Semantic.properties t model |> List.collect (fun p -> Semantic.outputTargets p model) |> List.fold (fun edits target -> Edits.addAttribute target (Attribute.create "System.Obsolete" [AttributeValue.String "legacy"]) edits) Edits.empty
                    Ok edits }
            let catalogConfig = {GeneratorConfig.Default with DeclarationCatalog = true}
            let generated = Pipeline.generateWith [extension "1"] catalogConfig producer |> Async.RunSynchronously
            let catalog = generated.Files |> List.find (fst >> ((=) "declarations.json")) |> snd
            Expect.stringContains catalog "variants" "catalog records variant"
            let companionOnly =
                {Identity = {Id = "companion"; Version = "1"; Configuration = Map.empty}
                 Transform = fun model -> Ok (Edits.empty |> Edits.emitCompanion (Companion.create "Example" "ModelProperties" (requireType "custom-shared" ["Model"] model) model))}
            let baseCatalog = Pipeline.generate catalogConfig producer |> Async.RunSynchronously |> fun r -> r.Files |> List.find (fst >> ((=) "declarations.json")) |> snd
            let companionCatalog = Pipeline.generateWith [companionOnly] catalogConfig producer |> Async.RunSynchronously |> fun r -> r.Files |> List.find (fst >> ((=) "declarations.json")) |> snd
            Expect.isTrue (baseCatalog = companionCatalog) "companion-only output leaves producer base API bytes unchanged"
            let catalogPath = Path.Combine(scratch.Path, "declarations.json")
            File.WriteAllText(catalogPath, catalog)
            let config = {GeneratorConfig.Default with DeclarationReferences = [catalogPath]}
            let reused = Pipeline.generate config consumer |> Async.RunSynchronously
            Expect.isTrue (reused.Files |> List.exists (fun (name, _) -> name.EndsWith ".fs")) "consumer authenticates source/API"
            let combined = {reused with Files = (generated.Files |> List.filter (fst >> fun n -> n.EndsWith ".fs")) @ reused.Files}
            let code, errors = compileConsumer combined "module Consumer\nlet roundtrip (x: CustomShared.Model) : CustomShared.Model = CustomConsumer.Exports.accept x\n"
            Expect.equal code 0 errors
            let different = Pipeline.generateWith [extension "2"] catalogConfig producer |> Async.RunSynchronously
            Expect.notEqual (different.Files |> List.find (fst >> ((=) "declarations.json")) |> snd) catalog "extension version changes variant"
            let consumerProfile version = {Identity = {Id = "attributes"; Version = version; Configuration = Map.empty}; Transform = fun _ -> Ok Edits.empty}
            let matched = Pipeline.generateWith [consumerProfile "1"] config consumer |> Async.RunSynchronously
            let matchedCode, matchedErrors = compileConsumer {matched with Files = (generated.Files |> List.filter (fst >> fun n -> n.EndsWith ".fs")) @ matched.Files} "module Consumer\nlet roundtrip (x: CustomShared.Model) = CustomConsumer.Exports.accept x\n"
            Expect.equal matchedCode 0 matchedErrors
            Expect.throws (fun () -> Pipeline.generateWith [consumerProfile "2"] config consumer |> Async.RunSynchronously |> ignore) "different registered extension profile rejects producer variant"
            let marker =
                {Identity = {Id = "marker"; Version = "1"; Configuration = Map.empty}
                 Transform = fun model ->
                    let selected = requireType "custom-shared" ["Model"] model
                    Ok (Edits.empty |> Edits.replaceDeclaration (Semantic.declarationTarget selected model |> Option.get) Replacement.marker)}
            let changedContract = Pipeline.generateWith [marker] catalogConfig producer |> Async.RunSynchronously
            File.WriteAllText(catalogPath, changedContract.Files |> List.find (fst >> ((=) "declarations.json")) |> snd)
            Expect.throws (fun () -> Pipeline.generate config consumer |> Async.RunSynchronously |> ignore) "marker replacement is not evidence of the original TypeScript contract"

        testCase "referenced companions use producer names and cannot mutate producer output" <| fun _ ->
            use scratch = Scratch.directory "customization-referenced-companion"
            let producer, consumer = Path.Combine(scratch.Path, "producer"), Path.Combine(scratch.Path, "consumer")
            for directory, owner, source in [producer, "companion-producer", "export interface Detail { label: string }; export interface Model { detail: Detail }"; consumer, "companion-consumer", "import { Model } from '../producer/index'; export declare function accept(x: Model): Model; export interface Local { model: Model; label: string }"] do
                Directory.CreateDirectory directory |> ignore
                File.WriteAllText(Path.Combine(directory, "package.json"), $"{{\"name\":\"{owner}\",\"version\":\"1.0.0\",\"types\":\"index.d.ts\"}}")
                File.WriteAllText(Path.Combine(directory, "index.d.ts"), source)
            let generatedProducer = Pipeline.generate {GeneratorConfig.Default with DeclarationCatalog = true} producer |> Async.RunSynchronously
            let catalogPath = Path.Combine(scratch.Path, "declarations.json")
            File.WriteAllText(catalogPath, generatedProducer.Files |> List.find (fst >> ((=) "declarations.json")) |> snd)
            let config = {GeneratorConfig.Default with DeclarationReferences = [catalogPath]}
            let extension mutate =
                {Identity = {Id = "referenced"; Version = "1"; Configuration = Map.empty}
                 Transform = fun model ->
                    let selected = requireType "companion-producer" ["Model"] model
                    if mutate then
                        let target = Semantic.declarationTarget selected model |> Option.get
                        Ok (Edits.empty |> Edits.replaceDeclaration target Replacement.marker)
                    else
                        let detail = Semantic.properties selected model |> List.head |> fun p -> Semantic.bindingType p model
                        let local = requireType "companion-consumer" ["Local"] model
                        let label = Semantic.properties local model |> List.find (fun p -> Semantic.jsName p model = "label")
                        let edits = Semantic.outputTargets label model |> List.fold (fun edits target -> Edits.addAttribute target (Attribute.create "System.ComponentModel.TypeConverter" [AttributeValue.Type detail]) edits) Edits.empty
                        Ok (edits |> Edits.emitCompanion (Companion.create "Example" "ModelProperties" selected model))}
            let generated = Pipeline.generateWith [extension false] config consumer |> Async.RunSynchronously
            let combined = {generated with Files = (generatedProducer.Files |> List.filter (fst >> fun name -> name.EndsWith ".fs")) @ generated.Files}
            let code, errors = compileConsumer combined "module Consumer\nopen Example\ntype Component() = interface ModelProperties\nlet read (x: ModelProperties) : CompanionProducer.Detail = x.detail\n"
            Expect.equal code 0 errors
            Expect.throws (fun () -> Pipeline.generateWith [extension true] config consumer |> Async.RunSynchronously |> ignore) "referenced producer output cannot be replaced"

        testCase "effective generic and diamond properties retain identity and optionality" <| fun _ ->
            let model = snapshot package
            let diamond = requireType "customization-lab" ["Diamond"] model
            let properties = Semantic.properties diamond model
            let names = properties |> List.map (fun m -> Semantic.jsName m model)
            Expect.equal (names |> List.filter ((=) "value") |> List.length) 1 "diamond value emitted once"
            let value = properties |> List.find (fun m -> Semantic.jsName m model = "value")
            Expect.equal (BindingType.display (Semantic.bindingType value model)) "string" "generic substitution"
            Expect.equal (Semantic.path (Semantic.declaringType value model) model) ["Properties"] "declaring identity"
            let stamp = properties |> List.find (fun m -> Semantic.jsName m model = "stamp")
            Expect.isTrue (Semantic.isReadOnly stamp model) "readonly independent of optional"
            Expect.isFalse (Semantic.isOptional stamp model) "stamp required"
            let optional = properties |> List.find (fun m -> Semantic.jsName m model = "optional")
            Expect.isTrue (Semantic.isOptional optional model) "optional retained"
            Expect.isFalse (Semantic.isReadOnly optional model) "optional mutable"
            let overrideType = requireType "customization-lab" ["Override"] model
            let replacement = Semantic.properties overrideType model |> List.find (fun m -> Semantic.jsName m model = "value")
            Expect.equal (Semantic.path (Semantic.declaringType replacement model) model) ["Override"] "override declares its own member"

        testCase "DOM descendants follow authenticated heritage and referenced source facts" <| fun _ ->
            let dom = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/customization-dom-lab"))
            let model = snapshot dom
            let root = requireType "typescript/lib" ["HTMLElement"] model
            let descendants = Semantic.descendants root model |> List.map (fun t -> Semantic.package t model, Semantic.path t model)
            Expect.contains descendants ("customization-dom-lab", ["Input"]) "real descendant"
            Expect.contains descendants ("customization-dom-lab", ["Grandchild"]) "transitive descendant"
            Expect.isFalse (List.contains ("customization-dom-lab", ["Impostors"; "Child"]) descendants) "named impostor excluded"
            Expect.isFalse (List.contains ("customization-dom-lab", ["Lookalike"]) descendants) "structural impostor excluded"
            Expect.isTrue (Semantic.properties root model |> List.exists (fun m -> Semantic.jsName m model = "title")) "referenced root properties retained"
            let rootAlias = requireType "customization-dom-lab" ["RootAlias"] model
            Expect.isTrue (Semantic.isSameDeclaration rootAlias root model) "alias resolves to true root"
            let second = snapshot dom
            Expect.equal (Semantic.identity root model) (Semantic.identity (requireType "typescript/lib" ["HTMLElement"] second) second) "stable fresh sessions"
    ]
