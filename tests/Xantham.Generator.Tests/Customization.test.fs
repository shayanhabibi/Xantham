module Xantham.Generator.Tests.CustomizationTests

open System.IO
open System
open System.Diagnostics
open Expecto
open Xantham.Generator
open Xantham.Generator.Customization

let private package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/customization-lab"))

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
    Semantic.tryFind owner path model |> Option.defaultWith (fun () -> failtestf "missing semantic type %s/%A" owner path)

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
    let support = Path.Combine(root, "src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj")
    File.WriteAllText(Path.Combine(scratch.Path, "Consumer.fsproj"), $"<Project Sdk='Microsoft.NET.Sdk'><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup>{items}<Compile Include='Consumer.fs' /><PackageReference Include='Fable.Core' Version='5.2.0' /><ProjectReference Include='{support}' /></ItemGroup></Project>")
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
