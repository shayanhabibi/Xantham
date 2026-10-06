module Xantham.Generator.Tests.CustomizationTests

open System.IO
open System
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

[<Tests>]
let tests =
    testList "customization" [
        testCase "ordinary interface contract remains abstract" <| fun _ ->
            let rendered = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
            let source = rendered.Files |> List.find (fst >> fun name -> name.EndsWith ".fs") |> snd
            Expect.stringContains source "abstract value: 'T with get, set" "generic property contract"
            Expect.stringContains source "abstract stamp: string\n" "readonly property contract"
            Expect.stringContains source "inherit Properties<string>" "generic substitution"

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
