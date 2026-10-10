module Xantham.Generator.Tests.CatalogMemberTests

open Expecto
open Xantham.Generator
open Xantham.TypeScript.Wire
open System.Text.Json
open System.IO
open Xantham.TypeScript.Wire.Proto

let private computed id name declarations =
    { Build.symbol id name SymbolFlags.Property with Declarations = ValueSome declarations }

let private normalized handle = handle / Measure.uom<Measure.declHandle>

[<Tests>]
let tests = testList "catalog computed member identity" [
    testCase "compiler allocation IDs never colour computed keys" <| fun () ->
        let handle = "12.188./library/index.d.ts"
        let expected = CatalogMember.key normalized (computed 1 "__@match@42" [|handle|])
        Expect.isSome expected "authenticated computed declaration has a key"
        for id in [2; 17; 1000] do
            Expect.equal (CatalogMember.key normalized (computed id ("__@match@" + string id) [|handle|])) expected
                "compiler symbol and escaped-name IDs are session-local"
    testCase "distinct unique symbols with equal names remain distinct" <| fun () ->
        let first = CatalogMember.key normalized (computed 1 "__@key@42" [|"12.188./first/index.d.ts"|])
        let second = CatalogMember.key normalized (computed 2 "__@key@99" [|"12.188./second/index.d.ts"|])
        Expect.notEqual first second "declaration ownership distinguishes equal computed spellings"
    testCase "computed keys use the full normalized declaration set" <| fun () ->
        let handles = [|"12.188./library/index.d.ts"; "16.188./library/index.d.ts"|]
        let expected = CatalogMember.key normalized (computed 1 "__@key@42" handles)
        Expect.equal (CatalogMember.key normalized (computed 2 "__@key@99" (Array.rev handles))) expected
            "wire ordering does not affect keys"
        Expect.notEqual (CatalogMember.key normalized (computed 3 "__@key@123" handles[0..0])) expected
            "losing an authenticated declaration changes the key"
    testCase "unanchored computed keys cannot acquire portable identity" <| fun () ->
        Expect.isNone (CatalogMember.key normalized (Build.symbol 1 "__@key@42" SymbolFlags.Property))
            "absent declaration evidence cannot be replaced with a session ID"
        Expect.isNone (CatalogMember.key normalized (computed 2 "__@key@99" [||])) "empty declaration evidence is absent"
    testCase "ordinary property spellings remain exact" <| fun () ->
        for name in ["match"; "user@42"; "__user@42"; "Symbol.match"; "key-123"] do
            let key = CatalogMember.key normalized (Build.symbol 1 name SymbolFlags.Property) |> Option.get
            use document = JsonDocument.Parse key
            Expect.equal (document.RootElement.GetProperty("ordinaryMember").GetString()) name
                "only compiler computed-symbol spellings require declaration keys"
    testCase "ordinary names cannot impersonate encoded computed keys" <| fun () ->
        let computedKey = CatalogMember.key normalized (computed 1 "__@key@42" [|"12.188./library/index.d.ts"|]) |> Option.get
        let ordinary = CatalogMember.key normalized (Build.symbol 2 computedKey SymbolFlags.Property)
        Expect.notEqual ordinary (Some computedKey) "property-key kinds remain disjoint even for adversarial spellings"
]

[<Tests>]
let wireTests = testSequenced <| testList "catalog computed member wire" [
    testCase "fresh compiler sessions preserve quoted and unique-symbol keys" <| fun () ->
        use scratch = Scratch.directory "catalog-computed-member-lab"
        File.WriteAllText(Path.Combine(scratch.Path, "package.json"),
            """{"name":"catalog-computed-member-lab","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"),
            "declare const key: unique symbol;\nexport interface Mixed { \"__@match@42\": string; [Symbol.match](text: string): boolean; [key]: number; }\n")
        let config = { GeneratorConfig.Default with Lib = Some ["es5"; "es2015.symbol"; "es2015.symbol.wellknown"] }
        let capture () =
            let mailbox, ctx = Bootstrap.start config scratch.Path |> Async.RunSynchronously
            use lifetime = mailbox
            let harvested, _ =
                Pipeline.runTier ctx Harvest.passes
                    { Exports = []; AmbientClasses = []; Namespaces = Map.empty; ShadowedByLib = 0 }
                |> Async.RunSynchronously
            let resolved, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvested) |> Async.RunSynchronously
            let mixed = resolved.Types |> Map.toList |> List.map snd
                        |> List.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> name / Measure.uom<Measure.symbolName> = "Mixed"))
            mixed.Members |> List.map (fun member_ -> CatalogMember.key normalized member_.Symbol |> Option.get) |> List.sort
        let expected = capture ()
        Expect.equal expected.Length 3 "quoted, well-known and user unique-symbol members were resolved"
        Expect.equal (List.distinct expected).Length 3 "equal-looking quoted and computed names stay distinct"
        let ordinary = expected |> List.filter (fun key -> use document = JsonDocument.Parse key in document.RootElement.TryGetProperty("ordinaryMember") |> fst)
        Expect.equal ordinary.Length 1 "the real compiler distinguishes quoted names from escaped computed symbols"
        Expect.equal (capture ()) expected "fresh checker allocations retain authenticated member keys"
]
