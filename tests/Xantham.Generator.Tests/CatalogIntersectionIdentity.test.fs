module Xantham.Generator.Tests.CatalogIntersectionIdentityTests

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

let private write directory file (text: string) =
    let path = Path.Combine(directory, file)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

let private configure directory name references =
    let path = Path.Combine(directory, name + ".json")
    File.WriteAllText(path,
        JsonSerializer.Serialize {| ``module`` = "Mini." + name; entry = "index.d.ts"
                                    lib = [| "esnext" |]; types = [| "catalog-owner" |]
                                    groups = Map.ofList [ "catalog-owner", "ship" ]
                                    declarationCatalog = true; declarationReferences = references |})
    GeneratorConfig.loadFile path

let private read path =
    JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText path, JsonSerializerOptions(PropertyNameCaseInsensitive = true))

[<Tests>]
let tests =
    testCase "catalog intersection identity follows source occurrence across reachable parents" <| fun _ ->
        use scratch = Scratch.directory "catalog-intersection-identity"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-intersection-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "index.d.ts")))
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { cf: BotEnterprise; }"
        let evidence = async {
            let config = configure directory "Evidence" ([||]: string array)
            let! mailbox, ctx = Bootstrap.start config owner
            use _ = mailbox :> IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let enterprise = resolve.Types |> Map.toSeq |> Seq.map snd |> Seq.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "BotEnterprise"))
            let member_ = enterprise.Members |> List.find (fun member_ -> member_.Symbol.Name = "botManagement")
            let intersection = resolve.Types[member_.TypeId]
            Expect.isEmpty intersection.Declarations "the checker intersection has no declaration of its own"
            let handles = intersection.IntersectionMembers |> List.collect (fun id -> resolve.Types[id].Declarations)
            Expect.isTrue
                (handles |> List.exists (fun handle -> NodeHandle.parse (string handle) |> ValueOption.exists (fun node -> node.Kind = SyntaxKind.TypeLiteral)))
                "a compiler-authenticated TypeLiteral constituent supplies source occurrence provenance"
            printfn "intersection constituent handles: %A" handles
        }
        evidence |> Async.RunSynchronously
        let generate name package references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) package output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let producer = generate "Owner" owner ([||]: string array)
        let consumer = generate "Consumer" package [||]
        let child (catalog: DeclarationCatalog.Catalog) =
            catalog.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(".BotEnterprise.BotManagement", StringComparison.Ordinal))
        let produced = child producer
        let consumed = child consumer
        printfn "owner child: %s %s %A; consumer child: %s %s %A" produced.Identity produced.Api produced.Handles consumed.Identity consumed.Api consumed.Handles
        Expect.equal produced.Api consumed.Api "the anonymous child has the same emitted contract"
        Expect.equal produced.Identity consumed.Identity "reaching Combined must preserve the written intersection identity"
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let owner (request: Mini.Imported.Request) : Mini.Owner.BotEnterprise = request.cf
let score (request: Mini.Imported.Request) : float = request.cf.botManagement.score
let hash (request: Mini.Imported.Request) : string = request.cf.botManagement.ja3Hash
"""
        Expect.equal code 0 output
        let reference = Path.Combine(directory, "Owner", "declarations.json")
        let original = File.ReadAllText reference
        let tampered = JsonNode.Parse original
        let child = tampered["declarations"].AsArray() |> Seq.find (fun entry -> entry["identity"].GetValue<string>() = produced.Identity)
        child["api"] <- JsonValue.Create "changed-contract"
        File.WriteAllText(reference, tampered.ToJsonString())
        Expect.throwsC
            (fun () -> generate "ChangedApi" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "F# API mismatch" "a changed member contract still rejects")
        File.WriteAllText(reference, original)
        File.AppendAllText(Path.Combine(owner, "index.d.ts"), "\n")
        Expect.throwsC
            (fun () -> generate "Stale" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "input source hash mismatch" "the source occurrence stays byte-authenticated")

[<Tests>]
let genericTests =
    testCase "catalog intersection identity retains generic applications and independent source contracts" <| fun _ ->
        use scratch = Scratch.directory "catalog-intersection-generic"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-intersection-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "generic.d.ts")))
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { text: GenericBot<string>; numeric: GenericBot<number>; separate: SeparateBot<string>; }"
        let generate name package references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) package output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let producer = generate "Owner" owner ([||]: string array)
        let consumer = generate "Consumer" package [||]
        let child suffix (catalog: DeclarationCatalog.Catalog) =
            catalog.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(suffix, StringComparison.Ordinal))
        for suffix in [ ".GenericBot.Payload"; ".SeparateBot.Payload" ] do
            Expect.equal (child suffix producer).Identity (child suffix consumer).Identity "source occurrence identity survives generic reachability"
        Expect.notEqual (child ".GenericBot.Payload" producer).Identity (child ".SeparateBot.Payload" producer).Identity "separate written type literals remain distinct"
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let text (request: Mini.Imported.Request) : string = request.text.payload.value
let numeric (request: Mini.Imported.Request) : float = request.numeric.payload.value
let separate (request: Mini.Imported.Request) : string = request.separate.payload.marker
"""
        Expect.equal code 0 output
        let code, _ = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let invalid (request: Mini.Imported.Request) : Mini.Owner.GenericBot<string> = request.numeric
"""
        Expect.notEqual code 0 "closed generic applications retain distinct contracts"
