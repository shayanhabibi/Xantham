module Xantham.Generator.Tests.CatalogIntersectionIdentityTests

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator
open Xantham.Generator.Measure

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
let privateBrandTests =
    testCase "catalog preserves named numeric brands reached through a private reference" <| fun _ ->
        use scratch = Scratch.directory "catalog-private-brand"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" "type EntryId = number & { readonly __brand: 'EntryId' };\ninterface Entry { id: EntryId; }\n"
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { entry: Entry; }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let producer = generate "Owner" owner ([||]: string array)
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        Expect.isTrue (producer.Declarations |> Array.exists (fun entry -> entry.FSharpName.EndsWith(".EntryId", StringComparison.Ordinal))) "the named brand has a catalog owner"
        Expect.isFalse (File.ReadAllText(Path.Combine(directory, "Owner", "Mini.Owner.fs")).Contains "abstract __brand") "brand markers are not exposed as intersection bases"
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let id (request: Mini.Imported.Request) : float<Mini.Owner.EntryId> = request.entry.id
"""
        Expect.equal code 0 output

[<Tests>]
let aliasedIntersectionTests =
    testCase "catalog reuses a named intersection through its source constituents" <| fun _ ->
        use scratch = Scratch.directory "catalog-aliased-intersection"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" "interface Image { data: string; }\ntype Content = string | Image[];\ntype Submission = { id: string } & ({ type: 'input'; content: Content } | { type: 'write'; text: string });\ntype Input = Extract<Submission, { type: 'input' }>;\ntype Create = Submission extends infer R ? R extends Submission ? Omit<R, 'id'> : never : never;\ninterface Handler { submit(input: Input): void; create(input: Create): void; }\n"
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { handler: Handler; }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let producer = generate "Owner" owner ([||]: string array)
        let input = producer.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(".Input", StringComparison.Ordinal))
        let create = producer.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(".Create", StringComparison.Ordinal))
        Expect.isNonEmpty input.Equivalents "the source intersection identifies the named alias"
        Expect.isNonEmpty create.Equivalents "the distributed union identifies the named conditional alias"
        for equivalent in input.Equivalents do
            Expect.isFalse (producer.Declarations |> Array.exists (fun entry -> entry.Identity = equivalent && entry.Identity <> input.Identity)) "an equivalent must not name a different producer declaration"
        let reference = Path.Combine(directory, "Owner", "declarations.json")
        generate "Imported" package [| reference |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let submit (request: Mini.Imported.Request) (input: Mini.Owner.Input) = request.handler.submit input
let create (request: Mini.Imported.Request) (input: Mini.Owner.Create) = request.handler.create input
"""
        Expect.equal code 0 output
        let original = File.ReadAllText reference
        let altered = JsonNode.Parse original
        let row = altered["declarations"].AsArray() |> Seq.find (fun entry -> entry["identity"].GetValue<string>() = input.Identity)
        row["equivalents"] <- JsonArray()
        File.WriteAllText(reference, altered.ToJsonString())
        Expect.throwsC
            (fun () -> generate "WithoutEquivalent" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "F# API mismatch" "removing the equivalent must reject the consumer")
        let alteredUnion = JsonNode.Parse original
        let unionRow = alteredUnion["declarations"].AsArray() |> Seq.find (fun entry -> entry["identity"].GetValue<string>() = create.Identity)
        unionRow["equivalents"] <- JsonArray()
        File.WriteAllText(reference, alteredUnion.ToJsonString())
        Expect.throwsC
            (fun () -> generate "WithoutUnionEquivalent" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "F# API mismatch" "removing the union equivalent must reject the consumer")

[<Tests>]
let sharedLiteralBaseTests =
    testCase "catalog preserves a shared union arm as an intersection base" <| fun _ ->
        use scratch = Scratch.directory "catalog-literal-base"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" "type Semantics = { scope: 'session'; history?: never; fork?: never } | { scope: 'task'; history?: never; fork?: never };\ntype Family<T, I> = { kind: string } & Semantics & { family: true; initial(seed: I): T };\ntype SessionFamily<T, I> = Family<T, I> & { scope: 'session' };\ninterface Handler { use(value: SessionFamily<string, number>): void; }\n"
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { handler: Handler; }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
        generate "Owner" owner ([||]: string array)
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |]
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let handler (request: Mini.Imported.Request) : Mini.Owner.Handler = request.handler
"""
        Expect.equal code 0 output

[<Tests>]
let genericBrandTests =
    testCase "catalog keeps separate private generic numeric brand aliases" <| fun _ ->
        use scratch = Scratch.directory "catalog-generic-brands"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" "type Brand<T, B> = T & { readonly __brand: B };\ntype EntryId = Brand<number, 'EntryId'>;\ntype TaskId = Brand<number, 'TaskId'>;\ninterface Entry { id: EntryId; task: TaskId; }\n"
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { entry: Entry; }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
        generate "Owner" owner ([||]: string array)
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |]
        let sources = [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ]
        let code, output = DeclarationCatalogTests.compileConsumer directory sources """module Mini.Check
let id (request: Mini.Imported.Request) : float<Mini.Owner.EntryId> = request.entry.id
let task (request: Mini.Imported.Request) : float<Mini.Owner.TaskId> = request.entry.task
"""
        Expect.equal code 0 output
        let invalid, _ = DeclarationCatalogTests.compileConsumer directory sources """module Mini.Invalid
let wrong (request: Mini.Imported.Request) : float<Mini.Owner.EntryId> = request.entry.task
"""
        Expect.notEqual invalid 0 "different branded aliases must not unify"

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

[<Tests>]
let unionTests =
    testCase "catalog union identity follows declared arms across reachable parents" <| fun _ ->
        use scratch = Scratch.directory "catalog-union-identity"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-union-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "index.d.ts")))
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { cf: Shield; }"
        let evidence = async {
            let config = configure directory "Evidence" ([||]: string array)
            let! mailbox, ctx = Bootstrap.start config owner
            use _ = mailbox :> IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let shield = resolve.Types |> Map.toSeq |> Seq.map snd |> Seq.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "Shield"))
            let member_ = shield.Members |> List.find (fun member_ -> member_.Symbol.Name = "tlsClientAuth")
            let union = resolve.Types[member_.TypeId]
            Expect.isEmpty union.Declarations "the anonymous union has no own declaration"
            Expect.isEmpty union.AliasDeclarations "the anonymous union has no named alias"
            Expect.equal union.UnionMembers.Length 2 "the compiler supplies both declared union arms"
            for arm in union.UnionMembers do
                let declarations = resolve.Types[arm].Declarations
                Expect.isNonEmpty declarations "each arm has actual source declarations"
                Expect.isTrue
                    (declarations |> List.forall (fun handle -> NodeHandle.parse (string handle) |> ValueOption.exists (fun node -> node.Kind = SyntaxKind.InterfaceDeclaration)))
                    "the union is over actual source-declared interfaces"
            printfn "union member occurrence: %A; arm handles: %A" member_.Symbol.Declarations (union.UnionMembers |> List.collect (fun id -> resolve.Types[id].Declarations))
        }
        evidence |> Async.RunSynchronously
        let generate name package references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) package output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let producer = generate "Owner" owner ([||]: string array)
        let consumer = generate "Consumer" package [||]
        let child (catalog: DeclarationCatalog.Catalog) =
            catalog.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(".TlsClientAuth", StringComparison.Ordinal))
        let produced = child producer
        let consumed = child consumer
        printfn "owner union: %s %s %A; consumer union: %s %s %A" produced.Identity produced.Api produced.Handles consumed.Identity consumed.Api consumed.Handles
        Expect.equal produced.Api consumed.Api "both runs emit the same union contract"
        Expect.equal produced.Identity consumed.Identity "the nominal-arm union must be independent of reachable parents"
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let shield (request: Mini.Imported.Request) : Mini.Owner.Shield = request.cf
"""
        Expect.equal code 0 output
        let reference = Path.Combine(directory, "Owner", "declarations.json")
        let original = File.ReadAllText reference
        let tampered = JsonNode.Parse original
        let child = tampered["declarations"].AsArray() |> Seq.find (fun entry -> entry["identity"].GetValue<string>() = produced.Identity)
        child["api"] <- JsonValue.Create "changed-union-representation"
        File.WriteAllText(reference, tampered.ToJsonString())
        Expect.throwsC
            (fun () -> generate "ChangedApi" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "F# API mismatch" "a changed union representation still rejects")
        File.WriteAllText(reference, original)
        File.AppendAllText(Path.Combine(owner, "index.d.ts"), "\n")
        Expect.throwsC
            (fun () -> generate "Stale" package [| reference |] |> ignore)
            (fun error -> Expect.stringContains error.Message "input source hash mismatch" "nominal union source bytes remain authenticated")

[<Tests>]
let unionContractTests =
    testCase "catalog union identity canonicalizes arm order and preserves source and generic contracts" <| fun _ ->
        use scratch = Scratch.directory "catalog-union-contracts"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let package = Path.Combine(directory, "consumer")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-union-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "contracts.d.ts")))
        write package "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write package "index.d.ts" "export interface Request { contracts: Contracts; }"
        let generate name package references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) package output |> Async.RunSynchronously |> ignore
            read (Path.Combine(output, "declarations.json"))
        let produced = generate "Owner" owner ([||]: string array)
        generate "Imported" package [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let shared (request: Mini.Imported.Request) = request.contracts.first <- request.contracts.reversed
"""
        Expect.equal code 0 output
        for field in [ "other"; "numeric" ] do
            let code, _ = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ]
                              ("module Mini.Check\nlet invalid (request: Mini.Imported.Request) = request.contracts.first <- request.contracts." + field + "\n")
            Expect.notEqual code 0 "different nominal source contracts and generic applications stay distinct"
        let unions = produced.Declarations |> Array.filter (fun entry -> entry.Role = "source-union")
        Expect.isNonEmpty unions "fully authenticated nominal union contracts get source identities"

[<Tests>]
let unionAliasTests =
    testCase "catalog union identity preserves named alias ownership" <| fun _ ->
        use scratch = Scratch.directory "catalog-union-aliases"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-union-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "aliases.d.ts")))
        let output = Path.Combine(directory, "Owner")
        Pipeline.run (configure directory "Owner" ([||]: string array)) owner output |> Async.RunSynchronously |> ignore
        let catalog = read (Path.Combine(output, "declarations.json"))
        let named name = catalog.Declarations |> Array.find (fun entry -> entry.FSharpName = "Mini.Owner." + name)
        Expect.notEqual (named "NamedFirst").Identity (named "NamedSecond").Identity "written named aliases retain declaration ownership"

[<Tests>]
let unionFallbackTests =
    testTheory "catalog union identity retains fallback for incomplete primitive and cyclic arms" [ "declarations"; "missing"; "primitive"; "cyclic" ] <| fun mutation ->
        use scratch = Scratch.directory "catalog-union-incomplete"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-owner-union-identity-lab")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText(Path.Combine(fixture, "index.d.ts")))
        let exercise = async {
            let config = configure directory "Owner" ([||]: string array)
            let! mailbox, ctx = Bootstrap.start config owner
            use _ = mailbox :> IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage config ctx.PackageName) resolve)
            let shield = shape.Types |> Map.toSeq |> Seq.map snd |> Seq.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "Shield"))
            let unionId = shield.Members |> List.find (fun member_ -> member_.Symbol.Name = "tlsClientAuth") |> _.TypeId
            let union = shape.Types[unionId]
            let arm = List.head union.UnionMembers
            let types =
                match mutation with
                | "declarations" -> Map.add arm { shape.Types[arm] with Declarations = []; AliasDeclarations = [] } shape.Types
                | "missing" -> Map.remove arm shape.Types
                | "primitive" ->
                    let primitive = shape.Types |> Map.toSeq |> Seq.find (fun (_, facts) -> facts.Response.Flags = TypeFlags.String) |> fst
                    Map.add unionId { union with UnionMembers = [ arm; primitive ] } shape.Types
                | _ -> Map.add unionId { union with UnionMembers = [ arm; unionId ] } shape.Types
            let groups: Render.GroupModule list = [
                { Group = "catalog-owner" * uom<npmDependency>
                  IsEntry = true
                  Module = "Mini.Owner"
                  Namespace = None
                  RuntimePackage = "catalog-owner" * uom<importSpecifier>
                  CompilerLib = None
                  Decls = shape.Decls
                  ModuleSpecifiers = Map.empty }
            ]
            let! _, serialized = DeclarationCatalog.apply ctx { shape with Types = types } groups
            let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(Option.get serialized, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
            let child = catalog.Declarations |> Array.find (fun entry -> entry.FSharpName.EndsWith(".TlsClientAuth", StringComparison.Ordinal))
            Expect.isTrue (child.Role.StartsWith("member:", StringComparison.Ordinal)) "unavailable direct nominal-arm evidence retains parent-role fallback"
        }
        exercise |> Async.RunSynchronously

[<Tests>]
let catalogObjectAliasTests =
    testCase "catalog preserves generic object aliases in union arms and abbreviated intersection bases" <| fun _ ->
        use scratch = Scratch.directory "catalog-object-alias-bases"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let consumer = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" """
interface Reg { name: string }
type Section<T extends Reg = Reg> = { key: string; render(input: T): string };
type Wrap<T extends Reg = Reg> = { tool: string; wrap(t: T): T } | { section: string; wrap(s: Section<T>): Section<T> };
interface Ext<T extends Reg = Reg> { sections?: readonly Section<T>[]; wraps?: readonly Wrap<T>[] }
interface Req<M = string> { apiKey?: string; onPayload?: (value: unknown, model: M) => void }
type Cancel = Req<string>;
interface Tx { transform?: (h: Record<string, string>) => void }
type ModelsCancel = Cancel & Tx;
interface Models { cancel(options?: ModelsCancel): void }
type Opt<T> = { deferredValue: T extends string ? string : number };
interface Specializations { a: Opt<string>; b: Opt<number> }
"""
        write consumer "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write consumer "index.d.ts" "export interface Request { ext: Ext; models: Models; specializations: Specializations; }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
        generate "Owner" owner [||]
        generate "Imported" consumer [| Path.Combine(directory, "Owner", "declarations.json") |]
        let source = File.ReadAllText(Path.Combine(directory, "Owner", "Mini.Owner.fs"))
        Expect.stringContains source "abstract deferredValue: string" "resolved string conditional stays precise"
        Expect.stringContains source "abstract deferredValue: float" "resolved numeric conditional stays precise"
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let baseOptions (x: Mini.Owner.ModelsCancel) : Mini.Owner.Req<string> = x :> _
let roundtrip<'T when 'T :> Mini.Owner.Reg> (s: Mini.Owner.Section<'T>) (wrap: Mini.Owner.Wrap3<'T>) : Mini.Owner.Section<'T> = wrap.wrap s
"""
        Expect.equal code 0 output


[<Tests>]
let catalogMappedSpecializationTests =
    testCase "catalog object aliases do not reuse another mapped specialization" <| fun _ ->
        use scratch = Scratch.directory "catalog-mapped-specializations"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let consumer = Path.Combine(directory, "consumer")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" """
type Flags<T> = { [K in keyof T]: boolean };
interface A { a: 1 } interface B { b: 1 }
interface H { x: Flags<A> }
interface P { y: Flags<B> }
"""
        write consumer "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write consumer "index.d.ts" "export interface Request { h: H; p: P }"
        let generate name root references =
            let output = Path.Combine(directory, name)
            Pipeline.run (configure directory name references) root output |> Async.RunSynchronously |> ignore
        generate "Owner" owner [||]
        generate "Imported" consumer [| Path.Combine(directory, "Owner", "declarations.json") |]
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let left (r: Mini.Imported.Request) : bool = r.h.x.a
let right (r: Mini.Imported.Request) : bool = r.p.y.b
"""
        Expect.equal code 0 output
