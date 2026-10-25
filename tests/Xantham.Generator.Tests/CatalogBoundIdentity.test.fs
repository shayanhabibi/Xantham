module Xantham.Generator.Tests.CatalogBoundIdentityTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator
open Xantham.TypeScript.Wire

let private write directory file (text: string) =
    let path = Path.Combine(directory, file)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

let private config directory name references =
    let path = Path.Combine(directory, name + ".json")
    File.WriteAllText(path,
        JsonSerializer.Serialize {| ``module`` = "Mini." + name
                                    entry = "index.d.ts"
                                    lib = [| "esnext" |]
                                    types = [| "catalog-owner" |]
                                    groups = Map.ofList [ "catalog-owner", "ship" ]
                                    declarationCatalog = true
                                    declarationReferences = references |})
    GeneratorConfig.loadFile path

[<Tests>]
let tests =
    testCase "catalog bound naming retains declared alias constraints across owner reachability" <| fun _ ->
        use scratch = Scratch.directory "catalog-bound-naming"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        let consumer = Path.Combine(directory, "consumer")
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "census-bound-alias-naming-lab", "index.d.ts")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" (File.ReadAllText fixture)
        write consumer "package.json" """{"name":"catalog-consumer","version":"1.0.0","types":"index.d.ts"}"""
        write consumer "index.d.ts" "export interface Request { api: Holder; }"
        let generate name package references =
            let output = Path.Combine(directory, name)
            Pipeline.run (config directory name references) package output |> Async.RunSynchronously |> ignore
            File.ReadAllText(Path.Combine(output, "declarations.json"))
            |> fun text -> JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(text, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let produced = generate "Owner" owner ([||]: string array)
        let consumed = generate "Consumer" consumer [||]
        let inlineLoss name =
            File.ReadLines(Path.Combine(directory, name, "symbols.jsonl"))
            |> Seq.exists (fun line -> line.Contains("inlineBound", System.StringComparison.OrdinalIgnoreCase) && line.Contains("TP002"))
        Expect.isTrue (inlineLoss "Owner") "inline structural method bounds retain their existing policy"
        Expect.isTrue (inlineLoss "Consumer") "consumer inline structural method bounds stay structural"
        let holder (catalog: DeclarationCatalog.Catalog) =
            catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName.EndsWith(".Holder"))
        Expect.equal (holder produced).Api (holder consumed).Api "complete source-named method bounds must retain the owner API"
        let env (catalog: DeclarationCatalog.Catalog) =
            catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName.EndsWith(".Env"))
        Expect.equal (env produced).Identity (env consumed).Identity "the bound keeps its source declaration identity"
        generate "Imported" consumer [| Path.Combine(directory, "Owner", "declarations.json") |] |> ignore
        let code, output = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let get (request: Mini.Imported.Request) (value: Mini.Owner.Env) : Mini.Owner.Env = request.api.get value
"""
        Expect.equal code 0 output
        let code, _ = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let invalid (request: Mini.Imported.Request) = request.api.get "not an Env"
"""
        Expect.notEqual code 0 "the imported method retains its nominal Env constraint"
        let code, _ = DeclarationCatalogTests.compileConsumer directory [ "Owner/Mini.Owner.fs"; "Imported/Mini.Imported.fs" ] """module Mini.Check
let invalid (request: Mini.Imported.Request) (value: Mini.Owner.OtherEnv) = request.api.get value
"""
        Expect.notEqual code 0 "an independent equal-shape source alias is not the Env contract"

[<Tests>]
let applicationTests =
    testCase "catalog bound naming rejects a transformed alias application's declaration name" <| fun _ ->
        use scratch = Scratch.directory "catalog-bound-application"
        let directory = scratch.Path
        let owner = Path.Combine(directory, "node_modules", "catalog-owner")
        write owner "package.json" """{"name":"catalog-owner","version":"1.0.0","types":"index.d.ts"}"""
        write owner "index.d.ts" "type GenericEnv<T> = { value: T }; interface AppliedHolder { apply<E extends GenericEnv<string>>(value: E): E; }"
        let exercise = async {
            let! mailbox, ctx = Bootstrap.start (config directory "Owner" ([||]: string array)) owner
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let applied =
                resolve.Types
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "AppliedHolder"))
            let member_ = applied.Members |> List.find (fun member_ -> member_.Symbol.Name = "apply")
            let signature = resolve.Types[member_.TypeId].CallSignatures |> List.head
            let bound = resolve.Types[resolve.Types[List.head signature.TypeParameters].Constraint.Value]
            let! alias = ctx.Session.getAliasSymbolOfType bound.Response.Id
            let alias = ValueOption.get alias
            Expect.equal alias.Name "GenericEnv" "the compiler retains the original generic alias symbol"
            let! declared = ctx.Session.getDeclaredTypeOfSymbol alias.Reference
            Expect.notEqual declared.TypeId bound.Response.TypeId "the applied bound is not the source generic declaration"
            Expect.notEqual bound.SymbolName (Some alias.SymbolName) "alias-symbol equality must not claim the generic declaration's name"
        }
        exercise |> Async.RunSynchronously
