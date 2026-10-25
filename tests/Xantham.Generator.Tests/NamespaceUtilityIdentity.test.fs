module Xantham.Generator.Tests.NamespaceUtilityIdentityTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "namespace utility declarations retain authenticated catalogue identities" <| fun _ ->
        use scratch = Scratch.directory "namespace-utility-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "zod-namespace-utility-lab")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Utilities"; DeclarationCatalog = true }
        Pipeline.run config fixture scratch.Path |> Async.RunSynchronously |> ignore
        let readCatalog directory =
            JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(directory, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let catalog = readCatalog scratch.Path
        let aliases =
            [ "Mini.Utilities.Omit"; "Mini.Utilities.V3.Util.Omit"; "Mini.Utilities.Other.Util.Omit" ]
            |> List.map (fun name -> catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = name))
        for alias in aliases do
            Expect.isNonEmpty alias.Handles "each alias authenticates its own source declaration"
            Expect.isNonEmpty alias.Sources "source bytes remain part of the contract"
        Expect.equal (aliases |> List.map _.Identity |> List.distinct |> List.length) 3 "same-shaped same-named aliases in separate namespaces cannot share identity"
        let mirrored = Path.Combine(scratch.Path, "mirrored")
        let mirroredConfig = { config with PublicInputs = Some(Map.ofList [ ".", "index.d.ts"; "./mirror", "index.d.ts" ]) }
        let inputs, _ = Bootstrap.publicPaths mirroredConfig fixture
        Expect.equal (inputs |> List.map _.Key) [ "."; "./mirror" ] "both public input occurrences are selected"
        Expect.equal (inputs |> List.map _.File |> List.distinct |> List.length) 1 "the occurrences authenticate the same source file"
        Pipeline.run mirroredConfig fixture mirrored
        |> Async.RunSynchronously |> ignore
        let mirrors = (readCatalog mirrored).Declarations |> Array.filter (fun declaration -> declaration.FSharpName.EndsWith ".V3.Util.Omit")
        Expect.isNonEmpty mirrors "public inputs retain the nested alias"
        Expect.equal (mirrors |> Array.map _.Identity |> Array.distinct |> Array.length) 1 "multiple public occurrences of the same source alias reuse its source identity"

[<Tests>]
let destructuredParameters =
    testCase "destructured function parameters retain authenticated parent roles" <| fun _ ->
        use scratch = Scratch.directory "destructured-parameter-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "destructured-parameter-identity-lab")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Parameters"; DeclarationCatalog = true }
        async {
            let! mailbox, ctx = Bootstrap.start config fixture
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolved, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage config ctx.PackageName) resolved)
            let parameterId = shape.DeclNames |> Map.toList |> List.find (fun (_, name) -> name = "CreateToolNameMapping0") |> fst
            Expect.isNonEmpty shape.Types[parameterId].Declarations "the inline type has source declaration evidence despite no complete structural key"
            let syntheticExports =
                harvest.Exports |> List.map (fun export ->
                    if export.ExportName = "createToolNameMapping" then
                        { export with Symbol = { export.Symbol with Declarations = ValueSome [||]; ValueDeclaration = ValueNone } }
                    else export)
            let synthetic = { shape with Harvest = { shape.Harvest with Exports = syntheticExports } }
            let! rejected = Async.Catch(DeclarationCatalog.apply ctx synthetic (Pipeline.groupModules ctx synthetic))
            match rejected with
            | Choice1Of2 _ -> failtest "a synthetic callable without source handles cannot anchor an anonymous parameter"
            | Choice2Of2 error -> Expect.stringContains error.Message "no stable declaration or parent role" "unanchored generated names remain rejected"
        } |> Async.RunSynchronously
        Pipeline.run config fixture scratch.Path |> Async.RunSynchronously |> ignore
        let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(scratch.Path, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let parameter = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.CreateToolNameMapping0")
        Expect.isNonEmpty parameter.Handles "anonymous parameters retain their actual source function anchor"
        Expect.isNonEmpty parameter.Sources "parameter source closure is authenticated"
        Expect.stringContains parameter.Role "call:0:parameter:" "the key is a source-function parameter role, not a generated name"
        let copied = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.CopyToolNameMapping0")
        Expect.notEqual parameter.Identity copied.Identity "same-shaped literals under distinct source functions remain distinct"
        let positions = catalog.Declarations |> Array.filter (fun declaration -> declaration.FSharpName.Contains ".Positions")
        Expect.equal positions.Length 3 "both parameter positions and the anonymous return are retained"
        Expect.equal (positions |> Array.map _.Identity |> Array.distinct |> Array.length) 3 "parameter and return roles cannot collide"
        let overloads = catalog.Declarations |> Array.filter (fun declaration -> declaration.FSharpName.Contains ".Overloaded")
        Expect.equal overloads.Length 2 "each overload retains its parameter declaration"
        Expect.equal (overloads |> Array.map _.Identity |> Array.distinct |> Array.length) 2 "overload indices distinguish anonymous parameters"
        let generic = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.GenericMapping0")
        Expect.equal generic.Arity 1 "generic anonymous parameters keep their binder"
        Expect.isNonEmpty generic.Constraints "generic constraints remain in the catalogue contract"
        Expect.isFalse (catalog.Declarations |> Array.exists (fun declaration -> declaration.Role = "source-function-parent")) "traversal roots never become complete callable catalogue identities"
        let altered = Path.Combine(scratch.Path, "altered")
        Directory.CreateDirectory altered |> ignore
        File.Copy(Path.Combine(fixture, "package.json"), Path.Combine(altered, "package.json"))
        File.WriteAllText(Path.Combine(altered, "index.d.ts"), File.ReadAllText(Path.Combine(fixture, "index.d.ts")).Replace("T extends Tool", "T extends Mapping"))
        let reused = Pipeline.run { config with DeclarationReferences = [Path.Combine(scratch.Path, "declarations.json")] } altered (Path.Combine(scratch.Path, "consumer")) |> Async.Catch |> Async.RunSynchronously
        match reused with
        | Choice1Of2 _ -> failtest "a changed source generic constraint cannot reuse an old parameter contract"
        | Choice2Of2 error -> Expect.stringContains error.Message "source" "source mutation remains authenticated"
