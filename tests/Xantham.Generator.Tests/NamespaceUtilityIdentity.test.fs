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
