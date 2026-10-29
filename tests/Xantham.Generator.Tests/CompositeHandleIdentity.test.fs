module Xantham.Generator.Tests.CompositeHandleIdentityTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "independent intersections may share an authenticated library operand" <| fun _ ->
        use scratch = Scratch.directory "composite-handle-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "composite-handle-identity-lab")
        let owner = Path.Combine(scratch.Path, "owner")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Owner"; Entry = Some "producer.d.ts"; DeclarationCatalog = true }
        Pipeline.run config fixture owner |> Async.RunSynchronously |> ignore
        let consumer = { config with ModuleName = Some "Mini.Consumer"; Entry = Some "consumer.d.ts"; DeclarationReferences = [Path.Combine(owner, "declarations.json")] }
        let independent = Path.Combine(scratch.Path, "independent")
        Pipeline.run { consumer with DeclarationReferences = [] } fixture independent |> Async.RunSynchronously |> ignore
        let readCatalog directory =
            JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(directory, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let ownerCatalog = readCatalog owner
        let independentCatalog = readCatalog independent
        for leaf, role in [ "Value", "source-intersection"; "Choice", "source-union" ] do
            let first = ownerCatalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Owner.Producer." + leaf)
            let second = independentCatalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Consumer.Consumer." + leaf)
            Expect.equal first.Role role "the source composition supplies the identity role"
            Expect.equal second.Role role "the independently written composition uses the same role"
            Expect.notEqual first.Identity second.Identity "different complete source compositions are distinct contracts"
            Expect.isNonEmpty (Set.intersect (Set.ofArray first.Handles) (Set.ofArray second.Handles)) "the genuine contracts share an authenticated operand handle"
        let output = Path.Combine(scratch.Path, "consumer")
        Pipeline.run consumer fixture output |> Async.RunSynchronously |> ignore
        let actual = readCatalog output
        for leaf in [ "Value"; "Choice" ] do
            Expect.isTrue (actual.Declarations |> Array.exists (fun declaration -> declaration.FSharpName = "Mini.Consumer.Consumer." + leaf && declaration.Owner = "Mini.Consumer")) "overlap cannot grant producer ownership"
        let code, log = DeclarationCatalogTests.compileConsumer scratch.Path [ "owner/Mini.Owner.fs"; "consumer/Mini.Consumer.fs" ] """module Mini.Check
let reused (value: Mini.Consumer.Consumer) : Mini.Owner.Producer = value.producer
let independent (value: Mini.Consumer.Consumer.Value) : string = value.text
let first (value: Mini.Owner.Producer.Value) : float = value.count
"""
        Expect.equal code 0 log
        let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ "owner/Mini.Owner.fs"; "consumer/Mini.Consumer.fs" ] """module Mini.Invalid
let wrong (value: Mini.Consumer.Consumer.Value) : Mini.Owner.Producer.Value = value
"""
        Expect.notEqual code 0 "sharing a mapped operand never makes the complete intersections interchangeable"
        for entry in [ "augmented.d.ts"; "augmented-union.d.ts" ] do
            Expect.throwsC
                (fun () -> Pipeline.run { consumer with Entry = Some entry } fixture (Path.Combine(scratch.Path, entry)) |> Async.RunSynchronously |> ignore)
                (fun error -> Expect.isTrue (error.Message.Contains "declaration handle set conflict" || error.Message.Contains "F# API mismatch") ("nominal merge drift inside a composite must still reject: " + error.Message))
