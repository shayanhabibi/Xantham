module Xantham.Generator.Tests.DependentConstraintTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "dependent generic constraints preserve supplied arguments" <| fun _ ->
        use scratch = Scratch.directory "dependent-generic-constraints"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "zod-dependent-constraints-lab")
        let output = Path.Combine(scratch.Path, "generated")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.DependentConstraints" }
        Pipeline.run config fixture output |> Async.RunSynchronously |> ignore
        let cyclicFindings =
            File.ReadLines(Path.Combine(output, "symbols.jsonl"))
            |> Seq.choose (fun line ->
                use document = JsonDocument.Parse line
                let symbol = document.RootElement
                if symbol.GetProperty("findings").EnumerateArray() |> Seq.exists (fun finding -> finding.GetProperty("key").GetString() = "TR074") then
                    Some(symbol.GetProperty("name").GetString())
                else None)
            |> Set.ofSeq
        Expect.contains cyclicFindings "AnySelf" "a corrected self-bound application is explicitly widened"
        Expect.isFalse (Set.contains "SelfConsumer" cyclicFindings) "valid nominal self recursion is retained"
        Expect.isFalse (Set.contains "CycleConsumer" cyclicFindings) "valid nominal mutual recursion is retained"
        let generated = "generated/Mini.DependentConstraints.fs"
        let code, log = DeclarationCatalogTests.compileConsumer scratch.Path [ generated ] """module Mini.Check
open Mini.DependentConstraints
let defaulted (value: ArrayConsumer<Schema<obj array, obj array, Internals<obj array, obj array>>>) = value.schema
let explicit (value: ExplicitConsumer<Schema<string, float, Internals<string, float>>>) = value.schema
let generic<'O, 'I, 'T when 'T :> Internals<'O, 'I>> (value: GenericConsumer<'O, 'I, 'T>) = value :> Schema<'O, 'I, 'T>
let method (value: Methods) (schema: Schema<string, float, Internals<string, float>>) = value.``use`` schema
let nested (value: NestedConsumer) : Schema<float, string, Internals<float, string>> = value.value.value
let forward (value: ForwardConsumer) : Wide = value.value.a.output
let recursive (value: SelfConsumer) : Recursive = value.value.value
let cycle (value: CycleConsumer) : Right = value.value.a.output
let refused (value: obj) : AnySelf = value
"""
        Expect.equal code 0 log
        let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ generated ] """module Mini.Invalid
open Mini.DependentConstraints
let invalid (value: ExplicitConsumer<Schema<float, string, Internals<float, string>>>) = value.schema
"""
        Expect.notEqual code 0 "a different dependent nominal contract remains invalid"
