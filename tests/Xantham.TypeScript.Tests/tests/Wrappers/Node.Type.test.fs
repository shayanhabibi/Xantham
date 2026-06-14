module NodeType

open System
open Microsoft.FSharp.Reflection
open Xantham.TypeScript.Types.Symbol
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha

let inline tests (runner: Spec.RunnerContext) = runner.testSuite "Node Types" <| fun _ ->
    runner.testCase "Node.Type is created in totality" <| fun _ ctx ->
        ctx.Nodes
        |> Array.filter ts.isTypeNode
        |> unbox<Ts.TypeNode array>
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (fun node ->
            Node.Type.create ctx.Program node
            |> Flip.Expect.isNotNull "Node.Type should not be null"
            ))
    let unionCaseNames =
        FSharpType.GetUnionCases(typeof<Node.Type>)
        |> Array.map _.Name
    let caseCount = Dictionary<string, int>()
    beforeTests "Initialise case count" <| fun _ ->
        for caseName in unionCaseNames do
            caseCount[caseName] <- 0
    afterTests "Print case count" <| fun _ ->
        let totalCount = caseCount |> Seq.sumBy _.Value
        caseCount
        |> Seq.sortByDescending _.Value
        |> Seq.iter (fun (KeyValue(caseName, count)) -> printfn "%s: %d (%.2f)" caseName count (float count / float totalCount * 100.0))
    runner.testCase "Node.Type has a case for each union case" <| fun _ ctx ->
        let incCount case = caseCount[case] <- caseCount[case] + 1
        ctx.Nodes
        |> Array.filter ts.isTypeNode
        |> unbox<Ts.TypeNode array>
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (Array.iter (fun node ->
            FSharpValue.GetUnionFields(Node.Type.create ctx.Program node, typeof<Node.Type>)
            |> fst
            |> _.Name
            |> incCount
            ))