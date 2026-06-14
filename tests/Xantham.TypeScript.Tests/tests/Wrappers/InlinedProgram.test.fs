module InlinedProgram

open System
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

type NodeWrapper =
    | Node of Ts.Node parentInlinedProgram
    interface IInlinedProgram

[<Erase>]
type InlinedWrapper = InlinedWrapper of Ts.Node inlinedProgram interface IUnwrappable<Ts.Node> interface IInlinedProgram
    

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "ParentInlinedProgram" <| fun _ ->
    runner.testCase "ParentInlinedProgram stores the program on the union" <| fun _ ctx ->
        ctx.Nodes
        |> Array.iter (
            ParentInlinedProgram.wrap ctx.Program NodeWrapper.Node
            >> _.checker
            >> Flip.Expect.isNotNull "ParentInlinedProgram should not be null"
            )
    runner.testCase "InlinedProgram stores the program on the object itself" <| fun _ ctx ->
        ctx.Nodes
        |> Array.iter (
            InlinedProgram.create ctx.Program
            >> InlinedWrapper
            >> _.checker
            >> Flip.Expect.isNotNull "InlinedProgram should not be null"
            )