module Tracers

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
open Xantham.TypeScript.Types.Type

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Tracers" <| fun _ ->
    runner.testCase "SymbolTracer covers all symbols" <| fun _ ctx ->
        ctx.Symbols.Value
        |> Array.map (SymbolTracer.create ctx.Program)
        |> Array.iter (Flip.Expect.isNotNull "")
    runner.testCase "NodeTracer covers all nodes" <| fun _ ctx ->
        ctx.Nodes
        |> Array.map (NodeTracer.create ctx.Program)
        |> Array.iter (Flip.Expect.isNotNull "")
    runner.testCase "TypeTracer covers all types" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.map (TypeTracer.create ctx.Program)
        |> Array.iter (Flip.Expect.isNotNull "")
