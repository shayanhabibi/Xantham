module NodeKind

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

let inline tests (runner: Spec.RunnerContext) = runner.testSuite "Node Kind" <| fun _ ->
    runner.testCase "Node.Kind is created in totality" <| fun _ ctx ->
        ctx.Nodes
        |> Array.iter (Node.Kind.create ctx.Program >> ignore)
        
