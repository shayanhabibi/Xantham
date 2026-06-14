module TypeKind

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

let inline tests (runner: Spec.RunnerContext) = runner.testSuite "Type Kind" <| fun _ ->
    runner.testCase "Type.Kind is created in totality" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.iter (Type.Kind.create ctx.Program >> ignore)