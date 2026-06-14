module TypeWrapper
open System
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


let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "TFW · Type Flag Wrapper" <| fun _ ->
    // ----------------------------------------------------------------------------------------------
    //                                  TFW - TYPE FLAG WRAPPER
    // ----------------------------------------------------------------------------------------------
    runner.testCase "TFW-1 - Wrappers" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.iter (Type.Kind.create ctx.Program >> function
            | Type.Kind.Enum e -> e.Value.flags.ToStringArray() |> printfn "%A"
            | _ -> ())
    runner.testCase "TFW-2 - Enum Wrappers" <| fun _ ctx ->
        let mutable count = 0
        ctx.Types.Value
        |> Array.iter (Type.Kind.create ctx.Program >> function
            | Type.Kind.Primitive primitive ->
                match primitive with
                | Primitive.Singleton primitiveSingleton -> ()
                | Primitive.Literal (Literal.EnumMember m) ->
                    count <- count + 1
                | _ -> ()
            | _ -> ()
            )
        ctx.Types.Value
        |> Array.choose _.getCanonicalSymbol()
        |> Array.choose _.valueDeclaration
        |> Array.filter (function Patterns.Node.EnumMember _ -> true | _ -> false)
        |> Flip.Expect.hasLength count ""
        
