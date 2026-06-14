module KeyOfWrapper
open System
open Xantham.TypeScript.Types.Node
open Xantham.TypeScript.Types.Symbol
open Scriptorium.Nib.Assertion
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
open Xantham.Mocha.Nib

let tests (runner: Spec.RunnerContext) : unit = runner.ftestSuite "KeyOf" <| fun _ ->
    let testSyntaxKind name fn = runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator name <| fun ctx t nodes ->
        nodes
        |> Array.choose (Node.TypeOperator.create ctx.Program >> function
            | TypeOperator.KeyOf _ as operator -> Some operator
            | _ -> None)
        |> fn ctx t
    testSyntaxKind "keyof" <| fun ctx _ ->
        assertThat (
            foreach (
                tag "keyof"
                >> satisfy _.IsKeyOf
                >> focus (Node.TypeOperator.getTypeNode >> _.parent >> ctx.Checker.getTypeAtLocation)
                )
            >> tag "type"
            >> focus (Array.map (Node.TypeOperator.getTypeNode >> _.parent >> ctx.Checker.getTypeAtLocation))
            >> foreach (
                inside (Type.Kind.create ctx.Program) (apply (printfn "%A"))
                >> apply (ctx.Checker.typeToString >> printfn "%s")
                )
            >> withTag "isIndexType" (exists _.isIndexType())
            >> withTag "isNotIndexType" (exists (_.isIndexType() >> not))
            )