module Parameters


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

let inline tests (runner: Spec.RunnerContext) : unit =
    runner.testSuite "Parameters" <| fun _ ->
        runner.testSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter "Parameters all have a symbol on the node" <| fun _ _ nodes ->
            nodes
            |> Array.map (_.getSymbol() >> Chain.Expect.wantSome "Parameter has a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.FunctionScopedVariable
                |> Flip.Expect.isTrue "Parameter symbol should have function scoped variable flag set"
                symbol.valueDeclaration
                |> Flip.Expect.isSome "Parameter should have a value declaration"
                symbol.declarations
                |> Flip.Expect.isSome "Parameter should have declarations"
                )
        runner.testSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter "Parameters can have more than one declaration on the symbol" <| fun _ _ nodes ->
            // This only exists when the parameter name is derived from a type parameter - the symbol
            // stores the parameter declaration, and the type parameter that informs it.
            nodes
            |> Array.filter (
                _.getSymbol()
                >> Chain.Expect.wantSome "Parameter has a symbol"
                >> _.declarations
                >> Chain.Expect.wantSome "Parameter should have declarations"
                >> _.AsArray.Length.Equals(1)
                >> not
                )
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (Array.iter (fun node ->
                node.parent
                |> unbox<Ts.Node>
                |> _.getText()
                |> printfn "%s"
                node.symbol.declarations.Value.AsArray
                |> Array.iter (_.kind.Name >> printfn "%s")
                ))
        runner.testSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter "Parameter symbols can have internal names" <| fun _ _ nodes ->
            nodes
            |> Array.map _.symbol.symbolName
            |> Array.filter _.IsInternalSymbol
            |> Chain.Expect.skipIfEmpty
            |> ignore
