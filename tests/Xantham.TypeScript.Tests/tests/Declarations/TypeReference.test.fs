module TypeReference

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
    runner.testSuite "TR · Type References" <| fun _ ->
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TR-1 · TypeReferences _.typeName always resolves to a symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (
                _.typeName
                >> unbox
                >> ctx.Checker.getSymbolAtLocation
                >> Flip.Expect.isSome "Expected type reference entity name to resolve to a symbol"
                )
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TR-2 · TypeReferences type arguments are either None or NonEmpty" <| fun ctx _ nodes ->
            nodes
            |> Array.choose _.typeArguments
            |> Array.iter (Flip.Expect.isNonEmpty "Expected at least one entry in type arguments if not null")
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TR-3 · TypeReferences _.typeName that resolve to a NON-TRANSIENT symbol have declarations" <| fun ctx _ nodes ->
            let getSymbolFlags idx = nodes[idx].typeName |> unbox |> ctx.Checker.getSymbolAtLocation |> Option.get |> _.flags.ToStringArray()
            nodes
            |> Array.iteri (fun idx ->
                _.typeName
                >> unbox
                >> ctx.Checker.getSymbolAtLocation
                >> Option.map ctx.Checker.getMergedSymbol
                >> Option.filter (ts.isTransientSymbol >> not)
                >> Option.iter (fun symbol ->
                    symbol.getDeclarations()
                    |> Option.map _.AsArray
                    |> Option.defaultValue [||]
                    |> Flip.Expect.isNonEmpty $"Expected at least one declaration for type reference entity name: {nodes[idx].getText()} | Flags: {getSymbolFlags idx}"
                    )
                )
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TR-4 · TypeReferences type arguments do not necessarily match the arity of the target" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (_.typeName >> unbox >> ctx.Checker.getSymbolAtLocation >> Option.get >> ctx.Checker.getMergedSymbol >> ts.isTransientSymbol >> not)
            |> Array.choose (fun node ->
                let symbol = 
                    node.typeName
                    |> unbox<Ts.Node>
                    |> ctx.Checker.getSymbolAtLocation
                    |> Option.get
                    |> ctx.Checker.getMergedSymbol
                symbol.declarations.Value.AsArray
                |> Array.filter (fun decl ->
                    ts.isClassDeclaration decl || ts.isInterfaceDeclaration decl || ts.isTypeAliasDeclaration decl
                    )
                |> Array.tryHead
                |> Option.map (unbox >> ts.getEffectiveTypeParameterDeclarations >> _.AsArray)
                |> Option.map (fun typars -> node, symbol, typars)
            )
            |> Array.choose (fun (node, symbol, typeParameters) ->
                let nodeTypeArguments = node.typeArguments |> Option.map _.AsArray |> Option.defaultValue Array.empty
                if nodeTypeArguments.Length > typeParameters.Length then
                    Some $"SymbolName: {symbol.name} Expected: {typeParameters.Length} Actual: {nodeTypeArguments.Length} | ActualTyparKinds: {nodeTypeArguments |> Array.map _.kind.Name} | ExpectedTypars: {typeParameters |> Array.map _.name.text}"
                elif nodeTypeArguments.Length <> typeParameters.Length then
                    typeParameters
                    |> Array.skip nodeTypeArguments.Length
                    |> Flip.Expect.all (fun typar -> typar.``default``.IsSome || typar.``constraint``.IsSome) "Expected missing typars to have a default or a constraint"
                    Some ""
                else None
                )
            |> function
                | [||] -> Expect.skip()
                | arr when Array.forall String.IsNullOrEmpty arr ->
                    Expect.pass()
                | arr ->
                    
                    arr
                    |> Array.filter (String.IsNullOrEmpty >> not)
                    |> String.concat "\n"
                    |> failtestf "Type arguments exceeded expected type parameters: %s"
