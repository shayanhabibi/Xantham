module Identifiers

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

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Identifier" <| fun _ ->
    for identifierKind in [|
        Ts.SyntaxKind.Identifier
        Ts.SyntaxKind.ComputedPropertyName
        Ts.SyntaxKind.QualifiedName
    |] do
        runner.testSyntaxKind<Ts.Node> identifierKind (sprintf "Identifier captures %A" identifierKind.Name) <| fun ctx _ nodes ->
            nodes
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (
                Array.map (Node.Identifier.tryCreate ctx.Program)
                >> Array.iter (Flip.Expect.isSome "Identifier is not None")
                )
        runner.testSyntaxKind<Ts.Node> identifierKind (sprintf "%s Does not always resolve to a symbol" identifierKind.Name) <| fun ctx _ nodes ->
            nodes
            |> Array.map (Node.Identifier.tryCreate ctx.Program)
            |> Array.filter _.IsNone
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.Node> identifierKind (sprintf "%s Will always resolve an identifier if the parent is a declaration statement" identifierKind.Name) <| fun ctx _ nodes ->
            nodes
            |> Array.filter (_.parent >> ts.isDeclarationStatement)
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (
                Array.map (Node.Identifier.tryCreate ctx.Program >> Chain.Expect.wantSome "Identifier should capture kind")
                >> Array.iter (fun identifier ->
                    Node.Identifier.getSymbolKind identifier
                    |> Option.orElseWith (fun () -> Utils.trace identifier; None)
                    |> Flip.Expect.isSome $"Identifier should resolve to a symbol: {Node.Identifier.toNode identifier |> _.getText()}"
                    )
                )
        runner.testSyntaxKind<Ts.Node> identifierKind (sprintf "%s successfully flatten to strings" identifierKind.Name) <| fun ctx _ nodes ->
            nodes
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (
                Array.map (Node.Identifier.tryCreate ctx.Program >> Flip.Expect.wantSome "Identifier should capture kind")
                >> Array.iter (Node.Identifier.flattenToStringArray >> Flip.Expect.isNotEmpty "Identifier should flatten to strings")
                )
            
        runner.testSyntaxKind<Ts.Node> identifierKind (sprintf "%s successfully flatten to strings: print a selection" identifierKind.Name) <| fun ctx _ nodes ->
            nodes
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (
                Array.map (Node.Identifier.tryCreate ctx.Program >> Flip.Expect.wantSome "Identifier should capture kind")
                >> Array.randomSample (min 10 nodes.Length)
                >> Array.iter (Node.Identifier.flattenToStringArray >> Chain.Expect.isNotEmpty "Identifier should flatten to strings" >> printfn "%A")
                )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName "ComputedPropertyName expression is always PropertyAccessExpression or Identifier" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (
                _.expression.kind.Name
                >> function
                    | "PropertyAccessExpression" | "Identifier" -> ()
                    | value -> failtest $"ComputedPropertyName expression should be PropertyAccessExpression or Identifier, not %s{value}"
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName "ComputedPropertyName always yields a valid symbol" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (
                ctx.Checker.getSymbolAtLocation 
                >> Flip.Expect.isSome "ComputedPropertyName should always yield a valid symbol"
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName "ComputedPropertyName expression always yields a valid symbol" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (
                _.expression
                >> ctx.Checker.getSymbolAtLocation 
                >> Flip.Expect.isSome "ComputedPropertyName should always yield a valid symbol"
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName "ComputedPropertyName always yields a transient symbol" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (
                ctx.Checker.getSymbolAtLocation
                >> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                >> Symbol.Kind.create ctx.Program
                >> _.IsTransient
                >> Flip.Expect.isTrue "ComputedPropertyName should always yield a transient symbol"
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName
        "ComputedPropertyName expression always yields a non-transient symbol which is a Variable, Property, or EnumMember -Kind" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun node ->
                node.expression
                |> ctx.Checker.getSymbolAtLocation
                |> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                |> Symbol.Kind.create ctx.Program
                |> function
                    | Symbol.Kind.Variable _
                    | Symbol.Kind.Property _ -> ()
                    | Symbol.Kind.EnumMember _ -> ()
                    | symbolKind ->
                        failtest $"ComputedPropertyName should always yield a transient symbol which is a Variable or Property-Kind, not %A{symbolKind}"
                )
            )
    // these should just be resolving immediately and skipping dynamic/late binding, but whatever.
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName
        "ComputedPropertyName expressions of EnumMember kind are always a PropertyAccessExpression" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun node ->
                node.expression
                |> ctx.Checker.getSymbolAtLocation
                |> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                |> Symbol.Kind.create ctx.Program
                |> function
                    | Symbol.Kind.EnumMember _ ->
                        node.expression.kind
                        |> Flip.Expect.equal Ts.SyntaxKind.PropertyAccessExpression $"ComputedPropertyName expression with an EnumMember symbol should be a PropertyAccessExpression. Got {node.expression.kind.Name} instead"
                    | _ -> ()
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName
        "ComputedPropertyName expressions of Property kind are always a PropertyAccessExpression" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun node ->
                node.expression
                |> ctx.Checker.getSymbolAtLocation
                |> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                |> Symbol.Kind.create ctx.Program
                |> function
                    | Symbol.Kind.Property _ ->
                        node.expression.kind
                        |> Flip.Expect.equal Ts.SyntaxKind.PropertyAccessExpression $"ComputedPropertyName expression with a Property symbol should be a PropertyAccessExpression. Got {node.expression.kind.Name} instead"
                    | _ -> ()
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName
        "ComputedPropertyName expressions of Variable kind are an identifier or a propertyaccessexpression" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun node ->
                node.expression
                |> ctx.Checker.getSymbolAtLocation
                |> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                |> Symbol.Kind.create ctx.Program
                |> function
                    | Symbol.Kind.Variable _ ->
                        node.expression.kind
                        |> (&&&) (Ts.SyntaxKind.Identifier ||| Ts.SyntaxKind.PropertyAccessExpression)
                        |> (=) node.expression.kind
                        |> Flip.Expect.isTrue $"ComputedPropertyName expression with a Variable symbol should be a PropertyAccessExpression or Identifier. Got {node.expression.kind.Name} instead"
                    | _ -> ()
                )
            )
    runner.testSyntaxKind<Ts.ComputedPropertyName> Ts.SyntaxKind.ComputedPropertyName
        "ComputedPropertyName expressions are always of type primitive literal unique symbol, string, or enum string" <| fun ctx _ nodes ->
        nodes
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.iter (fun node ->
                node.expression
                |> ctx.Checker.getSymbolAtLocation
                |> Flip.Expect.wantSome "ComputedPropertyName should always yield a valid symbol"
                |> Symbol.Kind.create ctx.Program
                |> function
                    | Symbol.Kind.Property sym ->
                        sym.canonicalWithType
                        |> snd
                        |> Type.Kind.create ctx.Program
                        |> function
                            | Kind.Primitive (Primitive.Literal (Literal.UniqueESSymbol _ )) -> ()
                            | x -> failtestf "ComputedPropertyName expression with a Property symbol (PropertyAccessExpression) should be a UniqueESSymbol. Got %A instead" x
                    | Symbol.Kind.Variable sym ->
                        sym.canonicalWithType
                        |> snd
                        |> Type.Kind.create ctx.Program
                        |> function
                            | Kind.Primitive (Primitive.Literal (Literal.UniqueESSymbol _ | Literal.PrimitiveLiteral (PrimitiveLiteral.String _))) -> ()
                            | x -> failtestf "ComputedPropertyName expression with a Variable symbol (PropertyAccessExpression/Identifier) should be a PrimitiveLiteral or UniqueESSymbol. Got %A instead" x
                    | Symbol.Kind.EnumMember sym ->
                        sym.canonicalWithType
                        |> snd
                        |> Type.Kind.create ctx.Program
                        |> function
                            | Kind.Primitive (
                                Primitive.Literal (Literal.EnumMember (EnumMember value))
                                ) ->
                                match value.Value with
                                | PrimitiveLiteral.String _ -> ()
                                | _ -> failtestf "ComputedPropertyName expression with a EnumMember symbol should be of the string subtype. Got %A instead" value.Value
                            | x -> failtestf "ComputedPropertyName expression with a EnumMember symbol should be a EnumMember type. Got %A instead" x
                    | _ -> ()
                )
            )
    
