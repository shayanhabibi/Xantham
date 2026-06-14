module NodeDeclaration

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
    // ----------------------------------------------------------------------------------------------
    //                                  ND - NODE DECLARATION
    // ----------------------------------------------------------------------------------------------
    runner.testSuite "ND · Node Declaration" <| fun _ ->
        runner.testSyntaxKind Ts.SyntaxKind.NumericLiteral "ND-1 · Numeric Literals are all parsable" <| fun _ _ (literals: Ts.NumericLiteral array) ->
            literals
            |> Array.iter (
                _.text
                >> JS.Constructors.Number.parseFloat
                >> function
                    | value when jsTypeof value = "number" -> ()
                    | value -> failtest $"Unrecognised numeric literal: %A{value}"
                )
        runner.testSyntaxKind<Ts.BigIntLiteral> Ts.SyntaxKind.BigIntLiteral "ND-2 · BigInt Literals are all parsable" <| fun _ _ literals ->
            literals
            |> Array.iter (
                _.text
                >> _.TrimEnd('n')
                >> System.Numerics.BigInteger.Parse
                >> ignore
                )
            
        runner.testSyntaxKind<Ts.StringLiteral> Ts.SyntaxKind.StringLiteral "ND-3 · String Literals all have valid string values" <| fun _ _ literals ->
            literals
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        runner.testSyntaxKind<Ts.NoSubstitutionTemplateLiteral> Ts.SyntaxKind.NoSubstitutionTemplateLiteral "ND-4 · NoSubstitutionTemplateLiteral values are valid" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        runner.testSyntaxKind<Ts.PrefixUnaryExpression> Ts.SyntaxKind.PrefixUnaryExpression "ND-5 · PrefixUnaryExpression Operators values are predictable" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.operator
                >> function
                    | Ts.PrefixUnaryOperator.MinusToken -> Expect.passWithMsg "MinusToken is only expected PrefixUnaryOperator in d.ts files"
                    | Ts.PrefixUnaryOperator.PlusPlusToken 
                    | Ts.PrefixUnaryOperator.PlusToken 
                    | Ts.PrefixUnaryOperator.MinusMinusToken 
                    | Ts.PrefixUnaryOperator.TildeToken 
                    | Ts.PrefixUnaryOperator.ExclamationToken as value -> failtest $"Unexpected PrefixUnaryOperator.{value.Name} in d.ts"
                    | value -> failtest $"Received an invalid/unknown PrefixUnaryOperator kind: {value.Name}" 
                )
        runner.testSyntaxKind<Ts.PrefixUnaryExpression> Ts.SyntaxKind.PrefixUnaryExpression "ND-6 · PrefixUnaryExpression Operand values are all numeric literals" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.operand
                >> function
                    | Patterns.SyntaxKind.NumericLiteral _ -> ()
                    | value -> failtest $"Received an invalid/unknown PostfixUnaryExpression operand kind: %s{value.kind.Name}" 
                )
        runner.testSyntaxKind<Ts.LiteralTypeNode> Ts.SyntaxKind.LiteralType "ND-7 · LiteralTypeNode _.literal values are parsed predictably" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.literal
                >> unbox
                >> function
                    | Patterns.SyntaxKind.NullKeyword _
                    | Patterns.SyntaxKind.FalseKeyword _
                    | Patterns.SyntaxKind.TrueKeyword _
                    | Patterns.Node.NumericLiteral _
                    | Patterns.Node.StringLiteral _
                    | Patterns.Node.BigIntLiteral _
                    | Patterns.Node.NoSubstitutionTemplateLiteral _ -> ()
                    | Patterns.Node.PrefixUnaryExpression _ -> ()
                    | node -> failtest $"Unrecognised literal for LiteralTypeNode: %s{node.kind.Name}"
                )
        runner.testCase "ND-8 · DeclarationFiles have a narrowed subset of valid nodes" <| fun _ ctx ->
            ctx.NodeMap.Keys
            |> Seq.distinct
            |> Seq.sortBy _.Name
            |> Seq.iter (function
                | value when DeclarationFileNodes.IsKnownDeclarationFileNodeSyntaxKind value -> ()
                | value -> failtest $"Unexpected node kind in a declaration file: %s{value.Name}"
                )
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "ND-9 · ClassDeclarations have a limited subset of nodes as members" <| fun _ _ nodes ->
            nodes
            |> Array.collect _.members.AsArray
            |> Array.iter (
                function
                    | Patterns.Node.PropertyDeclaration _
                    | Patterns.Node.MethodDeclaration _
                    | Patterns.Node.GetAccessorDeclaration _
                    | Patterns.Node.SetAccessorDeclaration _
                    | Patterns.Node.IndexSignatureDeclaration _
                    | Patterns.Node.ConstructorDeclaration _ -> ()
                    | node -> failtest $"Unrecognised member kind for ClassMember: %s{node.kind.Name}"
                )
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "ND-10 · InterfaceDeclarations have a limited subset of nodes as members" <| fun _ _ nodes ->
            nodes
            |> Array.collect _.members.AsArray
            |> Array.iter (
                function
                    | Patterns.Node.PropertySignature _
                    | Patterns.Node.MethodSignature _
                    | Patterns.Node.GetAccessorDeclaration _
                    | Patterns.Node.SetAccessorDeclaration _
                    | Patterns.Node.IndexSignatureDeclaration _
                    | Patterns.Node.CallSignatureDeclaration _
                    | Patterns.Node.ConstructSignatureDeclaration _ -> ()
                    | node -> failtest $"Unrecognised member kind for Interfacemember: %s{node.kind.Name}"
                )
        runner.testCase "ND-11 · No decorators are present on any node" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> Array.iter (unbox >> ts.getDecorators >>  Expect.isNone >> funApply "This had a decorator")
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "ND-12 · TypeOperators are predictable" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (
                _.operator
                >> function
                    | Ts.SyntaxKind.KeyOfKeyword 
                    | Ts.SyntaxKind.ReadonlyKeyword 
                    | Ts.SyntaxKind.UniqueKeyword -> None
                    | value -> Some value.Name
                )
            |> Expect.isEmpty
            |> funApply "Expected only KeyOf, Readonly and Unique operators."
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "ND-13 · TypeOperators type nodes are predictable" <| fun ctx _ nodes ->
            nodes
            |> Array.map _.``type``
            |> Array.filter ( TypeNode.IsTypeNodeKind >> not )
            |> Expect.isEmpty
            |> funApply "Expected all TypeOperator type nodes to be parsed into XanTagKind.TypeNode"
