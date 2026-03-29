module Xantham.Fable.Reading.LiteralTokenNode

open Fable.Core
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: LiteralTokenNodes) =
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    match tag with
    | LiteralTokenNodes.StringLiteral stringLiteral ->
        TsLiteral.String stringLiteral.text
    | LiteralTokenNodes.BigIntLiteral bigIntLiteral ->
        bigIntLiteral.text
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
    | LiteralTokenNodes.NumericLiteral numericLiteral ->
        let v = JS.Constructors.Number.parseFloat numericLiteral.text
        if JS.Constructors.Number.isSafeInteger v then v |> int |> TsLiteral.Int
        else v |> TsLiteral.Float
    | LiteralTokenNodes.TrueLiteral _ ->
        TsLiteral.Bool true
    | LiteralTokenNodes.FalseLiteral _ ->
        TsLiteral.Bool false
    | LiteralTokenNodes.NullLiteral _ ->
        TsLiteral.Null
    | LiteralTokenNodes.PrefixUnaryExpression prefixUnaryExpression ->
        let inline op factor =
            match prefixUnaryExpression.operator with
            | Ts.SyntaxKind.MinusToken -> (*) (factor -1) // -1
            | Ts.SyntaxKind.PlusToken -> id
            | _ -> failwith "unreachable"
        match prefixUnaryExpression.operand with
        | Patterns.Node.NumericLiteral node ->
            let v = JS.Constructors.Number.parseFloat node.text
            if JS.Constructors.Number.isSafeInteger v then v |> int |> op int |> TsLiteral.Int
            else v |> op float |> TsLiteral.Float
        | _ -> TsLiteral.Null
    | LiteralTokenNodes.NoSubstitutionTemplateLiteral noSubstitutionTemplateLiteral ->
        TsLiteral.String noSubstitutionTemplateLiteral.text
    |> STsAstNodeBuilder.Literal
    |> setAstSignal
    ctx.checker.getTypeAtLocation tag.Value
    |> _.TypeKey
    |> setTypeKeyForTag xanTag

