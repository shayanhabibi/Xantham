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
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | StringLiteral" xanTag
        TsLiteral.String stringLiteral.text
    | LiteralTokenNodes.BigIntLiteral bigIntLiteral ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | BigIntLiteral" xanTag
        bigIntLiteral.text
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
    | LiteralTokenNodes.NumericLiteral numericLiteral ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | NumericLiteral" xanTag
        let v = JS.Constructors.Number.parseFloat numericLiteral.text
        if JS.Constructors.Number.isSafeInteger v then v |> int |> TsLiteral.Int
        else v |> TsLiteral.Float
    | LiteralTokenNodes.TrueLiteral _ ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | TrueLiteral" xanTag
        TsLiteral.Bool true
    | LiteralTokenNodes.FalseLiteral _ ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | FalseLiteral" xanTag
        TsLiteral.Bool false
    | LiteralTokenNodes.NullLiteral _ ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | NullLiteral" xanTag
        TsLiteral.Null
    | LiteralTokenNodes.PrefixUnaryExpression prefixUnaryExpression ->
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | PrefixUnaryExpression" xanTag
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
        XanthamTag.debugLocationAndForget "LiteralTokenNode.dispatch | NoSubstitutionTemplateLiteral" xanTag
        TsLiteral.String noSubstitutionTemplateLiteral.text
    |> SType.Literal
    |> setAstSignal
    // It is inappropriate for the typekey to be assumed from a literal token node.
    // this MUST be handled by the parent that is handled the typenode.
    // // ctx.checker.getTypeAtLocation tag.Value
    // // |> _.TypeKey
    // // |> setTypeKeyForTag xanTag

