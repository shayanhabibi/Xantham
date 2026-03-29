module Xantham.Fable.Reading.TypeFlagLiteral

open Fable.Core
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeFlagLiteral) =
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    let inline forwardToTag (innerTag: XanthamTag) =
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (ctx.typeSignal innerTag).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault innerTag).Value)
    let inline routeToDecl (decl: TypeScript.Ts.Node) =
        let innerTag =
            match ctx.CreateXanthamTag decl |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        forwardToTag innerTag
    match tag with
    | TypeFlagLiteral.String stringLiteral ->
        stringLiteral.value
        |> TsLiteral.String
        |> STsAstNodeBuilder.Literal
        |> setAstSignal
        stringLiteral.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType when
        JS.Constructors.Number.isSafeInteger numberLiteralType.value ->
        
        int numberLiteralType.value
        |> TsLiteral.Int
        |> STsAstNodeBuilder.Literal
        |> setAstSignal
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType ->
        numberLiteralType.value
        |> TsLiteral.Float
        |> STsAstNodeBuilder.Literal
        |> setAstSignal 
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.BigInt bigIntLiteralType ->
        bigIntLiteralType.value
        |> _.base10Value
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
        |> STsAstNodeBuilder.Literal
        |> setAstSignal 
        bigIntLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Boolean literalType ->
        match unbox<string> literalType.value with
        | "true" -> TsLiteral.Bool true
        | _ -> TsLiteral.Bool false
        |> STsAstNodeBuilder.Literal
        |> setAstSignal
        literalType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.UniqueESSymbol key ->
        // UniqueESSymbol is a per-declaration singleton — emit as ESSymbol primitive
        STsAstNodeBuilder.Primitive TypeKindPrimitive.ESSymbol
        |> setAstSignal
        key.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.EnumLiteral literalType ->
        // Route to the enum member declaration so its SEnumCaseBuilder is used
        match literalType.symbol |> Option.ofObj |> Option.bind _.valueDeclaration with
        | None -> ()
        | Some decl -> routeToDecl decl
        literalType.TypeKey
        |> setTypeKeyForTag xanTag

