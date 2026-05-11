module Xantham.Fable.Reading.TypeFlagLiteral

open Fable.Core
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeFlagLiteral) =
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    let inline forwardToTag (innerTag: XanthamTag) =
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    let inline routeToDecl (decl: TypeScript.Ts.Node) =
        // Always push: a Visited tag whose Builder hasn't been populated yet
        // would otherwise dangle. The dispatcher's tryGetOrRegisterStore guard
        // makes a redundant pop a no-op.
        let tag = ctx.CreateXanthamTag decl |> fst |> TagState.value
        ctx.stack.Push tag
        XanthamTag.chainDebug xanTag tag |> forwardToTag
    match tag with
    | TypeFlagLiteral.String stringLiteral ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | String" xanTag
        stringLiteral.value
        |> TsLiteral.String
        |> SType.Literal
        |> setAstSignal
        stringLiteral.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType when
        JS.Constructors.Number.isSafeInteger numberLiteralType.value ->
        
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | Integer" xanTag
        int numberLiteralType.value
        |> TsLiteral.Int
        |> SType.Literal
        |> setAstSignal
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | Number" xanTag
        numberLiteralType.value
        |> TsLiteral.Float
        |> SType.Literal
        |> setAstSignal 
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.BigInt bigIntLiteralType ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | BigInt" xanTag
        bigIntLiteralType.value
        |> _.base10Value
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
        |> SType.Literal
        |> setAstSignal 
        bigIntLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Boolean literalType ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | Boolean" xanTag
        match unbox<string> literalType.value with
        | "true" -> TsLiteral.Bool true
        | _ -> TsLiteral.Bool false
        |> SType.Literal
        |> setAstSignal
        literalType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.UniqueESSymbol key ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | UniqueESSymbol" xanTag
        // UniqueESSymbol is a per-declaration singleton — emit as ESSymbol primitive
        SType.Primitive TypeKindPrimitive.ESSymbol
        |> setAstSignal
        key.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.EnumLiteral literalType ->
        XanthamTag.debugLocationAndForget "TypeFlagLiteral.dispatch | EnumLiteral" xanTag
        // Route to the enum member declaration so its SEnumCaseBuilder is used
        match literalType.symbol |> Option.ofObj |> Option.bind _.valueDeclaration with
        | None ->
            XanthamTag.debugLocationAndCommentAndForget "TypeFlagLiteral.dispatch | EnumLiteral" "No symbol or value declaration" xanTag
            ()
        | Some decl ->
            XanthamTag.debugLocationAndCommentAndForget "TypeFlagLiteral.dispatch | EnumLiteral" "Routing decl" xanTag
            routeToDecl decl
        literalType.TypeKey
        |> setTypeKeyForTag xanTag

