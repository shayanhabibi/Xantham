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
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    let inline routeToDecl (decl: TypeScript.Ts.Node) =
        ctx.CreateXanthamTag decl
        |> fst
        |> stackPushAndThen ctx id
        |> forwardToTag
    match tag with
    | TypeFlagLiteral.String stringLiteral ->
        stringLiteral.value
        |> TsLiteral.String
        |> SType.Literal
        |> setAstSignal
        stringLiteral.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType when
        JS.Constructors.Number.isSafeInteger numberLiteralType.value ->
        
        int numberLiteralType.value
        |> TsLiteral.Int
        |> SType.Literal
        |> setAstSignal
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType ->
        numberLiteralType.value
        |> TsLiteral.Float
        |> SType.Literal
        |> setAstSignal 
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.BigInt bigIntLiteralType ->
        bigIntLiteralType.value
        |> _.base10Value
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
        |> SType.Literal
        |> setAstSignal 
        bigIntLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Boolean literalType ->
        match unbox<string> literalType.value with
        | "true" -> TsLiteral.Bool true
        | _ -> TsLiteral.Bool false
        |> SType.Literal
        |> setAstSignal
        literalType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.UniqueESSymbol key ->
        // UniqueESSymbol is a per-declaration singleton — emit as ESSymbol primitive
        SType.Primitive TypeKindPrimitive.ESSymbol
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

