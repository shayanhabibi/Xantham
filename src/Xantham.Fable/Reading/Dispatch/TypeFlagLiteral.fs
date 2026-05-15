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
        tag.chainDebug xanTag |> forwardToTag
    let debugLocation = sprintf "Dispatching type flag literal of type %s" >> xanTag.doDebugMessage
    match tag with
    | TypeFlagLiteral.String stringLiteral ->
        nameof TypeFlagLiteral.String |> debugLocation
        stringLiteral.value
        |> TsLiteral.String
        |> SType.Literal
        |> setAstSignal
        stringLiteral.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType when
        JS.Constructors.Number.isSafeInteger numberLiteralType.value ->
        "Integer" |> debugLocation
        int numberLiteralType.value
        |> TsLiteral.Int
        |> SType.Literal
        |> setAstSignal
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Number numberLiteralType ->
        nameof TypeFlagLiteral.Number |> debugLocation
        numberLiteralType.value
        |> TsLiteral.Float
        |> SType.Literal
        |> setAstSignal 
        numberLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.BigInt bigIntLiteralType ->
        nameof TypeFlagLiteral.BigInt |> debugLocation
        bigIntLiteralType.value
        |> _.base10Value
        |> System.Numerics.BigInteger.Parse
        |> TsLiteral.BigInt
        |> SType.Literal
        |> setAstSignal 
        bigIntLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.Boolean literalType ->
        nameof TypeFlagLiteral.Boolean |> debugLocation
        match unbox<string> literalType.value with
        | "true" -> TsLiteral.Bool true
        | _ -> TsLiteral.Bool false
        |> SType.Literal
        |> setAstSignal
        literalType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.UniqueESSymbol key ->
        nameof TypeFlagLiteral.UniqueESSymbol |> debugLocation
        // UniqueESSymbol is a per-declaration singleton — emit as ESSymbol primitive
        SType.Primitive TypeKindPrimitive.ESSymbol
        |> setAstSignal
        key.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagLiteral.EnumLiteral literalType ->
        nameof TypeFlagLiteral.EnumLiteral |> debugLocation
        // Route to the enum member declaration so its SEnumCaseBuilder is used
        match literalType.symbol |> Option.ofObj |> Option.bind _.valueDeclaration with
        | None ->
            xanTag.doDebugMessage "No declaration found for enum literal"
        | Some decl ->
            routeToDecl decl
        literalType.TypeKey
        |> setTypeKeyForTag xanTag

