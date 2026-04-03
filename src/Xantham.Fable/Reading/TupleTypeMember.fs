module Xantham.Fable.Reading.TupleTypeMember

open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    ctx.CreateXanthamTag node
    |> fst
    |> stackPushAndThen ctx _.TypeSignal

let private namedTupleMemberToTupleElementBuilder (ctx: TypeScriptReader) (namedTupleMember: Ts.NamedTupleMember) =
    STupleElementBuilder.FixedLabeled(
        namedTupleMember.name.text,
        {
            STupleElementTypeBuilder.Type =
                namedTupleMember.``type``
                |> getTypeSignalFromNode ctx
            IsOptional = namedTupleMember.questionToken.IsSome
            IsRest = namedTupleMember.dotDotDotToken.IsSome
        }
    )

let forNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    // check if we've saved this information on the node at some point
    // if so, return it
    match ctx.CreateXanthamTag node |> fst with
    | TagState.Visited tag when tag.TryGet<STupleElementBuilder>() |> ValueOption.isSome ->
        tag.Get<STupleElementBuilder>()
    | TagState.Unvisited tag | TagState.Visited tag as tagState ->
    match tag.Value with
    | XanTagKind.TypeNode typeNode ->
        match typeNode with
        | TypeNode.NamedTupleMember namedTupleMember -> namedTupleMemberToTupleElementBuilder ctx namedTupleMember
        | TypeNode.RestType restType ->
            restType.``type``
            |> getTypeSignalFromNode ctx
            |> STupleElementBuilder.Variadic
        | TypeNode.OptionalType optionalType ->
            STupleElementBuilder.Fixed {
                Type =
                    optionalType.``type``
                    |> getTypeSignalFromNode ctx
                IsRest = false
                IsOptional = true
            }
        | _ ->
            STupleElementBuilder.Fixed {
                Type = stackPushAndThen ctx _.TypeSignal tagState
                IsRest = false
                IsOptional = false
            }
    | _ -> failwith "unexpected tag state"
        
        