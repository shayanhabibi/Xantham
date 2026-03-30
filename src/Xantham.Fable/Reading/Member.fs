module Xantham.Fable.Reading.Member

open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal

let resolveToMemberBuilder (ctx: TypeScriptReader) (node: Ts.TypeElement) =
    let tagVisitedState = ctx.CreateXanthamTag node |> fst
    let tag = tagVisitedState.Value
    if tagVisitedState.IsUnvisited then
        pushToStack ctx tag
    tag
    |> GuardedData.AstNodeBuilder.getOrSetDefault
    |> Signal.map (function
        // emit error?
        | ValueNone -> ValueNone
        | ValueSome builder ->
            match builder with
            | Property sPropertyBuilder ->
                SMemberBuilder.Property sPropertyBuilder |> ValueSome
            | Method sMethodBuilder ->
                SMemberBuilder.Method sMethodBuilder |> ValueSome
            | ConstructSignature sConstructSignatureBuilder ->
                SMemberBuilder.ConstructSignature sConstructSignatureBuilder |> ValueSome
            | IndexSignature sIndexSignatureBuilder ->
                SMemberBuilder.IndexSignature sIndexSignatureBuilder |> ValueSome
            | GetAccessor sGetAccessorBuilder ->
                SMemberBuilder.GetAccessor sGetAccessorBuilder |> ValueSome
            | SetAccessor sSetAccessorBuilder ->
                SMemberBuilder.SetAccessor sSetAccessorBuilder |> ValueSome
            | CallSignature sCallSignatureBuilder ->
                SMemberBuilder.CallSignature sCallSignatureBuilder |> ValueSome
            | _ ->
                // emit error?
                ValueNone
        )
    