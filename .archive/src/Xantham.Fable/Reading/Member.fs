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
    tag |> GuardedData.MemberBuilder.getOrSetDefault

let resolveClassMemberBuilder (ctx: TypeScriptReader) (node: Ts.ClassElement) =
    let tagVisitedState = ctx.CreateXanthamTag node |> fst
    let tag = tagVisitedState.Value
    if tagVisitedState.IsUnvisited then
        pushToStack ctx tag
    tag |> GuardedData.MemberBuilder.getOrSetDefault

let resolveToConstructorBuilder (ctx: TypeScriptReader) (node: Ts.ConstructorDeclaration) =
    let tagVisitedState = ctx.CreateXanthamTag node |> fst
    let tag = tagVisitedState.Value
    if tagVisitedState.IsUnvisited then
        pushToStack ctx tag
    tag |> GuardedData.ConstructorBuilder.getOrSetDefault

let resolveToParameterBuilder (ctx: TypeScriptReader) (node: Ts.ParameterDeclaration) =
    let tagVisitedState = ctx.CreateXanthamTag node |> fst
    let tag = tagVisitedState.Value
    if tagVisitedState.IsUnvisited then
        pushToStack ctx tag
    tag |> GuardedData.ParameterBuilder.getOrSetDefault