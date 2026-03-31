[<AutoOpen>]
module Xantham.Fable.Types.AutoOpenReaderExtensions

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Signal

type TypeScriptReader with
    member this.CreateXanthamTag(node: Ts.Node) =
        XanthamTag.Create(node, this.checker)
    member this.CreateXanthamTag(typ: Ts.Type) =
        XanthamTag.Create(typ, this.checker)

module XanthamTag =
    let createForNode (ctx: TypeScriptReader) (node: Ts.Node) = ctx.CreateXanthamTag node
    let createForType (ctx: TypeScriptReader) (node: Ts.Type) = ctx.CreateXanthamTag node

type XanthamTag with
    member this.Documentation
        with get() =
            if GuardedData.Documentation.has this then
                GuardedData.Documentation.get this
                |> ValueSome
            else ValueNone
        and set(value: TsComment array) =
            GuardedData.Documentation.set value this
            |> ignore
    member this.SummaryDocumentation
        with get() =
            if GuardedData.SummaryContent.has this then
                GuardedData.SummaryContent.get this
                |> ValueSome
            else ValueNone
        and set(value: TsComment) =
            GuardedData.SummaryContent.set value this |> ignore
    member this.KeyedDocumentation
        with get() =
            if GuardedData.Documentation.Keyed.has this then
                GuardedData.Documentation.Keyed.get this
                |> ValueSome
            else ValueNone
        and set(value: TsComment array) =
            GuardedData.Documentation.Keyed.set value this
            |> ignore
    member this.KeyedSummaryDocumentation
        with get() =
            if GuardedData.SummaryContent.Keyed.has this then
                GuardedData.SummaryContent.Keyed.get this
                |> ValueSome
            else ValueNone
        and set(value: TsComment) =
            GuardedData.SummaryContent.Keyed.set value this |> ignore
    member this.TypeSignal
        with get() =
            if GuardedData.TypeSignal.has this then
                GuardedData.TypeSignal.get this
                |> ValueSome
            else ValueNone
        and set(value: TypeKey) =
            GuardedData.TypeSignal.getOrSetDefault this
            |> _.Set(value)
    member this.Builder
        with get() =
            if GuardedData.AstNodeBuilder.has this then
                GuardedData.AstNodeBuilder.get this
                |> ValueSome
            else ValueNone
        and set(value: STsAstNodeBuilder) =
            GuardedData.AstNodeBuilder.getOrSetDefault this
            |> Signal.fill value
    member this.MemberBuilder
        with get() =
            if GuardedData.MemberBuilder.has this then
                GuardedData.MemberBuilder.get this
                |> ValueSome
            else ValueNone
        and set(value: SMemberBuilder) =
            GuardedData.MemberBuilder.getOrSetDefault this
            |> Signal.fill value
    member this.ParameterBuilder
        with get() =
            if GuardedData.ParameterBuilder.has this then
                GuardedData.ParameterBuilder.get this
                |> ValueSome
            else ValueNone
        and set(value: SParameterBuilder) =
            GuardedData.ParameterBuilder.getOrSetDefault this
            |> Signal.fill value
    member this.ConstructorBuilder
        with get() =
            if GuardedData.ConstructorBuilder.has this then
                GuardedData.ConstructorBuilder.get this
                |> ValueSome
            else ValueNone
        and set(value: SConstructorBuilder) =
            GuardedData.ConstructorBuilder.getOrSetDefault this
            |> Signal.fill value
