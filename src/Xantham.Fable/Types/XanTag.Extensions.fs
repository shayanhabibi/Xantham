[<AutoOpen>]
module Xantham.Fable.Types.XanTagExtensions

open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Signal

let inline private ifHasThenGetOrNone (xanTag: XanthamTag) (v: GuardedData.KeyedSymbolSlot<'T>) =
    if v.has xanTag then ValueSome (v.get xanTag) else ValueNone

type XanthamTag with
    // ===== Documentation Bag ======
    member this.TryDocumentation with get() = ifHasThenGetOrNone this GuardedData.Documentation
    member this.Documentation with set(value: TsComment array) =
            GuardedData.Documentation.set value this
            |> ignore
    member this.TryKeyedDocumentation with get() = ifHasThenGetOrNone this GuardedData.Documentation.Keyed
    member this.KeyedDocumentation with set(value: TsComment array) = GuardedData.Documentation.Keyed.set value this |> ignore
    // ===== Summary Documentation Bag ======
    member this.TrySummaryDocumentation with get() = ifHasThenGetOrNone this GuardedData.SummaryContent
    member this.SummaryDocumentation with set(value: TsComment) = GuardedData.SummaryContent.set value this |> ignore
    member this.TryKeyedSummaryDocumentation with get() = ifHasThenGetOrNone this GuardedData.SummaryContent.Keyed
    member this.KeyedSummaryDocumentation with set(value: TsComment) = GuardedData.SummaryContent.Keyed.set value this |> ignore
    // ===== TypeKey Bag ======
    member this.TryTypeSignal with get() = ifHasThenGetOrNone this GuardedData.TypeSignal
    member this.TypeSignal
        with get() = GuardedData.TypeSignal.getOrSetDefault this
        and set(value: TypeKey) =
            GuardedData.TypeSignal.getOrSetDefault this
            |> _.Set(value)
    // ===== Builder Bag ======   
    member this.TryBuilder with get() = ifHasThenGetOrNone this GuardedData.AstNodeBuilder
    member this.Builder
        with get() = GuardedData.AstNodeBuilder.getOrSetDefault this
        and set(value: SType) =
            GuardedData.AstNodeBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== MemberBuilder Bag ======  
    member this.TryMemberBuilder with get() = ifHasThenGetOrNone this GuardedData.MemberBuilder
    member this.MemberBuilder
        with get() = GuardedData.MemberBuilder.getOrSetDefault this
        and set(value: SMemberBuilder) =
            GuardedData.MemberBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== ParameterBuilder Bag ====== 
    member this.TryParameterBuilder with get() = ifHasThenGetOrNone this GuardedData.ParameterBuilder
    member this.ParameterBuilder
        with get() = GuardedData.ParameterBuilder.getOrSetDefault this
        and set(value: SParameterBuilder) =
            GuardedData.ParameterBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== ConstructorBuilder Bag ======
    member this.TryConstructorBuilder with get() = ifHasThenGetOrNone this GuardedData.ConstructorBuilder
    member this.ConstructorBuilder
        with get() = GuardedData.ConstructorBuilder.getOrSetDefault this
        and set(value: SConstructorBuilder) =
            GuardedData.ConstructorBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== Export Bag ======
    member this.TryExportBuilder with get() = ifHasThenGetOrNone this GuardedData.ExportBuilder
    member this.ExportBuilder
        with get() = GuardedData.ExportBuilder.getOrSetDefault this
        and set(value: STsExportDeclaration) =
            GuardedData.ExportBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== Source Bag ======
    member this.TrySource with get() = ifHasThenGetOrNone this GuardedData.Source
    member this.Source with set(value: ModuleName) =
        GuardedData.Source.getOrSetWith (fun () -> Signal.source value) this
        |> _.Set(value)
