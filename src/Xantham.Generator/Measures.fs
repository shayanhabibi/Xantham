namespace global
#nowarn 42
[<MeasureAnnotatedAbbreviation>] type string<[<Measure>] 'U> = string

[<AutoOpen>]
module LanguagePrimitiveExtensions =
    module LanguagePrimitives =
        let inline retype<'T, 'U> (x: 'T) = (# "" x : 'U #)
        let inline StringWithMeasure<[<Measure>] 'U> (x: string): string<'U> = retype x

namespace Xantham.Generator

module Measure =
    [<Measure>] type symbolId
    [<Measure>] type typeId
    [<Measure>] type nodeId
    [<Measure>] type symbolName
    [<Measure>] type declFile
    [<Measure>] type declHandle
    
    module String =
        let inline tag<[<Measure>] 'U> (x: string): string<'U> = LanguagePrimitives.retype x
        let inline untag (x: string<'U>): string = LanguagePrimitives.retype x
        let inline retag<[<Measure>] 'U, [<Measure>] 'New> (x: string<'U>): string<'New> = LanguagePrimitives.retype x
    
    module Int =
        let inline tag<[<Measure>] 'U> (x: int): int<'U> = LanguagePrimitives.retype x
        let inline untag (x: int<'U>): int = LanguagePrimitives.retype x
        let inline retag<[<Measure>] 'U, [<Measure>] 'New> (x: int<'U>): int<'New> = LanguagePrimitives.retype x

/// Tag the compiler's unmeasured protocol fields at the generator boundary.
/// The protocol remains unchanged; generator tables never mix type and symbol ids.
[<AutoOpen>]
module internal WireMeasures =
    open Xantham.TypeScript.Wire.Proto
    open Measure

    type TypeResponse with
        member this.TypeId = Measure.Int.tag<typeId> this.Id
        member this.TargetTypeId = this.Target |> ValueOption.map Measure.Int.tag<typeId>
        member this.ParameterTypeIds = this.TypeParameters |> ValueOption.map (Array.map Measure.Int.tag<typeId>)
        member this.ObjectTypeId = this.ObjectType |> ValueOption.map Measure.Int.tag<typeId>
        member this.IndexTypeId = this.IndexType |> ValueOption.map Measure.Int.tag<typeId>
        member this.SymbolId = this.Symbol |> ValueOption.map Measure.Int.tag<symbolId>
        member this.AliasSymbolId = this.AliasSymbol |> ValueOption.map Measure.Int.tag<symbolId>
        member this.AliasArgumentTypeIds = this.AliasTypeArguments |> ValueOption.map (Array.map Measure.Int.tag<typeId>)

    type SymbolResponse with
        member this.SymbolId = Measure.Int.tag<symbolId> this.Id
        member this.SymbolName = String.tag<symbolName> this.Name
        member this.ParentSymbolId = this.Parent |> ValueOption.map Measure.Int.tag<symbolId>
        member this.DeclarationHandles = this.Declarations |> ValueOption.map (Array.map String.tag<declHandle>)
