namespace global
#nowarn 42
[<MeasureAnnotatedAbbreviation>]
type string<[<Measure>] 'U> = string

[<AutoOpen>]
module LanguagePrimitiveExtensions =
    module LanguagePrimitives =
        let inline retype<'T, 'U> (x: 'T) = (# "" x : 'U #)
        let inline StringWithMeasure<[<Measure>] 'U> (x: string) : string<'U> = retype x

namespace Xantham.Generator

open System.Runtime.CompilerServices

module Measure =
    /// Measurement is a unique identifier (int)
    [<Measure>]
    type uniqueId

    /// Measurement is a handle, a string identifier
    [<Measure>]
    type handle

    /// Measurement relates to symbols
    [<Measure>]
    type symbol

    /// Measurement relates to types
    [<Measure>]
    type type'

    /// Measurement relates to nodes
    [<Measure>]
    type node

    /// Measurement relates to packages
    [<Measure>]
    type package

    /// Measurement relates to paths
    [<Measure>]
    type path

    /// Measurement relates to files
    [<Measure>]
    type file

    /// Measurement relates to directories
    [<Measure>]
    type directory

    [<Measure>]
    type symbolId = symbol * uniqueId

    [<Measure>]
    type typeId = type' * uniqueId

    [<Measure>]
    type nodeId = node * uniqueId

    [<Measure>]
    type symbolName = symbol * handle

    [<Measure>]
    type declHandle = node * handle

    [<Measure>]
    type npmDependency = package * handle

    [<Measure>]
    type dirPath = directory * path

    [<Measure>]
    type filePath = file * path

    [<Measure>]
    type declFile = node * filePath

    /// A JavaScript module specifier, including package subpaths and ambient modules.
    [<Measure>]
    type importSpecifier

    type UoM<[<Measure>] 'U> =
        [<Extension>]
        static member inline (/)(a: string<'X>, _: UoM<'U>) : string<'X / 'U> = LanguagePrimitives.retype a

        [<Extension>]
        static member inline (*)(a: string<'X>, _: UoM<'U>) : string<'X * 'U> = LanguagePrimitives.retype a

        static member inline (/)(a: int<'X>, _: UoM<'U>) : int<'X / 'U> = LanguagePrimitives.retype a
        static member inline (*)(a: int<'X>, _: UoM<'U>) : int<'X * 'U> = LanguagePrimitives.retype a

    let inline uom<[<Measure>] 'U> = Unchecked.defaultof<UoM<'U>>

/// Tag the compiler's unmeasured protocol fields at the generator boundary.
/// The protocol remains unchanged; generator tables never mix type and symbol ids.
[<AutoOpen>]
module internal WireMeasures =
    open Xantham.TypeScript.Wire.Proto
    open Measure

    type TypeResponse with
        member this.TypeId = this.Id * uom<typeId>

        member this.TargetTypeId =
            this.Target |> ValueOption.map (fun value -> value * uom<typeId>)

        member this.ParameterTypeIds =
            this.TypeParameters
            |> ValueOption.map (Array.map (fun value -> value * uom<typeId>))

        member this.ObjectTypeId =
            this.ObjectType |> ValueOption.map (fun value -> value * uom<typeId>)

        member this.IndexTypeId =
            this.IndexType |> ValueOption.map (fun value -> value * uom<typeId>)

        member this.SymbolId =
            this.Symbol |> ValueOption.map (fun value -> value * uom<symbolId>)

        member this.AliasSymbolId =
            this.AliasSymbol |> ValueOption.map (fun value -> value * uom<symbolId>)

        member this.AliasArgumentTypeIds =
            this.AliasTypeArguments
            |> ValueOption.map (Array.map (fun value -> value * uom<typeId>))

    type SymbolResponse with
        member this.SymbolId = this.Id * uom<symbolId>
        member this.SymbolName = this.Name * uom<symbolName>

        member this.ParentSymbolId =
            this.Parent |> ValueOption.map (fun value -> value * uom<symbolId>)

        member this.DeclarationHandles =
            this.Declarations
            |> ValueOption.map (Array.map (fun value -> value * uom<declHandle>))
