[<AutoOpen>]
module Xantham.Generator.Generator.Render_Enum

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender

module Enum =
    let renderEnum (ctx: GeneratorContext) (enumType: EnumType) =
        {
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    {
                        LiteralCaseRender.Name = case.Name
                        Value =
                            match case.Value with
                            | TsLiteral.Int value -> value
                            | _ -> 0
                        Documentation = case.Documentation
                    }
                    )
                |> List.toArray
            Documentation = enumType.Documentation
        }
        |> TypeRender.EnumUnion
    let renderStringUnion (ctx: GeneratorContext) (enumType: EnumType) =
        {
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    {
                        LiteralCaseRender.Name = case.Name
                        Value = case.Value
                        Documentation = case.Documentation
                    })
                |> List.toArray
            Documentation = enumType.Documentation
        }
        |> TypeRender.StringUnion
    let render (ctx: GeneratorContext) (enumType: EnumType) =
        if enumType.Members |> List.forall _.Value.Value.IsInt
        then renderEnum ctx enumType
        else renderStringUnion ctx enumType