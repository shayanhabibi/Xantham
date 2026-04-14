[<AutoOpen>]
module Xantham.Generator.Generator.Render_Enum

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.TypeRenders

module Enum =
    let renderEnumWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata =
        {
            Metadata = metadata
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    {
                        Metadata =
                            {
                                metadata with
                                    Path =
                                        match metadata.Path with
                                        | ValueSome path ->
                                            match path with
                                            | Path.Anchor (AnchorPath.Type parentPath) ->
                                                parentPath
                                                |> MemberPath.createOnType (case.Name |> Name.Case.valueOrSource)
                                                |> Path.create
                                                |> ValueSome
                                            | _ -> ValueNone
                                        | _ -> ValueNone
                            }
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
    let renderEnum (ctx: GeneratorContext) (enumType: EnumType) =
        Path.fromEnum enumType
        |> RenderMetadata.create
        |> renderEnumWithMetadata ctx enumType
    let inline renderEnumWithPath (ctx: GeneratorContext) (enumType: EnumType) (path: ^T) =
        RenderMetadata.create path
        |> renderEnumWithMetadata ctx enumType
    let renderStringUnionWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata =
        {
            Metadata = metadata
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    {
                        Metadata =
                            {
                                metadata with
                                    Path =
                                        match metadata.Path with
                                        | ValueSome path ->
                                            match path with
                                            | Path.Anchor (AnchorPath.Type parentPath) ->
                                                parentPath
                                                |> MemberPath.createOnType (case.Name |> Name.Case.valueOrSource)
                                                |> Path.create
                                                |> ValueSome
                                            | _ -> ValueNone
                                        | _ -> ValueNone
                            }
                        LiteralCaseRender.Name = case.Name
                        Value = case.Value
                        Documentation = case.Documentation
                    })
                |> List.toArray
            Documentation = enumType.Documentation
        }
        |> TypeRender.StringUnion
    let renderStringUnion (ctx: GeneratorContext) (enumType: EnumType) =
        Path.fromEnum enumType
        |> RenderMetadata.create
        |> renderStringUnionWithMetadata ctx enumType
    let renderWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata =
        if enumType.Members |> List.forall _.Value.Value.IsInt
        then renderEnumWithMetadata ctx enumType metadata
        else renderStringUnionWithMetadata ctx enumType metadata
    let render (ctx: GeneratorContext) (enumType: EnumType) =
        Path.fromEnum enumType
        |> RenderMetadata.create
        |> renderWithMetadata ctx enumType
    let inline renderWithPath (ctx: GeneratorContext) (enumType: EnumType) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx enumType