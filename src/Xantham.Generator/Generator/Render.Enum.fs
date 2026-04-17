[<AutoOpen>]
module Xantham.Generator.Generator.Render_Enum

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module Enum =
    let renderEnumWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata: TypeRender =
        {
            Metadata = metadata
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    let path = Path.fromEnumCase None case |> AnchorPath.create
                    let metadata = RenderMetadata.create path
                    {
                        Metadata = metadata
                        LiteralCaseRender.Name = case.Name
                        Value =
                            match case.Value with
                            | TsLiteral.Int value -> value
                            | _ -> 0
                        Documentation = case.Documentation
                    }
                    )
            Documentation = enumType.Documentation
        }
        |> TypeRender.EnumUnion
        
    let renderEnum (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType 
        let metadata = RenderMetadata.create path
        renderEnumWithMetadata ctx enumType metadata
        
    let renderStringUnionWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata: TypeRender =
        {
            Metadata = metadata
            LiteralUnionRender.Name = enumType.Name
            Cases =
                enumType.Members
                |> List.map (_.Value >> fun case ->
                    let metadata = RenderMetadata.create (Path.fromEnumCase None case)
                    {
                        Metadata = metadata
                        LiteralCaseRender.Name = case.Name
                        Value = case.Value
                        Documentation = case.Documentation
                    })
            Documentation = enumType.Documentation
        }
        |> TypeRender.StringUnion
        
    let renderStringUnion (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType 
        RenderMetadata.create path
        |> renderStringUnionWithMetadata ctx enumType
        
    let renderWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata =
        if enumType.Members |> List.forall _.Value.Value.IsInt
        then renderEnumWithMetadata ctx enumType metadata
        else renderStringUnionWithMetadata ctx enumType metadata
        
    let render (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType 
        RenderMetadata.create path
        |> renderWithMetadata ctx enumType