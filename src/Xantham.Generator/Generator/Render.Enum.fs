[<AutoOpen>]
module Xantham.Generator.Generator.Render_Enum

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module Enum =
    let renderEnum (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.Interceptors.pipeEnum ctx enumType |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path enumType
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
                                        | Path.Anchor (AnchorPath.Type parentPath) ->
                                            parentPath
                                            |> MemberPath.createOnType (case.Name |> Name.Case.valueOrSource)
                                            |> Path.create
                                        | Path.Transient (TransientPath.Type parentPath) ->
                                            parentPath
                                            |> TransientMemberPath.createOnTransientType (case.Name |> Name.Case.valueOrSource)
                                            |> Path.create
                                        | _ ->
                                            TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure case.Name)
                                            |> Path.create
                            }
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
        
        
    let renderStringUnion (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.Interceptors.pipeEnum ctx enumType |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path enumType
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
                                        | Path.Anchor (AnchorPath.Type parentPath) ->
                                            parentPath
                                            |> MemberPath.createOnType (case.Name |> Name.Case.valueOrSource)
                                            |> Path.create
                                        | Path.Transient (TransientPath.Type parentPath) ->
                                            parentPath
                                            |> TransientMemberPath.createOnTransientType (case.Name |> Name.Case.valueOrSource)
                                            |> Path.create
                                        | _ ->
                                            TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure case.Name)
                                            |> Path.create
                            }
                        LiteralCaseRender.Name = case.Name
                        Value = case.Value
                        Documentation = case.Documentation
                    })
            Documentation = enumType.Documentation
        }
        |> TypeRender.StringUnion
        
    let render (ctx: GeneratorContext) (enumType: EnumType) =
        if enumType.Members |> List.forall _.Value.Value.IsInt
        then renderEnum ctx enumType 
        else renderStringUnion ctx enumType 