[<AutoOpen>]
module Xantham.Generator.Generator.Render_Enum

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

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
        
    let renderEnum (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType |> Path.create
        {
            Path = path
            Original = path
            Source = enumType.Source |> Option.toValueOption
            FullyQualifiedName = ValueSome enumType.FullyQualifiedName
        }
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
        
    let renderStringUnion (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType |> Path.create
        { Path = path
          Original = path
          Source = enumType.Source |> Option.toValueOption
          FullyQualifiedName = ValueSome enumType.FullyQualifiedName }
        |> renderStringUnionWithMetadata ctx enumType
        
    let renderWithMetadata (ctx: GeneratorContext) (enumType: EnumType) metadata =
        if enumType.Members |> List.forall _.Value.Value.IsInt
        then renderEnumWithMetadata ctx enumType metadata
        else renderStringUnionWithMetadata ctx enumType metadata
        
    let render (ctx: GeneratorContext) (enumType: EnumType) =
        let path = Path.fromEnum enumType |> Path.create
        { Path = path
          Original = path
          Source = enumType.Source |> Option.toValueOption
          FullyQualifiedName = ValueSome enumType.FullyQualifiedName }
        |> renderWithMetadata ctx enumType