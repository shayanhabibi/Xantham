[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Prelude
open FSharp.SignalsDotnet

module Interface =
    let render (ctx: GeneratorContext) scopeStore (shape: Interface): TypeLikeRender =
        let path = Path.fromInterface shape
        let metadata = RenderMetadata.create path
        let members, functions =
            shape.Members
            |> Member.partitionRender ctx scopeStore
        {
            Metadata = metadata
            TypeLikeRender.Name = shape.Name
            TypeParameters =
                shape.TypeParameters
                |> List.map (_.Value >> TypeParameter.renderFromTypePath ctx scopeStore path)
            Members =
                members
            Functions =
                functions
            Inheritance =
                shape.Heritage.Extends
                |> List.map (fun ref ->
                    Signal.compute (fun () ->
                        ref
                        |> ResolvedType.TypeReference
                        |> GeneratorContext.Prelude.getRender ctx
                        |> _.Value.TypeRef.Value
                        )
                    )
            Constructors = []
            Documentation = shape.Documentation
        }
    
module Class =
    let render (ctx: GeneratorContext) scopeStore (shape: Class) =
        let path = Path.fromClass shape
        let metadata = RenderMetadata.create path
        let members, functions =
            shape.Members
            |> Member.partitionRender ctx scopeStore
        {
            Metadata = metadata
            TypeLikeRender.Name = shape.Name
            TypeParameters =
                shape.TypeParameters
                |> List.map (_.Value >> TypeParameter.renderFromTypePath ctx scopeStore path)
            Members = members
            Functions = functions
            Inheritance =
                shape.Heritage.Implements
                |> Option.map List.singleton
                |> Option.defaultValue []
                |> List.append shape.Heritage.Extends
                |> List.map (fun ref ->
                    Signal.compute (fun () ->
                    ref
                    |> ResolvedType.TypeReference
                    |> GeneratorContext.Prelude.getRender ctx
                    |> _.Value.TypeRef.Value
                    ))
            Constructors =
                shape.Constructors
                |> List.map (
                    _.Parameters
                    >> List.map (Parameter.render ctx scopeStore)
                    )
            Documentation = []
        }

module TypeLiteral =
    let render (ctx: GeneratorContext) scopeStore (shape: TypeLiteral) =
        let path = TransientTypePath.Anchored
        let metadata = RenderMetadata.create path
        let members, functions =
            shape.Members
            |> Member.partitionRender ctx scopeStore
        {
            Metadata = metadata
            TypeLikeRender.Name = Name.Pascal.create "Object"
            TypeParameters = []
            Members = members
            Functions = functions
            Inheritance = []
            Constructors = []
            Documentation = []
        }