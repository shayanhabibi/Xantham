[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.TypeRenders

module Interface =
    let renderWithMetadata (ctx: GeneratorContext) (shape: Interface) metadata =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        {
            Metadata = metadata
            TypeLikeRender.Name = shape.Name
            TypeParameters =
                shape.TypeParameters
                |> List.map (_.Value >> TypeParameter.render ctx)
                |> List.toArray
            Members =
                members
                |> List.toArray
            Functions =
                functions
                |> List.toArray
            Inheritance =
                shape.Heritage.Extends
                |> List.map (ResolvedType.TypeReference >> ctx.render)
                |> List.toArray
            Constructors = [||]
            Documentation = shape.Documentation
        }
    let render (ctx: GeneratorContext) (shape: Interface) =
        Path.fromInterface shape
        |> RenderMetadata.create
        |> renderWithMetadata ctx shape 
    
    let inline renderWithPath (ctx: GeneratorContext) (shape: Interface) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx shape

module Class =
    let renderWithMetadata (ctx: GeneratorContext) (shape: Class) metadata =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        {
            Metadata = metadata
            TypeLikeRender.Name = shape.Name
            TypeParameters =
                shape.TypeParameters
                |> List.map (_.Value >> TypeParameter.render ctx)
                |> List.toArray
            Members = List.toArray members
            Functions = List.toArray functions
            Inheritance =
                shape.Heritage.Implements
                |> Option.map List.singleton
                |> Option.defaultValue []
                |> List.append shape.Heritage.Extends
                |> List.map (ResolvedType.TypeReference >> ctx.render)
                |> List.toArray
            Constructors =
                shape.Constructors
                |> List.map (_.Parameters >> List.map (Parameter.render ctx) >> List.toArray)
                |> List.toArray
            Documentation = []
        }
    let render (ctx: GeneratorContext) (shape: Class) =
        Path.fromClass shape
        |> RenderMetadata.create
        |> renderWithMetadata ctx shape 
    let inline renderWithPath (ctx: GeneratorContext) (shape: Class) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx shape

module TypeLiteral =
    let prerender (ctx: GeneratorContext) (shape: TypeLiteral) =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        members, functions
    let renderWithName (ctx: GeneratorContext) (shape: TypeLiteral) =
        let members, functions = prerender ctx shape
        fun name ->
            {
                Metadata = RenderMetadata.empty
                TypeLikeRender.Name = name
                TypeParameters = [||]
                Members = List.toArray members
                Functions = List.toArray functions
                Inheritance = [||]
                Constructors = [||]
                Documentation = []
            }
    let renderWithNameString (ctx: GeneratorContext) (shape: TypeLiteral) (name: string) =
        renderWithName ctx shape (Name.Pascal.create name)
    let renderWithPath (ctx: GeneratorContext) (shape: TypeLiteral) =
        let members, functions = prerender ctx shape
        let path = TransientTypePath.Anchored
        fun concretePath ->
            let typePath = TransientTypePath.anchor concretePath path
            {
                Metadata = RenderMetadata.empty
                TypeLikeRender.Name = typePath.Name
                TypeParameters = [||]
                Members = List.toArray members
                Functions = List.toArray functions
                Inheritance = [||]
                Constructors = [||]
                Documentation = []
            }
            