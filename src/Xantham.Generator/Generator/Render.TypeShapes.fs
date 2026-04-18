[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Prelude

module Interface =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (shape: Interface) metadata =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore)
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
                |> List.map (_.Value >> TypeParameter.render ctx scopeStore)
            Members =
                members
            Functions =
                functions
            Inheritance =
                shape.Heritage.Extends
                |> List.map (
                    ResolvedType.TypeReference
                    >> LazyContainer.CreateFromValue
                    >> ctx.PreludeGetTypeRef ctx scopeStore
                    )
            Constructors = []
            Documentation = shape.Documentation
        }
    let render (ctx: GeneratorContext) scopeStore (shape: Interface) =
        { Path = Path.fromInterface shape |> Path.create }
        |> renderWithMetadata ctx scopeStore shape 
    
module Class =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (shape: Class) metadata =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore)
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
                |> List.map (_.Value >> TypeParameter.render ctx scopeStore)
            Members = members
            Functions = functions
            Inheritance =
                shape.Heritage.Implements
                |> Option.map List.singleton
                |> Option.defaultValue []
                |> List.append shape.Heritage.Extends
                |> List.map (
                    ResolvedType.TypeReference
                    >> LazyContainer.CreateFromValue
                    >> ctx.PreludeGetTypeRef ctx scopeStore
                    )
            Constructors =
                shape.Constructors
                |> List.map (
                    _.Parameters
                    >> List.map (Parameter.render ctx scopeStore)
                    )
            Documentation = []
        }
    let render (ctx: GeneratorContext) scopeStore (shape: Class) =
        { Path = Path.fromClass shape |> Path.create }
        |> renderWithMetadata ctx scopeStore shape 

module TypeLiteral =
    let prerender (ctx: GeneratorContext) scopeStore (shape: TypeLiteral) =
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        members, functions
    let renderWithName (ctx: GeneratorContext) scopeStore (shape: TypeLiteral) =
        let members, functions = prerender ctx scopeStore shape
        fun name ->
            {
                Metadata = { Path = Path.create TransientMemberPath.Anchored }
                TypeLikeRender.Name = name
                TypeParameters = []
                Members = members
                Functions = functions
                Inheritance = []
                Constructors = []
                Documentation = []
            }
    let renderWithNameString (ctx: GeneratorContext) scopeStore (shape: TypeLiteral) (name: string) =
        renderWithName ctx scopeStore shape (Name.Pascal.create name)