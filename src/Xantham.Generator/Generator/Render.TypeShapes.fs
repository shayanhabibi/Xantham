[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder.Case
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Prelude

module Interface =
    let render (ctx: GeneratorContext) scopeStore (shape: Interface) =
        let path = Path.fromInterface shape |> Path.create
        let transientPathCtx = TransientTypePath.AnchoredAndMoored shape.Name |> ValueSome
        let metadata = {
            Path = Path.create path
            Original = Path.create path
            Source = shape.Source |> Option.toValueOption
            FullyQualifiedName = ValueSome shape.FullyQualifiedName
        }
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore transientPathCtx)
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
                |> List.map (_.Value >> TypeParameter.render ctx scopeStore (ValueSome (TransientModulePath.AnchoredAndMoored shape.Name)))
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

module Class =
    let render (ctx: GeneratorContext) scopeStore (shape: Class) =
        let path = Path.fromClass shape |> Path.create
        let metadata = {
            Path = path
            Original = path
            Source = shape.Source |> Option.toValueOption
            FullyQualifiedName = ValueSome shape.FullyQualifiedName 
        }
        let transientPathCtx = TransientTypePath.AnchoredAndMoored shape.Name |> ValueSome
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore transientPathCtx)
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
                |> List.map (_.Value >> TypeParameter.render ctx scopeStore (ValueSome (TransientModulePath.AnchoredAndMoored shape.Name)))
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
                let transientPathCtx =
                    transientPathCtx
                    |> ValueOption.map (TransientMemberPath.createOnTransientType "Create")
                shape.Constructors
                |> List.map (
                    _.Parameters
                    >> List.map (Parameter.render ctx scopeStore transientPathCtx)
                    )
            Documentation = []
        }

module TypeLiteral =
    let prerender (ctx: GeneratorContext) scopeStore (transientPathCtx: TransientModulePath voption) (shape: TypeLiteral) =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore path)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        members, functions