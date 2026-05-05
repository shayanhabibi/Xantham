[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Prelude

module Interface =
    let render (ctx: GeneratorContext) scopeStore (shape: Interface) =
        let path = Interceptors.pipeInterface ctx shape |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path shape
        let members, functions =
            shape.Members
            |> Seq.collect (Member.render ctx scopeStore)
            |> Seq.fold (fun (members, functions) m ->
                match m with
                | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
                | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
                ) ([], [])
        let inheritanceRefs =
            shape.Heritage.Extends
            |> List.map (
                ResolvedType.TypeReference
                >> LazyContainer.CreateFromValue
                >> ctx.PreludeGetTypeRef ctx scopeStore
                )
        // F# rejects `inherit Y` inside an `interface ... end` body when
        // Y is a class. If any heritage target resolves to a class-shaped
        // F# type, force class-shape rendering for this binding too via
        // IsClass=true (which dispatches to renderAbstractClass and emits
        // `inherit Y()`). Detection: source is a TS class (P4+P5), or the
        // rendered TypeRef is a class-shaped intrinsic — `exn` (from
        // driver-level lib.es Error → System.Exception substitution) or
        // `obj` (cycle-break artifact).
        let inheritsClass =
            (shape.Heritage.Extends |> List.exists (fun tr ->
                match tr.Type.Value with
                | ResolvedType.Class _ -> true
                | _ -> false))
            ||
            (inheritanceRefs |> List.exists (fun r ->
                match r.Kind with
                | TypeRefKind.Atom atom ->
                    match atom with
                    | TypeRefAtom.Intrinsic ("exn" | "obj") -> true
                    | _ -> false
                | _ -> false))
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
            Inheritance = inheritanceRefs
            Constructors = []
            Documentation = shape.Documentation
            IsClass = inheritsClass
        }

module Class =
    let render (ctx: GeneratorContext) scopeStore (shape: Class) =
        let path = Interceptors.pipeClass ctx shape |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path shape
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
            IsClass = true
        }

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
