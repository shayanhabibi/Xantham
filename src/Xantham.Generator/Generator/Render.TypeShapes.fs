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
            // Interfaces don't have a TS `implements` clause; the
            // Implements channel is reserved for class-shaped renders.
            Implements = []
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
        let toTypeRef =
            ResolvedType.TypeReference
            >> LazyContainer.CreateFromValue
            >> ctx.PreludeGetTypeRef ctx scopeStore
        {
            Metadata = metadata
            TypeLikeRender.Name = shape.Name
            TypeParameters =
                shape.TypeParameters
                |> List.map (_.Value >> TypeParameter.render ctx scopeStore)
            Members = members
            Functions = functions
            // TS class `extends Base` → F# `inherit Base()` (single
            // class inheritance). TS class `implements I` → F#
            // `interface I with` (multi-interface implementation).
            // Keeping these separate lets the renderer emit the
            // correct shape for each (FS0946 fires on
            // `inherit X()` where X is an interface).
            Inheritance =
                shape.Heritage.Extends |> List.map toTypeRef
            Implements =
                shape.Heritage.Implements
                |> Option.map (toTypeRef >> List.singleton)
                |> Option.defaultValue []
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
