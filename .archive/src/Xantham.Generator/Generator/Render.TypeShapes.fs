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
    /// Decide the heritage-target's *declared* arity. For Interface/Class
    /// targets it's `TypeParameters.Length`. For everything else (an
    /// alias body's resolved form, a cycle-broken intrinsic, a synthetic)
    /// we use `ValueNone` — meaning "leave the args alone, let downstream
    /// handle it."
    let rec private resolvedKindArity (ctx: GeneratorContext) (rt: ResolvedType) : int voption =
        match rt with
        | ResolvedType.Interface iface -> ValueSome iface.TypeParameters.Length
        | ResolvedType.Class cls -> ValueSome cls.TypeParameters.Length
        | ResolvedType.TypeReference tr ->
            // Type alias references: consult ctx.TypeAliasArity (populated
            // by prerenderTypeAliases). The alias's declaration TypeKey
            // and body TypeKey are both registered there. If found, that's
            // the authoritative declared arity for the alias.
            match ctx.TypeAliasArity.TryGetValue tr.Type.Raw with
            | true, arity -> ValueSome arity
            | _ ->
                // Nested TypeReference without alias entry — walk through
                // to find named Interface/Class.
                match tr.ResolvedType with
                | Some innerLazy -> resolvedKindArity ctx innerLazy.Value
                | None -> resolvedKindArity ctx tr.Type.Value
        | _ -> ValueNone
    let heritageTargetArity (ctx: GeneratorContext) (typeRef: TypeReference) =
        resolvedKindArity ctx (ResolvedType.TypeReference typeRef)
    /// After rendering a heritage `TypeReference`, reconcile the rendered
    /// molecule's arg-count with the parent's declared arity. F# rejects
    /// `interface NonGenericParent<T>` and `inherit NonGenericParent<T>()`
    /// (FS0033). The existing arity reconciler in
    /// `RenderScope.Prelude.fs` lines ~481-494 handles non-heritage
    /// TypeReferences; heritage refs bypass it because they're consumed
    /// by `TypeAliasRemap`'s final-pass molecule preservation before the
    /// reconciler sees them. Cutting here is the catch-all for heritage.
    let reconcileHeritageArity (ctx: GeneratorContext) (typeRef: TypeReference) (rendered: TypeRefRender) =
        match heritageTargetArity ctx typeRef with
        | ValueSome 0 ->
            match rendered.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix (prefixRef, _)) ->
                prefixRef |> TypeRefRender.orNullable rendered.Nullable
            | _ -> rendered
        | _ -> rendered
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
            |> List.map (fun typeRef ->
                typeRef
                |> ResolvedType.TypeReference
                |> LazyContainer.CreateFromValue
                |> ctx.PreludeGetTypeRef ctx scopeStore
                |> reconcileHeritageArity ctx typeRef)
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
        let toTypeRef (typeRef: TypeReference) =
            typeRef
            |> ResolvedType.TypeReference
            |> LazyContainer.CreateFromValue
            |> ctx.PreludeGetTypeRef ctx scopeStore
            |> Interface.reconcileHeritageArity ctx typeRef
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
