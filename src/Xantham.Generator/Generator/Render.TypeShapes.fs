[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeShapes

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Prelude

// A heritage base (`extends`/`implements`) is emitted as an F# `inherit` clause. F# can only
// inherit a NOMINAL named type — never `option<...>` (a nullable/erased base), never `obj` or a
// bogus `obj<'A,'B>` (an unresolved base that fell back to `obj` but kept its type args), never a
// bare intrinsic. Such a base breaks the parser (`inherit option<obj<'Env,'Props>>`, FS0010, which
// cascades). Like the sealed type-parameter constraint fix, an un-inheritable base is NOT load-
// bearing for erased Fable bindings — drop it. Keep only a base whose head atom is a named path
// (ConcretePath/TransientPath), not nullable, not an intrinsic-headed molecule.
let private isInheritableBase (ref: Prelude.TypeRefRender) : bool =
    if ref.Nullable then false
    else
        let headAtomIsNominal (atom: Prelude.TypeRefAtom) =
            match atom with
            | Prelude.TypeRefAtom.ConcretePath _ | Prelude.TypeRefAtom.TransientPath _ -> true
            | Prelude.TypeRefAtom.Intrinsic _ | Prelude.TypeRefAtom.Widget _ -> false
        match ref.Kind with
        | Prelude.TypeRefKind.Atom atom -> headAtomIsNominal atom
        | Prelude.TypeRefKind.Molecule (Prelude.TypeRefMolecule.Prefix (head, _)) ->
            match head.Kind with
            | Prelude.TypeRefKind.Atom atom -> headAtomIsNominal atom
            | _ -> false
        // Tuple / Union / Function bases are never valid F# inherit targets.
        | Prelude.TypeRefKind.Molecule _ -> false

module Interface =
    let render (ctx: GeneratorContext) scopeStore (shape: Interface) =
        let path = Interceptors.pipeInterface ctx shape |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path shape
        let members, functions =
            // The ONE member partition (Member.partitionRender) — it owns the
            // duplicate-property dedup; a mirrored inline fold here silently skips it.
            shape.Members
            |> Member.partitionRender ctx scopeStore
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
                |> List.filter isInheritableBase
            Constructors = []
            Documentation = shape.Documentation
        }
    
module Class =
    let render (ctx: GeneratorContext) scopeStore (shape: Class) =
        let path = Interceptors.pipeClass ctx shape |> Path.create
        let metadata = RenderMetadata.createWithPathFromExport path shape
        let members, functions =
            // Same single-seam rule as Interface.render above.
            shape.Members
            |> Member.partitionRender ctx scopeStore
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
                |> List.filter isInheritableBase
            Constructors =
                shape.Constructors
                |> List.map (
                    _.Parameters
                    >> List.map (Parameter.render ctx scopeStore)
                    )
            Documentation = []
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
