module Xantham.Generator.Types.Anchored

open System.Collections.Generic
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath

[<CustomEquality; NoComparison>]
type TypeRefAtom =
    | Widget of WidgetBuilder<Type>
    | Intrinsic of string
    | Path of TypePath
    override this.Equals(other) =
        match other, this with
        | :? TypeRefAtom as (TypeRefAtom.Widget otherWidget), TypeRefAtom.Widget thisWidget ->
            otherWidget.Compile() = thisWidget.Compile()
        | :? TypeRefAtom as (TypeRefAtom.Path otherPath), TypeRefAtom.Path thisPath ->
            otherPath = thisPath
        | :? TypeRefAtom as (TypeRefAtom.Intrinsic otherIntrinsic), TypeRefAtom.Intrinsic thisIntrinsic ->
            otherIntrinsic = thisIntrinsic
        | _ -> false
    override this.GetHashCode() =
        match this with
        | TypeRefAtom.Widget widget ->
            widget.Compile().GetHashCode()
        | TypeRefAtom.Path path ->
            path.GetHashCode()
        | Intrinsic s -> s.GetHashCode()

type TypeRefMolecule =
    | Tuple of TypeRefRender list
    | Union of TypeRefRender list
    | Function of parameters: TypeRefRender list * returnType: TypeRefRender
    | Prefix of prefix: TypeRefRender * args: TypeRefRender list

and TypeRefKind =
    | Atom of TypeRefAtom
    | Molecule of TypeRefMolecule 

and TypeRefRender = {
    Kind: TypeRefKind
    Nullable: bool
}

module TypeRefAtom =
    let anchor anchorPath (atom: Prelude.TypeRefAtom) =
        match atom with
        | TypeRefAtom.TransientPath transientTypePath ->
            TransientTypePath.anchor anchorPath transientTypePath
            |> TypeRefAtom.Path
        | TypeRefAtom.ConcretePath path ->
            TypeRefAtom.Path path
        | Prelude.TypeRefAtom.Widget widgetBuilder ->
            TypeRefAtom.Widget widgetBuilder
        | Prelude.TypeRefAtom.Intrinsic intrinsic ->
            TypeRefAtom.Intrinsic intrinsic
    let localise anchorPath (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Path path ->
            Path.getRelativePath path anchorPath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
        | TypeRefAtom.Widget widgetBuilder -> widgetBuilder
        | Intrinsic s -> Ast.LongIdent s

module TypeRefRender =
    type SRTPHelper =
        static member inline Create(nullable: bool, kind: TypePath) = { Kind = TypeRefKind.Atom(TypeRefAtom.Path kind); Nullable = nullable }
        static member inline Create(nullable: bool, kind: WidgetBuilder<Type>) = { Kind = TypeRefKind.Atom(TypeRefAtom.Widget kind); Nullable = nullable }
    let inline create nullable kind = ((^T or SRTPHelper):(static member Create: bool * ^T -> TypeRefRender) (nullable, kind))
    let rec anchor (anchorPath: AnchorPath) (typeRefRender: Prelude.TypeRefRender) =
        {
            Kind =
                match typeRefRender.Kind with
                | Prelude.TypeRefKind.Atom atom ->
                    TypeRefAtom.anchor anchorPath atom
                    |> TypeRefKind.Atom
                | Prelude.TypeRefKind.Molecule molecule ->
                    match molecule with
                    | Prelude.TypeRefMolecule.Tuple typeRefRenders ->
                        typeRefRenders
                        |> List.map (anchor anchorPath)
                        |> TypeRefMolecule.Tuple
                        |> TypeRefKind.Molecule
                    | Prelude.TypeRefMolecule.Union typeRefRenders ->
                        typeRefRenders
                        |> List.map (anchor anchorPath)
                        |> TypeRefMolecule.Union
                        |> TypeRefKind.Molecule
                    | Prelude.TypeRefMolecule.Function(parameters, returnType) ->
                        TypeRefMolecule.Function (
                            parameters
                            |> List.map (anchor anchorPath),
                            anchor anchorPath returnType
                            )
                        |> TypeRefKind.Molecule
                    | Prelude.TypeRefMolecule.Prefix(prefix, args) ->
                        TypeRefMolecule.Prefix (
                            anchor anchorPath prefix,
                            args |> List.map (anchor anchorPath)
                            )
                        |> TypeRefKind.Molecule
            Nullable = typeRefRender.Nullable
        }
    
    let rec localise (anchorPath: AnchorPath) (typeRefRender: TypeRefRender) =
        let wrap value = { typeRefRender with Kind = value }
        match typeRefRender.Kind with
        | TypeRefKind.Atom atom ->
            TypeRefAtom.localise anchorPath atom
            |> TypeRefAtom.Widget
            |> TypeRefKind.Atom
            |> wrap
        | TypeRefKind.Molecule molecule ->
            match molecule with
            | TypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map (localise anchorPath)
                |> TypeRefMolecule.Tuple
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Union typeRefRenders -> 
                typeRefRenders
                |> List.map (localise anchorPath)
                |> TypeRefMolecule.Union
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Function(parameters, returnType) ->
                TypeRefMolecule.Function(
                    parameters
                    |> List.map (localise anchorPath),
                    localise anchorPath returnType
                    )
                |> TypeRefKind.Molecule
                |> wrap
            | TypeRefMolecule.Prefix(prefix, args) ->
                TypeRefMolecule.Prefix(
                    localise anchorPath prefix,
                    args |> List.map (localise anchorPath)
                    )
                |> TypeRefKind.Molecule
                |> wrap
    let anchorAndLocalise anchorPath (typeRefRender: Prelude.TypeRefRender) =
        anchor anchorPath typeRefRender
        |> localise anchorPath
    let inline setNullable nullable typeRefRender = { typeRefRender with TypeRefRender.Nullable = nullable }
    let inline orNullable nullable typeRefRender = { typeRefRender with TypeRefRender.Nullable = nullable || typeRefRender.Nullable }
    let nullable typeRefRender = setNullable true typeRefRender
    let nonNullable typeRefRender = setNullable false typeRefRender

    let substituteForHeritage (inScopeTyparNames: Set<string>) (render: TypeRefRender) : TypeRefRender =
        let rec walk (render: TypeRefRender) : TypeRefRender =
            match render.Kind with
            | TypeRefKind.Atom atom ->
                let newAtom =
                    match atom with
                    | TypeRefAtom.Intrinsic "_" ->
                        TypeRefAtom.Intrinsic "obj"
                    | TypeRefAtom.Intrinsic s when s.StartsWith("'") ->
                        if Set.contains s inScopeTyparNames then
                            atom
                        else
                            printfn "Warning: orphan type parameter '%s' in heritage clause, substituting with 'obj'" s
                            TypeRefAtom.Intrinsic "obj"
                    | _ -> atom
                { render with Kind = TypeRefKind.Atom newAtom }
            | TypeRefKind.Molecule molecule ->
                let newMolecule =
                    match molecule with
                    | TypeRefMolecule.Tuple typeRefs ->
                        typeRefs
                        |> List.map walk
                        |> TypeRefMolecule.Tuple
                    | TypeRefMolecule.Union typeRefs ->
                        typeRefs
                        |> List.map walk
                        |> TypeRefMolecule.Union
                    | TypeRefMolecule.Function(parameters, returnType) ->
                        TypeRefMolecule.Function(
                            parameters |> List.map walk,
                            walk returnType
                        )
                    | TypeRefMolecule.Prefix(prefix, args) ->
                        TypeRefMolecule.Prefix(
                            walk prefix,
                            args |> List.map walk
                        )
                let newRender = { render with Kind = TypeRefKind.Molecule newMolecule }
                // Cycle-break and other substitutions can leave a non-
                // generic intrinsic (`obj`, `exn`) at the head of a
                // `Prefix(intrinsic, args)`. F# rejects type-args on
                // these (FS0033). Collapse the prefix to its head atom
                // when the head is a non-generic intrinsic. Applied
                // after `walk` so any nested substitutions have run.
                match newRender.Kind with
                | TypeRefKind.Molecule (TypeRefMolecule.Prefix (head, _)) ->
                    match head.Kind with
                    | TypeRefKind.Atom (TypeRefAtom.Intrinsic ("obj" | "exn")) ->
                        { newRender with Kind = head.Kind }
                    | _ -> newRender
                | _ -> newRender
        walk render


type TypeName = Name<Case.pascal>
type MemberName = Name<Case.camel>
type TyparName = Name<Case.typar>
type TypeParameterRender = TypeParameterRender<TypeRefRender, TyparName>
type TypedNameRender = TypedNameRender<TypeRefRender, MemberName, TyparName>
type FunctionLikeSignature = FunctionLikeSignature<TypeRefRender, MemberName, TyparName>
type FunctionLikeRender = FunctionLikeRender<TypeRefRender, MemberName, TyparName>
type LiteralCaseRender<'T> = LiteralCaseRender<'T, TypeName>
type LiteralUnionRender<'T> = LiteralUnionRender<'T, TypeName>
type TypeLikeRender = TypeLikeRender<TypeRefRender, TypeName, MemberName, TyparName>
type TypeAliasRender = TypeAliasRender<TypeRefRender, TypeName, MemberName, TyparName>
type TypeAliasRenderRef = TypeAliasRenderRef<TypeRefRender, TypeName, TyparName>
type TypeRender = TypeRender<TypeRefRender, TypeName, MemberName, TyparName>
type MemberRender = MemberRender<TypeRefRender, MemberName, TyparName>
type Render = RenderKind<TypeRefRender, TypeName, MemberName, TyparName>

type RenderScope = {
    Type: ResolvedType
    Root: Choice<TypePath, MemberPath>
    TypeRef: TypeRefRender
    Render: Render
    /// Anchored emission entries for the inner types reachable from this
    /// export. Keyed by the final anchored `TypePath` (the emission
    /// location), valued by the anchored render.
    ///
    /// Previously keyed by `ResolvedType`, which prevented the same
    /// inner literal — shared across multiple parents within one export
    /// via reference-equality interning — from emitting at more than one
    /// location. References from the second-and-later parents resolved
    /// to a `<parent>.<X>` path that no body was emitted at, producing
    /// the multi-position-literal cascade documented in
    /// `docs/plans/post-pr2-progress.md`.
    ///
    /// Path-keyed allows the same `ResolvedType` to anchor at multiple
    /// distinct paths (one per parent that references it within the
    /// export), each emitting its own copy. `tryAdd` semantics still
    /// dedup the *path* itself (so the same final location can't be
    /// claimed twice). Cycle prevention moves to a separate `visited`
    /// `HashSet<ResolvedType>` tracked in the recursion.
    Anchors: Dictionary<TypePath, Render>
}

type AnchorScopeStore = Dictionary<ResolvedType, RenderScope>

// to create an anchored render scope, you need a concrete root, and the transient scope store.
// You then expand the store recursively, using the concrete path as the anchor.