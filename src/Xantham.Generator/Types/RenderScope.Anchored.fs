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

    // An F# interface can only inherit other interfaces. On an ANCHORED (post-anchor,
    // pre-localise) render, an interface base is a `Path` atom or a generic `Prefix`
    // molecule; a substituted scalar (`obj`/`exn`/`seq` via the lib.es heritage map, or a
    // primitive `Intrinsic`/bare `Widget`) is NOT a valid inherit base. Must be checked
    // BEFORE localise, which collapses every atom to a `Widget` and erases this distinction.
    let isInterfaceBase (render: TypeRefRender) =
        match render.Kind with
        | TypeRefKind.Atom (TypeRefAtom.Path _) -> true
        | TypeRefKind.Molecule (TypeRefMolecule.Prefix _) -> true
        | _ -> false

    /// A base whose path was rewritten to the `Erased.*` advisory aliases (the
    /// policy substitution's output contract) cannot be inherited — the alias IS
    /// `obj`. Checked at anchor time like `isInterfaceBase` (post-localise atoms
    /// are opaque Widgets); every drop is ledgered by the caller.
    let isErasedBase (render: TypeRefRender) =
        let rootOf (path: TypePath) =
            ModulePath.flatten path.Parent
            |> List.tryHead
            |> Option.map Name.Case.valueOrModified
        match render.Kind with
        | TypeRefKind.Atom (TypeRefAtom.Path p) -> rootOf p = Some "Erased"
        | TypeRefKind.Molecule (TypeRefMolecule.Prefix ({ Kind = TypeRefKind.Atom (TypeRefAtom.Path p) }, _)) -> rootOf p = Some "Erased"
        | _ -> false

    /// Map every ATOM in a render, recursing through all molecule shapes — the
    /// ONE walk the scrubbers share (no mirrored walks).
    let mapAtoms (f: TypeRefAtom -> TypeRefAtom) (render: TypeRefRender) : TypeRefRender =
        let rec walk (render: TypeRefRender) : TypeRefRender =
            match render.Kind with
            | TypeRefKind.Atom atom -> { render with Kind = TypeRefKind.Atom(f atom) }
            | TypeRefKind.Molecule molecule ->
                let newMolecule =
                    match molecule with
                    | TypeRefMolecule.Tuple typeRefs -> typeRefs |> List.map walk |> TypeRefMolecule.Tuple
                    | TypeRefMolecule.Union typeRefs -> typeRefs |> List.map walk |> TypeRefMolecule.Union
                    | TypeRefMolecule.Function(parameters, returnType) ->
                        TypeRefMolecule.Function(parameters |> List.map walk, walk returnType)
                    | TypeRefMolecule.Prefix(prefix, args) ->
                        TypeRefMolecule.Prefix(walk prefix, args |> List.map walk)
                { render with Kind = TypeRefKind.Molecule newMolecule }
        walk render

    let substituteForHeritage (inScopeTyparNames: Set<string>) (render: TypeRefRender) : TypeRefRender =
        render
        |> mapAtoms (fun atom ->
            match atom with
            | TypeRefAtom.Intrinsic "_" -> TypeRefAtom.Intrinsic "obj"
            | TypeRefAtom.Intrinsic s when s.StartsWith "'" ->
                if Set.contains s inScopeTyparNames then
                    atom
                else
                    // NOTE: stderr, not stdout — the generator writes the emitted F# to stdout,
                    // so a `printfn` here would corrupt the output stream.
                    eprintfn "Warning: orphan type parameter %s (not in scope), substituting with 'obj'" s
                    TypeRefAtom.Intrinsic "obj"
            | _ -> atom)

    /// OPAQUE/ERASED PREFIX COLLAPSE: generic applications whose HEAD is an
    /// opaque-handle or Erased advisory path carry policy-meaningless arguments
    /// (`Zod.ZodType<...garbage...>`, `Erased.X<...>`) — and malformed arg trees
    /// (`obj<...>`) break compilation. Collapse the whole application to the head
    /// atom; the phantom-arity interfaces make surviving shallow applications legal,
    /// this makes DEEP/broken ones vanish. Roots = ErasedRoots + "Erased" + handles.
    let collapseOpaquePrefixes (collapseRoots: string list) (render: TypeRefRender) : TypeRefRender =
        let isCollapsible (head: TypeRefRender) =
            match head.Kind with
            // Args applied to `obj` (scrub residue) are NEVER valid F# — always collapse.
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> true
            | TypeRefKind.Atom (TypeRefAtom.Path p) ->
                match ModulePath.flatten p.Parent |> List.tryHead |> Option.map Name.Case.valueOrModified with
                | Some root -> root = "Erased" || collapseRoots |> List.contains root
                | None -> false
            | _ -> false
        let rec walk (render: TypeRefRender) : TypeRefRender =
            match render.Kind with
            | TypeRefKind.Atom _ -> render
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix(head, _)) when isCollapsible head ->
                // Collapse the whole application to the bare head atom.
                { render with Kind = head.Kind }
            | TypeRefKind.Molecule molecule ->
                let newMolecule =
                    match molecule with
                    | TypeRefMolecule.Tuple rs -> TypeRefMolecule.Tuple(rs |> List.map walk)
                    | TypeRefMolecule.Union rs -> TypeRefMolecule.Union(rs |> List.map walk)
                    | TypeRefMolecule.Function(ps, r) -> TypeRefMolecule.Function(ps |> List.map walk, walk r)
                    | TypeRefMolecule.Prefix(head, args) -> TypeRefMolecule.Prefix(walk head, args |> List.map walk)
                { render with Kind = TypeRefKind.Molecule newMolecule }
        walk render

    /// FORWARD-REFERENCE SCRUB: under a placement order (unit DAG), a reference
    /// from a definition hosted at unit index H to a type rooted at index R > H
    /// points FORWARD in the DAG — uncompilable by construction. Degrade the atom
    /// to `obj`, LEDGERED via the callback (Types layer stays context-free).
    let scrubForwardRefs (order: string list) (ledger: string -> unit) (hostRoot: string option) (render: TypeRefRender) : TypeRefRender =
        match order, hostRoot with
        | [], _
        | _, None -> render
        | order, Some host ->
            let idxOf r = order |> List.tryFindIndex ((=) r)
            let hostIdx = idxOf host |> Option.defaultValue 0
            render
            |> mapAtoms (fun atom ->
                match atom with
                | TypeRefAtom.Path p ->
                    let refRoot =
                        ModulePath.flatten p.Parent
                        |> List.tryHead
                        |> Option.map Name.Case.valueOrModified
                    match refRoot |> Option.bind idxOf with
                    | Some r when r > hostIdx ->
                        ledger (refRoot |> Option.defaultValue "?")
                        TypeRefAtom.Intrinsic "obj"
                    | _ -> atom
                | _ -> atom)


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
    Anchors: Dictionary<ResolvedType, TypePath * Render>
}

type AnchorScopeStore = Dictionary<ResolvedType, RenderScope>

// to create an anchored render scope, you need a concrete root, and the transient scope store.
// You then expand the store recursively, using the concrete path as the anchor.