module Xantham.Generator.Types.Anchored

open System.Collections.Generic
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath

type TypeRefAtom =
    | Widget of WidgetBuilder<Type> 
    | Path of TypePath 

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
    let localise anchorPath (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Path path ->
            Path.getRelativePath path anchorPath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
        | TypeRefAtom.Widget widgetBuilder -> widgetBuilder

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
    let inline setNullable nullable typeRefRender = { typeRefRender with TypeRefRender.Nullable = nullable }
    let inline orNullable nullable typeRefRender = { typeRefRender with TypeRefRender.Nullable = nullable || typeRefRender.Nullable }
    let nullable typeRefRender = setNullable true typeRefRender
    let nonNullable typeRefRender = setNullable false typeRefRender


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