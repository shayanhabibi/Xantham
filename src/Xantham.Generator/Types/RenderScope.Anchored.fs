module Xantham.Generator.Types.Anchored

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder
open Xantham.Generator.NamePath

type AnchoredTypeRefAtom =
    | Widget of WidgetBuilder<Type> 
    | Path of TypePath 

type AnchoredTypeRefMolecule =
    | Tuple of AnchoredTypeRefRender list
    | Union of AnchoredTypeRefRender list
    | Function of parameters: AnchoredTypeRefRender list * returnType: AnchoredTypeRefRender
    | Prefix of prefix: AnchoredTypeRefRender * args: AnchoredTypeRefRender list

and AnchoredTypeRefKind =
    | Atom of AnchoredTypeRefAtom
    | Molecule of AnchoredTypeRefMolecule

and AnchoredTypeRefRender = {
    Kind: AnchoredTypeRefKind
    Nullable: bool
}

module TypeRefAtom =
    let anchor anchorPath (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.TransientPath transientTypePath ->
            TransientTypePath.anchor anchorPath transientTypePath
            |> AnchoredTypeRefAtom.Path
        | TypeRefAtom.ConcretePath path ->
            AnchoredTypeRefAtom.Path path
        | TypeRefAtom.Widget widgetBuilder ->
            Ast.LongIdent widgetBuilder
            |> AnchoredTypeRefAtom.Widget
    let localise anchorPath (atom: AnchoredTypeRefAtom) =
        match atom with
        | AnchoredTypeRefAtom.Path path ->
            Path.getRelativePath path anchorPath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
        | AnchoredTypeRefAtom.Widget widgetBuilder -> widgetBuilder

module TypeRefRender =
    let rec anchor (anchorPath: AnchorPath) (typeRefRender: Prelude.TypeRefRender) =
        {
            Kind =
                match typeRefRender.Kind with
                | TypeRefKind.Atom atom ->
                    TypeRefAtom.anchor anchorPath atom
                    |> AnchoredTypeRefKind.Atom
                | TypeRefKind.Molecule molecule ->
                    match molecule with
                    | TypeRefMolecule.Tuple typeRefRenders ->
                        typeRefRenders
                        |> List.map (anchor anchorPath)
                        |> AnchoredTypeRefMolecule.Tuple
                        |> AnchoredTypeRefKind.Molecule
                    | TypeRefMolecule.Union typeRefRenders ->
                        typeRefRenders
                        |> List.map (anchor anchorPath)
                        |> AnchoredTypeRefMolecule.Union
                        |> AnchoredTypeRefKind.Molecule
                    | TypeRefMolecule.Function(parameters, returnType) ->
                        AnchoredTypeRefMolecule.Function (
                            parameters
                            |> List.map (anchor anchorPath),
                            anchor anchorPath returnType
                            )
                        |> AnchoredTypeRefKind.Molecule
                    | TypeRefMolecule.Prefix(prefix, args) ->
                        AnchoredTypeRefMolecule.Prefix (
                            anchor anchorPath prefix,
                            args |> List.map (anchor anchorPath)
                            )
                        |> AnchoredTypeRefKind.Molecule
            Nullable = typeRefRender.Nullable
        }
    
    let rec localise (anchorPath: AnchorPath) (typeRefRender: AnchoredTypeRefRender) =
        let wrap value = { typeRefRender with Kind = value }
        match typeRefRender.Kind with
        | AnchoredTypeRefKind.Atom atom ->
            TypeRefAtom.localise anchorPath atom
            |> AnchoredTypeRefAtom.Widget
            |> AnchoredTypeRefKind.Atom
            |> wrap
        | AnchoredTypeRefKind.Molecule molecule ->
            match molecule with
            | AnchoredTypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map (localise anchorPath)
                |> AnchoredTypeRefMolecule.Tuple
                |> AnchoredTypeRefKind.Molecule
                |> wrap
            | AnchoredTypeRefMolecule.Union typeRefRenders -> 
                typeRefRenders
                |> List.map (localise anchorPath)
                |> AnchoredTypeRefMolecule.Union
                |> AnchoredTypeRefKind.Molecule
                |> wrap
            | AnchoredTypeRefMolecule.Function(parameters, returnType) ->
                AnchoredTypeRefMolecule.Function(
                    parameters
                    |> List.map (localise anchorPath),
                    localise anchorPath returnType
                    )
                |> AnchoredTypeRefKind.Molecule
                |> wrap
            | AnchoredTypeRefMolecule.Prefix(prefix, args) ->
                AnchoredTypeRefMolecule.Prefix(
                    localise anchorPath prefix,
                    args |> List.map (localise anchorPath)
                    )
                |> AnchoredTypeRefKind.Molecule
                |> wrap

module AnchoredTypeRefRender =
    let create (renderer: TypeRefRenderer) (ref: TypeRefRender) =
        TypeRefRender.anchor renderer.AnchorPath ref
        |> TypeRefRender.localise renderer.AnchorPath

module Metadata =
    let anchor (renderer: TypeRefRenderer) (metadata: RenderMetadata) =
        match metadata with
        | RenderMetadata.Concrete path -> path
        | RenderMetadata.Transient path -> TransientPath.anchor renderer.AnchorPath path

type AnchoredTypeParameter = {
    Path: AnchorPath
    Name: Name<Case.typar>
    Constraint: AnchoredTypeRefRender voption
    Default: AnchoredTypeRefRender voption
    Documentation: TsComment list
}


type AnchoredTypedName = {
    Path: AnchorPath
    Name: Name<Case.camel>
    Type: AnchoredTypeRefRender
    Traits: RenderTraits Set
    TypeParameters: AnchoredTypeParameter list
    Documentation: TsComment list
}

type AnchoredFunctionLikeSignature = {
    Path: AnchorPath
    Parameters: AnchoredTypedName list
    ReturnType: AnchoredTypeRefRender
    Traits: RenderTraits Set
    Documentation: TsComment list
    TypeParameters: AnchoredTypeParameter list
}

type AnchoredFunctionLike = {
    Path: AnchorPath
    Name: Name<Case.camel>
    Signatures: AnchoredFunctionLikeSignature list
    Traits: RenderTraits Set
    TypeParameters: AnchoredTypeParameter list
    Documentation: TsComment list
}

type AnchoredLiteralCase<'Value> = {
    Path: AnchorPath
    Name: Name<Case.pascal>
    Value: 'Value
    Documentation: TsComment list
}

type AnchoredLiteralUnion<'Value> = {
    Path: AnchorPath
    Name: Name<Case.pascal>
    Cases: AnchoredLiteralCase<'Value> list
    Documentation: TsComment list
}

type AnchoredTypeLike = {
    Path: AnchorPath
    Name: Name<Case.pascal>
    TypeParameters: AnchoredTypeParameter list
    Inheritance: AnchoredTypeRefRender list
    Members: AnchoredTypedName list
    Functions: AnchoredFunctionLike list
    Constructors: AnchoredTypedName list list
    Documentation: TsComment list
}

type AnchoredTypeAlias =
    | Alias of AnchoredTypeAliasRef
    | TypeDefn of AnchoredTypeLike
    | StringUnion of AnchoredLiteralUnion<TsLiteral>
    | EnumUnion of AnchoredLiteralUnion<int>
    | Function of AnchoredFunctionLike

and AnchoredTypeAliasRef = {
    Path: AnchorPath
    Name: Name<Case.pascal>
    TypeParameters: AnchoredTypeParameter list
    Documentation: TsComment list
    Type: AnchoredTypeRefRender
}

type AnchoredTypeRender =
    | TypeDefn of AnchoredTypeLike
    | StringUnion of AnchoredLiteralUnion<TsLiteral>
    | EnumUnion of AnchoredLiteralUnion<int>
    | Function of AnchoredFunctionLike
    | Variable of AnchoredTypedName
    | TypeAlias of AnchoredTypeAlias

type AnchoredRender =
    | RefOnly of AnchoredTypeRefRender
    | Render of AnchoredTypeRefRender * AnchoredTypeLike list

module AnchoredTypeParameter =
    let create (renderer: TypeRefRenderer) (param: TypeParameterRender) =
        {
            Path = param.Metadata |> Metadata.anchor renderer
            Name = param.Name
            Constraint =
                param.Constraint
                |> ValueOption.map (_.Value >> AnchoredTypeRefRender.create renderer)
            Default =
                param.Default
                |> ValueOption.map (_.Value >> AnchoredTypeRefRender.create renderer)
            Documentation = param.Documentation
        }

module AnchoredTypedName =
    let create (renderer: TypeRefRenderer) (typedName: TypedNameRender) =
        {
            Path = typedName.Metadata |> Metadata.anchor renderer
            Name = typedName.Name
            Type = typedName.Type.Value |> AnchoredTypeRefRender.create renderer
            Traits = typedName.Traits
            TypeParameters = typedName.TypeParameters |> List.map (AnchoredTypeParameter.create renderer)
            Documentation = typedName.Documentation
        }

module AnchoredFunctionLikeSignature =
    let create (renderer: TypeRefRenderer) (functionSignature: FunctionLikeSignature) =
        {
            Path = functionSignature.Metadata |> Metadata.anchor renderer
            Parameters =
                functionSignature.Parameters
                |> List.map (AnchoredTypedName.create renderer)
            ReturnType =
                functionSignature.ReturnType.Value
                |> AnchoredTypeRefRender.create renderer
            Traits = functionSignature.Traits
            Documentation = functionSignature.Documentation
            TypeParameters =
                functionSignature.TypeParameters
                |> List.map (AnchoredTypeParameter.create renderer)
        }

module AnchoredFunctionLike =
    let create (renderer: TypeRefRenderer) (functionLike: FunctionLikeRender) =
        {
            Path = functionLike.Metadata |> Metadata.anchor renderer
            Name = functionLike.Name
            Signatures = functionLike.Signatures |> List.map (AnchoredFunctionLikeSignature.create renderer)
            Traits = functionLike.Traits
            TypeParameters = functionLike.TypeParameters |> List.map (AnchoredTypeParameter.create renderer)
            Documentation = functionLike.Documentation
        }

module AnchoredLiteralCase =
    let create (renderer: TypeRefRenderer) (literalCase: LiteralCaseRender<'T>): AnchoredLiteralCase<'T> =
        {
            Path = literalCase.Metadata |> Metadata.anchor renderer
            Name = literalCase.Name
            Value = literalCase.Value
            Documentation = literalCase.Documentation
        }

module AnchoredLiteralUnion =
    let create (renderer: TypeRefRenderer) (literalUnion: LiteralUnionRender<'T>): AnchoredLiteralUnion<'T> =
        {
            Path = literalUnion.Metadata |> Metadata.anchor renderer
            Name = literalUnion.Name
            Cases = literalUnion.Cases |> List.map (AnchoredLiteralCase.create renderer)
            Documentation = literalUnion.Documentation
        }

module AnchoredTypeLike =
    let create (renderer: TypeRefRenderer) (typeLike: TypeLikeRender) =
        {
            Path = typeLike.Metadata |> Metadata.anchor renderer
            Name = typeLike.Name
            TypeParameters = typeLike.TypeParameters |> List.map (AnchoredTypeParameter.create renderer)
            Inheritance = typeLike.Inheritance |> List.map (_.Value >> AnchoredTypeRefRender.create renderer)
            Members = typeLike.Members |> List.map (AnchoredTypedName.create renderer)
            Functions = typeLike.Functions |> List.map (AnchoredFunctionLike.create renderer)
            Constructors = typeLike.Constructors |> List.map (List.map (AnchoredTypedName.create renderer))
            Documentation = typeLike.Documentation
        }

module AnchoredTypeAlias =
    let create (renderer: TypeRefRenderer) (typeAlias: TypeAliasRender) =
        match typeAlias with
        | TypeAliasRender.Alias alias ->
            {
                Path = alias.Metadata |> Metadata.anchor renderer
                Name = alias.Name
                TypeParameters = alias.TypeParameters |> List.map (AnchoredTypeParameter.create renderer)
                Documentation = alias.Documentation
                Type = alias.Type.Value |> AnchoredTypeRefRender.create renderer
            }
            |> Alias
        | TypeAliasRender.TypeDefn typeLikeRender ->
            AnchoredTypeLike.create renderer typeLikeRender
            |> AnchoredTypeAlias.TypeDefn
        | TypeAliasRender.StringUnion literalUnionRender ->
            AnchoredLiteralUnion.create renderer literalUnionRender
            |> AnchoredTypeAlias.StringUnion
        | TypeAliasRender.EnumUnion literalUnionRender ->
            AnchoredLiteralUnion.create renderer literalUnionRender
            |> AnchoredTypeAlias.EnumUnion
        | TypeAliasRender.Function functionLikeRender ->
            AnchoredFunctionLike.create renderer functionLikeRender
            |> AnchoredTypeAlias.Function

module AnchoredTypeRender =
    let create (renderer: TypeRefRenderer) (typeRender: TypeRender) =
        match typeRender with
        | TypeRender.TypeDefn typeLikeRender -> AnchoredTypeLike.create renderer typeLikeRender |> TypeDefn
        | TypeRender.TypeAlias typeAliasRender -> AnchoredTypeAlias.create renderer typeAliasRender |> TypeAlias
        | TypeRender.StringUnion literalUnionRender -> AnchoredLiteralUnion.create renderer literalUnionRender |> StringUnion
        | TypeRender.EnumUnion literalUnionRender -> AnchoredLiteralUnion.create renderer literalUnionRender |> EnumUnion
        | TypeRender.Function functionLikeRender -> AnchoredFunctionLike.create renderer functionLikeRender |> Function
        | TypeRender.Variable typedNameRender -> AnchoredTypedName.create renderer typedNameRender |> Variable