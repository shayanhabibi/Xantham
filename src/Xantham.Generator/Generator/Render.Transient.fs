[<AutoOpen>]
module Xantham.Generator.Generator.Render_Transient

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module Union =
    let private renderUnionLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase enumCase ->
            let path =
                Case.unboxMeasure enumCase.Name
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                LiteralCaseRender.Metadata = {
                    Path = path
                    Original = path
                    Source = enumCase.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome enumCase.FullyQualifiedName
                }
                Name = enumCase.Name |> ValueSome
                Value = enumCase.Value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal tsLiteral ->
            let path = TransientMemberPath.Anchored |> Path.create
            {
                LiteralCaseRender.Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name =
                    match tsLiteral with
                    | TsLiteral.String value -> value
                    | TsLiteral.Int value -> string value
                    | TsLiteral.Float value -> string value
                    | TsLiteral.Bool value -> string value
                    | TsLiteral.BigInt value -> string value
                    | TsLiteral.Null -> "null"
                    |> Name.Pascal.create
                    |> ValueSome
                Value = tsLiteral
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery typeQuery ->
            let value =
                match typeQuery.Type.Value with
                | ResolvedType.Literal tsLiteral -> tsLiteral
                | _ -> failwith "Cannot render union literal case for non literal type"
            let name =
                typeQuery.FullyQualifiedName
                |> List.last
                |> (_.Value >> Name.Pascal.create)
            let path =
                name
                |> Case.unboxMeasure
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueSome typeQuery.FullyQualifiedName
                }
                Name = ValueSome name
                Value = value
                Documentation = []
            }

    let private renderUnionLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = { Path = Path.create TransientTypePath.Anchored
                         Original = Path.create TransientTypePath.Anchored
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Prelude.Transient.LiteralUnionRender.Name = ValueNone
            Cases =
                literals
                |> List.map (renderUnionLiteralCase ctx scopeStore)
            Documentation = []
        }
    let private renderEnumLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
            let path =
                Case.unboxMeasure enumCase.Name
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata = { Path = path
                             Original = path
                             Source = enumCase.Source |> Option.toValueOption
                             FullyQualifiedName = ValueSome enumCase.FullyQualifiedName }
                Name = enumCase.Name |> ValueSome
                Value = value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Int value) ->
            let path = TransientMemberPath.Anchored |> Path.create
            {
                Metadata = { Path = path; Original = path
                             Source = ValueNone; FullyQualifiedName = ValueNone }
                Name = string value |> Name.Pascal.create |> ValueSome
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery { FullyQualifiedName = fqn; Type = Resolve (ResolvedType.Literal (TsLiteral.Int value))  } ->
            let name =
                fqn
                |> List.last
                |> _.Value
                |> Name.Pascal.create
            let path =
                name
                |> Case.unboxMeasure
                |> TransientMemberPath.AnchoredAndMoored
                |> Path.create
            {
                Metadata =
                    RenderMetadata.createWithPath path
                    |> RenderMetadata.withFullyQualifiedName fqn
                Name = ValueSome name
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.TypeQuery _
        | ResolvedTypeLiteralLike.EnumCase _ 
        | ResolvedTypeLiteralLike.Literal _ -> failwith "Cannot render enum literal case for non int literal. This should be guarded against"

    let private renderEnumLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        let path = TransientTypePath.Anchored |> Path.create
        {
            Metadata = { Path = path; Original = path
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Name = ValueNone
            Cases =
                literals
                |> List.map (renderEnumLiteralCase ctx scopeStore)
            Documentation = []
        }
    
    let renderLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        if
            literals
            |> List.forall (function
                | ResolvedTypeLiteralLike.EnumCase { Value = TsLiteral.Int _ }
                | ResolvedTypeLiteralLike.TypeQuery { Type = Resolve (ResolvedType.Literal (TsLiteral.Int _)) }
                | ResolvedTypeLiteralLike.Literal (TsLiteral.Int _) -> true
                | _ -> false
                )
        then
            renderEnumLiterals ctx scopeStore literals
            |> Transient.TypeRender.EnumUnion
        else
            renderUnionLiterals ctx scopeStore literals
            |> Transient.TypeRender.StringUnion
module TemplateLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (templateLiteral: TemplateLiteral) =
            let path =
                Name.Pascal.create "TemplateLiteral"
                |> TransientTypePath.AnchoredAndMoored
                |> Path.create
            {
                TypeLikeRender.Metadata = {
                    Path = path
                    Original = path
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name = Name.Pascal.create "TemplateLiteral" |> ValueSome
                TypeParameters = []
                Members = [
                    {
                        TypedNameRender.Metadata = {
                            Path = Path.create TransientMemberPath.Anchored
                            Original = Path.create TransientMemberPath.Anchored
                            Source = ValueNone
                            FullyQualifiedName = ValueNone
                        }
                        Name = Name.create "Value" |> Case.addCamelMeasure
                        Type =
                            RenderScopeStore.TypeRefRender.create
                                scopeStore
                                (ResolvedType.Primitive TypeKindPrimitive.String)
                                false
                                Intrinsic.string 
                        Traits = Set [ RenderTraits.EmitSelf ]
                        TypeParameters = []
                        Documentation = []
                    }
                ]
                Functions = [
                    {
                        FunctionLikeRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                        Source = ValueNone; FullyQualifiedName = ValueNone }
                        Name = Name.create "Create" |> Case.addCamelMeasure
                        Signatures = [
                            {
                                FunctionLikeSignature.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                                   Source = ValueNone; FullyQualifiedName = ValueNone }
                                Parameters =
                                    templateLiteral.Types
                                    |> List.mapi (fun i typeRef ->
                                        {
                                            TypedNameRender.Metadata = { Path = Path.create TransientParameterPath.Anchored; Original = Path.create TransientParameterPath.Anchored
                                                                         Source = ValueNone; FullyQualifiedName = ValueNone }
                                            Name = Name.Camel.create $"v{i}"
                                            Type = ctx.PreludeGetTypeRef ctx scopeStore typeRef
                                            Traits = Set.empty
                                            TypeParameters = []
                                            Documentation = []
                                        }
                                        )
                                ReturnType =
                                    Ast.Anon("TemplateLiteral")
                                    |> RenderScopeStore.TypeRefAtom.Unsafe.createWidget
                                    |> RenderScopeStore.TypeRef.Unsafe.createAtom
                                    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                                Traits = Set [
                                    RenderTraits.Inline
                                    RenderTraits.StringBuilder
                                ]
                                TypeParameters = []
                                Documentation = []
                            }
                        ]
                        Traits = Set [
                            RenderTraits.Inline
                            RenderTraits.StringBuilder
                        ]
                        TypeParameters = []
                        Documentation = []
                    }
                ]
                Inheritance = []
                Constructors = []
                Documentation = []
            }
            |> TypeDefn
module Members =
    let renderFromMembersAndFunctions (ctx: GeneratorContext) (scopeStore: RenderScopeStore) members functions =
        // Derive Metadata.Path from scopeStore.PathContext so that the
        // resulting TypeLikeRender carries this literal's position rather
        // than a bare Anchored placeholder. After anchoring, the Name
        // fallback in `anchorTypeDefn` reads the path's leaf segment as
        // the Name when none is supplied — giving inline parameter shapes
        // a meaningful name (e.g. "Context") instead of inheriting the
        // parent type's name (e.g. "DispatchWorkflow") via the surrounding
        // anchor.
        let scopedPath =
            TransientPath.toTransientModulePath scopeStore.PathContext
            |> TransientTypePath.graft
            |> Path.create
        {
            Transient.TypeLikeRender.Metadata = { Path = scopedPath; Original = scopedPath
                                                  Source = ValueNone; FullyQualifiedName = ValueNone }
            Name = ValueNone
            TypeParameters = []
            Inheritance = []
            Members = members
            Functions = functions
            Constructors = []
            Documentation = []
        }
        |> Transient.TypeRender.TypeDefn
    let inline render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (members: Member list) =
        Member.partitionRender ctx scopeStore members
        ||> renderFromMembersAndFunctions ctx scopeStore

module Intersection =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (intersection: Intersection) =
        Member.collectAllRecursively (ResolvedType.Intersection intersection)
        |> Members.render ctx scopeStore

module TypeLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (typeLiteral: TypeLiteral) =
        typeLiteral.Members
        |> Members.render ctx scopeStore

module Literal =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: TsLiteral) =
        {
            Metadata =
                TransientTypePath.Anchored
                |> Path.create
                |> RenderMetadata.createWithPath
            LiteralUnionRender.Name = ValueNone
            Cases = [
                {
                    Metadata = 
                        TransientMemberPath.Anchored
                        |> Path.create
                        |> RenderMetadata.createWithPath
                    Name =
                        match literal with
                        | TsLiteral.String value -> value
                        | TsLiteral.Int value -> string value
                        | TsLiteral.Float value -> string value
                        | TsLiteral.Bool value -> string value
                        | TsLiteral.BigInt value -> string value
                        | TsLiteral.Null -> "null"
                        |> Name.Pascal.create
                        |> ValueSome
                    Value = literal
                    Documentation = []
                }
            ]
            Documentation = []
        }
        |> Transient.TypeRender.StringUnion

module EnumCase =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (enumCase: EnumCase) =
        {
            Metadata =
                TransientTypePath.Anchored
                |> Path.create
                |> RenderMetadata.createWithPath
                |> RenderMetadata.withFullyQualifiedName enumCase.FullyQualifiedName
                |> RenderMetadata.withSourceOption enumCase.Source
            Name = ValueSome enumCase.Name
            Cases = [
                let path = (Case.unboxMeasure enumCase.Name) |> TransientMemberPath.AnchoredAndMoored |> Path.create
                {
                    Metadata = { Path = path; Original = path
                                 Source = enumCase.Source |> Option.toValueOption
                                 FullyQualifiedName = ValueSome enumCase.FullyQualifiedName }
                    Name = ValueSome enumCase.Name
                    Value = enumCase.Value
                    Documentation = enumCase.Documentation
                }
            ]
            Documentation = enumCase.Documentation
        }
        |> Transient.TypeRender.StringUnion