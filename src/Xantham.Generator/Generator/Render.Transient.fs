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
            {
                LiteralCaseRender.Metadata = { Path = Path.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name)) }
                Name = enumCase.Name |> ValueSome
                Value = enumCase.Value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal tsLiteral -> 
            {
                LiteralCaseRender.Metadata = { Path = Path.create TransientMemberPath.Anchored }
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
    let private renderUnionLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = { Path = Path.create TransientTypePath.Anchored }
            Prelude.Transient.LiteralUnionRender.Name = ValueNone
            Cases =
                literals
                |> List.map (renderUnionLiteralCase ctx scopeStore)
            Documentation = []
        }
    let private renderEnumLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
            {
                Metadata = { Path = Path.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name)) }
                Name = enumCase.Name |> ValueSome
                Value = value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Int value) ->
            {
                Metadata = { Path = Path.create TransientMemberPath.Anchored }
                Name = string value |> Name.Pascal.create |> ValueSome
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.EnumCase _ 
        | ResolvedTypeLiteralLike.Literal _ -> failwith "Cannot render enum literal case for non int literal. This should be guarded against"

    let private renderEnumLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = { Path = Path.create TransientTypePath.Anchored }
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
            {
                TypeLikeRender.Metadata = { Path = Path.create (TransientTypePath.AnchoredAndMoored (Name.Pascal.create "TemplateLiteral")) }
                Name = Name.Pascal.create "TemplateLiteral" |> ValueSome
                TypeParameters = []
                Members = [
                    {
                        TypedNameRender.Metadata = { Path = Path.create TransientMemberPath.Anchored }
                        Name = Name.create "Value" |> Case.addCamelMeasure
                        Type =
                            RenderScopeStore.TypeRefRender.create
                                scopeStore
                                (ResolvedType.Primitive TypeKindPrimitive.String)
                                false
                                Types.string 
                        Traits = Set [ RenderTraits.EmitSelf ]
                        TypeParameters = []
                        Documentation = []
                    }
                ]
                Functions = [
                    {
                        FunctionLikeRender.Metadata = { Path = Path.create TransientMemberPath.Anchored }
                        Name = Name.create "Create" |> Case.addCamelMeasure
                        Signatures = [
                            {
                                FunctionLikeSignature.Metadata = { Path = Path.create TransientMemberPath.Anchored }
                                Parameters =
                                    templateLiteral.Types
                                    |> List.mapi (fun i typeRef ->
                                        {
                                            TypedNameRender.Metadata = { Path = Path.create TransientParameterPath.Anchored }
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
        {
            Transient.TypeLikeRender.Metadata = { Path = Path.create TransientTypePath.Anchored }
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
            Metadata = { Path = Path.create TransientTypePath.Anchored }
            LiteralUnionRender.Name = ValueNone
            Cases = [
                {
                    Metadata = { Path = Path.create TransientMemberPath.Anchored }
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
            Metadata = { Path = Path.create TransientTypePath.Anchored }
            Name = ValueSome enumCase.Name
            Cases = [
                {
                    Metadata = { Path = Path.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name)) }
                    Name = ValueSome enumCase.Name
                    Value = enumCase.Value
                    Documentation = enumCase.Documentation
                }
            ]
            Documentation = enumCase.Documentation
        }
        |> Transient.TypeRender.StringUnion