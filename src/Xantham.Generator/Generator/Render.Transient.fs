[<AutoOpen>]
module Xantham.Generator.Generator.Render_Transient

open FSharp.SignalsDotnet
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
                LiteralCaseRender.Metadata = RenderMetadata.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name))
                Name = enumCase.Name
                Value = enumCase.Value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal tsLiteral -> 
            {
                LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                Name =
                    match tsLiteral with
                    | TsLiteral.String value -> value
                    | TsLiteral.Int value -> string value
                    | TsLiteral.Float value -> string value
                    | TsLiteral.Bool value -> string value
                    | TsLiteral.BigInt value -> string value
                    | TsLiteral.Null -> "null"
                    |> Name.Pascal.create
                Value = tsLiteral
                Documentation = []
            }
    let private renderUnionLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = RenderMetadata.create TransientTypePath.Anchored
            Prelude.LiteralUnionRender.Name = Name.Pascal.create "Literals"
            Cases = literals |> List.map (renderUnionLiteralCase ctx scopeStore)
            Documentation = []
        }
    let private renderEnumLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
            {
                Metadata = RenderMetadata.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name))
                Name = enumCase.Name 
                Value = value
                Documentation = enumCase.Documentation
            }
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Int value) ->
            {
                Metadata = RenderMetadata.create TransientMemberPath.Anchored
                Name = string value |> Name.Pascal.create 
                Value = value
                Documentation = []
            }
        | ResolvedTypeLiteralLike.EnumCase _ 
        | ResolvedTypeLiteralLike.Literal _ -> failwith "Cannot render enum literal case for non int literal. This should be guarded against"

    let private renderEnumLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (literals: ResolvedTypeLiteralLike list) =
        {
            Metadata = RenderMetadata.create TransientTypePath.Anchored
            Name = Name.Pascal.create "Literals"
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
            |> TypeRender.EnumUnion
        else
            renderUnionLiterals ctx scopeStore literals
            |> TypeRender.StringUnion
module TemplateLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (templateLiteral: TemplateLiteral) =
            {
                TypeLikeRender.Metadata = RenderMetadata.create (TransientTypePath.AnchoredAndMoored (Name.Pascal.create "TemplateLiteral"))
                Name = Name.Pascal.create "TemplateLiteral" 
                TypeParameters = []
                Members = [
                    {
                        TypedNameRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                        Name = Name.create "Value" |> Case.addCamelMeasure
                        Type =
                            ResolvedType.Primitive TypeKindPrimitive.String
                            |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
                        Traits = Set [ RenderTraits.EmitSelf ]
                        TypeParameters = []
                        Documentation = []
                    }
                ]
                Functions = [
                    {
                        FunctionLikeRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                        Name = Name.create "Create" |> Case.addCamelMeasure
                        Signatures = [
                            {
                                FunctionLikeSignature.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                                Parameters =
                                    templateLiteral.Types
                                    |> List.mapi (fun i typeRef ->
                                        {
                                            TypedNameRender.Metadata = RenderMetadata.create TransientParameterPath.Anchored
                                            Name = Name.Camel.create $"v{i}"
                                            Type =
                                                GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value) typeRef.Value
                                            Traits = Set.empty
                                            TypeParameters = []
                                            Documentation = []
                                        }
                                        )
                                ReturnType =
                                    "TemplateLiteral"
                                    |> TypeRefRender.create false
                                    |> Signal.linkWithValue
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
            TypeLikeRender.Metadata = RenderMetadata.create TransientTypePath.Anchored
            Name = Name.Pascal.create "Object"
            TypeParameters = []
            Inheritance = []
            Members = members
            Functions = functions
            Constructors = []
            Documentation = []
        }
        |> TypeRender.TypeDefn
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
            Metadata = RenderMetadata.create TransientTypePath.Anchored
            LiteralUnionRender.Name = Name.Pascal.create "Literal"
            Cases = [
                {
                    Metadata = RenderMetadata.create TransientMemberPath.Anchored
                    Name =
                        match literal with
                        | TsLiteral.String value -> value
                        | TsLiteral.Int value -> string value
                        | TsLiteral.Float value -> string value
                        | TsLiteral.Bool value -> string value
                        | TsLiteral.BigInt value -> string value
                        | TsLiteral.Null -> "null"
                        |> Name.Pascal.create
                    Value = literal
                    Documentation = []
                }
            ]
            Documentation = []
        }
        |> TypeRender.StringUnion

module EnumCase =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (enumCase: EnumCase) =
        {
            Metadata = RenderMetadata.create TransientTypePath.Anchored
            Name = enumCase.Name
            Cases = [
                {
                    Metadata = RenderMetadata.create (TransientMemberPath.AnchoredAndMoored (Case.unboxMeasure enumCase.Name))
                    Name = enumCase.Name
                    Value = enumCase.Value
                    Documentation = enumCase.Documentation
                }
            ]
            Documentation = enumCase.Documentation
        }
        |> TypeRender.StringUnion