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
    let private renderUnionLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientTypePath voption) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase enumCase ->
            let name =
                Case.unboxMeasure enumCase.Name
            let path =
                transientPathCtx
                |> ValueOption.map (TransientMemberPath.createOnTransientTypeWithName name)
                |> ValueOption.defaultValue (TransientMemberPath.AnchoredAndMoored name)
            {
                LiteralCaseRender.Metadata = {
                    Path = Path.create path
                    Original = TransientMemberPath.AnchoredAndMoored name |> Path.create
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
    let private renderUnionLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientModulePath voption) (literals: ResolvedTypeLiteralLike list) =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
            |> ValueOption.defaultValue TransientTypePath.Anchored
        {
            Metadata = { Path = Path.create path
                         Original = Path.create TransientTypePath.Anchored
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Prelude.Transient.LiteralUnionRender.Name = ValueNone
            Cases =
                literals
                |> List.map (renderUnionLiteralCase ctx scopeStore (ValueSome path))
            Documentation = []
        }
    let private renderEnumLiteralCase (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientTypePath voption) (literal: ResolvedTypeLiteralLike) =
        match literal with
        | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
            let name = 
                Case.unboxMeasure enumCase.Name
            let path =
                transientPathCtx
                |> ValueOption.map (TransientMemberPath.createOnTransientTypeWithName name)
                |> ValueOption.defaultValue (TransientMemberPath.AnchoredAndMoored name)
            {
                Metadata = { Path = Path.create path
                             Original = TransientMemberPath.AnchoredAndMoored name |> Path.create
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
        | ResolvedTypeLiteralLike.EnumCase _ 
        | ResolvedTypeLiteralLike.Literal _ -> failwith "Cannot render enum literal case for non int literal. This should be guarded against"

    let private renderEnumLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientModulePath voption) (literals: ResolvedTypeLiteralLike list) =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
            |> ValueOption.defaultValue TransientTypePath.Anchored
        {
            Metadata = { Path = Path.create path; Original = Path.create TransientTypePath.Anchored
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            Name = ValueNone
            Cases =
                literals
                |> List.map (renderEnumLiteralCase ctx scopeStore (ValueSome path))
            Documentation = []
        }
    
    let renderLiterals (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientModulePath voption) (literals: ResolvedTypeLiteralLike list) =
        if
            literals
            |> List.forall (function
                | ResolvedTypeLiteralLike.EnumCase { Value = TsLiteral.Int _ }
                | ResolvedTypeLiteralLike.Literal (TsLiteral.Int _) -> true
                | _ -> false
                )
        then
            renderEnumLiterals ctx scopeStore transientPathCtx literals
            |> Transient.TypeRender.EnumUnion
        else
            renderUnionLiterals ctx scopeStore transientPathCtx literals
            |> Transient.TypeRender.StringUnion
module TemplateLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (transientPathCtx: TransientModulePath voption) (templateLiteral: TemplateLiteral) =
            let path =
                transientPathCtx
                |> ValueOption.map TransientTypePath.graft
                |> ValueOption.defaultValue (
                        Name.Pascal.create "TemplateLiteral"
                        |> TransientTypePath.AnchoredAndMoored
                    )
            
            {
                TypeLikeRender.Metadata = {
                    Path = Path.create path
                    Original =
                        TransientTypePath.Anchored
                        |> Path.create
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name =
                    match path with
                    | TransientTypePath.AnchoredAndMoored name
                    | TransientTypePath.Moored(_, name) -> ValueSome name
                    | _ -> ValueNone
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
                                Types.string 
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
    let renderFromMembersAndFunctions (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx members functions =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
            |> ValueOption.defaultValue TransientTypePath.Anchored
        {
            Transient.TypeLikeRender.Metadata = { Path = Path.create path; Original = Path.create TransientTypePath.Anchored
                                                  Source = ValueNone; FullyQualifiedName = ValueNone }
            Name =
                match path with
                | TransientTypePath.Anchored -> ValueNone
                | TransientTypePath.Moored(_, name) 
                | TransientTypePath.AnchoredAndMoored name -> ValueSome name
            TypeParameters = []
            Inheritance = []
            Members = members
            Functions = functions
            Constructors = []
            Documentation = []
        }
        |> Transient.TypeRender.TypeDefn
    let inline render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx (members: Member list) =
        let typePathCtx = transientPathCtx |> ValueOption.map TransientTypePath.graft
        Member.partitionRender ctx scopeStore typePathCtx members
        ||> renderFromMembersAndFunctions ctx scopeStore transientPathCtx

module Intersection =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx (intersection: Intersection) =
        Member.collectAllRecursively (ResolvedType.Intersection intersection)
        |> Members.render ctx scopeStore transientPathCtx

module TypeLiteral =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx (typeLiteral: TypeLiteral) =
        typeLiteral.Members
        |> Members.render ctx scopeStore transientPathCtx

module Literal =
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx (literal: TsLiteral) =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
            |> ValueOption.defaultValue TransientTypePath.Anchored
        {
            Metadata = { Path = Path.create path; Original = Path.create TransientTypePath.Anchored
                         Source = ValueNone; FullyQualifiedName = ValueNone }
            LiteralUnionRender.Name = ValueNone
            Cases = [
                {
                    Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                 Source = ValueNone; FullyQualifiedName = ValueNone }
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
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) transientPathCtx (enumCase: EnumCase) =
        let path =
            transientPathCtx
            |> ValueOption.map TransientTypePath.graft
            |> ValueOption.defaultValue TransientTypePath.Anchored
        {
            Metadata = { Path = Path.create path
                         Original = Path.create TransientTypePath.Anchored
                         Source = enumCase.Source |> Option.toValueOption
                         FullyQualifiedName = ValueSome enumCase.FullyQualifiedName }
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