[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeAlias

open FSharp.SignalsDotnet
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.NamePath

module TypeAlias =
    let render (ctx: GeneratorContext) scopeStore (typ: TypeAlias) =
        let innerType = typ.Type
        let typeParameters =
            typ.TypeParameters
            |> List.map (_.Value >> TypeParameter.render ctx scopeStore)
        let documentation = typ.Documentation
        let name = typ.Name
        let path = Path.fromTypeAlias typ
        let metadata = RenderMetadata.create path
        let makeLiteralsDefn metadata = function
            | literals when literals |> List.forall (function
                | ResolvedTypeLiteralLike.Literal (TsLiteral.Int _)
                | ResolvedTypeLiteralLike.EnumCase { Value = TsLiteral.Int _ } -> true
                // We will filter this out anyway and count it as a primitive
                | _ -> false) ->
                {
                    Metadata = metadata
                    LiteralUnionRender.Name = Name.Pascal.create "Literals"
                    Cases =
                        literals
                        |> List.map (function
                            | ResolvedTypeLiteralLike.Literal (TsLiteral.Int value) ->
                                {
                                    LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                                    Name = Name.Pascal.create (string value)
                                    Value = value
                                    Documentation = []
                                }
                            | ResolvedTypeLiteralLike.EnumCase ({ Value = TsLiteral.Int value } as enumCase) ->
                                {
                                    LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                                    Name = enumCase.Name
                                    Value = value
                                    Documentation = enumCase.Documentation
                                }
                            | _ -> failwith "Unreachable branch guaranteed by pattern guard"
                            )
                    Documentation = documentation
                }
                |> TypeAliasRender.EnumUnion
            | literals ->
                {
                    Metadata = metadata
                    LiteralUnionRender.Name = Name.Pascal.create "Literals"
                    Cases =
                        literals
                        |> List.map (function
                            | ResolvedTypeLiteralLike.Literal value ->
                                let name =
                                    match value with
                                    | TsLiteral.String value -> Name.Pascal.create value
                                    | TsLiteral.Int value -> Name.Pascal.create (string value)
                                    | TsLiteral.Float value -> Name.Pascal.create (string value)
                                    | TsLiteral.Bool value -> Name.Pascal.create (string value)
                                    | TsLiteral.BigInt value -> Name.Pascal.create (string value)
                                    | TsLiteral.Null -> Name.Pascal.create "null"
                                {
                                    LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                                    Name = name
                                    Value = value
                                    Documentation = []
                                }
                            | ResolvedTypeLiteralLike.EnumCase enumCase ->
                                {
                                    LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                                    Name = enumCase.Name
                                    Value = enumCase.Value
                                    Documentation = enumCase.Documentation
                                }
                            )
                    Documentation = documentation
                }
                |> TypeAliasRender.StringUnion
        match innerType.Value with
        | ResolvedType.Interface _
        | ResolvedType.Class _
        | ResolvedType.Primitive _
        | ResolvedType.IndexedAccess _
        | ResolvedType.Index _
        | ResolvedType.Tuple _
        | ResolvedType.TypeParameter _
        | ResolvedType.TypeReference _
        | ResolvedType.Predicate _
        | ResolvedType.Substitution _
        | ResolvedType.Optional _
        | ResolvedType.GlobalThis
        | ResolvedType.Conditional _
        | ResolvedType.Enum _
        | ResolvedType.Array _ ->
            {
                TypeAliasRenderRef.Documentation = documentation
                Metadata = metadata
                Name = name
                TypeParameters = typeParameters
                Type =
                    innerType.Value
                    |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
            }
            |> TypeAliasRender.Alias
            |> List.singleton
        | ResolvedType.Intersection _ 
        | ResolvedType.TypeLiteral _ ->
            let members, functions =
                Member.collectAllRecursively innerType.Value
                |> Member.partitionRender ctx scopeStore
            // if members |> List.isEmpty && functions |> List.forall (_.Name >> Name.Case.valueOrSource >> (=) "Invoke") then
                // ()
            // else
            {
                TypeLikeRender.Metadata = metadata
                Name = name
                TypeParameters = typeParameters
                Members = members 
                Functions = functions 
                Inheritance = []
                Constructors = []
                Documentation = documentation
            }
            |> TypeAliasRender.TypeDefn
            |> List.singleton
            
        | ResolvedType.Union _ ->
            let typeRefRender =
                {
                    TypeAliasRenderRef.Documentation = documentation
                    Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Type =
                        innerType.Value
                        |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
                }
                |> TypeAliasRender.Alias
            let pathForLiterals =
                TransientModulePath.Anchored
                |> TransientTypePath.createOnTransientModule "Literals"
                |> TransientPath.create
                |> TransientPath.anchor (AnchorPath.create path)
            let mutable objectNumber = 0
            let makeObjectPath () =
                let name =
                    match objectNumber with
                    | 0 -> "ObjectVariant"
                    | i -> $"ObjectVariant%i{i + 1}"
                objectNumber <- objectNumber + 1
                let value =
                    TransientModulePath.Anchored
                    |> TransientTypePath.createOnTransientModule name
                    |> TransientPath.create
                    |> TransientPath.anchor (AnchorPath.create path)
                name, value
            let literalsMetadata = RenderMetadata.create pathForLiterals
            match ResolvedTypeCategories.create innerType.Value with
            // no literals, and no 'others' that require a transient type
            | { LiteralLike = [] } & { Others = others } when others |> List.forall (function
                | ResolvedTypeOther.Intersection _
                | ResolvedTypeOther.TypeLiteral _ -> false
                | _ -> true) -> [ typeRefRender ]
            | { LiteralLike = literals } & { Others = others } when others |> List.forall (function
                | ResolvedTypeOther.Intersection _
                | ResolvedTypeOther.TypeLiteral _ -> false
                | _ -> true) ->
                [
                    typeRefRender
                    makeLiteralsDefn literalsMetadata literals
                ]
            | { LiteralLike = literals } & { Others = others } ->
                let requireTransientDefns, _ =
                    others
                    |> List.partition (function
                        | ResolvedTypeOther.TypeLiteral _
                        | ResolvedTypeOther.Intersection _ -> true
                        | _ -> false
                        )
                let otherMemberSets =
                    requireTransientDefns
                    |> Seq.map (_.AsResolvedType >> Member.collectAllRecursively)
                    |> Seq.filter (not << List.isEmpty)
                    |> Seq.toList
                [
                    if literals |> List.isEmpty then ()
                    else makeLiteralsDefn literalsMetadata literals
                    typeRefRender
                    for members in otherMemberSets do
                        let name, path = makeObjectPath()
                        let members, functions =
                            Member.partitionRender ctx scopeStore members
                        {
                            TypeLikeRender.Metadata = RenderMetadata.create path
                            Name = Name.Pascal.create name
                            TypeParameters = []
                            Members = members
                            Functions = functions
                            Inheritance = []
                            Constructors = []
                            Documentation = []
                        }
                        |> TypeAliasRender.TypeDefn
                ]
            
        | ResolvedType.Literal tsLiteral ->
            {
                LiteralUnionRender.Metadata = metadata
                Name = name
                Cases = [
                    {
                        LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                        Name = name
                        Value = tsLiteral
                        Documentation = documentation
                    }
                ]
                Documentation = documentation
            }
            |> TypeAliasRender.StringUnion
            |> List.singleton
            
        | ResolvedType.ReadOnly resolvedType ->
            let members,functions =
                Member.collectAllRecursively resolvedType
                |> List.map Member.setReadOnly
                |> Member.partitionRender ctx scopeStore
            {
                TypeLikeRender.Metadata = metadata
                Name = name
                TypeParameters = typeParameters
                Members = members
                Functions = functions
                Inheritance = []
                Constructors = []
                Documentation = documentation
            }
            |> TypeAliasRender.TypeDefn
            |> List.singleton
        | ResolvedType.EnumCase enumCase ->
            {
                LiteralUnionRender.Metadata = metadata
                Name = name
                Cases = [
                    {
                        LiteralCaseRender.Metadata = RenderMetadata.create TransientMemberPath.Anchored
                        Name = enumCase.Name
                        Value = enumCase.Value
                        Documentation = enumCase.Documentation
                    }
                ]
                Documentation = documentation
            }
            |> TypeAliasRender.StringUnion
            |> List.singleton
        | ResolvedType.TemplateLiteral templateLiteral ->
            {
                TypeLikeRender.Metadata = metadata
                Name = name
                TypeParameters = typeParameters
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
                                                typeRef.Value
                                                |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
                                            Traits = Set.empty
                                            TypeParameters = []
                                            Documentation = []
                                        }
                                        )
                                ReturnType =
                                    Name.Case.valueOrModified name
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
                Documentation = documentation
            }
            |> TypeAliasRender.TypeDefn
            |> List.singleton