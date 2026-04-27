[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeAlias

open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Generator
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
        let path = Path.Interceptors.pipeTypeAlias ctx typ
        let metadata =
            (Path.create path, typ)
            ||> RenderMetadata.createWithPathFromExport
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
            let oldRef = ctx.PreludeGetTypeRef ctx scopeStore innerType
            let innerRef =
                match ctx.PreludeRenders.TryGetValue(innerType.Value) with
                | true, value -> ValueSome value
                | false, _ -> ValueNone
                |> ValueOption.map (fun newRef ->
                    match ctx.TypeAliasRemap.TryGetValue(innerType.Value) with
                    | true, value ->
                        TypeRefRender.replace value newRef.TypeRef oldRef
                    | _ -> newRef.TypeRef
                    )
                |> ValueOption.defaultValue oldRef
                
            {
                TypeAliasRenderRef.Documentation = documentation
                Metadata = metadata
                Name = name
                TypeParameters = typeParameters
                Type = innerRef
            }
            |> TypeAliasRender.Alias
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
        | ResolvedType.Union _ ->
            let typeRefRender =
                {
                    TypeAliasRenderRef.Documentation = documentation
                    Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Type = ctx.PreludeGetTypeRef ctx scopeStore innerType
                }
                |> TypeAliasRender.Alias
            match ResolvedTypeCategories.create innerType.Value with
            // no literals, and no 'others' that require a transient type
            | { LiteralLike = literals; Others = []; Nullable = nullable; Primitives = []; EnumLike = [] } ->
                match Union.renderLiterals ctx scopeStore literals with
                | TypeRender.EnumUnion enumRender ->
                    {
                        LiteralUnionRender.Metadata = metadata
                        Name = name
                        Cases =
                            enumRender.Cases
                            |> List.map (fun case ->
                                {
                                    LiteralCaseRender.Metadata = case.Metadata
                                    Name = case.Name.Value
                                    Value = case.Value
                                    Documentation = case.Documentation
                                })
                        Documentation = documentation
                    }
                    |> TypeAliasRender.EnumUnion
                | TypeRender.StringUnion literalRender ->
                    {
                        LiteralUnionRender.Metadata = metadata
                        Name = name
                        Cases =
                            literalRender.Cases
                            |> List.map (fun case ->
                                {
                                    LiteralCaseRender.Metadata = case.Metadata
                                    Name = case.Name.Value
                                    Value = case.Value
                                    Documentation = case.Documentation
                                })
                        Documentation = documentation
                    }
                    |> TypeAliasRender.StringUnion
                | _ -> typeRefRender
            | _ -> typeRefRender
        | ResolvedType.Literal tsLiteral ->
            {
                LiteralUnionRender.Metadata = metadata
                Name = name
                Cases = [
                    {
                        LiteralCaseRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                       Source = ValueNone; FullyQualifiedName = ValueNone }
                        Name = name
                        Value = tsLiteral
                        Documentation = documentation
                    }
                ]
                Documentation = documentation
            }
            |> TypeAliasRender.StringUnion
            
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
        | ResolvedType.EnumCase enumCase ->
            {
                LiteralUnionRender.Metadata = metadata
                Name = name
                Cases = [
                    {
                        LiteralCaseRender.Metadata = {
                            Path = Path.create TransientMemberPath.Anchored
                            Original = Path.create TransientMemberPath.Anchored
                            Source = enumCase.Source |> Option.toValueOption
                            FullyQualifiedName = ValueSome enumCase.FullyQualifiedName
                        }
                        Name = enumCase.Name
                        Value = enumCase.Value
                        Documentation = enumCase.Documentation
                    }
                ]
                Documentation = documentation
            }
            |> TypeAliasRender.StringUnion
        | ResolvedType.TemplateLiteral templateLiteral ->
            {
                TypeLikeRender.Metadata = metadata
                Name = name
                TypeParameters = typeParameters
                Members = [
                    {
                        TypedNameRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                     Source = ValueNone; FullyQualifiedName = ValueNone }
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
                                    Ast.Anon(Name.Case.valueOrModified name)
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
                Documentation = documentation
            }
            |> TypeAliasRender.TypeDefn