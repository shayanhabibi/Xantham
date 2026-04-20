[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Anchored

open System.Collections.Generic
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Generator
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Anchored

module Render =
    let anchorMetadataPath (ctx: GeneratorContext) (anchorPath: AnchorPath) (path: Path) =
        match path with
        | Path.Transient transientPath ->
            transientPath
            |> TransientPath.anchor anchorPath
        | Path.Anchor anchorPath -> anchorPath
    let anchorMetadata (ctx: GeneratorContext) (anchorPath: AnchorPath) (metadata: RenderMetadata) =
        if metadata.Path.IsAnchor then metadata else
        { Path = anchorMetadataPath ctx anchorPath metadata.Path |> Path.create }
    module Transient =
        let inline anchorUnionCase (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralCaseRender<'T>) =
            let anchoredPath =
                enumUnion.Metadata.Path
                |> anchorMetadataPath ctx parentPath
            {
                LiteralCaseRender.Name =
                    enumUnion.Name
                    |> ValueOption.defaultValue (
                        match anchoredPath with
                        | AnchorPath.Type typePath -> typePath.Name
                        | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
                        | _ -> failwith "Unreachable branch"
                        )
                Metadata = { Path = Path.create anchoredPath }
                Value = enumUnion.Value
                Documentation = enumUnion.Documentation
            }
        let inline anchorUnion (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralUnionRender<'T>) =
            let anchoredPath =
                enumUnion.Metadata.Path
                |> anchorMetadataPath ctx parentPath
            {
                Metadata = { Path = Path.create anchoredPath }
                LiteralUnionRender.Name =
                    enumUnion.Name
                    |> ValueOption.defaultWith (fun () ->
                        match anchoredPath with
                        | AnchorPath.Type typePath -> typePath.Name
                        | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
                        | _ -> failwith "Unreachable branch"
                        )
                Cases =
                    enumUnion.Cases
                    |> List.map (anchorUnionCase ctx anchoredPath)
                Documentation = enumUnion.Documentation
            }
        let anchorTypeParameters (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeParameter: Transient.TypeParameterRender) =
            let anchorPath =
                typeParameter.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                TypeParameterRender.Name =
                    typeParameter.Name
                Constraint =
                    typeParameter.Constraint
                    |> ValueOption.map (TypeRefRender.anchor anchorPath)
                Default =
                    typeParameter.Default
                    |> ValueOption.map (TypeRefRender.anchor anchorPath)
                Documentation = typeParameter.Documentation
            }
        let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Transient.TypedNameRender) =
            let anchorPath =
                typedName.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                TypedNameRender.Name = typedName.Name
                Type = typedName.Type |> TypeRefRender.anchor anchorPath
                Traits = typedName.Traits
                TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = typedName.Documentation
            }
        let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Transient.FunctionLikeSignature) =
            {
                FunctionLikeSignature.Metadata = { Path = Path.create anchorPath }
                Parameters =
                    functionSignature.Parameters
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                ReturnType =
                    functionSignature.ReturnType
                    |> TypeRefRender.anchor anchorPath
                Traits = functionSignature.Traits
                Documentation = functionSignature.Documentation
                TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
            }
        let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Transient.FunctionLikeRender) =
            let anchorPath =
                functionLike.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                FunctionLikeRender.Name = functionLike.Name
                Signatures = functionLike.Signatures |> List.map (anchorFunctionSignature ctx anchorPath)
                Traits = functionLike.Traits
                TypeParameters = functionLike.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = functionLike.Documentation
            }
        let anchorTypeDefn (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeDefn: Transient.TypeLikeRender) =
            let anchorPath =
                typeDefn.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                TypeLikeRender.Metadata = { Path = Path.create anchorPath }
                Name =
                    typeDefn.Name
                    |> ValueOption.defaultWith (fun () ->
                        match anchorPath with
                        | AnchorPath.Type typePath -> typePath.Name
                        | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
                        | _ -> failwith "Unreachable branch"
                        )
                TypeParameters =
                    typeDefn.TypeParameters
                    |> List.map (anchorTypeParameters ctx anchorPath)
                Inheritance =
                    typeDefn.Inheritance
                    |> List.map (TypeRefRender.anchor anchorPath)
                Members =
                    typeDefn.Members
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                Functions =
                    typeDefn.Functions
                    |> List.map (anchorFunction ctx anchorPath)
                Constructors =
                    typeDefn.Constructors
                    |> List.map (List.map (anchorTypedNameRender ctx anchorPath))
                Documentation = typeDefn.Documentation
            }
        let anchorTypeAlias (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeAlias: Transient.TypeAliasRender) =
            match typeAlias with
            | Transient.TypeAliasRender.Alias alias ->
                let anchorPath =
                    alias.Metadata.Path
                    |> anchorMetadataPath ctx anchorPath
                {
                    TypeAliasRenderRef.Metadata = { Path = Path.create anchorPath }
                    Name = alias.Name |> ValueOption.defaultWith (fun () ->
                        match anchorPath with
                        | AnchorPath.Type typePath -> typePath.Name
                        | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
                        | _ -> failwith "Unreachable branch"
                        )
                    TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    Type = alias.Type |> TypeRefRender.anchor anchorPath
                }
                |> TypeAliasRender.Alias
            | Transient.TypeAliasRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx anchorPath typeLikeRender
                |> TypeAliasRender.TypeDefn
            | Transient.TypeAliasRender.StringUnion literalUnionRender ->
                anchorUnion ctx anchorPath literalUnionRender
                |> TypeAliasRender.StringUnion
            | Transient.TypeAliasRender.EnumUnion literalUnionRender ->
                anchorUnion ctx anchorPath literalUnionRender
                |> TypeAliasRender.EnumUnion
            | Transient.TypeAliasRender.Function functionLikeRender ->
                anchorFunction ctx anchorPath functionLikeRender
                |> TypeAliasRender.Function

        let anchor (ctx: GeneratorContext) (anchorPath: AnchorPath) (render: Transient.Render) =
            let ref, render = render
            match render.Value with
            | Transient.TypeRender.EnumUnion enumUnion ->
                anchorUnion ctx anchorPath enumUnion
                |> TypeRender.EnumUnion
            | Transient.TypeRender.StringUnion enumUnion ->
                anchorUnion ctx anchorPath enumUnion
                |> TypeRender.StringUnion
            | Transient.TypeRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx anchorPath typeLikeRender
                |> TypeRender.TypeDefn
            | Transient.TypeRender.TypeAlias typeAliasRender ->
                anchorTypeAlias ctx anchorPath typeAliasRender
                |> TypeRender.TypeAlias
            | Transient.TypeRender.Function functionLikeRender ->
                anchorFunction ctx anchorPath functionLikeRender
                |> TypeRender.Function
            | Transient.TypeRender.Variable typedNameRender ->
                anchorTypedNameRender ctx anchorPath typedNameRender
                |> TypeRender.Variable
            |> fun typeRender ->
                Anchored.Render( TypeRefRender.anchor anchorPath ref, lazy typeRender )
    module Concrete =
        let inline anchorEnumCase (enumUnion: Concrete.LiteralCaseRender<'T>) =
            {
                Metadata = enumUnion.Metadata
                LiteralCaseRender.Name = enumUnion.Name
                Value = enumUnion.Value
                Documentation = enumUnion.Documentation
            }
        let inline anchorEnum (enumUnion: Concrete.LiteralUnionRender<'T>) =
            {
                Metadata = enumUnion.Metadata
                LiteralUnionRender.Name = enumUnion.Name
                Cases = enumUnion.Cases |> List.map anchorEnumCase
                Documentation = enumUnion.Documentation
            }
        let anchorTypeParameters (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeParameter: Concrete.TypeParameterRender) =
            let anchorPath =
                typeParameter.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                TypeParameterRender.Name =
                    typeParameter.Name
                Constraint =
                    typeParameter.Constraint
                    |> ValueOption.map (TypeRefRender.anchor anchorPath)
                Default =
                    typeParameter.Default
                    |> ValueOption.map (TypeRefRender.anchor anchorPath)
                Documentation = typeParameter.Documentation
            }
        let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Concrete.TypedNameRender) =
            let anchorPath =
                typedName.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                TypedNameRender.Name = typedName.Name
                Type = typedName.Type |> TypeRefRender.anchor anchorPath
                Traits = typedName.Traits
                TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = typedName.Documentation
            }
        let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Concrete.FunctionLikeSignature) =
            {
                FunctionLikeSignature.Metadata = { Path = Path.create anchorPath }
                Parameters =
                    functionSignature.Parameters
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                ReturnType =
                    functionSignature.ReturnType
                    |> TypeRefRender.anchor anchorPath
                Traits = functionSignature.Traits
                Documentation = functionSignature.Documentation
                TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
            }
        let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Concrete.FunctionLikeRender) =
            let anchorPath =
                functionLike.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = { Path = Path.create anchorPath }
                FunctionLikeRender.Name = functionLike.Name
                Signatures = functionLike.Signatures |> List.map (anchorFunctionSignature ctx anchorPath)
                Traits = functionLike.Traits
                TypeParameters = functionLike.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = functionLike.Documentation
            }
        let anchorTypeDefn (ctx: GeneratorContext) (typeDefn: Concrete.TypeLikeRender) =
            let anchorPath =
                match typeDefn.Metadata.Path with
                | Path.Anchor anchorPath -> anchorPath
                | _ -> failwith "UNREACHABLE"
            {
                TypeLikeRender.Metadata = { Path = Path.create anchorPath }
                Name = typeDefn.Name
                TypeParameters =
                    typeDefn.TypeParameters
                    |> List.map (anchorTypeParameters ctx anchorPath)
                Inheritance =
                    typeDefn.Inheritance
                    |> List.map (TypeRefRender.anchor anchorPath)
                Members =
                    typeDefn.Members
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                Functions =
                    typeDefn.Functions
                    |> List.map (anchorFunction ctx anchorPath)
                Constructors =
                    typeDefn.Constructors
                    |> List.map (List.map (anchorTypedNameRender ctx anchorPath))
                Documentation = typeDefn.Documentation
            }
        let anchorTypeAlias (ctx: GeneratorContext) (typeAlias: Concrete.TypeAliasRender) =
            match typeAlias with
            | Concrete.TypeAliasRender.Alias alias ->
                let anchorPath =
                    match alias.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                {
                    TypeAliasRenderRef.Metadata = { Path = Path.create anchorPath }
                    Name = alias.Name 
                    TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    Type = alias.Type |> TypeRefRender.anchor anchorPath
                }
                |> TypeAliasRender.Alias
            | Concrete.TypeAliasRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx typeLikeRender
                |> TypeAliasRender.TypeDefn
            | Concrete.TypeAliasRender.StringUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeAliasRender.StringUnion
            | Concrete.TypeAliasRender.EnumUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeAliasRender.EnumUnion
            | Concrete.TypeAliasRender.Function functionLikeRender ->
                let anchorPath =
                    match functionLikeRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                anchorFunction ctx anchorPath functionLikeRender
                |> TypeAliasRender.Function

        let anchor (ctx: GeneratorContext) (render: Concrete.Render) =
            let ref, render = render
            match render.Value with
            | Concrete.TypeRender.EnumUnion enumUnion ->
                anchorEnum enumUnion
                |> TypeRender.EnumUnion
            | Concrete.TypeRender.StringUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeRender.StringUnion
            | Concrete.TypeRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx typeLikeRender
                |> TypeRender.TypeDefn
            | Concrete.TypeRender.TypeAlias typeAliasRender ->
                anchorTypeAlias ctx typeAliasRender
                |> TypeRender.TypeAlias
            | Concrete.TypeRender.Function functionLikeRender ->
                let anchorPath =
                    match functionLikeRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                anchorFunction ctx anchorPath functionLikeRender
                |> TypeRender.Function
            | Concrete.TypeRender.Variable typedNameRender ->
                let anchorPath =
                    match typedNameRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                anchorTypedNameRender ctx anchorPath typedNameRender
                |> TypeRender.Variable
            |> fun typeRender ->
                let ref =
                    ModulePath.init ""
                    |> AnchorPath.create
                    |> TypeRefRender.anchor
                    |> funApply ref
                Anchored.Render(ref, lazy typeRender)

let inline addOrReplace ctx key value =
    GeneratorContext.Anchored.addOrReplace ctx key value
    

let rec anchor (ctx: GeneratorContext) anchors anchorPath resolvedType =
    GeneratorContext.Prelude.tryGet ctx resolvedType
    |> ValueOption.iter (anchorPreludeAnchorScope ctx (Some anchors) anchorPath)
        
and anchorPreludeAnchorScope (ctx: GeneratorContext) anchors anchorPath renderScope =
    let anchors = defaultArg anchors (Dictionary<ResolvedType, TypePath * Render>())
    match renderScope with
    | { Root = ValueSome (TypeLikePath.Anchored path); Render = Render.Concrete(renderTuple); TransientChildren = ValueSome transientChildren } ->
        let anchorPath = AnchorPath.create path
        let anchors = Dictionary<ResolvedType, TypePath * Render>()
        let render = Render.Concrete.anchor ctx renderTuple
        {
            RenderScope.Type = renderScope.Type
            Root = path
            TypeRef =
                renderScope.TypeRef
                |> TypeRefRender.anchor anchorPath
            Render = render
            Anchors = anchors
        }
        |> addOrReplace ctx renderScope.Type
        transientChildren.Keys
        |> Seq.iter (anchor ctx anchors anchorPath)
    | { Root = ValueSome (TypeLikePath.Transient path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
        let path = TransientTypePath.anchor anchorPath path
        let render = Render.Transient.anchor ctx anchorPath renderTuple
        anchors
        |> Dictionary.tryAdd renderScope.Type (path, render)
    | { Root = ValueNone; Render = Render.RefOnly typeRef } ->
        typeRef
        |> TypeRefRender.anchor anchorPath
        |> addOrReplace ctx renderScope.Type
    | badScope ->
        printfn $"Bad scope: %A{badScope}"
and anchorPreludeExportScope (ctx: GeneratorContext) export (renderScopeStore: RenderScopeStore) =
    let anchors = Dictionary<ResolvedType, TypePath * Render>()
    let anchorPath = Path.fromResolvedExport export
    renderScopeStore
    |> Seq.iter (fun (KeyValue(key, value)) ->
        let renderScope =
            GeneratorContext.Prelude.tryGet ctx key
            |> ValueOption.orElseWith (fun () ->
                prerender ctx renderScopeStore (LazyContainer.CreateTypeKeyDummy<ResolvedType> key)
                |> ignore
                GeneratorContext.Prelude.tryGet ctx key
                )
            |> ValueOption.defaultWith (fun () ->
                failwith "Could not find render scope for key")
        let path = TransientPath.anchor anchorPath (TransientPath.create value)
        anchorPreludeAnchorScope ctx (Some anchors) path renderScope)
    anchors

let rec registerAnchorFromExport (ctx: GeneratorContext) (export: ResolvedExport): unit =
    let scope = RenderScopeStore.create()
    match export with
    | ResolvedExport.Class value ->
        let path = Path.fromClass value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths value then
            ref |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let anchors = anchorPreludeExportScope ctx export scope
        let render =
            Class.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Class value
            Root = path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.Variable value ->
        let path =
            Path.fromVariable value
            |> AnchorPath.create
        let typeRef =
            value.Type
            |> prerender ctx scope
            |> TypeRefRender.anchor path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths value then
            typeRef |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let anchors = anchorPreludeExportScope ctx export scope
        let render =
            {
                Metadata = { Path = Path.create path }
                TypedNameRender.Name = value.Name
                Type = typeRef
                Traits = Set []
                TypeParameters = []
                Documentation = value.Documentation
            }
            |> TypeRender.Variable
        {
            Type = value.Type.Value
            Root = path |> AnchorPath.toTypePath
            TypeRef = typeRef
            Render = Anchored.Render( typeRef, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.Interface value ->
        let path = Path.fromInterface value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths value then
            ref |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let anchors = anchorPreludeExportScope ctx export scope
        let render =
            Interface.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Interface value
            Root = path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.TypeAlias value ->
        let path = Path.fromTypeAlias value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths value then
            ref |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let anchors = anchorPreludeExportScope ctx export scope
        let render =
            TypeAlias.render ctx scope value
            |> Render.Concrete.anchorTypeAlias ctx
            |> TypeRender.TypeAlias
        {
            Type = value.Type.Value
            Root = path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.Enum value ->
        let path = Path.fromEnum value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths value then
            ref |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let anchors = anchorPreludeExportScope ctx export scope
        let render =
            match Enum.render ctx value with
            | Concrete.TypeRender.EnumUnion enumUnion ->
                enumUnion
                |> Render.Concrete.anchorEnum
                |> TypeRender.EnumUnion
            | Concrete.TypeRender.StringUnion literalUnionRender ->
                literalUnionRender
                |> Render.Concrete.anchorEnum
                |> TypeRender.StringUnion
            | _ -> failwith "Unreachable branch"
        {
            Type = ResolvedType.Enum value
            Root = path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.Function [] -> failwith "Empty function list"
    | ResolvedExport.Function (headFunc :: rest) ->
        let path = Path.fromFunction headFunc
        let anchorPath = AnchorPath.create path
        let ref =
            headFunc.SignatureKey.Value
            |> ResolvedType.TypeLiteral
            |> LazyContainer.CreateTypeKeyDummy<ResolvedType>
            |> prerender ctx scope
            |> TypeRefRender.anchor anchorPath
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors.IgnoreRendersForPaths headFunc then
            ref |> GeneratorContext.Anchored.addOrReplace ctx export
        else
        let render =
            {
                FunctionLikeRender.Name = headFunc.Name
                Metadata = { Path = Path.create path }
                Signatures =
                    (headFunc :: rest)
                    |> List.map (fun func ->
                        {
                            FunctionLikeSignature.Metadata = { Path = Path.create path }
                            Parameters =
                                func.Parameters
                                |> List.map (
                                    Parameter.render ctx scope
                                    >> Render.Concrete.anchorTypedNameRender ctx anchorPath
                                    )
                            ReturnType =
                                func.Type
                                |> prerender ctx scope
                                |> TypeRefRender.anchor anchorPath
                            Traits = Set [ RenderTraits.Static ]
                            Documentation = func.Documentation
                            TypeParameters =
                                func.TypeParameters
                                |> List.map (
                                    _.Value
                                    >> TypeParameter.render ctx scope
                                    >> Render.Concrete.anchorTypeParameters ctx anchorPath
                                    )
                        }
                        )
                Traits = Set []
                TypeParameters =
                    headFunc.TypeParameters
                    |> List.map (fun typeParameter ->
                        let typeParameter = typeParameter.Value
                        let path =
                            path
                            |> TypeParamPath.createOnMember typeParameter.Name
                        {
                            TypeParameterRender.Metadata = { Path = Path.create path }
                            Name = typeParameter.Name
                            Constraint =
                                typeParameter.Constraint
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchor anchorPath)
                            Default = 
                                typeParameter.Default
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchor anchorPath)
                            Documentation = typeParameter.Documentation
                        }
                        )
                Documentation = headFunc.Documentation
            }
            |> TypeRender.Function
        let anchors = anchorPreludeExportScope ctx export scope
        {
            Type = headFunc.SignatureKey.Value |> ResolvedType.TypeLiteral
            Root = anchorPath |> AnchorPath.toTypePath
            TypeRef = ref 
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchors
        }
        |> GeneratorContext.Anchored.addOrReplace ctx export
    | ResolvedExport.Module value ->
        value.Exports
        |> List.iter (registerAnchorFromExport ctx)

let private registerExportsForAnchoring (ctx: GeneratorContext) = List.iter (registerAnchorFromExport ctx)

module ArenaInterner =
    let processExports (ctx: GeneratorContext) (interner: ArenaInterner) =
        interner.ExportMap
        |> Map.iter (fun _ -> registerExportsForAnchoring ctx)