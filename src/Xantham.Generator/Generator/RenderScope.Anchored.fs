[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Anchored

open System.Collections.Generic
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Anchored

module Render =
    let anchorMetadataPath (_ctx: GeneratorContext) (anchorPath: AnchorPath) (path: Path) =
        match path with
        | Path.Transient transientPath ->
            transientPath
            |> TransientPath.anchor anchorPath
        | Path.Anchor anchorPath -> anchorPath
    let anchorMetadata (ctx: GeneratorContext) (anchorPath: AnchorPath) (metadata: RenderMetadata) =
        if metadata.Path.IsAnchor then metadata else
        { Path = anchorMetadataPath ctx anchorPath metadata.Path |> Path.create
          Original = metadata.Original
          Source = ValueNone; FullyQualifiedName = ValueNone }
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
                Metadata = {
                    Path = Path.create anchoredPath
                    Original = enumUnion.Metadata.Original
                    Source = enumUnion.Metadata.Source
                    FullyQualifiedName = enumUnion.Metadata.FullyQualifiedName
                }
                Value = enumUnion.Value
                Documentation = enumUnion.Documentation
            }
        let inline anchorUnion (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralUnionRender<'T>) =
            let anchoredPath =
                enumUnion.Metadata.Path
                |> anchorMetadataPath ctx parentPath
            {
                Metadata = {
                    Path = Path.create anchoredPath
                    Original = enumUnion.Metadata.Original
                    Source = enumUnion.Metadata.Source
                    FullyQualifiedName = enumUnion.Metadata.FullyQualifiedName
                }
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
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typeParameter.Metadata.Original
                    Source = typeParameter.Metadata.Source
                    FullyQualifiedName = typeParameter.Metadata.FullyQualifiedName
                }
                TypeParameterRender.Name =
                    typeParameter.Name
                Constraint =
                    typeParameter.Constraint
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Default =
                    typeParameter.Default
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Documentation = typeParameter.Documentation
            }
        let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Transient.TypedNameRender) =
            let anchorPath =
                typedName.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typedName.Metadata.Original
                    Source = typedName.Metadata.Source
                    FullyQualifiedName = typedName.Metadata.FullyQualifiedName
                }
                TypedNameRender.Name = typedName.Name
                Type = typedName.Type |> TypeRefRender.anchorAndLocalise anchorPath 
                Traits = typedName.Traits
                TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = typedName.Documentation
            }
        let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Transient.FunctionLikeSignature) =
            {
                FunctionLikeSignature.Metadata = {
                    Path = Path.create anchorPath
                    Original = functionSignature.Metadata.Original
                    Source = functionSignature.Metadata.Source
                    FullyQualifiedName = functionSignature.Metadata.FullyQualifiedName
                }
                Parameters =
                    functionSignature.Parameters
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                ReturnType =
                    functionSignature.ReturnType
                    |> TypeRefRender.anchorAndLocalise anchorPath
                Traits = functionSignature.Traits
                Documentation = functionSignature.Documentation
                TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
            }
        let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Transient.FunctionLikeRender) =
            let anchorPath =
                functionLike.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = functionLike.Metadata.Original
                    Source = functionLike.Metadata.Source
                    FullyQualifiedName = functionLike.Metadata.FullyQualifiedName
                }
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
                TypeLikeRender.Metadata = {
                    Path = Path.create anchorPath
                    Original = typeDefn.Metadata.Original
                    Source = typeDefn.Metadata.Source
                    FullyQualifiedName = typeDefn.Metadata.FullyQualifiedName
                }
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
                    // An F# interface can only inherit other interfaces. Decide which bases
                    // are interface-shaped (Path atom / generic Prefix) versus scalars
                    // (Widget/Intrinsic from the lib.es substitution, e.g. Error->exn) BEFORE
                    // localising — localise rewrites every atom to a Widget, erasing the
                    // distinction (which silently dropped legitimate NON-generic interface
                    // bases). Keep interface bases, drop scalars, then localise the survivors.
                    typeDefn.Inheritance
                    |> List.map (TypeRefRender.anchor anchorPath)
                    |> List.filter TypeRefRender.isInterfaceBase
                    |> List.map (TypeRefRender.localise anchorPath)
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
                    TypeAliasRenderRef.Metadata = {
                        Path = Path.create anchorPath
                        Original = alias.Metadata.Original
                        Source = alias.Metadata.Source
                        FullyQualifiedName = alias.Metadata.FullyQualifiedName
                    }
                    Name = alias.Name |> ValueOption.defaultWith (fun () ->
                        match anchorPath with
                        | AnchorPath.Type typePath -> typePath.Name
                        | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
                        | _ -> failwith "Unreachable branch"
                        )
                    TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    Type = alias.Type |> TypeRefRender.anchorAndLocalise anchorPath
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
                Anchored.Render( TypeRefRender.anchorAndLocalise anchorPath ref, lazy typeRender )
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
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typeParameter.Metadata.Original
                    Source = typeParameter.Metadata.Source
                    FullyQualifiedName = typeParameter.Metadata.FullyQualifiedName
                }
                TypeParameterRender.Name =
                    typeParameter.Name
                Constraint =
                    typeParameter.Constraint
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Default =
                    typeParameter.Default
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Documentation = typeParameter.Documentation
            }
        let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Concrete.TypedNameRender) =
            let anchorPath =
                typedName.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typedName.Metadata.Original
                    Source = typedName.Metadata.Source
                    FullyQualifiedName = typedName.Metadata.FullyQualifiedName
                }
                TypedNameRender.Name = typedName.Name
                Type = typedName.Type |> TypeRefRender.anchorAndLocalise anchorPath
                Traits = typedName.Traits
                TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = typedName.Documentation
            }
        let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Concrete.FunctionLikeSignature) =
            {
                FunctionLikeSignature.Metadata = {
                    Path = Path.create anchorPath
                    Original = functionSignature.Metadata.Original
                    Source = functionSignature.Metadata.Source
                    FullyQualifiedName = functionSignature.Metadata.FullyQualifiedName
                }
                Parameters =
                    functionSignature.Parameters
                    |> List.map (anchorTypedNameRender ctx anchorPath)
                ReturnType =
                    functionSignature.ReturnType
                    |> TypeRefRender.anchorAndLocalise anchorPath
                Traits = functionSignature.Traits
                Documentation = functionSignature.Documentation
                TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
            }
        let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Concrete.FunctionLikeRender) =
            let anchorPath =
                functionLike.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = functionLike.Metadata.Original
                    Source = functionLike.Metadata.Source
                    FullyQualifiedName = functionLike.Metadata.FullyQualifiedName
                }
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
                TypeLikeRender.Metadata = {
                    Path = Path.create anchorPath
                    Original = typeDefn.Metadata.Original
                    Source = typeDefn.Metadata.Source
                    FullyQualifiedName = typeDefn.Metadata.FullyQualifiedName
                }
                Name = typeDefn.Name
                TypeParameters =
                    typeDefn.TypeParameters
                    |> List.map (anchorTypeParameters ctx anchorPath)
                Inheritance =
                    // An F# interface can only inherit other interfaces. Decide which bases
                    // are interface-shaped (Path atom / generic Prefix) versus scalars
                    // (Widget/Intrinsic from the lib.es substitution, e.g. Error->exn) BEFORE
                    // localising — localise rewrites every atom to a Widget, erasing the
                    // distinction (which silently dropped legitimate NON-generic interface
                    // bases). Keep interface bases, drop scalars, then localise the survivors.
                    typeDefn.Inheritance
                    |> List.map (TypeRefRender.anchor anchorPath)
                    |> List.filter TypeRefRender.isInterfaceBase
                    |> List.map (TypeRefRender.localise anchorPath)
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
                    TypeAliasRenderRef.Metadata = {
                        Path = Path.create anchorPath
                        Original = alias.Metadata.Original
                        Source = alias.Metadata.Source
                        FullyQualifiedName = alias.Metadata.FullyQualifiedName
                    }
                    Name = alias.Name 
                    TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    Type = alias.Type |> TypeRefRender.anchorAndLocalise anchorPath
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
                    |> TypeRefRender.anchorAndLocalise
                    |> funApply ref
                Anchored.Render(ref, lazy typeRender)

let inline addOrReplace (ctx: GeneratorContext) (key: ResolvedType) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
    GeneratorContext.Anchored.addResolvedType ctx key value
    

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
            Root = Choice1Of2 path
            TypeRef =
                renderScope.TypeRef
                |> TypeRefRender.anchor anchorPath
            Render = render
            Anchors = anchors
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
        transientChildren.TypeStore.Keys
        |> Seq.filter (anchors.ContainsKey >> not)
        |> Seq.iter (anchor ctx anchors anchorPath)
    | { Root = ValueSome (TypeLikePath.Anchored path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
        // Canonical hoisted enum (LiteralUnions.<name>): anchored at its OWN absolute path, NOT
        // re-anchored against any referencing owner. Registered directly so collectModules emits
        // it once; addResolvedType is idempotent by ResolvedType so repeated drives are harmless.
        let selfAnchor = AnchorPath.create path
        let render = Render.Transient.anchor ctx selfAnchor renderTuple
        {
            RenderScope.Type = renderScope.Type
            Root = Choice1Of2 path
            TypeRef = renderScope.TypeRef |> TypeRefRender.anchor selfAnchor
            Render = render
            Anchors = Dictionary<ResolvedType, TypePath * Render>()
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
        transientChildren.TypeStore.Keys
        |> Seq.filter (anchors.ContainsKey >> not)
        |> Seq.iter (anchor ctx anchors selfAnchor)
    | { Root = ValueSome (TypeLikePath.Transient path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
        let path = TransientTypePath.anchor anchorPath path
        let render = Render.Transient.anchor ctx anchorPath renderTuple
        // SHARED-LITERAL COUNTING (phase 1 only): record EVERY def-home VISIT for a hoisted
        // object-LITERAL, BEFORE the first-write-wins `tryAdd` below. A literal visited under
        // >1 distinct anchored path is reached through multiple owner contexts — its single def
        // lands under only the first, dangling the rest (the shared deep-nested FS0039 class).
        // `markSharedLiterals` consumes this to assign canonical `SharedLiterals.X` homes.
        ctx.SharedLiteralVisits
        |> ValueOption.iter (fun visits ->
            match renderScope.Type with
            | ResolvedType.TypeLiteral _ ->
                match visits.TryGetValue renderScope.Type with
                | true, homes -> homes.Add path |> ignore
                | _ ->
                    let homes = HashSet<TypePath>()
                    homes.Add path |> ignore
                    visits[renderScope.Type] <- homes
            | _ -> ())
        anchors
        |> Dictionary.tryAdd renderScope.Type (path, render)
        // Recurse into this hoisted literal's OWN nested literals (the two-deep case).
        // Each grandchild's NAMED transient (carrying its leaf, e.g. `Moored(_,"TimeRange")`)
        // is held here as the TypeStore VALUE — the same value that rendered its reference.
        // Anchor each grandchild against THAT stored transient (mirroring
        // `anchorPreludeExportScope`), NOT against the grandchild's own NAMELESS `Anchored`
        // root: re-deriving from the nameless root collapses the grandchild onto THIS node's
        // path (dropping the leaf), so its definition is misplaced/merged into the parent and
        // its `Parent.Leaf` reference dangles. Threading the stored transient lands the def at
        // `<thisPath>.<Leaf>`, restoring emission/reference symmetry.
        let childAnchorPath = AnchorPath.create path
        transientChildren.TypeStore
        |> Seq.filter (fun (KeyValue(key, _)) -> not (anchors.ContainsKey key))
        |> Seq.iter (fun (KeyValue(key, storedTransient)) ->
            let grandchildPath = TransientPath.anchor childAnchorPath (TransientPath.create storedTransient)
            anchor ctx anchors grandchildPath key)
    | { Root = ValueNone; Render = Render.RefOnly typeRef } ->
        typeRef
        |> TypeRefRender.anchor anchorPath
        |> Choice1Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
    | badScope ->
        eprintfn $"Bad scope: %A{badScope}"
and anchorPreludeExportScope (ctx: GeneratorContext) export (renderScopeStore: RenderScopeStore) =
    let anchors = Dictionary<ResolvedType, TypePath * Render>()
    let anchorPath = Interceptors.pipeExport ctx export
    renderScopeStore.TypeStore
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
        let path = Interceptors.pipeClass ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            Class.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Class value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Variable value ->
        let path = Interceptors.pipeVariable ctx value
        let anchorPath = AnchorPath.create path
        let typeRef =
            value.Type
            |> prerender ctx scope
            |> TypeRefRender.anchorAndLocalise anchorPath
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            typeRef |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            {
                Metadata = {
                    Path = Path.create path
                    Original = Path.create path
                    Source = value.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome value.FullyQualifiedName
                }
                TypedNameRender.Name = value.Name
                Type = typeRef
                Traits = Set []
                TypeParameters = []
                Documentation = value.Documentation
            }
            |> TypeRender.Variable
        {
            Type = value.Type.Value
            Root = Choice2Of2 path 
            TypeRef = typeRef
            Render = Anchored.Render( typeRef, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Interface value ->
        let path = Interceptors.pipeInterface ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            Interface.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Interface value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.TypeAlias value ->
        let path = Interceptors.pipeTypeAlias ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            TypeAlias.render ctx scope value
            |> Render.Concrete.anchorTypeAlias ctx
            |> TypeRender.TypeAlias
        {
            Type = value.Type.Value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Enum value ->
        let path = Interceptors.pipeEnum ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
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
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Function [] -> failwith "Empty function list"
    | ResolvedExport.Function (headFunc :: rest) ->
        let path = Interceptors.pipeFunction ctx headFunc
        let anchorPath = AnchorPath.create path
        let ref =
            headFunc.SignatureKey.Value
            |> ResolvedType.TypeLiteral
            |> LazyContainer.CreateTypeKeyDummy<ResolvedType>
            |> prerender ctx scope
            |> TypeRefRender.anchor anchorPath
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors headFunc && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            {
                FunctionLikeRender.Name = headFunc.Name
                Metadata = {
                    Path = Path.create path
                    Original = Path.create path
                    Source = headFunc.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome headFunc.FullyQualifiedName
                }
                Signatures =
                    (headFunc :: rest)
                    |> List.map (fun func ->
                        {
                            FunctionLikeSignature.Metadata = {
                                Path = Path.create path
                                Original = Path.create path
                                Source = func.Source |> Option.toValueOption
                                FullyQualifiedName = ValueSome func.FullyQualifiedName
                            }
                            Parameters =
                                func.Parameters
                                |> List.map (
                                    Parameter.render ctx scope
                                    >> Render.Concrete.anchorTypedNameRender ctx anchorPath
                                    )
                            ReturnType =
                                func.Type
                                |> prerender ctx scope
                                |> TypeRefRender.anchorAndLocalise anchorPath
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
                    |> List.distinct
                Traits = Set []
                TypeParameters =
                    headFunc.TypeParameters
                    |> List.map (fun typeParameter ->
                        let typeParameter = typeParameter.Value
                        let path =
                            path
                            |> TypeParamPath.createOnMember typeParameter.Name
                        {
                            TypeParameterRender.Metadata = {
                                Path = Path.create path
                                Original = Path.create path
                                Source = ValueNone
                                FullyQualifiedName = ValueNone
                            }
                            Name = typeParameter.Name
                            Constraint =
                                typeParameter.Constraint
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchorAndLocalise anchorPath)
                            Default = 
                                typeParameter.Default
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchorAndLocalise anchorPath)
                            Documentation = typeParameter.Documentation
                        }
                        )
                Documentation = headFunc.Documentation
            }
            |> TypeRender.Function
        {
            Type = headFunc.SignatureKey.Value |> ResolvedType.TypeLiteral
            Root = Choice2Of2 path
            TypeRef = ref 
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Module value ->
        value.Exports
        |> List.iter (registerAnchorFromExport ctx)

let private registerExportsForAnchoring (ctx: GeneratorContext) = List.iter (registerAnchorFromExport ctx)

/// Assign canonical `SharedLiterals.<name>` homes to genuinely-shared hoisted object-literals.
///
/// `countingCtx` is a throwaway context that has already run `processExports` with
/// `SharedLiteralVisits = ValueSome _`, so it holds, per literal ResolvedType, the set of distinct
/// anchored def-home paths the literal was visited under. A literal with >1 distinct home is
/// reached through multiple owner contexts (its single def lands under only the first, dangling the
/// rest — the shared deep-nested FS0039 class). Each such literal is given a canonical absolute
/// home in `realCtx.SharedLiteralHomes`, which `prerender` then roots it at (mirroring LiteralUnions).
///
/// Naming is DETERMINISTIC and COLLISION-FREE: the human-readable stem comes from the literal's
/// member names, but distinct ResolvedTypes that share a stem (same property names, different
/// property types) are disambiguated by a stable suffix derived from sorting on the literal's
/// smallest def-home path (proven run-to-run stable). Keyed by ResolvedType identity so the SAME
/// shared literal always maps to ONE name and DISTINCT shared literals never collapse together.
let markSharedLiterals (countingCtx: GeneratorContext) (realCtx: GeneratorContext) =
    countingCtx.SharedLiteralVisits
    |> ValueOption.iter (fun visits ->
        let pathKey (p: TypePath) =
            TypePath.flatten p |> List.map Name.Case.valueOrModified |> String.concat "."
        // Names already assigned (idempotent across calls) — never reassign an rt (keeps the
        // canonical name STABLE) and never reuse a name (keeps DISTINCT literals distinct).
        let usedNames =
            realCtx.SharedLiteralHomes.Values
            |> Seq.map (fun p -> Name.Case.valueOrModified p.Name)
            |> System.Collections.Generic.HashSet
        // Genuinely-shared literals NOT YET assigned: visited under >1 distinct def-home path.
        let shared =
            visits
            |> Seq.choose (fun (KeyValue(rt, homes)) ->
                if homes.Count > 1 && (GeneratorContext.SharedLiterals.tryGetHome realCtx rt |> ValueOption.isNone) then
                    let stem =
                        match rt with
                        | ResolvedType.TypeLiteral tl -> sharedLiteralNameStem tl
                        | _ -> "Lit"
                    // Deterministic representative: the lexicographically-smallest def-home path.
                    let rep = homes |> Seq.map pathKey |> Seq.sort |> Seq.head
                    Some (stem, rep, rt)
                else None)
            // Stable order independent of dictionary enumeration: by stem, then representative path.
            |> Seq.sortBy (fun (stem, rep, _) -> stem, rep)
            |> Seq.toList
        // Assign names, disambiguating against names already in use (this round and prior rounds)
        // by appending the smallest free numeric suffix — deterministic given the stable order.
        for (stem, _rep, rt) in shared do
            let name =
                if usedNames.Add stem then stem
                else
                    let mutable n = 2
                    while not (usedNames.Add $"{stem}_{n}") do n <- n + 1
                    $"{stem}_{n}"
            let typePath = sharedLiteralModule |> TypePath.createWithName (Name.Pascal.create name)
            GeneratorContext.SharedLiterals.addHome realCtx rt typePath)

/// Run the shared-literal counting pass over an explicit export LIST (no interner) and mark the
/// genuinely-shared literals on `ctx`. Drives a throwaway counting context whose
/// `SharedLiteralVisits` collector records every literal def-home VISIT, then assigns canonical
/// `SharedLiterals.<name>` homes to literals visited under >1 distinct home. Used by the
/// interner-driven entry point AND directly by tests that build exports via mocks.
let markSharedLiteralsFromExportList (ctx: GeneratorContext) (exports: ResolvedExport list) =
    let countingCtx =
        { GeneratorContext.Create(ctx.PreludeGetTypeRef, ctx.Customisation) with
            SharedLiteralVisits = ValueSome (Dictionary<ResolvedType, HashSet<TypePath>>()) }
    exports |> List.iter (registerAnchorFromExport countingCtx)
    markSharedLiterals countingCtx ctx

/// Emit the canonical OWNER-INDEPENDENT hoisted types once: literal-union enums at
/// `LiteralUnions.<name>` and shared object-literals at `SharedLiterals.<name>`. They are anchored
/// at their own absolute path, so the owner-driven export pass never registers them. Drive them
/// directly from the prelude cache — each anchored-root + Transient-render prelude scope is
/// registered into AnchorRenders by its self-anchor, so `collectModules` emits it. Interface/Class
/// scopes also have anchored roots but Concrete renders and are emitted through the export pass —
/// they must NOT be re-driven here. Shared by the interner-driven pipeline and the test harness.
let emitCanonicalPreludeScopes (ctx: GeneratorContext) =
    ctx.PreludeRenders.Values
    |> Seq.choose (fun renderScope ->
        match renderScope.Root, renderScope.Render with
        | ValueSome (TypeLikePath.Anchored path), Render.Transient _ -> Some (path, renderScope)
        | _ -> None)
    // Sort by the canonical absolute path so emission order is DETERMINISTIC: `PreludeRenders` is a
    // ResolvedType-keyed dictionary whose iteration order is hash-dependent (and the shared-literal
    // counting pre-pass forces interner lazies that perturb it run-to-run). Anchoring these
    // canonical scopes in path order makes `collectModules` lay out the LiteralUnions/SharedLiterals
    // members identically every run.
    |> Seq.sortBy (fun (path, _) -> TypePath.flatten path |> List.map Name.Case.valueOrModified)
    |> Seq.iter (fun (path, renderScope) ->
        anchorPreludeAnchorScope ctx None (AnchorPath.create path) renderScope)

module ArenaInterner =
    let processExports (ctx: GeneratorContext) (interner: ArenaInterner) =
        // Make the surface's own top-level globals visible to the source-ignore gate
        // (registerAnchorFromExport) so a `typescript`-sourced top-level export emits its
        // full definition at the global root rather than being dropped to a bare ref.
        ctx.TopLevelExports.UnionWith interner.TopLevelExports
        interner.ExportMap
        |> Map.iter (fun _ -> registerExportsForAnchoring ctx)
        emitCanonicalPreludeScopes ctx

    /// Phase 1: drive the full anchoring once on a throwaway context whose `SharedLiteralVisits`
    /// collector records every literal def-home VISIT, then mark the genuinely-shared literals on
    /// the real context. Must run BEFORE the real `processExports`/prerender so `prerender` roots
    /// shared literals at their canonical home on the real pass. The throwaway context is fully
    /// independent (its own prelude/anchor caches) so the count never pollutes the real output.
    let markSharedLiteralsFromExports (ctx: GeneratorContext) (interner: ArenaInterner) =
        let countingCtx =
            { GeneratorContext.Create(ctx.PreludeGetTypeRef, ctx.Customisation) with
                SharedLiteralVisits = ValueSome (Dictionary<ResolvedType, HashSet<TypePath>>()) }
        RenderScope_Prelude.ArenaInterner.prerenderTypeAliases countingCtx interner
        processExports countingCtx interner
        markSharedLiterals countingCtx ctx