[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Prelude

open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Control
open Xantham.Decoder.Types.Graph
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Fabulous.AST
open Xantham.Generator.NamePath

let private createConcreteTypeRef (path: TypePath) =
    RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
    |> RenderScopeStore.TypeRef.Unsafe.createAtom
    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false

[<Struct>]
type private Registered = Registered of TypeRefRender
let rec prerender (ctx: GeneratorContext) (scope: RenderScopeStore) (lazyResolvedType: LazyResolvedType): TypeRefRender =
    let remap = function
            | { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Value) ->
                ctx.TypeAliasRemap[lazyResolvedType.Value]
                |> TypeRefRender.orNullable nullable
            | ref -> ref
    let inline addOrReplaceScope ctx resolvedType renderScope =
        let renderScope = ctx.Customisation.Interceptors.ResolvedTypePrelude ctx resolvedType renderScope
        GeneratorContext.Prelude.addOrReplace ctx resolvedType renderScope
        Registered (remap renderScope.TypeRef)
    let valueIsCreated = lazyResolvedType.IsValueCreated
    let cachedRenderValue = GeneratorContext.Prelude.tryGet ctx lazyResolvedType.Value
    // a significant portion of the branching logic will not initially register the
    // type ref before proceeding.
    if valueIsCreated && cachedRenderValue.IsSome then
        let resolvedType = lazyResolvedType.Value
        match cachedRenderValue.Value with
        | { Root = ValueSome (TypeLikePath.Transient path); TypeRef = ref } ->
            scope
            |> RenderScopeStore.tryAdd resolvedType path
            remap ref
            
        | { TypeRef = ref } ->
            remap ref
    // a first visit to a type will either see the 'resolved type' as having been
    // lazily created but not yet processed (so it will not have a value in the
    // cache), or not created and not yet processed.
    // We protect against stack overflows by registering resolved types that have been
    // created on the first pass into the 'InFlight' set. This prevents infinite recursion.
    elif valueIsCreated && not(GeneratorContext.Prelude.canFlight ctx lazyResolvedType.Value) then
        printfn $"Stack overflow would be caused by rendering the type ref for {lazyResolvedType.Raw}"
        let (Registered ref) =
            RenderScopeStore.TypeRefRender.create scope lazyResolvedType.Value true Intrinsic.obj
            |> RenderScope.createRootless lazyResolvedType.Value
            |> addOrReplaceScope ctx lazyResolvedType.Value
        ref
    else
    let resolvedType = lazyResolvedType.Value
    let inline lift value = RenderScopeStore.TypeRefRender.create scope resolvedType false value
    let inline liftNullable value = RenderScopeStore.TypeRefRender.create scope resolvedType true value
    let inline liftWithNullable nullable value = if nullable then liftNullable value else lift value
    match resolvedType with
    | ResolvedType.GlobalThis ->
        lift Intrinsic.globalThis
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Conditional conditionalType ->
        if [
            conditionalType.True
            conditionalType.False
        ] |> List.contains lazyResolvedType
        then
            liftNullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        else
        [
            conditionalType.True
            conditionalType.False
        ]
        |> List.map (prerender ctx scope)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        
    | ResolvedType.Interface ``interface`` ->
        let scope = RenderScopeStore.create()
        let path = Path.Interceptors.pipeInterface ctx ``interface``
        let ref = path |> createConcreteTypeRef
        {
            RenderScope.Type = resolvedType
            Root = TypeLikePath.create path |> ValueSome
            TypeRef = ref
            Render =
                lazy
                    Interface.render ctx scope ``interface``
                    |> Concrete.TypeRender.TypeDefn
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
        // |> executeRender
    | ResolvedType.Class ``class`` ->
        let scope = RenderScopeStore.create()
        let path = Path.Interceptors.pipeClass ctx ``class``
        let ref = path |> createConcreteTypeRef
        {
            RenderScope.Type = resolvedType
            Root = path |> TypeLikePath.create |> ValueSome
            TypeRef = ref
            Render =
                lazy
                    Class.render ctx scope ``class``
                    |> Concrete.TypeRender.TypeDefn
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
        // |> executeRender
    | ResolvedType.Predicate _ ->
        lift Intrinsic.bool
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Primitive typeKindPrimitive ->
        match typeKindPrimitive with
        | TypeKindPrimitive.Unknown 
        | TypeKindPrimitive.Any -> liftNullable Intrinsic.obj
        | TypeKindPrimitive.NonPrimitive 
        | TypeKindPrimitive.ESSymbol -> lift Intrinsic.obj
        | TypeKindPrimitive.Never 
        | TypeKindPrimitive.Void 
        | TypeKindPrimitive.Undefined 
        | TypeKindPrimitive.Null -> lift Intrinsic.unit
        | TypeKindPrimitive.String -> lift Intrinsic.string
        | TypeKindPrimitive.Integer -> lift Intrinsic.int
        | TypeKindPrimitive.Number -> lift Intrinsic.float
        | TypeKindPrimitive.Boolean -> lift Intrinsic.bool
        | TypeKindPrimitive.BigInt -> lift Intrinsic.bigint
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Union _ ->
        match ResolvedTypeCategories.create resolvedType with
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            liftWithNullable nullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | { Others = []; EnumLike = []; Primitives = primitives; LiteralLike = []; Nullable = nullable } ->
            primitives
            |> List.map (
                _.AsResolvedType
                >> LazyContainer.CreateFromValue
                >> prerender ctx scope
                )
            |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
            prerender ctx scope (LazyContainer.CreateFromValue t)
            |> TypeRefRender.orNullable nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        // direct type render of literals union
        | { LiteralLike = literals; Others = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            let path =
                if lazyResolvedType.Raw = LazyContainer<_, _>.DummyTypeKey then
                    Name.Pascal.create "Literals"
                    |> TransientTypePath.AnchoredAndMoored 
                else
                    TransientTypePath.Anchored
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType nullable path
            {
                Transient.RenderScope.Type = resolvedType
                Root = TypeLikePath.create path |> ValueSome
                TypeRef = ref
                Render =
                    lazy Union.renderLiterals ctx scope literals
                    |> Render.create ref
                TransientChildren = ValueSome <| RenderScopeStore.create()
            }
            |> addOrReplaceScope ctx resolvedType
        | { Others = others; LiteralLike = literals; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
            seq {
                if not <| List.isEmpty literals then
                    { Union.Types =
                        literals
                        |> List.map (_.AsResolvedType >> LazyContainer.CreateTypeKeyDummy<ResolvedType>) }
                    |> ResolvedType.Union
                for other in others do other.AsResolvedType
                for primitive in primitives do primitive.AsResolvedType
                for enum in enumLike do enum.AsResolvedType
            }
            |> Seq.map (LazyContainer.CreateFromValue >> prerender ctx scope)
            |> Seq.toList
            |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Intersection intersection ->
        let path = TransientTypePath.Anchored
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        {
            Transient.RenderScope.Type = resolvedType
            Root = TypeLikePath.create path |> ValueSome
            TypeRef = ref
            Render =
                lazy Intersection.render ctx scope intersection
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Literal tsLiteral ->
        let path = TransientTypePath.Anchored
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        {
            RenderScope.Type = resolvedType
            Root = path |> TypeLikePath.create |> ValueSome
            TypeRef = ref
            Render =
                lazy Literal.render ctx scope tsLiteral
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.IndexedAccess indexAccessType ->
        let suffixes =
            [
                indexAccessType.Object
                indexAccessType.Index
            ]
            |> List.map (prerender ctx scope)
        let prefix = lift Intrinsic.proptypekey
        (prefix, suffixes)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Index index ->
        (
            lift Intrinsic.keyof,
            index.Type
            |> List.singleton
            |> List.map (prerender ctx scope)
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType }
    | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = [] } ->
        innerResolvedType
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { TypeArguments = typeArguments; Type = innerResolvedType } ->
        let innerResolvedTypeValue = innerResolvedType.Value
        let declaredParamCount =
            match innerResolvedTypeValue with
            | ResolvedType.Interface i -> i.TypeParameters.Length
            | ResolvedType.Class c -> c.TypeParameters.Length
            | _ -> typeArguments.Length
        // Encoder/TS may produce a TypeReference whose argument count doesn't
        // match the inner type's declared type parameter count — happens with
        // declaration merging where the encoder can't disambiguate which
        // declaration's arity to honour (e.g. lib.dom Body has 0 type params
        // but a downstream class `extends Body<X>` was authored against a
        // merged extension). Align the args to the declaration's arity so the
        // emitted F# is well-formed: truncate excess args, pad missing args
        // with `obj`. Both directions log a warning so the upstream encoder
        // discrepancy stays visible.
        let alignedArguments =
            if typeArguments.Length = declaredParamCount then
                typeArguments
            elif typeArguments.Length > declaredParamCount then
                printfn "Warning: TypeReference application has %d arguments but %A declares %d type parameters; truncating to declared arity"
                    typeArguments.Length innerResolvedTypeValue declaredParamCount
                typeArguments |> List.truncate declaredParamCount
            else
                printfn "Warning: TypeReference application has %d arguments but %A declares %d type parameters; padding with obj to declared arity"
                    typeArguments.Length innerResolvedTypeValue declaredParamCount
                let padCount = declaredParamCount - typeArguments.Length
                let objArg =
                    LazyContainer.CreateFromValue
                        (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive)
                typeArguments @ List.replicate padCount objArg
        if List.isEmpty alignedArguments then
            innerResolvedType
            |> prerender ctx scope
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        else
        let prefix =
            innerResolvedType
            |> prerender ctx scope
        let postfixArguments =
            alignedArguments
            |> List.map (prerender ctx scope)
        (prefix, postfixArguments)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Array innerResolvedType ->
        (
            lift Intrinsic.array,
            LazyContainer.CreateFromValue innerResolvedType
            |> prerender ctx scope
            |> List.singleton
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Enum enumType ->
        let path = Path.Interceptors.pipeEnum ctx enumType
        let ref = path |> createConcreteTypeRef
        let scope = RenderScopeStore.create()
        { RenderScope.Type = resolvedType
          Root = path |> TypeLikePath.create |> ValueSome
          TypeRef = ref
          Render =
              lazy Enum.render ctx enumType
              |> Render.create ref
          TransientChildren = ValueSome scope }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.EnumCase enumCase ->
        let path = TransientTypePath.AnchoredAndMoored enumCase.Name
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        { RenderScope.Type = resolvedType
          Root = TypeLikePath.create path |> ValueSome
          TypeRef = ref
          Render =
              lazy EnumCase.render ctx scope enumCase
              |> Render.create ref
          TransientChildren = ValueSome scope }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeParameter typeParameter ->
        typeParameter.Name
        |> Name.Case.valueOrModified
        |> Ast.LongIdent
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.ReadOnly innerResolvedType ->
        innerResolvedType
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Tuple tuple ->
        tuple.Types
        |> List.mapi (fun idx ->
            _.Type
            >> prerender ctx scope
            >> TypeRefRender.orNullable tuple.Types[idx].IsOptional
            )
        |> List.toArray
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeLiteral typeLiteral ->
        let callSignature, rest =
            typeLiteral.Members
            |> List.partition _.IsCallSignature
            ||> fun sigs rest ->
                sigs
                |> List.map (function
                    | Member.CallSignature callSignature -> callSignature
                    | _ -> failwith "Unreachable guaranteed by guard in partition"
                    )
                , rest
        let shouldInlineCallSignature (callSignature: CallSignature) =
            List.length callSignature.Parameters < 3
            &&
            callSignature.Parameters
            |> List.exists _.IsSpread
            |> not
        match callSignature, rest with
        | [], [] ->
            liftNullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | [ [ singleSig ] ], [] when shouldInlineCallSignature singleSig ->
            let parameters =
                singleSig.Parameters
                |> List.mapi (fun idx ->
                    _.Type
                    >> prerender ctx scope
                    >> TypeRefRender.orNullable singleSig.Parameters[idx].IsOptional)
            let returnValue = prerender ctx scope singleSig.Type
            (parameters, returnValue)
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | _, _ ->
            let path = TransientTypePath.Anchored
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
            let scope = RenderScopeStore.create()
            {
                RenderScope.Type = resolvedType
                Root = path |> TypeLikePath.create |> ValueSome
                TypeRef = ref
                Render =
                    lazy TypeLiteral.render ctx scope typeLiteral
                    |> Render.create ref
                TransientChildren = ValueSome scope
            }
            |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TemplateLiteral templateLiteral ->
        let path = TransientTypePath.Anchored
        scope |> RenderScopeStore.tryAdd resolvedType path
        let scope = RenderScopeStore.create()
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        {
            RenderScope.Type = resolvedType
            Root = path |> TypeLikePath.create |> ValueSome
            TypeRef = ref
            Render =
                lazy TemplateLiteral.render ctx scope templateLiteral
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Optional typeReference ->
        ResolvedType.TypeReference typeReference
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> TypeRefRender.nullable
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeQuery typeQuery ->
        prerender ctx scope typeQuery.Type
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Substitution substitutionType ->
        substitutionType.Base |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    |> function
        | Registered { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Value) ->
            ctx.TypeAliasRemap[lazyResolvedType.Value]
            |> TypeRefRender.orNullable nullable
        | Registered ref -> ref

module TestHelper =
    let prerender ctx resolvedType =
        let scope = RenderScopeStore.create()
        prerender ctx scope (LazyContainer.CreateFromValue resolvedType)
    
type GeneratorContext with
    static member Empty = GeneratorContext.Create prerender
    static member EmptyWithCustomisation customisation = GeneratorContext.Create(prerender, Customisation.Create customisation)
    
module ArenaInterner =
    /// True when a resolved type is memoised by the interner to a single shared
    /// instance (primitives, literals, globalThis). Such a type must NOT be used
    /// as a `TypeAliasRemap` key: the remap substitutes the alias name wherever
    /// that resolved type occurs, so aliasing a shared instance (e.g.
    /// `type D1SessionBookmark = string`, `type Mode = "primary-only"`) would make
    /// *every* occurrence of that primitive/literal render as the alias name across
    /// the entire surface. Only nominal/structural alias bodies are safe to remap.
    let isShareableAliasBody =
        function
        | ResolvedType.Primitive _
        | ResolvedType.Literal _
        | ResolvedType.GlobalThis -> true
        | _ -> false

    let prerenderTypeAliases (ctx: GeneratorContext) (arena: ArenaInterner) =
        // Iterate the export-key-keyed resolved exports (populated eagerly when the arena is
        // built) rather than ExportMap, so each alias's EXPORT key is in hand.
        arena.ResolvedExports
        |> Seq.iter (fun (KeyValue(exportKey, export)) ->
            match export with
            | ResolvedExport.TypeAlias value when not (isShareableAliasBody value.Type.Value) ->
                let path = Path.Interceptors.pipeTypeAlias ctx value
                let aliasRef =
                    RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
                    |> RenderScopeStore.TypeRef.Unsafe.createAtom
                    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                // (1) Remap the alias BODY instance (`value.Type.Value`) — this is the instance
                //     produced when the body's structural type is rendered directly.
                GeneratorContext.Prelude.addTypeAliasRemap ctx value.Type.Value aliasRef
                // (2) After the extractor's self-reference decoupling, the alias body lives at a
                //     distinct (generated) key, so a union member that references the alias by its
                //     EXPORT key resolves (via `ResolveType exportKey`) to an instance that the
                //     body-keyed remap above does NOT catch. Register that instance too, so members
                //     referencing the alias by export key render as the alias name
                //     (e.g. `U2<ResponseInputText, ResponseInputImage>`) instead of re-emitting the
                //     raw structural body. The alias's OWN definition is protected from self-remap
                //     by Render.TypeAlias.resolveInnerRef, which replaces the remapped name back
                //     with the real body render. Skip shareable (primitive/literal) export-key
                //     instances. ResolveType is the plain (memoised) resolver — no deep structural
                //     forcing — so this cannot stall on recursive graphs.
                let exportKeyType = arena.ResolveType exportKey
                if not (isShareableAliasBody exportKeyType) then
                    GeneratorContext.Prelude.addTypeAliasRemap ctx exportKeyType aliasRef
            | _ -> ())
    let private getTopologicalSort (_: ArenaInterner) (graph: Graph) =
        let degrees = ConcurrentDictionary graph.Degrees
        let dependencies =
            graph.Dependents
            |> Seq.map (fun (KeyValue(key, value)) ->
                KeyValuePair(key, HashSet(value)))
            |> Dictionary
        let cycles =
            graph.Cycles
            |> Seq.sortBy (fun (KeyValue(key, value)) -> key = value)
        
        taskSeq {
            for cycle in cycles do
                if cycle.Value = cycle.Key then
                    degrees[cycle.Value] <- degrees[cycle.Value] - 1
                else degrees[cycle.Value] <- degrees[cycle.Value] - 2
                yield cycle.Value
            while degrees.Count > 0 do
                let (KeyValue(key, _)) = degrees |> Seq.sortBy _.Value |> Seq.head
                match dependencies.TryGetValue key with
                | true, deps ->
                    for dep in deps do
                        match degrees.TryGetValue dep with
                        | true, value -> degrees[dep] <- value - 1
                        | _ -> ()
                    yield key
                    dependencies.Remove(key) |> ignore
                    degrees.Remove(key) |> ignore
                | _ ->
                    yield key
                    degrees.Remove(key) |> ignore
        }
        
    /// <summary>
    /// Performs prerendering of all types in the graph - the series of operations are performed in topological order,
    /// and provides guarantees of passing in deep transient filled type graphs such as with solid-js.
    /// The costs may outweigh the benefits in this scenario. Performance costs are significant.
    /// </summary>
    /// <remarks>
    /// <list type="number">
    /// <item><description>Type aliases from the export map in the <c>ArenaInterner</c> are used to seed
    /// the type reference map - TypeAliases are encapsulations of the contained type, so we must register
    /// the encapsulation before we encounter the types to ensure that we do not render the reference generated
    /// from the underlying type.</description></item>
    /// <item><description>The graph evaluation is forced from the lazy function in the <c>ArenaInterner</c>.</description></item>
    /// <item><description>The graph is traversed in topological order by first yielding the cyclical keys of a graph,
    /// before yielding keys in the order of the number dependencies they have such that a type with no dependencies is registered
    /// first.</description></item>
    /// </list>
    /// </remarks>
    let prerenderFromGraph (ctx: GeneratorContext) (interner: ArenaInterner) =
        prerenderTypeAliases ctx interner
        let renderScopes = ConcurrentDictionary<ResolvedType, RenderScopeStore>()
        let graph = interner.Graph.Value
        getTopologicalSort interner graph
        |> TaskSeq.iter (fun key ->
            let renderScope = RenderScopeStore.create()
            let renderType = interner.ResolveType key
            {
                Data = key
                Result = lazy renderType
            }
            |> prerender ctx renderScope
            |> ignore
            if renderScope.TypeStore.Count <> 0 then
                match renderScopes.TryGetValue renderType with
                | true, scope ->
                    for kv in renderScope.TypeStore do
                        scope.TypeStore.TryAdd(kv.Key, kv.Value) |> ignore
                | _ ->
                    renderScopes.TryAdd(renderType, renderScope) |> ignore
            )
        |> _.Wait()
        for kv in renderScopes do
            GeneratorContext.Prelude.tryGet ctx kv.Key
            |> ValueOption.iter (fun renderScope ->
                renderScope.TransientChildren
                |> ValueOption.iter (fun scope ->
                    for kv in kv.Value.TypeStore do
                        scope
                        |> RenderScopeStore.tryAdd kv.Key kv.Value
                    )
                )
        renderScopes.Clear()