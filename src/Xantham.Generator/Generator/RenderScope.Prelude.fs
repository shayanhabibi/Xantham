[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Prelude

open System.Collections.Generic
open Xantham.Decoder.Types.Graph
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Generator.NamePath

let private createConcreteTypeRef (path: TypePath) =
    RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
    |> RenderScopeStore.TypeRef.Unsafe.createAtom
    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false

let rec prerender (ctx: GeneratorContext) (scope: RenderScopeStore) (lazyResolvedType: LazyResolvedType) =
    let inline addOrReplaceScope ctx resolvedType renderScope =
        GeneratorContext.Prelude.addOrReplace ctx resolvedType renderScope
        renderScope
    // let inline executeRender (renderScope: ^T when 'T:(member Render: Render)) =
    //     // renderScope.Render.Value
    //     // |> ignore
    //     renderScope
    let valueIsCreated = lazyResolvedType.IsValueCreated
    let cachedRenderValue = GeneratorContext.Prelude.tryGet ctx lazyResolvedType.Value
    // a significant portion of the branching logic will not initially register the
    // type ref before proceeding.
    if valueIsCreated && cachedRenderValue.IsSome then
        let resolvedType = lazyResolvedType.Value
        match cachedRenderValue.Value with
        | { Root = ValueSome (TypeLikePath.Transient path); TypeRef = ref } ->
            scope
            |> Dictionary.tryAdd resolvedType path
            ref
        | { TypeRef = ref } ->
            ref
    elif valueIsCreated && not(GeneratorContext.Prelude.canFlight ctx lazyResolvedType.Value) then
        printfn $"Stack overflow would be caused by rendering the type ref for {lazyResolvedType.Raw}"
        RenderScopeStore.TypeRefRender.create scope lazyResolvedType.Value true Types.obj
    else
    let resolvedType = lazyResolvedType.Value
    let inline lift value = RenderScopeStore.TypeRefRender.create scope resolvedType false value
    let inline liftNullable value = RenderScopeStore.TypeRefRender.create scope resolvedType true value
    let inline liftWithNullable nullable value = if nullable then liftNullable value else lift value
    match resolvedType with
    | ResolvedType.GlobalThis ->
        lift Types.globalThis
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Conditional conditionalType ->
        if [
            conditionalType.True
            conditionalType.False
        ] |> List.contains lazyResolvedType
        then
            liftNullable Types.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        else
        [
            conditionalType.True
            conditionalType.False
        ]
        |> List.map (prerender ctx scope)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
        
    | ResolvedType.Interface ``interface`` ->
        let scope = RenderScopeStore.create()
        let path = Path.fromInterface ``interface``
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
        |> _.TypeRef
    | ResolvedType.Class ``class`` ->
        let scope = RenderScopeStore.create()
        let path = Path.fromClass ``class``
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
        |> _.TypeRef
    | ResolvedType.Predicate _ ->
        lift Types.bool
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Primitive typeKindPrimitive ->
        match typeKindPrimitive with
        | TypeKindPrimitive.Unknown 
        | TypeKindPrimitive.Any -> liftNullable Types.obj
        | TypeKindPrimitive.NonPrimitive 
        | TypeKindPrimitive.ESSymbol -> lift Types.obj
        | TypeKindPrimitive.Never 
        | TypeKindPrimitive.Void 
        | TypeKindPrimitive.Undefined 
        | TypeKindPrimitive.Null -> lift Types.unit
        | TypeKindPrimitive.String -> lift Types.string
        | TypeKindPrimitive.Integer -> lift Types.int
        | TypeKindPrimitive.Number -> lift Types.float
        | TypeKindPrimitive.Boolean -> lift Types.bool
        | TypeKindPrimitive.BigInt -> lift Types.bigint
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Union _ ->
        match ResolvedTypeCategories.create resolvedType with
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            liftWithNullable nullable Types.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
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
            |> _.TypeRef
        | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
            prerender ctx scope (LazyContainer.CreateFromValue t)
            |> TypeRefRender.orNullable nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | { Others = others; LiteralLike = []; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
            seq {
                for other in others do other.AsResolvedType
                for primitive in primitives do primitive.AsResolvedType
                for enum in enumLike do enum.AsResolvedType
            }
            |> Seq.map (LazyContainer.CreateFromValue >> prerender ctx scope)
            |> Seq.toList
            |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | { Others = others; LiteralLike = literals; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
            let scope = RenderScopeStore.create()
            let otherRefs =
                seq {
                    for other in others do other.AsResolvedType
                    for primitive in primitives do primitive.AsResolvedType
                    for enum in enumLike do enum.AsResolvedType
                }
                |> Seq.map (LazyContainer.CreateFromValue >> prerender ctx scope)
            let path =
                "Literals"
                |> Name.Pascal.create
                |> TransientTypePath.AnchoredAndMoored 
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType nullable path
            {
                RenderScope.Type = resolvedType
                Root = path |> TypeLikePath.create |> ValueSome
                TypeRef =
                    otherRefs |> Seq.insertAt 0 ref
                    |> Seq.toList
                    |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
                Render =
                    lazy Union.renderLiterals ctx scope literals
                    |> Render.create ref
                TransientChildren = ValueSome scope
            }
            |> addOrReplaceScope ctx resolvedType
            // |> executeRender
            |> _.TypeRef
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
        // |> executeRender
        |> _.TypeRef
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
        // |> executeRender
        |> _.TypeRef
    | ResolvedType.IndexedAccess indexAccessType ->
        indexAccessType.Object
        |> prerender ctx scope
    | ResolvedType.Index index ->
        (
            lift Types.keyofType,
            index.Type
            |> List.singleton
            |> List.map (prerender ctx scope)
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
    | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType } 
    | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = [] } ->
        innerResolvedType
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.TypeReference { TypeArguments = typeArguments; Type = innerResolvedType } ->
        let prefix =
            innerResolvedType
            |> prerender ctx scope
        let postfixArguments =
            typeArguments
            |> List.map (prerender ctx scope)
        (prefix, postfixArguments)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Array innerResolvedType ->
        (
            lift Types.arrayType,
            LazyContainer.CreateFromValue innerResolvedType
            |> prerender ctx scope
            |> List.singleton
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Enum enumType ->
        let path = Path.fromEnum enumType
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
        // |> executeRender
        |> _.TypeRef
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
        // |> executeRender
        |> _.TypeRef
    | ResolvedType.TypeParameter typeParameter ->
        typeParameter.Name
        |> Name.Case.valueOrModified
        |> Ast.LongIdent
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.ReadOnly innerResolvedType ->
        innerResolvedType
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Tuple tuple ->
        tuple.Types
        |> List.map (fun tupleElement ->
            tupleElement.Type
            |> prerender ctx scope
            |> TypeRefRender.orNullable tupleElement.IsOptional
            )
        |> List.toArray
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
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
            liftNullable Types.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | [ [ singleSig ] ], [] when shouldInlineCallSignature singleSig ->
            let parameters =
                singleSig.Parameters
                |> List.map (fun parameter ->
                    prerender ctx scope parameter.Type
                    |> TypeRefRender.orNullable parameter.IsOptional
                    )
            let returnValue = prerender ctx scope singleSig.Type
            (parameters, returnValue)
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
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
            |> _.TypeRef
    | ResolvedType.TemplateLiteral templateLiteral ->
        let path = TransientTypePath.Anchored
        scope |> Dictionary.tryAdd resolvedType path
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
        // |> executeRender
        |> _.TypeRef
    // === FALLBACK PATTERN FOR TEMPLATE LITERAL ===
    // | ResolvedType.TemplateLiteral _ ->
    //     {
    //         Widget.RenderScope.Type = resolvedType
    //         Widget.RenderScope.TypeRef = lift Types.string
    //     }
    //     |> addOrReplaceScope ctx resolvedType
    //     |> _.TypeRef
    | ResolvedType.Optional typeReference ->
        ResolvedType.TypeReference typeReference
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> TypeRefRender.nullable
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Substitution substitutionType ->
        substitutionType.Base |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    |> function
        | { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Value) ->
            ctx.TypeAliasRemap[lazyResolvedType.Value]
            |> TypeRefRender.orNullable nullable
        | ref -> ref
// let prerenderExport (ctx: GeneratorContext) (export: ResolvedExport) =
//     let scope = RenderScopeStore.create()
//     match export with
//     | ResolvedExport.Class value -> failwith "todo"
//     | ResolvedExport.Variable value -> failwith "todo"
//     | ResolvedExport.Interface value -> failwith "todo"
//     | ResolvedExport.TypeAlias value -> failwith "todo"
//     | ResolvedExport.Enum value -> failwith "todo"
//     | ResolvedExport.Function value -> failwith "todo"
//     | ResolvedExport.Module value -> failwith "todo"

module TestHelper =
    let prerender ctx resolvedType =
        let scope = RenderScopeStore.create()
        prerender ctx scope (LazyContainer.CreateFromValue resolvedType)
    
type GeneratorContext with
    static member Empty = GeneratorContext.Create prerender
    
module ArenaInterner =
    let prerenderTypeAliases (ctx: GeneratorContext) (arena: ArenaInterner) =
        arena.ExportMap
        |> Map.iter (fun _ -> List.iter (function
            | ResolvedExport.TypeAlias value ->
                let resolvedType = value.Type.Value
                let path = Path.fromTypeAlias value
                RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
                |> RenderScopeStore.TypeRef.Unsafe.createAtom
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                |> GeneratorContext.Prelude.addTypeAliasRemap ctx resolvedType
            | _ -> ()
            ))
    let getTopologicalSort (_: ArenaInterner) (graph: Graph) =
        let degrees = Dictionary graph.Degrees
        let dependencies =
            graph.Dependents
            |> Seq.map (fun (KeyValue(key, value)) ->
                KeyValuePair(key, HashSet(value)))
            |> Dictionary
        let cycles =
            graph.Cycles
            |> Seq.sortBy (fun (KeyValue(key, value)) -> key = value)
        seq {
            for cycle in cycles do
                if cycle.Value = cycle.Key then
                    degrees[cycle.Value] <- degrees[cycle.Value] - 1
                else degrees[cycle.Value] <- degrees[cycle.Value] - 2
                yield cycle.Value
            while degrees.Count > 0 do
                let (KeyValue(key, _)) = degrees |> Seq.sortBy _.Value |> Seq.head
                match
                    dependencies
                    |> Dictionary.tryItem key
                with
                | ValueSome deps ->
                    for dep in deps do
                        degrees
                        |> Dictionary.tryItem dep
                        |> ValueOption.iter (fun value ->
                            degrees[dep] <- value - 1
                            )
                    yield key
                    dependencies.Remove(key) |> ignore
                    degrees.Remove(key) |> ignore
                | ValueNone ->
                    yield key
                    degrees.Remove(key) |> ignore
        }
        
    let prerenderFromGraph (ctx: GeneratorContext) (interner: ArenaInterner) =
        prerenderTypeAliases ctx interner
        let renderScopes = Dictionary<ResolvedType, RenderScopeStore>()
        let graph = interner.Graph.Value
        let mutable renderScope = RenderScopeStore.create()
        getTopologicalSort interner graph
        |> Seq.iter (fun key ->
            let renderType = interner.ResolveType key
            {
                Data = key
                Result = lazy renderType
            }
            |> prerender ctx renderScope
            |> ignore
            if renderScope.Count <> 0 then
                renderScopes
                |> Dictionary.tryItem renderType
                |> ValueOption.map (fun scope ->
                    for kv in renderScope do
                        scope
                        |> Dictionary.tryAdd kv.Key kv.Value
                    renderScope.Clear()
                    )
                |> ValueOption.defaultWith(fun () ->
                    renderScopes
                    |> Dictionary.tryAdd renderType (Dictionary renderScope)
                    renderScope.Clear()
                    ))
        for kv in renderScopes do
            GeneratorContext.Prelude.tryGet ctx kv.Key
            |> ValueOption.iter (fun renderScope ->
                renderScope.TransientChildren
                |> ValueOption.iter (fun scope ->
                    for kv in kv.Value do
                        scope
                        |> Dictionary.tryAdd kv.Key kv.Value
                    )
                )
        renderScopes.Clear()