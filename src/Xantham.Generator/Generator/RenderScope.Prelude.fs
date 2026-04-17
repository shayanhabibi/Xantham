[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Prelude

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
    let inline executeRender (renderScope: ^T when 'T:(member Render: Lazy<_>)) =
        // renderScope.Render.Value
        // |> ignore
        renderScope
    let valueIsCreated = lazyResolvedType.IsValueCreated
    let cachedRenderValue = GeneratorContext.Prelude.tryGet ctx lazyResolvedType.Value
    // a significant portion of the branching logic will not initially register the
    // type ref before proceeding.
    if valueIsCreated && cachedRenderValue.IsSome then
        let resolvedType = lazyResolvedType.Value
        match cachedRenderValue.Value with
        | Choice2Of3 renderScope ->
            scope
            |> Dictionary.tryAdd resolvedType renderScope.Root
            renderScope.TypeRef
        | Choice1Of3 { TypeRef = typeRef } 
        | Choice3Of3 { TypeRef = typeRef } -> typeRef
    elif valueIsCreated && not(GeneratorContext.Prelude.canFlight ctx lazyResolvedType.Value) then
        RenderScopeStore.TypeRefRender.create scope lazyResolvedType.Value true Types.obj
    else
    let resolvedType = lazyResolvedType.Value
    let inline lift value = RenderScopeStore.TypeRefRender.create scope resolvedType false value
    let inline liftNullable value = RenderScopeStore.TypeRefRender.create scope resolvedType true value
    let inline liftWithNullable nullable value = if nullable then liftNullable value else lift value
    match resolvedType with
    | ResolvedType.GlobalThis ->
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = lift Types.globalThis }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Conditional conditionalType ->
        if [
            conditionalType.True
            conditionalType.False
        ] |> List.contains lazyResolvedType
        then
            {
                Widget.RenderScope.Type = resolvedType
                Widget.RenderScope.TypeRef = liftNullable Types.obj
            }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        else
        let ref =
            [
                conditionalType.True
                conditionalType.False
            ]
            |> List.map (prerender ctx scope)
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        {
            Widget.RenderScope.Type = resolvedType
            Widget.RenderScope.TypeRef = ref
        }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
        
    | ResolvedType.Interface ``interface`` ->
        let scope = RenderScopeStore.create()
        let path = Path.fromInterface ``interface``
        let ref = path |> createConcreteTypeRef
        {
            Concrete.RenderScope.Type = resolvedType
            Root = path
            TypeRef = ref
            Render =
                lazy
                Concrete.Render.Render (
                    ref,
                    Interface.render ctx scope ``interface``
                    |> Concrete.TypeRender.TypeDefn
                )
            TransientChildren = scope
        }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
        |> _.TypeRef
    | ResolvedType.Class ``class`` ->
        let scope = RenderScopeStore.create()
        let path = Path.fromClass ``class``
        let ref = path |> createConcreteTypeRef
        {
            Concrete.RenderScope.Type = resolvedType
            Root = path
            TypeRef = ref
            Render =
                lazy
                Concrete.Render.Render (
                    ref,
                    Class.render ctx scope ``class``
                    |> Concrete.TypeRender.TypeDefn
                )
            TransientChildren = scope
        }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
        |> _.TypeRef
    | ResolvedType.Predicate _ ->
        {
            Widget.RenderScope.Type = resolvedType
            Widget.RenderScope.TypeRef = lift Types.bool
        }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Primitive typeKindPrimitive ->
        {
            Widget.RenderScope.Type = resolvedType
            Widget.RenderScope.TypeRef =
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
            }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Union _ ->
        match ResolvedTypeCategories.create resolvedType with
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            { Widget.RenderScope.Type = resolvedType
              Widget.RenderScope.TypeRef = liftWithNullable nullable Types.obj }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | { Others = []; EnumLike = []; Primitives = primitives; LiteralLike = []; Nullable = nullable } ->
            let ref =
                primitives
                |> List.map (
                    _.AsResolvedType
                    >> Lazy.CreateFromValue
                    >> prerender ctx scope
                    )
                |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            { Widget.RenderScope.Type = resolvedType
              Widget.RenderScope.TypeRef = ref }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
            let ref = prerender ctx scope (Lazy.CreateFromValue t)
            { Widget.RenderScope.Type = resolvedType
              Widget.RenderScope.TypeRef = ref |> TypeRefRender.orNullable nullable }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | { Others = others; LiteralLike = []; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
            {
                Widget.RenderScope.Type = resolvedType
                Widget.RenderScope.TypeRef =
                    seq {
                        for other in others do other.AsResolvedType
                        for primitive in primitives do primitive.AsResolvedType
                        for enum in enumLike do enum.AsResolvedType
                    }
                    |> Seq.map (Lazy.CreateFromValue >> prerender ctx scope)
                    |> Seq.toList
                    |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            }
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
                |> Seq.map (Lazy.CreateFromValue >> prerender ctx scope)
            let path =
                "Literals"
                |> Name.Pascal.create
                |> TransientTypePath.AnchoredAndMoored 
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType nullable path
            {
                Transient.RenderScope.Type = resolvedType
                Root = path
                TypeRef =
                    otherRefs |> Seq.insertAt 0 ref
                    |> Seq.toList
                    |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
                Render = lazy Transient.Render.Render(ref, Union.renderLiterals ctx scope literals)
                TransientChildren = scope
            }
            |> addOrReplaceScope ctx resolvedType
            |> executeRender
            |> _.TypeRef
    | ResolvedType.Intersection intersection ->
        let path = TransientTypePath.Anchored
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        {
            Transient.RenderScope.Type = resolvedType
            Root = path
            TypeRef = ref
            Render = lazy Transient.Render.Render(ref, Intersection.render ctx scope intersection)
            TransientChildren = scope
        }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
        |> _.TypeRef
    | ResolvedType.Literal tsLiteral ->
        let path = TransientTypePath.Anchored
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        {
            Transient.RenderScope.Type = resolvedType
            Root = path
            TypeRef = ref
            Render = lazy Transient.Render.Render(ref, Literal.render ctx scope tsLiteral)
            TransientChildren = scope
        }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
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
        let ref =
            innerResolvedType
            |> prerender ctx scope
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.TypeReference { TypeArguments = typeArguments; Type = innerResolvedType } ->
        let prefix =
            innerResolvedType
            |> prerender ctx scope
        let postfixArguments =
            typeArguments
            |> List.map (prerender ctx scope)
        let ref =
            (prefix, postfixArguments)
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Array innerResolvedType ->
        let ref =
            (
                lift Types.arrayType,
                Lazy.CreateFromValue innerResolvedType
                |> prerender ctx scope
                |> List.singleton
            )
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Enum enumType ->
        let path = Path.fromEnum enumType
        let ref = path |> createConcreteTypeRef
        let scope = RenderScopeStore.create()
        { Concrete.RenderScope.Type = resolvedType
          Root = path
          TypeRef = ref
          Render =
              lazy
              Concrete.Render.Render(
                  ref,
                  Enum.render ctx enumType
                  )
          TransientChildren = scope }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
        |> _.TypeRef
    | ResolvedType.EnumCase enumCase ->
        let path = TransientTypePath.AnchoredAndMoored enumCase.Name
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        { Transient.RenderScope.Type = resolvedType
          Root = path
          TypeRef = ref
          Render = lazy Transient.Render.Render(ref, EnumCase.render ctx scope enumCase)
          TransientChildren = scope }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
        |> _.TypeRef
    | ResolvedType.TypeParameter typeParameter ->
        let ref =
            typeParameter.Name
            |> Name.Case.valueOrModified
            |> Ast.LongIdent
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.ReadOnly innerResolvedType ->
        let ref =
            innerResolvedType
            |> Lazy.CreateFromValue
            |> prerender ctx scope
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Tuple tuple ->
        let ref =
            tuple.Types
            |> List.map (fun tupleElement ->
                tupleElement.Type
                |> prerender ctx scope
                |> TypeRefRender.orNullable tupleElement.IsOptional
                )
            |> List.toArray
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
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
            {
                Widget.RenderScope.Type = resolvedType
                Widget.RenderScope.TypeRef = liftNullable Types.obj
            }
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
            {
                Widget.RenderScope.Type = resolvedType
                Widget.RenderScope.TypeRef = 
                    (parameters, returnValue)
                    |> RenderScopeStore.TypeRefRender.create scope resolvedType false
            }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
        | _, _ ->
            let path = TransientTypePath.Anchored
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
            let scope = RenderScopeStore.create()
            {
                Transient.RenderScope.Type = resolvedType
                Root = path
                TypeRef = ref
                Render =
                    lazy Transient.Render.Render(ref, TypeLiteral.render ctx scope typeLiteral)
                TransientChildren = scope
            }
            |> addOrReplaceScope ctx resolvedType
            |> _.TypeRef
    | ResolvedType.TemplateLiteral templateLiteral ->
        let path = TransientTypePath.Anchored
        scope |> Dictionary.tryAdd resolvedType path
        let scope = RenderScopeStore.create()
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        {
            Transient.RenderScope.Type = resolvedType
            Root = path
            TypeRef = ref
            Render = lazy Transient.Render.Render(ref, TemplateLiteral.render ctx scope templateLiteral)
            TransientChildren = scope
        }
        |> addOrReplaceScope ctx resolvedType
        |> executeRender
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
        let ref =
            ResolvedType.TypeReference typeReference
            |> Lazy.CreateFromValue
            |> prerender ctx scope
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = TypeRefRender.nullable ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef
    | ResolvedType.Substitution substitutionType ->
        let ref = substitutionType.Base |> prerender ctx scope
        { Widget.RenderScope.Type = resolvedType
          Widget.RenderScope.TypeRef = ref }
        |> addOrReplaceScope ctx resolvedType
        |> _.TypeRef

module TestHelper =
    let prerender ctx resolvedType =
        let scope = RenderScopeStore.create()
        prerender ctx scope (Lazy.CreateFromValue resolvedType)
    
type GeneratorContext with
    static member Empty = GeneratorContext.Create prerender