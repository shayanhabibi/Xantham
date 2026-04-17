[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Prelude

open Xantham.Generator.Generator.Render_Transient.Union
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open FSharp.SignalsDotnet

module Prelude =
    let globalThis =
        TypeRefRender.create false TypeString.globalThis
        |> RenderScope.createWithDummyStore ResolvedType.GlobalThis
    let string =
        TypeRefRender.create false TypeString.string
        |> RenderScope.createWithDummyStore (ResolvedType.Primitive TypeKindPrimitive.String)
    let float =
        TypeRefRender.create false TypeString.number
        |> RenderScope.createWithDummyStore (ResolvedType.Primitive TypeKindPrimitive.Number)
    let int =
        TypeRefRender.create false TypeString.int
        |> RenderScope.createWithDummyStore (ResolvedType.Primitive TypeKindPrimitive.Integer)
    let bigint =
        TypeRefRender.create false TypeString.bigint
        |> RenderScope.createWithDummyStore (ResolvedType.Primitive TypeKindPrimitive.BigInt)
    let bool =
        TypeRefRender.create false TypeString.bool
        |> RenderScope.createWithDummyStore (ResolvedType.Primitive TypeKindPrimitive.Boolean)
    let boolFor typ =
        TypeRefRender.create false TypeString.bool
        |> RenderScope.createWithDummyStore typ
    let objFor typ =
        TypeRefRender.create false TypeString.obj
        |> RenderScope.createWithDummyStore typ
    let objnullFor typ =
        TypeRefRender.create true TypeString.obj
        |> RenderScope.createWithDummyStore typ
    let unitFor typ =
        TypeRefRender.create false TypeString.unit
        |> RenderScope.createWithDummyStore typ

// only operable from stack
let private initialiseRenderScope (ctx: GeneratorContext) (resolvedType: ResolvedType) =
    match resolvedType with
    | ResolvedType.IndexedAccess _
    | ResolvedType.GlobalThis ->
        Prelude.globalThis
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Primitive typeKindPrimitive ->
        match typeKindPrimitive with
        | TypeKindPrimitive.Unknown
        | TypeKindPrimitive.Any ->
            Prelude.objnullFor resolvedType
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.Never 
        | TypeKindPrimitive.Void 
        | TypeKindPrimitive.Undefined 
        | TypeKindPrimitive.Null ->
            Prelude.unitFor resolvedType
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.String ->
            Prelude.string |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.Integer ->
            Prelude.int |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.Number ->
            Prelude.float |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.Boolean ->
            Prelude.bool |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.BigInt ->
            Prelude.bigint |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | TypeKindPrimitive.ESSymbol 
        | TypeKindPrimitive.NonPrimitive ->
            Prelude.objFor resolvedType
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Conditional conditionalType ->
        let types = [
            conditionalType.True.Value
            conditionalType.False.Value
        ]
        if types |> List.contains resolvedType then
            Prelude.objnullFor resolvedType
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        else
        let typeRef = Signal.compute <| fun _ ->
            types
            |> List.map (GeneratorContext.Prelude.getRender ctx >> _.Value.TypeRef.Value)
            |> TypeRefRender.create false
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Interface ``interface`` ->
        let renderScope =
            let path = Path.fromInterface ``interface``
            path
            |> TypeRefRender.create false
            |> RenderScope.createWithRoot resolvedType path
        GeneratorContext.Prelude.addOrReplace ctx resolvedType renderScope
        let render = Interface.render ctx renderScope.TransientChildren ``interface``
        Signal.batch <| fun _ ->
            {
                renderScope with
                    Render = Render.Render(renderScope.TypeRef.Value, TypeRender.TypeDefn render)
            }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
            
    | ResolvedType.Class ``class`` ->
        let renderScope =
            let path = Path.fromClass ``class``
            path
            |> TypeRefRender.create false
            |> RenderScope.createWithRoot resolvedType path
        renderScope
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        Signal.batch <| fun _ ->
            let typeRender =
                Class.render ctx renderScope.TransientChildren ``class``
                |> TypeRender.TypeDefn
            {
                renderScope with
                    Render = Render.Render(renderScope.TypeRef.Value, typeRender)
            }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
            
    | ResolvedType.Enum enumType ->
        let renderScope =
            let path = Path.fromEnum enumType
            path
            |> TypeRefRender.create false
            |> RenderScope.createWithRoot resolvedType path
        renderScope
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        Signal.batch <| fun _ ->
            let typeRender = Enum.render ctx enumType
            {
                renderScope with Render = Render.Render(renderScope.TypeRef.Value, typeRender)
            }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Union _ ->
        match ResolvedTypeCategories.create resolvedType with
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            let typeRef = TypeRefRender.create nullable TypeString.obj
            RenderScope.create resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { Others = []; EnumLike = []; Primitives = primitives; LiteralLike = []; Nullable = nullable } ->
            let typeRef = Signal.compute <| fun _ ->
                primitives
                |> List.map (_.AsResolvedType >> GeneratorContext.Prelude.getRender ctx >> _.Value.TypeRef.Value)
                |> TypeRefRender.create nullable
            RenderScope.linkWithDummyStore resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable }
        | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable }
        | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable }
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
            let typeRef = Signal.compute <| fun _ ->
                let t = GeneratorContext.Prelude.getRender ctx t
                let typeRef = t.Value.TypeRef.Value
                TypeRefRender.orNullable nullable typeRef
            RenderScope.linkWithDummyStore resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { LiteralLike = literals; EnumLike = []; Primitives = []; Others = []; Nullable = nullable } ->
            let path = TransientTypePath.Anchored
            let typeRef = TypeRefRender.create nullable path
            let renderScope = RenderScope.createWithRoot resolvedType path typeRef
            renderScope
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
            Signal.batch <| fun _ ->
                let typeRender =
                    renderLiterals ctx renderScope.TransientChildren literals
                {
                    renderScope with Render = Render.Render(renderScope.TypeRef.Value, typeRender)
                }
                |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { LiteralLike = literals; EnumLike = enumLike; Primitives = primitives; Others = others; Nullable = nullable } ->
            let literalUnion =
                { Union.Types = literals |> List.map (_.AsResolvedType >> Lazy.CreateFromValue) }
            let literalUnionResolvedType =
                ResolvedType.Union literalUnion
            let typeRef = Signal.compute <| fun _ ->
                let types = [
                    literalUnionResolvedType
                    yield! primitives |> List.map _.AsResolvedType
                    yield! enumLike |> List.map _.AsResolvedType
                    yield! others |> List.map _.AsResolvedType
                ]
                let typeRefs =
                    types
                    |> List.map (GeneratorContext.Prelude.getRender ctx >> _.Value.TypeRef.Value)
                TypeRefRender.create nullable typeRefs
            RenderScope.link resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        
    | ResolvedType.Intersection intersection ->
        let path = TransientTypePath.Anchored
        let typeRef = TypeRefRender.create false path
        let renderScope = RenderScope.createWithRoot resolvedType path typeRef
        renderScope
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        Signal.batch <| fun _ ->
            let typeRender = Intersection.render ctx renderScope.TransientChildren intersection
            {
                renderScope with Render = Render.Render(renderScope.TypeRef.Value, typeRender)
            }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Literal TsLiteral.Null ->
        Prelude.unitFor resolvedType
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Literal tsLiteral ->
        let path = TransientTypePath.Anchored
        let typeRef = TypeRefRender.create false path
        let renderScope = RenderScope.createWithRoot resolvedType path typeRef
        renderScope
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        Signal.batch <| fun _ ->
            let typeRender = Literal.render ctx renderScope.TransientChildren tsLiteral
            { renderScope with Render = Render.Render(renderScope.TypeRef.Value, typeRender) }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        
    | ResolvedType.Index index ->
        let typeRef = Signal.compute <| fun _ ->
            let renderScope = GeneratorContext.Prelude.getRender ctx index.Type.Value
            let prefix = TypeString.keyof |> TypeRefRender.create false
            let arg = renderScope.Value.TypeRef.Value
            (prefix, [ arg ])
            |> TypeRefRender.create false
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.TypeReference typeReference ->
        match typeReference with
        | { ResolvedType = Some (Resolve innerResolvedType) } 
        | { Type = Resolve innerResolvedType; TypeArguments = [] } ->
            let typeRef = Signal.compute <| fun _ ->
                let innerScope = GeneratorContext.Prelude.getRender ctx innerResolvedType
                innerScope.Value.TypeRef.Value
            RenderScope.link resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { Type = Resolve innerResolvedType; TypeArguments = typeArguments } ->
            let typeRef = Signal.compute <| fun _ ->
                let innerScope = GeneratorContext.Prelude.getRender ctx innerResolvedType
                let innerTypeRef = innerScope.Value.TypeRef.Value
                let typeArguments =
                    typeArguments
                    |> List.map (_.Value >> GeneratorContext.Prelude.getRender ctx >> _.Value.TypeRef.Value)
                // prefix type ref render
                (innerTypeRef, typeArguments)
                |> TypeRefRender.create false
            RenderScope.link resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Array innerResolvedType ->
        let typeRef = Signal.compute <| fun _ ->
            let renderScope = GeneratorContext.Prelude.getRender ctx innerResolvedType
            let typeRef = renderScope.Value.TypeRef.Value
            let arrayType = TypeRefRender.create false TypeString.array
            (arrayType, [ typeRef ])
            |> TypeRefRender.create false
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.EnumCase enumCase ->
        let path = TransientTypePath.AnchoredAndMoored enumCase.Name
        let typeRef = TypeRefRender.create false path
        let renderScope = RenderScope.createWithRoot resolvedType path typeRef
        renderScope
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        Signal.batch <| fun _ ->
            let typeRender = EnumCase.render ctx renderScope.TransientChildren enumCase
            { renderScope with Render = Render.Render(renderScope.TypeRef.Value, typeRender) }
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        
    | ResolvedType.TypeParameter typeParameter ->
        let typeRef =
            typeParameter.Name
            |> Name.Case.valueOrModified
            |> TypeRefRender.create false
        RenderScope.create resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.ReadOnly resolvedType ->
        let typeRef =
            Signal.compute <| fun _ ->
                let renderScope = GeneratorContext.Prelude.getRender ctx resolvedType
                let typeRef = renderScope.Value.TypeRef.Value
                typeRef
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Tuple tuple ->
        let typeRef =
            Signal.compute <| fun _ ->
                tuple.Types
                |> List.map (fun tupleElement ->
                    tupleElement.Type.Value
                    |> GeneratorContext.Prelude.getRender ctx
                    |> _.Value.TypeRef.Value
                    |> TypeRefRender.orNullable tupleElement.IsOptional
                    )
                |> List.toArray
                |> TypeRefRender.create false
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Predicate _ ->
        Prelude.boolFor resolvedType
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    // call signature - inline relatively simple types
    | ResolvedType.TypeLiteral { Members = [
        Member.CallSignature [
            { Parameters = [] | [ _ ] | [ _; _ ] as parameters } as callSignature
        ]] } when parameters |> List.forall (_.IsSpread >> not) ->
        let typeRef =
            Signal.compute <| fun _ ->
                let returnRenderScope = GeneratorContext.Prelude.getRender ctx callSignature.Type.Value
                let parameterRefs =
                    callSignature.Parameters
                    |> List.map (fun para ->
                        para.Type.Value
                        |> GeneratorContext.Prelude.getRender ctx
                        |> _.Value.TypeRef.Value
                        |> TypeRefRender.orNullable para.IsOptional)
                let returnTypeRef = returnRenderScope.Value.TypeRef.Value
                (parameterRefs, returnTypeRef)
                |> TypeRefRender.create false
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.TypeLiteral { Members = members } ->
        let path = TransientTypePath.Anchored
        let typeRef = TypeRefRender.create false path
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.TemplateLiteral { Types = []; Text = texts } ->
        let text = texts |> String.concat ""
        let textTypeName = Name.Pascal.create text
        let path = TransientTypePath.AnchoredAndMoored textTypeName
        let typeRef = TypeRefRender.create false path
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.TemplateLiteral templateLiteral ->
        let path = TransientTypePath.Anchored
        let typeRef = TypeRefRender.create false path
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Optional typeReference ->
        match typeReference with
        | { ResolvedType = Some (Resolve innerResolvedType) } 
        | { Type = Resolve innerResolvedType; TypeArguments = [] } ->
            let typeRef = Signal.compute <| fun _ ->
                let innerScope = GeneratorContext.Prelude.getRender ctx innerResolvedType
                let innerTypeRef = innerScope.Value.TypeRef.Value
                TypeRefRender.nullable innerTypeRef
            RenderScope.link resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
        | { Type = Resolve innerResolvedType; TypeArguments = typeArguments } ->
            let typeRef = Signal.compute <| fun _ ->
                let innerScope = GeneratorContext.Prelude.getRender ctx innerResolvedType
                let innerTypeRef = innerScope.Value.TypeRef.Value
                let typeArguments =
                    typeArguments
                    |> List.map (_.Value >> GeneratorContext.Prelude.getRender ctx >> _.Value.TypeRef.Value)
                // prefix type ref render
                (innerTypeRef, typeArguments)
                |> TypeRefRender.create true
            RenderScope.link resolvedType typeRef
            |> GeneratorContext.Prelude.addOrReplace ctx resolvedType
    | ResolvedType.Substitution substitutionType ->
        let renderScope = Signal.compute <| fun _ ->
            let renderScope = GeneratorContext.Prelude.getRender ctx substitutionType.Base.Value
            { renderScope.Value with Type = resolvedType }
        renderScope.Value
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedType

let rec private initialiseExportScope (ctx: GeneratorContext) (resolvedExport: ResolvedExport) =
    match resolvedExport with
    | ResolvedExport.Class ``class`` ->
        let path = Path.fromClass ``class``
        let typeRef = TypeRefRender.create false path
        let resolvedType = ResolvedType.Class ``class``
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.Variable variable ->
        let path = Path.fromVariable variable
        let typ = variable.Type.Value
        let typeRef = typ |> GeneratorContext.Prelude.getRender ctx |> _.Value.TypeRef
        RenderScope.link typ typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.Interface ``interface`` ->
        let path = Path.fromInterface ``interface``
        let typeRef = TypeRefRender.create false path
        let resolvedType = ResolvedType.Interface ``interface``
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.TypeAlias typeAlias ->
        let path = Path.fromTypeAlias typeAlias
        let resolvedType = typeAlias.Type.Value
        let typeRef = TypeRefRender.create false path
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.Enum enumType ->
        let path = Path.fromEnum enumType
        let resolvedType = ResolvedType.Enum enumType
        let typeRef = TypeRefRender.create false path
        RenderScope.createWithRoot resolvedType path typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.Function functions ->
        let head = functions[0]
        let path = Path.fromFunction functions[0]
        let resolvedType = head.SignatureKey.Value |> ResolvedType.TypeLiteral
        let typeRef = GeneratorContext.Prelude.getRender ctx resolvedType |> Signal.map _.TypeRef.Value
        RenderScope.link resolvedType typeRef
        |> GeneratorContext.Prelude.addOrReplace ctx resolvedExport
    | ResolvedExport.Module ``module`` ->
        ``module``.Exports
        |> List.iter (initialiseExportScope ctx)


let run (ctx: GeneratorContext) =
    let mutable resolvedType = ResolvedType.GlobalThis
    while ctx.WorkQueue.TryPop(&resolvedType) do
        initialiseRenderScope ctx resolvedType

let initFromInterner (interner: ArenaInterner) (ctx: GeneratorContext) =
    interner.ExportMap
    |> Seq.collect _.Value
    |> Seq.toArray
    |> Array.iter (initialiseExportScope ctx)



// let rec prerender (ctx: GeneratorContext) (scope: RenderScopeStore) (lazyResolvedType: LazyResolvedType) =
//     let inline addOrReplaceScope ctx resolvedType renderScope =
//         GeneratorContext.Prelude.addOrReplace ctx resolvedType renderScope
//     let lazyResolvedValueIsCreated = lazyResolvedType.IsValueCreated
//     let resolvedType = lazyResolvedType.Value
//     let cachedRenderValue =
//         ctx.PreludeRenders
//         |> DictionarySignal.tryGet resolvedType
//     // a significant portion of the branching logic will not initially register the
//     // type ref before proceeding.
//     let rec impl = function
//         | ResolvedType.GlobalThis ->
//             Prelude.globalThis
//             |> addOrReplaceScope ctx resolvedType
//         | ResolvedType.Conditional conditionalType ->
//             if [
//                 conditionalType.True
//                 conditionalType.False
//             ] |> List.contains lazyResolvedType
//             then
//                 Prelude.objNullFor resolvedType
//                 |> addOrReplaceScope ctx resolvedType
//             else
//             let ref =
//                 Signal.linkWith(fun () ->
//                     [
//                         conditionalType.True
//                         conditionalType.False
//                     ]
//                     |> List.map (prerender ctx scope >> _.Value)
//                     |> TypeRefRender.create false
//                 )
//             {
//                 Type = resolvedType
//                 Root = Prelude.nullRoot
//                 TypeRef = ref
//                 RenderScope.Render = Signal.create <| Render.RefOnly ref
//                 TransientChildren = RenderScopeStore()
//             }
//             |> addOrReplaceScope ctx resolvedType
//             
//         | ResolvedType.Interface ``interface`` ->
//             let scope = RenderScopeStore.create()
//             let path = Path.fromInterface ``interface``
//             let ref = path |> createConcreteTypeRef
//             {
//                 Concrete.RenderScope.Type = resolvedType
//                 Root = path
//                 TypeRef = ref
//                 Render =
//                     lazy
//                     Concrete.Render.Render (
//                         ref,
//                         Interface.render ctx scope ``interface``
//                         |> Concrete.TypeRender.TypeDefn
//                     )
//                 TransientChildren = scope
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Class ``class`` ->
//             let scope = RenderScopeStore.create()
//             let path = Path.fromClass ``class``
//             let ref = path |> createConcreteTypeRef
//             {
//                 Concrete.RenderScope.Type = resolvedType
//                 Root = path
//                 TypeRef = ref
//                 Render =
//                     lazy
//                     Concrete.Render.Render (
//                         ref,
//                         Class.render ctx scope ``class``
//                         |> Concrete.TypeRender.TypeDefn
//                     )
//                 TransientChildren = scope
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Predicate _ ->
//             {
//                 Widget.RenderScope.Type = resolvedType
//                 Widget.RenderScope.TypeRef = lift Types.bool
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Primitive typeKindPrimitive ->
//             {
//                 Widget.RenderScope.Type = resolvedType
//                 Widget.RenderScope.TypeRef =
//                     match typeKindPrimitive with
//                     | TypeKindPrimitive.Unknown 
//                     | TypeKindPrimitive.Any -> liftNullable Types.obj
//                     | TypeKindPrimitive.NonPrimitive 
//                     | TypeKindPrimitive.ESSymbol -> lift Types.obj
//                     | TypeKindPrimitive.Never 
//                     | TypeKindPrimitive.Void 
//                     | TypeKindPrimitive.Undefined 
//                     | TypeKindPrimitive.Null -> lift Types.unit
//                     | TypeKindPrimitive.String -> lift Types.string
//                     | TypeKindPrimitive.Integer -> lift Types.int
//                     | TypeKindPrimitive.Number -> lift Types.float
//                     | TypeKindPrimitive.Boolean -> lift Types.bool
//                     | TypeKindPrimitive.BigInt -> lift Types.bigint
//                 }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Union _ ->
//             match ResolvedTypeCategories.create resolvedType with
//             | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
//                 { Widget.RenderScope.Type = resolvedType
//                   Widget.RenderScope.TypeRef = liftWithNullable nullable Types.obj }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | { Others = []; EnumLike = []; Primitives = primitives; LiteralLike = []; Nullable = nullable } ->
//                 let ref =
//                     primitives
//                     |> List.map (
//                         _.AsResolvedType
//                         >> Lazy.CreateFromValue
//                         >> prerender ctx scope
//                         )
//                     |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
//                 { Widget.RenderScope.Type = resolvedType
//                   Widget.RenderScope.TypeRef = ref }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } 
//             | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable } 
//             | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable } 
//             | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
//                 let ref = prerender ctx scope (Lazy.CreateFromValue t)
//                 { Widget.RenderScope.Type = resolvedType
//                   Widget.RenderScope.TypeRef = ref |> TypeRefRender.orNullable nullable }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | { Others = others; LiteralLike = []; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
//                 {
//                     Widget.RenderScope.Type = resolvedType
//                     Widget.RenderScope.TypeRef =
//                         seq {
//                             for other in others do other.AsResolvedType
//                             for primitive in primitives do primitive.AsResolvedType
//                             for enum in enumLike do enum.AsResolvedType
//                         }
//                         |> Seq.map (Lazy.CreateFromValue >> prerender ctx scope)
//                         |> Seq.toList
//                         |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
//                 }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | { Others = others; LiteralLike = literals; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
//                 let scope = RenderScopeStore.create()
//                 let otherRefs =
//                     seq {
//                         for other in others do other.AsResolvedType
//                         for primitive in primitives do primitive.AsResolvedType
//                         for enum in enumLike do enum.AsResolvedType
//                     }
//                     |> Seq.map (Lazy.CreateFromValue >> prerender ctx scope)
//                 let path =
//                     "Literals"
//                     |> Name.Pascal.create
//                     |> TransientTypePath.AnchoredAndMoored 
//                 let ref = RenderScopeStore.TypeRefRender.create scope resolvedType nullable path
//                 {
//                     Transient.RenderScope.Type = resolvedType
//                     Root = path
//                     TypeRef =
//                         otherRefs |> Seq.insertAt 0 ref
//                         |> Seq.toList
//                         |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
//                     Render = lazy Transient.Render.Render(ref, Union.renderLiterals ctx scope literals)
//                     TransientChildren = scope
//                 }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//         | ResolvedType.Intersection intersection ->
//             let path = TransientTypePath.Anchored
//             let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
//             let scope = RenderScopeStore.create()
//             {
//                 Transient.RenderScope.Type = resolvedType
//                 Root = path
//                 TypeRef = ref
//                 Render = lazy Transient.Render.Render(ref, Intersection.render ctx scope intersection)
//                 TransientChildren = scope
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Literal tsLiteral ->
//             let path = TransientTypePath.Anchored
//             let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
//             let scope = RenderScopeStore.create()
//             {
//                 Transient.RenderScope.Type = resolvedType
//                 Root = path
//                 TypeRef = ref
//                 Render = lazy Transient.Render.Render(ref, Literal.render ctx scope tsLiteral)
//                 TransientChildren = scope
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.IndexedAccess indexAccessType ->
//             indexAccessType.Object
//             |> prerender ctx scope
//         | ResolvedType.Index index ->
//             (
//                 lift Types.keyofType,
//                 index.Type
//                 |> List.singleton
//                 |> List.map (prerender ctx scope)
//             )
//             |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//         | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType } 
//         | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = [] } ->
//             let ref =
//                 innerResolvedType
//                 |> prerender ctx scope
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.TypeReference { TypeArguments = typeArguments; Type = innerResolvedType } ->
//             let prefix =
//                 innerResolvedType
//                 |> prerender ctx scope
//             let postfixArguments =
//                 typeArguments
//                 |> List.map (prerender ctx scope)
//             let ref =
//                 (prefix, postfixArguments)
//                 |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Array innerResolvedType ->
//             let ref =
//                 (
//                     lift Types.arrayType,
//                     impl innerResolvedType
//                     |> List.singleton
//                 )
//                 |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Enum enumType ->
//             let path = Path.fromEnum enumType
//             let ref = path |> createConcreteTypeRef
//             let scope = RenderScopeStore.create()
//             { Concrete.RenderScope.Type = resolvedType
//               Root = path
//               TypeRef = ref
//               Render =
//                   lazy
//                   Concrete.Render.Render(
//                       ref,
//                       Enum.render ctx enumType
//                       )
//               TransientChildren = scope }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.EnumCase enumCase ->
//             let path = TransientTypePath.AnchoredAndMoored enumCase.Name
//             let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
//             let scope = RenderScopeStore.create()
//             { Transient.RenderScope.Type = resolvedType
//               Root = path
//               TypeRef = ref
//               Render = lazy Transient.Render.Render(ref, EnumCase.render ctx scope enumCase)
//               TransientChildren = scope }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.TypeParameter typeParameter ->
//             let ref =
//                 typeParameter.Name
//                 |> Name.Case.valueOrModified
//                 |> Ast.LongIdent
//                 |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.ReadOnly innerResolvedType ->
//             let ref = impl innerResolvedType
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Tuple tuple ->
//             let ref =
//                 tuple.Types
//                 |> List.map (fun tupleElement ->
//                     tupleElement.Type
//                     |> prerender ctx scope
//                     |> TypeRefRender.orNullable tupleElement.IsOptional
//                     )
//                 |> List.toArray
//                 |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.TypeLiteral typeLiteral ->
//             let callSignature, rest =
//                 typeLiteral.Members
//                 |> List.partition _.IsCallSignature
//                 ||> fun sigs rest ->
//                     sigs
//                     |> List.map (function
//                         | Member.CallSignature callSignature -> callSignature
//                         | _ -> failwith "Unreachable guaranteed by guard in partition"
//                         )
//                     , rest
//             let shouldInlineCallSignature (callSignature: CallSignature) =
//                 List.length callSignature.Parameters < 3
//                 &&
//                 callSignature.Parameters
//                 |> List.exists _.IsSpread
//                 |> not
//             match callSignature, rest with
//             | [], [] ->
//                 {
//                     Widget.RenderScope.Type = resolvedType
//                     Widget.RenderScope.TypeRef = liftNullable Types.obj
//                 }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | [ [ singleSig ] ], [] when shouldInlineCallSignature singleSig ->
//                 let parameters =
//                     singleSig.Parameters
//                     |> List.map (fun parameter ->
//                         prerender ctx scope parameter.Type
//                         |> TypeRefRender.orNullable parameter.IsOptional
//                         )
//                 let returnValue = prerender ctx scope singleSig.Type
//                 {
//                     Widget.RenderScope.Type = resolvedType
//                     Widget.RenderScope.TypeRef = 
//                         (parameters, returnValue)
//                         |> RenderScopeStore.TypeRefRender.create scope resolvedType false
//                 }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//             | _, _ ->
//                 let path = TransientTypePath.Anchored
//                 let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
//                 let scope = RenderScopeStore.create()
//                 {
//                     Transient.RenderScope.Type = resolvedType
//                     Root = path
//                     TypeRef = ref
//                     Render =
//                         lazy Transient.Render.Render(ref, TypeLiteral.render ctx scope typeLiteral)
//                     TransientChildren = scope
//                 }
//                 |> addOrReplaceScope ctx resolvedType
//                 |> _.TypeRef
//         | ResolvedType.TemplateLiteral templateLiteral ->
//             let path = TransientTypePath.Anchored
//             scope |> Dictionary.tryAdd resolvedType path
//             let scope = RenderScopeStore.create()
//             let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
//             {
//                 Transient.RenderScope.Type = resolvedType
//                 Root = path
//                 TypeRef = ref
//                 Render = lazy Transient.Render.Render(ref, TemplateLiteral.render ctx scope templateLiteral)
//                 TransientChildren = scope
//             }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         // === FALLBACK PATTERN FOR TEMPLATE LITERAL ===
//         // | ResolvedType.TemplateLiteral _ ->
//         //     {
//         //         Widget.RenderScope.Type = resolvedType
//         //         Widget.RenderScope.TypeRef = lift Types.string
//         //     }
//         //     |> addOrReplaceScope ctx resolvedType
//         //     |> _.TypeRef
//         | ResolvedType.Optional typeReference ->
//             let ref =
//                 ResolvedType.TypeReference typeReference
//                 |> Lazy.CreateFromValue
//                 |> prerender ctx scope
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = TypeRefRender.nullable ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//         | ResolvedType.Substitution substitutionType ->
//             let ref = substitutionType.Base |> prerender ctx scope
//             { Widget.RenderScope.Type = resolvedType
//               Widget.RenderScope.TypeRef = ref }
//             |> addOrReplaceScope ctx resolvedType
//             |> _.TypeRef
//     impl resolvedType
//
// let rec prerenderExport (ctx: GeneratorContext) (export: ResolvedExport) =
//     let addExport (renderScope: Concrete.RenderScope) =
//         GeneratorContext.Prelude.addOrReplace ctx export renderScope
//         renderScope.Render.Value
//         |> ignore
//     match export with
//     | ResolvedExport.Class value ->
//         let scope = RenderScopeStore.create()
//         let path = Path.fromClass value
//         let ref = path |> createConcreteTypeRef
//         {
//             Concrete.RenderScope.Type = ResolvedType.Class value
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 Concrete.Render.Render (
//                     ref,
//                     Class.render ctx scope value
//                     |> Concrete.TypeRender.TypeDefn
//                 )
//             TransientChildren = scope
//         }
//         |> addExport
//     | ResolvedExport.Variable value ->
//         let path = Path.fromVariableType value
//         let scope = RenderScopeStore.create()
//         let ref =
//             value.Type
//             |> prerender ctx scope
//         {
//             Concrete.RenderScope.Type = value.Type.Value
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 {
//                     Concrete.TypedNameRender.Metadata = { Path = Path.create path }
//                     Name = value.Name
//                     Type = ref
//                     Traits = Set.empty
//                     TypeParameters = []
//                     Documentation = value.Documentation
//                 }
//                 |> Concrete.TypeRender.Variable
//                 |> fun typedRender ->
//                     Concrete.Render.Render(ref, typedRender)
//             TransientChildren = scope
//         }
//         |> addExport
//     | ResolvedExport.Interface value ->
//         let path = Path.fromInterface value
//         let ref = path |> createConcreteTypeRef
//         let scope = RenderScopeStore.create()
//         {
//             Concrete.RenderScope.Type = ResolvedType.Interface value
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 Interface.render ctx scope value
//                 |> Concrete.TypeRender.TypeDefn
//                 |> fun typeDefnRender ->
//                     Concrete.Render.Render(ref, typeDefnRender)
//             TransientChildren = scope
//         }
//         |> addExport
//     | ResolvedExport.TypeAlias value ->
//         let path = Path.fromTypeAlias value
//         let ref = path |> createConcreteTypeRef
//         let scope = RenderScopeStore.create()
//         {
//             Concrete.RenderScope.Type = value.Type.Value
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 TypeAlias.render ctx scope value
//                 |> List.head
//                 |> Concrete.TypeRender.TypeAlias
//                 |> fun typeDefnRender ->
//                     Concrete.Render.Render(ref, typeDefnRender)
//             TransientChildren = scope
//         }
//         |> addExport
//     | ResolvedExport.Enum value ->
//         let path = Path.fromEnum value
//         let ref = path |> createConcreteTypeRef
//         {
//             Type = ResolvedType.Enum value
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 Concrete.Render.Render(
//                     ref,
//                     Enum.render ctx value
//                 )
//             TransientChildren = RenderScopeStore.create()
//         }
//         |> addExport
//     | ResolvedExport.Function value ->
//         let path = Path.fromFunctionType value[0]
//         let scope = RenderScopeStore.create()
//         let ref =
//             value[0].SignatureKey.Value
//             |> ResolvedType.TypeLiteral
//             |> Lazy.CreateFromValue
//             |> prerender ctx scope
//         {
//             Type = 
//                 value[0].SignatureKey.Value
//                 |> ResolvedType.TypeLiteral
//             Root = path
//             TypeRef = ref
//             Render =
//                 lazy
//                 Concrete.Render.RefOnly( ref )
//             TransientChildren = scope
//         }
//         |> addExport
//     | ResolvedExport.Module value ->
//         value.Exports
//         |> List.iter (prerenderExport ctx)

module TestHelper =
    let prerender ctx resolvedType =
        GeneratorContext.Prelude.getRender ctx resolvedType
        |> ignore
        run ctx
        GeneratorContext.Prelude.getRender ctx resolvedType
        |> _.Value.TypeRef
    
type GeneratorContext with
    static member Empty = GeneratorContext.Create()