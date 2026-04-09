module Xantham.Generator.Generator.Entry

open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Xantham.Decoder

type ResolvedTypeCategories = {
    EnumLike: ResolvedType list
    LiteralLike: ResolvedType list
    Primitives: ResolvedType list
    Others: ResolvedType list
    Nullable: bool
}

module ResolvedTypeCategories =
    let empty = { EnumLike = []
                  LiteralLike = []
                  Primitives = []
                  Others = []
                  Nullable = false }
    let addOthers categories resolved = { categories with Others = resolved :: categories.Others }
    let addPrimitives categories resolved = { categories with Primitives = resolved :: categories.Primitives }
    let addLiteralLike categories resolved = { categories with LiteralLike = resolved :: categories.LiteralLike }
    let addEnumLike categories resolved = { categories with EnumLike = resolved :: categories.EnumLike }
    let nullable categories = { categories with ResolvedTypeCategories.Nullable = true }
    let unify a b =
        let literals =
            (a.LiteralLike @ b.LiteralLike)
            |> List.distinct
        let primitives =
            (a.Primitives @ b.Primitives)
            |> List.distinct
        let others =
            (a.Others @ b.Others)
            |> List.distinct
        let enums =
            (a.EnumLike @ b.EnumLike)
            |> List.distinct
        {
            EnumLike = enums
            LiteralLike = literals
            Primitives = primitives
            Others = others
            Nullable = a.Nullable || b.Nullable
        }
        
open ResolvedTypeCategories

let categorize (resolved: ResolvedType) =
    let rec categorize (categories: ResolvedTypeCategories) (resolved: ResolvedType) =
        match resolved with
        // we ignore these and just make the entire structure nullable
        | ResolvedType.Primitive (
            TypeKindPrimitive.Never
           | TypeKindPrimitive.Undefined
           | TypeKindPrimitive.Null
           | TypeKindPrimitive.Void
            ) -> nullable categories
        // make the structure nullable, and remap to a non-nullable primitive
        // with the same intent
        | ResolvedType.Primitive (TypeKindPrimitive.Unknown | TypeKindPrimitive.Any) ->
            nullable categories
            |> addPrimitives
            |> funApply (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive)
        // unify primitives with the same intent to a single primitive type
        // any (already handled); essymbol; nonprimitive; unknown (already handled)
        | ResolvedType.Primitive (
            TypeKindPrimitive.ESSymbol
           | TypeKindPrimitive.NonPrimitive
            ) ->
            ResolvedType.Primitive TypeKindPrimitive.NonPrimitive
            |> addPrimitives categories
        // can now safely intake any other primitive
        | ResolvedType.Primitive _ ->
            addPrimitives categories resolved
            
        | ResolvedType.Conditional conditionalType ->
            [
                conditionalType.True.Value
                conditionalType.False.Value
            ]
            |> List.fold categorize categories
        | ResolvedType.Union union ->
            union.Types
            |> List.map _.Value
            |> List.fold categorize categories
        | ResolvedType.Optional { ResolvedType = Some (Resolve value) } 
        | ResolvedType.Optional { Type = Resolve value } ->
            categorize (nullable categories) value
        | ResolvedType.TypeReference { ResolvedType = Some (Resolve value) } 
        | ResolvedType.TypeParameter { Constraint = Some (Resolve value) } 
        | ResolvedType.ReadOnly value
        | ResolvedType.TypeReference { Type = Resolve value } ->
            categorize categories value
        // treat like a bool
        | ResolvedType.Predicate _ ->
            addPrimitives categories resolved
        | ResolvedType.IndexedAccess _ 
        | ResolvedType.Intersection _ 
        | ResolvedType.Class _ 
        | ResolvedType.GlobalThis 
        | ResolvedType.TypeLiteral _
        | ResolvedType.Array _
        | ResolvedType.TypeParameter _
        | ResolvedType.Tuple _
        | ResolvedType.Interface _ ->
            addOthers categories resolved
        | ResolvedType.Enum _
        | ResolvedType.Index _ ->
            addEnumLike categories resolved
        | ResolvedType.EnumCase _
        | ResolvedType.TemplateLiteral _
        | ResolvedType.Literal _ ->
            addLiteralLike categories resolved
        | ResolvedType.Substitution substitutionType ->
            categorize categories substitutionType.Base.Value
    let cat = categorize empty resolved
    {
        Primitives = cat.Primitives |> Seq.distinct |> Seq.rev |> Seq.toList
        LiteralLike = cat.LiteralLike |> Seq.distinct |> Seq.rev |> Seq.toList
        EnumLike = cat.EnumLike |> Seq.distinct |> Seq.rev |> Seq.toList
        Others = cat.Others |> Seq.distinct |> Seq.rev |> Seq.toList
        Nullable = cat.Nullable
    }
    

module RefRenderPhase =
    let rec typeRender (ctx: GeneratorContext) (resolved: ResolvedType) =
        let inline create value = TypeRefRender.create false value
        let inline createOptional value = TypeRefRender.create true value
        match resolved with
        | ResolvedType.GlobalThis ->
            ModulePath.init "Browser"
            |> ModulePath.create "Dom"
            |> TypePath.create "Window"
            |> create
        | ResolvedType.Conditional conditionalType ->
            [
                refTypeRender ctx conditionalType.True.Value
                refTypeRender ctx conditionalType.False.Value
            ]
            |> create
        | ResolvedType.Interface ``interface`` ->
            Path.fromInterface ``interface``
            |> create
        | ResolvedType.Class ``class`` ->
            Path.fromClass ``class``
            |> create
        | ResolvedType.Primitive typeKindPrimitive ->
            match typeKindPrimitive with
            | TypeKindPrimitive.Unknown 
            | TypeKindPrimitive.Any ->
                Types.obj
                |> createOptional
            | TypeKindPrimitive.ESSymbol 
            | TypeKindPrimitive.NonPrimitive ->
                Types.obj
                |> create
            | TypeKindPrimitive.Never 
            | TypeKindPrimitive.Void 
            | TypeKindPrimitive.Undefined 
            | TypeKindPrimitive.Null -> create Types.unit 
            | TypeKindPrimitive.String -> create Types.string 
            | TypeKindPrimitive.Integer -> create Types.int 
            | TypeKindPrimitive.Number -> create Types.float 
            | TypeKindPrimitive.Boolean -> create Types.bool 
            | TypeKindPrimitive.BigInt -> create Types.bigint 
        | ResolvedType.Union _ ->
            match categorize resolved with
            | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [] } ->
                Types.obj
                |> createOptional
            | { Others = []; EnumLike = []; Primitives = [ i ]; LiteralLike = []; Nullable = true } 
            | { Others = []; EnumLike = [ i ]; Primitives = []; LiteralLike = []; Nullable = true } 
            | { Others = [ i ]; EnumLike = []; Primitives = []; LiteralLike = []; Nullable = true } ->
                refTypeRender ctx i
                |> TypeRefRender.nullable
            | { Others = []; EnumLike = []; Primitives = []; LiteralLike = _; Nullable = isNullable } ->
                TransientTypePath.Anchored
                |> TypeRefRender.create isNullable
            | { Others = others; EnumLike = enums; Primitives = primitives; LiteralLike = []; Nullable = isNullable } ->
                primitives @ others @ enums
                |> List.map (refTypeRender ctx >> if isNullable then TypeRefRender.nonNullable else id)
                |> TypeRefRender.create isNullable
            | { Others = others; EnumLike = enums; Primitives = primitives; LiteralLike = literals; Nullable = isNullable } ->
                let refs =
                    primitives @ others @ enums @ literals
                    |> List.map (refTypeRender ctx >> if isNullable then TypeRefRender.nonNullable else id)
                create TransientTypePath.Anchored :: refs
                |> TypeRefRender.create isNullable
        | ResolvedType.Literal _
        | ResolvedType.IndexedAccess _
        | ResolvedType.Intersection _ ->
            create TransientTypePath.Anchored
        | ResolvedType.Index index ->
            (TypeRefRender.create false Types.keyofType, [ refTypeRender ctx index.Type.Value ])
            |> TypeRefRender.create false
        | ResolvedType.TypeReference { ResolvedType = Some (Resolve resolvedType) }
        | ResolvedType.TypeReference { Type = Resolve resolvedType; TypeArguments = [] } ->
            refTypeRender ctx resolvedType
        | ResolvedType.TypeReference typeReference ->
            let prefix = refTypeRender ctx typeReference.Type.Value
            let args = typeReference.TypeArguments |> List.map (_.Value >> refTypeRender ctx)
            create (prefix, args)
        | ResolvedType.Array resolvedType ->
            create (create Types.arrayType, [ refTypeRender ctx resolvedType ])
        | ResolvedType.Enum enumType -> Path.fromEnum enumType |> create
        | ResolvedType.EnumCase _ -> create TransientTypePath.Anchored
        | ResolvedType.TypeParameter typeParameter ->
            typeParameter.Name
            |> Name.Case.valueOrModified
            |> Ast.LongIdent
            |> create
        | ResolvedType.ReadOnly resolvedType -> refTypeRender ctx resolvedType
        | ResolvedType.Tuple tuple ->
            tuple.Types
            |> List.map (fun tupleElement ->
                tupleElement.Type.Value
                |> refTypeRender ctx
                |> TypeRefRender.orNullable tupleElement.IsOptional
                )
            |> List.toArray
            |> create
        | ResolvedType.Predicate _ ->
            create Types.bool
        | ResolvedType.TypeLiteral typeLiteral ->
            // is func?
            let callSignature, rest =
                typeLiteral.Members
                |> List.partition _.IsCallSignature
                ||> fun sigs rest ->
                    sigs
                    |> List.map (function
                        Member.CallSignature callSignature -> callSignature
                        | _ -> failwith "Expected call signature"),
                    rest
            let shouldInlineCallSignature (callSignature: CallSignature) =
                List.length callSignature.Parameters < 3
                &&
                callSignature.Parameters
                |> List.exists _.IsSpread
                |> not
            match callSignature, rest with
            | [], [] -> createOptional Types.obj
            // single sig; no overloads
            | [ [ singleSig ] ], [] when shouldInlineCallSignature singleSig ->
                let parameters =
                    singleSig.Parameters
                    |> List.map (fun parameter ->
                        refTypeRender ctx parameter.Type.Value
                        |> TypeRefRender.orNullable parameter.IsOptional
                        )
                let returnValue = refTypeRender ctx singleSig.Type.Value
                create (parameters, returnValue)
            | _, _ ->
                create TransientTypePath.Anchored
        | ResolvedType.TemplateLiteral _ -> create Types.string
        | ResolvedType.Optional { ResolvedType = Some (Resolve resolvedType) }
        | ResolvedType.Optional { Type = Resolve resolvedType; TypeArguments = [] } ->
            { refTypeRender ctx resolvedType with Nullable = true }
        | ResolvedType.Optional { Type = Resolve resolvedType; TypeArguments = typeArguments } ->
            let args = typeArguments |> List.map (_.Value >> refTypeRender ctx)
            createOptional (refTypeRender ctx resolvedType, args)
        | ResolvedType.Substitution _ -> create Types.obj

    and refTypeRender (ctx: GeneratorContext) (resolved: ResolvedType) =
        GeneratorContext.getTypeRef ctx resolved
        |> ValueOption.defaultWith(fun () -> typeRender ctx resolved)
