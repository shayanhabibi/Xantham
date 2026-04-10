module Xantham.Generator.Generator.ResolvedTypeCategorization

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator

type ResolvedTypeCategories = {
    EnumLike: ResolvedType list
    LiteralLike: ResolvedType list
    Primitives: ResolvedType list
    Others: ResolvedType list
    Nullable: bool
}

module ResolvedTypeCategories =
    let empty = {
        EnumLike = []
        LiteralLike = []
        Primitives = []
        Others = []
        Nullable = false
    }
    let addOthers categories resolved = { categories with Others = resolved :: categories.Others }
    let addPrimitives categories resolved = { categories with Primitives = resolved :: categories.Primitives }
    let addLiteralLike categories resolved = { categories with LiteralLike = resolved :: categories.LiteralLike }
    let addEnumLike categories resolved = { categories with EnumLike = resolved :: categories.EnumLike }
    let nullable categories = { categories with Nullable = true }
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
    let simplify (categories: ResolvedTypeCategories) =
        let enumCases, literals =
            categories.LiteralLike
            |> List.partition _.IsEnumCase
        let enumCases =
            enumCases |> List.map (function
                | ResolvedType.EnumCase case -> case
                | _ -> failwith "Expected enum case")
            |> List.groupBy _.Parent.Value
        let enums, enumCases =
            enumCases
            |> List.partition (function
                | { Members = expectedCases }, presentCases when List.length expectedCases = List.length presentCases -> true
                | _ -> false)
        let enums = enums |> List.map (fst >> ResolvedType.Enum)
        let enumCases = enumCases |> List.collect (snd >> List.map ResolvedType.EnumCase)
        let literalLikes = literals @ enumCases
        {
            categories with
                EnumLike = categories.EnumLike @ enums
                LiteralLike = literalLikes
        }
            
    let (|RenderTransientType|_|): ResolvedTypeCategories -> bool = _.LiteralLike >> List.isEmpty >> not
    let create (resolvedType: ResolvedType) =
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
                
            // intent is nullability
            | ResolvedType.Literal TsLiteral.Null ->
                nullable categories
            | ResolvedType.EnumCase _
            | ResolvedType.TemplateLiteral _
            | ResolvedType.Literal _ ->
                addLiteralLike categories resolved
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
            | ResolvedType.Substitution substitutionType ->
                categorize categories substitutionType.Base.Value
        let cat =
            categorize empty resolvedType
            |> simplify
        {
            Primitives = cat.Primitives |> Seq.distinct |> Seq.rev |> Seq.toList
            LiteralLike = cat.LiteralLike |> Seq.distinct |> Seq.rev |> Seq.toList
            EnumLike = cat.EnumLike |> Seq.distinct |> Seq.rev |> Seq.toList
            Others = cat.Others |> Seq.distinct |> Seq.rev |> Seq.toList
            Nullable = cat.Nullable
        }
     
    module Flip =
        let inline addOthers resolved categories = addOthers categories resolved
        let inline addPrimitives resolved categories = addPrimitives categories resolved
        let inline addLiteralLike resolved categories = addLiteralLike categories resolved
        let inline addEnumLike resolved categories = addEnumLike categories resolved
        
    