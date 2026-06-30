module Xantham.Generator.Generator.ResolvedTypeCategorization

open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator

[<RequireQualifiedAccess>]
type ResolvedTypeLiteralLike =
    | EnumCase of EnumCase
    | Literal of TsLiteral
    | TypeQuery of TypeQuery
    member this.AsResolvedType =
        match this with
        | EnumCase enumCase -> ResolvedType.EnumCase enumCase
        | Literal tsLiteral -> ResolvedType.Literal tsLiteral
        | TypeQuery typeQuery -> ResolvedType.TypeQuery typeQuery

[<RequireQualifiedAccess>]
type ResolvedTypeEnumLike =
    | Enum of EnumType
    | Index of Index
    member this.AsResolvedType =
        match this with
        | Enum enumType -> ResolvedType.Enum enumType
        | Index index -> ResolvedType.Index index

[<RequireQualifiedAccess>]
type ResolvedTypeOther =
    | IndexedAccess of IndexAccessType
    | Intersection of Intersection
    | Class of Class
    | GlobalThis
    | TemplateLiteral of TemplateLiteral
    | TypeLiteral of TypeLiteral
    | Array of ResolvedType
    | TypeParameter of TypeParameter
    | Tuple of Tuple
    | Interface of Interface
    | TypeReference of TypeReference
    member this.AsResolvedType =
        match this with
        | IndexedAccess indexAccessType -> ResolvedType.IndexedAccess indexAccessType
        | Intersection intersection -> ResolvedType.Intersection intersection
        | Class ``class`` -> ResolvedType.Class ``class``
        | GlobalThis -> ResolvedType.GlobalThis
        | TypeLiteral typeLiteral -> ResolvedType.TypeLiteral typeLiteral
        | Array resolvedType -> ResolvedType.Array resolvedType
        | TypeParameter typeParameter -> ResolvedType.TypeParameter typeParameter
        | Tuple tuple -> ResolvedType.Tuple tuple
        | Interface ``interface`` -> ResolvedType.Interface ``interface``
        | TypeReference typeReference -> ResolvedType.TypeReference typeReference
        | TemplateLiteral templateLiteral -> ResolvedType.TemplateLiteral templateLiteral

[<RequireQualifiedAccess>]
type ResolvedTypePrimitiveLike =
    | Predicate of Predicate
    | Primitive of TypeKindPrimitive
    member this.AsResolvedType =
        match this with
        | Predicate predicate -> ResolvedType.Predicate predicate
        | Primitive typeKindPrimitive -> ResolvedType.Primitive typeKindPrimitive

type ResolvedTypeCategories = {
    EnumLike: ResolvedTypeEnumLike list
    LiteralLike: ResolvedTypeLiteralLike list
    Primitives: ResolvedTypePrimitiveLike list
    Others: ResolvedTypeOther list
    Nullable: bool
}

module ResolvedTypeCategories =
    let (|PrimitiveLike|LiteralLike|EnumLike|OtherLike|) =
        let rec impl = function
            | ResolvedType.Array resolvedType -> OtherLike (ResolvedTypeOther.Array resolvedType)
            | ResolvedType.Class resolvedType -> OtherLike (ResolvedTypeOther.Class resolvedType)
            | ResolvedType.Interface resolvedType -> OtherLike (ResolvedTypeOther.Interface resolvedType)
            | ResolvedType.GlobalThis -> OtherLike ResolvedTypeOther.GlobalThis
            | ResolvedType.TypeLiteral resolvedType -> OtherLike (ResolvedTypeOther.TypeLiteral resolvedType)
            | ResolvedType.TypeParameter resolvedType -> OtherLike (ResolvedTypeOther.TypeParameter resolvedType)
            | ResolvedType.Tuple resolvedType -> OtherLike (ResolvedTypeOther.Tuple resolvedType)
            | ResolvedType.TypeReference resolvedType -> OtherLike (ResolvedTypeOther.TypeReference resolvedType)
            | ResolvedType.IndexedAccess resolvedType -> OtherLike (ResolvedTypeOther.IndexedAccess resolvedType)
            | ResolvedType.Intersection resolvedType -> OtherLike (ResolvedTypeOther.Intersection resolvedType)
            | ResolvedType.TemplateLiteral resolvedType -> OtherLike (ResolvedTypeOther.TemplateLiteral resolvedType)
            | ResolvedType.Predicate resolvedType -> PrimitiveLike (ResolvedTypePrimitiveLike.Predicate resolvedType)
            | ResolvedType.Primitive resolvedType -> PrimitiveLike (ResolvedTypePrimitiveLike.Primitive resolvedType)
            | ResolvedType.Enum resolvedType -> EnumLike (ResolvedTypeEnumLike.Enum resolvedType)
            | ResolvedType.Index resolvedType -> EnumLike (ResolvedTypeEnumLike.Index resolvedType)
            // A boolean literal (`true`/`false`) is not enum-like: the two booleans ARE the
            // inhabitants of `bool`, so a `true | false` union is just `bool`. Categorize it as
            // the bool primitive rather than a hoisted string-enum case.
            | ResolvedType.Literal (TsLiteral.Bool _) -> PrimitiveLike (ResolvedTypePrimitiveLike.Primitive TypeKindPrimitive.Boolean)
            | ResolvedType.Literal resolvedType -> LiteralLike (ResolvedTypeLiteralLike.Literal resolvedType)
            | ResolvedType.EnumCase resolvedType -> LiteralLike (ResolvedTypeLiteralLike.EnumCase resolvedType)
            | ResolvedType.TypeQuery ({ Type = Resolve (ResolvedType.Literal _) } as resolvedType) -> LiteralLike (ResolvedTypeLiteralLike.TypeQuery resolvedType)
            | ResolvedType.TypeQuery { Type = Resolve typ } -> impl typ
            | ResolvedType.Conditional _
            | ResolvedType.Union _
            | ResolvedType.ReadOnly _
            | ResolvedType.Optional _
            | ResolvedType.Substitution _ -> invalidOp "These operations are only available for expanded resolved types in the ResolvedTypeCategories module"

        impl
    let inline (|AsResolvedType|) (value: ^T when ^T:(member AsResolvedType: ResolvedType)) = value.AsResolvedType

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
                | ResolvedTypeLiteralLike.EnumCase case -> case
                | _ -> failwith "Expected enum case")
            |> List.groupBy _.Parent.Value
        let enums, enumCases =
            enumCases
            |> List.partition (function
                | { Members = expectedCases }, presentCases when List.length expectedCases = List.length presentCases -> true
                | _ -> false)
        let enums = enums |> List.map (fst >> ResolvedTypeEnumLike.Enum)
        let enumCases = enumCases |> List.collect (snd >> List.map ResolvedTypeLiteralLike.EnumCase)
        let literalLikes = literals @ enumCases
        {
            categories with
                EnumLike = categories.EnumLike @ enums
                LiteralLike = literalLikes
        }
            
    let create (resolvedType: ResolvedType) =
        // Cycle guard: the follow-through arms below (Substitution/Conditional/Union/Optional/
        // ReadOnly and the `TypeReference { ResolvedType = Some _ }` / constraint arms) recurse
        // into a NESTED resolved type. A self-referential structure — e.g. a mapped-type alias
        // body that reaches back to its own TypeReference through a union member — has no
        // structural base case, so unguarded recursion stack-overflows.
        //
        // `onPath` tracks the nodes on the CURRENT ANCESTOR PATH (not the whole traversal): each
        // recursion-following arm Adds its node before descending and Removes it after, so a node
        // legitimately reached on two SIBLING branches of a DAG is NOT mistaken for a cycle (that
        // distinction is essential — a flat `visited` set wrongly shelled DAG-shared nodes and
        // collapsed valid structures to `obj`). Only a node already on its own ancestor path is a
        // true cycle. Every ResolvedType case wraps a [<ReferenceEquality>] record, so the
        // HashSet hashes/compares by reference (O(1), itself cycle-safe).
        let onPath = System.Collections.Generic.HashSet<ResolvedType>(HashIdentity.Reference)
        let rec categorize (categories: ResolvedTypeCategories) (resolved: ResolvedType) =
            // Descend into `next` while marking `resolved` as on-path; restore on the way out.
            let inline guarded next =
                let added = onPath.Add resolved
                let result = next ()
                if added then onPath.Remove resolved |> ignore
                result
            match resolved with
            | ResolvedType.TypeReference _
            | ResolvedType.TypeParameter _ when onPath.Contains resolved ->
                // A TypeReference/TypeParameter already on the current ancestor path — break the
                // cycle by categorizing it as a plain cross-reference (its non-recursive shell)
                // rather than following its resolved body again.
                addOthers categories (
                    match resolved with
                    | ResolvedType.TypeReference tr -> ResolvedTypeOther.TypeReference tr
                    | ResolvedType.TypeParameter tp -> ResolvedTypeOther.TypeParameter tp
                    | _ -> failwith "Unreachable: guarded to TypeReference/TypeParameter")
            | ResolvedType.Substitution _
            | ResolvedType.Conditional _
            | ResolvedType.Union _
            | ResolvedType.Optional _
            | ResolvedType.ReadOnly _ when onPath.Contains resolved ->
                // A container node (Union/Conditional/...) already on the current ancestor path:
                // its members are being categorized higher on the live recursion, so re-following
                // it adds nothing and would loop. Skip it (these container kinds have no OtherLike
                // shell — the categorize active pattern rejects them).
                categories
            | ResolvedType.Substitution substitutionType ->
                guarded (fun () -> categorize categories substitutionType.Base.Value)
            | ResolvedType.Conditional conditionalType ->
                guarded (fun () ->
                    [
                        conditionalType.True.Value
                        conditionalType.False.Value
                    ]
                    |> List.fold categorize categories)
            | ResolvedType.Union union ->
                guarded (fun () ->
                    union.Types
                    |> List.map _.Value
                    |> List.fold categorize categories)
            | ResolvedType.Optional { ResolvedType = Some (Resolve value) }
            | ResolvedType.Optional { Type = Resolve value } ->
                guarded (fun () -> categorize (nullable categories) value)
            | ResolvedType.TypeReference { ResolvedType = Some (Resolve value) }
            | ResolvedType.TypeParameter { Constraint = Some (Resolve value) }
            | ResolvedType.ReadOnly value
            | ResolvedType.TypeReference { Type = Resolve value; TypeArguments = [] } ->
                guarded (fun () -> categorize categories value)
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
                |> funApply (ResolvedTypePrimitiveLike.Primitive TypeKindPrimitive.NonPrimitive)
            // unify primitives with the same intent to a single primitive type
            // any (already handled); essymbol; nonprimitive; unknown (already handled)
            | ResolvedType.Primitive (
                TypeKindPrimitive.ESSymbol
               | TypeKindPrimitive.NonPrimitive
                ) ->
                ResolvedTypePrimitiveLike.Primitive TypeKindPrimitive.NonPrimitive
                |> addPrimitives categories
            // can now safely intake any other primitive
            | PrimitiveLike primitive ->
                addPrimitives categories primitive
            // intent is nullability
            | ResolvedType.Literal TsLiteral.Null ->
                nullable categories
            | LiteralLike resolved ->
                addLiteralLike categories resolved
            | EnumLike resolved ->
                addEnumLike categories resolved
            | OtherLike resolved ->
                addOthers categories resolved
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
        
    