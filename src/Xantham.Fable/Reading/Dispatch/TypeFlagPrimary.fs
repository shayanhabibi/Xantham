module Xantham.Fable.Reading.TypeFlagPrimary

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal

/// Checks whether a flat array of enum-literal types represents a complete enum
/// (every member of the parent enum is present). If so, wires xanTag's signals to
/// forward from the enum declaration tag and returns true. Partial matches return false
/// and leave xanTag untouched so the caller can fall through to a normal union build.
module private EnumUnionReduction =

    let tryRoute (ctx: TypeScriptReader) (xanTag: XanthamTag) (types: Ts.Type array) : bool =
        if types.Length = 0 then false
        elif not (types |> Array.forall (fun t -> t.flags.HasFlag Ts.TypeFlags.EnumLiteral)) then false
        else

        // Obtain the parent enum type for each constituent via the checker.
        // For an enum literal, getBaseTypeOfLiteralType returns the full enum union.
        let parentTypes = types |> Array.map ctx.checker.getBaseTypeOfLiteralType
        let firstParentKey = parentTypes.[0].TypeKey

        // All constituents must share the same parent enum.
        if not (parentTypes |> Array.forall (fun t -> t.TypeKey = firstParentKey)) then false
        else

        let parentEnumType = parentTypes.[0]

        // Count all members in the parent enum.
        let allMemberCount =
            if parentEnumType.flags.HasFlag Ts.TypeFlags.Union then
                (unbox<Ts.UnionType> parentEnumType).types.AsArray.Length
            else 0

        // Only reduce when every member of the enum is present in the union.
        if allMemberCount = 0 || allMemberCount <> types.Length then false
        else

        // Route to the enum declaration node so the TypeDeclaration dispatcher handles it.
        match parentEnumType.getSymbol() with
        | None -> false
        | Some sym ->
        match sym.declarations |> Option.bind (fun d -> if d.Count > 0 then Some d.[0] else None) with
        | None -> false
        | Some decl ->

        let enumTag =
            match ctx.CreateXanthamTag decl |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag

        xanTag.Builder
        |> Signal.fulfillWith (fun () -> enumTag.Builder.Value)
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> enumTag.TypeSignal.Value)
        true

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeFlagPrimary) =
    /// <summary>
    /// Retrieves type signal from type tag; pushes to stack prn
    /// </summary>
    let inline getTypeSignal (typ: Ts.Type) =
        ctx.CreateXanthamTag typ
        |> fst
        |> stackPushAndThen ctx _.TypeSignal
    /// <summary>
    /// Sets current xantags builder signal to the given value
    /// </summary>
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    /// <summary>
    /// ties the current xantham tag to the signals of the given type
    /// </summary>
    let inline routeToType (typ: Ts.Type) =
        let underlyingType =
            match ctx.CreateXanthamTag typ |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        let underlyingTypeSignal = underlyingType.TypeSignal
        let underlyingBuilderSignal = underlyingType.Builder
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> underlyingTypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> underlyingBuilderSignal.Value)
    let setPrimitive = fun prim ->
        SType.Primitive prim
        |> setAstSignal
        prim.TypeKey
        |> setTypeKeyForTag xanTag
    match tag with
    | TypeFlagPrimary.Any _ -> setPrimitive TypeKindPrimitive.Any
    | TypeFlagPrimary.Unknown _ -> setPrimitive TypeKindPrimitive.Unknown
    | TypeFlagPrimary.String _ -> setPrimitive TypeKindPrimitive.String
    | TypeFlagPrimary.Number _ -> setPrimitive TypeKindPrimitive.Number
    | TypeFlagPrimary.Boolean _ -> setPrimitive TypeKindPrimitive.Boolean
    | TypeFlagPrimary.BigInt _ -> setPrimitive TypeKindPrimitive.BigInt
    | TypeFlagPrimary.ESSymbol _ -> setPrimitive TypeKindPrimitive.ESSymbol
    | TypeFlagPrimary.UniqueESSymbol _ -> setPrimitive TypeKindPrimitive.ESSymbol
    | TypeFlagPrimary.Void _ -> setPrimitive TypeKindPrimitive.Void
    | TypeFlagPrimary.Undefined _ -> setPrimitive TypeKindPrimitive.Undefined
    | TypeFlagPrimary.Null _ -> setPrimitive TypeKindPrimitive.Null
    | TypeFlagPrimary.Never _ -> setPrimitive TypeKindPrimitive.Never
    | TypeFlagPrimary.NonPrimitive _ -> setPrimitive TypeKindPrimitive.NonPrimitive
    | TypeFlagPrimary.Literal typeFlagLiteral -> TypeFlagLiteral.dispatch ctx xanTag typeFlagLiteral
    | TypeFlagPrimary.Object typeFlagObject -> TypeFlagObject.dispatch ctx xanTag typeFlagObject
    | TypeFlagPrimary.Intersection intersectionType ->
        {
            STypeIntersectionBuilder.Types =
                intersectionType.types.AsArray
                |> Array.map getTypeSignal
        }
        |> SType.Intersection
        |> setAstSignal
        intersectionType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.Index indexType ->
        { SIndexBuilder.Type = unbox<Ts.Type> indexType.``type`` |> getTypeSignal }
        |> SType.Index
        |> setAstSignal
        indexType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.IndexedAccess indexedAccessType ->
        // If the checker has simplified the type (e.g. T[K] where K's constraint resolves),
        // forward to the simplified form. Otherwise emit a raw IndexedAccess builder.
        match indexedAccessType.simplifiedForReading with
        | Some simplified when not (simplified.flags.HasFlag Ts.TypeFlags.IndexedAccess) ->
            routeToType simplified
        | _ ->
            {
                SIndexAccessTypeBuilder.Object = getTypeSignal indexedAccessType.objectType
                Index = getTypeSignal indexedAccessType.indexType
            }
            |> SType.IndexAccessType
            |> setAstSignal
            indexedAccessType.TypeKey
            |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.Conditional conditionalType ->
        let inline getTypeSignalFromNode (node: Ts.TypeNode) =
            ctx.CreateXanthamTag node |> fst |> stackPushAndThen ctx _.TypeSignal
        {
            SConditionalTypeBuilder.Check = getTypeSignal conditionalType.checkType
            Extends = getTypeSignal conditionalType.extendsType
            True =
                conditionalType.resolvedTrueType
                |> Option.map (getTypeSignal >> ctx.routeTypeTo xanTag)
                |> Option.defaultWith (fun () ->
                    getTypeSignalFromNode conditionalType.root.node.trueType)
            False =
                conditionalType.resolvedFalseType
                |> Option.map (getTypeSignal >> ctx.routeTypeTo xanTag)
                |> Option.defaultWith (fun () ->
                    getTypeSignalFromNode conditionalType.root.node.falseType)
        }
        |> SType.Conditional
        |> setAstSignal
        conditionalType.TypeKey |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.Substitution subType -> routeToType subType.baseType
    | TypeFlagPrimary.TemplateLiteral templateLiteralType ->
        {
            STemplateLiteralTypeBuilder.Texts = templateLiteralType.texts.AsArray
            Types = templateLiteralType.types.AsArray |> Array.map getTypeSignal
        }
        |> SType.TemplateLiteral
        |> setAstSignal
        templateLiteralType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.StringMapping stringMappingType ->
        // TODO - for now we're just returning the type of the string mapping directly
        routeToType stringMappingType.``type``
    | TypeFlagPrimary.Union unionType ->
        let types = unionType.types.AsArray
        if types |> Array.forall (TypeFlagPrimary.Create >> function
            | TypeFlagPrimary.Literal (TypeFlagLiteral.Boolean _)
            | TypeFlagPrimary.Boolean _ -> true
            | _ -> false)
        then setPrimitive TypeKindPrimitive.Boolean
        elif EnumUnionReduction.tryRoute ctx xanTag types then ()
        else
        { STypeUnionBuilder.Types = types |> Array.map getTypeSignal }
        |> SType.Union
        |> setAstSignal
        unionType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagPrimary.TypeParameter typ ->
        {
            STypeParameterBuilder.Name =
                typ.getSymbol()
                |> Option.orElse typ.aliasSymbol
                |> Option.map _.name
                |> Option.defaultValue "T"
            Constraint = typ.getConstraint() |> Option.map getTypeSignal |> Option.toValueOption
            Default = typ.getDefault() |> Option.map getTypeSignal |> Option.toValueOption
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SType.TypeParameter
        |> setAstSignal
        typ.TypeKey
        |> setTypeKeyForTag xanTag
            

