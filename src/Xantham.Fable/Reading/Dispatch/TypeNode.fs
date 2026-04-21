module Xantham.Fable.Reading.TypeNode

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer
open Xantham.Fable.Types.Signal

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeNode) =
    let inline getTypeSignalFromNode (node: Ts.TypeNode) =
        match ctx.CreateXanthamTag node |> fst with
        | TagState.Unvisited tag -> pushToStack ctx tag; GuardedData.TypeSignal.get tag
        | TagState.Visited tag -> GuardedData.TypeSignal.get tag
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    let inline setKeyword (prim: TypeKindPrimitive) =
        XanthamTag.debugLocationAndCommentAndForget "TypeNode.dispatch | Keyword" (sprintf "Primitive type: %A" prim) xanTag
        prim |> SType.Primitive |> setAstSignal
        prim.TypeKey |> setTypeKeyForTag xanTag
    let inline setTypeKeyFromNode (node: Ts.TypeNode) =
        ctx.checker.getTypeFromTypeNode node |> _.TypeKey |> setTypeKeyForTag xanTag
    /// Resolve a TypeNode via the checker and forward this tag's signals to the result.
    let inline routeViaChecker (node: Ts.TypeNode) =
        let resolved = ctx.checker.getTypeFromTypeNode node
        let innerTag =
            match ctx.CreateXanthamTag resolved |> fst with
            | TagState.Unvisited t ->
                XanthamTag.chainDebug xanTag t
                |> pushToStack ctx
                t
            | TagState.Visited t ->
                XanthamTag.chainDebug xanTag t
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () ->
            innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    /// Build parameter slots from a list of ParameterDeclarations.
    let inline getParamSlots (parameters: ResizeArray<Ts.ParameterDeclaration>) =
        parameters.AsArray
        |> Array.map (Member.resolveToParameterBuilder ctx)
    match tag with
    | TypeNode.AnyKeyword _       -> setKeyword TypeKindPrimitive.Any
    | TypeNode.StringKeyword _    -> setKeyword TypeKindPrimitive.String
    | TypeNode.NumberKeyword _    -> setKeyword TypeKindPrimitive.Number
    | TypeNode.BooleanKeyword _   -> setKeyword TypeKindPrimitive.Boolean
    | TypeNode.NullKeyword _      -> setKeyword TypeKindPrimitive.Null
    | TypeNode.UndefinedKeyword _ -> setKeyword TypeKindPrimitive.Undefined
    | TypeNode.VoidKeyword _      -> setKeyword TypeKindPrimitive.Void
    | TypeNode.NeverKeyword _     -> setKeyword TypeKindPrimitive.Never
    | TypeNode.UnknownKeyword _   -> setKeyword TypeKindPrimitive.Unknown
    | TypeNode.ObjectKeyword _    -> setKeyword TypeKindPrimitive.NonPrimitive
    | TypeNode.SymbolKeyword _    -> setKeyword TypeKindPrimitive.ESSymbol
    | TypeNode.BigIntKeyword _    -> setKeyword TypeKindPrimitive.BigInt
    | TypeNode.IntrinsicKeyword _ ->
         XanthamTag.debugLocationAndCommentAndForget "TypeNode.dispatch | IntrinsicKeyword" "Represents intrinsic type manipulations like Uppercase etc" xanTag
        (* represents intrinsic type manipulations like Uppercase etc *) 
    | TypeNode.UnionType unionTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | UnionType" xanTag
        let types = unionTypeNode.types.AsArray
        if
            types.Length = 2
            && types |> Array.forall (
                TypeNode.Create
                >> function
                    TypeNode.LiteralType value ->
                        LiteralTokenNodes.Create (unbox value.literal)
                        |> _.IsBooleanLiteral
                    | _ -> false
                )
        then
            setKeyword TypeKindPrimitive.Boolean
        else
            { STypeUnionBuilder.Types = Array.map getTypeSignalFromNode types }
            |> SType.Union
            |> setAstSignal
            setTypeKeyFromNode unionTypeNode
            // TypeStore.Key is generated (see Prelude.usesGeneratedKey) but TypeSignal is set to
            // the semantic union's natural TypeKey via setTypeKeyFromNode. Parents embed that
            // natural TypeKey in their type fields. Ensure the semantic union type-level entry
            // exists in signalCache so the generator can resolve it.
            let semanticUnion = ctx.checker.getTypeFromTypeNode unionTypeNode
            match ctx.CreateXanthamTag semanticUnion |> fst with
            | TagState.Unvisited innerTag -> pushToStack ctx innerTag
            | TagState.Visited _ -> ()
    | TypeNode.IntersectionType intersectionTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | IntersectionType" xanTag
        {
            STypeIntersectionBuilder.Types =
                intersectionTypeNode.types.AsArray
                |> Array.map getTypeSignalFromNode
        }
        |> SType.Intersection
        |> setAstSignal
        setTypeKeyFromNode intersectionTypeNode
        // Same pattern as UnionType: TypeStore.Key is generated but TypeSignal is the semantic
        // intersection's natural TypeKey. Push the semantic type to ensure it's registered.
        let semanticIntersection = ctx.checker.getTypeFromTypeNode intersectionTypeNode
        match ctx.CreateXanthamTag semanticIntersection |> fst with
        | TagState.Unvisited innerTag -> pushToStack ctx innerTag
        | TagState.Visited _ -> ()
    | TypeNode.ArrayType arrayTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ArrayType" xanTag
        let arrayElementTag =
            match ctx.CreateXanthamTag arrayTypeNode.elementType with
            | TagState.Unvisited tag, _ ->
                pushToStack ctx tag
                tag
            | TagState.Visited tag, _ -> tag
        xanTag
        |> GuardedData.AstNodeBuilder.getOrSetDefault
        |> Signal.fulfillWithSome (fun () ->
            {
                STypeReferenceBuilder.Type = GuardedData.TypeSignal.get arrayElementTag
                TypeArguments = [||]
                ResolvedType = ValueNone
            }
            |> SType.Array
            )
        setTypeKeyFromNode arrayTypeNode
    | TypeNode.TupleType tupleTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TupleType" xanTag
        let fixedLength, minLength =
            tupleTypeNode.elements.AsArray
            |> Array.map (unbox<Ts.TypeNode> >> function
                | Patterns.Node.NamedTupleMember node ->
                    node.dotDotDotToken.IsNone, node.questionToken.IsNone
                | Patterns.Node.RestTypeNode _ -> false, false
                | Patterns.Node.OptionalTypeNode _ -> true, false
                | _ -> true, true
                )
            |> Array.unzip
            |> fun (fixedElements, minElements) ->
                fixedElements
                |> Array.filter id
                |> Array.length,
                minElements
                |> Array.filter id
                |> Array.length
        {
            STupleBuilder.IsReadOnly = false
            FixedLength = fixedLength
            MinRequired = minLength
            Types =
                tupleTypeNode.elements.AsArray
                |> Array.map (unbox<Ts.TypeNode> >> TupleTypeMember.forNode ctx)
        }
        |> SType.Tuple
        |> setAstSignal
        setTypeKeyFromNode tupleTypeNode
    | TypeNode.NamedTupleMember namedTupleMemberNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | NamedTupleMember" xanTag
        let typeSignal = getTypeSignalFromNode namedTupleMemberNode.``type``
        
        STupleElementBuilder.FixedLabeled (
            namedTupleMemberNode.name.text,
            {
                Type = typeSignal
                IsOptional = namedTupleMemberNode.questionToken.IsSome
                IsRest = namedTupleMemberNode.dotDotDotToken.IsSome
            }
            )
        |> xanTag.Set
        
    | TypeNode.RestType restTupleTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | RestType" xanTag
        restTupleTypeNode.``type``
        |> getTypeSignalFromNode
        |> STupleElementBuilder.Variadic
        |> xanTag.Set
    | TypeNode.OptionalType optionalTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | OptionalType" xanTag
        {
            Type = optionalTypeNode.``type`` |> getTypeSignalFromNode
            IsRest = false
            IsOptional = true
        }
        |> STupleElementBuilder.Fixed
        |> xanTag.Set
        SType.Optional {
            Type = optionalTypeNode.``type`` |> getTypeSignalFromNode
            TypeArguments = [||]
            ResolvedType = ValueNone
        }
        |> setAstSignal
    | TypeNode.ParenthesizedType typeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ParenthesizedType" xanTag
        let typeSignal =
            typeNode.``type``
            |> getTypeSignalFromNode
        let builder =
            Tracer.unsafeGet<XanTagKind> typeNode.``type`` :?> XanthamTag
            |> GuardedData.AstNodeBuilder.getOrSetDefault
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> builder.Value)
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> typeSignal.Value)
    | TypeNode.TypeReference typeReferenceNode ->
        // Non-generic TypeReferences (no explicit type arguments) forward directly to the resolved
        // type — this avoids a TypeReference entry whose Type == its own TypeStore key (self-ref).
        // Generic TypeReferences keep their own TypeReference builder (captures type arguments).
        let hasTypeArgs = typeReferenceNode.typeArguments |> Option.map (fun a -> a.Count > 0) |> Option.defaultValue false
        if hasTypeArgs then
            XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeReference - hasTypeArgs" xanTag
            TypeReference.fromNode ctx xanTag typeReferenceNode
        else
            XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeReference - not hasTypeArgs" xanTag
            routeViaChecker typeReferenceNode
    | TypeNode.TypeParameterDeclaration typeParameterDeclaration ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeParameterDeclaration" xanTag
        TypeDeclaration.TypeParameter.read ctx xanTag typeParameterDeclaration
    | TypeNode.InferType inferTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | InferType" xanTag
        // Emit the scoped type variable as a TypeParameter so it has an identity in the cache.
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> inferTypeNode.typeParameter) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> t.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> t.Builder.Value)
    | TypeNode.TypePredicate typePredicateNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypePredicate" xanTag
        let paramName =
            if (unbox<Ts.Node> typePredicateNode.parameterName).kind = Ts.SyntaxKind.Identifier
            then (unbox<Ts.Identifier> typePredicateNode.parameterName).text
            else "this"
        let typeSignal =
            typePredicateNode.``type``
            |> Option.map getTypeSignalFromNode
            |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Boolean.TypeKey)
        {
            SPredicateBuilder.ParameterName = paramName
            Type = typeSignal
            IsAssertion = typePredicateNode.assertsModifier.IsSome
        }
        |> SType.Predicate
        |> setAstSignal
        // TypeStore.Key is a generated unique key (see Prelude.usesGeneratedKey). Use that key
        // for the TypeSignal — getTypeFromTypeNode on a TypePredicate returns the boolean type,
        // so setTypeKeyFromNode would produce the boolean TypeKey for all predicates.
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag
    | TypeNode.TypeQuery typeQueryNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeQuery" xanTag
        // typeof X — resolve to the static type of X
        routeViaChecker typeQueryNode
    | TypeNode.TypeOperator typeOperatorNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeOperator" xanTag
        // keyof T → union of property keys; unique symbol → UniqueESSymbol; readonly → inner type
        routeViaChecker typeOperatorNode
    | TypeNode.IndexedAccessType indexedAccessTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | IndexedAccessType" xanTag
        let resolved = ctx.checker.getTypeFromTypeNode indexedAccessTypeNode
        if resolved.flags.HasFlag Ts.TypeFlags.IndexedAccess then
            // Generic/unresolvable — emit raw IndexedAccess builder using the syntactic nodes
            {
                SIndexAccessTypeBuilder.Object = getTypeSignalFromNode indexedAccessTypeNode.objectType
                Index = getTypeSignalFromNode indexedAccessTypeNode.indexType
            }
            |> SType.IndexAccessType
            |> setAstSignal
            resolved.TypeKey |> setTypeKeyForTag xanTag
        else
            // Concrete — checker resolved it (e.g. Interface["prop"] → string); forward signals
            let innerTag =
                match ctx.CreateXanthamTag resolved |> fst with
                | TagState.Unvisited tag -> pushToStack ctx tag; tag
                | TagState.Visited tag -> tag
            xanTag.TypeSignal
            |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
            xanTag.Builder
            |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeNode.MappedType mappedTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | MappedType" xanTag
        // { [K in keyof T]: T[K] } — route via checker to ObjectFlags.Mapped at the type layer
        routeViaChecker mappedTypeNode
    | TypeNode.ConditionalType conditionalTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ConditionalType" xanTag
        {
            SConditionalTypeBuilder.Check = conditionalTypeNode.checkType |> getTypeSignalFromNode
            Extends = conditionalTypeNode.extendsType |> getTypeSignalFromNode
            True = conditionalTypeNode.trueType |> getTypeSignalFromNode
            False = conditionalTypeNode.falseType |> getTypeSignalFromNode
        }
        |> SType.Conditional
        |> setAstSignal
        setTypeKeyFromNode conditionalTypeNode
    | TypeNode.TemplateLiteralType templateLiteralTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TemplateLiteralType" xanTag
        let innerTag =
            match ctx.checker.getTypeAtLocation templateLiteralTypeNode |> ctx.CreateXanthamTag |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeNode.TemplateLiteralTypeSpan _ ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TemplateLiteralTypeSpan" xanTag
        // only reached as child of TemplateLiteralType; handled above
    | TypeNode.ImportType importTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ImportType" xanTag
        // import("module").TypeName — resolve via checker
        routeViaChecker importTypeNode
    | TypeNode.FunctionType funcTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | FunctionType" xanTag
        // (x: T) => R — emit as a CallSignature (type params not captured; schema has no slot)
        {
            Members = [|
                {
                    SCallSignatureBuilder.Parameters = getParamSlots funcTypeNode.parameters
                    Type = getTypeSignalFromNode funcTypeNode.``type``
                    Documentation = []
                }
                |> SMemberBuilder.CallSignature
                |> ValueSome
                |> Signal.source
            |]
        }
        |> SType.TypeLiteral
        |> setAstSignal
        setTypeKeyFromNode funcTypeNode
    | TypeNode.ConstructorType ctorTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ConstructorType" xanTag
        // new (x: T) => R — emit as a ConstructSignature
        {
            Members = [|
                {
                    SConstructSignatureBuilder.Parameters = getParamSlots ctorTypeNode.parameters
                    Type = getTypeSignalFromNode ctorTypeNode.``type``
                }
                |> SMemberBuilder.ConstructSignature
                |> ValueSome
                |> Signal.source
            |]
        }
        |> SType.TypeLiteral
        |> setAstSignal
        setTypeKeyFromNode ctorTypeNode
    | TypeNode.TypeLiteral typeLiteralNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeLiteral" xanTag
        {
            STypeLiteralBuilder.Members =
                typeLiteralNode.members.AsArray
                |> Array.map (Member.resolveToMemberBuilder ctx)
        }
        |> SType.TypeLiteral
        |> setAstSignal
        setTypeKeyFromNode typeLiteralNode
    | TypeNode.LiteralType literalToken ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | LiteralType" xanTag
        LiteralTokenNode.dispatch
            ctx
            xanTag
            (LiteralTokenNodes.Create (unbox<Ts.Node> literalToken.literal))
    // this type node doesn't really resolve to anything
    // without context
    | TypeNode.ThisType thisTypeNode ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | ThisType" xanTag
        // get type checker resolved value
        match
            ctx.checker.getTypeFromTypeNode thisTypeNode
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            let store = ctx.signalCache[ guard.Value ]
            xanTag.TypeSignal
            |> Signal.fulfillWith(fun () -> store.Key)
            xanTag.Builder
            |> Signal.fulfillWith(fun () -> store.Builder.Value)
        | tagState, _ ->
            let typeSignal, builder =
                tagState |> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
            xanTag.TypeSignal
            |> Signal.fulfillWith(fun () -> typeSignal.Value)
            xanTag.Builder
            |> Signal.fulfillWith(fun () -> builder.Value)
        
    | TypeNode.SatisfiesExpression _ ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | SatisfiesExpression" xanTag
    | TypeNode.AsExpression _ ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | AsExpression" xanTag
    | TypeNode.TypeAssertion _ ->
        XanthamTag.debugLocationAndForget "TypeNode.dispatch | TypeAssertion" xanTag
