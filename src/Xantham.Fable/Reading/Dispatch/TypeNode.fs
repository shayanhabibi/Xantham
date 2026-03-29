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
        prim |> STsAstNodeBuilder.Primitive |> setAstSignal
        prim.TypeKey |> setTypeKeyForTag xanTag
    let inline setTypeKeyFromNode (node: Ts.TypeNode) =
        ctx.checker.getTypeFromTypeNode node |> _.TypeKey |> setTypeKeyForTag xanTag
    /// Resolve a TypeNode via the checker and forward this tag's signals to the result.
    let inline routeViaChecker (node: Ts.TypeNode) =
        let resolved = ctx.checker.getTypeFromTypeNode node
        let innerTag =
            match ctx.CreateXanthamTag resolved |> fst with
            | TagState.Unvisited t -> pushToStack ctx t; t
            | TagState.Visited t -> t
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (ctx.typeSignal innerTag).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault innerTag).Value)
    /// Build parameter slots from a list of ParameterDeclarations.
    let inline getParamSlots (parameters: ResizeArray<Ts.ParameterDeclaration>) =
        parameters.AsArray
        |> Array.map (fun p ->
            let state = ctx.CreateXanthamTag (unbox<Ts.Node> p) |> fst
            let t = state.Value
            if state.IsUnvisited then pushToStack ctx t
            GuardedData.AstNodeBuilder.getOrSetDefault t
            |> Signal.map (function
                | ValueSome (STsAstNodeBuilder.Parameter p) -> ValueSome p
                | _ -> ValueNone
                )
            )
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
    | TypeNode.IntrinsicKeyword _ -> (* represents intrinsic type manipulations like Uppercase etc *) ()
    | TypeNode.UnionType unionTypeNode ->
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
            |> STsAstNodeBuilder.Union
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
        {
            STypeIntersectionBuilder.Types =
                intersectionTypeNode.types.AsArray
                |> Array.map getTypeSignalFromNode
        }
        |> STsAstNodeBuilder.Intersection
        |> setAstSignal
        setTypeKeyFromNode intersectionTypeNode
        // Same pattern as UnionType: TypeStore.Key is generated but TypeSignal is the semantic
        // intersection's natural TypeKey. Push the semantic type to ensure it's registered.
        let semanticIntersection = ctx.checker.getTypeFromTypeNode intersectionTypeNode
        match ctx.CreateXanthamTag semanticIntersection |> fst with
        | TagState.Unvisited innerTag -> pushToStack ctx innerTag
        | TagState.Visited _ -> ()
    | TypeNode.ArrayType arrayTypeNode ->
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
            |> STsAstNodeBuilder.Array
            )
        setTypeKeyFromNode arrayTypeNode
    | TypeNode.TupleType tupleTypeNode ->
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
        |> STsAstNodeBuilder.Tuple
        |> setAstSignal
        setTypeKeyFromNode tupleTypeNode
    | TypeNode.NamedTupleMember namedTupleMemberNode ->
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
        restTupleTypeNode.``type``
        |> getTypeSignalFromNode
        |> STupleElementBuilder.Variadic
        |> xanTag.Set
    | TypeNode.OptionalType optionalTypeNode ->
        {
            Type = optionalTypeNode.``type`` |> getTypeSignalFromNode
            IsRest = false
            IsOptional = true
        }
        |> STupleElementBuilder.Fixed
        |> xanTag.Set
        STsAstNodeBuilder.Optional {
            Type = optionalTypeNode.``type`` |> getTypeSignalFromNode
            TypeArguments = [||]
            ResolvedType = ValueNone
        }
        |> setAstSignal
    | TypeNode.ParenthesizedType typeNode ->
        let typeSignal =
            typeNode.``type``
            |> getTypeSignalFromNode
        let builder =
            Tracer.unsafeGet<XanTagKind> typeNode.``type`` :?> XanthamTag
            |> GuardedData.AstNodeBuilder.getOrSetDefault
        xanTag
        |> GuardedData.AstNodeBuilder.getOrSetDefault
        |> Signal.fulfillWith (fun () -> builder.Value)
        xanTag
        |> ctx.typeSignal
        |> Signal.fulfillWith (fun () -> typeSignal.Value)
    | TypeNode.TypeReference typeReferenceNode ->
        // Non-generic TypeReferences (no explicit type arguments) forward directly to the resolved
        // type — this avoids a TypeReference entry whose Type == its own TypeStore key (self-ref).
        // Generic TypeReferences keep their own TypeReference builder (captures type arguments).
        let hasTypeArgs = typeReferenceNode.typeArguments |> Option.map (fun a -> a.Count > 0) |> Option.defaultValue false
        if hasTypeArgs then
            TypeReference.fromNode ctx xanTag typeReferenceNode
        else
            routeViaChecker typeReferenceNode
    | TypeNode.TypeParameterDeclaration typeParameterDeclaration ->
        TypeDeclaration.TypeParameter.read ctx xanTag typeParameterDeclaration
    | TypeNode.InferType inferTypeNode ->
        // Emit the scoped type variable as a TypeParameter so it has an identity in the cache.
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> inferTypeNode.typeParameter) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (ctx.typeSignal t).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault t).Value)
    | TypeNode.TypePredicate typePredicateNode ->
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
        |> STsAstNodeBuilder.Predicate
        |> setAstSignal
        // TypeStore.Key is a generated unique key (see Prelude.usesGeneratedKey). Use that key
        // for the TypeSignal — getTypeFromTypeNode on a TypePredicate returns the boolean type,
        // so setTypeKeyFromNode would produce the boolean TypeKey for all predicates.
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag
    | TypeNode.TypeQuery typeQueryNode ->
        // typeof X — resolve to the static type of X
        routeViaChecker typeQueryNode
    | TypeNode.TypeOperator typeOperatorNode ->
        // keyof T → union of property keys; unique symbol → UniqueESSymbol; readonly → inner type
        routeViaChecker typeOperatorNode
    | TypeNode.IndexedAccessType indexedAccessTypeNode ->
        let resolved = ctx.checker.getTypeFromTypeNode indexedAccessTypeNode
        if resolved.flags.HasFlag Ts.TypeFlags.IndexedAccess then
            // Generic/unresolvable — emit raw IndexedAccess builder using the syntactic nodes
            {
                SIndexAccessTypeBuilder.Object = getTypeSignalFromNode indexedAccessTypeNode.objectType
                Index = getTypeSignalFromNode indexedAccessTypeNode.indexType
            }
            |> STsAstNodeBuilder.IndexAccessType
            |> setAstSignal
            resolved.TypeKey |> setTypeKeyForTag xanTag
        else
            // Concrete — checker resolved it (e.g. Interface["prop"] → string); forward signals
            let innerTag =
                match ctx.CreateXanthamTag resolved |> fst with
                | TagState.Unvisited tag -> pushToStack ctx tag; tag
                | TagState.Visited tag -> tag
            ctx.typeSignal xanTag
            |> Signal.fulfillWith (fun () -> (GuardedData.TypeSignal.get innerTag).Value)
            GuardedData.AstNodeBuilder.getOrSetDefault xanTag
            |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.get innerTag).Value)
    | TypeNode.MappedType mappedTypeNode ->
        // { [K in keyof T]: T[K] } — route via checker to ObjectFlags.Mapped at the type layer
        routeViaChecker mappedTypeNode
    | TypeNode.ConditionalType conditionalTypeNode ->
        {
            SConditionalTypeBuilder.Check = conditionalTypeNode.checkType |> getTypeSignalFromNode
            Extends = conditionalTypeNode.extendsType |> getTypeSignalFromNode
            True = conditionalTypeNode.trueType |> getTypeSignalFromNode
            False = conditionalTypeNode.falseType |> getTypeSignalFromNode
        }
        |> STsAstNodeBuilder.Conditional
        |> setAstSignal
        setTypeKeyFromNode conditionalTypeNode
    | TypeNode.TemplateLiteralType templateLiteralTypeNode ->
        let innerTag =
            match ctx.checker.getTypeAtLocation templateLiteralTypeNode |> ctx.CreateXanthamTag |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.TypeSignal.get innerTag).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.get innerTag).Value)
    | TypeNode.TemplateLiteralTypeSpan _ -> () // only reached as child of TemplateLiteralType; handled above
    | TypeNode.ImportType importTypeNode ->
        // import("module").TypeName — resolve via checker
        routeViaChecker importTypeNode
    | TypeNode.FunctionType funcTypeNode ->
        // (x: T) => R — emit as a CallSignature (type params not captured; schema has no slot)
        {
            SCallSignatureBuilder.Parameters = getParamSlots funcTypeNode.parameters
            Type = getTypeSignalFromNode funcTypeNode.``type``
            Documentation = []
        }
        |> STsAstNodeBuilder.CallSignature
        |> setAstSignal
        setTypeKeyFromNode funcTypeNode
    | TypeNode.ConstructorType ctorTypeNode ->
        // new (x: T) => R — emit as a ConstructSignature
        {
            SConstructSignatureBuilder.Parameters = getParamSlots ctorTypeNode.parameters
            Type = getTypeSignalFromNode ctorTypeNode.``type``
        }
        |> STsAstNodeBuilder.ConstructSignature
        |> setAstSignal
        setTypeKeyFromNode ctorTypeNode
    | TypeNode.TypeLiteral typeLiteralNode ->
        {
            STypeLiteralBuilder.Members =
                typeLiteralNode.members.AsArray
                |> Array.map (Member.resolveToMemberBuilder ctx)
        }
        |> STsAstNodeBuilder.TypeLiteral
        |> setAstSignal
        setTypeKeyFromNode typeLiteralNode
    | TypeNode.LiteralType literalToken ->
        LiteralTokenNode.dispatch
            ctx
            xanTag
            (LiteralTokenNodes.Create (unbox<Ts.Node> literalToken.literal))
    // this type node doesn't really resolve to anything
    // without context
    | TypeNode.ThisType thisTypeNode ->
        // get type checker resolved value
        match
            ctx.checker.getTypeFromTypeNode thisTypeNode
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            let store = ctx.signalCache[ guard.Value ]
            ctx.typeSignal xanTag
            |> Signal.fulfillWith(fun () -> store.Key)
            GuardedData.AstNodeBuilder.getOrSetDefault xanTag
            |> Signal.fulfillWith(fun () -> store.Builder.Value)
        | tagState, _ ->
            let tag = tagState.Value
            if tagState.IsUnvisited then
                pushToStack ctx tag
            let typeSignal = ctx.typeSignal tag
            let builder = GuardedData.AstNodeBuilder.getOrSetDefault tag
            ctx.typeSignal xanTag
            |> Signal.fulfillWith(fun () -> typeSignal.Value)
            GuardedData.AstNodeBuilder.getOrSetDefault xanTag
            |> Signal.fulfillWith(fun () -> builder.Value)
        
    | TypeNode.SatisfiesExpression _ -> ()
    | TypeNode.AsExpression _ -> ()
    | TypeNode.TypeAssertion _ -> ()
