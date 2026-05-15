module Xantham.Fable.Reading.TypeNode

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer
open Xantham.Fable.Types.Signal

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeNode) =
    let debugMessage = sprintf "Dispatching type node of kind %s" >> xanTag.doDebugMessage
    let inline getTypeSignalFromNode (node: Ts.TypeNode) =
        match ctx.CreateXanthamTag node |> fst with
        | TagState.Unvisited tag -> pushToStack ctx tag; GuardedData.TypeSignal.get tag
        | TagState.Visited tag -> GuardedData.TypeSignal.get tag
    let inline setAstSignal builder =
        xanTag.Builder <- builder
    let inline setKeyword (prim: TypeKindPrimitive) =
        prim.ToString() |> debugMessage
        prim |> SType.Primitive |> setAstSignal
        prim.TypeKey |> setTypeKeyForTag xanTag
    let inline setTypeKeyFromNode (node: Ts.TypeNode) =
        ctx.checker.getTypeFromTypeNode node |> _.TypeKey |> setTypeKeyForTag xanTag
    /// Resolve a TypeNode via the checker and forward this tag's signals to the result.
    let inline routeViaChecker (node: Ts.TypeNode) =
        let resolved = ctx.checker.getTypeFromTypeNode node
        let innerTag =
            ctx.CreateXanthamTag resolved
            |> fst
            |> _.Value
            |> _.chainDebug(xanTag)
        pushToStack ctx innerTag
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () ->
            innerTag.TypeSignal.Value)

        xanTag.Builder
        |> Signal.fulfillWith(fun () ->
            match innerTag.Builder.Value with
            | ValueSome _ as v -> v
            | ValueNone when innerTag.TryExportBuilder.IsSome && innerTag.ExportBuilder.Value.IsSome && ctx.exportCache.ContainsKey(innerTag.IdentityKey) ->
                let refKey = TypeSignal.ofKey ctx.exportCache[innerTag.IdentityKey].RefKey
                let makeTypeReferenceBuilder innerType innerTypeArguments = ValueSome <| SType.TypeReference {
                    Type = innerType
                    TypeArguments = innerTypeArguments
                    ResolvedType = ValueNone
                }
                let makeTypeArgument (signal: Signal<InlinedSTypeParameterBuilder voption>) =
                    match signal.Value with
                    | ValueSome { TypeParameter = { Constraint = ValueSome typeSignal } }
                    | ValueSome { TypeParameter = { Default = ValueSome typeSignal } } -> typeSignal
                    | ValueSome { Type = typeKey } -> Signal.source typeKey
                    | ValueNone -> TypeSignal.pending()
                innerTag.ExportBuilder.Value
                |> ValueOption.bind (function
                    | STsExportDeclaration.TypeAlias { TypeParameters = typeParams; Type = aliasType } -> 
                        typeParams
                        |> Array.map makeTypeArgument
                        |> makeTypeReferenceBuilder aliasType
                    | STsExportDeclaration.Class { TypeParameters = typeParams } 
                    | STsExportDeclaration.Interface { TypeParameters = typeParams } ->
                        typeParams
                        |> Array.map makeTypeArgument
                        |> makeTypeReferenceBuilder refKey
                    | STsExportDeclaration.Enum _
                    | STsExportDeclaration.Variable _
                    | STsExportDeclaration.Function _
                    | STsExportDeclaration.Module _ -> makeTypeReferenceBuilder refKey [||]
                    )
            | ValueNone ->
                ValueNone
            )
    /// Build parameter slots from a list of ParameterDeclarations.
    let inline getParamSlots (parameters: ResizeArray<Ts.ParameterDeclaration>) =
        parameters.AsArray
        |> Array.map (Member.resolveToParameterBuilder ctx)
    /// Build method-scope typar slots from an optional list of
    /// TypeParameterDeclarations (TS function-type / constructor-type nodes).
    let inline getTypeParamSlots (typeParams: ResizeArray<Ts.TypeParameterDeclaration> option) =
        typeParams
        |> Option.map _.AsArray
        |> Option.defaultValue [||]
        |> Array.map (
            ctx.CreateXanthamTag
            >> fst >> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
            >> fun signals -> signals ||> Signal.map2 (fun typeKey -> function
                | ValueSome (SType.TypeParameter tp) ->
                    ValueSome {
                        Type = typeKey
                        TypeParameter = tp
                    }
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
    | TypeNode.IntrinsicKeyword _ ->
         nameof TypeNode.IntrinsicKeyword |> debugMessage
         // Type references, aliases, and other declarations that somehow
         // require a dedicated type build for the intrinsic type will fail
         // on this dangling reference. Set a generic value. Most resolved shapes should not
         // even point to this.
         setAstSignal (SType.Primitive TypeKindPrimitive.Any)
         setTypeKeyForTag xanTag TypeKindPrimitive.Any.TypeKey
        (* represents intrinsic type manipulations like Uppercase etc *) 
    | TypeNode.UnionType unionTypeNode ->
        nameof TypeNode.UnionType |> debugMessage
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
            ctx.signalCache[xanTag.IdentityKey].Key
            |> setTypeKeyForTag xanTag
            // setTypeKeyFromNode unionTypeNode
            // TypeStore.Key is generated (see Prelude.usesGeneratedKey) but TypeSignal is set to
            // the semantic union's natural TypeKey via setTypeKeyFromNode. Parents embed that
            // natural TypeKey in their type fields. Ensure the semantic union type-level entry
            // exists in signalCache so the generator can resolve it.
            let semanticUnion = ctx.checker.getTypeFromTypeNode unionTypeNode
            // match ctx.CreateXanthamTag semanticUnion |> fst with
            // | TagState.Unvisited innerTag -> pushToStack ctx innerTag
            // | TagState.Visited _ -> ()
            
            // Force push regardless of visitation state, dispatcher will noop if visitation state
            // is reflected in the type store. Bug didn't show up for this, but we're applying
            // the same fix that was done for intersections proactively.
            ctx.CreateXanthamTag semanticUnion
            |> fst
            |> TagState.apply (fun _ -> pushToStack ctx)
            |> ignore
    | TypeNode.IntersectionType intersectionTypeNode ->
        nameof TypeNode.IntersectionType |> debugMessage
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
        // Force push regardless of visitation state, dispatcher will noop if visitation state
        // is reflected in the type store.
        ctx.CreateXanthamTag semanticIntersection
        |> fst
        |> TagState.apply (fun _ -> pushToStack ctx)
        |> ignore
    | TypeNode.ArrayType arrayTypeNode ->
        nameof TypeNode.ArrayType |> debugMessage
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
        // TypeStore.Key is a generated unique key (see `usesGeneratedKey` for
        // `TypeNode.ArrayType` in Prelude.fs) — `getTypeFromTypeNode` on `T[]`
        // returns the same `Ts.Type` (and TypeKey) as the underlying `Array<T>`
        // Interface declaration, so without a generated key the structural
        // `SType.Array` entry collided with the Interface entry in the types
        // map and lost the duplicate-resolution coin-flip (e.g. method returns
        // emitting bare `ResizeArray` instead of `ResizeArray<'T>`). Set the
        // TypeSignal to the generated TypeStore key so parents embed the
        // structural entry's key in their Type fields. Same pattern as
        // `TypeNode.UnionType` above.
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag
        // Force-push the semantic Array type so its declaration dispatcher
        // (TypeFlagObject.Interface for `Array`) still registers a TypeStore
        // entry at the semantic key — without this, downstream references to
        // the Array Interface symbol have no entry to resolve to.
        let semanticArray = ctx.checker.getTypeFromTypeNode arrayTypeNode
        ctx.CreateXanthamTag semanticArray
        |> fst
        |> TagState.apply (fun _ -> pushToStack ctx)
        |> ignore
    | TypeNode.TupleType tupleTypeNode ->
        nameof TypeNode.TupleType |> debugMessage
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
        nameof TypeNode.NamedTupleMember |> debugMessage
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
        nameof TypeNode.RestType |> debugMessage
        restTupleTypeNode.``type``
        |> getTypeSignalFromNode
        |> STupleElementBuilder.Variadic
        |> xanTag.Set
    | TypeNode.OptionalType optionalTypeNode ->
        nameof TypeNode.OptionalType |> debugMessage
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
        nameof TypeNode.ParenthesizedType |> debugMessage
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
            "TypeReference - has type args" |> debugMessage
            TypeReference.fromNode ctx xanTag typeReferenceNode
        else
            "TypeReference - no type args" |> debugMessage
            routeViaChecker typeReferenceNode
    | TypeNode.TypeParameterDeclaration typeParameterDeclaration ->
        nameof TypeNode.TypeParameterDeclaration |> debugMessage
        TypeDeclaration.TypeParameter.read ctx xanTag typeParameterDeclaration
    | TypeNode.InferType inferTypeNode ->
        nameof TypeNode.InferType |> debugMessage
        // Emit the scoped type variable as a TypeParameter so it has an identity in the cache.
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> inferTypeNode.typeParameter) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> t.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> t.Builder.Value)
    | TypeNode.TypePredicate typePredicateNode ->
        nameof TypeNode.TypePredicate |> debugMessage
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
        nameof TypeNode.TypeQuery |> debugMessage
        // typeof X — resolve to the static type of X
        let rec foldEntityName (acc: ResizeArray<string>) (entityName: Ts.EntityName) =
            match entityName with
            | Patterns.Node.EntityNamePatterns.Identifier identifier -> acc.Add(identifier.text)
            | Patterns.Node.EntityNamePatterns.QualifiedName qualifiedName ->
                foldEntityName acc qualifiedName.left
                acc.Add(qualifiedName.right.text)
        let entityName =
            let result = ResizeArray<string>()
            foldEntityName result typeQueryNode.exprName
            result.AsArray
        let resolvedType = ctx.checker.getTypeFromTypeNode typeQueryNode
        let innerTag =
            ctx.CreateXanthamTag resolvedType |> fst
            |> TagState.apply (function
                | true -> _.chainDebug(xanTag) >> pushToStack ctx
                | false -> _.chainDebug(xanTag) >> ignore)
            |> _.Value
        {
            STypeQueryBuilder.FullyQualifiedName = entityName
            Type = innerTag.TypeSignal
        }
        |> SType.TypeQuery
        |> setAstSignal
        // Type query uses a generated type signal
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag
    | TypeNode.TypeOperator typeOperatorNode ->
        nameof TypeNode.TypeOperator |> debugMessage
        // keyof T → union of property keys; unique symbol → UniqueESSymbol; readonly → inner type
        routeViaChecker typeOperatorNode
    | TypeNode.IndexedAccessType indexedAccessTypeNode ->
        nameof TypeNode.IndexedAccessType |> debugMessage
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
        nameof TypeNode.MappedType |> debugMessage
        // { [K in keyof T]: T[K] } — route via checker to ObjectFlags.Mapped at the type layer
        routeViaChecker mappedTypeNode
    | TypeNode.ConditionalType conditionalTypeNode ->
        nameof TypeNode.ConditionalType |> debugMessage
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
        nameof TypeNode.TemplateLiteralType |> debugMessage
        let innerTag =
            match ctx.checker.getTypeAtLocation templateLiteralTypeNode |> ctx.CreateXanthamTag |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeNode.TemplateLiteralTypeSpan _ ->
        nameof TypeNode.TemplateLiteralTypeSpan |> debugMessage
        // only reached as child of TemplateLiteralType; handled above
    | TypeNode.ImportType importTypeNode ->
        nameof TypeNode.ImportType |> debugMessage
        // import("module").TypeName — resolve via checker
        routeViaChecker importTypeNode
    | TypeNode.FunctionType funcTypeNode ->
        nameof TypeNode.FunctionType |> debugMessage
        // (x: T) => R — emit as a CallSignature with method-scope typars.
        {
            Members = [|
                {
                    SCallSignatureBuilder.Parameters = getParamSlots funcTypeNode.parameters
                    TypeParameters = getTypeParamSlots funcTypeNode.typeParameters
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
        nameof TypeNode.ConstructorType |> debugMessage
        // new (x: T) => R — emit as a ConstructSignature with method-scope typars.
        {
            Members = [|
                {
                    SConstructSignatureBuilder.Parameters = getParamSlots ctorTypeNode.parameters
                    TypeParameters = getTypeParamSlots ctorTypeNode.typeParameters
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
        nameof TypeNode.TypeLiteral |> debugMessage
        {
            STypeLiteralBuilder.Members =
                typeLiteralNode.members.AsArray
                |> Array.map (Member.resolveToMemberBuilder ctx)
        }
        |> SType.TypeLiteral
        |> setAstSignal
        setTypeKeyFromNode typeLiteralNode
    | TypeNode.LiteralType literalToken ->
        nameof TypeNode.LiteralType |> debugMessage
        LiteralTokenNode.dispatch
            ctx
            xanTag
            (LiteralTokenNodes.Create (unbox<Ts.Node> literalToken.literal))
        // It would be incorrect for the literaltokennode dispatcher to set the type signal,
        // as the type key is held by the type node, not the literal token node.
        // This was the cause of the bug where the type key was being incorrectly set for literal tokens to `5`.
        setTypeKeyFromNode literalToken
    // this type node doesn't really resolve to anything
    // without context
    | TypeNode.ThisType thisTypeNode ->
        nameof TypeNode.ThisType |> debugMessage
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
        
    | TypeNode.SatisfiesExpression _ -> nameof TypeNode.SatisfiesExpression |> debugMessage
    | TypeNode.AsExpression _ -> nameof TypeNode.AsExpression |> debugMessage
    | TypeNode.TypeAssertion _ -> nameof TypeNode.TypeAssertion |> debugMessage
