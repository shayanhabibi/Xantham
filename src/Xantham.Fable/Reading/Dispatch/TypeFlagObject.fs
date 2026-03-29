module Xantham.Fable.Reading.TypeFlagObject

open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Create an index-signature member slot from an optional index type.
let private makeIndexSlot (ctx: TypeScriptReader) (primitiveType: TypeKindPrimitive) (typeMaybe: Ts.Type option) : Signal<SMemberBuilder voption> voption =
    typeMaybe
    |> Option.map (fun typ ->
        {
            SIndexSignatureBuilder.Parameters =
                [|
                    {
                        SParameterBuilder.Name = "key"
                        IsOptional = false
                        IsSpread = false
                        Type = TypeSignal.ofKey primitiveType.TypeKey
                        Documentation = []
                    }
                    |> ValueSome
                    |> Signal.source
                |]
            Type =
                match ctx.CreateXanthamTag typ with
                | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
                    TypeSignal.ofKey ctx.signalCache[guard.Value].Key
                | TagState.Unvisited tag, _ ->
                    pushToStack ctx tag
                    ctx.typeSignal tag
                | TagState.Visited tag, _ ->
                    ctx.typeSignal tag
            IsReadOnly = false
        }
        |> SMemberBuilder.IndexSignature
        |> ValueSome
        |> Signal.source
        )
    |> Option.toValueOption

/// Forward this tag's signals to the first usable declaration of a named type's symbol.
/// Skips re-export specifiers (ExportSpecifier, ExportDeclaration, etc.) which are Ignore-
/// classified and would leave the forwarded signals permanently unfulfilled.
let private forwardToSymbolDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (sym: Ts.Symbol) =
    let firstUsableDecl =
        sym.declarations
        |> Option.bind (fun decls ->
            decls.AsArray
            |> Array.tryFind (fun d ->
                not (ModulesAndExports.IsModulesAndExportsKind d)))
    match firstUsableDecl with
    | None -> ()
    | Some decl ->
        let innerTag =
            match ctx.CreateXanthamTag (unbox<Ts.Node> (decl :> obj)) |> fst with
            | TagState.Unvisited t -> pushToStack ctx t; t
            | TagState.Visited t -> t
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (ctx.typeSignal innerTag).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault innerTag).Value)

/// Build parameter slots from the checker-level parameters of a Ts.Signature.
let private signatureToParamSlots (ctx: TypeScriptReader) (signature: Ts.Signature) =
    signature.getParameters().AsArray
    |> Array.map (fun sym ->
        match sym.valueDeclaration with
        | Some decl ->
            let state = ctx.CreateXanthamTag (unbox<Ts.Node> decl) |> fst
            let t = state.Value
            if state.IsUnvisited then pushToStack ctx t
            GuardedData.AstNodeBuilder.getOrSetDefault t
            |> Signal.map (function
                | ValueSome (STsAstNodeBuilder.Parameter p) -> ValueSome p
                | _ -> ValueNone
                )
        | None ->
            // Synthesized parameter — build inline from the checker
            let t =
                match ctx.CreateXanthamTag (ctx.checker.getTypeOfSymbol sym) |> fst with
                | TagState.Unvisited t -> pushToStack ctx t; t
                | TagState.Visited t -> t
            {
                SParameterBuilder.Name = sym.name
                IsOptional = sym.flags.HasFlag Ts.SymbolFlags.Optional
                IsSpread = false
                Type = ctx.typeSignal t
                Documentation = []
            }
            |> ValueSome
            |> Signal.source
        )

/// Convert a Ts.Signature to a call-signature member slot.
let private callSigToMemberSlot (ctx: TypeScriptReader) (signature: Ts.Signature) : Signal<SMemberBuilder voption> =
    let decl = signature.getDeclaration()
    if !!decl then
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> decl) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        GuardedData.AstNodeBuilder.getOrSetDefault t
        |> Signal.map (function
            | ValueSome (STsAstNodeBuilder.CallSignature cs) -> SMemberBuilder.CallSignature cs |> ValueSome
            | _ -> ValueNone
            )
    else
        let returnTag =
            match ctx.CreateXanthamTag (ctx.checker.getReturnTypeOfSignature signature) |> fst with
            | TagState.Unvisited t -> pushToStack ctx t; t
            | TagState.Visited t -> t
        {
            SCallSignatureBuilder.Parameters = signatureToParamSlots ctx signature
            Type = ctx.typeSignal returnTag
            Documentation = []
        }
        |> SMemberBuilder.CallSignature
        |> ValueSome
        |> Signal.source

/// Convert a Ts.Signature to a construct-signature member slot.
let private constructSigToMemberSlot (ctx: TypeScriptReader) (signature: Ts.Signature) : Signal<SMemberBuilder voption> =
    let decl = signature.getDeclaration()
    if !!decl then
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> decl) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        GuardedData.AstNodeBuilder.getOrSetDefault t
        |> Signal.map (function
            | ValueSome (STsAstNodeBuilder.ConstructSignature cs) -> SMemberBuilder.ConstructSignature cs |> ValueSome
            | _ -> ValueNone
            )
    else
        let returnTag =
            match ctx.CreateXanthamTag (ctx.checker.getReturnTypeOfSignature signature) |> fst with
            | TagState.Unvisited t -> pushToStack ctx t; t
            | TagState.Visited t -> t
        {
            SConstructSignatureBuilder.Parameters = signatureToParamSlots ctx signature
            Type = ctx.typeSignal returnTag
        }
        |> SMemberBuilder.ConstructSignature
        |> ValueSome
        |> Signal.source

/// Convert a property symbol to a member slot.
let private propertySymToMemberSlot (ctx: TypeScriptReader) (sym: Ts.Symbol) : Signal<SMemberBuilder voption> =
    match sym.valueDeclaration with
    | Some decl ->
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> decl) |> fst
        let t = state.Value
        if state.IsUnvisited then pushToStack ctx t
        GuardedData.AstNodeBuilder.getOrSetDefault t
        |> Signal.map (function
            | ValueNone -> ValueNone
            | ValueSome b ->
                match b with
                | Property p       -> SMemberBuilder.Property p       |> ValueSome
                | Method m         -> SMemberBuilder.Method m         |> ValueSome
                | GetAccessor g    -> SMemberBuilder.GetAccessor g    |> ValueSome
                | SetAccessor s    -> SMemberBuilder.SetAccessor s    |> ValueSome
                | CallSignature cs -> SMemberBuilder.CallSignature cs |> ValueSome
                | _ -> ValueNone
            )
    | None ->
        // Synthesized / computed property — build inline from the checker
        let t =
            match ctx.CreateXanthamTag (ctx.checker.getTypeOfSymbol sym) |> fst with
            | TagState.Unvisited t -> pushToStack ctx t; t
            | TagState.Visited t -> t
        {
            SPropertyBuilder.Name = sym.name
            Type = ctx.typeSignal t
            IsStatic = false
            IsOptional = sym.flags.HasFlag Ts.SymbolFlags.Optional
            IsPrivate = false
            Accessor = TsAccessor.ReadWrite
            Documentation = []
        }
        |> SMemberBuilder.Property
        |> ValueSome
        |> Signal.source

/// Enumerate all members of an object type (props + call/construct sigs + index sigs).
let private buildMembersFromType (ctx: TypeScriptReader) (objType: Ts.ObjectType) =
    let props =
        ctx.checker.getPropertiesOfType(objType).AsArray
        |> Array.map (propertySymToMemberSlot ctx)
    let callSigs =
        ctx.checker.getSignaturesOfType(objType, Ts.SignatureKind.Call).AsArray
        |> Array.map (callSigToMemberSlot ctx)
    let constructSigs =
        ctx.checker.getSignaturesOfType(objType, Ts.SignatureKind.Construct).AsArray
        |> Array.map (constructSigToMemberSlot ctx)
    let numberIndex = makeIndexSlot ctx TypeKindPrimitive.Number (objType.getNumberIndexType())
    let stringIndex = makeIndexSlot ctx TypeKindPrimitive.String (objType.getStringIndexType())
    [|
        yield! callSigs
        yield! constructSigs
        yield! props
        if numberIndex.IsSome then yield numberIndex.Value
        if stringIndex.IsSome then yield stringIndex.Value
    |]

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: TypeFlagObject) =
    let inline setAstSignal (astValue: STsAstNodeBuilder) =
        xanTag.Builder <- astValue
    match tag with
    | TypeFlagObject.Anonymous anonType ->
        {
            STypeLiteralBuilder.Members = buildMembersFromType ctx anonType
        }
        |> STsAstNodeBuilder.TypeLiteral
        |> setAstSignal
        anonType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Interface interfaceType ->
        // Route to the InterfaceDeclaration node; the node-level dispatch fills the builders.
        interfaceType.symbol
        |> Option.ofObj
        |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        interfaceType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Class classType ->
        // Route to the ClassDeclaration node.
        classType.symbol
        |> Option.ofObj
        |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        classType.TypeKey |> setTypeKeyForTag xanTag

    | TypeFlagObject.Mapped mappedType ->
        // Try enumerating concrete properties first; fall back to a string-index for generic cases.
        let props = ctx.checker.getPropertiesOfType(mappedType).AsArray
        let members =
            if props.Length > 0 then
                Array.map (propertySymToMemberSlot ctx) props
            else
                // Fully generic mapped type — emit { [key: string]: any } as a safe fallback
                [|
                    {
                        SIndexSignatureBuilder.Parameters =
                            [|
                                {
                                    SParameterBuilder.Name = "key"
                                    IsOptional = false
                                    IsSpread = false
                                    Type = TypeSignal.ofKey TypeKindPrimitive.String.TypeKey
                                    Documentation = []
                                }
                                |> ValueSome
                                |> Signal.source
                            |]
                        Type = TypeSignal.ofKey TypeKindPrimitive.Any.TypeKey
                        IsReadOnly = false
                    }
                    |> SMemberBuilder.IndexSignature
                    |> ValueSome
                    |> Signal.source
                |]
        {
            STypeLiteralBuilder.Members = members
        }
        |> STsAstNodeBuilder.TypeLiteral
        |> setAstSignal
        mappedType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Instantiated objType ->
        // A generic type applied to concrete arguments — same shape as Reference.
        // If the target equals itself (uninstantiated), forward to the declaration instead.
        let typeRef = unbox<Ts.TypeReference> objType
        if typeRef.target.TypeKey = typeRef.TypeKey then
            typeRef.getSymbol() |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        else
            TypeReference.fromType ctx xanTag typeRef
        objType.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagObject.Reference typeReference ->
        // If the target equals the reference itself (uninstantiated generic like bare `Array`),
        // forward to the symbol declaration to avoid a self-referential TypeReference entry.
        if typeReference.target.TypeKey = typeReference.TypeKey then
            typeReference.getSymbol() |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        else
            TypeReference.fromType ctx xanTag typeReference
        typeReference.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagObject.Tuple tupleType ->
        let elementTypes = ctx.checker.getTypeArguments(unbox<Ts.TypeReference> tupleType).AsArray
        let elementFlags = tupleType.elementFlags.AsArray
        let labeledDecls =
            tupleType.labeledElementDeclarations
            |> Option.map _.AsArray
            |> Option.defaultValue (Array.create elementTypes.Length None)
        let buildElement (typ: Ts.Type) (flags: Ts.ElementFlags) (labelDecl: U2<Ts.NamedTupleMember, Ts.ParameterDeclaration> option) =
            let typeTag =
                match ctx.CreateXanthamTag typ |> fst with
                | TagState.Unvisited t -> pushToStack ctx t; t
                | TagState.Visited t -> t
            let typeSignal = ctx.typeSignal typeTag
            if flags.HasFlag Ts.ElementFlags.Variadic || flags.HasFlag Ts.ElementFlags.Rest then
                STupleElementBuilder.Variadic typeSignal
            else
                match labelDecl with
                | Some (U2.Case1 namedMember) ->
                    STupleElementBuilder.FixedLabeled(
                        namedMember.name.text,
                        {
                            STupleElementTypeBuilder.Type = typeSignal
                            IsOptional = flags.HasFlag Ts.ElementFlags.Optional
                            IsRest = false
                        }
                        )
                | _ ->
                    STupleElementBuilder.Fixed {
                        Type = typeSignal
                        IsOptional = flags.HasFlag Ts.ElementFlags.Optional
                        IsRest = false
                    }
        {
            STupleBuilder.IsReadOnly = tupleType.readonly
            FixedLength = int tupleType.fixedLength
            MinRequired = int tupleType.minLength
            Types = Array.map3 buildElement elementTypes elementFlags labeledDecls
        }
        |> STsAstNodeBuilder.Tuple
        |> setAstSignal
        tupleType.TypeKey |> setTypeKeyForTag xanTag

    | TypeFlagObject.EvolvingArray _ ->
        () // Internal checker type; never present in .d.ts files
