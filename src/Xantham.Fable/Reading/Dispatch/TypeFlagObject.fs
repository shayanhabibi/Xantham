module Xantham.Fable.Reading.TypeFlagObject

open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Reading.Member
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

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
                | tagState, _ -> stackPushAndThen ctx _.TypeSignal tagState
            IsReadOnly = false
        }
        |> SMemberBuilder.IndexSignature
        |> ValueSome
        |> Signal.source
        )
    |> Option.toValueOption

/// <summary>
/// Forward the given tag's type signal and builder signal to the first declaration of the given symbol
/// that does not match the tag, and provides a valid builder value.
/// </summary>
let private forwardToSymbolDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (sym: Ts.Symbol) =
    let declarations =
        // if we default to 'the first usable decl', then we may find ourselves in a situation where
        // the first symbol declaration points to the tag itself (and will therefor stall and never resolve).
        sym.declarations
        |> Option.map (
            _.AsArray
            >> Array.map (ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx _.chainDebug(xanTag))
            )
        |> Option.defaultValue [||]
        |> Array.filter (fun tag ->
            tag <> xanTag
            && not (unbox<Ts.Node> tag.Value.Value |> ModulesAndExports.IsModulesAndExportsKind))
    let firstValidTag: Signal<XanthamTag voption> = Signal.source ValueNone
    // We track the declarations, and will accept the first declaration that provides us a builder value.
    // TODO - determinism
    let runner =
        declarations
        |> Array.map _.Builder.Invalidated
        |> Array.toList
        |> Signal.effect (fun () ->
            declarations
            |> Array.tryFind _.Builder.Value.IsSome
            |> Option.iter (fun tag ->
                firstValidTag
                |> Signal.fill tag
                )
            )
    let combinedSignal =
        // When we get a valid tag value, we cease tracking the declarations to save memory.
        Signal.auto (fun () ->
            firstValidTag.Value
            |> ValueOption.map (fun tag ->
                runner.Dispose()
                tag.TypeSignal, tag.Builder
            ))
    xanTag.TypeSignal
    |> Signal.fulfillWith (fun () ->
        combinedSignal.Value
        |> ValueOption.map (fst >> _.Value)
        |> ValueOption.defaultValue (
            declarations
            |> Array.head
            |> _.TypeSignal.Value
            )
        )
    xanTag.Builder
    |> Signal.fulfillWith (fun () ->
        combinedSignal.Value
        |> ValueOption.map (snd >> _.Value)
        |> ValueOption.defaultValue (
            declarations
            |> Array.head
            |> _.Builder.Value
            )
        )

/// Build parameter slots from the checker-level parameters of a Ts.Signature.
let private signatureToParamSlots (ctx: TypeScriptReader) (signature: Ts.Signature) =
    signature.getParameters().AsArray
    |> Array.map (fun sym ->
        match sym.valueDeclaration with
        | Some decl -> Member.resolveToParameterBuilder ctx (unbox decl)
        | None ->
            // Synthesized parameter — build inline from the checker
            let t =
                ctx.checker.getTypeOfSymbol sym
                |> ctx.CreateXanthamTag
                |> fst
                |> stackPushAndThen ctx id
            {
                SParameterBuilder.Name = sym.name
                IsOptional = sym.flags.HasFlag Ts.SymbolFlags.Optional
                IsSpread = false
                Type = t.TypeSignal
                Documentation = []
            }
            |> ValueSome
            |> Signal.source
        )

/// Convert a Ts.Signature to a call-signature member slot.
let private callSigToMemberSlot (ctx: TypeScriptReader) (signature: Ts.Signature) : Signal<SMemberBuilder voption> =
    // let decl = signature.getDeclaration()
    // if !!decl then
    //     resolveToMemberBuilder ctx !!decl
    // else
    let returnTag =
        match ctx.CreateXanthamTag (ctx.checker.getReturnTypeOfSignature signature) |> fst with
        | TagState.Unvisited t -> pushToStack ctx t; t
        | TagState.Visited t -> t
    {
        SCallSignatureBuilder.Parameters = signatureToParamSlots ctx signature
        // TS Signature objects don't expose typeParameters as nodes; the
        // declaration-side reads (CallSignature.read in MemberDeclaration.fs)
        // do. Leave empty for these synthetic signature-only call sites.
        TypeParameters = [||]
        Type = returnTag.TypeSignal
        Documentation = []
    }
    |> SMemberBuilder.CallSignature
    |> ValueSome
    |> Signal.source

/// Convert a Ts.Signature to a construct-signature member slot.
let private constructSigToMemberSlot (ctx: TypeScriptReader) (signature: Ts.Signature) : Signal<SMemberBuilder voption> =
    // let decl = signature.getDeclaration()
    // if !!decl then
    //     resolveToMemberBuilder ctx !!decl
    // else
    let returnTag =
        match ctx.CreateXanthamTag (ctx.checker.getReturnTypeOfSignature signature) |> fst with
        | TagState.Unvisited t -> pushToStack ctx t; t
        | TagState.Visited t -> t
    {
        SConstructSignatureBuilder.Parameters = signatureToParamSlots ctx signature
        TypeParameters = [||]
        Type = returnTag.TypeSignal
    }
    |> SMemberBuilder.ConstructSignature
    |> ValueSome
    |> Signal.source

/// Convert a property symbol to a member slot.
let private propertySymToMemberSlot (ctx: TypeScriptReader) (sym: Ts.Symbol) : Signal<SMemberBuilder voption> =
    match sym.valueDeclaration with
    | Some decl ->
        resolveToMemberBuilder ctx !!decl
    | None ->
        {
            SPropertyBuilder.Name = sym.name
            Type =
                // Synthesized / computed property — build inline from the checker
                ctx.checker.getTypeOfSymbol sym
                |> ctx.CreateXanthamTag
                |> fst
                |> stackPushAndThen ctx _.TypeSignal
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
    let debugLocation = sprintf "Dispatching type flag object of type %s" >> xanTag.doDebugMessage
    let inline setAstSignal (astValue: SType) =
        xanTag.Builder <- astValue
    match tag with
    | TypeFlagObject.Anonymous anonType ->
        nameof TypeFlagObject.Anonymous |> debugLocation
        {
            STypeLiteralBuilder.Members = buildMembersFromType ctx anonType
        }
        |> SType.TypeLiteral
        |> setAstSignal
        anonType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Interface interfaceType ->
        nameof TypeFlagObject.Interface |> debugLocation
        // Route to the InterfaceDeclaration node; the node-level dispatch fills the builders.
        interfaceType.symbol
        |> Option.ofObj
        |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        interfaceType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Class classType ->
        nameof TypeFlagObject.Class |> debugLocation
        // Route to the ClassDeclaration node.
        classType.symbol
        |> Option.ofObj
        |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        classType.TypeKey |> setTypeKeyForTag xanTag

    | TypeFlagObject.Mapped mappedType ->
        nameof TypeFlagObject.Mapped |> debugLocation
        // Try enumerating concrete properties first; fall back to a string-index for generic cases.
        let decl = mappedType.declaration
        let props = ctx.checker.getPropertiesOfType(mappedType).AsArray
        let members =
            if props.Length > 0 then
                Array.map (propertySymToMemberSlot ctx) props
            else
                let keyType =
                    mappedType.nameType
                    |> Option.orElse mappedType.constraintType
                    |> Option.orElse (mappedType.typeParameter |> Option.bind _.getConstraint())
                    |> Option.filter (_.TypeKey >> (<>) mappedType.TypeKey)
                    |> Option.map (pushTypeToStack ctx >> _.TypeSignal)
                let valueType =
                    mappedType.templateType
                    |> Option.filter (_.TypeKey >> (<>) mappedType.TypeKey)
                    |> Option.map (pushTypeToStack ctx >> _.TypeSignal)
                let isReadOnly = decl.readonlyToken.IsSome
                let isOptional = decl.questionToken.IsSome
                [|
                    {
                        SIndexSignatureBuilder.Parameters =
                            [|
                                {
                                    SParameterBuilder.Name = "key"
                                    IsOptional = isOptional
                                    IsSpread = false
                                    Type = keyType |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.String.TypeKey)
                                    Documentation = []
                                }
                                |> ValueSome
                                |> Signal.source
                            |]
                        Type = valueType |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Any.TypeKey)
                        IsReadOnly = isReadOnly
                    }
                    |> SMemberBuilder.IndexSignature
                    |> ValueSome
                    |> Signal.source
                |]
        {
            STypeLiteralBuilder.Members = members
        }
        |> SType.TypeLiteral
        |> setAstSignal
        mappedType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.ReverseMapped objType ->
        nameof TypeFlagObject.ReverseMapped |> debugLocation
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
        nameof TypeFlagObject.Reference |> debugLocation
        // If the target equals the reference itself (uninstantiated generic like bare `Array`),
        // forward to the symbol declaration to avoid a self-referential TypeReference entry.
        if typeReference.target.TypeKey = typeReference.TypeKey then
            typeReference.getSymbol() |> Option.iter (forwardToSymbolDeclaration ctx xanTag)
        else
            TypeReference.fromType ctx xanTag typeReference
        typeReference.TypeKey
        |> setTypeKeyForTag xanTag
    | TypeFlagObject.Tuple tupleType ->
        nameof TypeFlagObject.Tuple |> debugLocation
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
            let typeSignal = typeTag.TypeSignal
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
        |> SType.Tuple
        |> setAstSignal
        tupleType.TypeKey |> setTypeKeyForTag xanTag

    | TypeFlagObject.EvolvingArray _ ->
        nameof TypeFlagObject.EvolvingArray |> debugLocation
        () // Internal checker type; never present in .d.ts files
