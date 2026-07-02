module Xantham.Fable.Reading.TypeFlagObject

open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Reading.Member
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
                | tagState, _ -> stackPushAndThen ctx _.TypeSignal tagState
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
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)

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
        // Type-checker-level signature: its type parameters are Ts.Type-level (not declaration
        // nodes), which this slot reader does not resolve — leave empty. Declaration-node methods /
        // call-signatures (the runWorkflow<P> case) capture their type params in MemberDeclaration.
        SCallSignatureBuilder.Parameters = signatureToParamSlots ctx signature
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
        // Type-checker-level signature — type params are Ts.Type-level, not resolved here (empty).
        SConstructSignatureBuilder.Parameters = signatureToParamSlots ctx signature
        TypeParameters = [||]
        Type = returnTag.TypeSignal
    }
    |> SMemberBuilder.ConstructSignature
    |> ValueSome
    |> Signal.source

/// Convert a property symbol to a member slot.
///
/// `closeInstantiated`: an INSTANTIATED anonymous object's property symbols are TRANSIENT clones
/// whose `valueDeclaration` is the OPEN generic declaration node — shared by every instantiation.
/// Routing them to that node emits an open, typar-referencing COPY of the member (the
/// identity-conflation root: `JsonSchemaType = JSONSchema.Interface` came out byte-identical to
/// the open JSONSchema object body, so the decoder's structural compress merged them and the
/// paramless alias claimed the generic body — 16 such shape-groups / 203 keys per the IR
/// sentinel). With the flag set, a transient PropertySignature member takes its TYPE from the
/// checker (the instantiated type) while name/optionality stay symbol-derived; every other
/// symbol keeps the declaration route byte-for-byte. Scoped to PropertySignature declarations
/// (methods keep the node route: overloads/docs) and NOT enabled for the Mapped arm (pinned
/// fragile — mapped-type arg-drop). The OPEN declaration's own members are non-transient, so
/// the open body keeps its faithful typar references.
let private propertySymToMemberSlot (ctx: TypeScriptReader) (closeInstantiated: bool) (sym: Ts.Symbol) : Signal<SMemberBuilder voption> =
    let hybridInstantiated =
        closeInstantiated
        && sym.flags.HasFlag Ts.SymbolFlags.Transient
        && (match sym.valueDeclaration with
            | Some decl -> (!!decl: Ts.Node).kind = Ts.SyntaxKind.PropertySignature
            | None -> false)
    match sym.valueDeclaration with
    | Some decl when not hybridInstantiated ->
        resolveToMemberBuilder ctx !!decl
    | _ ->
        let isOptional = sym.flags.HasFlag Ts.SymbolFlags.Optional
        {
            SPropertyBuilder.Name = sym.name
            Type =
                // Synthesized/computed property (no declaration) OR an instantiated (transient)
                // member routed away from its shared open declaration — build from the checker.
                // strictNullChecks makes getTypeOfSymbol return `T | undefined` for an optional
                // member; `IsOptional` already carries the optionality, so strip the nullable
                // wrapper on the hybrid path (accepts the rare loss of an explicit `| null`
                // on an optional member — the declaration route never saw it either).
                (let symType = ctx.checker.getTypeOfSymbol sym
                 if hybridInstantiated && isOptional then ctx.checker.getNonNullableType symType
                 else symType)
                |> ctx.CreateXanthamTag
                |> fst
                |> stackPushAndThen ctx _.TypeSignal
            IsStatic = false
            IsOptional = isOptional
            IsPrivate = false
            Accessor = TsAccessor.ReadWrite
            Documentation = []
        }
        |> SMemberBuilder.Property
        |> ValueSome
        |> Signal.source

/// Enumerate all members of an object type (props + call/construct sigs + index sigs).
let private buildMembersFromType (ctx: TypeScriptReader) (closeInstantiated: bool) (objType: Ts.ObjectType) =
    let props =
        ctx.checker.getPropertiesOfType(objType).AsArray
        |> Array.map (propertySymToMemberSlot ctx closeInstantiated)
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
    let debugLocation typeFlagObjectType =
        XanthamTag.debugLocationAndForget $"TypeFlagObject.dispatch | %s{typeFlagObjectType}" xanTag
    let inline setAstSignal (astValue: SType) =
        xanTag.Builder <- astValue
    match tag with
    | TypeFlagObject.Anonymous anonType ->
        nameof TypeFlagObject.Anonymous |> debugLocation
        {
            // closeInstantiated=true: an instantiated anonymous object's transient property
            // members take their instantiated types from the checker (identity-conflation fix);
            // the open declaration's members are non-transient and keep the node route.
            STypeLiteralBuilder.Members = buildMembersFromType ctx true anonType
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
        let props = ctx.checker.getPropertiesOfType(mappedType).AsArray
        let members =
            if props.Length > 0 then
                // closeInstantiated=false: every Mapped-arm property symbol is Transient in tsc,
                // so the hybrid predicate would reroute ALL mapped types (Partial/Pick/Readonly)
                // through the checker — a pinned fragile area (mapped-type arg-drop). Mapped
                // members keep the declaration route unchanged.
                Array.map (propertySymToMemberSlot ctx false) props
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
        |> SType.TypeLiteral
        |> setAstSignal
        mappedType.TypeKey
        |> setTypeKeyForTag xanTag

    | TypeFlagObject.Instantiated objType ->
        nameof TypeFlagObject.Instantiated |> debugLocation
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
