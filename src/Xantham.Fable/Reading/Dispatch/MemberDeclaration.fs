module Xantham.Fable.Reading.MemberDeclaration

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

// ---------------------------------------------------------------------------
// Module-level helpers (shared across all declaration sub-readers)
// ---------------------------------------------------------------------------

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (parent: XanthamTag) (node: Ts.TypeNode)  =
    ctx.CreateXanthamTag node |> fst
    |> fun tagState ->
        XanthamTag.chainDebug parent tagState.Value
        |> XanthamTag.debugLocationAndForget "MemberDeclaration.getTypeSignalFromNode"
        tagState
    |> stackPushAndThen ctx _.TypeSignal

/// Returns Void TypeSignal when the type annotation is absent.
let inline private getReturnTypeSignal (ctx: TypeScriptReader) (parent: XanthamTag) (typeNode: Ts.TypeNode option) =
    typeNode
    |> Option.map (getTypeSignalFromNode ctx parent)
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Void.TypeKey)

/// Resolves an optional TypeNode to a TypeSignal, falling back to checker.getTypeAtLocation when None.
let private getSignalFromTypeNodeOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) (parentTag: XanthamTag) =
    typ
    |> Option.map (getTypeSignalFromNode ctx parentTag)
    |> Option.defaultWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            XanthamTag.chainDebug parentTag (unbox guard) |> ignore
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key
        | tagState, _ ->
            XanthamTag.chainDebug parentTag (unbox tagState.Value) |> ignore
            stackPushAndThen ctx _.TypeSignal tagState
        )
/// Returns TypeSignal voption for constraint/default slots on type parameters.
let inline getTypeSignalFromOption (ctx: TypeScriptReader) (parent: XanthamTag) (typ: Ts.TypeNode option) =
    typ
    |> Option.map (getTypeSignalFromNode ctx parent)
    |> Option.toValueOption

let inline getFullyQualifiedName (ctx: TypeScriptReader) (tag: XanthamTag) =
    let name =
        match tag.IdentityKey with
        | IdentityKey.AliasSymbol symbol | IdentityKey.Symbol symbol ->
            ctx.checker.getFullyQualifiedName symbol
            |> Some
        | IdentityKey.DeclarationPosition _ ->
            match tag.ToUnderlyingValue() with
            | Choice1Of2 typ ->
                typ.aliasSymbol
                |> Option.orElse (typ.getSymbol())
                |> Option.map ctx.checker.getFullyQualifiedName
            | Choice2Of2 decl ->
                ctx.checker.getSymbolAtLocation decl
                |> Option.orElseWith (fun () ->
                    let typ = ctx.checker.getTypeAtLocation decl
                    typ.aliasSymbol
                    |> Option.orElse (typ.getSymbol()))
                |> Option.map ctx.checker.getFullyQualifiedName
        | _ -> None
    match name with
    | Some name when not(name.StartsWith"{") -> name.Split('.')
    | _ -> [||]

/// Maps parameter declarations to reactive builder slots.
let private getParameterSlots (ctx: TypeScriptReader) (parameters: ResizeArray<Ts.ParameterDeclaration>) =
    let parameterValues =
        parameters.AsArray
        |> Array.map (Member.resolveToParameterBuilder ctx)
    parameterValues

/// Maps type parameter declarations to reactive builder slots.
let private getTypeParamSlots (ctx: TypeScriptReader) (typeParams: ResizeArray<Ts.TypeParameterDeclaration> option) =
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

module Property =
    let readSignature (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.PropertySignature) =
        let builder = {
            SPropertyBuilder.Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            Name = NameHelpers.getName node.name
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` node xanTag 
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> optionArrayHasModifier _.IsStatic
            IsOptional = node.questionToken.IsSome
            IsPrivate = node.modifiers |> optionArrayHasModifier _.IsPrivate
            Accessor =
                if node.modifiers |> optionArrayHasModifier _.IsReadOnly
                then TsAccessor.ReadOnly
                else TsAccessor.ReadWrite
        }
        xanTag.MemberBuilder <- builder |> SMemberBuilder.Property

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.PropertyDeclaration) =
        let builder = {
            Name = NameHelpers.getName node.name
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node) xanTag
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            IsOptional = node.questionToken.IsSome
            IsPrivate = node.modifiers |> unbox |> optionArrayHasModifier _.IsPrivate
            Accessor =
                if node.modifiers |> unbox |> optionArrayHasModifier _.IsReadOnly
                then TsAccessor.ReadOnly
                else TsAccessor.ReadWrite
            SPropertyBuilder.Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.MemberBuilder <- builder |> SMemberBuilder.Property

module Method =
    let readSignature (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.MethodSignature) =
        XanthamTag.debugLocationAndForget "Method.readSignature" xanTag
        let builder = {
            SMethodBuilder.Name = NameHelpers.getName node.name
            Parameters = getParameterSlots ctx node.parameters
            Type = getReturnTypeSignal ctx xanTag node.``type``
            IsOptional = node.questionToken.IsSome
            IsStatic = false
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.MemberBuilder <-  builder |> SMemberBuilder.Method

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.MethodDeclaration) =
        XanthamTag.debugLocationAndForget "Method.readDeclaration" xanTag
        xanTag.MemberBuilder <-
            {
                SMethodBuilder.Name = NameHelpers.getName node.name
                Parameters = getParameterSlots ctx node.parameters
                Type = getReturnTypeSignal ctx xanTag node.``type``
                IsOptional = false
                IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }
            |> SMemberBuilder.Method

module IndexSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.IndexSignatureDeclaration) =
        XanthamTag.debugLocationAndForget "IndexSignature.read" xanTag
        xanTag.MemberBuilder <-
            {
                SIndexSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
                Type = getTypeSignalFromNode ctx xanTag node.``type``
                IsReadOnly = node.modifiers |> unbox |> optionArrayHasModifier _.IsReadOnly
            }
            |> SMemberBuilder.IndexSignature

module CallSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.CallSignatureDeclaration) =
        XanthamTag.debugLocationAndForget "CallSignature.read" xanTag
        xanTag.MemberBuilder <-
            {
                SCallSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
                Type = getReturnTypeSignal ctx xanTag node.``type``
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }
            |> SMemberBuilder.CallSignature

module ConstructSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructSignatureDeclaration) =
        XanthamTag.debugLocationAndForget "ConstructSignature.read" xanTag
        xanTag.MemberBuilder <-
            {
                SConstructSignatureBuilder.Type = getReturnTypeSignal ctx xanTag node.``type``
                Parameters = getParameterSlots ctx node.parameters
            }
            |> SMemberBuilder.ConstructSignature

module Constructor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructorDeclaration) =
        XanthamTag.debugLocationAndForget "Constructor.read" xanTag
        xanTag.ConstructorBuilder <-
            {
                SConstructorBuilder.Parameters = getParameterSlots ctx node.parameters
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }

module GetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.GetAccessorDeclaration) =
        XanthamTag.debugLocationAndForget "GetAccessor.read" xanTag
        xanTag.MemberBuilder <-
        {
            SGetAccessorBuilder.Name = NameHelpers.getName node.name
            Type =
                getReturnTypeSignal ctx xanTag node.``type``
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            IsPrivate = node.modifiers |> unbox |> optionArrayHasModifier _.IsPrivate
        }
        |> SMemberBuilder.GetAccessor

module SetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.SetAccessorDeclaration) =
        XanthamTag.debugLocationAndForget "SetAccessor.read" xanTag
        // Argument type comes from the first parameter; fall back to checker on the node itself.
        let argType =
            node.parameters.AsArray
            |> Array.tryHead
            |> Option.bind _.``type``
            |> fun t -> getSignalFromTypeNodeOption ctx t (unbox<Ts.Node> node) xanTag
        xanTag.MemberBuilder <-
        {
            SSetAccessorBuilder.Name = NameHelpers.getName node.name
            ArgumentType =
                argType
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            IsPrivate = node.modifiers |> unbox |> optionArrayHasModifier _.IsPrivate
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SMemberBuilder.SetAccessor

module Parameter =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ParameterDeclaration) =
        XanthamTag.debugLocationAndForget "Parameter.read" xanTag
        xanTag.ParameterBuilder <-
        {
            SParameterBuilder.Name = NameHelpers.getName node.name
            IsOptional = node.questionToken.IsSome
            IsSpread = node.dotDotDotToken.IsSome
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node) xanTag
                |> ctx.routeTypeTo xanTag
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        
let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: MemberDeclaration) =
    XanthamTag.debugLocationAndForget "MemberDeclaration.dispatch" xanTag
    match node with
    | MemberDeclaration.PropertySignature propertySignature ->
        Property.readSignature ctx xanTag propertySignature
    | MemberDeclaration.MethodSignature methodSignature ->
        Method.readSignature ctx xanTag methodSignature
    | MemberDeclaration.IndexSignature indexSignatureDeclaration ->
        IndexSignature.read ctx xanTag indexSignatureDeclaration
    | MemberDeclaration.CallSignature callSignatureDeclaration ->
        CallSignature.read ctx xanTag callSignatureDeclaration
    | MemberDeclaration.ConstructSignature constructSignatureDeclaration ->
        ConstructSignature.read ctx xanTag constructSignatureDeclaration
    | MemberDeclaration.Property propertyDeclaration ->
        Property.readDeclaration ctx xanTag propertyDeclaration
    | MemberDeclaration.Method methodDeclaration ->
        Method.readDeclaration ctx xanTag methodDeclaration
    | MemberDeclaration.Constructor constructorDeclaration ->
        Constructor.read ctx xanTag constructorDeclaration
    | MemberDeclaration.GetAccessor accessorDeclaration ->
        GetAccessor.read ctx xanTag accessorDeclaration
    | MemberDeclaration.SetAccessor accessorDeclaration ->
        SetAccessor.read ctx xanTag accessorDeclaration
    | MemberDeclaration.Parameter parameterDeclaration ->
        Parameter.read ctx xanTag parameterDeclaration
