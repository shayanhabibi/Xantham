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

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    ctx.CreateXanthamTag node |> fst |> stackPushAndThen ctx _.TypeSignal

/// Returns Void TypeSignal when the type annotation is absent.
let inline private getReturnTypeSignal (ctx: TypeScriptReader) (typeNode: Ts.TypeNode option) =
    typeNode
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Void.TypeKey)

/// Resolves an optional TypeNode to a TypeSignal, falling back to checker.getTypeAtLocation when None.
let private getSignalFromTypeNodeOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) =
    typ
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.defaultWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key
        | tagState, _ -> stackPushAndThen ctx _.TypeSignal tagState
        )
/// Returns TypeSignal voption for constraint/default slots on type parameters.
let inline getTypeSignalFromOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) =
    typ
    |> Option.map (getTypeSignalFromNode ctx)
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
    parameters.AsArray
    |> Array.map (Member.resolveToParameterBuilder ctx)

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
                getSignalFromTypeNodeOption ctx node.``type`` node
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
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node)
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
        let builder = {
            SMethodBuilder.Name = NameHelpers.getName node.name
            Parameters = getParameterSlots ctx node.parameters
            Type = getReturnTypeSignal ctx node.``type``
            IsOptional = node.questionToken.IsSome
            IsStatic = false
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.MemberBuilder <-  builder |> SMemberBuilder.Method

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.MethodDeclaration) =
        xanTag.MemberBuilder <-
            {
                SMethodBuilder.Name = NameHelpers.getName node.name
                Parameters = getParameterSlots ctx node.parameters
                Type = getReturnTypeSignal ctx node.``type``
                IsOptional = false
                IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }
            |> SMemberBuilder.Method

module IndexSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.IndexSignatureDeclaration) =
        xanTag.MemberBuilder <-
            {
                SIndexSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
                Type = getTypeSignalFromNode ctx node.``type``
                IsReadOnly = node.modifiers |> unbox |> optionArrayHasModifier _.IsReadOnly
            }
            |> SMemberBuilder.IndexSignature

module CallSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.CallSignatureDeclaration) =
        xanTag.MemberBuilder <-
            {
                SCallSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
                Type = getReturnTypeSignal ctx node.``type``
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }
            |> SMemberBuilder.CallSignature

module ConstructSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructSignatureDeclaration) =
        xanTag.MemberBuilder <-
            {
                SConstructSignatureBuilder.Type = getReturnTypeSignal ctx node.``type``
                Parameters = getParameterSlots ctx node.parameters
            }
            |> SMemberBuilder.ConstructSignature

module Constructor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructorDeclaration) =
        xanTag.ConstructorBuilder <-
            {
                SConstructorBuilder.Parameters = getParameterSlots ctx node.parameters
                Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            }

module GetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.GetAccessorDeclaration) =
        xanTag.MemberBuilder <-
        {
            SGetAccessorBuilder.Name = NameHelpers.getName node.name
            Type =
                getReturnTypeSignal ctx node.``type``
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            IsPrivate = node.modifiers |> unbox |> optionArrayHasModifier _.IsPrivate
        }
        |> SMemberBuilder.GetAccessor

module SetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.SetAccessorDeclaration) =
        // Argument type comes from the first parameter; fall back to checker on the node itself.
        let argType =
            node.parameters.AsArray
            |> Array.tryHead
            |> Option.bind _.``type``
            |> fun t -> getSignalFromTypeNodeOption ctx t (unbox<Ts.Node> node)
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
        xanTag.ParameterBuilder <-
        {
            SParameterBuilder.Name = NameHelpers.getName node.name
            IsOptional = node.questionToken.IsSome
            IsSpread = node.dotDotDotToken.IsSome
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node)
                |> ctx.routeTypeTo xanTag
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        
let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: MemberDeclaration) =
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
