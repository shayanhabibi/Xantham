module Xantham.Fable.Reading.MemberDeclaration

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

let setParameterSignal (xanTag: XanthamTag) (param: SParameterBuilder) =
    GuardedData.ParameterBuilder.getOrSetDefault xanTag
    |> Signal.fill param

let setMemberSignal (xanTag: XanthamTag) (member': SMemberBuilder) =
    GuardedData.MemberBuilder.getOrSetDefault xanTag
    |> Signal.fill member'

let setConstructorSignal (xanTag: XanthamTag) (constructor: SConstructorBuilder) =
    GuardedData.ConstructorBuilder.getOrSetDefault xanTag
    |> Signal.fill constructor

// ---------------------------------------------------------------------------
// Module-level helpers (shared across all declaration sub-readers)
// ---------------------------------------------------------------------------

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    match ctx.CreateXanthamTag node with
    | TagState.Unvisited tag, _ -> pushToStack ctx tag; ctx.typeSignal tag
    | TagState.Visited tag, _ -> ctx.typeSignal tag

/// Returns Void TypeSignal when the type annotation is absent.
let inline private getReturnTypeSignal (ctx: TypeScriptReader) (typeNode: Ts.TypeNode option) =
    typeNode
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Void.TypeKey)

/// Resolves an optional TypeNode to a TypeSignal, falling back to checker.getTypeAtLocation when None.
let private getSignalFromTypeNodeOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) =
    typ
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.orElseWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key |> Some
        | TagState.Unvisited typeTag, _ ->
            pushToStack ctx typeTag
            ctx.typeSignal typeTag |> Some
        | TagState.Visited typeTag, _ ->
            ctx.typeSignal typeTag |> Some
        )
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Any.TypeKey)

/// Returns TypeSignal voption for constraint/default slots on type parameters.
let inline getTypeSignalFromOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) =
    typ
    |> Option.map (ctx.CreateXanthamTag >> function
        | TagState.Unvisited tag, _ ->
            pushToStack ctx tag
            ctx.typeSignal tag
        | TagState.Visited tag, _ -> ctx.typeSignal tag
        )
    |> Option.toValueOption

let inline getFullyQualifiedName (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.IdentityKey with
    | IdentityKey.AliasSymbol symbol | IdentityKey.Symbol symbol ->
        let name = ctx.checker.getFullyQualifiedName symbol
        if name.StartsWith("{") then [||]
        else name.Split('.')
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
        >> fst >> function
        | TagState.Unvisited tag ->
            pushToStack ctx tag
            GuardedData.TypeSignal.getOrSetDefault tag,
            GuardedData.AstNodeBuilder.getOrSetDefault tag
        | TagState.Visited tag ->
            GuardedData.TypeSignal.getOrSetDefault tag,
            GuardedData.AstNodeBuilder.getOrSetDefault tag
        >> fun signals -> signals ||> Signal.map2 (fun typeKey -> function
            | ValueSome (STsAstNodeBuilder.TypeParameter tp) ->
                ValueSome {
                    Type = typeKey
                    TypeParameter = tp
                }
            | _ -> ValueNone
            )
        )

module Property =
    let readSignature (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.PropertySignature) =
        {
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
        |> SMemberBuilder.Property
        |> setMemberSignal xanTag

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.PropertyDeclaration) =
        {
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
        |> SMemberBuilder.Property
        |> setMemberSignal xanTag

module Method =
    let readSignature (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.MethodSignature) =
        {
            SMethodBuilder.Name = NameHelpers.getName node.name
            Parameters = getParameterSlots ctx node.parameters
            Type = getReturnTypeSignal ctx node.``type``
            IsOptional = node.questionToken.IsSome
            IsStatic = false
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SMemberBuilder.Method
        |> setMemberSignal xanTag

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.MethodDeclaration) =
        {
            SMethodBuilder.Name = NameHelpers.getName node.name
            Parameters = getParameterSlots ctx node.parameters
            Type = getReturnTypeSignal ctx node.``type``
            IsOptional = false
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SMemberBuilder.Method
        |> setMemberSignal xanTag

module IndexSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.IndexSignatureDeclaration) =
        {
            SIndexSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
            Type = getTypeSignalFromNode ctx node.``type``
            IsReadOnly = node.modifiers |> unbox |> optionArrayHasModifier _.IsReadOnly
        }
        |> SMemberBuilder.IndexSignature
        |> setMemberSignal xanTag

module CallSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.CallSignatureDeclaration) =
        {
            SCallSignatureBuilder.Parameters = getParameterSlots ctx node.parameters
            Type = getReturnTypeSignal ctx node.``type``
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SMemberBuilder.CallSignature
        |> setMemberSignal xanTag

module ConstructSignature =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructSignatureDeclaration) =
        {
            SConstructSignatureBuilder.Type = getReturnTypeSignal ctx node.``type``
            Parameters = getParameterSlots ctx node.parameters
        }
        |> SMemberBuilder.ConstructSignature
        |> setMemberSignal xanTag

module Constructor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ConstructorDeclaration) =
        {
            SConstructorBuilder.Parameters = getParameterSlots ctx node.parameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> setConstructorSignal xanTag

module GetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.GetAccessorDeclaration) =
        {
            SGetAccessorBuilder.Name = NameHelpers.getName node.name
            Type =
                getReturnTypeSignal ctx node.``type``
                |> ctx.routeTypeTo xanTag
            IsStatic = node.modifiers |> unbox |> optionArrayHasModifier _.IsStatic
            IsPrivate = node.modifiers |> unbox |> optionArrayHasModifier _.IsPrivate
        }
        |> SMemberBuilder.GetAccessor
        |> setMemberSignal xanTag

module SetAccessor =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.SetAccessorDeclaration) =
        // Argument type comes from the first parameter; fall back to checker on the node itself.
        let argType =
            node.parameters.AsArray
            |> Array.tryHead
            |> Option.bind _.``type``
            |> fun t -> getSignalFromTypeNodeOption ctx t (unbox<Ts.Node> node)
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
        |> setMemberSignal xanTag

module Parameter =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ParameterDeclaration) =
        {
            SParameterBuilder.Name = NameHelpers.getName node.name
            IsOptional = node.questionToken.IsSome
            IsSpread = node.dotDotDotToken.IsSome
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node)
                |> ctx.routeTypeTo xanTag
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> setParameterSignal xanTag
        
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
