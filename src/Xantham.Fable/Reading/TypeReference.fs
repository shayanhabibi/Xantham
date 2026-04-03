module Xantham.Fable.Reading.TypeReference

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

let private resolveBase (ctx: TypeScriptReader) (_xanTag: XanthamTag) (node: Ts.TypeReferenceNode) =
    let rec getSymbol (name: Ts.EntityName): Ts.Symbol option =
        match name with
        | Patterns.Node.EntityNamePatterns.Identifier node ->
            ctx.checker.getSymbolAtLocation node
        | Patterns.Node.EntityNamePatterns.QualifiedName node ->
            match ctx.checker.getSymbolAtLocation node.right with
            | Some value -> Some value
            | None -> getSymbol node.left
    let getTypeSymbol (tracer: TagState<XanthamTag> * TagState<GuardTracer>) =
        let tagState, guardState = tracer
        let guard = guardState.Value
        // Check signalCache regardless of guard's Visited/Unvisited state — same shared-symbol
        // issue as resolveTypeBase: a pre-existing guard (Unvisited) may still have its
        // IdentityKey already registered in the cache.
        if ctx.signalCache.ContainsKey(guard.Value) then
            TypeSignal.ofKey ctx.signalCache[ guard.Value ].Key
        else
            match tagState with
            | TagState.Visited tag -> tag.TypeSignal
            | TagState.Unvisited tag ->
                pushToStack ctx tag
                tag.TypeSignal
    match getSymbol node.typeName with
    | Some symbol when GuardTracer.has symbol && ctx.signalCache.ContainsKey(GuardTracer.unsafeGet symbol |> _.Value) ->
        let store =
            ctx.signalCache[
                GuardTracer.unsafeGet symbol
                |> _.Value
            ]
        TypeSignal.ofKey store.Key
    | Some symbol when symbol.flags.HasFlag Ts.SymbolFlags.TypeParameter ->
        unbox<Ts.TypeNode> node.typeName
        |> ctx.checker.getTypeFromTypeNode
        |> ctx.CreateXanthamTag
        |> getTypeSymbol
    | Some _ ->
        unbox<Ts.Node> node.typeName
        |> ctx.checker.getTypeAtLocation
        |> ctx.CreateXanthamTag
        |> getTypeSymbol
    | None ->
        ctx.checker.getTypeFromTypeNode node
        |> ctx.CreateXanthamTag
        |> getTypeSymbol

let private resolveTypeBase (ctx: TypeScriptReader) (xanTag: XanthamTag) (typ: Ts.TypeReference) =
    let tagState, guardState = ctx.CreateXanthamTag typ.target
    let guard = guardState.Value
    // Check signalCache for the guard's IdentityKey regardless of Visited/Unvisited state.
    // When two distinct types share the same underlying symbol (e.g. Array<string> and Array<T>),
    // the guard's TagState may be Unvisited (pre-existing on the shared symbol) even though the
    // IdentityKey is already registered in the signalCache.  The original Visited-only check
    // missed this case and incorrectly pushed the target to the stack, leading to a cycle when
    // tryGetOrRegisterStore later found the same entry and wrote the outer type's Key into the
    // target's TypeSignal.
    // However, if a typereference itself shares the same underlying symbol as its target,
    // this would result in a cyclical dependency which causes the type to never be emitted.
    if guard <> xanTag.Guard &&  ctx.signalCache.ContainsKey(guard.Value) then
        let cachedKey = ctx.signalCache[ guard.Value ].Key
        // Guard: if the cached entry is for the outer instantiated type itself (shared-symbol
        // scenario, e.g. Array<string> and Array<T> both map to Array's symbol), return the
        // target's own TS TypeKey directly.  That key matches the signalCache entry registered
        // for the generic declaration (interface/class), which is the semantically correct
        // referent for TypeReference.Type.
        if cachedKey = typ.TypeKey then TypeSignal.ofKey typ.target.TypeKey
        else TypeSignal.ofKey cachedKey
    else
        match tagState with
        | TagState.Visited tag -> tag.TypeSignal
        | TagState.Unvisited tag ->
            pushToStack ctx tag
            tag.TypeSignal

let private resolveTypeArgumentsFromType (ctx: TypeScriptReader) (_xanTag: XanthamTag) (typ: Ts.Type) =
    // Use checker.getTypeArguments on the semantic TypeReference so that default
    // type arguments and inferred positions are included, not just what is written.
    if typ.flags.HasFlag Ts.TypeFlags.Object then
        let objType = unbox<Ts.ObjectType> typ
        if objType.objectFlags.HasFlag Ts.ObjectFlags.Reference then
            ctx.checker.getTypeArguments(unbox<Ts.TypeReference> typ).AsArray
            |> Array.map (ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx _.TypeSignal)
        else [||]
    else [||]

let fromNode (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.TypeReferenceNode) =
    // Resolve the instantiated type once; shared by TypeArguments and ResolvedType
    // to avoid redundant checker calls.
    let resolvedType = ctx.checker.getTypeFromTypeNode node
    let resolvedTypeTag = ctx.CreateXanthamTag resolvedType
    {
        STypeReferenceBuilder.Type = resolveBase ctx xanTag node
        TypeArguments = resolveTypeArgumentsFromType ctx xanTag resolvedType
        ResolvedType =
            // Only emit a ResolvedType when the instantiated type is a distinct identity
            // from the tag itself (avoids self-referential noise).
            match resolvedTypeTag with
            | _, tagstate when tagstate.Value = xanTag.Guard -> ValueNone
            | TagState.Visited tag, _ ->
                tag.TypeSignal
                |> Signal.map (fun typeKey ->
                    match xanTag.TryTypeSignal with
                    | ValueSome typeSignal when typeSignal.Value = typeKey -> ValueNone
                    | _ -> ValueSome typeKey)
                |> ValueSome
            | TagState.Unvisited tag, _ ->
                pushToStack ctx tag
                tag.TypeSignal
                |> Signal.map (fun typeKey ->
                    match xanTag.TryTypeSignal with
                    | ValueSome typeSignal when typeSignal.Value = typeKey -> ValueNone
                    | _ -> ValueSome typeKey)
                |> ValueSome
    }
    |> SType.TypeReference
    |> fun builder -> xanTag.Builder <- builder
    // TypeStore.Key is a generated unique key (see Prelude.usesGeneratedKey). Use that key for
    // the TypeSignal so TypeStore.Key == TypeSignal.Value (avoids self-referential entries when
    // resolveBase happens to return the same TypeKey as the instantiated type).
    ctx.signalCache[xanTag.IdentityKey].Key
    |> setTypeKeyForTag xanTag

let fromType (ctx: TypeScriptReader) (xanTag: XanthamTag) (typ: Ts.TypeReference) =
    {
        STypeReferenceBuilder.Type = resolveTypeBase ctx xanTag typ
        TypeArguments =
            ctx.checker.getTypeArguments(typ).AsArray
            |> Array.map (ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx _.TypeSignal)
        ResolvedType = ValueNone
    }
    |> SType.TypeReference
    |> fun builder -> xanTag.Builder <- builder
    