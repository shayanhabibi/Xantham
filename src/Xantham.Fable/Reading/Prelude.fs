[<AutoOpen>]
module Xantham.Fable.Reading.Prelude

open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer
open Xantham.Fable.Types.Signal
open TypeScriptExtensions

type MemoryHandlerError =
    | DuplicateValue
let private isLibFile (file: string) =
    file.Contains("/lib/lib.") && file.EndsWith(".d.ts")

let private libCacheMemoryHandler (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.Guard.Value with
    | IdentityKey.Symbol sym when Some sym |> isFromEs5Lib ->
        ctx.libCache.Add(tag.IdentityKey) |> ignore
    | IdentityKey.AliasSymbol sym when Some sym |> isFromEs5Lib ->
        ctx.libCache.Add(tag.IdentityKey) |> ignore
    | IdentityKey.DeclarationPosition(file, _, _) when isLibFile file ->
        ctx.libCache.Add(tag.IdentityKey) |> ignore
    | _ -> ()


module TypeStore =
    /// Nodes whose TypeStore.Key must be a generated unique value, NOT getTypeAtLocation.
    ///
    /// Member-level nodes: getTypeAtLocation returns the member's *type* (e.g. string),
    /// which equals the member's own Type field → self-referential entry.
    ///
    /// TypeAlias nodes: getTypeAtLocation returns the aliased type's TypeKey, which is
    /// the same value that SAliasBuilder.Type resolves to → self-referential entry.
    /// (The old Reader.fs used TypeKey.create() for TypeAlias for this reason.)
    let private usesGeneratedKey (tag: XanthamTag) =
        match tag.Value with
        | XanTagKind.TypeDeclaration decl ->
            match decl with
            | TypeDeclaration.Parameter _
            | TypeDeclaration.PropertySignature _
            | TypeDeclaration.Property _
            | TypeDeclaration.MethodSignature _
            | TypeDeclaration.Method _
            | TypeDeclaration.GetAccessor _
            | TypeDeclaration.SetAccessor _
            | TypeDeclaration.Constructor _
            | TypeDeclaration.CallSignature _
            | TypeDeclaration.ConstructSignature _
            | TypeDeclaration.IndexSignature _
            
            // | TypeDeclaration.HeritageClause _
            (* Generating keys for heritage members causes chaotic MISSREF
            errors to fire if the type is extended more than once. The actual
            output is fine, but the dangling builder messages is a frustrating UX.*)
            
            | TypeDeclaration.ExpressionWithTypeArguments _
            | TypeDeclaration.EnumMember _
            | TypeDeclaration.TypeAlias _
            | TypeDeclaration.VariableDeclaration _
            // TypeDeclaration.TypeParameter: getTypeAtLocation on a TypeParameterDeclaration
            // returns the same type object as TypeFlagPrimary.TypeParameter (type-level), causing
            // their TypeStore entries to collide when their constraint representations differ.
            // Generated key keeps each declaration-level TypeParameter entry separate.
            | TypeDeclaration.TypeParameter _ -> true
            | _ -> false
        // TypeNode.TypeReference with explicit type arguments: resolveBase may return the
        // same TypeKey as the instantiated type (e.g. Foo<T> where T = default type arg),
        // creating a self-referential TypeReference entry. Generated key avoids this.
        // Non-generic TypeReferences use routeViaChecker and are not affected.
        | XanTagKind.TypeNode (TypeNode.TypeReference typeRef) ->
            typeRef.typeArguments |> Option.map (fun a -> a.Count > 0) |> Option.defaultValue false
        // TypeNode.UnionType and TypeNode.IntersectionType: getTypeFromTypeNode expands
        // structural members (e.g. `keyof T` in a union becomes a 200-member union at
        // the type level). The TypeStore.Key would collide with the expanded semantic
        // type's entry while the builder represents only the syntactic members. Generated
        // key gives each union/intersection node its own identity, separate from the
        // semantic type produced by the checker.
        | XanTagKind.TypeNode (TypeNode.UnionType _)
        | XanTagKind.TypeNode (TypeNode.IntersectionType _) -> true
        // TypeNode.TypePredicate: getTypeFromTypeNode on a TypePredicate node returns the
        // boolean type, so all predicates would share the boolean TypeKey. Generated key
        // gives each predicate node its own unique TypeStore entry.
        | XanTagKind.TypeNode (TypeNode.TypePredicate _) -> true
        | XanTagKind.ModulesAndExports _ -> true
        | _ -> false

    module Create =
        module XanthamTag =

            let withTypeKeyMap (map: Choice<Ts.Type, Ts.Node> -> TypeKey) (tag: XanthamTag) : TypeStore =
                { Key = map (tag.ToUnderlyingValue()); Builder = GuardedData.AstNodeBuilder.getOrSetDefault tag }

            let withGeneratedKey (tag: XanthamTag) =
                withTypeKeyMap (fun _ -> TypeKey.create()) tag
                
            let create (ctx: TypeScriptReader) (tag: XanthamTag) =
                if usesGeneratedKey tag then
                    withGeneratedKey tag
                else tag |> withTypeKeyMap (function
                    | Choice1Of2 typ -> typ.TypeKey
                    | Choice2Of2 node ->
                        ctx.checker.getTypeAtLocation node
                        |> _.TypeKey)

let addToMemory (ctx: TypeScriptReader) (tag: XanthamTag) =
    if ctx.signalCache.ContainsKey(tag.IdentityKey)
    then Result.Error MemoryHandlerError.DuplicateValue else

    libCacheMemoryHandler ctx tag
    let store = TypeStore.Create.XanthamTag.create ctx tag
    ctx.signalCache.Add(tag.IdentityKey, store)
    Result.Ok()


let setDeclarationNodeBuilderSignal (declaration: XanthamTag) =
    declaration
    |> GuardedData.AstNodeBuilder.getOrSetWith (fun () -> Signal.pending<STsAstNodeBuilder>())
let setTypeSignal (declaration: XanthamTag) =
    declaration
    |> GuardedData.TypeSignal.getOrSetWith (fun () -> TypeSignal.pending())

type TypeScriptReader with
    member inline this.typeSignal (tag: XanthamTag) =
        GuardedData.TypeSignal.getOrSetDefault tag
    member this.routeTypeTo (tag: XanthamTag) (signal: TypeSignal) =
        this.typeSignal tag
        |> Signal.fulfillWith(fun () -> signal.Value)
        signal
    /// The tags source is filled by the given source signal
    member this.routeSourceTo (tag: XanthamTag) (source: Signal<ModuleName>) =
        GuardedData.Source.getOrSetWith (fun () -> source) tag
        |> Signal.fulfillWith(fun () -> source.Value)

let setTypeKeyForTag (tag: XanthamTag) (typ: TypeKey) =
    (GuardedData.TypeSignal.getOrSetDefault tag).Set typ

/// Sets the source if it doesn't have a connected signal
let trySetSourceForTag (tag: XanthamTag) (source: ModuleName) =
    GuardedData.Source.getOrSetWith (fun () -> Signal.source source) tag
let setSourceForTag (tag: XanthamTag) (source: ModuleName) =
    GuardedData.Source.getOrSetWith (fun () -> Signal.source source) tag
    |> _.Set(source)

let tagPrimitives (ctx: TypeScriptReader) =
    let checker = ctx.checker
    // Register the predefined literals
    [|
        checker.getTrueType(), true
        checker.getFalseType(), false
    |]
    |> Array.iter (fun (typ, value) ->
        // build tag
        ctx.CreateXanthamTag typ
        |> fst
        |> TagState.value
        |> fun tag ->
            tag
            |> setDeclarationNodeBuilderSignal
            |> Signal.fill (TsLiteral.Bool value |> STsAstNodeBuilder.Literal)
            tag
            |> setTypeSignal
            |> _.Set(typ.TypeKey)
            // register to memory
            tag
            |> addToMemory ctx
            |> ignore)
    // Register predefined primitives
    [|
        TypeKindPrimitive.Boolean, checker.getBooleanType()
        TypeKindPrimitive.ESSymbol, checker.getESSymbolType()
        TypeKindPrimitive.Never, checker.getNeverType()
        TypeKindPrimitive.Undefined, checker.getUndefinedType()
        TypeKindPrimitive.Unknown, checker.getUnknownType()
        TypeKindPrimitive.Void, checker.getVoidType()
        TypeKindPrimitive.Any, checker.getAnyType()
        TypeKindPrimitive.BigInt, checker.getBigIntType()
    |]
    |> Array.iter (fun (typeKindPrimitive, typ) ->
        let tag = ctx.CreateXanthamTag typ |> fst |> TagState.value
        tag
        |> setDeclarationNodeBuilderSignal
        |> Signal.fill (STsAstNodeBuilder.Primitive typeKindPrimitive)
        tag
        |> setTypeSignal
        |> _.Set(typ.TypeKey)
        tag
        |> addToMemory ctx
        |> ignore
        )
    [|
        TypeKindPrimitive.Any
        TypeKindPrimitive.Unknown
        TypeKindPrimitive.Never
        TypeKindPrimitive.Void
        TypeKindPrimitive.Undefined
        TypeKindPrimitive.Null
        TypeKindPrimitive.String
        TypeKindPrimitive.Integer
        TypeKindPrimitive.Number
        TypeKindPrimitive.Boolean
        TypeKindPrimitive.BigInt
        TypeKindPrimitive.ESSymbol
        TypeKindPrimitive.NonPrimitive
    |]
    |> Array.iter (fun typeKindPrimitive ->
        let identity = IdentityKey.Id typeKindPrimitive.TypeKey
        let value = {
            Key = typeKindPrimitive.TypeKey
            Builder = Signal.pending<STsAstNodeBuilder>()
        }
        value.Builder
        |> Signal.fill (STsAstNodeBuilder.Primitive typeKindPrimitive)
        if ctx.signalCache.ContainsKey(identity) |> not then
            ctx.signalCache.Add(identity, value)
        )
    ctx

/// Returns Some store if newly registered (dispatch should proceed),
/// or None if already cached (signal wired to cached store, skip dispatch).
let tryGetOrRegisterStore (ctx: TypeScriptReader) (tag: XanthamTag) : TypeStore option =
    let key = tag.IdentityKey
    let maybeTypeKey =
        match tag.Value.UnderlyingValue with
        | Choice1Of2 typ ->
            Some typ.TypeKey
        | _ -> None
    match ctx.signalCache.TryGetValue key with
    | true, store when maybeTypeKey.IsNone || store.Key = maybeTypeKey.Value ->
        GuardedData.AstNodeBuilder.getOrSetDefault tag
        |> Signal.fulfillWith (fun () -> store.Builder.Value)
        // Two different Ts.Type objects can share the same IdentityKey (same symbol) because
        // GuardTracer.fromType stores the guard on the symbol, not the type object. The second
        // type's XanthamTag is a distinct object whose TypeSignal is never set by dispatch
        // (dispatch is skipped). Without this line its TypeSignal stays at pending() = Unknown
        // (-14), causing Unknown saturation in parent builders' type fields.
        // (GuardedData.TypeSignal.getOrSetDefault tag).Set store.Key
        GuardedData.TypeSignal.getOrSetDefault tag
        |> _.Set(
            match tag.Value.UnderlyingValue with
            | Choice1Of2 typ when typ.TypeKey <> store.Key -> typ.TypeKey
            | _ -> store.Key
            )
        None
    | true, _ ->
        // Hack for now; need to wire up the signals properly
        tag.Guard <- unbox<GuardTracer> {| Value = IdentityKey.Id maybeTypeKey.Value |}
        tag.Guard.Imprint
        libCacheMemoryHandler ctx tag
        let store = TypeStore.Create.XanthamTag.create ctx tag
        ctx.signalCache.Add(tag.Guard.Value, store)
        Some store
    | false, _ ->
        libCacheMemoryHandler ctx tag
        let store = TypeStore.Create.XanthamTag.create ctx tag
        ctx.signalCache.Add(key, store)
        Some store

let pushToStack (ctx: TypeScriptReader) (tag: XanthamTag) =
    setDeclarationNodeBuilderSignal tag |> ignore
    ctx.typeSignal tag |> ignore
    ctx.stack.Push tag
let arrayHasModifier (modifier: Modifiers -> bool) (arr: ResizeArray<Ts.Modifier>) =
    arr.AsArray |> Array.exists (Modifiers.Create >> modifier)
let optionArrayHasModifier (modifier: Modifiers -> bool) (arr: ResizeArray<Ts.Modifier> option) =
    arr |> Option.exists (arrayHasModifier modifier)