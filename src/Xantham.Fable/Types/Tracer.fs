/// <summary>
/// Tracer infrastructure for attaching typed, symbol-keyed metadata to arbitrary JS objects
/// without dictionary lookup.
/// </summary>
/// <remarks>
/// <b>Core concept — proxied tracer pattern:</b> a <c>Tracer&lt;'T&gt;</c> is stored directly on
/// a target JS object under the well-known <c>TRACER_TAG</c> symbol key as a plain wrapper
/// <c>{ Value: 'T }</c>. Because the wrapper lives on the object itself, lookup is O(1) property
/// access rather than a dictionary probe. The <c>TRACER_PROXY</c> symbol is used as a
/// type-imprint stamp to verify that a retrieved tracer carries the expected <c>'T</c> parameter.
/// <para>
/// <b><c>IdentityKey</c></b> is a discriminated union that unifies all ways a TypeScript
/// symbol/type/node can be identified for cycle-detection and caching purposes:
/// <c>Id TypeKey</c> for anonymous types, <c>AliasSymbol</c>/<c>Symbol</c> for named symbols,
/// and <c>DeclarationPosition</c> for nodes without an accessible symbol.
/// </para>
/// <para>
/// <b><c>GuardTracer = Tracer&lt;IdentityKey&gt;</c></b> is the identity guard attached to
/// TypeScript objects (symbols, types, nodes) via the dedicated <c>TRACER_GUARD</c> symbol key.
/// <c>module GuardTracer</c> provides <c>fromNode</c>, <c>fromType</c>, <c>fromSymbol</c> smart
/// constructors that choose the correct target object and <c>IdentityKey</c> case — notably
/// <c>fromType</c> stores on the alias/canonical symbol when one exists, keeping the guard with
/// the most-canonical object. Active pattern <c>(|Id|AliasSymbol|Symbol|DeclarationPosition|)</c>
/// allows pattern matching directly on a <c>GuardTracer</c>.
/// </para>
/// <para>
/// <b><c>CyclicTracer&lt;'T&gt;</c></b> is a self-referential tracer where the wrapped value
/// <c>'T</c> is the same object the tracer is stored on (useful for cycle detection on the
/// object itself). <c>module CyclicTracer</c> provides helpers analogous to
/// <c>module Tracer</c>.
/// </para>
/// <para>
/// <b><c>GuardedTracer&lt;'WrappedType,'Guard&gt;</c></b> extends <c>Tracer&lt;'WrappedType&gt;</c>
/// with a mutable <c>Guard</c> property for associating an independent guard object.
/// <c>KeyedGuardedTracer</c> further adds a mutable <c>Key</c>.
/// </para>
/// <para>
/// <b><c>Tracer.Data</c></b> sub-module provides named-slot access on a tracer object —
/// additional properties stored by string key (or via <c>Generic</c> sub-module using
/// <c>typeof&lt;'T&gt;.Name</c> as key). This forms a lightweight property bag without
/// allocating a separate dictionary.
/// </para>
/// </remarks>
module Xantham.Fable.Types.Tracer

open Fable.Core
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop
open TypeScript
open Xantham
open Xantham.Fable
open FSharp.Core

[<RequireQualifiedAccess>]
type IdentityKey =
    | Id of TypeKey
    | AliasSymbol of Ts.Symbol
    | Symbol of Ts.Symbol
    | DeclarationPosition of file: string * pos: float * endPos: float
    static member Create(node: Ts.Node) =
        let sf = node.getSourceFile()
        let startPos = node.getStart()
        let endPos = node.getEnd()
        let start = sf.getLineAndCharacterOfPosition(startPos)
        let end' = sf.getLineAndCharacterOfPosition(endPos)
        let fileString = $"file:///%s{sf.fileName}:{start.line + 1.}:{start.character + 1.} (end {end'.line + 1.}:{end'.character + 1.})"
        DeclarationPosition(fileString, startPos, endPos)
    static member Create(typ: Ts.Type) =
        typ.aliasSymbol
        |> Option.map AliasSymbol
        |> Option.orElse (typ.getSymbol() |> Option.map Symbol)
        |> Option.defaultValue (Id typ.TypeKey)
    static member Create(typ: Ts.Symbol) = Symbol typ
    
let TRACER_TAG = Symbol "XanTracer"
let TRACER_PROXY = SymbolTypeKey.create<string> "XanTracerProxy"

type Tracer<'T> =
    abstract Value: 'T with get

type GuardTracer =
    inherit Tracer<IdentityKey>

let TRACER_GUARD = SymbolTypeKey.create<GuardTracer> "XanGuard"
let mutable DebugIdCounter = 0
let getDebugId() =
    DebugIdCounter <- DebugIdCounter + 1
    DebugIdCounter

type Tracer<'T> with
    #if DEBUG
    /// <summary>
    /// Only compiled in Debug builds
    /// </summary>
    member inline this.TraceId with get() =
        this["DebugId"] :?> int option
        |> Option.defaultWith (fun () ->
            this["DebugId"] <- getDebugId()
            this["DebugId"] :?> int
            )
    #endif
    member inline this.DebugId with get() = this["DebugId"] :?> int option |> Option.defaultValue -1
    member inline this.Debug
        with inline get() = this["Debug"] :?> bool option |> Option.defaultValue false
        and inline set(value: bool) =
            #if DEBUG
            if value && not this.Debug && (this["DebugId"] :?> int option).IsNone then
            #else
            if value && not this.Debug then
            #endif
                this["DebugId"] <- getDebugId()
            this["Debug"] <- value
        
    member inline this.TYPE_Valid = TRACER_PROXY.Invoke(this).IsSome && TRACER_PROXY.UnsafeInvoke(this) = typeof<'T>.Name
    member inline this.TYPE_Invalid = this.TYPE_Valid |> not
    member inline this.Imprint =
        if TRACER_PROXY.Invoke(this).IsNone then
            TRACER_PROXY.Set(this, typeof<'T>.Name)
        elif this.TYPE_Invalid then  failwith "Attempted to imprint a tracer twice, second imprint was different"

[<StringEnum>]
type TracerCreateError =
    | AlreadyExists
    | ExistsWithDifferentValue
    | DifferentTypeTag

module Tracer =
    let inline get<'T> (target: obj) =
        (unbox<SymbolTypeKey<Tracer<'T>>> TRACER_TAG).Invoke(target)
    let inline unsafeGet<'T> (target: obj) =
        (unbox<SymbolTypeKey<Tracer<'T>>> TRACER_TAG).UnsafeInvoke(target)
    let inline withDebug<'T> (fn: Tracer<'T> -> unit) (target: Tracer<'T>)=
        #if DEBUG
        if target.Debug then fn target
        #endif
        target
    let inline withTracerDebug<'T, 'a> (fn: 'a -> Tracer<'T> -> unit) (target: 'a): 'a =
        #if DEBUG
        get<'T> target
        |> ValueOption.iter (withDebug (fn target) >> ignore)
        #endif
        target
    let inline setDebug<'T> (target: Tracer<'T>) =
        target.Debug <- true
        target
    let inline setDebugTracer<'T> (target: obj) =
        match get<'T> target with
        | ValueSome tracer ->
            tracer.Debug <- true
            true
        | ValueNone -> false
    let inline set<'T> (value: 'T) (target: obj) =
        target.Item(unbox<string> TRACER_TAG) <- {| Value = value |}
    let inline unsafeCreate<'T> (value: 'T) (target: obj) =
        target.Item(unbox<string> TRACER_TAG) <- {| Value = value |}
        (unsafeGet<'T> target).Imprint
        unsafeGet<'T> target
    let inline create<'T> (value: 'T) (target: obj) =
        if (get<'T> target).IsNone then
            unsafeCreate<'T> value target
        else unsafeGet<'T> target
    let inline safeCreate<'T when 'T:equality> (value: 'T) (target: obj) =
        if (get<'T> target).IsNone then
            unsafeCreate<'T> value target
            |> Ok
        elif (unsafeGet<'T> target).Value <> value then
            Error ExistsWithDifferentValue
        else
            Error AlreadyExists
    let inline imprintedCreate<'T when 'T:equality> (value: 'T) (target: obj) =
        let result = create<'T> value target
        result.Imprint
        result
    let inline safeImprintedCreate<'T when 'T:equality> (value: 'T) (target: obj) =
        match safeCreate<'T> value target with
        | Ok tracer ->
            tracer.Imprint
            Ok tracer
        | Error AlreadyExists as error ->
            if unsafeGet<'T> target |> _.TYPE_Valid then
                error
            else Error DifferentTypeTag
        | error -> error 
    let inline has<'T> (target: obj) = (get<'T> target).IsSome
    
    module Data =
        let inline get<'T> (propName: string) (target: Tracer<_>) =
            target.Item(propName)
            |> unbox<'T voption>
        let inline unsafeGet<'T> (propName: string) (target: Tracer<_>) =
            target.Item(propName) |> unbox<'T>
        let inline set<'T> (propName: string) (value: 'T) (target: Tracer<_>) =
            target.Item propName <- value
        let inline getOrDefault<'T> (propName: string) (defaultValue: 'T) (target: Tracer<_>) =
            get<'T> propName target
            |> ValueOption.defaultValue defaultValue
        let inline getOrSet<'T> (propName: string) (value: 'T) (target: Tracer<_>) =
            if get<'T> propName target |> ValueOption.isNone then
                set<'T> propName value target
                unsafeGet<'T> propName target
            else unsafeGet<'T> propName target
        let inline getOrSetWith<'T> propName (value: unit -> 'T) (target: Tracer<_>) =
            if get<'T> propName target |> ValueOption.isNone then
                set<'T> propName (value()) target
                unsafeGet<'T> propName target
            else unsafeGet<'T> propName target
        let inline clear<'T> propName (target: Tracer<_>) =
            set<'T> propName JS.undefined target
        module Generic =
            let inline get<'T> (target: Tracer<_>) =
                get<'T> typeof<'T>.Name target
            let inline unsafeGet<'T> (target: Tracer<_>) =
                unsafeGet<'T> typeof<'T>.Name target
            let inline set<'T> (value: 'T) (target: Tracer<_>) =
                set<'T> typeof<'T>.Name value target
            let inline getOrDefault<'T> (defaultValue: 'T) (target: Tracer<_>) =
                getOrDefault<'T> typeof<'T>.Name defaultValue target
            let inline getOrSet<'T> (value: 'T) (target: Tracer<_>) =
                getOrSet<'T> typeof<'T>.Name value target
            let inline getOrSetWith<'T> (value: unit -> 'T) (target: Tracer<_>) =
                getOrSetWith<'T> typeof<'T>.Name value target
            let inline clear<'T> (target: Tracer<_>) =
                clear typeof<'T>.Name target

type CyclicTracer<'T> =
    inherit Tracer<'T>

module CyclicTracer =
    let inline has (target: 'T) =
        Tracer.has<'T> target
    let inline get (target: 'T) =
        Tracer.get<'T> target |> unbox<CyclicTracer<'T> voption>
    let inline unsafeGet (target: 'T) =
        Tracer.unsafeGet<'T> target :?> CyclicTracer<'T>
    let inline unsafeCreate<'T> (target: 'T) = Tracer.unsafeCreate<'T> target target :?> CyclicTracer<'T>
    let inline create<'T> (target: 'T) =
        Tracer.create<'T> target target :?> CyclicTracer<'T>
    let inline safeCreate<'T> (target: 'T) =
        if (get target).IsSome then
            Error AlreadyExists
        else
            let result = unsafeCreate target
            result.Imprint
            Ok result
    let inline imprintedCreate<'T> (target: 'T) =
        let result = create target
        result.Imprint
        result
    let inline safeGet (target: 'T) =
        if
            get target
            |> ValueOption.exists _.TYPE_Valid
        then
            unsafeGet target
            |> Ok
        else
            Tracer.get target 
            |> Error

module GuardTracer =
    let inline get (target: obj) : GuardTracer voption =
        TRACER_GUARD.Invoke(target)
    let inline unsafeGet (target: obj) : GuardTracer =
        TRACER_GUARD.UnsafeInvoke(target)
    let inline has (target: obj) = (get target).IsSome
    let inline unsafeCreate (identity: IdentityKey) (target: obj) : GuardTracer =
        target.Item(unbox<string> TRACER_GUARD) <- {| Value = identity |}
        let result = unsafeGet target
        result.Imprint
        result
    let inline create (identity: IdentityKey) (target: obj) : GuardTracer =
        if (get target).IsNone then unsafeCreate identity target
        else unsafeGet target
    let inline imprintedCreate (identity: IdentityKey) (target: obj) : GuardTracer =
        let result = create identity target
        result.Imprint
        result
    let inline safeCreate (identity: IdentityKey) (target: obj) =
        if (get target).IsNone then unsafeCreate identity target |> Ok
        else Error AlreadyExists
    let inline fromNode (node: Ts.Node) : GuardTracer =
        imprintedCreate (IdentityKey.Create node) node
    let inline fromType (typ: Ts.Type) : GuardTracer =
        match IdentityKey.Create typ with
        | IdentityKey.AliasSymbol sym as identity -> imprintedCreate identity sym
        | IdentityKey.Symbol sym as identity -> imprintedCreate identity sym
        | identity -> imprintedCreate identity typ
    let inline fromSymbol (checker: Ts.TypeChecker) (sym: Ts.Symbol) : GuardTracer =
        let resolved =
            if sym.flags.HasFlag Ts.SymbolFlags.Alias && sym.declarations.IsNone then
                checker.getAliasedSymbol sym
            else sym
        imprintedCreate (IdentityKey.Symbol resolved) resolved
    let inline (|Id|AliasSymbol|Symbol|DeclarationPosition|) (guard: GuardTracer) =
        match guard.Value with
        | IdentityKey.Id key -> Id key
        | IdentityKey.AliasSymbol sym -> AliasSymbol sym
        | IdentityKey.Symbol sym -> Symbol sym
        | IdentityKey.DeclarationPosition(file, pos, endPos) -> DeclarationPosition(file, pos, endPos)

type GuardedTracer<'WrappedType, 'Guard> =
    inherit Tracer<'WrappedType>
    abstract Guard: 'Guard with get, set

module GuardedTracer =
    let inline containerIsValid (tracer: GuardedTracer<'T, 'G>) = tracer.TYPE_Valid
    let inline hasGuard (tracer: GuardedTracer<'T, 'G>) =
        tracer.Guard
        |> unbox<'G option>
        |> Option.isSome
    let inline isValid (tracer: GuardedTracer<'T, 'G>) =
        containerIsValid tracer && hasGuard tracer
    let inline unsafeGet<'WrappedType, 'Guard> (target: obj) =
        Tracer.unsafeGet<'WrappedType> target
        |> unbox<GuardedTracer<'WrappedType, 'Guard>>
    let inline get<'WrappedType, 'Guard> (target: obj) =
        Tracer.get<'WrappedType> target
        |> unbox<GuardedTracer<'WrappedType, 'Guard> voption>
    let inline has<'WrappedType, 'Guard> (target: obj) =
        Tracer.has<'WrappedType> target &&
        unsafeGet<'WrappedType, 'Guard> target
        |> hasGuard
    let inline hasAndIsValid<'WrappedType, 'Guard> (target: obj) =
        has<'WrappedType, 'Guard> target &&
        unsafeGet<'WrappedType, 'Guard> target
        |> isValid
    let inline safeGet<'WrappedType, 'Guard> (target: obj) =
        if hasAndIsValid<'WrappedType, 'Guard> target then
            unsafeGet<'WrappedType, 'Guard> target |> Some
        else None
    let inline unsafeCreateWithGuard<'WrappedType, 'Guard> (guard: 'Guard) (value: 'WrappedType) (target: obj) =
        target |> Tracer.unsafeCreate<'WrappedType> value |> ignore
        let result = unsafeGet<'WrappedType, 'Guard> target
        result.Guard <- guard
        result
    let inline imprintContainer (tracer: GuardedTracer<'T, 'G>) =
        tracer.Imprint
        tracer

type KeyedGuardedTracer<'WrappedType, 'Guard, 'Key> =
    inherit GuardedTracer<'WrappedType, 'Guard>
    abstract Key: 'Key with get, set

type GuardedTracer<'T, 'U> with
    // -----------------------------------
    // Data access
    // -----------------------------------
    member inline this.TryGet(symbol: SymbolTypeKey<'Data>) =
        symbol.Invoke(this)
    member inline this.Get(symbol: SymbolTypeKey<'Data>) =
        SymbolTypeKey.unsafeAccess symbol this
    member inline this.Has(symbol: SymbolTypeKey<'Data>) = SymbolTypeKey.has symbol this
    member inline this.GetOrInit(symbol: SymbolTypeKey<'Data>, initFn: unit -> 'Data) =
        SymbolTypeKey.accessOrInit symbol initFn this
    member inline this.Set(symbol: SymbolTypeKey<'Data>, value: 'Data) =
        SymbolTypeKey.set symbol value this
    member inline this.Clear(symbol: SymbolTypeKey<'Data>) = SymbolTypeKey.remove symbol this
    member inline this.TryGet<'Data>(): 'Data voption =
        this.Item(typeof<'Data>.Name)
        |> unbox
    member inline this.Get<'Data>(): 'Data =
        this.TryGet<'Data>() |> ValueOption.defaultWith (fun () -> failwith "Attempted unwrap Get value")
    member inline this.Has<'Data>() = this.TryGet<'Data>() |> _.IsSome
    member inline this.GetOrInit<'Data>(initFn: unit -> 'Data) =
        match this.TryGet<'Data>() with
        | ValueNone ->
            this.Item typeof<'Data>.Name <- initFn()
            this.Get<'Data>()
        | ValueSome value -> value
    member inline this.Set<'Data>(value: 'Data) =
        this.Item typeof<'Data>.Name <- value
    member inline this.Clear<'Data>() = this.Item typeof<'Data>.Name <- JS.undefined
    // -----------------------------------
    // Keyed data access (stored on Guard)
    // -----------------------------------
    /// <summary>
    /// Reads/writes data stored on the <c>Guard</c> object rather than the tag itself,
    /// giving data partitioned by the guard's identity (symbol or <c>TypeKey</c>).
    /// All <c>Keyed*</c> members mirror the plain <c>Get/Set/Has/Clear/GetOrInit</c>
    /// API but target <c>this.Guard</c> as the backing store.
    /// </summary>
    member inline this.KeyedTryGet(symbol: SymbolTypeKey<'Data>) =
        symbol.Invoke(this.Guard)
    member inline this.KeyedGet(symbol: SymbolTypeKey<'Data>) =
        SymbolTypeKey.unsafeAccess symbol this.Guard
    member inline this.KeyedHas(symbol: SymbolTypeKey<'Data>) = SymbolTypeKey.has symbol this.Guard
    member inline this.KeyedGetOrInit(symbol: SymbolTypeKey<'Data>, initFn: unit -> 'Data) =
        SymbolTypeKey.accessOrInit symbol initFn this.Guard
    member inline this.KeyedSet(symbol: SymbolTypeKey<'Data>, value: 'Data) =
        SymbolTypeKey.set symbol value this.Guard
    member inline this.KeyedClear(symbol: SymbolTypeKey<'Data>) = SymbolTypeKey.remove symbol this.Guard
    member inline this.KeyedTryGet<'Data>(): 'Data voption =
        this.Guard.Item(typeof<'Data>.Name)
        |> unbox
    member inline this.KeyedGet<'Data>(): 'Data =
        this.KeyedTryGet<'Data>() |> ValueOption.defaultWith (fun () -> failwith "Attempted unwrap KeyedGet value")
    member inline this.KeyedHas<'Data>() = this.KeyedTryGet<'Data>() |> _.IsSome
    member inline this.KeyedGetOrInit<'Data>(initFn: unit -> 'Data) =
        match this.KeyedTryGet<'Data>() with
        | ValueNone ->
            this.Guard.Item typeof<'Data>.Name <- initFn()
            this.KeyedGet<'Data>()
        | ValueSome value -> value
    member inline this.KeyedSet<'Data>(value: 'Data) =
        this.Guard.Item typeof<'Data>.Name <- value
    member inline this.KeyedClear<'Data>() = this.Guard.Item typeof<'Data>.Name <- JS.undefined

[<RequireQualifiedAccess>]
type TagState<'T> =
    | Visited of 'T
    | Unvisited of 'T
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// </summary>
    member inline this.Value: 'T =
        emitJsExpr this "$0.fields[0]"

module TagState =
    let createVisited (value: 'T) = TagState.Visited value
    let createUnvisited (value: 'T) = TagState.Unvisited value
    let inline isVisited (state: TagState<'T>) = state.IsVisited
    let inline isUnvisited (state: TagState<'T>) = state.IsUnvisited
    let inline value (state: TagState<'T>) = state.Value
    let inline mapUnvisited (f: 'T -> 'T) (state: TagState<'T>) =
        match state with
        | TagState.Unvisited v -> TagState.Unvisited (f v)
        | v -> v
    let inline mapVisited (f: 'T -> 'T) (state: TagState<'T>) =
        match state with
        | TagState.Visited v -> TagState.Visited (f v)
        | v -> v
    let inline applyUnvisited (f: 'T -> 'U) (state: TagState<'T>) =
        match state with
        | TagState.Unvisited v -> f v |> ValueSome
        | _ -> ValueNone
    let inline applyVisited (f: 'T -> 'U) (state: TagState<'T>) =
        match state with
        | TagState.Visited v -> f v |> ValueSome
        | _ -> ValueNone
    /// <param name="fn">First parameter is true when the state has been seen for the first time.</param>
    /// <param name="state"></param>
    let inline map (fn: bool -> 'T -> 'U) (state: TagState<'T>) =
        match state with
        | TagState.Unvisited v -> TagState.Unvisited (fn true v)
        | TagState.Visited v -> TagState.Visited (fn false v)
    let inline bindUnvisited (fn: 'T -> 'T) (state: TagState<'T>) =
        match state with
        | TagState.Unvisited v -> TagState.Visited (fn v)
        | _ -> state
    let inline bind (fn: 'T -> 'T) (state: TagState<'T>) =
        match state with
        | TagState.Visited v -> TagState.Visited (fn v)
        | TagState.Unvisited v -> TagState.Visited (fn v)
    let inline apply (fn: bool -> 'T -> unit) (state: TagState<'T>) =
        match state with
        | TagState.Unvisited v -> fn true v
        | TagState.Visited v -> fn false v
        state
