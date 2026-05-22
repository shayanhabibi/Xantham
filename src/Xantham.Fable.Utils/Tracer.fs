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
open Xantham.Fable
open FSharp.Core

    
let private TRACER_TAG = Symbol "XanTracer"
let private TRACER_PROXY = SymbolTypeKey.create<string> "XanTracerProxy"

type Tracer<'T> =
    abstract Value: 'T with get

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
    /// <summary>
    /// Returns the tracer object stored on the target object, if any.
    /// </summary>
    /// <param name="target"></param>
    let inline get<'T> (target: obj) =
        (unbox<SymbolTypeKey<Tracer<'T>>> TRACER_TAG).Invoke(target)
    /// <summary>
    /// Returns the tracer object stored on the target object. Fails if none.
    /// </summary>
    /// <param name="target"></param>
    let inline unsafeGet<'T> (target: obj) =
        (unbox<SymbolTypeKey<Tracer<'T>>> TRACER_TAG).UnsafeInvoke(target)
    /// <summary>
    /// If the tag has debug set to true, then it will run the passed fn
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="target"></param>
    let inline withDebug<'T> (fn: Tracer<'T> -> unit) (target: Tracer<'T>)=
        #if DEBUG
        if target.Debug then fn target
        #endif
        target
    /// <summary>
    /// If the tag exists on the target, and the tag has debug set to true, then it will run
    /// the passed fn against the tracer and target.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="target"></param>
    let inline withTracerDebug<'T, 'a> (fn: 'a -> Tracer<'T> -> unit) (target: 'a): 'a =
        #if DEBUG
        get<'T> target
        |> ValueOption.iter (withDebug (fn target) >> ignore)
        #endif
        target
    /// <summary>
    /// Sets debug to true on the tracer
    /// </summary>
    /// <param name="target"></param>
    let inline setDebug<'T> (target: Tracer<'T>) =
        target.Debug <- true
        target
    /// <summary>
    /// If a tracer exists on the target, then the debug is set to true.
    /// </summary>
    /// <param name="target"></param>
    let inline setDebugTracer<'T> (target: obj) =
        match get<'T> target with
        | ValueSome tracer ->
            tracer.Debug <- true
            true
        | ValueNone -> false
    /// <summary>
    /// Creates a tracer on the target with the provided value.
    /// Overwrites any existing tracer.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="target"></param>
    let inline unsafeCreate<'T> (value: 'T) (target: obj) =
        target.Item(unbox<string> TRACER_TAG) <- {| Value = value |}
        (unsafeGet<'T> target).Imprint
        unsafeGet<'T> target
    /// <summary>
    /// Creates a tracer on the target with the provided value if one does not already exist.
    /// Does not fail if type <c>'T</c> is different.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="target"></param>
    let inline create<'T> (value: 'T) (target: obj) =
        if (get<'T> target).IsNone then
            unsafeCreate<'T> value target
        else unsafeGet<'T> target
    /// <summary>
    /// Creates a tracer on the target with the provided value. Returns an error that describes whether the
    /// target already had a tracer, or if the tracer on the target had a different value stored.
    /// Does not fail if type <c>'T</c> is different.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="target"></param>
    let inline safeCreate<'T when 'T:equality> (value: 'T) (target: obj) =
        if (get<'T> target).IsNone then
            unsafeCreate<'T> value target
            |> Ok
        elif (unsafeGet<'T> target).Value <> value then
            Error ExistsWithDifferentValue
        else
            Error AlreadyExists
    /// <summary>
    /// Same as create, except it will fail if type <c>'T</c> is different.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="target"></param>
    let inline imprintedCreate<'T when 'T:equality> (value: 'T) (target: obj) =
        let result = create<'T> value target
        result.Imprint
        result
    /// <summary>
    /// Same as safeCreate, except it will fail if type <c>'T</c> is different.
    /// </summary>
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
    
    /// <summary>
    /// Checks whether the given target has a tracer.
    /// </summary>
    /// <param name="target"></param>
    let inline has<'T> (target: obj) = (get<'T> target).IsSome
    
    module Data =
        /// <summary>
        /// Returns the value for the given property name on the tracer.
        /// If the value is not set, will return none.
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="target"></param>
        let inline get<'T> (propName: string) (target: Tracer<_>) =
            target.Item(propName)
            |> unbox<'T voption>
        /// <summary>
        /// Returns the value for the given property name, and ASSUMES it is not null (unsafe).
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="target"></param>
        let inline unsafeGet<'T> (propName: string) (target: Tracer<_>) =
            target.Item(propName) |> unbox<'T>
        /// <summary>
        /// Sets a value for the property name on the given tracer.
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="value"></param>
        /// <param name="target"></param>
        let inline set<'T> (propName: string) (value: 'T) (target: Tracer<_>) =
            target.Item propName <- value
        /// <summary>
        /// Gets the value for the given property name on the tracer, or the default value if it is not set.
        /// </summary>
        /// <remarks>Does not assign the default value to the prop.</remarks>
        /// <param name="propName"></param>
        /// <param name="defaultValue"></param>
        /// <param name="target"></param>
        let inline getOrDefault<'T> (propName: string) (defaultValue: 'T) (target: Tracer<_>) =
            get<'T> propName target
            |> ValueOption.defaultValue defaultValue
        /// <summary>
        /// Gets the value for the given property name on the tracer, or sets it to the default value if it is not set.
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="value"></param>
        /// <param name="target"></param>
        let inline getOrSet<'T> (propName: string) (value: 'T) (target: Tracer<_>) =
            if get<'T> propName target |> ValueOption.isNone then
                set<'T> propName value target
                unsafeGet<'T> propName target
            else unsafeGet<'T> propName target
        /// <summary>
        /// Gets the value for the given property name on the tracer, or sets it to the result of the given function if it is not set.
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="value"></param>
        /// <param name="target"></param>
        let inline getOrSetWith<'T> propName (value: unit -> 'T) (target: Tracer<_>) =
            if get<'T> propName target |> ValueOption.isNone then
                set<'T> propName (value()) target
                unsafeGet<'T> propName target
            else unsafeGet<'T> propName target
        /// <summary>
        /// Removes the given prop from the tracer.
        /// </summary>
        /// <param name="propName"></param>
        /// <param name="target"></param>
        let inline clear<'T> propName (target: Tracer<_>) =
            set<'T> propName JS.undefined target
        /// <summary>
        /// All prop names are automatically calculated from the type argument.
        /// </summary>
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
