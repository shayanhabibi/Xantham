/// <summary>
/// Provides <see cref="T:Xantham.Fable.AutoOpenSymbol.SymbolTypeKey`1"/>, a typed, zero-overhead
/// wrapper for JS Symbol-keyed property access on arbitrary objects.
/// </summary>
/// <remarks>
/// JS Symbols are unique keys that never collide with plain string properties or with each other.
/// <c>SymbolTypeKey&lt;'T&gt;</c> wraps one such symbol and carries the expected value type at the
/// F# level (the wrapper is erased at runtime). Use <c>SymbolTypeKey.create&lt;'T&gt;</c> to
/// allocate a named symbol and keep the result as a module-level <c>let</c> binding — creating
/// two symbols with the same name produces two distinct, non-equal keys.
/// <para>
/// All access operations (<c>Invoke</c>, <c>UnsafeInvoke</c>, <c>Set</c>) index the target
/// object by the underlying symbol string. <c>Invoke</c> returns <c>'T voption</c>
/// (upcasting <c>undefined</c> to <c>ValueNone</c>); <c>UnsafeInvoke</c> skips the option wrap.
/// </para>
/// <para>
/// The <see cref="M:Xantham.Fable.AutoOpenSymbol.SymbolTypeKey.accessOrInit"/> helper is an
/// atomic get-or-init: if the key is absent it calls <paramref name="initFn"/> once, sets the
/// value, then returns it — safe for use inside reactive computations because the write happens
/// before any read that could trigger re-entrancy.
/// </para>
/// </remarks>
[<AutoOpen>]
module Xantham.Fable.AutoOpenSymbols

open System.Collections.Generic
open System.ComponentModel
open Fable.Core
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop


[<Erase>]
type SymbolTypeKey<'Type> = private SymbolTypeKey of obj with
    member inline this.Invoke(o: obj): 'Type voption = o.Item(unbox<string> this) |> unbox
    member inline this.Set(o: obj, value: 'Type) = o.Item(unbox<string> this) <- value
    member inline this.UnsafeInvoke(o: obj): 'Type = o.Item(unbox<string> this) |> unbox

[<EditorBrowsable(EditorBrowsableState.Never)>]
module SymbolTypeKeyCache =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let cache = Dictionary<string, obj>()

[<Erase>]
module SymbolTypeKey =
    let inline create<'T> (value: string): SymbolTypeKey<'T> = emitJsExpr value "Symbol($0)"
    [<Erase>]
    module Cache =
        let inline tryGetNamed<'T> (name: string) =
            match typeof<'T>.FullName + name |> SymbolTypeKeyCache.cache.TryGetValue with
            | true, v -> v |> unbox<SymbolTypeKey<'T>> |> Some
            | _ -> None
        let inline getOrCreateNamed<'T> (name: string) =
            tryGetNamed<'T> name
            |> Option.defaultWith(fun () ->
                let symbolTypeKey = create<'T> name
                let key = typeof<'T>.FullName + name
                SymbolTypeKeyCache.cache[key] <- symbolTypeKey
                symbolTypeKey
                )
        let inline tryGet<'T> =
            match typeof<'T>.FullName |> SymbolTypeKeyCache.cache.TryGetValue with
            | true, v -> v |> unbox<SymbolTypeKey<'T>> |> Some
            | _ -> None
        let inline getOrCreate<'T> =
            tryGet<'T>
            |> Option.defaultWith(fun () ->
                let key = typeof<'T>.FullName
                let symbolTypeKey = create<'T> key
                SymbolTypeKeyCache.cache[key] <- symbolTypeKey
                symbolTypeKey
                )
    let inline access (symbol: SymbolTypeKey<'a>) (o: obj) = symbol.Invoke(o)
    let inline accessOrInit (symbol: SymbolTypeKey<'a>) (initFn: unit -> 'a) (o: obj) =
        if symbol.Invoke(o).IsValueNone then
            symbol.Set(o, initFn())
        symbol.UnsafeInvoke(o)
    let inline unsafeAccess (symbol: SymbolTypeKey<'a>) (o: obj) = symbol.UnsafeInvoke(o)
    let inline has (symbol: SymbolTypeKey<_>) (o: obj) = (access symbol o).IsValueSome
    let inline remove (symbol: SymbolTypeKey<_>) (o: obj) = o.Item(unbox<string> symbol) <- JS.undefined
    let inline set (symbol: SymbolTypeKey<'Type>) (value: 'Type) (o: obj) = o.Item(unbox<string> symbol) <- value
    let inline setIfAbsent (symbol: SymbolTypeKey<'Type>) (value: 'Type) (o: obj) = if not (has symbol o) then set symbol value o
    let inline addIfAbsent (symbol: SymbolTypeKey<'Type>) (value: 'Type) (o: 'Container) =
        setIfAbsent symbol value o
        o
    