[<AutoOpen>]
module Xantham.Fable.AutoOpenXTag

open System.Collections.Generic
open System.ComponentModel
open System.Runtime.CompilerServices
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop
open Glutinum.Chalk
open TypeScript
open Fable.Core
open Xantham
open Xantham.Fable.Types.Tracer
open Xantham.Fable.Types

/// <summary>
/// SRTP creator for <see cref="T:Xantham.Fable.Types.Tracer.GuardTracer"/> — each overload
/// corresponds to one TypeScript input type.
/// </summary>
type GuardTracerSRTPCreator =
    static member Create(typ: Ts.Type, _checker: Ts.TypeChecker) =
        GuardTracer.fromType typ
    static member Create(node: Ts.Node, _checker: Ts.TypeChecker) =
        GuardTracer.fromNode node
    static member Create(sym: Ts.Symbol, checker: Ts.TypeChecker) = GuardTracer.fromSymbol checker sym
    static member Seen(node: Ts.Node, _checker: Ts.TypeChecker) = GuardTracer.has node
    static member Seen(typ: Ts.Type, _checker: Ts.TypeChecker) =
        match IdentityKey.Create typ with
        | IdentityKey.AliasSymbol sym
        | IdentityKey.Symbol sym -> GuardTracer.has sym
        | _ -> GuardTracer.has typ
    static member Seen(sym: Ts.Symbol, checker: Ts.TypeChecker) =
        let resolved =
            if sym.flags.HasFlag Ts.SymbolFlags.Alias && sym.declarations.IsNone then
                checker.getAliasedSymbol sym
            else sym
        GuardTracer.has resolved
    

/// <summary>
/// A tag attached to a TypeScript AST node or type that carries its <see cref="T:Xantham.Fable.AutoOpenXTag.XanTagKind"/>
/// classification and a <see cref="T:Xantham.Fable.Types.Tracer.GuardTracer"/> for cycle-detection / identity.
/// </summary>
/// <remarks>
/// <b>Dual property bags:</b> each <c>XanthamTag</c> exposes two independent JS property bags:<br/>
/// • <b>Tag bag</b> (<c>Get/Set/Has/Clear/GetOrInit</c>) — stored directly on the tag object.
///   Suitable for data that is scoped to the node/type itself regardless of context.<br/>
/// • <b>Keyed bag</b> (<c>KeyedGet/Set/Has/Clear/GetOrInit</c>) — stored on the <c>Guard</c> object.
///   Partitioned by the guard's identity (symbol or <c>TypeKey</c>), so the same logical
///   node visited under different guards carries independent keyed data.<br/>
/// Both bags support two key schemes: a <c>SymbolTypeKey&lt;'Data&gt;</c> (explicit symbol key) and
/// a generic <c>'Data</c> parameter (key derived from <c>typeof&lt;'Data&gt;.Name</c>).
/// </remarks>
type XanthamTag =
    inherit GuardedTracer<XanTagKind, GuardTracer>
type XanthamTag with
    /// <summary>
    /// Creates or retrieves the <c>XanthamTag</c> for <paramref name="node"/> and returns a
    /// tuple <c>(TagState&lt;XanthamTag&gt; * TagState&lt;GuardTracer&gt;)</c> indicating visit
    /// state for both the tag container and its identity guard.
    /// </summary>
    /// <remarks>
    /// <b>Three SRTP constraints</b> dispatch over <c>Ts.Node</c>, <c>Ts.Type</c>, and
    /// <c>Ts.Symbol</c> without runtime overhead:<br/>
    /// • <c>(^T or GuardTracerSRTPCreator).Create(^T, TypeChecker) → GuardTracer</c> —
    ///   calls <c>GuardTracer.fromNode/fromType/fromSymbol</c> to create the identity guard.<br/>
    /// • <c>(^T or GuardTracerSRTPCreator).Seen(^T, TypeChecker) → bool</c> —
    ///   checks whether a <c>GuardTracer</c> is already attached to the underlying JS object.<br/>
    /// • <c>(^T or XanTagKind).Create(^T) → XanTagKind</c> —
    ///   classifies the node/type into its <c>XanTagKind</c> DU case on first creation.<br/>
    /// <br/>
    /// <b>Tag container (first return element):</b> if no <c>Tracer&lt;XanTagKind&gt;</c> exists
    /// on the object, one is created (XanTagKind classified + imprinted) and
    /// <c>TagState.Unvisited</c> is returned. On subsequent calls <c>TagState.Visited</c>
    /// is returned. Use <c>TagState.Value</c> (zero-cost <c>fields[0]</c> access) to unwrap.<br/>
    /// <br/>
    /// <b>Guard (second return element):</b> if the container has no <c>Guard</c> yet,
    /// the guard is created and assigned. <c>seenGuard</c> (captured before guard assignment)
    /// drives the second element: <c>Unvisited</c> when the guard pre-existed before this call;
    /// <c>Visited</c> when the guard is freshly attached.<br/>
    /// <br/>
    /// <b>First-write-wins on the guard:</b> the guard is only set when
    /// <c>GuardedTracer.hasGuard</c> returns false; subsequent calls leave the existing guard
    /// untouched.
    /// </remarks>
    static member inline Create<^T when
        (^T or GuardTracerSRTPCreator): (static member Create: ^T * Ts.TypeChecker -> GuardTracer) and
        (^T or GuardTracerSRTPCreator): (static member Seen: ^T * Ts.TypeChecker -> bool) and
        (^T or XanTagKind) : (static member Create: ^T -> XanTagKind)
    >(node: ^T, checker: Ts.TypeChecker) =
        let seen = Tracer.has node
        let container =
            // see if there is already a tag for this node
            Tracer.get<XanTagKind> node
            |> ValueOption.defaultWith(fun () ->
                Tracer.set<XanTagKind>
                    ((^T or XanTagKind):(static member Create: ^T -> XanTagKind) node)
                    node
                let result = Tracer.unsafeGet<XanTagKind> node
                result.Imprint
                result
                )
            :?> GuardedTracer<XanTagKind, GuardTracer>
        let seenGuard = ((^T or GuardTracerSRTPCreator):(static member Seen: ^T * Ts.TypeChecker -> bool) (node, checker))
        if
            container
            |> GuardedTracer.hasGuard
            |> not
        then
            container.Guard <- ((^T or GuardTracerSRTPCreator):(static member Create: ^T * Ts.TypeChecker -> GuardTracer) node, checker)
        container :?> XanthamTag
        |> if seen then TagState.createVisited else TagState.createUnvisited
        , if seenGuard then TagState.createUnvisited container.Guard else TagState.createVisited container.Guard

    member inline this.IdentityKey : IdentityKey = this.Guard.Value

type XanTagHelpers =
    [<Extension>]
    static member ToUnderlyingValue(this: XanthamTag) =
        match this.Value with
        | Type typeFlagPrimary -> Choice1Of2 typeFlagPrimary.Value
        | TypeDeclaration typeDeclaration -> Choice2Of2 typeDeclaration.Value
        | TypeNode typeNode -> typeNode.Value |> unbox |> Choice2Of2
        | JSDocTag jsDocTags -> jsDocTags.Value |> unbox |> Choice2Of2
        | LiteralTokenNode literalTokenNodes -> literalTokenNodes.Value |> unbox |> Choice2Of2
        | ModulesAndExports modulesAndExports -> modulesAndExports.Value |> unbox |> Choice2Of2
        | MemberDeclaration memberDeclaration -> memberDeclaration.Value |> Choice2Of2
        | Ignore _ -> failwith "Ignored node accessed"

    [<Extension>]
    static member ToNode(this: XanthamTag) =
        let nodeType = this.Value.Value 
        if nodeType?kind then unbox<Ts.Node> nodeType |> Some
        else None
    [<Extension>]
    static member ToType(this: XanthamTag) =
        let nodeType = this.Value.Value
        if nodeType?flags then unbox<Ts.Type> nodeType |> Some
        else None


// ---------------------------------------------------------------------------
// Curried accessor modules
// ---------------------------------------------------------------------------

/// <summary>
/// Curried data-accessor functions for <see cref="T:Xantham.Fable.AutoOpenXTag.XanthamTag"/>
/// (<c>XanTag&lt;XanTagKind&gt;</c>).
/// All operations are keyed by <c>typeof&lt;'Data&gt;.Name</c> on the tag's JS property bag.
/// </summary>
/// <remarks>
/// This module coexists with the static factory members on <c>type XanTag</c> (e.g.
/// <c>XanTag.Create</c>, <c>XanTag.Seen</c>). Use it for pipeline-style composition:
/// <code lang="fsharp">
/// tag |> XanTag.getOrMapSet resolveKey
/// tags |> List.filter (XanTag.has&lt;TypeKey&gt;)
/// </code>
/// </remarks>
[<RequireQualifiedAccess>]
module XanthamTag =
    /// <summary>
    /// Retrieves the stored <typeparamref name="'Data"/> entry from <paramref name="tag"/>.
    /// </summary>
    /// <param name="tag">The tag whose property bag is queried.</param>
    /// <returns><c>ValueSome</c> if an entry is present; <c>ValueNone</c> otherwise.</returns>
    let inline get<'Data> (tag: XanthamTag) : 'Data = tag.Get<'Data>()
    let inline tryGet<'Data> (tag: XanthamTag) : 'Data voption = tag.TryGet<'Data>()

    /// <summary>
    /// Stores <paramref name="value"/> as <c>ValueSome</c> on <paramref name="tag"/>.
    /// </summary>
    /// <param name="value">The value to store.</param>
    /// <param name="tag">The tag whose property bag is written.</param>
    let inline set (value: 'Data) (tag: XanthamTag) : unit = tag.Set value

    /// <summary>
    /// Sets the <typeparamref name="'Data"/> entry on <paramref name="tag"/> to <c>ValueNone</c>.
    /// </summary>
    /// <param name="tag">The tag whose property bag entry is cleared.</param>
    let inline clear<'Data> (tag: XanthamTag) : unit = tag.Clear<'Data>()

    /// <summary>
    /// Returns <c>true</c> if a <typeparamref name="'Data"/> entry is present on <paramref name="tag"/>.
    /// </summary>
    /// <param name="tag">The tag whose property bag is checked.</param>
    let inline has<'Data> (tag: XanthamTag) : bool = tag.Has<'Data>()

    /// <summary>
    /// Returns the stored <typeparamref name="'Data"/> if present; otherwise maps the tag's
    /// <c>Value</c> via <paramref name="fn"/>, stores the result, and returns it.
    /// </summary>
    /// <param name="fn">Function mapping the tag's <c>XanTagKind</c> value to the data to cache.</param>
    /// <param name="tag">The tag whose property bag is read or populated.</param>
    let inline getOrMapSet (fn: XanTagKind -> 'Data) (tag: XanthamTag) : 'Data = tag.GetOrInit<'Data> (fun () -> fn tag.Value)

    /// <summary>
    /// Returns the stored <typeparamref name="'Data"/> if present; otherwise calls
    /// <paramref name="fn"/>, stores the result, and returns it.
    /// </summary>
    /// <param name="fn">Thunk that produces the value to cache.</param>
    /// <param name="tag">The tag whose property bag is read or populated.</param>
    let inline getOrSetWith (fn: unit -> 'Data) (tag: XanthamTag) : 'Data = tag.GetOrInit<'Data> fn