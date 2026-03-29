/// <summary>
/// Named data-slot accessors for <see cref="T:Xantham.Fable.AutoOpenXTag.XanthamTag"/>.
/// </summary>
/// <remarks>
/// Each module (<c>SummaryContent</c>, <c>Documentation</c>, <c>AstNodeBuilder</c>,
/// <c>TypeSignal</c>) represents one logical piece of data that readers may attach to a tag.
/// All modules follow the same shape: <c>get</c>, <c>has</c>, <c>set</c>, <c>clear</c>,
/// <c>getOrSetWith</c>, <c>getOrMapSet</c> operating on the <b>tag bag</b> (stored on the tag
/// object itself), plus a <c>Keyed</c> sub-module operating on the <b>keyed bag</c> (stored on
/// the guard object, partitioned by identity).
/// <para>
/// The internal <c>Helpers</c> module wraps the raw <c>XanthamTag</c> member calls and is
/// intentionally hidden from IntelliSense — consumers should use the named data modules
/// directly rather than calling <c>Helpers</c>.
/// </para>
/// </remarks>
module Xantham.Fable.Types.GuardedData

open System.ComponentModel
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal

[<EditorBrowsable(EditorBrowsableState.Never)>]
[<RequireQualifiedAccess>]
module Helpers =
    let inline get symbol (tag: XanthamTag) = tag.Get(symbol)
    let inline has symbol (tag: XanthamTag) = tag.Has(symbol)
    let inline set symbol value (tag: XanthamTag) = tag.Set(symbol, value); tag
    let inline clear symbol (tag: XanthamTag) = tag.Clear(symbol); tag
    let inline getOrSetWith symbol (fn: unit -> _) (tag: XanthamTag) = tag.GetOrInit(symbol, fn)
    let inline getOrMapSet symbol (fn: XanTagKind -> _) (tag: XanthamTag) = tag.GetOrInit(symbol, fun () -> fn tag.Value)
    module Keyed =
        let inline get symbol (tag: XanthamTag) = tag.KeyedGet(symbol)
        let inline has symbol (tag: XanthamTag) = tag.KeyedHas(symbol)
        let inline set symbol value (tag: XanthamTag) = tag.KeyedSet(symbol, value); tag
        let inline clear symbol (tag: XanthamTag) = tag.KeyedClear(symbol); tag
        let inline getOrSetWith symbol (fn: unit -> _) (tag: XanthamTag) = tag.KeyedGetOrInit(symbol, fn)
        let inline getOrMapSet symbol (fn: XanTagKind -> _) (tag: XanthamTag) = tag.KeyedGetOrInit(symbol, fun () -> fn tag.Value)
            

[<RequireQualifiedAccess>]
module SummaryContent =
    let symbol = SymbolTypeKey.create<TsComment> "SummaryContent"
    let get tag = Helpers.get symbol tag
    let has tag = Helpers.has symbol tag
    let set value tag = Helpers.set symbol value tag
    let clear tag = Helpers.clear symbol tag
    let getOrSetWith fn tag = Helpers.getOrSetWith symbol fn tag
    let getOrMapSet fn tag = Helpers.getOrMapSet symbol fn tag
    module Keyed =
        let get tag = Helpers.Keyed.get symbol tag
        let has tag = Helpers.Keyed.has symbol tag
        let set value tag = Helpers.Keyed.set symbol value tag
        let clear tag = Helpers.Keyed.clear symbol tag
        let getOrSetWith fn tag = Helpers.Keyed.getOrSetWith symbol fn tag
        let getOrMapSet fn tag = Helpers.Keyed.getOrMapSet symbol fn tag
[<RequireQualifiedAccess>]
module Documentation =
    let symbol = SymbolTypeKey.create<TsComment array> "Documentation"
    let get tag = Helpers.get symbol tag
    let has tag = Helpers.has symbol tag
    let set value tag = Helpers.set symbol value tag
    let clear tag = Helpers.clear symbol tag
    let getOrSetWith fn tag = Helpers.getOrSetWith symbol fn tag
    let getOrMapSet fn tag = Helpers.getOrMapSet symbol fn tag
    module Keyed =
        let get tag = Helpers.Keyed.get symbol tag
        let has tag = Helpers.Keyed.has symbol tag
        let set value tag = Helpers.Keyed.set symbol value tag
        let clear tag = Helpers.Keyed.clear symbol tag
        let getOrSetWith fn tag = Helpers.Keyed.getOrSetWith symbol fn tag
        let getOrMapSet fn tag = Helpers.Keyed.getOrMapSet symbol fn tag
[<RequireQualifiedAccess>]
module AstNodeBuilder =
    let symbol = SymbolTypeKey.create<PendingSignal<STsAstNodeBuilder>> "AstNodeBuilder"
    let get tag = Helpers.get symbol tag
    let has tag = Helpers.has symbol tag
    let set value tag = Helpers.set symbol value tag
    let clear tag = Helpers.clear symbol tag
    let getOrSetWith fn tag = Helpers.getOrSetWith symbol fn tag
    let getOrMapSet fn tag = Helpers.getOrMapSet symbol fn tag
    let getOrSetDefault tag = getOrSetWith (fun () -> Signal.pending<STsAstNodeBuilder>()) tag
    module Keyed =
        let get tag = Helpers.Keyed.get symbol tag
        let has tag = Helpers.Keyed.has symbol tag
        let set value tag = Helpers.Keyed.set symbol value tag
        let clear tag = Helpers.Keyed.clear symbol tag
        let getOrSetWith fn tag = Helpers.Keyed.getOrSetWith symbol fn tag
        let getOrMapSet fn tag = Helpers.Keyed.getOrMapSet symbol fn tag
        let getOrSetDefault tag = getOrSetWith (fun () -> Signal.pending<STsAstNodeBuilder>()) tag
[<RequireQualifiedAccess>]
module TypeSignal =
    let symbol = SymbolTypeKey.create<TypeSignal> "TypeSignal"
    let get tag = Helpers.get symbol tag
    let has tag = Helpers.has symbol tag
    let set value tag = Helpers.set symbol value tag
    let clear tag = Helpers.clear symbol tag
    let getOrSetWith fn tag = Helpers.getOrSetWith symbol fn tag
    let getOrMapSet fn tag = Helpers.getOrMapSet symbol fn tag
    let getOrSetDefault tag = getOrSetWith (fun () -> TypeSignal.pending()) tag
    module Keyed =
        let get tag = Helpers.Keyed.get symbol tag
        let has tag = Helpers.Keyed.has symbol tag
        let set value tag = Helpers.Keyed.set symbol value tag
        let clear tag = Helpers.Keyed.clear symbol tag
        let getOrSetWith fn tag = Helpers.Keyed.getOrSetWith symbol fn tag
        let getOrMapSet fn tag = Helpers.Keyed.getOrMapSet symbol fn tag
        let getOrSetDefault tag = getOrSetWith (fun () -> TypeSignal.pending()) tag

/// <summary>
/// Source file/module. Since symbols can be re-exported, we need to have some contextual
/// method of linking paths of exported types/symbols to the highest level export point.
/// </summary>
module Source =
    let symbol = SymbolTypeKey.create<Signal<ModuleName>> "Source"
    let get tag = Helpers.get symbol tag
    let has tag = Helpers.has symbol tag
    let set value tag = Helpers.set symbol value tag
    let clear tag = Helpers.clear symbol tag
    let getOrSetWith fn tag = Helpers.getOrSetWith symbol fn tag
    let getOrMapSet fn tag = Helpers.getOrMapSet symbol fn tag
    module Keyed =
        let get tag = Helpers.Keyed.get symbol tag
        let has tag = Helpers.Keyed.has symbol tag
        let set value tag = Helpers.Keyed.set symbol value tag
        let clear tag = Helpers.Keyed.clear symbol tag
        let getOrSetWith fn tag = Helpers.Keyed.getOrSetWith symbol fn tag
        let getOrMapSet fn tag = Helpers.Keyed.getOrMapSet symbol fn tag
