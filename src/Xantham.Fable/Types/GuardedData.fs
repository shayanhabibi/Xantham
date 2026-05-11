/// <summary>
/// Named data-slot accessors for <see cref="T:Xantham.Fable.AutoOpenXTag.XanthamTag"/>.
/// </summary>
/// <remarks>
/// Each module (<c>SummaryContent</c>, <c>Documentation</c>, <c>AstNodeBuilder</c>,
/// <c>TypeSignal</c>) represents one logical piece of data that readers may attach to a tag.
/// All modules follow the same shape: <c>get</c>, <c>has</c>, <c>set</c>, <c>clear</c>,
/// <c>getOrSetWith</c>, <c>getOrMapSet</c> operating on the <b>tag bag</b> (stored on the tag
/// object itself), plus a <c>Keyed</c> sub-module operating on the <b>keyed bag</b> (stored on
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
open Xantham.Fable.Types.Tracer

type KeyedSymbolSlot<'TagKind, 'T> =
    abstract member get: XanthamTag -> 'T
    abstract member has: XanthamTag -> bool
    abstract member set: 'T -> XanthamTag -> XanthamTag
    abstract member clear: XanthamTag -> XanthamTag
    abstract member getOrSetWith: (unit -> 'T) -> XanthamTag -> 'T
    abstract member getOrMapSet: (XanTagKind -> 'T) -> XanthamTag -> 'T
type SymbolSlot<'TagValue, 'TagGuard, 'T> =
    inherit KeyedSymbolSlot<'TagValue, 'T>
    abstract Keyed: KeyedSymbolSlot<'TagGuard, 'T>
type KeyedSymbolSlotWithDefault<'TagKind, 'T> =
    inherit KeyedSymbolSlot<'TagKind, 'T>
    abstract getOrSetDefault: XanthamTag -> 'T
type SymbolSlotWithDefault<'TagValue, 'TagGuard, 'T> =
    inherit SymbolSlot<'TagValue, 'TagGuard, 'T>
    abstract getOrSetDefault: XanthamTag -> 'T
type KeyedPendingSymbolSlot<'TagKind, 'T> =
    inherit KeyedSymbolSlotWithDefault<'TagKind, PendingSignal<'T>>
type PendingSymbolSlot<'TagValue, 'TagGuard, 'T> =
    inherit SymbolSlotWithDefault<'TagValue, 'TagGuard, PendingSignal<'T>>


[<EditorBrowsable(EditorBrowsableState.Never)>]
[<RequireQualifiedAccess>]
module Helpers =
    let inline makeSlotWithDefault<'TagValue, 'TagGuard, 'T> thunk name =
        let symbol = SymbolTypeKey.create<'T> name
        { new SymbolSlotWithDefault<'TagValue, 'TagGuard, 'T> with
            member _.get tag = tag.Get(symbol)
            member _.has tag = tag.Has(symbol)
            member _.set value tag = tag.Set(symbol, value); tag
            member _.clear tag = tag.Clear(symbol); tag
            member _.getOrSetWith thunk tag = tag.GetOrInit(symbol, thunk)
            member _.getOrMapSet thunk tag = tag.GetOrInit(symbol, fun () -> thunk tag.Value)
            member _.getOrSetDefault tag = tag.GetOrInit(symbol, thunk)
            member _.Keyed = {
                new KeyedSymbolSlotWithDefault<'TagGuard, 'T> with
                    member _.get tag = tag.KeyedGet(symbol)
                    member _.has tag = tag.KeyedHas(symbol)
                    member _.set value tag = tag.KeyedSet(symbol, value); tag
                    member _.clear tag = tag.KeyedClear(symbol); tag
                    member _.getOrSetWith thunk tag = tag.KeyedGetOrInit(symbol, thunk)
                    member _.getOrMapSet thunk tag = tag.KeyedGetOrInit(symbol, fun () -> thunk tag.Value)
                    member _.getOrSetDefault tag = tag.KeyedGetOrInit(symbol, thunk)
            }
        }
    let inline makeSlot<'TagValue, 'TagGuard, 'T> name =
        makeSlotWithDefault<'TagValue, 'TagGuard, 'T>
            (fun () -> Fable.Core.JS.undefined)
            name
        :> SymbolSlot<'TagValue, 'TagGuard, 'T>
    let inline makePendingSlot<'TagValue, 'TagGuard, 'T> name =
        makeSlotWithDefault<'TagValue, 'TagGuard, PendingSignal<'T>>
            (fun () -> Signal.pending())
            name
        :?> PendingSymbolSlot<'TagValue, 'TagGuard, 'T>

let SummaryContent = Helpers.makeSlot<XanTagKind, GuardTracer, TsComment> "SummaryContent"
let Documentation = Helpers.makeSlot<XanTagKind, GuardTracer, TsComment array> "Documentation"
let ParameterBuilder = Helpers.makePendingSlot<XanTagKind, GuardTracer, SParameterBuilder> "ParameterBuilder"
let ConstructorBuilder = Helpers.makePendingSlot<XanTagKind, GuardTracer, SConstructorBuilder> "ConstructorBuilder"
let MemberBuilder = Helpers.makePendingSlot<XanTagKind, GuardTracer, SMemberBuilder> "MemberBuilder"
let AstNodeBuilder = Helpers.makePendingSlot<XanTagKind, GuardTracer, SType> "AstNodeBuilder"
let TypeSignal = Helpers.makeSlotWithDefault<XanTagKind, GuardTracer, TypeSignal> (fun () -> TypeSignal.pending()) "TypeSignal"
let Source = Helpers.makeSlot<XanTagKind, GuardTracer, Signal<ExportCollection voption>> "Source"
let Metadata = Helpers.makeSlot<XanTagKind, GuardTracer, Signal<Metadata>> "Metadata"
let ExportBuilder = Helpers.makePendingSlot<XanTagKind, GuardTracer, STsExportDeclaration> "ExportBuilder"
type OutputKind =
    | None = 0
    | Type = (1 <<< 0)
    | Member = (1 <<< 1)
    | DocsOrOther = (1 <<< 2)
    | Exported = (1 <<< 3)
module OutputKind =
    let symbol = SymbolTypeKey.create<OutputKind> "OutputKind"
    let get (tag: XanthamTag) =
        tag.GetOrInit(symbol, fun () ->
            match tag.Value with
            | Patterns.XanTagKind.IsType _ -> OutputKind.Type
            | Patterns.XanTagKind.IsMember _ -> OutputKind.Member
            | Patterns.XanTagKind.IsDocsOrOther _ -> OutputKind.DocsOrOther
            | Patterns.XanTagKind.IsExported _ -> OutputKind.Exported
            | Patterns.XanTagKind.IsExportedType _ -> OutputKind.Exported ||| OutputKind.Type
        )
    let isType: XanthamTag -> bool = get >> _.HasFlag(OutputKind.Type)
    let isMember: XanthamTag -> bool = get >> _.HasFlag(OutputKind.Member)
    let isDocsOrOther: XanthamTag -> bool = get >> _.HasFlag(OutputKind.DocsOrOther)
    let isExported: XanthamTag -> bool = get >> _.HasFlag(OutputKind.Exported)
    let isTypeOnly tag = isType tag && not(isExported tag)
    let isExportedOnly tag = isExported tag && not(isType tag)
    