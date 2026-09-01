[<AutoOpen>]
module Xantham.Fable.Types.XanTagExtensions

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions

let inline private ifHasThenGetOrNone (xanTag: XanthamTag) (v: GuardedData.KeyedSymbolSlot<_, 'T>) =
    if v.has xanTag then ValueSome (v.get xanTag) else ValueNone

type XanthamTag with
    // ===== Documentation Bag ======
    member this.TryDocumentation with get() = ifHasThenGetOrNone this GuardedData.Documentation
    member this.Documentation with set(value: TsComment array) =
            GuardedData.Documentation.set value this
            |> ignore
    member this.TryKeyedDocumentation with get() = ifHasThenGetOrNone this GuardedData.Documentation.Keyed
    member this.KeyedDocumentation with set(value: TsComment array) = GuardedData.Documentation.Keyed.set value this |> ignore
    // ===== Summary Documentation Bag ======
    member this.TrySummaryDocumentation with get() = ifHasThenGetOrNone this GuardedData.SummaryContent
    member this.SummaryDocumentation with set(value: TsComment) = GuardedData.SummaryContent.set value this |> ignore
    member this.TryKeyedSummaryDocumentation with get() = ifHasThenGetOrNone this GuardedData.SummaryContent.Keyed
    member this.KeyedSummaryDocumentation with set(value: TsComment) = GuardedData.SummaryContent.Keyed.set value this |> ignore
    // ===== TypeKey Bag ======
    member this.TryTypeSignal with get() = ifHasThenGetOrNone this GuardedData.TypeSignal
    member this.TypeSignal
        with get() = GuardedData.TypeSignal.getOrSetDefault this
        and set(value: TypeKey) =
            GuardedData.TypeSignal.getOrSetDefault this
            |> _.Set(value)
    // ===== Builder Bag ======   
    member this.TryBuilder with get() = ifHasThenGetOrNone this GuardedData.AstNodeBuilder
    member this.Builder
        with get() = GuardedData.AstNodeBuilder.getOrSetDefault this
        and set(value: SType) =
            GuardedData.AstNodeBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== MemberBuilder Bag ======  
    member this.TryMemberBuilder with get() = ifHasThenGetOrNone this GuardedData.MemberBuilder
    member this.MemberBuilder
        with get() = GuardedData.MemberBuilder.getOrSetDefault this
        and set(value: SMemberBuilder) =
            GuardedData.MemberBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== ParameterBuilder Bag ====== 
    member this.TryParameterBuilder with get() = ifHasThenGetOrNone this GuardedData.ParameterBuilder
    member this.ParameterBuilder
        with get() = GuardedData.ParameterBuilder.getOrSetDefault this
        and set(value: SParameterBuilder) =
            GuardedData.ParameterBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== ConstructorBuilder Bag ======
    member this.TryConstructorBuilder with get() = ifHasThenGetOrNone this GuardedData.ConstructorBuilder
    member this.ConstructorBuilder
        with get() = GuardedData.ConstructorBuilder.getOrSetDefault this
        and set(value: SConstructorBuilder) =
            GuardedData.ConstructorBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== Export Bag ======
    member this.TryExportBuilder with get() = ifHasThenGetOrNone this GuardedData.ExportBuilder
    member this.ExportBuilder
        with get() = GuardedData.ExportBuilder.getOrSetDefault this
        and set(value: STsExportDeclaration) =
            GuardedData.ExportBuilder.getOrSetDefault this
            |> Signal.fill value
    // ===== Source Bag ======
    member this.TrySource with get() = ifHasThenGetOrNone this GuardedData.Source
    member this.Source with set(value: ExportCollection) =
        GuardedData.Source.getOrSetWith (fun () -> Signal.source (ValueSome value)) this
        |> Signal.fill value
    member this.Logger with get() =
        if GuardedData.Log.has this then
            GuardedData.Log.get this
        else ValueNone
        |> ValueOption.defaultValue Utils.Logging.Log.Default
        
type XanthamTag with
    /// <summary>
    /// For tags where <c>Debug</c> is true, calls <paramref name="fn"/> with the tag.
    /// </summary>
    /// <remarks>
    /// Only available in DEBUG builds. Noop otherwise.
    /// See <see cref="M:XanthamTag.setDebug"/> for flipping the <c>Debug</c> flag.
    /// </remarks>
    /// <param name="fn"></param>
    member inline tag.withDebug([<InlineIfLambda>] fn: XanthamTag -> unit) =
        #if DEBUG
        if tag.Debug then fn tag
        #endif
        tag
    member inline this.doWithDebug(fn) = this.withDebug(fn) |> ignore
    /// <summary>
    /// Sets the <c>Debug</c> flag to true on tag.
    /// </summary>
    /// <param name="condition"></param>
    member inline this.setDebug([<InlineIfLambda>] condition: XanthamTag -> bool) =
        #if FAIL_ON_DEBUG_TRACKING
        failwith "Debug tracking activated"
        #endif
        #if DEBUG
        if condition this then
            this.Debug <- true
        #endif
        this
    member inline tag.withDebugForReason (
        reason: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = fun (condition: XanthamTag -> bool) ->
        #if FAIL_ON_DEBUG_TRACKING
        failwith "Debug tracking activated"
        #endif
        #if DEBUG
        if not tag.Debug && condition tag then
            tag.Debug <- true
            match tag.ToUnderlyingValue() with
            | Choice1Of2 typ ->
                tag.Logger.logfd
                    ("[%i{debugId}] [%s{event}] [%s{tagType}] [%A{tagTypeDiscriminators}] Tracking %A{tagIdentityKey}: %s{trackingReason}.", filePath, fileLine)
                    tag.DebugId
                    "TRACKING"
                    "TYPE"
                    (typ.flags.ToStringArray())
                    (tag.IdentityKey.ToString())
                    reason
            | Choice2Of2 decl ->
                tag.Logger.logfd
                    ("[%i{debugId}] [%s{event}] [%s{tagType}] [%A{tagTypeDiscriminators}] Tracking %A{tagIdentityKey}: %s{trackingReason}.", filePath, fileLine)
                    tag.DebugId
                    "TRACKING"
                    "DECLARATION"
                    decl.kind.Name
                    (tag.IdentityKey.ToString())
                    reason
        #endif
        tag
    member inline tag.setDebugForReason(
        reason: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = tag.withDebugForReason (reason, filePath, fileLine) >> ignore
    member inline tag.withDebugForReasonOr(
        onFail: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = fun (reason: string) (condition: XanthamTag -> bool) ->
        #if FAIL_ON_DEBUG_TRACKING
        failwith "Debug tracking activated"
        #endif
        #if DEBUG
        if condition tag then
            let isDebug = tag.Debug
            if not tag.Debug then
                tag.Debug <- true
            match tag.ToUnderlyingValue() with
            | Choice1Of2 typ ->
                tag.Logger.logfd
                    ("[%i{debugId}] [%s{event}] [%b{firstTrack}] [%s{tagType}] [%A{tagTypeDiscriminators}] Tracking %A{tagIdentityKey}: %s{trackingReason}.", filePath, fileLine)
                    tag.DebugId
                    "TRACKING"
                    isDebug
                    "TYPE"
                    (typ.flags.ToStringArray())
                    (tag.IdentityKey.ToString())
                    reason
            | Choice2Of2 decl -> 
                tag.Logger.logfd
                    ("[%i{debugId}] [%s{event}] [%b{firstTrack}] [%s{tagType}] [%A{tagTypeDiscriminators}] Tracking %A{tagIdentityKey}: %s{trackingReason}.", filePath, fileLine)
                    tag.DebugId
                    "TRACKING"
                    isDebug
                    "DECLARATION"
                    decl.kind.Name
                    (tag.IdentityKey.ToString())
                    onFail
        #endif
        tag
    member inline tag.setDebugForReasonOr(
        onFail: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = fun reason -> tag.withDebugForReasonOr (onFail, filePath, fileLine) reason >> ignore
    /// <summary>
    /// For tags where <c>Debug</c> is true, calls <paramref name="fn"/> with the tag once for the
    /// given key.
    /// </summary>
    /// <remarks>
    /// Only available in DEBUG builds. Noop otherwise.
    /// See <see cref="M:XanthamTag.setDebug"/> for flipping the <c>Debug</c> flag.
    /// </remarks>
    /// <param name="key"></param>
    /// <param name="fn"></param>
    member inline tag.withDebugOneShot (key: string) ([<InlineIfLambda>] fn: XanthamTag -> unit) =
        #if DEBUG
        tag.withDebug (fun tag ->
            if !!(tag["DebugOneShots"]) then
                if !!(tag["DebugOneShots"][key]) then ()
                else
                fn tag
                tag["DebugOneShots"][key] <- true
            else
                tag["DebugOneShots"] <- createObj [ key ==> true ]
                fn tag
            )
        #else
        tag
        #endif
    member inline tag.doDebugOneShot (key: string) ([<InlineIfLambda>] fn: XanthamTag -> unit) = tag.withDebugOneShot key fn |> ignore
    member inline tag.chainDebug(
        parent: XanthamTag,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) =
        #if DEBUG
        if parent <> tag then
            parent.doWithDebug (fun _ ->
                let debugIdParent = $"[{parent.DebugId}]"
                tag.setDebugForReasonOr($"Parent {debugIdParent} attempted to chain", filePath, fileLine) $"Parent {debugIdParent} chained debug" (fun _ -> true))
        #endif
        tag
    member inline tag.debugMessage(
        message: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) =
        tag.withDebug (fun tag ->
            tag.Logger.logfd("[%i{debugId}] [%s{event}] %s{message}", filePath, fileLine)
                tag.DebugId
                "EMIT"
                message
            )
    member inline tag.doDebugMessage(
        message: string,
        [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) =
        tag.debugMessage(message, filePath, fileLine) |> ignore
        
    member inline tag.trace ([<InlineIfLambda>] fn: Utils.Logging.Log -> int -> unit): unit =
        #if !DEBUG
        ()
        #else
        fn tag.Logger tag.TraceId
        #endif