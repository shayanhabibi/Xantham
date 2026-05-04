namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator

exception EncoderInvariantViolation of string

/// Wrapper that chooses the type of dictionary (concurrent/normal)
/// depending on the presence of the CONCURRENT_DICT constant on compilation
type DictionaryImpl<'Key, 'Value> =
    #if CONCURRENT_DICT
    System.Collections.Concurrent.ConcurrentDictionary<'Key, 'Value>
    #else
    Dictionary<'Key, 'Value>
    #endif

type RenderMode = Prelude.Render

type SkippableHookResult<'T> =
    | Pass
    | Replace of 'T
    | Skip
type HookResult<'T> =
    | Pass
    | Replace of 'T
type TypeRefPosition =
    | Standalone
    | InheritanceRef
    | TypeArg
    | TupleElement
    | UnionMember
    | FunctionParameter
    | FunctionReturn
    | AliasTarget
    | MemberType
type PathPosition =
    | TopLevelType
    | MemberPath
    | VariablePath
    | FunctionPath
type RenderStage =
    | PathResolution
    | TypeRefBuild
    | TypeRefEmit
    | RenderScopeBuild
    | TypeDefBuild
    | TypeDefEmit
    | Anchored
type RenderPosition =
    | RefPos of TypeRefPosition
    | PathPos of PathPosition
    | NotApplicable
type [<Struct>] RenderContext = {
    Position: RenderPosition
    Owner: ResolvedType voption
    Render: RenderMode
    Stage: RenderStage
}
[<RequireQualifiedAccess>]
type SlotId =
    | PathResolution
    | TypeRefBuild
    | TypeRefEmit
    | RenderScopeBuild
    | TypeDefBuildClass
    | TypeDefBuildAlias
    | TypeDefBuildEnum
    | TypeDefBuildStringUnion
    | TypeDefEmit
    | AnchoredRef
    | AnchoredScope
    
type HandlerToken = HandlerToken of SlotId * obj

    
type PreludeScopeStore = DictionaryImpl< ResolvedType, RenderScope >
type AnchorScopeStore = DictionaryImpl<Choice<ResolvedType, ResolvedExport>, Choice<Anchored.TypeRefRender, Anchored.RenderScope>>
type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> LazyResolvedType -> TypeRefRender

and Hook<'T> = GeneratorContext -> RenderContext -> 'T -> HookResult<'T>
and SkippableHook<'T> = GeneratorContext -> RenderContext -> 'T -> SkippableHookResult<'T>
and HookSlot<'T> = { Handlers: Hook<'T> list; HasAny: bool }
and SkippableHookSlot<'T> = { Handlers: SkippableHook<'T> list; HasAny: bool }
and Customisation = {
    PathResolution: SkippableHookSlot<TypePath>
    TypeRefBuild: HookSlot<TypeRefRender>
    TypeRefEmit: HookSlot<WidgetBuilder<Type>>
    RenderScopeBuild: SkippableHookSlot<RenderScope>
    TypeDefBuildClass: SkippableHookSlot<Concrete.TypeLikeRender>
    TypeDefBuildAlias: SkippableHookSlot<Concrete.TypeAliasRender>
    TypeDefBuildEnum: SkippableHookSlot<Concrete.LiteralUnionRender<int>>
    TypeDefBuildStringUnion: SkippableHookSlot<Concrete.LiteralUnionRender<TsLiteral>>
    TypeDefEmit: HookSlot<WidgetBuilder<Type>>
    AnchoredRef: SkippableHookSlot<Anchored.TypeRefRender>
    AnchoredScope: SkippableHookSlot<Anchored.RenderScope>
}

and GeneratorContext =
    {
        TypeAliasRemap: DictionaryImpl<ResolvedType, TypeRefRender>
        PreludeGetTypeRef: PreludeGetTypeRefFunc
        PreludeRenders: PreludeScopeStore
        AnchorRenders: AnchorScopeStore
        InFlight: HashSet<ResolvedType>
        Customisation: Customisation
        
    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"

module Hook =
    let ofMap (f: 'T -> 'T): Hook<'T> = fun _ _ v -> HookResult.Replace( f v )
module SkippableHook =
    let ofMap (f: 'T -> 'T): SkippableHook<'T> = fun _ _ v -> SkippableHookResult.Replace(f v)
    let ofPredicate (shouldSkip: 'T -> bool): SkippableHook<'T> =
        fun _ _ v -> if shouldSkip v then SkippableHookResult.Skip else SkippableHookResult.Pass
        
[<AutoOpen>]
module HookAutoOpenExtensions =
    let inline hook<'T> ([<InlineIfLambda>] f: GeneratorContext -> RenderContext -> 'T -> HookResult<'T>): Hook<'T> = f
    let inline skippableHook<'T> ([<InlineIfLambda>] f: GeneratorContext -> RenderContext -> 'T -> SkippableHookResult<'T>): SkippableHook<'T> = f


module HookSlot =
    let empty<'T> : HookSlot<'T> = { Handlers = []; HasAny = false }

    // Chainable: most consumers don't need a token.
    let add (h: Hook<'T>) (slot: HookSlot<'T>) : HookSlot<'T> =
        { Handlers = h :: slot.Handlers; HasAny = true }

    // Tracked: returns the slot plus the boxed handler reference for use as a token.
    // The `Customisation.add{Slot}Tracked` wrapper is responsible for tagging the
    // boxed reference with its SlotId before exposing a `HandlerToken`.
    let addTracked (h: Hook<'T>) (slot: HookSlot<'T>) : HookSlot<'T> * obj =
        { Handlers = h :: slot.Handlers; HasAny = true }, box h

    let remove (token: obj) (slot: HookSlot<'T>) : HookSlot<'T> =
        let h' = slot.Handlers |> List.filter (fun h -> not (System.Object.ReferenceEquals(box h, token)))
        { Handlers = h'; HasAny = not (List.isEmpty h') }

    let clear<'T> : HookSlot<'T> -> HookSlot<'T> = fun _ -> empty<'T>

    let rec private loop (result: HookResult<'T>) current handlers ctx rctx =
        match handlers with
        | [] -> result
        | h :: rest ->
            match h ctx rctx current with
            | HookResult.Pass      -> loop result current rest ctx rctx
            | HookResult.Replace v -> loop (HookResult.Replace v) v rest ctx rctx

    let inline run (slot: HookSlot<'T>) ctx rctx value =
        if not slot.HasAny then HookResult.Pass
        else loop HookResult.Pass value slot.Handlers ctx rctx

module SkippableHookSlot =
    let empty<'T> : SkippableHookSlot<'T> = { Handlers = []; HasAny = false }

    let add (h: SkippableHook<'T>) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> =
        { Handlers = h :: slot.Handlers; HasAny = true }

    let addTracked (h: SkippableHook<'T>) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> * obj =
        { Handlers = h :: slot.Handlers; HasAny = true }, box h

    let remove (token: obj) (slot: SkippableHookSlot<'T>) : SkippableHookSlot<'T> =
        let h' = slot.Handlers |> List.filter (fun h -> not (System.Object.ReferenceEquals(box h, token)))
        { Handlers = h'; HasAny = not (List.isEmpty h') }

    let clear<'T> : SkippableHookSlot<'T> -> SkippableHookSlot<'T> = fun _ -> empty<'T>

    let rec private loop result current handlers ctx rctx =
        match handlers with
        | [] -> result
        | h :: rest ->
            match h ctx rctx current with
            | SkippableHookResult.Pass      -> loop result current rest ctx rctx
            | SkippableHookResult.Replace v -> loop (SkippableHookResult.Replace v) v rest ctx rctx
            | SkippableHookResult.Skip      -> SkippableHookResult.Skip

    let inline run slot ctx rctx value =
        if not slot.HasAny then SkippableHookResult.Pass
        else loop SkippableHookResult.Pass value slot.Handlers ctx rctx

type Customisation with
    static member Default = {
        PathResolution = SkippableHookSlot.empty
        TypeRefBuild = HookSlot.empty
        TypeRefEmit = HookSlot.empty
        RenderScopeBuild = SkippableHookSlot.empty
        TypeDefBuildClass = SkippableHookSlot.empty
        TypeDefBuildAlias = SkippableHookSlot.empty
        TypeDefBuildEnum = SkippableHookSlot.empty
        TypeDefBuildStringUnion = SkippableHookSlot.empty
        TypeDefEmit = HookSlot.empty
        AnchoredRef = SkippableHookSlot.empty
        AnchoredScope = SkippableHookSlot.empty
    }

module Customisation =
    let addPathResolution hook customisation = { customisation with PathResolution = SkippableHookSlot.add hook customisation.PathResolution }
    let addTypeRefBuild hook customisation = { customisation with TypeRefBuild = HookSlot.add hook customisation.TypeRefBuild }
    let addTypeRefEmit hook customisation = { customisation with TypeRefEmit = HookSlot.add hook customisation.TypeRefEmit }
    let addRenderScopeBuild hook customisation = { customisation with RenderScopeBuild = SkippableHookSlot.add hook customisation.RenderScopeBuild }
    let addTypeDefBuildClass hook customisation = { customisation with TypeDefBuildClass = SkippableHookSlot.add hook customisation.TypeDefBuildClass }
    let addTypeDefBuildAlias hook customisation = { customisation with TypeDefBuildAlias = SkippableHookSlot.add hook customisation.TypeDefBuildAlias }
    let addTypeDefBuildEnum hook customisation = { customisation with TypeDefBuildEnum = SkippableHookSlot.add hook customisation.TypeDefBuildEnum }
    let addTypeDefBuildStringUnion hook customisation = { customisation with TypeDefBuildStringUnion = SkippableHookSlot.add hook customisation.TypeDefBuildStringUnion }
    let addTypeDefEmit hook customisation = { customisation with TypeDefEmit = HookSlot.add hook customisation.TypeDefEmit }
    let addAnchoredRef hook customisation = { customisation with AnchoredRef = SkippableHookSlot.add hook customisation.AnchoredRef }
    let addAnchoredScope hook customisation = { customisation with AnchoredScope = SkippableHookSlot.add hook customisation.AnchoredScope }
    let addPathResolutionTracked hook customisation: Customisation * HandlerToken =
        let slot, token = SkippableHookSlot.addTracked hook customisation.PathResolution
        { customisation with PathResolution = slot },
        (SlotId.PathResolution, token)
        |> HandlerToken
    let addTypeRefBuildTracked hook customisation =
        let slot, token = HookSlot.addTracked hook customisation.TypeRefBuild
        { customisation with TypeRefBuild = slot },
        (SlotId.TypeRefBuild, token)
        |> HandlerToken
    let addTypeRefEmitTracked hook customisation =
        let slot, token = HookSlot.addTracked hook customisation.TypeRefEmit
        { customisation with TypeRefEmit = slot },
        (SlotId.TypeRefEmit, token)
        |> HandlerToken
    let addRenderScopeBuildTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.RenderScopeBuild
        { customisation with RenderScopeBuild = slot },
        (SlotId.RenderScopeBuild, token)
        |> HandlerToken
    let addTypeDefBuildClassTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.TypeDefBuildClass
        { customisation with TypeDefBuildClass = slot },
        (SlotId.TypeDefBuildClass, token)
        |> HandlerToken
    let addTypeDefBuildAliasTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.TypeDefBuildAlias
        { customisation with TypeDefBuildAlias = slot },
        (SlotId.TypeDefBuildAlias, token)
        |> HandlerToken
    let addTypeDefBuildEnumTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.TypeDefBuildEnum
        { customisation with TypeDefBuildEnum = slot },
        (SlotId.TypeDefBuildEnum, token)
        |> HandlerToken
    let addTypeDefBuildStringUnionTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.TypeDefBuildStringUnion
        { customisation with TypeDefBuildStringUnion = slot },
        (SlotId.TypeDefBuildStringUnion, token)
        |> HandlerToken
    let addTypeDefEmitTracked hook customisation =
        let slot, token = HookSlot.addTracked hook customisation.TypeDefEmit
        { customisation with TypeDefEmit = slot },
        (SlotId.TypeDefEmit, token)
        |> HandlerToken
    let addAnchoredRefTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.AnchoredRef
        { customisation with AnchoredRef = slot },
        (SlotId.AnchoredRef, token)
        |> HandlerToken
    let addAnchoredScopeTracked hook customisation =
        let slot, token = SkippableHookSlot.addTracked hook customisation.AnchoredScope
        { customisation with AnchoredScope = slot },
        (SlotId.AnchoredScope, token)
        |> HandlerToken
    let remove (HandlerToken (slotId, token)) customisation =
        match slotId with
        | SlotId.PathResolution ->
            { customisation with PathResolution = SkippableHookSlot.remove token customisation.PathResolution }
        | SlotId.TypeRefBuild ->
            { customisation with TypeRefBuild = HookSlot.remove token customisation.TypeRefBuild }
        | SlotId.TypeRefEmit ->
            { customisation with TypeRefEmit = HookSlot.remove token customisation.TypeRefEmit }
        | SlotId.RenderScopeBuild ->
            { customisation with RenderScopeBuild = SkippableHookSlot.remove token customisation.RenderScopeBuild }
        | SlotId.TypeDefBuildClass ->
            { customisation with TypeDefBuildClass = SkippableHookSlot.remove token customisation.TypeDefBuildClass }
        | SlotId.TypeDefBuildAlias ->
            { customisation with TypeDefBuildAlias = SkippableHookSlot.remove token customisation.TypeDefBuildAlias }
        | SlotId.TypeDefBuildEnum ->
            { customisation with TypeDefBuildEnum = SkippableHookSlot.remove token customisation.TypeDefBuildEnum }
        | SlotId.TypeDefBuildStringUnion ->
            { customisation with TypeDefBuildStringUnion = SkippableHookSlot.remove token customisation.TypeDefBuildStringUnion }
        | SlotId.TypeDefEmit ->
            { customisation with TypeDefEmit = HookSlot.remove token customisation.TypeDefEmit }
        | SlotId.AnchoredRef ->
            { customisation with AnchoredRef = SkippableHookSlot.remove token customisation.AnchoredRef }
        | SlotId.AnchoredScope ->
            { customisation with AnchoredScope = SkippableHookSlot.remove token customisation.AnchoredScope }

type GeneratorContext with
    static member internal Create(preludeGetTypeRefFunc, ?customisation) = {
        PreludeRenders = DictionaryImpl()
        AnchorRenders = DictionaryImpl()
        PreludeGetTypeRef = preludeGetTypeRefFunc
        InFlight = HashSet()
        TypeAliasRemap = DictionaryImpl()
        Customisation = defaultArg customisation Customisation.Default
    }

module GeneratorContext =
    module private Operation =
        let inline getOrAdd func key (dict: DictionaryImpl<'Key, 'Value>) =
            #if CONCURRENT_DICT
            dict.GetOrAdd(key, System.Func<_,_>(func))
            #else
            match dict.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = func key
                dict[key] <- value
                value
            #endif
        let inline tryGet key (dict: DictionaryImpl<'Key, 'Value>) =
            match dict.TryGetValue(key) with
            | true, value -> ValueSome value
            | _ -> ValueNone
        let inline tryAdd key value (dict: DictionaryImpl<'Key, 'Value>) =
            dict.TryAdd(key, value)
        let inline addOrReplace key value (dict: DictionaryImpl<'Key, 'Value>) =
            dict[key] <- value
    
    module Prelude =
        let addTypeAliasRemap ctx key value =
            ctx.TypeAliasRemap
            |> Operation.addOrReplace key value
        let canFlight ctx key =
            #if CONCURRENT_DICT
            lock ctx.InFlight (fun () ->
            #endif
            ctx.InFlight.Add key
            #if CONCURRENT_DICT
                )
            #endif
        let tryGet ctx key =
            ctx.PreludeRenders
            |> Operation.tryGet key
        let addOrReplace ctx key value =
            ctx.PreludeRenders
            |> Operation.addOrReplace key value
    module Anchored =
        let tryGet (ctx: GeneratorContext) (key: Choice<ResolvedType, ResolvedExport>) =
            ctx.AnchorRenders
            |> Operation.tryGet key
        
        let tryGetResolvedType (ctx: GeneratorContext) (key: ResolvedType) =
            tryGet ctx (Choice1Of2 key)
            
        let tryGetResolvedExport (ctx: GeneratorContext) (key: ResolvedExport) =
            tryGet ctx (Choice2Of2 key)

        let addOrReplace (ctx: GeneratorContext) (key: Choice<ResolvedType, ResolvedExport>) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            let value = ctx.Customisation.Interceptors.AnchoredRender ctx key value
            ctx.AnchorRenders
            |> Operation.addOrReplace key value
            
        let addResolvedType (ctx: GeneratorContext) (key: ResolvedType) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice1Of2 key) value
            
        let addResolvedExport (ctx: GeneratorContext) (key: ResolvedExport) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice2Of2 key) value
        
            
            
