namespace Xantham.Generator

open System.Collections.Generic
open Xantham
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator
open FSharp.SignalsDotnet

/// <summary>
/// Push to stack
/// </summary>
type WorkQueue = Stack<ResolvedType>
type PreludeScopeStore = DictionarySignal<ResolvedType, RenderScope>
type PreludeExportStore = DictionarySignal<ResolvedExport, RenderScope>
type PreludeGetRenderScope = LazyResolvedType -> RenderScopeStore -> IReadOnlySignal<RenderScope>
type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> ResolvedType -> IReadOnlySignal<TypeRefRender>
and PreludeProcessor = GeneratorContext -> RenderScope -> unit

and GeneratorContext =
    {
        WorkQueue: WorkQueue
        PreludeExports: PreludeExportStore
        PreludeRenders: PreludeScopeStore
    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"
    static member internal Create() = {
        PreludeRenders = DictionarySignal()
        PreludeExports = DictionarySignal()
        WorkQueue = WorkQueue()
    }
    member this.Push(resolvedType) =
        this.WorkQueue.Push(resolvedType)
    member this.Run() =
        this.WorkQueue
        |> Stack.iter (fun resolvedType ->
            ()
            )

module GeneratorContext =
    module Prelude =
        type SRTPHelper =
            static member inline Add(ctx: GeneratorContext, key, value) =
                ctx.PreludeRenders
                |> DictionarySignal.addOrReplace key value
            static member inline Add(ctx: GeneratorContext, key, value) =
                ctx.PreludeExports
                |> DictionarySignal.addOrReplace key value
            static member inline GetOrAdd(ctx: GeneratorContext, key, value) =
                ctx.PreludeRenders
                |> DictionarySignal.getOrAdd key value
            static member inline GetOrAdd(ctx: GeneratorContext, key, value) =
                ctx.PreludeExports
                |> DictionarySignal.getOrAdd key value
        let inline addOrReplace ctx key value =
            ((^T or SRTPHelper): (static member Add: GeneratorContext * ^U * ^T -> unit) ctx, key, value)
        let inline getOrAdd ctx key value =
            ((^T or SRTPHelper): (static member GetOrAdd: GeneratorContext * ^U * ^T -> ^T) ctx, key, value)
        let getOrAddDefault ctx key =
            ctx.PreludeRenders
            |> DictionarySignal.getOrAddWithThen
                   key RenderScope.createDefault ctx.Push
        let getRender (ctx: GeneratorContext) (resolvedType: ResolvedType) =
            Signal.compute <| fun _ ->
                getOrAddDefault ctx resolvedType
        let getRenderWithScope (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (work: RenderScopeStore -> RenderScope -> 'T) (resolvedType: ResolvedType) =
            Signal.compute <| fun _ ->
                getOrAddDefault ctx resolvedType
                |> work scopeStore
            
            
