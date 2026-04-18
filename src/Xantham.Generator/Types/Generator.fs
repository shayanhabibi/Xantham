namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator

/// Wrapper that chooses the type of dictionary (concurrent/normal)
/// depending on the presence of the CONCURRENT_DICT constant on compilation
type DictionaryImpl<'Key, 'Value> =
    #if CONCURRENT_DICT
    System.Collections.Concurrent.ConcurrentDictionary<'Key, 'Value>
    #else
    Dictionary<'Key, 'Value>
    #endif

type PreludeScopeStore = DictionaryImpl< ResolvedType, RenderScope >
type AnchorScopeStore = DictionaryImpl<Choice<ResolvedType, ResolvedExport>, Choice<Anchored.TypeRefRender, Anchored.RenderScope>>
type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> LazyResolvedType -> TypeRefRender
and GeneratorContext =
    {
        TypeAliasRemap: DictionaryImpl<ResolvedType, TypeRefRender>
        PreludeGetTypeRef: PreludeGetTypeRefFunc
        PreludeRenders: PreludeScopeStore
        AnchorRenders: AnchorScopeStore
        InFlight: HashSet<ResolvedType>
        
    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"
    static member internal Create(preludeGetTypeRefFunc) = {
        PreludeRenders = DictionaryImpl()
        AnchorRenders = DictionaryImpl()
        PreludeGetTypeRef = preludeGetTypeRefFunc
        InFlight = HashSet()
        TypeAliasRemap = DictionaryImpl()
    }

module GeneratorContext =
    /// <summary>
    /// Inlined implementations of the required functions with
    /// branching compile time logic depending on the type of dictionary
    /// we are working with due to the absence/presence of the CONCURRENT_DICT constant.
    /// </summary>
    module private Operation =
        let inline getOrAdd func key dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.tryItemOrAdd func key dict
            #else
            Dictionary.tryItem key dict
            |> ValueOption.defaultWith (fun () ->
                Dictionary.tryAddOrGet key (func key) dict)
            #endif
        let inline tryGet key dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.tryItem key dict
            #else
            Dictionary.tryItem key dict
            #endif
        let inline addOrReplace key value dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.addOrReplace key value dict
            #else
            Dictionary.addOrReplace key value dict
            #endif
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
        let addOrReplaceChoice ctx key value =
            ctx.PreludeRenders
            |> Operation.addOrReplace key value
        let addOrReplace ctx key value =
            ctx.PreludeRenders
            |> Operation.addOrReplace key value
    module Anchored =
        type SRTPHelper =
            static member inline Add(ctx: GeneratorContext, key, value) =
                ctx.AnchorRenders
                |> Operation.addOrReplace key value
            static member inline Add(ctx, key, value) =
                ctx.AnchorRenders
                |> Operation.addOrReplace (Choice1Of2 key) (Choice1Of2 value)
            static member inline Add(ctx, key, value) =
                ctx.AnchorRenders
                |> Operation.addOrReplace (Choice1Of2 key) (Choice2Of2 value)
            static member inline Add(ctx, key, value) =
                ctx.AnchorRenders
                |> Operation.addOrReplace (Choice2Of2 key) (Choice2Of2 value)
            static member inline TryGet(ctx, key) =
                ctx.AnchorRenders
                |> Operation.tryGet key
            static member inline TryGet(ctx, key) =
                ctx.AnchorRenders
                |> Operation.tryGet (Choice1Of2 key)
            static member inline TryGet(ctx, key) =
                ctx.AnchorRenders
                |> Operation.tryGet (Choice2Of2 key)
                
        let inline tryGet ctx key =
            ((^T or SRTPHelper): (static member TryGet: GeneratorContext * ^T -> Choice<Anchored.TypeRefRender, Anchored.RenderScope> voption) ctx, key)
            
        let inline addOrReplace ctx key value =
            ((^T or SRTPHelper): (static member Add: GeneratorContext * ^T * ^U -> unit) ctx, key, value)
        
            
            
