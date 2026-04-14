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

type PreludeScopeStore = DictionaryImpl<
    ResolvedType, // Key
    Choice< // 1 of 3 value types
        Prelude.Widget.RenderScope,
        Prelude.Transient.RenderScope, // transient render scopes
        Prelude.Concrete.RenderScope // concrete render scopes
        >
    >
type AnchorScopeStore = DictionaryImpl<Choice<ResolvedType, ResolvedExport>, Anchored.RenderScope>

and GeneratorContext =
    {
        PreludeRenders: PreludeScopeStore
        AnchorRenders: AnchorScopeStore
    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"
    static member Create() = {
        PreludeRenders = DictionaryImpl()
        AnchorRenders = DictionaryImpl()
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
        type SRTPHelper =
            static member inline Add(ctx, key, value) =
                ctx.PreludeRenders
                |> Operation.addOrReplace key (Choice1Of3 value)
            static member inline Add(ctx, key, value) =
                ctx.PreludeRenders
                |> Operation.addOrReplace key (Choice2Of3 value)
            static member inline Add(ctx, key, value) =
                ctx.PreludeRenders
                |> Operation.addOrReplace key (Choice3Of3 value)
            static member inline Add(ctx, key, value) =
                ctx.PreludeRenders
                |> Operation.addOrReplace key value
                
        let tryGet ctx key =
            ctx.PreludeRenders
            |> Operation.tryGet key
        let addOrReplaceChoice ctx key value =
            ctx.PreludeRenders
            |> Operation.addOrReplace key value
        let inline addOrReplace ctx key value =
            ((^T or SRTPHelper): (static member Add: GeneratorContext * ResolvedType * ^T -> unit) ctx, key, value)
