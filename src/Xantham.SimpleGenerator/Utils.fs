[<AutoOpen>]
module Xantham.SimpleGenerator.Utils

open System.Collections.Frozen
open System.Collections.Generic

[<AutoOpen>]
module DictionaryExtensions =
    module Dictionary =
        let inline item (key: 'Key) (dict: Dictionary<'Key, 'Value>): 'Value = dict[key]
        let inline tryItem (key: 'Key) (dict: Dictionary<'Key, 'Value>): 'Value voption =
            match dict.TryGetValue key with
            | true, value -> ValueSome value
            | _ -> ValueNone
        let inline tryAdd (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            dict.TryAdd(key, value) |> ignore
        let inline addOrReplace (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            if not <| dict.TryAdd(key, value) then
                dict[key] <- value
        let inline tryAddOrGet (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            match dict.TryGetValue key with
            | true, value -> value
            | _ ->
                dict.Add(key, value)
                value
        let inline tryAddKeyPassthrough (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            tryAdd key value dict
            key
        let inline addOrModify (key: 'Key) (fn: 'Value -> 'Value) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            match dict.TryGetValue key with
            | true, value ->
                dict[key] <- fn value
            | _ -> dict.Add(key, value)
        let inline addOrMutate (key: 'Key) (fn: 'Value -> unit) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            match dict.TryGetValue key with
            | true, value -> fn value
            | _ -> dict.Add(key, value)
        module Flip =
            let inline item dict key = item key dict
            let inline tryItem dict key = tryItem key dict
            let inline tryAdd dict key value = tryAdd key value dict
            let inline addOrReplace dict key value = addOrReplace key value dict
            let inline tryAddOrGet dict key value = tryAddOrGet key value dict
            let inline addOrModify dict key fn value =
                addOrModify key fn value dict
            let inline addOrMutate dict key fn value =
                addOrMutate key fn value dict
            let inline tryAddKeyPassthrough dict value key  = tryAddKeyPassthrough key value dict
 [<AutoOpen>]
module FrozenDictionaryExtensions =
    module FrozenDictionary =
        let inline item (key: 'Key) (dict: FrozenDictionary<'Key, 'Value>): 'Value = dict[key]
        let inline tryItem (key: 'Key) (dict: FrozenDictionary<'Key, 'Value>): 'Value voption =
            match dict.TryGetValue key with
            | true, value -> ValueSome value
            | _ -> ValueNone
        module Flip =
            let inline item dict key = item key dict
            let inline tryItem dict key = tryItem key dict
            
[<AutoOpen>]
module SetExtensions =
    module Set =
        let inline tryIntersectMany (sets: Set<'T> seq) =
            if sets |> Seq.isEmpty
            then Set.empty
            else Set.intersectMany sets
        let inline tryUnionMany (sets: Set<'T> seq) =
            if sets |> Seq.isEmpty
            then Set.empty
            else Set.unionMany sets

[<AutoOpen>]
module ArrayExtensions =
    module Array =
        /// <summary>
        /// Supples the value to an array of functions.
        /// </summary>
        let inline mapApply (value: 'T) (array: ('T -> 'U)[]) =
            array
            |> Array.map (fun fn -> fn value)

[<AutoOpen>]
module ValueOptionExtensions =
    module ValueOption =
        /// <summary>
        /// Supples the value to a <c>voption</c> function.
        /// </summary>
        let inline mapApply (value: 'T) (valueOption: ('T -> 'U) voption) =
            valueOption |> ValueOption.map (fun fn -> fn value)

[<AutoOpen>]
module HelperExtensions =
    /// <summary>
    /// Supples the value to the given function.
    /// Used in long pipe chains.
    /// </summary>
    let inline funApply (value: 'T) (fn: 'T -> 'U) = fn value

[<AutoOpen>]
module StackExtensions =
    module Stack =
        let inline tryPeek (stack: Stack<'T>) =
            match stack.TryPeek() with
            | true, value -> ValueSome value
            | _ -> ValueNone
        
        let inline tryPop (stack: Stack<'T>) =
            match stack.TryPop() with
            | true, value -> ValueSome value
            | _ -> ValueNone
        
        let inline push (value: 'T) (stack: Stack<'T>) = stack.Push value
        
        let inline pop (stack: Stack<'T>) = stack.Pop()
        
        let inline popAndForget (stack: Stack<'T>) = stack.Pop() |> ignore