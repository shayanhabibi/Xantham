[<AutoOpen>]
module Xantham.Generator.Utils

open System.Collections.Generic

[<AutoOpen>]
module DictionaryExtensions =
    module Dictionary =
        let inline tryItem (key: 'Key) (dict: Dictionary<'Key, 'Value>): 'Value voption =
            match dict.TryGetValue key with
            | true, value -> ValueSome value
            | _ -> ValueNone
        let inline tryAdd (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            dict.TryAdd(key, value) |> ignore
        let inline addOrReplace (key: 'Key) (value: 'Value) (dict: Dictionary<'Key, 'Value>) =
            dict[key] <- value

[<AutoOpen>]
module HelperExtensions =
    /// <summary>
    /// Supples the value to the given function.
    /// Used in long pipe chains.
    /// </summary>
    let inline funApply (value: 'T) (fn: 'T -> 'U) = fn value