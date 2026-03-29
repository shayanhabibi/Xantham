[<AutoOpen>]
module Xantham.Decoder.FrozenExtensions

open System.Collections.Frozen

type FrozenDictionary<'TKey, 'TValue> with
    member this.TryFind key =
        match this.TryGetValue key with
        | true, value -> Some value
        | _ -> None