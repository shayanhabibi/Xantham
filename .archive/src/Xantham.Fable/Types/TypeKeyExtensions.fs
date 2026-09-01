[<AutoOpen>]
module Xantham.Fable.Types.TypeKeyExtensions

open Xantham
open TypeScript
open Fable.Core

module TypeKey =
    /// <summary>
    /// A counter for generating unique TypeKeys.
    /// </summary>
    let mutable private keyNum = - 1
    /// <summary>
    /// Performs a thunk with the <c>keyNum</c> counter, returns the result, and decrements the counter.
    /// </summary>
    /// <param name="thunk"></param>
    let inline private withKeyNumThenDecr (thunk: int -> 'T) =
        let key = thunk keyNum
        keyNum <- keyNum - 1
        key
    let createWith = TypeKey
    /// <summary>
    /// Creates a unique generated typekey.
    /// </summary>
    let create() = withKeyNumThenDecr TypeKey
