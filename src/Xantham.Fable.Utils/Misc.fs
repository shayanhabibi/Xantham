[<AutoOpen>]
module Xantham.Fable.AutoOpenUtils

open Fable.Core

/// <summary>
/// Apply args to a function. Used for sugar in long pipe chains.
/// <example><code lang="fsharp">
/// fn |> funApply args
/// // Same as
/// fn args
/// 
/// // Example use case
/// (* Some calc *)
/// |> (+) 1
/// |> Array.skip
/// |> funApply arr
/// // Same as
/// (* Some calc *)
/// |> (+) 1
/// |> fun count -> Array.skip count arr
/// </code></example>
/// </summary>
/// <param name="args"></param>
/// <param name="fn"></param>
let inline funApply args fn = fn args

module Utils =
    let traceTo (depth: int) o = JS.console.dir(o, {| depth = depth; colors = true |}) 
    let traceInf o = JS.console.dir(o, {| depth = null; colors = true |})
    let trace o = traceTo 3 o
    
type System.Collections.Generic.List<'T> with
    [<Emit "$0">]
    member inline this.AsArray: 'T array = unbox this
