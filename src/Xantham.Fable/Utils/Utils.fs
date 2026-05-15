[<AutoOpen>]
module Xantham.Fable.Utils

open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Fable.FSharp.Logf
open Fable.Microsoft.Extensions.Logging

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

type System.Collections.Generic.List<'T> with
    [<Emit "$0">]
    member inline this.AsArray: 'T array = unbox this

module Log =
    open Glutinum.Chalk

    let inline emit text = JS.console.error text

    let success (text: string) = chalk.greenBright.Invoke text |> emit
    let log (text: string) = text |> emit
    let info (text: string) = chalk.blueBright.Invoke text |> emit
    let warn (text: string) = chalk.yellowBright.Invoke text |> emit
    let error (text: string) = chalk.redBright.Invoke text |> emit
    let debug (text: string) = chalk.gray.Invoke text |> emit
    
    let traceTo (depth: int) o = JS.console.dir(o, {| depth = depth; colors = true |}) 
    let traceInf o = JS.console.dir(o, {| depth = null; colors = true |})
    let trace o = traceTo 3 o
