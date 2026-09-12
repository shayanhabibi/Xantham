[<AutoOpen>]
module Fable.Core.TSExtensions

open Fable.Core.TS
open Fable.Core.JsInterop


[<AutoOpen>]
type Utils =
    static member inline toJSFunc(fn: FSharpFunc<_, _>) : Function = !!fn

type ConstrainFunction< ^T when (^T or Utils): (static member toJSFunc: ^T -> Function)> = ^T
