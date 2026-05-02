[<AutoOpen>]
module Xantham.Fable.Types.AutoOpenReaderExtensions

open TypeScript
open Xantham.Fable

type TypeScriptReader with
    member this.CreateXanthamTag(node: Ts.Node) =
        XanthamTag.Create(node, this.checker)
    member this.CreateXanthamTag(typ: Ts.Type) =
        XanthamTag.Create(typ, this.checker)

module XanthamTag =
    let createForNode (ctx: TypeScriptReader) (node: Ts.Node) = ctx.CreateXanthamTag node
    let createForType (ctx: TypeScriptReader) (node: Ts.Type) = ctx.CreateXanthamTag node

