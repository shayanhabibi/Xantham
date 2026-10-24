module UnionAliasApplicationConsumer

open Fable.Core.JsInterop
open UnionAliasApplicationLab

// Every constituent of an applied or flattened alias casts in with a single `!^`.
let assign (attrs: Attrs) =
    attrs.mode <- !^Live.Off
    attrs.label <- !^"x"
    attrs.label <- !^(fun () -> "x")
    attrs.size <- Some !^1.0
    attrs.text <- !^"x"
    attrs.text <- !^1.0
    attrs.value <- Some !^"x"
    attrs.pair <- !^true
