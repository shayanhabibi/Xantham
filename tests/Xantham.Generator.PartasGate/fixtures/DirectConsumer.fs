module Xantham.CustomizationDirect.Consumer

open Fable.Core
open Fable.Core.JsInterop
open Xantham.CustomizationDirect

[<Emit("({value: 'before', stamp: 'readonly', title: 'title', 'aria-label': 'label', 'quote\\\"key': 'quoted'})")>]
let makeObject () : InputProperties = jsNative

let private require label expected actual =
    if expected <> actual then
        failwithf "%s: expected %s, got %s" label expected actual

let check () =
    let props = makeObject ()
    require "initial aria key" "label" props.``aria-label``
    require "initial quote key" "quoted" props.``quote"key``
    props.value <- "after"
    props.``aria-label`` <- "new label"
    props.``quote"key`` <- "new quoted"
    require "mutable" "after" props.value
    require "readonly" "readonly" props.stamp
    require "inherited" "title" props.title
    require "aria key" "new label" props.``aria-label``
    require "quote key" "new quoted" props.``quote"key``

check ()
