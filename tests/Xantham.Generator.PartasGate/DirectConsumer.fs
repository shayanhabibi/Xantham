module Xantham.CustomizationDirect.Consumer

open Fable.Core
open Fable.Core.JsInterop
open Partas.Solid.CustomizationAcceptance

[<Emit("({value: 'before', stamp: 'readonly', title: 'title', disabled: false, enabled: false, 'aria-label': 'label', 'quote\\\"key': 'quoted'})")>]
let makeObject () : InputProperties = jsNative

let private require expected actual =
    if expected <> actual then
        failwithf "Expected %s, got %s" expected actual

let check () =
    let props = makeObject ()
    let odd = unbox<OddKeysProperties> props
    require "label" odd.``aria-label``
    require "quoted" odd.``quote"key``
    props.value <- "after"
    odd.``aria-label`` <- "new label"
    odd.``quote"key`` <- "new quoted"
    require "after" props.value
    require "readonly" props.stamp
    require "title" props.title
    require "new label" odd.``aria-label``
    require "new quoted" odd.``quote"key``
    let ordinary = unbox<CustomizationLab.Input> props

    if ordinary.disabled then
        failwith "Expected false interop getter"

    ordinary.disabled <- true

    if not ordinary.disabled || props.disabled then
        failwith "Replacement must access enabled independently"

check ()
