namespace Partas.Solid.CustomizationProbe

open Partas.Solid
open Fable.Core
open Partas.Solid.CustomizationAcceptance

[<Erase>]
type CustomInput() =
    interface HtmlElement
    interface InputProperties

    [<SolidTypeComponent>]
    member props.View = input (value = props.value, title = props.title)

module App =
    [<SolidComponent>]
    let view () =
        CustomInput(value = "custom value", title = "custom title")
