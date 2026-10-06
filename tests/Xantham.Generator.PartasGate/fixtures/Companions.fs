namespace Partas.Solid.CustomizationAcceptance

open Fable.Core
open Fable.Core.JsInterop

[<Interface>]
type InputProperties = interface end

[<AutoOpen>]
module InputPropertyExtensions =
    type InputProperties with
        [<Erase>]
        member _.value
            with get (): string = jsNative
            and set (value: string) = ()

        [<Erase>]
        member _.title
            with get (): string = jsNative
            and set (value: string) = ()

        [<Erase>]
        member _.stamp: string = jsNative
