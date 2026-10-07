namespace Xantham.CustomizationDirect

open Fable.Core
open Fable.Core.JsInterop

[<Interface>]
type InputProperties = interface end

[<AutoOpen>]
module Properties =
    type InputProperties with
        member _.value
            with [<Emit("$0[\"value\"]")>] get (): string = jsNative
            and [<Emit("$0[\"value\"] = $1")>] set (value: string) = jsNative

        member _.title
            with [<Emit("$0[\"title\"]")>] get (): string = jsNative
            and [<Emit("$0[\"title\"] = $1")>] set (value: string) = jsNative

        [<Emit("$0[\"stamp\"]")>]
        member _.stamp: string = jsNative

        member _.``aria-label``
            with [<Emit("$0[\"aria-label\"]")>] get (): string = jsNative
            and [<Emit("$0[\"aria-label\"] = $1")>] set (value: string) = jsNative

        member _.``quote"key``
            with [<Emit("$0[\"quote\\\"key\"]")>] get (): string = jsNative
            and [<Emit("$0[\"quote\\\"key\"] = $1")>] set (value: string) = jsNative

type Concrete() =
    interface InputProperties
