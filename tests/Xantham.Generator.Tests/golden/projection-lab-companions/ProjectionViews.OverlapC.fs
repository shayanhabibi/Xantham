module ProjectionViews.OverlapC

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | Auto
    | Off

[<Emit("$0 === $1")>]
let private equalsLiteral (_: obj) (_: string) : bool = jsNative

let encode (value: Value) : obj =
    match value with
    | Value.Auto -> box "auto"
    | Value.Off -> box "off"

let decode (value: obj) : Microsoft.FSharp.Core.Result<Value, string> =
    if equalsLiteral value "auto" then Ok Value.Auto
    elif equalsLiteral value "off" then Ok Value.Off
    else Error "Value is outside the declared union"

let (|Decoded|Invalid|) (value: obj) =
    match decode value with
    | Ok typed -> Decoded typed
    | Error diagnostic -> Invalid diagnostic
