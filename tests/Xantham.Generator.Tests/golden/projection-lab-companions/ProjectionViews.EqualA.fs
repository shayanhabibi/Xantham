module ProjectionViews.EqualA

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | Auto
    | Manual

[<Emit("$0 === $1")>]
let private equalsLiteral (_: obj) (_: string) : bool = jsNative

let encode (value: Value) : obj =
    match value with
    | Value.Auto -> box "auto"
    | Value.Manual -> box "manual"

let decode (value: obj) : Microsoft.FSharp.Core.Result<Value, string> =
    if equalsLiteral value "auto" then Ok Value.Auto
    elif equalsLiteral value "manual" then Ok Value.Manual
    else Error "Value is outside the declared union"

let (|Decoded|Invalid|) (value: obj) =
    match decode value with
    | Ok typed -> Decoded typed
    | Error diagnostic -> Invalid diagnostic
