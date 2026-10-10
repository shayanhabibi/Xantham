module ProjectionViews.Choice

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | Auto
    | Manual
    | Number of float
    | Null
    | Undefined

[<Emit("$0 === $1")>]
let private equalsLiteral (_: obj) (_: string) : bool = jsNative

[<Emit("typeof $0 === 'number'")>]
let private isNumber (_: obj) : bool = jsNative

[<Emit("$0")>]
let private numberAfterCheck (_: obj) : float = jsNative

[<Emit("$0 === null")>]
let private isNull (_: obj) : bool = jsNative

[<Emit("$0 === undefined")>]
let private isUndefined (_: obj) : bool = jsNative

[<Emit("undefined")>]
let private undefinedValue () : obj = jsNative

let encode (value: Value) : obj =
    match value with
    | Value.Auto -> box "auto"
    | Value.Manual -> box "manual"
    | Value.Number number -> box number
    | Value.Null -> null
    | Value.Undefined -> undefinedValue ()

let decode (value: obj) : Microsoft.FSharp.Core.Result<Value, string> =
    if equalsLiteral value "auto" then Ok Value.Auto
    elif equalsLiteral value "manual" then Ok Value.Manual
    elif isNumber value then Ok (Value.Number (numberAfterCheck value))
    elif isNull value then Ok Value.Null
    elif isUndefined value then Ok Value.Undefined
    else Error "Value is outside the declared union"

let (|Decoded|Invalid|) (value: obj) =
    match decode value with
    | Ok typed -> Decoded typed
    | Error diagnostic -> Invalid diagnostic
