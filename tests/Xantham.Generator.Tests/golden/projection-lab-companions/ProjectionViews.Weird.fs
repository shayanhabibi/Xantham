module ProjectionViews.Weird

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | Empty
    | IsTag2
    | Null2
    | Number2
    | Tag
    | Tags2
    | ToString2
    | Undefined2
    | AutoMode
    | AutoMode2
    | QuoteAndSlash
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
    | Value.Empty -> box ""
    | Value.IsTag2 -> box "IsTag"
    | Value.Null2 -> box "Null"
    | Value.Number2 -> box "Number"
    | Value.Tag -> box "Tag"
    | Value.Tags2 -> box "Tags"
    | Value.ToString2 -> box "ToString"
    | Value.Undefined2 -> box "Undefined"
    | Value.AutoMode -> box "auto-mode"
    | Value.AutoMode2 -> box "auto_mode"
    | Value.QuoteAndSlash -> box "quote\u0022and\\slash"
    | Value.Number number -> box number
    | Value.Null -> null
    | Value.Undefined -> undefinedValue ()

let decode (value: obj) : Microsoft.FSharp.Core.Result<Value, string> =
    if equalsLiteral value "" then Ok Value.Empty
    elif equalsLiteral value "IsTag" then Ok Value.IsTag2
    elif equalsLiteral value "Null" then Ok Value.Null2
    elif equalsLiteral value "Number" then Ok Value.Number2
    elif equalsLiteral value "Tag" then Ok Value.Tag
    elif equalsLiteral value "Tags" then Ok Value.Tags2
    elif equalsLiteral value "ToString" then Ok Value.ToString2
    elif equalsLiteral value "Undefined" then Ok Value.Undefined2
    elif equalsLiteral value "auto-mode" then Ok Value.AutoMode
    elif equalsLiteral value "auto_mode" then Ok Value.AutoMode2
    elif equalsLiteral value "quote\u0022and\\slash" then Ok Value.QuoteAndSlash
    elif isNumber value then Ok (Value.Number (numberAfterCheck value))
    elif isNull value then Ok Value.Null
    elif isUndefined value then Ok Value.Undefined
    else Error "Value is outside the declared union"

let (|Decoded|Invalid|) (value: obj) =
    match decode value with
    | Ok typed -> Decoded typed
    | Error diagnostic -> Invalid diagnostic
