module ProjectionOperations.Thinking

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | Auto
    | Manual
    | Null
    | Number of float
    | Undefined

[<Emit("typeof $0 === 'string'")>]
let private isString (_: string) : bool = jsNative

let private stringValue (value: string) : obj =
    if isString value then box value
    else invalidArg "value" "The operation requires a JavaScript string"

[<Emit("undefined")>]
let private undefinedValue () : obj = jsNative

[<Emit("({})")>]
let private newObject () : obj = jsNative

[<Emit("Object.defineProperty($0, $1, {value: $2, enumerable: true, writable: true, configurable: true})")>]
let private setField (_: obj) (_: string) (_: obj) : unit = jsNative

[<Emit("({[$0]: $1})")>]
let private fieldArgument (_: string) (_: obj) : obj = jsNative

let private encodeValue (value: Value) : obj =
    match value with
    | Value.Auto -> box "auto"
    | Value.Manual -> box "manual"
    | Value.Null -> null
    | Value.Number value -> box value
    | Value.Undefined -> undefinedValue ()

let configure (receiver: ProjectionLab.Session) (value: Value option) argument1 =
    receiver.configure(unbox (match value with None -> newObject () | Some field -> fieldArgument "thinkingLevel" (encodeValue field)), argument1)
