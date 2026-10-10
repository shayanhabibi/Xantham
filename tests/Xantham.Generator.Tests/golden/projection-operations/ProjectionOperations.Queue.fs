module ProjectionOperations.Queue

open Fable.Core

[<RequireQualifiedAccess>]
type Value =
    | FollowUp
    | Steer
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
    | Value.FollowUp -> box "followUp"
    | Value.Steer -> box "steer"
    | Value.Undefined -> undefinedValue ()

let submit (receiver: ProjectionLab.Session) (value: Value option) argument0 =
    receiver.submit(argument0, options = unbox (match value with None -> newObject () | Some field -> fieldArgument "whenBusy" (encodeValue field)))
