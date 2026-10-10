module ProjectionOperations.Fields

open Fable.Core

[<RequireQualifiedAccess>]
type ValueOptional =
    | Text of string
    | Undefined

type Value =
    {
        Proto: string
        Constructor: string
        Optional: (ValueOptional) option
    }

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

let private encodeValueOptional (value: ValueOptional) : obj =
    match value with
    | ValueOptional.Text value -> stringValue value
    | ValueOptional.Undefined -> undefinedValue ()

let private encodeValue (value: Value) : obj =
    let result = newObject ()
    setField result "__proto__" (stringValue value.Proto)
    setField result "constructor" (stringValue value.Constructor)
    match value.Optional with
    | None -> ()
    | Some field -> setField result "optional" (encodeValueOptional field)
    result

let inspect (receiver: ProjectionLab.Session) (value: Value)  =
    receiver.inspect(unbox (encodeValue value))
