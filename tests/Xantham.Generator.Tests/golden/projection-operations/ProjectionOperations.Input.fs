module ProjectionOperations.Input

open Fable.Core

type ValueItemsItemImage =
    {
        Data: string
        MimeType: string
    }

[<RequireQualifiedAccess>]
type ValueItemsItemTextTextSignature =
    | Text of string
    | Undefined

type ValueItemsItemText =
    {
        Text: string
        TextSignature: (ValueItemsItemTextTextSignature) option
    }

[<RequireQualifiedAccess>]
type ValueItemsItem =
    | Image of ValueItemsItemImage
    | Text of ValueItemsItemText

[<RequireQualifiedAccess>]
type Value =
    | Items of (ValueItemsItem) array
    | Text of string

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

let private encodeValueItemsItemImage (value: ValueItemsItemImage) : obj =
    let result = newObject ()
    setField result "data" (stringValue value.Data)
    setField result "mimeType" (stringValue value.MimeType)
    setField result "type" (box "image")
    result

let private encodeValueItemsItemTextTextSignature (value: ValueItemsItemTextTextSignature) : obj =
    match value with
    | ValueItemsItemTextTextSignature.Text value -> stringValue value
    | ValueItemsItemTextTextSignature.Undefined -> undefinedValue ()

let private encodeValueItemsItemText (value: ValueItemsItemText) : obj =
    let result = newObject ()
    setField result "text" (stringValue value.Text)
    match value.TextSignature with
    | None -> ()
    | Some field -> setField result "textSignature" (encodeValueItemsItemTextTextSignature field)
    setField result "type" (box "text")
    result

let private encodeValueItemsItem (value: ValueItemsItem) : obj =
    match value with
    | ValueItemsItem.Image value -> encodeValueItemsItemImage value
    | ValueItemsItem.Text value -> encodeValueItemsItemText value

let private encodeValue (value: Value) : obj =
    match value with
    | Value.Items value -> (fun values -> values |> Array.map encodeValueItemsItem |> box) value
    | Value.Text value -> stringValue value

let submit (receiver: ProjectionLab.Session) (value: Value) argument1 =
    receiver.submit(unbox (encodeValue value), ?options = argument1)
