module Xantham.Generator.RunGate.Projections

open Fable.Core.JsInterop

module Choice = ProjectionViews.Choice
module EqualA = ProjectionViews.EqualA
module EqualB = ProjectionViews.EqualB
module OverlapC = ProjectionViews.OverlapC
module Weird = ProjectionViews.Weird
module Input = ProjectionOperations.Input
module Thinking = ProjectionOperations.Thinking
module Queue = ProjectionOperations.Queue
module Fields = ProjectionOperations.Fields
module Shared = ProjectionOperations.Shared
module SharedNext = ProjectionOperations.SharedNext

type private Branch =
    | Left of EqualA.Value
    | Right of EqualB.Value

let private classify value =
    match value with
    | Choice.Decoded Choice.Value.Auto -> "auto"
    | Choice.Decoded Choice.Value.Manual -> "manual"
    | Choice.Decoded(Choice.Value.Number _) -> "number"
    | Choice.Decoded Choice.Value.Null -> "null"
    | Choice.Decoded Choice.Value.Undefined -> "undefined"
    | Choice.Invalid _ -> "invalid"

let private allMatches value =
    [
        match EqualA.decode value with
        | Ok _ -> "A"
        | Error _ -> ()
        match EqualB.decode value with
        | Ok _ -> "B"
        | Error _ -> ()
        match OverlapC.decode value with
        | Ok _ -> "C"
        | Error _ -> ()
    ]

let run check =
    let undefined: obj = emitJsExpr () "undefined"

    check
        "projection active patterns match literal cases"
        (classify (box "auto") = "auto" && classify (box "manual") = "manual")

    check
        "projection active patterns distinguish null and undefined"
        (classify null = "null" && classify undefined = "undefined")

    check
        "projection active patterns expose number payloads"
        (match box 42.5 with
         | Choice.Decoded(Choice.Value.Number number) -> number = 42.5
         | _ -> false)

    check
        "projection number membership includes NaN"
        (match box System.Double.NaN with
         | Choice.Decoded(Choice.Value.Number number) -> System.Double.IsNaN number
         | _ -> false)

    check
        "projection active patterns retain invalid diagnostics"
        (match box "outside" with
         | Choice.Invalid message -> not (System.String.IsNullOrWhiteSpace message)
         | _ -> false)

    for invalid in
        [
            box true
            box [||]
            emitJsExpr () "({})"
            emitJsExpr () "1n"
            emitJsExpr () "Symbol('auto')"
        ] do
        check "projection codecs reject values outside the union" (classify invalid = "invalid")

    let negativeZero: obj = emitJsExpr () "-0"

    check
        "projection number roundtrip preserves negative zero"
        (match Choice.decode negativeZero with
         | Ok value -> emitJsExpr (Choice.encode value, negativeZero) "Object.is($0, $1)"
         | Error _ -> false)

    for value in
        [
            Choice.Value.Auto
            Choice.Value.Manual
            Choice.Value.Number 3.5
            Choice.Value.Null
            Choice.Value.Undefined
        ] do
        let raw = Choice.encode value
        let returned = box (ProjectionLab.Exports.echo (unbox raw))
        check "projection codec crosses the raw echo binding" (Choice.decode returned = Ok value)

    check
        "equal named projections encode the same JavaScript primitive"
        (emitJsExpr (EqualA.encode EqualA.Value.Auto, EqualB.encode EqualB.Value.Auto) "$0 === $1")

    check
        "equal-set conversion validates both members"
        (EqualA.encode EqualA.Value.Auto |> EqualB.decode = Ok EqualB.Value.Auto
         && EqualA.encode EqualA.Value.Manual |> EqualB.decode = Ok EqualB.Value.Manual)

    check
        "overlapping-set conversion accepts its shared member"
        (EqualA.encode EqualA.Value.Auto |> OverlapC.decode = Ok OverlapC.Value.Auto)

    check
        "overlapping-set conversion rejects the unshared member"
        (EqualA.encode EqualA.Value.Manual |> OverlapC.decode |> Result.isError)

    let branchName =
        function
        | Left _ -> "left"
        | Right _ -> "right"

    let encodeBranch =
        function
        | Left value -> EqualA.encode value
        | Right value -> EqualB.encode value

    let left, right = Left EqualA.Value.Auto, Right EqualB.Value.Auto
    check "an explicit outer DU preserves branch provenance" (branchName left <> branchName right)

    check
        "encoding an outer DU erases branch provenance"
        (emitJsExpr (encodeBranch left, encodeBranch right) "$0 === $1")

    check "raw membership reports every matching declaration" (allMatches (box "auto") = [ "A"; "B"; "C" ])
    check "raw membership reports equal-set ambiguity" (allMatches (box "manual") = [ "A"; "B" ])

    check
        "raw membership validates distinct and unknown literals"
        (allMatches (box "off") = [ "C" ] && allMatches (box "unknown") = [])

    for literal in
        [
            ""
            "Number"
            "Null"
            "Undefined"
            "Tags"
            "Tag"
            "IsTag"
            "ToString"
            "auto-mode"
            "auto_mode"
            "quote\"and\\slash"
        ] do
        check
            "projection codecs preserve exact strings through naming collisions and escaping"
            (match Weird.decode (box literal) with
             | Ok value -> emitJsExpr (Weird.encode value, literal) "$0 === $1"
             | Error _ -> false)

    check "a Number string remains separate from the number arm" (Weird.decode (box "Number") = Ok Weird.Value.Number2)
    check "a Null string remains separate from null" (Weird.decode (box "Null") = Ok Weird.Value.Null2)

    check
        "an Undefined string remains separate from undefined"
        (Weird.decode (box "Undefined") = Ok Weird.Value.Undefined2)

    let session = ProjectionLab.Exports.session ()
    let send input = Input.submit session input None |> box

    check
        "typed operation sends text through the real method"
        (emitJsExpr (send (Input.Value.Text "hello")) "$0 === 'hello'")

    let text: Input.ValueItemsItemText = { Text = "hello"; TextSignature = None }

    let image: Input.ValueItemsItemImage =
        {
            Data = "AQID"
            MimeType = "image/png"
        }

    let contents =
        Input.Value.Items [| Input.ValueItemsItem.Text text; Input.ValueItemsItem.Image image |]

    let returned = send contents

    check
        "typed operation writes exact text and image tags and payloads"
        (emitJsExpr
            returned
            "$0.length === 2 && $0[0].type === 'text' && $0[0].text === 'hello' && $0[1].type === 'image' && $0[1].data === 'AQID' && $0[1].mimeType === 'image/png'")

    check
        "typed operation omits absent optional properties"
        (emitJsExpr returned "!Object.hasOwn($0[0], 'textSignature')")

    let presentUndefined =
        { text with
            TextSignature = Some Input.ValueItemsItemTextTextSignature.Undefined
        }

    let presentText =
        { text with
            TextSignature = Some(Input.ValueItemsItemTextTextSignature.Text "signature")
        }

    let present =
        send (
            Input.Value.Items
                [|
                    Input.ValueItemsItem.Text presentUndefined
                    Input.ValueItemsItem.Text presentText
                |]
        )

    check
        "typed operation preserves present undefined independently of omission"
        (emitJsExpr
            present
            "Object.hasOwn($0[0], 'textSignature') && $0[0].textSignature === undefined && $0[1].textSignature === 'signature'")

    check
        "typed operation preserves empty content arrays"
        (emitJsExpr (send (Input.Value.Items [||])) "Array.isArray($0) && $0.length === 0")

    let nullStringRejected =
        try
            send (Input.Value.Text null) |> ignore
            false
        with _ ->
            true

    check "typed operation rejects F# null strings before the SDK call" nullStringRejected

    for value, expected in
        [
            Thinking.Value.Auto, "auto"
            Thinking.Value.Manual, "manual"
            Thinking.Value.Number 4., "number"
            Thinking.Value.Null, "null"
            Thinking.Value.Undefined, "undefined"
        ] do
        let returned = Thinking.configure session (Some value) "context" |> box
        check "typed field operation retains the original value kind" (classify returned = expected)

    check
        "typed field operation permits omission"
        (Thinking.configure session None "context" |> box |> classify = "undefined")

    Queue.submit session (Some Queue.Value.Steer) (Fable.Core.U2.Case1 "queue")
    |> ignore

    check
        "typed optional object argument forwards its selected field"
        (emitJsExpr session "$0.options.whenBusy === 'steer'")

    Queue.submit session None (Fable.Core.U2.Case1 "queue") |> ignore

    check
        "typed field operation can omit the selected property"
        (emitJsExpr session "!Object.hasOwn($0.options, 'whenBusy')")

    Queue.submit session (Some Queue.Value.Undefined) (Fable.Core.U2.Case1 "queue")
    |> ignore

    check
        "typed field operation can include an explicitly undefined property"
        (emitJsExpr session "Object.hasOwn($0.options, 'whenBusy') && $0.options.whenBusy === undefined")

    let hostile: Fields.Value =
        {
            Proto = "safe"
            Constructor = "ctor"
            Optional = None
        }

    check
        "typed records preserve __proto__ as an own data property"
        (Fields.inspect session hostile = "__proto__,constructor;safe;ctor;true")

    check
        "typed records retain present undefined and their ordinary prototype"
        (Fields.inspect
            session
            { hostile with
                Optional = Some Fields.ValueOptional.Undefined
            }
            =
            "__proto__,constructor,optional;safe;ctor;true")

    let shared = Shared.Value.Text "shared"
    let first = Shared.submit session shared None |> box
    let second = SharedNext.submit session shared None |> box

    check
        "one declared input contract composes across generated operations"
        (emitJsExpr (first, second) "$0 === 'shared' && $1 === $0")
