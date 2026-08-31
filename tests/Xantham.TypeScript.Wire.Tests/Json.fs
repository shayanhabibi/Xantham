module Xantham.TypeScript.Wire.Tests.Json

open System.Text.Json
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private write (value: 'T) = JsonSerializer.Serialize<'T>(value, ProtoJson.options)

let private read<'T> (json: string) = JsonSerializer.Deserialize<'T>(json, ProtoJson.options)

/// A symbol response with every optional field absent, as the server sends one for a symbol
/// with no parent and no declarations.
let private symbol = """{"id":7,"project":"p","name":"yy","flags":4,"checkFlags":0}"""

let private snapshotWith openProjects : UpdateSnapshotParams =
    { OpenProjects = openProjects
      CloseProjects = ValueNone
      FileChanges = ValueNone
      OpenFiles = ValueNone
      CloseFiles = ValueNone }

[<Tests>]
let jsonTests =
    testList "wire json" [

        // The load-bearing property of the whole generated surface. The server distinguishes an
        // absent field from an explicit null, so a ValueNone that serialised as `null` would send
        // "close every project" where the caller meant "leave projects alone".
        testCase "ValueNone is omitted, not written as null" <| fun _ ->
            snapshotWith (ValueSome [| DocumentIdentifier.FileName "C:/x/tsconfig.json" |])
            |> write
            |> Flip.Expect.equal "only the field that was set" """{"openProjects":["C:/x/tsconfig.json"]}"""

        testCase "an all-ValueNone record is an empty object" <| fun _ ->
            snapshotWith ValueNone |> write |> Flip.Expect.equal "" "{}"

        testList "DocumentIdentifier" [
            // The schema's one structural union: `string | { uri: string }`. Neither arm follows
            // from the F# union's shape, so both directions go through a hand-written converter.
            let inline (==>) identifier json = identifier, json

            testTheory "writes the arm the server expects" [
                DocumentIdentifier.FileName "a.ts" ==> "\"a.ts\""
                DocumentIdentifier.Uri "file:///a.ts" ==> """{"uri":"file:///a.ts"}"""
            ] <| fun (identifier, expected) -> write identifier |> Flip.Expect.equal "" expected

            testTheory "reads either arm back" [
                DocumentIdentifier.FileName "a.ts" ==> "\"a.ts\""
                DocumentIdentifier.Uri "file:///a.ts" ==> """{"uri":"file:///a.ts"}"""
            ] <| fun (expected, json) -> read<DocumentIdentifier> json |> Flip.Expect.equal "" expected
        ]

        testList "reading optionals" [
            // Absent and null both mean "no value" on the way in, even though only absent is
            // legal on the way out.
            let inline (==>) json expected = json, expected

            testTheory "absent and null both read as ValueNone" [
                symbol ==> ValueNone
                """{"id":7,"project":"p","name":"yy","flags":4,"checkFlags":0,"parent":null}""" ==> ValueNone
                """{"id":7,"project":"p","name":"yy","flags":4,"checkFlags":0,"parent":3}""" ==> ValueSome 3
            ] <| fun (json, expected) -> read<SymbolResponse>(json).Parent |> Flip.Expect.equal "" expected
        ]

        // Records have no parameterless constructor, so this fails outright if System.Text.Json
        // cannot bind a payload to the all-args constructor by JsonPropertyName.
        testCase "a record deserialises through its constructor" <| fun _ ->
            let response = read<SymbolResponse> symbol
            response.Name |> Flip.Expect.equal "name" "yy"
            response.Id |> Flip.Expect.equal "id" 7
            response.Declarations |> Flip.Expect.equal "declarations" ValueNone

        // `end` and `file` are F# keywords, so the generator escapes them. The escaping must not
        // leak into the wire name.
        testCase "keyword-named fields keep their wire names" <| fun _ ->
            let diagnostic =
                read<DiagnosticResponse> """{"pos":1,"end":9,"code":2322,"category":1,"text":"t"}"""

            diagnostic.End |> Flip.Expect.equal "end" 9

            { Snapshot = 1; Project = "p"; File = DocumentIdentifier.FileName "a.ts"; Position = 4 }
            |> write
            |> Flip.Expect.equal "file" """{"snapshot":1,"project":"p","file":"a.ts","position":4}"""

        testList "BatchRequest.Params" [
            // The field is raw UTF-8 JSON rather than a JsonNode DOM, so any parameter record
            // encodes into it through the same `serialize` the single-shot calls use.
            let symbolParams: GetSymbolAtPositionParams =
                { Snapshot = 1; Project = "p"; File = DocumentIdentifier.FileName "a.ts"; Position = 4 }

            testCase "any parameter record encodes into a batch entry" <| fun _ ->
                { Requests = ValueSome [| ProtoJson.batchEntry Method.GetSymbolAtPosition symbolParams |] }
                |> write
                |> Flip.Expect.equal ""
                    """{"requests":[{"method":"getSymbolAtPosition","params":{"snapshot":1,"project":"p","file":"a.ts","position":4}}]}"""

            // The entry is byte-identical to what the single-shot path would have sent.
            testCase "the payload matches the single-shot encoding" <| fun _ ->
                (ProtoJson.batchEntry Method.GetSymbolAtPosition symbolParams).Params
                |> Flip.Expect.equal "" (ProtoJson.serialize symbolParams)

            // Null is still the absent value, exactly as it was for the JsonNode it replaced.
            testCase "a parameterless entry omits the payload" <| fun _ ->
                { Requests = ValueSome [| ProtoJson.batchEntryNoParams Method.GetVoidType |] }
                |> write
                |> Flip.Expect.equal "" """{"requests":[{"method":"getVoidType"}]}"""

            testCase "the payload reads back as the bytes that were written" <| fun _ ->
                read<BatchRequest>("""{"method":"getSymbolAtPosition","params":{"snapshot":1,"position":4}}""").Params
                |> System.Text.Encoding.UTF8.GetString
                |> Flip.Expect.equal "" """{"snapshot":1,"position":4}"""
        ]

        testCase "method names are the wire names" <| fun _ ->
            Method.GetSymbolAtPosition |> Flip.Expect.equal "" "getSymbolAtPosition"
            Method.UpdateSnapshot |> Flip.Expect.equal "" "updateSnapshot"
    ]
