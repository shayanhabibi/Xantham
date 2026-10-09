module CatalogCompatibilityTests

open System
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator.CatalogCompatibility

let private identity = TypeScriptPackage("7.1.0-dev.20260902.1", String.replicate 40 "a", 8u)

let private expected =
    { Compiler = "compiler-consumer"
      Generator = "generator-consumer"
      InferenceProfile = "profile"
      Contract = current identity }

let private validateContract schema compiler generator profile contract =
    validate "producer.json" schema expected compiler generator profile contract

let private rejects part action =
    Expect.throwsC action (fun error ->
        Expect.stringContains error.Message "producer.json" "diagnostic identifies the reference"
        Expect.stringContains error.Message part "diagnostic identifies the incompatible component")

let private readText text =
    use document = JsonDocument.Parse(text: string)
    read "producer.json" document.RootElement

let private encoded () =
    let document = JsonObject()
    document["schemaVersion"] <- JsonValue.Create 2
    document["compatibility"] <- write expected.Contract
    document

[<Tests>]
let tests =
    testList "catalog compatibility" [
        testCase "portable contract accepts rebuilt generator and platform binary" <| fun _ ->
            validateContract 2 "compiler-producer" "generator-producer" "profile" (Some expected.Contract)

        testCase "catalog composition requires regenerated source closures and constraints" <| fun _ ->
            rejects "identity version" (fun () ->
                validateContract 2 expected.Compiler expected.Generator "profile"
                    (Some { expected.Contract with IdentityVersion = 1 }))
            rejects "API version" (fun () ->
                validateContract 2 expected.Compiler expected.Generator "profile"
                    (Some { expected.Contract with ApiVersion = 1 }))

        testCase "binary contract requires identical executable fingerprint" <| fun _ ->
            let binary = { expected with Contract = current (Binary 8u) }
            validate "producer.json" 2 binary binary.Compiler "different-generator" "profile" (Some binary.Contract)
            rejects "compiler fingerprint" (fun () ->
                validate "producer.json" 2 binary "different-compiler" binary.Generator "profile" (Some binary.Contract))

        testCase "legacy schema retains compiler and generator guards" <| fun _ ->
            validateContract 1 expected.Compiler expected.Generator "profile" None
            rejects "compiler" (fun () -> validateContract 1 "different" expected.Generator "profile" None)
            rejects "generator" (fun () -> validateContract 1 expected.Compiler "different" "profile" None)
            Expect.isNone (readText "{\"schemaVersion\":1}") "legacy metadata is optional"

        testCase "inference profile remains authenticated in both schemas" <| fun _ ->
            rejects "inference profile" (fun () ->
                validateContract 2 expected.Compiler expected.Generator "different" (Some expected.Contract))
            rejects "inference profile" (fun () ->
                validateContract 1 expected.Compiler expected.Generator "different" None)

        testCase "schema two requires metadata even when binary hashes match" <| fun _ ->
            rejects "compatibility" (fun () -> validateContract 2 expected.Compiler expected.Generator "profile" None)
            rejects "compatibility" (fun () -> readText "{\"schemaVersion\":2}" |> ignore)

        testCase "contract JSON preserves all portable fields" <| fun _ ->
            let document = encoded ()
            Expect.equal (readText (document.ToJsonString())) (Some expected.Contract) "metadata round trips"
            let metadata = document["compatibility"]
            let compiler = metadata["compiler"]
            let kind = compiler["kind"]
            Expect.equal (kind.GetValue<string>()) "typescript-package" "portable format"

        testCase "contract JSON preserves binary protocol" <| fun _ ->
            let document = encoded ()
            let binary = current (Binary 8u)
            document["compatibility"] <- write binary
            Expect.equal (readText (document.ToJsonString())) (Some binary) "binary metadata round trips"

        for name, change in [
            "contract version", fun c -> { c with ContractVersion = c.ContractVersion + 1 }
            "identity version", fun c -> { c with IdentityVersion = c.IdentityVersion + 1 }
            "API version", fun c -> { c with ApiVersion = c.ApiVersion + 1 }
            "inference version", fun c -> { c with InferenceVersion = c.InferenceVersion + 1 }
            "customization version", fun c -> { c with CustomizationVersion = c.CustomizationVersion + 1 }
            "compiler version", fun c -> { c with Compiler = TypeScriptPackage("7.2.0", String.replicate 40 "a", 8u) }
            "compiler revision", fun c -> { c with Compiler = TypeScriptPackage("7.1.0-dev.20260902.1", String.replicate 40 "b", 8u) }
            "AST protocol", fun c -> { c with Compiler = TypeScriptPackage("7.1.0-dev.20260902.1", String.replicate 40 "a", 9u) }
            "compiler identity kind", fun c -> { c with Compiler = Binary 8u }
        ] do
            testCase ("rejects incompatible " + name) <| fun _ ->
                rejects name (fun () ->
                    validateContract 2 expected.Compiler expected.Generator "profile" (Some(change expected.Contract)))

        for name, text in [
            "unsupported schema", "{\"schemaVersion\":3}"
            "schema", "{\"schemaVersion\":\"2\"}"
            "schema", "{\"schemaVersion\":1,\"schemaVersion\":2}"
            "compatibility", "{\"schemaVersion\":2,\"compatibility\":null}"
            "compatibility", "{\"schemaVersion\":2,\"compatibility\":{},\"compatibility\":{}}"
        ] do
            testCase ("rejects malformed root " + text) <| fun _ ->
                rejects name (fun () -> readText text |> ignore)

        for field, value in [
            "contractVersion", "null"
            "identityVersion", "0"
            "apiVersion", "\"1\""
            "inferenceVersion", "-1"
            "customizationVersion", "1.5"
        ] do
            testCase ("rejects malformed " + field) <| fun _ ->
                let document = encoded ()
                document["compatibility"][field] <- JsonNode.Parse value
                rejects field (fun () -> readText (document.ToJsonString()) |> ignore)

        for field, value in [
            "kind", "\"unknown\""
            "version", "\"\""
            "revision", "\"bad\""
            "astProtocolVersion", "-1"
        ] do
            testCase ("rejects malformed compiler " + field) <| fun _ ->
                let document = encoded ()
                let metadata = document["compatibility"]
                let compiler = metadata["compiler"]
                compiler[field] <- JsonNode.Parse value
                rejects field (fun () -> readText (document.ToJsonString()) |> ignore)

        testCase "duplicate contract and compiler fields cannot choose policy" <| fun _ ->
            let text = (encoded ()).ToJsonString()
            rejects "contractVersion" (fun () ->
                readText (text.Replace("\"contractVersion\":1", "\"contractVersion\":1,\"contractVersion\":2")) |> ignore)
            rejects "kind" (fun () ->
                readText (text.Replace("\"kind\":\"typescript-package\"", "\"kind\":\"binary\",\"kind\":\"typescript-package\"")) |> ignore)

        testCase "revision encoding normalizes hexadecimal case" <| fun _ ->
            let document = encoded ()
            let metadata = document["compatibility"]
            let compiler = metadata["compiler"]
            compiler["revision"] <- JsonValue.Create(String.replicate 40 "A")
            Expect.equal (readText (document.ToJsonString())) (Some expected.Contract) "case does not change source revision"
    ]
