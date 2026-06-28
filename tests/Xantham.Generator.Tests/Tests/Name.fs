module Xantham.Generator.Tests.Tests.Name

open Expecto
open Xantham.Decoder

// Shorthand helpers matching the DU constructors
let private modified original modified_ = Name.Modified(original, modified_)
let private modifiedCase<[<Measure>] 'U> original modified: Name<'U> = Name.Modified(original, modified) |> Case.addMeasure
let private source original = Name.Source original
let private sourceCase<[<Measure>] 'U> original: Name<'U> = Name.Source original |> Case.addMeasure

[<Tests>]
let caseTests =
    testList "Name.Case" [
        let inline (==>) a b = a, modifiedCase a b
        let inline (<=>) a b = a, sourceCase b
        testTheory "Case.pascal" [
            "``pascal_case``" ==> "PascalCase"
            "pascal_case" ==> "PascalCase"
            "pascal-case" ==> "PascalCase"
            "pascalCase" ==> "PascalCase"
            "PASCAL_CASE" <=> "PASCAL_CASE" // upper snake case is preserved
            "_pascal_case" ==> "_pascalCase"
        ] <| fun (original, expected) ->
            Name.Pascal.create original |> Flip.Expect.equal "" expected
        testTheory "Case.camel" [
            "``camel_case``" ==> "camelCase"
            "camel_case" ==> "camelCase"
            "camel-case" ==> "camelCase"
            "camelCase" <=> "camelCase"
            "CAMEL_CASE" <=> "CAMEL_CASE" // upper snake case is preserved
            "_camel_case" ==> "_camelCase"
            "_Camel_Case" ==> "_camelCase"
        ] <| fun (original, expected) ->
            Name.Camel.create original |> Flip.Expect.equal "" expected
        testTheory "Case.module" [
            "``module_name``" ==> "ModuleName"
            "ModuleName" ==> "IModuleName"
            "module_name" ==> "IModuleName"
            "MODULE_NAME" ==> "IMODULE_NAME" // upper snake case is preserved
            "module-name" ==> "IModuleName"
            "_moduleName" ==> "IModuleName"
        ] <| fun (original, expected) ->
            Name.Module.create original |> Flip.Expect.equal "" expected
        testTheory "Case.typar" [
            "``typar``" ==> "'typar"
            "Typar" ==> "'Typar"
            "t" ==> "'t"
            "_T" ==> "'_T"
        ] <| fun (original, expected) ->
            Name.Typar.create original |> Flip.Expect.equal "" expected
    ]

[<Tests>]
let identifierSanitizationTests =
    // Regression: destructured object-binding parameter names (e.g. `{ type, payload }`)
    // arrive as multiline source text. They must not survive into the modified name as
    // raw braces/newlines/commas, which are illegal even inside double-backticks and
    // produce un-parseable F#. Name.create runs the normalization pipeline.
    let stripBackticks (s: string) =
        if s.StartsWith "``" && s.EndsWith "``" then s.Substring(2, s.Length - 4) else s
    let hasIllegal (s: string) =
        s |> Seq.exists (fun c -> c = '{' || c = '}' || c = ',' || c = '\n' || c = '\r' || c = '`')
    testList "Name.create identifier sanitization" [
        testTheory "strips characters illegal even in backticks" [
            "{ type, payload }"
            "{\n    type,\n    payload\n  }"
            "{ a, b, c }"
        ] <| fun source ->
            (Name.create source).ValueOrModified
            |> hasIllegal
            |> Flip.Expect.isFalse "sanitized name still contains an illegal char"

        testTheory "produces the expected identifier body" [
            "{ type, payload }", "typepayload"
            "{ a, b, c }", "abc"
        ] <| fun (source, expected) ->
            (Name.create source).ValueOrModified
            |> stripBackticks
            |> Flip.Expect.equal "sanitized identifier body" expected

        testCase "empty-after-strip falls back to a token" <| fun () ->
            let modified = (Name.create "{\n}").ValueOrModified
            Expect.isFalse (System.String.IsNullOrWhiteSpace modified) "must not be blank"
            modified |> hasIllegal |> Flip.Expect.isFalse "fallback still has illegal chars"

        testTheory "leaves already-valid names untouched" [
            "foo"; "fooBar"; "_x"; "Name123"
        ] <| fun source ->
            (Name.create source).ValueOrModified |> Flip.Expect.equal "valid name unchanged" source
    ]

[<Tests>]
let sourceCaseTests =
    testList "Name.Case.sourceMap" [
        testTheory "Case.pascal" [
            let inline (==>) a b = Name.Pascal.fromCase a, b 
            modifiedCase "ab" "ab" ==> modifiedCase "ab" "Ab"
            modifiedCase "ba" "ab" ==> modifiedCase "ba" "Ba"
        ] <| fun (a, b) ->
            (a,b)
            ||> Flip.Expect.equal ""
        testTheory "Case.camel" [
            let inline (==>) a b = Name.Camel.fromCase a, b 
            modifiedCase "Bb" "Aa" ==> modifiedCase "Bb" "bb"
            modifiedCase "a" "a" ==> sourceCase "a"
            modifiedCase "A" "a" ==> sourceCase "A" // counts as UPPER_SNAKE_CASE
        ] <| fun (actual, expected) ->
            Flip.Expect.equal "" expected actual
    ]