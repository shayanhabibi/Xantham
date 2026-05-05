module Xantham.Decoder.IdentifierTests

open Expecto
open Xantham.Decoder

[<Tests>]
let tests =
    testList "Identifier.sanitize" [
        testCase "drops the npm scope marker" <| fun _ ->
            Expect.equal (Identifier.sanitize "@cloudflare") "cloudflare" "@ should be dropped"

        testCase "drops the JS-convention $ prefix" <| fun _ ->
            Expect.equal (Identifier.sanitize "$scope") "scope" "$ should be dropped"

        testCase "leaves valid identifiers untouched" <| fun _ ->
            Expect.equal (Identifier.sanitize "Cloudflare") "Cloudflare" "no change expected"
            Expect.equal (Identifier.sanitize "dynamic-workflows") "dynamic-workflows" "kebab preserved for pascal-case downstream"

        testCase "leaves reserved words untouched (backticks handled downstream)" <| fun _ ->
            Expect.equal (Identifier.sanitize "type") "type" "reserved-word handling deferred to Fantomas"
            Expect.equal (Identifier.sanitize "module") "module" "reserved-word handling deferred to Fantomas"

        testCase "leaves leading-digit names untouched (backticks handled downstream)" <| fun _ ->
            Expect.equal (Identifier.sanitize "2fa") "2fa" "leading-digit handling deferred to Fantomas"

        testCase "leaves structural separators untouched (pascal-case folds them)" <| fun _ ->
            Expect.equal (Identifier.sanitize "foo.bar") "foo.bar" "dot is a pascal-case word boundary"
            Expect.equal (Identifier.sanitize "@scope/foo") "scope/foo" "@ dropped, slash kept for pascal-case folding"

        testCase "drops multiple decorative chars" <| fun _ ->
            Expect.equal (Identifier.sanitize "@$weird") "weird" "both @ and $ dropped"
    ]

[<Tests>]
let pascalCaseTests =
    // Pascal-casing folds `.` and `/` into word boundaries — alongside the
    // pre-existing `-` and `_`. `@cloudflare` and friends round-trip to a
    // clean identifier when sanitize+pascalCase compose.
    testList "Name pascal-case with extended boundaries" [
        testCase "treats `.` as a word boundary" <| fun _ ->
            let actual = Name.pascalCase (Name.create "foo.bar") |> Name.valueOrModified
            Expect.equal actual "FooBar" "expected `.` to be folded"

        testCase "treats `/` as a word boundary" <| fun _ ->
            let actual = Name.pascalCase (Name.create "foo/bar") |> Name.valueOrModified
            Expect.equal actual "FooBar" "expected `/` to be folded"

        testCase "still treats `-` and `_` as boundaries" <| fun _ ->
            let actualKebab = Name.pascalCase (Name.create "dynamic-workflows") |> Name.valueOrModified
            Expect.equal actualKebab "DynamicWorkflows" "kebab regression check"
            let actualSnake = Name.pascalCase (Name.create "dynamic_workflows") |> Name.valueOrModified
            Expect.equal actualSnake "DynamicWorkflows" "snake regression check"
    ]
