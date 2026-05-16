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

        // Destructured TS parameters like `fn({type, payload}: T)` come into
        // the encoder with the literal destructure text as the parameter
        // name. Without stripping, this becomes a multi-line backtick-
        // wrapped F# identifier the parser can't parse. Drop `{`, `}`, `,`,
        // and whitespace/newlines so the result fuses into a single F#-
        // valid token (e.g. "typepayload").
        testCase "strips destructure-pattern braces and commas" <| fun _ ->
            Expect.equal (Identifier.sanitize "{type, payload}") "typepayload" "{}, , and whitespace dropped"

        testCase "strips multi-line destructure patterns" <| fun _ ->
            let multi = "{\n    type,\n    payload,\n  }"
            Expect.equal (Identifier.sanitize multi) "typepayload" "newlines/indent dropped along with structural punctuation"

        testCase "leaves carriage-return-only lines empty rather than corrupt" <| fun _ ->
            // CR-stripping is also part of doc-comment handling; verify CR
            // alone doesn't survive sanitize.
            Expect.equal (Identifier.sanitize "foo\rbar") "foobar" "CR dropped"

        testCase "tab characters are stripped" <| fun _ ->
            Expect.equal (Identifier.sanitize "foo\tbar") "foobar" "tab dropped"

        // TS computed property names like `[Symbol.iterator]` and
        // `[__WORKFLOW_ENTRYPOINT_BRAND]` come through with literal `[` `]`.
        // F# allows backticked member identifiers with brackets but
        // REJECTS bracketed type names (FS0883). Strip in sanitize; the
        // unstripped original survives via `[<CompiledName>]` for
        // round-tripping the JS-side name.
        testCase "strips computed-property-name brackets" <| fun _ ->
            Expect.equal (Identifier.sanitize "[SymbolUnscopables]") "SymbolUnscopables" "brackets dropped"

        testCase "strips brackets in nested-symbol names" <| fun _ ->
            Expect.equal (Identifier.sanitize "[__WORKFLOW_ENTRYPOINT_BRAND]") "__WORKFLOW_ENTRYPOINT_BRAND" "brackets dropped"
    ]

[<Tests>]
let toSafeTests =
    // toSafe is the backtick-free identifier renamer. Renames are recorded
    // by callers via `Name.Modified`, and `[<CompiledName>]` emission picks
    // the original up automatically.
    testList "Identifier.toSafe" [
        testCase "F# keyword gets trailing underscore" <| fun _ ->
            Expect.equal (Identifier.toSafe "fixed") "fixed_" "keyword renamed"
            Expect.equal (Identifier.toSafe "module") "module_" "keyword renamed"
            Expect.equal (Identifier.toSafe "type") "type_" "keyword renamed"

        testCase "reserved future-use words pass through unchanged" <| fun _ ->
            // F# accepts these as identifiers today (mixin/event/method/etc.);
            // renaming them would mass-rename real TS API surface.
            Expect.equal (Identifier.toSafe "mixin") "mixin" "no rename — F# accepts mixin"
            Expect.equal (Identifier.toSafe "method") "method" "no rename — F# accepts method"
            Expect.equal (Identifier.toSafe "event") "event" "no rename — F# accepts event"

        testCase "bare underscore renamed to anon" <| fun _ ->
            // F# parses `_` as wildcard pattern; rejecting it as member name.
            Expect.equal (Identifier.toSafe "_") "anon" "bare _ renamed"

        testCase "leading-digit name gets _ prefix" <| fun _ ->
            Expect.equal (Identifier.toSafe "2fa") "_2fa" "leading-digit prefixed"

        testCase "newline-only input falls back to Newline" <| fun _ ->
            Expect.equal (Identifier.toSafe "\n") "Newline" "special-char fallback"

        testCase "empty input falls back to Anon" <| fun _ ->
            Expect.equal (Identifier.toSafe "") "Anon" "empty fallback"

        testCase "leaves valid identifiers untouched" <| fun _ ->
            Expect.equal (Identifier.toSafe "Cloudflare") "Cloudflare" "no change for valid identifier"
            Expect.equal (Identifier.toSafe "myProperty") "myProperty" "no change for valid identifier"

        testCase "preserves casing boundary characters for downstream folding" <| fun _ ->
            // toSafe is the first step; pascal/camel-case still needs to see
            // `-`, `_`, `.`, `/` to fold them into a single identifier.
            Expect.equal (Identifier.toSafe "dynamic-workflows") "dynamic-workflows" "kebab preserved"
            Expect.equal (Identifier.toSafe "foo.bar") "foo.bar" "dot preserved"
            Expect.equal (Identifier.toSafe "@scope/foo") "scope/foo" "@ dropped, slash kept"
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
