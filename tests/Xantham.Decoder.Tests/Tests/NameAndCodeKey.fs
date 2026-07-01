module Xantham.DecoderTests.Tests.NameAndCodeKey

// =====================================================================================
// PASS UNDER TEST: DECODER NAME + CODEKEY passes (pure functions)
//   (1) src/Xantham.Decoder/Types/Name.fs
//       Name.create / Name.Pascal|Camel|Module|Typar.create / Case.* measures /
//       sanitizeIdentifierSource (private, exercised via Name.create) /
//       NormalizeIdentifierBackticks (re-exported via Normalization.normalize).
//   (2) src/Xantham.Decoder/Types/CodedTypeKey.fs (module Xantham.Decoder.CodeKey)
//       TypeCodeKey / ExportCodeKey / CodeKey struct DUs — case tagging + equality.
//
// CONTRACT we assert: every name produced by the pass must be a VALID F# identifier
// (a bare ident, or a body legally wrappable in ``double backticks``). Where the pass
// emits an identifier that does NOT compile, the case is marked ptestCase with the
// root cause; we never assert the buggy output to make a test pass.
// =====================================================================================

open Expecto
open Xantham.Decoder
open Xantham.Decoder.CodeKey
open Xantham

// ------------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------------

/// The (modified, if any; else source) string a Name presents to the renderer.
let private modified (n: Name) = n.ValueOrModified
/// The (modified) string of a measure-tagged Name<_>.
let private modifiedCased (n: Name<'u>) = (Case.withoutMeasure n).ValueOrModified

let private stripBackticks (s: string) =
    if s.StartsWith "``" && s.EndsWith "``" && s.Length >= 4 then s.Substring(2, s.Length - 4) else s

/// True when the (un-backticked) body still contains a char that is illegal even inside
/// F# double-backticks (backtick, newline, carriage-return, tab) — these never compile.
let private hasBacktickIllegalChar (s: string) =
    let body = stripBackticks s
    body |> Seq.exists (fun c -> c = '`' || c = '\n' || c = '\r' || c = '\t')

/// True when the string is a syntactically VALID F# identifier:
///   - non-empty, AND
///   - either a bare identifier (letters/digits/underscore, not leading-digit), OR
///   - wrapped in non-empty double-backticks whose body has no backtick-illegal char.
let private isValidFSharpIdent (s: string) =
    if System.String.IsNullOrEmpty s then false
    elif s.StartsWith "``" && s.EndsWith "``" then
        let body = stripBackticks s
        body.Length > 0 && not (hasBacktickIllegalChar body)
    else
        let okChars = s |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')
        okChars && not (System.Char.IsDigit s.[0])

/// True when a type-parameter rendering ('foo) is a valid F# typar:
/// a leading single-quote followed by a bare-identifier body (typars cannot be
/// backtick-escaped, so the body must already be a legal bare ident).
let private isValidTypar (s: string) =
    s.StartsWith "'" && isValidFSharpIdent (s.Substring 1)

// ====================================================================================
// (1a) Name.create — normalization / sanitization of raw source strings
// ====================================================================================

[<Tests>]
let nameCreateTests =
    testList "Name.create" [
        // Already-valid identifiers pass through verbatim as a Source (unmodified) case.
        testTheory "valid identifiers pass through unmodified" [
            "valid"; "fooBar"; "_x"; "Name123"; "PascalCase"; "_"
        ] <| fun source ->
            match Name.create source with
            | Name.Source s -> Flip.Expect.equal "valid name stays a Source" source s
            | Name.Modified(o, m) -> failtestf "expected Source for %A, got Modified(%A,%A)" source o m

        // Names needing escaping become Modified(original, ``escaped``) and the modified
        // value must be a valid backtick identifier.
        testTheory "names needing escaping become valid backtick identifiers" [
            "scale-down", "``scale-down``"   // hyphen
            "foo.bar",    "``foo.bar``"      // dot (legal inside backticks)
            "a/b",        "``a/b``"          // slash (legal inside backticks)
            "type",       "``type``"         // reserved word
            "global",     "``global``"       // reserved word
            "class",      "``class``"        // reserved word
            "0",          "``0``"            // leading-digit / all-digit
            "0abc",       "``0abc``"         // leading digit
            "1stPlace",   "``1stPlace``"     // leading digit
        ] <| fun (source, expected) ->
            let m = modified (Name.create source)
            Expect.equal m expected "escaped identifier"
            Expect.isTrue (isValidFSharpIdent m) "escaped identifier must be valid F#"

        // sanitizeIdentifierSource: empty / whitespace-only collapses to the "Empty"
        // placeholder (an empty `````` backtick ident is FS3563-invalid).
        testTheory "empty / whitespace source collapses to the Empty placeholder" [
            ""; "   "; "\t"; "\n"
        ] <| fun source ->
            let m = modified (Name.create source)
            Expect.equal m "Empty" "empty/ws collapses to Empty"
            Expect.isTrue (isValidFSharpIdent m) "placeholder must be valid F#"

        // sanitizeIdentifierSource: destructuring/multiline source (braces, commas,
        // newlines — illegal even in backticks) is stripped to a bare ident body.
        testTheory "destructuring source strips chars illegal even in backticks" [
            "{ type, payload }", "typepayload"
            "{ a, b, c }",       "abc"
            "{\n    x,\n    y\n}", "xy"
        ] <| fun (source, expectedBody) ->
            let m = modified (Name.create source)
            Expect.equal (stripBackticks m) expectedBody "stripped body"
            Expect.isFalse (hasBacktickIllegalChar m) "no backtick-illegal chars remain"
            Expect.isTrue (isValidFSharpIdent m) "sanitized name must be valid F#"

        // empty-after-strip falls back to a non-blank token ("arg").
        testCase "empty-after-strip falls back to a non-blank token" <| fun _ ->
            let m = modified (Name.create "{\n}")
            Expect.isFalse (System.String.IsNullOrWhiteSpace m) "must not be blank"
            Expect.isTrue (isValidFSharpIdent m) "fallback must be valid F#"

        // ValueOrSource always preserves the original, even when modified.
        testCase "ValueOrSource preserves the original source" <| fun _ ->
            (Name.create "scale-down").ValueOrSource
            |> Flip.Expect.equal "source preserved" "scale-down"
    ]

// ====================================================================================
// (1b) Name static members & the map family (Source/Modified algebra)
// ====================================================================================

[<Tests>]
let nameStaticAndMapTests =
    testList "Name static + map" [
        testCase "Name.Create(value) is an unmodified Source (no normalization)" <| fun _ ->
            // Static Create does NOT normalize — a reserved word stays raw.
            Name.Create "type" |> Flip.Expect.equal "" (Name.Source "type")
        testCase "Name.Create(original, modified) is a Modified" <| fun _ ->
            Name.Create("o", "m") |> Flip.Expect.equal "" (Name.Modified("o", "m"))
        testCase "CreateModified builds a Modified" <| fun _ ->
            Name.CreateModified("o", "m") |> Flip.Expect.equal "" (Name.Modified("o", "m"))

        // map operates over the CURRENT (modified-or-source) value; collapses to Source
        // when the result equals the original.
        testCase "map over modified changes the modified value" <| fun _ ->
            Name.map (fun _ -> "c") (Name.Modified("a", "b"))
            |> Flip.Expect.equal "" (Name.Modified("a", "c"))
        testCase "map collapses to Source when result equals original" <| fun _ ->
            Name.map (fun _ -> "a") (Name.Modified("a", "b"))
            |> Flip.Expect.equal "" (Name.Source "a")
        testCase "map over a Source that diverges becomes Modified" <| fun _ ->
            Name.map (fun _ -> "c") (Name.Source "a")
            |> Flip.Expect.equal "" (Name.Modified("a", "c"))
        testCase "map identity on Source stays Source" <| fun _ ->
            Name.map id (Name.Source "a") |> Flip.Expect.equal "" (Name.Source "a")

        // mapSource maps the ORIGINAL; result differs => Modified, else Source.
        testCase "mapSource maps the original source" <| fun _ ->
            Name.mapSource (fun _ -> "c") (Name.Modified("a", "b"))
            |> Flip.Expect.equal "" (Name.Modified("a", "c"))
        testCase "mapSource noop collapses to Source" <| fun _ ->
            Name.mapSource id (Name.Modified("a", "b"))
            |> Flip.Expect.equal "" (Name.Source "a")

        // mapModified maps the modified value; on a Source it yields Source (no modified).
        testCase "mapModified maps the modified value" <| fun _ ->
            Name.mapModified (fun _ -> "c") (Name.Modified("a", "b"))
            |> Flip.Expect.equal "" (Name.Modified("a", "c"))
        testCase "mapModified on a Source yields Source" <| fun _ ->
            Name.mapModified (fun _ -> "c") (Name.Source "a")
            |> Flip.Expect.equal "" (Name.Source "a")

        testCase "valueOrSource / valueOrModified accessors" <| fun _ ->
            let n = Name.Modified("orig", "mod")
            Expect.equal (Name.valueOrSource n) "orig" "valueOrSource"
            Expect.equal (Name.valueOrModified n) "mod" "valueOrModified"
    ]

// ====================================================================================
// (1c) Case measures — add/remove/re-tag are pure casts (no string change)
// ====================================================================================

[<Tests>]
let caseMeasureTests =
    testList "Case measure round-trips" [
        testCase "addMeasure then withoutMeasure is identity" <| fun _ ->
            let n = Name.create "foo"
            let tagged : Name<Case.pascal> = Case.addMeasure n
            Case.withoutMeasure tagged |> Flip.Expect.equal "round-trip identity" n
        testCase "unboxMeasure re-tags without changing the underlying string" <| fun _ ->
            let p : Name<Case.pascal> = Case.addPascalMeasure (Name.Modified("a", "b"))
            let c : Name<Case.camel> = Case.unboxMeasure p
            modifiedCased c |> Flip.Expect.equal "string unchanged by re-tag" "b"
        testCase "addPascal/Camel/Module/Typar measure leave the string intact" <| fun _ ->
            let n = Name.Modified("orig", "Mod")
            Expect.equal (modifiedCased (Case.addPascalMeasure n)) "Mod" "pascal"
            Expect.equal (modifiedCased (Case.addCamelMeasure n)) "Mod" "camel"
            Expect.equal (modifiedCased (Case.addModuleMeasure n)) "Mod" "module"
            Expect.equal (modifiedCased (Case.addTyparMeasure n)) "Mod" "typar"
        testCase "Name.Case.valueOrModified / valueOrSource on a tagged name" <| fun _ ->
            let n : Name<Case.pascal> = Case.addMeasure (Name.Modified("orig", "Mod"))
            Expect.equal (Name.Case.valueOrModified n) "Mod" "valueOrModified"
            Expect.equal (Name.Case.valueOrSource n) "orig" "valueOrSource"
        testCase "Name.Case.isModified / isSource reflect the tagged DU case" <| fun _ ->
            let m : Name<Case.pascal> = Case.addMeasure (Name.Modified("a", "b"))
            let s : Name<Case.pascal> = Case.addMeasure (Name.Source "a")
            Expect.isTrue (Name.Case.isModified m) "modified is modified"
            Expect.isTrue (Name.Case.isSource s) "source is source"
    ]

// ====================================================================================
// (1d) CasedName envelope
// ====================================================================================

[<Tests>]
let casedNameTests =
    testList "CasedName.Value" [
        testCase "Pascal envelope drops the measure" <| fun _ ->
            let cn = CasedName.Pascal(Case.addMeasure (Name.create "Foo"))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "Foo"
        testCase "Camel envelope drops the measure" <| fun _ ->
            let cn = CasedName.Camel(Case.addMeasure (Name.Source "foo"))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "foo"
        testCase "Module envelope drops the measure" <| fun _ ->
            let cn = CasedName.Module(Case.addMeasure (Name.Source "IFoo"))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "IFoo"
        testCase "Typar envelope drops the measure" <| fun _ ->
            let cn = CasedName.Typar(Case.addMeasure (Name.Source "'t"))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "'t"
    ]

// ====================================================================================
// (1e) Pascal casing — Name.Pascal.create / pascalCase
// ====================================================================================

[<Tests>]
let pascalTests =
    testList "Name.Pascal.create" [
        testTheory "pascal-cases and keeps a valid identifier" [
            "scale-down", "ScaleDown"
            "my-type",    "MyType"
            "pascal_case","PascalCase"
            "pascalCase", "PascalCase"
            "global",     "Global"      // reserved word -> capitalized, no longer reserved
            "type",       "Type"        // reserved word -> Type (valid bare ident)
            "class",      "Class"
            "_pascal_case","_pascalCase" // leading underscore preserved
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "pascal output"
            Expect.isTrue (isValidFSharpIdent m) "pascal output must be valid F#"

        testCase "UPPER_SNAKE_CASE is preserved verbatim" <| fun _ ->
            modifiedCased (Name.Pascal.create "PASCAL_CASE")
            |> Flip.Expect.equal "" "PASCAL_CASE"

        testCase "empty source pascal-cases to the Empty placeholder" <| fun _ ->
            modifiedCased (Name.Pascal.create "") |> Flip.Expect.equal "" "Empty"

        // BUG (pending): a leading-digit / all-digit source survives pascal-casing as a
        // ``digit`` backtick identifier, but a *type name* "``0``" — while a valid F#
        // identifier — is the kind of un-letter-led name pascal casing is meant to fix.
        // It is at least a VALID identifier here (so we assert validity, not the body).
        testCase "all-digit source pascal output is still a valid identifier" <| fun _ ->
            let m = modifiedCased (Name.Pascal.create "0")
            Expect.isTrue (isValidFSharpIdent m) "must be a valid F# identifier"
    ]

// ====================================================================================
// (1f) Camel casing — Name.Camel.create
// ====================================================================================

[<Tests>]
let camelTests =
    testList "Name.Camel.create" [
        testTheory "camel-cases and keeps a valid identifier" [
            "Scale-Down", "scaleDown"
            "camel_case", "camelCase"
            "camel-case", "camelCase"
            "MyType",     "myType"
            "FooBar",     "fooBar"
            "_Camel_Case","_camelCase"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "camel output"
            Expect.isTrue (isValidFSharpIdent m) "camel output must be valid F#"

        testCase "camelCase that lands on a reserved word is re-backticked" <| fun _ ->
            // "Type" -> "type" (reserved) -> re-normalized to a backtick identifier.
            let m = modifiedCased (Name.Camel.create "Type")
            Expect.equal m "``type``" "reserved camel re-backticked"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testCase "UPPER_SNAKE_CASE is preserved verbatim" <| fun _ ->
            modifiedCased (Name.Camel.create "CAMEL_CASE")
            |> Flip.Expect.equal "" "CAMEL_CASE"
    ]

// ====================================================================================
// (1g) Module naming — Name.Module.create (I-prefixed interface module name)
// ====================================================================================

[<Tests>]
let moduleTests =
    testList "Name.Module.create" [
        testTheory "produces a valid I-prefixed PascalCase module name" [
            "foo",       "IFoo"
            "Foo",       "IFoo"
            "module_name","IModuleName"
            "my-mod",    "IMyMod"
            "type",      "IType"      // reserved word -> IType (no longer reserved)
            "_moduleName","IModuleName"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Module.create source)
            Expect.equal m expected "module name output"
            Expect.isTrue (isValidFSharpIdent m) "module name must be valid F#"

        testCase "UPPER_SNAKE_CASE module name keeps the snake body, I-prefixed" <| fun _ ->
            modifiedCased (Name.Module.create "MODULE_NAME")
            |> Flip.Expect.equal "" "IMODULE_NAME"

        // The backtick-passthrough branch of sourceMapToModuleName: a name whose SOURCE
        // genuinely starts with `` (i.e. cannot be un-escaped) is pascal-cased WITHOUT the
        // I-prefix. (Name.create strips redundant backticks, so we drive the pass directly.)
        // Note: toPascalCase only capitalizes after '-'/'_'/start, NOT after spaces, so the
        // inner space survives — which is fine, a space is legal inside double-backticks.
        testCase "a genuinely backtick-wrapped source is pascal-cased without the I-prefix" <| fun _ ->
            let m =
                Name.sourceMapToModuleName (Name.Source "``my type``")
                |> Case.withoutMeasure
                |> fun n -> n.ValueOrModified
            Expect.equal m "``My type``" "no I-prefix; space preserved inside backticks"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"
    ]

// ====================================================================================
// (1h) Type-parameter naming — Name.Typar.create  (' + body)
// ====================================================================================

[<Tests>]
let typarTests =
    testList "Name.Typar.create" [
        testTheory "prefixes a single quote and yields a valid typar" [
            "t",     "'t"
            "T",     "'T"
            "key",   "'key"
            "``t``", "'t"   // backticks stripped before prefixing
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Typar.create source)
            Expect.equal m expected "typar output"
            Expect.isTrue (isValidTypar m) "typar must be a valid F# type parameter"

        // BUG (pending): Name.Typar.create only strips backticks and prepends `'`
        // (Name.fs:331 `normalizeForTypeParameter = map (stripBackticks >> sprintf "'%s")`).
        // It never sanitizes the body, so a hyphenated source like "key-of" yields
        // "'key-of" — INVALID F# (a typar body may not contain '-', and unlike ordinary
        // identifiers a typar CANNOT be backtick-escaped because of the leading quote;
        // `type Foo<'key-of>` fails with FS0010 "Unexpected symbol '-' in type name").
        // ROOT CAUSE: Name.fs:331/340 apply no identifier sanitization to the typar body.
        // FIX: sanitize the body to a bare identifier (e.g. camelCase + strip illegal
        // chars) BEFORE prefixing the quote, e.g. "key-of" -> "'keyOf".
        testCase "hyphenated typar source must sanitize to a valid bare-ident body" <| fun _ ->
            let m = modifiedCased (Name.Typar.create "key-of")
            Expect.isTrue (isValidTypar m) "typar with a hyphen body must still be valid F#"
            Expect.equal m "'keyOf" "hyphen collapsed to a camelCase typar body"
    ]

// ====================================================================================
// (1i) Normalization.normalize — re-exported NormalizeIdentifierBackticks (+ sanitize)
// ====================================================================================

[<Tests>]
let normalizeTests =
    testList "Name.Normalization.normalize" [
        testTheory "leaves valid identifiers untouched" [
            "foo"; "fooBar"; "_x"; "Name123"
        ] <| fun source ->
            Name.Normalization.normalize source |> Flip.Expect.equal "unchanged" source
        testTheory "wraps invalid identifiers in valid backticks" [
            "scale-down", "``scale-down``"
            "type",       "``type``"
            "0",          "``0``"
        ] <| fun (source, expected) ->
            let n = Name.Normalization.normalize source
            Expect.equal n expected "wrapped"
            Expect.isTrue (isValidFSharpIdent n) "normalized must be valid F#"
        testCase "empty source normalizes to the Empty placeholder (not ``)" <| fun _ ->
            // Without the sanitize prefix, NormalizeIdentifierBackticks "" would yield the
            // FS3563-invalid empty `````` identifier. The pass substitutes "Empty".
            Name.Normalization.normalize "" |> Flip.Expect.equal "" "Empty"
    ]

// ====================================================================================
// (2) CodeKey — TypeCodeKey / ExportCodeKey / CodeKey tagging + equality
//     These are plain struct DUs over TypeKey; coverage = construction, tag extraction,
//     and structural equality (collision behavior: same TypeKey, different case => not
//     equal; same case + same key => equal).
// ====================================================================================

[<Tests>]
let codeKeyTests =
    let k1 : TypeKey = TypeKey.createWith 1
    let k2 : TypeKey = TypeKey.createWith 2
    testList "CodeKey tagging" [
        // ---- TypeCodeKey: every case round-trips its payload key ----
        testCase "TypeCodeKey cases carry their TypeKey payload" <| fun _ ->
            // pattern-extract the key back out of each payload-bearing case
            let extract =
                function
                | TypeCodeKey.GlobalThis -> None
                | TypeCodeKey.Conditional k | TypeCodeKey.Primitive k | TypeCodeKey.EnumCase k
                | TypeCodeKey.Union k | TypeCodeKey.Intersection k | TypeCodeKey.Literal k
                | TypeCodeKey.IndexAccess k | TypeCodeKey.TypeReference k | TypeCodeKey.Array k
                | TypeCodeKey.TypeParameter k | TypeCodeKey.ReadOnly k | TypeCodeKey.Tuple k
                | TypeCodeKey.Index k | TypeCodeKey.Predicate k | TypeCodeKey.TypeLiteral k
                | TypeCodeKey.TemplateLiteral k | TypeCodeKey.Optional k
                | TypeCodeKey.Substitution k | TypeCodeKey.TypeQuery k -> Some k
            let payloadCases =
                [ TypeCodeKey.Conditional k1; TypeCodeKey.Primitive k1; TypeCodeKey.EnumCase k1
                  TypeCodeKey.Union k1; TypeCodeKey.Intersection k1; TypeCodeKey.Literal k1
                  TypeCodeKey.IndexAccess k1; TypeCodeKey.TypeReference k1; TypeCodeKey.Array k1
                  TypeCodeKey.TypeParameter k1; TypeCodeKey.ReadOnly k1; TypeCodeKey.Tuple k1
                  TypeCodeKey.Index k1; TypeCodeKey.Predicate k1; TypeCodeKey.TypeLiteral k1
                  TypeCodeKey.TemplateLiteral k1; TypeCodeKey.Optional k1
                  TypeCodeKey.Substitution k1; TypeCodeKey.TypeQuery k1 ]
            payloadCases
            |> List.iter (fun c -> Expect.equal (extract c) (Some k1) "payload key extracted")
            Expect.equal (extract TypeCodeKey.GlobalThis) None "GlobalThis carries no key"

        testCase "TypeCodeKey: same case + same key are equal" <| fun _ ->
            Expect.equal (TypeCodeKey.Union k1) (TypeCodeKey.Union k1) "structural equality"
        testCase "TypeCodeKey: same case + different key are not equal" <| fun _ ->
            Expect.notEqual (TypeCodeKey.Union k1) (TypeCodeKey.Union k2) "different key"
        testCase "TypeCodeKey: same key + different case are not equal" <| fun _ ->
            // The whole point of tagging: a Union(k) and a Primitive(k) over the SAME
            // TypeKey are distinct keys and must never collide.
            Expect.notEqual (TypeCodeKey.Union k1) (TypeCodeKey.Primitive k1) "case discriminates"

        // ---- ExportCodeKey: every case round-trips its payload key ----
        testCase "ExportCodeKey cases carry their TypeKey payload" <| fun _ ->
            let extract =
                function
                | ExportCodeKey.Variable k | ExportCodeKey.Interface k | ExportCodeKey.TypeAlias k
                | ExportCodeKey.Class k | ExportCodeKey.Enum k | ExportCodeKey.Module k
                | ExportCodeKey.Function k -> k
            [ ExportCodeKey.Variable k1; ExportCodeKey.Interface k1; ExportCodeKey.TypeAlias k1
              ExportCodeKey.Class k1; ExportCodeKey.Enum k1; ExportCodeKey.Module k1
              ExportCodeKey.Function k1 ]
            |> List.iter (fun c -> Expect.equal (extract c) k1 "payload key extracted")

        testCase "ExportCodeKey: case discriminates over the same key" <| fun _ ->
            Expect.notEqual (ExportCodeKey.Class k1) (ExportCodeKey.Interface k1) "case discriminates"

        // ---- CodeKey outer envelope ----
        testCase "CodeKey Export vs Type discriminate over the same key" <| fun _ ->
            let asExport = CodeKey.Export(ExportCodeKey.Class k1)
            let asType = CodeKey.Type(TypeCodeKey.TypeReference k1)
            Expect.notEqual asExport asType "Export and Type never collide"
        testCase "CodeKey Export round-trips its ExportCodeKey" <| fun _ ->
            match CodeKey.Export(ExportCodeKey.Enum k1) with
            | CodeKey.Export(ExportCodeKey.Enum k) -> Expect.equal k k1 "round-trip"
            | other -> failtestf "unexpected %A" other
        testCase "CodeKey Type round-trips its TypeCodeKey" <| fun _ ->
            match CodeKey.Type(TypeCodeKey.Tuple k2) with
            | CodeKey.Type(TypeCodeKey.Tuple k) -> Expect.equal k k2 "round-trip"
            | other -> failtestf "unexpected %A" other
        testCase "CodeKey equality is structural" <| fun _ ->
            Expect.equal
                (CodeKey.Type(TypeCodeKey.Union k1))
                (CodeKey.Type(TypeCodeKey.Union k1))
                "structural equality"
    ]

// ====================================================================================
// APPENDED EXHAUSTIVE CASES (Name + CodeKey passes)
// All outputs below were captured from the live pass and verified to be VALID F#
// identifiers (or, where a case documents a defect, a ptestCase with the root cause).
// ====================================================================================

// ------------------------------------------------------------------------------------
// (1a+) Name.create — exhaustive reserved-word escaping
// Every F# keyword is NOT a legal bare identifier and must come back ``backticked``.
// ------------------------------------------------------------------------------------

[<Tests>]
let nameCreateReservedTests =
    testList "Name.create reserved words" [
        testTheory "every F# keyword is escaped to a valid backtick identifier" [
            "abstract"; "and"; "as"; "base"; "do"; "done"; "downcast"; "else"; "end"
            "exception"; "extern"; "for"; "if"; "in"; "inherit"; "inline"; "lazy"
            "module"; "mutable"; "namespace"; "new"; "null"; "of"; "open"; "or"
            "override"; "rec"; "return"; "static"; "struct"; "then"; "to"; "try"
            "upcast"; "use"; "val"; "void"; "when"; "while"; "yield"
            "let"; "fun"; "match"; "with"; "member"; "type"; "class"; "global"; "true"
        ] <| fun kw ->
            let n = Name.create kw
            let m = modified n
            Expect.equal m (sprintf "``%s``" kw) "keyword wrapped in backticks"
            Expect.isTrue (isValidFSharpIdent m) "escaped keyword must be valid F#"
            // original is always preserved verbatim
            Expect.equal n.ValueOrSource kw "source preserved"

        // Unicode letters are legal in F# identifiers — they pass through unmodified.
        testTheory "unicode-letter identifiers pass through unmodified" [
            "café"; "naïve"; "Ωmega"; "Ωmega2"; "λambda"; "über"
        ] <| fun source ->
            match Name.create source with
            | Name.Source s -> Flip.Expect.equal "unicode ident stays a Source" source s
            | Name.Modified(o, m) -> failtestf "expected Source for %A, got Modified(%A,%A)" source o m

        // A mid-word apostrophe (the F# "prime") is a legal identifier-part char.
        testTheory "mid/trailing-prime identifiers pass through unmodified" [
            "a'b"; "x'"; "foo'bar'"; "T'"
        ] <| fun source ->
            match Name.create source with
            | Name.Source s -> Flip.Expect.equal "prime ident stays a Source" source s
            | Name.Modified(o, m) -> failtestf "expected Source for %A, got Modified(%A,%A)" source o m

        // A LEADING quote is not a legal first char (it begins a typar / char literal),
        // so it is escaped.
        testCase "a leading-quote source is escaped to a backtick identifier" <| fun _ ->
            let m = modified (Name.create "'single")
            Expect.equal m "``'single``" "leading quote escaped"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        // Punctuation / bracket sources that are legal *inside* backticks round-trip
        // through the backtick branch (no sanitize, since none is backtick-illegal).
        testTheory "bracketed / punctuated sources become valid backtick identifiers" [
            "(x)",   "``(x)``"
            "[a]",   "``[a]``"
            "a:b",   "``a:b``"
            "a;b",   "``a;b``"
            "a=b",   "``a=b``"
            "a+b",   "``a+b``"
        ] <| fun (source, expected) ->
            let m = modified (Name.create source)
            Expect.equal m expected "punctuation wrapped"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        // A genuinely backtick-wrapped source: the inner backticks are themselves
        // backtick-illegal, so sanitize strips them (and any space) to a bare body.
        testTheory "an already-backtick-wrapped source is sanitized through the strip path" [
            "``foo``",        "foo"
            "``with space``", "withspace"
        ] <| fun (source, expected) ->
            let m = modified (Name.create source)
            Expect.equal m expected "stripped to bare body"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"
        testCase "a backtick-wrapped reserved word re-escapes to the reserved backtick form" <| fun _ ->
            // ``type`` -> strip -> "type" (reserved) -> NormalizeIdentifierBackticks -> ``type``
            let m = modified (Name.create "``type``")
            Expect.equal m "``type``" "reserved re-escaped"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        // sanitizeIdentifierSource: more destructuring shapes collapse to bare bodies.
        testTheory "destructuring sources strip braces/commas/newlines to a bare body" [
            "{ first, second }",        "firstsecond"
            "{ a }",                    "a"
            "{ x, y, z, }",             "xyz"
            "{\n  alpha,\n  beta\n}",   "alphabeta"
        ] <| fun (source, expectedBody) ->
            let m = modified (Name.create source)
            Expect.equal (stripBackticks m) expectedBody "stripped body"
            Expect.isFalse (hasBacktickIllegalChar m) "no backtick-illegal chars remain"
            Expect.isTrue (isValidFSharpIdent m) "sanitized name must be valid F#"
    ]

// ------------------------------------------------------------------------------------
// (1e+) Pascal casing — exhaustive
// ------------------------------------------------------------------------------------

[<Tests>]
let pascalExhaustiveTests =
    testList "Name.Pascal.create exhaustive" [
        testTheory "separators collapse and the lead segment capitalizes" [
            "scale-down",      "ScaleDown"
            "a-b-c",           "ABC"
            "foo_bar_baz",     "FooBarBaz"
            "mixed-_-sep",     "Mixed_Sep"    // each separator capitalizes the NEXT char only; a lone inner '_' survives
            "café-bar",        "CaféBar"      // unicode survives casing
            "type-x",          "TypeX"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "pascal output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "leading underscores are preserved, then the next char capitalizes" [
            "_foo",  "_foo"     // single leading underscore: body already lower, stays
            "__x",   "_X"       // double underscore collapses, X capitalized
            "_",     "_"        // bare underscore is a valid identifier
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "pascal output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "already-Pascal and UPPER_SNAKE sources are preserved verbatim" [
            "AlreadyPascal", "AlreadyPascal"
            "FooBarBaz",     "FooBarBaz"
            "UPPER_SNAKE",   "UPPER_SNAKE"
            "ABC",           "ABC"
            "MixedUPPER_case","MixedUPPERCase"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "pascal output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "reserved words capitalize into non-reserved bare identifiers" [
            "global",   "Global"
            "internal", "Internal"
            "process",  "Process"
            "default",  "Default"
            "type",     "Type"
            "yield",    "Yield"
            "class",    "Class"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "reserved -> capitalized bare ident"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "spaced sources keep the space inside backticks (only lead capitalized)" [
            "with space",   "``With space``"
            "foo bar baz",  "``Foo bar baz``"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "spaced pascal output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "leading-digit / dotted sources stay valid backtick identifiers" [
            "123",      "``123``"
            "1stPlace", "``1stPlace``"
            // `.`/`/`/`$` are INVALID in a TYPE name even backticked (FS0883 — `` ``A.b`` `` does NOT
            // compile, the prior expectation was a bug). They are treated as word boundaries so the
            // type name PascalCases across them into a clean bare identifier.
            "a.b",            "AB"
            "image/avif",     "ImageAvif"
            "$eq$gt",         "EqGt"
            "Message.inputImage", "MessageInputImage"
            // `@` (scoped-npm-package segment, e.g. `@modelcontextprotocol` from a cross-package
            // `node_modules/@scope/pkg` ref) is INVALID in a TYPE / module name even backticked —
            // `` ``@scope``.Sub.T `` is unparseable. Treated as a word boundary like `.`/`/`/`$`.
            "@modelcontextprotocol", "Modelcontextprotocol"
            "@scope/pkg",            "ScopePkg"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Pascal.create source)
            Expect.equal m expected "pascal output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"
    ]

// ------------------------------------------------------------------------------------
// (1f+) Camel casing — exhaustive
// ------------------------------------------------------------------------------------

[<Tests>]
let camelExhaustiveTests =
    testList "Name.Camel.create exhaustive" [
        testTheory "separators collapse and the lead letter lowercases" [
            "Scale-Down",  "scaleDown"
            "camel_case",  "camelCase"
            "camel-case",  "camelCase"
            "MyType",      "myType"
            "FooBar",      "fooBar"
            "MyHTTPServer","myHTTPServer"   // acronym run: only first letter lowercased
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "camel output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "leading non-letters are kept, the first letter lowercased" [
            "_Foo",  "_foo"
            "_Camel_Case", "_camelCase"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "camel output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "already-camel and UPPER_SNAKE / acronym sources are preserved" [
            "alreadyCamel", "alreadyCamel"
            "fooBar",       "fooBar"
            "x",            "x"
            "CAMEL_CASE",   "CAMEL_CASE"
            "ABC",          "ABC"           // all-upper short acronym is upper-snake-ish: preserved
            "A",            "A"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "camel output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "camelCasing that lands on a reserved word is re-backticked" [
            "Type",     "``type``"
            "Yield",    "``yield``"
            "Global",   "``global``"
            "Internal", "``internal``"
            "Default",  "``default``"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "reserved camel re-backticked"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testCase "Process camelCases to a bare ident (process is NOT reserved)" <| fun _ ->
            let m = modifiedCased (Name.Camel.create "Process")
            Expect.equal m "process" "process is not an F# keyword"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        testTheory "leading-digit / dotted camel sources stay valid backtick identifiers" [
            "123", "``123``"
            "a.b", "``a.b``"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Camel.create source)
            Expect.equal m expected "camel output"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"
    ]

// ------------------------------------------------------------------------------------
// (1g+) Module naming — exhaustive (plus a NEW defect)
// ------------------------------------------------------------------------------------

[<Tests>]
let moduleExhaustiveTests =
    testList "Name.Module.create exhaustive" [
        testTheory "I-prefixed PascalCase module names for bare-ident-producing sources" [
            "foo",         "IFoo"
            "Foo",         "IFoo"
            "module_name", "IModuleName"
            "my-mod",      "IMyMod"
            "scale-down",  "IScaleDown"
            "type",        "IType"
            "type-x",      "ITypeX"
            "_foo",        "IFoo"      // leading underscore trimmed before I-prefix
            "__x",         "IX"
            "_moduleName", "IModuleName"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Module.create source)
            Expect.equal m expected "module name output"
            Expect.isTrue (isValidFSharpIdent m) "module name must be valid F#"

        testCase "UPPER_SNAKE_CASE module name keeps the snake body, I-prefixed" <| fun _ ->
            modifiedCased (Name.Module.create "MODULE_NAME")
            |> Flip.Expect.equal "" "IMODULE_NAME"

        // A source whose SOURCE already starts with `` is pascal-cased WITHOUT the I-prefix
        // (the backtick-passthrough branch of sourceMapToModuleName).
        testCase "a genuinely backtick-wrapped source is pascal-cased without the I-prefix" <| fun _ ->
            let m =
                Name.sourceMapToModuleName (Name.Source "``my type``")
                |> Case.withoutMeasure
                |> fun n -> n.ValueOrModified
            Expect.equal m "``My type``" "no I-prefix; space preserved inside backticks"
            Expect.isTrue (isValidFSharpIdent m) "must be valid F#"

        // BUG (pending): when the pascal-cased body STILL needs backticks (because it
        // contains a space, dot, or a leading digit), Module naming prepends a bare `I`
        // onto an already-``backtick``-wrapped body, e.g.
        //   "with space" -> "I``With space``"   "a.b" -> "I``A.b``"   "123" -> "I``123``".
        // None of these compile: `module I``With space`` =` fails (FS0010 / the bare `I`
        // is a separate token from the backtick identifier — there is no single
        // identifier `I``With space```).
        // ROOT CAUSE: Name.fs `mapToModuleName`/`sourceMapToModuleName` apply
        //   `map (sprintf "I%s")` to the OUTPUT of pascalCase, which has already been
        //   run through NormalizeIdentifierBackticks. The "I" is glued OUTSIDE the
        //   backtick fence instead of inside it.
        // FIX: prepend "I" to the bare (stripped) body and re-normalize once, so a
        //   spaced source yields the single valid identifier ``IWith space`` (or
        //   "IWithSpace" if spaces are also collapsed) — never the split `I``...```.
        testCase "module name of a backtick-needing source must be a single valid identifier" <| fun _ ->
            let m = modifiedCased (Name.Module.create "with space")
            Expect.isTrue (isValidFSharpIdent m)
                "module name with a spaced source must be a single valid F# identifier"
        testCase "module name of a dotted source must be a single valid identifier" <| fun _ ->
            let m = modifiedCased (Name.Module.create "a.b")
            Expect.isTrue (isValidFSharpIdent m)
                "module name with a dotted source must be a single valid F# identifier"
        testCase "module name of a leading-digit source must be a single valid identifier" <| fun _ ->
            let m = modifiedCased (Name.Module.create "123")
            Expect.isTrue (isValidFSharpIdent m)
                "module name with a leading-digit source must be a single valid F# identifier"
    ]

// ------------------------------------------------------------------------------------
// (1h+) Typar naming — exhaustive (the just-fixed separator sanitization + a NEW defect)
// ------------------------------------------------------------------------------------

[<Tests>]
let typarExhaustiveTests =
    testList "Name.Typar.create exhaustive" [
        // Valid bare-ident bodies are left UNTOUCHED (only the quote is prepended).
        testTheory "valid bodies are prefixed verbatim" [
            "t",     "'t"
            "T",     "'T"
            "key",   "'key"
            "TKey",  "'TKey"
            "K1",    "'K1"
            "T1",    "'T1"
            "a_b",   "'a_b"     // underscore is a legal typar-body char: preserved
            "``t``", "'t"       // backticks stripped before prefixing
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Typar.create source)
            Expect.equal m expected "typar output"
            Expect.isTrue (isValidTypar m) "typar must be a valid F# type parameter"

        // JUST-FIXED: separatored sources camelCase into a valid bare-ident body.
        testTheory "hyphenated sources sanitize to a camelCase bare-ident typar body" [
            "key-of",          "'keyOf"
            "a-b",             "'aB"
            "T-Value",         "'tValue"
            "Key-Value-Pair",  "'keyValuePair"
        ] <| fun (source, expected) ->
            let m = modifiedCased (Name.Typar.create source)
            Expect.equal m expected "sanitized typar body"
            Expect.isTrue (isValidTypar m) "typar with a separator body must still be valid F#"

        // BUG (pending): `sanitizeTyparBody` only collapses the '-'/'_' separators that
        // its `toPascalCase` regex matches `(?:^|[-_])(.)`. A DOT, SPACE, SLASH, or a
        // LEADING QUOTE is neither a legal bare-ident char nor collapsed, so it survives
        // into the body and yields an un-parseable typar:
        //   "a.b" -> "'a.b"   "a b" -> "'a b"   "a/b" -> "'a/b"   "'key" -> "''key".
        // `type Foo<'a.b> = ...` fails (FS0010) and a typar CANNOT be backtick-escaped.
        // ROOT CAUSE: Name.fs `sanitizeTyparBody` — the else-branch runs only
        //   `toPascalCase` (which collapses '-'/'_' but leaves '.'/' '/'/'/'\'' intact)
        //   and never strips/replaces the remaining backtick-illegal-for-typar chars.
        // FIX: in the separator branch, strip ALL non-(letter|digit|underscore) chars
        //   from the body after casing, e.g. "a.b" -> "'aB" or "'ab".
        testCase "dotted typar body must sanitize to a valid bare-ident typar" <| fun _ ->
            let m = modifiedCased (Name.Typar.create "a.b")
            Expect.isTrue (isValidTypar m) "typar with a dotted body must still be valid F#"
        testCase "spaced typar body must sanitize to a valid bare-ident typar" <| fun _ ->
            let m = modifiedCased (Name.Typar.create "a b")
            Expect.isTrue (isValidTypar m) "typar with a spaced body must still be valid F#"
        testCase "leading-quote typar body must sanitize to a valid bare-ident typar" <| fun _ ->
            let m = modifiedCased (Name.Typar.create "'key")
            Expect.isTrue (isValidTypar m) "typar with a leading-quote body must still be valid F#"
    ]

// ------------------------------------------------------------------------------------
// (1c+) Case measures — round-trip add/remove for EVERY measure
// ------------------------------------------------------------------------------------

[<Tests>]
let caseMeasureExhaustiveTests =
    testList "Case measure exhaustive round-trips" [
        // addMeasure<m> then withoutMeasure is identity for every measure, over both
        // Source and Modified shapes.
        testCase "pascal measure round-trips both Source and Modified" <| fun _ ->
            let s = Name.Source "Foo"
            let m = Name.Modified("o", "M")
            Expect.equal (Case.withoutMeasure (Case.addPascalMeasure s)) s "source"
            Expect.equal (Case.withoutMeasure (Case.addPascalMeasure m)) m "modified"
        testCase "camel measure round-trips both Source and Modified" <| fun _ ->
            let s = Name.Source "foo"
            let m = Name.Modified("o", "m")
            Expect.equal (Case.withoutMeasure (Case.addCamelMeasure s)) s "source"
            Expect.equal (Case.withoutMeasure (Case.addCamelMeasure m)) m "modified"
        testCase "module measure round-trips both Source and Modified" <| fun _ ->
            let s = Name.Source "IFoo"
            let m = Name.Modified("o", "IM")
            Expect.equal (Case.withoutMeasure (Case.addModuleMeasure s)) s "source"
            Expect.equal (Case.withoutMeasure (Case.addModuleMeasure m)) m "modified"
        testCase "typar measure round-trips both Source and Modified" <| fun _ ->
            let s = Name.Source "'t"
            let m = Name.Modified("o", "'m")
            Expect.equal (Case.withoutMeasure (Case.addTyparMeasure s)) s "source"
            Expect.equal (Case.withoutMeasure (Case.addTyparMeasure m)) m "modified"

        // unboxMeasure re-tags between every pair without touching the string.
        testCase "unboxMeasure re-tags pascal->camel->module->typar unchanged" <| fun _ ->
            let p : Name<Case.pascal> = Case.addPascalMeasure (Name.Modified("o", "X"))
            let c : Name<Case.camel> = Case.unboxMeasure p
            let md : Name<Case.modulename> = Case.unboxMeasure c
            let tp : Name<Case.typar> = Case.unboxMeasure md
            Expect.equal (Name.Case.valueOrModified tp) "X" "string never changes across re-tags"
            Expect.equal (Name.Case.valueOrSource tp) "o" "source never changes across re-tags"

        // Name.Case.map / mapSource operate through the measure envelope.
        testCase "Name.Case.map maps the modified value under the measure" <| fun _ ->
            let p : Name<Case.pascal> = Case.addPascalMeasure (Name.Modified("o", "a"))
            let r = Name.Case.map (fun _ -> "b") p
            Expect.equal r.ValueOrModified "b" "mapped under measure"
        testCase "Name.Case.mapSource maps the source under the measure" <| fun _ ->
            let p : Name<Case.pascal> = Case.addPascalMeasure (Name.Modified("o", "a"))
            let r = Name.Case.mapSource (fun _ -> "b") p
            Expect.equal r.ValueOrModified "b" "mapped source -> modified under measure"

        testCase "addMeasure (generic) round-trips through withoutMeasure for an arbitrary measure" <| fun _ ->
            let n = Name.create "round-trip"
            let tagged : Name<Case.camel> = Case.addMeasure n
            Case.withoutMeasure tagged |> Flip.Expect.equal "generic measure round-trip" n
    ]

// ------------------------------------------------------------------------------------
// (1b+) Name map algebra — exhaustive Source/Modified transitions
// ------------------------------------------------------------------------------------

[<Tests>]
let nameMapAlgebraTests =
    testList "Name map algebra exhaustive" [
        // map: divergence-from-original collapses to Source; otherwise Modified.
        testCase "map on Modified collapsing back to original yields Source" <| fun _ ->
            Name.map (fun _ -> "orig") (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Source "orig")
        testCase "map on Modified to a new value stays Modified" <| fun _ ->
            Name.map (fun _ -> "new") (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Modified("orig", "new"))
        testCase "map sees the modified value, not the original" <| fun _ ->
            // The mapper receives "m" (modified), not "orig".
            Name.map (fun s -> s + "!") (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Modified("orig", "m!"))

        // mapSource: always maps the ORIGINAL, regardless of modified.
        testCase "mapSource maps original even when a different modified exists" <| fun _ ->
            Name.mapSource (fun s -> s + "X") (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Modified("orig", "origX"))
        testCase "mapSource that reproduces the original collapses to Source" <| fun _ ->
            Name.mapSource id (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Source "orig")
        testCase "mapSource on a Source that diverges becomes Modified" <| fun _ ->
            Name.mapSource (fun _ -> "z") (Name.Source "orig")
            |> Flip.Expect.equal "" (Name.Modified("orig", "z"))

        // mapModified: maps the modified value; on a Source it discards to Source.
        testCase "mapModified collapsing to original yields Source" <| fun _ ->
            Name.mapModified (fun _ -> "orig") (Name.Modified("orig", "m"))
            |> Flip.Expect.equal "" (Name.Source "orig")
        testCase "mapModified on a Source always yields Source" <| fun _ ->
            Name.mapModified (fun _ -> "anything") (Name.Source "orig")
            |> Flip.Expect.equal "" (Name.Source "orig")

        // createModified and the static constructors.
        testCase "Name.createModified builds a Modified DU" <| fun _ ->
            Name.createModified "o" "m" |> Flip.Expect.equal "" (Name.Modified("o", "m"))

        // pascalCase vs sourcePascalCase divergence on a Modified name.
        testCase "pascalCase maps the modified value; sourcePascalCase maps the original" <| fun _ ->
            let mn = Name.Modified("a-b", "OVERRIDE")
            Expect.equal (Name.pascalCase mn |> modified) "OVERRIDE" "pascalCase uses modified"
            Expect.equal (Name.sourcePascalCase mn |> modified) "AB" "sourcePascalCase uses original"
        testCase "capitalize uppercases only the first character" <| fun _ ->
            Expect.equal (Name.capitalize (Name.Source "foo") |> modified) "Foo" "capitalize"
    ]

// ------------------------------------------------------------------------------------
// (1d+) CasedName envelope — Value drops measure for each constructor, both shapes
// ------------------------------------------------------------------------------------

[<Tests>]
let casedNameExhaustiveTests =
    testList "CasedName.Value exhaustive" [
        testCase "Pascal envelope over a Modified drops the measure, keeps modified" <| fun _ ->
            let cn = CasedName.Pascal(Case.addPascalMeasure (Name.Modified("o", "Foo")))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "Foo"
            cn.Value.ValueOrSource |> Flip.Expect.equal "" "o"
        testCase "Camel envelope over a Modified drops the measure" <| fun _ ->
            let cn = CasedName.Camel(Case.addCamelMeasure (Name.Modified("o", "foo")))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "foo"
        testCase "Module envelope over a Modified drops the measure" <| fun _ ->
            let cn = CasedName.Module(Case.addModuleMeasure (Name.Modified("o", "IFoo")))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "IFoo"
        testCase "Typar envelope over a Modified drops the measure" <| fun _ ->
            let cn = CasedName.Typar(Case.addTyparMeasure (Name.Modified("o", "'t")))
            cn.Value.ValueOrModified |> Flip.Expect.equal "" "'t"
    ]

// ------------------------------------------------------------------------------------
// (1i+) Normalization.normalize — exhaustive
// ------------------------------------------------------------------------------------

[<Tests>]
let normalizeExhaustiveTests =
    testList "Name.Normalization.normalize exhaustive" [
        testTheory "valid identifiers (incl. unicode, underscore, prime) are untouched" [
            "foo"; "fooBar"; "_x"; "Name123"; "café"; "a'b"; "_"; "__proto__"; "UPPER_CASE"
        ] <| fun source ->
            Name.Normalization.normalize source |> Flip.Expect.equal "unchanged" source
        testTheory "invalid bare identifiers are wrapped in valid backticks" [
            "scale-down", "``scale-down``"
            "a.b",        "``a.b``"
            "a/b",        "``a/b``"
            "with space", "``with space``"
            "type",       "``type``"
            "0",          "``0``"
            "0abc",       "``0abc``"
        ] <| fun (source, expected) ->
            let n = Name.Normalization.normalize source
            Expect.equal n expected "wrapped"
            Expect.isTrue (isValidFSharpIdent n) "normalized must be valid F#"
        testTheory "empty / whitespace-only sources normalize to the Empty placeholder" [
            ""; "   "; "\t"; "\n"
        ] <| fun source ->
            Name.Normalization.normalize source |> Flip.Expect.equal "Empty placeholder" "Empty"
    ]

// ------------------------------------------------------------------------------------
// (2+) CodeKey — exhaustive equality / discrimination matrix
// ------------------------------------------------------------------------------------

[<Tests>]
let codeKeyExhaustiveTests =
    let k1 : TypeKey = TypeKey.createWith 1
    let k2 : TypeKey = TypeKey.createWith 2
    testList "CodeKey exhaustive discrimination" [
        // Pairwise: every distinct TypeCodeKey case over the SAME key is unequal to the
        // others — the tag is load-bearing, not just the payload.
        testCase "all payload-bearing TypeCodeKey cases over one key are pairwise distinct" <| fun _ ->
            let cases =
                [ TypeCodeKey.Conditional k1; TypeCodeKey.Primitive k1; TypeCodeKey.EnumCase k1
                  TypeCodeKey.Union k1; TypeCodeKey.Intersection k1; TypeCodeKey.Literal k1
                  TypeCodeKey.IndexAccess k1; TypeCodeKey.TypeReference k1; TypeCodeKey.Array k1
                  TypeCodeKey.TypeParameter k1; TypeCodeKey.ReadOnly k1; TypeCodeKey.Tuple k1
                  TypeCodeKey.Index k1; TypeCodeKey.Predicate k1; TypeCodeKey.TypeLiteral k1
                  TypeCodeKey.TemplateLiteral k1; TypeCodeKey.Optional k1
                  TypeCodeKey.Substitution k1; TypeCodeKey.TypeQuery k1; TypeCodeKey.GlobalThis ]
            cases
            |> List.mapi (fun i c -> i, c)
            |> List.iter (fun (i, ci) ->
                cases
                |> List.mapi (fun j cj -> j, cj)
                |> List.iter (fun (j, cj) ->
                    if i <> j then
                        Expect.notEqual ci cj (sprintf "case %d must differ from case %d" i j)))

        testCase "each TypeCodeKey case is self-equal (reflexive) over the same key" <| fun _ ->
            Expect.equal (TypeCodeKey.GlobalThis) (TypeCodeKey.GlobalThis) "GlobalThis self-equal"
            Expect.equal (TypeCodeKey.Conditional k1) (TypeCodeKey.Conditional k1) "Conditional self-equal"
            Expect.equal (TypeCodeKey.TypeQuery k2) (TypeCodeKey.TypeQuery k2) "TypeQuery self-equal"

        testCase "every TypeCodeKey payload case differs when only the key differs" <| fun _ ->
            Expect.notEqual (TypeCodeKey.Primitive k1) (TypeCodeKey.Primitive k2) "Primitive key"
            Expect.notEqual (TypeCodeKey.Array k1) (TypeCodeKey.Array k2) "Array key"
            Expect.notEqual (TypeCodeKey.Tuple k1) (TypeCodeKey.Tuple k2) "Tuple key"

        // ExportCodeKey: all 7 cases over one key are pairwise distinct.
        testCase "all ExportCodeKey cases over one key are pairwise distinct" <| fun _ ->
            let cases =
                [ ExportCodeKey.Variable k1; ExportCodeKey.Interface k1; ExportCodeKey.TypeAlias k1
                  ExportCodeKey.Class k1; ExportCodeKey.Enum k1; ExportCodeKey.Module k1
                  ExportCodeKey.Function k1 ]
            cases
            |> List.mapi (fun i c -> i, c)
            |> List.iter (fun (i, ci) ->
                cases
                |> List.mapi (fun j cj -> j, cj)
                |> List.iter (fun (j, cj) ->
                    if i <> j then
                        Expect.notEqual ci cj (sprintf "export case %d must differ from %d" i j)))

        testCase "ExportCodeKey same case differs when only the key differs" <| fun _ ->
            Expect.notEqual (ExportCodeKey.Function k1) (ExportCodeKey.Function k2) "Function key"
            Expect.notEqual (ExportCodeKey.Module k1) (ExportCodeKey.Module k2) "Module key"

        // CodeKey outer envelope: an Export and a Type that wrap the same underlying
        // TypeKey never collide, across several case pairings.
        testTheory "CodeKey Export vs Type never collide over a shared key" [
            0; 1; 2
        ] <| fun _ ->
            let asExport = CodeKey.Export(ExportCodeKey.Module k1)
            let asType = CodeKey.Type(TypeCodeKey.TypeReference k1)
            Expect.notEqual asExport asType "Export vs Type"
        testCase "CodeKey Export wrapping different ExportCodeKey cases stay distinct" <| fun _ ->
            Expect.notEqual
                (CodeKey.Export(ExportCodeKey.Interface k1))
                (CodeKey.Export(ExportCodeKey.Class k1))
                "inner export case discriminates the envelope"
        testCase "CodeKey Type wrapping different TypeCodeKey cases stay distinct" <| fun _ ->
            Expect.notEqual
                (CodeKey.Type(TypeCodeKey.Union k1))
                (CodeKey.Type(TypeCodeKey.Intersection k1))
                "inner type case discriminates the envelope"
        testCase "CodeKey full structural equality across both layers" <| fun _ ->
            Expect.equal
                (CodeKey.Export(ExportCodeKey.Enum k2))
                (CodeKey.Export(ExportCodeKey.Enum k2))
                "deep structural equality"
            Expect.notEqual
                (CodeKey.Export(ExportCodeKey.Enum k1))
                (CodeKey.Export(ExportCodeKey.Enum k2))
                "deep key discrimination"
    ]
