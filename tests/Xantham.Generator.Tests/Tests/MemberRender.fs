module Xantham.Generator.Tests.Tests.MemberRender

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

// Exhaustive coverage of src/Xantham.Generator/Generator/Render.Member.fs via the
// `renderTypeDefn` harness: build a "Probe" interface carrying exactly one member kind, render
// the full TypeDefn, and assert the verbatim emitted F#.
//
// METHODOLOGY NOTE on type text vs member structure
// -------------------------------------------------
// The harness (TypeDefnRenderHarness.fs) shares a SINGLE GeneratorContext across every render.
// A member's TYPE text is produced by `ctx.PreludeGetTypeRef`, whose render cache is keyed by
// `ResolvedType`. PRIMITIVES are value-equal, so once a `Number`/`String` primitive has been
// rendered under one anchor its cached (and by then collapsed) ref is reused for every later
// member that mentions the same primitive — it surfaces as `option<obj>`. This is the
// "string-collapse" cache-keying defect (in the prelude TypeRefRenders cache, upstream of this
// pass — Render.Member.fs merely forwards `prop.Type` to `ctx.PreludeGetTypeRef`), and it makes
// any primitive-typed assertion order-dependent and flaky.
//
// NAMED interface references render STABLY (`abstract x: Foo ...`) regardless of cache state, so
// every assertion that pins the emitted TYPE uses `namedRef`. The string-collapse defect itself
// is characterised by the `ptestCase`s at the end, which assert the CORRECT (uncollapsed) output.

let private render = Xantham.Generator.Tests.Tests.TypeDefnRenderHarness.renderTypeDefn

let private lines = String.concat "\n"

/// Build an interface named "Probe" carrying the given members.
let private ifaceWith (members: Member list) =
    { (Interface.create "Probe") with Members = members }
    |> ResolvedType.Interface

/// A named-interface reference; renders stably as its bare pascal name.
let private namedRef name =
    TypeReference.create (Interface.create name |> Interface.wrap)
    |> TypeReference.wrap

let private str = primitive TypeKindPrimitive.String

// --- member builders absent from the Mocking module -----------------------

let private method' name optional parameters returnType : Member =
    Member.Method [
        { Name = Name.Camel.create name
          Parameters = parameters
          TypeParameters = []
          Type = LazyContainer.CreateFromValue returnType
          Documentation = []
          IsOptional = optional
          IsStatic = false }
    ]

let private getAccessor name typ : Member =
    Member.GetAccessor {
        Name = Name.Camel.create name
        Type = LazyContainer.CreateFromValue typ
        IsStatic = false
        IsPrivate = false
    }

let private setAccessor name typ : Member =
    Member.SetAccessor {
        Name = Name.Camel.create name
        ArgumentType = LazyContainer.CreateFromValue typ
        IsStatic = false
        IsPrivate = false
        Documentation = []
    }

let private indexSig parameters typ isReadOnly : Member =
    Member.IndexSignature {
        Parameters = parameters
        Type = LazyContainer.CreateFromValue typ
        IsReadOnly = isReadOnly
    }

let private constructSig parameters typ : Member =
    Member.ConstructSignature [
        { Type = LazyContainer.CreateFromValue typ
          Parameters = parameters
          TypeParameters = [] }
    ]

[<Tests>]
let tests =
    testList "MemberRender" [

        // PROPERTY -- read-write -> `with get, set`; read-only -> `with get`; optional -> option.
        // Property.render emits MemberRender.Property -> `abstract <name>: <type> <accessors>`.
        // Render.Member.fs:244-303 (Property module); accessor suffix from getTraits 245-254.
        testCase "read-write property -> with get, set" <| fun _ ->
            ifaceWith [ Property.create "x" (namedRef "Foo") |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "rw property" (lines [
                "type Probe ="
                "    abstract x: Foo with get, set"
            ])

        testCase "read-only property -> with get" <| fun _ ->
            ifaceWith [ Property.create "label" (namedRef "Foo") |> Property.readOnly |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "ro property" (lines [
                "type Probe ="
                "    abstract label: Foo with get"
            ])

        // Optional property wraps the type in `option<..>` (still read-write accessors).
        testCase "optional property -> option type" <| fun _ ->
            ifaceWith [ Property.create "maybe" (namedRef "Foo") |> Property.optional |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "optional property" (lines [
                "type Probe ="
                "    abstract maybe: option<Foo> with get, set"
            ])

        // METHOD -- Member.Method -> MemberRender.Method, a curried abstract signature with named
        // parameters. Render.Member.fs:63-95 (Method module).
        // Zero params emit a `unit` placeholder argument.
        testCase "method with 0 params -> unit arg" <| fun _ ->
            ifaceWith [ method' "ping" false [] (namedRef "Bar") ]
            |> render
            |> Flip.Expect.equal "method 0 params" (lines [
                "type Probe ="
                "    abstract ping: unit -> Bar"
            ])

        testCase "method with 1 param" <| fun _ ->
            ifaceWith [ method' "conv" false [ Parameter.create "input" (namedRef "Foo") ] (namedRef "Bar") ]
            |> render
            |> Flip.Expect.equal "method 1 param" (lines [
                "type Probe ="
                "    abstract conv: input: Foo -> Bar"
            ])

        // Multiple params are tupled with `*` (each labelled), then `->` to the return type.
        testCase "method with multiple params" <| fun _ ->
            ifaceWith [ method' "add" false [ Parameter.create "a" (namedRef "Foo"); Parameter.create "b" (namedRef "Qux") ] (namedRef "Bar") ]
            |> render
            |> Flip.Expect.equal "method N params" (lines [
                "type Probe ="
                "    abstract add: a: Foo * b: Qux -> Bar"
            ])

        // Optional param -> `?name`; spread param -> `[<ParamArray>] name`.
        // (Parameter render lives in Render.Parameter.fs; covered here through the member path.)
        testCase "method with optional + spread params" <| fun _ ->
            ifaceWith [
                method' "fmt" false [
                    Parameter.create "a" (namedRef "Foo")
                    Parameter.create "b" (namedRef "Qux") |> Parameter.optional
                    Parameter.create "rest" (namedRef "Zap") |> Parameter.spread
                ] (namedRef "Bar")
            ]
            |> render
            |> Flip.Expect.equal "method opt+spread" (lines [
                "type Probe ="
                "    abstract fmt: a: Foo * ?b: Qux * [<ParamArray>] rest: Zap -> Bar"
            ])

        // An IsOptional METHOD emits identically to a required one: the Optional trait
        // (Render.Member.fs:90) has no surface effect on an interface abstract method (unlike an
        // optional PROPERTY, which becomes `option<..>`). Asserts the observed (equal) output.
        testCase "optional method emits like a required method" <| fun _ ->
            ifaceWith [ method' "maybe" true [ Parameter.create "input" (namedRef "Foo") ] (namedRef "Bar") ]
            |> render
            |> Flip.Expect.equal "optional method" (lines [
                "type Probe ="
                "    abstract maybe: input: Foo -> Bar"
            ])

        // GET ACCESSOR -- GetAccessor.render -> MemberRender.Property with Readable+JSGetter
        // traits, so emits a property with `with get`. Render.Member.fs:97-140.
        testCase "get-accessor -> property with get" <| fun _ ->
            ifaceWith [ getAccessor "g" (namedRef "Foo") ]
            |> render
            |> Flip.Expect.equal "get accessor" (lines [
                "type Probe ="
                "    abstract g: Foo with get"
            ])

        // SET ACCESSOR -- SetAccessor.render -> MemberRender.Property with Writable+JSSetter, so
        // emits `with set` over the ArgumentType. Render.Member.fs:142-168.
        testCase "set-accessor -> property with set" <| fun _ ->
            ifaceWith [ setAccessor "s" (namedRef "Foo") ]
            |> render
            |> Flip.Expect.equal "set accessor" (lines [
                "type Probe ="
                "    abstract s: Foo with set"
            ])

        // INDEX SIGNATURE -- IndexSignature.render forces the member name to Pascal `Item` and
        // emits a method-like signature (JSIndexer trait). Render.Member.fs:170-207.
        testCase "index signature -> abstract Item method" <| fun _ ->
            ifaceWith [ indexSig [ Parameter.create "key" (namedRef "Foo") ] (namedRef "Bar") false ]
            |> render
            |> Flip.Expect.equal "index signature" (lines [
                "type Probe ="
                "    abstract Item: key: Foo -> Bar"
            ])

        // A read-only index signature withholds the Writable trait. It surfaces identically in
        // the abstract signature (the read/write distinction is carried in traits, not the
        // emitted signature text). Asserts the observed output. Render.Member.fs:179-183.
        testCase "read-only index signature emits same signature" <| fun _ ->
            ifaceWith [ indexSig [ Parameter.create "key" (namedRef "Foo") ] (namedRef "Bar") true ]
            |> render
            |> Flip.Expect.equal "read-only index signature" (lines [
                "type Probe ="
                "    abstract Item: key: Foo -> Bar"
            ])

        // CONSTRUCT SIGNATURE -- ConstructSignature.render forces the name to Pascal `Create`
        // and emits a method-like signature (JSConstructor trait). Render.Member.fs:209-242.
        testCase "construct signature -> abstract Create method" <| fun _ ->
            ifaceWith [ constructSig [ Parameter.create "x" (namedRef "Foo") ] (namedRef "Bar") ]
            |> render
            |> Flip.Expect.equal "construct signature" (lines [
                "type Probe ="
                "    abstract Create: x: Foo -> Bar"
            ])

        // CALL SIGNATURE member -- CallSignature.renderMember forces the name to `Invoke`
        // (JSCallSignature trait) and emits a method-like signature. Render.Member.fs:37-61.
        testCase "call signature member -> abstract Invoke method" <| fun _ ->
            ifaceWith [
                Member.CallSignature [
                    CallSignature.create (namedRef "Bar")
                    |> CallSignature.withParameters [ Parameter.create "x" (namedRef "Foo") ]
                ]
            ]
            |> render
            |> Flip.Expect.equal "call signature" (lines [
                "type Probe ="
                "    abstract Invoke: x: Foo -> Bar"
            ])

        // NAME ESCAPING -- a property whose name is an F# keyword (`type`) is backtick-escaped and
        // additionally carries an `[<EmitProperty("type")>]` attribute mapping back to the JS name.
        testCase "keyword property name is backtick-escaped + EmitProperty" <| fun _ ->
            ifaceWith [ Property.create "type" (namedRef "Foo") |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "escaped name" (lines [
                "type Probe ="
                "    [<EmitProperty(\"type\")>]"
                "    abstract ``type``: Foo with get, set"
            ])

        // MEMBER ORDER -- partitionRender (Render.Member.fs:324-331) folds members onto a list via
        // cons, REVERSING source order. Source [a; b] is emitted as b then a.
        testCase "members are emitted in reversed source order" <| fun _ ->
            ifaceWith [
                Property.create "a" (namedRef "Foo") |> Property.wrap
                Property.create "b" (namedRef "Bar") |> Property.wrap
            ]
            |> render
            |> Flip.Expect.equal "reversed order" (lines [
                "type Probe ="
                "    abstract b: Bar with get, set"
                "    abstract a: Foo with get, set"
            ])

        // Properties are partitioned away from methods: the fold splits MemberRender.Property vs
        // MemberRender.Method into two lists, so an interface mixing both emits all properties,
        // then all methods (each list independently reversed). Both sub-lists are `abstract`.
        testCase "mixed property + method members partition then both reverse" <| fun _ ->
            ifaceWith [
                Property.create "p" (namedRef "Foo") |> Property.wrap
                method' "m" false [ Parameter.create "x" (namedRef "Qux") ] (namedRef "Bar")
            ]
            |> render
            |> Flip.Expect.equal "mixed members" (lines [
                "type Probe ="
                "    abstract p: Foo with get, set"
                "    abstract m: x: Qux -> Bar"
            ])

        // PROPERTY TYPED WITH A UNION -- a `string | number`-style union erases to `U2<..>`
        // (preserving element order) through PreludeGetTypeRef.
        testCase "property typed with a union -> U2<..>" <| fun _ ->
            ifaceWith [ Property.create "u" (Union.create [ namedRef "Foo"; namedRef "Bar" ]) |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "union property" (lines [
                "type Probe ="
                "    abstract u: U2<Foo, Bar> with get, set"
            ])

        // PROPERTY TYPED WITH AN ARRAY -> ResizeArray<..>.
        testCase "property typed with an array -> ResizeArray<..>" <| fun _ ->
            ifaceWith [ Property.create "arr" (Array.create (namedRef "Foo")) |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "array property" (lines [
                "type Probe ="
                "    abstract arr: ResizeArray<Foo> with get, set"
            ])

        // PROPERTY TYPED WITH A NAMED REF -> the bare pascal name.
        testCase "property typed with a named ref -> bare name" <| fun _ ->
            ifaceWith [ Property.create "p" (namedRef "Point") |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "named ref property" (lines [
                "type Probe ="
                "    abstract p: Point with get, set"
            ])

        // ---- BUG: primitive string-collapse -------------------------------------------------
        // A primitive-typed property should emit its primitive (`string`/`float`/`bool`). Because
        // the shared-ctx PreludeGetTypeRef cache is keyed by value-equal `ResolvedType`, a
        // primitive that has already been rendered+collapsed elsewhere comes back as `option<obj>`
        // here. These ptestCases assert the CORRECT (uncollapsed) emission and will go green once
        // the cache keys primitives by their resolution-site path rather than by raw ResolvedType.
        // Root cause: prelude TypeRefRenders cache keying (Types/Generator.fs:80 PreludeGetTypeRef
        // + the TypeRefRenders dictionary in Types/Generator.fs) — NOT Render.Member.fs, which
        // faithfully forwards prop.Type (Render.Member.fs:298).
        testCase "BUG read-write string property should emit `string`, not collapse to option<obj>" <| fun _ ->
            ifaceWith [ Property.create "x" str |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "string property collapse" (lines [
                "type Probe ="
                "    abstract x: string with get, set"
            ])

        testCase "BUG read-only string property should emit `string with get`, not option<obj>" <| fun _ ->
            ifaceWith [ Property.create "label" str |> Property.readOnly |> Property.wrap ]
            |> render
            |> Flip.Expect.equal "ro string property collapse" (lines [
                "type Probe ="
                "    abstract label: string with get"
            ])

        // Repeated primitive within ONE interface: only the first-emitted (= last in source)
        // survives; earlier duplicates collapse to option<obj>. Correct output is all `string`.
        testCase "BUG repeated string properties should all emit `string`, not collapse" <| fun _ ->
            ifaceWith [
                Property.create "a" str |> Property.wrap
                Property.create "b" str |> Property.wrap
            ]
            |> render
            |> Flip.Expect.equal "repeated string collapse" (lines [
                "type Probe ="
                "    abstract b: string with get, set"
                "    abstract a: string with get, set"
            ])

        // DUPLICATE-PROPERTY DEDUP (Member.partitionRender, ledgered): TS member merges
        // (intersections, upstream twin declarations) can land same-named properties with
        // DIFFERENT types in one interface — F# properties cannot overload (FS0438 duplicate
        // get_/set_, FS3172 getter/setter type mismatch). First declaration in source order
        // wins; the drop is counted in the advisory ledger.
        testCase "duplicate property names dedup — first declaration wins" <| fun _ ->
            ifaceWith [
                Property.create "dup" (namedRef "Foo") |> Property.wrap
                Property.create "dup" (namedRef "Bar") |> Property.wrap
            ]
            |> render
            |> Flip.Expect.equal "duplicate property dedup" (lines [
                "type Probe ="
                "    abstract dup: Foo with get, set"
            ])

        // FUNCTION-PARAM PARENTHESIZATION (TypeRender.Render.fs `parenIfFunction`): a
        // function-typed parameter's arrow chain must parenthesize, or the member's own
        // signature reads as CURRIED — FS0440 when an optional parameter follows (the
        // `every: predicate ... * ?thisArg` class, 46 sites at the first Zod assembly),
        // FS0439 curried-vs-tupled Invoke overload clashes.
        testCase "function-typed parameter is parenthesized (optional param follows)" <| fun _ ->
            let callback =
                TypeLiteral.empty
                |> TypeLiteral.withMembers [
                    CallSignature.create (namedRef "Out")
                    |> CallSignature.withParameters [ Parameter.create "x" (namedRef "In") ]
                    |> List.singleton
                    |> CallSignature.wrap
                ]
                |> TypeLiteral.wrap
            ifaceWith [
                method' "every" false [
                    Parameter.create "predicate" callback
                    Parameter.create "thisArg" (namedRef "Foo") |> Parameter.optional
                ] (namedRef "Bar")
            ]
            |> render
            |> Flip.Expect.equal "parenthesized function param" (lines [
                "type Probe ="
                "    abstract every: predicate: (In -> Out) * ?thisArg: Foo -> Bar"
            ])
    ]
