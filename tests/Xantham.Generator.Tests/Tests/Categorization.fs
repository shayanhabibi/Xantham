module Xantham.Generator.Tests.Tests.Categorization

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.TypeRefRender
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Mocking.ArenaInterner.ResolvedType

// =============================================================================
// CATEGORIZATION pass — src/Xantham.Generator/Generator/ResolvedType.Categorization.fs
//   ResolvedTypeCategories.create : ResolvedType -> ResolvedTypeCategories
//   record = { EnumLike; LiteralLike; Primitives; Others; Nullable }
//
// This pass DECIDES TYPE-MAPPING: how a union's members bucket. Those buckets then
// drive the union match arms in prerender (RenderScope.Prelude.fs). It has ZERO direct
// tests today. We vet it two ways:
//   (1) END-TO-END through the prerender harness (`testRender`): assert the rendered F#
//       type each categorization OUTCOME produces. This is how categorization is observed
//       in production — its only consumer is prerender's `ResolvedType.Union` arm.
//   (2) DIRECT-CONTRACT (`ResolvedTypeCategories.create` is accessible): assert the bucket
//       lists themselves for representative inputs — the truest per-pass assertion.
//
// Where current behaviour looks WRONG, the test asserts the CORRECT expected output and is
// marked `ptest` (pending) with a comment — never asserting buggy behaviour just to pass.
// =============================================================================

let private testRender = Xantham.Generator.Tests.Tests.TypeRefRender.testRender

let private strLit (v: string) = Literal.create v |> Literal.wrap
let private intLit (v: int) = Literal.create v |> Literal.wrap
let private boolLit (v: bool) = Literal.create v |> Literal.wrap

// An enum whose Members back-reference (Parent) the completed EnumType, so the
// `simplify` pass can compare present-case-count against expected-member-count.
let private buildEnum name (caseSpecs: (string * int) list) =
    let enum0 = Enum.create name
    let cases0 = caseSpecs |> List.map (fun (n, v) -> EnumCase.create n (TsLiteral.Int v) enum0)
    let enum = enum0 |> Enum.withMembers (cases0 |> List.map Lazy.CreateFromValue)
    let cases = cases0 |> List.map (fun c -> { c with Parent = Lazy.CreateFromValue enum })
    enum, cases

// -----------------------------------------------------------------------------
// (1) END-TO-END through prerender — rendered-type outcomes per categorization.
// -----------------------------------------------------------------------------

// Union of pure primitives -> erased U2/U3 (Primitives bucket only).
let unionOfPrimitives = testList "Union of primitives -> erased Un" [
    testCase "string | number -> U2<string, int>" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Integer ]
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
    testCase "string | number | boolean -> U3" <| fun _ ->
        Union.create [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Number
            primitive TypeKindPrimitive.Boolean
        ]
        |> testRender "U3<string, float, bool>"
        ||> Flip.Expect.equal ""
]

// Nullability intent — undefined/null/void/never/unknown/any either nullable-only or stacked.
let nullability = testList "Nullability categorization" [
    testCase "string | undefined -> option<string>" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Undefined ]
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "string | null -> option<string>" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Null ]
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "all-nullable union -> option<obj>" <| fun _ ->
        // null/undefined/void/never carry only nullability; unknown/any carry nullability
        // AND a NonPrimitive intent. So the union collapses to a single nullable obj.
        Union.create [
            primitive TypeKindPrimitive.Null
            primitive TypeKindPrimitive.Undefined
            primitive TypeKindPrimitive.Void
            primitive TypeKindPrimitive.Never
            primitive TypeKindPrimitive.Unknown
            primitive TypeKindPrimitive.Any
        ]
        |> testRender "option<obj>"
        ||> Flip.Expect.equal ""
    testCase "null literal contributes only nullability" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.Integer; Literal.createNull |> Literal.wrap ]
        |> testRender "option<int>"
        ||> Flip.Expect.equal ""
]

// Single-member union collapse (the `[ single ]` arms in prerender).
let singleCollapse = testList "Single-member union collapses" [
    testCase "single primitive collapses to primitive" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.String ]
        |> testRender "string"
        ||> Flip.Expect.equal ""
    testCase "single primitive + null -> option" <| fun _ ->
        Union.create [ primitive TypeKindPrimitive.Number; primitive TypeKindPrimitive.Null ]
        |> testRender "option<float>"
        ||> Flip.Expect.equal ""
]

// LiteralLike categorization -> hoisted canonical enum at LiteralUnions.<identity-name>.
// The name is the sorted literal tokens joined by '_', pascal-cased (so the '_' vanishes):
//   "a"|"b"  -> tokens [a;b]   -> "a_b"   -> AB
//   1|2      -> tokens [i1;i2] -> "i1_i2" -> I1I2
let literalUnionsHoist = testList "Literal unions -> hoisted enum reference" [
    testCase "string-literal union \"a\"|\"b\" hoists to LiteralUnions.AB" <| fun _ ->
        Union.create [ strLit "a"; strLit "b" ]
        |> testRender "LiteralUnions.AB"
        ||> Flip.Expect.equal ""
    testCase "int-literal union 1|2 hoists to LiteralUnions.I1I2" <| fun _ ->
        Union.create [ intLit 1; intLit 2 ]
        |> testRender "LiteralUnions.I1I2"
        ||> Flip.Expect.equal ""
    testCase "nullable string-literal union -> option<LiteralUnions.AB>" <| fun _ ->
        Union.create [ strLit "a"; strLit "b"; primitive TypeKindPrimitive.Null ]
        |> testRender "option<LiteralUnions.AB>"
        ||> Flip.Expect.equal ""
]

// `literalUnionName` is the union's canonical IDENTITY (the dedup key): same sorted value-set ->
// same name -> one emission -> all refs resolve. Concatenating EVERY member value gives an
// unbounded, unusable public name for a many-member union (200-335 chars on the real surface).
// It is BOUNDED: a long name keeps a readable prefix + a content-stable hash of the FULL sorted
// token list. These tests pin the three contract properties: BOUNDED, DETERMINISTIC, COLLISION-SAFE.
let literalUnionNameBounding =
    let manyTokens prefix n =
        [ for i in 1..n -> ResolvedTypeLiteralLike.Literal (TsLiteral.String $"{prefix}LongLiteralValueNumber{i}") ]
    let nameOf literals : string =
        (literalUnionName literals |> Xantham.Decoder.Case.withoutMeasure).ValueOrModified
    testList "literalUnionName bounding (canonical-enum identity)" [
        testCase "a short union keeps its verbatim concatenated name" <| fun _ ->
            let n = nameOf [ ResolvedTypeLiteralLike.Literal (TsLiteral.String "a"); ResolvedTypeLiteralLike.Literal (TsLiteral.String "b") ]
            Flip.Expect.equal "short union name is the verbatim pascal-cased concatenation" "AB" n
        testCase "a many-member union name is BOUNDED (not the full smash)" <| fun _ ->
            let n = nameOf (manyTokens "x" 30)
            Flip.Expect.isTrue $"bounded name must be short, got {n.Length} chars: {n}" (n.Length <= 48)
        testCase "the bounded name is DETERMINISTIC (same value-set -> same name)" <| fun _ ->
            Flip.Expect.equal "same union must yield the same bounded name across calls"
                (nameOf (manyTokens "x" 30)) (nameOf (manyTokens "x" 30))
        testCase "DETERMINISTIC regardless of input order (sorted by value)" <| fun _ ->
            Flip.Expect.equal "member order must not change the canonical name"
                (nameOf (manyTokens "x" 30)) (nameOf (manyTokens "x" 30 |> List.rev))
        testCase "distinct many-member unions get DISTINCT bounded names (no collision)" <| fun _ ->
            Flip.Expect.notEqual "two different value-sets must not collapse to one canonical name"
                (nameOf (manyTokens "x" 30)) (nameOf (manyTokens "y" 30))
    ]

// Mixed buckets: LiteralLike + Primitive -> the literals collapse to ONE inner literal-union
// molecule member, unioned with the primitive (an Un over [literalUnion; primitive]).
let mixedBuckets = testList "Mixed literal + primitive union" [
    testCase "(\"a\"|\"b\") | number -> U2<LiteralUnions.AB, int>" <| fun _ ->
        Union.create [ strLit "a"; strLit "b"; primitive TypeKindPrimitive.Integer ]
        |> testRender "U2<LiteralUnions.AB, int>"
        ||> Flip.Expect.equal ""
]

// EnumLike categorization: when EVERY case of an enum appears in the union, `simplify`
// collapses the cases into a single EnumLike.Enum -> the union renders as the enum reference.
let enumLike = testList "EnumLike categorization" [
    testCase "all enum cases present collapse to the enum reference" <| fun _ ->
        let _, cases = buildEnum "Color" [ "Red", 0; "Blue", 1 ]
        cases
        |> List.map EnumCase.wrap
        |> Union.create
        |> testRender "Global.Color"
        ||> Flip.Expect.equal ""
]

// Structural unwrapping inside categorization: ReadOnly / Optional / single-arg TypeReference /
// TypeParameter-with-constraint are transparent — categorize the inner type.
let unwrapping = testList "Structural unwrapping" [
    testCase "ReadOnly<string> categorizes as string" <| fun _ ->
        ResolvedType.ReadOnly (primitive TypeKindPrimitive.String)
        |> testRender "string"
        ||> Flip.Expect.equal ""
    testCase "ReadOnly inside a union categorizes its inner member" <| fun _ ->
        // ReadOnly<string> | number  ->  the readonly is transparent  ->  U2<string, int>
        Union.create [
            ResolvedType.ReadOnly (primitive TypeKindPrimitive.String)
            primitive TypeKindPrimitive.Integer
        ]
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
    testCase "TypeReference (no args) inside a union unwraps to inner" <| fun _ ->
        Union.create [
            primitive TypeKindPrimitive.String |> TypeReference.create |> TypeReference.wrap
            primitive TypeKindPrimitive.Integer
        ]
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
    testCase "Optional inside a union lifts nullability to the whole union" <| fun _ ->
        // (string | undefined) | number  ->  nullability lifts to the top  ->  option<U2<string,int>>
        Union.create [
            Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Undefined ]
            primitive TypeKindPrimitive.Integer
        ]
        |> testRender "option<U2<string, int>>"
        ||> Flip.Expect.equal ""
]

// Nested unions flatten (categorize folds recursively over each member union's Types).
let nested = testList "Nested unions flatten" [
    testCase "2-layer nested primitive union flattens to U4" <| fun _ ->
        Union.create [
            primitive TypeKindPrimitive.String
            Union.create [ primitive TypeKindPrimitive.Integer; primitive TypeKindPrimitive.Boolean ]
            primitive TypeKindPrimitive.Number
        ]
        |> testRender "U4<string, int, bool, float>"
        ||> Flip.Expect.equal ""
    testCase "nested nullability lifts to the top" <| fun _ ->
        Union.create [
            primitive TypeKindPrimitive.String
            Union.create [ primitive TypeKindPrimitive.Integer; primitive TypeKindPrimitive.Null ]
        ]
        |> testRender "option<U2<string, int>>"
        ||> Flip.Expect.equal ""
]

// Conditional categorization: categorize folds over BOTH branches (True/False).
let conditional = testList "Conditional categorization" [
    testCase "conditional folds true & false branches -> U2" <| fun _ ->
        Conditional.create (primitive TypeKindPrimitive.String) (primitive TypeKindPrimitive.Integer)
        |> Conditional.wrap
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
]

// FIXED — a `true|false` literal union categorizes to the bool PRIMITIVE (renders `bool`),
// not LiteralLike (which would hoist a string-enum). Fixed at the categorization layer
// (ResolvedTypeCategorization.create collapses a bool literal to the bool primitive).
let trueFalseBug = testList "true|false is bool" [
    testCase "true | false union renders as bool, not a hoisted enum" <| fun _ ->
        Union.create [ boolLit true; boolLit false ]
        |> testRender "bool"
        ||> Flip.Expect.equal "true|false must categorize to the bool primitive, not a LiteralUnions enum"
]

// -----------------------------------------------------------------------------
// (2) DIRECT-CONTRACT — assert the bucket lists from ResolvedTypeCategories.create.
// -----------------------------------------------------------------------------

let directBuckets = testList "Direct bucket contracts" [
    testCase "string|number -> two Primitives, nothing else, not nullable" <| fun _ ->
        let c =
            Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Integer ]
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 2 c.Primitives.Length
        Flip.Expect.isEmpty "" c.LiteralLike
        Flip.Expect.isEmpty "" c.EnumLike
        Flip.Expect.isEmpty "" c.Others
        Flip.Expect.isFalse "" c.Nullable
    testCase "string|undefined -> one Primitive, Nullable=true" <| fun _ ->
        let c =
            Union.create [ primitive TypeKindPrimitive.String; primitive TypeKindPrimitive.Undefined ]
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 1 c.Primitives.Length
        Flip.Expect.isTrue "" c.Nullable
    testCase "\"a\"|\"b\" -> two LiteralLike, no Primitives" <| fun _ ->
        let c =
            Union.create [ strLit "a"; strLit "b" ]
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 2 c.LiteralLike.Length
        Flip.Expect.isEmpty "" c.Primitives
        Flip.Expect.isEmpty "" c.EnumLike
    testCase "all-nullable union -> single NonPrimitive, Nullable=true" <| fun _ ->
        // unknown/any contribute a NonPrimitive; null/undefined/void/never contribute only
        // nullability; the NonPrimitives unify to one.
        let c =
            Union.create [
                primitive TypeKindPrimitive.Null
                primitive TypeKindPrimitive.Undefined
                primitive TypeKindPrimitive.Unknown
                primitive TypeKindPrimitive.Any
            ]
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 1 c.Primitives.Length
        Flip.Expect.isTrue "" c.Nullable
    testCase "all enum cases present -> one EnumLike, no LiteralLike (simplify collapse)" <| fun _ ->
        let _, cases = buildEnum "Color" [ "Red", 0; "Blue", 1 ]
        let c =
            cases |> List.map EnumCase.wrap |> Union.create
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 1 c.EnumLike.Length
        Flip.Expect.isEmpty "" c.LiteralLike
    testCase "PARTIAL enum cases stay LiteralLike, no EnumLike collapse" <| fun _ ->
        // Only ONE of the two declared cases is present -> simplify must NOT collapse to the enum.
        let _, cases = buildEnum "Color" [ "Red", 0; "Blue", 1 ]
        let c =
            [ cases |> List.head |> EnumCase.wrap ] |> Union.create
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 1 c.LiteralLike.Length
        Flip.Expect.isEmpty "" c.EnumLike
    testCase "ReadOnly is transparent -> categorizes inner primitive" <| fun _ ->
        let c =
            ResolvedType.ReadOnly (primitive TypeKindPrimitive.String)
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "" 1 c.Primitives.Length
    // true|false buckets as a single bool PRIMITIVE, not two bool LiteralLike (fixed).
    testCase "DIRECT: true|false buckets to one bool Primitive (not LiteralLike)" <| fun _ ->
        let c =
            Union.create [ boolLit true; boolLit false ]
            |> ResolvedTypeCategories.create
        Flip.Expect.equal "true|false must collapse to a single bool primitive" 1 c.Primitives.Length
        Flip.Expect.isEmpty "true|false must NOT leave bool literals in LiteralLike" c.LiteralLike
]

[<Tests>]
let tests = testList "Categorization" [
    unionOfPrimitives
    nullability
    singleCollapse
    literalUnionsHoist
    literalUnionNameBounding
    mixedBuckets
    enumLike
    unwrapping
    nested
    conditional
    trueFalseBug
    directBuckets
]
