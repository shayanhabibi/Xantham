module Xantham.Generator.Tests.Tests.PrerenderTypeMapping

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.TypeRefRender
open Mocking.ArenaInterner.ResolvedType

// Reuse the existing prerender harness verbatim — this suite only ADDS coverage for
// ResolvedType -> rendered SHAPE cases not already exercised by TypeRefRender.fs /
// TypeRefRenderExtra.fs. The cases here are the remaining ResolvedType union arms that
// render to a CONCRETE inline shape through the bare TypeRefRender (i.e. not a hoisted
// transient anchored ref, which needs the anchored-render harness):
//   Conditional, IndexedAccess, Index, ReadOnly, Optional, TypeQuery, Substitution,
//   Predicate, Enum/Interface references, the call-signature non-inlining edge, and the
//   inline single-call-signature object literal.
let private testRender = Xantham.Generator.Tests.Tests.TypeRefRender.testRender

let private lz (t: ResolvedType) = LazyContainer.CreateFromValue t

// ── Conditional ──────────────────────────────────────────────────────────────
// `T extends U ? A : B` erases to the union of its two branches (the only two values it
// can resolve to at runtime): A | B -> U2<A, B>.
let conditionalTests = testList "Conditional" [
    testCase "primitive branches erase to union of branches" <| fun _ ->
        Conditional.create
            (primitive TypeKindPrimitive.String)
            (primitive TypeKindPrimitive.Integer)
        |> Conditional.wrap
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
    // PENDING — the Conditional arm builds a RAW erased union of [True; False] directly,
    // bypassing the Union categorisation/simplification pass. So identical branches do NOT
    // dedupe: `A extends B ? string : string` currently renders `U2<string, string>` rather
    // than collapsing to the single inhabited type `string`. A faithful erasure of a
    // conditional whose two branches are the same type is just that type.
    ptestCase "identical branches collapse to a single type" <| fun _ ->
        Conditional.create
            (primitive TypeKindPrimitive.String)
            (primitive TypeKindPrimitive.String)
        |> Conditional.wrap
        |> testRender "string"
        ||> Flip.Expect.equal ""
    // PENDING — same root cause: the raw union skips nullability lifting, so a `null`
    // branch renders as a `unit` union case (`U2<string, unit>`) instead of being lifted
    // to `option<string>` the way the Union pass treats a nullable member.
    ptestCase "nullable branch lifts to option over the union" <| fun _ ->
        Conditional.create
            (primitive TypeKindPrimitive.String)
            (primitive TypeKindPrimitive.Null)
        |> Conditional.wrap
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "array branches" <| fun _ ->
        Conditional.create
            (primitive TypeKindPrimitive.String |> Array.create)
            (primitive TypeKindPrimitive.Integer |> Array.create)
        |> Conditional.wrap
        |> testRender "U2<ResizeArray<string>, ResizeArray<int>>"
        ||> Flip.Expect.equal ""
]

// ── Index (`keyof T`) ────────────────────────────────────────────────────────
// `keyof T` is the set of property keys of T; rendered as the binding intrinsic
// `keyof<T>` (a string at runtime, typed for F#).
let indexTests = testList "Index (keyof)" [
    testCase "keyof primitive" <| fun _ ->
        ResolvedType.Index { Type = lz (primitive TypeKindPrimitive.String) }
        |> testRender "keyof<string>"
        ||> Flip.Expect.equal ""
    testCase "keyof interface" <| fun _ ->
        ResolvedType.Index {
            Type =
                Interface.create "Bar"
                |> Interface.withPath [ "Foo" ]
                |> Interface.wrap
                |> lz }
        |> testRender "keyof<Foo.Bar>"
        ||> Flip.Expect.equal ""
]

// ── IndexedAccess (`T[K]`) ───────────────────────────────────────────────────
// `T[K]` (indexed access) renders to the binding intrinsic `proptypekey<Object, Index>`.
let indexedAccessTests = testList "IndexedAccess" [
    testCase "object and index primitives" <| fun _ ->
        ResolvedType.IndexedAccess {
            Object = lz (primitive TypeKindPrimitive.String)
            Index = lz (primitive TypeKindPrimitive.Integer) }
        |> testRender "proptypekey<string, int>"
        ||> Flip.Expect.equal ""
    testCase "object interface, index literal-erased key" <| fun _ ->
        ResolvedType.IndexedAccess {
            Object =
                Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap |> lz
            Index = lz (ResolvedType.Literal (TsLiteral.String "name")) }
        |> testRender "proptypekey<Foo.Bar, string>"
        ||> Flip.Expect.equal ""
]

// ── ReadOnly ─────────────────────────────────────────────────────────────────
// `readonly T` / `Readonly<T>` is a compile-time mutability refinement with no distinct
// runtime shape in an erased binding — it transparently renders as its inner type.
let readOnlyTests = testList "ReadOnly" [
    testCase "readonly primitive is transparent" <| fun _ ->
        ResolvedType.ReadOnly (primitive TypeKindPrimitive.String)
        |> testRender "string"
        ||> Flip.Expect.equal ""
    testCase "readonly array is transparent" <| fun _ ->
        ResolvedType.ReadOnly (primitive TypeKindPrimitive.Integer |> Array.create)
        |> testRender "ResizeArray<int>"
        ||> Flip.Expect.equal ""
    testCase "readonly union is transparent" <| fun _ ->
        Union.create [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
        ]
        |> ResolvedType.ReadOnly
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
]

// ── Optional ─────────────────────────────────────────────────────────────────
// An Optional<TypeReference> wraps the referenced type in option<...>.
let optionalTests = testList "Optional" [
    testCase "optional primitive reference renders option" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> TypeReference.create
        |> ResolvedType.Optional
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "optional array reference renders option of array" <| fun _ ->
        primitive TypeKindPrimitive.Integer
        |> Array.create
        |> TypeReference.create
        |> ResolvedType.Optional
        |> testRender "option<ResizeArray<int>>"
        ||> Flip.Expect.equal ""
    testCase "optional already-optional reference does not double-wrap" <| fun _ ->
        // option<option<T>> is never the faithful mapping; nullability is idempotent.
        primitive TypeKindPrimitive.Any
        |> TypeReference.create
        |> ResolvedType.Optional
        |> testRender "option<obj>"
        ||> Flip.Expect.equal ""
]

// ── TypeQuery (`typeof x`) ───────────────────────────────────────────────────
// `typeof someValue` resolves to the type of that value and renders as that inner type.
let typeQueryTests = testList "TypeQuery (typeof)" [
    testCase "typeof primitive value" <| fun _ ->
        ResolvedType.TypeQuery {
            FullyQualifiedName = [ QualifiedNamePart.Normal "Foo" ]
            Type = lz (primitive TypeKindPrimitive.String) }
        |> testRender "string"
        ||> Flip.Expect.equal ""
    testCase "typeof interface value" <| fun _ ->
        ResolvedType.TypeQuery {
            FullyQualifiedName = [ QualifiedNamePart.Normal "Foo" ]
            Type = Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap |> lz }
        |> testRender "Foo.Bar"
        ||> Flip.Expect.equal ""
]

// ── Substitution ─────────────────────────────────────────────────────────────
// A substitution type carries a Base and a Constraint; the erased binding takes the Base.
let substitutionTests = testList "Substitution" [
    testCase "renders the Base, not the Constraint" <| fun _ ->
        ResolvedType.Substitution {
            Base = lz (primitive TypeKindPrimitive.Integer)
            Constraint = lz (primitive TypeKindPrimitive.String) }
        |> testRender "int"
        ||> Flip.Expect.equal ""
    testCase "Base array" <| fun _ ->
        ResolvedType.Substitution {
            Base = lz (primitive TypeKindPrimitive.String |> Array.create)
            Constraint = lz (primitive TypeKindPrimitive.NonPrimitive) }
        |> testRender "ResizeArray<string>"
        ||> Flip.Expect.equal ""
]

// ── Predicate (`x is T`) ─────────────────────────────────────────────────────
// A type predicate (`arg is Foo`) is a boolean refinement at the call site; in the erased
// binding the function returns a plain bool.
let predicateTests = testList "Predicate" [
    testCase "type predicate renders bool" <| fun _ ->
        ResolvedType.Predicate {
            Type = lz (Interface.create "Foo" |> Interface.wrap)
            ParameterName = Name.Camel.create "x"
            IsAssertion = false }
        |> testRender "bool"
        ||> Flip.Expect.equal ""
    testCase "assertion predicate renders bool" <| fun _ ->
        ResolvedType.Predicate {
            Type = lz (primitive TypeKindPrimitive.String)
            ParameterName = Name.Camel.create "x"
            IsAssertion = true }
        |> testRender "bool"
        ||> Flip.Expect.equal ""
]

// ── Named-type references ────────────────────────────────────────────────────
// A reference to a named interface/enum renders to its dotted qualified path.
let namedReferenceTests = testList "Named references" [
    testCase "interface with module path" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> TypeReference.create
        |> TypeReference.wrap
        |> testRender "Foo.Bar"
        ||> Flip.Expect.equal ""
    testCase "enum reference renders to its qualified path" <| fun _ ->
        // A path-less enum is anchored at the synthetic root module `Global`.
        Enum.create "Color"
        |> Enum.wrap
        |> TypeReference.create
        |> TypeReference.wrap
        |> testRender "Global.Color"
        ||> Flip.Expect.equal ""
    testCase "array of interface reference" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> Array.create
        |> testRender "ResizeArray<Foo.Bar>"
        ||> Flip.Expect.equal ""
    testCase "tuple of named references" <| fun _ ->
        [
            Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap
            |> Tuple.createElement
            Enum.create "Color" |> Enum.wrap
            |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "Foo.Bar * Global.Color"
        ||> Flip.Expect.equal ""
]

// ── Inline object-literal call signature (the inlining edge) ─────────────────
// A TypeLiteral whose ONLY member is a single call signature with < 3 params and no
// spread inlines to an F# function type rather than hoisting a named nested type.
let inlineCallSignatureTests = testList "Inline object-literal call signatures" [
    testCase "two-parameter signature inlines" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Integer |> Parameter.create "a"
            primitive TypeKindPrimitive.Boolean |> Parameter.create "b"
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "int -> bool -> string"
        ||> Flip.Expect.equal ""
    testCase "optional parameter inlines as option in signature" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Integer |> Parameter.create "a"
            primitive TypeKindPrimitive.Boolean |> Parameter.create "b" |> Parameter.optional
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "int -> option<bool> -> string"
        ||> Flip.Expect.equal ""
    testCase "empty object literal renders option<obj>" <| fun _ ->
        // `{}` carries no structure to bind against in an erased binding.
        TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "option<obj>"
        ||> Flip.Expect.equal ""
]

[<Tests>]
let tests = testList "Prerender Type Mapping" [
    conditionalTests
    indexTests
    indexedAccessTests
    readOnlyTests
    optionalTests
    typeQueryTests
    substitutionTests
    predicateTests
    namedReferenceTests
    inlineCallSignatureTests
]
