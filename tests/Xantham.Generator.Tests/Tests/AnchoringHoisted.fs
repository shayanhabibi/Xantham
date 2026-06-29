module Xantham.Generator.Tests.Tests.AnchoringHoisted

// Isolated test suite for the ANCHORING / PLACEMENT pass, focused on the
// HOISTED-TRANSIENT arms of `prerender` (RenderScope.Prelude.fs):
//
//   * object-literal `TypeLiteral` with >= 3 members  (TypeLiteral arm, `_,_` branch)
//   * a single inline call-signature `TypeLiteral`     (function-shape branch)
//   * the empty `{}` `TypeLiteral`                      (`[], []` branch -> obj)
//   * `Intersection` with >= 2 members                 (Intersection arm)
//   * `TemplateLiteral`                                (TemplateLiteral arm)
//
// Each of the genuinely-hoisted arms produces a `TypeRefRender` whose atom is a
// `TransientTypePath.Anchored` — a RELATIVE, unnamed path that carries NO module
// trace of its own. The ANCHORING pass (`TransientTypePath.anchor`, NamePath.fs
// ~497) resolves that transient against the OWNER `AnchorPath` by prepending the
// owner's full module trace. Because the transient is `Anchored` (empty
// `toAnchored`), the result COLLAPSES to exactly the owner anchor's own absolute
// path — the hoisted type lives AT its owner, no leaf name of its own, and
// crucially NO double-prefixing.
//
// That collapse is precisely why the SAME hoisted `ResolvedType` referenced from
// two DIFFERENT owners resolves to two DIFFERENT absolute paths: `anchor` is
// NON-idempotent / anchor-sensitive (it has nothing of its own to anchor, so it
// simply becomes whichever owner anchors it). This is the structural seed of the
// hoist-dedup divergence captured in MEMORY — characterised below.
//
// The harness renders only the hoisted type's REFERENCE (its `TypeRef`), via the
// `let _: <ref> = JS.undefined` form, so the assertions pin the absolute PATH the
// owner's anchor produces, not the emitted record body.

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Xantham.Generator.Generator.TypeRefRender
open Mocking.ArenaInterner.ResolvedType

let private ctx = GeneratorContext.Empty

let private renderAnchored (expectedTypeText: string) (ref: Anchored.TypeRefRender) =
    $"let _: %s{expectedTypeText} = JS.undefined",
    Ast.Oak() { Ast.AnonymousModule() { Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref) } }
    |> Gen.mkOak |> Gen.run |> _.Trim()

let private testAnchoredRender (anchorPosition: AnchorPath) (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender ctx ref
    |> Anchored.TypeRefRender.anchor anchorPosition
    |> renderAnchored expectedTypeText

let private testAnchoredRelativeRender (relativePosition: AnchorPath) (anchorPosition: AnchorPath) (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender ctx ref
    |> Anchored.TypeRefRender.anchor anchorPosition
    |> Anchored.TypeRefRender.localise relativePosition
    |> renderAnchored expectedTypeText

let private parameterAnchor =
    ModulePath.init "Root" |> TypePath.create "Holder" |> MemberPath.createOnType "member" |> ParameterPath.create "para" |> AnchorPath.Parameter
let private typeAnchor =
    ModulePath.createFromList [ "Other"; "Place" ] |> TypePath.create "Local" |> AnchorPath.Type

// A representative Member anchor: Root.Owner.field
let private memberAnchor =
    ModulePath.init "Root"
    |> TypePath.create "Owner"
    |> MemberPath.createOnType "field"
    |> AnchorPath.Member

// ---------------------------------------------------------------------------
// Builders for the hoisted shapes.
// ---------------------------------------------------------------------------

// An object-literal TypeLiteral with >= 3 plain properties — this HOISTS
// (RenderScope.Prelude.fs TypeLiteral arm, `_, _` catch-all branch -> Anchored).
let private threePropObjectLiteral =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        Property.create "alpha" (primitive TypeKindPrimitive.String) |> Property.wrap
        Property.create "beta" (primitive TypeKindPrimitive.Number) |> Property.wrap
        Property.create "gamma" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
    ]
    |> TypeLiteral.wrap

// An Intersection of two interfaces — always hoists (Intersection arm -> Anchored).
let private twoMemberIntersection =
    ResolvedType.Intersection {
        Types =
            [
                Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap
                Interface.create "Baz" |> Interface.withPath [ "Foo" ] |> Interface.wrap
            ]
            |> List.map LazyContainer.CreateFromValue
    }

let private templateLiteral =
    ResolvedType.TemplateLiteral {
        Text = [ "prefix-"; "-suffix" ]
        Types = [ primitive TypeKindPrimitive.String |> LazyContainer.CreateFromValue ]
    }

// ---------------------------------------------------------------------------
// OBJECT-LITERAL TypeLiteral (>=3 props): hoists; anchors AT its owner.
// ---------------------------------------------------------------------------
let objectLiteralHoistTests = testList "object-literal TypeLiteral hoist anchoring" [
    testCase "hoisted object-literal anchored at a Type owner collapses to the owner path" <| fun _ ->
        // Transient `Anchored` has no name of its own; anchoring at Other.Place.Local
        // yields exactly Other.Place.Local — the owner's absolute path, NO double-prefix.
        threePropObjectLiteral
        |> testAnchoredRender typeAnchor "Other.Place.Local"
        ||> Flip.Expect.equal "hoisted object-literal lives AT its Type owner with no extra leaf"

    testCase "hoisted object-literal anchored at a Member owner collapses to the owner path" <| fun _ ->
        // Member anchor Root.Owner.field -> the member becomes the leaf of the
        // anchored type path (Root.Owner.Field). The hoisted literal lives there.
        threePropObjectLiteral
        |> testAnchoredRender memberAnchor "Root.Owner.Field"
        ||> Flip.Expect.equal "hoisted object-literal lives AT its Member owner with no extra leaf"

    testCase "hoisted object-literal anchored at a Parameter owner threads member+parameter" <| fun _ ->
        threePropObjectLiteral
        |> testAnchoredRender parameterAnchor "Root.Holder.Member.Para"
        ||> Flip.Expect.equal "hoisted object-literal at a Parameter owner anchors to the parameter's type path"
]

// ---------------------------------------------------------------------------
// NO-DOUBLE-PREFIX + anchor-sensitivity (the hoist-dedup seed).
// ---------------------------------------------------------------------------
let objectLiteralOwnerSensitivityTests = testList "hoisted object-literal owner-sensitivity" [
    testCase "SAME hoisted ResolvedType under two owners renders two different paths" <| fun _ ->
        // One ResolvedType instance, two owners. Because the transient is `Anchored`
        // (nothing of its own), each owner's anchor produces THAT owner's path — the
        // type DIVERGES per owner. This is the structural reason a shared hoisted type
        // cannot keep a single canonical name purely through anchoring: there is no
        // owner-independent identity to anchor.
        let shared = threePropObjectLiteral
        let underType = shared |> testAnchoredRender typeAnchor "Other.Place.Local" |> snd
        let underMember = shared |> testAnchoredRender memberAnchor "Root.Owner.Field" |> snd
        Flip.Expect.notEqual
            "an Anchored transient is anchor-sensitive — the same hoisted type renders differently per owner"
            underType underMember

    testCase "re-anchoring against the SAME owner is stable (no double-graft)" <| fun _ ->
        // Anchoring twice against the same owner must give the same absolute path —
        // the anchor does not accumulate the owner's trace a second time.
        let first = threePropObjectLiteral |> testAnchoredRender typeAnchor "Other.Place.Local" |> snd
        let second = threePropObjectLiteral |> testAnchoredRender typeAnchor "Other.Place.Local" |> snd
        Flip.Expect.equal "same owner, same path — no double-prefix on re-anchor" first second
]

// ---------------------------------------------------------------------------
// INLINE call-signature TypeLiteral (<3 params, no spread): does NOT hoist;
// renders inline as a function type. Anchor-invariant in shape.
// ---------------------------------------------------------------------------
let inlineCallSignatureTests = testList "inline call-signature TypeLiteral (non-hoisting)" [
    testCase "single inline call-signature renders as a function type at a Type anchor" <| fun _ ->
        TypeLiteral.empty
        |> TypeLiteral.withMembers [
            CallSignature.create (primitive TypeKindPrimitive.Boolean)
            |> CallSignature.withParameters [
                Parameter.create "x" (primitive TypeKindPrimitive.String)
                Parameter.create "y" (primitive TypeKindPrimitive.Number)
            ]
            |> List.singleton
            |> CallSignature.wrap
        ]
        |> TypeLiteral.wrap
        |> testAnchoredRender typeAnchor "string -> float -> bool"
        ||> Flip.Expect.equal "an inline call-signature is a function type, not a hoisted named type"

    testCase "inline call-signature is anchor-invariant (Member anchor same shape)" <| fun _ ->
        TypeLiteral.empty
        |> TypeLiteral.withMembers [
            CallSignature.create (primitive TypeKindPrimitive.Boolean)
            |> CallSignature.withParameters [
                Parameter.create "x" (primitive TypeKindPrimitive.String)
                Parameter.create "y" (primitive TypeKindPrimitive.Number)
            ]
            |> List.singleton
            |> CallSignature.wrap
        ]
        |> TypeLiteral.wrap
        |> testAnchoredRender memberAnchor "string -> float -> bool"
        ||> Flip.Expect.equal "function-shape ref does not depend on the owner anchor"
]

// ---------------------------------------------------------------------------
// EMPTY {} TypeLiteral: `[], []` branch -> nullable obj, NOT hoisted.
// ---------------------------------------------------------------------------
let emptyObjectLiteralTests = testList "empty {} TypeLiteral (obj fallback)" [
    testCase "empty object-literal renders as obj option at a Type anchor" <| fun _ ->
        TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testAnchoredRender typeAnchor "option<obj>"
        ||> Flip.Expect.equal "empty {} falls through to nullable obj — never hoisted"

    testCase "empty object-literal is anchor-invariant at a Member anchor" <| fun _ ->
        TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testAnchoredRender memberAnchor "option<obj>"
        ||> Flip.Expect.equal "obj fallback ignores the owner anchor"
]

// ---------------------------------------------------------------------------
// INTERSECTION (>=2 members): always hoists; anchors AT its owner.
// ---------------------------------------------------------------------------
let intersectionHoistTests = testList "Intersection hoist anchoring" [
    testCase "intersection anchored at a Type owner collapses to the owner path" <| fun _ ->
        twoMemberIntersection
        |> testAnchoredRender typeAnchor "Other.Place.Local"
        ||> Flip.Expect.equal "hoisted intersection lives AT its Type owner — no extra leaf, no double-prefix"

    testCase "intersection anchored at a Member owner collapses to the owner path" <| fun _ ->
        twoMemberIntersection
        |> testAnchoredRender memberAnchor "Root.Owner.Field"
        ||> Flip.Expect.equal "hoisted intersection lives AT its Member owner"

    testCase "SAME intersection ResolvedType under two owners diverges" <| fun _ ->
        let shared = twoMemberIntersection
        let underType = shared |> testAnchoredRender typeAnchor "Other.Place.Local" |> snd
        let underMember = shared |> testAnchoredRender memberAnchor "Root.Owner.Field" |> snd
        Flip.Expect.notEqual
            "an Anchored intersection is owner-sensitive — diverges across owners"
            underType underMember
]

// ---------------------------------------------------------------------------
// TEMPLATE LITERAL: always hoists; anchors AT its owner.
// ---------------------------------------------------------------------------
let templateLiteralHoistTests = testList "TemplateLiteral hoist anchoring" [
    testCase "template literal anchored at a Type owner collapses to the owner path" <| fun _ ->
        templateLiteral
        |> testAnchoredRender typeAnchor "Other.Place.Local"
        ||> Flip.Expect.equal "hoisted template literal lives AT its Type owner"

    testCase "template literal anchored at a Member owner collapses to the owner path" <| fun _ ->
        templateLiteral
        |> testAnchoredRender memberAnchor "Root.Owner.Field"
        ||> Flip.Expect.equal "hoisted template literal lives AT its Member owner"
]

// ---------------------------------------------------------------------------
// NESTED hoist: an object-literal whose property is itself a hoisting
// object-literal. The OUTER ref is what an owner anchors; assert it collapses
// to the owner path exactly as a flat hoist does (the inner hoist is a child of
// the outer transient scope, not part of the outer's own reference).
// ---------------------------------------------------------------------------
let nestedHoistTests = testList "nested object-literal hoist anchoring" [
    testCase "nested object-literal outer ref anchors to the owner path" <| fun _ ->
        let inner =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "a" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "b" (primitive TypeKindPrimitive.Number) |> Property.wrap
                Property.create "c" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        let outer =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "nested" inner |> Property.wrap
                Property.create "x" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "y" (primitive TypeKindPrimitive.Number) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        outer
        |> testAnchoredRender typeAnchor "Other.Place.Local"
        ||> Flip.Expect.equal "outer hoisted object-literal anchors to the owner; inner hoist is a child of its scope"

    testCase "nested object-literal outer ref anchors to a Member owner" <| fun _ ->
        let inner =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "a" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "b" (primitive TypeKindPrimitive.Number) |> Property.wrap
                Property.create "c" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        let outer =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "nested" inner |> Property.wrap
                Property.create "x" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "y" (primitive TypeKindPrimitive.Number) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        outer
        |> testAnchoredRender memberAnchor "Root.Owner.Field"
        ||> Flip.Expect.equal "outer hoisted object-literal anchors to the Member owner"
]

[<Tests>]
let tests = testList "AnchoringHoisted" [
    objectLiteralHoistTests
    objectLiteralOwnerSensitivityTests
    inlineCallSignatureTests
    emptyObjectLiteralTests
    intersectionHoistTests
    templateLiteralHoistTests
    nestedHoistTests
]
