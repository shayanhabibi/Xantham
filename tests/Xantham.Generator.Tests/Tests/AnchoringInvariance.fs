module Xantham.Generator.Tests.Tests.AnchoringInvariance

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

// ===========================================================================
// This suite isolates the ANCHORING / PLACEMENT invariance property at its
// core: a ConcretePath atom (an already-absolute `TypePath` carried by an
// interface/class/enum ref) is RE-ANCHOR-INVARIANT. `Anchored.TypeRefAtom.anchor`
// (RenderScope.Anchored.fs:48-59) maps `ConcretePath path -> Path path` by
// identity, so no anchor's module trace is ever prepended. Contrast a raw
// `TransientTypePath`, whose `.anchor` (NamePath.fs:497) prepends the anchor's
// module trace and is therefore NON-idempotent (re-anchoring against a second
// anchor yields a DIFFERENT absolute path). That divergence is exactly why the
// path-system fixes require every cross-reference to carry a ConcretePath, not
// a transient: a ConcretePath cannot double-graft.
//
// Coverage:
//   (1) ConcretePath invariance across ALL AnchorPath kinds + double-anchor idempotence
//   (2) deeply-nested A.B.C.Bar survives anchoring
//   (3) molecule recursion: Tuple / Array / Union / generic Prefix
//   (4) localisation: shared-prefix shortening vs disjoint full-qualification
//   (5) NON-idempotence characterization of a raw transient across two anchors
// ===========================================================================

// Build a few more anchor flavours so invariance is shown across the WHOLE
// AnchorPath surface (Type / Member / Parameter / Module / TypeParam).
let private moduleAnchor =
    ModulePath.createFromList [ "Deep"; "Nest"; "Mod" ] |> AnchorPath.Module
let private memberAnchor =
    ModulePath.init "Svc" |> TypePath.create "Client" |> MemberPath.createOnType "send" |> AnchorPath.Member
let private typeParamAnchor =
    ModulePath.init "Generic"
    |> TypePath.create "Box"
    |> fun tp -> TypeParamPath.createOnType (Name.Typar.create "T") tp
    |> AnchorPath.TypeParam

let private concreteFooBar () =
    Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap

// ---------------------------------------------------------------------------
// (1) ConcretePath re-anchor invariance across ALL AnchorPath kinds.
// `Foo.Bar` must render `Foo.Bar` regardless of which anchor it is anchored at.
// ---------------------------------------------------------------------------
let concretePathAllAnchorsTests = testList "ConcretePath invariance across all AnchorPath kinds" [
    testCase "Type anchor leaves Foo.Bar unchanged" <| fun _ ->
        concreteFooBar ()
        |> testAnchoredRender typeAnchor "Foo.Bar"
        ||> Flip.Expect.equal "Type anchor must not prepend its module trace to a ConcretePath"

    testCase "Member anchor leaves Foo.Bar unchanged" <| fun _ ->
        concreteFooBar ()
        |> testAnchoredRender memberAnchor "Foo.Bar"
        ||> Flip.Expect.equal "Member anchor must not prepend its module trace to a ConcretePath"

    testCase "Parameter anchor leaves Foo.Bar unchanged" <| fun _ ->
        concreteFooBar ()
        |> testAnchoredRender parameterAnchor "Foo.Bar"
        ||> Flip.Expect.equal "Parameter anchor must not prepend its module trace to a ConcretePath"

    testCase "Module anchor leaves Foo.Bar unchanged" <| fun _ ->
        concreteFooBar ()
        |> testAnchoredRender moduleAnchor "Foo.Bar"
        ||> Flip.Expect.equal "Module anchor must not prepend its module trace to a ConcretePath"

    testCase "TypeParam anchor leaves Foo.Bar unchanged" <| fun _ ->
        concreteFooBar ()
        |> testAnchoredRender typeParamAnchor "Foo.Bar"
        ||> Flip.Expect.equal "TypeParam anchor must not prepend its module trace to a ConcretePath"

    testCase "anchoring a ConcretePath twice (two different anchors) is idempotent" <| fun _ ->
        // The load-bearing property: re-anchoring a ConcretePath never double-grafts.
        // Anchoring the SAME source ref against two unrelated anchors must render identically.
        let renderAt anchor =
            concreteFooBar ()
            |> TestHelper.prerender ctx
            |> Anchored.TypeRefRender.anchor anchor
            |> renderAnchored ""
            |> snd
        Flip.Expect.equal
            "ConcretePath anchored at Module vs Parameter renders identically (idempotent)"
            (renderAt moduleAnchor)
            (renderAt parameterAnchor)
]

// ---------------------------------------------------------------------------
// (2) Deeply-nested ConcretePath A.B.C.Bar survives anchoring whole.
// ---------------------------------------------------------------------------
let deepNestTests = testList "Deeply nested ConcretePath survives anchoring" [
    testCase "A.B.C.Bar renders fully-qualified under a Parameter anchor" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "A"; "B"; "C" ]
        |> Interface.wrap
        |> testAnchoredRender parameterAnchor "A.B.C.Bar"
        ||> Flip.Expect.equal "deep ConcretePath trace is preserved verbatim — no segment dropped or prepended"

    testCase "A.B.C.Bar renders identically under a disjoint Type anchor" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "A"; "B"; "C" ]
        |> Interface.wrap
        |> testAnchoredRender typeAnchor "A.B.C.Bar"
        ||> Flip.Expect.equal "deep ConcretePath is anchor-independent"
]

// ---------------------------------------------------------------------------
// (3) Molecule recursion: anchoring descends into Tuple / Array / Union /
// generic Prefix and anchors each inner atom (ConcretePath stays invariant,
// primitives stay intrinsic).
// ---------------------------------------------------------------------------
let moleculeRecursionTests = testList "Molecule recursion anchors inner atoms" [
    // Array -> Prefix(ResizeArray, [inner]). Inner ConcretePath must survive.
    testCase "ResizeArray<Foo.Bar> anchors its inner ConcretePath" <| fun _ ->
        concreteFooBar ()
        |> Array.create
        |> testAnchoredRender parameterAnchor "ResizeArray<Foo.Bar>"
        ||> Flip.Expect.equal "Array Prefix molecule: inner ConcretePath stays Foo.Bar"

    // Tuple of a ConcretePath and a primitive.
    testCase "tuple (Foo.Bar * int) anchors each element" <| fun _ ->
        [
            concreteFooBar () |> Tuple.createElement
            primitive TypeKindPrimitive.Integer |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testAnchoredRender parameterAnchor "Foo.Bar * int"
        ||> Flip.Expect.equal "tuple recursion: ConcretePath element invariant, primitive element intrinsic"

    // Tuple of two distinct ConcretePaths at different module roots.
    testCase "tuple of two disjoint ConcretePaths keeps both full traces" <| fun _ ->
        [
            (Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap) |> Tuple.createElement
            (Interface.create "Quux" |> Interface.withPath [ "Baz" ] |> Interface.wrap) |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testAnchoredRender typeAnchor "Foo.Bar * Baz.Quux"
        ||> Flip.Expect.equal "both ConcretePath elements anchor-invariant"

    // U2 union of a named ConcretePath and a primitive.
    testCase "U2<Foo.Bar, string> anchors named member, leaves primitive intrinsic" <| fun _ ->
        [
            concreteFooBar ()
            primitive TypeKindPrimitive.String
        ]
        |> Union.create
        |> testAnchoredRender parameterAnchor "U2<Foo.Bar, string>"
        ||> Flip.Expect.equal "Union molecule: named member is invariant ConcretePath, primitive is intrinsic"

    // U3 union of two named ConcretePaths and a primitive.
    testCase "U3<Foo.Bar, Baz.Quux, int> anchors both named members" <| fun _ ->
        [
            Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap
            Interface.create "Quux" |> Interface.withPath [ "Baz" ] |> Interface.wrap
            primitive TypeKindPrimitive.Integer
        ]
        |> Union.create
        |> testAnchoredRender typeAnchor "U3<Foo.Bar, Baz.Quux, int>"
        ||> Flip.Expect.equal "all three union members anchored correctly"

    // Generic Prefix: TypeReference with type arguments -> Prefix(prefixHead, args).
    // A non-lib interface with declared type parameters, applied with a ConcretePath arg.
    testCase "generic Prefix Foo.Bar<Baz.Quux> anchors prefix head and arg" <| fun _ ->
        (Interface.create "Bar"
         |> Interface.withPath [ "Foo" ]
         |> Interface.withTypeParameters [ TypeParameter.create "T" ]
         |> Interface.wrap)
        |> TypeReference.create
        |> TypeReference.withTypeArguments [
            Interface.create "Quux" |> Interface.withPath [ "Baz" ] |> Interface.wrap
        ]
        |> TypeReference.wrap
        |> testAnchoredRender parameterAnchor "Foo.Bar<Baz.Quux>"
        ||> Flip.Expect.equal "generic Prefix: head ConcretePath and ConcretePath arg both invariant"

    // Generic Prefix with a primitive argument.
    testCase "generic Prefix Foo.Bar<string> anchors prefix head, leaves primitive arg" <| fun _ ->
        (Interface.create "Bar"
         |> Interface.withPath [ "Foo" ]
         |> Interface.withTypeParameters [ TypeParameter.create "T" ]
         |> Interface.wrap)
        |> TypeReference.create
        |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
        |> TypeReference.wrap
        |> testAnchoredRender typeAnchor "Foo.Bar<string>"
        ||> Flip.Expect.equal "generic Prefix head invariant, primitive arg intrinsic"

    // Nested molecules: ResizeArray of a tuple of ConcretePaths.
    testCase "ResizeArray<Foo.Bar * Baz.Quux> recurses through nested molecules" <| fun _ ->
        [
            (Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap) |> Tuple.createElement
            (Interface.create "Quux" |> Interface.withPath [ "Baz" ] |> Interface.wrap) |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> Array.create
        |> testAnchoredRender parameterAnchor "ResizeArray<Foo.Bar * Baz.Quux>"
        ||> Flip.Expect.equal "nested Prefix->Tuple molecule: both inner ConcretePaths invariant"
]

// ---------------------------------------------------------------------------
// (4) Localisation via testAnchoredRelativeRender. A ConcretePath sharing the
// relative anchor's module prefix is SHORTENED; a disjoint one stays fully
// qualified.
// ---------------------------------------------------------------------------
let localisationTests = testList "Localisation shortens shared-prefix paths" [
    testCase "ref sharing the relative anchor's module collapses to its leaf" <| fun _ ->
        // Interface at Root.Bar; localised relative to a Type living in Root ->
        // the shared Root module prefix is stripped, leaving `Bar`.
        let relative =
            ModulePath.init "Root"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Interface.create "Bar"
        |> Interface.withPath [ "Root" ]
        |> Interface.wrap
        |> testAnchoredRelativeRender relative parameterAnchor "Bar"
        ||> Flip.Expect.equal "shared Root module prefix stripped by localise"

    testCase "ref sharing a DEEP module prefix is shortened to its tail" <| fun _ ->
        // Interface at Shared.Deep.Bar; localised relative to Shared.Other.Local ->
        // common prefix `Shared` stripped, leaving `Deep.Bar`.
        let relative =
            ModulePath.createFromList [ "Shared"; "Other" ]
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Interface.create "Bar"
        |> Interface.withPath [ "Shared"; "Deep" ]
        |> Interface.wrap
        |> testAnchoredRelativeRender relative parameterAnchor "Deep.Bar"
        ||> Flip.Expect.equal "common Shared prefix stripped, Deep.Bar retained"

    testCase "ref in a disjoint module keeps its full path under localisation" <| fun _ ->
        let relative =
            ModulePath.init "Elsewhere"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> testAnchoredRelativeRender relative parameterAnchor "Foo.Bar"
        ||> Flip.Expect.equal "no shared module prefix — full path retained"

    testCase "localisation recurses into molecules: only the shared member is shortened" <| fun _ ->
        // U2<Root.Bar, Foo.Quux> localised relative to a Type in Root.
        // Root.Bar -> Bar (shared), Foo.Quux stays fully qualified.
        let relative =
            ModulePath.init "Root"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        [
            Interface.create "Bar" |> Interface.withPath [ "Root" ] |> Interface.wrap
            Interface.create "Quux" |> Interface.withPath [ "Foo" ] |> Interface.wrap
        ]
        |> Union.create
        |> testAnchoredRelativeRender relative parameterAnchor "U2<Bar, Foo.Quux>"
        ||> Flip.Expect.equal "localisation descends into Union: shared member shortened, disjoint stays full"
]

// ---------------------------------------------------------------------------
// (5) NON-idempotence characterization of the RAW transient — the property that
// makes ConcretePath necessary. `TransientTypePath.anchor` prepends the anchor's
// module trace, so the SAME transient anchored against two DIFFERENT anchors
// yields two DIFFERENT absolute paths (anchoring is anchor-sensitive, not a
// no-op). A ConcretePath, by contrast, is identical across anchors (covered in
// (1)) — that is the precise reason cross-references must be ConcretePaths.
// ---------------------------------------------------------------------------
let private flat (typePath: TypePath) =
    TypePath.flatten typePath |> List.map Name.Case.valueOrModified

let nonIdempotenceTests = testList "Raw transient anchor is NON-idempotent (justifies ConcretePath)" [
    testCase "same transient at two different anchors yields different absolute paths" <| fun _ ->
        let transient = TransientTypePath.AnchoredAndMoored (Name.Pascal.create "Child")
        let anchorA = ModulePath.init "Alpha" |> TypePath.create "A" |> AnchorPath.Type
        let anchorB = ModulePath.init "Beta"  |> TypePath.create "B" |> AnchorPath.Type
        let underA = TransientTypePath.anchor anchorA transient |> flat
        let underB = TransientTypePath.anchor anchorB transient |> flat
        underA |> Flip.Expect.equal "anchored under Alpha.A" [ "Alpha"; "A"; "Child" ]
        underB |> Flip.Expect.equal "anchored under Beta.B"  [ "Beta"; "B"; "Child" ]
        Flip.Expect.notEqual "transient anchoring is anchor-sensitive — NOT idempotent" underA underB

    testCase "re-anchoring a transient against a DEEPER anchor double-grafts (path grows)" <| fun _ ->
        // Characterize the double-graft hazard concretely: anchoring against a
        // parameter anchor threads the full member/parameter trace in. A second
        // (deeper) anchor produces a yet-longer path — re-anchoring is additive,
        // never a fixpoint. This is why an already-anchored result must be carried
        // as a ConcretePath, not re-fed to `.anchor`.
        let transient = TransientTypePath.AnchoredAndMoored (Name.Pascal.create "Opt")
        let shallow = TransientTypePath.anchor typeAnchor transient |> flat
        let deep = TransientTypePath.anchor parameterAnchor transient |> flat
        shallow |> Flip.Expect.equal "type anchor trace" [ "Other"; "Place"; "Local"; "Opt" ]
        deep |> Flip.Expect.equal "parameter anchor threads member+param" [ "Root"; "Holder"; "Member"; "Para"; "Opt" ]
        Flip.Expect.isGreaterThan
            "the deeper (parameter) anchor yields a strictly longer absolute trace"
            (List.length deep, List.length shallow)

    testCase "ConcretePath flatten is anchor-free (the contrasting invariant)" <| fun _ ->
        // A ConcretePath carries its full absolute trace and has NO anchor parameter:
        // flattening is the same value no matter what anchor context exists. This is
        // the structural reason `Anchored.TypeRefAtom.anchor` returns ConcretePath by
        // identity, and why it can never double-graft.
        let concrete = ModulePath.createFromList [ "Foo"; "Inner" ] |> TypePath.create "Bar"
        flat concrete
        |> Flip.Expect.equal "concrete path is its own absolute trace" [ "Foo"; "Inner"; "Bar" ]
]

[<Tests>]
let tests = testList "AnchoringInvariance" [
    concretePathAllAnchorsTests
    deepNestTests
    moleculeRecursionTests
    localisationTests
    nonIdempotenceTests
]
