module Xantham.Generator.Tests.Tests.AnchoringPlacement

// Isolated test suite for the ANCHORING / PLACEMENT pass.
//
// This pass converts a TRANSIENT (relative) path into an ANCHORED (absolute)
// path by prepending the module trace of a concrete AnchorPath. It is the
// double-graft-prone subsystem: `TransientTypePath.anchor` is NON-idempotent
// (re-anchoring an already-anchored result against a different anchor yields a
// different absolute path), while a `ConcretePath` atom is re-anchor-INVARIANT
// (`TypeRefAtom.anchor` maps ConcretePath -> Path by identity). The path-system
// fixes rely on that invariance.
//
// Two layers are exercised:
//   1. The RENDER pipeline: prerender -> TypeRefRender.anchor -> render, asserting
//      the rendered F# string. This revives the commented-out
//      `testAnchoredRender` / `testAnchoredRelativeRender` helpers from
//      TypeRefRender.fs, adapted to the current `Anchored` API (anchor produces an
//      Anchored.TypeRefRender; render is SRTP over both Prelude and Anchored).
//   2. The NamePath functions directly (TransientTypePath.anchor /
//      TypePath.flatten / Path.getRelativePath) with constructed paths.

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

let ctx = GeneratorContext.Empty

// Render an anchored TypeRefRender to its `let _: <type> = JS.undefined` form.
let private renderAnchored (expectedTypeText: string) (ref: Anchored.TypeRefRender) =
    $"let _: %s{expectedTypeText} = JS.undefined",
    Ast.Oak() {
        Ast.AnonymousModule() {
            Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref)
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// Render an un-anchored (Prelude) TypeRefRender — used for baselines.
let private renderPrelude (expectedTypeText: string) (ref: Prelude.TypeRefRender) =
    $"let _: %s{expectedTypeText} = JS.undefined",
    Ast.Oak() {
        Ast.AnonymousModule() {
            Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref)
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// Revived `testAnchoredRender`: prerender, anchor at the given AnchorPath, render.
let private testAnchoredRender (anchorPosition: AnchorPath) (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender ctx ref
    |> Anchored.TypeRefRender.anchor anchorPosition
    |> renderAnchored expectedTypeText

// Revived `testAnchoredRelativeRender`: prerender, anchor at `anchorPosition`,
// then localise (shorten paths sharing a module prefix) against `relativePosition`.
let private testAnchoredRelativeRender
    (relativePosition: AnchorPath)
    (anchorPosition: AnchorPath)
    (expectedTypeText: string)
    (ref: ResolvedType) =
    TestHelper.prerender ctx ref
    |> Anchored.TypeRefRender.anchor anchorPosition
    |> Anchored.TypeRefRender.localise relativePosition
    |> renderAnchored expectedTypeText

// A representative concrete anchor: Root.Holder.member.para
let private parameterAnchor =
    ModulePath.init "Root"
    |> TypePath.create "Holder"
    |> MemberPath.createOnType "member"
    |> ParameterPath.create "para"
    |> AnchorPath.Parameter

let private typeAnchor =
    ModulePath.createFromList [ "Other"; "Place" ]
    |> TypePath.create "Local"
    |> AnchorPath.Type

// ---------------------------------------------------------------------------
// RENDER-LEVEL: ConcretePath atoms are re-anchor-INVARIANT.
// An interface prerenders to a ConcretePath atom. Anchoring it against ANY
// AnchorPath must leave its absolute path untouched (Foo.Bar stays Foo.Bar).
// ---------------------------------------------------------------------------
let concretePathInvarianceTests = testList "ConcretePath re-anchor invariance" [
    testCase "interface renders to its absolute path unanchored (baseline)" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> fun ref ->
            TestHelper.prerender ctx ref
            |> renderPrelude "Foo.Bar"
        ||> Flip.Expect.equal "interface Foo.Bar renders as its absolute path"

    testCase "anchoring a ConcretePath at a Parameter anchor leaves it unchanged" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> testAnchoredRender parameterAnchor "Foo.Bar"
        ||> Flip.Expect.equal "ConcretePath is re-anchor invariant — the anchor's module trace must NOT be prepended"

    testCase "anchoring a ConcretePath at a Type anchor leaves it unchanged" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> testAnchoredRender typeAnchor "Foo.Bar"
        ||> Flip.Expect.equal "anchoring against a different anchor still yields Foo.Bar"

    testCase "anchoring a ConcretePath TWICE at different anchors is idempotent" <| fun _ ->
        // The key property the path-system fixes rely on: re-anchoring a
        // ConcretePath atom never double-grafts.
        let once =
            Interface.create "Bar"
            |> Interface.withPath [ "Foo" ]
            |> Interface.wrap
            |> TestHelper.prerender ctx
            |> Anchored.TypeRefRender.anchor parameterAnchor
        let twice =
            once
            |> fun r ->
                // re-anchor the already-anchored render's Prelude-equivalent is
                // not directly possible (anchor consumes a Prelude render), so we
                // re-anchor from the source against a *second* anchor and assert
                // both render identically.
                Interface.create "Bar"
                |> Interface.withPath [ "Foo" ]
                |> Interface.wrap
                |> TestHelper.prerender ctx
                |> Anchored.TypeRefRender.anchor typeAnchor
        let renderOf r = renderAnchored "" r |> snd
        Flip.Expect.equal
            "ConcretePath anchored against two different anchors renders identically"
            (renderOf once)
            (renderOf twice)
]

// ---------------------------------------------------------------------------
// RENDER-LEVEL: nested ConcretePath inside molecules is also invariant.
// ---------------------------------------------------------------------------
let nestedAnchorTests = testList "Nested anchoring through molecules" [
    // BUG SURFACED BY THIS SUITE: the anchored render path cannot render an
    // `Intrinsic` atom. `Anchored.TypeRefAtom.anchor` (RenderScope.Anchored.fs)
    // faithfully maps a Prelude `Intrinsic` to an Anchored `Intrinsic`, but
    // `Implementation.Anchored.renderAtom` (TypeRefRender.Render.fs:77-82) only
    // matches `Widget` and `Path` — so any anchored render that reaches an
    // intrinsic (a bare primitive, OR the `ResizeArray`/`U2` prefix head, OR a
    // primitive tuple element) throws "match cases were incomplete".
    // The three cases below now PASS: the anchored renderer handles `Intrinsic`
    // (mirroring the Prelude renderer's `TypeRefAtom.Intrinsic s -> Ast.LongIdent [ s ]`).

    // ResizeArray prefix head is an Intrinsic.
    testCase "ConcretePath inside a ResizeArray survives anchoring" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> Array.create
        |> testAnchoredRender parameterAnchor "ResizeArray<Foo.Bar>"
        ||> Flip.Expect.equal "anchoring recurses into the Prefix molecule; inner ConcretePath stays Foo.Bar"

    // Bare primitive is an Intrinsic.
    testCase "primitive intrinsic survives anchoring untouched" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> testAnchoredRender parameterAnchor "string"
        ||> Flip.Expect.equal "intrinsic atoms are anchor-invariant"

    // `int` tuple element is an Intrinsic.
    testCase "ConcretePath in a tuple survives anchoring" <| fun _ ->
        [
            Interface.create "Bar" |> Interface.withPath [ "Foo" ] |> Interface.wrap |> Tuple.createElement
            primitive TypeKindPrimitive.Integer |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testAnchoredRender parameterAnchor "Foo.Bar * int"
        ||> Flip.Expect.equal "tuple anchoring recurses; ConcretePath element unchanged"
]

// ---------------------------------------------------------------------------
// RENDER-LEVEL: localise shortens a ConcretePath sharing a module prefix with
// the current anchor.
// ---------------------------------------------------------------------------
let localiseTests = testList "localisePaths shortens shared-prefix paths" [
    testCase "type sharing the anchor's module collapses to its leaf" <| fun _ ->
        // Interface at Root.Bar, anchored & localised relative to a member living
        // in the same Root module -> the Root prefix is stripped, leaving `Bar`.
        let relative =
            ModulePath.init "Root"
            |> TypePath.create "Local"
            |> MemberPath.createOnType "op"
            |> AnchorPath.Member
        Interface.create "Bar"
        |> Interface.withPath [ "Root" ]
        |> Interface.wrap
        |> testAnchoredRelativeRender relative parameterAnchor "Bar"
        ||> Flip.Expect.equal "shared Root module prefix is stripped by localise"

    testCase "type in a disjoint module keeps its full path under localise" <| fun _ ->
        let relative =
            ModulePath.init "Elsewhere"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> testAnchoredRelativeRender relative parameterAnchor "Foo.Bar"
        ||> Flip.Expect.equal "no shared module prefix — full path retained"
]

// ---------------------------------------------------------------------------
// NAMEPATH-LEVEL: TransientTypePath.anchor prepends the anchor's module trace.
// These characterise the pass at its purest contract.
// ---------------------------------------------------------------------------
let private flat (typePath: TypePath) =
    TypePath.flatten typePath
    |> List.map Name.Case.valueOrModified

let transientAnchorTests = testList "TransientTypePath.anchor contract" [
    testCase "moored transient type anchored at a Type prepends anchor module + anchor type" <| fun _ ->
        // anchor Root.Local (a Type); transient `Child` moored to anchored module
        // -> the anchor's own type name (Local) becomes a module segment.
        let anchor =
            ModulePath.init "Root"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        let transient = TransientTypePath.AnchoredAndMoored (Name.Pascal.create "Child")
        TransientTypePath.anchor anchor transient
        |> flat
        |> Flip.Expect.equal "anchored absolute path" [ "Root"; "Local"; "Child" ]

    testCase "anchored (unnamed) transient type collapses to the anchor itself" <| fun _ ->
        let anchor =
            ModulePath.init "Root"
            |> TypePath.create "Local"
            |> AnchorPath.Type
        TransientTypePath.anchor anchor TransientTypePath.Anchored
        |> flat
        |> Flip.Expect.equal "unnamed transient on a Type anchor IS the anchor" [ "Root"; "Local" ]

    testCase "transient type anchored at a Parameter threads through member+parameter" <| fun _ ->
        let transient = TransientTypePath.AnchoredAndMoored (Name.Pascal.create "Opt")
        TransientTypePath.anchor parameterAnchor transient
        |> flat
        |> Flip.Expect.equal "" [ "Root"; "Holder"; "Member"; "Para"; "Opt" ]
]

// ---------------------------------------------------------------------------
// NAMEPATH-LEVEL: NON-idempotence of TransientTypePath.anchor.
// Anchoring the SAME transient against two DIFFERENT anchors yields two
// DIFFERENT absolute paths — re-anchoring is path-sensitive, not a no-op.
// ---------------------------------------------------------------------------
let nonIdempotenceTests = testList "TransientTypePath.anchor non-idempotence" [
    testCase "same transient at two different anchors yields different absolute paths" <| fun _ ->
        let transient = TransientTypePath.AnchoredAndMoored (Name.Pascal.create "Child")
        let anchorA =
            ModulePath.init "Alpha"
            |> TypePath.create "A"
            |> AnchorPath.Type
        let anchorB =
            ModulePath.init "Beta"
            |> TypePath.create "B"
            |> AnchorPath.Type
        let underA = TransientTypePath.anchor anchorA transient |> flat
        let underB = TransientTypePath.anchor anchorB transient |> flat
        underA |> Flip.Expect.equal "" [ "Alpha"; "A"; "Child" ]
        underB |> Flip.Expect.equal "" [ "Beta"; "B"; "Child" ]
        Flip.Expect.notEqual "anchoring is anchor-sensitive — NOT idempotent across anchors" underA underB
]

// ---------------------------------------------------------------------------
// NAMEPATH-LEVEL: ConcretePath (an anchored TypePath, not a transient) is the
// re-anchor-invariant atom. `TypeRefAtom.anchor` (Anchored) maps ConcretePath ->
// Path by identity. Constructing the equivalent at the type level: flattening a
// TypePath is stable regardless of any anchor we might try to apply.
// ---------------------------------------------------------------------------
let concreteInvarianceTypeLevelTests = testList "ConcretePath flatten invariance" [
    testCase "flattening a concrete TypePath is independent of any anchor context" <| fun _ ->
        let concrete =
            ModulePath.createFromList [ "Foo"; "Inner" ]
            |> TypePath.create "Bar"
        // There is no anchor parameter to TypePath.flatten — a concrete path
        // already carries its full absolute trace, which is exactly why the
        // Anchored TypeRefAtom.anchor returns it by identity.
        flat concrete
        |> Flip.Expect.equal "concrete path is its own absolute trace" [ "Foo"; "Inner"; "Bar" ]
]

// ---------------------------------------------------------------------------
// NAMEPATH-LEVEL: getRelativePath strips a shared module prefix.
// ---------------------------------------------------------------------------
let relativePathTests = testList "getRelativePath shared-prefix shortening" [
    testCase "target sharing the anchor's module is shortened to its tail" <| fun _ ->
        let target =
            ModulePath.createFromList [ "Shared"; "Deep" ]
            |> TypePath.create "Target"
        let from =
            ModulePath.createFromList [ "Shared"; "Other" ]
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Path.getRelativePath target from
        |> List.map Name.Case.valueOrModified
        |> Flip.Expect.equal "Shared prefix stripped" [ "Deep"; "Target" ]

    testCase "fully disjoint target keeps its whole path" <| fun _ ->
        let target =
            ModulePath.createFromList [ "Alpha"; "Beta" ]
            |> TypePath.create "Target"
        let from =
            ModulePath.createFromList [ "Gamma"; "Delta" ]
            |> TypePath.create "Local"
            |> AnchorPath.Type
        Path.getRelativePath target from
        |> List.map Name.Case.valueOrModified
        |> Flip.Expect.equal "no shared prefix" [ "Alpha"; "Beta"; "Target" ]
]

[<Tests>]
let tests = testList "AnchoringPlacement" [
    concretePathInvarianceTests
    nestedAnchorTests
    localiseTests
    transientAnchorTests
    nonIdempotenceTests
    concreteInvarianceTypeLevelTests
    relativePathTests
]
