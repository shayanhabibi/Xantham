module Xantham.Generator.Tests.Tests.SharedDeepNestedLiteralPlacement

// Isolated characterization (now a regression test) of the SHARED DEEP-NESTED LITERAL
// bug — the depth-3+, multi-owner-chain variant of the nested-hoist placement family
// that was the DOMINANT FS0039 class on the cloudflare surface (~670 errors; BoostBy,
// Tools, Fetch, Messages, Filters, ...).
//
// THE BUG (worked example: `BoostBy`, reproduced below):
//   ONE structurally-identical inline object literal is reached through TWO+ DIFFERENT
//   owner contexts. The decoder's structural compression interns the structurally-equal
//   literals into a SINGLE shared ResolvedType. That single shared literal is then:
//     * REFERENCED from N distinct owner chains, each localised to its OWN enclosing type
//       -> `<ownerA>.BoostBy`, `<ownerB>.BoostBy` (distinct concrete reference paths).
//     * DEFINED exactly ONCE (per-ResolvedType, first-write-wins), landing under whichever
//       owner the generator processed FIRST. The other owners' references dangle (FS0039).
//
// THE FIX (mirrors the proven LiteralUnions canonical-home mechanism):
//   A counting pre-pass (`markSharedLiteralsFromExportList`) records, per literal
//   ResolvedType, the set of distinct anchored def-home paths it is visited under. A
//   literal reached under >1 distinct home is SHARED across owners and is given a canonical
//   owner-independent home `SharedLiterals.<structural-name>` in `ctx.SharedLiteralHomes`.
//   `prerender` then roots that literal at the absolute `ConcretePath` home (instead of a
//   per-owner transient), so EVERY reference resolves to the same re-anchor-invariant path
//   and the single def is emitted once by the same PreludeRenders driver that emits the
//   canonical literal-union enums. A literal reached under exactly ONE owner is NOT
//   canonicalized — it stays nested under its owner (the depth-2 single-owner behaviour,
//   pinned by NestedUnderTypePlacement / AliasBodyLiteralPlacement).

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Mocking.ArenaInterner.ResolvedType

// An inline object literal with >=3 plain string properties (so it hoists).
let private objLit (names: string list) =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        for n in names do Property.create n (primitive TypeKindPrimitive.String) |> Property.wrap
    ]
    |> TypeLiteral.wrap

// Render a SET of interface exports end-to-end. Mirrors `Render.fs`'s pipeline: FIRST the
// shared-literal counting pre-pass (so genuinely-shared literals get a canonical home), THEN
// register each export, collect, and render.
let private renderInterfaceSurfaces (ifaces: Interface list) : string =
    let ctx = GeneratorContext.Empty
    let exports = ifaces |> List.map ResolvedExport.Interface
    markSharedLiteralsFromExportList ctx exports
    for export in exports do
        registerAnchorFromExport ctx export
    // Emit the canonical SharedLiterals/LiteralUnions scopes (the same driver `processExports`
    // runs after the owner pass) so the shared-literal DEFS land in AnchorRenders, not just their
    // references.
    emitCanonicalPreludeScopes ctx
    let root = RootModule.collectModules ctx
    Ast.Oak() {
        Ast.AnonymousModule() {
            renderRoot ctx root
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// ONE shared literal (the `Retrieval`-analogue on the real surface: the literal the decoder's
// structural compression interns into a SINGLE node, then references through N distinct owner
// chains). Built ONCE so both owners share the exact same `[<ReferenceEquality>]` ResolvedType
// identity. >=3 members so it hoists. Its `boostBy` member references a FURTHER nested literal —
// the inner `BoostBy` analogue — so the def, when canonicalized, carries a nested child too.
let private sharedInnerBoost : ResolvedType =
    objLit [ "direction"; "field"; "weight" ]

let private sharedRetrieval : ResolvedType =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        Property.create "boostBy" (Array.create sharedInnerBoost) |> Property.wrap
        Property.create "tag" (primitive TypeKindPrimitive.String) |> Property.wrap
        Property.create "note" (primitive TypeKindPrimitive.String) |> Property.wrap
    ]
    |> TypeLiteral.wrap

// An owner interface that references the SHARED `sharedRetrieval` literal DIRECTLY via a property.
// The two owners use DIFFERENT property names so, absent canonicalization, the localised def-home
// DIFFERS per owner (`AlphaOwner.Retrieval` vs `BetaOwner.RetrievalOptions`) and the single
// per-ResolvedType def lands under only one — dangling the other. Mirrors the real surface where
// `Retrieval` is referenced by AiSearchOptions / AiSearchChatCompletionsRequest / ... under
// distinct owners.
let private ownerWithSharedLiteral (ifaceName: string) (ownerProp: string) : Interface =
    { (Interface.create ifaceName |> Interface.withPath [ "Mod" ]) with
        Members = [ Property.create ownerProp sharedRetrieval |> Property.wrap ] }

let private alphaOwner = ownerWithSharedLiteral "AlphaOwner" "retrieval"
let private betaOwner = ownerWithSharedLiteral "BetaOwner" "retrievalOptions"

let sharedDeepNestedLiteralTests = testList "shared deep-nested literal placement" [
    // The shared literal, reached through BOTH owner chains under DIFFERENT property names, now
    // resolves from BOTH: it is given a single canonical `SharedLiterals.*` home that ALL
    // references point to (instead of a single per-owner def that dangles the other owner).
    testCase "shared literal reached through two owner chains resolves from BOTH" <| fun _ ->
        let out = renderInterfaceSurfaces [ alphaOwner; betaOwner ]
        // The shared literal gets an owner-independent canonical home.
        Expect.stringContains out "module SharedLiterals" "canonical SharedLiterals module emitted"
        // Both owners reference the canonical home: `retrieval`/`retrievalOptions` resolve to a
        // `SharedLiterals.*` type, NOT to a per-owner `AlphaOwner.Retrieval` / `BetaOwner.*` def
        // that would leave one side dangling.
        Expect.stringContains out "retrieval: SharedLiterals." "AlphaOwner.retrieval resolves to the canonical home"
        Expect.stringContains out "retrievalOptions: SharedLiterals." "BetaOwner.retrievalOptions resolves to the canonical home"
        // The shared literal's def is emitted exactly ONCE (under SharedLiterals), not once per
        // owner. Its member-name stem is `BoostByNoteTag` (sorted member names, pascal-cased).
        let sharedDefs =
            out.Split('\n')
            |> Array.filter (fun l -> l.Trim().StartsWith("type BoostByNoteTag"))
            |> Array.length
        Expect.equal sharedDefs 1
            (sprintf "expected exactly ONE canonical def for the shared literal; got %d.\n%s" sharedDefs out)

    // No over-collapse: two DISTINCT shared literals (same property NAMES, different property
    // TYPES -> distinct ResolvedTypes) get DISTINCT canonical names. Each is shared across two
    // owners (so each is canonicalized), and they must NOT be merged into one def.
    testCase "two distinct shared literals get distinct canonical names (no over-collapse)" <| fun _ ->
        // literalA: { kind: string; value: string }   literalB: { kind: bool; value: bool }
        let literalA =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "kind" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "value" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "extra" (primitive TypeKindPrimitive.String) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        let literalB =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "kind" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
                Property.create "value" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
                Property.create "extra" (primitive TypeKindPrimitive.Boolean) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        // Each literal is referenced DIRECTLY under two owners with DIFFERENT prop names, so each
        // is shared (>1 distinct def-home) and canonicalized.
        let owner name propA propB =
            { (Interface.create name |> Interface.withPath [ "Mod" ]) with
                Members = [
                    Property.create propA literalA |> Property.wrap
                    Property.create propB literalB |> Property.wrap
                ] }
        let out =
            renderInterfaceSurfaces [
                owner "OwnerOne" "alpha" "beta"
                owner "OwnerTwo" "gamma" "delta"
            ]
        // Both distinct literals are canonicalized (shared across the two owners). They share the
        // SAME member-name stem (`ExtraKindValue`) but are DISTINCT ResolvedTypes -> two DISTINCT
        // canonical names disambiguated by suffix (`ExtraKindValue`, `ExtraKindValue2`). Both must
        // appear as references AND as distinct `type <Name> =` defs (no over-collapse to one).
        Expect.stringContains out "SharedLiterals.ExtraKindValue " "first shared literal referenced at its canonical home"
        Expect.stringContains out "SharedLiterals.ExtraKindValue2 " "second shared literal referenced at a DISTINCT canonical home"
        let canonicalDefNames =
            out.Split('\n')
            |> Array.choose (fun l ->
                let t = l.Trim()
                if t.StartsWith("type ExtraKindValue") && t.EndsWith("=") then Some t else None)
            |> Array.distinct
        Expect.isGreaterThanOrEqual canonicalDefNames.Length 2
            (sprintf "expected >=2 DISTINCT canonical def names for the two structurally-distinct shared literals (no over-collapse); got %d: %A\n%s"
                canonicalDefNames.Length canonicalDefNames out)

    // Control: a SINGLE-owner deep-nested literal is NOT canonicalized — it stays nested under its
    // owner (the depth-2 behaviour). Only ONE owner references the inner literal, so it has exactly
    // ONE def-home and must remain there (no `SharedLiterals` entry for it).
    testCase "single-owner deep-nested literal stays under its owner (not canonicalized)" <| fun _ ->
        let inner = objLit [ "lat"; "lng"; "alt" ]
        let outer =
            TypeLiteral.empty
            |> TypeLiteral.withMembers [
                Property.create "origin" (Array.create inner) |> Property.wrap
                Property.create "label" (primitive TypeKindPrimitive.String) |> Property.wrap
                Property.create "extra" (primitive TypeKindPrimitive.String) |> Property.wrap
            ]
            |> TypeLiteral.wrap
        let onlyOwner =
            { (Interface.create "SoleOwner" |> Interface.withPath [ "Mod" ]) with
                Members = [ Property.create "place" outer |> Property.wrap ] }
        let out = renderInterfaceSurfaces [ onlyOwner ]
        // The inner literal's reference is localised under its single owner (`Place.Origin`), NOT
        // hoisted to a canonical SharedLiterals home.
        Expect.stringContains out "Place.Origin" "single-owner inner ref stays localised under its owner"
        Expect.isFalse (out.Contains "module SharedLiterals")
            (sprintf "single-owner literal must NOT be canonicalized into SharedLiterals.\n%s" out)

    // UNION-WRAPPED shared literal (the real-surface `Tools`/`ToolCalls`/`Messages` class): the
    // shared literal is reached ONLY inside a union member position (`U2<Owner.Lit, Owner.Lit>`),
    // NEVER as a direct property def-home. The def-VISIT counting gate never sees it as a def-home
    // (the cached union molecule carries its reference, so no owner scope holds it as a recursion
    // child), so at HEAD its single def lands under the first owner and the other owner's reference
    // dangles (FS0039). The reference-count gate — distinct REFERENCING owners from the structural
    // ResolvedType walk — catches it: >1 owner references the union member, so it is canonicalized.
    testCase "union-member shared literal (U2<Lit,...>) resolves from BOTH owners via SharedLiterals" <| fun _ ->
        // ONE shared literal wrapped in ONE shared union (`U2<sharedTool, sharedTool>` — the
        // self-duplicated shape the decoder compression produces), referenced by two owners ONLY
        // inside that union-member position. Crucially BOTH owners reference the SAME `[<Reference
        // Equality>]` union instance — mirroring the decoder's structural INTERNING. That interning
        // is precisely what hides the literal from the def-VISIT gate: anchoring descends into the
        // shared union subtree under only the FIRST owner, so the literal is visited as a def-home
        // exactly once; the second owner's `Owner.Tool` reference dangles. The reference-count gate
        // (distinct referencing owners from the structural walk) is what recovers it.
        let sharedTool = objLit [ "description"; "name"; "parameters" ]
        let sharedToolUnion = Union.create [ sharedTool; sharedTool ]
        let sharedToolArray = Array.create sharedToolUnion
        let ownerWithUnionMember name =
            { (Interface.create name |> Interface.withPath [ "Mod" ]) with
                Members = [ Property.create "tools" sharedToolArray |> Property.wrap ] }
        let out =
            renderInterfaceSurfaces [
                ownerWithUnionMember "AlphaUnionOwner"
                ownerWithUnionMember "BetaUnionOwner"
            ]
        // The union-wrapped shared literal gets a canonical owner-independent home.
        Expect.stringContains out "module SharedLiterals"
            (sprintf "union-member shared literal must be canonicalized into SharedLiterals.\n%s" out)
        // BOTH owners' union members reference the canonical home (`U2<SharedLiterals.*, ...>`),
        // NOT a per-owner `AlphaUnionOwner.*` / `BetaUnionOwner.*` def that dangles the other side.
        Expect.stringContains out "SharedLiterals.DescriptionNameParameters"
            "union-member shared literal referenced at its canonical SharedLiterals home"
        // Exactly ONE canonical def for the shared literal (member-name stem, pascal-cased), not one
        // per owner.
        let sharedDefs =
            out.Split('\n')
            |> Array.filter (fun l -> l.Trim().StartsWith("type DescriptionNameParameters"))
            |> Array.length
        Expect.equal sharedDefs 1
            (sprintf "expected exactly ONE canonical def for the union-member shared literal; got %d.\n%s" sharedDefs out)

    // Control for the union-member case: a literal reached through union-member position under
    // EXACTLY ONE owner is NOT canonicalized — it stays nested under that owner (mirrors the
    // direct-property single-owner control). Only ONE owner references the union, so the
    // reference-count gate sees one distinct owner and leaves it nested.
    testCase "single-owner union-member literal stays under its owner (not canonicalized)" <| fun _ ->
        let soleTool = objLit [ "kindd"; "labell"; "valuee" ]
        let onlyOwner =
            { (Interface.create "SoleUnionOwner" |> Interface.withPath [ "Mod" ]) with
                Members = [
                    Property.create "options"
                        (Array.create (Union.create [ soleTool; soleTool ]))
                    |> Property.wrap
                ] }
        let out = renderInterfaceSurfaces [ onlyOwner ]
        Expect.isFalse (out.Contains "module SharedLiterals")
            (sprintf "single-owner union-member literal must NOT be canonicalized into SharedLiterals.\n%s" out)
]

[<Tests>]
let tests = testList "SharedDeepNestedLiteralPlacement" [
    sharedDeepNestedLiteralTests
]
