module DispatchKeyingPolicy

// ---------------------------------------------------------------------------
// PER-PASS ISOLATION COVERAGE — IDENTITY-KEYING POLICY + MODIFIER EXTRACTION
//
// TARGET #1 — the identity-keying policy in src/Xantham.Fable/Reading/Prelude.fs:
//   TypeStore.usesGeneratedKey  and  ExportStore.usesGeneratedKey.
//
// Both functions read ONLY `tag.Value` (a XanTagKind) — no checker, no signal
// graph — to decide whether a node/type gets a freshly GENERATED TypeKey
// (TypeKey.create()) instead of the checker's getTypeAtLocation key. This is the
// single most-implicated identity surface in the encoder: it is the root of the
// union over-minting / hoist-dedup / string->D1SessionBookmark TypeAliasRemap
// defect family, yet it was asserted NOWHERE before this file.
//
// These tests CHARACTERIZE the current contract (they do not endorse it as
// bug-free). In particular the cross-function asymmetry for TypeAlias
// (TypeStore=true, ExportStore=false) is DELIBERATE per the source and is the
// exact suspect MEMORY flags for the TypeAliasRemap defect — see that test.
//
// TARGET #2 — modifier extraction in Prelude.fs:
//   arrayHasModifier / optionArrayHasModifier + Modifiers.Create. This drives
//   every member's IsStatic/IsReadonly/IsPrivate and the accessor
//   ReadOnly/ReadWrite split.
//
// Mocks: a XanthamTag is `{ Value: XanTagKind }` stored on a JS object (the
// Tracer<'T> interface exposes `.Value`). We build the XanTagKind via
// XanTagKind.Create on a `nodeWithKind`-style createObj mock (mirroring
// DispatchClassification.fs) and wrap it as `createObj ["Value" ==> kind]`.
// usesGeneratedKey only reads `tag.Value`, so this minimal wrapper exercises the
// exact policy the live encoder uses. The internal debug helpers it calls are
// gated on `tag.Debug`, which defaults to false on a createObj mock — so they
// are inert here.
//
// Fable.Mocha Expect idiom (matches DispatchClassification.fs):
//   Expect.isTrue value "message" ;  Expect.isFalse value "message"
// ---------------------------------------------------------------------------

open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Xantham.Fable.Reading
open Fable.Mocha

// A ts-morph Node is classified off its `.kind` (mirrors DispatchClassification.fs).
let private nodeWithKind (kind: Ts.SyntaxKind) : Ts.Node =
    createObj [ "kind" ==> kind ] |> unbox

// Wrap a XanTagKind as a minimal XanthamTag: the Tracer<'T> interface exposes
// `.Value`, and usesGeneratedKey reads nothing else off the tag.
let private tagWith (kind: XanTagKind) : XanthamTag =
    createObj [ "Value" ==> kind ] |> unbox

// Build the XanTagKind for a node of the given kind, then wrap it as a tag.
let private tagFromKind (kind: Ts.SyntaxKind) : XanthamTag =
    tagWith (XanTagKind.Create (nodeWithKind kind))

// ---------------------------------------------------------------------------
// TypeStore.usesGeneratedKey / ExportStore.usesGeneratedKey
// ---------------------------------------------------------------------------
let private keyingPolicyTests =
    testList "usesGeneratedKey policy" [

        // --- TypeStore: TypeDeclaration cases that MUST use a generated key ----
        testCase "TypeStore: TypeAlias -> generated key (true)" <| fun _ ->
            "TypeAlias must use a generated key (aliased type's TypeKey would self-collide)"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.TypeAliasDeclaration))

        testCase "TypeStore: VariableDeclaration -> generated key (true)" <| fun _ ->
            "VariableDeclaration must use a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.VariableDeclaration))

        testCase "TypeStore: VariableStatement -> generated key (true)" <| fun _ ->
            "VariableStatement must use a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.VariableStatement))

        testCase "TypeStore: TypeParameter -> generated key (true)" <| fun _ ->
            // XanTagKind.Create routes a bare TypeParameter node to TypeDeclaration
            // (TypeDeclaration is checked before TypeNode), matching the
            // TypeDeclaration.TypeParameter arm of usesGeneratedKey.
            "TypeParameter must use a generated key (collides with type-level TypeParameter otherwise)"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.TypeParameter))

        testCase "TypeStore: ExpressionWithTypeArguments -> generated key (true)" <| fun _ ->
            "ExpressionWithTypeArguments must use a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.ExpressionWithTypeArguments))

        // --- TypeStore: TypeDeclaration regression guards (must NOT generate) --
        testCase "TypeStore: Interface -> checker key (false)" <| fun _ ->
            "Interface must use the checker key, not a generated one"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.InterfaceDeclaration))

        testCase "TypeStore: Class -> checker key (false)" <| fun _ ->
            "Class must use the checker key, not a generated one"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.ClassDeclaration))

        testCase "TypeStore: Enum -> checker key (false)" <| fun _ ->
            "Enum must use the checker key, not a generated one"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.EnumDeclaration))

        testCase "TypeStore: FunctionDeclaration -> checker key (false)" <| fun _ ->
            "FunctionDeclaration must use the checker key, not a generated one"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.FunctionDeclaration))

        // --- TypeStore: TypeNode.TypeReference depends on typeArguments.Count ---
        // usesGeneratedKey reads: typeRef.typeArguments |> Option.map (_.Count > 0)
        //                                                |> Option.defaultValue false
        // The payload of TypeNode.TypeReference IS the node. typeArguments is typed
        // `ResizeArray<TypeNode> option`, so `.Count` is the ResizeArray length —
        // we use real ResizeArrays of dummy nodes to give it an honest Count.
        let typeRefWith (typeArgs: ResizeArray<Ts.TypeNode> option) : XanthamTag =
            let node : Ts.TypeReferenceNode =
                createObj [
                    "kind" ==> Ts.SyntaxKind.TypeReference
                    "typeArguments" ==> typeArgs
                ] |> unbox
            tagWith (XanTagKind.TypeNode (TypeNode.TypeReference node))
        let dummyArgs (n: int) : ResizeArray<Ts.TypeNode> =
            ResizeArray(Array.init n (fun _ -> createObj [ "kind" ==> Ts.SyntaxKind.StringKeyword ] |> unbox<Ts.TypeNode>))

        testCase "TypeStore: TypeReference, typeArguments None -> false" <| fun _ ->
            "A non-generic TypeReference (no typeArguments) uses the checker key"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (typeRefWith None))

        testCase "TypeStore: TypeReference, typeArguments Some {Count=0} -> false" <| fun _ ->
            "An empty type-argument list (Count=0) uses the checker key"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (typeRefWith (Some (dummyArgs 0))))

        testCase "TypeStore: TypeReference, typeArguments Some {Count=2} -> true" <| fun _ ->
            "An explicit-type-argument TypeReference (Count>0) uses a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (typeRefWith (Some (dummyArgs 2))))

        // --- TypeStore: the union/intersection-gets-own-identity invariant -----
        // This PINS the over-minting root: each union/intersection node gets its
        // own identity, separate from the checker's structurally-expanded type.
        testCase "TypeStore: UnionType -> generated key (true)" <| fun _ ->
            "UnionType node gets its own identity (separate from expanded semantic type)"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.UnionType))

        testCase "TypeStore: IntersectionType -> generated key (true)" <| fun _ ->
            "IntersectionType node gets its own identity (separate from expanded semantic type)"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.IntersectionType))

        testCase "TypeStore: ArrayType (non-special TypeNode) -> false" <| fun _ ->
            "A plain ArrayType TypeNode uses the checker key"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.ArrayType))

        testCase "TypeStore: StringKeyword (non-special TypeNode) -> false" <| fun _ ->
            "A StringKeyword TypeNode uses the checker key"
            |> Expect.isFalse (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.StringKeyword))

        // --- TypeStore: TypeQuery / TypePredicate wrappers ---------------------
        testCase "TypeStore: TypeQuery -> generated key (true)" <| fun _ ->
            "TypeQuery wraps the queried name; gets its own identity"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.TypeQuery))

        testCase "TypeStore: TypePredicate -> generated key (true)" <| fun _ ->
            "TypePredicate would otherwise share the boolean TypeKey; gets its own identity"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.TypePredicate))

        // --- TypeStore: ModulesAndExports / MemberDeclaration always generate --
        testCase "TypeStore: ModulesAndExports -> generated key (true)" <| fun _ ->
            "A ModulesAndExports tag always uses a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.ExportDeclaration))

        testCase "TypeStore: MemberDeclaration -> generated key (true)" <| fun _ ->
            "A MemberDeclaration tag always uses a generated key"
            |> Expect.isTrue (TypeStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.PropertySignature))

        // --- ExportStore: only Variable* generate; everything else is false ----
        testCase "ExportStore: VariableDeclaration -> generated key (true)" <| fun _ ->
            "ExportStore generates a key only for VariableDeclaration"
            |> Expect.isTrue (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.VariableDeclaration))

        testCase "ExportStore: VariableStatement -> generated key (true)" <| fun _ ->
            "ExportStore generates a key only for VariableStatement"
            |> Expect.isTrue (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.VariableStatement))

        testCase "ExportStore: Interface/Class/TypeAlias/FunctionDeclaration -> all false" <| fun _ ->
            "ExportStore does NOT generate a key for Interface"
            |> Expect.isFalse (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.InterfaceDeclaration))
            "ExportStore does NOT generate a key for Class"
            |> Expect.isFalse (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.ClassDeclaration))
            "ExportStore does NOT generate a key for TypeAlias"
            |> Expect.isFalse (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.TypeAliasDeclaration))
            "ExportStore does NOT generate a key for FunctionDeclaration"
            |> Expect.isFalse (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.FunctionDeclaration))

        testCase "ExportStore: non-TypeDeclaration tag -> false" <| fun _ ->
            "ExportStore returns false for any non-TypeDeclaration tag (e.g. a TypeNode)"
            |> Expect.isFalse (ExportStore.usesGeneratedKey (tagFromKind Ts.SyntaxKind.UnionType))

        // --- THE KEY ONE: cross-function divergence for TypeAlias --------------
        // For a TypeAlias tag, TypeStore.usesGeneratedKey = TRUE while
        // ExportStore.usesGeneratedKey = FALSE. This locks the exact asymmetry
        // MEMORY flags as the string->D1SessionBookmark TypeAliasRemap suspect.
        // The asymmetry is DELIBERATE per the source (TypeStore needs a distinct
        // key to avoid a self-referential alias entry; ExportStore reuses the
        // checker key). It is CHARACTERIZED here, NOT endorsed as bug-free — if an
        // e2e repro later shows it wrong, this test documents the current contract.
        testCase "DIVERGENCE: TypeAlias -> TypeStore=true while ExportStore=false (deliberate, characterized)" <| fun _ ->
            let aliasTag = tagFromKind Ts.SyntaxKind.TypeAliasDeclaration
            "TypeStore.usesGeneratedKey is TRUE for a TypeAlias"
            |> Expect.isTrue (TypeStore.usesGeneratedKey aliasTag)
            "ExportStore.usesGeneratedKey is FALSE for the SAME TypeAlias (deliberate asymmetry; TypeAliasRemap suspect)"
            |> Expect.isFalse (ExportStore.usesGeneratedKey aliasTag)
    ]

// ---------------------------------------------------------------------------
// arrayHasModifier / optionArrayHasModifier + Modifiers.Create
//   arrayHasModifier modifier arr        = arr.AsArray |> Array.exists (Modifiers.Create >> modifier)
//   optionArrayHasModifier modifier arr  = arr |> Option.exists (arrayHasModifier modifier)
// A modifier mock is `createObj ["kind" ==> Ts.SyntaxKind.X]`; Modifiers.Create
// maps the kind to a Modifiers DU case, and the auto Is* testers read the case.
// ---------------------------------------------------------------------------
let private modifierMock (kind: Ts.SyntaxKind) : Ts.Modifier =
    createObj [ "kind" ==> kind ] |> unbox

let private modifierArray (kinds: Ts.SyntaxKind list) : ResizeArray<Ts.Modifier> =
    ResizeArray(kinds |> List.map modifierMock)

let private modifiersTests =
    testList "modifiers (arrayHasModifier / Modifiers.Create)" [

        // --- Modifiers.Create maps kind -> case (with auto Is* testers) --------
        testCase "Modifiers.Create: StaticKeyword -> Static (IsStatic=true)" <| fun _ ->
            "StaticKeyword classifies as Modifiers.Static"
            |> Expect.isTrue (Modifiers.Create (modifierMock Ts.SyntaxKind.StaticKeyword)).IsStatic

        testCase "Modifiers.Create: ExportKeyword -> Export (IsStatic=false)" <| fun _ ->
            "ExportKeyword is NOT Static"
            |> Expect.isFalse (Modifiers.Create (modifierMock Ts.SyntaxKind.ExportKeyword)).IsStatic

        testCase "Modifiers.Create: ReadonlyKeyword -> ReadOnly (IsReadOnly=true)" <| fun _ ->
            "ReadonlyKeyword classifies as Modifiers.ReadOnly"
            |> Expect.isTrue (Modifiers.Create (modifierMock Ts.SyntaxKind.ReadonlyKeyword)).IsReadOnly

        testCase "Modifiers.Create: PrivateKeyword -> Private (IsPrivate=true)" <| fun _ ->
            "PrivateKeyword classifies as Modifiers.Private"
            |> Expect.isTrue (Modifiers.Create (modifierMock Ts.SyntaxKind.PrivateKeyword)).IsPrivate

        // --- optionArrayHasModifier None / empty -------------------------------
        testCase "optionArrayHasModifier None -> false (silent non-readonly default)" <| fun _ ->
            "No modifier list at all yields false (pins the ReadWrite/non-readonly default)"
            |> Expect.isFalse (optionArrayHasModifier _.IsReadOnly None)

        testCase "optionArrayHasModifier (Some empty) -> false" <| fun _ ->
            "An empty modifier list yields false"
            |> Expect.isFalse (optionArrayHasModifier _.IsReadOnly (Some (modifierArray [])))

        // --- arrayHasModifier presence / absence -------------------------------
        testCase "arrayHasModifier IsStatic over [Static] -> true; over [Export] -> false" <| fun _ ->
            "[StaticKeyword] has a Static modifier"
            |> Expect.isTrue (arrayHasModifier _.IsStatic (modifierArray [ Ts.SyntaxKind.StaticKeyword ]))
            "[ExportKeyword] has no Static modifier"
            |> Expect.isFalse (arrayHasModifier _.IsStatic (modifierArray [ Ts.SyntaxKind.ExportKeyword ]))

        // --- arrayHasModifier Array.exists across a mixed list -----------------
        testCase "arrayHasModifier IsReadOnly over [Static; Readonly] -> true (Array.exists)" <| fun _ ->
            "Mixed [Static; Readonly] DOES contain a Readonly modifier"
            |> Expect.isTrue (arrayHasModifier _.IsReadOnly (modifierArray [ Ts.SyntaxKind.StaticKeyword; Ts.SyntaxKind.ReadonlyKeyword ]))

        testCase "arrayHasModifier IsStatic over [Readonly] -> false (no cross-flag false-positive)" <| fun _ ->
            "[Readonly] does NOT register as Static"
            |> Expect.isFalse (arrayHasModifier _.IsStatic (modifierArray [ Ts.SyntaxKind.ReadonlyKeyword ]))

        // --- Accessor-selection contract (ReadOnly vs ReadWrite) ---------------
        // The accessor split is exactly:
        //   if optionArrayHasModifier _.IsReadOnly mods then ReadOnly else ReadWrite
        testCase "Accessor selection: [Readonly] -> ReadOnly; None & [Static] -> ReadWrite" <| fun _ ->
            let accessor mods =
                if optionArrayHasModifier _.IsReadOnly mods then "ReadOnly" else "ReadWrite"
            "[ReadonlyKeyword] selects ReadOnly"
            |> Expect.equal (accessor (Some (modifierArray [ Ts.SyntaxKind.ReadonlyKeyword ]))) "ReadOnly"
            "No modifiers selects ReadWrite"
            |> Expect.equal (accessor None) "ReadWrite"
            "[StaticKeyword]-only selects ReadWrite (no readonly present)"
            |> Expect.equal (accessor (Some (modifierArray [ Ts.SyntaxKind.StaticKeyword ]))) "ReadWrite"
    ]

let dispatchKeyingPolicyTests =
    testList "DispatchKeyingPolicy" [
        keyingPolicyTests
        modifiersTests
    ]
