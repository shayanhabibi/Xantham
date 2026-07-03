module Xantham.Generator.Tests.Tests.OverloadUnification

(*
Coverage plane for the RENDER-ENTRY OVERLOAD SEAMS in TypeRender.Render.fs — the pair of
passes that guarantee a member NAME reaches emission exactly once per .NET-distinct signature:

  1. `TypeLikeRender.groupFunctionsByName` (private; pinned through `renderInterface`):
     same-name FunctionLikeRenders arriving as SEPARATE renders (partitionRender output,
     TypeLiteral.prerender's groupless fold, collection `combine`) must fuse into ONE render
     before overload unification, or identical-signature twins emit as duplicate members
     (FS0438 — the Agents TypedAgentClientCall Invoke class).

  2. `FunctionLikeRender.unifyReturnOnlyOverloads` (private; pinned through `renderAbstract`):
     signatures grouped by their .NET-VISIBLE key — param types AS TEXT (localised atoms are
     opaque widgets; text is the compiler's view), optionality, ParamArray, typar arity.
     A group's distinct returns merge into ONE erased-union return; exact duplicates drop.

     KEY SUBTLETIES, each a closed generation defect:
       - OPTIONALITY IS TWO-CHANNEL: a param is option-typed EITHER via the Optional trait
         OR via its type's own Nullable flag (orNullable at render). Two signatures can carry
         it on different sides and print/compile IDENTICALLY — key on the NON-NULLABLE type
         text + the COMBINED flag (the byte-identical TypedAgentClientCall Invoke pair, the
         last Agents FS0438).
       - BARE `Erased.X` IS `obj`: arity-0 Erased advisory aliases are obj ABBREVIATIONS; the
         .NET signature sees through them, so the key must too (UntypedAgentClientCall class).
         Applied `Erased.X<...>` forms are real interfaces and stay distinct.
*)

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Anchored

// ---------------------------------------------------------------------------
// Construction helpers (Anchored shapes are public records — direct construction)
// ---------------------------------------------------------------------------

let private meta : Prelude.RenderMetadata = {
    Path = Path.create TransientTypePath.Anchored
    Original = Path.create TransientTypePath.Anchored
    Source = ValueNone
    FullyQualifiedName = ValueNone
}

let private atom (a: TypeRefAtom) : TypeRefRender = { Kind = TypeRefKind.Atom a; Nullable = false }
let private intrinsic (s: string) = atom (TypeRefAtom.Intrinsic s)
let private widget (text: string) = atom (TypeRefAtom.Widget(Ast.LongIdent text))

let private param name (traits: RenderTraits Set) (t: TypeRefRender) : TypedNameRender =
    { Metadata = meta; Name = Name.Camel.create name; Type = t; Traits = traits; TypeParameters = []; Documentation = [] }

let private signature' parameters (ret: TypeRefRender) : FunctionLikeSignature =
    { Metadata = meta; Parameters = parameters; ReturnType = ret; Traits = Set.empty; Documentation = []; TypeParameters = [] }

let private func name signatures : FunctionLikeRender =
    { Metadata = meta; Name = Name.Camel.create name; Signatures = signatures; Traits = Set.empty; TypeParameters = []; Documentation = [] }

let private iface functions : TypeLikeRender = {
    Metadata = meta
    Name = Name.Pascal.create "Probe"
    TypeParameters = []
    Inheritance = []
    Members = []
    Functions = functions
    Constructors = []
    Documentation = []
}

let private renderIfaceText (ctx: GeneratorContext) (typeLike: TypeLikeRender) =
    Ast.Oak() {
        Ast.AnonymousModule() {
            TypeLikeRender.renderInterface ctx typeLike
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

let private countOccurrences (needle: string) (hay: string) =
    (hay.Split(needle) |> Array.length) - 1

let private memberCount (flr: FunctionLikeRender) =
    let ctx = GeneratorContext.Empty
    FunctionLikeRender.renderAbstract ctx flr |> List.length

// ---------------------------------------------------------------------------
// unifyReturnOnlyOverloads — the .NET-visible signature key
// ---------------------------------------------------------------------------

[<Tests>]
let unifyTests =
    testList "FunctionLikeRender.renderAbstract overload unification" [

        testCase "exact duplicate signatures collapse to one member (overload-duplicate-drop)" <| fun _ ->
            let s = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            func "invoke" [ s; s ]
            |> memberCount
            |> Flip.Expect.equal "identical twins emit once" 1

        testCase "signatures differing in a param type stay distinct overloads" <| fun _ ->
            let s1 = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            let s2 = signature' [ param "x" Set.empty (intrinsic "float") ] (intrinsic "unit")
            func "invoke" [ s1; s2 ]
            |> memberCount
            |> Flip.Expect.equal "distinct .NET signatures survive" 2

        // THE two-channel optionality key (the last Agents FS0438, TypedAgentClientCall):
        // Optional trait + non-nullable type vs Optional trait + Nullable type print and
        // compile IDENTICALLY — the key must see one signature, not two.
        testCase "Optional trait vs Nullable flag on the same param unify (byte-identical twins)" <| fun _ ->
            let viaTrait = signature' [ param "args" (Set.singleton RenderTraits.Optional) (intrinsic "string") ] (intrinsic "unit")
            let viaFlag = signature' [ param "args" (Set.singleton RenderTraits.Optional) { intrinsic "string" with Nullable = true } ] (intrinsic "unit")
            func "invoke" [ viaTrait; viaFlag ]
            |> memberCount
            |> Flip.Expect.equal "nullability carried on either side is ONE .NET signature" 1

        testCase "?x: string and x: option<string> are the same .NET slot — unified" <| fun _ ->
            let optionalTrait = signature' [ param "x" (Set.singleton RenderTraits.Optional) (intrinsic "string") ] (intrinsic "unit")
            let nullableOnly = signature' [ param "x" Set.empty { intrinsic "string" with Nullable = true } ] (intrinsic "unit")
            func "invoke" [ optionalTrait; nullableOnly ]
            |> memberCount
            |> Flip.Expect.equal "trait-optional and option-typed param share the compiled slot" 1

        testCase "return-only overloads merge into one erased-union return (overload-return-union)" <| fun _ ->
            let s1 = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "string")
            let s2 = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "float")
            let text = renderIfaceText GeneratorContext.Empty (iface [ func "load" [ s1; s2 ] ])
            Expect.equal (countOccurrences "abstract load" text) 1 "one member"
            Expect.stringContains text "U2<string, float>" "returns merged into the erased union"

        // Bare Erased.X advisory aliases are obj abbreviations — signature-transparent.
        testCase "bare Erased.X param and obj param are one .NET signature — unified" <| fun _ ->
            let viaAlias = signature' [ param "x" Set.empty (widget "Erased.Empty") ] (intrinsic "unit")
            let viaObj = signature' [ param "x" Set.empty (intrinsic "obj") ] (intrinsic "unit")
            func "invoke" [ viaAlias; viaObj ]
            |> memberCount
            |> Flip.Expect.equal "arity-0 Erased alias is an obj abbreviation to .NET" 1

        testCase "applied Erased.X<...> is a real interface — stays distinct from obj" <| fun _ ->
            let applied = signature' [ param "x" Set.empty (widget "Erased.Foo<string>") ] (intrinsic "unit")
            let viaObj = signature' [ param "x" Set.empty (intrinsic "obj") ] (intrinsic "unit")
            func "invoke" [ applied; viaObj ]
            |> memberCount
            |> Flip.Expect.equal "arity>0 Erased forms are nominal" 2

        testCase "the ledger counts the drop and the union merge" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let dup = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            let ret1 = signature' [ param "y" Set.empty (intrinsic "float") ] (intrinsic "string")
            let ret2 = signature' [ param "y" Set.empty (intrinsic "float") ] (intrinsic "bool")
            FunctionLikeRender.renderAbstract ctx (func "invoke" [ dup; dup; ret1; ret2 ]) |> ignore
            let ledger = GeneratorContext.Advisory.dump ctx |> Map.ofList
            Expect.equal (Map.tryFind "overload-duplicate-drop" ledger) (Some 1) "one exact duplicate dropped"
            Expect.equal (Map.tryFind "overload-return-union" ledger) (Some 1) "one return-union merge"
    ]

// ---------------------------------------------------------------------------
// groupFunctionsByName — pinned through renderInterface (the emission seam)
// ---------------------------------------------------------------------------

[<Tests>]
let groupingTests =
    testList "TypeLikeRender.renderInterface same-name function grouping" [

        testCase "two same-name renders with identical signatures emit ONE member" <| fun _ ->
            let s = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            let text = renderIfaceText GeneratorContext.Empty (iface [ func "invoke" [ s ]; func "invoke" [ s ] ])
            Expect.equal (countOccurrences "abstract invoke" text) 1
                "separate renders of one name fuse before overload unification (FS0438 guard)"

        testCase "two same-name renders with distinct signatures emit as overloads of one name" <| fun _ ->
            let s1 = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            let s2 = signature' [ param "x" Set.empty (intrinsic "float") ] (intrinsic "unit")
            let text = renderIfaceText GeneratorContext.Empty (iface [ func "invoke" [ s1 ]; func "invoke" [ s2 ] ])
            Expect.equal (countOccurrences "abstract invoke" text) 2
                "grouping concatenates signature sets; unification keeps the distinct pair"

        testCase "distinct names are untouched" <| fun _ ->
            let s = signature' [ param "x" Set.empty (intrinsic "string") ] (intrinsic "unit")
            let text = renderIfaceText GeneratorContext.Empty (iface [ func "start" [ s ]; func "stop" [ s ] ])
            Expect.equal (countOccurrences "abstract start" text) 1 "start emits"
            Expect.equal (countOccurrences "abstract stop" text) 1 "stop emits"
    ]
