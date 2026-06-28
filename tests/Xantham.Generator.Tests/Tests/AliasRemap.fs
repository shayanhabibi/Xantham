module Xantham.Generator.Tests.Tests.AliasRemap

open Expecto
open Xantham
open Xantham.Generator.Generator
open Mocking.ArenaInterner

// Regression coverage for the alias-over-primitive collapse: a type alias whose
// underlying body is a shared/memoised resolved type (a primitive, a literal, or
// globalThis) must NOT be registered into TypeAliasRemap. Otherwise the alias
// name (e.g. D1SessionBookmark for `type D1SessionBookmark = string`) hijacks
// every occurrence of that primitive across the whole generated surface.
[<Tests>]
let isShareableAliasBodyTests =
    testList "ArenaInterner.isShareableAliasBody" [
        testTheory "shared/non-nominal bodies are NOT remappable (skipped)" [
            ResolvedType.primitive TypeKindPrimitive.String      // type D1SessionBookmark = string
            ResolvedType.primitive TypeKindPrimitive.Number
            ResolvedType.primitive TypeKindPrimitive.Boolean
            ResolvedType.globalThis
            ResolvedType.Literal.wrap (ResolvedType.Literal.createString "primary-only") // type Mode = "primary-only"
            ResolvedType.Literal.wrap (ResolvedType.Literal.createInt 1)
        ] <| fun body ->
            ArenaInterner.isShareableAliasBody body
            |> Flip.Expect.isTrue "shared body must be treated as non-remappable"

        testTheory "nominal/structural bodies ARE remappable (kept)" [
            ResolvedType.Interface.create "MyInterface" |> ResolvedType.Interface.wrap  // type Alias = MyInterface
            ResolvedType.Union.create [
                ResolvedType.Interface.create "A" |> ResolvedType.Interface.wrap
                ResolvedType.Interface.create "B" |> ResolvedType.Interface.wrap
            ]                                                     // type U = A | B (reference union)
            ResolvedType.Interface.create "Item" |> ResolvedType.Interface.wrap |> ResolvedType.Array.create
        ] <| fun body ->
            ArenaInterner.isShareableAliasBody body
            |> Flip.Expect.isFalse "nominal body must remain remappable"
    ]
