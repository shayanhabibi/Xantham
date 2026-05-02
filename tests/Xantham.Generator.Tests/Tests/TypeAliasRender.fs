module Xantham.Generator.Tests.Tests.TypeAliasRender

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

// Issue #39: https://github.com/shayanhabibi/Xantham/issues/39
// TypeAlias render of a self-referencing type alias causes infinite recursion.
//
// Root cause: in resolveInnerRef (Render.TypeAlias), calling
//   TypeRefRender.replace value newRef.TypeRef stripped
// where value = TypeAliasRemap[innerType] and newRef.TypeRef = PreludeRenders[innerType].TypeRef.
// When the inner type is a self-reference both sides resolve to the same TypeRefRender (call it A),
// so replace(A, A, A) recurses on itself indefinitely.
//
// Fix: add `if old = new' then render` guard at the top of TypeRefRender.replace.

let private ctx = GeneratorContext.Empty

// ---------------------------------------------------------------------------
// TypeRefRender.replace — root-cause unit tests
// ---------------------------------------------------------------------------

let typeRefReplaceTests =
    testList "TypeRefRender.replace" [

        testCase "replace substitutes render when it matches old" <| fun _ ->
            let strRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.String)
            let intRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.Integer)
            // replace strRef with intRef everywhere in strRef → intRef
            TypeRefRender.replace strRef intRef strRef
            |> Flip.Expect.equal "" intRef

        testCase "replace is no-op when render does not match old" <| fun _ ->
            let strRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.String)
            let intRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.Integer)
            // intRef does not equal strRef, so nothing is replaced
            TypeRefRender.replace strRef intRef intRef
            |> Flip.Expect.equal "" intRef

        // replace(A, A, A) must be identity; old fix caused infinite recursion.
        testCase "replace is identity when old equals new' (issue #39)" <| fun _ ->
            let strRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.String)
            TypeRefRender.replace strRef strRef strRef
            |> Flip.Expect.equal "" strRef
    ]

// ---------------------------------------------------------------------------
// TypeAlias.render — self-reference integration tests
// ---------------------------------------------------------------------------

let typeAliasSelfReferenceTests =
    testList "TypeAlias self-referencing render" [

        // Reproduces issue #39: type Simplify<'T> = Simplify — inner type maps to itself via TypeAliasRemap.
        testCase "self-referencing alias resolves without infinite recursion (issue #39)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // NonPrimitive prerenders to { Intrinsic "obj"; Nullable = false } = innerRef
            let innerResolvedType = primitive TypeKindPrimitive.NonPrimitive
            // Populate PreludeRenders so PreludeRenders[innerResolvedType].TypeRef = innerRef
            let innerRef = TestHelper.prerender ctx innerResolvedType
            // Simulate prerenderTypeAliases: innerResolvedType → innerRef
            // This makes TypeAliasRemap[innerType] = innerRef = PreludeRenders[innerType].TypeRef
            GeneratorContext.Prelude.addTypeAliasRemap ctx innerResolvedType innerRef
            let typeAlias = TypeAlias.create innerResolvedType "Simplify"
            let scopeStore = RenderScopeStore.create()
            // resolveInnerRef calls replace(innerRef, innerRef, innerRef); must not recurse.
            let result = Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            match result with
            | TypeAliasRender.Alias aliasRef ->
                aliasRef.Type
                |> Flip.Expect.equal "self-referencing alias Type should be the alias ref itself" innerRef
            | other ->
                failtestf "expected TypeAliasRender.Alias but got %A" other
    ]

[<Tests>]
let tests =
    testList "Issue #39" [
        typeRefReplaceTests
        typeAliasSelfReferenceTests
    ]
