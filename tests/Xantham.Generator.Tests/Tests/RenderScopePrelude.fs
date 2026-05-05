module Xantham.Generator.Tests.Tests.RenderScopePrelude

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

// prerender's `ResolvedType.TypeLiteral` handler used to set the new
// RenderScope's Root to a hardcoded `TransientTypePath.Anchored`.
// That made distinct TypeLiterals processed under different scopes
// collapse onto the same final TypePath after anchoring, and
// `combine` then merged their members into a synthesized type that
// shouldn't exist (e.g. a function's call-signature literal merging
// with one of its inline parameter-shape literals on the same path).
//
// The fix grafts `scope.PathContext` onto the transient root so that
// each TypeLiteral carries the nesting of where it was processed.
// These tests pin that contract directly on prerender's output.

[<Tests>]
let tests =
    testList "prerender — TypeLiteral Root carries scope.PathContext" [

        // Empty PathContext: Root must remain Anchored. Top-level
        // TypeLiterals (e.g. a function's call-signature type literal
        // with no surrounding parameter scope) anchor at their export
        // path's leaf, with no transient prefix.
        testCase "empty PathContext → Root is Anchored" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let scope = RenderScopeStore.create ()
            // Two-property TypeLiteral hits the multi-member branch.
            let typeLit =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "a" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.addMember (Property.create "b" (primitive TypeKindPrimitive.Integer) |> Property.wrap)
            let resolvedType = ResolvedType.TypeLiteral typeLit
            prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> resolvedType)
            |> ignore
            let renderScope = GeneratorContext.Prelude.tryGet ctx resolvedType
            match renderScope with
            | ValueSome { Root = ValueSome (TypeLikePath.Transient TransientTypePath.Anchored) } ->
                ()  // expected
            | ValueSome { Root = root } ->
                failtestf "expected Root = TransientTypePath.Anchored for empty PathContext; got %A" root
            | ValueNone ->
                failtest "expected RenderScope to be registered for the TypeLiteral"

        // Non-empty PathContext (one segment): Root must Moor the
        // segment so the TypeLiteral doesn't collide with a top-level
        // sibling literal.
        testCase "PathContext with one segment → Root is Moored at that segment" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let scope =
                RenderScopeStore.create ()
                |> fun s -> RenderScopeStore.appendStringToPathContext s "context"
            let typeLit =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "env" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.addMember (Property.create "ctx" (primitive TypeKindPrimitive.Integer) |> Property.wrap)
            let resolvedType = ResolvedType.TypeLiteral typeLit
            prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> resolvedType)
            |> ignore
            let renderScope = GeneratorContext.Prelude.tryGet ctx resolvedType
            match renderScope with
            | ValueSome { Root = ValueSome (TypeLikePath.Transient (TransientTypePath.Moored(_, name))) } ->
                Expect.equal
                    (Name.Case.valueOrSource name)
                    "context"
                    "expected Root's leaf segment to match scope.PathContext"
            | ValueSome { Root = root } ->
                failtestf "expected Moored Root for one-segment PathContext; got %A" root
            | ValueNone ->
                failtest "expected RenderScope to be registered for the TypeLiteral"

        // Two distinct TypeLiterals processed under different scopes
        // must produce distinct Roots — that's the property the bug
        // violated, leading to the merge in combine.
        testCase "two TypeLiterals under different scopes produce distinct Roots" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // First literal under empty PathContext
            let scopeA = RenderScopeStore.create ()
            let litA =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "a" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.addMember (Property.create "b" (primitive TypeKindPrimitive.Integer) |> Property.wrap)
            let typeA = ResolvedType.TypeLiteral litA
            prerender ctx scopeA (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeA) |> ignore
            // Second literal under a non-empty PathContext
            let scopeB =
                RenderScopeStore.create ()
                |> fun s -> RenderScopeStore.appendStringToPathContext s "param"
            let litB =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "x" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.addMember (Property.create "y" (primitive TypeKindPrimitive.Integer) |> Property.wrap)
            let typeB = ResolvedType.TypeLiteral litB
            prerender ctx scopeB (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeB) |> ignore

            let rootA =
                GeneratorContext.Prelude.tryGet ctx typeA
                |> ValueOption.bind (fun s -> s.Root |> ValueOption.map id)
            let rootB =
                GeneratorContext.Prelude.tryGet ctx typeB
                |> ValueOption.bind (fun s -> s.Root |> ValueOption.map id)
            match rootA, rootB with
            | ValueSome a, ValueSome b ->
                Expect.notEqual a b
                    "expected the two TypeLiterals' Roots to differ when their scopes differ"
            | _ ->
                failtest "expected both TypeLiterals to register render scopes"

        // The same underlying ResolvedType reached through TWO different
        // (independently constructed) Lazy wrappers, processed under TWO
        // different scope contexts, must NOT cause the second processing
        // to overwrite the first in the prelude. Previously the cache
        // check `valueIsCreated && cachedRenderValue.IsSome` short-
        // circuited on each fresh Lazy (since IsValueCreated reflects the
        // local Lazy, not the cached value), so the second call bypassed
        // the cache and rewrote the prelude entry with a Root derived
        // from its own scope. The fix makes cache-hit take precedence
        // regardless of the local Lazy's force state.
        testCase "second prerender of the same ResolvedType (via fresh Lazy) hits cache" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let typeLit =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "env" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.addMember (Property.create "ctx" (primitive TypeKindPrimitive.Integer) |> Property.wrap)
            let resolvedType = ResolvedType.TypeLiteral typeLit
            // First call: deferred Lazy (matches encoder's `lazy resolve ...` shape;
            // IsValueCreated starts false). PathContext = "first".
            let scope1 =
                RenderScopeStore.create ()
                |> fun s -> RenderScopeStore.appendStringToPathContext s "first"
            let firstLazy: LazyContainer<TypeKey, ResolvedType> =
                { Data = 0; Result = lazy resolvedType }
            prerender ctx scope1 firstLazy |> ignore
            let rootAfterFirst =
                GeneratorContext.Prelude.tryGet ctx resolvedType
                |> ValueOption.map (fun s -> s.Root)
            // Second call: a different deferred Lazy wrapping the SAME
            // resolvedType, but under a DIFFERENT PathContext. With the
            // bug, this overwrites the first. With the fix, cache wins.
            let scope2 =
                RenderScopeStore.create ()
                |> fun s -> RenderScopeStore.appendStringToPathContext s "second"
            let secondLazy: LazyContainer<TypeKey, ResolvedType> =
                { Data = 0; Result = lazy resolvedType }
            prerender ctx scope2 secondLazy |> ignore
            let rootAfterSecond =
                GeneratorContext.Prelude.tryGet ctx resolvedType
                |> ValueOption.map (fun s -> s.Root)
            Expect.equal rootAfterFirst rootAfterSecond
                "expected the prelude Root for the same ResolvedType to be unchanged after a second prerender via a fresh Lazy"
    ]
