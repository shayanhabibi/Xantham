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

        // Encountered with the agents and workers-types packages: when the
        // substitution target (`new'`) contains the value being substituted
        // (`old`) — e.g. old=A, new'=Union<A,B> — the previous code recursed
        // into new' via `replace old new' new'`, which immediately re-entered
        // the same case for new's inner A, looping forever. Fix: when
        // render = old, return new' without further recursion. The substitute
        // is the value to insert verbatim; sub-references inside it are not
        // additional substitution opportunities.
        testCase "replace returns new' verbatim when render = old and new' contains old" <| fun _ ->
            let strRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.String)
            let unionContainingStr =
                TestHelper.prerender ctx (
                    Union.create [
                        primitive TypeKindPrimitive.String
                        primitive TypeKindPrimitive.Integer
                    ])
            // Pre-condition sanity: new' really does contain old structurally.
            // (If this changes, the test is no longer covering the regression.)
            Expect.notEqual unionContainingStr strRef
                "new' must be different from old to exercise the second guard"
            TypeRefRender.replace strRef unionContainingStr strRef
            |> Flip.Expect.equal "" unionContainingStr
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
            // Simulate prerenderTypeAliases: innerType's TypeKey → innerRef.
            // TypeAliasRemap is keyed by TypeKey (the encoder's stable identity)
            // — TestHelper.prerender wraps via CreateFromValue (Raw = default TypeKey),
            // so the alias body's lookup must match that same default key.
            let innerTypeKey = Unchecked.defaultof<TypeKey>
            GeneratorContext.Prelude.addTypeAliasRemap ctx innerTypeKey innerRef
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

        // C1 cycle-break: when a TypeAlias's body contains a TypeReference
        // back to the alias itself, the prerender path should substitute
        // that recursive reference with `obj` so the emitted F# is a
        // valid alias rather than a recursive abbreviation (FS0953).
        //
        // Real-world case (workers-types):
        //   type Stubable = U2<RpcTargetBranded, Stubable>
        //   ↓ rendering with C1
        //   type Stubable = obj
        //
        // Here we build a Union whose second branch is a TypeReference to
        // the alias body. Without RenderingAliasBodyKeys protection, the
        // remap path inside prerender would return the alias's own
        // ConcretePath ref, producing a recursive emission. With C1, the
        // remap detects the in-flight body and short-circuits to obj.
        testCase "cycle-break for self-referencing alias body (C1)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // Construct a Union body whose second branch references the
            // alias body itself. The encoder produces this shape for TS
            // recursive aliases like `type T = A | T` (a degenerate but
            // legal TS construct).
            let firstBranch = primitive TypeKindPrimitive.String
            let recursiveBranch =
                ResolvedType.TypeReference {
                    Type = LazyContainer.CreateFromValue firstBranch  // placeholder, replaced below
                    TypeArguments = []
                    ResolvedType = None
                }
            let body = Union.create [ firstBranch; recursiveBranch ]
            // Wire the recursive branch's Type to the body itself so
            // prerender of that branch sees the body's TypeKey (default).
            let body = body
            // TypeAliasRemap entry: body's TypeKey → alias's ConcretePath
            // ref. This is what the render path would otherwise return for
            // the recursive branch — and what cycle-break must override.
            let aliasPathRef = TestHelper.prerender ctx (primitive TypeKindPrimitive.NonPrimitive)
            let bodyKey = Unchecked.defaultof<TypeKey>
            GeneratorContext.Prelude.addTypeAliasRemap ctx bodyKey aliasPathRef
            // Now render the alias. With C1, push of bodyKey to
            // RenderingAliasBodyKeys happens at the top of TypeAlias.render;
            // the recursive branch's prerender hits remap → cycle-break.
            let typeAlias = TypeAlias.create body "Recursive"
            let scopeStore = RenderScopeStore.create()
            let _ = Render_TypeAlias.TypeAlias.render ctx scopeStore typeAlias
            // After rendering, the in-flight target ref MUST have been
            // removed so it doesn't poison subsequent rendering. This is
            // the most observable invariant that doesn't depend on the
            // internal structure of the rendered Union.
            Expect.isFalse
                (ctx.RenderingAliasTargetRefs.Contains(aliasPathRef))
                "RenderingAliasTargetRefs should release the alias's target after render returns"
    ]

[<Tests>]
let tests =
    testList "Issue #39" [
        typeRefReplaceTests
        typeAliasSelfReferenceTests
    ]
