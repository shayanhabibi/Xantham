module Xantham.Generator.Tests.Tests.RenderScopeAnchored

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

// After upstream PR #55 wired exportdeclaration into export specifiers, a
// renamed re-export (`dispatcherBindingImpl as _dispatcherBindingImpl`) and the
// canonical declaration both flow into a single `ResolvedExport.Function` as a
// list of two reference-different Function records with structurally identical
// rendered signatures. Because Function carries [<ReferenceEquality>],
// List.distinct on the funcs themselves doesn't dedup; only the resulting
// FunctionLikeSignature records (plain F# records → structural equality) are
// deduplicable. Visible symptom: duplicate `_dispatcherBindingImpl` static
// member in dynamic-workflows.fs. Fix: List.distinct on the constructed
// Signatures list inside `registerAnchorFromExport`.

[<Tests>]
let tests =
    testList "registerAnchorFromExport — Function dedup" [
        testCase "two reference-different Functions with same content collapse to one signature" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // Two structurally identical Function values, different references
            // (mirrors the post-#55 case where canonical and renamed export
            // bind to the same shape).
            let func1 = Function.create "doThing" (primitive TypeKindPrimitive.String)
            let func2 = Function.create "doThing" (primitive TypeKindPrimitive.String)
            // Confirm they are reference-different but structurally equal in
            // the resulting paths (sanity: this is the regression scenario)
            Expect.isFalse (System.Object.ReferenceEquals(func1, func2))
                "test mocks must produce reference-different Function records"
            let export = ResolvedExport.Function [ func1; func2 ]
            registerAnchorFromExport ctx export
            // Find the registered FunctionLikeRender for this export.
            let renderScope =
                ctx.AnchorRenders
                |> Seq.choose (function
                    | KeyValue(_, Choice2Of2 scope) -> Some scope
                    | _ -> None)
                |> Seq.tryHead
            match renderScope with
            | None -> failtest "expected a render scope to be registered"
            | Some scope ->
                match snd scope.Render |> _.Value with
                | Anchored.TypeRender.Function functionLike ->
                    Flip.Expect.equal
                        "two duplicate input funcs should collapse to a single signature"
                        1
                        functionLike.Signatures.Length
                | other ->
                    failtestf "expected TypeRender.Function but got %A" other

        testCase "single Function produces one signature (baseline)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let func = Function.create "doThing" (primitive TypeKindPrimitive.String)
            let export = ResolvedExport.Function [ func ]
            registerAnchorFromExport ctx export
            let renderScope =
                ctx.AnchorRenders
                |> Seq.choose (function
                    | KeyValue(_, Choice2Of2 scope) -> Some scope
                    | _ -> None)
                |> Seq.tryHead
            match renderScope with
            | None -> failtest "expected a render scope to be registered"
            | Some scope ->
                match snd scope.Render |> _.Value with
                | Anchored.TypeRender.Function functionLike ->
                    Flip.Expect.equal "single func → single signature" 1 functionLike.Signatures.Length
                | other ->
                    failtestf "expected TypeRender.Function but got %A" other
    ]
