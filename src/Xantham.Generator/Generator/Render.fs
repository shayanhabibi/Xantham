module Xantham.Generator.Generator.Render

open System
open Fantomas.FCS.IO
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Fabulous.AST
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

open Xantham.Generator.Types

//
[<EntryPoint>]
let main argv =
    let file = IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
    let tree = Decoder.Runtime.create file
    let interner = tree.GetArenaInterner()
    // ---- Hook helpers ---------------------------------------------------
    //
    // Migrated from the old `Customisation.Interceptors` block. Two
    // policies, registered as separate hook handlers:
    //   1. Library-ES types AND types whose source is babel/typescript
    //      render ref-only (was `ResolvedTypePrelude` + `IgnorePathRender.Source`).
    //      Both kept the type *reference* in `AnchorRenders` while skipping
    //      the type *definition* render — so refs from other types still
    //      resolve. Implemented as a single `RenderScopeBuild` `Replace` to
    //      `RefOnly`, NOT a `PathResolution Skip` (which would drop the ref).
    //   2. For library-ES types, prune any "Typescript" parent from the
    //      path (was `Paths.TypePaths` / `Paths.MemberPaths`).
    let sourceMatches (text: string) =
        text.Contains("babel", StringComparison.OrdinalIgnoreCase)
        || text.Contains("typescript", StringComparison.OrdinalIgnoreCase)

    let ownerSourceMatches (owner: RenderOwner voption) =
        let src =
            match owner with
            | ValueSome (RenderOwner.Type t) ->
                match t with
                | ResolvedType.Interface i -> i.Source
                | ResolvedType.Class c -> c.Source
                | ResolvedType.Enum e -> e.Source
                | _ -> None
            | ValueSome (RenderOwner.Export e) ->
                match e with
                | ResolvedExport.Interface i -> i.Source
                | ResolvedExport.Class c -> c.Source
                | ResolvedExport.Enum en -> en.Source
                | ResolvedExport.TypeAlias ta -> ta.Source
                | ResolvedExport.Variable v -> v.Source
                | ResolvedExport.Function (f :: _) -> f.Source
                | _ -> None
            | ValueNone -> None
        match src with
        | Some (QualifiedNamePart.Normal text)
        | Some (QualifiedNamePart.Abnormal(text, _)) -> sourceMatches text
        | None -> false

    let isLibEsOwner (owner: RenderOwner voption) =
        match owner with
        | ValueSome (RenderOwner.Type t) ->
            match t with
            | ResolvedType.Interface { IsLibEs = true }
            | ResolvedType.Class { IsLibEs = true }
            | ResolvedType.Enum { IsLibEs = true } -> true
            | _ -> false
        | ValueSome (RenderOwner.Export e) ->
            match e with
            | ResolvedExport.Interface { IsLibEs = true }
            | ResolvedExport.Class { IsLibEs = true }
            | ResolvedExport.Enum { IsLibEs = true }
            | ResolvedExport.TypeAlias { IsLibEs = true } -> true
            | _ -> false
        | ValueNone -> false

    // Combined hook: IsLibEs OR babel/typescript source ⇒ keep the ref,
    // collapse the def render to RefOnly. This preserves the original
    // `IgnorePathRender.Source` semantics, where the type reference still
    // landed in `AnchorRenders` and only the def render was skipped.
    let renderScopeBuildHook : SkippableHook<RenderScope> =
        fun _ rctx renderScope ->
            if isLibEsOwner rctx.Owner || ownerSourceMatches rctx.Owner then
                SkippableHookResult.Replace { renderScope with Render = Render.RefOnly renderScope.TypeRef }
            else SkippableHookResult.Pass

    let pruneTypescriptParentType : SkippableHook<TypePath> =
        fun _ rctx s ->
            if isLibEsOwner rctx.Owner then
                SkippableHookResult.Replace
                    (TypePath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s)
            else SkippableHookResult.Pass

    let pruneTypescriptParentMember : SkippableHook<MemberPath> =
        fun _ rctx s ->
            if isLibEsOwner rctx.Owner then
                SkippableHookResult.Replace
                    (MemberPath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s)
            else SkippableHookResult.Pass

    let generatorContext: GeneratorContext =
         GeneratorContext.EmptyWithCustomisation (fun customiser ->
             customiser
             |> Customisation.addRenderScopeBuild renderScopeBuildHook
             |> Customisation.addPathResolutionType pruneTypescriptParentType
             |> Customisation.addPathResolutionMember pruneTypescriptParentMember
         )
    ArenaInterner.prerenderTypeAliases generatorContext interner
    // ArenaInterner.prerenderFromGraph generatorContext interner
    ArenaInterner.processExports generatorContext interner
    let renders =
        RootModule.collectModules generatorContext
        // |> _.Modules["Typescript"]
        // |> renderModule generatorContext
        |> renderRoot generatorContext
    // generatorContext.AnchorRenders
    // |> Seq.take 100
    // |> Seq.choose (_.Value >> function
    //     | Choice1Of2 x ->
    //         Ast.Value("_", "jsNative", TypeRefRender.Anchored.render x)
    //         |> Choice1Of2
    //         |> Some
    //     | Choice2Of2 x ->
    //         match x.Render.Deconstruct() |> snd |> _.Value with
    //         | TypeRender.EnumUnion enumUnion ->
    //             LiteralUnionRender.renderEnum generatorContext enumUnion
    //             |> Choice2Of2
    //             |> Some
    //         | TypeDefn typeLikeRender ->
    //             TypeLikeRender.renderClass generatorContext typeLikeRender
    //             |> Choice2Of2
    //             |> Some
    //         | _ -> None
    //         )
    // |> fun x ->
    Ast.Oak() {
        // Ast.AnonymousModule() {
        //     for x in x do
        //         match x with
        //         | Choice1Of2 x -> x
        //         | Choice2Of2 x -> x
        // }
        // Ast.AnonymousModule() {
        //     for i in erasedUnion.UnionLengths |> Seq.sort do
        //         SpecialRender.renderErasedUnion i
        // }
        Ast.AnonymousModule() {
            
            renders
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> fun s ->
        IO.File.OpenWrite("test.fs").WriteAllText(s)
        s
    |> printfn "%s"
    // |> Seq.iter (printfn "%A")
    0