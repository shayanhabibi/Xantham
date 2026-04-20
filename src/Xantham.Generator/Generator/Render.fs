module Xantham.Generator.Generator.Render

open System
open Xantham
open Xantham.Generator
open Fabulous.AST
open Xantham.Decoder.ArenaInterner

open Xantham.Generator.Types

//
[<EntryPoint>]
let main argv =
    let file = IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
    let tree = Decoder.Runtime.create file
    let interner = tree.GetArenaInterner()
    let generatorContext: GeneratorContext =
         GeneratorContext.EmptyWithCustomisation (fun customiser ->
         {
             customiser with
                 Customisation.Interceptors.ResolvedTypePrelude = fun _ -> function
                     | ResolvedType.Interface { IsLibEs = true }
                     | ResolvedType.Class { IsLibEs = true }
                     | ResolvedType.Enum { IsLibEs = true } -> fun renderScope ->
                         { renderScope with Render = Render.RefOnly renderScope.TypeRef }
                     | _ -> id
                 Customisation.Interceptors.IgnorePathRender.Source = function
                     | QualifiedNamePart.Normal(text)
                     | QualifiedNamePart.Abnormal(text,_) -> text.Contains("babel", StringComparison.OrdinalIgnoreCase) }
             )
    ArenaInterner.prerenderFromGraph generatorContext interner
    ArenaInterner.processExports generatorContext interner
    let renders =
        collectModules generatorContext
        |> _.Modules["Typescript"]
        |> renderModule generatorContext
        // |> renderRoot generatorContext
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
    |> printfn "%s"
    // |> Seq.iter (printfn "%A")
    0