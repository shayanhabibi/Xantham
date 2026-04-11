module Xantham.Generator.Generator.Render

open System
open Xantham
open Xantham.Generator
open Fabulous.AST
//
// [<EntryPoint>]
// let main argv =
//     let file = IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
//     let tree = Decoder.Runtime.create file
//     let interner = tree.GetArenaInterner()
//     let generatorContext = GeneratorContext.Empty
//     Prerender.prerenderTypeRefs
//         generatorContext
//         (
//             interner.ExportMap
//             |> Seq.collect _.Value
//             |> Seq.toList
//         )
//     generatorContext.TypeRefRenders
//     |> Seq.take 5
//     |> Seq.map _.Value
//     |> Seq.map TypeRefRender.TypeRefRender.render
//     |> fun x ->
//         Ast.Oak() {
//             Ast.AnonymousModule() {
//                 for x in x do
//                     Ast.Value("_", "jsNative",x)
//             }
//         }
//         |> Gen.mkOak
//         |> Gen.run
//         |> printfn "%s"
//     // |> Seq.iter (printfn "%A")
//     0