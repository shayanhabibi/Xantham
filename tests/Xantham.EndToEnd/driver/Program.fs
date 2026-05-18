module Xantham.Driver.Program

open System
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Fabulous.AST
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

[<EntryPoint>]
let main argv =
    let inputFile, outputFile =
        match argv with
        | [| input; output |] -> input, output
        | _ ->
            eprintfn "Usage: dotnet run -- <input.json> <output.fs>"
            exit 1

    if not (IO.File.Exists inputFile) then
        eprintfn "Input file not found: %s" inputFile
        exit 1

    let tree = Decoder.Runtime.create inputFile
    let interner = tree.GetArenaInterner()

    // The Xantham defaults handle:
    //   - lib.es type substitutions (Error→exn, Array→ResizeArray, etc.)
    //   - Typescript-namespace pruning in TypePaths/MemberPaths
    //   - babel/typescript source ignore in IgnorePathRender
    // No Cloudflare-specific overrides are currently needed.
    let generatorContext: GeneratorContext = GeneratorContext.Empty

    ArenaInterner.prerenderTypeAliases generatorContext interner
    ArenaInterner.processExports generatorContext interner

    let renders =
        RootModule.collectModules generatorContext
        |> renderRoot generatorContext

    let output =
        Ast.Oak() {
            Ast.Namespace("Xantham") {
                Ast.Open "System"
                Ast.Open "Fable.Core"
                Ast.Open "Fable.Core.JS"
                Ast.Open "Fable.Core.JsInterop"
                renders
            }
        }
        |> Gen.mkOak
        |> Gen.run

    IO.File.WriteAllText(outputFile, output)
    printfn "Wrote %d bytes to %s" output.Length outputFile
    0
