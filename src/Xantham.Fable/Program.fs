module Main

// ====================
// CLI implementation
// ====================

open Xantham.Fable
open Node.Api
open Xantham.Fable.Types
open Fable.Core.JsInterop

#if !FABLE_TEST
// let dtsFile = path.join(__SOURCE_DIRECTORY__, "../../node_modules/solid-js/types/index.d.ts")
let dtsFile file = path.join(__SOURCE_DIRECTORY__, $"../../node_modules/{file}")
let reader =
    dtsFile "solid-js/types/index.d.ts"
    // dtsFile "typescript/lib/lib.dom.d.ts"
    // path.join(__SOURCE_DIRECTORY__, "../../tests/Xantham.Fable.Tests/TypeFiles/multiple-extends.d.ts")
    |> TypeScriptReader.create 

reader
|> readAndWrite (__SOURCE_DIRECTORY__ + "/output.json")
#endif

let private readFile (file: string) (destination: string) =
    let fn fileExists =
        if fileExists then
            TypeScriptReader.create file
            |> readAndWrite (
                if isNull destination then
                    __SOURCE_DIRECTORY__ + "/output.json"
                else
                    destination
                )
        else failwithf "File not found: %s" file
    fs.exists(!^file, fn)

let printHelp() =
    """
Generate Xantham IR json.

USAGE
    xantham <INPUT> [OPTIONS]       Processes the given input `.d.ts` file.

EXAMPLE
    xantham ./node_modules/solid-js/types/index.d.ts

OPTIONS
    --help                          Prints this message.
    -o, --output <OUTPUT>           Sets the output path for the generated json.
"""
    |> printfn "%s"
    
#if !FABLE_TEST
[<EntryPoint>]
let main argv =
    let argv = argv |> Array.toList
    match argv with
    | args when args |> List.contains "--help" || args = [] -> printHelp()
    | input :: ("--output" | "-o") :: [ output ] ->
        readFile input output
    | [ input ] ->
        readFile input null
    | _ ->
        printHelp()
    0
#endif