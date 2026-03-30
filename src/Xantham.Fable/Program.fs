module Main

// ====================
// CLI implementation
// ====================

open Xantham.Fable
open Node.Api
open Xantham.Fable.Types

// let dtsFile = path.join(__SOURCE_DIRECTORY__, "../../node_modules/solid-js/types/index.d.ts")
let dtsFile file = path.join(__SOURCE_DIRECTORY__, $"../../node_modules/{file}")
let reader =
    // dtsFile "solid-js/types/index.d.ts"
    dtsFile "typescript/lib/typescript.d.ts"
    |> TypeScriptReader.create 

reader
|> readAndWrite (__SOURCE_DIRECTORY__ + "/output.json")

let private readFile (file: string) (destination: string) =
    TypeScriptReader.create file
    |> readAndWrite (
        if isNull destination then
            __SOURCE_DIRECTORY__ + "/output.json"
        else
            destination
        )

let printHelp() =
    """
Generate Xantham IR json.

USAGE
    xantham <input> [--output <output>]
"""
    |> printfn "%s"
    
[<EntryPoint>]
let main argv =
    let argv = argv |> Array.toList
    match argv with
    | args when args |> List.contains "--help" || args = [] -> printHelp()
    | input :: "--output" :: [ output ] ->
        readFile input output
    | [ input ] ->
        readFile input null
    | _ ->
        printHelp()
    0