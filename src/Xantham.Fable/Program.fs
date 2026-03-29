module Main

// ====================
// CLI implementation
// ====================

open Xantham.Fable
open Xantham.Fable.Types

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