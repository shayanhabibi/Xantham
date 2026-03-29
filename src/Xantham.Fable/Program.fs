module Main

// ====================
// CLI implementation
// ====================

open Xantham
open Xantham.Fable
open Node.Api
open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json
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
    
#if !DEBUG
[<EntryPoint>]
let main argv =
    let argv = argv |> Array.toList
    match argv with
    | [] ->
        printHelp()
    | input :: "--output" :: [ output ] ->
        readFile input output
    | [ input ] ->
        readFile input null
    | _ ->
        printHelp()
    0
#endif