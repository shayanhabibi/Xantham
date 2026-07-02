module Main

// ====================
// CLI implementation
// ====================

open Xantham.Fable
open Node.Api
open Xantham.Fable.Types
open Fable.Core.JsInterop

let private ensureOutputDir (output: string) =
    let pathIsFile = path.extname output <> ""
    let dirPath =
        if pathIsFile
        then path.join(output, "..")
        else output
    if fs.existsSync(!^dirPath) |> not then
        fs.mkdirSync(dirPath)

let private readFile (file: string) (destination: string) =
    let fn fileExists =
        if fileExists then
            TypeScriptReader.create file
            |> readAndWrite (
                if isNull destination then
                    "output.json"
                else
                    destination
                )
        else failwithf "File not found: %s" file
    fs.exists(!^file, fn)

let private readRecipe (recipePath: string) (destination: string) =
    match Recipe.load recipePath with
    | Error errors ->
        errors |> List.iter (eprintfn "recipe error: %s")
        failwithf "recipe %s did not resolve (%d error(s))" recipePath errors.Length
    | Ok resolved ->
        resolved |> List.iter (fun r -> printfn $"entry: {r.Label} -> {r.File}")
        resolved
        |> List.map _.File
        |> List.toArray
        |> TypeScriptReader.createFor
        |> readAndWrite destination

let printHelp() =
    """
Generate Xantham IR json.

USAGE
    xantham <INPUT> [OPTIONS]           Processes the given input `.d.ts` file.
    xantham --recipe <RECIPE> [OPTIONS] Processes every crawlable [[entry]] of a
                                        recipe TOML (multi-entry, per-entry provenance).

EXAMPLE
    xantham ./node_modules/solid-js/types/index.d.ts
    xantham --recipe cloudflare.pilot.toml -o src/Xantham.Fable/output.json

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
    | "--recipe" :: recipePath :: ("--output" | "-o") :: [ output ] ->
        ensureOutputDir output
        readRecipe recipePath output
    | [ "--recipe"; recipePath ] ->
        readRecipe recipePath "output.json"
    | input :: ("--output" | "-o") :: [ output ] ->
        ensureOutputDir output
        readFile input output
    | [ input ] ->
        readFile input null
    | _ ->
        printHelp()
    0
#endif
