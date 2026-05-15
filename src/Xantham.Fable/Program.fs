module Main

// ====================
// CLI implementation
// ====================

open Fable.Core
open Xantham.Fable
open Node.Api
open Xantham.Fable.Types
open Fable.Core.JsInterop
open Xantham.Fable.Types.SourceTag

#if !RELEASE && !FABLE_TEST
// Can insert play/debug code here.
#endif

let private readFile (argsv: string list) (file: string) (destination: string) =
    let reader =
        #if !RELEASE && !FABLE_TEST
        // If in debug mode, and not in a test environment, create a log entry.
        TypeScriptReader.createWithLogger file
        #else
        // In release mode, create a log entry only if the `--debug` flag is passed.
        if argsv |> List.contains "--debug" then
            TypeScriptReader.createWithLogger file
        else TypeScriptReader.create file
        #endif
    reader
    |> readAndWrite (
        if isNull destination then
            "output.json"
        else
            destination
        )
    // Keep the temp directory open if the `--debug` flag is passed.
    // Otherwise, close it.
    if argsv |> List.contains "--debug" |> not then
        reader.tempDirectory
        |> Temp.Directory.closeRunDirectory

let printHelp() =
    """
Generate Xantham IR json.

USAGE
    xantham <INPUT> [OPTIONS]       Processes the given input (installed) package or `.d.ts` file.
    xantham clean                   Clean & remove the temporary directory created by this tool.

EXAMPLE
    xantham solid-js

OPTIONS
    --help                          Prints this message.
    -o, --output <OUTPUT>           Sets the output path for the generated json.
    --clean                         Removes any stale folders in the `.xantham` directory at the end of the operation.
    --debug                         Logs are written to {working directory}/.xantham/run_*/log_*.txt
"""
    |> printfn "%s"
    
#if !FABLE_TEST
[<EntryPoint>]
let main argv =
    let argv = argv |> Array.toList
    let inline postCommandOps() =
        if List.contains "--clean" argv then
            Temp.Directory.closeXanthamDirectory()
    match argv with
    | args when args |> List.contains "--help" || args = [] -> printHelp()
    | "clean" :: _ ->
        Temp.Directory.closeXanthamDirectory()
    | input :: ("--output" | "-o") :: [ output ] ->
        let pathIsFile = path.extname output <> ""
        let dirPath =
            if pathIsFile
            then path.join(output, "..")
            else output
        if fs.existsSync(!^dirPath) |> not then
            fs.mkdirSync(dirPath)
        readFile argv input output
        postCommandOps()
    | [ input ] ->
        readFile argv input null
        postCommandOps()
    | _ ->
        printHelp()
    0
#endif