module Xantham.Fable.Temp.Directory
open Fable.Core
open Fable.Core.JsInterop
open Node.Api

open Xantham.Fable
// sync check for temp directory

module Literals =
    [<Literal>]
    let tempDirName = ".xantham"
    [<Literal>]
    let runTempPrefix = "run_"
    [<Literal>]
    let runTempFileName = "temp"
    [<Literal>]
    let tempLogFileName = "log"

let tempDir = "./" + Literals.tempDirName
let runTempFileName = $"{Literals.runTempFileName}.d.ts"
let runTempDirPrefix = $"{Literals.tempDirName}{path.sep}{Literals.runTempPrefix}"

let private mkTempLogName () =
    let now = JS.Constructors.Date.now()
    $"{Literals.tempLogFileName}_{now}.txt"
    

let private runThunkIfTempDirExists (thunk: unit -> unit) =
    if fs.existsSync(!^tempDir) then
        thunk()
        
let private runThunkIfTempDirExistsOrElse (orElse: unit -> unit) (thunk: unit -> unit) =
    if fs.existsSync(!^tempDir) then
        thunk()
    else orElse()

let closeRunDirectory (runDirName: string) =
    let runPath = path.join(tempDir, path.basename runDirName)
    if
        fs.existsSync(!^runPath)
        && fs.lstatSync(!^runPath).isDirectory()
        && try fs.accessSync(!^runPath, fs.constants.W_OK); true with _ -> false
    then
        try
        fs.readdirSync(!^runPath).AsArray
        |> Array.map (fun subpath -> path.join(runPath, subpath))
        |> Array.filter ((!^) >> fs.lstatSync >> _.isFile())
        |> Array.map (fun tempFilePath ->
            fs.accessSync(!^tempFilePath, fs.constants.W_OK)
            tempFilePath
            )
        |> Array.iter ((!^) >> fs.unlinkSync)
        fs.rmdirSync(!^runPath)
        with _ -> ()
    
let private cleanupXanthamDirectory () =
    fs.readdirSync(!^tempDir).AsArray
    |> Array.filter _.StartsWith(Literals.runTempPrefix)
    |> Array.iter (fun runDirName -> path.join(tempDir, runDirName) |> closeRunDirectory)

let closeXanthamDirectory () =
    fun () ->
        cleanupXanthamDirectory()
        try fs.rmdirSync(!^tempDir) with _ -> ()
    |> runThunkIfTempDirExists

let private createAndCleanXanthamDirectory() =
    cleanupXanthamDirectory
    |> runThunkIfTempDirExistsOrElse (fun () -> fs.mkdirSync(tempDir)) 

let private createXanthamDirectory () =
    runThunkIfTempDirExistsOrElse (fun () -> fs.mkdirSync(tempDir)) ignore

let createXanthamRunDirectory () =
    createXanthamDirectory()
    fs.mkdtempSync runTempDirPrefix

let createXanthamLogWriter prependLevel logLevel (runDirectory: string) =
    if fs.existsSync(!^runDirectory) |> not then
        Error $"Run directory {runDirectory} does not exist"
    else
    let stats = fs.lstatSync(!^runDirectory)
    let dir =
        if stats.isFile()
        then path.dirname runDirectory
        else runDirectory
    let fileName = path.join(dir, mkTempLogName())
    Utils.Logging.TextWriterLogger(fileName, prependLevel, logLevel)
    |> Ok

let createXanthamDummyFileWithRefs (runDirectory: string) (paths: string seq) =
    let tempFilePath = path.join(runDirectory, runTempFileName)
    fs.writeFileSync(tempFilePath, String.concat "\n" <| [
        for entryFile in paths do
            "import * as _ from '" + String.normalizePath entryFile + "';"
    ])
    tempFilePath
