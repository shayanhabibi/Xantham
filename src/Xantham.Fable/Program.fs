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
// let dtsFile = path.join(__SOURCE_DIRECTORY__, "../../node_modules/solid-js/types/index.d.ts")
let dtsFile file = path.join(__SOURCE_DIRECTORY__, $"../../node_modules/{file}")
let reader =
    // dtsFile "agents/dist/index.d.ts"
    dtsFile "agents/dist/index.d.ts"
    // dtsFile "solid-js/types/index.d.ts"
    // dtsFile "typescript/lib/lib.dom.d.ts"
    // path.join(__SOURCE_DIRECTORY__, "../../tests/Xantham.Fable.Tests/TypeFiles/multiple-extends.d.ts")
    |> TypeScriptReader.create 

reader
|> readAndWrite (__SOURCE_DIRECTORY__ + "/output.json")

open TypeScript
// reader.program.forEachResolvedModule(fun f m r p ->
//     Log.traceTo 2 f
//     Log.trace m
//     Log.trace p
//     f.resolvedModule
//     |> Option.bind (_.resolvedFileName >> reader.program.getSourceFile)
//     |> Log.traceTo 1
//     )
reader.program.SeedResolvedModules()
let packages = reader.program.CompilePackageCache()
// reader.program.getSourceFiles().AsArray
// |> Array.iter (fun sf ->
//     // let guard = SourceGuard.create reader.program reader.checker sf
//     // guard
//     // |> Log.traceTo 1
//     // if guard.Tag.IsSome then
//     // reader.program.forEachResolvedModule((fun a m f ->
//     //     match a with
//     //     | Some (filePath, None) -> failwith ""
//     //     | Some (filePath, Some projectId) ->
//     //         Log.trace projectId
//     //         Log.trace filePath
//     //         Log.trace f
//     //     | None -> ()
//     //     ()
//     //     ))
//     // sf.packageJsonScope
//     // |> Option.iter (Log.traceTo 4)
//     let a,b = SourceTag.Create(reader.program, sf)
//     // Log.traceTo 2 a
//     // Log.traceTo 2 b.Value
//     // b.Value.Source.packageJsonScope
//     // b.Value.Exports
//     // |> Option.iter (fun x ->
//     //     // x.contents.packageJsonContent.exports
//     //     x.ToString()
//     //     |> Log.traceTo 1
//     //     )
//     a.Value.PackageInfo
//     |> Log.traceTo 1
//     )
match packages with
| Ok pkgs -> Log.traceTo 4 pkgs
| Error e -> Log.error <| e.ToString()
// |> Log.traceTo 2
// reader.program.getSourceFiles().AsArray
// |> Array.iter (fun sf ->
//     match SourceTag.CreateValue(reader.program, sf).Value with
//     | Package value ->
//     | _ -> ()
//     )

// reader.program?resolvedModules
// |> Log.traceTo 1
// reader.program.getSourceFiles().AsArray
// |> Array.last
// |> fun sf ->
//     Log.traceTo 2 sf.packageJsonScope
//     reader.program.forEachResolvedModule((fun f m _ p ->
//         Log.traceTo 2 f
//         Log.traceTo 2 m
//         Log.traceTo 2 p
//         ), sf)
//     let symbol = reader.checker.getSymbolAtLocation(sf).Value
//     Log.traceTo 2 symbol
//     let messageType =
//         symbol.exports.Value.get(!!"MessageType")
//         |> _.declarations.Value.AsArray
//         |> Array.head
//         :?> Ts.ExportSpecifier
//     messageType.kind.Name
//     |> Log.success
//     match messageType.name with
//     | Patterns.Node.ModuleExportNamePatterns.Identifier name ->
//         reader.checker.getAliasedSymbol(reader.checker.getExportSymbolOfSymbol(reader.checker.getExportSpecifierLocalTargetSymbol(U2.Case1 messageType).Value)).declarations.Value.AsArray
//         |> Array.head
//         
//         |> Log.traceTo 1
//     | Patterns.Node.ModuleExportNamePatterns.StringLiteral name -> ()
//     // reader.program?sourceFileToPackageName?get(sf.fileName.ToLower())
//     // |> Log.traceTo 2

// |> Array.iter (fun sf ->
//     sf
//     |> Log.traceTo 1
//     )
// |> Log.traceTo 1
#endif

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
        let pathIsFile = path.extname output <> ""
        let dirPath =
            if pathIsFile
            then path.join(output, "..")
            else output
        if fs.existsSync(!^dirPath) |> not then
            fs.mkdirSync(dirPath)
        readFile input output
    | [ input ] ->
        readFile input null
    | _ ->
        printHelp()
    0
#endif