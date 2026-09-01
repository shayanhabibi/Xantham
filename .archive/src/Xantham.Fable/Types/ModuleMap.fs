// =======================================================================
// Implements a module mapping helper which applies multiple strategies
// to determining a module name from a file path with decreasing fidelity.
// =======================================================================
[<AutoOpen>]
module Xantham.Fable.AutoOpenModuleMap

open System.Collections.Generic
open Fable.Core.JS
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open TypeScript
open Node
open Fable.Core
open System // This import allows compiler to correctly infer `Array.choose` et al

[<Erase>]
type ModuleName = ModuleName of string
type FilePath = string

/// <summary>
/// A module map which can be used to resolve module names from file paths.
/// </summary>
/// <remarks>
/// Construction must be done with the <c>ModuleMap.Create</c> static method.
/// The map is then used as an indexer to resolve module names from file paths.
/// </remarks>
/// <example>
/// <code lang="fsharp">
/// let moduleMap = ModuleMap.Create(program)
/// // index via Ts.Node
/// moduleMap[node]
/// // index via Ts.SourceFile
/// moduleMap[sourceFile]
/// // index via string
/// moduleMap["filepath.d.ts"]
/// </code>
/// </example>
type ModuleMap = private {
    /// <summary>
    /// A dictionary of module names and their resolved file names.
    /// </summary>
    Map: Dictionary<FilePath, ModuleName>
    /// <summary>
    /// A function which can be used to resolve module names from file paths
    /// in the case they are not found in the module map.
    /// </summary>
    Fallback: FilePath -> ModuleName
}

[<AutoOpen>]
module private InternalHelpers =
    /// In Fable/JavaScript, <c>ResizeArray&lt;'T&gt;</c> and <c>'T array</c> are both
    /// plain JS arrays; this cast is a zero-cost identity at runtime.
    /// Do not use in .NET contexts — <c>ResizeArray&lt;'T&gt;</c> is a <c>List&lt;T&gt;</c>
    /// there and the cast would be unsound.
    let inline toArray (resizeArray: ResizeArray<'T>)  = unbox<'T array> resizeArray
    let inline getImports (file: Ts.SourceFile) =
        file.Item("imports")
        |> Option.ofObj
        |> Option.defaultValue [||]
        |> unbox : Ts.StringLiteral array

/// <summary>
/// Creates a dictionary of known module names and their resolved file names by
/// traversing imports for source files in the program.
/// </summary>
/// <param name="program"></param>
let private makeModuleMapDict (program: Ts.Program) =
    let options = program.getCompilerOptions()
    let host = ts.createCompilerHost(options)
    program.getSourceFiles()
    |> toArray
    |> Array.collect (fun file ->
        file
        |> getImports
        |> Array.choose (fun imp ->
            let moduleName = imp.text
            ts.resolveModuleName(
                moduleName,
                file.fileName,
                options,
                host
                )
            |> _.resolvedModule
            |> Option.map (_.resolvedFileName >> fun fileName ->
                KeyValuePair(fileName, ModuleName moduleName)
                )
            )
        |> Array.distinctBy _.Key
        )
    |> Dictionary
/// <summary>
/// Tries to find the module name of a file path by traversing the directory
/// tree upwards until a package.json file is found, and then reads the name from that
/// package.json file.<br/><br/>
/// Failing the above would default to the path after the last node_modules folder.
/// </summary>
let private tryGetFileModuleName (* maxDepth: int *) (file: string) =
    // let maxDepth =
    //     if maxDepth < 0 then None
    //     else Some maxDepth
    // let mutable depth = 0
    let mutable dir = path.dirname file
    let mutable result: string option = None
    while
        result.IsNone
        && dir <> path.dirname dir do
        // && maxDepth |> Option.map ((>) depth) |> Option.defaultValue true do
        let packageJsonPath = path.join(dir, "package.json")
        result <-
            if fs.existsSync(!^packageJsonPath) then
                try
                    fs.readFileSync(packageJsonPath, "utf8")
                    |> JSON.parse
                    |> unbox<{| name: string option |}>
                    |> _.name
                with _ -> None
            else None
        // depth <- depth + 1
        dir <- path.dirname dir
    match result with
    | Some path -> path
    | None ->
        path.dirname file
        |> _.Trim('"').Split("node_modules")
        |> Array.last
        |> _.Trim('/').Trim('\\')
    |> ModuleName

type ModuleMap with
    static member Create(program: Ts.Program) =
        {
            Map = makeModuleMapDict program
            Fallback = tryGetFileModuleName
        }
    member this.Item(key: string) =
        match this.Map.TryGetValue(key) with
        | true, value -> value
        | _ -> this.Fallback key
    member this.Item(sourceFile: Ts.SourceFile) =
        this.Item(sourceFile.fileName)
    member this.Item(value: TypeDeclaration) =
        this.Item(value.Value.getSourceFile())

                

