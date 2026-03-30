[<AutoOpen>]
module Xantham.Fable.Types.Reader

open System.Collections.Generic
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Xantham
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer

let private commonCompilerOptions = jsOptions<Ts.CompilerOptions>(fun c ->
    c.moduleResolution <- Some Ts.ModuleResolutionKind.Bundler
    c.target <- Some Ts.ScriptTarget.Latest
    c.skipLibCheck <- Some true
    c.declaration <- Some true
    c.emitDeclarationOnly <- Some true
    // Without this, unions with null and undefined are reduced out when resolving
    // a type node to a type. This increases workaround logic.
    c.strictNullChecks <- Some true
    )

let private createProgram (entryFile: string): Ts.Program =
    let entryFile = String.normalizePath entryFile
    ts.createProgram(jsOptions<Ts.CreateProgramOptions> (fun o ->
        o.rootNames <- ResizeArray [| entryFile |]
        o.options <- commonCompilerOptions
        ))
    
let private createProgramForFiles (entryFiles: string array): Ts.Program =
    let entryFiles = entryFiles |> Array.map String.normalizePath
    ts.createProgram(jsOptions<Ts.CreateProgramOptions>(fun o ->
        o.rootNames <- ResizeArray entryFiles
        o.options <- commonCompilerOptions
        ))

// Original impl Credit @MangelMaxine
let isFromEs5Lib (symbolOpt: Ts.Symbol option) =
    match symbolOpt with
    | None -> false
    | Some symbol ->
        match symbol.declarations with
        | Some declarations when declarations.Count > 0 && !!declarations[0].parent ->
            match declarations[0].parent with
            | node when ts.isSourceFile node ->
                (node :?> Ts.SourceFile).fileName.EndsWith("lib/lib.es5.d.ts")
            | _ -> false
        | _ ->
            // For some reason, I can't seem to resolve the actual symbol for some Es5 types
            // So, we make a naive fallback checking the name of the symbol
            [ "Iterable"; "IterableIterator" ] |> List.contains symbol.name

[<ReferenceEquality>]
type TypeScriptReader = {
    Stack: Stack<XanthamTag>
    EntryFiles: string array
    Program: Ts.Program
    Checker: Ts.TypeChecker
    Warnings: ResizeArray<string>
    ModuleMap: ModuleMap
    SignalCache: Dictionary<IdentityKey, TypeStore>
    LibCache: HashSet<IdentityKey>
} with
    member inline this.stack = this.Stack
    member inline this.entryFile = this.EntryFiles |> Array.head
    member inline this.entryFiles = this.EntryFiles
    member inline this.program = this.Program
    member inline this.checker = this.Checker
    member inline this.warnings = this.Warnings
    member inline this.moduleMap = this.ModuleMap
    member inline this.signalCache = this.SignalCache
    member inline this.libCache = this.LibCache

module TypeScriptReader =
    let createFor (entryFiles: string array) =
        let entryFiles = entryFiles |> Array.map String.normalizePath
        let stack = Stack<XanthamTag>()
        let warnings = ResizeArray<string>()
        let signalCache = Dictionary()
        let libCache = HashSet()
        let program = createProgramForFiles entryFiles
        let checker = program.getTypeChecker()
        #if DEBUG && !FABLE_TEST
        do Log.debug $"Starting XanthamFableRuntime for files: %A{entryFiles}"
        #endif
        {
            Program = program
            Checker = checker
            Stack = stack
            EntryFiles = entryFiles
            Warnings = warnings
            ModuleMap = ModuleMap.Create program
            SignalCache = signalCache
            LibCache = libCache
        }
    let inline create (entryFile: string) =
        createFor [| entryFile |]
    