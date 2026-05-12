[<AutoOpen>]
module Xantham.Fable.Types.Reader

open System.Collections.Generic
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer

let private commonCompilerOptions = jsOptions<Ts.CompilerOptions>(fun c ->
    // c.traceResolution <- Some true
    c.moduleResolution <- Some Ts.ModuleResolutionKind.Bundler
    c.target <- Some Ts.ScriptTarget.Latest
    c.skipLibCheck <- Some true
    c.declaration <- Some true
    c.emitDeclarationOnly <- Some true
    c.resolveJsonModule <- Some true
    // Without this, unions with null and undefined are reduced out when resolving
    // a type node to a type. This increases workaround logic.
    c.strictNullChecks <- Some true
    c.resolvePackageJsonExports <- Some true
    c.resolvePackageJsonImports <- Some true)

let private createProgramForFiles (entryFiles: string array) =
    let entryFiles = entryFiles |> Array.map String.normalizePath
    let xanthamTempDir = Node.Api.fs.mkdtempSync("xantham_")
    let tempFilePath = Node.Api.path.join(xanthamTempDir, "temp.d.ts")
    Node.Api.fs.writeFileSync(tempFilePath, String.concat "\n" <| [
        for entryFile in entryFiles do
            "import * as _ from '" + entryFile + "';"
    ])
    {|
        TempFilePath = tempFilePath
        Program =
            ts.createProgram(jsOptions<Ts.CreateProgramOptions>(fun o ->
                o.rootNames <- ResizeArray [
                    tempFilePath
                ]
                o.options <- commonCompilerOptions
                ))
    |}

[<ReferenceEquality>]
type TypeScriptReader = {
    Stack: Stack<XanthamTag>
    EntryFiles: string array
    Program: Ts.Program
    Checker: Ts.TypeChecker
    Warnings: ResizeArray<string>
    SignalCache: Dictionary<IdentityKey, TypeStore>
    ExportCache: Dictionary<IdentityKey, ExportStore>
    MemberCache: Dictionary<IdentityKey, MemberStore>
    LibCache: HashSet<IdentityKey>
    TempFilePath: string
} with
    member inline this.tempFilePath = this.TempFilePath
    member inline this.stack = this.Stack
    member inline this.entryFile = this.EntryFiles |> Array.head
    member inline this.entryFiles = this.EntryFiles
    member inline this.program = this.Program
    member inline this.checker = this.Checker
    member inline this.warnings = this.Warnings
    member inline this.signalCache = this.SignalCache
    member inline this.memberCache = this.MemberCache
    member inline this.libCache = this.LibCache
    member inline this.exportCache = this.ExportCache

module TypeScriptReader =
    let createFor (entryFiles: string array) =
        let entryFiles = entryFiles |> Array.map String.normalizePath
        let stack = Stack<XanthamTag>()
        let warnings = ResizeArray<string>()
        let signalCache = Dictionary()
        let memberCache = Dictionary()
        let libCache = HashSet()
        let tempFile, program =
            let programResult = createProgramForFiles entryFiles
            programResult.TempFilePath, programResult.Program
            
        let checker = program.getTypeChecker()
        let exportCache = Dictionary()
        #if DEBUG && !FABLE_TEST
        do Log.debug $"Starting XanthamFableRuntime for files: %A{entryFiles}"
        #endif
        {
            Program = program
            Checker = checker
            Stack = stack
            EntryFiles = entryFiles
            Warnings = warnings
            SignalCache = signalCache
            LibCache = libCache
            MemberCache = memberCache
            ExportCache = exportCache
            TempFilePath = tempFile
        }
    let inline create (entryFile: string) =
        createFor [| entryFile |]
    