[<AutoOpen>]
module Xantham.Fable.Types.Reader

open System.Collections.Generic
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer
open Fable.Microsoft.Extensions.Logging

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


let private createProgramForFiles runDirectory (entryFiles: string array) =
    let entryFiles = entryFiles |> Array.map String.normalizePath
    let tempFilePath = Temp.Directory.createXanthamDummyFileWithRefs runDirectory entryFiles
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

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
[<ReferenceEquality>]
type TypeScriptReader = {
    Stack: Stack<XanthamTag>
    EntryFiles: string array
    Program: Ts.Program
    Checker: Ts.TypeChecker
    SignalCache: Dictionary<IdentityKey, TypeStore>
    ExportCache: Dictionary<IdentityKey, ExportStore>
    MemberCache: Dictionary<IdentityKey, MemberStore>
    LibCache: HashSet<IdentityKey>
    TempRunDirectory: string
    TempFilePath: string
    Log: Utils.Logging.Log
} with
    member inline this.tempDirectory = this.TempRunDirectory
    member inline this.tempFilePath = this.TempFilePath
    member inline this.stack = this.Stack
    member inline this.entryFile = this.EntryFiles |> Array.head
    member inline this.entryFiles = this.EntryFiles
    member inline this.program = this.Program
    member inline this.checker = this.Checker
    member inline this.signalCache = this.SignalCache
    member inline this.memberCache = this.MemberCache
    member inline this.libCache = this.LibCache
    member inline this.exportCache = this.ExportCache
    member inline this.logger = this.Log

module TypeScriptReader =
    type Environment = {
        stack: Stack<XanthamTag>
        signalCache: Dictionary<IdentityKey, TypeStore>
        memberCache: Dictionary<IdentityKey, MemberStore>
        libCache: HashSet<IdentityKey>
        runDirectory: string
        exportCache: Dictionary<IdentityKey, ExportStore>
    }
    let private setupEnvironment() =
        { stack = Stack<XanthamTag>()
          signalCache = Dictionary<IdentityKey, TypeStore>()
          memberCache = Dictionary<IdentityKey, MemberStore>()
          libCache = HashSet<IdentityKey>()
          runDirectory = Temp.Directory.createXanthamRunDirectory()
          exportCache = Dictionary<IdentityKey, ExportStore>() }
        
    let inline private createWithLoggersFor (environment: Environment) (loggers: ILogger seq) (entryFiles: string array) =
        let entryFiles = entryFiles |> Array.map String.normalizePath
        let tempFile, program =
            let programResult = createProgramForFiles environment.runDirectory entryFiles
            programResult.TempFilePath, programResult.Program
        let checker = program.getTypeChecker()
        let log =
            if Seq.length loggers = 1 then
                loggers |> Seq.head
            elif Seq.isEmpty loggers then
                Utils.Logging.ConsoleLogger(true, LogLevel.Debug)
            else
                Utils.Logging.CombinedLogger(loggers)
            |> Utils.Logging.Log
        #if DEBUG && !FABLE_TEST
        do entryFiles |> log.logfd "Starting XanthamFableRuntime for files: %A{entryFiles}"
        do program.getSourceFiles().AsArray
            |> Array.map (_.fileName >> Utils.Logging.relativizePath (Node.Api.``process``.cwd()))
            |> log.logft "Source files in run: %A{sourceFiles}"
        #endif
        {
            Program = program
            Checker = checker
            Stack = environment.stack
            EntryFiles = entryFiles
            SignalCache = environment.signalCache
            LibCache = environment.libCache
            MemberCache = environment.memberCache
            ExportCache = environment.exportCache
            TempFilePath = tempFile
            Log = log
            TempRunDirectory = environment.runDirectory
        }
    
    let createWithLoggerFor (entryFiles: string array) =
        let environment = setupEnvironment()
        let logPath = Node.Api.path.join (environment.runDirectory, "..")
        let loggers = [
            Utils.Logging.ConsoleLogger(true, LogLevel.Debug) :> ILogger
            match Temp.Directory.createXanthamLogWriter true LogLevel.Trace logPath with
            | Ok writer -> writer
            | Error e -> Log.error e
        ]
        createWithLoggersFor environment loggers entryFiles
        
    let createWithLogger (entryFile: string) =
        createWithLoggerFor [| entryFile |]
    
    let createFor (entryFiles: string array) =
        createWithLoggersFor (setupEnvironment()) [| Utils.Logging.ConsoleLogger(true, LogLevel.Debug) |] entryFiles
        
    let inline create (entryFile: string) =
        createFor [| entryFile |]
    