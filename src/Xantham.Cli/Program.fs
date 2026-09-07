/// `xantham` - the command line over the generator. Every command is a shell over a library
/// function the test harness calls as well: `generate` is `Pipeline.run`, `schema` is
/// `Schema.json`.
module Xantham.Cli.Program

open System
open System.Diagnostics
open System.IO
open System.Text
open Xantham.Generator
open FSharp.SystemCommandLine
open Fake.JavaScript

/// What the process exits with.
[<RequireQualifiedAccess>]
module Exit =
    /// The binding and its manifest were written.
    [<Literal>]
    let Generated = 0

    /// The command line is not one this program accepts.
    [<Literal>]
    let Usage = 1

    /// The path holds no package the generator can read.
    [<Literal>]
    let NoPackage = 2

    /// `xantham.json` was refused.
    [<Literal>]
    let Configuration = 3

    /// Generation itself failed.
    [<Literal>]
    let Failed = 4

module Options =
    let out =
        Input.option<string> "--out"
        |> Input.alias "-o"
        |> Input.description "where the binding, the shipped groups, manifest.json and symbols.jsonl are written."
        |> Input.defaultValue "xantham-out"
        |> Input.arity Arity.ExactlyOne
        |> Input.acceptLegalFilePathsOnly
        |> Input.helpName "dir"

    let config =
        Input.optionMaybe<string> "--config"
        |> Input.description
            "the xantham.json configuring the run, or the directory holding one (default: the package directory)."
        |> Input.arity Arity.ExactlyOne
        |> Input.acceptLegalFilePathsOnly
        |> Input.helpName "path"

    let quiet =
        Input.option<bool> "--quiet"
        |> Input.description "write the file list alone, dropping the findings summary."

    let schemaOut =
        Input.optionMaybe<string> "--out"
        |> Input.alias "-o"
        |> Input.helpName "path"
        |> Input.acceptLegalFilePathsOnly

    let packageDir =
        Input.argument<string> "package-dir"
        |> Input.description "a directory holding package.json and the node_modules its declarations resolve through."
        |> Input.arity ExactlyOne

/// The configuration for a run: `xantham.json` under the package directory, or under
/// `--config` when that names a directory. A `--config` naming the file itself reads it under
/// whatever name it carries.
let private loadConfig =
    input {
        let! config = Options.config
        and! packageDir = Options.packageDir

        return
            match config with
            | None -> GeneratorConfig.load packageDir
            | Some path when Directory.Exists path -> GeneratorConfig.load path
            | Some path when not (File.Exists path) -> failwith $"no configuration at {path}"
            | Some path -> GeneratorConfig.loadFile path
    }

type private GenerateOptions =
    {
        PackageDir: string
        Out: string
        Config: GeneratorConfig
        Quiet: bool
    }

    static member Default =
        input {
            let! packageDir = Options.packageDir
            and! out = Options.out
            and! config = loadConfig
            and! quiet = Options.quiet

            return
                {
                    PackageDir = packageDir
                    Out = out
                    Config = config
                    Quiet = quiet
                }
        }

/// The findings a run raised, in the manifest's own vocabulary: the four tiers, then the count
/// of each finding key, commonest first.
let private summary (report: RunReport) =
    let counts = report.Counts

    let keys =
        report.Findings
        |> List.countBy _.Key
        |> List.sortBy (fun (key, count) -> -count, key)

    [
        $"  exact {counts.Exact}  ergonomic {counts.Ergonomic}  widened {counts.Widened}  escape {counts.Escape}"
        for key, count in keys do
            $"  {key} {count}"
    ]

/// Below this count, a run's `ShadowedByLib` reads as ordinary declaration merging (a polyfill
/// augmenting one or two global interfaces); at or above it, as a global type library whose own
/// declarations are being lost to the compiler's default lib wholesale.
let private shadowedByLibThreshold = 20

/// A run's own diagnostic for a package that looks like a global type library with no `lib`
/// configured: the compiler's default DOM lib loaded, and this many of the package's own
/// declarations share a name with a DOM declaration and merged into it instead of reaching the
/// binding. Silent whenever `lib` is already configured, since that is the fix this names.
let private libShadowWarning (config: GeneratorConfig) (report: RunReport) : string option =
    if config.Lib.IsSome || report.ShadowedByLib < shadowedByLibThreshold then
        None
    else
        Some
            $"xantham: {report.ShadowedByLib} declarations in this package share a name with a \
              default-lib declaration (e.g. the DOM lib) and merged into it instead of reaching \
              {report.ModuleName} - set \"lib\" in xantham.json to the set this package's own \
              documentation prescribes"

/// Why a package directory is refused before a compiler session starts.
let private refusePackage (packageDir: string) =
    if not (Directory.Exists packageDir) then
        Some $"no directory at {packageDir}"
    elif not (File.Exists(Path.Combine(packageDir, "package.json"))) then
        Some $"{packageDir} holds no package.json"
    else
        None

let private emit (out: TextWriter) (err: TextWriter) (options: GenerateOptions) config packageDir =
    let outDir = Path.GetFullPath options.Out

    try
        let report = Async.RunSynchronously(Pipeline.run config packageDir outDir)

        for name in report.OutputFiles do
            out.WriteLine(Path.Combine(outDir, name.Replace('/', Path.DirectorySeparatorChar)))

        if not options.Quiet then
            err.WriteLine $"{Bootstrap.packageName packageDir} -> {report.ModuleName}"

            for line in summary report do
                err.WriteLine line

            match libShadowWarning config report with
            | Some warning -> err.WriteLine warning
            | None -> ()

        Exit.Generated
    with e ->
        err.WriteLine $"xantham: generating {packageDir} failed - {e.Message}"
        Exit.Failed

let private generate (out: TextWriter) (err: TextWriter) (options: GenerateOptions) =
    let packageDir = Path.GetFullPath options.PackageDir

    match refusePackage packageDir with
    | Some message ->
        err.WriteLine $"xantham: {message}"
        Exit.NoPackage
    | None ->
        match
            (try
                Ok options.Config
             with e ->
                 Error e.Message)
        with
        | Error message ->
            err.WriteLine $"xantham: {message}"
            Exit.Configuration
        | Ok config ->
            match
                (try
                    Ok(Bootstrap.resolveEntryFile config packageDir)
                 with e ->
                     Error e.Message)
            with
            | Ok _ -> emit out err options config packageDir
            | Error message ->
                err.WriteLine $"xantham: {message}"

                if config.Entry.IsSome then
                    Exit.Configuration
                else
                    Exit.NoPackage

let private schema (out: TextWriter) (err: TextWriter) (destination: string option) =
    let text = Schema.json ()

    match destination with
    | None ->
        out.Write text
        Exit.Generated
    | Some path ->
        try
            let full = Path.GetFullPath path
            Directory.CreateDirectory(Path.GetDirectoryName full) |> ignore
            File.WriteAllText(full, text, UTF8Encoding false)
            out.WriteLine full
            Exit.Generated
        with e ->
            err.WriteLine $"xantham: writing {path} failed - {e.Message}"
            Exit.Failed

let private version =
    Reflection.Assembly.GetExecutingAssembly()
    |> _.GetCustomAttributes(typeof<Reflection.AssemblyInformationalVersionAttribute>, false)
    |> Array.tryHead
    |> Option.map (fun found -> (found :?> Reflection.AssemblyInformationalVersionAttribute).InformationalVersion)
    |> Option.defaultValue "0.0.0"

let private cache =
    Path.Combine(
        Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
        ".cache",
        "xantham",
        Spec.tscVersion
        |> String.filter (function
            | c when Char.IsAsciiLetterOrDigit c -> true
            | '-' | '_' | '.' -> true
            | _ -> false
            )
        )

let private ensureInstall() =
    if not (Directory.Exists cache) then
        Directory.CreateDirectory(cache) |> ignore
    let packageJson = Path.Combine(cache, "package.json")
    if not (File.Exists(packageJson)) then
        File.WriteAllText(packageJson, Spec.tscVersion |> sprintf (*language=json*) """{
    "name": "xantham",
    "private": true,
    "type": "module",
    "description": "xantham tool dependencies pinned to the xantham tool version",
    "devDependencies": {
        "typescript": "%s"
    }
}""" )
    if not (Directory.Exists(Path.Combine(cache, "node_modules"))) then
        Npm.install (fun p -> { p with WorkingDirectory = cache })
        match Xantham.TypeScript.Wire.Tsc.locate cache with
        | Some tsc -> Ok tsc
        | None -> Error $"tsc not found at {cache}"
    else
        match Xantham.TypeScript.Wire.Tsc.locate cache with
        | Some tsc -> Ok tsc
        | None -> Error $"tsc not found at {cache}"
    |> Result.map (fun tsc ->
        Environment.SetEnvironmentVariable("XANTHAM_TSGO_EXE", tsc)
        )

/// Checks the cache for a compiler executable, and sets the environment variable if found.
/// Else no-op.
let private checkCache() =
    Xantham.TypeScript.Wire.Tsc.locate cache
    |> Option.iter (fun tsc -> Environment.SetEnvironmentVariable("XANTHAM_TSGO_EXE", tsc))
    

/// One invocation, over the writers the caller supplies. The entry point calls it against the
/// console; the acceptance test calls it against a string writer.
let run (out: TextWriter) (err: TextWriter) (argv: string[]) : int =
    let cmd =
        ManualInvocation.rootCommand {
            description "TypeScript declarations to F# Fable bindings"
            Input.context

            addCommands
                [
                    command "tsc" {
                        description "manage the typescript compiler cache"
                        inputs Input.context
                        helpAction
                        addCommands [
                            command "init" {
                                description "install the xantham typescript compiler dependencies"
                                setAction (fun _ ->
                                    match ensureInstall() with
                                    | Error message -> err.WriteLine message; Exit.Failed
                                    | Ok _ -> Exit.Generated
                                    )
                            }
                            command "version" {
                                description "show the xantham typescript compiler version"
                                setAction (fun _ ->
                                    match Xantham.TypeScript.Wire.Tsc.locate cache with
                                    | Some tsc ->
                                        $"{Spec.tscVersion} cached at: {tsc}"
                                        |> out.WriteLine
                                    | None ->
                                        $"{Spec.tscVersion} not found in cache. Run `xantham tsc init`."
                                        |> err.WriteLine
                                    Exit.Generated
                                    )
                            }
                            command "clean" {
                                description "remove all cached xantham compilers"
                                setAction (fun _ ->
                                    let path =
                                        Path.Combine(
                                            Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
                                            ".cache",
                                            "xantham"
                                            )
                                    if Directory.Exists(path) then
                                        Directory.Delete(path, true)
                                        out.WriteLine "xantham cache removed"
                                    else
                                        out.WriteLine "no xantham cache to remove"
                                    Exit.Generated
                                    )
                            }
                        ]
                    }
                    command "generate" {
                        description "generate a binding and its manifest"
                        inputs GenerateOptions.Default
                        setAction (fun opts ->
                            checkCache()
                            generate out err opts)
                    }
                    command "schema" {
                        description "write the JSON Schema for xantham.json"
                        hidden
                        Options.schemaOut
                        setAction (schema out err)
                    }
                ]

            helpAction
        }

    cmd.Parse(argv).Invoke()

[<EntryPoint>]
let main argv =
    try
        Console.OutputEncoding <- UTF8Encoding false
    with _ ->
        ()

    run Console.Out Console.Error argv
