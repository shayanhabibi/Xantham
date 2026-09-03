/// `xantham` - the command line over the generator. Every command is a shell over a library
/// function the test harness calls as well: `generate` is `Pipeline.run`, `schema` is
/// `Schema.json`.
module Xantham.Cli.Program

open System
open System.IO
open System.Text
open Xantham.Generator

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

let private usage =
    [
        "xantham - TypeScript declarations to F# Fable bindings"
        ""
        "usage:"
        "  xantham generate <package-dir> [-o <dir>] [--config <path>] [--quiet]"
        "  xantham schema [-o <path>]"
        "  xantham --help | --version"
        ""
        "generate"
        "  <package-dir>    a directory holding package.json and the node_modules its"
        "                   declarations resolve through"
        "  -o, --out <dir>  where the binding, the shipped groups, manifest.json and"
        "                   symbols.jsonl are written (default: ./xantham-out)"
        "  --config <path>  the xantham.json configuring the run, or the directory holding"
        "                   one (default: the package directory)"
        "  --quiet          write the file list alone, dropping the findings summary"
        ""
        "schema"
        "  -o, --out <path> where the JSON Schema for xantham.json is written"
        "                   (default: standard output)"
        ""
        "exit codes"
        "  0 generated  1 usage  2 no package  3 configuration refused  4 generation failed"
        ""
    ]
    |> String.concat "\n"

type private GenerateOptions =
    {
        PackageDir: string
        Out: string
        Config: string option
        Quiet: bool
    }

    static member Default =
        {
            PackageDir = ""
            Out = "xantham-out"
            Config = None
            Quiet = false
        }

let private parseGenerate (args: string list) =
    let rec go options seen args =
        match args with
        | [] when seen -> Ok options
        | [] -> Error "generate needs a package directory"
        | ("-o" | "--out") :: value :: rest -> go { options with Out = value } seen rest
        | "--config" :: value :: rest -> go { options with Config = Some value } seen rest
        | "--quiet" :: rest -> go { options with Quiet = true } seen rest
        | [ ("-o" | "--out" | "--config") as flag ] -> Error $"{flag} needs a value"
        | value :: _ when value.StartsWith "-" -> Error $"unknown option {value}"
        | value :: rest when not seen -> go { options with PackageDir = value } true rest
        | value :: _ -> Error $"generate takes one package directory; {value} is a second"

    go GenerateOptions.Default false args

let private parseSchema (args: string list) =
    match args with
    | [] -> Ok None
    | [ ("-o" | "--out") ] -> Error "-o needs a value"
    | [ ("-o" | "--out"); value ] -> Ok(Some value)
    | value :: _ -> Error $"unknown argument {value}"

/// The configuration for a run: `xantham.json` under the package directory, or under
/// `--config` when that names a directory. A `--config` naming the file itself reads it under
/// whatever name it carries.
let private loadConfig (options: GenerateOptions) =
    match options.Config with
    | None -> GeneratorConfig.load options.PackageDir
    | Some path when Directory.Exists path -> GeneratorConfig.load path
    | Some path when not (File.Exists path) -> failwith $"no configuration at {path}"
    | Some path -> GeneratorConfig.loadFile path

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

/// Why a package directory is refused before a compiler session starts.
let private refusePackage (packageDir: string) =
    if not (Directory.Exists packageDir) then
        Some $"no directory at {packageDir}"
    elif not (File.Exists(Path.Combine(packageDir, "package.json"))) then
        Some $"{packageDir} holds no package.json"
    else
        let entry = Bootstrap.entryFile packageDir

        if File.Exists entry then
            None
        else
            Some $"{packageDir} declares no TypeScript entry - looked for {entry}"

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
                Ok(loadConfig options)
             with e ->
                 Error e.Message)
        with
        | Error message ->
            err.WriteLine $"xantham: {message}"
            Exit.Configuration
        | Ok config -> emit out err options config packageDir

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
    |> fun assembly -> assembly.GetCustomAttributes(typeof<Reflection.AssemblyInformationalVersionAttribute>, false)
    |> Array.tryHead
    |> Option.map (fun found -> (found :?> Reflection.AssemblyInformationalVersionAttribute).InformationalVersion)
    |> Option.defaultValue "0.0.0"

/// One invocation, over the writers the caller supplies. The entry point calls it against the
/// console; the acceptance test calls it against a string writer.
let run (out: TextWriter) (err: TextWriter) (argv: string[]) : int =
    let refuse (message: string) =
        err.WriteLine $"xantham: {message}"
        err.Write usage
        Exit.Usage

    match List.ofArray argv with
    | [] ->
        err.Write usage
        Exit.Usage
    | [ "--help" ]
    | [ "-h" ]
    | [ "help" ] ->
        out.Write usage
        Exit.Generated
    | [ "--version" ] ->
        out.WriteLine version
        Exit.Generated
    | "generate" :: rest ->
        match parseGenerate rest with
        | Error message -> refuse message
        | Ok options -> generate out err options
    | "schema" :: rest ->
        match parseSchema rest with
        | Error message -> refuse message
        | Ok destination -> schema out err destination
    | command :: _ -> refuse $"unknown command {command}"

[<EntryPoint>]
let main argv =
    try
        Console.OutputEncoding <- UTF8Encoding false
    with _ ->
        ()

    run Console.Out Console.Error argv
