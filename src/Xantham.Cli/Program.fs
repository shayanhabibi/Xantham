/// `xantham` - the command line over the generator. Every command is a shell over a library
/// function the test harness calls as well: `generate` is `Pipeline.run`, `schema` is
/// `Schema.json`.
module Xantham.Cli.Program

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open Xantham.Cli.Xantham
open Xantham.Generator
open FSharp.SystemCommandLine
open Fake.JavaScript

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

        if options.Json then
            JsonSerializer.Serialize(report, JsonSerializerOptions.Default) |> out.WriteLine
            Exit.Generated
        else
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

        if (Xantham.TypeScript.Wire.Tsc.locate packageDir).IsNone then
            err.WriteLine "xantham: run `xantham tsc init` to cache the pinned compiler"

        Exit.Failed

let private generate (out: TextWriter) (err: TextWriter) (options: GenerateOptions) =
    let packageDir = Path.GetFullPath options.PackageDir

    // An unrecognised option binds as the package-dir argument.
    let refusal =
        if options.PackageDir.StartsWith "-" then
            Some(Exit.Usage, $"unrecognised option {options.PackageDir}")
        else
            refusePackage packageDir |> Option.map (fun message -> Exit.NoPackage, message)

    match refusal with
    | Some(code, message) ->
        err.WriteLine $"xantham: {message}"
        code
    | None ->
        match
            (try
                Ok(options.Config())
             with e ->
                 Error e.Message)
        with
        | Error message ->
            err.WriteLine $"xantham: {message}"
            Exit.Configuration
        | Ok config ->
            match
                (try
                    Ok(Bootstrap.publicPaths config packageDir)
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
            | '-'
            | '_'
            | '.' -> true
            | _ -> false)
    )

let private ensureInstall () =
    if not (Directory.Exists cache) then
        Directory.CreateDirectory(cache) |> ignore

    let packageJson = Path.Combine(cache, "package.json")

    if not (File.Exists(packageJson)) then
        File.WriteAllText(
            packageJson,
            Spec.tscVersion
            |> sprintf (*language=json*)
                """{
    "name": "xantham",
    "private": true,
    "type": "module",
    "description": "xantham tool dependencies pinned to the xantham tool version",
    "devDependencies": {
        "typescript": "%s"
    }
}"""
        )

    let installed =
        if Directory.Exists(Path.Combine(cache, "node_modules")) then
            Ok()
        else
            try
                Npm.install (fun p -> { p with WorkingDirectory = cache })
                Ok()
            with e ->
                Error $"npm install in {cache} failed - {e.Message}"

    installed
    |> Result.bind (fun () ->
        match Xantham.TypeScript.Wire.Tsc.locate cache with
        | Some tsc -> Ok tsc
        | None -> Error $"tsc not found at {cache}")
    |> Result.map (fun tsc -> Environment.SetEnvironmentVariable("XANTHAM_TSGO_EXE", tsc))

/// Points `XANTHAM_TSGO_EXE` at the cached compiler, when one is cached and the variable does
/// not already name an existing file. The compiler precedence is `XANTHAM_TSGO_EXE`, then the
/// cache, then the walk up from the package directory.
let private checkCache () =
    match Environment.GetEnvironmentVariable "XANTHAM_TSGO_EXE" with
    | path when not (String.IsNullOrWhiteSpace path) && File.Exists path -> ()
    | _ ->
        Xantham.TypeScript.Wire.Tsc.locate cache
        |> Option.iter (fun tsc -> Environment.SetEnvironmentVariable("XANTHAM_TSGO_EXE", tsc))

/// The `tsc version --json` payload for the cached compiler at `path`, or for none cached.
let tscVersionJson (path: string option) =
    match path with
    | Some tsc ->
        JsonSerializer.Serialize
            {|
                version = Spec.tscVersion
                path = tsc
            |}
    | None ->
        JsonSerializer.Serialize
            {|
                version = Spec.tscVersion
                path = (null: string)
                error = "not found. run `xantham tsc init`"
            |}


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

                        addCommands
                            [
                                command "init" {
                                    description "install the xantham typescript compiler dependencies"

                                    setAction (fun _ ->
                                        match ensureInstall () with
                                        | Error message ->
                                            err.WriteLine message
                                            Exit.Failed
                                        | Ok _ -> Exit.Generated)
                                }
                                command "version" {
                                    description "show the xantham typescript compiler version"
                                    inputs Options.useJsonOutput

                                    setAction (fun useJsonOutput ->
                                        let located = Xantham.TypeScript.Wire.Tsc.locate cache

                                        match located with
                                        | Some tsc ->
                                            if useJsonOutput then
                                                tscVersionJson located
                                            else
                                                $"{Spec.tscVersion} cached at: {tsc}"
                                            |> out.WriteLine

                                            Exit.Generated
                                        | None ->
                                            if useJsonOutput then
                                                tscVersionJson located
                                            else
                                                $"{Spec.tscVersion} not found in cache. Run `xantham tsc init`."
                                            |> err.WriteLine

                                            Exit.Failed)
                                }
                                command "clean" {
                                    description "remove all cached xantham compilers"
                                    inputs Options.useJsonOutput

                                    setAction (fun useJsonOutput ->
                                        let path =
                                            Path.Combine(
                                                Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
                                                ".cache",
                                                "xantham"
                                            )

                                        if Directory.Exists(path) then
                                            Directory.Delete(path, true)

                                            if
                                                useJsonOutput
                                            //language=json
                                            then
                                                """{"msg":"cache removed"}"""
                                            else
                                                "xantham cache removed"
                                            |> out.WriteLine
                                        else
                                            if
                                                useJsonOutput
                                            //language=json
                                            then
                                                """{"msg":"nothing to remove"}"""
                                            else
                                                "no xantham cache to remove"
                                            |> out.WriteLine

                                        Exit.Generated)
                                }
                            ]
                    }
                    command "generate" {
                        description "generate a binding and its manifest"
                        inputs (renderFigletFn, GenerateOptions.Default)

                        setAction (fun (renderFn, opts) ->
                            renderFn ()
                            checkCache ()
                            generate out err opts)
                    }
                    command "schema" {
                        description "write the JSON Schema for xantham.json"
                        inputs (renderFigletFn, Options.schemaOut)

                        setAction (fun (fn, op) ->
                            fn ()
                            schema out err op)
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
