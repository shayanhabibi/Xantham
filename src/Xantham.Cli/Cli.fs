namespace Xantham.Cli

open System.IO
open FSharp.SystemCommandLine
open Xantham.Cli.Types
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

    let private outputMode =
        input {
            let! mode =
                Input.option<bool> "--json"
                |> Input.desc "Whether to output JSON."
                |> Input.def false

            return if mode then OutputMode.Json else OutputMode.Pretty
        }

    let private bannerMode =
        input {
            let! mode =
                Input.option<string> "--banner"
                |> Input.desc "Whether to show the CLI banner."
                |> Input.def "auto"
                |> Input.acceptOnlyFromAmong [ "never"; "auto"; "always" ]

            return
                match mode with
                | "never" -> BannerMode.Never
                | "always" -> BannerMode.Always
                | _ -> BannerMode.Auto
        }

    let private colorMode =
        input {
            let! mode =
                Input.option<string> "--color"
                |> Input.desc "Whether to use colors in the CLI output."
                |> Input.def "auto"
                |> Input.acceptOnlyFromAmong [ "never"; "always"; "auto" ]

            return
                match mode with
                | "never" -> ColorMode.Never
                | "always" -> ColorMode.Always
                | _ -> ColorMode.Auto
        }

    let shouldShowBanner =
        input {
            let! bannerMode = bannerMode
            and! outputMode = outputMode
            and! quiet = quiet
            return not quiet && BannerMode.shouldShowBanner outputMode bannerMode
        }

    let shouldUseColor =
        input {
            let! colorMode = colorMode
            and! outputMode = outputMode
            return ColorMode.shouldUseColor outputMode colorMode
        }

    let useJsonOutput =
        input {
            let! outputMode = outputMode
            return outputMode.IsJson
        }

module internal Xantham =
    /// The configuration for a run: `xantham.json` under the package directory, or under
    /// `--config` when that names a directory. A `--config` naming the file itself reads it under
    /// whatever name it carries.
    let loadConfig =
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

    type GenerateOptions =
        {
            PackageDir: string
            Out: string
            Config: GeneratorConfig
            Quiet: bool
            Json: bool
        }

        static member Default =
            input {
                let! packageDir = Options.packageDir
                and! out = Options.out
                and! config = loadConfig
                and! quiet = Options.quiet
                and! json = Options.useJsonOutput

                return
                    {
                        PackageDir = packageDir
                        Out = out
                        Config = config
                        Quiet = quiet
                        Json = json
                    }
            }

    let renderFigletFn =
        input {
            let! shouldShowBanner = Options.shouldShowBanner
            and! shouldUseColor = Options.shouldUseColor

            return
                fun () ->
                    if shouldShowBanner then
                        Style.renderFiglet shouldUseColor
        }
