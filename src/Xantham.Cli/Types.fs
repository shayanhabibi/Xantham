module Xantham.Cli.Types

open System

[<Struct>]
type OutputMode =
    | Pretty
    | Json

[<Struct>]
type ColorMode =
    | Auto
    | Always
    | Never

[<Struct>]
type BannerMode =
    | Auto
    | Always
    | Never

module ColorMode =
    let shouldUseColor (outputMode: OutputMode) mode =
        match mode with
        | ColorMode.Always -> true
        | ColorMode.Never -> false
        | ColorMode.Auto when outputMode.IsJson -> false
        | ColorMode.Auto ->
            (not Console.IsOutputRedirected
             && String.IsNullOrEmpty(Environment.GetEnvironmentVariable("NO_COLOR")))
            || not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable("FORCE_COLOR")))

module BannerMode =
    let shouldShowBanner (outputMode: OutputMode) mode =
        match mode with
        | BannerMode.Always -> true
        | BannerMode.Never -> false
        | BannerMode.Auto when outputMode.IsJson -> false
        | BannerMode.Auto -> not Console.IsOutputRedirected
