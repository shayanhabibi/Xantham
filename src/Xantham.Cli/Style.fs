module Xantham.Cli.Style

open System.Text
open Spectre.Console
open Xantham.Cli.Types

let private wordmark =
    [|
        "██╗  ██╗ █████╗ ███╗   ██╗████████╗██╗  ██╗ █████╗ ███╗   ███╗"
        "╚██╗██╔╝██╔══██╗████╗  ██║╚══██╔══╝██║  ██║██╔══██╗████╗ ████║"
        " ╚███╔╝ ███████║██╔██╗ ██║   ██║   ███████║███████║██╔████╔██║"
        " ██╔██╗ ██╔══██║██║╚██╗██║   ██║   ██╔══██║██╔══██║██║╚██╔╝██║"
        "██╔╝ ██╗██║  ██║██║ ╚████║   ██║   ██║  ██║██║  ██║██║ ╚═╝ ██║"
        "╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝"
    |]

[<Struct>]
type private Rgb = { R: int; G: int; B: int }

let private startColour = { R = 0x00; G = 0x78; B = 0xE0 } // #0078E0

let private endColour = { R = 0xA0; G = 0x7E; B = 0xE8 } // #A07EE8


let inline private lerp a b numerator denominator =
    a + ((b - a) * numerator + denominator / 2) / denominator

let private interpolate a b numerator denominator =
    {
        R = lerp a.R b.R numerator denominator
        G = lerp a.G b.G numerator denominator
        B = lerp a.B b.B numerator denominator
    }

let private colourAt column lastColumn =
    interpolate startColour endColour column lastColumn


let private markup =
    let width = wordmark |> Array.maxBy String.length |> String.length

    let lastColumn = width - 1

    // Calculated once and reused for every row.
    let tags =
        Array.init width (fun column ->
            let colour = colourAt column lastColumn

            $"[#{colour.R:X2}{colour.G:X2}{colour.B:X2}]")

    // ~20 chars of markup overhead per visible glyph.
    let builder = StringBuilder(wordmark.Length * width * 20)

    for row = 0 to wordmark.Length - 1 do
        let line = wordmark[row]

        for column = 0 to line.Length - 1 do
            let ch = line[column]

            if ch = ' ' then
                builder.Append(' ') |> ignore
            else
                builder.Append(tags[column]).Append(ch).Append("[/]") |> ignore

        if row <> wordmark.Length - 1 then
            builder.AppendLine() |> ignore

    Markup(builder.ToString())

let renderFiglet useColor =
    if useColor then
        AnsiConsole.Write(markup)
        AnsiConsole.WriteLine()
    else
        AnsiConsole.Write(wordmark |> String.concat "\n")
        AnsiConsole.WriteLine()
