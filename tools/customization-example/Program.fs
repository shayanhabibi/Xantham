module Xantham.CustomizationExample.Program

open System.IO
open System.Text.Json
open Xantham.Generator

[<EntryPoint>]
let main arguments =
    if arguments.Length < 2 then
        eprintfn "Usage: customization-example <input-directory> <output-directory> [--direct] [--lab]"
        2
    else
        let input = Path.GetFullPath arguments[0]

        use package =
            JsonDocument.Parse(File.ReadAllText(Path.Combine(input, "package.json")))

        let owner = package.RootElement.GetProperty("name").GetString()

        let extension =
            PartasDom.extension owner (Array.contains "--direct" arguments) (Array.contains "--lab" arguments)

        let report =
            Pipeline.runWith [ extension ] GeneratorConfig.Default input arguments[1]
            |> Async.RunSynchronously

        printfn "Generated %d files for %s" report.OutputFiles.Length report.ModuleName
        0
