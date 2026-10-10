module Xantham.CustomizationExample.Program

open System.IO
open System.Text.Json
open Xantham.Generator
open Xantham.Generator.Customization
open Xantham.Generator.Myriad

let private usage () =
    eprintfn "Usage: customization-example <input-directory> <output-directory> [--direct] [--lab]"

    eprintfn
        "       customization-example <input-directory> <output-directory> --union <export.path> --workspace <directory> [--reference <assembly.dll>]..."

    eprintfn "Union mode emits Projected.Choice.Value and validates all output against the supplied references."

let private projectionOptions arguments =
    let rec read union workspace references =
        function
        | [] ->
            match union, workspace with
            | Some union, Some workspace -> union, workspace, List.rev references
            | _ -> invalidArg "arguments" "Union mode requires --union and --workspace"
        | "--union" :: value :: rest when union.IsNone -> read (Some value) workspace references rest
        | "--workspace" :: value :: rest when workspace.IsNone ->
            read union (Some(Path.GetFullPath value)) references rest
        | "--reference" :: value :: rest -> read union workspace (Path.GetFullPath value :: references) rest
        | value :: _ -> invalidArg "arguments" $"Unknown, repeated or incomplete union option: {value}"

    read None None [] arguments

let private runProjection owner input output arguments =
    let export, workspace, references = projectionOptions arguments
    let path = export.Split('.') |> Array.toList

    if path |> List.exists System.String.IsNullOrWhiteSpace then
        invalidArg "arguments" "The selected export must contain nonempty path segments"

    let selection =
        {
            Package = owner
            Path = path
            ModuleName = "Projected.Choice"
            TypeName = "Value"
        }

    let extension =
        LiteralUnions.create
            {
                Id = "example.literal-union"
                Version = "1"
                Configuration = Map.empty
            }
            (Path.Combine(workspace, "myriad"))
            [ selection ]

    let compiler = Compiler.dotnet (Path.Combine(workspace, "validation")) references
    Pipeline.runProjectedWith compiler [ extension ] [] GeneratorConfig.Default input output

[<EntryPoint>]
let main arguments =
    if arguments.Length < 2 then
        usage ()
        2
    else
        let input = Path.GetFullPath arguments[0]

        use package =
            JsonDocument.Parse(File.ReadAllText(Path.Combine(input, "package.json")))

        let owner = package.RootElement.GetProperty("name").GetString()

        let report =
            (if Array.contains "--union" arguments then
                 runProjection owner input arguments[1] (arguments |> Array.skip 2 |> Array.toList)
             else
                 let extension =
                     PartasDom.extension owner (Array.contains "--direct" arguments) (Array.contains "--lab" arguments)

                 Pipeline.runWith [ extension ] GeneratorConfig.Default input arguments[1])
            |> Async.RunSynchronously

        printfn "Generated %d files for %s" report.OutputFiles.Length report.ModuleName
        0
