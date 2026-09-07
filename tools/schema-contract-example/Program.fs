module SchemaContractExample.Program

open System
open System.IO
open NJsonSchema
open SchemaContractExample

[<EntryPoint>]
let main args =
    try
        let output =
            match args with
            | [||] -> None
            | [| "--out"; path |] -> Some path
            | _ -> invalidArg "args" "Usage: schema-contract-example [--out <schema.json>]"

        let schema = SchemaContract.generate typeof<Model.ConfigInput>
        // Validate the serialized artifact too: references must survive writing
        // and reading, not merely work as in-memory object pointers.
        let json = schema.ToJson()
        let reloaded = JsonSchema.FromJsonAsync(json).GetAwaiter().GetResult()
        Checks.run reloaded

        match output with
        | Some path ->
            File.WriteAllText(path, json + Environment.NewLine)
            printfn "Wrote %s" (Path.GetFullPath path)
        | None -> ()

        0
    with error ->
        eprintfn "%s" (error.ToString())
        1
