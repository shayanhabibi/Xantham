module Xantham.EndToEnd

open Expecto

[<EntryPoint>]
let main argv =
    argv
    |> Array.filter (function
        | "nobuild" ->
            Tests.buildDriver <- false
            false
        | "buildxantham" ->
            Tests.buildXantham <- true
            false
        | "runxantham" ->
            Tests.runXantham <- true
            false
        | _ -> true
        )
    |> Tests.runTestsInAssemblyWithCLIArgs [
        CLIArguments.Colours 256
        CLIArguments.Log_Name "Xantham.EndToEnd"
        CLIArguments.Summary
    ]