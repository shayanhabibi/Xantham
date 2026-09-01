module Main

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [
        CLIArguments.Colours 256
        CLIArguments.Log_Name "Xantham.Generator.Tests"
    ] argv