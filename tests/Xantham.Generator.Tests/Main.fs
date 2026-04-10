module Main

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [
        CLIArguments.Sequenced
    ] argv