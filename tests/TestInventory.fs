module Xantham.Tests.TestInventory

open Expecto

/// Lists discovered cases as JSON so parameterized names retain embedded newlines and quotes.
let run argv =
    if argv = [| "--xantham-list-tests" |] then
        let tests =
            System.Reflection.Assembly.GetEntryAssembly()
            |> Expecto.Impl.testFromAssembly
            |> Option.defaultWith (fun () -> failwith "No Expecto tests discovered")
            |> Test.toTestCodeList
            |> List.map (fun test -> test.fullName ".")
            |> List.toArray

        System.Text.Json.JsonSerializer.Serialize tests |> printfn "%s"
        0
    else
        runTestsInAssemblyWithCLIArgs [] argv
