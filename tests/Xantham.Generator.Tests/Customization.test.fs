module Xantham.Generator.Tests.CustomizationTests

open System.IO
open Expecto
open Xantham.Generator

let private package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/customization-lab"))

[<Tests>]
let tests =
    testList "customization" [
        testCase "ordinary interface contract remains abstract" <| fun _ ->
            let rendered = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
            let source = rendered.Files |> List.find (fst >> fun name -> name.EndsWith ".fs") |> snd
            Expect.stringContains source "abstract value: 'T with get, set" "generic property contract"
            Expect.stringContains source "abstract stamp: string\n" "readonly property contract"
            Expect.stringContains source "inherit Properties<string>" "generic substitution"
    ]
