module Xantham.Generator.Tests.PhantomCaseNamesTests

open System.IO
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "phantom private cases remain legal for exact lowercase alias names" <| fun _ ->
        use scratch = Scratch.directory "phantom-case-names"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "phantom-case-name-lab")
        Pipeline.run { GeneratorConfig.Default with ModuleName = Some "Mini.Phantom" } fixture (Path.Combine(scratch.Path, "generated"))
        |> Async.RunSynchronously |> ignore
        let code, log = DeclarationCatalogTests.compileConsumer scratch.Path [ "generated/Mini.Phantom.fs" ] """module Mini.Check
let exact (value: Mini.Phantom.lower<string>) : Mini.Phantom.lower<string> = value
let upper (value: Mini.Phantom.Lower<string>) : Mini.Phantom.Lower<string> = value
let keyword (value: Mini.Phantom.``match``<string>) : Mini.Phantom.``match``<string> = value
let prefix (value: Mini.Phantom.Phantom_lower<string>) : Mini.Phantom.Phantom_lower<string> = value
let underscore (value: Mini.Phantom._lower<string>) : Mini.Phantom._lower<string> = value
"""
        Expect.equal code 0 log
        for invalid in [
            "let wrong (value: Mini.Phantom.lower<string>) : Mini.Phantom.lower<float> = value"
            "let wrong (value: Mini.Phantom.lower<string>) : Mini.Phantom.Lower<string> = value"
            "let wrong : Mini.Phantom.lower<string> = Mini.Phantom.lower.Phantom_lower__ null"
        ] do
            let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ "generated/Mini.Phantom.fs" ] ("module Mini.Invalid\n" + invalid)
            Expect.notEqual code 0 "phantom binders, nominal distinctions and private representation remain intact"
