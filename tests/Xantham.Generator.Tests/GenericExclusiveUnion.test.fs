module Xantham.Generator.Tests.GenericExclusiveUnionTests

open System.IO
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "generic exclusive union folds retain binders and callable references" <| fun _ ->
        use scratch = Scratch.directory "generic-exclusive-union"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "workers-generic-exclusive-lab")
        let output = Path.Combine(scratch.Path, "generated")
        Pipeline.run { GeneratorConfig.Default with ModuleName = Some "Mini.Exclusive" } fixture output
        |> Async.RunSynchronously |> ignore
        let code, log = DeclarationCatalogTests.compileConsumer scratch.Path [ "generated/Mini.Exclusive.fs" ] """module Mini.Check
open Mini.Exclusive
let count (workflow: Workflow<string>) = workflow.createBatch(Options<string>.Create(2.0, ``params`` = "text"))
let instances (workflow: Workflow<string>) (items: Instance<string> array) = workflow.createBatch(Options<string>.Create items)
let read (value: Options<string>) : string option = value.``params``
let named (workflow: NamedWorkflow<string>) (arm: CountArm<string>) = workflow.count arm
let namedCount (arm: CountArm<string>) : float = arm.count
let extractedCount (arm: ExtractedCount<string>) : float = arm.count
let transformed (value: BooleanOptions) : bool option = value.``params``
"""
        Expect.equal code 0 log
        let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ "generated/Mini.Exclusive.fs" ] """module Mini.Invalid
open Mini.Exclusive
let wrong (workflow: Workflow<string>) (value: Options<float>) = workflow.createBatch value
"""
        Expect.notEqual code 0 "a folded application keeps its source argument rather than erasing it"
        let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ "generated/Mini.Exclusive.fs" ] """module Mini.InvalidArm
open Mini.Exclusive
let wrong (workflow: NamedWorkflow<string>) (value: NamedOptions<string>) = workflow.count value
"""
        Expect.notEqual code 0 "a folded union cannot replace an independently referenced narrower arm"
