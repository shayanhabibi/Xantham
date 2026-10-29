module Xantham.Generator.Tests.DependentConstraintTests

open System.IO
open System.Text.Json
open Expecto
open Xantham.Generator

[<Tests>]
let tests =
    testCase "dependent generic constraints preserve supplied arguments" <| fun _ ->
        use scratch = Scratch.directory "dependent-generic-constraints"
        let original = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "zod-dependent-constraints-lab")
        let fixture = Path.Combine(scratch.Path, "fixture")
        Directory.CreateDirectory fixture |> ignore
        File.Copy(Path.Combine(original, "package.json"), Path.Combine(fixture, "package.json"))
        let declarations = File.ReadAllText(Path.Combine(original, "index.d.ts"))
        File.WriteAllText(Path.Combine(fixture, "index.d.ts"), declarations.Replace("use<T extends Schema<string, number>>(value: T): T;", "use<T extends Schema<string, number>>(value: T): T; cyclic<T extends Box<T>>(value: T): T; keyed<T extends keyof Box<string>>(value: T): T; keyedPromise<T extends keyof Box<string>>(value: T): Promise<T>; keyedRead<T extends keyof Box<string>>(value: T, read: (value: Box<string>[T]) => void): T;"))
        let output = Path.Combine(scratch.Path, "generated")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.DependentConstraints" }
        Pipeline.run config fixture output |> Async.RunSynchronously |> ignore
        let source = File.ReadAllText(Path.Combine(output, "Mini.DependentConstraints.fs"))
        Expect.isTrue (System.Text.RegularExpressions.Regex.IsMatch(source, @"cyclic<'T[^\n]*:\s*value: 'T\s*->\s*'T")) "recursive constraint retains the same generic argument and result"
        Expect.isTrue (System.Text.RegularExpressions.Regex.IsMatch(source, @"keyed<'T[^\n]*:\s*value: 'T\s*->\s*'T")) ("keyof constraint retains the same generic argument and result: " + source)
        Expect.isTrue (System.Text.RegularExpressions.Regex.IsMatch(source, @"keyedPromise<'T[^\n]*:\s*value: 'T\s*->\s*JS.Promise<'T>")) "wrapped result keeps the same generic argument"
        let readSignature = source.Split('\n') |> Array.find (fun line -> line.Contains "abstract keyedRead")
        Expect.isFalse (readSignature.Contains "obj") "indexed access keeps its typed key mapping"
        let cyclicFindings =
            File.ReadLines(Path.Combine(output, "symbols.jsonl"))
            |> Seq.choose (fun line ->
                use document = JsonDocument.Parse line
                let symbol = document.RootElement
                if symbol.GetProperty("findings").EnumerateArray() |> Seq.exists (fun finding -> finding.GetProperty("key").GetString() = "TR074") then
                    Some(symbol.GetProperty("name").GetString())
                else None)
            |> Set.ofSeq
        Expect.contains cyclicFindings "AnySelf" "a corrected self-bound application is explicitly widened"
        Expect.isFalse (Set.contains "SelfConsumer" cyclicFindings) "valid nominal self recursion is retained"
        Expect.isFalse (Set.contains "CycleConsumer" cyclicFindings) "valid nominal mutual recursion is retained"
        let generated = "generated/Mini.DependentConstraints.fs"
        let code, log = DeclarationCatalogTests.compileConsumer scratch.Path [ generated ] """module Mini.Check
open Mini.DependentConstraints
let defaulted (value: ArrayConsumer<Schema<obj array, obj array, Internals<obj array, obj array>>>) = value.schema
let explicit (value: ExplicitConsumer<Schema<string, float, Internals<string, float>>>) = value.schema
let generic<'O, 'I, 'T when 'T :> Internals<'O, 'I>> (value: GenericConsumer<'O, 'I, 'T>) = value :> Schema<'O, 'I, 'T>
let method (value: Methods) (schema: Schema<string, float, Internals<string, float>>) : Schema<string, float, Internals<string, float>> = value.``use`` schema
let cyclic (value: Methods) (recursive: Recursive) : Recursive = value.cyclic recursive
let keepT<'T when 'T :> Box<'T>> (value: Methods) (input: 'T) : 'T = value.cyclic input
let nested (value: NestedConsumer) : Schema<float, string, Internals<float, string>> = value.value.value
let forward (value: ForwardConsumer) : Wide = value.value.a.output
let recursive (value: SelfConsumer) : Recursive = value.value.value
let cycle (value: CycleConsumer) : Right = value.value.a.output
let refused (value: obj) : AnySelf = value
"""
        Expect.equal code 0 log
        let code, _ = DeclarationCatalogTests.compileConsumer scratch.Path [ generated ] """module Mini.Invalid
open Mini.DependentConstraints
let invalid (value: ExplicitConsumer<Schema<float, string, Internals<float, string>>>) = value.schema
"""
        Expect.notEqual code 0 "a different dependent nominal contract remains invalid"
