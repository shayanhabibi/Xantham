module Xantham.Generator.Tests.MeasuresTests

open System
open System.Diagnostics
open System.IO
open Expecto
open Xantham.Generator

/// Compile against the built consumer surface, rather than testing only helper inference.
let private compile source =
    let directory = Path.Combine(Path.GetTempPath(), "xantham-measures-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory directory |> ignore
    try
        let script = Path.Combine(directory, "check.fsx")
        let assembly = typeof<GeneratorConfig>.Assembly.Location.Replace("\"", "\"\"")
        File.WriteAllText(script, $"#r @\"{assembly}\"\nopen Xantham.Generator\n{source}\n")
        let start = ProcessStartInfo("dotnet")
        start.UseShellExecute <- false
        start.CreateNoWindow <- true
        start.RedirectStandardOutput <- true
        start.RedirectStandardError <- true
        for argument in [ "fsi"; "--exec"; "--nologo"; script ] do
            start.ArgumentList.Add argument
        use child = Process.Start start
        let output = child.StandardOutput.ReadToEndAsync()
        let errors = child.StandardError.ReadToEndAsync()
        if not (child.WaitForExit 30000) then
            child.Kill(true)
            child.WaitForExit()
            failtest "measure compilation exceeded 30 seconds"
        child.ExitCode, output.GetAwaiter().GetResult() + errors.GetAwaiter().GetResult()
    finally
        Directory.Delete(directory, true)

[<Tests>]
let tests =
    testList "generator identifier measures" [
        testCase "tagging preserves integer values and string identity" <| fun _ ->
            let identifier = Measure.Int.tag<Measure.typeId> 42
            Expect.equal (Measure.Int.untag identifier) 42 "compiler ID survives the boundary"
            let original = String([| 'n'; 'o'; 'd'; 'e' |])
            let name = Measure.String.tag<Measure.symbolName> original
            Expect.isTrue (Object.ReferenceEquals(original, Measure.String.untag name)) "tagging allocates no replacement string"

        testCase "consumer compiler separates ID and string domains" <| fun _ ->
            let positive = """
let takesType (id: int<Measure.typeId>) = id
let takesName (name: string<Measure.symbolName>) = name
let id = takesType (Measure.Int.tag<Measure.typeId> 42)
let name = takesName (Measure.String.tag<Measure.symbolName> "node")
if Measure.Int.untag id <> 42 || Measure.String.untag name <> "node" then failwith "representation changed"
"""
            let code, output = compile positive
            Expect.equal code 0 ("correctly tagged consumer must compile and run: " + output)
            for kind, sourceMeasure, targetMeasure in [
                "int", "symbolId", "typeId"
                "int", "typeId", "nodeId"
                "int", "nodeId", "symbolId"
                "string", "symbolName", "declFile"
                "string", "declFile", "declHandle"
                "string", "declHandle", "symbolName"
            ] do
                let source = $"let incompatible (value: {kind}<Measure.{sourceMeasure}>) : {kind}<Measure.{targetMeasure}> = value"
                let code, output = compile source
                Expect.notEqual code 0 $"{sourceMeasure} must not satisfy {targetMeasure}"
                Expect.stringContains output "FS0001" ("must fail for a type mismatch: " + output)
                Expect.stringContains output sourceMeasure "diagnostic identifies the supplied domain"
                Expect.stringContains output targetMeasure "diagnostic identifies the required domain"
    ]
