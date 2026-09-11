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
        testCase "raising and lowering preserves integer values and string identity" <| fun _ ->
            let identifier = 42 * Measure.uom<Measure.typeId>
            Expect.equal (identifier / Measure.uom<Measure.typeId>) 42 "compiler ID survives the boundary"
            let original = String([| 'n'; 'o'; 'd'; 'e' |])
            let name = original * Measure.uom<Measure.symbolName>
            Expect.isTrue (Object.ReferenceEquals(original, name / Measure.uom<Measure.symbolName>)) "raising allocates no replacement string"

        testCase "lowering and raising can compose string measures" <| fun _ ->
            let dependency = "@scope/package" * Measure.uom<Measure.npmDependency>
            let importPath = dependency / Measure.uom<Measure.npmDependency> * Measure.uom<Measure.importSpecifier>
            let roundTrip = importPath / Measure.uom<Measure.importSpecifier> * Measure.uom<Measure.npmDependency>
            Expect.equal (roundTrip / Measure.uom<Measure.npmDependency>) "@scope/package" "composed measures preserve the payload"

        testCase "lowering one component preserves the remaining measure" <| fun _ ->
            let declaration = "/pkg/index.d.ts" * Measure.uom<Measure.declFile>
            let path = declaration / Measure.uom<Measure.node>
            let restored = path * Measure.uom<Measure.node>
            Expect.equal (restored / Measure.uom<Measure.declFile>) "/pkg/index.d.ts" "component cancellation restores the original value"

        testCase "consumer compiler separates ID and string domains" <| fun _ ->
            let positive = """
let takesType (id: int<Measure.typeId>) = id
let takesName (name: string<Measure.symbolName>) = name
let id = takesType (42 * Measure.uom<Measure.typeId>)
let name = takesName ("node" * Measure.uom<Measure.symbolName>)
if id / Measure.uom<Measure.typeId> <> 42 || name / Measure.uom<Measure.symbolName> <> "node" then failwith "representation changed"
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
                "string", "npmDependency", "importSpecifier"
                "string", "dirPath", "filePath"
            ] do
                let source = $"let incompatible (value: {kind}<Measure.{sourceMeasure}>) : {kind}<Measure.{targetMeasure}> = value"
                let code, output = compile source
                Expect.notEqual code 0 $"{sourceMeasure} must not satisfy {targetMeasure}"
                Expect.stringContains output "FS0001" ("must fail for a type mismatch: " + output)
                Expect.stringContains output sourceMeasure "diagnostic identifies the supplied domain"
                Expect.stringContains output targetMeasure "diagnostic identifies the required domain"
    ]
