/// The early projection boundary, its compiled companions, and its independence from raw catalogs.
module Xantham.Generator.Tests.ProjectionTests

open System
open System.IO
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator
open Xantham.Generator.Customization
open Xantham.Generator.Myriad

let private root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../.."))
let private package = Path.Combine(root, "tests/fixtures/projection-lab")
let private identity = { Id = "lab.literal-unions"; Version = "1"; Configuration = Map.empty }
let private compiler workspace =
    let configuration = DirectoryInfo(AppContext.BaseDirectory).Parent.Name
    let references = ["Xantham.Fable.Core"; "Xantham.Fable.Core.TS"] |> List.map (fun name ->
        Path.Combine(root, "src", name, "bin", configuration, "net8.0", name + ".dll"))
    Compiler.dotnet workspace references

let private selections =
    ["Choice"; "EqualA"; "EqualB"; "OverlapC"; "Weird"] |> List.map (fun name ->
        { Package = "projection-lab"; Path = [name]; ModuleName = "ProjectionViews." + name; TypeName = "Value" })

let private extension workspace = LiteralUnions.create identity workspace selections
let private file name (model: RenderModel) = model.Files |> List.find (fst >> (=) name) |> snd
let private companionFiles (model: RenderModel) = model.Files |> List.filter (fst >> fun name -> name.StartsWith "ProjectionViews.")
let private run compiler projections config =
    Pipeline.generateProjectedWith compiler projections [] config package |> Async.RunSynchronously

let private rejects code action =
    let failure = try action (); None with error -> Some (error.ToString())
    match failure with
    | Some text -> Expect.stringContains text code "fails for the intended rule"
    | None -> failtestf "expected %s" code

let private custom make : ProjectionExtension =
    { Identity = identity
      Transform = fun snapshot ->
          let source = Resolved.tryFind "projection-lab" ["Choice"] snapshot |> Option.get
          Ok (make source snapshot) }

[<Tests>]
let tests = testList "projection companions" [
    testCase "Myriad companions are deterministic, gated goldens and leave the raw ABI intact" <| fun _ ->
        use scratch = Scratch.directory "projection-golden"
        let compile = compiler scratch.Path
        let baseline = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
        let generated = run compile [extension scratch.Path] GeneratorConfig.Default
        let repeated = run compile [extension scratch.Path] GeneratorConfig.Default
        Expect.equal generated repeated "fresh snapshot nonce never escapes into output"
        Expect.equal (file "ProjectionLab.fs" generated) (file "ProjectionLab.fs" baseline) "raw bindings unchanged"
        let originalFindings = generated.Findings |> List.filter (fun finding -> finding.Key <> "CU006")
        Expect.equal originalFindings baseline.Findings "existing widening findings remain truthful"
        Expect.equal (generated.Findings |> List.filter (fun finding -> finding.Key = "CU006") |> List.length) 5 "each source projection recorded"
        let source = file "ProjectionViews.Choice.fs" generated
        for arm in ["Auto"; "Manual"; "Number of float"; "Null"; "Undefined"; "(|Decoded|Invalid|)"] do
            Expect.stringContains source arm "original union survives widening"
        let golden = Path.Combine(__SOURCE_DIRECTORY__, "golden/projection-lab-companions")
        let sources = companionFiles generated
        if Environment.GetEnvironmentVariable("XANTHAM_UPDATE_GOLDEN") = "1" then
            Directory.CreateDirectory golden |> ignore
            for name, text in sources do File.WriteAllText(Path.Combine(golden, name), text)
        Expect.equal (Directory.GetFiles golden |> Array.map Path.GetFileName |> Array.sort |> Array.toList)
            (sources |> List.map fst |> List.sort) "golden closure contains exactly the companions"
        for name, text in sources do
            Expect.equal text (File.ReadAllText(Path.Combine(golden, name)).Replace("\r\n", "\n")) ("generated " + name)
        let provenance = JsonNode.Parse(file "manifest.json" generated)["projections"]
        let artifacts = provenance["companions"].AsArray()
        Expect.equal artifacts.Count 5 "provenance covers every artifact"
        let identities = artifacts |> Seq.map (fun n -> n["sourceIdentity"].GetValue<string>()) |> Seq.toList
        Expect.equal (List.distinct identities).Length 5 "same-set aliases retain named source identities"
        for artifact in artifacts do
            let name = artifact["file"].GetValue<string>()
            let expected = System.Security.Cryptography.SHA256.HashData(Text.Encoding.UTF8.GetBytes(file name generated)) |> Convert.ToHexStringLower
            Expect.equal (artifact["artifact"].GetValue<string>()) expected "artifact hash binds output"
            Expect.equal (artifact["sourceFingerprint"].GetValue<string>().Length) 64 "source fingerprint included"

    testCase "empty registration is identical and raw catalog authentication is unchanged" <| fun _ ->
        use scratch = Scratch.directory "projection-catalog"
        let compile = compiler scratch.Path
        let baseline = Pipeline.generate GeneratorConfig.Default package |> Async.RunSynchronously
        Expect.equal (run compile [] GeneratorConfig.Default) baseline "empty registration is byte-identical"
        let config = { GeneratorConfig.Default with DeclarationCatalog = true }
        let ordinary = Pipeline.generate config package |> Async.RunSynchronously
        let projected = run compile [extension scratch.Path] config
        Expect.equal (file "declarations.json" projected) (file "declarations.json" ordinary) "raw catalog identity, API and policy unchanged"
        Expect.equal (file "ProjectionLab.fs" projected) (file "ProjectionLab.fs" ordinary) "catalog's raw owner unchanged"

    testCase "equal literal sets remain distinct F# types and conversion is explicit" <| fun _ ->
        use scratch = Scratch.directory "projection-nominal"
        let compile = compiler scratch.Path
        let projected = run compile [LiteralUnions.create identity scratch.Path (selections |> List.filter (fun s -> s.Path = ["EqualA"] || s.Path = ["EqualB"]))] GeneratorConfig.Default
        let files = companionFiles projected
        let valid = "module Consumer\nlet convert (x: ProjectionViews.EqualA.Value) = ProjectionViews.EqualA.encode x |> ProjectionViews.EqualB.decode\n"
        Compile.validate compile (files @ ["Consumer.fs", valid]) [] |> Async.RunSynchronously
        let invalid = "module Consumer\nlet wrong : ProjectionViews.EqualB.Value = ProjectionViews.EqualA.Value.Auto\n"
        rejects "FS0001" (fun () -> Compile.validate compile (files @ ["Consumer.fs", invalid]) [] |> Async.RunSynchronously)

    testCase "malformed source and false type claims fail before writing" <| fun _ ->
        use scratch = Scratch.directory "projection-invalid"
        let compile = compiler scratch.Path
        let output = Path.Combine(scratch.Path, "output")
        Directory.CreateDirectory output |> ignore
        File.WriteAllText(Path.Combine(output, "sentinel.txt"), "keep")
        for source, names in ["module Broken\ntype Value = Value of NoSuchProjectionType_Forbidden\n", ["Broken.Value"];
                              "module Broken\nlet value = 1\n", ["Broken.Value"]] do
            let projection = custom (fun selected snapshot -> [ProjectionCompanion.create selected "Broken.fs" names source snapshot])
            rejects "customization/compile-failed" (fun () ->
                Pipeline.runProjectedWith compile [projection] [] GeneratorConfig.Default package output |> Async.RunSynchronously |> ignore)
        Expect.equal (Directory.GetFiles output |> Array.map Path.GetFileName) [|"sentinel.txt"|] "destination untouched"
        Expect.equal (File.ReadAllText(Path.Combine(output, "sentinel.txt"))) "keep" "existing content preserved"

    testCase "paths and exported types cannot collide with any existing owner" <| fun _ ->
        use scratch = Scratch.directory "projection-collision"
        let compile = compiler scratch.Path
        let make filename names = custom (fun source snapshot ->
            [ProjectionCompanion.create source filename names "module NewView\ntype Value = A\n" snapshot])
        for filename, names, code in [
            "../Escape.fs", ["NewView.Value"], "projection/invalid-path"
            "C:\\Escape.fs", ["NewView.Value"], "projection/invalid-path"
            "./Escape.fs", ["NewView.Value"], "projection/invalid-path"
            "Contracts.fs", ["NewView.Value"], "projection/file-collision"
            "projectionlab.fs", ["NewView.Value"], "projection/file-collision"
            "NewView.fs", ["ProjectionLab.Choice"], "projection/name-collision"
            "NewView.fs", ["NewView.Value"; "NewView.Value"], "projection/name-collision"
            "NewView.fs", [], "projection/empty-companion"
        ] do rejects code (fun () -> run compile [make filename names] GeneratorConfig.Default |> ignore)
        let duplicate = custom (fun selected snapshot ->
            [ProjectionCompanion.create selected "Duplicate.fs" ["First.Value"] "module First\ntype Value = A\n" snapshot
             ProjectionCompanion.create selected "Duplicate.fs" ["Second.Value"] "module Second\ntype Value = A\n" snapshot])
        rejects "projection/file-collision" (fun () -> run compile [duplicate] GeneratorConfig.Default |> ignore)

    testCase "a cached companion cannot be replayed into a new generation" <| fun _ ->
        use scratch = Scratch.directory "projection-replay"
        let compile = compiler scratch.Path
        let mutable cached = None
        let projection = custom (fun source snapshot ->
            let plan = match cached with
                       | Some plan -> plan
                       | None -> ProjectionCompanion.create source "Replay.fs" ["Replay.Value"] "module Replay\ntype Value = Auto\n" snapshot
            cached <- Some plan
            [plan])
        run compile [projection] GeneratorConfig.Default |> ignore
        rejects "projection/stale-plan" (fun () -> run compile [projection] GeneratorConfig.Default |> ignore)

    testCase "identity and diagnostics fail closed across both extension phases" <| fun _ ->
        use scratch = Scratch.directory "projection-identities"
        let compile = compiler scratch.Path
        let projection = extension scratch.Path
        let late = { Identity = identity; Transform = fun _ -> Ok Edits.empty }
        rejects "customization/duplicate-extension" (fun () ->
            Pipeline.generateProjectedWith compile [projection] [late] GeneratorConfig.Default package |> Async.RunSynchronously |> ignore)
        rejects "customization/invalid-identity" (fun () ->
            run compile [{projection with Identity = {identity with Version = ""}}] GeneratorConfig.Default |> ignore)
        let missing = LiteralUnions.create identity scratch.Path [{selections.Head with Path = ["Missing"]}]
        rejects "projection/" (fun () -> run compile [missing] GeneratorConfig.Default |> ignore)

    testCase "source mutation regenerates a new case and never falls back to widened strings" <| fun _ ->
        use scratch = Scratch.directory "projection-mutation"
        let compile = compiler scratch.Path
        File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-lab\",\"types\":\"index.d.ts\"}")
        let projection = LiteralUnions.create identity scratch.Path [selections.Head]
        let generate (declaration: string) =
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), declaration)
            Pipeline.generateProjectedWith compile [projection] [] GeneratorConfig.Default scratch.Path |> Async.RunSynchronously
        let original = generate "export type Choice = 'auto' | 'manual' | number | null | undefined;"
        let mutated = generate "export type Choice = 'auto' | 'manual' | 'scheduled' | number | null | undefined;"
        Expect.stringContains (file "ProjectionViews.Choice.fs" mutated) "Scheduled" "new source arm reached Myriad"
        let fingerprint model =
            let node = JsonNode.Parse(file "manifest.json" model)
            node["projections"].["companions"].[0].["sourceFingerprint"].GetValue<string>()
        Expect.notEqual (fingerprint original) (fingerprint mutated) "source changes invalidate provenance"
        rejects "projection/unsupported" (fun () -> generate "export type Choice = 'auto' | boolean | null | undefined;" |> ignore)
]
