module Xantham.Generator.Tests.ResolvedProjectionTests

open System
open System.IO
open Expecto
open Xantham.Generator
open Xantham.Generator.Customization
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire.Proto

let private package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/projection-lab"))
let private config = { GeneratorConfig.Default with Lib = Some ["es5"]; Types = Some [] }

let private resolve path =
    async {
        let! mailbox, ctx = Bootstrap.start config path
        let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
        let! model, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
        return mailbox :> IDisposable, ctx, model
    } |> Async.RunSynchronously

let private snapshot ctx model = Semantics.projectResolved ctx model |> Async.RunSynchronously
let private select owner name model = Resolved.tryFind owner [name] model |> Option.defaultWith (fun () -> failtestf "Missing %s/%s" owner name)
let private fixtureSource name model = select "projection-lab" name model

let private hasCode code result =
    match result with
    | Error errors -> errors |> List.exists (fun diagnostic -> Diagnostic.code diagnostic = code)
    | Ok _ -> false

let private create source model = ProjectionCompanion.create source "Choice.fs" ["Projection.Choice"] "module Projection" model
let private fingerprint plan = let _, hash, _, _, _ = ContractData.projectionCompanionInfo plan in hash

let private choiceArms =
    [ ResolvedUnionArm.StringLiteral "auto"; ResolvedUnionArm.StringLiteral "manual"
      ResolvedUnionArm.Number; ResolvedUnionArm.Null; ResolvedUnionArm.Undefined ]

[<Tests>]
let tests =
    testList "projection resolved" [
        testCase "facts precede shaping and preserve both absence values" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let source = fixtureSource "Choice" model
            Expect.equal (Resolved.union source model) (Ok choiceArms) "original literals and both absence arms"
            Expect.equal (Resolved.package source model, Resolved.path source model) ("projection-lab", ["Choice"]) "source selection is package qualified"

        testCase "equal and overlapping named aliases retain independent declaration identity" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let a, b, c = fixtureSource "EqualA" model, fixtureSource "EqualB" model, fixtureSource "OverlapC" model
            Expect.equal (Resolved.union a model) (Resolved.union b model) "equal runtime sets"
            Expect.notEqual (Resolved.identity a model) (Resolved.identity b model) "equal sets do not merge source declarations"
            Expect.notEqual (Resolved.union a model) (Resolved.union c model) "overlap does not merge value sets"
            let next = snapshot ctx resolve
            let nextA = fixtureSource "EqualA" next
            Expect.equal (Resolved.identity a model) (Resolved.identity nextA next) "identity does not contain a session nonce"
            Expect.equal (fingerprint (create a model)) (fingerprint (create nextA next)) "fact fingerprint is deterministic"

        testCase "arbitrary string literal values reach the extension unchanged" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let arms = Resolved.union (fixtureSource "Weird" model) model |> Result.defaultWith (fun _ -> failtest "supported weird union")
            for value in [""; "Number"; "Null"; "Undefined"; "auto-mode"; "auto_mode"; "quote\"and\\slash"] do
                Expect.contains arms (ResolvedUnionArm.StringLiteral value) "case naming is not a semantic transformation"

        testCase "unsupported selected unions diagnose without hiding valid neighbors" <| fun _ ->
            use scratch = Scratch.directory "projection-unsupported"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-controls\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export type Good = 'auto' | 'manual'; export type Alias = Good; export type Boolean = 'auto' | boolean; export type Generic<T> = 'auto' | T; export type NumericLiteral = 'auto' | 7; export type BroadString = 'auto' | string; export type ObjectArm = 'auto' | { x: string }; export type VoidArm = 'auto' | void;")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolve
            let source name = select "projection-controls" name model
            let good, alias = source "Good", source "Alias"
            Expect.isOk (Resolved.union good model) "unsupported unrelated exports do not block selection"
            Expect.equal (Resolved.union good model) (Resolved.union alias model) "alias has the same resolved values"
            Expect.notEqual (Resolved.identity good model) (Resolved.identity alias model) "named alias remains its own declaration"
            for name in ["Boolean"; "Generic"; "NumericLiteral"; "BroadString"; "ObjectArm"; "VoidArm"] do
                let selected = source name
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/unsupported-union") name
                Expect.throws (fun () -> create selected model |> ignore) "unsupported input cannot become an authenticated plan"
            Expect.isNonEmpty (Resolved.diagnostics model) "unsupported declarations remain visible as diagnostics"

        testCase "phantom generic declarations reject without poisoning a shared literal type" <| fun _ ->
            use scratch = Scratch.directory "projection-phantom-generic"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-phantom\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export type Phantom<T> = 'auto'; export type PhantomUnion<T> = 'auto' | 'manual'; export type Plain = 'auto';")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolve
            let source name = select "projection-phantom" name model
            let declared name =
                let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = name)
                resolve.ExportTypes[exported.Symbol.SymbolId].Declared |> Option.get
            Expect.equal (declared "Phantom") (declared "Plain") "control shares the normalized checker type"
            Expect.equal (Resolved.union (source "Plain") model) (Ok [ResolvedUnionArm.StringLiteral "auto"]) "nongeneric declaration remains supported"
            for name in ["Phantom"; "PhantomUnion"] do
                let selected = source name
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/unsupported-union") "genericity belongs to the declaration"
                Expect.throws (fun () -> create selected model |> ignore) "phantom parameters cannot escape the bounded contract"

        testCase "missing and deliberately unresolved arms reject the entire selected union" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = "Choice")
            let id = resolve.ExportTypes[exported.Symbol.SymbolId].Declared |> Option.get
            let arm = resolve.Types[id].UnionMembers.Head
            let controls =
                [ { resolve with Types = Map.remove arm resolve.Types }
                  { resolve with NotFollowed = Map.add arm "controlled unresolved constituent" resolve.NotFollowed }
                  { resolve with Types = Map.add id { resolve.Types[id] with UnionMembers = [] } resolve.Types }
                  { resolve with Types = Map.add id { resolve.Types[id] with UnionMembers = [id] } resolve.Types } ]
            for control in controls do
                let model = snapshot ctx control
                let selected = fixtureSource "Choice" model
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/incomplete-union") "no incomplete arm set is accepted"
                Expect.throws (fun () -> create selected model |> ignore) "no partial companion plan"

        testCase "missing source metadata remains selectable and diagnoses" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = "Choice")
            let exported = { exported with Symbol = { exported.Symbol with Declarations = ValueNone } }
            let model = snapshot ctx { resolve with Harvest = { resolve.Harvest with Exports = [exported] } }
            let selected = fixtureSource "Choice" model
            Expect.isTrue (Resolved.union selected model |> hasCode "projection/missing-source-metadata") "source is not silently omitted"
            Expect.throws (fun () -> create selected model |> ignore) "missing identity cannot authenticate a plan"

        testCase "source tokens and cached plans are sealed to the current snapshot" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let first, second = snapshot ctx resolve, snapshot ctx resolve
            let selected = fixtureSource "Choice" first
            let plan = create selected first
            Expect.isTrue (ContractData.projectionCompanionIsCurrent first plan) "own plan is valid"
            Expect.isFalse (ContractData.projectionCompanionIsCurrent second plan) "cached plan is rejected even when values match"
            Expect.isTrue (Resolved.union selected second |> hasCode "projection/stale-source") "foreign token returns a diagnostic"
            Expect.throws (fun () -> create selected second |> ignore) "factory rejects a foreign token"

        testCase "source authentication changes even when union values do not" <| fun _ ->
            use scratch = Scratch.directory "projection-source-hash"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-hash\",\"types\":\"index.d.ts\"}")
            let file = Path.Combine(scratch.Path, "index.d.ts")
            File.WriteAllText(file, "export type Choice = 'auto' | 'manual';")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let first = snapshot ctx resolve
            let firstSource = select "projection-hash" "Choice" first
            File.AppendAllText(file, "\n// changed source content\n")
            let second = snapshot ctx resolve
            let secondSource = select "projection-hash" "Choice" second
            Expect.equal (Resolved.union firstSource first) (Resolved.union secondSource second) "control retains the same facts"
            Expect.notEqual (fingerprint (create firstSource first)) (fingerprint (create secondSource second)) "content is part of authentication"

        testCase "unannotated existing extension records still infer GeneratorExtension" <| fun _ ->
            let existing =
                { Identity = { Id = "source-compatible"; Version = "1"; Configuration = Map.empty }
                  Transform = fun _ -> Ok Edits.empty }
            let requiresExisting (_: GeneratorExtension) = ()
            requiresExisting existing
    ]
