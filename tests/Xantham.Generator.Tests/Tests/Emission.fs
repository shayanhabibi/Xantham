module Xantham.Generator.Tests.Tests.Emission

// ─────────────────────────────────────────────────────────────────────────────
// Coverage plane for src/Xantham.Generator/Generator/Emission.fs — the
// partitioned-emission plan and split. UNIT: name derivation, plan ordering/
// references, split totality + pooling. INTEGRATION: the committed recipe.
// (renderUnitFile/emitUnits are exercised end-to-end by scripts/partition-gate.sh,
// which is the executable spec for the file-producing half.)
// ─────────────────────────────────────────────────────────────────────────────

open System.Collections.Generic
open System.IO
open Expecto
open Xantham
open Xantham.Generator.Generator
open Xantham.Generator.Generator.Emission

let private entry package lib dependsOn : RecipeEntry = {
    Package = package
    Kind = Module
    Root = None
    Entries = [ "." ]
    Crawl = true
    Lib = Some lib
    Policy = None
    DependsOn = dependsOn
}

let private emptyModule name : Module = {
    Name = name
    Types = Dictionary()
    Members = Dictionary()
    Modules = Dictionary()
}

let private rootWith (moduleNames: string list) : RootModule =
    let modules = Dictionary()
    for name in moduleNames do modules.Add(name, emptyModule name)
    { Types = Dictionary(); Members = Dictionary(); Modules = modules }

[<Tests>]
let unitTests =
    testList "Emission (unit)" [
        testCase "packageTopModule mirrors the path plane" <| fun _ ->
            Expect.equal (packageTopModule "agents") "Agents" "bare"
            Expect.equal (packageTopModule "partyserver") "Partyserver" "single word stays one hump"
            Expect.equal (packageTopModule "@cloudflare/workers-types") "CloudflareWorkersTypes" "scoped merges"
            Expect.equal (packageTopModule "@modelcontextprotocol/sdk") "ModelcontextprotocolSdk" "scoped merges (2)"

        testCase "planUnits keeps [[entry]] order and accumulates references" <| fun _ ->
            let recipe = {
                Entries = [ entry "a" "Lib.A" []; { entry "b" "Lib.B" [] with Crawl = false }; entry "c" "Lib.C" [] ]
                Dependencies = []
            }
            let plan = planUnits recipe
            Expect.equal (plan |> List.map _.Lib) [ "Lib.A"; "Lib.B"; "Lib.C" ] "order; crawl=false entries ARE units"
            Expect.equal plan[2].References [ "Lib.A"; "Lib.B" ] "each unit references all earlier"
            Expect.equal plan[0].References [] "first references nothing"

        testCase "splitRoot is total: matched modules to their units, rest pooled to FIRST" <| fun _ ->
            let plan = planUnits { Entries = [ entry "zod" "Lib.Zod" []; entry "agents" "Lib.Agents" [] ]; Dependencies = [] }
            let root = rootWith [ "Zod"; "Agents"; "SharedLiterals"; "Empty" ]
            let split = splitRoot plan root
            let sliceOf lib = split.Units |> List.find (fun (u, _) -> u.Lib = lib) |> snd
            Expect.isTrue ((sliceOf "Lib.Zod").Modules.ContainsKey "Zod") "zod module to zod unit"
            Expect.isTrue ((sliceOf "Lib.Agents").Modules.ContainsKey "Agents") "agents module to agents unit"
            Expect.isTrue ((sliceOf "Lib.Zod").Modules.ContainsKey "SharedLiterals") "pool to first"
            Expect.equal (split.Pooled |> List.sort) [ "Empty"; "SharedLiterals" ] "pool reported, never silent"
            let total = split.Units |> List.sumBy (fun (_, s) -> s.Modules.Count)
            Expect.equal total root.Modules.Count "every module lands exactly once"

        testCase "root-level types and members pool to the first unit" <| fun _ ->
            let plan = planUnits { Entries = [ entry "zod" "Lib.Zod" []; entry "agents" "Lib.Agents" [] ]; Dependencies = [] }
            let root = rootWith []
            root.Types.Add("RootThing", Unchecked.defaultof<_>)
            let split = splitRoot plan root
            Expect.isTrue ((split.Units.Head |> snd).Types.ContainsKey "RootThing") "root types to first unit"
    ]

let private repoRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..") |> Path.GetFullPath

[<Tests>]
let integrationTests =
    testList "Emission (integration, committed recipe)" [
        testCase "the committed recipe plans 7 units, Zod first, Agents last" <| fun _ ->
            let recipe =
                match Generator.RecipeLoad.load (Path.Combine(repoRoot, "cloudflare.pilot.toml")) with
                | Ok r -> r
                | Error e -> failwith $"recipe: %A{e}"
            let plan = planUnits recipe
            Expect.hasLength plan 7 "seven units"
            Expect.equal plan.Head.Lib "Fidelity.CloudEdge.Zod" "Zod first (measured debt minimization)"
            Expect.equal (plan |> List.last).Lib "Fidelity.CloudEdge.Agents" "Agents last"
            Expect.equal (plan |> List.last).References (plan |> List.take 6 |> List.map _.Lib) "agents references all six"
            Expect.equal (plan |> List.map _.TopModule |> List.distinct |> List.length) 7 "top modules distinct (splittable)"
    ]
