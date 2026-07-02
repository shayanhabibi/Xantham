module Xantham.Generator.Tests.Tests.AreaAssignment

// ─────────────────────────────────────────────────────────────────────────────
// Coverage plane for src/Xantham.Common/AreaAssignment.fs — Phase 1's ownership
// algebra. UNIT: constructed EncodedResults exercising each authority rule and
// the fixpoint. INTEGRATION: the committed IR + committed recipe — the totality
// measurement (unassigned must be 0 or characterized here, never elsewhere).
// ─────────────────────────────────────────────────────────────────────────────

open System.IO
open System.Text.RegularExpressions
open Expecto
open Xantham
open Xantham.Schema

let private key = TypeKey.createWith

let private iface name fqn members : TsInterface = {
    Source = None
    FullyQualifiedName = fqn
    Enumerable = false
    Name = name
    Members = members
    TypeParameters = []
    Documentation = []
    Heritage = { Extends = [] }
}

let private prop name typeKey : TsMember =
    TsMember.Property {
        Name = name
        Type = key typeKey
        IsStatic = false
        IsOptional = false
        IsPrivate = false
        Accessor = TsAccessor.ReadWrite
        Documentation = []
    }

let private emptyResult : EncodedResult = {
    ExportedDeclarations = Map.empty
    Types = Map.empty
    DuplicateExports = Map.empty
    DuplicateTypes = Map.empty
    TopLevelExports = []
    LibEsExports = []
    EntryExports = Map.empty
}

let private entry package lib : RecipeEntry = {
    Package = package
    Kind = Module
    Root = None
    Entries = [ "." ]
    Crawl = true
    Lib = Some lib
    Policy = None
    DependsOn = []
    Overlay = None
}

let private recipe : Recipe = {
    Entries = [
        entry "pkg-a" "Lib.A"
        entry "pkg-b" "Lib.B"
    ]
    Dependencies = [ { Package = "dep-x"; Policy = EraseWithAdvisory } ]
}

/// The same node_modules extraction the production caller injects.
let packageOfPath (part: string) : string option =
    let m = Regex.Match(part, @"node_modules/((?:@[^/]+/)?[^/""]+)")
    if m.Success then Some m.Groups[1].Value else None

let private assign result = AreaAssignment.assign packageOfPath recipe result

let private pathFqn package name = [ $"\"/repo/node_modules/{package}/dist/index\""; name ]

[<Tests>]
let unitTests =
    testList "AreaAssignment (unit)" [
        testCase "FQN provenance assigns the declaring package's lib" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ key 1, TsType.Interface(iface "T" (pathFqn "pkg-a" "T") []) ] }
            let a = assign result
            Expect.equal a.Owners[key 1] (OwnedBy "Lib.A") "declared in pkg-a"
            Expect.hasLength a.Dead 0 "total"

        testCase "FQN provenance WINS over a re-exporting entry (ownership never moves)" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ key 1, TsType.Interface(iface "T" (pathFqn "pkg-a" "T") []) ]
                    // pkg-b's entry re-exports key 1
                    EntryExports = Map [ "/repo/node_modules/pkg-b/dist/index.d.ts", [ key 1 ] ] }
            let a = assign result
            Expect.equal a.Owners[key 1] (OwnedBy "Lib.A") "declaring package wins"

        testCase "EntryExports seeds FQN-less declarations (ambient/entry decls)" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ key 1, TsType.Union(TsTypeUnion [ key 2; key 3 ]) ]
                    EntryExports = Map [ "/repo/node_modules/pkg-b/dist/index.d.ts", [ key 1 ] ] }
            let a = assign result
            Expect.equal a.Owners[key 1] (OwnedBy "Lib.B") "seeded from the entry"

        testCase "LibEsExports classify as LibEs" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ key 9, TsType.Union(TsTypeUnion []) ]
                    LibEsExports = [ key 9 ] }
            Expect.equal (assign result).Owners[key 9] LibEs "lib.es surface"

        testCase "a synthetic referenced from ONE lib inherits that lib (fixpoint)" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 1, TsType.Interface(iface "T" (pathFqn "pkg-a" "T") [ prop "x" 10 ])
                            key 10, TsType.Union(TsTypeUnion [])
                        ] }
            Expect.equal (assign result).Owners[key 10] (OwnedBy "Lib.A") "single-owner synthetic"

        testCase "a synthetic referenced from TWO libs goes to SharedRoot" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 1, TsType.Interface(iface "A" (pathFqn "pkg-a" "A") [ prop "x" 10 ])
                            key 2, TsType.Interface(iface "B" (pathFqn "pkg-b" "B") [ prop "y" 10 ])
                            key 10, TsType.Union(TsTypeUnion [])
                        ] }
            Expect.equal (assign result).Owners[key 10] SharedRoot "shared synthetic"

        testCase "ownership propagates through CHAINS of synthetics" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 1, TsType.Interface(iface "T" (pathFqn "pkg-a" "T") [ prop "x" 10 ])
                            key 10, TsType.Union(TsTypeUnion [ key 11 ])
                            key 11, TsType.Union(TsTypeUnion [])
                        ] }
            Expect.equal (assign result).Owners[key 11] (OwnedBy "Lib.A") "two hops"

        testCase "erased-dependency types classify by policy and propagate" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 1, TsType.Interface(iface "D" (pathFqn "dep-x" "D") [ prop "x" 10 ])
                            key 10, TsType.Union(TsTypeUnion [])
                        ] }
            let a = assign result
            Expect.equal a.Owners[key 1] (ErasedDependency "dep-x") "policy classification"
            Expect.equal a.Owners[key 10] (ErasedDependency "dep-x") "erased propagation"

        testCase "a lib referencer beats an erased referencer (emission needs it)" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 1, TsType.Interface(iface "A" (pathFqn "pkg-a" "A") [ prop "x" 10 ])
                            key 2, TsType.Interface(iface "D" (pathFqn "dep-x" "D") [ prop "y" 10 ])
                            key 10, TsType.Union(TsTypeUnion [])
                        ] }
            Expect.equal (assign result).Owners[key 10] (OwnedBy "Lib.A") "lib wins over erased"

        testCase "ownership is ORDER-INDEPENDENT: late second-lib evidence upgrades to SharedRoot" <| fun _ ->
            // The measured defect: a literal argument shared by an instantiation node
            // (itself fixpoint-owned by lib A) and a lib-B declaration must be
            // SharedRoot regardless of which referencer the sweep sees first.
            let result =
                { emptyResult with
                    Types =
                        Map [
                            // pkg-a decl -> reference node -> literal
                            key 1, TsType.Interface(iface "A" (pathFqn "pkg-a" "A") [ prop "x" 10 ])
                            key 10, TsType.Union(TsTypeUnion [ key 20 ])
                            // pkg-b decl -> the SAME literal, directly
                            key 2, TsType.Interface(iface "B" (pathFqn "pkg-b" "B") [ prop "y" 20 ])
                            key 20, TsType.Union(TsTypeUnion [])
                        ] }
            let a = assign result
            Expect.equal a.Owners[key 10] (OwnedBy "Lib.A") "reference node stays single-owner"
            Expect.equal a.Owners[key 20] SharedRoot "shared literal upgrades to SharedRoot"

        testCase "an unreachable key (no provenance, no entry, no referencer) is classified Dead" <| fun _ ->
            let result =
                { emptyResult with Types = Map [ key 42, TsType.Union(TsTypeUnion []) ] }
            let a = assign result
            Expect.equal a.Dead [ key 42 ] "reported dead, never silently dropped"

        testCase "an unreachable CLUSTER (mutual references, no owned root) is Dead" <| fun _ ->
            let result =
                { emptyResult with
                    Types =
                        Map [
                            key 42, TsType.Union(TsTypeUnion [ key 43 ])
                            key 43, TsType.Union(TsTypeUnion [ key 42 ])
                        ] }
            let a = assign result
            Expect.equal (a.Dead |> List.sort) [ key 42; key 43 ] "cycles without owners are dead"
    ]

// ── integration: the committed IR + committed recipe — the totality measurement ──

let private repoRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..") |> Path.GetFullPath

[<Tests>]
let integrationTests =
    testList "AreaAssignment (integration, committed IR + recipe)" [
        testCase "assignment is TOTAL over the committed v0 IR (owned + verified-dead)" <| fun _ ->
            let recipe =
                match Generator.RecipeLoad.load (Path.Combine(repoRoot, "cloudflare.pilot.toml")) with
                | Ok r -> r
                | Error e -> failwith $"recipe: %A{e}"
            let ir =
                match Thoth.Json.Net.Decode.fromString EncodedResult.decode (File.ReadAllText(Path.Combine(repoRoot, "src/Xantham.Fable/output.json"))) with
                | Ok r -> r
                | Error e -> failwith $"IR decode: {e}"
            let a = AreaAssignment.assign packageOfPath recipe ir
            let allKeys =
                Seq.append ir.Types.Keys ir.ExportedDeclarations.Keys |> Seq.distinct |> Seq.length
            // totality: every key is owned or dead
            Expect.equal (a.Owners.Count + a.Dead.Length) allKeys "owned + dead = all keys"
            // the dead invariant: no OWNED node references a dead key — dead keys are
            // invisible to emission. A failure here means Dead is misclassified.
            let dead = Set.ofList a.Dead
            let ownedReferencingDead =
                Seq.append
                    (ir.Types |> Seq.map (fun (KeyValue(k, t)) -> k, AreaAssignment.keysOfType t))
                    (ir.ExportedDeclarations |> Seq.map (fun (KeyValue(k, e)) -> k, AreaAssignment.keysOfExport e))
                |> Seq.filter (fun (k, _) -> a.Owners.ContainsKey k)
                |> Seq.filter (fun (_, refs) -> refs |> List.exists dead.Contains)
                |> Seq.truncate 5
                |> Seq.map (fst >> string)
                |> Seq.toList
            Expect.isEmpty ownedReferencingDead $"owned nodes referencing dead keys: %A{ownedReferencingDead}"
            // the distribution is the Phase-1 signal — printed for the report, asserted coarse
            let dist =
                a.Owners.Values
                |> Seq.countBy (function
                    | OwnedBy lib -> lib
                    | SharedRoot -> "<shared-root>"
                    | LibEs -> "<lib-es>"
                    | ErasedDependency p -> $"<erased:{p}>")
                |> Seq.sortByDescending snd
                |> Seq.toList
            printfn "AreaAssignment distribution (dead=%d): %A" a.Dead.Length dist
            let ownedByLib = dist |> List.sumBy (fun (o, n) -> if o.StartsWith "<" then 0 else n)
            Expect.isTrue (ownedByLib > 0) "some keys owned by libs"
            Expect.isTrue
                (dist |> List.exists (fun (o, _) -> o = "Fidelity.CloudEdge.Workers"))
                "Workers lib present in distribution"
    ]
