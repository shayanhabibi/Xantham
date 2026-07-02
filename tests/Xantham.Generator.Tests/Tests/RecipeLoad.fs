module Xantham.Generator.Tests.Tests.RecipeLoad

// ─────────────────────────────────────────────────────────────────────────────
// Coverage plane for src/Xantham.Generator/RecipeLoad.fs — the .NET (Tomlyn)
// recipe loader. Mirrors the Fable-side suite (tests/Xantham.Fable.Tests/
// RecipeTests.fs) so the two boundaries cannot drift: same decode rules, same
// error contracts, and the committed cloudflare.pilot.toml as a live fixture.
// ─────────────────────────────────────────────────────────────────────────────

open System.IO
open Expecto
open Xantham
open Xantham.Generator.RecipeLoad

let private decodeOk text =
    match decodeRecipe text with
    | Ok recipe -> recipe
    | Error errors -> failwith $"expected Ok, got errors: %A{errors}"

let private decodeErrors text =
    match decodeRecipe text with
    | Ok recipe -> failwith $"expected errors, got Ok with {recipe.Entries.Length} entries"
    | Error errors -> errors

let private repoRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..") |> Path.GetFullPath

[<Tests>]
let tests =
    testList "RecipeLoad (.NET Tomlyn boundary)" [
        testCase "module entry with explicit entries decodes fully" <| fun _ ->
            let recipe =
                decodeOk """
                [[entry]]
                package = "agents"
                kind = "module"
                entries = [".", "./mcp"]
                """
            Expect.hasLength recipe.Entries 1 "one entry"
            let entry = recipe.Entries.Head
            Expect.equal entry.Package "agents" "package"
            Expect.equal entry.Kind Module "kind"
            Expect.equal entry.Entries [ "."; "./mcp" ] "entries"
            Expect.equal entry.Crawl true "crawl defaults true"

        testCase "module entry without entries defaults to [\".\"]" <| fun _ ->
            let entry = (decodeOk "[[entry]]\npackage = \"partyserver\"\nkind = \"module\"").Entries.Head
            Expect.equal entry.Entries [ "." ] "default entries"

        testCase "lib, policy, and depends-on decode into the shared model" <| fun _ ->
            let entry =
                (decodeOk """
                 [[entry]]
                 package = "zod"
                 kind = "module"
                 crawl = false
                 lib = "Fidelity.CloudEdge.Zod"
                 policy = "opaque-handle"
                 depends-on = ["Fidelity.CloudEdge.Workers"]
                 """).Entries.Head
            Expect.equal entry.Lib (Some "Fidelity.CloudEdge.Zod") "lib"
            Expect.equal entry.Policy (Some OpaqueHandle) "policy"
            Expect.equal entry.DependsOn [ "Fidelity.CloudEdge.Workers" ] "depends-on"
            Expect.equal entry.Crawl false "crawl"

        testCase "ambient-root without root is an error naming the field" <| fun _ ->
            let errors = decodeErrors "[[entry]]\npackage = \"p\"\nkind = \"ambient-root\""
            Expect.hasLength errors 1 "one error"
            Expect.isTrue (errors.Head.Contains "root") "names the field"

        testCase "an unknown policy string is an error naming the value" <| fun _ ->
            let errors = decodeErrors "[[entry]]\npackage = \"zod\"\nkind = \"module\"\npolicy = \"wing-it\""
            Expect.isTrue (errors.Head.Contains "wing-it") "names the bad policy"

        testCase "errors ACCUMULATE across entries (not first-wins)" <| fun _ ->
            let errors =
                decodeErrors """
                [[entry]]
                kind = "module"

                [[entry]]
                package = "p"
                kind = "header"
                """
            Expect.hasLength errors 2 "both entries' errors reported"

        testCase "[dependencies] tables decode with their policies" <| fun _ ->
            let recipe =
                decodeOk """
                [[entry]]
                package = "agents"
                kind = "module"

                [dependencies.eventsource]
                policy = "erase-with-advisory"

                [dependencies."@types/json-schema"]
                policy = "erase-with-advisory"
                """
            Expect.hasLength recipe.Dependencies 2 "two rules"
            Expect.isTrue
                (recipe.Dependencies |> List.forall (fun d -> d.Policy = EraseWithAdvisory))
                "policies parsed"

        testCase "the COMMITTED cloudflare.pilot.toml decodes end-to-end" <| fun _ ->
            // The real recipe is a live fixture on BOTH loader boundaries: if its shape
            // drifts from the shared model, the drift fails here before any generation.
            let recipe =
                match load (Path.Combine(repoRoot, "cloudflare.pilot.toml")) with
                | Ok recipe -> recipe
                | Error errors -> failwith $"committed recipe failed to load: %A{errors}"
            Expect.hasLength recipe.Entries 7 "seven [[entry]] tables"
            Expect.isTrue
                (recipe.Entries |> List.forall (fun e -> e.Lib.IsSome))
                "every entry declares a lib"
            Expect.isTrue (recipe.Dependencies.Length >= 5) "dependencies present"
            let zod = recipe.Entries |> List.find (fun e -> e.Package = "zod")
            Expect.equal zod.Crawl false "zod is a policy-holder"
            Expect.equal zod.Policy (Some OpaqueHandle) "zod policy"
            let agents = recipe.Entries |> List.find (fun e -> e.Package = "agents")
            Expect.hasLength agents.DependsOn 6 "agents depends on the other six libs"
    ]
