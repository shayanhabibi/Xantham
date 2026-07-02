module Xantham.Fable.Tests.RecipeTests

// ─────────────────────────────────────────────────────────────────────────────
// Coverage plane for src/Xantham.Fable/Recipe.fs — the recipe loader.
//
// UNIT: decodeRecipe — the TOML→typed-model boundary. Every decode rule has a
// case: required fields, kind dispatch, entries default, crawl default/override,
// error accumulation (ALL errors reported, not first-wins).
//
// INTEGRATION: resolveEntryFiles against the repo's own node_modules — the
// declared devDependency pins ARE the fixtures. Includes the ratified defects:
// relative recipe dirs (absolute-path fix) and the MCP SDK's lying root export.
// ─────────────────────────────────────────────────────────────────────────────

open Fable.Core.JsInterop
open Fable.Mocha
open Node.Api
open Xantham
open Xantham.Fable.Recipe

let private decodeOk text =
    match decodeRecipe text with
    | Ok recipe -> recipe
    | Error errors -> failwith $"expected Ok, got errors: %A{errors}"

let private decodeErrors text =
    match decodeRecipe text with
    | Ok recipe -> failwith $"expected errors, got Ok with {recipe.Entries.Length} entries"
    | Error errors -> errors

let unitTests =
    testList "Recipe.decodeRecipe (unit)" [
        testCase "module entry with explicit entries decodes fully" <| fun _ ->
            let recipe =
                decodeOk """
                [[entry]]
                package = "agents"
                kind = "module"
                entries = [".", "./mcp"]
                """
            "one entry" |> Expect.hasLength recipe.Entries 1
            let entry = recipe.Entries.Head
            "package" |> Expect.equal entry.Package "agents"
            "kind" |> Expect.equal entry.Kind Module
            "entries" |> Expect.equal entry.Entries [ "."; "./mcp" ]
            "crawl defaults true" |> Expect.equal entry.Crawl true
            "no root on module entries" |> Expect.equal entry.Root None

        testCase "module entry without entries defaults to [\".\"]" <| fun _ ->
            let entry = (decodeOk "[[entry]]\npackage = \"partyserver\"\nkind = \"module\"").Entries.Head
            "default entries" |> Expect.equal entry.Entries [ "." ]

        testCase "ambient-root entry carries its root" <| fun _ ->
            let entry =
                (decodeOk """
                 [[entry]]
                 package = "@cloudflare/workers-types"
                 kind = "ambient-root"
                 root = "latest/index.ts"
                 """).Entries.Head
            "kind" |> Expect.equal entry.Kind AmbientRoot
            "root" |> Expect.equal entry.Root (Some "latest/index.ts")

        testCase "crawl = false is decoded (policy-holder entries)" <| fun _ ->
            let entry = (decodeOk "[[entry]]\npackage = \"zod\"\nkind = \"module\"\ncrawl = false").Entries.Head
            "crawl" |> Expect.equal entry.Crawl false

        testCase "ambient-root without root is an error naming the entry" <| fun _ ->
            let errors = decodeErrors "[[entry]]\npackage = \"p\"\nkind = \"ambient-root\""
            "one error" |> Expect.hasLength errors 1
            "names the field" |> Expect.isTrue (errors.Head.Contains "root")

        testCase "unknown kind is an error naming the value" <| fun _ ->
            let errors = decodeErrors "[[entry]]\npackage = \"p\"\nkind = \"header\""
            "names the bad kind" |> Expect.isTrue (errors.Head.Contains "header")

        testCase "missing package is an error" <| fun _ ->
            let errors = decodeErrors "[[entry]]\nkind = \"module\""
            "names the field" |> Expect.isTrue (errors.Head.Contains "package")

        testCase "errors ACCUMULATE across entries (not first-wins)" <| fun _ ->
            let errors =
                decodeErrors """
                [[entry]]
                kind = "module"

                [[entry]]
                package = "p"
                kind = "header"
                """
            "both entries' errors reported" |> Expect.hasLength errors 2

        testCase "a recipe with no [[entry]] tables is an error" <| fun _ ->
            let errors = decodeErrors "[recipe]\nname = \"x\""
            "one error" |> Expect.hasLength errors 1

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
            "lib" |> Expect.equal entry.Lib (Some "Fidelity.CloudEdge.Zod")
            "policy" |> Expect.equal entry.Policy (Some Xantham.DependencyPolicy.OpaqueHandle)
            "depends-on" |> Expect.equal entry.DependsOn [ "Fidelity.CloudEdge.Workers" ]

        testCase "an unknown policy string is an error naming the value" <| fun _ ->
            let errors =
                decodeErrors """
                [[entry]]
                package = "zod"
                kind = "module"
                policy = "wing-it"
                """
            "names the bad policy" |> Expect.isTrue (errors.Head.Contains "wing-it")

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
            "two rules" |> Expect.hasLength recipe.Dependencies 2
            "policy parsed"
            |> Expect.isTrue (recipe.Dependencies |> List.forall (fun d -> d.Policy = Xantham.DependencyPolicy.EraseWithAdvisory))

    ]

// Integration fixtures: the repo root's own node_modules, whose exact versions
// are declared devDependencies (the recipe's pin mechanism).
let private repoRoot = path.join(__SOURCE_DIRECTORY__, "..", "..")

let private resolveIn (entries: RecipeEntry list) =
    resolveEntryFiles repoRoot { Entries = entries; Dependencies = [] }

let integrationTests =
    testList "Recipe.resolveEntryFiles (integration, repo node_modules)" [
        testCase "module '.' resolves through the exports map to the types file" <| fun _ ->
            match resolveIn [ { Package = "agents"; Kind = Module; Root = None; Entries = [ "." ]; Crawl = true; Lib = None; Policy = None; DependsOn = [] } ] with
            | Ok [ r ] ->
                "label" |> Expect.equal r.Label "agents:."
                "types file" |> Expect.isTrue (r.File.EndsWith ".d.ts")
            | other -> failwith $"unexpected: %A{other}"

        testCase "module subpath './mcp' resolves distinctly from '.'" <| fun _ ->
            match resolveIn [ { Package = "agents"; Kind = Module; Root = None; Entries = [ "."; "./mcp" ]; Crawl = true; Lib = None; Policy = None; DependsOn = [] } ] with
            | Ok [ root; mcp ] ->
                "distinct files" |> Expect.notEqual root.File mcp.File
                "mcp label" |> Expect.equal mcp.Label "agents:./mcp"
            | other -> failwith $"unexpected: %A{other}"

        testCase "ambient-root resolves to an ABSOLUTE existing file" <| fun _ ->
            match resolveIn [ { Package = "@cloudflare/workers-types"; Kind = AmbientRoot; Root = Some "latest/index.ts"; Entries = []; Crawl = true; Lib = None; Policy = None; DependsOn = [] } ] with
            | Ok [ r ] ->
                "absolute" |> Expect.isTrue (path.isAbsolute r.File)
                "exists" |> Expect.isTrue (fs.existsSync !^r.File)
            | other -> failwith $"unexpected: %A{other}"

        testCase "crawl=false entries resolve to NOTHING (policy-holders)" <| fun _ ->
            match resolveIn [ { Package = "zod"; Kind = Module; Root = None; Entries = [ "." ]; Crawl = false; Lib = None; Policy = None; DependsOn = [] } ] with
            | Ok [] -> ()
            | other -> failwith $"unexpected: %A{other}"

        testCase "PINNED: the MCP SDK root export is a lie (declared but unshipped)" <| fun _ ->
            // The exports map declares "." -> dist/esm/index.d.ts; the package does not
            // ship it (verified 2026-07-02). The recipe must use subpath entries. If this
            // test ever FAILS, upstream fixed their root — the recipe can then simplify.
            match resolveIn [ { Package = "@modelcontextprotocol/sdk"; Kind = Module; Root = None; Entries = [ "." ]; Crawl = true; Lib = None; Policy = None; DependsOn = [] } ] with
            | Error [ e ] -> "resolution failure names the entry" |> Expect.isTrue (e.Contains "@modelcontextprotocol/sdk:.")
            | other -> failwith $"unexpected (did upstream ship a root barrel?): %A{other}"

        testCase "MCP SDK '.js' touchpoint subpaths resolve to their .d.ts twins" <| fun _ ->
            match resolveIn [ { Package = "@modelcontextprotocol/sdk"; Kind = Module; Root = None; Entries = [ "./types.js"; "./server/mcp.js" ]; Crawl = true; Lib = None; Policy = None; DependsOn = [] } ] with
            | Ok [ types; mcp ] ->
                "types twin" |> Expect.isTrue (types.File.EndsWith "types.d.ts")
                "mcp twin" |> Expect.isTrue (mcp.File.EndsWith "mcp.d.ts")
            | other -> failwith $"unexpected: %A{other}"

        testCase "the COMMITTED cloudflare.pilot.toml decodes end-to-end" <| fun _ ->
            // The real recipe is itself a fixture: if its shape drifts from the model,
            // this fails before any crawl does.
            let recipe =
                fs.readFileSync(path.join(repoRoot, "cloudflare.pilot.toml"), "utf8")
                |> decodeOk
            "seven [[entry]] tables" |> Expect.hasLength recipe.Entries 7
            "every crawlable entry declares a lib"
            |> Expect.isTrue (recipe.Entries |> List.forall (fun e -> e.Lib.IsSome))
            "dependencies present" |> Expect.isTrue (recipe.Dependencies.Length >= 5)

        testCase "errors accumulate across ALL failing resolutions" <| fun _ ->
            let missing sub = { Package = "agents"; Kind = Module; Root = None; Entries = [ sub ]; Crawl = true; Lib = None; Policy = None; DependsOn = [] }
            match resolveIn [ missing "./no-such-entry-a"; missing "./no-such-entry-b" ] with
            | Error errors -> "both reported" |> Expect.hasLength errors 2
            | other -> failwith $"unexpected: %A{other}"
    ]

let tests = testList "Recipe" [ unitTests; integrationTests ]
