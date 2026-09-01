/// End-to-end against the live compiler: fixtures through the whole pipeline, diffed against
/// the committed goldens, plus the run-twice determinism property.
///
/// The npm fixture packages are installed and therefore untracked: a linked worktree carries
/// tracked files only, so - like `tools/workspace.fsx` does for the compiler itself - the
/// lookup falls back to the main checkout's install. The `lab` fixture is hand-authored and
/// tracked, so it always resolves locally. `XANTHAM_REQUIRE_TSC` turns every skip here into a
/// failure, because a green run that generated nothing tested nothing.
module Xantham.Generator.Tests.PipelineTests

open System
open System.IO
open System.Text.Json
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

let private required =
    match Environment.GetEnvironmentVariable "XANTHAM_REQUIRE_TSC" with
    | null
    | ""
    | "0"
    | "false" -> false
    | _ -> true

/// The main working tree of a linked worktree, resolved the way `tools/workspace.fsx` does:
/// the worktree's `.git` is a file holding `gitdir:`, and `<gitdir>/commondir` points at the
/// common git directory whose parent is the main checkout.
let private mainCheckout (root: string) : string option =
    let pointer = Path.Combine(root, ".git")

    if not (File.Exists pointer) then
        None
    else
        let text = File.ReadAllText(pointer).Trim()

        if not (text.StartsWith "gitdir:") then
            None
        else
            let gitDir = Path.GetFullPath(Path.Combine(root, text.Substring(7).Trim()))
            let commonDir = Path.Combine(gitDir, "commondir")

            if not (File.Exists commonDir) then
                None
            else
                let common = Path.GetFullPath(Path.Combine(gitDir, File.ReadAllText(commonDir).Trim()))
                let checkout = Path.GetDirectoryName common
                if Directory.Exists checkout then Some checkout else None

let private root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

/// An npm-installed fixture package: this checkout's install, or the main checkout's when this
/// checkout is a worktree with no install of its own.
let private npmFixture (name: string) =
    [ root; yield! mainCheckout root |> Option.toList ]
    |> List.map (fun checkout -> Path.Combine(checkout, "tests", "fixtures", name, "node_modules", name))
    |> List.tryFind Directory.Exists

/// The hand-authored lab fixture, tracked in git - always present.
let private labFixture = Path.Combine(root, "tests", "fixtures", "lab")

/// The version every npm rung is pinned at, from the tracked `tests/fixtures/pins.json`. The
/// install is untracked, so this file is the only record of what a golden was generated
/// against; it is JSONC, like every other configuration Xantham reads.
let private pins: Map<string, string> =
    let path = Path.Combine(root, "tests", "fixtures", "pins.json")

    if not (File.Exists path) then
        Map.empty
    else
        let options =
            JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

        use doc = JsonDocument.Parse(File.ReadAllText path, options)

        doc.RootElement.EnumerateObject()
        |> Seq.map (fun property -> property.Name, property.Value.GetString())
        |> Map.ofSeq

/// The version an install actually carries, from the installed package's own manifest.
let private installedVersion (package: string) =
    let path = Path.Combine(package, "package.json")

    if not (File.Exists path) then
        None
    else
        use doc = JsonDocument.Parse(File.ReadAllText path)

        match doc.RootElement.TryGetProperty "version" with
        | true, value -> Some(value.GetString())
        | _ -> None

let private updateGoldens =
    match Environment.GetEnvironmentVariable "XANTHAM_UPDATE_GOLDEN" with
    | null
    | ""
    | "0" -> false
    | _ -> true

/// Golden files are committed, so git may have rewritten their line endings; the generator
/// itself emits `\n` unconditionally.
let private readGolden (goldenDir: string) name =
    let path = Path.Combine(goldenDir, name)

    if File.Exists path then
        Some(File.ReadAllText(path).Replace("\r\n", "\n"))
    else
        None

/// The golden diff for one fixture: every rendered file matches its committed text, byte for
/// byte (`XANTHAM_UPDATE_GOLDEN=1` rewrites the corpus instead - review the diff).
let private matchesGoldens (fixture: string) (config: GeneratorConfig) (package: string) =
    let goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "golden", fixture)
    let rendered = Async.RunSynchronously(Pipeline.generate config package)

    Expect.equal
        (rendered.Files |> List.map fst)
        [ $"{rendered.ModuleName}.fs"; "manifest.json" ]
        "one source file and the manifest"

    if updateGoldens then
        Directory.CreateDirectory goldenDir |> ignore

        for name, content in rendered.Files do
            File.WriteAllText(Path.Combine(goldenDir, name), content, Text.UTF8Encoding false)
    else
        for name, content in rendered.Files do
            match readGolden goldenDir name with
            | None ->
                failtest
                    $"golden {fixture}/{name} does not exist - run once with XANTHAM_UPDATE_GOLDEN=1 \
                      and review the diff"
            | Some golden -> Expect.equal content golden $"{fixture}/{name} matches its golden"

    rendered

let private fixtureTests (fixture: string) (package: string option) (config: GeneratorConfig) extra =
    match Tsc.locate __SOURCE_DIRECTORY__, package with
    | None, _ ->
        [ testCase $"{fixture}: live generation skipped - no compiler" <| fun _ ->
              if required then
                  failtest
                      "XANTHAM_REQUIRE_TSC is set and no tsc was found: `npm install` did not run, or \
                       the worktree redirect in tools/workspace.fsx broke"
              else
                  skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | _, None ->
        [ testCase $"{fixture}: live generation skipped - no fixture install" <| fun _ ->
              if required then
                  failtest
                      $"XANTHAM_REQUIRE_TSC is set and tests/fixtures/{fixture} has no node_modules: \
                        run `npm install` in that fixture directory"
              else
                  skiptest $"run `npm install` in tests/fixtures/{fixture}" ]
    | Some _, Some package when
        (match Map.tryFind fixture pins, installedVersion package with
         | Some pinned, Some installed -> installed <> pinned
         | _ -> false)
        ->
        // Drift replaces the golden diff rather than joining it: a package that moved and a
        // generator that regressed produce the same diff, and only one of them is a bug here.
        [ testCase $"{fixture}: the install has drifted from its pin" <| fun _ ->
              let pinned = Map.find fixture pins
              let installed = installedVersion package |> Option.defaultValue "?"

              failtest
                  $"tests/fixtures/pins.json pins {fixture} at {pinned}, but the install is {installed}, so \
                    the committed goldens describe a different package. Reinstall the pin, or bump it and \
                    regenerate the goldens (XANTHAM_UPDATE_GOLDEN=1) in the same commit." ]
    | Some _, Some package ->
        [ testCase $"{fixture} generates the committed goldens" <| fun _ ->
              matchesGoldens fixture config package |> ignore

          testCase $"{fixture} generation is deterministic run to run" <| fun _ ->
              let first = Async.RunSynchronously(Pipeline.generate config package)
              let second = Async.RunSynchronously(Pipeline.generate config package)

              Expect.equal second.Files first.Files "byte-identical output across fresh sessions"

          yield! extra package ]

[<Tests>]
let pipelineTests =
    testList "generator e2e" [
        yield!
            fixtureTests "ansi-regex" (npmFixture "ansi-regex") GeneratorConfig.Default (fun package ->
                [ testCase "no export of ansi-regex is silently dropped" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let counts = Render.counts (Render.symbolTiers rendered)

                      Expect.equal counts.Escape 0 "ansi-regex is declared fully representable - no escapes"

                  testCase "a reference disposition templates lib types instead of widening" <| fun _ ->
                      let config =
                          { GeneratorConfig.Default with
                              Groups = Map.ofList [ "typescript/lib", Reference ] }

                      let rendered = Async.RunSynchronously(Pipeline.generate config package)
                      let source = rendered.Files |> List.find (fst >> (=) "AnsiRegex.fs") |> snd

                      Expect.stringContains source ": TypeScript.Lib.RegExp = jsNative" "the return is templated (O7)"

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun finding -> finding.Message.Contains "RegExp"))
                          "a reference emission is Exact - no finding" ])

        yield!
            fixtureTests
                "lab"
                (if Directory.Exists labFixture then Some labFixture else None)
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "no export of the lab is silently dropped" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let counts = Render.counts (Render.symbolTiers rendered)

                          Expect.equal counts.Escape 0 "the lab exercises only supported features - no escapes" ])

        yield! fixtureTests "animejs" (npmFixture "animejs") GeneratorConfig.Default (fun _ -> [])
    ]
