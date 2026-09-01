/// End-to-end against the live compiler: the ansi-regex fixture through the whole pipeline,
/// diffed against the committed goldens, plus the run-twice determinism property.
///
/// The fixture packages are npm-installed and therefore untracked: a linked worktree carries
/// tracked files only, so - like `tools/workspace.fsx` does for the compiler itself - the
/// lookup falls back to the main checkout's install. `XANTHAM_REQUIRE_TSC` turns every skip
/// here into a failure, because a green run that generated nothing tested nothing.
module Xantham.Generator.Tests.PipelineTests

open System
open System.IO
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

/// The installed ansi-regex package: this checkout's fixture install, or the main checkout's
/// when this checkout is a worktree with no install of its own.
let private fixturePackage =
    let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    [ root; yield! mainCheckout root |> Option.toList ]
    |> List.map (fun checkout ->
        Path.Combine(checkout, "tests", "fixtures", "ansi-regex", "node_modules", "ansi-regex"))
    |> List.tryFind Directory.Exists

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "golden", "ansi-regex")

/// Golden files are committed, so git may have rewritten their line endings; the generator
/// itself emits `\n` unconditionally.
let private readGolden name =
    let path = Path.Combine(goldenDir, name)

    if File.Exists path then
        Some(File.ReadAllText(path).Replace("\r\n", "\n"))
    else
        None

let private updateGoldens =
    match Environment.GetEnvironmentVariable "XANTHAM_UPDATE_GOLDEN" with
    | null
    | ""
    | "0" -> false
    | _ -> true

[<Tests>]
let pipelineTests =
    testList "generator e2e" [
        match Tsc.locate __SOURCE_DIRECTORY__, fixturePackage with
        | None, _ ->
            testCase "live generation skipped - no compiler" <| fun _ ->
                if required then
                    failtest
                        "XANTHAM_REQUIRE_TSC is set and no tsc was found: `npm install` did not run, or \
                         the worktree redirect in tools/workspace.fsx broke"
                else
                    skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE"
        | _, None ->
            testCase "live generation skipped - no fixture install" <| fun _ ->
                if required then
                    failtest
                        "XANTHAM_REQUIRE_TSC is set and tests/fixtures/ansi-regex has no node_modules: \
                         run `npm install` in that fixture directory"
                else
                    skiptest "run `npm install` in tests/fixtures/ansi-regex"
        | Some _, Some package ->
            testCase "ansi-regex generates the committed goldens" <| fun _ ->
                let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                Expect.equal
                    (rendered.Files |> List.map fst)
                    [ "AnsiRegex.fs"; "manifest.json" ]
                    "one source file and the manifest"

                if updateGoldens then
                    Directory.CreateDirectory goldenDir |> ignore

                    for name, content in rendered.Files do
                        File.WriteAllText(Path.Combine(goldenDir, name), content, Text.UTF8Encoding false)
                else
                    for name, content in rendered.Files do
                        match readGolden name with
                        | None ->
                            failtest
                                $"golden {name} does not exist - run once with XANTHAM_UPDATE_GOLDEN=1 \
                                  and review the diff"
                        | Some golden -> Expect.equal content golden $"{name} matches its golden"

            testCase "no export of the fixture is silently dropped" <| fun _ ->
                let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                let counts = Render.counts (Render.symbolTiers rendered)

                Expect.equal counts.Escape 0 "ansi-regex is declared fully representable - no escapes"

            testCase "generation is deterministic run to run" <| fun _ ->
                let first = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                let second = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                Expect.equal second.Files first.Files "byte-identical output across fresh sessions"

            testCase "a reference disposition templates lib types instead of widening" <| fun _ ->
                let config =
                    { GeneratorConfig.Default with
                        Groups = Map.ofList [ "typescript/lib", Reference ] }

                let rendered = Async.RunSynchronously(Pipeline.generate config package)
                let source = rendered.Files |> List.find (fst >> (=) "AnsiRegex.fs") |> snd

                Expect.stringContains source ": TypeScript.Lib.RegExp = jsNative" "the return is templated (O7)"

                Expect.isEmpty
                    (rendered.Findings |> List.filter (fun finding -> finding.Message.Contains "RegExp"))
                    "a reference emission is Exact - no finding"
    ]
