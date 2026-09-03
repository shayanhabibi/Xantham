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

/// A hand-authored fixture, tracked in git - always present, so it needs no pin.
let private handFixture (name: string) =
    let path = Path.Combine(root, "tests", "fixtures", name)
    if Directory.Exists path then Some path else None

/// A hand-authored fixture laid out the way an install is: the package under its own
/// `node_modules`, with its hand-authored dependencies beside it. A declaration's group is
/// read off its path (O7), so a multi-package lab has to sit where npm would have put it.
let private handInstalledFixture (name: string) =
    let path = Path.Combine(root, "tests", "fixtures", name, "node_modules", name)
    if Directory.Exists path then Some path else None

/// A hand-authored fixture's own `xantham.json` (O4), so a lab's configuration is the file
/// beside its declarations rather than a copy of it here, and the committed golden gates the
/// configured spelling itself.
let private handConfig (path: string option) =
    path |> Option.map GeneratorConfig.load |> Option.defaultValue GeneratorConfig.Default

/// The `inherit` lines a rendered declaration carries, in emission order. Reading them off the
/// source rather than the model is deliberate: §4.4's is-a relation is only real if it survives
/// to the file the compile gate builds.
let private inheritsOf (source: string) (name: string) =
    let lines = source.Replace("\r\n", "\n").Split '\n'

    match
        lines
        |> Array.tryFindIndex (fun line -> line.StartsWith $"type {name} =" || line.StartsWith $"type {name}<")
    with
    | None -> failtest $"no declaration named {name} in the rendered source"
    | Some start ->
        lines
        |> Array.skip (start + 1)
        |> Array.takeWhile (fun line -> line.StartsWith "    ")
        |> Array.choose (fun line ->
            let trimmed = line.Trim()

            if trimmed.StartsWith "inherit " then
                Some(trimmed.Substring 8)
            else
                None)
        |> List.ofArray

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

/// Where two golden texts first differ, as a bounded report rather than as both texts.
///
/// `Expect.equal` on a golden prints *both* strings in full: measured on the `workers-types`
/// golden's shape - 30k lines differing on one - that is a 2.9 MB, 60,000-line failure message
/// for a one-line change. It is unreadable in a terminal and it is worse than unreadable to an
/// agent, whose context window it consumes entirely. The failing line, its neighbours and the
/// counts say everything the dump did; `git diff` is where the whole change is read.
let private goldenMismatch (label: string) (rendered: string) (golden: string) =
    let renderedLines = rendered.Split '\n'
    let goldenLines = golden.Split '\n'

    let differing =
        Seq.init (max renderedLines.Length goldenLines.Length) id
        |> Seq.filter (fun i ->
            (if i < renderedLines.Length then renderedLines[i] else null)
            <> (if i < goldenLines.Length then goldenLines[i] else null))
        |> Seq.toList

    match differing with
    | [] -> None
    | first :: _ ->
        let window (lines: string[]) =
            [ max 0 (first - 3) .. min (lines.Length - 1) (first + 3) ]
            |> List.map (fun i -> $"  {i + 1,6}| {lines[i]}")
            |> String.concat "\n"

        Some(
            $"{label} does not match its golden.\n\
              First difference at line {first + 1}; {differing.Length} lines differ by position \
              (golden {goldenLines.Length} lines, rendered {renderedLines.Length}). An inserted \
              line shifts every line after it, so that count is a bound, not a hunk count.\n\
              --- golden ---\n{window goldenLines}\n\
              --- rendered ---\n{window renderedLines}\n\
              Regenerate with XANTHAM_UPDATE_GOLDEN=1 and read the change through `git diff \
              --stat` and the manifest counts - see .claude/rules/generator-fixtures.md."
        )

/// The golden diff for one fixture: every rendered file matches its committed text, byte for
/// byte (`XANTHAM_UPDATE_GOLDEN=1` rewrites the corpus instead - review the diff).
let private matchesGoldens (fixture: string) (config: GeneratorConfig) (package: string) =
    let goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "golden", fixture)
    let rendered = Async.RunSynchronously(Pipeline.generate config package)

    // A shipped group is written under `groups/` (O7); what every run owes is the entry
    // package's module and the manifest.
    Expect.equal
        (rendered.Files |> List.map fst |> List.filter (fun name -> not (name.StartsWith "groups/")))
        [ $"{rendered.ModuleName}.fs"; "manifest.json" ]
        "the entry module and the manifest"

    if updateGoldens then
        Directory.CreateDirectory goldenDir |> ignore

        for name, content in rendered.Files do
            let path = Path.Combine(goldenDir, name)
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, content, Text.UTF8Encoding false)
    else
        for name, content in rendered.Files do
            match readGolden goldenDir name with
            | None ->
                failtest
                    $"golden {fixture}/{name} does not exist - run once with XANTHAM_UPDATE_GOLDEN=1 \
                      and review the diff"
            | Some golden ->
                match goldenMismatch $"{fixture}/{name}" content golden with
                | Some report -> failtest report
                | None -> ()

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

/// The exports `audit-coverage` reported missing that no other pass explained. A rung cannot
/// always claim zero escapes, but it can claim the safety net never fires alone: some pass owns
/// every drop and named the reason.
let private unexplainedDrops (rendered: RenderModel) =
    let symbolsOf predicate =
        rendered.Findings |> List.filter predicate |> List.map _.Symbol |> Set.ofList

    let audited = symbolsOf (fun finding -> finding.Pass = "audit-coverage")
    let explained = symbolsOf (fun finding -> finding.Pass <> "audit-coverage")
    Set.difference audited explained |> Set.toList

/// `xantham.json` is authored by hand next to the package manifest (O4); a missing file is the
/// default, and every key it does carry has to round-trip into the config record.
[<Tests>]
let configTests =
    let withConfig (json: string) (test: GeneratorConfig -> unit) =
        let dir = Path.Combine(Path.GetTempPath(), "xantham-config-" + Guid.NewGuid().ToString "N")
        Directory.CreateDirectory dir |> ignore

        try
            File.WriteAllText(Path.Combine(dir, "xantham.json"), json)
            test (GeneratorConfig.load dir)
        finally
            Directory.Delete(dir, true)

    testList "generator config" [
        testCase "a missing file is the default" <| fun _ ->
            let dir = Path.Combine(Path.GetTempPath(), "xantham-config-" + Guid.NewGuid().ToString "N")
            Expect.equal (GeneratorConfig.load dir) GeneratorConfig.Default "nothing configured"

        testCase "lib is the compiler's lib option, as tsconfig spells it" <| fun _ ->
            // A global type library that replaces the DOM (`@cloudflare/workers-types`) has to
            // be generated without it, or every name it shares with `lib.dom.d.ts` merges into
            // the compiler lib and is not the package's to harvest.
            withConfig """{ "lib": ["esnext", "webworker"], /* comment */ "groups": { "typescript/lib": "reference" } }"""
            <| fun config ->
                Expect.equal config.Lib (Some [ "esnext"; "webworker" ]) "lib carried through in order"
                Expect.equal (Map.find "typescript/lib" config.Groups) Reference "groups still parsed beside it"

        testCase "lib that is not an array of strings is an error, not a silent default" <| fun _ ->
            Expect.throws (fun () -> withConfig """{ "lib": "esnext" }""" ignore) "a bare string is refused"

        // Wave two lane D (recon blocker 5): the escape hatch for a runtime package the
        // DefinitelyTyped convention cannot derive from the types package's name.
        testCase "runtime overrides the package an import names" <| fun _ ->
            withConfig """{ "runtime": "three" }"""
            <| fun config -> Expect.equal config.RuntimePackage (Some "three") "the key round-trips"

        testCase "the derived runtime package is DefinitelyTyped's own naming convention" <| fun _ ->
            let derived = GeneratorConfig.derivedRuntimePackage

            // The types are published under `@types/`; the code is not published there at all.
            Expect.equal (derived "@types/three") "three" "an unscoped package loses the prefix"

            // DefinitelyTyped publishes one flat `@types` scope, so it folds a scoped package's
            // own scope into the name with a double underscore. Unfolding it is the only way a
            // scoped package's runtime name is recoverable: nothing in a DT manifest states it.
            Expect.equal (derived "@types/babel__core") "@babel/core" "a scope-mangled name unfolds"
            Expect.equal (derived "@types/babel__plugin-transform-react-jsx") "@babel/plugin-transform-react-jsx" "hyphens are untouched"

            // Everything that is its own runtime keeps its own name, which is every rung of the
            // corpus - a scoped package included, since only the `@types` scope means this.
            Expect.equal (derived "three") "three" "an ordinary package is unchanged"
            Expect.equal (derived "@cloudflare/workers-types") "@cloudflare/workers-types" "another scope is not @types"
            Expect.equal (derived "phase-b-lab") "phase-b-lab" "and so is a lab"

        // Wave five, lane R. A group's value is a string for the dispositions that need no
        // detail, and an object for the one that does: a mapped group has to say which name
        // goes where, and at what arity.
        testCase "a mapped group carries the table its destinations need" <| fun _ ->
            withConfig
                """{
                    "groups": {
                        "typescript/lib": "reference",
                        "@types/node": {
                            "map": {
                                "Buffer": "Node.Buffer.Buffer",
                                "Readable": { "name": "Node.Stream.Readable", "arity": 1 }
                            }
                        }
                    }
                }"""
            <| fun config ->
                Expect.equal (Map.find "typescript/lib" config.Groups) Reference "a string group is unchanged"

                Expect.equal
                    (Map.find "@types/node" config.Groups)
                    (GroupDisposition.Map(
                        Map.ofList
                            [
                                "Buffer",
                                {
                                    FSharpName = "Node.Buffer.Buffer"
                                    Arity = 0
                                }
                                "Readable",
                                {
                                    FSharpName = "Node.Stream.Readable"
                                    Arity = 1
                                }
                            ]
                    ))
                    "a bare destination takes no type arguments; one that does states its arity"

        testCase "a mapped group without a table is refused, not read as a widening" <| fun _ ->
            Expect.throws
                (fun () -> withConfig """{ "groups": { "@types/node": "map" } }""" ignore)
                "the table is the disposition"

            Expect.throws
                (fun () -> withConfig """{ "groups": { "@types/node": { "widen": {} } } }""" ignore)
                "and an object group is a mapped one"

        testCase "a configured runtime package wins over the derivation" <| fun _ ->
            let config =
                { GeneratorConfig.Default with
                    RuntimePackage = Some "not-derivable" }

            Expect.equal (GeneratorConfig.runtimePackage config "@types/three") "not-derivable" "config decides"
            Expect.equal (GeneratorConfig.runtimePackage GeneratorConfig.Default "@types/three") "three" "unset derives"
    ]

/// Wave five, lane R: the `map` disposition, generated under the lab's own `xantham.json`.
let private groupMapLab = handFixture "group-map-lab"

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
                (handFixture "lab")
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "no export of the lab is silently dropped" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let counts = Render.counts (Render.symbolTiers rendered)

                          Expect.equal counts.Escape 0 "the lab exercises only supported features - no escapes" ])

        yield!
            fixtureTests "globals-lab" (handFixture "globals-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a package with no module is harvested from global scope" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "[<Global(\"registry\")>]" "a global value binds off globalThis"
                      Expect.stringContains source "[<Global(\"Gadget\"); EmitConstructor>]" "so does a global class"
                      Expect.isFalse (source.Contains "[<Import(") "a global library imports nothing"

                      // repair-arity, end to end: a brand holds no value, so it has no setter.
                      // `__brand` reaches the checker escaped to `___brand`; the emitted member
                      // has to name the key the object actually carries.
                      Expect.stringContains source "abstract __brand: unit\n" "the brand reads but does not write"

                      // `Loose<P> = { [key: string]: string }` used to widen away its parameter
                      // and be dropped by repair-arity, because an index signature is not a
                      // property and the type read as empty. Now that §4.10's signatures are
                      // shaped, it has a body - and a declaration keeps an unused parameter
                      // happily, where the abbreviation it used to become could not (FS0035).
                      Expect.stringContains source "type Loose<'P> =" "the alias survives once its index signature is shape"
                      Expect.stringContains source "abstract Item: string -> string with get, set" "and carries an EmitIndexer"

                  testCase "an ambient module declaration is dropped loudly" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      // Its name is its quoted specifier, which is not an F# declaration name;
                      // the escape is the promise that it will not vanish unremarked.
                      Expect.contains
                          (rendered.Findings
                           |> List.filter (fun finding -> finding.Pass = "harvest-globals")
                           |> List.map (fun finding -> finding.Tier, finding.Symbol))
                          (Escape, "\"globals-lab:extra\"")
                          "the ambient module is an escape, not a silence" ])

        yield!
            fixtureTests "generics-lab" (handFixture "generics-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a generic declaration reached only through instantiations is declared once, generically" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `Ready<T>` is not exported; `Resource<T>` and `StringResource` reach two
                    // instantiations of it. The generic declaration is what gets named - once -
                    // and each instantiation is written as an application of it.
                    Expect.stringContains source "type Ready<'T> =" "the generic target is the declaration"
                    Expect.stringContains source "abstract latest: 'T with get, set" "its members read its own parameter"
                    Expect.stringContains source "type Resource<'T> = U2<Pending<'T>, Ready<'T>>" "instantiations are applications"
                    Expect.stringContains source "type StringResource = Ready<string>" "a concrete instantiation too"
                    Expect.isFalse (source.Contains "type Ready2") "no second copy of the expansion under a made-up name"

                  testCase "a generic union alias keeps its parameter" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd
                    Expect.stringContains source "type Ref<'T> = U2<'T, Action<'T>>" "the arms read the alias's parameter"
                    Expect.stringContains source "type Source<'S> = U2<'S, Func<'S>> option" "so does one with a nullish arm hoisted"

                  testCase "an anonymous object type nested in a generic scope is declared over the variables it reads" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `each<T, U>(props: { items: T[]; ... render: (item: T, index: number) => U })`
                    // hoists the parameter object to a declaration; that declaration binds
                    // nothing of its own, so it is declared over the free variables its members
                    // mention and the parameter position applies them back.
                    Expect.stringContains source "type EachProps<'T, 'U> =" "the hoisted declaration is generic over what it reads"
                    Expect.stringContains source "abstract items: 'T[] with get, set" "a member reads the outer variable"
                    Expect.stringContains source "abstract render: Func<'T, float, 'U> with get, set" "so does a callback member"
                    Expect.stringContains source "static member each<'T, 'U> (props: EachProps<'T, 'U>) : 'U[] = jsNative" "the use applies them back"
                    Expect.stringContains source "type Handle<'T> = Func<'T> * HandleItem<'T>" "the same inside a generic alias"

                  testCase "nothing in the lab widens a type parameter out of scope" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                    Expect.isEmpty
                        (rendered.Findings |> List.filter (fun finding -> finding.Message.Contains "not in scope here"))
                        "every type parameter the lab writes is in scope where it is read" ])

        yield!
            fixtureTests "intersection-lab" (handFixture "intersection-lab") GeneratorConfig.Default (fun package ->
                [ testCase "an intersection of object types flattens into one interface (§4.6)" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `Named & Timed` carries both member sets; the checker hands them over
                    // flattened, so the declaration is one interface and the reference names it.
                    Expect.stringContains source "type NamedTimed =" "the alias is declared as an interface"
                    Expect.stringContains source "abstract name: string with get, set" "the first operand's member"
                    Expect.stringContains source "abstract stamp: string option" "the second operand's optional readonly member"
                    Expect.stringContains source "type Extended =" "an anonymous operand flattens the same way"
                    Expect.stringContains source "abstract extra: bool with get, set" "with its own members"
                    Expect.isFalse (source.Contains "type NamedTimed = obj") "nothing flattenable widens"

                  testCase "an intersection at a parameter position is hoisted and named by path" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "type LabelTarget =" "hoisted like an anonymous object"
                    Expect.stringContains source "abstract id: float with get, set" "carrying the anonymous operand's member"
                    Expect.stringContains source "static member label (target: LabelTarget) : unit" "and applied at the parameter"

                  testCase "a generic intersection alias binds its parameter and reads it" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "type WithValue<'T> =" "declared over the alias's parameter"
                    Expect.stringContains source "abstract value: 'T with get, set" "which the member reads in scope"

                  testCase "overlapping, indexed, mapped and callable operands flatten" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "type LoudPitched =" "operands sharing a member declare once"
                    Expect.equal (source.Split("abstract volume: float").Length - 1) 3 "volume once per declaration that has it"
                    Expect.stringContains source "type Bag =" "an index-signature operand"
                    Expect.stringContains source "abstract Item: string -> obj with get, set" "carries its indexer"
                    Expect.stringContains source "type Loose =" "a mapped operand expands under D6"
                    Expect.stringContains source "abstract at: float option with get, set" "so its members arrive optional"
                    Expect.stringContains source "type Cancelable =" "a callable operand keeps its properties"
                    Expect.stringContains source "abstract cancel: unit -> unit" "and loses its call signature loudly"

                  testCase "an intersection over a type-parameter operand still widens, and says so" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "static member merge<'T> (``base``: obj) : 'T" "T & { id: number } has no members to flatten"

                    Expect.contains
                        (rendered.Findings |> List.map (fun finding -> finding.Tier, finding.Message))
                        (Widened, "intersection over a non-object operand has no members to flatten; widened to obj (§4.6)")
                        "the widening is owned"

                    Expect.isEmpty
                        (rendered.Findings
                         |> List.filter (fun finding -> finding.Message.Contains "has no F# form yet"))
                        "the old blanket widening is gone" ])

        yield!
            fixtureTests "intersection-empty-lab" (handFixture "intersection-empty-lab") GeneratorConfig.Default (fun package ->
                [ testCase "an empty object operand reduces away and the union keeps its arms (§4.6)" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "type Ease = string" "the literals and the primitive are one string"
                    Expect.stringContains source "type Loose = string" "the idiom outside a union reduces the same way"
                    Expect.stringContains source "type Size = string" "`Record<never, never>` is the same empty operand"
                    Expect.stringContains source "type Weight = float" "and a number carries it the same way"

                    Expect.equal
                        (rendered.Findings
                         |> List.filter (fun finding -> finding.Key = "TR049")
                         |> List.map _.Symbol
                         |> List.distinct
                         |> List.sort)
                        [ "Ease"; "Loose"; "Size"; "Weight" ]
                        "each reduction is owned by the declaration that carries it"

                  testCase "an operand that declares something is not an empty operand" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "type UserId" "a marker-only operand is still a brand"
                    Expect.stringContains source "[<Measure>]" "emitted as a measure (§4.6, D11)"
                    Expect.stringContains source "type Counted = obj" "an operand with a real member widens as it did"
                    Expect.stringContains source "abstract width: float with get, set" "an object operand still flattens"

                    Expect.isEmpty
                        (rendered.Findings
                         |> List.filter (fun finding ->
                             finding.Key = "TR049"
                             && (finding.Symbol.StartsWith "Counted"
                                 || finding.Symbol.StartsWith "UserId"
                                 || finding.Symbol.StartsWith "Padded")))
                        "nothing carrying a member is reduced" ])

        yield!
            fixtureTests
                "intersection-callable-lab"
                (handFixture "intersection-callable-lab")
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "a callable intersection at a member position reaches its signatures (§4.6)" <| fun _ ->
                        let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                        let source = rendered.Files |> List.head |> snd

                        Expect.stringContains
                            source
                            "abstract round: Func<float, float, float>"
                            "the member reads the first signature rather than obj"

                        Expect.isFalse (source.Contains "abstract round: obj") "and no longer widens"

                        Expect.equal
                            (rendered.Findings
                             |> List.filter (fun finding -> finding.Key = "TR050")
                             |> List.map (fun finding -> finding.Symbol, finding.Message))
                            [ "Utils.round", "intersection of callable operands rendered from its 2 call signatures" ]
                            "the member position is the one site, and it counts both signatures"

                      testCase "the export position already rendered both signatures, and still does" <| fun _ ->
                        let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                        let source = rendered.Files |> List.head |> snd

                        Expect.stringContains
                            source
                            "static member roundPad (value: float, length: float) : float"
                            "the first operand's signature"

                        Expect.stringContains
                            source
                            "static member roundPad (length: float) : float"
                            "and the second, as an overload"

                        Expect.isEmpty
                            (rendered.Findings
                             |> List.filter (fun finding -> finding.Symbol.StartsWith "roundPad"))
                            "an export position loses nothing and says nothing"

                      testCase "a hybrid carrying properties still flattens into a shape" <| fun _ ->
                        let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                        let source = rendered.Files |> List.head |> snd

                        Expect.stringContains source "abstract cancel: unit -> unit" "the property survives"

                        Expect.isEmpty
                            (rendered.Findings
                             |> List.filter (fun finding ->
                                 finding.Key = "TR050" && finding.Symbol.StartsWith "Timers"))
                            "and the hybrid is not read as a callback" ])

        yield!
            fixtureTests "inherit-lab" (handFixture "inherit-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a declared base is inherited beside the members it redeclares (§4.4)" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.equal (inheritsOf source "Derived") [ "Base" ] "the is-a relation is emitted"

                    Expect.stringContains
                        source
                        "static member Create (extra: bool, name: string, at: float) : Derived"
                        "and the inherited members are still declared, which is what keeps Create exact"

                    Expect.equal (inheritsOf source "Narrowed") [ "Base" ] "a narrowed member does not cost the edge"

                  testCase "a diamond inherits both arms, and the shared member declares once each" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.equal (inheritsOf source "Pitched") [ "Loud" ] "the near arm"
                    Expect.equal (inheritsOf source "Both") [ "Loud"; "Pitched" ] "both arms of the diamond"

                    Expect.equal
                        (source.Split("abstract volume: float").Length - 1)
                        3
                        "F# admits the redeclaration down every arm"

                  testCase "a generic base carries its argument to the inherit" <| fun _ ->
                    // `inherit Box` alone is FS0033: F# has no bare generic base.
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.equal (inheritsOf source "Labelled") [ "Box<'T>" ] "the parameter travels"
                    Expect.equal (inheritsOf source "Tagged") [ "Box<string>" ] "so does a fixed argument"
                    Expect.equal (inheritsOf source "Leaf") [ "Node" ] "a class base is the instance side's base"
                    Expect.equal (inheritsOf source "Slim") [ "SlimBase" ] "a utility-type base is what it synthesized"

                  testCase "a base this run does not declare stays flattened, and says which case it is" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.isEmpty (inheritsOf source "Failure") "Error has no F# name at this position"
                    Expect.isEmpty (inheritsOf source "Deferred") "JS.Promise is not declared by this run"

                    let byKey =
                        rendered.Findings
                        |> List.map (fun finding -> finding.Key, finding.Symbol, finding.Tier)

                    Expect.contains byKey ("SI002", "Failure", Ergonomic) "the nameless base"
                    Expect.contains byKey ("SI006", "Deferred", Ergonomic) "the named-but-undeclared base"

                    Expect.contains
                        (rendered.Findings |> List.map _.Message)
                        "base JS.Promise is not declared by this run as an interface; its members are flattened in and the is-a relation is not emitted (§4.4)"
                        "the manifest names the base that was left behind"

                    Expect.isFalse
                        (source.Contains "inherit obj")
                        "FS0887: obj is not an interface type, so it is never inherited" ])


        yield!
            fixtureTests "statics-lab" (handFixture "statics-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a class static binds through a dotted import selector" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // Fable reads `Counter.MAX` as "import `Counter`, then take `MAX` off it",
                    // which is exactly what the constructor object carries at runtime.
                    Expect.stringContains source "[<Import(\"Counter.MAX\", \"statics-lab\")>]" "the selector is dotted"
                    Expect.stringContains source "static member MAX: float = jsNative" "a const-like static reads get-only"
                    Expect.stringContains source "static member from (value: float) : Counter = jsNative" "a static factory"
                    Expect.stringContains source "static member ``of`` (value: float) : Counter = jsNative" "one overload"
                    Expect.stringContains source "static member ``of`` (text: string) : Counter = jsNative" "and the other"

                    // The statics sit on the class's own type, so a consumer spells them the
                    // way TypeScript does - `Counter.MAX`, not `Exports.Counter_MAX`.
                    Expect.stringContains
                        source
                        "[<Interface>]\n[<Import(\"Counter\", \"statics-lab\")>]\ntype Counter ="
                        "which makes the type need the attribute, and the settable static a second one"

                  testCase "a subclass carries the statics JavaScript inherits for it" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "[<Import(\"Doubling.MAX\", \"statics-lab\")>]" "off the subclass's own constructor"

                  testCase "a static on a generic declaration is emitted" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains source "[<Import(\"Box.EMPTY\", \"statics-lab\")>]" "the declaration is legal F#"

                  testCase "a settable static gets a setter, under the declaration's own binding" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // The member carries no attribute of its own: a per-member `[<Import>]`
                    // turns `Counter.tick <- 8.0` into the call `Counter.tick(8)`.
                    Expect.stringContains
                        source
                        "    static member tick\n        with get (): float = jsNative\n        and set (_: float): unit = jsNative"
                        "the setter is emitted"

                    Expect.isFalse
                        (source.Contains "[<Import(\"Counter.tick\", \"statics-lab\")>]")
                        "and the dotted selector is gone"

                    Expect.contains
                        (rendered.Findings |> List.map (fun finding -> finding.Tier, finding.Message))
                        (Exact, "settable static emitted with a setter")
                        "the setter is owned"

                  testCase "only method-over-method survives a static/instance name collision" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // F# admits a static method beside an abstract method of the same name and
                    // nothing else: property/property is FS0441, method/property FS0434, and a
                    // static property under an abstract method is FS3214 at every use.
                    Expect.stringContains source "static member json (body: float) : Clash = jsNative" "method over method"

                    for dropped in [ "Clash.status"; "Clash.text"; "Clash.url" ] do
                        Expect.isFalse (source.Contains $"[<Import(\"{dropped}\", \"statics-lab\")>]") $"{dropped} is dropped"

                    let messages = rendered.Findings |> List.map (fun finding -> finding.Symbol, finding.Message)

                    for dropped in [ "Clash.status"; "Clash.text"; "Clash.url" ] do
                        Expect.contains
                            messages
                            (dropped,
                             "static member dropped: its name is an instance member's, which F# admits only between two methods")
                            $"{dropped} says why" ])

        yield!
            fixtureTests "ctor-lab" (handFixture "ctor-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a constructor object is declared as its own interface (§4.4)" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `declare const Widget: { prototype: Widget; new (label: string): Widget }`.
                    // F# has no first-class type for a class's static side, so it becomes an
                    // interface whose construct signatures are `[<EmitConstructor>] Create`
                    // members - `$0` is the object the member is read off.
                    Expect.stringContains source "type WidgetConstructor =" "named after the export it is the value of"
                    Expect.stringContains source "abstract DEFAULT_LABEL: string" "a property of it is a class static"
                    Expect.stringContains source "[<EmitConstructor>]" "the construct signature is a constructor"
                    Expect.stringContains source "abstract Create: label: string -> Widget" "returning the instance side"

                    // `prototype` *is* the instance side, which is a declaration of its own.
                    Expect.isFalse (source.Contains "abstract prototype") "prototype is not a member of the static side"

                    Expect.stringContains
                        source
                        "static member Widget: WidgetConstructor = jsNative"
                        "so the export is constructible rather than obj"

                  testCase "typeof X at a member position names the constructor object" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // The construct the whole `ServiceWorkerGlobalScope` constructor table is
                    // made of; it used to reach `abstract Gauge: obj`.
                    Expect.stringContains source "abstract Gauge: GaugeConstructor" "the member names the static side"
                    Expect.stringContains source "type GaugeConstructor =" "which is declared"
                    Expect.stringContains source "abstract UNIT: string" "carrying the class's statics"
                    Expect.stringContains source "abstract Create: size: float -> Gauge" "and its constructor"

                  testCase "an interface of construct signatures only is no longer obj" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.isFalse (source.Contains "type ParcelFactory = obj") "it has members after all"
                    Expect.stringContains source "abstract Create<'T>: value: 'T -> Parcel<'T>" "a generic construct signature"
                    Expect.stringContains source "abstract Create: unit -> Parcel<string>" "and its overload"

                  testCase "nothing else grows a constructor interface" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // A class no `typeof` names keeps its static side on `Exports` and its own
                    // declaration, where `shape-classes` puts it; `typeof` over a plain value is
                    // that value's type.
                    Expect.isFalse (source.Contains "SoloConstructor") "an unreferenced class's static side is not declared"
                    Expect.isFalse (source.Contains "VersionConstructor") "nor is typeof over a plain value"
                    Expect.stringContains source "abstract version: string" "which reads as the value's own type"

                  testCase "the idiom is recorded, and nothing in the lab widens" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                    Expect.contains
                        (rendered.Findings |> List.map (fun finding -> finding.Tier, finding.Message))
                        (Ergonomic,
                         "constructor object declared as its own interface; 1 construct signature(s) read as EmitConstructor Create members (§4.4)")
                        "the mapping owns itself"

                    Expect.isEmpty
                        (rendered.Findings |> List.filter (fun finding -> finding.Tier = Widened || finding.Tier = Escape))
                        "every constructor object in the lab is declared" ])

        yield!
            fixtureTests "phantom-arity-lab" (handFixture "phantom-arity-lab") GeneratorConfig.Default (fun package ->
                [ testCase "an alias whose target leaves a parameter unused is written as a phantom" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `Reserved` is on the left of `SurplusParameter` and nowhere on the right,
                    // which FS0035 forbids in an abbreviation and a union admits.
                    Expect.stringContains
                        source
                        "type SurplusParameter<'T, 'Reserved> = private SurplusParameter__ of"
                        "the head keeps both parameters behind an erased private case"

                    Expect.stringContains source "[<Erase>]" "erased, so the phantom costs nothing at runtime"

                  testCase "an alias whose target uses every parameter stays an abbreviation" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    Expect.stringContains
                        source
                        "type EveryParameter<'T, 'Reserved> = Func<'T, 'Reserved>"
                        "the negative is untouched"

                  testCase "the erasure is reported once, by repair-arity" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                    let arity =
                        rendered.Findings
                        |> List.filter (fun finding -> finding.Key = "RA006")
                        |> List.map (fun finding -> finding.Pass, finding.Symbol)

                    Expect.equal arity [ "repair-arity", "SurplusParameter" ] "one pass owns the condition"

                    Expect.isEmpty
                        (rendered.Findings
                         |> List.filter (fun finding ->
                             finding.Symbol = "SurplusParameter"
                             && (finding.Key = "SA002" || finding.Key = "RA001" || finding.Pass = "audit-coverage")))
                        "no second report of the same erasure, and the export is represented" ])

        yield!
            fixtureTests "keyof-lab" (handFixture "keyof-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a mapped type over a concrete operand is expanded, not widened (D6)" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `Partial`, `Pick`, `Omit`, `Readonly` and `Record` are written in
                    // lib.es5.d.ts, so they group as the compiler lib - but they are type-level
                    // functions with no runtime identity, and what they stand for is the entry
                    // package's own operand transformed. Deferring to a name that does not
                    // exist would widen every one of these to obj.
                    Expect.stringContains source "abstract duration: float option with get, set" "Partial hoists to option"
                    Expect.stringContains source "static member Create (duration: float, label: string) : OptionsHead" "Pick selects"
                    Expect.stringContains source "static member Create (label: string, loop: bool) : OptionsTail" "Omit removes"
                    Expect.stringContains source "abstract duration: float\n" "Readonly drops the setter"

                    for widened in [ "type Registry = obj"; "type PartialOptions = obj"; "type FrozenOptions = obj" ] do
                        Expect.isFalse (source.Contains widened) $"{widened} is no longer the emission"

                  testCase "the closed keyof regime resolves without the support package" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // §4.10's first regime: the checker finishes these on its own, so they need
                    // nothing beyond the union and literal machinery phase C already landed.
                    Expect.stringContains source "type Duration = float" "a concrete indexed access"
                    Expect.stringContains source "type DurationOrLabel = U2<string, float>" "over a union of keys"
                    Expect.stringContains source "| [<CompiledName(\"duration\")>] Duration" "keyof Options is a StringEnum"

                  testCase "the open keyof regime is carried by the support package" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // §4.10's second regime: the checker cannot finish these, and F# cannot
                    // state `K extends keyof T` at all. The key variable is dropped and its
                    // uses are written as the erased idioms instead - which is the whole
                    // reason the support package exists.
                    Expect.stringContains
                        source
                        "static member get<'T, 'R> (source: 'T, key: typekeyof<'T, 'R>) : 'R = jsNative"
                        "K extends keyof T plus T[K] is the typed accessor"

                    Expect.stringContains
                        source
                        "static member keys<'T> (source: 'T) : keyof<'T>[] = jsNative"
                        "a bare keyof T"

                    Expect.stringContains
                        source
                        "abstract read<'R>: key: typekeyof<'T, 'R> -> 'R"
                        "the same idiom on a member, over the interface's own operand"

                    Expect.stringContains source "abstract all: unit -> keyof<'T>[]" "keyof T in return position"

                    // `T[keyof T]` is a different animal: no key variable selects it, so there
                    // is nothing to name the value type. It stays widened, and says so.
                    Expect.stringContains
                        source
                        "static member values<'T> (source: 'T) : obj[] = jsNative"
                        "the value-of idiom has no F# form"

                    Expect.isTrue
                        (rendered.Findings
                         |> List.exists (fun f -> f.Symbol.StartsWith "values" && f.Tier = Widened))
                        "and the widening is recorded"

                  testCase "a type-level computation over an open operand emits an erased phantom" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // §4.10/§4.11: mapped, conditional and template-literal declarations whose
                    // operand the checker could not supply. F# can express none of them and has
                    // no unused type variable in an abbreviation either, so the name and arity
                    // survive as a phantom with a private case - a cast is the only use of it.
                    Expect.stringContains
                        source
                        "type DeepPartial<'T> = private DeepPartial__ of obj"
                        "a mapped type over an open operand"

                    Expect.stringContains
                        source
                        "type Unwrap<'T> = private Unwrap__ of obj"
                        "a conditional over an open operand"

                    // A template literal is still a string at runtime whatever it interpolates,
                    // so the phantom carries one.
                    Expect.stringContains
                        source
                        "type Prefixed<'T> = private Prefixed__ of string"
                        "a template literal over an open operand"

                    Expect.stringContains source "[<Erase>]" "and each is erased"

                    // The concrete siblings are unaffected: the checker finished those, so they
                    // are ordinary declarations rather than phantoms.
                    Expect.stringContains source "type ConcreteBranch = string" "a resolved conditional"
                    Expect.isFalse (source.Contains "EventName__") "a resolved template literal is a StringEnum"

                    for name in [ "DeepPartial"; "Flags"; "Unwrap"; "Prefixed" ] do
                        Expect.isTrue
                            (rendered.Findings
                             |> List.exists (fun f ->
                                 f.Symbol = name && f.Tier = Widened && f.Message.Contains "erased phantom"))
                            $"{name} says in the manifest that it is a phantom" ])

        yield!
            fixtureTests "lib-lab" (handFixture "lib-lab") GeneratorConfig.Default (fun package ->
                [ testCase "the compiler-lib names a shipped Fable package binds are referenced, not widened" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // O7's compiler-lib group widened to obj for want of a shipped binding.
                      // For the ECMAScript half of the lib there is one, and every generated
                      // file already opens it.
                      Expect.stringContains source "static member fetchOne (url: string) : JS.Promise<string>" "a promise is a promise"
                      Expect.stringContains source "JS.Map<string, string[]>" "both parameters carried"
                      Expect.stringContains source "at: JS.Date) : JS.Date" "and the non-generic names too"

                      // The argument is shaped at its own position, which is the half the old
                      // wholesale widening cost most: `Promise<T>` used to erase T with it.
                      Expect.stringContains source "abstract load: key: string -> JS.Promise<JS.Uint8Array>" "nested through a member"
                      Expect.stringContains source "abstract boxed: Box<JS.Date>" "and through a generic this run declares"

                      // The DOM half, from the `Fable.Browser.*` family. The table is generated,
                      // so these pin the rule that reads it: an ordinary position, the packages
                      // of the family other than `Dom`, and a name bound at two arities.
                      Expect.stringContains
                          source
                          "static member handle (target: Browser.Types.EventTarget)"
                          "a DOM name is written, not widened"

                      Expect.stringContains
                          source
                          "mount (host: Browser.Types.HTMLElement, on: Browser.Types.Event)"
                          "elements and events together"

                      Expect.stringContains
                          source
                          "upload (body: Browser.Types.Blob, ``to``: Browser.Types.URL) : Browser.Types.FormData"
                          "across three packages of the family"

                      Expect.stringContains
                          source
                          "emit (detail: Browser.Types.CustomEvent<string>)"
                          "and the generic arity wins where the reference has an argument"

                      // Names nothing shipped binds keep widening. `seq<'T>` is not a JS
                      // iterable; `Range` is in two packages of the family at once and no
                      // qualification picks one; `Response` belongs to `Fable.Fetch`.
                      Expect.stringContains source "static member each (values: obj)" "the sync iteration protocol is unbound"
                      Expect.stringContains source "static member ``select`` (over: obj)" "an ambiguous DOM name is not guessed at"
                      Expect.stringContains source "static member respond () : obj" "and fetch is a different family"

                      // Every loss is in the manifest: the arity the lib drifted away from, and
                      // the restrictions the readonly views express and F# has no binding for.
                      let says fragment =
                          rendered.Findings |> List.exists (fun f -> f.Message.Contains(fragment: string))

                      Expect.isTrue (says "Uint8Array carries 1 type arguments where JS.Uint8Array takes 0") "the dropped buffer parameter"
                      Expect.isTrue (says "PromiseLike reads as JS.Promise") "a thenable is not a promise"
                      Expect.isTrue (says "ReadonlyMap reads as JS.Map") "and readonly is not carried" ])


        yield!
            fixtureTests "group-map-lab" groupMapLab (handConfig groupMapLab) (fun package ->
                [ testCase "a mapped group is redirected to the bindings its table names" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate (handConfig groupMapLab) package)
                      let source = rendered.Files |> List.head |> snd

                      // O7's `map`: the group resolves to identity only, and each name the
                      // table carries is written as the destination somebody already bound.
                      Expect.stringContains
                          source
                          "static member compile (pattern: System.Text.RegularExpressions.Regex) : System.Text.RegularExpressions.Regex"
                          "a destination taking no type arguments"

                      Expect.stringContains
                          source
                          "static member hold (handle: Handle) : System.WeakReference<Handle>"
                          "a generic destination at the arity its table states"

                      // The destination composes wherever an ordinary reference does.
                      Expect.stringContains source "abstract patterns: System.Text.RegularExpressions.Regex[]" "under an array"
                      Expect.stringContains source "abstract held: System.WeakReference<Handle> option" "under an option"
                      Expect.stringContains source "abstract boxed: Box<System.Text.RegularExpressions.Regex>" "and through a generic this run declares"

                  testCase "a mapped group carries only the names its table carries" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate (handConfig groupMapLab) package)
                      let source = rendered.Files |> List.head |> snd

                      let says key fragment =
                          rendered.Findings
                          |> List.exists (fun finding ->
                              finding.Key = (key: string) && finding.Message.Contains(fragment: string))

                      // A name outside the table keeps the widening the group had, so mapping
                      // is per name rather than per group.
                      Expect.stringContains source "static member respond () : obj" "an unmapped name widens"
                      Expect.isTrue (says "TR023" "Response is not among the generated declarations") "and says so"

                      // The arity rule: the destination takes one argument and the site applies
                      // three, so the application is not written at all.
                      Expect.stringContains source "static member walk (over: obj) : unit" "an arity the destination does not take"
                      Expect.isTrue (says "TR053" "Iterator is applied to 3 type arguments") "reported with the arity applied"

                  testCase "the pinned Fable tables answer ahead of a mapped group's own" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate (handConfig groupMapLab) package)
                      let source = rendered.Files |> List.head |> snd

                      // Mapping the compiler lib extends `Naming.LibBindings` and
                      // `Naming.BrowserBindings` by name; it does not replace either.
                      Expect.stringContains source "static member fetchOne (url: string) : JS.Promise<string>" "the ECMAScript table"
                      Expect.stringContains source "static member handle (target: Browser.Types.EventTarget) : unit" "and the DOM table" ])


        yield!
            fixtureTests "brand-lab" (handFixture "brand-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a branding intersection becomes a measure its uses carry" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // §4.6/D11: a brand is a compile-time-only nominal distinction over a
                      // shared runtime representation, which is what a unit of measure is - and
                      // Fable erases both, so the JavaScript sees the primitive either way.
                      Expect.stringContains source "[<Measure>]\ntype UserId" "the brand declares a measure"
                      Expect.stringContains source "abstract get: id: string<UserId> -> string" "and its uses carry it"

                      // Numbers take a measure natively; the other primitives go through the
                      // support package's abbreviations, which is why the open is emitted.
                      Expect.stringContains source "at: float<Millis>" "a numeric brand needs no support"
                      Expect.stringContains source "open Xantham.Fable.Core" "the abbreviations are in scope"

                      // Under an array, under an option, and on a bare exported function.
                      Expect.stringContains source "abstract ids: unit -> string<UserId>[]" "a brand under an array"
                      Expect.stringContains source "?id: string<SessionId> -> string<UserId> option" "and under an option"
                      Expect.stringContains source "static member mint (seed: string) : string<UserId>" "and on an export"

                      // `boolean & Marker` and `(\"read\" | \"write\") & Marker` are handed back
                      // distributed, as a union of intersections. Each is still one brand.
                      Expect.stringContains source "type Verified" "a branded boolean survives distribution"
                      Expect.stringContains source "type Mode" "so does a branded literal union"

                      // The negatives. None of these is a brand and none may be read as one.
                      // `Merged` and `Wrapped` are object intersections, so they flatten into
                      // interfaces (§4.6) rather than measures; `Counted` intersects a primitive
                      // with a readable member, which is neither a brand nor a shape.
                      for name in [ "Merged"; "Wrapped" ] do
                          Expect.isTrue
                              (rendered.Findings
                               |> List.exists (fun f ->
                                   f.Symbol = name && f.Tier = Ergonomic && f.Message.Contains "flattened into one interface"))
                              $"{name} is an ordinary intersection, and flattens"

                      Expect.isTrue
                          (rendered.Findings
                           |> List.exists (fun f ->
                               f.Symbol = "Counted" && f.Tier = Widened && f.Message.Contains "non-object operand"))
                          "Counted is neither a brand nor a shape, and says so"

                      Expect.isFalse (source.Contains "Merged>") "and none of them is written as a measure" ])


        yield!
            fixtureTests "flags-lab" (handFixture "flags-lab") GeneratorConfig.Default (fun package ->
                [ testCase "the type flags the tier used to refuse map, and say what each cost" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      let says key fragment =
                          rendered.Findings
                          |> List.exists (fun f -> f.Key = key && f.Message.Contains(fragment: string))

                      // A template literal is a string at runtime, so `string` is the read
                      // (§4.11) - in an alias, on a member, and at a parameter and a return.
                      Expect.stringContains source "type EventName = string" "the alias position"
                      Expect.stringContains source "abstract channel: string" "written inline on a member"
                      Expect.stringContains source "abstract resolve: scope: string -> string" "parameter and return"

                      Expect.stringContains
                          source
                          "static member normalize (name: string) : string"
                          "and on an exported function"

                      Expect.isTrue (says "TR037" "pattern is not carried") "each site says what was lost"
                      Expect.isTrue (says "TR038" "transform it applies") "and an intrinsic mapping says its own"

                      // The negatives. A closed template literal is expanded by the checker
                      // into its literal union and stays exact; one over a type parameter
                      // stays the phantom, which keeps the arity `string` would have lost.
                      Expect.stringContains source "type ModeEvent =" "a closed template literal is a StringEnum"
                      Expect.stringContains source "[<CompiledName(\"onRead\")>] OnRead" "expanded, not widened"
                      Expect.stringContains source "type Tagged<'T> = private Tagged__ of string" "open operand: phantom"

                      // bigint is exact, and an exact mapping is reported nowhere at all.
                      Expect.stringContains source "abstract balance: bigint" "a bigint member"
                      Expect.stringContains source "static member total (amounts: bigint[]) : bigint" "and an export"

                      // `synthesize-paramobjects` reports `Ledger` for its method member, which
                      // is construction ergonomics rather than anything bigint cost.
                      Expect.isEmpty
                          (rendered.Findings
                           |> List.filter (fun f ->
                               f.Symbol.StartsWith "Ledger" && f.Pass <> "synthesize-paramobjects"))
                          "nothing is lost mapping bigint, so nothing is reported"

                      Expect.stringContains source "type Two = bigint" "a bigint literal keeps its own widening"
                      Expect.isTrue (says "TR039" "bigint literal") "reported as the literal it was"

                      // `object`, `symbol` and `unique symbol` still widen - nothing shipped
                      // binds a symbol - but each names its construct instead of a flag.
                      Expect.stringContains source "abstract holder: obj" "object is obj"
                      Expect.stringContains source "static member describe (key: obj) : string" "so is symbol"
                      Expect.isTrue (says "TR040" "admits the primitives object excludes") "object's widening"
                      Expect.isTrue (says "TR041" "no binding in Fable.Core") "symbol's"
                      Expect.isTrue (says "TR042" "no F# form for its identity") "and unique symbol's"

                      // The point of the work: no site in this fixture answers with a flag name.
                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Key = "TR014"))
                          "no construct here is left reported as an unmapped flag" ])

        yield!
            fixtureTests "parse-lab" (handFixture "parse-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a member head ending in a generic constraint keeps the colon a separate token" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `>>:` lexes as one token, so the member's colon is swallowed and the file
                    // fails to parse (FS0010) - taking the whole compile gate with it, before any
                    // other diagnostic is reported. One space is the entire fix.
                    Expect.stringContains
                        source
                        "abstract intersectObject<'TIntersected when 'TIntersected :> Object3D<EventMap>> :"
                        "a constraint that is a generic application ends the head in >>"

                    Expect.stringContains
                        source
                        "and 'TSecond :> Object3D<EventMap>> :"
                        "two constraints join with and, and still end the head in >>"

                    Expect.isFalse (source.Contains ">>:") "no head runs into its colon"

                  testCase "a head ending in a single > keeps the colon tight" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd

                    // `>:` lexes correctly, so the space is spent only where it is needed and no
                    // existing golden moves.
                    Expect.stringContains source "abstract echo<'T>:" "a parameter with no constraint"
                    Expect.stringContains source "abstract on<'T when 'T :> EventMap>:" "a constraint that is a bare name" ])

        // Wave two lane A's fixture. `docs/plans/generator-three-rung.md` §9 blocker 1: a
        // polymorphic-`this` method returning an intersection *containing* `this` mints a
        // strictly larger anonymous type per application, and the shaper hoisted each one to a
        // `<Member>Result` declaration whose own `toVar` minted another - 518 declarations and
        // 369,116 lines on the `three` rung, stopped only by the depth cutoff. These 11
        // declarative lines reproduce it; what follows says what bounded output looks like.
        yield!
            fixtureTests "chain-lab" (handFixture "chain-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a this-returning intersection is written as an application, not hoisted again" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // The bound itself: the source declares four types and one export
                      // container, and generation adds none. An unbounded run mints one
                      // declaration per application instead, so this count is the regression
                      // test - not the line count, which moves with unrelated rendering.
                      Expect.equal
                          (rendered.Decls |> List.length)
                          6
                          "five declared types and the export container, and nothing minted"

                      Expect.equal
                          (rendered.Decls
                           |> List.choose (fun decl ->
                               match decl with
                               | FsInterface iface -> Some iface.Name
                               | _ -> None))
                          [ "NodeExtensions"; "Node"; "VarNodeInterface"; "VarNode"; "Plain" ]
                          "the source's own names, once each, in declaration order"

                      // The mint was named by appending the member name plus `Result`. No such
                      // name may exist: every one of them was an application of a type the run
                      // already declares.
                      Expect.isFalse (source.Contains "Result") "no <Member>Result declaration is minted"

                  testCase "each hoist site reads back as the declaration it instantiates" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // `this` in `VarNode<TNodeType, this>` is the declaration the member is
                      // written on, so each of the three sites applies a different argument and
                      // none of them is a new type.
                      Expect.stringContains
                          source
                          "abstract toVar: Func<string option, VarNode<'TNodeType, NodeExtensions<'TNodeType>>> with get, set"
                          "the interface `this` is written on"

                      Expect.stringContains
                          source
                          "abstract toVar: Func<string option, VarNode<'TNodeType, Node<'TNodeType>>> with get, set"
                          "the alias that intersects it, applied over its own parameter"

                      Expect.stringContains
                          source
                          "abstract toVar: Func<string option, VarNode<'TNodeType, VarNode<'TNodeType, 'TNode>>> with get, set"
                          "and the alias that intersects that one, applied over both of its parameters"

                      // `const seed: Node<number>` resolves to the same erased intersection as
                      // the alias, so the export is the fourth site.
                      Expect.stringContains source "static member seed: Node<float> = jsNative" "the export site"

                  testCase "the shaper reports the hash-consing, and nothing widens for want of it" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      let keyed key =
                          rendered.Findings |> List.filter (fun f -> f.Key = key)

                      Expect.equal
                          (keyed "SY001" |> List.map _.Symbol)
                          [ "NodeExtensionsToVarResult"; "NodeToVarResult"; "VarNodeToVarResult"; "Seed" ]
                          "one SY001 per site, named for the declaration that would have been minted"

                      // The fallback exists for an application whose arguments cannot be
                      // recovered from the operands. Recovery works here, so it stays silent.
                      Expect.isEmpty (keyed "SY002") "no hoist is refused outright"

                      // A self-reference that came back out bare would be widened by
                      // `repair-arity` instead of applied, which is the failure this rung is
                      // most likely to regress into.
                      Expect.isEmpty (keyed "RA003") "no generic is left in a position with no arguments"
                      Expect.isFalse (source.Contains ": obj") "nothing on the chain widens"

                  testCase "a shape that closes no cycle keeps its own name" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // The negatives. `wrap` names an already-declared generic and `self`
                      // returns bare `this`; neither mints a larger type, so neither may be
                      // rewritten by the recognition that fixes the chain.
                      Expect.stringContains
                          source
                          "abstract wrap: value: 'TValue -> VarNodeInterface<'TValue>"
                          "an application of a declared generic is written as itself"

                      Expect.stringContains
                          source
                          "abstract self: unit -> Plain<'TValue>"
                          "bare polymorphic `this` still reads as its own declaration" ])

        // Wave three lane J's fixture. `docs/plans/generator-three-rung.md` §11.4: the same
        // chain as `chain-lab` with one operand of the alias body replaced by a conditional
        // deferred on the alias's own parameter, paired against a control differing in that
        // operand alone. `three`'s `Node<TNodeType>` is written this way, and the runaway it
        // produced held at 518 declarations and 76.5% of the rendered file while `chain-lab`
        // passed.
        yield!
            fixtureTests "hoist-conditional-lab" (handFixture "hoist-conditional-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a conditional operand in the alias body bounds the chain, as the control does" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // The size assertion, and the whole point of the lab: eight declared
                      // types and one export container, whatever the two halves render as. An
                      // unbounded run mints one declaration per application until the depth
                      // cutoff stops it - thirteen for the reproducer's five declarations, and
                      // 518 on `three`.
                      Expect.equal (rendered.Decls |> List.length) 9 "the source's own eight types and the export container"

                      // A mint is named by appending the member name plus `Result`. Neither
                      // half may produce one: the control writes applications, the reproducer
                      // widens.
                      Expect.isFalse (source.Contains "Result") "no <Member>Result declaration minted"

                  testCase "the conditional operand is the whole difference between the halves" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      let keyed key =
                          rendered.Findings |> List.filter (fun finding -> finding.Key = key)

                      // The control recognises every application of its two aliases and writes
                      // each as one. The reproducer recognises `CondNode` too and stops there:
                      // `TNodeType` appears in the conditional operand alone, and a deferred
                      // conditional carries neither branch nor argument, so the application has
                      // nothing to be written with and widens instead.
                      Expect.equal
                          (keyed "SY001" |> List.map _.Symbol)
                          [ "DirectExtensionsToVarResult"
                            "DirectNodeToVarResult"
                            "DirectVarNodeToVarResult"
                            "DirectSeed" ]
                          "the control writes an application at every site"

                      Expect.equal (keyed "SY002" |> List.map _.Symbol) [ "CondSeed" ] "the reproducer widens one site"

                  testCase "the control's chain is written as applications" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains
                          source
                          "abstract toVar: Func<string option, DirectVarNode<'TNodeType, DirectNode<'TNodeType>>> with get, set"
                          "the alias intersects its own operand, applied over its parameter"

                      Expect.stringContains
                          source
                          "static member directSeed: DirectNode<float> = jsNative"
                          "and the export is a fourth site" ])

        // Wave two lane C's fixture. `docs/plans/generator-three-rung.md` §9 blocker 3: a
        // structural `extends` constraint whose argument satisfies it structurally but not
        // nominally. TypeScript accepts `Geometry<Narrow>` where `Narrow extends Wide`
        // structurally; F#'s `:>` is nominal, so the rendered head is 328 FS0001 on the `three`
        // rung. These lines reproduce it, and carry the two negatives a fix must not move.
        yield!
            fixtureTests "nominal-lab" (handFixture "nominal-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a constraint no nominal relation supports is dropped from the head" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // `Narrow` and `Wide` are two `[<EmitIndexer>]` interfaces with no
                      // `inherit` between them, so `'Attributes :> Wide` would reject the
                      // declaration's own default argument. The parameter stays free instead.
                      Expect.stringContains source "type Geometry<'Attributes> =" "the head carries no bound"
                      Expect.isFalse (source.Contains "'Attributes :>") "and states no nominal relation"

                      Expect.isTrue
                          (rendered.Findings
                           |> List.exists (fun f ->
                               f.Key = "TP008" && f.Message.Contains "Wide" && f.Message.Contains "Attributes"))
                          "the drop is recorded against the parameter and its bound"

                  testCase "a constraint the run can prove keeps its `:>`" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // The negatives. `exact<T extends Base>` is only ever applied to `Base`
                      // itself, and `Derived` `inherit`s `Base` (SI005), so both relations hold
                      // nominally and neither may be dropped by the fix.
                      Expect.stringContains source "exact<'T when 'T :> Base>" "a bound the argument is"
                      Expect.isTrue
                          (inheritsOf source "Derived" |> List.contains "Base")
                          "and one the argument inherits"

                  testCase "the use site keeps the default argument the declaration states" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // With no bound on the head there is nothing to widen the argument to, so
                      // `g` reads as the type TypeScript resolved it to rather than as `Wide`.
                      Expect.stringContains source "static member g: Geometry<Narrow>" "the default, written out" ])

        // Wave two lane E's fixture. `T extends U ? X : Y` reaches the shaper deferred: the
        // whole of the old `TR014` was this construct. Two of the three shapes name a branch
        // and the third stays `obj`; the lines below pin one of each, plus the negatives.
        yield!
            fixtureTests "conditional-lab" (handFixture "conditional-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a condition the parameter's bound decides reads as its branch" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // `Tagged` inherits `Marker`, so `T extends Marker` holds for every
                      // argument the head admits and the true branch is the whole mapping.
                      Expect.stringContains source "type Proven<'T when 'T :> Tagged> = 'T" "the true branch"

                      Expect.isTrue
                          (rendered.Findings
                           |> List.exists (fun f ->
                               f.Key = "TR046" && f.Symbol = "Proven" && f.Message.Contains "true"))
                          "recorded as resolved to its true branch"

                  testCase "a pair with an uninhabited branch reads as the other one" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // `T extends () => unknown ? never : T`: nothing is ever the `never`
                      // branch, so the alias is its parameter.
                      Expect.stringContains source "type Inhabited<'T> = 'T" "the inhabited branch"

                      Expect.isTrue
                          (rendered.Findings
                           |> List.exists (fun f -> f.Key = "TR046" && f.Symbol = "Inhabited"))
                          "recorded against the alias"

                  testCase "a pair the run cannot decide stays obj and says so" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      let deferred =
                          rendered.Findings
                          |> List.filter (fun f -> f.Key = "TR045")
                          |> List.map _.Symbol
                          |> List.distinct
                          |> List.sort

                      // `Divergent` has two inhabited branches with no shared F# form, and
                      // `OrUndefined`'s second branch is `undefined`, which an application does
                      // land in - `never` is the only branch this drops.
                      Expect.equal
                          deferred
                          [ "Divergent"; "OrUndefined"; "divergent(value)" ]
                          "both divergent pairs and the use site of one"

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Key = "TR014"))
                          "and no conditional is reported as an unmapped flag"

                  testCase "the negatives keep their own shape" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // A condition over no type variable is answered by the checker, so it
                      // arrives already a branch and carries no conditional finding at all.
                      Expect.stringContains source "type Decided = float" "the checker's own answer"

                      Expect.isFalse
                          (rendered.Findings |> List.exists (fun f -> f.Symbol = "Decided"))
                          "and nothing is recorded against it"

                      // A generic alias with no condition in it.
                      Expect.stringContains source "type Box<'T> =" "an ordinary generic alias" ])

        // Wave three lane H's fixture. An array reaches the shaper under whatever name the
        // author put on it, and `Array` is only one of them. The three spellings below each
        // collapse to an F# array; the fourth is indexable by number and is not an array.
        yield!
            fixtureTests "array-shape-lab" (handFixture "array-shape-lab") GeneratorConfig.Default (fun package ->
                [ testCase "an interface extending Array is its element array" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Chapters = string[]" "the element, not a page of obj"

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Symbol.StartsWith "Chapters."))
                          "and Array's own members are never walked"

                  testCase "an array intersected with a shape reports the members it drops" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Tagged = float[]" "the element array"

                      let dropped =
                          rendered.Findings
                          |> List.filter (fun f -> f.Key = "TR048" && f.Symbol = "Tagged")

                      Expect.equal dropped.Length 1 "one drop reported against the declaration"
                      Expect.stringContains dropped.Head.Message "1 member" "`kind`, and only `kind`"

                  testCase "a mapped type over a deferred tuple is an array" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // The expansion leaves `Element` out of scope, so the element widens; the
                      // array around it does not.
                      Expect.stringContains source "type ReadonlyTuple = obj[]" "an array of a widened element"

                  testCase "an indexable shape with none of Array's members stays a shape" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Register =" "declared as an interface"
                      Expect.stringContains source "abstract Item: float -> string" "with its indexer intact"

                  testCase "no array member is reported as a missing declaration" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Key = "TR023"))
                          "the whole of TR023 here was Array's member set" ])

        yield!
            fixtureTests "setter-lab" (handFixture "setter-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a name every call signature declares is one variable on the head" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Setter<'T, 'U> =" "one 'U, not one per signature"

                      let collapsed = rendered.Findings |> List.filter (fun f -> f.Key = "TP009")

                      Expect.equal (collapsed |> List.map _.Symbol) [ "Setter" ] "reported once, on the alias"
                      Expect.stringContains collapsed.Head.Message "3 signatures" "and says how many declared it"

                  testCase "signatures declaring different names keep a slot each" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type Distinct<'T, 'A, 'B> =" "'A and 'B are two variables"
                      Expect.stringContains source "type Single<'T, 'U> =" "and one signature collapses nothing"

                  testCase "one name under two bounds is refused rather than retyped" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.isFalse (source.Contains "type DivergentBound") "the head F# refuses does not render"

                      Expect.equal
                          (rendered.Findings
                           |> List.filter (fun f -> f.Key = "RA001")
                           |> List.map _.Symbol)
                          [ "DivergentBound" ]
                          "and the drop is graded as an escape, not an ergonomic collapse"

                  testCase "a tuple-typed rest parameter reads as the parameters it stands for" <| fun _ ->
                      // Wave two's second handback: `Setter<string | undefined>` reached the
                      // empty tuple and rendered `Action<obj[]>`.
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "abstract optional: Action with" "no parameter at all"
                      Expect.stringContains source "abstract setter: Func<obj, obj> with" "and one for a one-element tuple"

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Key = "TR029"))
                          "neither arity is a tuple the renderer has to widen" ])

        yield!
            fixtureTests "constraint-arg-lab" (handFixture "constraint-arg-lab") GeneratorConfig.Default (fun package ->
                [ testCase "an argument F# seals is written as the constraint it cannot inherit" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type HeldString = Holder<Lengthy>" "a primitive"
                      Expect.stringContains source "type HeldTuple = Holder<Lengthy>" "a tuple"
                      Expect.stringContains source "type HeldArray = Holder<Lengthy>" "an array"

                      Expect.equal
                          (rendered.Findings |> List.filter (fun f -> f.Key = "TR044") |> List.length)
                          6
                          "each of the three, in alias and in member position"

                  testCase "a named subtype of the constraint is applied as itself" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type HeldSized = Holder<Sized>" "the negative stands" ])
        // Wave three lane L's fixture. A compiler-lib type reached structurally hands over
        // member symbols; their types carry a member's name, so they resolve by content. A lib
        // declaration named at a reference position keeps the O7 shortcut.
        yield!
            fixtureTests "member-shape-lab" (handFixture "member-shape-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a lib method reached through an intersection renders as a delegate" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "abstract catch" "the member survives"
                      Expect.stringContains source "Func<" "carrying its call signature"

                      Expect.isEmpty
                          (rendered.Findings
                           |> List.filter (fun f -> f.Key = "TR023" && f.Symbol.StartsWith "Deferred"))
                          "and no member of it is reported as a missing declaration"

                  testCase "a lib method reached by heritage resolves the same way" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "abstract dispatchEvent" "the member survives"
                      Expect.stringContains source "Browser.Types.Event" "with its parameter type bound"

                      // What is left under TR023 names a type this run does not declare. A
                      // member's name arriving there is the defect this lab pins.
                      Expect.isEmpty
                          (rendered.Findings
                           |> List.filter (fun f -> f.Key = "TR023" && Char.IsLower f.Message[0]))
                          "no finding in the lab names a member"

                  testCase "a bound lib declaration at a reference position stays identity only" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "abstract at: JS.Date" "Date binds, and binds whole"
                      Expect.isFalse (source.Contains "abstract getTime") "with none of its members walked"

                  testCase "an unbound lib declaration widens under its own name" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      let named =
                          rendered.Findings
                          |> List.filter (fun f -> f.Key = "TR023" && f.Symbol = "Located.matrix")

                      Expect.equal named.Length 1 "one finding, against the property"
                      Expect.stringContains named.Head.Message "DOMMatrix" "naming the declaration, not a member" ])

        yield! fixtureTests "animejs" (npmFixture "animejs") GeneratorConfig.Default (fun _ -> [])

        // workers-types is a global type library that *replaces* the DOM lib: its README
        // prescribes `"lib": ["esnext"]`, and with the DOM loaded every name it shares with
        // `lib.dom.d.ts` (`Headers`, `Response`, `Event`, `Crypto` - most of its 154 classes)
        // merges with the lib's declaration, groups as the compiler lib by its first
        // declaration, and never reaches the harvest. The rung runs the way a consumer would.
        let workersTypesConfig =
            { GeneratorConfig.Default with
                Lib = Some [ "esnext" ] }

        yield!
            fixtureTests
                "@cloudflare/workers-types"
                (npmFixture "@cloudflare/workers-types")
                workersTypesConfig
                (fun package ->
                    [ testCase "a package that declares no module is harvested from global scope" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate workersTypesConfig package)
                          let source = rendered.Files |> List.head |> snd

                          // The whole point of the rung: workers-types has no module symbol, so
                          // every name here comes from `harvest-globals`, and a value it declares
                          // is already on `globalThis` rather than importable.
                          Expect.stringContains source "[<Global(\"Cloudflare\")>]" "a declared global binds with Global"
                          Expect.isFalse (source.Contains "[<Import(") "a global library imports nothing"

                      testCase "a class that shares a DOM name is the package's own to harvest" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate workersTypesConfig package)
                          let source = rendered.Files |> List.head |> snd

                          for name in [ "Headers"; "Response"; "Event"; "Crypto" ] do
                              Expect.isTrue
                                  (source.Contains($"\ntype {name} =") || source.Contains($"\nand {name} ="))
                                  $"{name} is declared by the package, not merged away into the DOM lib"

                      testCase "no declaration of workers-types is dropped without saying why" <| fun _ ->
                          // This rung cannot claim zero escapes - ambient module declarations and
                          // generic aliases whose parameters all widened away do get dropped.
                          let rendered = Async.RunSynchronously(Pipeline.generate workersTypesConfig package)

                          Expect.isEmpty
                              (unexplainedDrops rendered)
                              "every export missing from the output is the subject of an explaining finding" ])

        // D9's calibration rungs: utility-type depth (`type-fest`) and a reactive UI library
        // whose surface is mostly generic callbacks (`solid-js`). Neither claims zero escapes;
        // what each pins is that the pipeline survives the package and owns every drop.
        yield!
            fixtureTests "solid-js" (npmFixture "solid-js") GeneratorConfig.Default (fun package ->
                [ testCase "no declaration of solid-js is dropped without saying why" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      Expect.isEmpty (unexplainedDrops rendered) "every drop is owned by a pass that named its reason" ])

        yield!
            fixtureTests "type-fest" (npmFixture "type-fest") GeneratorConfig.Default (fun package ->
                [ testCase "a type the compiler cannot encode is an owned escape, not a crash" <| fun _ ->
                      // `PositiveInfinity = 1e999` is `+Inf` to the server's JSON encoder, which
                      // refuses the response outright. The resolve tier turns that refusal into a
                      // finding on the export and carries on with the other 247.
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      for name in [ "PositiveInfinity"; "NegativeInfinity" ] do
                          Expect.isTrue
                              (rendered.Findings
                               |> List.exists (fun f ->
                                   f.Symbol = name
                                   && f.Pass = "resolve-export-types"
                                   && f.Tier = Escape
                                   && f.Message.Contains "Inf"))
                              $"{name} is escaped by the resolve tier with the encoder's complaint"

                      Expect.isEmpty (unexplainedDrops rendered) "every drop is owned by a pass that named its reason"

                  testCase "typeof globalThis is widened rather than declared as the whole global scope" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.isFalse (source.Contains "abstract setTimeout") "the global scope's members are not inlined"

                      Expect.isTrue
                          (rendered.Findings
                           |> List.exists (fun f -> f.Symbol = "GlobalThis" && f.Message.Contains "globalThis"))
                          "and the widening names the global scope" ])

        // Wave two lane D (recon blocker 5). `@types/*` packages ship no JavaScript, so an
        // import naming one resolves to nothing at run time - the reason `@types/three` rendered
        // 737 unusable imports. The fixture is published as `@types/types-only-lab`; everything
        // it binds must name `types-only-lab`.
        yield!
            fixtureTests "types-only-lab" (handFixture "types-only-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a types-only package binds its imports to the runtime package" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      // Two call sites write the specifier - `Exports` members and a class
                      // static - so both are asserted rather than one standing for the other.
                      Expect.stringContains source "[<Import(\"greet\", \"types-only-lab\")>]" "a function export"
                      Expect.stringContains source "[<Import(\"version\", \"types-only-lab\")>]" "a value export"
                      Expect.stringContains source "[<Import(\"Counter.MAX\", \"types-only-lab\")>]" "a class static"
                      Expect.isFalse (source.Contains "\"@types/types-only-lab\"") "and nothing names the types package"

                      // Provenance is not the specifier: the header still says which package
                      // the declarations were read out of.
                      Expect.stringContains
                          source
                          "Generated by Xantham.Generator from @types/types-only-lab"
                          "the header records where the types came from"

                  testCase "the derivation is reported once for the run, not once per import" <| fun _ ->
                      // `@types/three` renders 737 imports at one specifier; a per-import
                      // finding would be the same sentence 737 times.
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                      let derived = rendered.Findings |> List.filter (fun f -> f.Key = "SE002")

                      Expect.equal
                          (derived |> List.map (fun f -> f.Pass, f.Symbol, f.Tier))
                          [ "shape-exports", "<module>", Ergonomic ]
                          "one run-level finding, owned by the pass whose prefix SE is"

                      Expect.stringContains
                          (List.head derived).Message
                          "types-only-lab"
                          "and it names the specifier it derived"

                  testCase "a configured runtime package overrides the derivation and reports nothing" <| fun _ ->
                      let config =
                          { GeneratorConfig.Default with
                              RuntimePackage = Some "some-other-runtime" }

                      let rendered = Async.RunSynchronously(Pipeline.generate config package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "[<Import(\"greet\", \"some-other-runtime\")>]" "config decides"
                      Expect.isFalse (source.Contains "\"types-only-lab\"") "the derivation does not survive it"

                      Expect.isEmpty
                          (rendered.Findings |> List.filter (fun f -> f.Key = "SE002"))
                          "a configured runtime was not derived, so there is nothing to report" ])
        // The fixture behind docs/fable5-workarounds.md. Each declaration is one documented
        // Fable 5 loss; the assertions pin the emitted shape the document quotes, and the run
        // gate proves the workaround against tests/fixtures/fable-workaround-lab/index.js.
        yield!
            fixtureTests
                "fable-workaround-lab"
                (handFixture "fable-workaround-lab")
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "the shapes the workaround document quotes are the shapes emitted" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains
                              source
                              "type Outcome = U2<Err, Ok>"
                              "an undiscriminated union of two object arms"

                          Expect.equal (inheritsOf source "Circle") [ "Shape" ] "the is-a relation a downcast would want"

                          Expect.stringContains
                              source
                              "[<Import(\"Budget\", \"fable-workaround-lab\")>]\ntype Budget ="
                              "a settable static, bound through the declaration"

                          Expect.stringContains
                              source
                              "    static member limit\n        with get (): float = jsNative\n        and set (_: float): unit = jsNative"
                              "and emitted with a setter"

                          Expect.stringContains source "abstract value: string option" "string | null hoisted to option"

                          Expect.stringContains
                              source
                              "abstract notify: count: float -> string"
                              "a member the consumer has to supply" ])

        // Wave four lane O (docs/fable5-workarounds.md §3). A method reads as a delegate-typed
        // Create parameter; the four negatives keep getting no Create, each with its reason.
        yield!
            fixtureTests
                "paramobject-method-lab"
                (handFixture "paramobject-method-lab")
                GeneratorConfig.Default
                (fun package ->
                    [ testCase "a method member becomes a delegate-typed Create parameter" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains
                              source
                              "static member Create (name: string, notify: Func<float, string>, reset: Action, ?tag: string)"
                              "the methods bind delegates, required ahead of the optional property"

                          let carried =
                              rendered.Findings
                              |> List.filter (fun f -> f.Key = "SP002")
                              |> List.map _.Symbol

                          Expect.equal carried [ "Listener.notify"; "Listener.reset" ] "one finding per method carried in"

                      testCase "the shapes that still get no Create each report their reason" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                          let refused =
                              rendered.Findings
                              |> List.filter (fun f -> f.Key = "SP003")
                              |> List.map (fun f -> f.Symbol, f.Message)

                          let reasonFor name =
                              refused
                              |> List.tryPick (fun (symbol, message) -> if symbol = name then Some message else None)
                              |> Option.defaultValue "<none>"

                          Expect.stringContains (reasonFor "Bag") "index signature" "an indexed type has no name to bind"
                          Expect.stringContains (reasonFor "Formatter") "overload" "two parameters would share a name"
                          Expect.stringContains (reasonFor "Wide") "budget" "twenty-five members is one too many"

                          Expect.isFalse
                              (refused |> List.exists (fun (symbol, _) -> symbol = "Listener"))
                              "and the interface that gained one reports nothing"

                      testCase "a constructor object keeps the Create its construct signature gave it" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd

                          Expect.stringContains source "EmitConstructor" "the construct signature still renders"

                          Expect.isEmpty
                              (rendered.Findings
                               |> List.filter (fun f -> f.Key = "SP003" && f.Symbol.StartsWith "Handle"))
                              "a declaration that already has Create members is not reported as missing one" ])

        // Wave five lanes T and U (O7). `cross-package-lab` is two packages under one
        // `node_modules`. The dependency half is registered as its own entry package and ships;
        // the entry half carries an `xantham.json` configuring the dependency `reference`, so
        // its golden templates names into `CrossPackageDep` and the compile gate compiles the
        // two together. The contract those templated names have to satisfy is in
        // `MultiPackage.test.fs`.
        yield!
            fixtureTests
                "cross-package-dep"
                (handFixture "cross-package-lab/node_modules/cross-package-dep")
                GeneratorConfig.Default
                (fun _ -> [])

        yield!
            fixtureTests
                "cross-package-lab"
                (handFixture "cross-package-lab/node_modules/cross-package-lab")
                (handConfig (handFixture "cross-package-lab/node_modules/cross-package-lab"))
                (fun package ->
                    [ testCase "a dependency left unconfigured widens rather than being resolved" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

                          Expect.equal
                              (rendered.Findings
                               |> List.filter (fun finding -> finding.Message.Contains "not among the generated")
                               |> List.map _.Symbol)
                              [ "Panel.widget"; "Panel.boxed"; "Panel.pair"; "mount(widget)"; "mount()" ]
                              "every reference into the dependency is a widening with a name" ])

        // Wave five lane S (O7's `ship` disposition). Two dependencies are installed beside the
        // entry package and both configured `ship`. `dep-lab` is emitted as its own module and
        // the entry names its type across the boundary; `dep_lab` templates the same module
        // name, so it keeps its declarations in the entry module and says so.
        yield!
            fixtureTests
                "multi-ship-lab"
                (handInstalledFixture "multi-ship-lab")
                (handConfig (handInstalledFixture "multi-ship-lab"))
                (fun package ->
                    let config = handConfig (Some package)

                    let generate () =
                        Async.RunSynchronously(Pipeline.generate config package)

                    let fileNamed (rendered: RenderModel) name =
                        rendered.Files |> List.tryFind (fst >> (=) name) |> Option.map snd

                    [ testCase "a shipped dependency is written as its own module" <| fun _ ->
                          let rendered = generate ()

                          Expect.equal
                              (rendered.Files |> List.map fst)
                              [ "MultiShipLab.fs"; "groups/DepLab.fs"; "manifest.json" ]
                              "the entry module, the shipped group under groups/, and the manifest"

                          let group =
                              fileNamed rendered "groups/DepLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no module for the shipped group")

                          Expect.stringContains group "module rec DepLab" "under the name O7 templates for it"
                          Expect.stringContains group "type Widget =" "carrying the dependency's declaration"
                          Expect.stringContains group "abstract size: float" "resolved in full, not by identity"

                          Expect.stringContains
                              group
                              "Generated by Xantham.Generator from dep-lab"
                              "and recording which package it came out of"

                      testCase "the entry names the dependency's type across the module boundary" <| fun _ ->
                          let rendered = generate ()

                          let entry =
                              fileNamed rendered "MultiShipLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no entry module")

                          // The same spelling the `reference` disposition templates: a shipped
                          // group and a referenced one are interchangeable at the use site.
                          Expect.stringContains entry "abstract widget: DepLab.Widget" "qualified into the group's module"
                          Expect.isFalse (entry.Contains "type Widget =") "and declared there once, not twice"

                      testCase "emission is reported per group, and reaches the manifest" <| fun _ ->
                          let rendered = generate ()

                          let emitted =
                              rendered.Findings
                              |> List.filter (fun f -> f.Key.StartsWith "GE")
                              |> List.map (fun f -> f.Key, f.Symbol, f.Tier)

                          Expect.equal
                              emitted
                              [ "GE001", "dep-lab", Exact
                                "GE003", "dep_lab", Escape
                                "GE002", "absent-lab", Widened ]
                              "one shipped, one collided, one configured and never reached"

                          let manifest =
                              fileNamed rendered "manifest.json"
                              |> Option.defaultWith (fun () -> failtest "no manifest")

                          Expect.stringContains manifest "\"GE001\"" "the manifest carries what emission found"

                      testCase "a group that loses the module name keeps its declarations in the entry" <| fun _ ->
                          let rendered = generate ()

                          let entry =
                              fileNamed rendered "MultiShipLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no entry module")

                          // `dep-lab` and `dep_lab` both template `DepLab`. The loser is not
                          // written twice and its references are not left dangling: the
                          // declarations stay where a run writing one module puts them.
                          Expect.stringContains entry "type Spare =" "declared in the entry module"
                          Expect.stringContains entry "abstract spare: Spare" "and named there unqualified" ])

        // Wave five lane W (O7 group classification). npm installs a package's dependencies
        // under the package's own `node_modules`, and a version conflicting with one the entry
        // package resolved goes under that dependency's `node_modules` in turn. Both depths
        // carry the entry package's directory as a path prefix, so this lab is what separates a
        // dependency from its host.
        yield!
            fixtureTests
                "nested-dep-lab"
                (handFixture "nested-dep-lab")
                (handConfig (handFixture "nested-dep-lab"))
                (fun package ->
                    let config = handConfig (Some package)

                    let generate () =
                        Async.RunSynchronously(Pipeline.generate config package)

                    let fileNamed (rendered: RenderModel) name =
                        rendered.Files |> List.tryFind (fst >> (=) name) |> Option.map snd

                    [ testCase "a dependency under the entry package's own node_modules is its own group" <| fun _ ->
                          let rendered = generate ()

                          let outer =
                              fileNamed rendered "groups/OuterLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no module for outer-lab")

                          Expect.stringContains outer "module rec OuterLab" "the group's own module"
                          Expect.stringContains outer "type Signal =" "carrying its declaration"

                      testCase "a dependency nested under another dependency is that dependency" <| fun _ ->
                          let rendered = generate ()

                          let inner =
                              fileNamed rendered "groups/InnerLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no module for inner-lab")

                          Expect.stringContains inner "module rec InnerLab" "named for the package npm nested"
                          Expect.stringContains inner "type Pulse =" "and carrying its declaration"

                          let entry =
                              fileNamed rendered "NestedDepLab.fs"
                              |> Option.defaultWith (fun () -> failtest "no entry module")

                          Expect.isFalse (entry.Contains "type Signal =") "neither depth lands in the entry module"
                          Expect.isFalse (entry.Contains "type Pulse =") "at either level of nesting"
                          Expect.stringContains entry "abstract signal: OuterLab.Signal" "each is named where it ships"

                      testCase "both depths are reported as shipped groups" <| fun _ ->
                          let rendered = generate ()

                          Expect.equal
                              (rendered.Findings
                               |> List.filter (fun f -> f.Key.StartsWith "GE")
                               |> List.map (fun f -> f.Key, f.Symbol, f.Tier)
                               |> List.sortBy (fun (_, symbol, _) -> symbol))
                              [ "GE001", "inner-lab", Exact; "GE001", "outer-lab", Exact ]
                              "both shipped, neither collided" ])
        // Wave five lane V (O7). An alias over an object literal carries `__type` on the type's
        // own symbol and its declared name on the alias symbol, so the shape is named by the
        // second question rather than the first. Both halves are registered as entry packages,
        // which puts the templated module in the gated corpus and makes the F# compiler the
        // judge of whether the entry names what the dependency ships.
        yield!
            fixtureTests
                "alias-copy-dep-lab"
                (handFixture "alias-copy-lab/node_modules/alias-copy-dep-lab")
                GeneratorConfig.Default
                (fun _ -> [])

        yield!
            fixtureTests
                "alias-copy-lab"
                (handInstalledFixture "alias-copy-lab")
                (handConfig (handInstalledFixture "alias-copy-lab"))
                (fun package ->
                    let config = handConfig (Some package)

                    let source () =
                        Async.RunSynchronously(Pipeline.generate config package).Files
                        |> List.find (fun (name, _) -> name.EndsWith ".fs")
                        |> snd

                    /// The names a rendered source declares, in emission order. Read off the
                    /// source: a second declaration of one TypeScript type is only real if it
                    /// survives to the file the compile gate builds.
                    let declarationsIn (rendered: string) =
                        rendered.Split('\n')
                        |> Array.choose (fun line ->
                            if line.StartsWith "type " then
                                Some(line.Substring(5).Split([| ' '; '<'; '\r' |]).[0])
                            else
                                None)
                        |> List.ofArray

                    [ testCase "a referenced alias over an object literal is named, not copied" <| fun _ ->
                          let rendered = source ()

                          Expect.stringContains
                              rendered
                              "abstract pair: AliasCopyDepLab.WidgetPair"
                              "the alias is read under the name the dependency ships"

                          Expect.equal
                              (declarationsIn rendered)
                              [ "Panel"; "PanelPair"; "DraftPanel"; "Exports" ]
                              "and the dependency's shape is not re-derived under a second name"

                      testCase "the entry package's own alias over an object literal is declared here" <| fun _ ->
                          Expect.stringContains (source ()) "type PanelPair =" "the entry group resolves by content"

                      testCase "a mapped type over an entry operand still resolves by content" <| fun _ ->
                          // `Partial<Panel>` groups as the compiler lib and is widened there, so
                          // deferring to its name would lose the operand with it (D6).
                          Expect.stringContains
                              (source ())
                              "abstract widget: AliasCopyDepLab.Widget option"
                              "the mapped expansion carries the operand's own members"

                      testCase "a referenced callback alias is named rather than expanded" <| fun _ ->
                          Expect.stringContains
                              (source ())
                              "abstract format: AliasCopyDepLab.Formatter"
                              "an alias with no members is still a name the dependency ships" ])

    ]
