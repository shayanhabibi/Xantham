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
    ]

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
    ]
