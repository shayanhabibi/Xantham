/// Two packages, generated separately: the O7 contract that generation order does not matter.
///
/// A `reference` group renders every one of its types as `<groupModule>.<typeName>` on the
/// promise that a `ship` run of that group declares exactly those names, at exactly those
/// arities. `cross-package-lab` generates both halves - the dependency standalone, where it is
/// its own entry package and ships, and the entry with the dependency configured `reference` -
/// and holds the templated names against the declarations the dependency's own golden carries.
module Xantham.Generator.Tests.MultiPackageTests

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

let private updateGoldens =
    match Environment.GetEnvironmentVariable "XANTHAM_UPDATE_GOLDEN" with
    | null
    | ""
    | "0" -> false
    | _ -> true

let private root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "golden")

/// The lab's two packages sit side by side under one `node_modules`, which is how npm installs
/// a dependency and how `Grouping.classify` recognises one: a path under the entry package's
/// own directory classifies as the entry package whatever else it is.
let private packages =
    Path.Combine(root, "tests", "fixtures", "cross-package-lab", "node_modules")

let private entryPackage = Path.Combine(packages, "cross-package-lab")

let private dependencyPackage = Path.Combine(packages, "cross-package-dep")

[<Literal>]
let private DependencyModule = "CrossPackageDep"

/// The entry package's own `xantham.json`, which configures the dependency `reference`, so the
/// templated rendering is the one the corpus commits and the compile gate compiles.
let private referenceConfig = GeneratorConfig.load entryPackage

/// The entry package rendered against that configuration, written by its `Pipeline.test.fs`
/// registration and read here as a file.
let private entryGolden =
    Path.Combine(goldenDir, "cross-package-lab", "CrossPackageLab.fs")

/// The dependency's committed golden: what a `ship` run of the group produced, generated
/// separately and read here as a file rather than as a value.
let private dependencyGolden =
    Path.Combine(goldenDir, "cross-package-dep", "CrossPackageDep.fs")

let private isIdentifierChar (c: char) =
    Char.IsLetterOrDigit c || c = '_' || c = '\''

/// The number of type arguments an F# type application opens at `at` carries, or 0 where `at`
/// opens no application. `->` and `:>` are read as themselves rather than as a closing bracket.
let private angleArity (source: string) (at: int) =
    if at >= source.Length || source[at] <> '<' then
        0
    else
        let mutable depth = 0
        let mutable index = at
        let mutable arguments = 1
        let mutable finished = false

        while not finished && index < source.Length do
            let previous = if index > 0 then source[index - 1] else ' '

            match source[index] with
            | '<' -> depth <- depth + 1
            | '>' when previous <> '-' && previous <> ':' ->
                depth <- depth - 1
                if depth = 0 then finished <- true
            | ',' when depth = 1 -> arguments <- arguments + 1
            | _ -> ()

            index <- index + 1

        arguments

/// Every distinct name a rendered source templates into `moduleName`, each with the arity it is
/// applied at. Read off the source rather than off the model: the templated name is only real
/// if it survives to the file the compile gate builds.
let private templatedInto (moduleName: string) (source: string) =
    let prefix = moduleName + "."

    let rec collect (from: int) acc =
        match source.IndexOf(prefix, from, StringComparison.Ordinal) with
        | -1 -> List.rev acc
        | at ->
            let qualified = at > 0 && (isIdentifierChar source[at - 1] || source[at - 1] = '.')
            let nameStart = at + prefix.Length
            let mutable stop = nameStart

            while stop < source.Length && isIdentifierChar source[stop] do
                stop <- stop + 1

            if qualified || stop = nameStart then
                collect nameStart acc
            else
                collect stop ((source.Substring(nameStart, stop - nameStart), angleArity source stop) :: acc)

    collect 0 [] |> List.distinct |> List.sort

/// Every type a rendered source declares, with the arity of its parameter list.
let private declaredIn (source: string) =
    source.Replace("\r\n", "\n").Split '\n'
    |> Array.choose (fun line ->
        let keyword =
            [ "type "; "and " ]
            |> List.tryFind (fun keyword -> line.StartsWith(keyword, StringComparison.Ordinal))

        match keyword with
        | None -> None
        | Some keyword ->
            let start = keyword.Length
            let mutable stop = start

            while stop < line.Length && isIdentifierChar line[stop] do
                stop <- stop + 1

            if stop = start then
                None
            else
                Some(line.Substring(start, stop - start), angleArity line stop))
    |> Map.ofArray

/// A templated name held against the declarations the dependency ships. `Declared` is `None`
/// where the dependency declares nothing of that name.
type private Violation =
    {
        Name: string
        Applied: int
        Declared: int option
    }

let private violations (templated: (string * int) list) (declared: Map<string, int>) =
    templated
    |> List.choose (fun (name, applied) ->
        match Map.tryFind name declared with
        | Some arity when arity = applied -> None
        | declared ->
            Some
                {
                    Name = name
                    Applied = applied
                    Declared = declared
                })

let private generated config package =
    Async.RunSynchronously(Pipeline.generate config package)

let private sourceOf (rendered: RenderModel) =
    rendered.Files
    |> List.find (fun (name, _) -> name.EndsWith(".fs", StringComparison.Ordinal))
    |> snd

let private readText (path: string) =
    File.ReadAllText(path).Replace("\r\n", "\n")

[<Tests>]
let multiPackageTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "generator multi-package" [
            testCase "cross-package-lab: live generation skipped - no compiler"
            <| fun _ ->
                if required then
                    failtest
                        "XANTHAM_REQUIRE_TSC is set and no tsc was found: `npm install` did not run, or \
                         the worktree redirect in tools/workspace.fsx broke"
                else
                    skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE"
        ]
    | Some _ ->

    let entryReference = lazy (sourceOf (generated referenceConfig entryPackage))

    let templated = lazy (templatedInto DependencyModule entryReference.Value)

    let shipped = lazy (declaredIn (readText dependencyGolden))

    testList "generator multi-package" [
        testCase "cross-package-lab: the reference rendering is the committed golden"
        <| fun _ ->
            // The gate compiles the file, so the contract below is held against the same text
            // the F# compiler judges.
            if not updateGoldens then
                Expect.equal (readText entryGolden) entryReference.Value "the reference rendering is pinned"

        testCase "cross-package-lab: the dependency's types are qualified and the entry's own are not"
        <| fun _ ->
            Expect.isNonEmpty templated.Value "the entry references the dependency"

            Expect.isFalse
                (entryReference.Value.Contains "CrossPackageLab.")
                "the entry leaves its own declarations unqualified"

        testCase "cross-package-lab: every templated name is a name the dependency declares"
        <| fun _ ->
            let missing =
                violations templated.Value shipped.Value
                |> List.filter (fun violation -> violation.Declared.IsNone)

            Expect.isEmpty missing $"templated into {DependencyModule} and declared nowhere in its golden"

        // The O7 contract itself, over arity.
        testCase "cross-package-lab: every templated name carries the arity the dependency declares"
        <| fun _ ->
            Expect.isEmpty
                (violations templated.Value shipped.Value)
                $"every name templated into {DependencyModule} is declared there at the same arity"

        // An application into a referenced group is an escape: the shipped declaration's arity
        // belongs to a run this one does not make. `Box<string>` and `Box<Widget>` are the two.
        testCase "cross-package-lab: an applied reference reports its arity as unconfirmed"
        <| fun _ ->
            let unconfirmed =
                (generated referenceConfig entryPackage).Findings
                |> List.filter (fun finding -> finding.Key = "TR054")

            Expect.hasLength unconfirmed 2 "each applied reference raises TR054"

            Expect.isTrue
                (unconfirmed
                 |> List.forall (fun finding -> finding.Message.Contains "Box is referenced with 1 type argument"))
                "and each names the referenced type with the count applied"

        // The other half of identity-only resolution: a referenced group's types are named, not
        // copied. This does not hold - `WidgetPair` is re-derived into the entry package as
        // `PanelPair`, so one TypeScript type gets two unrelated F# declarations. Lane T's
        // section of docs/plans/generator-wave-five.md records the repair as belonging to
        // `Resolve.fs`.
        ptestCase "cross-package-lab: a referenced type is templated rather than re-declared"
        <| fun _ ->
            let entry = declaredIn entryReference.Value

            for name in Map.keys shipped.Value do
                Expect.isFalse
                    (entry |> Map.exists (fun declared _ -> declared.EndsWith(name, StringComparison.Ordinal)))
                    $"{name} belongs to the dependency and is declared in the entry package too"

        testCase "cross-package-lab: generation order does not matter"
        <| fun _ ->
            let dependencyFirst = generated GeneratorConfig.Default dependencyPackage
            let entryAfter = generated referenceConfig entryPackage
            let entryFirst = generated referenceConfig entryPackage
            let dependencyAfter = generated GeneratorConfig.Default dependencyPackage

            Expect.equal entryAfter.Files entryFirst.Files "the entry is byte-identical in either order"

            Expect.equal dependencyAfter.Files dependencyFirst.Files "the dependency is byte-identical in either order"

        testCase "cross-package-lab: the dependency's committed golden is the run the entry templates against"
        <| fun _ ->
            // The contract holds between two separate runs, so the shipped side is read from the
            // committed file; this is what says the committed file is still that run's output.
            if not updateGoldens then
                let shippedNow = generated GeneratorConfig.Default dependencyPackage
                Expect.equal (sourceOf shippedNow) (readText dependencyGolden) "the dependency's golden is current"
    ]
