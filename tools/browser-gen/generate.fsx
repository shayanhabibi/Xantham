/// Emits the DOM half of the compiler-lib disposition: the table mapping `lib.dom.d.ts` names
/// to the `Browser.Types.*` bindings the `Fable.Browser.*` family ships, plus the compile-gate
/// file that proves every entry in it resolves.
///
/// The ECMAScript half of `lib.d.ts` is bound by `Fable.Core.JS` and is a hand-written table in
/// `Model.fs` (`Naming.LibBindings`) - thirty-odd names, each with a hand-judged note about what
/// the mapping gives up. The DOM half is five hundred names that nobody should transcribe, and
/// AGENTS.md's rule applies: nothing that can be generated is hand-written. So this script reads
/// both sides of the mapping off disk and emits the intersection.
///
/// - **Fable's side** is reflection over the referenced `Browser.*` assemblies. A type's arity
///   comes from the assembly rather than from a guess, which is the same safety argument the
///   ECMAScript table makes in prose: TypeScript's lib drifts, and a mapping that assumed the
///   two agreed would emit code that does not compile.
/// - **TypeScript's side** is the `lib.*.d.ts` files shipped in the pinned compiler package.
///   Intersecting against them is what makes the table a table of *lib names*: without it the
///   output would carry Fable's own helpers (`Tags`, `Constrain`, the `*Type` constructor-side
///   interfaces), entries that can never match and that make the count mean nothing.
///
/// Two things are dropped rather than emitted, each loudly in the summary:
///
/// - A name two referenced assemblies both define. `Browser.Types.Cursor` is in both
///   `Browser.IndexedDB` and `Browser.MediaStream`, and a reference to it does not compile -
///   F# has no way to pick. Widening is the honest outcome.
/// - A Fable type that is an F# module rather than a type (`Browser.Dom`, the per-assembly
///   `Tags`). These compile to abstract sealed classes and reflection cannot otherwise tell
///   them from a binding.
///
/// Usage: `dotnet fsi tools/browser-gen/generate.fsx <libDir> <tableFile> <gateFile>`.
/// Routed by `tools/generate-wire.fsx -- generate browser`, which supplies the defaults.

// The pinned family. Every one is an interface-only assembly that Fable erases, so the cost of
// referencing all of them is a `PackageReference` line each and nothing at runtime - which is
// why the set is "the family" rather than a subset. A subset would make the *table* partial in
// a way invisible at the call site: a `.d.ts` mentioning `MediaStream` would widen for a reason
// that has nothing to do with fidelity and everything to do with which packages we listed.
//
// These pins have to match the ones in `tests/Xantham.Generator.CompileGate` - the gate is what
// proves the emitted table resolves, and it can only prove it against the versions it
// references. The generated gate file fails to compile if the two drift apart.
#r "nuget: Fable.Browser.Blob, 1.4.0"
#r "nuget: Fable.Browser.Css, 2.5.0"
#r "nuget: Fable.Browser.Dom, 2.20.0"
#r "nuget: Fable.Browser.Event, 1.7.0"
#r "nuget: Fable.Browser.EventSource, 1.0.0"
#r "nuget: Fable.Browser.Gamepad, 1.3.0"
#r "nuget: Fable.Browser.Geolocation, 1.3.0"
#r "nuget: Fable.Browser.IndexedDB, 2.2.0"
#r "nuget: Fable.Browser.IntersectionObserver, 1.0.0"
#r "nuget: Fable.Browser.MediaQueryList, 1.5.0"
#r "nuget: Fable.Browser.MediaRecorder, 2.2.0"
#r "nuget: Fable.Browser.MediaStream, 3.4.0"
#r "nuget: Fable.Browser.Navigator, 2.5.0"
#r "nuget: Fable.Browser.Performance, 1.3.0"
#r "nuget: Fable.Browser.ResizeObserver, 1.0.0"
#r "nuget: Fable.Browser.Svg, 2.4.0"
#r "nuget: Fable.Browser.Url, 1.4.0"
#r "nuget: Fable.Browser.WebGL, 1.3.0"
#r "nuget: Fable.Browser.WebRTC, 1.6.0"
#r "nuget: Fable.Browser.WebSocket, 1.4.0"
#r "nuget: Fable.Browser.WebStorage, 1.3.0"
#r "nuget: Fable.Browser.Worker, 1.4.0"
#r "nuget: Fable.Browser.XMLHttpRequest, 1.4.0"

open System
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions

/// The assembly each package ships, in the order the packages are pinned above. `#r` only
/// records a reference - nothing is loaded until a type is asked for - so the list is also what
/// forces the load.
let assemblyNames =
    [
        "Browser.Blob"
        "Browser.Css"
        "Browser.Dom"
        "Browser.Event"
        "Browser.EventSource"
        "Browser.Gamepad"
        "Browser.Geolocation"
        "Browser.IndexedDB"
        "Browser.IntersectionObserver"
        "Browser.MediaQueryList"
        "Browser.MediaRecorder"
        "Browser.MediaStream"
        "Browser.Navigator"
        "Browser.Performance"
        "Browser.ResizeObserver"
        "Browser.Svg"
        "Browser.Url"
        "Browser.WebGL"
        "Browser.WebRTC"
        "Browser.WebSocket"
        "Browser.WebStorage"
        "Browser.Worker"
        "Browser.XMLHttpRequest"
    ]

let argv = fsi.CommandLineArgs |> Array.skip 1

if argv.Length <> 3 then
    eprintfn "usage: generate.fsx <libDir> <tableFile> <gateFile>"
    exit 1

let libDir = argv[0]
let tableFile = argv[1]
let gateFile = argv[2]

// ---------------------------------------------------------------------------
// Fable's side: what the referenced assemblies export under `Browser.Types`.
// ---------------------------------------------------------------------------

/// An F# module compiles to an abstract sealed class, which is how a `Browser.Types.Tags`
/// helper module is told from a `Browser.Types.Element` binding. Nested types are skipped for
/// the same reason: a name written `Browser.Types.X.Y` is not a lib name.
let private isBinding (t: Type) =
    not t.IsNested && not (t.IsAbstract && t.IsSealed)

type FableType =
    {
        Name: string
        Arity: int
        Assembly: string
    }

let fableTypes =
    assemblyNames
    |> List.collect (fun name ->
        let assembly = Assembly.Load(AssemblyName name)

        assembly.GetExportedTypes()
        |> Array.filter (fun t -> t.Namespace = "Browser.Types" && isBinding t)
        |> Array.map (fun t ->
            {
                Name = t.Name.Split('`')[0]
                Arity = t.GetGenericArguments().Length
                Assembly = name
            })
        |> Array.toList)

/// A name two assemblies both define cannot be written: `Browser.Types.Cursor` is in
/// `Browser.IndexedDB` and `Browser.MediaStream`, and there is no qualification that picks one.
/// Ambiguity is by *name*, not by name and arity - F# resolves the name first.
let ambiguous =
    fableTypes
    |> List.groupBy _.Name
    |> List.filter (fun (_, xs) -> (xs |> List.map _.Assembly |> List.distinct).Length > 1)
    |> List.map fst
    |> Set.ofList

// ---------------------------------------------------------------------------
// TypeScript's side: what the pinned compiler's `lib.*.d.ts` files declare.
// ---------------------------------------------------------------------------

/// The type names a lib file declares. Values (`declare var Response: {...}`) are deliberately
/// not counted: a value declaration introduces a name in the value namespace, and what the
/// generator resolves at a type position is the `interface` of the same name. A file that
/// declares only the value has no type to bind.
let private declarationPattern =
    Regex(
        @"^\s*(?:declare\s+)?(?:abstract\s+)?(?:interface|class|type)\s+([A-Za-z_$][A-Za-z0-9_$]*)",
        RegexOptions.Multiline
    )

let libNames =
    if not (Directory.Exists libDir) then
        failwith $"no lib directory at {libDir} - the pinned `typescript` package ships `lib.*.d.ts` beside `tsc`"

    let files = Directory.GetFiles(libDir, "lib.*.d.ts")

    if files.Length = 0 then
        failwith $"no lib.*.d.ts under {libDir}"

    files
    |> Array.collect (fun file ->
        declarationPattern.Matches(File.ReadAllText file)
        |> Seq.map (fun m -> m.Groups[1].Value)
        |> Seq.toArray)
    |> Set.ofArray

// ---------------------------------------------------------------------------
// The intersection, and the two files it produces.
// ---------------------------------------------------------------------------

let entries =
    fableTypes
    |> List.filter (fun t -> libNames.Contains t.Name && not (ambiguous.Contains t.Name))
    |> List.distinctBy (fun t -> t.Name, t.Arity)
    |> List.sortBy (fun t -> t.Name, t.Arity)

let droppedAmbiguous =
    ambiguous |> Set.filter libNames.Contains |> Set.toList |> List.sort

let unbound =
    fableTypes
    |> List.map _.Name
    |> List.distinct
    |> List.filter (fun name -> not (libNames.Contains name))
    |> List.length

let private header (generatedBy: string) =
    [
        "// <auto-generated>"
        $"//   Generated by tools/browser-gen/generate.fsx from the pinned Fable.Browser.* family."
        $"//   {generatedBy}"
        "//   Do not edit by hand - regenerate instead."
        "// </auto-generated>"
    ]

let private write (path: string) (lines: string list) =
    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path))
    |> ignore

    File.WriteAllText(path, String.concat "\n" lines + "\n", UTF8Encoding false)

// The table the shape tier reads. Data only: the *rule* for choosing an entry (exact arity,
// else the widest binding the reference has arguments for) is hand-written in `Model.fs`
// alongside the ECMAScript table's rule, and tested there.
write
    tableFile
    [
        yield! header "The DOM half of the compiler-lib disposition; see Naming.BrowserBindings."
        ""
        // A different name from the `Naming.BrowserBindings` module that reads it, so that the
        // rule can name the table without shadowing itself.
        "module Xantham.Generator.BrowserBindingTable"
        ""
        "/// Every `lib.*.d.ts` type name the referenced `Fable.Browser.*` assemblies bind, as"
        "/// (TypeScript name, F# arity, F# name). A name may appear at more than one arity -"
        "/// `CustomEvent` is bound both bare and generic - so the arity is part of the key."
        "let entries: (string * int * string) list ="
        "    ["
        for entry in entries do
            $"      \"{entry.Name}\", {entry.Arity}, \"Browser.Types.{entry.Name}\""
        "    ]"
    ]

// The gate. A type abbreviation proves the name resolves *and* that the arity is the one the
// table claims, which a value binding would not - and it does it without instantiating a type
// parameter, so a constrained one costs nothing here.
write
    gateFile
    [
        yield! header "Proves every entry of BrowserBindings resolves against the referenced packages."
        ""
        "module Xantham.Generator.CompileGate.BrowserBindings"
        ""
        "// One abbreviation per table entry. Unused by construction: compiling is the assertion."
        "#nowarn \"1182\""
        ""
        for entry in entries do
            let parameters =
                if entry.Arity = 0 then
                    ""
                else
                    let names =
                        List.init entry.Arity (fun i -> "'T" + string (i + 1)) |> String.concat ", "

                    $"<{names}>"

            $"type private T{entry.Arity}_{entry.Name}{parameters} = Browser.Types.{entry.Name}{parameters}"
    ]

printfn ""
printfn $"browser-gen: {entries.Length} bindings over {assemblyNames.Length} assemblies"
printfn $"browser-gen:   {unbound} Fable types are not lib.*.d.ts names (helpers, constructor-side interfaces)"

if not droppedAmbiguous.IsEmpty then
    let named = String.concat ", " droppedAmbiguous
    printfn $"browser-gen:   {droppedAmbiguous.Length} lib names dropped as ambiguous: {named}"

printfn $"browser-gen: wrote {tableFile}"
printfn $"browser-gen: wrote {gateFile}"
