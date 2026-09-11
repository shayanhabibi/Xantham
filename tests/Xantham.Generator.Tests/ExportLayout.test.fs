module Xantham.Generator.Tests.ExportLayoutTests

open System
open System.IO
open Expecto
open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire

let private root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let private package = Path.Combine(root, "tests", "fixtures", "export-layout-lab")

let private generate () : RenderModel =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> failtest "export-layout-lab requires the repository's pinned TypeScript compiler"
    | Some _ -> Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)

let private sourceOf (rendered: RenderModel) =
    rendered.Files
    |> List.find (fst >> (=) "LayoutLab.fs")
    |> snd
    |> fun source -> source.Replace("\r\n", "\n")

let private containersOf (rendered: RenderModel) =
    rendered.Decls
    |> List.choose (function
        | FsExports container -> Some(container.Name, container)
        | _ -> None)
    |> Map.ofList

let private occurrences (needle: string) (source: string) =
    let rec loop count start =
        match source.IndexOf(needle, start, StringComparison.Ordinal) with
        | -1 -> count
        | index -> loop (count + 1) (index + needle.Length)

    loop 0 0

[<Tests>]
let tests =
    testList
        "export module layout regression"
        [ testCase "Cloudflare ambient owners share the existing type's companion module" <| fun _ ->
              let config = { GeneratorConfig.Default with Lib = Some [ "esnext" ] }
              let packageDir =
                  Path.Combine(root, "tests", "fixtures", "@cloudflare", "workers-types", "node_modules", "@cloudflare", "workers-types")
              let rendered =
                  Pipeline.generate config packageDir
                  |> Async.RunSynchronously
              let containers = containersOf rendered
              for child in [ "Email"; "Workers"; "Workflows" ] do
                  Expect.isTrue (Map.containsKey $"Cloudflare.{child}.Exports" containers) $"{child} shares Cloudflare"
                  Expect.equal
                      containers[$"Cloudflare.{child}.Exports"].Owner
                      (AmbientModule($"cloudflare:{child.ToLowerInvariant()}" * uom<importSpecifier>))
                      "the shared parent preserves each exact public owner"
              let source = rendered.Files |> List.map snd |> String.concat "\n"
              Expect.equal (occurrences "module Cloudflare =" source) 1 "one companion module"
              Expect.stringContains source "type Cloudflare =" "the original type keeps its identity"
              Expect.isFalse (source.Contains "module Cloudflare_") "no owner-specific parent hashes"

          testCase "values are nested under their public module owners" <| fun _ ->
              let rendered = generate ()
              let source = sourceOf rendered
              let containers = containersOf rendered

              for name, owner, exports in
                  [ "Exports", EntryModule, [ "check"; "mode"; "echo" ]
                    "Strict.Exports", AmbientModule("layout-lab/strict" * uom<importSpecifier>), [ "check"; "mode"; "echo" ]
                    "Aliases.Exports", AmbientModule("layout-lab/aliases" * uom<importSpecifier>), [ "renamedCheck" ]
                    "Globals.Exports", GlobalScope, [ "sharedFlag" ] ] do
                  let container =
                      match Map.tryFind name containers with
                      | Some container -> container
                      | None -> failtestf "missing value container %s; emitted %A" name (containers |> Map.keys |> Seq.toList)

                  Expect.equal container.Owner owner $"{name} retains its public owner"

                  let actual = container.Members |> List.map _.ExportName |> Set.ofList
                  for export in exports do
                      Expect.contains actual export $"{name} owns {export}"

              for expected in [ "module Strict ="; "module Aliases ="; "module Globals =" ] do
                  Expect.stringContains source expected $"{expected} is emitted"

              for selector, specifier in
                  [ "check", "layout-lab"
                    "check", "layout-lab/strict"
                    "renamedCheck", "layout-lab/aliases" ] do
                  Expect.stringContains
                      source
                      $"[<Import(\"{selector}\", \"{specifier}\")>]"
                      $"{selector} retains its exact JavaScript import"

              Expect.equal (occurrences "static member check " source) 2 "both owners retain check once"
              Expect.equal (occurrences "static member mode" source) 2 "both owners retain mode once"
              Expect.equal (occurrences "static member echo " source) 2 "both owners retain echo once"
              Expect.equal (occurrences "static member sharedFlag" source) 1 "the global remains one member"

          testCase "same-owner overloads and incompatible candidates all remain callable" <| fun _ ->
              let rendered = generate ()
              let source = sourceOf rendered

              Expect.equal (occurrences "static member convert " source) 2 "the legal overload set remains intact"
              Expect.equal (occurrences "[<Import(\"pick\", \"layout-lab\")>]" source) 2 "both return-only candidates bind pick"
              Expect.stringContains source "static member pick " "the first return-only candidate keeps its public name"
              Expect.stringContains source "static member pick_Overload2 " "the second candidate receives a callable F# name"
              Expect.equal (occurrences "[<Import(\"dispatch\", \"layout-lab\")>]" source) 2 "both collapsed literal candidates bind dispatch"
              Expect.stringContains source "static member dispatch_Overload2 " "the collapsed literal candidate is retained"

              let dropped =
                  rendered.Findings
                  |> List.filter (fun finding -> finding.Key = "DO004")
                  |> List.map _.Symbol

              Expect.isEmpty dropped "owner separation and final collision repair replace the old export-drop baseline"

          testCase "type-only aliases do not create runtime members" <| fun _ ->
              let source = generate () |> sourceOf
              Expect.isFalse (source.Contains "typeOnlyCheck") "export type contributes no value binding" ]
