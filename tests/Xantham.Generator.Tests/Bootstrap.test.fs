module Xantham.Generator.Tests.BootstrapTests

open System
open System.IO
open Expecto
open Xantham.Generator

let private selection expected (manifest: string) =
    let package = Path.Combine(Path.GetTempPath(), "xantham-entry-" + Guid.NewGuid().ToString "N")
    Directory.CreateDirectory package |> ignore

    try
        File.WriteAllText(Path.Combine(package, "package.json"), manifest)
        File.WriteAllText(Path.Combine(package, "index.d.ts"), "export declare const fallback: number;")

        let actual =
            try
                Bootstrap.entryFile package
                |> fun path -> Path.GetRelativePath(package, path).Replace('\\', '/')
                |> Ok
            with e ->
                Error(e.Message.Replace(package, "<package>"))

        actual, expected
    finally
        Directory.Delete(package, true)

let private explicitSelection expected (selected: string) =
    let package = Path.Combine(Path.GetTempPath(), "xantham-entry-" + Guid.NewGuid().ToString "N")
    let path = Path.Combine(package, selected)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore

    try
        File.WriteAllText(path, "export declare const selected: number;")
        let config = { GeneratorConfig.Default with Entry = Some selected }
        let actual = Bootstrap.resolveEntryFile config package |> fun entry -> Path.GetRelativePath(package, entry).Replace('\\', '/')
        actual, expected
    finally
        Directory.Delete(package, true)

[<Tests>]
let tests =
    testList "generator declaration entry" [
        let inline (==>) manifest entry = manifest, Ok entry
        let inline (=!>) manifest message = manifest, Error message
        let inline (<=>) selected entry = selected, entry

        testTheory "explicit TypeScript inputs" [
            "entry.ts" <=> "entry.ts"
            "entry.tsx" <=> "entry.tsx"
            "entry.mts" <=> "entry.mts"
            "entry.cts" <=> "entry.cts"
            "entry.d.ts" <=> "entry.d.ts"
            "entry.d.mts" <=> "entry.d.mts"
            "entry.d.cts" <=> "entry.d.cts"
            "dist/adapter.d.ts" <=> "dist/adapter.d.ts"
        ] <| fun (selected, expected) ->
            explicitSelection expected selected ||> Flip.Expect.equal "the selected input is relative to the package directory"

        testTheory "default entry" [
            """{ "types": "root.d.ts", "typings": "legacy.d.ts" }""" ==> "root.d.ts"
            """{ "typings": "legacy.d.ts" }""" ==> "legacy.d.ts"
            """{ "exports": { "types": "root.d.mts", "default": "root.mjs" } }""" ==> "root.d.mts"
            """{ "exports": { ".": { "import": { "types": "root.d.mts" } } } }""" ==> "root.d.mts"
            """{ "exports": { "./adapter": { "types": "adapter.d.ts" }, ".": { "types": "root.d.ts" } } }""" ==> "root.d.ts"
            """{ "types": "root.d.ts", "exports": { ".": { "types": "other.d.ts" } } }""" ==> "root.d.ts"
            """{ "exports": "./index.js" }""" ==> "index.d.ts"
            """{ "types": "root.d.ts", "exports": null }""" ==> "root.d.ts"
            """{}""" ==> "index.d.ts"
        ] <| fun (manifest, expected) ->
            selection expected manifest ||> Flip.Expect.equal "the default describes the package root"

        testTheory "a package without a root requires an explicit entry" [
            let refusal = "package at <package> exports no root entry - set \"entry\" to a TypeScript file and \"runtime\" to its public import in xantham.json"
            """{ "exports": { "./adapter": { "types": "adapter.d.ts" } } }""" =!> refusal
            """{ "types": "root.d.ts", "exports": { "./adapter": { "types": "adapter.d.ts" } } }""" =!> refusal
            """{ "exports": { ".": null, "./adapter": { "types": "adapter.d.ts" } } }""" =!> refusal
            """{ "types": "root.d.ts", "exports": {} }""" =!> refusal
        ] <| fun (manifest, expected) ->
            selection expected manifest ||> Flip.Expect.equal "types and index.d.ts cannot invent a public root"
    ]
