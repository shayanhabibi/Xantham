module Xantham.Generator.Tests.BootstrapTests

open System
open System.IO
open Expecto
open Xantham.Generator
open Xantham.Generator.Measure

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

let private enumeration (files: string list) (manifest: string) =
    let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
    Directory.CreateDirectory package |> ignore

    try
        File.WriteAllText(Path.Combine(package, "package.json"), manifest)

        for file in files do
            let path = Path.Combine(package, file)
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, "export declare const value: number;")

        let paths, skipped = Bootstrap.publicPaths GeneratorConfig.Default package

        paths
        |> List.map (fun p -> p.Key, Path.GetRelativePath(package, p.File / uom<declFile>).Replace('\\', '/')),
        skipped |> List.map fst
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

        // manifest <-- files on disk ==| paths generated /-/ exports keys skipped,
        // each generated path written as key --> its declaration file
        let inline (<--) manifest files = manifest, files
        let inline (==|) (manifest, files) generated = manifest, files, generated
        let inline (/-/) paths skipped = paths, skipped
        let inline (-->) key file = key, file

        testTheory "public paths" [
            """{ "types": "index.d.ts" }"""
            <-- [ "index.d.ts" ] ==| [ "." --> "index.d.ts" ] /-/ []

            """{ "exports": { ".": { "types": "./root.d.ts" }, "./client": { "types": "./client.d.ts" } } }"""
            <-- [ "root.d.ts"; "client.d.ts" ]
                ==| [ "." --> "root.d.ts"; "./client" --> "client.d.ts" ] /-/ []

            """{ "exports": { "./b": { "types": "./b.d.ts" }, "./a": { "import": { "types": "./a.d.ts" } } } }"""
            <-- [ "a.d.ts"; "b.d.ts" ] ==| [ "./a" --> "a.d.ts"; "./b" --> "b.d.ts" ] /-/ []

            """{ "exports": { ".": { "types": "./root.d.ts" }, "./features/*": { "types": "./f/*.d.ts" } } }"""
            <-- [ "root.d.ts" ] ==| [ "." --> "root.d.ts" ] /-/ [ "./features/*" ]

            // a bare condition string names a runtime file, so the key supplies no declaration
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./js": { "default": "./js.js" } } }"""
            <-- [ "root.d.ts" ] ==| [ "." --> "root.d.ts" ] /-/ [ "./js" ]

            """{ "exports": { ".": { "types": "./root.d.ts" }, "./client/index.js": { "types": "./client/index.d.ts" } } }"""
            <-- [ "root.d.ts"; "client/index.d.ts" ]
                ==| [ "." --> "root.d.ts"; "./client/index.js" --> "client/index.d.ts" ] /-/ []

            // a declared key whose file is absent is reported, not fatal
            """{ "exports": { ".": { "types": "./root.d.ts" }, "./gone": { "types": "./gone.d.ts" } } }"""
            <-- [ "root.d.ts" ] ==| [ "." --> "root.d.ts" ] /-/ [ "./gone" ]

            """{ "exports": { ".": { "types": "./root.d.ts" }, "./package.json": "./package.json" } }"""
            <-- [ "root.d.ts" ] ==| [ "." --> "root.d.ts" ] /-/ []
        ] <| fun (manifest, files, expected) ->
            enumeration files manifest
            |> Flip.Expect.equal "root first, subpaths ordinal, skipped keys listed" expected

        testCase "a configured entry is the root alone" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory(Path.Combine(package, "dist")) |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { ".": { "types": "./dist/root.d.ts" }, "./adapter": { "types": "./dist/adapter.d.ts" } } }""")
                File.WriteAllText(Path.Combine(package, "dist", "root.d.ts"), "export declare const r: number;")
                File.WriteAllText(Path.Combine(package, "dist", "adapter.d.ts"), "export declare const a: number;")
                let config = { GeneratorConfig.Default with Entry = Some "dist/adapter.d.ts" }
                let paths, skipped = Bootstrap.publicPaths config package
                Expect.equal (paths |> List.map _.Key) [ "." ] "entry is the only path"
                Expect.isEmpty skipped "nothing skipped"
            finally
                Directory.Delete(package, true)

        testCase "a subpaths allowlist restricts enumeration and rejects unknown keys" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory package |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { ".": { "types": "./root.d.ts" }, "./a": { "types": "./a.d.ts" }, "./b": { "types": "./b.d.ts" } } }""")
                for f in [ "root.d.ts"; "a.d.ts"; "b.d.ts" ] do
                    File.WriteAllText(Path.Combine(package, f), "export declare const v: number;")
                let config = { GeneratorConfig.Default with Subpaths = Some [ "./b" ] }
                let paths, _ = Bootstrap.publicPaths config package
                Expect.equal (paths |> List.map _.Key) [ "."; "./b" ] "root plus the allowlisted key"
                let bad = { GeneratorConfig.Default with Subpaths = Some [ "./missing" ] }
                Expect.throwsC (fun () -> Bootstrap.publicPaths bad package |> ignore) (fun e ->
                    Expect.stringContains e.Message "./missing" "the unknown key is named")
            finally
                Directory.Delete(package, true)

        testCase "a root-less map yields subpaths and no root" <| fun _ ->
            let package = Path.Combine(Path.GetTempPath(), "xantham-paths-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory package |> ignore
            try
                File.WriteAllText(Path.Combine(package, "package.json"), """{ "exports": { "./a": { "types": "./a.d.ts" } } }""")
                File.WriteAllText(Path.Combine(package, "a.d.ts"), "export declare const v: number;")
                let paths, _ = Bootstrap.publicPaths GeneratorConfig.Default package
                Expect.equal (paths |> List.map _.Key) [ "./a" ] "no root path"
            finally
                Directory.Delete(package, true)
    ]
