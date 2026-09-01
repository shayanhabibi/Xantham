/// The resolve tier's origin classification (O7): declaration paths to package groups, and the
/// naming contract those groups template under. Pure - the paths are fabricated handles.
module Xantham.Generator.Tests.ResolveTests

open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

let private packageDir = "C:/repo/tests/fixtures/ansi-regex/node_modules/ansi-regex"

/// A symbol whose first declaration handle points at `path`.
let private declaredAt (path: string) =
    ValueSome
        { Build.symbol 1 "x" SymbolFlags.Property with
            Declarations = ValueSome [| $"7.262.{path}" |] }

[<Tests>]
let classifyTests =
    testList "resolve classify" [
        testCase "a declaration under the package directory is the entry package" <| fun _ ->
            Expect.equal
                (Resolve.classify packageDir (declaredAt $"{packageDir}/index.d.ts"))
                EntryPackage
                "entry"

        testCase "the compiler's bundled libs are the compiler-lib group" <| fun _ ->
            for path in
                [ "bundled:///libs/lib.es2023.d.ts"
                  "C:/repo/node_modules/typescript/lib/lib.dom.d.ts"
                  // The platform package is where the live wire actually reports them from.
                  "c:/repo/node_modules/@typescript/typescript-win32-x64/lib/lib.es5.d.ts" ] do
                Expect.equal (Resolve.classify packageDir (declaredAt path)) CompilerLib $"{path}"

        testCase "a node_modules entry is that dependency, scoped names kept whole" <| fun _ ->
            Expect.equal
                (Resolve.classify packageDir (declaredAt "C:/repo/node_modules/left-pad/index.d.ts"))
                (Dependency "left-pad")
                "plain"

            Expect.equal
                (Resolve.classify packageDir (declaredAt "C:/repo/node_modules/@types/node/fs.d.ts"))
                (Dependency "@types/node")
                "scoped"

        testCase "no declaration path is unclassified, which dispositions as the entry" <| fun _ ->
            Expect.equal (Resolve.classify packageDir ValueNone) Unclassified "no symbol"

            Expect.equal
                (GeneratorConfig.disposition GeneratorConfig.Default Unclassified)
                Ship
                "unclassified ships"

        testCase "dispositions default to widen for external groups until configured" <| fun _ ->
            Expect.equal (GeneratorConfig.disposition GeneratorConfig.Default CompilerLib) Widen "lib default"

            let config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib", Reference ] }

            Expect.equal (GeneratorConfig.disposition config CompilerLib) Reference "configured"

        testCase "the naming contract: package names to module names" <| fun _ ->
            Expect.equal (Naming.packageModule "ansi-regex") "AnsiRegex" "plain"
            Expect.equal (Naming.packageModule "@cloudflare/workers-types") "Cloudflare.WorkersTypes" "scoped"
            Expect.equal (Naming.groupModule "ansi-regex" CompilerLib) "TypeScript.Lib" "the lib module"
            Expect.equal (Naming.groupModule "ansi-regex" (Dependency "left-pad")) "LeftPad" "a dependency"
            Expect.equal (Naming.groupModule "ansi-regex" EntryPackage) "AnsiRegex" "the entry"
    ]
