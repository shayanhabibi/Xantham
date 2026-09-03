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
                (Grouping.classify packageDir (declaredAt $"{packageDir}/index.d.ts"))
                EntryPackage
                "entry"

        testCase "the compiler's bundled libs are the compiler-lib group" <| fun _ ->
            for path in
                [ "bundled:///libs/lib.es2023.d.ts"
                  "C:/repo/node_modules/typescript/lib/lib.dom.d.ts"
                  // The platform package is where the live wire actually reports them from.
                  "c:/repo/node_modules/@typescript/typescript-win32-x64/lib/lib.es5.d.ts" ] do
                Expect.equal (Grouping.classify packageDir (declaredAt path)) CompilerLib $"{path}"

        testCase "a node_modules entry is that dependency, scoped names kept whole" <| fun _ ->
            Expect.equal
                (Grouping.classify packageDir (declaredAt "C:/repo/node_modules/left-pad/index.d.ts"))
                (Dependency "left-pad")
                "plain"

            Expect.equal
                (Grouping.classify packageDir (declaredAt "C:/repo/node_modules/@types/node/fs.d.ts"))
                (Dependency "@types/node")
                "scoped"

        // Wave five lane W. npm's own layout: a package's dependencies are installed under its
        // `node_modules`, so every dependency path carries the entry package's directory as a
        // prefix, and a conflicting version is nested a level deeper again. The deepest
        // `node_modules` boundary names the group.
        testCase "a dependency installed under the entry package is that dependency" <| fun _ ->
            Expect.equal
                (Grouping.classify packageDir (declaredAt $"{packageDir}/node_modules/left-pad/index.d.ts"))
                (Dependency "left-pad")
                "one level below the entry directory"

            Expect.equal
                (Grouping.classify
                    packageDir
                    (declaredAt $"{packageDir}/node_modules/left-pad/node_modules/@types/node/fs.d.ts"))
                (Dependency "@types/node")
                "and nested under that dependency in turn"

        testCase "no declaration path is unclassified, which dispositions as the entry" <| fun _ ->
            Expect.equal (Grouping.classify packageDir ValueNone) Unclassified "no symbol"

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
            let plain = GeneratorConfig.Default

            Expect.equal (Naming.groupModule plain "ansi-regex" CompilerLib) "TypeScript.Lib" "the lib module"

            Expect.equal (Naming.groupModule plain "ansi-regex" (Dependency "left-pad")) "LeftPad" "a dependency"

            Expect.equal (Naming.groupModule plain "ansi-regex" EntryPackage) "AnsiRegex" "the entry"

            // A DefinitelyTyped package is named for the library it describes, so the module an
            // F# consumer opens is the library's.
            Expect.equal (Naming.packageModule "@types/three") "Three" "a types package"
            Expect.equal (Naming.packageModule "@types/babel__core") "Babel.Core" "and one whose scope DT mangled"

            // The reference side derives it too, or a dependency would be opened under a name no
            // `ship` run of it ever writes.
            Expect.equal
                (Naming.groupModule GeneratorConfig.Default "ansi-regex" (Dependency "@types/three"))
                "Three"
                "a types dependency"

        testCase "a configured namespace names the entry package and the family it lists" <| fun _ ->
            let sdk =
                { GeneratorConfig.Default with
                    Namespace = Some "FSharp.CloudEdge"
                    Groups =
                        Map.ofList
                            [ "@cloudedge/agents", Reference
                              "@cloudedge/kv-store", Reference
                              "cloudedge-legacy", Reference ] }

            let entry = "@cloudedge/sdk"

            Expect.equal
                (Naming.groupModule sdk entry EntryPackage)
                "FSharp.CloudEdge.Sdk"
                "the entry is named under the namespace like any member"

            Expect.equal
                (Naming.groupModule sdk entry (Dependency "@cloudedge/agents"))
                "FSharp.CloudEdge.Agents"
                "a listed group takes a leaf under it"

            Expect.equal
                (Naming.groupModule sdk entry (Dependency "@cloudedge/kv-store"))
                "FSharp.CloudEdge.KvStore"
                "whose leaf is PascalCased like any other segment"

            // Membership is what `groups` says, so a family spanning scopes stays one family.
            Expect.equal
                (Naming.groupModule sdk entry (Dependency "cloudedge-legacy"))
                "FSharp.CloudEdge.CloudedgeLegacy"
                "including an unscoped member"

            // A dependency the configuration leaves unnamed keeps the name an independently
            // generated binding gives it.
            Expect.equal (Naming.groupModule sdk entry (Dependency "left-pad")) "LeftPad" "an unlisted dependency"

            Expect.equal (Naming.groupModule sdk entry CompilerLib) "TypeScript.Lib" "and the compiler lib"

            Expect.equal
                (Naming.groupModule { sdk with ModuleName = Some "FSharp.CloudEdge" } entry EntryPackage)
                "FSharp.CloudEdge"
                "and the family's root sets `module` to take the namespace bare"

            // The two sides of a reference agree without either naming the other outright: the
            // root templates the member, and the member's own run lands on the same module.
            let member' =
                { GeneratorConfig.Default with
                    Namespace = Some "FSharp.CloudEdge" }

            Expect.equal
                (Naming.groupModule member' "@cloudedge/agents" EntryPackage)
                (Naming.groupModule sdk entry (Dependency "@cloudedge/agents"))
                "a member generated as the entry takes the name the root templates"

        testCase "a namespace reaches no group the configuration leaves unnamed" <| fun _ ->
            let config =
                { GeneratorConfig.Default with
                    Namespace = Some "FSharp.CloudEdge" }

            Expect.equal
                (Naming.groupModule config "@cloudedge/sdk" (Dependency "@cloudedge/agents"))
                "Cloudedge.Agents"
                "an unlisted sibling"
    ]
