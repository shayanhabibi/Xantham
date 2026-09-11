/// The resolve tier's origin classification (O7): declaration paths to package groups, and the
/// naming contract those groups template under. Pure - the paths are fabricated handles.
module Xantham.Generator.Tests.ResolveTests

open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

let private packageDir = "C:/repo/tests/fixtures/ansi-regex/node_modules/ansi-regex" * Measure.uom<Measure.dirPath>

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
                (Dependency ("left-pad" * Measure.uom<Measure.npmDependency>))
                "plain"

            Expect.equal
                (Grouping.classify packageDir (declaredAt "C:/repo/node_modules/@types/node/fs.d.ts"))
                (Dependency ("@types/node" * Measure.uom<Measure.npmDependency>))
                "scoped"

        // Wave five lane W. npm's own layout: a package's dependencies are installed under its
        // `node_modules`, so every dependency path carries the entry package's directory as a
        // prefix, and a conflicting version is nested a level deeper again. The deepest
        // `node_modules` boundary names the group.
        testCase "a dependency installed under the entry package is that dependency" <| fun _ ->
            Expect.equal
                (Grouping.classify packageDir (declaredAt $"{packageDir}/node_modules/left-pad/index.d.ts"))
                (Dependency ("left-pad" * Measure.uom<Measure.npmDependency>))
                "one level below the entry directory"

            Expect.equal
                (Grouping.classify
                    packageDir
                    (declaredAt $"{packageDir}/node_modules/left-pad/node_modules/@types/node/fs.d.ts"))
                (Dependency ("@types/node" * Measure.uom<Measure.npmDependency>))
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
                    Groups = Map.ofList [ "typescript/lib" * Measure.uom<Measure.npmDependency>, Reference ] }

            Expect.equal (GeneratorConfig.disposition config CompilerLib) Reference "configured"

        testCase "the naming contract: package names to module names" <| fun _ ->
            Expect.equal (Naming.packageModule ("ansi-regex" * Measure.uom<Measure.npmDependency>)) "AnsiRegex" "plain"
            Expect.equal (Naming.packageModule ("@cloudflare/workers-types" * Measure.uom<Measure.npmDependency>)) "Cloudflare.WorkersTypes" "scoped"
            let plain = GeneratorConfig.Default

            Expect.equal (Naming.groupModule plain ("ansi-regex" * Measure.uom<Measure.npmDependency>) CompilerLib) "TypeScript.Lib" "the lib module"

            Expect.equal (Naming.groupModule plain ("ansi-regex" * Measure.uom<Measure.npmDependency>) (Dependency ("left-pad" * Measure.uom<Measure.npmDependency>))) "LeftPad" "a dependency"

            Expect.equal (Naming.groupModule plain ("ansi-regex" * Measure.uom<Measure.npmDependency>) EntryPackage) "AnsiRegex" "the entry"

            // A DefinitelyTyped package is named for the library it describes, so the module an
            // F# consumer opens is the library's.
            Expect.equal (Naming.packageModule ("@types/three" * Measure.uom<Measure.npmDependency>)) "Three" "a types package"
            Expect.equal (Naming.packageModule ("@types/babel__core" * Measure.uom<Measure.npmDependency>)) "Babel.Core" "and one whose scope DT mangled"

            // The reference side derives it too, or a dependency would be opened under a name no
            // `ship` run of it ever writes.
            Expect.equal
                (Naming.groupModule GeneratorConfig.Default ("ansi-regex" * Measure.uom<Measure.npmDependency>) (Dependency ("@types/three" * Measure.uom<Measure.npmDependency>)))
                "Three"
                "a types dependency"

        testCase "a configured namespace names the entry package and the family it lists" <| fun _ ->
            let sdk =
                { GeneratorConfig.Default with
                    Namespace = Some "FSharp.CloudEdge"
                    Groups =
                        Map.ofList
                            [ "@cloudedge/agents" * Measure.uom<Measure.npmDependency>, Reference
                              "@cloudedge/kv-store" * Measure.uom<Measure.npmDependency>, Reference
                              "cloudedge-legacy" * Measure.uom<Measure.npmDependency>, Reference ] }

            let entry = "@cloudedge/sdk" * Measure.uom<Measure.npmDependency>

            Expect.equal
                (Naming.groupModule sdk entry EntryPackage)
                "FSharp.CloudEdge.Sdk"
                "the entry is named under the namespace like any member"

            Expect.equal
                (Naming.groupModule sdk entry (Dependency ("@cloudedge/agents" * Measure.uom<Measure.npmDependency>)))
                "FSharp.CloudEdge.Agents"
                "a listed group takes a leaf under it"

            Expect.equal
                (Naming.groupModule sdk entry (Dependency ("@cloudedge/kv-store" * Measure.uom<Measure.npmDependency>)))
                "FSharp.CloudEdge.KvStore"
                "whose leaf is PascalCased like any other segment"

            // Membership is what `groups` says, so a family spanning scopes stays one family.
            Expect.equal
                (Naming.groupModule sdk entry (Dependency ("cloudedge-legacy" * Measure.uom<Measure.npmDependency>)))
                "FSharp.CloudEdge.CloudedgeLegacy"
                "including an unscoped member"

            // A dependency the configuration leaves unnamed keeps the name an independently
            // generated binding gives it.
            Expect.equal (Naming.groupModule sdk entry (Dependency ("left-pad" * Measure.uom<Measure.npmDependency>))) "LeftPad" "an unlisted dependency"

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
                (Naming.groupModule member' ("@cloudedge/agents" * Measure.uom<Measure.npmDependency>) EntryPackage)
                (Naming.groupModule sdk entry (Dependency ("@cloudedge/agents" * Measure.uom<Measure.npmDependency>)))
                "a member generated as the entry takes the name the root templates"

        testCase "a namespace reaches no group the configuration leaves unnamed" <| fun _ ->
            let config =
                { GeneratorConfig.Default with
                    Namespace = Some "FSharp.CloudEdge" }

            Expect.equal
                (Naming.groupModule config ("@cloudedge/sdk" * Measure.uom<Measure.npmDependency>) (Dependency ("@cloudedge/agents" * Measure.uom<Measure.npmDependency>)))
                "Cloudedge.Agents"
                "an unlisted sibling"

        testCase "a synthesized name is reduced to what a declaration admits" <| fun _ ->
            Expect.equal (Naming.identifierName "RegistryCfMeta") "RegistryCfMeta" "an identifier stands"
            Expect.equal (Naming.identifierName "Widget_2'") "Widget_2'" "including underscore, digit and tick"

            Expect.equal (Naming.identifierName "Registry@cf/meta") "RegistryCfMeta" "@ and / separate segments"

            Expect.equal
                (Naming.identifierName "Registry@cf/meta/llama-3")
                "RegistryCfMetaLlama3"
                "two keys reduce to two names"

            Expect.equal (Naming.identifierName "RegistryBeta channel") "RegistryBetaChannel" "and so does a space"
            Expect.equal (Naming.identifierName "Registry$ref") "RegistryRef" "a $ drops out"

            // The result heads a declaration, so it opens with a letter and is never empty,
            // whatever the key spelled.
            Expect.equal (Naming.identifierName "2fa") "N2fa" "a digit-led result takes a prefix"
            Expect.equal (Naming.identifierName "@/") "Item" "and a key of separators alone names a position"
    ]
