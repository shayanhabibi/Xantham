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
        testCase "a signature declaration reads as its JSDoc description and one line" <| fun _ ->
            let text =
                "\n  /**\n   * Fires on close.\n   * @param listener called once\n   */\n  on(\n    event: \"close\", // the event\n    listener: () => void,\n  ): this;"

            Expect.equal
                (Resolve.signatureText text)
                ("Fires on close.", "on(event: \"close\", listener: () => void): this")
                "comments and line breaks collapse"

            Expect.equal
                (Resolve.signatureText "getElementsByTagNameNS(namespaceURI: \"http://www.w3.org/1999/xhtml\", localName: string): HTMLCollectionOf<HTMLElement>;")
                ("", "getElementsByTagNameNS(namespaceURI: \"http://www.w3.org/1999/xhtml\", localName: string): HTMLCollectionOf<HTMLElement>")
                "a comment marker inside a string literal is text"

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


[<Tests>]
let frontierDeclarationTests =
    testCase "resolve oversized frontiers preserve source methods and terminate generic expansion" <| fun _ ->
        use scratch = Scratch.directory "frontier-declarations"
        let directory = scratch.Path
        System.IO.File.WriteAllText(System.IO.Path.Combine(directory, "package.json"), """{"name":"frontier-declarations-lab","version":"1.0.0","types":"index.d.ts"}""")
        let declaration = """export interface Ordinary {
    read(value: string): number;
    map<T>(value:T): {kind:"ok";value:T} | {kind:"none"};
}
export interface Expanding<T> { next:Expanding<T[]>; run<U>(value:U):Expanding<U> }
export const first:Expanding<string>;
type Tree<S extends string> = {left:Tree<`${S}l`>;right:Tree<`${S}r`>} & {value:S};
export interface Root {tree:Tree<"">}
type Shared = { registry: { [key:string]: { value:string } } };
export declare const shared: Shared;
type MemoOptions<T> = { equals?: (previous:T, next:T) => boolean };
export declare function memo<T>(read:()=>T, options?:MemoOptions<T>):()=>T;
export type Fetcher<T> = (info: { value:T }) => T | Promise<T>;
export type Options<T> = { onHydrated?: (key:string, details:{value:T}) => void };
export declare function hydrate<T>(options:Options<T | undefined>):void;
type Calls<S extends string> = (() => Calls<`${S}l`>) | ((x:S) => Calls<`${S}r`>);
export declare const calls:Calls<"">;
type Inputs<S extends string> = (left:Inputs<`${S}l`>, right:Inputs<`${S}r`>) => void;
export declare const inputs:Inputs<"">;
type Mapper<T> = <U>(value: T, project: (v: T) => U) => U[];
export declare function mapWith<T>(seed: T): Mapper<T>;
"""
        System.IO.File.WriteAllText(System.IO.Path.Combine(directory, "index.d.ts"), declaration)
        let configFile = System.IO.Path.Combine(directory, "xantham.json")
        System.IO.File.WriteAllText(configFile, """{"module":"Frontier","entry":"index.d.ts","lib":["esnext"],"types":[]}""")
        let resolve = async {
            let! mailbox, ctx = Bootstrap.start (GeneratorConfig.loadFile configFile) directory
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolved, _ = Pipeline.runTier ctx [ Resolve.resolveExportTypes; Resolve.resolveTypeTableWithWidth 1 ] (Pipeline.toResolve harvest)
            return resolved
        }
        let resolved = Async.RunSynchronously resolve
        let ordinary = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "Ordinary"))
        for name in ["read"; "map"] do
            let member_ = ordinary.Members |> List.find (fun member_ -> member_.Symbol.Name = name)
            let method_ = resolved.Types |> Map.tryFind member_.TypeId
            Expect.isSome method_ ("source method survives the width cutoff: " + name)
            Expect.isFalse (Map.containsKey member_.TypeId resolved.NotFollowed) "successful derivation clears stale deferral"
            Expect.isNonEmpty method_.Value.CallSignatures "the method retains its signature"
        let genericMethod = ordinary.Members |> List.find (fun member_ -> member_.Symbol.Name = "map")
        let signature = resolved.Types[genericMethod.TypeId].CallSignatures.Head
        let result = resolved.Types |> Map.tryFind signature.ReturnTypeId
        Expect.isSome result "the generic method result is resolved"
        Expect.equal result.Value.UnionMembers.Length 2 "both return arms survive"
        let payload = result.Value.UnionMembers |> List.map (fun id -> resolved.Types[id]) |> List.collect _.Members |> List.find (fun member_ -> member_.Symbol.Name = "value")
        Expect.equal payload.TypeId signature.TypeParameters.Head "the nested payload retains the method type parameter"
        let shared = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.Members |> List.exists (fun member_ -> member_.Symbol.Name = "registry"))
        let registry = shared.Members |> List.find (fun member_ -> member_.Symbol.Name = "registry")
        Expect.isTrue (Map.containsKey registry.TypeId resolved.Types) "an exported anonymous object's member survives the width cutoff"
        Expect.isFalse (Map.containsKey registry.TypeId resolved.NotFollowed) "the exported object's contract is not deferred"
        let memo = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "memo"))
        let options = memo.CallSignatures.Head.Parameters |> List.find (fun parameter -> parameter.Symbol.Name = "options")
        let optionType = resolved.Types[options.TypeId]
        for operand in optionType.UnionMembers do
            Expect.isTrue (Map.containsKey operand resolved.Types) "an exported generic function's options survive the width cutoff"
        let fetcher = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.CallSignatures |> List.exists (fun signature -> signature.Parameters |> List.exists (fun parameter -> parameter.Symbol.Name = "info")))
        let fetchSignature = fetcher.CallSignatures.Head
        Expect.isTrue (Map.containsKey fetchSignature.Parameters.Head.TypeId resolved.Types) "an exported generic callback alias retains its parameter contract"
        let fetchResult = resolved.Types[fetchSignature.ReturnTypeId]
        for operand in fetchResult.UnionMembers do
            Expect.isTrue (Map.containsKey operand resolved.Types) "an exported generic callback alias retains its return operands"
        let hydratedCallbacks = resolved.Types |> Map.toList |> List.map snd |> List.collect _.CallSignatures |> List.collect _.Parameters |> List.filter (fun parameter -> parameter.Symbol.Name = "details")
        Expect.isNonEmpty hydratedCallbacks "the nested hydration callback is reached"
        for parameter in hydratedCallbacks do
            Expect.isTrue (Map.containsKey parameter.TypeId resolved.Types) "a substituted non-generic callback retains its nested parameter contract"
        let hydrate = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "hydrate"))
        let hydratedOptions = resolved.Types[hydrate.CallSignatures.Head.Parameters.Head.TypeId]
        let onHydrated = hydratedOptions.Members |> List.find (fun member_ -> member_.Symbol.Name = "onHydrated")
        Expect.isTrue (Map.containsKey onHydrated.TypeId resolved.Types) "the substituted callback property is resolved"
        let callbackUnion = resolved.Types[onHydrated.TypeId]
        for operand in callbackUnion.UnionMembers do
            Expect.isTrue (Map.containsKey operand resolved.Types) "the substituted callback union operand is resolved"
        let hydratedSignature = callbackUnion.UnionMembers |> List.map (fun id -> resolved.Types[id]) |> List.collect _.CallSignatures |> List.exactlyOne
        let details = resolved.Types[hydratedSignature.Parameters[1].TypeId]
        let hydratedValue = details.Members |> List.find (fun member_ -> member_.Symbol.Name = "value")
        Expect.contains resolved.Types[hydratedValue.TypeId].UnionMembers hydrate.CallSignatures.Head.TypeParameters.Head "the substituted callback keeps the enclosing function type parameter"
        let mapWith = resolved.Types |> Map.toList |> List.map snd |> List.find (fun facts -> facts.SymbolName |> Option.exists (fun name -> string name = "mapWith"))
        let mapper = resolved.Types[mapWith.CallSignatures.Head.ReturnTypeId]
        let mapSignature = mapper.CallSignatures |> List.exactlyOne
        let project = mapSignature.Parameters |> List.find (fun parameter -> parameter.Symbol.Name = "project")
        Expect.isTrue (Map.containsKey project.TypeId resolved.Types) "a generic signature on an alias application retains its callback parameter"
        Expect.isTrue (Map.containsKey mapSignature.ReturnTypeId resolved.Types) "and its return type"
        Expect.isTrue (Map.containsKey mapSignature.TypeParameters.Head resolved.Types) "and its own type parameter"
        Expect.isLessThan resolved.Types.Count 200 "generic expansion terminates under the bounded frontier"


/// Resolves `declaration` as a package's entry under a frontier width of one, so every
/// generation past the seeds runs the width cutoff and the source-contract walk. Also returns
/// the greatest ratio of contract-walk visits to table size.
let private resolveAtWidthOne prefix (declaration: string) =
    use scratch = Scratch.directory prefix
    let directory = scratch.Path
    System.IO.File.WriteAllText(System.IO.Path.Combine(directory, "package.json"), $"""{{"name":"{prefix}-lab","version":"1.0.0","types":"index.d.ts"}}""")
    System.IO.File.WriteAllText(System.IO.Path.Combine(directory, "index.d.ts"), declaration)
    let configFile = System.IO.Path.Combine(directory, "xantham.json")
    System.IO.File.WriteAllText(configFile, """{"module":"Frontier","entry":"index.d.ts","lib":["esnext"],"types":[]}""")
    let mutable ratio = 0.0
    let observe visits tableSize = ratio <- max ratio (float visits / float (max 1 tableSize))
    let resolve = async {
        let! mailbox, ctx = Bootstrap.start (GeneratorConfig.loadFile configFile) directory
        use _ = mailbox :> System.IDisposable
        let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
        let! resolved, _ = Pipeline.runTier ctx [ Resolve.resolveExportTypes; Resolve.resolveTypeTableObserved 1 observe ] (Pipeline.toResolve harvest)
        return resolved
    }
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let resolved = Async.RunSynchronously resolve
    resolved, stopwatch.Elapsed, ratio

/// Two fresh instantiations per generation keep the width cutoff engaged.
let private widthKeepers =
    [ "export interface Box<T> { value: T; next: Box<T[]> }"
      "export interface Wide { left: Box<string>; right: Box<number> }" ]

[<Tests>]
let frontierCostTests =
    testList "resolve oversized frontier cost" [
        testCase "a diamond chain of interfaces resolves in time linear in its depth" <| fun _ ->
            let depth = 20
            let declaration =
                String.concat "\n" [
                    for k in 0 .. depth - 1 do
                        $"export interface L{k} {{ a: A{k + 1}; b: B{k + 1} }}"
                        $"export interface A{k + 1} {{ n: L{k + 1} }}"
                        $"export interface B{k + 1} {{ n: L{k + 1} }}"
                    $"export interface L{depth} {{ leaf: string }}"
                    // Two fresh instantiations per generation keep the width cutoff engaged.
                    "export interface Box<T> { value: T; next: Box<T[]> }"
                    "export interface Wide { left: Box<string>; right: Box<number> }"
                ]
            let resolved, elapsed, ratio = resolveAtWidthOne "frontier-diamond" declaration
            Expect.isLessThan resolved.Types.Count 200 "the table holds each declaration once"
            Expect.isLessThan ratio 4.0 "each contract walk visits a bounded multiple of the table"
            Expect.isLessThan elapsed.TotalSeconds 60.0 "the contract walk finishes"

        testCase "mutually wrapping generic declarations stay bounded under the width cutoff" <| fun _ ->
            let count = 6
            // An interface instantiation is derived as identity; an alias instantiation of an
            // object literal is derived with its members, which wrap every other alias again.
            let wrapping name (head: int -> string) =
                [
                    for i in 1 .. count do
                        let members =
                            [ for j in 1 .. count do
                                  if j <> i then $"m{j}: {name}{j}<{name}{i}<T>>" ]
                        let body = String.concat "; " members
                        $"{head i} {{ {body} }}"
                ]
            let declaration =
                String.concat "\n" [
                    yield! wrapping "G" (fun i -> $"export interface G{i}<T>")
                    "export declare const g: G1<string>;"
                    yield! wrapping "H" (fun i -> $"type H{i}<T> =")
                    "export declare const h: H1<string>;"
                ]
            let resolved, _, _ = resolveAtWidthOne "frontier-mutual" declaration
            Expect.isLessThan resolved.Types.Count 400 "protection reaches a bounded number of wrapped instantiations"

        testCase "mutually recursive aliases over computed literal arguments stay bounded" <| fun _ ->
            let count = 6
            // Each alias appends its own digit, so a literal argument spells out its path.
            let declaration =
                String.concat "\n" [
                    for i in 1 .. count do
                        let members =
                            [ for j in 1 .. count do
                                  if j <> i then $"m{j}: P{j}<`${{S}}{j}`>" ]
                        let body = String.concat "; " members
                        $"type P{i}<S extends string> = {{ {body} }}"
                    "export declare const p: P1<\"\">;"
                    yield! widthKeepers
                ]
            let resolved, _, ratio = resolveAtWidthOne "frontier-literal" declaration
            Expect.isLessThan resolved.Types.Count 400 "computed literal arguments reach a bounded number of instantiations"
            Expect.isLessThan ratio 10.0 "each contract walk visits a bounded multiple of the table"

        testCase "a contract below a shared application survives whichever source root is walked first" <| fun _ ->
            let lab first second =
                String.concat "\n" (
                    [ "type A<T> = { x: Mid<T>; cb: (v: T) => void };"
                      "type Mid<T> = { z: A<number> };"
                      first
                      second ]
                    @ widthKeepers)
            let instance name = $"export declare const {name}: A<string>;"
            let consumer name = $"export declare function {name}(m: Mid<string>): void;"
            for order, declaration in
                [ "instance first", lab (instance "a") (consumer "h")
                  "consumer first", lab (consumer "c") (instance "z") ] do
                let resolved, _, _ = resolveAtWidthOne "frontier-ancestry" declaration
                let callbacks =
                    resolved.Types
                    |> Map.toList
                    |> List.collect (fun (_, facts) -> facts.Members |> List.filter (fun member_ -> member_.Symbol.Name = "cb"))
                Expect.isGreaterThanOrEqual callbacks.Length 2 $"{order}: both A instantiations carry cb"
                for callback in callbacks do
                    Expect.isTrue (Map.containsKey callback.TypeId resolved.Types) $"{order}: every cb callback is resolved"
                    Expect.isFalse (Map.containsKey callback.TypeId resolved.NotFollowed) $"{order}: no cb callback is deferred"
    ]
