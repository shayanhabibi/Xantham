module Xantham.Generator.Tests.DeclarationCatalogTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator
open Xantham.Generator.Measure

let private fixture = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "declaration-identity-lab"))

let private temporaryRoot = Scratch.root

let private configured (directory: string) name entry (references: string array) =
    let path = Path.Combine(directory, name + ".json")
    File.WriteAllText(path,
        JsonSerializer.Serialize {| ``module`` = "Identity." + name; entry = entry
                                    runtime = "declaration-identity-lab/" + name.ToLowerInvariant()
                                    lib = [| "esnext" |]; types = ([||] : string array)
                                    declarationCatalog = true; declarationReferences = references |})
    GeneratorConfig.loadFile path

let private coreTs =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Xantham.Fable.Core.TS", "Xantham.Fable.Core.TS.fsproj")
    |> Path.GetFullPath

let internal compileConsumer directory sources (consumer: string) =
    // The consumer builds with the repository's SDK.
    File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "global.json"), Path.Combine(directory, "global.json"), true)
    File.WriteAllText(Path.Combine(directory, "Consumer.fs"), consumer)
    let includes = sources |> List.map (fun file -> $"<Compile Include=\"{file}\"/>") |> String.concat ""
    File.WriteAllText(Path.Combine(directory, "Consumer.fsproj"), $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup>
<ItemGroup>{includes}<Compile Include="Consumer.fs"/></ItemGroup>
<ItemGroup><PackageReference Include="Fable.Core" Version="5.2.0"/></ItemGroup>
<ItemGroup><ProjectReference Include="{coreTs}"/></ItemGroup>
</Project>""")
    let start = ProcessStartInfo("dotnet")
    start.WorkingDirectory <- directory
    start.ArgumentList.Add "build"
    start.ArgumentList.Add "Consumer.fsproj"
    start.ArgumentList.Add "--disable-build-servers"
    start.ArgumentList.Add "-m:1"
    start.ArgumentList.Add "-p:BuildProjectReferences=false"
    start.ArgumentList.Add "--configuration"
#if DEBUG
    start.ArgumentList.Add "Debug"
#else
    start.ArgumentList.Add "Release"
#endif
    start.ArgumentList.Add "--verbosity"
    start.ArgumentList.Add "quiet"
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    child.WaitForExit()
    child.ExitCode, output.Result + errors.Result

let private sharedConsumer = """module Identity.Consumer
let attach (client: Identity.Root.PublicClient) : Identity.Root.PublicClient =
    Identity.Adapter.Exports.attach client
let bind<'T when 'T :> Identity.Root.Item> (box: Identity.Root.PublicBox<'T>) : Identity.Root.PublicBox<'T> =
    Identity.Adapter.Exports.bind box
let alias (client: Identity.Adapter.Client) : Identity.Root.PublicClient = client
let follow (client: Identity.Root.PublicClient) : Identity.Root.PublicClient =
    Identity.Next.Exports.follow client
let text (config: Identity.Root.TextConfig) : Identity.Root.TextConfig =
    Identity.Adapter.Exports.acceptText config
let generic<'T> (config: Identity.Root.Config<'T>) : Identity.Root.Config<'T> =
    Identity.Adapter.Exports.acceptGeneric config
let constrained<'T> (config: Identity.Root.Config<'T>) : Identity.Root.Config<'T> =
    Identity.Adapter.Exports.acceptConstrained config
let constructor (ctor: Identity.Root.StringConstructor) =
    Identity.Adapter.Exports.acceptConstructor ctor
let right (callback: Identity.Adapter.RightCallback) (value: Identity.Root.Right) : Identity.Root.Right =
    callback.run value
"""

let private sharedSources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs"; "next/Identity.Next.fs" ]

let private writePackageFile directory file (text: string) =
    let path = Path.Combine(directory, file)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

let private staticConsumer = """module Identity.Consumer
let tag: string = Identity.Adapter.Client.tag
let createText () : Identity.Root.Client = Identity.Adapter.Client.create "client"
let createNumber () : Identity.Root.Client = Identity.Adapter.Client.create 2.0
let construct () : Identity.Root.Client = Identity.Adapter.Client.Create "client"
let generic () : string = Identity.Adapter.Client.identity "value"
let setCount () = Identity.Adapter.Client.count <- 3.0
let getCount () : float = Identity.Adapter.Client.count
let accept (client: Identity.Adapter.Client) = Identity.Root.Exports.``use`` client
"""

[<Tests>]
let releaseIdentityTests =
    testList "catalog release identity edge cases" [
        testCase "identical declarations retain exact package releases across relocated installs" <| fun _ ->
            use scratch = Scratch.directory "catalog-release-identity"
            let versions = [ "1.0.0"; "1.0.1"; "1.0.0-beta.1"; "1.0.0-beta.2"; "1.0.0+build.1"; "1.0.0+build.2" ]
            let source = "export interface Item { value: string; }\n"
            let install name version =
                let package = Path.Combine(scratch.Path, name)
                writePackageFile package "package.json"
                    (JsonSerializer.Serialize {| name = "release-identity-lab"; version = version; types = "index.d.ts" |})
                writePackageFile package "index.d.ts" source
                package
            let config = configured scratch.Path "Root" "index.d.ts" [||]
            let produce name version =
                let package = install name version
                let output = Path.Combine(scratch.Path, name + "-out")
                Pipeline.run config package output |> Async.RunSynchronously |> ignore
                let reference = Path.Combine(output, "declarations.json")
                let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText reference,
                    JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                let item = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.Item")
                package, reference, item
            let _, reference, original = produce "original" "1.0.0"
            let relocated, _, copy = produce "relocated" "1.0.0"
            Expect.equal copy.Identity original.Identity "install paths do not change declaration identity"
            Expect.equal copy.Api original.Api "install paths do not change the emitted API"
            let adapter = configured scratch.Path "Adapter" "index.d.ts" [| reference |]
            Pipeline.run adapter relocated (Path.Combine(scratch.Path, "adapter")) |> Async.RunSynchronously |> ignore
            let binding = File.ReadAllText(Path.Combine(scratch.Path, "adapter", "Identity.Adapter.fs"))
            Expect.stringContains binding "Identity.Root.Item" "the reader selects the original nominal owner"
            let identities = ResizeArray<string>()
            identities.Add original.Identity
            for index, version in versions.Tail |> List.indexed do
                let package, _, item = produce ("release-" + string index) version
                identities.Add item.Identity
                Expect.equal item.Api original.Api "identical declarations keep the same F# API across releases"
                let output = Path.Combine(scratch.Path, "independent-" + string index)
                Pipeline.run adapter package output |> Async.RunSynchronously |> ignore
                let binding = File.ReadAllText(Path.Combine(output, "Identity.Adapter.fs"))
                Expect.stringContains binding "type Item" ("another release emits its own nominal declaration: " + version)
                Expect.isFalse (binding.Contains "Identity.Root.Item") "another release must not alias the original nominal owner"
                let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(output, "declarations.json")),
                    JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                let emitted = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Adapter.Item")
                Expect.equal emitted.Identity item.Identity "the reader preserves the new release's identity"
            Expect.equal (identities |> Seq.distinct |> Seq.length) versions.Length
                "patch, prerelease and build metadata differences remain separate declaration identities"
    ]

[<Tests>]
let mappedOptionsTests =
    testCase "declaration catalog mapped options preserve named and anonymous literal references" <| fun _ ->
        let directory = Path.Combine(temporaryRoot, "xantham-mapped-options-" + Guid.NewGuid().ToString "N")
        Directory.CreateDirectory directory |> ignore
        try
            let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-mapped-options-lab"))
            let config = configured directory "Root" "index.d.ts" [||]
            Pipeline.run config package (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
            let catalog =
                JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(
                    File.ReadAllText(Path.Combine(directory, "root", "declarations.json")),
                    JsonSerializerOptions(PropertyNameCaseInsensitive = true))
            let options name =
                catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root." + name + ".SupportedLocalesOf.Options")
            Expect.notEqual (options "First").Identity (options "Second").Identity "named literal references retain distinct mapped identities"
            let consumer = """module Identity.Consumer
let first (value: Identity.Root.First.SupportedLocalesOf.Options) : Identity.Root.LocaleMatcher option = value.localeMatcher
let second (value: Identity.Root.Second.SupportedLocalesOf.Options) : Identity.Root.SecondOptions.LocaleMatcher option = value.localeMatcher
"""
            let sources = [ "root/Identity.Root.fs" ]
            let code, output = compileConsumer directory sources consumer
            Expect.equal code 0 output
            let invalid = consumer + "\nlet invalid (first: Identity.Root.First.SupportedLocalesOf.Options) (second: Identity.Root.Second.SupportedLocalesOf.Options) = first.localeMatcher <- second.localeMatcher\n"
            let code, output = compileConsumer directory sources invalid
            Expect.notEqual code 0 "same literals do not erase named F# type identity"
            Expect.stringContains output "FS0193" "the incompatible literal enums reject cross-assignment"
        finally Directory.Delete(directory, true)

[<Tests>]
let literalUnionOwnershipTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "literal union ownership skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "literal union ownership requires coordinated producer catalogs" <| fun _ ->
            use scratch = Scratch.directory "literal-union-ownership"
            let directory = scratch.Path
            let source = Path.GetFullPath(Path.Combine(fixture, "..", "literal-union-ownership-lab"))
            let package = Path.Combine(directory, "package")
            for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
            let config = GeneratorConfig.loadFile (Path.Combine(package, "xantham.json"))
            let producer name = Path.Combine(package, "node_modules", "literal-owner-" + name + "-lab")
            let generate name input references =
                let output = Path.Combine(directory, name)
                let config =
                    { config with
                        ModuleName = Some ("Identity." + name)
                        DeclarationReferences = references }
                Pipeline.run config input output |> Async.RunSynchronously |> ignore
                Path.Combine(output, "declarations.json")
            let readCatalog path =
                JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText path,
                    JsonSerializerOptions(PropertyNameCaseInsensitive = true))
            let literal path = (readCatalog path).Declarations |> Array.find (fun declaration -> declaration.Role = "literal-union")
            let a = generate "OwnerA" (producer "a") []
            let b = generate "OwnerB" (producer "b") []
            Expect.equal (literal a).Identity (literal b).Identity "opposite member orders share structural identity"
            Expect.equal (literal a).Api (literal b).Api "the enum surfaces agree despite separate nominal ownership"
            Expect.notEqual (literal a).FSharpName (literal b).FSharpName "independent producers emit distinct F# enum names"
            for references in [ [ a; b ]; [ b; a ] ] do
                Expect.throwsC (fun () -> generate "Consumer" package references |> ignore)
                    (fun error -> Expect.stringContains error.Message "references disagree on declaration" "independent ownership is rejected in either order")
            let b = generate "OwnerB" (producer "b") [ a ]
            Expect.equal (literal b).FSharpName (literal a).FSharpName "producer B selects A as the shared owner"
            let owner = (readCatalog b).Owners |> Array.find (fun owner -> owner.Name = "Identity.OwnerB")
            Expect.equal owner.Dependencies [| "Identity.OwnerA" |] "the catalog records the generated F# dependency"
            let named catalog name = (readCatalog catalog).Declarations |> Array.find (fun declaration -> declaration.FSharpName = name)
            Expect.notEqual (named a "Identity.OwnerA.NamedA").Identity (named b "Identity.OwnerB.NamedB").Identity
                "equal-valued named aliases retain their separate declaration identities"
            generate "Consumer" package [ a; b ] |> ignore
            let forward = File.ReadAllText(Path.Combine(directory, "Consumer", "Identity.Consumer.fs"))
            generate "Consumer" package [ b; a ] |> ignore
            Expect.equal (File.ReadAllText(Path.Combine(directory, "Consumer", "Identity.Consumer.fs"))) forward
                "coordinated catalog order does not change the consumer API"
            let sources = [ "OwnerA/Identity.OwnerA.fs"; "OwnerB/Identity.OwnerB.fs"; "Consumer/Identity.Consumer.fs" ]
            let consumer = """module Identity.Usage
let useBoth (a: Identity.OwnerA.AOptions) (b: Identity.OwnerB.BOptions) =
    Identity.Consumer.Exports.accept(a, b)
    Identity.Consumer.Exports.takeA(?value = a.maxTokensField)
    Identity.Consumer.Exports.takeB(?value = b.maxTokensField)
    b.maxTokensField <- a.maxTokensField
"""
            let code, output = compileConsumer directory sources consumer
            Expect.equal code 0 output
            let invalid = consumer + "\nlet invalid (value: Identity.OwnerA.NamedA) : Identity.OwnerB.NamedB = value\n"
            let code, output = compileConsumer directory sources invalid
            Expect.notEqual code 0 "coordinating anonymous unions does not merge named aliases"
            Expect.stringContains output "FS0001" "the named aliases remain distinct F# enum types"
            let stale = Path.Combine(directory, "stale-api.json")
            let catalog = JsonNode.Parse(File.ReadAllText a)
            for declaration in catalog["declarations"].AsArray() do
                if declaration["role"].GetValue<string>() = "literal-union" then
                    declaration["api"] <- JsonValue.Create("stale-api-hash")
            File.WriteAllText(stale, catalog.ToJsonString())
            Expect.throwsC (fun () -> generate "OwnerB" (producer "b") [ stale ] |> ignore)
                (fun error -> Expect.stringContains error.Message "F# API mismatch" "selecting an owner retains API authentication")
            File.AppendAllText(Path.Combine(producer "a", "index.d.ts"), "\n// Changed producer source.\n")
            Expect.throwsC (fun () -> generate "Consumer" package [ a; b ] |> ignore)
                (fun error -> Expect.stringContains error.Message "input source hash mismatch" "a changed producer input invalidates the coordinated catalog")

[<Tests>]
let shippedOverloadTests =
    testCase "declaration catalog shipped dependencies retain literal overload contracts" <| fun _ ->
        let directory = Path.Combine(temporaryRoot, "xantham-shipped-overloads-" + Guid.NewGuid().ToString "N")
        let package = Path.Combine(directory, "package")
        let dependency = Path.Combine(package, "node_modules", "store-owner-lab")
        try
            writePackageFile package "package.json" """{"name":"store-consumer-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile package "index.d.ts" """import { Store } from 'store-owner-lab'; export function use(store: Store): Store;"""
            writePackageFile dependency "package.json" """{"name":"store-owner-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile dependency "index.d.ts" """export interface Store {
    get(kind: "text"): string;
    get(kind: "bytes"): number;
}"""
            let configure name references =
                let path = Path.Combine(directory, name + ".json")
                File.WriteAllText(path,
                    JsonSerializer.Serialize {| ``module`` = "Identity." + name; ``namespace`` = "Identity.Support"
                                                lib = [| "esnext" |]; types = ([||] : string array)
                                                groups = Map.ofList [ "store-owner-lab", "ship" ]
                                                declarationCatalog = true; declarationReferences = references |})
                GeneratorConfig.loadFile path
            let producer = Path.Combine(directory, "root")
            Pipeline.run (configure "Root" [||]) dependency producer |> Async.RunSynchronously |> ignore
            let reference = Path.Combine(producer, "declarations.json")
            Pipeline.run (configure "Adapter" [| reference |]) package (Path.Combine(directory, "adapter"))
            |> Async.RunSynchronously |> ignore
            let consumer = """module Identity.Consumer
let read (store: Identity.Root.Store) (text: Identity.Root.Store.Text) (bytes: Identity.Root.Store.Bytes) : string * float =
    let shared = Identity.Adapter.Exports.``use`` store
    shared.get text, shared.get bytes
"""
            let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
            Expect.equal code 0 output
            let invalid = consumer + "\nlet invalid (store: Identity.Root.Store) = store.get \"anything\"\n"
            let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] invalid
            Expect.notEqual code 0 "arbitrary strings must not enter a literal-discriminated overload"
            Expect.stringContains output "FS0041" "overload resolution rejects the wrong discriminator"
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let shippedIntersectionTests =
    testCase "declaration catalog shipped intersections retain named alias bases" <| fun _ ->
        let directory = Path.Combine(temporaryRoot, "xantham-shipped-intersection-" + Guid.NewGuid().ToString "N")
        let package = Path.Combine(directory, "package")
        let dependency = Path.Combine(package, "node_modules", "intersection-owner-lab")
        try
            writePackageFile package "package.json" """{"name":"intersection-consumer-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile package "index.d.ts" """import { Result } from 'intersection-owner-lab'; export function use(value: Result): Result;"""
            writePackageFile dependency "package.json" """{"name":"intersection-owner-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile dependency "index.d.ts" """export type Base = { name: string }; export type Result = Base & { id: number };"""
            let configure name references =
                let path = Path.Combine(directory, name + ".json")
                File.WriteAllText(path,
                    JsonSerializer.Serialize {| ``module`` = "Identity." + name; ``namespace`` = "Identity.Support"
                                                lib = [| "esnext" |]; types = ([||] : string array)
                                                groups = Map.ofList [ "intersection-owner-lab", "ship" ]
                                                declarationCatalog = true; declarationReferences = references |})
                GeneratorConfig.loadFile path
            let producer = Path.Combine(directory, "root")
            Pipeline.run (configure "Root" [||]) dependency producer |> Async.RunSynchronously |> ignore
            let reference = Path.Combine(producer, "declarations.json")
            Pipeline.run (configure "Adapter" [| reference |]) package (Path.Combine(directory, "adapter"))
            |> Async.RunSynchronously |> ignore
            let consumer = """module Identity.Consumer
let useBase (value: Identity.Root.Result) : Identity.Root.Base = Identity.Adapter.Exports.``use`` value :> Identity.Root.Base
let id (value: Identity.Root.Result) : float = (Identity.Adapter.Exports.``use`` value).id
"""
            let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
            Expect.equal code 0 output
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)

/// The trailing syntax kind of a normalized catalog handle (`<source>#<index>.<kind>`).
let private handleKind (handle: string) =
    handle.Substring(handle.LastIndexOf '.' + 1) |> UInt32.Parse |> LanguagePrimitives.EnumOfValue<uint32, SyntaxKind>

[<Tests>]
let privateNamespaceTests =
    testCase "declaration catalog merged namespace values retain intrinsic empty object arguments" <| fun _ ->
        let directory = Path.Combine(temporaryRoot, "xantham-namespace-value-" + Guid.NewGuid().ToString "N")
        try
            writePackageFile directory "package.json" """{"name":"namespace-value-lab","version":"1.0.0","types":"index.d.ts"}"""
            // A function merged with a namespace has a structural identity, which depends on the
            // checker's intrinsic `{}` in the second `Object.keys` overload.
            writePackageFile directory "util.d.ts" """export function util(): void; export namespace util { const keys: ObjectConstructor["keys"]; }"""
            writePackageFile directory "index.d.ts" """import { util } from './util'; export const helpers: typeof util;"""
            let producer = Path.Combine(directory, "root")
            Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory producer
            |> Async.RunSynchronously |> ignore
            let reference = Path.Combine(producer, "declarations.json")
            let read path = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText path, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
            let util = (read reference).Declarations |> Array.find (fun entry -> entry.FSharpName = "Identity.Root.Util")
            util.Handles |> Array.map handleKind |> Array.sort
            |> Flip.Expect.equal "the merged value is identified by both declarations" [| SyntaxKind.FunctionDeclaration; SyntaxKind.ModuleDeclaration |]
            writePackageFile directory "adapter.d.ts" """import { helpers } from './index'; export const helpers2: typeof helpers;"""
            Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter"))
            |> Async.RunSynchronously |> ignore
            (read (Path.Combine(directory, "adapter", "declarations.json"))).Declarations
            |> Array.filter (fun entry -> entry.Identity = util.Identity)
            |> Array.map (fun entry -> entry.FSharpName, entry.Owner)
            |> Flip.Expect.equal "the adapter reuses the producer's merged value" [| "Identity.Root.Util", "Identity.Root" |]
            let consumer = """module Identity.Consumer
let keys (helpers: Identity.Root.Util) (value: obj): string[] = helpers.keys value
"""
            let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
            Expect.equal code 0 output
            let unresolved = async {
                let config = configured directory "Unresolved" "index.d.ts" [||]
                let! mailbox, ctx = Bootstrap.start config directory
                use _ = mailbox :> IDisposable
                let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
                let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
                let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage config ctx.PackageName) resolve)
                let intrinsic =
                    shape.Types |> Map.toList |> List.filter (fun (_, facts) ->
                        facts.Response.Flags.HasFlag TypeFlags.Object && facts.Origin = Unclassified
                        && facts.DeclFile.IsNone && facts.Declarations.IsEmpty
                        && facts.Members.IsEmpty && facts.CallSignatures.IsEmpty && facts.ConstructSignatures.IsEmpty
                        && facts.SymbolName |> Option.exists (fun name -> name / uom<symbolName> = "__type"))
                Expect.equal intrinsic.Length 1 "the fixture reaches one intrinsic empty object"
                let id, facts = intrinsic.Head
                let unresolved =
                    { shape with
                        Types = Map.add id (TypeFacts.shallow facts.Response) shape.Types
                        NotFollowed = Map.add id "fixture: derivation failed" shape.NotFollowed }
                return! DeclarationCatalog.apply ctx unresolved (Pipeline.groupModules ctx unresolved)
            }
            Expect.throwsC
                (fun () -> unresolved |> Async.RunSynchronously |> ignore)
                (fun error -> Expect.stringContains error.Message "no stable declaration" "checker MembersResolved cannot certify a locally failed derivation")
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let namespaceIdentityTests =
    let read path =
        JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText path, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
    // Each value carries a conditional member, which has no structural key: the namespace and
    // the module object are identified by their declarations alone.
    let producer directory =
        writePackageFile directory "package.json" """{"name":"namespace-identity-lab","version":"1.0.0","types":"index.d.ts"}"""
        writePackageFile directory "util.d.ts" """export namespace util { const keys: ObjectConstructor["keys"]; function check<T>(value: T): T extends string ? string : number; }"""
        writePackageFile directory "external.d.ts" """export declare const version: string; export declare function check<T>(value: T): T extends string ? string : number;"""
        writePackageFile directory "index.d.ts" """import * as external from "./external";
export { util } from "./util";
export const moduleValue: typeof external;
export const frozen: Readonly<typeof import("./util").util>;
"""
        let output = Path.Combine(directory, "root")
        Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory output |> Async.RunSynchronously |> ignore
        Path.Combine(output, "declarations.json")
    testList "declaration catalog namespace identities" [
        testCase "re-exported namespaces and module objects keep their producer identity" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-namespace-identity-" + Guid.NewGuid().ToString "N")
            try
                let reference = producer directory
                let find name = (read reference).Declarations |> Array.find (fun entry -> entry.FSharpName = "Identity.Root." + name)
                let util, moduleValue, frozen = find "Util", find "ModuleValue", find "Frozen"
                (util.Role, util.Handles |> Array.map handleKind)
                |> Flip.Expect.equal "a namespace value is identified by its declaration" ("type", [| SyntaxKind.ModuleDeclaration |])
                (moduleValue.Role, moduleValue.Handles |> Array.map handleKind)
                |> Flip.Expect.equal "a module object is identified by its source file" ("type", [| SyntaxKind.SourceFile |])
                Expect.equal frozen.Role "alias" "a mapped derivative is identified by its alias application"
                Expect.notEqual frozen.Identity util.Identity "a mapped derivative is a different type from the namespace it maps"
                writePackageFile directory "adapter.d.ts" """import { util, moduleValue } from "./index";
export const helpers: typeof util;
export const module2: typeof moduleValue;
"""
                Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter"))
                |> Async.RunSynchronously |> ignore
                let consumer = """module Identity.Consumer
let helpers: Identity.Root.Util = Identity.Adapter.Exports.helpers
let moduleValue: Identity.Root.ModuleValue = Identity.Adapter.Exports.module2
let keys (value: obj) : string[] = helpers.keys value
let version : string = moduleValue.version
"""
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                Expect.equal code 0 output
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

        testCase "an augmented namespace cannot claim its owner's declaration" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-namespace-augmentation-" + Guid.NewGuid().ToString "N")
            try
                let reference = producer directory
                // The augmentation is a second declaration of the producer's namespace, visible
                // only in the consumer's program.
                writePackageFile directory "augmented.d.ts" """import { util } from "./util";
declare module "./util" { namespace util { const extra: number; } }
export const helpers: typeof util;
"""
                Expect.throwsC
                    (fun () ->
                        Pipeline.run (configured directory "Augmented" "augmented.d.ts" [| reference |]) directory (Path.Combine(directory, "augmented"))
                        |> Async.RunSynchronously |> ignore)
                    (fun error -> Expect.stringContains error.Message "declaration handle set conflict for Util" "the augmented namespace keeps both declarations in its identity")
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)
    ]

[<Tests>]
let specializedUnionTests =
    testCase "declaration catalog distinguishes closed and open extracted generic unions" <| fun _ ->
        let directory = Path.Combine(temporaryRoot, "xantham-catalog-extracted-union-" + Guid.NewGuid().ToString "N")
        let dependency = Path.Combine(directory, "node_modules", "task-owner-lab")
        try
            writePackageFile directory "package.json" """{"name":"task-consumer-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile dependency "package.json" """{"name":"task-owner-lab","version":"1.0.0","types":"index.d.ts"}"""
            writePackageFile dependency "index.d.ts" """export type Outcome<R> = {kind:"ok";value:R}|{kind:"error";error:string};
export type State<R> = {status:"live";checkpoint:R}|{status:"done";outcome:Outcome<R>}|{status:"cancelled";outcome:Outcome<R>};
export type Record<R> = {id:string} & ({state:Extract<State<R>,{status:"live"}>}|{state:Extract<State<R>,{status:"done"|"cancelled"}>});
"""
            writePackageFile directory "index.d.ts" """import { Record } from 'task-owner-lab';
export function write(value:Record<string>):void;
export function read<R>():Record<R>;
"""
            let config =
                { configured directory "Task" "index.d.ts" [||] with
                    Groups = Map.ofList [ "task-owner-lab" * uom<npmDependency>, Ship ] }
            let generated = Path.Combine(directory, "generated")
            Pipeline.run config directory generated |> Async.RunSynchronously |> ignore
            let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(generated, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
            let find name = catalog.Declarations |> Array.find (fun entry -> entry.FSharpName = "Identity.Task." + name)
            let generic = find "Read.Result2.State"
            let closed = find "Write.Value2.State"
            Expect.equal generic.Arity 1 "the read result retains its caller's result type"
            Expect.equal closed.Arity 0 "the write result is specialized to string"
            Expect.notEqual generic.Identity closed.Identity "a concrete argument cannot be alpha-normalized as a bound parameter"
            let consumer = """module Identity.Consumer
let generic (value: Identity.Task.Read.Result2.State<string>) = value
let closed (value: Identity.Task.Write.Value2.State) = value
"""
            let code, output = compileConsumer directory [ "generated/Identity.Task.fs" ] consumer
            Expect.equal code 0 output
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let classImplementsTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "class implements catalog skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "class implements catalog preserves explicit generic conformance" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-class-implements-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "class-implements-lab"))
                let root = Path.Combine(directory, "root")
                let rootConfig = configured directory "Root" "model.d.ts" [||]
                Pipeline.run rootConfig package root |> Async.RunSynchronously |> ignore
                let adapter = configured directory "Adapter" "index.d.ts" [| Path.Combine(root, "declarations.json") |]
                Pipeline.run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                let consumer = """module Identity.Consumer
let client (value: Identity.Adapter.Client) : Identity.Root.Model<string> = value :> _
let generic<'T when 'T :> Identity.Root.Model<string>>
    (value: Identity.Adapter.GenericClient<'T>) : Identity.Root.Model<'T> = value :> _
"""
                let code, output = compileConsumer directory sources consumer
                Expect.equal code 0 output
                let structural = """module Identity.Consumer
let unrelated (value: Identity.Adapter.StructuralClient) : Identity.Root.Model<string> = value :> _
"""
                let code, output = compileConsumer directory sources structural
                Expect.notEqual code 0 "matching members without implements retain their distinct nominal type"
                Expect.stringContains output "FS0193" "the unrelated class has no generated subtype relation"
                let invalidArgument = """module Identity.Consumer
let invalid (value: Identity.Adapter.GenericClient<string>) = value
"""
                let code, output = compileConsumer directory sources invalidArgument
                Expect.notEqual code 0 "the declared generic constraint survives inheritance"
                Expect.stringContains output "FS0001" "string does not satisfy the model constraint"
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let classImplementsEntrypointTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "class implements entrypoint skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "class implements preserves entrypoint constructors and hooks" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-class-implements-entrypoint-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "class-implements-entrypoint-lab"))
                let config = configured directory "Entrypoint" "index.d.ts" [||]
                Pipeline.run config package (Path.Combine(directory, "entrypoint")) |> Async.RunSynchronously |> ignore
                let consumer = """module Identity.Consumer
type Station() =
    inherit Identity.Entrypoint.ImplementsLab.Runtime.Station("station")
    override _.run() = "ready"
    interface Identity.Entrypoint.ImplementsLab.Runtime.Station.IFetchHandler with
        member _.fetch() = "fetched"
type Halt() =
    inherit Identity.Entrypoint.ImplementsLab.Runtime.Halt("halt")
    override _.run() = "stopped"
let error (value: Halt) : exn = value :> exn
"""
                let code, output = compileConsumer directory [ "entrypoint/Identity.Entrypoint.fs" ] consumer
                Expect.equal code 0 output
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let dependencyEntrypointTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "dependency entrypoint catalog skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "dependency entrypoint catalog preserves constructors and optional hooks" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-dependency-entrypoint-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "dependency-entrypoint-lab"))
                let root = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) package root |> Async.RunSynchronously |> ignore
                let adapter = configured directory "Adapter" "adapter.d.ts" [| Path.Combine(root, "declarations.json") |]
                Pipeline.run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let consumer = """module Identity.Consumer
type Actor() =
    inherit Identity.Root.EntrypointLab.Runtime.Actor<string>(Identity.Root.EntrypointLab.Runtime.Actor.Options<string>.Create "seed")
    interface Identity.Root.EntrypointLab.Runtime.Actor.IFetchHandler<string> with
        member _.fetch value = value
let accept (actor: Identity.Root.EntrypointLab.Runtime.Actor<string>) : Identity.Root.EntrypointLab.Runtime.Actor<string> =
    Identity.Adapter.Exports.accept actor
"""
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                Expect.equal code 0 output
                let invalid = """module Identity.Consumer
type Actor() =
    inherit Identity.Root.EntrypointLab.Runtime.Actor<string>(Identity.Root.EntrypointLab.Runtime.Actor.Options<float>.Create 1.0)
"""
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] invalid
                Expect.notEqual code 0 "the inline constructor option retains the class type parameter"
                Expect.stringContains output "FS0193" "numeric options do not construct the string actor"
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let dependencyEntrypointOrdinaryTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "dependency entrypoint negatives skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "dependency entrypoint retains ordinary and type-only class interfaces" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-dependency-ordinary-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "dependency-entrypoint-lab"))
                for name, entry, declaration in [ "plain", "plain-adapter.d.ts", "Plain"; "type-only", "type-only-adapter.d.ts", "Hidden" ] do
                    let run = Path.Combine(directory, name)
                    Directory.CreateDirectory run |> ignore
                    Pipeline.run (configured run "Root" entry [||]) package (Path.Combine(run, "root")) |> Async.RunSynchronously |> ignore
                    let consumer = $"""module Identity.Consumer
let optional (value: Identity.Root.{declaration}<string>) : (string -> string) option = value.fetch
"""
                    let code, output = compileConsumer run [ "root/Identity.Root.fs" ] consumer
                    Expect.equal code 0 output
                    let invalid = $"""module Identity.Consumer
type Invalid() = inherit Identity.Root.{declaration}<string>("seed")
"""
                    let code, output = compileConsumer run [ "root/Identity.Root.fs" ] invalid
                    Expect.notEqual code 0 "an ordinary or type-only class has no subclassable runtime constructor"
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let dependencyEntrypointPublicImportTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "dependency entrypoint public import skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "dependency entrypoint catalog retains a package's ordinary ambient public import" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-dependency-public-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "dependency-entrypoint-lab"))
                let owner = Path.Combine(package, "node_modules", "public-entrypoint-owner-lab")
                let configure name entry runtime references =
                    let file = Path.Combine(directory, name + ".json")
                    File.WriteAllText(file, JsonSerializer.Serialize {| ``module`` = "Identity." + name; entry = entry; runtime = runtime
                                                                        lib = [| "esnext" |]; types = ([||] : string array)
                                                                        groups = Map.ofList [ "public-entrypoint-owner-lab", "ship" ]
                                                                        declarationCatalog = true; declarationReferences = references |})
                    GeneratorConfig.loadFile file
                let root = Path.Combine(directory, "root")
                Pipeline.run (configure "Root" "index.d.ts" "public-entrypoint-owner-lab" ([||] : string array)) owner root |> Async.RunSynchronously |> ignore
                let adapter = configure "Adapter" "public-adapter.d.ts" "dependency-entrypoint-lab" [| Path.Combine(root, "declarations.json") |]
                Pipeline.run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let consumer = """module Identity.Consumer
let optional value : (string -> string) option = (Identity.Adapter.Exports.accept value).fetch
"""
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                Expect.equal code 0 output
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

[<Tests>]
let classInheritanceCatalogTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "class inheritance catalog skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "class inheritance catalog retains constructors and flattened members" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-class-inheritance-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-class-inheritance-lab"))
                let root = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) package root |> Async.RunSynchronously |> ignore
                let adapter = configured directory "Adapter" "adapter.d.ts" [| Path.Combine(root, "declarations.json") |]
                Pipeline.run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                let consumer = """module Identity.Consumer
let explicit () : Identity.Adapter.Explicit<string> = Identity.Adapter.Exports.Explicit "seed"
let implicit () : Identity.Adapter.Implicit<string> = Identity.Adapter.Exports.Implicit "seed"
let inherited (value: Identity.Adapter.Explicit<string>) : string = value.seed
let inheritedImplicit (value: Identity.Adapter.Implicit<string>) : string = value.seed
let extension (value: Identity.Adapter.Extension<string>) : string = value.seed
let optional (value: Identity.Adapter.Explicit<string>) : (string -> string) option = value.fetch
let own (value: Identity.Adapter.Explicit<string>) : string = value.send "message"
let ordinary (value: Identity.Adapter.PlainDerived<string>) : Identity.Adapter.Plain<string> = value :> _
type Actor() =
    inherit Identity.Root.ClassLab.Runtime.Actor<string>("seed")
    interface Identity.Root.ClassLab.Runtime.Actor.IFetchHandler<string> with
        member _.fetch value = value
"""
                let code, output = compileConsumer directory sources consumer
                Expect.equal code 0 output
                let invalid = """module Identity.Consumer
let invalid () : Identity.Adapter.Explicit<string> = Identity.Adapter.Exports.Explicit 1.0
"""
                let code, output = compileConsumer directory sources invalid
                Expect.notEqual code 0 "the constructor retains its inherited generic contract"
                Expect.stringContains output "FS0001" "a numeric seed cannot construct the string instance"
                let nominal = """module Identity.Consumer
let invalid (value: Identity.Adapter.Explicit<string>) : Identity.Root.ClassLab.Runtime.Actor<string> = value :> _
"""
                let code, output = compileConsumer directory sources nominal
                Expect.notEqual code 0 "flattened class bases do not claim a nominal upcast"
                Expect.stringContains output "FS0193" "the emitted interface cannot upcast to an abstract class"
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

let private authenticationTests compression =
    let run config package output = Pipeline.run (CatalogFixtures.references compression config package) package output
    let generate config package = Pipeline.generate (CatalogFixtures.references compression config package) package
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList ("declaration catalog " + string compression) [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList ("declaration catalog " + string compression) [
            testCase "generic result declarations preserve their own constraints across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-result-constraints-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-result-constraints-lab"))
                    let dependency = Path.Combine(package, "node_modules", "reader-owner-lab")
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let consumer = """module Identity.Consumer
open Fable.Core
let accept (reader: Identity.Root.Reader) : Identity.Root.Reader = Identity.Adapter.Exports.accept reader
let unconstrained (value: Identity.Root.ReadResult2<string>) : string = value.value
let read<'T when 'T :> JS.ArrayBufferView> (reader: Identity.Root.Reader) (view: 'T)
    : JS.Promise<U2<Identity.Root.ReadResult2<'T>, Identity.Root.ReadResult3>> = reader.read view
let readAtLeast<'T when 'T :> JS.ArrayBufferView> (reader: Identity.Root.Reader) (view: 'T)
    : JS.Promise<U2<Identity.Root.ReadResult2<'T>, Identity.Root.ReadResult3>> = reader.readAtLeast(1.0, view)
let bound<'T when 'T :> JS.ArrayBufferView> (reader: Identity.Root.Reader) (view: 'T)
    : JS.Promise<U2<Identity.Root.BoundResult2<'T>, Identity.Root.BoundResult3>> = reader.readBound view
"""
                    let code, output = compileConsumer directory sources consumer
                    Expect.equal code 0 output
                    let invalidDeclaration = """module Identity.Consumer
let invalid (value: Identity.Root.BoundResult2<string>) = value
"""
                    let code, output = compileConsumer directory sources invalidDeclaration
                    Expect.notEqual code 0 "the result's own declared bound remains enforced"
                    Expect.stringContains output "FS0001" "string is outside the declared buffer bound"
                    let invalidCall = """module Identity.Consumer
let invalid (reader: Identity.Root.Reader) = reader.read "text"
"""
                    let code, output = compileConsumer directory sources invalidCall
                    Expect.notEqual code 0 "the method's own generic bound remains enforced"
                    Expect.stringContains output "FS0001" "the caller must supply a buffer view"
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "dependency generic markers retain producer phantom contracts" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-generic-marker-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "generic-marker-catalog-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let dependency = Path.Combine(package, "node_modules", "generic-marker-owner-lab")
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let consumer = """module Identity.Consumer
let accept (value: Identity.Root.Options<string>) : Identity.Root.Options<string> = Identity.Adapter.Exports.accept value
let marker (value: Identity.Root.Options<string>) : Identity.Root.Marker<string> = value.marker
let setMarker (value: Identity.Root.Options<string>) (marker: Identity.Root.Marker<string>) = value.marker <- marker
let loose (value: Identity.Root.LooseOptions) (candidate: obj) = value.value <- candidate
"""
                    let code, output = compileConsumer directory sources consumer
                    Expect.equal code 0 output
                    let wrongMarker = """module Identity.Consumer
let wrong (value: Identity.Root.Options<string>) (marker: Identity.Root.Marker<int>) = value.marker <- marker
"""
                    let code, output = compileConsumer directory sources wrongMarker
                    Expect.notEqual code 0 "different marker arguments remain incompatible"
                    Expect.stringContains output "FS0001" "the wrong marker type is rejected without a cast"
                    File.AppendAllText(Path.Combine(dependency, "index.d.ts"), "\n")
                    Expect.throwsC
                        (fun () -> run adapter package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "marker declarations remain authenticated")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "local obj aliases normalize shared erased unions without erasing real contracts" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-empty-union-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "empty-union-alias-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let dependency = Path.Combine(package, "node_modules", "empty-union-owner-lab")
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
let accept (value: Identity.Root.Value) : Identity.Root.Value = Identity.Adapter.Exports.accept value
let property (value: Identity.Root.Value) : obj option = value.event
let setProperty (value: Identity.Root.Value) (event: obj option) = value.event <- event
let aliased (value: Identity.Root.AliasedValue) : Identity.Root.AliasedValue = Identity.Adapter.Exports.aliased value
let setAliased (value: Identity.Root.AliasedValue) (event: obj) = value.event <- event
let callable (value: Identity.Root.Controls) = match value.callable with U2.Case1 _ -> () | U2.Case2 _ -> ()
let indexed (value: Identity.Root.Controls) = match value.indexed with U2.Case1 _ -> () | U2.Case2 _ -> ()
let inherited (value: Identity.Root.Controls) = match value.inherited with U2.Case1 _ -> () | U2.Case2 _ -> ()
let generic (value: Identity.Root.Controls) = match value.generic with U2.Case1 _ -> () | U2.Case2 _ -> ()
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                    let symbols = File.ReadAllText(Path.Combine(root, "symbols.jsonl"))
                    Expect.stringContains symbols "TR035" "the producer reports the existing obj-union widening"
                    File.AppendAllText(Path.Combine(dependency, "index.d.ts"), "\n")
                    Expect.throwsC
                        (fun () -> run adapter package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "normalization does not weaken source authentication")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "anonymous literal enums retain identity beside named aliases across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-literal-alias-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "literal-alias-identity-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let dependency = Path.Combine(package, "node_modules", "literal-alias-owner-lab")
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(root, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                    let mode = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.Mode")
                    let reversed = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.ReversedMode")
                    let nominal = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.Nominal")
                    Expect.notEqual mode.Identity reversed.Identity "named literal aliases retain distinct declaration owners"
                    Expect.notEqual mode.Identity nominal.Identity "TypeScript enum declarations retain nominal identity"
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let accept (value: Identity.Root.Value) (mode: Identity.Root.Value.Kind) : Identity.Root.Value = Identity.Adapter.Exports.accept(value, mode)
let property (value: Identity.Root.Value) : Identity.Root.Value.Kind option = value.kind
let nominal (value: Identity.Root.Nominal) : Identity.Root.Nominal = Identity.Adapter.Exports.nominal value
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                    File.AppendAllText(Path.Combine(dependency, "index.d.ts"), "\n")
                    Expect.throwsC
                        (fun () -> run adapter package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "shared alias input remains authenticated")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "NonNullable recursive aliases retain canonical payload types across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-recursive-json-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-recursive-json-lab"))
                    let dependency = Path.Combine(package, "node_modules", "recursive-json-owner-lab")
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
let accept (output: Identity.Root.Output) : Identity.Root.Output = Identity.Adapter.Exports.accept output
let result (output: Identity.Root.Output) : U3<string, float, Identity.Root.Value[]> =
    match output with
    | Identity.Root.Output.Result result -> result
    | Identity.Root.Output.Text text -> U3.Case1 text
let nullable (value: Identity.Root.Value) : U3<string, float, obj[]> option = value
let array (values: Identity.Root.Value[]) : Identity.Root.Output =
    Identity.Adapter.Exports.accept (Identity.Root.Output.Result (U3.Case3 values))
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "anonymous parent roles do not add consumer sources to shared declarations" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-anonymous-parent-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "anonymous-parent-source-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let dependency = Path.Combine(package, "node_modules", "optional-array-shared-lab")
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    let output = Path.Combine(directory, "adapter")
                    run adapter package output |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let shared (value: Identity.Adapter.Container) : Identity.Root.Shared = value.cache
let first (value: Identity.Root.Shared) : string option = value.values |> Option.map (fun values -> values.[0])
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                    File.AppendAllText(Path.Combine(dependency, "index.d.ts"), "\n")
                    Expect.throwsC
                        (fun () -> run adapter package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "shared declaration sources remain authenticated")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "nullable aliases retain tagged unions and one option layer across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-alias-api-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-alias-api-lab"))
                    let dependency = Path.Combine(package, "node_modules", "alias-api-owner-lab")
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root" }
                    let root = Path.Combine(directory, "root")
                    run config dependency root |> Async.RunSynchronously |> ignore
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
let accept (client: Identity.Adapter.Client) (part: Identity.Root.Part) : Identity.Root.Part =
    client.accept part
let generate (client: Identity.Adapter.Client) (options: Identity.Root.Options) : JS.Promise<Identity.Root.Part> =
    client.generate options
let choice (options: Identity.Root.Options) : Identity.Root.Choice option = options.choice
let payload (output: Identity.Root.Output) : U2<string, float> option =
    match output with
    | Identity.Root.Output.Json value -> value
    | Identity.Root.Output.Text _ -> None
let json (value: Identity.Root.Value) : Identity.Root.Output = Identity.Root.Output.Json value
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "producer source closure is authenticated against the consumer program" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-source-closure-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let root = configured directory "Root" "index.d.ts" [||]
                    let producer = Path.Combine(directory, "root")
                    run root fixture producer |> Async.RunSynchronously |> ignore
                    let path = Path.Combine(producer, "declarations.json")
                    let catalog = JsonNode.Parse(File.ReadAllText path)
                    let client = catalog["declarations"].AsArray() |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.PublicClient")
                    let sources = client["sources"].AsArray()
                    // A producer may traverse more of a shared type than the consumer. The
                    // extra dependency is still authenticated, even though its reachability
                    // through the consumer's resolved graph is different.
                    let extra = catalog["inputs"].AsArray() |> Seq.find (fun input -> input["file"].GetValue<string>() = "lib/lib.es5.d.ts")
                    Expect.isTrue (sources |> Seq.forall (fun source -> not (JsonNode.DeepEquals(source, extra))))
                        "the common compiler input is outside this declaration's closure"
                    sources.Add(extra.DeepClone())
                    File.WriteAllText(path, catalog.ToJsonString())
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| path |]
                    run adapter fixture (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    sources[sources.Count - 1]["sha256"] <- JsonValue.Create "changed"
                    File.WriteAllText(path, catalog.ToJsonString())
                    Expect.throwsC
                        (fun () -> run adapter fixture (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "source hash mismatch" "the producer's additional source is authenticated")
                    sources.RemoveAt(sources.Count - 1)
                    let producerOnly = catalog["inputs"].AsArray() |> Seq.find (fun input ->
                        input["package"].GetValue<string>() = "declaration-identity-lab"
                        && input["file"].GetValue<string>() = "index.d.ts")
                    sources.Add(producerOnly.DeepClone())
                    File.WriteAllText(path, catalog.ToJsonString())
                    Expect.throwsC
                        (fun () -> run adapter fixture (Path.Combine(directory, "absent")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "source hash mismatch" "a source absent from the consumer cannot authenticate the producer")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "independent record aliases share canonical sources and preserve input authentication" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-record-alias-source-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "record-alias-source-identity-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    run config package root |> Async.RunSynchronously |> ignore
                    let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(root, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                    let record = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.PipelineRecord")
                    let modelRecord = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.ModelRecord")
                    Expect.isFalse (record.Sources |> Array.exists (fun source -> source.Package = "record-alias-source-identity-lab")) "transparent alias application is not a canonical declaration source"
                    Expect.isTrue (modelRecord.Sources |> Array.exists (fun source -> source.Package = "record-model-lab")) "the applied model declaration remains authenticated"
                    Expect.isTrue (catalog.Inputs |> Array.exists (fun source -> source.Package = "record-alias-source-identity-lab" && source.File = "index.d.ts")) "alias source remains a catalog input"
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    let consumerPackage = Path.Combine(package, "consumer")
                    run adapter consumerPackage (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let echo (value: Identity.Root.PipelineRecord) : Identity.Root.PipelineRecord = Identity.Adapter.Exports.echo value
let alias (value: Identity.Adapter.OutboundHandlerParams) : Identity.Root.PipelineRecord = value
let model (value: Identity.Adapter.ModelParams) : Identity.Root.ModelRecord = value
let text (value: Identity.Adapter.TextParams) (key: string) : string = value.[key]
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                    let aliasFile = Path.Combine(package, "index.d.ts")
                    let aliasSource = File.ReadAllText aliasFile
                    File.AppendAllText(aliasFile, "\n")
                    File.AppendAllText(Path.Combine(consumerPackage, "index.d.ts"), "\nimport '../index';\n")
                    Expect.throwsC
                        (fun () -> run adapter consumerPackage (Path.Combine(directory, "stale-alias")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "reachable alias input changes still invalidate the producer")
                    File.WriteAllText(aliasFile, aliasSource)
                    writePackageFile package "node_modules/record-model-lab/index.d.ts" "export interface Model { value: number }\n"
                    Expect.throwsC
                        (fun () -> run adapter consumerPackage (Path.Combine(directory, "stale-model")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "changed semantic dependencies still invalidate the producer")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "primitive aliases preserve nested declaration identities across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-primitive-argument-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "primitive-argument-identity-lab"))
                    let dependency = Path.Combine(package, "node_modules", "intrinsic-owner-lab")
                    let configure name references =
                        let path = Path.Combine(directory, name + ".json")
                        File.WriteAllText(path,
                            JsonSerializer.Serialize {| ``module`` = "Identity." + name; ``namespace`` = "Identity.Support"
                                                        lib = [| "esnext" |]; types = [| "intrinsic-owner-lab" |]
                                                        groups = Map.ofList [ "intrinsic-owner-lab", "ship" ]
                                                        declarationCatalog = true; declarationReferences = references |})
                        GeneratorConfig.loadFile path
                    let root = Path.Combine(directory, "root")
                    run (configure "Root" ([||] : string array)) dependency root |> Async.RunSynchronously |> ignore
                    let adapter = configure "Adapter" [| Path.Combine(root, "declarations.json") |]
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let reuse (value: Identity.Root.Item) : Identity.Root.Item = Identity.Adapter.Exports.``use`` value
let text (value: Identity.Root.Item) : string = value.nested.value
let number (value: Identity.Root.Item) : float = value.nested.count
let enabled (value: Identity.Root.Item) : bool = value.nested.enabled
let textAlias (value: Identity.Root.StringAlias) : string = value
let numberAlias (value: Identity.Root.NumberAlias) : float = value
let booleanAlias (value: Identity.Root.BooleanAlias) : bool = value
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "grouped DOM aliases retain reusable dependency ownership" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-grouped-dom-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "grouped-dom-aliases-lab"))
                    let config =
                        { GeneratorConfig.loadFile (Path.Combine(package, "xantham.json")) with
                            ModuleName = Some "Identity.Root"
                            Namespace = Some "Identity"
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    run config package root |> Async.RunSynchronously |> ignore
                    let adapterConfig =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapterConfig package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Identity.WorkerAugmentationLab
let cache (request: Request) : RequestCache =
    (Identity.Adapter.Exports.roundTrip (Identity.Root.Exports.roundTrip request)).cache
let metadata (request: Request) : string = request.cf
"""
                    let sources = [ "root/groups/Identity.WorkerAugmentationLab.fs"; "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let code, output = compileConsumer directory sources consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "indexed callbacks retain reusable parent identities" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-indexed-callback-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "indexed-callback-lab"))
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    run config package root |> Async.RunSynchronously |> ignore
                    let adapterConfig =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapterConfig package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let reuse (value: Identity.Adapter.Handlers<obj, string>) : Identity.Root.Handlers<obj, string> = value
let callback (value: Identity.Adapter.Handlers<obj, string>) : Identity.Root.BivarianceHack<string> option =
    value.["fetch"]
let invoke (value: Identity.Root.BivarianceHack<string>) = value.Invoke("environment", null)
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "nested module manifests retain installed ownership and invalidate stale catalogs" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-package-submanifest-" + Guid.NewGuid().ToString "N")
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                try
                    let source = Path.GetFullPath(Path.Combine(fixture, "..", "package-submanifest-lab"))
                    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        writePackageFile package (Path.GetRelativePath(source, file)) (File.ReadAllText file)
                    writePackageFile package "node_modules/@scope/catalog-lab/package.json" """{"name":"@scope/catalog-lab","version":"4.5.6","types":"index.d.ts"}"""
                    writePackageFile package "node_modules/@scope/catalog-lab/index.d.ts" "export interface Scoped { scoped: string }"
                    writePackageFile package "node_modules/package-owner-lab/subpath/node_modules/inner-lab/package.json" """{"name":"inner-lab","version":"7.8.9","types":"index.d.ts"}"""
                    writePackageFile package "node_modules/package-owner-lab/subpath/node_modules/inner-lab/index.d.ts" "export interface Inner { inner: string }"
                    File.AppendAllText(Path.Combine(package, "index.d.ts"), "\nexport { Scoped } from '@scope/catalog-lab';\nexport { Inner } from 'package-owner-lab/subpath';\n")
                    File.AppendAllText(Path.Combine(package, "node_modules/package-owner-lab/subpath/index.d.ts"), "\nexport { Inner } from 'inner-lab';\n")
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    run config package root |> Async.RunSynchronously |> ignore
                    let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(root, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                    let owners = catalog.Inputs |> Array.map (fun source -> source.Package, source.Version, source.File)
                    Expect.contains owners ("package-owner-lab", "1.2.3", "subpath/index.d.ts") "subpath belongs to its installed package"
                    Expect.contains owners ("@scope/catalog-lab", "4.5.6", "index.d.ts") "scoped installation is its own owner"
                    Expect.contains owners ("inner-lab", "7.8.9", "index.d.ts") "nested node_modules starts a new owner"
                    let adapter =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    File.AppendAllText(Path.Combine(package, "node_modules/package-owner-lab/subpath/package.json"), "\n")
                    Expect.throwsC
                        (fun () -> run adapter package (Path.Combine(directory, "stale")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "package manifest mismatch" "nested resolution metadata is authenticated")
                    writePackageFile package "node_modules/package-owner-lab/package.json" """{"name":"package-owner-lab"}"""
                    Expect.throwsC
                        (fun () -> run config package (Path.Combine(directory, "unversioned")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "must declare its version" "installed packages still require versions")
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "contextual constructor bounds remain distinct and reusable" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-constructor-bounds-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-constructor-bounds-lab"))
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    let adapter = Path.Combine(directory, "adapter")
                    run config package root |> Async.RunSynchronously |> ignore
                    let adapterConfig =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapterConfig package adapter |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let broad<'T when 'T :> Identity.Root.Base> (factory: Identity.Root.Factory<'T>) : 'T = factory.Create()
let narrow<'T when 'T :> Identity.Root.Derived> (factory: Identity.Root.Create.FactoryConstructor<'T>) : 'T =
    Identity.Adapter.Exports.create factory
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "readonly dependency enums keep distinct parent identities" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-readonly-enums-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "readonly-enums-lab"))
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let root = Path.Combine(directory, "root")
                    run config package root |> Async.RunSynchronously |> ignore
                    let adapterConfig =
                        { config with
                            ModuleName = Some "Identity.Adapter"
                            DeclarationReferences = [ Path.Combine(root, "declarations.json") ] }
                    run adapterConfig package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let view = Identity.Root.View.Create(Identity.Root.Permissions.Access.Allow, Identity.Root.Permissions.Waiting.Manual)
let reuse (value: Identity.Adapter.View) : Identity.Root.View = value
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    Expect.equal code 0 output
                finally
                    if Directory.Exists directory then Directory.Delete(directory, true)

            testCase "augmented DOM types and global function values compile against Core.TS" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-dom-augmentation-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "augmented-dom-lab"))
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext"; "dom" ]
                            Types = Some [] }
                    run config package (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
let value (request: Identity.Root.Request<string>) : string option = request.cf
let options (init: Identity.Root.RequestInit<float>) : float option = init.cf
let plain () : Fable.Core.TS.Dom.Response = Identity.Root.Exports.plain()
let invoke (request: Identity.Root.Request<Identity.Root.Request.Value.Item>)
           (init: Identity.Root.RequestInit<Identity.Root.Request.Init.Item>) : JS.Promise<Fable.Core.TS.Dom.Response> =
    let fetch = Identity.Root.Exports.request(request, init)
    fetch.Invoke(U3.Case1 "https://example.invalid/", None)
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "subpath function results retain their qualified parent identity" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-function-result-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-subpath-lab"))
                    let root =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let producer = Path.Combine(directory, "root")
                    run root package producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    let adapter = { root with ModuleName = Some "Identity.Adapter"; DeclarationReferences = [ reference ] }
                    run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
open Identity.Root.Chat.Create.Result
let item (key: string) : U4<Item, Item2, Item3, Item4> =
    Identity.Adapter.Chat.Exports.create().[key]
let title (key: string) : string option =
    match item key with
    | U4.Case1 value -> value.title
    | U4.Case2 value -> value.title
    | U4.Case3 value -> value.title
    | U4.Case4 value -> value.title
let options (value: Identity.Root.Chat.LegacyOptions) : Identity.Root.Chat.Options = value
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "nested class aliases retain their constructor and static value surface" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-nested-class-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"nested-class-lab","version":"1.0.0","exports":{"./client":{"types":"./client.d.ts"}}}"""
                    writePackageFile directory "client.d.ts" """export declare class Client {
    constructor(value: string);
    static create(value: string): Client;
    static count: number;
    readonly value: string;
}
"""
                    let root =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let producer = Path.Combine(directory, "root")
                    run root directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    let adapter = { root with ModuleName = Some "Identity.Adapter"; DeclarationReferences = [ reference ] }
                    run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let construct () : Identity.Root.Client.Client = Identity.Adapter.Client.Client.Create "value"
let create () : Identity.Root.Client.Client = Identity.Adapter.Client.Client.create "value"
let count () : float = Identity.Adapter.Client.Client.count
let setCount () = Identity.Adapter.Client.Client.count <- 2.0
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "subpath containers retain local imports while their types reuse a catalog" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-subpaths-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let package = Path.GetFullPath(Path.Combine(fixture, "..", "subpath-lab"))
                    let root =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let producer = Path.Combine(directory, "root")
                    run root package producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    let adapter = { root with ModuleName = Some "Identity.Adapter"; DeclarationReferences = [ reference ] }
                    let consumerDir = Path.Combine(directory, "adapter")
                    run adapter package consumerDir |> Async.RunSynchronously |> ignore
                    let source = File.ReadAllText(Path.Combine(consumerDir, "Identity.Adapter.fs"))
                    for specifier in [ "subpath-lab"; "subpath-lab/client"; "subpath-lab/client/deep"; "subpath-lab/alias"; "subpath-lab/mirror" ] do
                        Expect.stringContains source ("\"" + specifier + "\"") "each value container retains its own runtime imports"
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    let names = catalog.RootElement.GetProperty("declarations").EnumerateArray() |> Seq.map (fun d -> d.GetProperty("fSharpName").GetString()) |> Seq.toList
                    Expect.isFalse (names |> List.exists (fun name -> name.EndsWith ".Exports")) "value containers are not reusable type declarations"
                    let consumer = """module Identity.Consumer
let shared (payload: Identity.Root.Payload) : string = Identity.Adapter.Client.Exports.describe payload
let connect (options: Identity.Adapter.Client.ClientOptions) : Identity.Root.Internal =
    Identity.Adapter.Client.Exports.connect options
let deep () = Identity.Adapter.Client.Deep.Exports.depth ()
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "opaque specializations retain declaration arguments" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-opaque-arguments-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"opaque-arguments-lab","version":"1.0.0"}"""
                    writePackageFile directory "node_modules/identity-dep/package.json" """{"name":"identity-dep","version":"1.0.0","types":"index.d.ts"}"""
                    writePackageFile directory "node_modules/identity-dep/context.d.ts" """export interface Context { source: string; }"""
                    writePackageFile directory "node_modules/identity-dep/index.d.ts" """import type { Context } from "./context.js";
export type Result<T, R = Context> = { item: T; context: R };
export type Callback<T, R = Context> = (event: Result<T, R>) => void;
export type Forward<A, B> = Result<A, B>;
export type Reverse<A, B> = Result<B, A>;
export type Repeated<T> = Result<T, T>;
"""
                    writePackageFile directory "index.d.ts" """import type { Callback } from "identity-dep";
export { Result, Forward, Reverse, Repeated } from "identity-dep";
export type Current<T> = Parameters<Callback<T>>[0];
export type Alpha<U> = Parameters<Callback<U>>[0];
export type Numeric<T> = Parameters<Callback<T, number>>[0];
"""
                    writePackageFile directory "adapter.d.ts" """export { Result, Current, Alpha, Numeric, Forward, Reverse, Repeated } from "./index.js";"""
                    let root = configured directory "Root" "index.d.ts" [||]
                    run root directory (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(directory, "root", "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    let alpha =
                        catalog.RootElement.GetProperty("declarations").EnumerateArray()
                        |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Alpha")
                    alpha.GetProperty("sources").EnumerateArray()
                    |> Seq.exists (fun source -> source.GetProperty("file").GetString() = "context.d.ts")
                    |> Flip.Expect.equal "the concrete default's source remains in the specialization closure" true
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| reference |]
                    run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let alpha<'T> (value: Identity.Root.Current<'T>) : Identity.Root.Alpha<'T> = value
let share<'T> (value: Identity.Root.Current<'T>) : Identity.Adapter.Current<'T> = value
let numeric<'T> (value: Identity.Root.Numeric<'T>) : Identity.Adapter.Numeric<'T> = value
let forward<'A, 'B> (value: Identity.Root.Forward<'A, 'B>) : Identity.Adapter.Forward<'A, 'B> = value
let reverse<'A, 'B> (value: Identity.Root.Reverse<'A, 'B>) : Identity.Adapter.Reverse<'A, 'B> = value
let repeated<'T> (value: Identity.Root.Repeated<'T>) : Identity.Adapter.Repeated<'T> = value
"""
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let code, output = compileConsumer directory sources consumer
                    code |> Flip.Expect.equal output 0
                    for invalid in [
                        "let invalid (value: Identity.Root.Numeric<string>) : Identity.Root.Current<string> = value"
                        "let invalid (value: Identity.Root.Reverse<string, float>) : Identity.Root.Forward<string, float> = value"
                        "let invalid (value: Identity.Root.Repeated<string>) : Identity.Root.Current<string> = value"
                    ] do
                        let code, output = compileConsumer directory sources (consumer + "\n" + invalid + "\n")
                        (code <> 0 && output.Contains "FS0001") |> Flip.Expect.equal (invalid + "\n" + output) true
                finally Directory.Delete(directory, true)

            testCase "contextual bounds do not redeclare generic result members" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-contextual-bound-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"contextual-bound-lab","version":"1.0.0"}"""
                    writePackageFile directory "index.d.ts" """export interface Bound { bytes: number; }
export type Result<R = any> = { done: false; value: R } | { done: true; value?: undefined };
export type Pair<A, B> = { left: A; right: B } | { done: true };
export interface Reader {
    read<T extends Bound>(value: T): Result<T>;
    duplicate<T extends Bound>(value: T): Pair<T, T>;
    reverse<A, B>(left: A, right: B): Pair<B, A>;
}
"""
                    let root = configured directory "Root" "index.d.ts" [||]
                    run root directory (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let read<'T when 'T :> Identity.Root.Bound> (reader: Identity.Root.Reader) (value: 'T) : Identity.Root.Result<'T> =
    reader.read value
let duplicate<'T when 'T :> Identity.Root.Bound> (reader: Identity.Root.Reader) (value: 'T) : Identity.Root.Pair<'T, 'T> =
    reader.duplicate value
let reverse<'A, 'B> (reader: Identity.Root.Reader) (left: 'A) (right: 'B) : Identity.Root.Pair<'B, 'A> =
    reader.reverse(left, right)
"""
                    let sources = [ "root/Identity.Root.fs" ]
                    let code, output = compileConsumer directory sources consumer
                    code |> Flip.Expect.equal output 0
                    let code, output = compileConsumer directory sources (consumer + "\nlet invalid (reader: Identity.Root.Reader) = reader.read \"unbounded\"\n")
                    (code <> 0 && output.Contains "FS0001") |> Flip.Expect.equal output true
                finally Directory.Delete(directory, true)

            testCase "transparent aliases preserve API identity across entry points" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-transparent-alias-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"transparent-alias-lab","version":"1.0.0"}"""
                    writePackageFile directory "shared.d.ts" """export type Message = string | number;
export interface Peer { send(message: Message): void; }
"""
                    writePackageFile directory "index.d.ts" """export * from "./shared.js";"""
                    writePackageFile directory "adapter.d.ts" """export { Peer } from "./shared.js";"""
                    let root = configured directory "Root" "index.d.ts" [||]
                    run root directory (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| Path.Combine(directory, "root", "declarations.json") |]
                    run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
open Fable.Core
let send (peer: Identity.Adapter.Peer) (message: Identity.Root.Message) = peer.send message
let share (peer: Identity.Adapter.Peer) : Identity.Root.Peer = peer
"""
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let code, output = compileConsumer directory sources consumer
                    code |> Flip.Expect.equal output 0
                    let code, output = compileConsumer directory sources (consumer + "\nlet invalid (peer: Identity.Adapter.Peer) = peer.send true\n")
                    (code <> 0 && output.Contains "FS0001") |> Flip.Expect.equal output true
                finally Directory.Delete(directory, true)

            testCase "generic alias applications retain their declaration owner" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-alias-applications-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let input = Path.GetFullPath(Path.Combine(fixture, "..", "default-intersection-lab"))
                    let root = configured directory "Root" "index.d.ts" [||]
                    run root input (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(directory, "root", "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.filter (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Connection")
                    |> Seq.length
                    |> Flip.Expect.equal "one owner for the generic declaration and its default applications" 1
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| reference |]
                    run adapter input (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let connect (agent: Identity.Adapter.Agent) (connection: Identity.Root.Connection<obj>) =
    agent.onConnect connection
let share (agent: Identity.Adapter.Agent) : Identity.Root.Agent = agent
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "generic tagged unions retain their arity across packages" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-generic-tag-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let input = Path.GetFullPath(Path.Combine(fixture, "..", "generic-tag-lab"))
                    let root = configured directory "Root" "index.d.ts" [||]
                    run root input (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(directory, "root", "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    let job =
                        catalog.RootElement.GetProperty("declarations").EnumerateArray()
                        |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Job")
                    job.GetProperty("arity").GetInt32() |> Flip.Expect.equal "the union's parameter is part of its arity" 1
                    job.GetProperty("constraints").GetArrayLength() |> Flip.Expect.equal "one constraint slot per parameter" 1
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| reference |]
                    run adapter input (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let once (payload: string) : Identity.Root.Job<string> = Identity.Root.Job.Once("id", payload, 1.0)
let reexported (job: Identity.Adapter.Job<string>) : Identity.Root.Job<string> = job
let run (runner: Identity.Adapter.Runner<string>) (job: Identity.Root.Job<string>) : Identity.Root.Job<string> = runner.run job
let poll (runner: Identity.Adapter.Runner<string>) : Identity.Root.Job<string> option = runner.poll ()
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "renamed exports and generic subpath types share producer identity" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-declaration-catalog-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let root = configured directory "Root" "index.d.ts" [||]
                    let producer = Path.Combine(directory, "root")
                    run root fixture producer |> Async.RunSynchronously |> ignore
                    let catalog = Path.Combine(producer, "declarations.json")
                    File.Exists catalog |> Flip.Expect.equal "producer emits its declaration catalog" true
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| catalog |]
                    run adapter fixture (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let next = configured directory "Next" "next.d.ts" [| Path.GetRelativePath(fixture, Path.Combine(directory, "adapter", "declarations.json")) |]
                    run next fixture (Path.Combine(directory, "next")) |> Async.RunSynchronously |> ignore
                    use chained = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, "next", "declarations.json")))
                    chained.RootElement.GetProperty("owners").EnumerateArray()
                    |> Seq.map (fun owner -> owner.GetProperty("name").GetString())
                    |> Seq.toList
                    |> Flip.Expect.equal "inherited owners precede consumers" [ "Identity.Root"; "Identity.Adapter"; "Identity.Next" ]
                    let exitCode, output = compileConsumer directory sharedSources sharedConsumer
                    exitCode |> Flip.Expect.equal output 0
                    let invalid = sharedConsumer + "\nlet invalid (callback: Identity.Adapter.RightCallback) (left: Identity.Root.Left) = callback.run left\n"
                    let exitCode, output = compileConsumer directory sharedSources invalid
                    (exitCode <> 0 && output.Contains "FS0001") |> Flip.Expect.equal output true
                finally
                    Directory.Delete(directory, true)

            testCase "private producer ownership preserves a later class value export" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-statics-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let root = configured directory "Root" "static-root.d.ts" [||]
                    run root fixture (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(directory, "root", "declarations.json")
                    let adapter = configured directory "Adapter" "static-adapter.d.ts" [| reference |]
                    run adapter fixture (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] staticConsumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)

            testCase "unchanged nested dependency versions remain separate inputs" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-versions-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    // Each `dep` installation resolves a different `peer` version, which gives the
                    // copies distinct compiler package identities at equal versions.
                    let dependencyManifest version =
                        $"""{{"name":"dep","version":"{version}","types":"index.d.ts","peerDependencies":{{"peer":"*"}}}}"""
                    writePackageFile directory "package.json" """{"name":"versions-lab","version":"1.0.0","type":"module"}"""
                    writePackageFile directory "index.d.ts" """export { One } from "dep"; export { Two } from "owner";"""
                    writePackageFile directory "adapter.d.ts" """import { One, Two } from "./index.js"; export function one(value: One): One; export function two(value: Two): Two;"""
                    writePackageFile directory "node_modules/dep/package.json" (dependencyManifest "1.0.0")
                    writePackageFile directory "node_modules/dep/index.d.ts" "export interface One { one: string; } export interface Two { two: number; }"
                    writePackageFile directory "node_modules/peer/package.json" """{"name":"peer","version":"1.0.0","types":"index.d.ts"}"""
                    writePackageFile directory "node_modules/peer/index.d.ts" "export {};"
                    writePackageFile directory "node_modules/owner/package.json" """{"name":"owner","version":"1.0.0","types":"index.d.ts"}"""
                    writePackageFile directory "node_modules/owner/index.d.ts" """export { Two } from "dep";"""
                    writePackageFile directory "node_modules/owner/node_modules/dep/package.json" (dependencyManifest "2.0.0")
                    writePackageFile directory "node_modules/owner/node_modules/dep/index.d.ts" "export interface One { one: string; } export interface Two { two: number; extra?: boolean; }"
                    writePackageFile directory "node_modules/owner/node_modules/peer/package.json" """{"name":"peer","version":"2.0.0","types":"index.d.ts"}"""
                    writePackageFile directory "node_modules/owner/node_modules/peer/index.d.ts" "export {};"
                    let groups = Map.ofList [ "dep" * uom<npmDependency>, Ship; "owner" * uom<npmDependency>, Ship ]
                    let root = { configured directory "Root" "index.d.ts" [||] with Groups = groups }
                    run root directory (Path.Combine(directory, "root")) |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(directory, "root", "declarations.json")
                    let adapter = { configured directory "Adapter" "adapter.d.ts" [| reference |] with Groups = groups }
                    run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("inputs").EnumerateArray()
                    |> Seq.filter (fun source -> source.GetProperty("package").GetString() = "dep")
                    |> Seq.map (fun source -> source.GetProperty("version").GetString())
                    |> Seq.distinct |> Seq.sort |> Seq.toList
                    |> Flip.Expect.equal "dependency versions keep separate provenance" [ "1.0.0"; "2.0.0" ]
                    // The second installation now claims the first version with different bytes.
                    // A matching copy must not authenticate the conflicting one.
                    writePackageFile directory "node_modules/owner/node_modules/dep/package.json" (dependencyManifest "1.0.0")
                    let unreferenced = { configured directory "Inputs" "adapter.d.ts" [||] with Groups = groups }
                    run unreferenced directory (Path.Combine(directory, "inputs")) |> Async.RunSynchronously |> ignore
                    use inputs = JsonDocument.Parse(File.ReadAllText(Path.Combine(directory, "inputs", "declarations.json")))
                    let copies =
                        inputs.RootElement.GetProperty("inputs").EnumerateArray()
                        |> Seq.filter (fun source -> source.GetProperty("package").GetString() = "dep")
                        |> Seq.map (fun source ->
                            source.GetProperty("version").GetString(),
                            source.GetProperty("file").GetString(),
                            source.GetProperty("sha256").GetString())
                        |> Seq.toList
                    copies |> List.map (fun (version, file, _) -> version, file)
                    |> Flip.Expect.equal "both installations enter the adapter's program under one source key" [ "1.0.0", "index.d.ts"; "1.0.0", "index.d.ts" ]
                    copies |> List.map (fun (_, _, sha256) -> sha256) |> List.distinct |> List.length
                    |> Flip.Expect.equal "the same-version installations differ in content" 2
                    Expect.throwsC
                        (fun () -> run adapter directory (Path.Combine(directory, "conflicting-copy")) |> Async.RunSynchronously |> ignore)
                        (fun error -> Expect.stringContains error.Message "input source hash mismatch" "a matching copy cannot authenticate conflicting same-version bytes")
                finally Directory.Delete(directory, true)

            let inline (==>) input expected = input, expected
            testTheory "only suppressed unresolved imports may certify a catalog" [
                "" ==> false
                "// @ts-ignore\n" ==> true
            ] <| fun (prefix, succeeds) ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-missing-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"missing-lab","version":"1.0.0","type":"module"}"""
                    writePackageFile directory "index.d.ts" (prefix + "import { Missing } from 'missing-catalog-provider'; export function use(value: Missing): void;")
                    let config = { configured directory "Root" "index.d.ts" [||] with Types = None }
                    let output = Path.Combine(directory, "output")
                    let message = try run config directory output |> Async.RunSynchronously |> ignore; "" with error -> error.Message
                    (Directory.Exists output, message.Contains "TS2307") |> Flip.Expect.equal message (succeeds, not succeeds)
                finally Directory.Delete(directory, true)

            let inline (=!>) mutation expected = mutation, expected
            testTheory "incompatible producers fail before output is written" [
                "compiler" =!> "different compiler version"
                "generator" =!> "different identity version"
                "inferenceProfile" =!> "different inference profile"
                "manifest" =!> "package manifest mismatch"
                "input" =!> "input source hash mismatch"
                "api" =!> "F# API mismatch"
                "source" =!> "source hash mismatch"
                "source-missing" =!> "source hash mismatch"
                "source-removed" =!> "source hash mismatch"
                "arity" =!> "arity mismatch"
                "constraints" =!> "constraint mismatch"
                "cycle" =!> "owner dependency cycle"
                "merged" =!> "declaration handle set"
            ] <| fun (mutation, expected) ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-conflict-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    let root = configured directory "Root" "index.d.ts" [||]
                    let producer = Path.Combine(directory, "root")
                    run root fixture producer |> Async.RunSynchronously |> ignore
                    let path = Path.Combine(producer, "declarations.json")
                    let catalog = JsonNode.Parse(File.ReadAllText path)
                    let declarations = catalog["declarations"].AsArray()
                    let client = declarations |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.PublicClient")
                    let box = declarations |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.PublicBox")
                    let sharedInput =
                        catalog["inputs"].AsArray()
                        |> Seq.find (fun input -> input["file"].GetValue<string>() = "lib/lib.es5.d.ts")
                    match mutation with
                    | "compiler" ->
                        let compatibility = catalog["compatibility"]
                        let compiler = compatibility["compiler"]
                        compiler["version"] <- JsonValue.Create "incompatible"
                    | "generator" ->
                        let compatibility = catalog["compatibility"]
                        compatibility["identityVersion"] <- JsonValue.Create(compatibility["identityVersion"].GetValue<int>() + 1)
                    | "inferenceProfile" -> catalog[mutation] <- JsonValue.Create "incompatible"
                    | "manifest" -> sharedInput["manifestSha256"] <- JsonValue.Create "changed"
                    | "input" -> sharedInput["sha256"] <- JsonValue.Create "changed"
                    | "api" -> client["api"] <- JsonValue.Create "changed"
                    | "source" -> (client["sources"][0])["sha256"] <- JsonValue.Create "changed"
                    | "source-missing" -> (client["sources"][0])["file"] <- JsonValue.Create "missing.d.ts"
                    | "source-removed" -> client["sources"] <- JsonArray()
                    | "arity" ->
                        client["arity"] <- JsonValue.Create 1
                        client["constraints"] <- JsonArray(JsonValue.Create "None")
                    | "constraints" -> box["constraints"][0] <- JsonValue.Create "None"
                    | "cycle" -> (catalog["owners"][0])["dependencies"] <- JsonArray(JsonValue.Create "Identity.Root")
                    | "merged" -> ()
                    | _ -> failtest "unknown mutation"
                    File.WriteAllText(path, catalog.ToJsonString())
                    let entry = if mutation = "merged" then "augmented.d.ts" else "adapter.d.ts"
                    let config = configured directory "Adapter" entry [| path |]
                    let output = Path.Combine(directory, "adapter")
                    let message =
                        try run config fixture output |> Async.RunSynchronously |> ignore; ""
                        with error -> error.Message
                    (message.Contains expected, Directory.Exists output)
                    |> Flip.Expect.equal message (true, false)
                finally
                    Directory.Delete(directory, true)
        ]

[<Tests>]
let tests = testList "declaration catalog formats" [authenticationTests CatalogCompression.Uncompressed; authenticationTests CatalogCompression.Brotli]

[<Tests>]
let callableTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "declaration catalog callable signatures" [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList "declaration catalog callable signatures" [
            let inline (==>) argument name = argument, name
            testTheory "anonymous arguments retain source-backed callable ownership" [
                "string | number" ==> "union"
                "string | typeof create" ==> "recursive"
                "readonly [string, number?]" ==> "tuple"
                "{ left: string } & { right: number }" ==> "intersection"
            ] <| fun (argument, name) ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-callable-" + name + "-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"catalog-callable-lab","version":"1.0.0","types":"index.d.ts"}"""
                    writePackageFile directory "index.d.ts" $"""export namespace Factory {{
    interface Result {{ count: number; }}
    function create(value: {argument}, options?: object): Promise<Result>;
}}
"""
                    writePackageFile directory "adapter.d.ts" """export { Factory as AdapterFactory } from "./index.js";"""
                    let producer = Path.Combine(directory, "root")
                    let root = configured directory "Root" "index.d.ts" [||]
                    Pipeline.run root directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Create")
                    |> fun entry -> entry.GetProperty("handles").GetArrayLength() > 0
                    |> Flip.Expect.equal "callable identity retains its declaration anchor" true
                    let adapter = configured directory "Adapter" "adapter.d.ts" [| reference |]
                    Pipeline.run adapter directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let share () : Identity.Root.Create = Identity.Adapter.Exports.AdapterFactory.create
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)
        ]

[<Tests>]
let sourceClosureTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "declaration catalog source closure" [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList "declaration catalog source closure" [
            let projectionFixture = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-source-projection-lab"))
            let prepareProjection directory =
                let package = Path.Combine(directory, "package")
                Directory.CreateDirectory package |> ignore
                for file in Directory.GetFiles projectionFixture do
                    File.Copy(file, Path.Combine(package, Path.GetFileName file))
                let producer = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "model.d.ts" [||]) package producer
                |> Async.RunSynchronously |> ignore
                package, Path.Combine(producer, "declarations.json")

            testCase "indexed exports do not enter the referenced type source closure" <| fun _ ->
                use scratch = Scratch.directory "catalog-source-projection"
                let directory = scratch.Path
                let package, reference = prepareProjection directory
                let adapter = Path.Combine(directory, "adapter")
                Pipeline.run (configured directory "Adapter" "index.d.ts" [| reference |]) package adapter
                |> Async.RunSynchronously |> ignore
                use producerCatalog = JsonDocument.Parse(File.ReadAllText reference)
                use catalog = JsonDocument.Parse(File.ReadAllText(Path.Combine(adapter, "declarations.json")))
                let sources (document: JsonDocument) name =
                    document.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = name)
                    |> fun entry -> entry.GetProperty("sources").EnumerateArray()
                    |> Seq.toList
                let packageSources name =
                    sources catalog name
                    |> List.filter (fun source -> source.GetProperty("package").GetString() = "catalog-source-projection-lab")
                    |> Seq.map (fun source -> source.GetProperty("file").GetString())
                    |> Seq.sort |> Seq.toList
                for name in [ "Scalar"; "ArrayContent"; "Nullable"; "TupleContent"; "ObjectContent" ] do
                    let name = "Identity.Root." + name
                    sources catalog name |> List.map _.GetRawText()
                    |> Flip.Expect.equal "all producer sources, including compiler libraries, remain unchanged"
                        (sources producerCatalog name |> List.map _.GetRawText())
                    packageSources name
                    |> Flip.Expect.equal "the source closure retains the owner declaration" [ "model.d.ts" ]
                for name in [ "ScalarProjection"; "ArrayProjection"; "NullableProjection" ] do
                    packageSources ("Identity.Adapter." + name)
                    |> Flip.Expect.equal "the projection retains its own declaration source" [ "index.d.ts" ]
                packageSources "Identity.Root.Named"
                |> Flip.Expect.equal "an explicitly referenced alias remains in the source closure" [ "model.d.ts"; "named.d.ts" ]
                let consumer = """module Identity.Consumer
let scalar (value: Identity.Root.Scalar) : Identity.Root.Scalar = Identity.Adapter.Exports.scalar value
let array (value: Identity.Root.ArrayContent) : Identity.Root.ArrayContent = Identity.Adapter.Exports.array value
let nullable (value: Identity.Root.Nullable) : Identity.Root.Nullable = Identity.Adapter.Exports.nullable value
let tuple (value: Identity.Root.TupleContent) : Identity.Root.TupleContent = Identity.Adapter.Exports.tuple value
let object (value: Identity.Root.ObjectContent) : Identity.Root.ObjectContent = Identity.Adapter.Exports.``object`` value
let named (value: Identity.Root.Named) : Identity.Root.Named = Identity.Adapter.Exports.named value
"""
                File.WriteAllText(Path.Combine(directory, "Directory.Build.props"), "<Project />")
                File.WriteAllText(Path.Combine(directory, "Directory.Build.targets"), "<Project />")
                let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                code |> Flip.Expect.equal output 0

            testTheory "projected exports retain source and API authentication" [ "source"; "api"; "input" ] <| fun mutation ->
                use scratch = Scratch.directory "catalog-source-projection-mutation"
                let directory = scratch.Path
                let package, reference = prepareProjection directory
                let catalog = JsonNode.Parse(File.ReadAllText reference)
                let scalar =
                    catalog["declarations"].AsArray()
                    |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.Scalar")
                let expected =
                    match mutation with
                    | "source" ->
                        (scalar["sources"][0])["sha256"] <- JsonValue.Create "changed"
                        "source hash mismatch"
                    | "api" ->
                        scalar["api"] <- JsonValue.Create "changed"
                        "F# API mismatch"
                    | _ ->
                        File.AppendAllText(Path.Combine(package, "named.d.ts"), "\n// changed declaration source\n")
                        "input source hash mismatch"
                File.WriteAllText(reference, catalog.ToJsonString())
                let output = Path.Combine(directory, "adapter")
                let message =
                    try
                        Pipeline.run (configured directory "Adapter" "index.d.ts" [| reference |]) package output
                        |> Async.RunSynchronously |> ignore
                        ""
                    with error -> error.Message
                (message.Contains expected, Directory.Exists output)
                |> Flip.Expect.equal message (true, false)

            let inline (==>) declaration scenario = declaration, scenario
            testTheory "value exports preserve referenced declaration ownership" [
                "export declare const current: Client;" ==> "instance"
                "export declare const label: string;" ==> "primitive"
            ] <| fun (declaration, scenario) ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-source-" + scenario + "-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"catalog-source-lab","version":"1.0.0","type":"module"}"""
                    writePackageFile directory "message.d.ts" "export interface Message { text: string; }"
                    writePackageFile directory "shared.d.ts" """import { Message } from "./message.js"; export interface Client { send(message: Message): void; }"""
                    writePackageFile directory "index.d.ts" """export { Client } from "./shared.js";"""
                    writePackageFile directory "adapter.d.ts" ("""import { Client } from "./shared.js"; export function attach(client: Client): Client; """ + declaration)
                    let producer = Path.Combine(directory, "root")
                    Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Client")
                    |> fun entry -> entry.GetProperty("sources").EnumerateArray()
                    |> Seq.map (fun source -> source.GetProperty("file").GetString())
                    |> Seq.sort |> Seq.toList
                    |> Flip.Expect.equal "member declaration sources remain in the ownership closure" [ "message.d.ts"; "shared.d.ts" ]
                    Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter"))
                    |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let share (client: Identity.Root.Client) : Identity.Root.Client = Identity.Adapter.Exports.attach client
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)
        ]

[<Tests>]
let literalUnionTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "declaration catalog anonymous literal unions" [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList "declaration catalog anonymous literal unions" [
            testTheory "unrelated parent properties preserve shared literal union ownership" [ ""; "?" ] <| fun optional ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-literal-union-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"icon-identity-lab","version":"1.0.0"}"""
                    writePackageFile directory "shared.d.ts" ("export interface Icon { src: string; theme" + optional + """: "dark" | "light"; }""")
                    writePackageFile directory "root.d.ts" """export interface Palette { color?: "dark" | "light"; }
export { Icon } from "./shared";
"""
                    writePackageFile directory "adapter.d.ts" """export { Icon } from "./shared";"""
                    let producer = Path.Combine(directory, "root")
                    Pipeline.run (configured directory "Root" "root.d.ts" [||]) directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter"))
                    |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let share (icon: Identity.Adapter.Icon) : Identity.Root.Icon = icon
let copyTheme (source: Identity.Root.Icon) (target: Identity.Adapter.Icon) = target.theme <- source.theme
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Icon")
                    |> fun entry -> entry.GetProperty("sources").EnumerateArray()
                    |> Seq.map (fun source -> source.GetProperty("file").GetString()) |> Seq.toList
                    |> Flip.Expect.equal "literal use sites do not replace the enclosing source dependency" [ "shared.d.ts" ]
                finally Directory.Delete(directory, true)

            testCase "named unions and mixed enum unions retain declaration identity" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-catalog-nominal-union-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"nominal-union-lab","version":"1.0.0"}"""
                    writePackageFile directory "shared.d.ts" """export enum Left { Dark = "dark", Light = "light" }
export enum Right { Dark = "dark", Light = "light" }
export type Theme = "dark" | "light";
export interface Mixed { left?: Left | "auto"; right?: Right | "auto"; theme?: Theme; }
"""
                    writePackageFile directory "index.d.ts" """export * from "./shared";"""
                    writePackageFile directory "adapter.d.ts" """export * from "./shared";"""
                    let producer = Path.Combine(directory, "root")
                    Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    let named =
                        catalog.RootElement.GetProperty("declarations").EnumerateArray()
                        |> Seq.filter (fun entry -> List.contains (entry.GetProperty("fSharpName").GetString()) [ "Identity.Root.Left"; "Identity.Root.Right"; "Identity.Root.Theme" ])
                        |> Seq.toList
                    named |> List.length |> Flip.Expect.equal "each named declaration retains an owner" 3
                    named |> List.map (fun entry -> entry.GetProperty("identity").GetString()) |> List.distinct |> List.length
                    |> Flip.Expect.equal "equal literal values do not merge named declarations" 3
                    named |> List.forall (fun entry -> entry.GetProperty("handles").GetArrayLength() > 0)
                    |> Flip.Expect.equal "named declarations retain source anchors" true
                    Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter"))
                    |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let share (value: Identity.Adapter.Mixed) : Identity.Root.Mixed = value
let copyLeft (source: Identity.Root.Mixed) (target: Identity.Adapter.Mixed) = target.left <- source.left
"""
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let code, output = compileConsumer directory sources consumer
                    code |> Flip.Expect.equal output 0
                    let code, output = compileConsumer directory sources (consumer + "\nlet invalid (value: Identity.Root.Left) : Identity.Root.Right = value\n")
                    (code <> 0 && output.Contains "FS0001") |> Flip.Expect.equal output true
                finally Directory.Delete(directory, true)
        ]

[<Tests>]
let privateNullableAliasTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "declaration catalog private nullable aliases" [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList "declaration catalog private nullable aliases" [
            let cases = [ "?", ""; "", " | null"; "", " | undefined"; "", " | null | undefined" ]
            testTheory "compiler relation preserves private aliases and export order" [
                for optional, nullish in cases do
                    for reverse in [ false; true ] do yield optional, nullish, reverse
            ] <| fun (optional, nullish, reverse) ->
                let directory = Path.Combine(temporaryRoot, "xantham-private-nullable-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"private-nullable-lab","version":"1.0.0"}"""
                    writePackageFile directory "aliases.d.ts" """export type First = "a" | "b";
export type Second = "a" | "b";
"""
                    writePackageFile directory "shared.d.ts" ("import { First, Second } from './aliases';\nexport { First, Second } from './aliases';\nexport interface Options { first" + optional + ": First" + nullish + "; second" + optional + ": Second" + nullish + "; }\n")
                    writePackageFile directory "index.d.ts" (if reverse then "export { Options, Second, First } from './shared';" else "export { First, Second, Options } from './shared';")
                    writePackageFile directory "adapter.d.ts" "export interface Earlier { unrelated?: 'a' | 'b'; }\nexport { Options } from './shared';"
                    let producer = Path.Combine(directory, "root")
                    Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let consumer = """module Identity.Consumer
let share (value: Identity.Adapter.Options) : Identity.Root.Options = value
let first (value: Identity.Adapter.Options) : Identity.Root.First option = value.first
let second (value: Identity.Adapter.Options) : Identity.Root.Second option = value.second
let copy (source: Identity.Root.Options) (target: Identity.Adapter.Options) =
    target.first <- source.first
    target.second <- source.second
"""
                    let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] consumer
                    code |> Flip.Expect.equal output 0
                    if optional = "?" && not reverse then
                        let invalid = consumer + "\nlet invalid (source: Identity.Root.Options) (target: Identity.Adapter.Options) = target.first <- source.second\n"
                        let code, output = compileConsumer directory [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ] invalid
                        (code <> 0 && output.Contains "FS0193") |> Flip.Expect.equal output true
                    use catalog = JsonDocument.Parse(File.ReadAllText reference)
                    catalog.RootElement.GetProperty("declarations").EnumerateArray()
                    |> Seq.find (fun entry -> entry.GetProperty("fSharpName").GetString() = "Identity.Root.Options")
                    |> fun entry -> entry.GetProperty("sources").EnumerateArray()
                    |> Seq.map (fun source -> source.GetProperty("file").GetString()) |> Seq.sort |> Seq.toList
                    |> Flip.Expect.equal "nullable references retain alias source dependencies" [ "aliases.d.ts"; "shared.d.ts" ]
                    let aliases = catalog.RootElement.GetProperty("declarations").EnumerateArray() |> Seq.filter (fun entry -> [ "Identity.Root.First"; "Identity.Root.Second" ] |> List.contains (entry.GetProperty("fSharpName").GetString())) |> Seq.toList
                    aliases |> List.length |> Flip.Expect.equal "two source declaration owners" 2
                    aliases |> List.map (fun entry -> entry.GetProperty("identity").GetString()) |> List.distinct |> List.length |> Flip.Expect.equal "equal values retain distinct alias declarations" 2
                finally Directory.Delete(directory, true)
        ]

[<Tests>]
let genericNullableAliasTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testList "declaration catalog generic nullable aliases" [
            testCase "skipped - no compiler" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE" ]
    | Some _ ->
        testList "declaration catalog generic nullable aliases" [
            testCase "nullable tagged unions retain payload type arguments" <| fun _ ->
                let directory = Path.Combine(temporaryRoot, "xantham-generic-nullable-" + Guid.NewGuid().ToString "N")
                Directory.CreateDirectory directory |> ignore
                try
                    writePackageFile directory "package.json" """{"name":"generic-nullable-lab","version":"1.0.0"}"""
                    writePackageFile directory "aliases.d.ts" """export type Result<T> = { kind: 'ok'; value: T } | { kind: 'error'; message: string };
export type Maybe<T> = Result<T> | null;
"""
                    writePackageFile directory "shared.d.ts" """import { Result, Maybe } from './aliases';
export interface Options<T> { direct?: Result<T>; wrapped?: Maybe<T>; }
"""
                    writePackageFile directory "index.d.ts" "export * from './aliases'; export { Options } from './shared';"
                    writePackageFile directory "adapter.d.ts" "export { Options } from './shared';"
                    let producer = Path.Combine(directory, "root")
                    Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory producer |> Async.RunSynchronously |> ignore
                    let reference = Path.Combine(producer, "declarations.json")
                    Pipeline.run (configured directory "Adapter" "adapter.d.ts" [| reference |]) directory (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                    let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                    let consumer = """module Identity.Consumer
let share<'T> (value: Identity.Adapter.Options<'T>) : Identity.Root.Options<'T> = value
let copy<'T> (source: Identity.Root.Options<'T>) (target: Identity.Adapter.Options<'T>) =
    target.direct <- source.direct
    target.wrapped <- source.wrapped
"""
                    let invalid = consumer + "\nlet invalid (source: Identity.Root.Options<string>) (target: Identity.Adapter.Options<float>) = target.direct <- source.direct\n"
                    let code, output = compileConsumer directory sources invalid
                    (code <> 0 && output.Contains "FS0193") |> Flip.Expect.equal "distinct payload types must reject cross-assignment" true
                    let payloads = """
let stringPayload (value: Identity.Root.Options<string>) : string option =
    value.direct |> Option.bind (function Identity.Root.Result.Ok payload -> Some payload | _ -> None)
let numberPayload (value: Identity.Adapter.Options<float>) : float option =
    value.wrapped |> Option.bind (function Identity.Root.Result.Ok payload -> Some payload | _ -> None)
"""
                    let code, output = compileConsumer directory sources (consumer + payloads)
                    code |> Flip.Expect.equal output 0
                finally Directory.Delete(directory, true)
        ]

[<Tests>]
let wideSurfaceTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None -> testCase "declaration catalog wide surface skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testCase "declaration catalog API hash covers every member of a 101-member interface" <| fun _ ->
            let directory = Path.Combine(temporaryRoot, "xantham-catalog-wide-" + Guid.NewGuid().ToString "N")
            Directory.CreateDirectory directory |> ignore
            try
                // Member names sort in declaration order, so the member that differs sorts last.
                let api (last: string) =
                    let package = Path.Combine(directory, last)
                    let members = [ for index in 0..99 -> $"    m{index:D3}: string;" ] @ [ $"    m100: {last};" ]
                    writePackageFile package "package.json" """{"name":"wide-lab","version":"1.0.0","types":"index.d.ts"}"""
                    writePackageFile package "index.d.ts" ("export interface Wide {\n" + String.concat "\n" members + "\n}\n")
                    let config =
                        { GeneratorConfig.Default with
                            ModuleName = Some "Identity.Root"
                            Lib = Some [ "esnext" ]
                            Types = Some []
                            DeclarationCatalog = true }
                    let output = Path.Combine(package, "out")
                    Pipeline.run config package output |> Async.RunSynchronously |> ignore
                    let catalog =
                        JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(
                            File.ReadAllText(Path.Combine(output, "declarations.json")),
                            JsonSerializerOptions(PropertyNameCaseInsensitive = true))
                    (catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Identity.Root.Wide")).Api
                Expect.notEqual (api "string") (api "number") "the 101st member participates in the API hash"
            finally Directory.Delete(directory, true)

[<Tests>]
let genericDefaultConstraintTests =
    match Tsc.locate __SOURCE_DIRECTORY__ with
    | None ->
        testCase "declaration catalog generic defaults skipped - no compiler" <| fun _ -> skiptest "no tsc"
    | Some _ ->
        testList "declaration catalog generic defaults" [
            testCase "never defaults retain shared constraints and compile concrete defaults" <| fun _ ->
                use scratch = Scratch.directory "catalog-generic-defaults"
                let directory = scratch.Path
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-generic-defaults-lab"))
                let root = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) package root |> Async.RunSynchronously |> ignore
                let reference = Path.Combine(root, "declarations.json")
                let adapter = configured directory "Adapter" "index.d.ts" [| reference |]
                Pipeline.run adapter package (Path.Combine(directory, "adapter")) |> Async.RunSynchronously |> ignore
                let sources = [ "root/Identity.Root.fs"; "adapter/Identity.Adapter.fs" ]
                let consumer = """module Identity.Consumer
let project<'T when 'T :> Identity.Root.Def> (value: Identity.Root.Middleware<'T>) : Identity.Root.Result<'T> option =
    value.callback |> Option.map (fun callback -> callback ())
let share<'T when 'T :> Identity.Root.Def> (value: Identity.Adapter.Middleware<'T>) : Identity.Root.Middleware<'T> = value
let defaults (value: Identity.Root.Defaults) : Identity.Root.Middleware<Identity.Root.Def> = value.middleware
"""
                let code, output = compileConsumer directory sources consumer
                Expect.equal code 0 output
                let code, output = compileConsumer directory sources (consumer + "\nlet invalid (value: Identity.Root.Middleware<string>) = value\n")
                Expect.notEqual code 0 "the default does not remove the declared nominal bound"
                Expect.stringContains output "FS0001" "string cannot satisfy the declaration's bound"
                let catalog = JsonNode.Parse(File.ReadAllText reference)
                let declaration =
                    catalog["declarations"].AsArray()
                    |> Seq.find (fun entry -> entry["fSharpName"].GetValue<string>() = "Identity.Root.Result")
                declaration["constraints"] <- JsonNode.Parse """["none"]"""
                File.WriteAllText(reference, catalog.ToJsonString())
                Expect.throwsC
                    (fun () -> Pipeline.run adapter package (Path.Combine(directory, "tampered")) |> Async.RunSynchronously |> ignore)
                    (fun error -> Expect.stringContains error.Message "constraint mismatch" "catalog authentication still rejects different constraints")

            testTheory "nullable and promised applications share the declaration's bound" [
                for fallback in [ " = never"; " = Def"; "" ] do
                    for nullable in [ false; true ] do yield fallback, nullable
            ] <| fun (fallback, nullable) ->
                use scratch = Scratch.directory "catalog-generic-default-shapes"
                let directory = scratch.Path
                writePackageFile directory "package.json" """{"name":"catalog-generic-default-shapes-lab","version":"1.0.0","types":"index.d.ts"}"""
                let absence = if nullable then "undefined | " else ""
                writePackageFile directory "index.d.ts"
                    ("export interface Def { id: string; }\n"
                     + "export type Result<T extends Def = Def> = " + absence + "{ definition: T };\n"
                     + "export interface Middleware<U extends Def" + fallback + "> { callback?: () => Result<U> | Promise<Result<U>>; }\n")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory (Path.Combine(directory, "root"))
                |> Async.RunSynchronously |> ignore

            testCase "applied bounds retain their arguments through any and never applications" <| fun _ ->
                use scratch = Scratch.directory "catalog-applied-default-bound"
                let directory = scratch.Path
                let package = Path.GetFullPath(Path.Combine(fixture, "..", "catalog-generic-defaults-lab"))
                let root = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) package root |> Async.RunSynchronously |> ignore
                let consumer = """module Identity.Consumer
let any (value: Identity.Root.AnyApplied) : Identity.Root.Applied<Identity.Root.Bound<string>> = value
let never (value: Identity.Root.NeverApplied) : Identity.Root.Applied<Identity.Root.Bound<string>> = value
let inherited (value: Identity.Root.InheritedApplied) : Identity.Root.Applied<Identity.Root.Bound<string>> = value :> _
let nominal (value: Identity.Root.StringApplied) : Identity.Root.Applied<Identity.Root.StringBound> = value
let distinctArguments (value: Identity.Root.ErasedString) : Identity.Root.Erased<Identity.Root.Bound<obj>> = value
let recursive (value: Identity.Root.RecursiveUse) : Identity.Root.Recursive<Identity.Root.RecursiveValue> = value
"""
                let sources = [ "root/Identity.Root.fs" ]
                let code, output = compileConsumer directory sources consumer
                Expect.equal code 0 output
                let invalid = consumer + "\nlet invalid (value: Identity.Root.Applied<Identity.Root.NumberBound>) = value\n"
                let code, output = compileConsumer directory sources invalid
                Expect.notEqual code 0 "a different application of Bound does not satisfy Bound<string>"
                Expect.stringContains output "FS0001" "the generic arguments participate in the nominal constraint"

            testCase "inhabited structural defaults keep the existing constraint mapping" <| fun _ ->
                use scratch = Scratch.directory "catalog-structural-default"
                let directory = scratch.Path
                writePackageFile directory "package.json" """{"name":"catalog-structural-default-lab","version":"1.0.0","types":"index.d.ts"}"""
                writePackageFile directory "index.d.ts" """export interface Def { id: string; }
export interface Structural { id: string; extra: number; }
export interface Holder<T extends Def = Structural> { value: T; }
"""
                let output = Path.Combine(directory, "root")
                Pipeline.run (configured directory "Root" "index.d.ts" [||]) directory output |> Async.RunSynchronously |> ignore
                let binding = File.ReadAllText(Path.Combine(output, "Identity.Root.fs"))
                Expect.stringContains binding "type Holder<'T>" "structural default policy remains unconstrained"
        ]
