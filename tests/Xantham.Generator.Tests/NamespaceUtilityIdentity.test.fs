module Xantham.Generator.Tests.NamespaceUtilityIdentityTests

open System.IO
open System.Text.Json
open System.Diagnostics
open Expecto
open Xantham.Generator
open Xantham.Generator.Measure

[<Tests>]
let tests =
    testCase "namespace utility declarations retain authenticated catalogue identities" <| fun _ ->
        use scratch = Scratch.directory "namespace-utility-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "zod-namespace-utility-lab")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Utilities"; DeclarationCatalog = true }
        Pipeline.run config fixture scratch.Path |> Async.RunSynchronously |> ignore
        let readCatalog directory =
            JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(directory, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let catalog = readCatalog scratch.Path
        let aliases =
            [ "Mini.Utilities.Omit"; "Mini.Utilities.V3.Util.Omit"; "Mini.Utilities.Other.Util.Omit" ]
            |> List.map (fun name -> catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = name))
        for alias in aliases do
            Expect.isNonEmpty alias.Handles "each alias authenticates its own source declaration"
            Expect.isNonEmpty alias.Sources "source bytes remain part of the contract"
        Expect.equal (aliases |> List.map _.Identity |> List.distinct |> List.length) 3 "same-shaped same-named aliases in separate namespaces cannot share identity"
        let mirrored = Path.Combine(scratch.Path, "mirrored")
        let mirroredConfig = { config with PublicInputs = Some(Map.ofList [ ".", "index.d.ts"; "./mirror", "index.d.ts" ]) }
        let inputs, _ = Bootstrap.publicPaths mirroredConfig fixture
        Expect.equal (inputs |> List.map _.Key) [ "."; "./mirror" ] "both public input occurrences are selected"
        Expect.equal (inputs |> List.map _.File |> List.distinct |> List.length) 1 "the occurrences authenticate the same source file"
        Pipeline.run mirroredConfig fixture mirrored
        |> Async.RunSynchronously |> ignore
        let mirrors = (readCatalog mirrored).Declarations |> Array.filter (fun declaration -> declaration.FSharpName.EndsWith ".V3.Util.Omit")
        Expect.isNonEmpty mirrors "public inputs retain the nested alias"
        Expect.equal (mirrors |> Array.map _.Identity |> Array.distinct |> Array.length) 1 "multiple public occurrences of the same source alias reuse its source identity"

[<Tests>]
let destructuredParameters =
    testCase "destructured function parameters retain authenticated parent roles" <| fun _ ->
        use scratch = Scratch.directory "destructured-parameter-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "destructured-parameter-identity-lab")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Parameters"; DeclarationCatalog = true }
        async {
            let! mailbox, ctx = Bootstrap.start config fixture
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolved, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage config ctx.PackageName) resolved)
            let parameterId = shape.DeclNames |> Map.toList |> List.find (fun (_, name) -> name = "CreateToolNameMapping0") |> fst
            Expect.isNonEmpty shape.Types[parameterId].Declarations "the inline type has source declaration evidence despite no complete structural key"
            let syntheticExports =
                harvest.Exports |> List.map (fun export ->
                    if export.ExportName = "createToolNameMapping" then
                        { export with Symbol = { export.Symbol with Declarations = ValueSome [||]; ValueDeclaration = ValueNone } }
                    else export)
            let synthetic = { shape with Harvest = { shape.Harvest with Exports = syntheticExports } }
            let! rejected = Async.Catch(DeclarationCatalog.apply ctx synthetic (Pipeline.groupModules ctx synthetic))
            match rejected with
            | Choice1Of2 _ -> failtest "a synthetic callable without source handles cannot anchor an anonymous parameter"
            | Choice2Of2 error -> Expect.stringContains error.Message "no stable declaration or parent role" "unanchored generated names remain rejected"
        } |> Async.RunSynchronously
        Pipeline.run config fixture scratch.Path |> Async.RunSynchronously |> ignore
        let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(scratch.Path, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let parameter = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.CreateToolNameMapping0")
        Expect.isNonEmpty parameter.Handles "anonymous parameters retain their actual source function anchor"
        Expect.isNonEmpty parameter.Sources "parameter source closure is authenticated"
        Expect.stringContains parameter.Role "call:0:parameter:" "the key is a source-function parameter role, not a generated name"
        let copied = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.CopyToolNameMapping0")
        Expect.notEqual parameter.Identity copied.Identity "same-shaped literals under distinct source functions remain distinct"
        let positions = catalog.Declarations |> Array.filter (fun declaration -> declaration.FSharpName.Contains ".Positions")
        Expect.equal positions.Length 3 "both parameter positions and the anonymous return are retained"
        Expect.equal (positions |> Array.map _.Identity |> Array.distinct |> Array.length) 3 "parameter and return roles cannot collide"
        let overloads = catalog.Declarations |> Array.filter (fun declaration -> declaration.FSharpName.Contains ".Overloaded")
        Expect.equal overloads.Length 2 "each overload retains its parameter declaration"
        Expect.equal (overloads |> Array.map _.Identity |> Array.distinct |> Array.length) 2 "overload indices distinguish anonymous parameters"
        let generic = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Parameters.GenericMapping0")
        Expect.equal generic.Arity 1 "generic anonymous parameters keep their binder"
        Expect.isNonEmpty generic.Constraints "generic constraints remain in the catalogue contract"
        Expect.isFalse (catalog.Declarations |> Array.exists (fun declaration -> declaration.Role = "source-function-parent")) "traversal roots never become complete callable catalogue identities"
        let altered = Path.Combine(scratch.Path, "altered")
        Directory.CreateDirectory altered |> ignore
        File.Copy(Path.Combine(fixture, "package.json"), Path.Combine(altered, "package.json"))
        File.WriteAllText(Path.Combine(altered, "index.d.ts"), File.ReadAllText(Path.Combine(fixture, "index.d.ts")).Replace("T extends Tool", "T extends Mapping"))
        let reused = Pipeline.run { config with DeclarationReferences = [Path.Combine(scratch.Path, "declarations.json")] } altered (Path.Combine(scratch.Path, "consumer")) |> Async.Catch |> Async.RunSynchronously
        match reused with
        | Choice1Of2 _ -> failtest "a changed source generic constraint cannot reuse an old parameter contract"
        | Choice2Of2 error -> Expect.stringContains error.Message "source" "source mutation remains authenticated"

[<Tests>]
let keyofEnumOwners =
    testCase "keyof value alias owners remain stable across consumer contexts" <| fun _ ->
        use scratch = Scratch.directory "keyof-enum-owner"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "keyof-enum-owner-lab")
        let owner = Path.Combine(scratch.Path, "owner")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.EnumOwner"; DeclarationCatalog = true }
        Pipeline.run config fixture owner |> Async.RunSynchronously |> ignore
        Pipeline.run { config with ModuleName = Some "Mini.EnumConsumer"; Entry = Some "consumer.d.ts"; DeclarationReferences = [Path.Combine(owner, "declarations.json")] } fixture (Path.Combine(scratch.Path, "consumer"))
        |> Async.RunSynchronously |> ignore
        let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(owner, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let named name = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.EnumOwner." + name)
        let parsed, other = named "Parsed", named "OtherParsed"
        Expect.equal parsed.Handles.Length 2 "the producer retains both merged value/type declaration handles"
        Expect.equal other.Handles.Length 2 "each source enum retains its own complete symbol"
        Expect.notEqual parsed.Identity other.Identity "same-valued source aliases are not identified by their literal set"
        let raw = catalog.Declarations |> Array.find (fun declaration -> declaration.Role = "literal-union" && declaration.Api = parsed.Api)
        let ownerText = File.ReadAllText(Path.Combine(owner, "Mini.EnumOwner.fs"))
        Expect.stringContains ownerText "OtherParsedValue2" "the value helper avoids an existing public type name"
        Expect.stringContains ownerText "Import(\"OtherParsed\"" "the JavaScript const import keeps its original name"
        Expect.isEmpty raw.Handles "a raw literal set does not acquire a named source declaration"
        Expect.notEqual raw.Identity parsed.Identity "unannotated literals stay separate from source alias ownership"
        let repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
        File.Copy(Path.Combine(repository, "global.json"), Path.Combine(scratch.Path, "global.json"), true)
        File.WriteAllText(Path.Combine(scratch.Path, "Consumer.fsproj"), $"""<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup><ItemGroup><Compile Include="owner/Mini.EnumOwner.fs"/><Compile Include="consumer/Mini.EnumConsumer.fs"/><Compile Include="Consumer.fs"/></ItemGroup><ItemGroup><PackageReference Include="Fable.Core" Version="5.6.0"/><ProjectReference Include="{repository}/src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj"/></ItemGroup></Project>""")
        let compile (source: string) =
            File.WriteAllText(Path.Combine(scratch.Path, "Consumer.fs"), source)
            let start = ProcessStartInfo("dotnet", WorkingDirectory = scratch.Path, RedirectStandardOutput = true, RedirectStandardError = true)
            for argument in ["build"; "Consumer.fsproj"; "--disable-build-servers"; "-m:1"; "-p:BuildProjectReferences=false"; "-c";
#if DEBUG
                             "Debug"
#else
                             "Release"
#endif
                             "-v"; "quiet"] do start.ArgumentList.Add argument
            use child = Process.Start start
            let output, errors = child.StandardOutput.ReadToEndAsync(), child.StandardError.ReadToEndAsync()
            if not (child.WaitForExit 60000) then
                child.Kill(entireProcessTree = true)
                child.WaitForExit()
                failtest "the owned enum consumer compile exceeded sixty seconds"
            child.ExitCode, output.Result + errors.Result
        let consumer = $"""module Mini.Check
let expected (value: Mini.EnumOwner.Invalid) : Mini.EnumOwner.Parsed = value.expected
let distinct (value: Mini.EnumOwner.Invalid) : Mini.EnumOwner.OtherParsed = value.other
let raw (value: Mini.EnumOwner.Invalid) : {raw.FSharpName} = value.raw
let forwarded (value: Mini.EnumOwner.Alias) : Mini.EnumOwner.Parsed = value
let imported (value: Mini.EnumConsumer.AliasUse) : Mini.EnumOwner.Parsed = value.parsed
let shared (value: Mini.EnumConsumer.Container) : Mini.EnumOwner.Issue = value.issue
let constValue : Mini.EnumOwner.OtherParsedValue2 = Mini.EnumOwner.Exports.OtherParsed
let constField : string = constValue.``string``
let existing (value: Mini.EnumOwner.OtherParsedValue) : bool = value.existing
"""
        let code, log = compile consumer
        Expect.equal code 0 log
        let invalidCode, invalidLog = compile (consumer + $"\nlet invalid (value: Mini.EnumOwner.Parsed) : Mini.EnumOwner.OtherParsed = value\nlet invalidRaw (value: Mini.EnumOwner.Parsed) : {raw.FSharpName} = value\n")
        Expect.notEqual invalidCode 0 "same-valued nominal and anonymous enums cannot be silently interchanged"
        Expect.stringContains invalidLog "FS0001" "typed consumer proves nominal boundaries"

        let altered = Path.Combine(scratch.Path, "altered")
        Directory.CreateDirectory altered |> ignore
        for file in Directory.GetFiles fixture do File.Copy(file, Path.Combine(altered, Path.GetFileName file))
        let common = Path.Combine(altered, "common.d.ts")
        File.WriteAllText(common, File.ReadAllText(common).Replace("number: \"number\"", "number: \"numeric\""))
        let changedSource =
            Pipeline.run { config with ModuleName = Some "Mini.Changed"; Entry = Some "consumer.d.ts"; DeclarationReferences = [Path.Combine(owner, "declarations.json")] } altered (Path.Combine(scratch.Path, "changed"))
            |> Async.Catch |> Async.RunSynchronously
        match changedSource with
        | Choice1Of2 _ -> failtest "unchanged keyof cases cannot hide changed merged const source bytes"
        | Choice2Of2 error -> Expect.stringContains error.Message "source" "source authentication rejects changed const contents"

        async {
            let consumerConfig = { config with ModuleName = Some "Mini.Forged"; DeclarationReferences = [Path.Combine(owner, "declarations.json")] }
            let! mailbox, ctx = Bootstrap.start consumerConfig fixture
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolved, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage consumerConfig ctx.PackageName) resolved)
            let unsupported = shape.Types |> Map.toList |> List.collect (snd >> _.Members) |> List.filter (fun member_ -> List.contains member_.Symbol.Name ["generic"; "nullable"])
            Expect.isNonEmpty unsupported "unsupported wrapper annotations are present in the source lab"
            for member_ in unsupported do
                Expect.isNone member_.SourceStringAnnotation "generic and nullable source wrappers retain their existing shaping path"
            let aliases = shape.SourceStringAliases |> Map.map (fun _ alias ->
                if alias.AliasSymbol.SymbolName = "Parsed" * uom<symbolName> then
                    { alias with AliasSymbol = { alias.AliasSymbol with Declarations = alias.AliasSymbol.Declarations |> ValueOption.map (Array.truncate 1) } }
                else alias)
            let forged = { shape with SourceStringAliases = aliases }
            let! rejected = Async.Catch(DeclarationCatalog.apply ctx forged (Pipeline.groupModules ctx forged))
            match rejected with
            | Choice1Of2 _ -> failtest "partial merged source handles cannot impersonate a complete enum contract"
            | Choice2Of2 error -> Expect.stringContains error.Message "declaration catalog" "the full source contract remains guarded"
        } |> Async.RunSynchronously

[<Tests>]
let sourceEnumDependencyOwnership =
    testCase "source enum dependency ownership does not capture entry literals" <| fun _ ->
        use scratch = Scratch.directory "source-enum-dependency"
        let package = Path.Combine(scratch.Path, "package")
        let dependency = Path.Combine(package, "node_modules", "source-enums")
        Directory.CreateDirectory dependency |> ignore
        File.WriteAllText(Path.Combine(package, "package.json"), """{"name":"entry-enum-lab","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(dependency, "package.json"), """{"name":"source-enums","version":"1.0.0","types":"index.d.ts"}""")
        File.WriteAllText(Path.Combine(dependency, "index.d.ts"), """export declare const Parsed: { left: "left"; right: "right" }; export type Parsed = keyof typeof Parsed;""")
        File.WriteAllText(Path.Combine(package, "index.d.ts"), """import type { Parsed } from "source-enums"; export interface Entry { expected: Parsed; raw: "left" | "right"; }""")
        let output = Path.Combine(scratch.Path, "output")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Entry"; DeclarationCatalog = true; Groups = Map.ofList ["source-enums" * uom<npmDependency>, Ship] }
        Pipeline.run config package output |> Async.RunSynchronously |> ignore
        let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(output, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        let named = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName.EndsWith ".Parsed" && declaration.Handles.Length = 2)
        let raw = catalog.Declarations |> Array.find (fun declaration -> declaration.Role = "literal-union" && declaration.Api = named.Api)
        Expect.stringStarts named.FSharpName "SourceEnums." "the actual named source alias is emitted in the shipped dependency group"
        Expect.stringStarts raw.FSharpName "Mini.Entry." "the entry literal retains its occurrence group"
        Expect.notEqual named.FSharpName raw.FSharpName "an interned compiler type cannot merge source and anonymous names"
        Expect.isEmpty raw.Handles "the entry literal does not acquire dependency source handles"
        Expect.notEqual raw.Identity named.Identity "interned literal values do not confer nominal ownership"

[<Tests>]
let callableInterfaceOwners =
    testCase "generic callable interfaces keep their declaration in consumers that reach them transitively" <| fun _ ->
        use scratch = Scratch.directory "callable-interface-owner"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "callable-interface-owner-lab")
        let owner = Path.Combine(scratch.Path, "owner")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Owner"; DeclarationCatalog = true }
        Pipeline.run config fixture owner |> Async.RunSynchronously |> ignore
        let standalone = Path.Combine(scratch.Path, "standalone")
        Pipeline.run { config with ModuleName = Some "Mini.Standalone"; Entry = Some "consumer.d.ts" } fixture standalone
        |> Async.RunSynchronously |> ignore
        let standaloneText = File.ReadAllText(Path.Combine(standalone, "Mini.Standalone.fs"))
        Expect.stringContains standaloneText "type ErrorMap<'T when 'T :> IssueBase>" "the transitively reached callable interface keeps its parameter"
        Expect.stringContains standaloneText "abstract error: ErrorMap<'T> option" "its application reads as the declaration, not an expanded function"
        Pipeline.run { config with ModuleName = Some "Mini.Consumer"; Entry = Some "consumer.d.ts"; DeclarationReferences = [ Path.Combine(owner, "declarations.json") ] } fixture (Path.Combine(scratch.Path, "consumer"))
        |> Async.RunSynchronously |> ignore
        let consumer = File.ReadAllText(Path.Combine(scratch.Path, "consumer", "Mini.Consumer.fs"))
        Expect.stringContains consumer "Mini.Owner.Schema" "the consumer reuses the producer's declarations"

[<Tests>]
let electedAliasOwners =
    testCase "an unaliased union reuses the producer's alias elected for its member set" <| fun _ ->
        use scratch = Scratch.directory "alias-election-owner"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "alias-election-owner-lab")
        let owner = Path.Combine(scratch.Path, "owner")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Owner"; DeclarationCatalog = true }
        Pipeline.run config fixture owner |> Async.RunSynchronously |> ignore
        let catalog = JsonSerializer.Deserialize<DeclarationCatalog.Catalog>(File.ReadAllText(Path.Combine(owner, "declarations.json")), JsonSerializerOptions(PropertyNameCaseInsensitive = true))
        for alias in [ "IssueData"; "FauxContentBlock" ] do
            let elected = catalog.Declarations |> Array.find (fun declaration -> declaration.FSharpName = "Mini.Owner." + alias)
            Expect.isNotNull elected.Equivalents "the elected alias records the unaliased union it names"
            Expect.equal elected.Equivalents.Length 1 "the elected alias records exactly one equivalent member contract"
            Expect.notEqual elected.Equivalents[0] elected.Identity "the alias keeps its own declaration identity"
        let consume name entry =
            let output = Path.Combine(scratch.Path, name)
            Pipeline.run { config with ModuleName = Some("Mini." + name); Entry = Some entry; DeclarationReferences = [ Path.Combine(owner, "declarations.json") ] } fixture output
            |> Async.RunSynchronously |> ignore
            File.ReadAllText(Path.Combine(output, $"Mini.{name}.fs"))
        let transitive = consume "Transitive" "consumer.d.ts"
        Expect.stringContains transitive "Mini.Owner.Schema" "a consumer reaching the union through a member reuses the producer's declarations"
        Expect.stringContains transitive "Mini.Owner.AssistantMessage" "a wrapper over declared nominal arms reuses the producer's elected alias"
        Expect.isFalse (transitive.Contains "Issues.Item") "the union is not declared a second time"
        let direct = consume "Direct" "direct.d.ts"
        Expect.stringContains direct "issue: Mini.Owner.IssueData" "a consumer naming the alias reuses the same declaration"
        Expect.stringContains direct "content: Mini.Owner.FauxContentBlock" "a consumer naming the nominal-arm alias reuses the same declaration"
        let code, output = DeclarationCatalogTests.compileConsumer scratch.Path [ "owner/Mini.Owner.fs"; "Transitive/Mini.Transitive.fs"; "Direct/Mini.Direct.fs" ] """module Mini.Check
let content (message: Mini.Owner.AssistantMessage) : Mini.Owner.FauxContentBlock[] = message.content
let forward (message: Mini.Owner.AssistantMessage) = Mini.Transitive.Exports.message(message)
let block (message: Mini.Owner.AssistantMessage) = Mini.Direct.Exports.content(message.content[0])
"""
        Expect.equal code 0 output
        async {
            let consumerConfig = { config with ModuleName = Some "Mini.Changed"; Entry = Some "consumer.d.ts"; DeclarationReferences = [ Path.Combine(owner, "declarations.json") ] }
            let! mailbox, ctx = Bootstrap.start consumerConfig fixture
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage consumerConfig ctx.PackageName) resolve)
            let mutable changedCases = 0
            let declarations =
                shape.Decls |> List.map (function
                    | FsTaggedUnion union ->
                        let cases =
                            union.Cases |> List.map (fun case ->
                                if case.CompiledName = Some "text" then
                                    changedCases <- changedCases + 1
                                    { case with Name = case.Name + "Changed" }
                                else case)
                        FsTaggedUnion { union with Cases = cases }
                    | declaration -> declaration)
            Expect.equal changedCases 1 "the negative control changes the nominal union case while retaining its source identity"
            let changed = { shape with Decls = declarations }
            let! rejected = Async.Catch(DeclarationCatalog.apply ctx changed (Pipeline.groupModules ctx changed))
            match rejected with
            | Choice1Of2 _ -> failtest "a changed union case must not reuse the producer's nominal union alias"
            | Choice2Of2 error -> Expect.stringContains error.Message "F# API mismatch" "equivalent source identity cannot bypass a changed union API"
        } |> Async.RunSynchronously

[<Tests>]
let keyofMemberKeys =
    testCase "an exported keyof literal union keys structural members by its values" <| fun _ ->
        use scratch = Scratch.directory "keyof-member-key"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "keyof-member-key-lab")
        let owner = Path.Combine(scratch.Path, "owner")
        let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Owner"; DeclarationCatalog = true }
        Pipeline.run config fixture owner |> Async.RunSynchronously |> ignore
        let output = Path.Combine(scratch.Path, "consumer")
        Pipeline.run { config with ModuleName = Some "Mini.Consumer"; Entry = Some "consumer.d.ts"; DeclarationReferences = [ Path.Combine(owner, "declarations.json") ] } fixture output
        |> Async.RunSynchronously |> ignore
        let consumer = File.ReadAllText(Path.Combine(output, "Mini.Consumer.fs"))
        Expect.stringContains consumer "holder: Mini.Owner.Holder" "a consumer reaching the union only through members reuses the producer's declarations"
