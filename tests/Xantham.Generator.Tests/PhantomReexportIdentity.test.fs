module Xantham.Generator.Tests.PhantomReexportIdentityTests

open System.IO
open System.Text.Json
open System.Diagnostics
open Expecto
open Xantham.Generator

let private compile directory (consumer: string) =
    let repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
    File.Copy(Path.Combine(repository, "global.json"), Path.Combine(directory, "global.json"), true)
    File.WriteAllText(Path.Combine(directory, "Consumer.fs"), consumer)
    File.WriteAllText(Path.Combine(directory, "Consumer.fsproj"), $"""<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>net10.0</TargetFramework><NuGetAudit>false</NuGetAudit></PropertyGroup>
<ItemGroup><Compile Include="owner/Mini.Aliases.fs"/><Compile Include="Consumer.fs"/></ItemGroup>
<ItemGroup><PackageReference Include="Fable.Core" Version="5.5.0"/><ProjectReference Include="{repository}/src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj"/></ItemGroup>
</Project>""")
    let start = ProcessStartInfo("dotnet", WorkingDirectory = directory, RedirectStandardOutput = true, RedirectStandardError = true)
    for argument in [ "build"; "Consumer.fsproj"; "--disable-build-servers"; "-m:1"; "-p:BuildProjectReferences=false"; "--configuration";
#if DEBUG
                      "Debug"
#else
                      "Release"
#endif
                      "--verbosity"; "quiet" ] do start.ArgumentList.Add argument
    use child = Process.Start start
    let output = child.StandardOutput.ReadToEndAsync()
    let errors = child.StandardError.ReadToEndAsync()
    if not (child.WaitForExit 60000) then
        child.Kill(entireProcessTree = true)
        child.WaitForExit()
        failtest "the owned reexport consumer compile exceeded sixty seconds"
    child.ExitCode, output.Result + errors.Result

[<Tests>]
let tests =
    testCase "faithful phantom reexports retain their authenticated declaration API" <| fun _ ->
        use scratch = Scratch.directory "phantom-reexport-identity"
        let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "phantom-reexport-identity-lab")
        Pipeline.run { GeneratorConfig.Default with ModuleName = Some "Mini.Aliases"; DeclarationCatalog = true } fixture (Path.Combine(scratch.Path, "owner"))
        |> Async.RunSynchronously |> ignore
        let code, log = compile scratch.Path """module Mini.Check
let exact (value: Mini.Aliases.Mirror.Compute<string, float>) : Mini.Aliases.Original.Compute<string, float> = value
"""
        Expect.equal code 0 log
        async {
            let config = { GeneratorConfig.Default with ModuleName = Some "Mini.Consumer"; DeclarationCatalog = true; DeclarationReferences = [Path.Combine(scratch.Path, "owner", "declarations.json")] }
            let! mailbox, ctx = Bootstrap.start config fixture
            use _ = mailbox :> System.IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolved, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
            let! shape, _ = Pipeline.runTier ctx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage config ctx.PackageName) resolved)
            let apply candidate = DeclarationCatalog.apply ctx candidate (Pipeline.groupModules ctx candidate)
            let! _ = apply shape
            let alias = shape.Decls |> List.pick (function FsAbbrev alias when alias.Name = "Mirror.Compute" -> Some alias | _ -> None)
            let target, arguments = match alias.Target with FsApp(name, arguments) -> name, arguments | other -> failwithf "expected generic forwarding target, got %A" other
            let mutateAlias change =
                { shape with Decls = shape.Decls |> List.map (function FsAbbrev current when current.Name = alias.Name -> FsAbbrev(change current) | decl -> decl) }
            let alpha = mutateAlias (fun current ->
                let parameters = current.TypeParameters |> List.mapi (fun index parameter -> { parameter with Name = "Renamed" + string index })
                { current with TypeParameters = parameters; Target = FsApp(target, parameters |> List.map (fun parameter -> FsTypeVar parameter.Name)) })
            let! _ = apply alpha
            let badCarrier =
                { shape with Decls = shape.Decls |> List.map (function FsPhantom current when current.Name = target -> FsPhantom { current with Carrier = FsNamed "string" } | decl -> decl) }
            let badCases = [
                "permuted", mutateAlias (fun current -> { current with Target = FsApp(target, List.rev arguments) }), "F# API mismatch"
                "partial", mutateAlias (fun current -> { current with Target = FsApp(target, [arguments.Head]) }), "F# API mismatch"
                "concrete", mutateAlias (fun current -> { current with Target = FsApp(target, FsNamed "obj" :: arguments.Tail) }), "F# API mismatch"
                "constraints", mutateAlias (fun current -> { current with TypeParameters = { current.TypeParameters.Head with Constraint = Some(FsNamed "System.IDisposable") } :: current.TypeParameters.Tail }), "constraint mismatch"
                "value", mutateAlias (fun current -> { current with Value = Some(ImportNamed "Compute", FsNamed "obj") }), "F# API mismatch"
                "same carrier distinct source", mutateAlias (fun current -> { current with Target = FsApp("Separate.Compute", arguments) }), "F# API mismatch"
                "structural source", mutateAlias (fun current -> { current with Target = FsApp("Structural.Compute", arguments) }), "F# API mismatch"
                "cycle", mutateAlias (fun current -> { current with Target = FsApp(current.Name, arguments) }), "F# API mismatch"
                "carrier", badCarrier, "F# API mismatch"
            ]
            for label, candidate, expected in badCases do
                let! result = Async.Catch(apply candidate)
                match result with
                | Choice1Of2 _ -> failtest (label + " must not borrow a defining declaration's surface")
                | Choice2Of2 error -> Expect.stringContains error.Message expected label

            // A changed current target must not redefine the original target's base API.
            let localConfig = { config with DeclarationReferences = [] }
            let! localMailbox, localCtx = Bootstrap.start localConfig fixture
            use _ = localMailbox :> System.IDisposable
            let! localHarvest, _ = Pipeline.runTier localCtx Harvest.passes HarvestModel.Empty
            let! localResolved, _ = Pipeline.runTier localCtx Resolve.passes (Pipeline.toResolve localHarvest)
            let! localShape, _ = Pipeline.runTier localCtx Shape.Passes.passes (Pipeline.toShape (GeneratorConfig.runtimePackage localConfig localCtx.PackageName) localResolved)
            let localBadCarrier = { localShape with Decls = localShape.Decls |> List.map (function FsPhantom current when current.Name = target -> FsPhantom { current with Carrier = FsNamed "string" } | decl -> decl) }
            let! _, catalog, _ = DeclarationCatalog.applyWith (Some "carrier-change") Map.empty localShape localCtx localBadCarrier (Pipeline.groupModules localCtx localBadCarrier)
            use document = JsonDocument.Parse(catalog.Value)
            let variants = document.RootElement.GetProperty("variants").EnumerateArray() |> Seq.toArray
            Expect.isNonEmpty variants "a customized target carrier records an API variant"
            for variant in variants do
                Expect.isTrue (variant.GetProperty("contractChanged").GetBoolean()) "original and current defining maps remain isolated"
                Expect.notEqual (variant.GetProperty("baseApi").GetString()) (variant.GetProperty("api").GetString()) "changed current carrier cannot rewrite its original base API"
            let customized = Path.Combine(scratch.Path, "customized.json")
            File.WriteAllText(customized, catalog.Value)
            let! reused = Async.Catch(Pipeline.run { config with ModuleName = Some "Mini.Reuse"; DeclarationReferences = [customized] } fixture (Path.Combine(scratch.Path, "invalid-reuse")))
            match reused with
            | Choice1Of2 _ -> failtest "a changed target contract must not certify a raw consumer"
            | Choice2Of2 error -> Expect.stringContains error.Message "customization replacement changed the TypeScript contract" "changed original/current carrier remains an authenticated mismatch"
        } |> Async.RunSynchronously
