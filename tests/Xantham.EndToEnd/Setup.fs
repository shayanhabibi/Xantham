[<AutoOpen>]
module Global

open EasyBuild.FileSystemProvider
open Fake.Core
open Fake.IO
open Fake.JavaScript

module private Literals =
    let [<Literal>] targetFramework = "net10.0"
    let [<Literal>] fablePackageReference = "Fable.Core"
    let [<Literal>] fablePackageVersion = "5.0.0"
    let [<Literal>] repoRoot = __SOURCE_DIRECTORY__ + "/../.."

type TestFixture = {
    Name: string
    Json: string
    FSharp: string
    VerifyTarget: string
    VerifyProject: string
    TypeDefRoot: string
    TypeDefinitionFile: string
    TypeDefTarget: string option
} with
    override this.ToString() = this.Name

module Xantham =
    module Fable =
        [<Literal>]
        let private source = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Fable.Core"
        type Core = AbsoluteFileSystem<source>
    [<Literal>]
    let private fableSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Fable"
    type Fable = AbsoluteFileSystem<fableSource>
    [<Literal>]
    let private decoderSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Decoder"
    type Decoder = AbsoluteFileSystem<decoderSource>
    [<Literal>]
    let private generatorSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Generator"
    type Generator = AbsoluteFileSystem<generatorSource>
    [<Literal>]
    let private endToEndDriverSource = __SOURCE_DIRECTORY__ + "/driver"
    type Driver = AbsoluteFileSystem<endToEndDriverSource>
    
type Root = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
type RepoRoot = AbsoluteFileSystem<Literals.repoRoot>
type VirtualRoot = VirtualFileSystem<__SOURCE_DIRECTORY__, "
    output/
        agents.json
        agents.fs

        dynamic-workflows.json
        dynamic-workflows.fs

        workers-types.fs
        workers-types.json
    verify/
        agents.wrapped.fs
        Verify.Agents.fsproj
        
        dynamic-workflows.wrapped.fs
        Verify.DynamicWorkflows.fsproj

        workersTypes.wrapped.fs
        Verify.WorkersTypes.fsproj
">

module VirtualRoot =
    let setupDirectories =
        lazy
        [
            VirtualRoot.output.``.``
            VirtualRoot.verify.``.``
        ] |> List.iter Directory.ensure

let private createProcess exe numItems args dir =
    CreateProcess.fromRawCommand exe args
    |> CreateProcess.withWorkingDirectory dir
    |> CreateProcess.redirectOutput
    |> CreateProcess.mapResult (function
        | { Error = null | ""; Output = text } ->
            Ok, text
        | { Error = text } ->
            Error, text
        >> fun (fn, text) ->
            text
            |> String.convertTextToWindowsLineBreaks
            |> String.splitStr String.WindowsLineBreaks
            |> List.rev
            |> List.truncate numItems
            |> List.rev
            |> String.concat String.WindowsLineBreaks
            |> fn
            )

let dotnet args dir =
    createProcess "dotnet" 3 args dir |> Proc.run

let dotnetx args dir =
    createProcess "dotnet" 3 args dir |> CreateProcess.ensureExitCode |> Proc.run |> ignore

let node args dir =
    createProcess "node" 3 args dir |> Proc.run
    
let nodex args dir =
    createProcess "node" 3 args dir |> CreateProcess.ensureExitCode |> Proc.run |> ignore

module Npm =
    let setDir dir = fun param -> { param with Npm.NpmParams.WorkingDirectory = dir }

module TestFixture =
    let private createVerifyProject (fixture: TestFixture) =
        File.create fixture.VerifyProject
        Xml.createDoc $"""<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <TargetFramework>{Literals.targetFramework}</TargetFramework>
        <IsPackable>false</IsPackable>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="{Xantham.Fable.Core.``Library.fs``}">
            <Link>XanthamFableCoreLibrary.fs</Link>
        </Compile>
        <Compile Include="{fixture.VerifyTarget}" />
    </ItemGroup>

    <ItemGroup>
        <PackageReference Include="{Literals.fablePackageReference}" Version="{Literals.fablePackageVersion}" />
    </ItemGroup>

</Project>
"""
        |> Xml.saveDoc fixture.VerifyProject
        
    let setup (fixture: TestFixture) =
        VirtualRoot.setupDirectories.Value
        if not <| File.exists fixture.VerifyProject then
            createVerifyProject fixture