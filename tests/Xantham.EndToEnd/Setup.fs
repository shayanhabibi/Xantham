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
    NpmPackage: string
    NodePackagesJson: string
} with
    override this.ToString() = this.Name

type TestFixtureBuilder = {
    Name: string
    TypeDefinitionFilePath: string option
    TypeDefTarget: string option
    NpmPackage: string option
} with override this.ToString() = this.Name

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
    fixtures/
        _FIXTURE_NAME_/
            package.json
    output/
        _FIXTURE_NAME_.json
        _FIXTURE_NAME_.fs
    verify/
        _FIXTURE_NAME_.wrapped.fs
        Verify._FIXTURE_NAME_.fsproj
">

module VirtualRoot =
    let setupDirectories =
        lazy
        [
            VirtualRoot.fixtures.``.``
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
            if numItems = -1 then fn text else
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
    createProcess "node" -1 args dir |> Proc.run
    
let nodex args dir =
    createProcess "node" -1 args dir |> CreateProcess.ensureExitCode |> Proc.run |> ignore

module Npm =
    let setDir dir = fun param -> { param with Npm.NpmParams.WorkingDirectory = dir }

module TestFixture =
    open Farse
    let create name = {
        TestFixtureBuilder.Name = name
        TypeDefinitionFilePath = None
        TypeDefTarget = None
        NpmPackage = None
    }
    let withNpmPackage npmPackage fixture = { fixture with TestFixtureBuilder.NpmPackage = Some npmPackage }
    let withTypeDefnFilePath filePath fixture = { fixture with TestFixtureBuilder.TypeDefinitionFilePath = Some filePath }
    let withTypeDefnTarget target fixture = { fixture with TestFixtureBuilder.TypeDefTarget = Some target }
    let private replace placeholderReplacement = String.replace "_FIXTURE_NAME_" placeholderReplacement
    let build (fixtureBuilder: TestFixtureBuilder) =
        let replace = replace fixtureBuilder.Name
        {
            Name = fixtureBuilder.Name
            Json = replace VirtualRoot.output.``_FIXTURE_NAME_.json``
            FSharp = replace VirtualRoot.output.``_FIXTURE_NAME_.fs``
            VerifyTarget = replace VirtualRoot.verify.``_FIXTURE_NAME_.wrapped.fs``
            VerifyProject = replace VirtualRoot.verify.``Verify._FIXTURE_NAME_.fsproj``
            TypeDefRoot = replace VirtualRoot.fixtures._FIXTURE_NAME_.``.``
            TypeDefinitionFile =
                fixtureBuilder.TypeDefinitionFilePath
                |> Option.map (sprintf "node_modules/%s")
                |> Option.defaultValue "node_modules/_FIXTURE_NAME_/dist/index.d.ts"
                |> Path.combine VirtualRoot.fixtures._FIXTURE_NAME_.``.``
                |> replace
            TypeDefTarget = fixtureBuilder.TypeDefTarget
            NpmPackage = fixtureBuilder.NpmPackage |> Option.defaultValue fixtureBuilder.Name
            NodePackagesJson = replace VirtualRoot.fixtures._FIXTURE_NAME_.``package.json``
        }
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
        Directory.ensure fixture.TypeDefRoot
        if not <| File.exists fixture.NodePackagesJson then
            File.create fixture.NodePackagesJson
            JObj [
                "name", JStr "xantham"
                "type", JStr "module"
                "dependencies", JObj [
                    fixture.NpmPackage, JStr "*"
                ]
            ]
            |> Json.asString Indented
            |> File.writeString false fixture.NodePackagesJson
        if not <| File.exists fixture.TypeDefinitionFile then
            Npm.setDir fixture.TypeDefRoot
            |> Npm.install