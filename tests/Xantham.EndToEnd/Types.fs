[<AutoOpen>]
module Global

open Fake.Core
open Fake.IO
open Fake.JavaScript

module Literals =
    let [<Literal>] targetFramework = "net10.0"
    let [<Literal>] fablePackageReference = "Fable.Core"
    let [<Literal>] fablePackageVersion = "5.0.0"

[<CLIMutable>]
type TestFixture =
    {
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
    } with override this.ToString() = this.Name

type TestFixtureBuilder = {
    Name: string
    TypeDefinitionFilePath: string option
    TypeDefTarget: string option
    NpmPackage: string option
} with override this.ToString() = this.Name

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
    
    let build (fixtureBuilder: TestFixtureBuilder) =
        let replace = String.replace FileSystem.fixtureNameSigil fixtureBuilder.Name
        {
            Name = fixtureBuilder.Name
            Json = replace FileSystem.VirtualThis.output.``_FIXTURE_NAME_.json``
            FSharp = replace FileSystem.VirtualThis.output.``_FIXTURE_NAME_.fs``
            VerifyTarget = replace FileSystem.VirtualThis.verify.``_FIXTURE_NAME_.wrapped.fs``
            VerifyProject = replace FileSystem.VirtualThis.verify.``Verify._FIXTURE_NAME_.fsproj``
            TypeDefRoot = replace FileSystem.VirtualThis.fixtures._FIXTURE_NAME_.``.``
            TypeDefinitionFile =
                fixtureBuilder.TypeDefinitionFilePath
                |> Option.map (sprintf "node_modules/%s")
                |> Option.defaultValue $"node_modules/{FileSystem.fixtureNameSigil}/dist/index.d.ts"
                |> Path.combine FileSystem.VirtualThis.fixtures._FIXTURE_NAME_.``.``
                |> replace
            TypeDefTarget = fixtureBuilder.TypeDefTarget
            NpmPackage = fixtureBuilder.NpmPackage |> Option.defaultValue fixtureBuilder.Name
            NodePackagesJson = replace FileSystem.VirtualThis.fixtures._FIXTURE_NAME_.``package.json``
        }
    let private createVerifyProject (fixture: TestFixture) =
        File.create fixture.VerifyProject
        Xml.createDoc $"""<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <TargetFramework>{Literals.targetFramework}</TargetFramework>
        <IsPackable>false</IsPackable>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="{FileSystem.Source.Fable.Core.``Library.fs``}">
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
            Npm.install (fun p -> { p with WorkingDirectory = fixture.TypeDefRoot })
