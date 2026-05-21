[<AutoOpen>]
module SetupDriver

open Fake.Core
open Fake.IO

[<Literal>]
let private programSourceCode = """
module Program

open System
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Fabulous.AST
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

[<EntryPoint>]
let main argv =
    let inputFile, outputFile =
        match argv with
        | [| input; output |] -> input, output
        | _ ->
            eprintfn "Usage: dotnet run -- <input.json> <output.fs>"
            exit 1

    if not (IO.File.Exists inputFile) then
        eprintfn "Input file not found: %s" inputFile
        exit 1

    let tree = Decoder.Runtime.create inputFile
    let interner = tree.GetArenaInterner()

    // The Xantham defaults handle:
    //   - lib.es type substitutions (Error→exn, Array→ResizeArray, etc.)
    //   - Typescript-namespace pruning in TypePaths/MemberPaths
    //   - babel/typescript source ignore in IgnorePathRender
    // No Cloudflare-specific overrides are currently needed.
    let generatorContext: GeneratorContext = GeneratorContext.Empty

    ArenaInterner.prerenderTypeAliases generatorContext interner
    ArenaInterner.processExports generatorContext interner

    let renders =
        RootModule.collectModules generatorContext
        |> renderRoot generatorContext

    let output =
        Ast.Oak() {
            Ast.Namespace("Xantham") {
                Ast.Open "System"
                Ast.Open "Fable.Core"
                Ast.Open "Fable.Core.JS"
                Ast.Open "Fable.Core.JsInterop"
                renders
            }
        }
        |> Gen.mkOak
        |> Gen.run

    IO.File.WriteAllText(outputFile, output)
    printfn "Wrote %d bytes to %s" output.Length outputFile
    0

"""
    
let private createDriverProject ()  =
    File.create FileSystem.VirtualThis.driver.``Driver.fsproj``
    Xml.createDoc $"""
<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>{Literals.targetFramework}</TargetFramework>
        <RootNamespace>Driver</RootNamespace>
        <DefineConstants>CONCURRENT_DICT</DefineConstants>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="{FileSystem.VirtualThis.driver.``Program.fs``}"/>
    </ItemGroup>

    <ItemGroup>
      <ProjectReference Include="{FileSystem.Source.Decoder.``Xantham.Decoder.fsproj``}" />
      <ProjectReference Include="{FileSystem.Source.Generator.``Xantham.Generator.fsproj``}" />
    </ItemGroup>

</Project>
"""
    |> Xml.saveDoc FileSystem.VirtualThis.driver.``Driver.fsproj``

let private createDriverProgram () =
    File.create FileSystem.VirtualThis.driver.``Program.fs``
    programSourceCode
    |> File.writeString false FileSystem.VirtualThis.driver.``Program.fs``
 
let setupDriver =
    lazy
    createDriverProgram()
    createDriverProject()