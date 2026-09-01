#r "nuget: Fake.JavaScript.Npm"
#r "nuget: EasyBuild.FileSystemProvider"
#r "nuget: Farse"

(*
Central setup for installing test fixtures for Xantham Test projects.
Forcefully writes the package.json if it is absent.
*)

open EasyBuild.FileSystemProvider
open Fake.JavaScript
open Fake.IO
open Fake.Core
open Farse

let apply fn x = fn x; x

module FileSystem =
    [<Literal>]
    let private repoRoot = __SOURCE_DIRECTORY__ + "/.."
    type This = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
    type Repo = AbsoluteFileSystem<repoRoot>
    type VirtualThis = VirtualFileSystem<This.``.``, "
        fixtures/
            _FIXTURE_NAME_/
                package.json
    ">
type Fixture = { Name: string; Version: string voption }
module Fixture =
    let create name = { Name = name; Version = ValueNone }
    let createWithVersion name version = { Name = name; Version = ValueSome version }
    let withVersion version fixture = { fixture with Version = ValueSome version }
    let toKeyValuePair fixture = (fixture.Name, fixture.Version |> ValueOption.defaultValue "*" |> JStr)
type PackageJson = { Dependencies: Fixture list }
module PackageJson =
    let create fixtures = { Dependencies = fixtures }
    let toJson packageJson = JObj [
        "name", JStr "xantham-test-fixtures"
        "type", JStr "module"
        
        "dependencies",
        packageJson.Dependencies |> List.map Fixture.toKeyValuePair |> JObj
    ]
    let toString = toJson >> Json.asString JsonFormat.Indented
    let createSingletonString fixture = JObj [
        "name", JStr fixture.Name
        "type", JStr "module"
        "dependencies", JObj [ Fixture.toKeyValuePair fixture ]
    ]



// ===============================================
//              IMPLEMENTATION
// ===============================================

let replaceSymbol fixture = String.replace "_FIXTURE_NAME_" fixture.Name 
[
    let create = Fixture.create
    let createTypes = sprintf "@types/%s" >> create
    let createCloudFlare = sprintf "@cloudflare/%s" >> create
    
    create "agents"
    create "solid-js"
    create "ansi-regex"
    create "type-fest"
    create "animejs"
    create "typescript"
    
    createTypes "three"
    createTypes "d3"
    createTypes "node"
    createTypes "semver"
    createTypes "lodash"
    
    createCloudFlare "workers-types"
    createCloudFlare "dynamic-workflows"
    createCloudFlare "ai-chat"
    createCloudFlare "containers"
    createCloudFlare "puppeteer"
    createCloudFlare "sandbox"
    createCloudFlare "shell"
    createCloudFlare "think"
    createCloudFlare "voice"
    createCloudFlare "worker-bundler"
]
|> List.iter (fun fixture ->
    FileSystem.VirtualThis.fixtures._FIXTURE_NAME_.``.``
    |> replaceSymbol fixture
    |> apply Directory.ensure
    |> fun dir ->
        let filePath = Path.combine dir "package.json"
        filePath
        |> File.create
        PackageJson.createSingletonString fixture
        |> Json.asString JsonFormat.Indented
        |> File.writeString false filePath
        Npm.install <| fun p -> { p with WorkingDirectory = dir }
    )