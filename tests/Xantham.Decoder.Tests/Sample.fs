module Fixture

open EasyBuild.FileSystemProvider
open Expecto
open Fake.Core
open Fake.JavaScript
open Xantham.Decoder

[<Literal>]
let repoRoot = __SOURCE_DIRECTORY__ + "/../.."

type Root = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
type RepoRoot = AbsoluteFileSystem<repoRoot>

module Npm =
    let setDir dir  = fun param -> { param with Npm.NpmParams.WorkingDirectory = dir }
    
type Fixture = {
    Name: string
    TypeDefinitionFile: string
    Target: string option
    Output: string
    Root: string
} with override this.ToString() = this.Name

module Virtual =
    [<Literal>]
    let agentsFixtureLayout = "
            node_modules/
                agents/
                    dist/
                        index.d.ts
            output.json"
    [<Literal>]
    let solidJsFixtureLayout = "
            node_modules/
                solid-js/
                    types/
                        index.d.ts
            output.json"
    [<Literal>]
    let threeFixtureLayout = "
            node_modules/
                @types/
                    three/
                        index.d.ts
            output.json"
    [<Literal>]
    let cloudFlareFixtureLayout = "
            node_modules/
                @cloudflare/
                    dynamic-workflows/
                        dist/
                            index.d.ts
                    workers-types/
                        index.d.ts
            output.json"
    
    type Agents =
        VirtualFileSystem<Root.fixtures.agents.``.``, agentsFixtureLayout>
    type SolidJs =
        VirtualFileSystem<Root.fixtures.``solid-js``.``.``, solidJsFixtureLayout>
    type Three =
        VirtualFileSystem<Root.fixtures.three.``.``, threeFixtureLayout>
    type DynamicWorkflows =
        VirtualFileSystem<Root.fixtures.``dynamic-workflows``.``.``, cloudFlareFixtureLayout>
    type WorkersTypes =
        VirtualFileSystem<Root.fixtures.``workers-types``.``.``, cloudFlareFixtureLayout>
        

module Fixtures =
    let private createProcess exe args dir =
        CreateProcess.fromRawCommand exe args
        |> CreateProcess.withWorkingDirectory dir
        |> CreateProcess.ensureExitCode
    let private node args dir = createProcess "node" args dir |> Proc.run |> ignore
    
    Npm.setDir RepoRoot.``.``
    |> Npm.run "build"

    let setup fixture =
        if System.IO.Path.Exists(fixture.TypeDefinitionFile) then fixture else
        Npm.setDir fixture.Root
        |> Npm.install
        fixture
        
    
    let encode fixture =
        try
        RepoRoot.``.``
        |> node [ "index.js"; fixture.Target |> Option.defaultValue fixture.TypeDefinitionFile; "-o"; fixture.Output ]
        Ok fixture
        with e -> Error(e)
    let decode fixture =
        match fixture with
        | Ok fixture ->
            try
            Runtime.create fixture.Output
            |> Ok
            with e -> Error e
        | Error e -> Error e
    
    let agents = {
        Name = "Agents"
        TypeDefinitionFile = Virtual.Agents.node_modules.agents.dist.``index.d.ts``
        Target = None
        Output = Virtual.Agents.``output.json``
        Root = Root.fixtures.agents.``.``
    }
    let solidjs = {
        Name = "SolidJs"
        TypeDefinitionFile = Virtual.SolidJs.node_modules.``solid-js``.types.``index.d.ts``
        Target = None
        Output = Virtual.SolidJs.``output.json``
        Root = Root.fixtures.``solid-js``.``.``
    }
    let three = {
        Name = "Three"
        TypeDefinitionFile = Virtual.Three.node_modules.``@types``.three.``index.d.ts``
        Target = None
        Output = Virtual.Three.``output.json``
        Root = Root.fixtures.three.``.``
    }
    let dynamicWorkflows = {
        Name = "Dynamic Workflows"
        TypeDefinitionFile = Virtual.DynamicWorkflows.node_modules.``@cloudflare``.``dynamic-workflows``.dist.``index.d.ts``
        Target = None
        Output = Virtual.DynamicWorkflows.``output.json``
        Root = Root.fixtures.``dynamic-workflows``.``.``
    }
    let workersTypes = {
        Name = "Workers Types"
        TypeDefinitionFile = Virtual.WorkersTypes.node_modules.``@cloudflare``.``workers-types``.``index.d.ts``
        Target = Some "@cloudflare/workers-types"
        Output = Virtual.WorkersTypes.``output.json``
        Root = Root.fixtures.``workers-types``.``.``
    }

let inline funApply f fn = fn f

[<Tests>]
let tests =
    testTheory "Decoder" [
        Fixtures.agents
        Fixtures.solidjs
        Fixtures.three
        Fixtures.dynamicWorkflows
        Fixtures.workersTypes
    ] <| fun fixture ->
        Fixtures.setup fixture
        |> Fixtures.encode
        |> Fixtures.decode
        |> Expect.isOk
        |> funApply ""
    

    

