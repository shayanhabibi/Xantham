module Build

open Fake.DotNet
open Spec
open Fake.Core
open Fake.IO
open Fake.Tools
open Fake.IO.Globbing.Operators

// disable warning of implicit conversion of ops to string
#nowarn 3391


initializeContext()

let private root = Root.``.``

let inline create (target: ^T when ^T:(member targetName: string)) (fn: _ -> unit) =
    Target.create target.targetName fn
let inline createAnchor (target: ^T when ^T:(member targetName: string)) = Target.create target.targetName ignore

type HouseKeeping with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | HouseKeeping.clean -> create <| fun _ ->
            !!"**/**/bin"
            --"bin"
            |> Shell.cleanDirs
        | HouseKeeping.fableClean -> create <| fun _ ->
            let func = fable [ "clean"; "-e"; ".fs.js"; "--yes" ]
            [|
                Projects.Directory.Fable.``.``
                Projects.Directory.FableCore.``.``
                Projects.Directory.Common.``.``
                Tests.Directory.Tests.``.``
            |]
            |> Array.Parallel.iter func
        | HouseKeeping.format -> create <| fun _ ->
            sourceFiles
            |> Seq.map (sprintf "\"%s\"")
            |> String.concat " "
            |> DotNet.exec id "fantomas"
            |> function
                | { ExitCode = 0 } -> ()
                | result -> Trace.log $"Errors while formatting all files: %A{result.Messages}"

type ProjectManagement with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | ProjectManagement.build -> create <| fun _ ->
            Projects.FsProj.Generator
            |> DotNet.build (fun p -> {
                p with
                    Configuration = DotNet.BuildConfiguration.Release
                    DotNet.BuildOptions.MSBuildParams.DisableInternalBinLog = true
                    DotNet.BuildOptions.MSBuildParams.Verbosity = Some MSBuildVerbosity.Quiet
            })
        | ProjectManagement.pack -> createAnchor()
        | ProjectManagement.restore -> create <| fun _ ->
            dotnet [ "restore"; Solutions.Xantham; "--verbosity"; "q" ] root
            dotnet [ "tool"; "restore"; "--verbosity"; "q" ] root
        | ProjectManagement.compile -> create <| fun _ ->
            if not Args.watch then
                fable [
                    "-e"; ".fs.js"
                    "-c"; "Release"
                    "--noCache"
                    "-o"; "output"
                ] Projects.Directory.Fable.``.``
            else
                fable [
                    "-e"; ".fs.js"
                    "-o"; "output"
                    "-s"; "--optimize"
                    "--watch"
                ] Tests.Directory.TypeScript.``.``
        | ProjectManagement.publish -> createAnchor()
        | ProjectManagement.publishNpm -> createAnchor()

type DotNetTestManagement with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | DotNetTestManagement.setup -> createAnchor()
        | DotNetTestManagement.test -> createAnchor()
        | DotNetTestManagement.postTest -> createAnchor()

type FableTestManagement with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | FableTestManagement.postTest -> createAnchor()
        | FableTestManagement.setup -> createAnchor()
        | FableTestManagement.test -> createAnchor()

type AuxiliaryTests with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | AuxiliaryTests.signalSetup 
        | AuxiliaryTests.signalTest 
        | AuxiliaryTests.signalPost 
        | AuxiliaryTests.loggingSetup 
        | AuxiliaryTests.loggingTest 
        | AuxiliaryTests.loggingPost -> createAnchor()

type DocManagement with
    member this.buildTarget =
        let inline create fn = create this fn
        let inline createAnchor() = createAnchor this
        match this with
        | DocManagement.build -> createAnchor()
        | DocManagement.watch -> create <| fun _ ->
            dotnet [ "fsdocs"; "watch"; "--eval" ] root


Reflection.buildTargets<HouseKeeping> _.buildTarget
Reflection.buildTargets<ProjectManagement> _.buildTarget
Reflection.buildTargets<DotNetTestManagement> _.buildTarget
Reflection.buildTargets<FableTestManagement> _.buildTarget
Reflection.buildTargets<AuxiliaryTests> _.buildTarget
Reflection.buildTargets<DocManagement> _.buildTarget

//             
//
// /// <summary>
// /// Ops are targets for FAKE dependency graph.
// /// Try to partition them into composing actions
// /// </summary>
// let buildOps (op: Ops) =
//     let inline create fn = Target.create op fn
//     let inline createAnchor () = Target.create op <| fun _ -> ()
//     match op with
//     | Ops.restore -> create <| fun _ ->
//         dotnet [ "restore"; Solutions.Xantham; "--verbosity"; "q" ] root
//         dotnet [ "tool"; "restore"; "--verbosity"; "q" ] root
//     | Ops.watchDocs -> create <| fun _ ->
//         dotnet [ "fsdocs"; "watch"; "--eval" ] root
//     | Ops.build -> create <| fun _ ->
//     | Ops.pack -> createAnchor()
//     | Ops.push -> failwith "todo"
//     | Ops.setupFableTest -> create <| fun _ ->
//         root |> if Args.npmCi then Npm.cleanInstall else Npm.install
//         fable [ "-c"; "Debug"; "-o"; "dist/tests"; "-e"; ".fs.js" ] Tests.Directory.Fable.``.``
//     | Ops.setupTest -> createAnchor()
//     | Ops.fableTest -> create <| fun _ ->
//         mocha [ Path.combine Tests.Directory.Fable.``.`` "dist/tests" ] root
//     | Ops.test -> create <| fun _ ->
//         dotnet [ "run" ] Tests.Directory.Generator.``.``
//     | Ops.fableTestSignal -> create <| fun _ ->
//         fable [
//             "--noCache"
//             Tests.Directory.Tests.``Signal.test.fsx``
//             "--run"; "node"; "tests/Signal.test.fs.js"
//         ] root
//         Target.activateFinal Ops.postFableTest
//     | Ops.postFableTest -> create <| fun _ ->
//         Trace.log "Running fable clean..."
//         Target.runSimple Ops.fableClean []
//         |> _.Error |> Option.iter raise
//     | Ops.postTest -> create <| fun _ ->
//         Trace.log "Running clean..."
//         Target.runSimple Ops.clean []
//         |> _.Error |> Option.iter raise
//     | Ops.runAllTests -> createAnchor()
//     | Ops.tests -> failwith "todo"
//     | Ops.format -> create <| fun _ ->
//         sourceFiles
//         |> Seq.map (sprintf "\"%s\"")
//         |> String.concat " "
//         |> DotNet.exec id "fantomas"
//         |> function
//             | { ExitCode = 0 } -> ()
//             | result -> Trace.log $"Errors while formatting all files: %A{result.Messages}"
//     | Ops.fableTestWatch -> create <| fun _ ->
//     | Ops.fableBuild -> create <| fun _ ->
//         fable [
//             "-e"; ".fs.js"
//             "-c"; "Release"
//             "--noCache"
//             "-o"; "output"
//         ] Projects.Directory.Fable.``.``
//     | Ops.watch -> createAnchor()


open FSharp.SystemCommandLine


[<RequireQualifiedAccess>]
type TestTargets =
    | dotnet
    | typescript
    | ``aux:signal``
    | ``aux:logging``
    member this.commandName = this.ToString()
    member this.command =
        match this with
        | TestTargets.``aux:logging`` -> None
        | TestTargets.``aux:signal`` -> None
        | TestTargets.dotnet -> None
        | TestTargets.typescript ->
            command this.commandName {
                description "Xantham.TypeScript test suites"
                inputs Input.context
                setAction (runTarget FableTestManagement.postTest)
                addInputs testOptions
            } |> Some

[<RequireQualifiedAccess>]
type WatchCommands =
    | compile
    | test
    | run
    member this.commandName = this.ToString()
    member this.command =
        let command = command this.commandName
        match this with
        | WatchCommands.compile ->
            command {
                description "Monitor fable projects and recompile them when changes are detected."
                inputs Input.context
                setAction (runTarget ProjectManagement.compile)
            }
        | WatchCommands.test ->
            command {
                description "Run tests in watch mode, automatically rebuilding/recompiling and running when changes are detected."
                inputs Input.context
                helpAction
            }
        | WatchCommands.run ->
            command {
                description "Run projects in watch mode, automatically rebuilding/recompiling and running when changes are detected."
                inputs Input.context
                helpAction
            }

[<RequireQualifiedAccess>]
type RootCommands =
    | run
    | test
    | watch
    static member inline op_Implicit(this: RootCommands) = this.ToString()
    member this.command =
        let command = command this
        match this with
        | RootCommands.run ->
            command {
                description "Run a FAKE target in isolation (for development)"
                inputs Input.context
                helpAction
                addCommands (Reflection.commandsFrom<HouseKeeping> _.runCommand)
                addCommands (Reflection.commandsFrom<ProjectManagement> _.runCommand)
                addCommands (Reflection.commandsFrom<DotNetTestManagement> _.runCommand)
                addCommands (Reflection.commandsFrom<AuxiliaryTests> _.runCommand)
                addCommands (Reflection.commandsFrom<DocManagement> _.runCommand)
                addInputs (
                    List.concat [
                        publishingOptions
                        npmOptions
                        testOptions
                        globalOptions
                    ]
                    |> List.distinct
                    )
                
            }
        | RootCommands.test ->
            command {
                description "Run a test suite for xantham."
                inputs Input.context
                helpAction
                addCommands Reflection.commands<TestTargets>
            }
        | RootCommands.watch ->
            command {
                description "Commands for Xantham in watch mode."
                inputs Input.context
                helpAction
                addCommands Reflection.commands<WatchCommands>
            }

open Fake.Core.TargetOperators
open FSharp.SystemCommandLine
[<EntryPoint>]
let rec main argsv =
    let generalDependencyMapping = fun () ->
        ProjectManagement.restore ===> [
            HouseKeeping.clean
            ==> HouseKeeping.fableClean
            
            HouseKeeping.clean
            
            DotNetTestManagement.setup
            DotNetTestManagement.test
            DotNetTestManagement.postTest
            
            FableTestManagement.setup
            FableTestManagement.test
            FableTestManagement.postTest
            
            HouseKeeping.format
            DocManagement.watch
            DocManagement.build
            
            AuxiliaryTests.signalSetup
            AuxiliaryTests.signalTest
            AuxiliaryTests.signalPost
            
            AuxiliaryTests.loggingSetup
            AuxiliaryTests.loggingTest
            AuxiliaryTests.loggingPost
        ]
        
        
        FableTestManagement.setup <==? [
            AuxiliaryTests.signalPost
            AuxiliaryTests.loggingPost
        ]
        =?> (HouseKeeping.format, Args.format)
        =?> (FableTestManagement.test, not Args.skipTests)
        ?=> FableTestManagement.test
        ==> FableTestManagement.postTest
        =?> (ProjectManagement.pack, not Args.skipTests)
        ?=> DocManagement.build
        |> ignore
        
        DotNetTestManagement.setup
        =?> (HouseKeeping.format, Args.format)
        =?> (DotNetTestManagement.test, not Args.skipTests)
        ?=> DotNetTestManagement.test
        ==> DotNetTestManagement.postTest
        =?> (ProjectManagement.pack, not Args.skipTests)
        ?=> DocManagement.build
        |> ignore
        
    setDependencies generalDependencyMapping

    rootCommand argsv {
        description "Xantham"
        inputs Input.context
        helpAction
        addInputs globalOptions
        addCommands Reflection.commands<RootCommands>
    }