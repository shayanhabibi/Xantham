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

// Dummy targets
Target.create Ops.runAllTests ignore

Target.create Ops.watchDocs <| fun _ ->
    dotnet [ "fsdocs"; "watch"; "--eval" ] root

Target.create Ops.restore <| fun _ ->
    dotnet [ "restore"; Solutions.Xantham; "--verbosity"; "q" ] root
    dotnet [ "tool"; "restore"; "--verbosity"; "q" ] root
Target.create Ops.clean <| fun _ -> !!"**/**/bin" -- "bin" |> Shell.cleanDirs

Target.create Ops.fableClean <| fun _ ->
    let func = fable [ "clean"; "-e"; ".fs.js"; "--yes" ]
    [|
        Projects.Directory.Fable.``.``
        Projects.Directory.FableCore.``.``
        Projects.Directory.Common.``.``
        Tests.Directory.Tests.``.``
    |]
    |> Array.Parallel.iter func

Target.create Ops.setupTest ignore

Target.create Ops.build <| fun _ ->
    Projects.FsProj.Generator
    |> DotNet.build (fun p -> {
        p with
            Configuration = DotNet.BuildConfiguration.Release
            DotNet.BuildOptions.MSBuildParams.DisableInternalBinLog = true
            DotNet.BuildOptions.MSBuildParams.Verbosity = Some MSBuildVerbosity.Quiet
    })
Target.create Ops.format <| fun _ ->
    sourceFiles
    |> Seq.map (sprintf "\"%s\"")
    |> String.concat " "
    |> DotNet.exec id "fantomas"
    |> function
        | { ExitCode = 0 } -> ()
        | result -> Trace.log $"Errors while formatting all files: %A{result.Messages}"
Target.create Ops.pack ignore

Target.create Ops.setupFableTest <| fun _ ->
    root |> if Args.npmCi then Npm.cleanInstall else Npm.install
    fable [ "-c"; "Debug"; "-o"; "dist/tests"; "-e"; ".fs.js" ] Tests.Directory.Fable.``.``

Target.create Ops.fableTest <| fun _ ->
    mocha [ Path.combine Tests.Directory.Fable.``.`` "dist/tests" ] root
    
Target.create Ops.postFableTest <| fun _ ->
    Trace.log "Running fable clean..."
    Target.runSimple Ops.fableClean []
    |> _.Error |> Option.iter raise
    
Target.create Ops.test <| fun _ ->
    dotnet [ "run" ] Tests.Directory.Generator.``.``
Target.create Ops.postTest <| fun _ ->
    Trace.log "Running clean..."
    Target.runSimple Ops.clean []
    |> _.Error |> Option.iter raise

Target.create Ops.fableTestWatch <| fun _ ->
    fable [
        "-e"; ".fs.js"
        "-c"; "Debug"
        "--noCache"
        "-o"; "output"
        "--watch"
        "--run"; "node"; "--watch"; "output/Program.fs.js"
    ] Projects.Directory.Fable.``.``
    Target.activateFinal Ops.postFableTest

Target.create Ops.fableTestSignal <| fun _ ->
    fable [
        "--noCache"
        Tests.Directory.Tests.``Signal.test.fsx``
        "--run"; "node"; "tests/Signal.test.fs.js"
    ] root
    Target.activateFinal Ops.postFableTest

Target.create Ops.fableBuild <| fun _ ->
    fable [
        "-e"; ".fs.js"
        "-c"; "Release"
        "--noCache"
        "-o"; "output"
    ] Projects.Directory.Fable.``.``

open Fake.Core.TargetOperators
let (|Stringify|_|) (op: Ops) (comp: string)  = comp = op
[<EntryPoint>]
let main argsv =
    let printHelp () = printfn $"%s{Cli.spec}"
    if argsv |> Array.isEmpty then printHelp (); 0
    else
    argsv |> Args.setArgs
    let dependencyMapping =
        Ops.restore ===> [
            Ops.clean ==> Ops.fableClean
            Ops.fableClean
            Ops.test
            Ops.fableTest
            Ops.setupTest
            Ops.setupFableTest
            Ops.format
            Ops.watchDocs
        ]
        // Running tests requires both test and fable test to be run
        Ops.runAllTests <== [
            Ops.postTest
            Ops.postFableTest <==? [
                Ops.fableTestSignal
            ]
            // Ops.fableTestSignal
        ]
        [
            // post test dep of packing/pushing unless skipping tests
            Ops.postTest
            =?> (Ops.pack, not Args.skipTests)
            
            Ops.postFableTest
            =?> (Ops.pack, not Args.skipTests)
            
            // setup test dep of test unless running in quick mode
            Ops.setupTest
            =?> (Ops.test, not Args.quick)
            ==> Ops.postTest
            
            Ops.setupFableTest
            ?=?> [
                Ops.fableTestWatch, not Args.quick
                Ops.fableTestSignal, not Args.quick
            ]
            =?> (Ops.fableTest, not Args.quick)
            ==> Ops.postFableTest
            
            Ops.setupFableTest
            ==> Ops.fableTestWatch
        ]
    let run =
        if Args.dryDebug
        then Target.printDependencyGraph true
        else Target.runOrDefaultWithArguments
    match argsv[0] with
    | _ when Args.help -> printHelp()
    | "run" -> run argsv[1]
    | Stringify Ops.tests -> run Ops.runAllTests
    | Stringify Ops.watch ->
        match argsv[1] with
        | "fable" -> run Ops.fableTestWatch
        | "docs" -> run Ops.watchDocs
        | _ -> printHelp()
    | Stringify Ops.test ->
        match argsv[1] with
        | "dotnet" -> run Ops.postTest
        | "fable" -> run Ops.postFableTest
        | "signal" -> run Ops.fableTestSignal
        | _ -> run Ops.runAllTests
    | Stringify Ops.fableTest -> run Ops.postFableTest
    | maybeTarget -> run maybeTarget
    0