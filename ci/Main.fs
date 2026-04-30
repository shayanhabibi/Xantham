module Build

open Spec
open Fake.Core
open Fake.IO
open Fake.Tools

// disable warning of implicit conversion of ops to string
#nowarn 3391


initializeContext()

let private root = Root.``.``

Target.create Ops.restore (ignore >> Xantham.restore)
Target.create Ops.clean (ignore >> Xantham.clean)
Target.create Ops.fableClean (ignore >> Xantham.Fable.clean)
Target.create Ops.setupTest ignore
Target.create Ops.format (ignore >> Xantham.format)
Target.create Ops.pack ignore
Target.create Ops.tests ignore

Target.create Ops.setupFableTest <| fun _ ->
    root |> if Args.npmCi then Npm.cleanInstall else Npm.install

Target.create Ops.fableTest (ignore >> Xantham.Fable.test)
Target.create Ops.postFableTest (ignore >> Xantham.Fable.clean)
Target.create Ops.test (ignore >> Xantham.test)
Target.create Ops.postTest (ignore >> Xantham.clean)

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
            Ops.test
            Ops.fableTest
            Ops.format
        ]
        Ops.tests <== [
            Ops.postTest
            Ops.postFableTest
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
            =?> (Ops.fableTest, not Args.quick)
            ==> Ops.postFableTest
            
        ]
    let run =
        if Args.dryDebug
        then Target.printDependencyGraph true
        else Target.runOrDefaultWithArguments
    match argsv[0] with
    | Stringify Ops.test -> run Ops.postTest
    | Stringify Ops.fableTest -> run Ops.postFableTest
    | _ when Args.help -> printHelp()
    | maybeTarget -> run maybeTarget
    0