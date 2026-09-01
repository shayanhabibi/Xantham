#i "nuget: C:/users/shaya/riderprojects/xantham/bin"
#r "nuget: Xantham.TypeScript.Wire"
#r "nuget: Partas.TypeProvider.BuildHelper"

open Xantham.TypeScript.Wire
open Partas.TypeProvider.BuildHelper
open Xantham.TypeScript.Wire.Proto

type Repo = BuildHelperProvider<__SOURCE_DIRECTORY__, capabilityFullOverride = true>

let private exePath =
    Tsc.locate <| Repo.FileSystem.``Xantham.TypeScript.Wire.Tests``.ToString()

let private workerTypes = Repo.FileSystem.fixtures.``@cloudflare``.``workers-types``

let channel = new TscMailbox(exePath.Value, workerTypes.ToString())
Async.RunSynchronously(AsyncApi.initialize channel) |> ignore

let private indexDts =
    workerTypes.node_modules.``@cloudflare``.``workers-types``.``index.d.ts``.ToString()

// Every field of CreateProgramOptions is either optional or a default of its own, so the whole
// options record is `Default` - copy-update it to set the ones this run cares about. Without a
// root file the program is empty, so that one is passed.
let program =
    Async.RunSynchronously(
        channel.createProgram(CreateProgramOptions.Default, rootFiles = [| DocumentIdentifier.FileName indexDts |]))

let snapshot = program.Snapshot
let project = program.Project.Value.Id

Async.RunSynchronously(channel.getSourceFileNames(snapshot, project))

Async.RunSynchronously(channel.getSourceFile(snapshot, project, DocumentIdentifier.FileName indexDts))
