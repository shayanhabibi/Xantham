/// Exercises the generated session layer.
///
/// Two kinds of test here. The coverage guard reads the emitted surface with reflection and needs
/// no compiler: it fails when the schema moves and nobody re-ran `tools/session-gen/generate.mjs`,
/// which is the failure mode a generated layer has and a hand-written one does not. The rest are
/// live, and their point is less the assertions than the code they are written in - not one line
/// below spells `Snapshot =` or `Project =`, and each is checked against the same call made the
/// long way.
module Xantham.TypeScript.Wire.Tests.Session

open System.IO
open System.Reflection
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")
let private exePath = Tsc.locate __SOURCE_DIRECTORY__
let private file name = DocumentIdentifier.FileName(Path.Combine(fixtures, name))

/// A fresh session per test, as in `Live.fs` and for the same reason: snapshot state from a
/// failing test would otherwise leak into its successors.
let private withSession (test: Session<TscChannel> -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use channel = new TscChannel(exe, fixtures)
            Api.initialize channel |> ignore
            let snapshot = channel.updateSnapshot(openProjects = [| file "tsconfig.json" |])
            test (channel.Session snapshot)

let private withMailboxSession (test: Session<TscMailbox> -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use mailbox = new TscMailbox(exe, fixtures)
            mailbox.initialize() |> Async.RunSynchronously |> ignore
            let snapshot = mailbox.updateSnapshot(openProjects = [| file "tsconfig.json" |]) |> Async.RunSynchronously
            test (mailbox.Session snapshot)

/// The extension members of one generated class, by name. `[<Extension>]` classes are static, and
/// every member on them carries the receiver as its first argument.
let private membersOf (name: string) =
    let assembly = typeof<Session<TscChannel>>.Assembly
    match assembly.GetType($"Xantham.TypeScript.Wire.{name}") with
    | null -> failtestf "no generated type named %s - the session layer did not compile as expected" name
    | ty ->
        ty.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.filter (fun m -> not m.IsSpecialName)
        |> Array.map _.Name
        |> Set.ofArray

/// The wire methods `Api` exposes, which is the population the session layer partitions.
let private apiFunctions =
    let assembly = typeof<Session<TscChannel>>.Assembly
    assembly.GetType("Xantham.TypeScript.Wire.Api").GetMethods(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.filter (fun m -> not m.IsSpecialName)
    |> Array.map _.Name
    |> Set.ofArray

[<Tests>]
let sessionCoverageTests =
    testList "session coverage" [
        // The generator's whole premise is that every method is reachable. If the schema grows one
        // and nobody regenerates, `Api` gains it and the session does not - which is exactly the
        // difference this subtracts.
        testCase "every Api method is reachable from a session or its Sessionless half" <| fun _ ->
            let reachable = Set.union (membersOf "SessionExtensions") (membersOf "SessionlessExtensions")
            Set.difference apiFunctions reachable
            |> Set.toList
            |> Flip.Expect.isEmpty
                "methods on Api with no session counterpart - re-run `dotnet fsi build.fsx -- generate --only session`"

        // The two surfaces are emitted from one method table for this reason. Anything that made
        // them diverge would be a caller writing against the sync layer and finding the async one
        // does not have it.
        testCase "the async surface mirrors the sync one" <| fun _ ->
            membersOf "AsyncSessionExtensions"
            |> Flip.Expect.equal "async session members match the sync ones" (membersOf "SessionExtensions")

        // `batchRequests` is the one deliberate asymmetry, and it is inherited from `AsyncApi`:
        // the mailbox is already the batcher, so a batch nested inside one would return a result
        // the layer cannot attribute to a caller.
        testCase "the async Sessionless half omits only batchRequests" <| fun _ ->
            Set.difference (membersOf "SessionlessExtensions") (membersOf "AsyncSessionlessExtensions")
            |> Flip.Expect.equal "only batchRequests is absent asynchronously" (Set.ofList [ "batchRequests" ])

        // The partition is the claim the layer rests on: a method belongs on the session when it
        // names a snapshot, and on `Sessionless` when it does not. `updateSnapshot` creates one
        // and so cannot presuppose it; `getSourceFile` reads one and so can.
        testCase "the partition puts snapshot-creating methods outside the session" <| fun _ ->
            let session = membersOf "SessionExtensions"
            let sessionless = membersOf "SessionlessExtensions"
            for method in [ "updateSnapshot"; "createProgram"; "initialize"; "transpileModule" ] do
                Set.contains method sessionless
                |> Flip.Expect.isTrue $"{method} takes no snapshot, so it belongs on Sessionless"
                Set.contains method session |> Flip.Expect.isFalse $"{method} is not a session method"
            for method in [ "getSourceFile"; "getSymbolAtPosition"; "getAnyType"; "release" ] do
                Set.contains method session |> Flip.Expect.isTrue $"{method} names a snapshot"
                Set.contains method sessionless |> Flip.Expect.isFalse $"{method} is not sessionless"
    ]

[<Tests>]
let sessionLiveTests =
    testList "session" [
        match exePath with
        | None ->
            testCase "native tsc not found - live tests skipped" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE"
        | Some _ ->

        // The claim under test is that eliding the pair changes nothing but the call site, so the
        // assertion is against the same call with the pair spelled out rather than against a
        // transcribed expectation.
        testCase "a session answers as the channel does with the pair spelled out" <| withSession (fun session ->
            let direct =
                Api.getSourceFileNames session.Transport
                    { Snapshot = session.Snapshot; Project = session.Project }

            session.getSourceFileNames()
            |> Flip.Expect.equal "session and explicit call agree" direct)

        testCase "an AST arrives through a session" <| withSession (fun session ->
            match session.getSourceFile(file "main.ts") with
            | ValueNone -> failtest "expected an AST for main.ts"
            | ValueSome ast -> ast.NodeCount > 0 |> Flip.Expect.isTrue "the AST has nodes")

        // Optional arguments still have to reach the wire as absent rather than as null, which is
        // the distinction the records' JsonIgnore attributes exist for. Every diagnostic method
        // takes an optional file list.
        testCase "an omitted optional argument is absent, not null" <| withSession (fun session ->
            session.getSemanticDiagnostics() |> ignore
            session.getSemanticDiagnostics(files = [| file "main.ts" |]) |> ignore)

        testCase "the async session answers as the sync one does" <| withMailboxSession (fun session ->
            let names = session.getSourceFileNames() |> Async.RunSynchronously
            names.Length > 0 |> Flip.Expect.isTrue "the project has source files"

            // Two in flight, so this pair really does travel through batchRequests.
            [ session.getSourceFile(file "main.ts"); session.getSourceFile(file "main.ts") ]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.iter (fun ast ->
                ast |> ValueOption.isSome |> Flip.Expect.isTrue "an AST arrives on the batched path"))

        // `Sessionless` exists so that opening a snapshot is reachable without a snapshot, and
        // `WithSnapshot` is how the result gets used. Together they are the loop a caller runs
        // when a file changes.
        testCase "Sessionless opens a snapshot the session can move to" <| withSession (fun session ->
            let updated = session.Sessionless.updateSnapshot(openProjects = [| file "tsconfig.json" |])
            updated.Snapshot > 0 |> Flip.Expect.isTrue "a snapshot id came back"

            let moved = session.WithSnapshot updated.Snapshot
            moved.Snapshot |> Flip.Expect.equal "the session moved" updated.Snapshot
            moved.Project |> Flip.Expect.equal "and stayed in its project" session.Project
            moved.getSourceFileNames() |> Array.isEmpty |> Flip.Expect.isFalse "the moved session works")

        // A symbol names the project its follow-up lookups should use, which is the whole reason
        // rebinding exists rather than a per-call project argument.
        testCase "ForSymbol retargets to the project the symbol names" <| withSession (fun session ->
            match session.getSymbolOfSourceFile(file "main.ts") with
            | ValueNone -> skiptest "main.ts has no module symbol to follow"
            | ValueSome symbol ->
                let rebound = session.ForSymbol symbol
                rebound.Project |> Flip.Expect.equal "the session took the symbol's project" symbol.Project
                rebound.Snapshot |> Flip.Expect.equal "and kept its snapshot" session.Snapshot

                let direct =
                    Api.getTypeOfSymbol session.Transport
                        { Snapshot = session.Snapshot; Project = symbol.Project; Symbol = symbol.Id }

                rebound.getTypeOfSymbol(symbol.Id).Id
                |> Flip.Expect.equal "the rebound session agrees with the explicit call" direct.Id)

        // The constructor overloads exist so a caller never pairs a snapshot with another
        // snapshot's project by hand. The single-project one is the common case and has to agree
        // with naming the project explicitly.
        testCase "the constructors agree on what a session is" <| fun _ ->
            match exePath with
            | None -> ()
            | Some exe ->
                use channel = new TscChannel(exe, fixtures)
                Api.initialize channel |> ignore
                let snapshot = channel.updateSnapshot(openProjects = [| file "tsconfig.json" |])
                let inferred = channel.Session snapshot
                let named = channel.Session(snapshot, snapshot.Projects[0].Id)
                inferred |> Flip.Expect.equal "inferring the only project matches naming it" named
                inferred.Snapshot |> Flip.Expect.equal "and carries the snapshot" snapshot.Snapshot
    ]
