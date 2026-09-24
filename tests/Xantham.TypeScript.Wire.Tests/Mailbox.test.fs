module Xantham.TypeScript.Wire.Tests.Mailbox

open System
open System.IO
open System.Threading
open Expecto
open Xantham.TypeScript.Wire

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")

let private exePath = Tsc.locate __SOURCE_DIRECTORY__

let private file name = Proto.DocumentIdentifier.FileName(Path.Combine(fixtures, name))

let private commandLine: Proto.ParseCommandLineParams =
    { CommandLine = ValueSome [| "--strict"; "main.ts" |] }

/// The mailbox owns its channel: it is constructed internally and never handed out, so there is
/// no way to hold both and use the sync surface behind the agent's back.
let private withMailbox (test: TscMailbox -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use mailbox = new TscMailbox(exe, fixtures)
            mailbox.initialize () |> Async.RunSynchronously |> ignore
            test mailbox

[<Tests>]
let mailboxTests =
    testList "mailbox" [
        match exePath with
        | None ->
            testCase "native tsc not found - mailbox tests skipped" <| fun _ ->
                skiptest "run `npm install` at the repository root"
        | Some _ ->

        // The agent is a loop, not a single Receive. A body that served one message and completed
        // would pass this at i = 1 and hang forever at i = 2.
        testCase "the agent serves more than one request" <| withMailbox (fun mailbox ->
            for _ in 1 .. 3 do
                let response = mailbox.parseCommandLine commandLine |> Async.RunSynchronously
                response.FileNames.Length |> Flip.Expect.equal "fileNames" 1)

        // A failure has to reach the caller. Letting it escape the agent would kill the loop and
        // turn every later call into the same hang.
        testCase "a server error raises rather than hanging" <| withMailbox (fun mailbox ->
            Expect.throwsT<TsGoError>
                (fun () ->
                    mailbox.Request<Proto.ParseCommandLineParams, Proto.ConfigFileResponse>("noSuchMethod", commandLine)
                    |> Async.RunSynchronously
                    |> ignore)
                "an unknown method"

            // ...and the agent is still serving afterwards.
            (mailbox.parseCommandLine commandLine |> Async.RunSynchronously).FileNames.Length
            |> Flip.Expect.equal "still alive" 1)

        testCase "concurrent callers all get their own answer" <| withMailbox (fun mailbox ->
            let counts =
                [ for i in 1 .. 50 ->
                    mailbox.parseCommandLine(commandLine = [| "--strict"; $"file{i}.ts" |]) ]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.map (fun response -> response.FileNames.Length)

            counts.Length |> Flip.Expect.equal "every caller replied" 50
            counts |> Array.forall ((=) 1) |> Flip.Expect.isTrue "each reply is that caller's own result")

        // The generated surface comes in three layers, and each has to reach the same server.
        testCase "the free function, the record overload and the flattened one agree" <| withMailbox (fun mailbox ->
            let viaFunction = AsyncApi.parseCommandLine mailbox commandLine |> Async.RunSynchronously
            let viaRecord = mailbox.parseCommandLine commandLine |> Async.RunSynchronously
            let viaFields = mailbox.parseCommandLine(commandLine = [| "--strict"; "main.ts" |]) |> Async.RunSynchronously

            viaRecord.FileNames |> Flip.Expect.equal "record overload" viaFunction.FileNames
            viaFields.FileNames |> Flip.Expect.equal "flattened overload" viaFunction.FileNames)

        // An optional argument left out has to end up absent from the payload, not sent as null -
        // the same distinction the generated records' JsonIgnore attributes exist for. `--strict`
        // alone yields no file names; a null commandLine is an error.
        testCase "an omitted optional argument is absent, not null" <| withMailbox (fun mailbox ->
            let response = mailbox.parseCommandLine() |> Async.RunSynchronously
            response.FileNames.Length |> Flip.Expect.equal "no command line, no file names" 0)

        // The transport returns the AST raw when a request travels alone, but a batch response is
        // JSON and carries it base64-encoded instead. The agent normalises the two, and this is
        // the assertion that keeps it honest.
        testCase "a batched AST decodes to the same tree as a solo one" <| withMailbox (fun mailbox ->
            let snapshot =
                mailbox.updateSnapshot(openProjects = [| file "tsconfig.json" |]) |> Async.RunSynchronously

            let project = snapshot.Projects[0].Id
            let nodes (ast: Ast.SourceFile voption) = ast |> ValueOption.map (fun ast -> ast.NodeCount)

            let solo =
                mailbox.getSourceFile(snapshot.Snapshot, project, file "main.ts")
                |> Async.RunSynchronously
                |> nodes

            solo |> ValueOption.isSome |> Flip.Expect.isTrue "an AST arrives on the single-request path"

            // Two in flight, so this pair really does go through batchRequests.
            [ mailbox.getSourceFile(snapshot.Snapshot, project, file "main.ts")
              mailbox.getSourceFile(snapshot.Snapshot, project, file "main.ts") ]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.iter (fun ast -> nodes ast |> Flip.Expect.equal "the batched AST matches the solo one" solo))

        // A batch response is marshalled by the server in one piece, so one result it cannot
        // encode refuses every request travelling with it. `infinity.ts` declares `1e999`, which
        // is `+Inf` to Go's JSON encoder (upstream, at the pinned compiler); the agent replays a
        // refused batch member by member, so only the requests for the two unencodable types
        // fail and their fellow travellers still get answers.
        testCase "a result the server cannot encode fails its own request, not its batch" <| withMailbox (fun mailbox ->
            let program =
                mailbox.createProgram(Proto.CreateProgramOptions.Default, rootFiles = [| file "infinity.ts" |])
                |> Async.RunSynchronously

            let session = mailbox.Session program

            let moduleSymbol =
                session.getSymbolOfSourceFile(file "infinity.ts")
                |> Async.RunSynchronously
                |> ValueOption.defaultWith (fun () -> failtest "infinity.ts is a module")

            let exports =
                session.getExportsOfModule moduleSymbol.Id
                |> Async.RunSynchronously
                |> ValueOption.defaultValue [||]

            exports.Length |> Flip.Expect.equal "the fixture's exports" 8

            // All in flight at once, so that the unencodable pair shares a batch with the rest.
            let outcomes =
                exports
                |> Array.map (fun export ->
                    async {
                        try
                            let! _ = session.getDeclaredTypeOfSymbol export.Id
                            return export.Name, None
                        with TsGoError(_, message) ->
                            return export.Name, Some message
                    })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Map.ofArray

            let refused = outcomes |> Map.filter (fun _ message -> message.IsSome) |> Map.keys |> List.ofSeq

            refused
            |> Flip.Expect.equal "exactly the non-finite literals are refused" [ "Infinite"; "NegativeInfinite" ]

            for message in outcomes |> Map.values |> Seq.choose id do
                Expect.stringContains message "Inf" "and the refusal names the encoder's complaint")

        testCase "disposal is idempotent, and the channel goes with it" <| withMailbox (fun mailbox ->
            mailbox.Dispose()
            // The fixture's `use` calls this a third time. The second call is the regression:
            // disposing the cancellation source twice used to throw ObjectDisposedException.
            mailbox.Dispose()

            // Timed rather than open-ended: a mailbox that failed to shut down would otherwise
            // hang the suite instead of failing it.
            Expect.throwsT<ObjectDisposedException>
                (fun () -> Async.RunSynchronously(mailbox.parseCommandLine commandLine, timeout = 2000) |> ignore)
                "nothing is served once the mailbox and its channel are closed")

        // The agent stops with messages still queued. Each of their callers has to hear back,
        // with an answer or with the disposal, rather than wait forever.
        testCase "requests queued at disposal are all answered" <| withMailbox (fun mailbox ->
            let calls =
                [| for i in 1 .. 50 ->
                    Async.StartAsTask(mailbox.parseCommandLine(commandLine = [| "--strict"; $"file{i}.ts" |])) |]

            mailbox.Dispose()

            let all = Tasks.Task.WhenAll(calls |> Array.map (fun call -> call :> Tasks.Task))
            all.ContinueWith(ignore).Wait 10_000 |> Flip.Expect.isTrue "every caller completes"

            for call in calls do
                if call.IsFaulted then
                    match call.Exception.InnerException with
                    | :? ObjectDisposedException -> ()
                    | :? IOException -> ()
                    | other -> failtest $"a queued request failed with {other}")
    ]

/// The batch dispatch rule, with no server: `one` and `many` stand in for the channel.
[<Tests>]
let batchTests =
    let entry method = ProtoJson.batchEntryNoParams method

    testList "mailbox batch" [
        // A whole-batch refusal arrives after the server has executed every member, so a replay
        // would run a mutating request twice.
        testCase "a refused batch replays read-only requests and never re-sends a mutating one" <| fun _ ->
            let sent = ResizeArray<string>()

            let one (request: Proto.BatchRequest) =
                sent.Add request.Method
                [| 1uy |]

            let many (_: Proto.BatchRequest[]) : Result<byte[], exn>[] =
                raise (TsGoError(Proto.Method.BatchRequests, "json: unsupported value: +Inf"))

            let results =
                Batch.dispatch one many [| entry Proto.Method.GetAnyType; entry Proto.Method.Release; entry Proto.Method.GetStringType |]

            List.ofSeq sent
            |> Flip.Expect.equal "only the read-only requests are replayed" [ Proto.Method.GetAnyType; Proto.Method.GetStringType ]

            match results with
            | [| Ok _; Error(TsGoError(method, _)); Ok _ |] ->
                method |> Flip.Expect.equal "the release carries the batch's own error" Proto.Method.BatchRequests
            | other -> failtest $"unexpected results %A{other}"

        testCase "a transport failure is every member's result, with nothing replayed" <| fun _ ->
            let sent = ResizeArray<string>()

            let one (request: Proto.BatchRequest) =
                sent.Add request.Method
                [||]

            let failure = IOException "closed"
            let many (_: Proto.BatchRequest[]) : Result<byte[], exn>[] = raise failure

            let results = Batch.dispatch one many [| entry Proto.Method.GetAnyType; entry Proto.Method.GetStringType |]

            sent.Count |> Flip.Expect.equal "nothing replayed" 0

            for result in results do
                match result with
                | Error e -> Expect.isTrue (obj.ReferenceEquals(e, failure)) "the channel's own error"
                | Ok _ -> failtest "a failed transport answered"
    ]
