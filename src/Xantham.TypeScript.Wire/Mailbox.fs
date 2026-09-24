namespace Xantham.TypeScript.Wire

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open Xantham.TypeScript.Wire.Proto

/// How <see cref="T:Xantham.TypeScript.Wire.TscMailbox"/> sends a group of queued requests.
[<RequireQualifiedAccess>]
module Batch =

    /// The methods that change server state: they create or free a snapshot or program, write
    /// files, or toggle profiling. A batch refused as a whole has already executed them, so they
    /// are reported as failed with the batch error and are never re-sent.
    let sideEffectingMethods =
        HashSet<string>
            [
                Method.Release
                Method.UpdateSnapshot
                Method.UpdateTemporarySnapshot
                Method.CreateProgram
                Method.Emit
                Method.StartCPUProfile
                Method.StopCPUProfile
                Method.SaveHeapProfile
            ]

    /// The result of each of `requests`, in order: `one` for a lone request, `many` for several.
    /// A whole-batch `TsGoError` replays the read-only members one by one through `one`; any
    /// other failure is every member's result.
    let internal dispatch
        (one: BatchRequest -> byte[])
        (many: BatchRequest[] -> Result<byte[], exn>[])
        (requests: BatchRequest[])
        : Result<byte[], exn>[] =
        try
            if requests.Length = 1 then
                [| Ok(one requests[0]) |]
            else
                many requests
        with
        | TsGoError _ as batchError when requests.Length > 1 ->
            // The server refused the batch as a whole, not one member of it: a batch response is
            // marshalled in one piece, so a single result that cannot be encoded (verified live:
            // a number literal type whose value is `1e999` is `+Inf` to Go's JSON encoder) fails
            // every request travelling with it. The channel survived - the refusal is an
            // ordinary error frame - so replay the read-only members one by one and let only the
            // guilty one fail.
            requests
            |> Array.map (fun request ->
                if sideEffectingMethods.Contains request.Method then
                    Error batchError
                else
                    try
                        Ok(one request)
                    with error ->
                        Error error)
        | error ->
            // The channel failed, and keeps failing every later request with the same error, so
            // nobody in this group gets an answer.
            Array.create requests.Length (Error error)

/// <summary>
/// <para>Asynchronous wrapper for the serial channel. Avoids the async-api overhead of the tsc encoding.</para>
/// <para>If there is a backlog of requests after the first, then the mailbox will batch them together.</para>
/// </summary>
/// <remarks>
/// <para>Lone callers never batch, and costs nothing extra. The synchronous api is generated to avoid unwrapping
/// async replies in small PoCs.</para>
/// <para>The mailbox, under pressure, outperforms serial usage by a factor of 2.1-2.3 times.</para>
/// </remarks>
type TscMailbox(exePath, cwd, ?callbacks: IDictionary<string, TsGoCallback>) =
    let channel = new TscChannel(exePath, cwd, ?callbacks = callbacks)

    let cancellation = new CancellationTokenSource()
    let mutable disposed = 0

    // Every reply not yet completed. Dispose fails them, since the agent stops with them queued.
    let pending = ConcurrentDictionary<TaskCompletionSource<Result<byte[], exn>>, unit>()

    let disposedError () = ObjectDisposedException(nameof TscMailbox) :> exn

    // Replies carry the failure rather than raising inside the agent
    let agent =
        MailboxProcessor<BatchRequest * TaskCompletionSource<Result<byte[], exn>>>
            .Start(
                (fun inbox ->
                    // Everything already queued goes out together. The count is read after the first
                    // Receive, so it reflects what accumulated while the previous batch was in flight.
                    let rec drain (batch: ResizeArray<_>) =
                        async {
                            if inbox.CurrentQueueLength = 0 then
                                return batch
                            else
                                let! message = inbox.Receive()
                                batch.Add message
                                return! drain batch
                        }

                    let one (request: BatchRequest) =
                        // Not `ProtoJson.request<byte[], _>`: the params are already UTF-8 JSON, and
                        // serialising a byte[] would base64 it into a string.
                        if isNull request.Params then
                            channel.Request(request.Method, "null")
                        else
                            channel.Request(request.Method, request.Params)

                    let many (requests: BatchRequest[]) =
                        let response =
                            ProtoJson.request<BatchRequestsParams, BatchRequestsResponse>
                                channel
                                Method.BatchRequests
                                { Requests = ValueSome requests }

                        if response.Responses.Length <> requests.Length then
                            failwithf
                                $"batchRequests answered %d{response.Responses.Length} of %d{requests.Length} requests"

                        response.Responses
                        |> Array.map (fun response ->
                            match response.Error with
                            | ValueSome error -> Error(TsGoError(response.Method, error))
                            | ValueNone when isNull response.Result -> Ok [||]
                            // The AST methods return their blob raw when sent alone, but a batch
                            // response is JSON and cannot carry bytes, so here the same result is a
                            // base64 string.
                            | ValueNone when Method.binaryResultMethods.Contains response.Method ->
                                Ok(Convert.FromBase64String(ProtoJson.deserialize<string> response.Result))
                            // caller's payload as raw UTF-8 JSON
                            | ValueNone -> Ok response.Result)

                    let rec loop () =
                        async {
                            let! first = inbox.Receive()
                            let! batch = drain (ResizeArray [ first ])
                            let requests = batch |> Seq.map fst |> Array.ofSeq
                            let results = Batch.dispatch one many requests

                            batch
                            |> Seq.iteri (fun i (_, reply: TaskCompletionSource<_>) ->
                                reply.TrySetResult results[i] |> ignore
                                pending.TryRemove reply |> ignore)

                            return! loop ()
                        }

                    loop ()),
                cancellationToken = cancellation.Token
            )

    let send entry =
        async {
            if Volatile.Read &disposed = 1 then
                raise (disposedError ())

            let reply =
                TaskCompletionSource<Result<byte[], exn>>(TaskCreationOptions.RunContinuationsAsynchronously)

            pending[reply] <- ()
            // A Dispose between the check above and the registration has already swept `pending`.
            if Volatile.Read &disposed = 1 then
                reply.TrySetResult(Error(disposedError ())) |> ignore
            else
                agent.Post(entry, reply)

            match! Async.AwaitTask reply.Task with
            | Ok bytes -> return bytes
            // Rethrow where it was raised, so the caller sees the channel's stack and not this one.
            | Error error ->
                ExceptionDispatchInfo.Capture(error).Throw()
                return Unchecked.defaultof<byte[]>
        }

    /// A request whose result the schema says is always present.
    member _.Request<'Params, 'Result>(method: string, parameters: 'Params) : Async<'Result> =
        async {
            let! response = send (ProtoJson.batchEntry method parameters)

            if ProtoJson.isAbsent response then
                return failwithf "%s returned no result, but the schema declares one" method
            else
                return ProtoJson.deserialize<'Result> response
        }

    /// A request whose result the schema permits to be null.
    member _.RequestOption<'Params, 'Result>(method: string, parameters: 'Params) : Async<'Result voption> =
        async {
            let! response = send (ProtoJson.batchEntry method parameters)

            return
                if ProtoJson.isAbsent response then
                    ValueNone
                else
                    ValueSome(ProtoJson.deserialize<'Result> response)
        }

    /// A request that returns nothing. The reply is still awaited: the point of the round trip is
    /// to know the server got there, and a failure has to surface somewhere.
    member _.RequestUnit<'Params>(method: string, parameters: 'Params) : Async<unit> =
        async {
            let! _ = send (ProtoJson.batchEntry method parameters)
            return ()
        }

    /// A parameterless request whose result the schema says is always present.
    member _.RequestNoParams<'Result>(method: string) : Async<'Result> =
        async {
            let! response = send (ProtoJson.batchEntryNoParams method)

            if ProtoJson.isAbsent response then
                return failwithf "%s returned no result, but the schema declares one" method
            else
                return ProtoJson.deserialize<'Result> response
        }

    /// A parameterless request whose result the schema permits to be null.
    member _.RequestNoParamsOption<'Result>(method: string) : Async<'Result voption> =
        async {
            let! response = send (ProtoJson.batchEntryNoParams method)

            return
                if ProtoJson.isAbsent response then
                    ValueNone
                else
                    ValueSome(ProtoJson.deserialize<'Result> response)
        }

    /// A parameterless request that returns nothing.
    member _.RequestUnitNoParams(method: string) : Async<unit> =
        async {
            let! _ = send (ProtoJson.batchEntryNoParams method)
            return ()
        }

    /// One of the four AST-returning methods.
    ///
    /// The blob is decoded here rather than handed back as bytes, matching `ProtoJson.requestAst`.
    /// The agent has already normalised the two encodings the transport uses for it, so this sees
    /// raw bytes regardless of whether the request travelled alone or in a batch.
    member _.RequestAst<'Params>(method: string, parameters: 'Params) : Async<Ast.SourceFile voption> =
        async {
            match! send (ProtoJson.batchEntry method parameters) with
            | [||] -> return ValueNone
            | bytes -> return ValueSome(Ast.read bytes)
        }

    /// Stops the agent and closes the channel, which is owned here. Requests still queued fail
    /// with `ObjectDisposedException`, as does every later request. Idempotent.
    member _.Dispose() =
        if Interlocked.Exchange(&disposed, 1) = 0 then
            for reply in pending.Keys do
                reply.TrySetResult(Error(disposedError ())) |> ignore

            pending.Clear()
            cancellation.Cancel()
            (agent :> IDisposable).Dispose()
            cancellation.Dispose()
            (channel :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
