/// The msgpack frame codec against in-memory streams, with no server involved.
module Xantham.TypeScript.Wire.Tests.Frame

open System
open System.IO
open Expecto
open Xantham.TypeScript.Wire

/// A read-only view of `inner` that returns at most `chunk` bytes per `Read`, as a pipe does when
/// a frame arrives in pieces.
type private ShortReads(inner: Stream, chunk: int) =
    inherit Stream()
    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Read(buffer: byte[], offset: int, count: int) = inner.Read(buffer, offset, min count chunk)
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())

let private frame (method: string) (payload: byte[]) =
    use buffer = new MemoryStream()
    Msgpack.writeFrame buffer MessageType.Response (ReadOnlySpan(Text.Encoding.UTF8.GetBytes method)) (ReadOnlySpan payload)
    buffer.ToArray()

[<Tests>]
let frameTests =
    testList "frame" [
        // Sizes chosen to cross each bin header width: bin8, bin16 and bin32.
        testTheory "a frame delivered in short reads decodes whole" [ 1; 7; 200; 70_000 ] <| fun size ->
            let payload = Array.init size byte
            let bytes = frame "getSourceFile" payload

            for chunk in [ 1; 3; 4096 ] do
                use stream = new ShortReads(new MemoryStream(bytes), chunk)
                let messageType, method, decoded = Msgpack.readFrame stream

                messageType |> Flip.Expect.equal $"type, chunk {chunk}" MessageType.Response
                Text.Encoding.UTF8.GetString method |> Flip.Expect.equal $"method, chunk {chunk}" "getSourceFile"
                decoded |> Flip.Expect.equal $"payload, chunk {chunk}" payload

        testCase "a frame cut off inside its payload reports the closed pipe" <| fun _ ->
            let bytes = frame "getSourceFile" (Array.zeroCreate 300)
            use stream = new ShortReads(new MemoryStream(bytes[.. bytes.Length - 10]), 64)

            Expect.throwsC
                (fun () -> Msgpack.readFrame stream |> ignore)
                (fun e -> e.Message |> Flip.Expect.stringContains "the reader's message" "closed the pipe mid-frame")
    ]
