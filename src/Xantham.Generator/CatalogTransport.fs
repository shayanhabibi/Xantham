module internal Xantham.Generator.CatalogTransport

open System
open System.Buffers
open System.IO
open System.IO.Compression
open System.Text
open System.Text.Json

[<Literal>]
let MaxJsonBytes = 134217728L

[<AbstractClass>]
type ReadOnlyStream() =
    inherit Stream()
    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())

/// Decodes exactly one complete Brotli stream and owns its input.
type StrictBrotliStream(input: Stream) =
    inherit ReadOnlyStream()
    let buffer = Array.zeroCreate<byte> 4096
    let mutable decoder = new BrotliDecoder()
    let mutable offset = 0
    let mutable available = 0
    let mutable finished = false
    let mutable disposed = false

    let refill () =
        if available > 0 then
            Array.Copy(buffer, offset, buffer, 0, available)

        offset <- 0
        let count = input.Read(buffer, available, buffer.Length - available)

        if count = 0 then
            raise (InvalidDataException "truncated Brotli stream")

        available <- available + count

    override _.Read(output, start, count) =
        ObjectDisposedException.ThrowIf(disposed, input)

        if isNull output then
            nullArg "output"

        if start < 0 || count < 0 || start > output.Length - count then
            invalidArg "count" "Invalid buffer range."

        if count = 0 || finished then
            0
        else
            let mutable written = 0

            while written = 0 && not finished do
                let mutable consumed = 0
                let mutable produced = 0

                let status =
                    decoder.Decompress(
                        ReadOnlySpan<byte>(buffer, offset, available),
                        Span<byte>(output, start, count),
                        &consumed,
                        &produced
                    )

                offset <- offset + consumed
                available <- available - consumed
                written <- produced

                match status with
                | OperationStatus.Done ->
                    if available <> 0 || input.ReadByte() <> -1 then
                        raise (InvalidDataException "trailing bytes after Brotli stream")

                    finished <- true
                | OperationStatus.NeedMoreData -> refill ()
                | OperationStatus.DestinationTooSmall ->
                    if produced = 0 && consumed = 0 then
                        raise (InvalidDataException "Brotli decoder made no progress")
                | _ -> raise (InvalidDataException "invalid Brotli stream")

            written

    override _.Dispose(disposing) =
        if disposing && not disposed then
            disposed <- true
            decoder.Dispose()
            input.Dispose()

        base.Dispose(disposing)

/// Counts decoded bytes and probes one byte beyond the inclusive limit.
type private BoundedStream(input: Stream, limit: int64) =
    inherit ReadOnlyStream()
    let mutable total = 0L

    override _.Read(buffer, offset, count) =
        if count = 0 then
            0
        else
            let request = int (min (int64 count) (limit - total + 1L))
            let read = input.Read(buffer, offset, request)
            total <- total + int64 read

            if total > limit then
                raise (InvalidDataException $"decoded JSON exceeds byte limit {limit}")

            read

    override _.Dispose(disposing) =
        if disposing then
            input.Dispose()

        base.Dispose(disposing)

let readJsonWithLimit (limit: int64) (path: string) =
    if limit < 0L || limit = Int64.MaxValue then
        invalidArg "limit" "Expected a nonnegative finite byte limit."

    try
        use file = File.OpenRead path

        use decoded =
            if path.EndsWith(".br", StringComparison.OrdinalIgnoreCase) then
                new StrictBrotliStream(file) :> Stream
            else
                file :> Stream

        use bounded = new BoundedStream(decoded, limit)
        JsonDocument.Parse(bounded)
    with
    | :? InvalidDataException as error ->
        raise (InvalidDataException($"Declaration catalogue '{path}': {error.Message}", error))
    | :? IOException as error -> raise (InvalidDataException($"Declaration catalogue '{path}': {error.Message}", error))
    | :? UnauthorizedAccessException as error ->
        raise (InvalidDataException($"Declaration catalogue '{path}': {error.Message}", error))
    | :? JsonException as error ->
        raise (InvalidDataException($"Declaration catalogue '{path}': invalid JSON: {error.Message}", error))

let readJson path = readJsonWithLimit MaxJsonBytes path

let writeBrotli (path: string) (content: string) =
    use file = File.Create path
    use compressed = new BrotliStream(file, CompressionLevel.SmallestSize)
    use writer = new StreamWriter(compressed, UTF8Encoding(false))
    writer.Write content
