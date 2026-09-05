namespace JetBrains.Annotations

open System

type InjectedLanguage =
    | CSS = 0
    | HTML = 1
    | JAVASCRIPT = 2
    | JSON = 3
    | XML = 4

[<AttributeUsage(AttributeTargets.Parameter
                 ||| AttributeTargets.Field
                 ||| AttributeTargets.Property)>]
type LanguageInjectionAttribute private (?injectedLanguage: InjectedLanguage, ?injectedLanguageName: string) =
    inherit Attribute()
    member x.InjectedLanguage = injectedLanguage
    member x.InjectedLanguageName = injectedLanguageName
    member val Prefix = "" with get, set
    member val Suffix = "" with get, set
    new(injectedLanguage: InjectedLanguage) = LanguageInjectionAttribute(injectedLanguage = injectedLanguage)
    new(injectedLanguageName: string) = LanguageInjectionAttribute(injectedLanguageName = injectedLanguageName)

namespace Xantham.TypeScript.Wire

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Text
open System.Text.Json.Nodes
open System.Threading.Tasks
open JetBrains.Annotations
open Xantham.TypeScript.Wire.Measures

/// Frame message types. The method-name slot doubles as the correlation key - there are no
/// request ids on the wire, and the server is strictly serial.
type internal MessageType =
    | Request = 1uy
    | CallResponse = 2uy
    | CallError = 3uy
    | Response = 4uy
    | Error = 5uy
    | Call = 6uy

/// A method call failed server-side. a bare message with no code, so there is
/// nothing structured to match on.
exception TsGoError of method: string * message: string

/// A filesystem callback the server may invoke on us (`--callbacks=...`). The argument arrives
/// **JSON-encoded** - a path is `"C:/..."` with the quotes - so it must be parsed, not used raw.
/// The result is JSON too, and `null` is not the same as absent: an explicit null changes module
/// resolution semantics.
type TsGoCallback = string -> string

/// The entries of one directory, as `getAccessibleEntries` answers them: names, not paths.
type FileSystemEntries =
    {
        Files: string[]
        Directories: string[]
    }

/// What `readFile` found. The server distinguishes all three, and the distinction is the reason
/// the reply is an object rather than a bare string: `Missing` stops resolution at this path,
/// where `FallBack` sends it on to the real filesystem. Getting the two the wrong way round
/// silently changes module resolution.
type FileRead =
    /// The file's content, `Content ""` included - an empty file is not a missing one.
    | Content of content: string
    /// The file does not exist, and the real filesystem is not consulted.
    | Missing
    /// Not answered here; the server reads the real filesystem.
    | FallBack

/// <summary>
/// The virtual filesystem the server delegates to when the process is started with
/// <c>--callbacks=</c>, mirroring <c>dist/api/fs.d.ts</c>. Every member is optional, and one left
/// unset is not registered at all, so the server never asks about it.
/// </summary>
/// <remarks>
/// <para>This exists because the raw <see cref="T:Xantham.TypeScript.Wire.TsGoCallback"/> surface is
/// unforgiving: each callback has its own reply shape, the shapes appear nowhere in the schema, and
/// a wrong one is not an error but a Go panic that kills the process mid-request. The encoding is
/// applied here once, transcribed from <c>dist/api/sync/client.js:35-56</c>.</para>
/// <para><c>ValueNone</c> from a member means "not answered" - the empty reply the server reads as
/// "fall back to the real filesystem" - which is why <c>ReadFile</c> has its own three-way type
/// rather than returning an option.</para>
/// </remarks>
type VirtualFileSystem =
    {
        DirectoryExists: (string -> bool voption) voption
        FileExists: (string -> bool voption) voption
        GetAccessibleEntries: (string -> FileSystemEntries voption) voption
        ReadFile: (string -> FileRead) voption
        Realpath: (string -> string voption) voption
        /// Takes the path and then the content to write. The server expects no answer.
        WriteFile: (string -> string -> unit) voption
    }

    /// A filesystem that answers nothing, so every path falls back to the real one. Copy-update
    /// it with the members you mean to serve.
    static member Default =
        {
            DirectoryExists = ValueNone
            FileExists = ValueNone
            GetAccessibleEntries = ValueNone
            ReadFile = ValueNone
            Realpath = ValueNone
            WriteFile = ValueNone
        }

[<RequireQualifiedAccess>]
module VirtualFileSystem =
    /// The argument always arrives as a JSON string - `"C:/..."`, quotes included.
    let private path (argument: string) =
        Json.JsonSerializer.Deserialize<string> argument

    let private json (value: 'T) = Json.JsonSerializer.Serialize<'T> value

    /// An empty reply is how "not answered" reaches the server; anything else is JSON.
    let private orFallBack (encode: 'T -> string) (value: 'T voption) =
        match value with
        | ValueSome value -> encode value
        | ValueNone -> ""

    let private ofBool exists = if exists then "true" else "false"

    /// <summary>
    /// The callback table to hand to <c>TscChannel</c> or <c>TscMailbox</c>. Only the members that
    /// are set appear in it, and only those names reach <c>--callbacks=</c>.
    /// </summary>
    let callbacks (fs: VirtualFileSystem) : IDictionary<string, TsGoCallback> =
        let table = Dictionary<string, TsGoCallback>()
        let add name (callback: TsGoCallback) = table[name] <- callback

        fs.ReadFile
        |> ValueOption.iter (fun readFile ->
            // The one callback whose reply is an object: it has to carry the difference between a
            // content of `null` and no answer at all, which a bare string cannot.
            add "readFile" (fun argument ->
                match readFile (path argument) with
                | Content content -> "{\"content\":" + json content + "}"
                | Missing -> "{\"content\":null}"
                | FallBack -> ""))

        fs.FileExists
        |> ValueOption.iter (fun fileExists -> add "fileExists" (path >> fileExists >> orFallBack ofBool))

        fs.DirectoryExists
        |> ValueOption.iter (fun directoryExists ->
            add "directoryExists" (path >> directoryExists >> orFallBack ofBool))

        fs.Realpath
        |> ValueOption.iter (fun realpath -> add "realpath" (path >> realpath >> orFallBack json))

        fs.GetAccessibleEntries
        |> ValueOption.iter (fun getEntries ->
            add
                "getAccessibleEntries"
                (path
                 >> getEntries
                 >> orFallBack (fun entries ->
                     "{\"files\":"
                     + json entries.Files
                     + ",\"directories\":"
                     + json entries.Directories
                     + "}")))

        fs.WriteFile
        |> ValueOption.iter (fun writeFile ->
            // The only callback whose argument is an object rather than a path, and the only one
            // the server expects nothing back from.
            add "writeFile" (fun argument ->
                use document = Json.JsonDocument.Parse argument

                let field name =
                    document.RootElement.GetProperty(name: string).GetString()

                writeFile (field "path") (field "data")
                ""))

        table

/// The AST string table is WTF-8, not UTF-8: an unpaired UTF-16 surrogate is encoded as the
/// three bytes `ED A0-BF 80-BF`, which strict UTF-8 rejects. `Encoding.UTF8.GetString` replaces
/// those with U+FFFD, so identifiers and string literals carrying a lone surrogate come back
/// silently corrupted. TypeScript permits them, so we decode them ourselves.
[<RequireQualifiedAccess>]
module Wtf8 =
    /// True when `bytes.[i]` starts a WTF-8 surrogate sequence.
    let inline private isSurrogateStart (bytes: ReadOnlySpan<byte>) (i: int) =
        bytes[i] = 0xEDuy
        && i + 2 < bytes.Length
        && bytes[i + 1] >= 0xA0uy
        && bytes[i + 1] <= 0xBFuy

    let decode (bytes: ReadOnlySpan<byte>) : string =
        // Fast path: ordinary UTF-8.
        if bytes.IndexOf 0xEDuy < 0 then
            Encoding.UTF8.GetString bytes
        else

            let sb = StringBuilder(bytes.Length)
            let mutable runStart = 0
            let mutable i = 0

            while i < bytes.Length do
                if isSurrogateStart bytes i then
                    if i > runStart then
                        sb.Append(Encoding.UTF8.GetString(bytes.Slice(runStart, i - runStart)))
                        |> ignore

                    let b1 = int bytes[i + 1]
                    let b2 = int bytes[i + 2]
                    sb.Append(char (0xD000 ||| ((b1 &&& 0x3F) <<< 6) ||| (b2 &&& 0x3F))) |> ignore
                    i <- i + 3
                    runStart <- i
                else
                    i <- i + 1

            if runStart < bytes.Length then
                sb.Append(Encoding.UTF8.GetString(bytes.Slice runStart)) |> ignore

            sb.ToString()

    let decodeArray (bytes: byte[]) = decode (ReadOnlySpan bytes)


/// <summary>
/// Sync channel envelope - msgpack 3-tuple of <c>[type; method; payload]</c>.
/// </summary>
/// <remarks>
/// Length prefixes here are **big-endian**. The binary AST inside the payload is
/// **little-endian**
/// </remarks>
[<RequireQualifiedAccess>]
module internal Msgpack =

    [<Literal>]
    let private Fixarray3 = 0x93uy

    [<Literal>]
    let private Bin8 = 0xC4uy

    [<Literal>]
    let private Bin16 = 0xC5uy

    [<Literal>]
    let private Bin32 = 0xC6uy

    [<Literal>]
    let private Uint8 = 0xCCuy

    let private writeBin (stream: Stream) (value: ReadOnlySpan<byte>) =
        let len = value.Length

        if len < 0x100 then
            stream.WriteByte Bin8
            stream.WriteByte(byte len)
        elif len < 0x10000 then
            let header = [| Bin16; 0uy; 0uy |]
            BinaryPrimitives.WriteUInt16BigEndian(Span(header, 1, 2), uint16 len)
            stream.Write(ReadOnlySpan header)
        else
            let header = Array.zeroCreate<byte> 5
            header[0] <- Bin32
            BinaryPrimitives.WriteUInt32BigEndian(Span(header, 1, 4), uint32 len)
            stream.Write(ReadOnlySpan header)

        stream.Write value

    /// <summary>
    /// Writes a frame, and flushes the stream.
    /// </summary>
    let writeFrame
        (stream: Stream)
        (messageType: MessageType)
        (method: ReadOnlySpan<byte>)
        (payload: ReadOnlySpan<byte>)
        =
        stream.WriteByte Fixarray3
        // A type below 0x80 is a positive fixint; the wider form is legal but nothing emits it.
        let t = byte messageType

        if t < 0x80uy then
            stream.WriteByte t
        else
            stream.WriteByte Uint8
            stream.WriteByte t

        writeBin stream method
        writeBin stream payload
        stream.Flush()

    let private readByte (stream: Stream) =
        match stream.ReadByte() with
        | -1 -> failwith "tsgo closed the pipe mid-frame"
        | b -> byte b

    let private readExactly (stream: Stream) (count: int) =
        let buffer = Array.zeroCreate<byte> count
#if !NETSTANDARD2_1
        stream.ReadExactly(buffer, 0, count)
#else
        stream.Read(buffer, 0, count) |> ignore
#endif
        buffer

    let private readBin (stream: Stream) =
        let len =
            match readByte stream with
            | Bin8 -> int (readByte stream)
            | Bin16 -> int (BinaryPrimitives.ReadUInt16BigEndian(ReadOnlySpan(readExactly stream 2)))
            | Bin32 -> int (BinaryPrimitives.ReadUInt32BigEndian(ReadOnlySpan(readExactly stream 4)))
            | other -> failwithf "expected a msgpack bin header, got 0x%02X" other

        readExactly stream len


    /// <summary>
    /// Reads one frame. Blocks until a whole frame has arrived.
    /// </summary>
    let readFrame (stream: Stream) : MessageType * byte[] * byte[] =
        match readByte stream with
        | Fixarray3 -> ()
        | other -> failwithf "expected a msgpack 3-tuple header (0x93), got 0x%02X" other

        let messageType =
            match readByte stream with
            | Uint8 -> readByte stream
            | fixint when fixint < 0x80uy -> fixint
            | other -> failwithf "expected a msgpack message type, got 0x%02X" other

        let method = readBin stream
        let payload = readBin stream
        LanguagePrimitives.EnumOfValue<byte, MessageType> messageType, method, payload

    // The other msgpack in this protocol: the structured-data section of a binary AST blob is a
    // run of self-contained values, each addressed by a byte offset held in a node's
    // extended-data record. It is a buffer rather than a stream, and it is read rather than
    // written.
    //
    // The compiler's writer emits eleven tags and no others (`encoder.go:849-891`), so an
    // unexpected one means the format moved underneath us. That is worth a failure, not a skip.

    [<Literal>]
    let private FixarrayLow = 0x90uy

    [<Literal>]
    let private FixarrayHigh = 0x9Fuy

    [<Literal>]
    let private Array16 = 0xDCuy

    [<Literal>]
    let private Array32 = 0xDDuy

    [<Literal>]
    let private FixstrLow = 0xA0uy

    [<Literal>]
    let private FixstrHigh = 0xBFuy

    [<Literal>]
    let private Str8 = 0xD9uy

    [<Literal>]
    let private Str16 = 0xDAuy

    [<Literal>]
    let private Str32 = 0xDBuy

    [<Literal>]
    let private Uint16 = 0xCDuy

    [<Literal>]
    let private Uint32 = 0xCEuy

    [<Literal>]
    let private False = 0xC2uy

    [<Literal>]
    let private True = 0xC3uy

    /// <summary>
    /// A cursor over one msgpack value in a buffer.
    /// </summary>
    /// <remarks>
    /// Every read advances the cursor, so a caller reads an array header and then exactly that
    /// many elements, in the order the writer put them.
    /// </remarks>
    type internal Reader(data: byte[], start: int<structuredOffset>) =
        let mutable position = int start

        let take (count: int) =
            if position + count > data.Length then
                failwithf $"structured data ended mid-value at byte %d{position}"

            let span = ReadOnlySpan(data, position, count)
            position <- position + count
            span

        let tag () = (take 1)[0]

        /// The element count of the array that starts here.
        member _.ReadArrayLength() =
            match tag () with
            | fixarray when fixarray >= FixarrayLow && fixarray <= FixarrayHigh -> int (fixarray &&& 0x0Fuy)
            | Array16 -> int (BinaryPrimitives.ReadUInt16BigEndian(take 2))
            | Array32 -> int (BinaryPrimitives.ReadUInt32BigEndian(take 4))
            | other -> failwithf $"expected a msgpack array header, got 0x%02X{other}"

        member _.ReadUInt32() =
            match tag () with
            | fixint when fixint < 0x80uy -> uint32 fixint
            | Uint8 -> uint32 ((take 1)[0])
            | Uint16 -> uint32 (BinaryPrimitives.ReadUInt16BigEndian(take 2))
            | Uint32 -> BinaryPrimitives.ReadUInt32BigEndian(take 4)
            | other -> failwithf $"expected a msgpack unsigned integer, got 0x%02X{other}"

        member _.ReadString() =
            let length =
                match tag () with
                | fixstr when fixstr >= FixstrLow && fixstr <= FixstrHigh -> int (fixstr &&& 0x1Fuy)
                | Str8 -> int ((take 1)[0])
                | Str16 -> int (BinaryPrimitives.ReadUInt16BigEndian(take 2))
                | Str32 -> int (BinaryPrimitives.ReadUInt32BigEndian(take 4))
                | other -> failwithf $"expected a msgpack string, got 0x%02X{other}"

            // Go strings, so WTF-8 for the same reason the string table is.
            Wtf8.decode (take length)

        member _.ReadBool() =
            match tag () with
            | True -> true
            | False -> false
            | other -> failwithf $"expected a msgpack boolean, got 0x%02X{other}"

        /// Reads `count` elements with `read`, in order.
        member this.ReadArray(count: int, read: Reader -> 'T) = Array.init count (fun _ -> read this)

[<RequireQualifiedAccess>]
module Tsc =
    /// <summary>
    /// Locates the native compiler by walking up from <paramref name="searchRoot"/> looking for an npm install.
    /// </summary>
    /// <param name="searchRoot">The directory to start searching from.</param>
    let locate (searchRoot: string) =
        let rid =
            let platform =
#if !NETSTANDARD2_1
                if OperatingSystem.IsWindows() then "win32"
                elif OperatingSystem.IsMacOS() then "darwin"
                elif OperatingSystem.IsFreeBSD() then "freebsd"
                else "linux"
#else
                if
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.Windows
                    )
                then
                    "win32"
                elif
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.OSX
                    )
                then
                    "darwin"
                elif
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.Linux
                    )
                then
                    "linux"
                else
                    "freebsd"
#endif


            let arch =
                match Runtime.InteropServices.RuntimeInformation.OSArchitecture with
                | Runtime.InteropServices.Architecture.Arm64 -> "arm64"
                | Runtime.InteropServices.Architecture.Arm -> "arm"
                | _ -> "x64"

            $"{platform}-{arch}"

#if !NETSTANDARD2_1
        let extension = if OperatingSystem.IsWindows() then ".exe" else ""
#else
        let extension =
            if
                System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                    System.Runtime.InteropServices.OSPlatform.Windows
                )
            then
                ".exe"
            else
                ""
#endif

        // Platform package and executable stem, most current layout first.
        let layouts = [ $"typescript-{rid}", "tsc"; $"native-preview-{rid}", "tsgo" ]

        let rec walk (dir: DirectoryInfo) =
            if isNull (box dir) then
                None
            else
                let candidate =
                    layouts
                    |> List.tryPick (fun (package, stem) ->
                        let path =
                            Path.Combine(dir.FullName, "node_modules", "@typescript", package, "lib", stem + extension)

                        if File.Exists path then Some path else None)

                match candidate with
                | Some _ -> candidate
                | None -> walk dir.Parent

        match Environment.GetEnvironmentVariable "XANTHAM_TSGO_EXE" with
        | path when not (String.IsNullOrWhiteSpace path) && File.Exists path -> Some path
        | _ -> walk (DirectoryInfo searchRoot)


// A live `[tsc|tsgo] --api` process speaking the sync MessagePack protocol over redirected stdio.
/// <summary>
/// <b>SYNC</b> TypeScript compiler speaking the sync MessagePack protocol over redirected stdio.
/// </summary>
type TscChannel(exePath: string, cwd: string, ?callbacks: IDictionary<string, TsGoCallback>) =
    let callbacks = defaultArg callbacks (dict [])

    let startInfo =
        let args = ResizeArray [ "--api"; "--cwd"; cwd ]

        if callbacks.Count > 0 then
            args.Add $"""--callbacks={String.Join(",", callbacks.Keys)}"""

        let psi =
            ProcessStartInfo(
                FileName = exePath,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = cwd
            )

        for arg in args do
            psi.ArgumentList.Add arg

        psi

    let proc =
        match Process.Start startInfo with
        | null -> failwith $"could not start {exePath}"
        | p -> p

    // stderr carries panics and log noise and must be drained
    // to prevent the proc hanging
    let stderr = StringBuilder()

    let drain =
        Task.Run(fun () ->
            let mutable line = proc.StandardError.ReadLine()

            while not (isNull line) do
                lock stderr (fun () -> stderr.AppendLine line |> ignore)
                line <- proc.StandardError.ReadLine())

    // CARE - the protocol is raw bytes and any text translation corrupts it.
    let input = proc.StandardInput.BaseStream
    let output = proc.StandardOutput.BaseStream

    member _.Diagnostics = lock stderr (fun () -> stderr.ToString())

    /// <param name="method">The protocol method name.</param>
    /// <param name="payload">payload</param>
    /// <returns>Response in bytes</returns>
    member _.Request(method: string, payload: byte[]) : byte[] =
        let methodBytes = Encoding.UTF8.GetBytes method
        Msgpack.writeFrame input MessageType.Request (ReadOnlySpan methodBytes) (ReadOnlySpan payload)

        let mutable result = ValueNone

        while result.IsNone do
            let messageType, responseMethod, responsePayload = Msgpack.readFrame output

            match messageType with
            | MessageType.Response -> result <- ValueSome responsePayload
            | MessageType.Error -> raise (TsGoError(method, Encoding.UTF8.GetString responsePayload))
            | MessageType.Call ->
                // A callback must be answered inline, tagged with the same method name, before
                // the server will continue toward our response.
                let name = Encoding.UTF8.GetString responseMethod

                match callbacks.TryGetValue name with
                | true, callback ->
                    let reply =
                        try
                            let value = callback (Encoding.UTF8.GetString responsePayload)
                            Ok(Encoding.UTF8.GetBytes value)
                        with e ->
                            Error(Encoding.UTF8.GetBytes e.Message)

                    match reply with
                    | Ok bytes ->
                        Msgpack.writeFrame
                            input
                            MessageType.CallResponse
                            (ReadOnlySpan responseMethod)
                            (ReadOnlySpan bytes)
                    | Error bytes ->
                        Msgpack.writeFrame
                            input
                            MessageType.CallError
                            (ReadOnlySpan responseMethod)
                            (ReadOnlySpan bytes)
                | _ ->
                    let message = Encoding.UTF8.GetBytes $"no callback registered for {name}"
                    Msgpack.writeFrame input MessageType.CallError (ReadOnlySpan responseMethod) (ReadOnlySpan message)
            | other -> failwithf $"unexpected frame type %A{other} from tsgo"

        result.Value

    /// <param name="method">The protocol method name.</param>
    /// <param name="json">UTF-8 JSON payload</param>
    /// <returns>Response in bytes</returns>
    member this.Request(method: string, [<LanguageInjection(InjectedLanguage.JSON)>] json: string) =
        this.Request(method, Encoding.UTF8.GetBytes json)

    /// <param name="method">The protocol method name.</param>
    /// <param name="json">JSON payload</param>
    /// <returns>Response in bytes</returns>
    member inline this.Request(method, json: JsonObject) =
        this.Request(method, json.ToJsonString())

    /// <param name="method">The protocol method name.</param>
    /// <param name="json">JSON payload</param>
    /// <returns>Response in UTF-8 JSON</returns>
    member this.RequestJson(method: string, [<LanguageInjection(InjectedLanguage.JSON)>] json: string) =
        match this.Request(method, json) with
        | [||] -> ValueNone
        | bytes -> ValueSome(Encoding.UTF8.GetString bytes)

    /// <param name="method">The protocol method name.</param>
    /// <param name="json">JSON payload</param>
    /// <returns>Response in UTF-8 JSON</returns>
    member inline this.RequestJson(method, json: JsonObject) =
        this.RequestJson(method, json.ToJsonString())

    /// Must be called once before anything else
    member this.Initialize() =
        this.Request("initialize", "null") |> ignore

    interface IDisposable with
        member _.Dispose() =
            try
                // Closing stdin is the documented shutdown; there is no protocol-level method.
                input.Close()

                if not (proc.WaitForExit 2000) then
#if !NETSTANDARD2_1
                    proc.Kill true
#else
                    proc.Kill()
#endif
            with _ ->
                ()

            drain.Wait 1000 |> ignore
            proc.Dispose()

/// The version-5 binary AST returned by `getSourceFile`: one blob per file, from which every
/// node is readable with no further round-trips. All integers here are **little-endian**, unlike
/// the msgpack envelope that carried them.
[<RequireQualifiedAccess>]
module Ast =


    [<Literal>]
    let ProtocolVersion = 8u

    [<Literal>]
    let private HeaderSize = 44

    [<Literal>]
    let private NodeLen = 28

    /// A synthetic node representing a list of siblings rather than a syntax node. Outside the
    /// schema's numbering, hence not a `SyntaxKind` case.
    let KindNodeList: SyntaxKind = LanguagePrimitives.EnumOfValue 0xFFFFFFFFu

    [<Literal>]
    let private DataTypeMask = 0xC0000000u<mask>

    [<Literal>]
    let private DataTypeChildren = 0x00000000u<word>

    [<Literal>]
    let private DataTypeString = 0x40000000u<word>

    [<Literal>]
    let private DataTypeExtended = 0x80000000u<word>

    [<Literal>]
    let private ChildMask = 0x000000FFu<mask>

    [<Literal>]
    let private StringIndexMask = 0x00FFFFFFu<mask>

    [<Literal>]
    let private ExtendedDataMask = 0x00FFFFFFu<mask>

    // Bits 24-29 of the data word, whatever the two type bits above them say. They are free in
    // every layout: the child mask is 8 bits wide and the string and extended indices are 24.
    [<Literal>]
    let private CommonDataMask = 0x3F000000u<mask>

    [<Literal>]
    let private CommonDataShift = 24<bits>

    /// <summary>
    /// What a node's <c>data</c> word means.
    /// </summary>
    [<Struct>]
    type NodeData =
        /// A bitmap over the node's declared child slots.
        | Children of mask: uint32<mask>
        /// An index into the string table.
        | StringIndex of index: uint32<stringIndex>
        /// An index into the extended-data array.
        | Extended of index: uint32<byteOffset>

    /// A decoded `getSourceFile` response. Cheap to hold: it is the response bytes plus six
    /// section offsets read out of the header.
    [<Struct>]
    type SourceFile =
        {
            Data: byte[]
            StringTableOffsets: int
            StringTable: int
            ExtendedData: int
            StructuredData: int
            Nodes: int
        }

        /// Number of node records in the blob. Index 0 is a null slot; index 1 is the SourceFile.
        member this.NodeCount = (this.Data.Length - this.Nodes) / NodeLen

    let inline private u32 (data: byte[]) (offset: int) =
        BinaryPrimitives.ReadUInt32LittleEndian(ReadOnlySpan(data, offset, 4))

    let inline private i32 (data: byte[]) (offset: int) =
        BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(data, offset, 4))

    /// Reads the 44-byte header. Rejects incompatible protocol versions
    let read (data: byte[]) : SourceFile =
        if data.Length < HeaderSize then
            failwithf $"source file blob is %d{data.Length} bytes, shorter than the %d{HeaderSize}-byte header"

        let version = (u32 data 0) >>> 24

        if version <> ProtocolVersion then
            failwithf $"unsupported AST protocol version %d{version} (this client implements %d{ProtocolVersion})"

        {
            Data = data
            StringTableOffsets = int (u32 data 24)
            StringTable = int (u32 data 28)
            ExtendedData = int (u32 data 32)
            StructuredData = int (u32 data 36)
            Nodes = int (u32 data 40)
        }

    /// The 128-bit content hash, used to tell whether a cached blob is still current.
    let contentHash (file: SourceFile) =
        let hex (v: uint32) = $"%08x{v}"

        hex (u32 file.Data 16)
        + hex (u32 file.Data 12)
        + hex (u32 file.Data 8)
        + hex (u32 file.Data 4)

    let inline private nodeAt (file: SourceFile) (index: int) = file.Nodes + index * NodeLen

    /// The node's kind. `KindNodeList` for a node list, which is not a schema kind.
    let kind (file: SourceFile) (index: int) : SyntaxKind =
        LanguagePrimitives.EnumOfValue(u32 file.Data (nodeAt file index))

    /// <summary>
    /// Start offset w/ leading trivia in UTF-16 code units.
    /// </summary>
    let pos (file: SourceFile) (index: int) = i32 file.Data (nodeAt file index + 4)

    /// <summary>
    /// End offset in UTF-16 code units.
    /// </summary>
    let endPos (file: SourceFile) (index: int) = i32 file.Data (nodeAt file index + 8)

    /// The next sibling within a node list; 0 terminates.
    let next (file: SourceFile) (index: int) =
        int (u32 file.Data (nodeAt file index + 12))

    let parent (file: SourceFile) (index: int) =
        int (u32 file.Data (nodeAt file index + 16))

    /// The node's `NodeFlags` - `Const`, `Ambient`, `JavaScriptFile` and the rest of the word
    /// the parser set while reading it.
    let flags (file: SourceFile) (index: int) : NodeFlags =
        LanguagePrimitives.EnumOfValue<uint32, NodeFlags>(u32 file.Data (nodeAt file index + 24))

    let data (file: SourceFile) (index: int) =
        // The word is one role until its type bits are read and another afterwards, so each
        // branch reinterprets explicitly - that call is the whole of what the measures cannot
        // check, and it is now three lines instead of being spread across the module.
        let raw: uint32<word> = tag (u32 file.Data (nodeAt file index + 20))

        match raw &&&& DataTypeMask with
        | DataTypeString -> StringIndex(reinterpret (raw &&&& StringIndexMask))
        | DataTypeExtended -> Extended(reinterpret (raw &&&& ExtendedDataMask))
        | _ -> Children(reinterpret (raw &&&& ChildMask))

    /// A node has children when the record that follows it names it as parent.
    let hasChildren (file: SourceFile) (index: int) =
        index + 1 < file.NodeCount && parent file (index + 1) = index

    /// Reads string `index` from the string table.
    let getString (file: SourceFile) (index: uint32<stringIndex>) =
        // The offsets are UTF-8 byte offsets
        let index = int index
        let start = int (u32 file.Data (file.StringTableOffsets + index * 4))
        let finish = int (u32 file.Data (file.StringTableOffsets + (index + 1) * 4))
        Wtf8.decode (ReadOnlySpan(file.Data, file.StringTable + start, finish - start))

    /// <summary>
    /// The six commonData bits of a node's data word, shifted down to bit 0.
    /// </summary>
    /// <remarks>
    /// What they mean is per kind and is generated: see the accessors in <c>AstNode</c>. A few
    /// nodes - the literals, and <c>SyntheticExpression</c> - fill these bits from a compiler
    /// function that is not part of the schema, and have no generated accessors as a result.
    /// </remarks>
    let commonData (file: SourceFile) (index: int) =
        // Returns a bare `uint32`: the generated accessors in `AstNode` mask these bits with
        // plain literals, so measuring the result is a generator change, not a decoder one.
        let raw: uint32<word> = tag (u32 file.Data (nodeAt file index + 20))
        uint32 (raw &&&& CommonDataMask >>>> CommonDataShift)

    // Extended data is a per-node record in its own section, and its layout is the one part of
    // the format the schema does not describe - it is fixed in the compiler's own client
    // (`generate-encoder.ts:1897-1955`), so these offsets are hand-written and only these kinds
    // are supported. `SourceFile` has a record of its own, nineteen words wide; its offsets are
    // generated into `SourceFileRecord` and read through the file-level accessors at the end of
    // this module.
    /// A raw word of the node's extended record. Deliberately unmeasured: which role the word
    /// plays is decided by `offset`, so the caller tags it. `offset` stays a plain `int` because
    /// the `SourceFileRecord` literals that feed it are generated; emitting those as
    /// `int<byteOffset>` is a generator change.
    let inline private extendedWord (file: SourceFile) (index: int) (offset: int<byteOffset>) =
        match data file index with
        | Extended record -> ValueSome(u32 file.Data (file.ExtendedData + int record + int offset))
        | _ -> ValueNone

    /// The node's text, for the kinds that carry one. Identifiers and the like read straight
    /// from the string table; literals go through their extended-data record.
    let text (file: SourceFile) (index: int) =
        let kind = kind file index

        if AstKind.hasStringText kind then
            match data file index with
            | StringIndex string -> ValueSome(getString file string)
            | _ -> ValueNone
        elif AstKind.hasExtendedText kind then
            extendedWord file index 0<byteOffset> |> ValueOption.map (tag >> getString file)
        else
            ValueNone

    /// The unescaped source text of a template literal fragment.
    let rawText (file: SourceFile) (index: int) =
        match kind file index with
        | SyntaxKind.TemplateHead
        | SyntaxKind.TemplateMiddle
        | SyntaxKind.TemplateTail -> extendedWord file index 4<byteOffset> |> ValueOption.map (tag >> getString file)
        | _ -> ValueNone

    /// The literal's `TokenFlags`, for the kinds that record them.
    let tokenFlags (file: SourceFile) (index: int) =
        match kind file index with
        | SyntaxKind.StringLiteral
        | SyntaxKind.NumericLiteral
        | SyntaxKind.BigIntLiteral
        | SyntaxKind.RegularExpressionLiteral ->
            extendedWord file index 4<byteOffset>
            |> ValueOption.map LanguagePrimitives.EnumOfValue<uint32, TokenFlags>
        | _ -> ValueNone

    /// The template fragment's `TokenFlags`, which sit past its raw text.
    let templateFlags (file: SourceFile) (index: int) =
        match kind file index with
        | SyntaxKind.TemplateHead
        | SyntaxKind.TemplateMiddle
        | SyntaxKind.TemplateTail ->
            extendedWord file index 8<byteOffset>
            |> ValueOption.map LanguagePrimitives.EnumOfValue<uint32, TokenFlags>
        | _ -> ValueNone

    /// The child occupying declared slot `order`, if present.
    let childAtOrder (file: SourceFile) (index: int) (order: int<astSlot>) =
        // Only a `Children` data word carries a slot bitmap. A node whose word was spent on a
        // string or extended-data index still has its children in the blob, and every declared
        // slot of one is present - so the mask reads as all ones, as it does upstream
        // (`generate-encoder.ts:1808-1815`, where `childMask` returns -1). `SourceFile` is the
        // only such node with children, and it always has both of them.
        let mask =
            match data file index with
            | Children mask -> mask
            | StringIndex _
            | Extended _ -> ChildMask

        // With an all-ones fallback the emptiness check is load-bearing: a childless leaf would
        // otherwise report every slot present and walk `next` into unrelated nodes.
        if not (hasChildren file index) || not (hasAny (slotBit order) mask) then
            ValueNone
        else
            // Children are **not** stored contiguously - a child's own subtree sits between it and
            // its sibling - so we count how many present slots precede `order` and then walk that
            // many `next` pointers from the first child.
            //
            // Present slots below `order`; equivalently `order` minus the missing ones.
#if !NETSTANDARD2_1
            let propertyIndex = BitOperations.PopCount(uint32 (mask &&&& slotsBelow order))
#else
            let popCount32 (value: uint32) : int =
                // Subtract the shifted value to count pairs of bits
                let v1 = value - ((value >>> 1) &&& 0x55555555u)

                // Combine neighboring 2-bit fields into 4-bit fields
                let v2 = (v1 &&& 0x33333333u) + ((v1 >>> 2) &&& 0x33333333u)

                // Combine 4-bit fields into 8-bit fields, then multiply to sum all bytes
                int (((v2 + (v2 >>> 4)) &&& 0x0F0F0F0Fu) * 0x01010101u) >>> 24

            let propertyIndex = popCount32 (uint32 (mask &&&& slotsBelow order))
#endif

            let mutable child = index + 1

            for _ in 1..propertyIndex do
                child <- next file child

            ValueSome child

    /// Every child of a node, in order. For a node list these are its elements.
    let children (file: SourceFile) (index: int) =
        seq {
            if hasChildren file index then
                let mutable child = index + 1

                while child <> 0 do
                    yield child
                    child <- next file child
        }

    /// Depth-first walk from `index` inclusive.
    let rec descendants (file: SourceFile) (index: int) =
        seq {
            yield index

            for child in children file index do
                yield! descendants file child
        }

    /// The root SourceFile node.
    [<Literal>]
    let Root = 1

    // ─────────────────────────────────────────────────────────────────────────────────────────
    // The root node's extended-data record
    //
    // `SourceFile` spends its data word on an extended-data offset like the literals do, but its
    // record is nineteen words of file-level metadata rather than a text index. The offsets are
    // generated - see `SourceFileRecord` and `tools/tsc-ast/record.mts` - because the only
    // statement of the layout upstream is a table in a comment.
    //
    // There is one such node per blob, so these read as properties of the file rather than as
    // accessors on a node, and they take no index.
    // ─────────────────────────────────────────────────────────────────────────────────────────

    /// A `/// <reference ... />` directive, with its positions already in UTF-16 code units.
    [<Struct>]
    type FileReference =
        {
            Pos: int
            End: int
            FileName: string
            /// `ResolutionMode`, which the schema does not name; 0 is "unspecified".
            ResolutionMode: uint32
            Preserve: bool
        }

    /// One mapping between a virtual file and the original it was extracted from.
    [<Struct>]
    type SpanMapSegment =
        {
            VirtualStart: int
            VirtualLength: int
            OriginalStart: int
            OriginalLength: int
            Kind: SpanMapKind
            /// <summary>
            /// Absent when the compiler wrote a five-element segment.
            /// </summary>
            /// <remarks>
            /// Absent is not "no features": the reference client reads a segment with no sixth
            /// element as <c>SpanMapFeature.All</c> (`dist/api/node/node.js`, `get spanMap`). The
            /// wire fact is kept as it is rather than defaulted, so that a caller can tell the two
            /// forms apart - but a caller asking what the segment supports should read
            /// <c>ValueNone</c> as everything.
            /// </remarks>
            Features: SpanMapFeature voption
        }

    /// A suppression directive mapped from an original file onto a virtual one, in UTF-16 code
    /// units.
    [<Struct>]
    type DiagnosticDirective =
        {
            OriginalStart: int
            OriginalLength: int
            VirtualStart: int
            VirtualLength: int
            Policy: DiagnosticDirectivePolicy
            UnusedCode: uint32
        }

    let private sourceFileWord (file: SourceFile) (offset: int<byteOffset>) =
        match extendedWord file Root offset with
        | ValueSome word -> word
        | ValueNone ->
            failwithf $"the root node is a %A{kind file Root} carrying %A{data file Root}, not a SourceFile record"

    /// A record field that is an index into the string table. The one place the three meanings a
    /// record word can carry - string index, node index, structured-data offset - are told apart,
    /// so the measure earns its keep here.
    let private stringIndexField (file: SourceFile) (offset: int<byteOffset>) : uint32<stringIndex> =
        tag (sourceFileWord file offset)


    /// A string field of the record, absent when the compiler had nothing to put there.
    let private sourceFileString (file: SourceFile) (offset: int<byteOffset>) =
        match sourceFileWord file offset with
        | SourceFileRecord.Absent -> ValueNone
        | index -> ValueSome(getString file (tag index))

    /// <summary>
    /// A cursor over the structured-data blob a record field points at.
    /// </summary>
    /// <remarks>
    /// An empty collection is written as "absent" rather than as an empty array, so the two are
    /// the same thing here.
    /// </remarks>
    let private structured (file: SourceFile) (offset: int<byteOffset>) =
        match sourceFileWord file offset with
        | SourceFileRecord.Absent -> ValueNone
        | value -> ValueSome(Msgpack.Reader(file.Data, tagInt (file.StructuredData + int value)))

    let private readArray (file: SourceFile) (offset: int<byteOffset>) (read: Msgpack.Reader -> 'T) =
        match structured file offset with
        | ValueNone -> Array.empty
        | ValueSome reader -> reader.ReadArray(reader.ReadArrayLength(), read)

    let private readFileReferences (file: SourceFile) (offset: int<byteOffset>) =
        readArray file offset (fun reader ->
            // `[pos; end; fileName; resolutionMode; preserve]`, per `encoder.go:734-750`.
            reader.ReadArrayLength() |> ignore

            {
                Pos = int (reader.ReadUInt32())
                End = int (reader.ReadUInt32())
                FileName = reader.ReadString()
                ResolutionMode = reader.ReadUInt32()
                Preserve = reader.ReadBool()
            })

    /// The file's whole source text, which the string table is mostly made of.
    let sourceText (file: SourceFile) =
        getString file (stringIndexField file SourceFileRecord.Text)

    /// The file name the compiler knows this file by.
    let fileName (file: SourceFile) =
        getString file (stringIndexField file SourceFileRecord.FileName)

    /// The file's canonicalised path.
    let path (file: SourceFile) =
        getString file (stringIndexField file SourceFileRecord.Path)

    /// <summary>
    /// The text as it was before any transformation, equal to <c>sourceText</c> for an ordinary
    /// file.
    /// </summary>
    let originalText (file: SourceFile) =
        getString file (stringIndexField file SourceFileRecord.OriginalText)

    /// The file's `ScriptKind`: what the compiler decided it was reading.
    let scriptKind (file: SourceFile) : ScriptKind =
        LanguagePrimitives.EnumOfValue<uint32, ScriptKind>(sourceFileWord file SourceFileRecord.ScriptKind)

    /// The file's `LanguageVariant`, i.e. whether JSX syntax is on.
    let languageVariant (file: SourceFile) : LanguageVariant =
        LanguagePrimitives.EnumOfValue<uint32, LanguageVariant>(sourceFileWord file SourceFileRecord.LanguageVariant)

    /// <summary>
    /// The node whose presence made this file a module, if any.
    /// </summary>
    let externalModuleIndicator (file: SourceFile) =
        match sourceFileWord file SourceFileRecord.ExternalModuleIndicator with
        | 0u -> ValueNone
        | index -> ValueSome(int index)

    /// The `/// <reference path=... />` directives.
    let referencedFiles (file: SourceFile) =
        readFileReferences file SourceFileRecord.ReferencedFiles

    /// The `/// <reference types=... />` directives.
    let typeReferenceDirectives (file: SourceFile) =
        readFileReferences file SourceFileRecord.TypeReferenceDirectives

    /// The `/// <reference lib=... />` directives.
    let libReferenceDirectives (file: SourceFile) =
        readFileReferences file SourceFileRecord.LibReferenceDirectives

    /// The module specifiers this file imports, as nodes.
    let imports (file: SourceFile) =
        readArray file SourceFileRecord.Imports (fun reader -> int (reader.ReadUInt32()))

    /// The `declare module "..."` augmentations, as nodes.
    let moduleAugmentations (file: SourceFile) =
        readArray file SourceFileRecord.ModuleAugmentations (fun reader -> int (reader.ReadUInt32()))

    /// The names of the ambient modules this file declares.
    let ambientModuleNames (file: SourceFile) =
        readArray file SourceFileRecord.AmbientModuleNames _.ReadString()

    // The rest of the record describes virtual files - a source file the compiler synthesised
    // from part of another one, which every one of these fields hangs off
    // (`ast.ContentMapperSourceFileInfo`). `tsc` over an ordinary file never writes them, so they
    // read as absent, and they are here for completeness rather than because anything produces
    // them yet.

    /// <summary>
    /// The suppression directives carried over from the file this one was mapped from.
    /// </summary>
    /// <remarks>
    /// Not the `@ts-ignore` and `@ts-expect-error` comments of an ordinary file, which the
    /// compiler keeps elsewhere: these are the mapped directives of a virtual file, so an
    /// ordinary one has none however many such comments it contains.
    /// </remarks>
    let diagnosticDirectives (file: SourceFile) =
        readArray file SourceFileRecord.DiagnosticDirectives (fun reader ->
            // `[originalStart; originalLength; virtualStart; virtualLength; policy; unusedCode]`.
            reader.ReadArrayLength() |> ignore

            {
                OriginalStart = int (reader.ReadUInt32())
                OriginalLength = int (reader.ReadUInt32())
                VirtualStart = int (reader.ReadUInt32())
                VirtualLength = int (reader.ReadUInt32())
                Policy = LanguagePrimitives.EnumOfValue<uint32, DiagnosticDirectivePolicy>(reader.ReadUInt32())
                UnusedCode = reader.ReadUInt32()
            })

    /// <summary>
    /// The mapping back to the file this one was extracted from, empty when there is none.
    /// </summary>
    let spanMap (file: SourceFile) =
        readArray file SourceFileRecord.SpanMap (fun reader ->
            // The sixth element is optional, so the header length is load-bearing here.
            let length = reader.ReadArrayLength()

            {
                VirtualStart = int (reader.ReadUInt32())
                VirtualLength = int (reader.ReadUInt32())
                OriginalStart = int (reader.ReadUInt32())
                OriginalLength = int (reader.ReadUInt32())
                Kind = LanguagePrimitives.EnumOfValue<uint32, SpanMapKind>(reader.ReadUInt32())
                Features =
                    if length > 5 then
                        ValueSome(LanguagePrimitives.EnumOfValue<uint32, SpanMapFeature>(reader.ReadUInt32()))
                    else
                        ValueNone
            })

    /// The names of the files this one was assembled from.
    let supplementalSourceFileNames (file: SourceFile) =
        readArray file SourceFileRecord.SupplementalSourceFileNames _.ReadString()

    /// The name of the file this one is a projection of.
    let canonicalSourceFileName (file: SourceFile) =
        sourceFileString file SourceFileRecord.CanonicalSourceFileName

    /// The content mapper that produced this file.
    let contentMapper (file: SourceFile) =
        sourceFileString file SourceFileRecord.ContentMapper

    /// The name this file is addressed by when it is virtual.
    let virtualFileName (file: SourceFile) =
        sourceFileString file SourceFileRecord.VirtualFileName

/// <summary>
/// How a checker response names a syntax node: <c>SymbolResponse.Declarations</c>,
/// <c>SignatureResponse.Declaration</c> and the <c>location</c> argument of every request that
/// takes one all carry this string. It is <c>RemoteNode.id</c> in the typescript package,
/// spelled <c>index.kind.path</c>.
/// </summary>
/// <remarks>
/// A handle identifies a node within the program that produced it - the same snapshot and the
/// same project. Against any other program it points at a different node or at none.
/// </remarks>
[<Struct>]
type NodeHandle =
    {
        /// Index of the node's record in its file's blob, as `Node.ofIndex` takes it.
        Index: int
        /// The kind the response claims. The blob is the authority; narrow with a view.
        Kind: SyntaxKind
        /// Canonical path of the file, as `Ast.path` spells it.
        Path: string
    }

[<RequireQualifiedAccess>]
module NodeHandle =

    /// <summary>
    /// Decodes a handle. <c>ValueNone</c> where the string is not one.
    /// </summary>
    /// <remarks>
    /// A path carries dots and, on Windows, a drive colon, so only the first two dots separate
    /// fields; everything after the second is the path.
    /// </remarks>
    let parse (handle: string) : NodeHandle voption =
        if String.IsNullOrEmpty handle then
            ValueNone
        else
            match handle.Split([| '.' |], 3) with
            | [| index; kind; path |] ->
                match Int32.TryParse index, UInt32.TryParse kind with
                | (true, index), (true, kind) when path.Length > 0 ->
                    ValueSome
                        {
                            Index = index
                            Kind = LanguagePrimitives.EnumOfValue<uint32, SyntaxKind> kind
                            Path = path
                        }
                | _ -> ValueNone
            | _ -> ValueNone

    /// The string form, for the `location` argument of a checker request.
    let format (handle: NodeHandle) : string =
        $"{handle.Index}.{uint32 handle.Kind}.{handle.Path}"
