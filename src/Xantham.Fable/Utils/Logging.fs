// ---------------------------------------------------------------------------
// Attribution
// The printf / .NET message-template parsing below -- printfFmtSpecPattern,
// netMsgHolePattern, logMsgParamNameRegex, processLogMsgParams,
// collectArgsDynamic and the klogf / logf / vlogf / elogf family -- is derived
// from the FSharp.Logf project (distributed as the Fable.FSharp.Logf package),
// used here under its original license. Adapted for Xantham's structured
// (JSON) logging needs.
// ---------------------------------------------------------------------------
module Xantham.Fable.Utils.Logging
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Microsoft.Extensions.Logging
open Printf
open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices


/// <summary>
/// Regex fragment matching a single printf-style format specifier (e.g. <c>%s</c>, <c>%+6.4d</c>).
/// </summary>
/// <remarks>
/// Not exhaustive: it does not reject specifiers that are syntactically plausible but invalid — repeated
/// flags (<c>%+++d</c>), width/precision on types that disallow them, or non-format letters (<c>%z</c>).
/// Interpolated-string holes appear as the special <c>%P()</c> case.
/// See the
/// <see href="https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf">F# format specifier reference</see>.
/// </remarks>
let printfFmtSpecPattern =
    """%"""
    // flags
    + """(0|-|\+| )*"""
    // width
    + """[0-9]*"""
    // precision
    + """(\.\d+)?"""
    // type -- interpolated-string holes are the special "%P()" case
    + """(P\(\)|[a-zA-Z])"""

/// <summary>
/// Regex fragment matching a .NET-style message hole (e.g. <c>{argName}</c>, <c>{@argName:fmt}</c>),
/// exposing named groups <c>start</c>, <c>argName</c>, <c>fmt</c> and <c>end</c>.
/// </summary>
let netMsgHolePattern =
    """(?<start>"""
        + """\{(?<argName>@?"""
        + """[a-zA-Z0-9_]+)"""
    + """)"""
    + """(?<fmt>"""
        + """(,[^:\}]+)?"""
        + """(:[^\}]+)?"""
    + """)"""
    + """(?<end>"""
        + """\}"""
    + """)"""

/// <summary>
/// Matches a printf-style format specifier optionally followed immediately by a .NET-style message hole
/// (e.g. <c>%s{myValue}</c>). The <c>printfFmt</c> group captures the specifier.
/// </summary>
let logMsgParamNameRegex =
    Regex("""(?<printfFmt>""" + printfFmtSpecPattern + """)(""" + netMsgHolePattern + """)?""", RegexOptions.ECMAScript)

/// <summary>
/// Structured representation of a single log call: the rendered <see cref="P:Message"/> for the console
/// logger, plus the original template and positional arg names/values for structured loggers such as the
/// JSON text writer.
/// </summary>
/// <category>Logging</category>
type LogEntry =
    {
      /// The fully rendered message — printf specifiers applied, holes removed.
      Message: string
      /// The message template with printf specifiers removed and <c>{name}</c> holes preserved.
      Template: string
      /// Positional argument names extracted from holes; <c>None</c> where a specifier had no hole.
      ArgNames: string option list
      /// Positional argument values, in application order.
      Args: obj[]
      /// Caller file path captured by <see cref="T:Log"/>; empty when not provided. Surfaces in JSON output only.
      FilePath: string
      /// Caller line number captured by <see cref="T:Log"/>; <c>0</c> when not provided. Surfaces in JSON output only.
      FileLine: int }

/// <summary>
/// Scans a printf-style format literal and splits out its message holes.
/// </summary>
/// <param name="format">The format literal to process.</param>
/// <returns>
/// A triple of: the positional arg names (<c>None</c> when a specifier had no hole); the format with holes
/// stripped and printf specifiers kept (fed to <c>ksprintf</c>); and the JSON template with printf
/// specifiers stripped and bare <c>{name}</c> holes kept (Serilog <c>@</c> sigil preserved).
/// </returns>
let processLogMsgParams (format: Format<'Printer, 'State, 'Residue, 'Result, 'Tuple>) : string option list * Format<'Printer, 'State, 'Residue, 'Result, 'Tuple> * string =
    let paramArgNames = System.Collections.Generic.List<string option>()
    let processed: string =
        logMsgParamNameRegex.Replace (format.Value, (fun m ->
            let arg =
                m.Groups["argName"]
                |> Option.ofObj
                |> Option.map _.Value
                |> Option.filter (String.IsNullOrWhiteSpace >> not)
            paramArgNames.Add arg
            m.Groups["printfFmt"].Value
        ))
    let jsonTemplate: string =
        logMsgParamNameRegex.Replace (format.Value, (fun m ->
            let start = m.Groups["start"]
            if start.Success then start.Value + m.Groups["end"].Value
            else ""
        ))
    Seq.toList paramArgNames, (new Format<'Printer, 'State, 'Residue, 'Result, 'Tuple>(processed)), jsonTemplate

/// <summary>
/// Wraps a constructed printf function so each of its <paramref name="n"/> curried arguments is captured
/// left-to-right into <paramref name="collected"/> as it is applied.
/// </summary>
/// <param name="n">The number of curried arguments to intercept.</param>
/// <param name="collected">The list each applied argument value is appended to.</param>
/// <param name="f">The constructed printf function to wrap.</param>
/// <returns>A function with the same arity as <paramref name="f"/> that records its arguments as a side effect.</returns>
/// <remarks>Fable-only: relies on reinterpret-casting the function, which is impossible on the .NET runtime.</remarks>
let rec collectArgsDynamic (n: int) (collected: System.Collections.Generic.List<obj>) (f: obj) : obj =
    if n <= 0 then f
    else
        let f' = f :?> obj -> obj
        (fun (x: obj) ->
            collected.Add x
            let y = f' x
            collectArgsDynamic (n - 1) collected y) :> obj

/// <summary>
/// Builds a <see cref="T:LogEntry"/> from a printf-style format — capturing both the rendered message and
/// the structured arg names/values — then passes it to <paramref name="continuation"/>.
/// </summary>
/// <param name="continuation">Receives the assembled <see cref="T:LogEntry"/> once all arguments are applied.</param>
/// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
let klogf (continuation: LogEntry -> 'Result) (format: Format<'T, unit, string, 'Result, 'Tuple>) : 'T =
    let argNames, processedFmt, jsonTemplate = processLogMsgParams format
    let collected = System.Collections.Generic.List<obj>()
    let f =
        ksprintf
            (fun msg ->
                continuation
                    { Message = msg
                      Template = jsonTemplate
                      ArgNames = argNames
                      Args = collected.ToArray()
                      FilePath = ""
                      FileLine = 0 })
            processedFmt
        |> unbox<'T>
    let f' = collectArgsDynamic (List.length argNames) collected f
    f' |> unbox<'T>

/// <summary>Logs a printf-style formatted message to <paramref name="logger"/> at the given level.</summary>
/// <param name="logger">The logger to write to.</param>
/// <param name="logLevel">The severity level of the entry.</param>
/// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
let logf (logger: ILogger) logLevel (format: Format<'T, unit, string, unit, 'Tuple>) : 'T =
    klogf
        (fun entry -> logger.Log (logLevel, EventId(0), box entry, null, Func<_,_,_>(fun _ _ -> entry.Message)))
        format

/// <summary>Logs a printf-style formatted message with an explicit <see cref="T:EventId"/> and exception.</summary>
/// <param name="logger">The logger to write to.</param>
/// <param name="logLevel">The severity level of the entry.</param>
/// <param name="eventId">The event id to associate with the entry.</param>
/// <param name="exn">The exception to associate with the entry.</param>
/// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
let vlogf (logger: ILogger) (logLevel: LogLevel) (eventId: EventId) (exn: Exception) format =
    klogf (fun entry -> logger.Log (logLevel, eventId, box entry, exn, Func<_,_,_>(fun _ _ -> entry.Message))) format

/// <summary>Logs a printf-style formatted message together with an associated exception.</summary>
/// <param name="logger">The logger to write to.</param>
/// <param name="logLevel">The severity level of the entry.</param>
/// <param name="exn">The exception to associate with the entry.</param>
/// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
let elogf (logger: ILogger) (logLevel: LogLevel) (exn: Exception) (format: Format<'T, unit, string, unit, 'Tuple>) : 'T =
    klogf
        (fun entry -> logger.Log (logLevel, EventId(0), box entry, exn, Func<_,_,_>(fun _ _ -> entry.Message)))
        format

/// <summary>
/// Renders a <see cref="T:LogEntry"/> as a single-line JSON object with a stable, LLM-friendly shape:
/// <c>{ level, format, at?, data?, exception? }</c>.
/// </summary>
/// <param name="level">The severity level, emitted as its enum name.</param>
/// <param name="entry">The structured entry to render.</param>
/// <param name="err">The associated exception, or <c>null</c> when there is none.</param>
/// <returns>A single-line JSON string.</returns>
/// <remarks>
/// <c>at</c> is <c>"file:line"</c>, omitted when there is no caller info. <c>data</c> nests all arguments
/// under one key, omitted when there are none — keeping the top-level schema fixed and preventing arg
/// names from colliding with reserved fields. Named holes are keyed by name (Serilog <c>@</c> sigil
/// stripped from the key); unnamed specifiers are keyed <c>argN</c>.
/// </remarks>
let formatEntryAsJson (level: LogLevel) (entry: LogEntry) (err: Exception) : string =
    let data =
        entry.ArgNames
        |> List.mapi (fun i name ->
            let key =
                match name with
                | Some n -> n.TrimStart('@')
                | None -> sprintf "arg%d" i
            let value = if i < entry.Args.Length then entry.Args[i] else null
            key, value)
    let fields = [
        if not (isNull err) then "exception", box err.Message
        if not (List.isEmpty data) then
            "data", createObj data
        if not (String.IsNullOrEmpty entry.FilePath) then
            "at", box (sprintf "%s:%d" entry.FilePath entry.FileLine)
        "level", box (Enum.GetName(typeof<LogLevel>, level))
        "format", box entry.Template
    ]
    JS.JSON.stringify (createObj fields)

/// <summary>
/// Base <see cref="T:ILogger"/> implementation that writes to a JS <c>Console</c>, prefixing each entry
/// with its level by default. Override <see cref="M:WriteEntry"/> to change the output format.
/// </summary>
/// <param name="console">The JS console to write to; defaults to the global console.</param>
/// <param name="prependLevelToEntries">Whether to prefix each rendered message with <c>[LEVEL]</c>; defaults to <c>true</c>.</param>
/// <param name="logLevel">The minimum level emitted; defaults to <c>Debug</c> in debug builds, <c>Warning</c> otherwise.</param>
/// <category>Loggers</category>
type Logger(?console: JS.Console, ?prependLevelToEntries: bool, ?logLevel: LogLevel) =
    let prependLevelToEntries = defaultArg prependLevelToEntries true
    let console = defaultArg console JS.console
    /// The minimum level emitted; entries below it are dropped.
    member val LogLevel =
        #if DEBUG
        defaultArg logLevel LogLevel.Debug
        #else
        defaultArg logLevel LogLevel.Warning
        #endif
    /// The underlying JS console entries are written to.
    member _.Console = console
    /// Whether each rendered message is prefixed with <c>[LEVEL]</c>.
    member _.PrependLevelToEntries = prependLevelToEntries
    /// <summary>
    /// Emits a single, already level-filtered entry. Override to change the output format.
    /// </summary>
    /// <param name="level">The severity level of the entry.</param>
    /// <param name="entry">Structured data when the call came through <c>klogf</c>; otherwise <c>None</c>.</param>
    /// <param name="message">The rendered fallback message string.</param>
    /// <param name="err">The associated exception, or <c>null</c>.</param>
    abstract member WriteEntry: level: LogLevel * entry: LogEntry option * message: string * err: Exception -> unit
    default this.WriteEntry (level, _entry, message, err) =
        let log m =
            match level with
            | LogLevel.None -> ()
            | LogLevel.Critical | LogLevel.Error -> console.error m
            | LogLevel.Warning -> console.warn m
            | LogLevel.Information -> console.info m
            | LogLevel.Trace -> console.debug m
            | _ -> console.log m
        let m =
            if prependLevelToEntries then
                sprintf "[%s] " (Enum.GetName(typeof<LogLevel>, level).ToUpper()) + message
            else
                message
        log m
        if not (isNull err) then
            log ("Exception: " + err.Message)
    interface ILogger with
        member this.BeginScope state = raise (System.NotImplementedException())
        member this.IsEnabled l = l >= this.LogLevel
        member this.Log (level, _, state, err, formatter) =
            if (this :> ILogger).IsEnabled level |> not then () else
            let entry =
                match box state with
                | :? LogEntry as e -> Some e
                | _ -> None
            this.WriteEntry (level, entry, formatter.Invoke (state, err), err)

/// <summary><see cref="T:Logger"/> bound to the global JS console.</summary>
/// <param name="prependLevelToEntries">Whether to prefix each rendered message with <c>[LEVEL]</c>; defaults to <c>true</c>.</param>
/// <param name="logLevel">The minimum level emitted; defaults to <c>Debug</c> in debug builds, <c>Warning</c> otherwise.</param>
/// <category>Loggers</category>
type ConsoleLogger(?prependLevelToEntries: bool, ?logLevel: LogLevel) =
    inherit Logger(JS.console, ?prependLevelToEntries = prependLevelToEntries, ?logLevel = logLevel)
    /// This logger upcast to <see cref="T:ILogger"/>.
    member inline this.ILogger = this :> ILogger

/// <summary>
/// <see cref="T:Logger"/> that writes one JSON object per line (via <see cref="M:formatEntryAsJson"/>) to a
/// file-backed stream; non-structured entries fall back to the base console renderer.
/// </summary>
/// <category>Loggers</category>
type TextWriterLogger private (_console, writeStream, prependLevelToEntries: bool, logLevel: LogLevel) =
    inherit Logger(_console, prependLevelToEntries = prependLevelToEntries, logLevel = logLevel)
    /// The underlying Node write stream.
    member val WriteStream = writeStream with get
    /// This logger upcast to <see cref="T:ILogger"/>.
    member inline this.ILogger = this :> ILogger
    override this.WriteEntry (level, entry, message, err) =
        match entry with
        | Some e -> this.Console.log (formatEntryAsJson level e err)
        | None -> base.WriteEntry (level, entry, message, err)
    /// <summary>Creates a logger that appends JSON lines to <paramref name="file"/>.</summary>
    /// <param name="file">Path of the file to append log lines to.</param>
    /// <param name="prependLevelToEntries">Unused by JSON output; forwarded to the base console fallback.</param>
    /// <param name="logLevel">The minimum level emitted.</param>
    new(file: string, ?prependLevelToEntries: bool, ?logLevel: LogLevel) =
        let prependLevelToEntries = defaultArg prependLevelToEntries true
        let logLevel = defaultArg logLevel LogLevel.Trace
        let writeStream = Node.Api.fs.createWriteStream(file:string)
        let myConsole =
            emitJsExpr writeStream "new console.Console($0)"
            |> unbox<JS.Console>
        TextWriterLogger(myConsole, writeStream, prependLevelToEntries = prependLevelToEntries, logLevel = logLevel)

type CombinedLogger(loggers: ILogger seq) =
    let loggers = loggers |> Seq.toArray
    interface ILogger with
        member this.BeginScope _ = raise <| NotImplementedException()
        member this.IsEnabled _ = true
        member this.Log(logLevel, eventId, state, ``exception``, var0) =
            for logger in loggers do
                logger.Log(logLevel, eventId, state, ``exception``, var0)


/// <summary>
/// Normalizes a caller file path: backslashes to forward slashes, and — when <paramref name="rootPath"/>
/// is given — strips that leading prefix so the result is repo-relative.
/// </summary>
/// <param name="rootPath">Prefix to strip; when empty, the path is only slash-normalized.</param>
/// <param name="path">The caller file path to normalize.</param>
/// <returns>The normalized, optionally repo-relative path.</returns>
let relativizePath (rootPath: string) (path: string) : string =
    let path = path.Replace('\\', '/')
    if String.IsNullOrEmpty rootPath then
        path
    else
        let root = rootPath.Replace('\\', '/').TrimEnd('/') + "/"
        if path.StartsWith(root, StringComparison.OrdinalIgnoreCase)
        then path.Substring root.Length
        else path

/// <summary>
/// Pre-wraps an <see cref="T:ILogger"/> so call sites pass only a format string. The caller's file path and
/// line number are captured automatically via <c>CallerFilePath</c>/<c>CallerLineNumber</c> and threaded
/// into the <see cref="T:LogEntry"/> — surfacing only in the structured JSON output, never the console
/// renderer.
/// </summary>
/// <param name="logger">The logger to forward entries to.</param>
/// <param name="rootPath">When supplied, stripped from captured caller paths so they are repo-relative.</param>
/// <category>Logging</category>
type Log(logger: ILogger, ?rootPath: string) =
    let rootPath = defaultArg rootPath (Node.Api.``process``.cwd())
    static let defaultLog = Log(ConsoleLogger())
    static member Default = defaultLog
    /// The wrapped logger.
    member _.Logger = logger
    /// Prefix stripped from captured caller paths; empty to keep paths absolute.
    member val RootPath = rootPath with get

    /// <summary>Logs <paramref name="format"/> at <paramref name="level"/>, capturing the call site automatically.</summary>
    /// <param name="level">The severity level of the entry.</param>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.emit
        (
            level: LogLevel,
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) =
        klogf
            (fun entry ->
                let entry = { entry with FilePath = relativizePath this.RootPath filePath; FileLine = fileLine }
                this.Logger.Log (level, EventId(0), box entry, null, Func<_,_,_>(fun _ _ -> entry.Message)))
            format

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Trace"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logft
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Trace, format, filePath, fileLine)

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Debug"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logfd
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Debug, format, filePath, fileLine)

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Information"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logfi
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Information, format, filePath, fileLine)

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Warning"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logfw
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Warning, format, filePath, fileLine)

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Error"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logfe
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Error, format, filePath, fileLine)

    /// <summary>Logs <paramref name="format"/> at <see cref="F:LogLevel.Critical"/>.</summary>
    /// <param name="format">The printf-style format, optionally containing <c>{name}</c> message holes.</param>
    /// <param name="filePath">Caller file path; supplied automatically by the compiler.</param>
    /// <param name="fileLine">Caller line number; supplied automatically by the compiler.</param>
    member inline this.logfc
        (
            format,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
            [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
        ) = this.emit (LogLevel.Critical, format, filePath, fileLine)
