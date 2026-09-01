// Logging.test.fsx
// Run via: npm run test:logging
// Compiled by Fable, executed in Node.js.
#r "nuget: Fable.Core, 5.0.0-beta.3"
#r "nuget: Fable.FSharp.Logf, 1.2.2"
#r "nuget: Fable.Node, 1.6.0"
#load "../src/Xantham.Fable/Utils/Logging.fs"

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Microsoft.Extensions.Logging
open Xantham.Fable.Utils.Logging

// ---------------------------------------------------------------------------
// Minimal inline test runner (mirrors Signal.test.fsx)
// ---------------------------------------------------------------------------

let mutable private _passed = 0
let mutable private _failed = 0

let describe (name: string) (f: unit -> unit) =
    printfn $"\n  %s{name}"
    f ()

let it (name: string) (f: unit -> unit) =
    try
        f ()
        printfn $"    ✓ %s{name}"
        _passed <- _passed + 1
    with ex ->
        printfn $"    ✗ %s{name}"
        printfn $"      %s{ex.Message}"
        _failed <- _failed + 1

let shouldEqual<'a when 'a : equality> (expected: 'a) (actual: 'a) =
    if actual <> expected then
        failwithf $"  expected: %A{expected}\n       got: %A{actual}"

let shouldBeTrue (msg: string) (condition: bool) =
    if not condition then failwith msg

// Fake JS.Console that records (channel, message) pairs.
let makeConsole (sink: ResizeArray<string * string>) : JS.Console =
    let push channel (m: obj) = sink.Add(channel, string m)
    createObj [
        "log",   box (fun (m: obj) -> push "log" m)
        "error", box (fun (m: obj) -> push "error" m)
        "warn",  box (fun (m: obj) -> push "warn" m)
        "info",  box (fun (m: obj) -> push "info" m)
        "debug", box (fun (m: obj) -> push "debug" m)
    ] |> unbox

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe "processLogMsgParams" (fun () ->

    it "extracts named arg, strips holes in processedFmt, strips specs in jsonTemplate" (fun () ->
        let names, processed, json = processLogMsgParams "Hello %s{personName}"
        shouldEqual [Some "personName"] names
        shouldEqual "Hello %s" processed.Value
        shouldEqual "Hello {personName}" json)

    it "None argName when a spec has no hole" (fun () ->
        let names, processed, json = processLogMsgParams "Hello %s"
        shouldEqual [None] names
        shouldEqual "Hello %s" processed.Value
        shouldEqual "Hello " json)

    it "preserves the @ sigil in jsonTemplate" (fun () ->
        let _, _, json = processLogMsgParams "Got %A{@payload}"
        shouldEqual "Got {@payload}" json)

    it "handles multiple holes positionally" (fun () ->
        let names, processed, json = processLogMsgParams "%s{a} and %d{b}"
        shouldEqual [Some "a"; Some "b"] names
        shouldEqual "%s and %d" processed.Value
        shouldEqual "{a} and {b}" json)

    it "drops a :customFmt from the jsonTemplate hole" (fun () ->
        let names, _, json = processLogMsgParams "%d{count:#.#}"
        shouldEqual [Some "count"] names
        shouldEqual "{count}" json)
)

describe "klogf / LogEntry construction" (fun () ->

    it "renders Message and captures structured fields" (fun () ->
        let entry: LogEntry = klogf id "Hello %s{personName}" "World"
        shouldEqual "Hello World" entry.Message
        shouldEqual "Hello {personName}" entry.Template
        shouldEqual [Some "personName"] entry.ArgNames
        shouldEqual 1 entry.Args.Length
        shouldEqual (box "World") entry.Args[0])

    it "captures multiple args left-to-right" (fun () ->
        let entry: LogEntry = klogf id "%s{a} %d{b}" "x" 7
        shouldEqual [| box "x"; box 7 |] entry.Args
        shouldEqual "x 7" entry.Message)

    it "handles a format with no args" (fun () ->
        let entry: LogEntry = klogf id "just text"
        shouldEqual "just text" entry.Message
        shouldEqual "just text" entry.Template
        shouldEqual ([]: string option list) entry.ArgNames
        shouldEqual 0 entry.Args.Length)
)

describe "formatEntryAsJson" (fun () ->

    it "emits level name, format and nests args under data" (fun () ->
        let entry: LogEntry = klogf id "Hello %s{personName}" "World"
        let parsed = JS.JSON.parse (formatEntryAsJson LogLevel.Debug entry null)
        shouldEqual "Debug" (unbox parsed?level)
        shouldEqual "Hello {personName}" (unbox parsed?format)
        shouldEqual "World" (unbox parsed?data?personName))

    it "keys unnamed args as argN under data" (fun () ->
        let entry: LogEntry = klogf id "val %d" 42
        let parsed = JS.JSON.parse (formatEntryAsJson LogLevel.Information entry null)
        shouldEqual 42 (unbox parsed?data?arg0))

    it "strips @ from the data key but keeps it in the format" (fun () ->
        let entry: LogEntry = klogf id "%s{@thing}" "payload"
        let parsed = JS.JSON.parse (formatEntryAsJson LogLevel.Warning entry null)
        shouldEqual "{@thing}" (unbox parsed?format)
        shouldEqual "payload" (unbox parsed?data?thing))

    it "omits the data key entirely when there are no args" (fun () ->
        let entry: LogEntry = klogf id "no args here"
        let json = formatEntryAsJson LogLevel.Debug entry null
        shouldBeTrue $"data should be absent: {json}" (not (json.Contains "\"data\"")))

    it "includes an exception field when an error is present" (fun () ->
        let entry: LogEntry = klogf id "boom"
        let json = formatEntryAsJson LogLevel.Error entry (Exception "kaboom")
        shouldBeTrue $"exception field missing in: {json}" (json.Contains "\"exception\":\"kaboom\"")
        shouldBeTrue "level field missing" (json.Contains "\"level\":\"Error\""))
)

describe "Logger — console output" (fun () ->

    it "prepends the level and routes Information to console.info" (fun () ->
        let sink = ResizeArray()
        let logger = Logger(makeConsole sink, logLevel = LogLevel.Trace)
        logf logger LogLevel.Information "Hello %s{name}" "World"
        shouldEqual 1 sink.Count
        shouldEqual ("info", "[INFORMATION] Hello World") sink[0])

    it "routes Error to console.error" (fun () ->
        let sink = ResizeArray()
        let logger = Logger(makeConsole sink, logLevel = LogLevel.Trace)
        logf logger LogLevel.Error "bad %d{code}" 500
        shouldEqual "error" (fst sink[0]))

    it "suppresses entries below the configured LogLevel" (fun () ->
        let sink = ResizeArray()
        let logger = Logger(makeConsole sink, logLevel = LogLevel.Warning)
        logf logger LogLevel.Debug "noise %s{x}" "y"
        shouldEqual 0 sink.Count)

    it "omits the level prefix when prependLevelToEntries is false" (fun () ->
        let sink = ResizeArray()
        let logger = Logger(makeConsole sink, prependLevelToEntries = false, logLevel = LogLevel.Trace)
        logf logger LogLevel.Information "plain %s{x}" "msg"
        shouldEqual "plain msg" (snd sink[0]))
)

describe "Logger.WriteEntry — override seam" (fun () ->

    it "a subclass override receives the structured LogEntry" (fun () ->
        let captured = ResizeArray<LogEntry>()
        let logger =
            { new Logger(logLevel = LogLevel.Trace) with
                override _.WriteEntry(_, entry, _, _) =
                    match entry with
                    | Some e -> captured.Add e
                    | None -> () }
        logf logger LogLevel.Debug "Hi %s{who}" "there"
        shouldEqual 1 captured.Count
        shouldEqual "Hi {who}" captured[0].Template
        shouldEqual [Some "who"] captured[0].ArgNames
        shouldEqual (box "there") captured[0].Args[0])
)

describe "Log — wrapper with caller tracing" (fun () ->

    it "logfi routes through the wrapped logger at Information level" (fun () ->
        let sink = ResizeArray()
        let log = Log(Logger(makeConsole sink, logLevel = LogLevel.Trace))
        log.logfi "Hello %s{name}" "World"
        shouldEqual 1 sink.Count
        shouldEqual ("info", "[INFORMATION] Hello World") sink[0])

    it "logfe routes to console.error" (fun () ->
        let sink = ResizeArray()
        let log = Log(Logger(makeConsole sink, logLevel = LogLevel.Trace))
        log.logfe "boom %d{code}" 500
        shouldEqual "error" (fst sink[0]))

    it "captures caller file path and line number into the LogEntry" (fun () ->
        let captured = ResizeArray<LogEntry>()
        let inner =
            { new Logger(logLevel = LogLevel.Trace) with
                override _.WriteEntry(_, entry, _, _) =
                    match entry with
                    | Some e -> captured.Add e
                    | None -> () }
        let log = Log(inner)
        log.logfi "trace me %s{what}" "this"
        shouldEqual 1 captured.Count
        shouldBeTrue $"file path not captured: '{captured[0].FilePath}'" (captured[0].FilePath.EndsWith "Logging.test.fsx")
        shouldBeTrue $"line not captured: {captured[0].FileLine}" (captured[0].FileLine > 0))

    it "strips the rootPath prefix from captured file paths" (fun () ->
        let captured = ResizeArray<LogEntry>()
        let inner =
            { new Logger(logLevel = LogLevel.Trace) with
                override _.WriteEntry(_, entry, _, _) =
                    match entry with
                    | Some e -> captured.Add e
                    | None -> () }
        // rootPath = the absolute dir holding this test file, derived from a no-root capture
        let probe = Log(inner)
        probe.logfi "probe"
        let abs = captured[0].FilePath
        let root = abs.Substring(0, abs.Length - "Logging.test.fsx".Length)
        captured.Clear()
        let log = Log(inner, root)
        log.logfi "rel"
        shouldEqual "Logging.test.fsx" captured[0].FilePath)

    it "formatEntryAsJson emits 'at' as file:line when caller info is present" (fun () ->
        let entry: LogEntry =
            { Message = "m"; Template = "t"; ArgNames = []; Args = [||]
              FilePath = "src/Foo/Bar.fs"; FileLine = 42 }
        let parsed = JS.JSON.parse (formatEntryAsJson LogLevel.Debug entry null)
        shouldEqual "src/Foo/Bar.fs:42" (unbox parsed?at))

    it "formatEntryAsJson omits 'at' when filePath is empty" (fun () ->
        let entry: LogEntry = klogf id "no caller %s{x}" "y"
        let json = formatEntryAsJson LogLevel.Debug entry null
        shouldBeTrue $"'at' should be absent: {json}" (not (json.Contains "\"at\"")))
)

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

let total = _passed + _failed
printfn $"\n  %d{_passed}/%d{total} passing"

if _failed > 0 then
    printfn $"  %d{_failed} failing\n"
    failwithf $"%d{_failed} test(s) failed"
else
    printfn ""
