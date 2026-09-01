module Xantham.TypeScript.Wire.Tests.Callbacks

open System.IO
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")

let private exePath = Tsc.locate __SOURCE_DIRECTORY__

/// A file that exists nowhere on disk. Everything the compiler asks for other than this one -
/// the lib files, tsconfig.json, the package.json probes on the way up the tree - falls back to
/// the real filesystem, which is what makes a two-member filesystem enough to compile it.
let private virtualFile = Path.Combine(fixtures, "virtual.ts").Replace('\\', '/')

let private source = "export const answer: number = 42;\n"

/// The calls the server made, so a test can assert it asked at all rather than quietly reading
/// the real filesystem and passing for the wrong reason.
let private serving () =
    let seen = ResizeArray<string * string>()

    let fs =
        { VirtualFileSystem.Default with
            ReadFile =
                ValueSome(fun path ->
                    seen.Add("readFile", path)
                    if path = virtualFile then Content source else FallBack)
            FileExists =
                ValueSome(fun path ->
                    seen.Add("fileExists", path)
                    if path = virtualFile then ValueSome true else ValueNone) }

    seen, fs

let private withChannel fs (test: TscChannel -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use channel = new TscChannel(exe, fixtures, VirtualFileSystem.callbacks fs)
            Api.initialize channel |> ignore
            test channel

/// Compiles the virtual file as the only root, and hands back the session.
let private programOf (channel: TscChannel) =
    let program =
        Api.createProgram channel
            { CreateProgramParams.Default with
                RootFiles = ValueSome [| DocumentIdentifier.FileName virtualFile |] }

    program.Snapshot, program.Project.Value.Id

[<Tests>]
let callbackTests =
    testList "virtual filesystem" [
        match exePath with
        | None ->
            testCase "native tsc not found - callback tests skipped" <| fun _ ->
                skiptest "run `npm install` in tests/Xantham.TypeScript.Wire.Tests, or set XANTHAM_TSGO_EXE"
        | Some _ ->

        // The whole point, and the only test here that would notice if the frames were right but
        // the payloads were not: a file the disk does not have is parsed from the bytes we served.
        testCase "a file that exists only in memory compiles" <| fun () ->
            let seen, fs = serving ()

            withChannel fs (fun channel ->
                File.Exists virtualFile |> Flip.Expect.isFalse "the fixture is not on disk"

                let snapshot, project = programOf channel

                let request: GetSourceFileParams =
                    { Snapshot = snapshot; Project = project; File = DocumentIdentifier.FileName virtualFile }

                match Api.getSourceFile channel request with
                | ValueNone -> failtest "expected an AST for the virtual file"
                | ValueSome ast ->
                    Ast.sourceText ast |> Flip.Expect.equal "the text we served" source
                    Ast.kind ast Ast.Root |> Flip.Expect.equal "root" SyntaxKind.SourceFile

                seen
                |> Seq.exists (fun (name, path) -> name = "readFile" && path = virtualFile)
                |> Flip.Expect.isTrue "the server asked us for the file rather than the disk") ()

        // The argument is a JSON string, not a bare path: a quoted "C:/...". The typed surface
        // parses it, so what a caller sees is the path itself.
        testCase "a callback is handed the decoded path" <| fun () ->
            let seen, fs = serving ()

            withChannel fs (fun channel ->
                programOf channel |> ignore

                seen
                |> Seq.forall (fun (_, path) -> not (path.StartsWith "\"") && Path.IsPathRooted path)
                |> Flip.Expect.isTrue $"every argument decoded to a path, got %A{Seq.truncate 3 seen |> List.ofSeq}") ()

        // A member left unset is not registered at all, so the server never asks and goes
        // straight to the real filesystem. That is what makes a partial filesystem workable.
        testCase "only the members that are set are registered" <| fun _ ->
            let table = VirtualFileSystem.callbacks (snd (serving ()))
            table.Keys |> List.ofSeq |> List.sort |> Flip.Expect.equal "" [ "fileExists"; "readFile" ]

            VirtualFileSystem.callbacks VirtualFileSystem.Default
            |> Flip.Expect.isEmpty "a filesystem that answers nothing registers nothing"

        // Encoding, checked without a server, because the server's answer to a wrong shape is a
        // panic that kills the process rather than an error frame.
        testList "reply encoding" [
            let reply name fs = (VirtualFileSystem.callbacks fs)[name] "\"C:/x.ts\""

            testCase "readFile distinguishes content, missing and fall-back" <| fun _ ->
                reply "readFile" { VirtualFileSystem.Default with ReadFile = ValueSome(fun _ -> Content "a\"b") }
                |> Flip.Expect.equal "content is a JSON string in an object" """{"content":"a\u0022b"}"""

                reply "readFile" { VirtualFileSystem.Default with ReadFile = ValueSome(fun _ -> Content "") }
                |> Flip.Expect.equal "an empty file is not a missing one" """{"content":""}"""

                reply "readFile" { VirtualFileSystem.Default with ReadFile = ValueSome(fun _ -> Missing) }
                |> Flip.Expect.equal "null means the file does not exist" """{"content":null}"""

                reply "readFile" { VirtualFileSystem.Default with ReadFile = ValueSome(fun _ -> FallBack) }
                |> Flip.Expect.equal "an empty reply means fall back" ""

            testCase "the predicates answer bare booleans, or nothing" <| fun _ ->
                reply "fileExists" { VirtualFileSystem.Default with FileExists = ValueSome(fun _ -> ValueSome true) }
                |> Flip.Expect.equal "" "true"

                reply "directoryExists" { VirtualFileSystem.Default with DirectoryExists = ValueSome(fun _ -> ValueSome false) }
                |> Flip.Expect.equal "" "false"

                reply "fileExists" { VirtualFileSystem.Default with FileExists = ValueSome(fun _ -> ValueNone) }
                |> Flip.Expect.equal "unanswered falls back" ""

            testCase "getAccessibleEntries answers names in two arrays" <| fun _ ->
                let entries = { Files = [| "a.ts" |]; Directories = [| "sub" |] }

                reply "getAccessibleEntries"
                    { VirtualFileSystem.Default with GetAccessibleEntries = ValueSome(fun _ -> ValueSome entries) }
                |> Flip.Expect.equal "" """{"files":["a.ts"],"directories":["sub"]}"""

            testCase "realpath answers a JSON string" <| fun _ ->
                reply "realpath" { VirtualFileSystem.Default with Realpath = ValueSome(fun p -> ValueSome p) }
                |> Flip.Expect.equal "" "\"C:/x.ts\""

            // The only callback whose argument is an object rather than a path, and the only one
            // the server wants no answer from.
            testCase "writeFile takes a path and content, and answers nothing" <| fun _ ->
                let mutable written = ValueNone

                let fs =
                    { VirtualFileSystem.Default with
                        WriteFile = ValueSome(fun path data -> written <- ValueSome(path, data)) }

                (VirtualFileSystem.callbacks fs)["writeFile"] """{"path":"C:/x.ts","data":"hi"}"""
                |> Flip.Expect.equal "no answer" ""

                written |> Flip.Expect.equal "" (ValueSome("C:/x.ts", "hi"))
        ]

        // A throwing callback is answered with a CALL_ERROR frame, which the server treats as
        // unrecoverable - it exits rather than failing just the one request. So the caller gets an
        // exception rather than a hang, and the channel is spent: the next request fails too, and
        // recovering means starting a new one.
        testCase "a callback that raises fails the request and spends the channel" <| fun () ->
            let fs =
                { VirtualFileSystem.Default with
                    ReadFile = ValueSome(fun _ -> failwith "callback blew up") }

            withChannel fs (fun channel ->
                Expect.throws (fun () -> programOf channel |> ignore) "the request fails rather than hanging"

                Expect.throws
                    (fun () -> Api.parseCommandLine channel { CommandLine = ValueSome [| "main.ts" |] } |> ignore)
                    "the server is gone, so the channel is spent") ()
    ]
