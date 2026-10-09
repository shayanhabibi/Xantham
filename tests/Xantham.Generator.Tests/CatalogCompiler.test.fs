module CatalogCompilerTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open Expecto
open Xantham.Generator
open Xantham.Generator.CatalogCompatibility
open Xantham.Generator.CatalogCompiler
open Xantham.Generator.Tests
open Xantham.TypeScript.Wire

let private version = "7.1.0-dev.20260902.1"
let private revision = String.replicate 40 "a"

let private install root =
    let platformRoot = Path.Combine(root, "node_modules", "@typescript", "typescript-win32-x64")
    let wrapper = Path.Combine(root, "node_modules", "typescript", "package.json")
    let platform = Path.Combine(platformRoot, "package.json")
    let executable = Path.Combine(platformRoot, "lib", "tsc.exe")
    Directory.CreateDirectory(Path.GetDirectoryName wrapper) |> ignore
    Directory.CreateDirectory(Path.GetDirectoryName executable) |> ignore
    File.WriteAllText(executable, "compiler")
    File.WriteAllText(platform, """{"name":"@typescript/typescript-win32-x64","version":"7.1.0-dev.20260902.1","gitHead":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}""")
    File.WriteAllText(wrapper, """{"name":"typescript","version":"7.1.0-dev.20260902.1","gitHead":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","optionalDependencies":{"@typescript/typescript-win32-x64":"7.1.0-dev.20260902.1"}}""")
    executable, platform, wrapper

let private edit path (key: string) (value: string) =
    let node = JsonNode.Parse(File.ReadAllText path)
    node[key] <- JsonNode.Parse value
    File.WriteAllText(path, node.ToJsonString())

let private probe _ = async.Return("Version " + version)

let private rejects part action =
    Expect.throwsC action (fun error -> Expect.stringContains error.Message part "toolchain diagnostic identifies failure")

[<Tests>]
let tests =
    testList "catalog compiler" [
        testCase "installed executable supplies portable release and revision" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, _ = install scratch.Path
            let identity = discoverWith probe id executable |> Async.RunSynchronously
            Expect.equal identity (TypeScriptPackage(version, revision, Ast.ProtocolVersion)) "installed compiler identity"

        testCase "copied executable cannot borrow nearby wrapper identity" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            install scratch.Path |> ignore
            let executable = Path.Combine(scratch.Path, "tsc.exe")
            File.WriteAllText(executable, "custom compiler")
            let unexpectedProbe _ = async { return failwith "binary discovery must not probe" }
            let identity = discoverWith unexpectedProbe id executable |> Async.RunSynchronously
            Expect.equal identity (Binary Ast.ProtocolVersion) "custom compiler stays binary"

        testCase "physical executable identity wins over caller path" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, _ = install scratch.Path
            let linked = Path.Combine(scratch.Path, "linked", "tsc.exe")
            let resolve path = if path = linked then executable else path
            let identity = discoverWith probe resolve linked |> Async.RunSynchronously
            Expect.equal identity (TypeScriptPackage(version, revision, Ast.ProtocolVersion)) "resolved package is authenticated"

        testCase "physical path retains filesystem root" <| fun _ ->
            let root = Path.GetPathRoot(Path.GetFullPath __SOURCE_DIRECTORY__)
            Expect.equal (physicalPath root) root "root terminates ancestor resolution"

        testCase "missing package metadata falls back to exact binary" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, platform, _ = install scratch.Path
            File.Delete platform
            Expect.equal (discoverWith probe id executable |> Async.RunSynchronously) (Binary Ast.ProtocolVersion) "missing provenance"

        testCase "missing wrapper falls back to exact binary" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, wrapper = install scratch.Path
            File.Delete wrapper
            Expect.equal (discoverWith probe id executable |> Async.RunSynchronously) (Binary Ast.ProtocolVersion) "missing wrapper provenance"

        testCase "missing revision falls back to exact binary" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, platform, _ = install scratch.Path
            let node = JsonNode.Parse(File.ReadAllText platform)
            node.AsObject().Remove "gitHead" |> ignore
            File.WriteAllText(platform, node.ToJsonString())
            Expect.equal (discoverWith probe id executable |> Async.RunSynchronously) (Binary Ast.ProtocolVersion) "missing compiler revision"

        for field, value, part in [
            "version", "\"7.2.0\"", "version"
            "gitHead", "\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"", "revision"
            "gitHead", "\"bad\"", "gitHead"
            "name", "\"typescript\"", "package name"
        ] do
            testCase ("rejects contradictory platform " + field + value) <| fun _ ->
                use scratch = Scratch.directory "catalog-compiler"
                let executable, platform, _ = install scratch.Path
                edit platform field value
                rejects part (fun () -> discoverWith probe id executable |> Async.RunSynchronously |> ignore)

        testCase "wrapper must declare the selected platform release" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, wrapper = install scratch.Path
            edit wrapper "optionalDependencies" "{\"@typescript/typescript-win32-x64\":\"7.2.0\"}"
            rejects "dependency" (fun () -> discoverWith probe id executable |> Async.RunSynchronously |> ignore)

        testCase "wrapper package name must identify TypeScript" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, wrapper = install scratch.Path
            edit wrapper "name" "\"other\""
            rejects "package name" (fun () -> discoverWith probe id executable |> Async.RunSynchronously |> ignore)

        testCase "invalid executable location remains binary" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, _, _ = install scratch.Path
            let copied = Path.Combine(Path.GetDirectoryName executable, "other.exe")
            File.WriteAllText(copied, "compiler")
            Expect.equal (discoverWith probe id copied |> Async.RunSynchronously) (Binary Ast.ProtocolVersion) "layout cannot identify renamed custom binary"

        for output in [ "Version 7.2.0"; ""; "Version " + version + "\nVersion 7.2.0"; version ] do
            testCase ("rejects contradictory executable version " + output) <| fun _ ->
                use scratch = Scratch.directory "catalog-compiler"
                let executable, _, _ = install scratch.Path
                rejects "version" (fun () -> discoverWith (fun _ -> async.Return output) id executable |> Async.RunSynchronously |> ignore)

        testCase "revision case is normalized" <| fun _ ->
            use scratch = Scratch.directory "catalog-compiler"
            let executable, platform, _ = install scratch.Path
            edit platform "gitHead" "\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\""
            Expect.equal (discoverWith probe id executable |> Async.RunSynchronously) (TypeScriptPackage(version, revision, Ast.ProtocolVersion)) "hexadecimal case is portable"

        testCase "real bootstrap preserves executable selected for the session" <| fun _ ->
            let package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "brand-lab"))
            let selected = Tsc.locate package |> Option.defaultWith (fun () -> failwith "compiler required")
            let mailbox, ctx = Bootstrap.start GeneratorConfig.Default package |> Async.RunSynchronously
            use _ = mailbox :> IDisposable
            Expect.equal (Bootstrap.compilerPath ctx) selected "session executable is captured"
            match discover selected |> Async.RunSynchronously with
            | TypeScriptPackage(actualVersion, actualRevision, protocol) ->
                Expect.equal actualVersion version "live pinned release"
                Expect.equal actualRevision "43a90f4c105bc9db7cb7aa299beddafbabe1d23e" "live pinned revision"
                Expect.equal protocol 8u "live AST protocol"
            | Binary _ -> failtest "the pinned installed compiler must have portable identity"

        testCase "unregistered context cannot infer a session compiler" <| fun _ ->
            rejects "session" (fun () -> Bootstrap.compilerPath Build.context |> ignore)

        testCase "version probe drains pipes and terminates timed out process" <| fun _ ->
            use scratch = Scratch.directory "catalog-probe"
            let root = scratch.Path
            File.WriteAllText(Path.Combine(root, "Directory.Build.props"), "<Project />")
            File.WriteAllText(Path.Combine(root, "Directory.Build.targets"), "<Project />")
            File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "global.json"), Path.Combine(root, "global.json"))
            File.WriteAllText(Path.Combine(root, "Probe.csproj"), """<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework></PropertyGroup></Project>""")
            File.WriteAllText(Path.Combine(root, "Program.cs"), """using System; using System.IO; using System.Threading;
            class Program { static int Main(string[] args) {
            File.WriteAllText(Path.Combine(AppContext.BaseDirectory, "pid.txt"), Environment.ProcessId.ToString());
            var mode = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "mode.txt"));
            if (mode == "sleep") { Thread.Sleep(30000); return 0; }
            Console.Error.Write(new string('x', 100000));
            Console.WriteLine("Version 7.1.0-dev.20260902.1");
            return mode == "fail" ? 1 : 0;
            } }""")
            let start = ProcessStartInfo("dotnet", WorkingDirectory = root, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, CreateNoWindow = true)
            for argument in [ "build"; "Probe.csproj"; "--disable-build-servers"; "-v"; "q" ] do start.ArgumentList.Add argument
            use child = Process.Start start
            let stdout = child.StandardOutput.ReadToEndAsync()
            let stderr = child.StandardError.ReadToEndAsync()
            child.WaitForExit()
            Expect.equal child.ExitCode 0 (stdout.Result + stderr.Result)
            let bin = Path.Combine(root, "bin", "Debug", "net10.0")
            let executable = Path.Combine(bin, if OperatingSystem.IsWindows() then "Probe.exe" else "Probe")
            File.WriteAllText(Path.Combine(bin, "mode.txt"), "success")
            let output = probeVersionWithTimeout 5000 executable |> Async.RunSynchronously
            Expect.equal (output.Trim()) ("Version " + version) "stdout survives large stderr"
            File.WriteAllText(Path.Combine(bin, "mode.txt"), "fail")
            rejects "exit" (fun () -> probeVersionWithTimeout 5000 executable |> Async.RunSynchronously |> ignore)
            File.WriteAllText(Path.Combine(bin, "mode.txt"), "sleep")
            rejects "timed out" (fun () -> probeVersionWithTimeout 1000 executable |> Async.RunSynchronously |> ignore)
            let pid = File.ReadAllText(Path.Combine(bin, "pid.txt")) |> int
            let exited =
                try
                    use running = Process.GetProcessById pid
                    running.HasExited
                with :? ArgumentException -> true
            Expect.isTrue exited "timed-out probe has been reaped"
    ]
