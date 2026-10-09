module internal Xantham.Generator.CatalogCompiler

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Xantham.TypeScript.Wire
open Xantham.Generator.CatalogCompatibility

let private fail executable message =
    failwith $"declaration catalog: compiler toolchain {executable}: {message}"

let probeVersionWithTimeout timeoutMs (executable: string) =
    async {
        let! cancellation = Async.CancellationToken
        use timeout = new CancellationTokenSource(timeoutMs: int)

        use linked =
            CancellationTokenSource.CreateLinkedTokenSource(cancellation, timeout.Token)

        let start =
            ProcessStartInfo(
                executable,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true
            )

        start.ArgumentList.Add "--version"
        use child = new Process(StartInfo = start)

        if not (child.Start()) then
            fail executable "version probe could not start"

        let stdout = child.StandardOutput.ReadToEndAsync()
        let stderr = child.StandardError.ReadToEndAsync()

        try
            try
                do! child.WaitForExitAsync(linked.Token) |> Async.AwaitTask
            with :? OperationCanceledException when
                timeout.IsCancellationRequested && not cancellation.IsCancellationRequested ->
                fail executable $"version probe timed out after {timeoutMs}ms"

            let! output = stdout |> Async.AwaitTask
            let! error = stderr |> Async.AwaitTask

            if child.ExitCode <> 0 then
                let error =
                    if error.Length > 512 then
                        error.Substring(0, 512)
                    else
                        error

                fail executable $"version probe exit code {child.ExitCode}: {error.Trim()}"

            return output
        finally
            if not child.HasExited then
                child.Kill(entireProcessTree = true)
                child.WaitForExit()
    }

let rec physicalPath (path: string) =
    let path = Path.GetFullPath path

    if path = Path.GetPathRoot path then
        path
    else
        let info: FileSystemInfo =
            if Directory.Exists path then
                DirectoryInfo path
            else
                FileInfo path

        let target = info.ResolveLinkTarget true

        if not (isNull target) then
            target.FullName
        else
            let parent = Path.GetDirectoryName path
            Path.Combine(physicalPath parent, Path.GetFileName path)

let private platforms =
    set
        [
            "typescript-win32-x64"
            "typescript-win32-arm64"
            "typescript-linux-x64"
            "typescript-linux-arm"
            "typescript-linux-arm64"
            "typescript-darwin-x64"
            "typescript-darwin-arm64"
        ]

let isLibraryPackage (name: string) =
    name = "typescript"
    || (name.StartsWith("@typescript/", StringComparison.Ordinal)
        && Set.contains (name.Substring "@typescript/".Length) platforms)

let discoverWith (probe: string -> Async<string>) (resolve: string -> string) (executable: string) =
    async {
        let selected = Path.GetFullPath executable
        let executable = resolve selected

        let packageAt path =
            let lib = DirectoryInfo(Path.GetDirectoryName(path: string))
            let package = lib.Parent

            if
                not (isNull package)
                && lib.Name = "lib"
                && Set.contains package.Name platforms
                && not (isNull package.Parent)
                && package.Parent.Name = "@typescript"
                && not (isNull package.Parent.Parent)
                && package.Parent.Parent.Name = "node_modules"
                && Path.GetFileName path = (if package.Name.Contains "-win32-" then "tsc.exe" else "tsc")
            then
                Some package
            else
                None

        let logicalPackage =
            packageAt selected
            |> Option.filter (fun package ->
                let expected =
                    Path.Combine(resolve package.FullName, "lib", Path.GetFileName selected)

                let comparison =
                    if OperatingSystem.IsWindows() then
                        StringComparison.OrdinalIgnoreCase
                    else
                        StringComparison.Ordinal

                String.Equals(expected, executable, comparison))

        let pairing =
            [ packageAt executable; logicalPackage ]
            |> List.choose id
            |> List.tryPick (fun package ->
                let platformManifest = Path.Combine(resolve package.FullName, "package.json")

                let wrapperManifest =
                    Path.Combine(package.Parent.Parent.FullName, "typescript", "package.json")

                if File.Exists platformManifest && File.Exists wrapperManifest then
                    Some(package, platformManifest, wrapperManifest)
                else
                    None)

        let binary () = Binary Ast.ProtocolVersion

        match pairing with
        | None -> return binary ()
        | Some(package, platformManifest, wrapperManifest) ->
            let readManifest path =
                try
                    JsonDocument.Parse(File.ReadAllText path)
                with :? JsonException ->
                    fail executable $"invalid package metadata: {path}"

            use platform = readManifest platformManifest
            use wrapper = readManifest (resolve wrapperManifest)

            let text name (root: JsonElement) =
                match root.TryGetProperty(name: string) with
                | false, _ -> None
                | true, value when value.ValueKind = JsonValueKind.String ->
                    let text = value.GetString()

                    if String.IsNullOrWhiteSpace text || text <> text.Trim() then
                        fail executable $"invalid package metadata {name}"

                    Some text
                | _ -> fail executable $"invalid package metadata {name}"

            let metadata expectedName root =
                match text "name" root with
                | Some actual when actual <> expectedName ->
                    fail executable $"package name mismatch: expected {expectedName}, actual {actual}"
                | _ -> ()

                let revision = text "gitHead" root

                revision
                |> Option.iter (fun value ->
                    if not (validRevision value) then
                        fail executable "invalid gitHead; expected full hexadecimal revision")

                match text "name" root, text "version" root, revision with
                | Some _, Some version, Some revision -> Some(version, revision.ToLowerInvariant())
                | _ -> None

            let platformName = "@typescript/" + package.Name

            match metadata platformName platform.RootElement, metadata "typescript" wrapper.RootElement with
            | Some(platformVersion, platformRevision), Some(wrapperVersion, wrapperRevision) ->
                if platformVersion <> wrapperVersion then
                    fail executable $"package version mismatch: platform {platformVersion}, wrapper {wrapperVersion}"

                if platformRevision <> wrapperRevision then
                    fail executable $"package revision mismatch: platform {platformRevision}, wrapper {wrapperRevision}"

                match wrapper.RootElement.TryGetProperty "optionalDependencies" with
                | true, dependencies when dependencies.ValueKind = JsonValueKind.Object ->
                    match text platformName dependencies with
                    | Some version when version = platformVersion -> ()
                    | _ -> fail executable $"wrapper dependency {platformName} must select {platformVersion}"
                | _ -> fail executable $"wrapper dependency {platformName} is missing"

                let! output = probe executable

                let lines =
                    output.Trim().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

                if lines <> [| "Version " + platformVersion |] then
                    fail executable $"executable version mismatch: expected Version {platformVersion}"

                return TypeScriptPackage(platformVersion, platformRevision, Ast.ProtocolVersion)
            | _ -> return binary ()
    }

let discover executable =
    discoverWith (probeVersionWithTimeout 5000) physicalPath executable
