// Builds the Fable packages Xantham compiles against from the commit pinned in
// tools/fable-fork/fork.json.
//
//   dotnet fsi tools/fable-fork.fsx -- sync [--source <fable checkout>] [--force] [--rebuild-libraries]
//   dotnet fsi tools/fable-fork.fsx -- check
//
// `sync` packs Fable.Core and Fable (the CLI) into tools/fable-fork/packages, to push to the
// Xantham feed NuGet.config lists. Without `--source` it clones the pinned commit into
// tools/fable-fork/upstream. `check` fails when a pin in the repository names a version other
// than fork.json's.

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Text.Json
open System.Text.RegularExpressions

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let forkDir = Path.Combine(root, "tools", "fable-fork")
let feed = Path.Combine(forkDir, "packages")

type Fork =
    {
        Repo: string
        Ref: string
        Version: string
        Packages: string list
    }

let fork =
    use document =
        JsonDocument.Parse(File.ReadAllText(Path.Combine(forkDir, "fork.json")))

    let value (name: string) =
        document.RootElement.GetProperty(name).GetString()

    {
        Repo = value "repo"
        Ref = value "ref"
        Version = value "version"
        Packages =
            [
                for package in document.RootElement.GetProperty("packages").EnumerateArray() -> package.GetString()
            ]
    }

/// The package ids `sync` writes, one per project in `fork.packages`.
let packageIds = [ "Fable.Core"; "Fable" ]

let nupkg (id: string) = $"{id}.{fork.Version}.nupkg"

let fail (message: string) =
    eprintfn $"fable-fork: %s{message}"
    exit 1

let run (workingDir: string) (file: string) (args: string) =
    printfn $"> %s{file} %s{args}"

    let info =
        ProcessStartInfo(file, args, WorkingDirectory = workingDir, UseShellExecute = false)

    use proc = Process.Start info
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        fail $"`%s{file} %s{args}` exited with %d{proc.ExitCode} in %s{workingDir}"

let output (workingDir: string) (file: string) (args: string) =
    let info =
        ProcessStartInfo(
            file,
            args,
            WorkingDirectory = workingDir,
            UseShellExecute = false,
            RedirectStandardOutput = true
        )

    use proc = Process.Start info
    let text = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        fail $"`%s{file} %s{args}` exited with %d{proc.ExitCode} in %s{workingDir}"

    text.Trim()

/// `npm` is `npm.cmd` on Windows, which `Process.Start` resolves only by its full name.
let npm = if OperatingSystem.IsWindows() then "npm.cmd" else "npm"

let feedComplete (directory: string) =
    packageIds
    |> List.forall (fun id -> File.Exists(Path.Combine(directory, nupkg id)))

/// The directories whose sources reach the packed packages. `sync` refuses a checkout with local
/// changes under any of them, because the packages would then differ from the pinned commit.
let packagedSources =
    [
        "src/Fable.AST"
        "src/Fable.Core"
        "src/Fable.Cli"
        "src/Fable.Compiler"
        "src/Fable.Transforms"
        "src/fable-library-ts"
    ]

let clone () =
    let upstream = Path.Combine(forkDir, "upstream")

    if not (Directory.Exists(Path.Combine(upstream, ".git"))) then
        run forkDir "git" $"clone --filter=blob:none --no-checkout {fork.Repo} upstream"

    run upstream "git" $"fetch --quiet origin {fork.Ref}"
    run upstream "git" $"-c advice.detachedHead=false checkout --quiet --detach {fork.Ref}"
    upstream

let verifySource (source: string) =
    let head = output source "git" "rev-parse HEAD"

    if head <> fork.Ref then
        fail $"%s{source} is at %s{head}, not the pinned %s{fork.Ref}."

    let paths = String.concat " " packagedSources
    let changes = output source "git" $"status --porcelain -- {paths}"

    if changes <> "" then
        fail $"%s{source} has local changes under the packaged sources:\n%s{changes}"

/// Builds the JavaScript and TypeScript fable-library copies `Fable.Cli` packs from `temp/`. The
/// other targets' libraries are left out: the bindings compile to JavaScript only, and those
/// builds need their own toolchains.
let buildLibraries (source: string) (rebuild: bool) =
    let built name =
        File.Exists(Path.Combine(source, "temp", name, "package.json"))

    if rebuild || not (built "fable-library-js" && built "fable-library-ts") then
        if not (Directory.Exists(Path.Combine(source, "node_modules"))) then
            run source npm "ci --no-audit --no-fund"

        run source "dotnet" "run --project src/Fable.Build -- fable-library --typescript"
        run source "dotnet" "run --project src/Fable.Build -- fable-library --javascript"

let pack (source: string) =
    Directory.CreateDirectory feed |> ignore

    for project in fork.Packages do
        run
            source
            "dotnet"
            // `Version` stamps every assembly, `Fable.AST` included, with the fork version. Fable
            // plugins reference `Fable.AST` 5.x and fail to load against a lower assembly version.
            // EasyBuild's own stamping is disabled because it also overrides `PackageVersion`.
            $"pack {project} -c Release -p:EasyBuildPackageReleaseNotes_DisableSetVersion=true -p:Version={fork.Version} -p:PackageVersion={fork.Version} -o \"{feed}\" -v q -nologo"

/// Fails unless the packed `Fable.AST` and `Fable.Core` assemblies carry the fork version.
let verifyAssemblyVersions () =
    let expected = Version(fork.Version.Split('-')[0])
    let scratch = Path.Combine(feed, ".verify")

    let entries =
        [
            nupkg "Fable", "/Fable.AST.dll"
            nupkg "Fable.Core", "lib/netstandard2.0/Fable.Core.dll"
        ]

    try
        for package, suffix in entries do
            use archive = ZipFile.OpenRead(Path.Combine(feed, package))

            match archive.Entries |> Seq.tryFind (fun entry -> entry.FullName.EndsWith suffix) with
            | None -> fail $"%s{package} carries no %s{suffix}"
            | Some entry ->
                Directory.CreateDirectory scratch |> ignore
                let file = Path.Combine(scratch, entry.Name)
                entry.ExtractToFile(file, true)
                let actual = Reflection.AssemblyName.GetAssemblyName(file).Version

                if Version(actual.Major, actual.Minor, actual.Build) <> expected then
                    fail $"%s{package}: %s{entry.FullName} is stamped %O{actual}, not %O{expected}"
    finally
        if Directory.Exists scratch then
            Directory.Delete(scratch, true)

let sync (args: string list) =
    let rec parse source force libraries =
        function
        | "--source" :: dir :: rest -> parse (Some(Path.GetFullPath dir)) force libraries rest
        | "--force" :: rest -> parse source true libraries rest
        | "--rebuild-libraries" :: rest -> parse source force true rest
        | [] -> source, force, libraries
        | other :: _ -> fail $"unknown argument %s{other}"

    let source, force, libraries = parse None false false args

    if not force && feedComplete feed then
        printfn $"fable-fork: {fork.Version} is already in %s{feed}"
    else
        let source = source |> Option.defaultWith clone
        verifySource source
        buildLibraries source libraries
        pack source

        if not (feedComplete feed) then
            fail $"packing finished without %s{nupkg packageIds[0]} and %s{nupkg packageIds[1]} in %s{feed}"

        verifyAssemblyVersions ()
        printfn $"fable-fork: packed {fork.Version} from %s{source}"

/// Every file that pins a Fable package, with the pattern capturing the version it pins.
let pins =
    let fableCore = """Include=["']Fable\.Core["'] Version=["']([^"']+)["']"""

    [
        ".config/dotnet-tools.json", """"fable":\s*\{\s*"version":\s*"([^"]+)"""
        "src/Xantham.Fable.Core/Xantham.Fable.Core.fsproj", fableCore
        "src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj", fableCore
        "src/Xantham.Fable.Node/Xantham.Fable.Node.fsproj", fableCore
        "tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj", fableCore
        "tests/Xantham.Generator.CompilerLibCompileGate/Xantham.Generator.CompilerLibCompileGate.fsproj", fableCore
        "tests/Xantham.Generator.RunGate/Xantham.Generator.RunGate.fsproj", fableCore
        "tests/repros/support-helper-globals/SupportHelperGlobals.fsproj", fableCore
        "tests/Xantham.Generator.PartasGate/verify.mjs", fableCore
        "tools/catalog-portability.fsx", fableCore
    ]

let check () =
    let wrong =
        [
            for file, pattern in pins do
                let text = File.ReadAllText(Path.Combine(root, file))

                let found =
                    Regex.Matches(text, pattern)
                    |> Seq.map (fun m -> m.Groups[1].Value)
                    |> List.ofSeq

                if List.isEmpty found then
                    yield file + ": no Fable pin found"
                else
                    for version in found do
                        if version <> fork.Version then
                            yield file + ": pins " + version
        ]

    if List.isEmpty wrong then
        printfn $"fable-fork: every pin names {fork.Version}"
    else
        let listed = String.concat "\n  " wrong
        fail $"pins differ from tools/fable-fork/fork.json (%s{fork.Version}):\n  %s{listed}"

match fsi.CommandLineArgs |> Array.toList |> List.skip 1 |> List.skipWhile ((=) "--") with
| "sync" :: rest -> sync rest
| [ "check" ] -> check ()
| _ ->
    fail
        "usage: dotnet fsi tools/fable-fork.fsx -- sync [--source <fable checkout>] [--force] [--rebuild-libraries] | check"
