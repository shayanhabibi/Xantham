#r "nuget: Partas.Build, 0.3.0"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Str"

open Partas.Build
open Partas.TypeProvider.BuildHelper

[<Literal>]
let __REPOSITORY_DIRECTORY__ = __SOURCE_DIRECTORY__ + "/.."
type Repo = BuildHelperProvider<
    __REPOSITORY_DIRECTORY__,
    capabilityFullOverride = true
>

/// Vendors upstream `microsoft/TypeScript` sources into `tools/tsc-ast/upstream/`:
/// the AST schema and its generators, plus the encoder and reader that define the
/// binary AST format.
///
/// The file set, grouped by upstream directory, and the commit to pull it from live
/// in `upstream.json`; `upstream.lock.json` records a sha256 per file, so resyncing
/// off a moving ref shows up as a diff rather than as mysterious generator output.
///
/// The vendored tree keeps the upstream directory layout: `schema.ts` locates
/// `ast.json` relative to its own dir (`../../..` + `tools/scripts/tsc`), so it
/// only loads if the nesting is preserved.
module TscAst =
    open System
    open System.IO
    open System.Net.Http
    open System.Security.Cryptography
    open System.Text.Json
    open System.Text.Json.Nodes

    let private toolDir = Path.Combine(__SOURCE_DIRECTORY__, "tsc-ast")
    let private manifestFile = Path.Combine(toolDir, "upstream.json")
    let private lockFile = Path.Combine(toolDir, "upstream.lock.json")
    let private vendorRoot = Path.Combine(toolDir, "upstream")

    let private client =
        let c = new HttpClient()
        c.DefaultRequestHeaders.UserAgent.ParseAdd "xantham-tsc-ast-sync"
        c

    let private sha256 (bytes: byte array) =
        Convert.ToHexStringLower(SHA256.HashData bytes)

    let private get (url: string) (accept: string option) = async {
        use request = new HttpRequestMessage(HttpMethod.Get, url)
        accept |> Option.iter request.Headers.Accept.ParseAdd
        let! response = client.SendAsync request |> Async.AwaitTask
        if not response.IsSuccessStatusCode then
            failwith $"{url}: {int response.StatusCode} {response.ReasonPhrase}"
        return! response.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
    }

    /// Resolves a branch/tag to the commit it currently points at, so the lock pins a commit.
    let private resolveRef (repo: string) (ref: string) = async {
        if ref.Length = 40 && ref |> Seq.forall Uri.IsHexDigit then return ref
        else
            let! body =
                get $"https://api.github.com/repos/{repo}/commits/{ref}" (Some "application/vnd.github+json")
            return JsonNode.Parse(body).["sha"].GetValue<string>()
    }

    /// `ref` empty means "use the pin in upstream.json"; `check` reports drift without writing.
    let sync (ref: string) (check: bool) = async {
        let manifest = JsonNode.Parse(File.ReadAllText manifestFile)
        let str (key: string) = manifest[key].GetValue<string>()
        let repo = str "repo"

        // Files come from more than one upstream directory - the schema, the Go encoder that
        // defines the binary format, and the TypeScript reader - so the manifest groups them by
        // directory and the lock keys them by `dir/file`.
        let sources = [
            for source in manifest["sources"].AsArray() ->
                source["dir"].GetValue<string>(),
                [| for file in source["files"].AsArray() -> file.GetValue<string>() |]
        ]

        let! resolved = resolveRef repo (if ref = "" then str "ref" else ref)

        let digests = JsonObject()
        let changed = ResizeArray()
        for dir, files in sources do
            let outDir = Path.Combine(vendorRoot, dir.Replace('/', Path.DirectorySeparatorChar))
            Directory.CreateDirectory outDir |> ignore

            for file in files do
                let path = $"{dir}/{file}"
                let! body = get $"https://raw.githubusercontent.com/{repo}/{resolved}/{path}" None
                let digest = sha256 body
                digests[path] <- JsonValue.Create digest
                let dest = Path.Combine(outDir, file)
                let current = if File.Exists dest then Some(sha256 (File.ReadAllBytes dest)) else None
                let status =
                    if current = Some digest then "  ok"
                    else
                        changed.Add path
                        if check then "diff"
                        else
                            File.WriteAllBytes(dest, body)
                            "wrote"
                printfn $"%s{status}  %s{path}  %.1f{float body.Length / 1024.}kb"

        let fileCount = sources |> List.sumBy (snd >> Array.length)

        let previousRef =
            if File.Exists lockFile then
                JsonNode.Parse(File.ReadAllText lockFile).["ref"].GetValue<string>() |> Some
            else None

        if check then
            return
                if changed.Count = 0 && previousRef = Some resolved then
                    Ok $"up to date at {resolved}"
                else
                    let what = if changed.Count = 0 then "ref moved" else String.Join(", ", changed)
                    Error $"out of date at {resolved}: {what}"
        else
            let lock = JsonObject()
            lock["repo"] <- JsonValue.Create repo
            lock["ref"] <- JsonValue.Create resolved
            lock["fetched"] <- JsonValue.Create(DateTime.UtcNow.ToString "o")
            lock["files"] <- digests
            File.WriteAllText(lockFile, lock.ToJsonString(JsonSerializerOptions(WriteIndented = true)) + "\n")
            let suffix = if changed.Count = 0 then "no changes" else $"{changed.Count} changed"
            return Ok $"vendored {fileCount} files at {resolved} ({suffix})"
    }

module Options =
    /// The repository-level install pinned by the root `package.json`; `build.fsx -- generate`
    /// runs `npm install` for it first.
    ///
    /// Built as a string rather than through `Repo.FileSystem`, which only exposes directories that
    /// exist when the script compiles - `node_modules` does not, on a clean checkout.
    let typescriptPkgDir =
        Input.option<string> "--typescript-pkg"
        |> Input.desc "Path to the typescript package directory"
        |> Input.def (System.IO.Path.GetFullPath(System.IO.Path.Combine(__REPOSITORY_DIRECTORY__, "node_modules", "typescript")))
    let parserDir =
        Input.option<string> "--parser-dir"
        |> Input.def (Repo.FileSystem.ToString())
    let outputDir =
        Input.option<string> "--output"
        |> Input.def (Repo.FileSystem.src.``Xantham.TypeScript.Wire``.ToString())
    let astOutputDir =
        Input.option<string> "--out-dir"
        |> Input.desc "Directory to emit the generated AST bindings into"
        |> Input.def (Repo.FileSystem.src.``Xantham.TypeScript.Wire``.ToString())
    let upstreamRef =
        Input.option<string> "--ref"
        |> Input.desc "Git ref of microsoft/TypeScript to vendor the AST generator sources from. Defaults to the pin in tools/tsc-ast/upstream.json"
        |> Input.def ""
    let checkOnly =
        Input.option<bool> "--check"
        |> Input.desc "Report drift against the vendored sources instead of overwriting them"
        |> Input.def false

let syncTscAst = input {
    let! ref = Options.upstreamRef
    and! check = Options.checkOnly
    return stage "sync tsc-ast" {
        echo "Vendoring microsoft/TypeScript AST generator sources"
        workingDir Repo.FileSystem.``.``
        run (fun _ -> async {
            match! TscAst.sync ref check with
            | Ok summary ->
                printfn ""
                printfn $"%s{summary}"
                return Ok (None: Cmd option)
            | Error problem -> return Error problem
        })
    }
}

/// Emits `Ast.generated.fs`, `AstNode.generated.fs` and `Typed.generated.fs` from the vendored
/// `ast.json`, via the upstream `SchemaAPI`.
let generateAst = input {
    let! outputDir = Options.astOutputDir
    return stage "generate ast" {
        echo "Generating AST bindings"
        workingDir Repo.FileSystem.``.``
        run (cmd $"node tools/tsc-ast/generate-ast.mts {outputDir}")
    }
}

let generateProto = input {
    let! typescriptPkgDir = Options.typescriptPkgDir
    and! parserDir = Options.parserDir
    and! outputDir = Options.outputDir
    return stage "generate proto" {
        echo "Generating proto files"
        workingDir Repo.FileSystem.``.``
        run (cmd $"node tools/proto-gen/generate.mjs {typescriptPkgDir} {parserDir} {outputDir}/Proto.generated.fs")
    }
}

rootCommand fsi.CommandLineArgs[1..] {
    command "sync" {
        command "tsc-ast" {
            syncTscAst
        }
    }
    command "generate" {
        command "proto" {
            generateProto
        }
        command "ast" {
            generateAst
        }
    }
}
