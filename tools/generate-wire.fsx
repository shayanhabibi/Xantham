#r "nuget: Partas.Build, 0.3.0"
#r "nuget: Partas.TypeProvider.BuildHelper, 0.2.5"
#r "nuget: Str"

#load "workspace.fsx"

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
    ///
    /// In an agent worktree the install lives in the main checkout instead; `Workspace` resolves
    /// whichever of the two is actually present, nearest first.
    let typescriptPkgDir =
        Input.option<string> "--typescript-pkg"
        |> Input.desc "Path to the typescript package directory"
        |> Input.def (Workspace.typescriptPackage __REPOSITORY_DIRECTORY__)
    /// Where `generate.mjs` resolves its `typescript` parser from. That has to be a TypeScript
    /// 5.x install - the 7.x package exposes only `version` to `require` - and it is found by
    /// Node's usual walk up from this directory, so it must be a checkout that has one.
    let parserDir =
        Input.option<string> "--parser-dir"
        |> Input.def (Workspace.nodeModulesRoot __REPOSITORY_DIRECTORY__)
    let outputDir =
        Input.option<string> "--output"
        |> Input.def (Repo.FileSystem.src.``Xantham.TypeScript.Wire``.ToString())
    let astOutputDir =
        Input.option<string> "--out-dir"
        |> Input.desc "Directory to emit the generated AST bindings into"
        |> Input.def (Repo.FileSystem.src.``Xantham.TypeScript.Wire``.ToString())
    /// Where the pinned compiler's `lib.*.d.ts` files are; `browser-gen` intersects them with
    /// what the `Fable.Browser.*` family exports.
    let libDir =
        Input.option<string> "--lib-dir"
        |> Input.desc "Directory holding the compiler's lib.*.d.ts files"
        |> Input.def (Workspace.tscLibDir __REPOSITORY_DIRECTORY__)
    /// The generator project, which is where the browser table lands - not the wire project the
    /// other layers are emitted into.
    let generatorOutputDir =
        Input.option<string> "--generator-output"
        |> Input.def (Repo.FileSystem.src.``Xantham.Generator``.ToString())
    let compileGateDir =
        Input.option<string> "--compile-gate"
        |> Input.def (Repo.FileSystem.tests.``Xantham.Generator.CompileGate``.ToString())
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

/// `tools/tsc-ast/upstream/` is gitignored, so it is missing from a fresh clone and from every
/// agent worktree until someone vendors it. Say so with the command that fixes it, rather than
/// letting `generate-ast.mts` fail on a module it cannot resolve. The vendoring hits the
/// network, so it stays an explicit step and is never run implicitly here.
let requireUpstream = input {
    return stage "check upstream" {
        quiet
        run (fun _ -> async {
            let vendored = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "tsc-ast", "upstream")

            if System.IO.Directory.Exists vendored then
                return Ok (None: Cmd option)
            else
                return
                    Error
                        $"no vendored upstream sources at {vendored} - run `dotnet fsi tools/generate-wire.fsx -- sync tsc-ast` first"
        })
    }
}

/// Emits `Ast.generated.fs`, `AstNode.generated.fs` and `Typed.generated.fs` from the vendored
/// `ast.json`, via the upstream `SchemaAPI`.
let generateAst = input {
    let! outputDir = Options.astOutputDir
    Workspace.ensureTsc __REPOSITORY_DIRECTORY__ |> ignore
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
    Workspace.ensureTsc __REPOSITORY_DIRECTORY__ |> ignore
    return stage "generate proto" {
        echo "Generating proto files"
        workingDir Repo.FileSystem.``.``
        run (cmd $"node tools/proto-gen/generate.mjs {typescriptPkgDir} {parserDir} {outputDir}/Proto.generated.fs")
    }
}

/// Emits `Session.generated.fs`: `Session<'T>`, which binds the snapshot and project that most
/// of the wire's methods repeat, and re-exposes them without it.
///
/// A separate generator from `generateProto`, reading the same schema, because the pair it binds
/// is a property of the schema rather than a promise it makes. If the compiler ever splits or
/// renames it, the proto layers stay correct and this one shrinks - which is only true while
/// nothing here can reach into the emitter that produces them.
let generateSession = input {
    let! typescriptPkgDir = Options.typescriptPkgDir
    and! parserDir = Options.parserDir
    and! outputDir = Options.outputDir
    Workspace.ensureTsc __REPOSITORY_DIRECTORY__ |> ignore
    return stage "generate session" {
        echo "Generating session layer"
        workingDir Repo.FileSystem.``.``
        run (cmd $"node tools/session-gen/generate.mjs {typescriptPkgDir} {parserDir} {outputDir}/Session.generated.fs")
    }
}

/// Emits `BrowserBindingTable.generated.fs` into the generator, and the compile-gate file that
/// proves every entry of it resolves.
///
/// The odd one out among these stages: it emits into `src/Xantham.Generator` rather than the
/// wire, and its input is a NuGet family rather than the vendored compiler sources. It lives
/// here anyway because "regenerate a generated file" is one command in this repository, and a
/// second entry point for one table would be the surprising choice.
let generateBrowser = input {
    let! libDir = Options.libDir
    and! outputDir = Options.generatorOutputDir
    and! gateDir = Options.compileGateDir
    Workspace.ensureTsc __REPOSITORY_DIRECTORY__ |> ignore
    return stage "generate browser" {
        echo "Generating Fable.Browser binding table"
        workingDir Repo.FileSystem.``.``
        run (cmd $"dotnet fsi tools/browser-gen/generate.fsx {libDir} {outputDir}/BrowserBindingTable.generated.fs {gateDir}/BrowserBindings.fs")
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
            requireUpstream
            generateAst
        }
        command "session" {
            generateSession
        }
        command "browser" {
            generateBrowser
        }
    }
}
