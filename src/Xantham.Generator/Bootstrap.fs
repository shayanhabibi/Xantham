/// Session bootstrap: locate the compiler, start a mailbox over the package directory, create
/// a program over the package's declaration entry, and bind the session the whole run shares.
module Xantham.Generator.Bootstrap

open System.IO
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private manifestOptions =
    JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

let private readManifest (packageDir: string) (read: JsonElement -> 'T option) : 'T option =
    let path = Path.Combine(packageDir, "package.json")

    if not (File.Exists path) then
        None
    else
        use doc = JsonDocument.Parse(File.ReadAllText path, manifestOptions)
        read doc.RootElement

let private stringField (name: string) (el: JsonElement) =
    match el.TryGetProperty name with
    | true, v when v.ValueKind = JsonValueKind.String -> Some(v.GetString())
    | _ -> None

/// The package's declaration entry: `types`/`typings` from the manifest, then the first
/// `types` string anywhere under `exports` (conditions nest arbitrarily, so this walks), then
/// `index.d.ts` as npm's own fallback.
let entryFile (packageDir: string) : string =
    let fromExports (el: JsonElement) =
        match el.TryGetProperty "exports" with
        | true, exports ->
            let rec findTypes (el: JsonElement) =
                match el.ValueKind with
                | JsonValueKind.Object ->
                    match stringField "types" el with
                    | Some t -> Some t
                    | None -> el.EnumerateObject() |> Seq.tryPick (fun p -> findTypes p.Value)
                | _ -> None

            findTypes exports
        | _ -> None

    let declared =
        readManifest packageDir (fun root ->
            stringField "types" root
            |> Option.orElse (stringField "typings" root)
            |> Option.orElse (fromExports root))

    Path.GetFullPath(Path.Combine(packageDir, declared |> Option.defaultValue "index.d.ts"))

/// The manifest's `name`, or the directory name when the manifest lacks one.
let packageName (packageDir: string) : string =
    readManifest packageDir (stringField "name")
    |> Option.defaultValue (Path.GetFileName(Path.TrimEndingDirectorySeparator packageDir))

/// Starts a run's compiler session. The caller owns the returned mailbox and must dispose it -
/// a leaked mailbox leaks a `tsc` process.
let start (config: GeneratorConfig) (packageDir: string) : Async<TscMailbox * Context> =
    async {
        let packageDir = Path.GetFullPath packageDir

        let exe =
            match Tsc.locate packageDir with
            | Some exe -> exe
            | None ->
                failwith
                    $"no TypeScript compiler found above {packageDir} - run `npm install` at the \
                      repository root, or set XANTHAM_TSGO_EXE"

        let entry = entryFile packageDir

        if not (File.Exists entry) then
            failwith $"package at {packageDir} declares no entry - looked for {entry}"

        let mailbox = new TscMailbox(exe, packageDir)

        try
            let! _ = AsyncApi.initialize mailbox

            let compilerOptions =
                match config.Lib with
                | None -> CompilerOptions.Default
                | Some lib ->
                    { CompilerOptions.Default with
                        Lib = ValueSome(List.toArray lib)
                    }

            let! program =
                mailbox.createProgram (
                    { CreateProgramOptions.Default with
                        CompilerOptions = compilerOptions
                    },
                    rootFiles = [| DocumentIdentifier.FileName entry |]
                )

            return
                mailbox,
                {
                    Session = mailbox.Session program
                    Config = config
                    PackageDir = packageDir
                    PackageName = packageName packageDir
                    EntryFile = entry
                }
        with e ->
            (mailbox :> System.IDisposable).Dispose()
            return raise e
    }
