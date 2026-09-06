/// Session bootstrap: locate the compiler, start a mailbox over the package directory, create
/// a program over the package's declaration entry, and bind the session the whole run shares.
module Xantham.Generator.Bootstrap

open System
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

/// The default root declaration: `types`, `typings`, a root-export `types` string, then
/// `index.d.ts`. A missing or blocked root in an exports map requires an explicit input.
let entryFile (packageDir: string) : string =
    let fromExports (el: JsonElement) =
        let noRoot () =
            failwith
                $"package at {packageDir} exports no root entry - set \"entry\" to a TypeScript file \
                  and \"runtime\" to its public import in xantham.json"

        let rootExport (exports: JsonElement) =
            match exports.ValueKind with
            | JsonValueKind.Object ->
                match exports.TryGetProperty "." with
                | true, root when root.ValueKind = JsonValueKind.Null -> noRoot ()
                | true, root -> root
                | _ ->
                    let fields = exports.EnumerateObject() |> Seq.toArray

                    if
                        Array.isEmpty fields
                        || Array.exists (fun (p: JsonProperty) -> p.Name.StartsWith '.') fields
                    then
                        noRoot ()

                    exports
            | _ -> exports

        match el.TryGetProperty "exports" with
        | true, exports when exports.ValueKind <> JsonValueKind.Null ->
            let root = rootExport exports

            let rec findTypes (el: JsonElement) =
                match el.ValueKind with
                | JsonValueKind.Object ->
                    match stringField "types" el with
                    | Some t -> Some t
                    | None ->
                        el.EnumerateObject()
                        |> Seq.filter (fun p -> not (p.Name.StartsWith '.'))
                        |> Seq.tryPick (fun p -> findTypes p.Value)
                | _ -> None

            findTypes root
        | _ -> None

    let declared =
        readManifest packageDir (fun root ->
            let rootTypes = fromExports root

            stringField "types" root
            |> Option.orElse (stringField "typings" root)
            |> Option.orElse rootTypes)

    Path.GetFullPath(Path.Combine(packageDir, declared |> Option.defaultValue "index.d.ts"))

/// The existing input file selected for a run. Explicit inputs are relative to the package
/// directory; runtime imports and F# module names are configured independently.
let resolveEntryFile (config: GeneratorConfig) (packageDir: string) : string =
    let packageDir = Path.GetFullPath packageDir

    let entry =
        match config.Entry with
        | None -> entryFile packageDir
        | Some selected ->
            if String.IsNullOrWhiteSpace selected then
                failwith "xantham.json: entry must be a nonempty relative path"

            if Path.IsPathRooted selected then
                failwith "xantham.json: entry must be a relative path within the package directory"

            let path = Path.GetFullPath(Path.Combine(packageDir, selected))
            let relative = Path.GetRelativePath(packageDir, path)

            if
                relative = ".."
                || relative.StartsWith(".." + string Path.DirectorySeparatorChar)
            then
                failwith "xantham.json: entry must stay within the package directory"

            if
                not (
                    [ ".ts"; ".tsx"; ".mts"; ".cts" ]
                    |> List.exists (fun suffix -> path.EndsWith(suffix, StringComparison.Ordinal))
                )
            then
                failwith "xantham.json: entry must name a TypeScript file (.ts, .tsx, .mts or .cts)"

            path

    if not (File.Exists entry) then
        match config.Entry with
        | Some _ -> failwith $"xantham.json: entry file does not exist: {entry}"
        | None -> failwith $"{packageDir} declares no TypeScript entry - looked for {entry}"

    entry

/// The manifest's `name`, or the directory name when the manifest lacks one.
let packageName (packageDir: string) : string =
    readManifest packageDir (stringField "name")
    |> Option.defaultValue (Path.GetFileName(Path.TrimEndingDirectorySeparator packageDir))

/// Starts a run's compiler session. The caller owns the returned mailbox and must dispose it -
/// a leaked mailbox leaks a `tsc` process.
let start (config: GeneratorConfig) (packageDir: string) : Async<TscMailbox * Context> =
    async {
        let packageDir = Path.GetFullPath packageDir
        let entry = resolveEntryFile config packageDir

        let exe =
            match Tsc.locate packageDir with
            | Some exe -> exe
            | None ->
                failwith
                    $"no TypeScript compiler found above {packageDir} - run `npm install` at the \
                      repository root, or set XANTHAM_TSGO_EXE"

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
