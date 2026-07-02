module Xantham.Fable.Recipe

// ─────────────────────────────────────────────────────────────────────────────
// Recipe loading: the policy-overlay TOML (e.g. cloudflare.pilot.toml) parsed
// at the interop boundary into a closed typed model, then resolved to the
// concrete entry files the reader crawls. Untyped access is confined to
// `decodeRecipe`; everything downstream is total over the model.
//
// Loader scope (Phase 0): [[entry]] only — package, kind, root/entries, crawl.
// Emission-policy keys (lib, policy, builders, depends-on, ...) are consumed by
// later stages, not the crawler.
// ─────────────────────────────────────────────────────────────────────────────

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Node.Api
open TypeScript
open Xantham
// Node.Api/TypeScript expose the JS `Error` class, shadowing FSharp.Core's
// Result case constructor in expression position — restore Core's precedence.
open Microsoft.FSharp.Core

/// A resolved crawl root: the recipe coordinate it came from and the file on disk.
type ResolvedEntry = {
    /// "<package>:<subpath-or-root>" — stable, human-readable provenance label.
    Label: string
    File: string
}

[<Import("parse", "smol-toml")>]
let private tomlParse: string -> obj = jsNative

// ── boundary decode (the only untyped region) ────────────────────────────────

let private asString (o: obj) : string option =
    if jsTypeof o = "string" then Some(unbox o) else None
let private asBool (o: obj) : bool option =
    if jsTypeof o = "boolean" then Some(unbox o) else None
let private asStringList (o: obj) : string list option =
    if JS.Constructors.Array.isArray o then
        let items = (unbox<obj array> o) |> Array.map asString
        if items |> Array.forall Option.isSome
        then Some(items |> Array.choose id |> Array.toList)
        else None
    else None

let private decodeEntry (index: int) (o: obj) : Result<RecipeEntry, string> =
    let at = $"[[entry]] #{index}"
    match asString o?package with
    | None -> Error $"{at}: required string field 'package' missing or not a string"
    | Some package ->
    let at = $"{at} ({package})"
    match asString o?kind with
    | None -> Error $"{at}: required string field 'kind' missing or not a string"
    | Some kindText ->
    let root = o?root |> asString
    let entries = o?entries |> asStringList
    let crawl = o?crawl |> asBool |> Option.defaultValue true
    let lib = o?lib |> asString
    let dependsOn = o.["depends-on"] |> asStringList |> Option.defaultValue []
    let overlay = o?overlay |> asString
    let policy =
        match o?policy |> asString with
        | None -> Ok None
        | Some text -> DependencyPolicy.parse text |> Result.map Some |> Result.mapError (fun e -> $"{at}: {e}")
    match policy with
    | Error e -> Error e
    | Ok policy ->
    match kindText with
    | "ambient-root" ->
        match root with
        | None -> Error $"{at}: kind 'ambient-root' requires string field 'root'"
        | Some _ -> Ok { Package = package; Kind = AmbientRoot; Root = root; Entries = []; Crawl = crawl; Lib = lib; Policy = policy; DependsOn = dependsOn; Overlay = overlay }
    | "module" ->
        Ok { Package = package; Kind = Module; Root = None; Entries = entries |> Option.defaultValue [ "." ]; Crawl = crawl; Lib = lib; Policy = policy; DependsOn = dependsOn; Overlay = overlay }
    | other -> Error $"{at}: unknown kind '{other}' (expected 'module' or 'ambient-root')"

let private decodeDependencies (table: obj) : Result<DependencyRule list, string list> =
    match table?dependencies with
    | deps when not (isNull deps) && jsTypeof deps = "object" ->
        let results =
            JS.Constructors.Object.keys deps
            |> Seq.toList
            |> List.map (fun package ->
                match deps?(package)?policy |> asString with
                | None -> Error $"[dependencies.\"{package}\"]: required string field 'policy'"
                | Some text ->
                    DependencyPolicy.parse text
                    |> Result.map (fun policy -> { Package = package; Policy = policy })
                    |> Result.mapError (fun e -> $"[dependencies.\"{package}\"]: {e}"))
        match results |> List.choose (function Error e -> Some e | Ok _ -> None) with
        | [] -> Ok(results |> List.choose (function Ok r -> Some r | Error _ -> None))
        | errors -> Error errors
    | _ -> Ok []

let decodeRecipe (tomlText: string) : Result<Recipe, string list> =
    let table = tomlParse tomlText
    match table?entry with
    | entries when JS.Constructors.Array.isArray entries ->
        let decodedEntries =
            (unbox<obj array> entries)
            |> Array.mapi decodeEntry
            |> Array.toList
        let entryErrors = decodedEntries |> List.choose (function Error e -> Some e | Ok _ -> None)
        match decodeDependencies table with
        | Error depErrors -> Error(entryErrors @ depErrors)
        | Ok dependencies when entryErrors.IsEmpty ->
            Ok {
                Entries = decodedEntries |> List.choose (function Ok e -> Some e | Error _ -> None)
                Dependencies = dependencies
            }
        | Ok _ -> Error entryErrors
    | _ -> Error [ "recipe has no [[entry]] tables" ]

// ── resolution (typed, total over the model) ─────────────────────────────────

let private resolutionOptions = jsOptions<Ts.CompilerOptions>(fun c ->
    // Bundler resolution honors exports maps and resolves to the types file —
    // the same mode the reader's program uses (Types/Reader.fs).
    c.moduleResolution <- Some Ts.ModuleResolutionKind.Bundler
    c.target <- Some Ts.ScriptTarget.Latest)

// ts.sys structurally satisfies ModuleResolutionHost (fileExists/readFile/...);
// the binding does not declare the inheritance, so the cast is at this boundary only.
let private resolutionHost: Ts.ModuleResolutionHost = !!ts.sys

let private moduleName (package: string) (subpath: string) =
    if subpath = "." then package
    elif subpath.StartsWith "./" then package + subpath.Substring 1
    else package + "/" + subpath

let private resolveOne (recipeDir: string) (entry: RecipeEntry) (subpathOrRoot: string) : Result<ResolvedEntry, string> =
    let label = $"{entry.Package}:{subpathOrRoot}"
    match entry.Kind with
    | AmbientRoot ->
        let file = path.join(recipeDir, "node_modules", entry.Package, subpathOrRoot)
        if fs.existsSync !^file
        then Ok { Label = label; File = file }
        else Error $"{label}: root file not found at {file}"
    | Module ->
        let containingFile = path.join(recipeDir, "__xantham_resolve__.ts")
        let resolution = ts.resolveModuleName(moduleName entry.Package subpathOrRoot, containingFile, resolutionOptions, resolutionHost)
        match resolution.resolvedModule with
        | Some resolved -> Ok { Label = label; File = resolved.resolvedFileName }
        | None -> Error $"{label}: module resolution failed (from {containingFile})"

/// Resolves every crawlable [[entry]] to concrete files, accumulating ALL errors.
let resolveEntryFiles (recipeDir: string) (recipe: Recipe) : Result<ResolvedEntry list, string list> =
    let results =
        recipe.Entries
        |> List.filter _.Crawl
        |> List.collect (fun entry ->
            match entry.Kind with
            | AmbientRoot -> [ resolveOne recipeDir entry entry.Root.Value ]
            | Module -> entry.Entries |> List.map (resolveOne recipeDir entry))
    match results |> List.choose (function Error e -> Some e | Ok _ -> None) with
    | [] -> Ok(results |> List.choose (function Ok r -> Some r | Error _ -> None))
    | errors -> Error errors

/// Reads, decodes, and resolves a recipe file in one step.
let load (recipePath: string) : Result<ResolvedEntry list, string list> =
    if fs.existsSync !^recipePath |> not then Error [ $"recipe file not found: {recipePath}" ]
    else
        // Absolute: ts.resolveModuleName requires an absolute containing file, and
        // the reader normalizes entry paths — relative roots break both.
        let recipeDir = path.resolve (path.dirname recipePath)
        fs.readFileSync(recipePath, "utf8")
        |> decodeRecipe
        |> Result.bind (resolveEntryFiles recipeDir)
