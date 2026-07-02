module Xantham.Generator.RecipeLoad

// ─────────────────────────────────────────────────────────────────────────────
// .NET-side recipe loading: Tomlyn parses the policy-overlay TOML into the
// SHARED model (src/Xantham.Common/Recipe.Model.fs) — the same shapes the
// encoder's smol-toml loader produces. Untyped access is confined to this file;
// area assignment and partitioned emission consume the typed Recipe only.
// Mirrors src/Xantham.Fable/Recipe.fs decode rules exactly (accumulated errors,
// entries default ["."], crawl default true, total policy parse).
// ─────────────────────────────────────────────────────────────────────────────

open System.IO
open Tomlyn
open Tomlyn.Model
open Xantham

let private asString (o: obj) : string option =
    match o with
    | :? string as s -> Some s
    | _ -> None

let private asBool (o: obj) : bool option =
    match o with
    | :? bool as b -> Some b
    | _ -> None

let private asStringList (o: obj) : string list option =
    match o with
    | :? TomlArray as arr ->
        let items = [ for v in arr -> asString v ]
        if items |> List.forall Option.isSome
        then Some(items |> List.choose id)
        else None
    | _ -> None

let private tryGet (table: TomlTable) (key: string) : obj option =
    match table.TryGetValue key with
    | true, value -> Some value
    | _ -> None

let private decodeEntry (index: int) (table: TomlTable) : Result<RecipeEntry, string> =
    let at = $"[[entry]] #{index}"
    match tryGet table "package" |> Option.bind asString with
    | None -> Error $"{at}: required string field 'package' missing or not a string"
    | Some package ->
    let at = $"{at} ({package})"
    match tryGet table "kind" |> Option.bind asString with
    | None -> Error $"{at}: required string field 'kind' missing or not a string"
    | Some kindText ->
    let root = tryGet table "root" |> Option.bind asString
    let entries = tryGet table "entries" |> Option.bind asStringList
    let crawl = tryGet table "crawl" |> Option.bind asBool |> Option.defaultValue true
    let lib = tryGet table "lib" |> Option.bind asString
    let dependsOn = tryGet table "depends-on" |> Option.bind asStringList |> Option.defaultValue []
    let overlay = tryGet table "overlay" |> Option.bind asString
    let policy =
        match tryGet table "policy" |> Option.bind asString with
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

let private decodeDependencies (table: TomlTable) : Result<DependencyRule list, string list> =
    match tryGet table "dependencies" with
    | Some(:? TomlTable as deps) ->
        let results =
            [ for KeyValue(package, value) in deps ->
                match value with
                | :? TomlTable as rule ->
                    match tryGet rule "policy" |> Option.bind asString with
                    | None -> Error $"[dependencies.\"{package}\"]: required string field 'policy'"
                    | Some text ->
                        let modules = tryGet rule "modules" |> Option.bind asStringList |> Option.defaultValue []
                        DependencyPolicy.parse text
                        |> Result.map (fun policy -> { Package = package; Policy = policy; Modules = modules })
                        |> Result.mapError (fun e -> $"[dependencies.\"{package}\"]: {e}")
                | _ -> Error $"[dependencies.\"{package}\"]: expected a table with a 'policy' field" ]
        match results |> List.choose (function Error e -> Some e | Ok _ -> None) with
        | [] -> Ok(results |> List.choose (function Ok r -> Some r | Error _ -> None))
        | errors -> Error errors
    | _ -> Ok []

let decodeRecipe (tomlText: string) : Result<Recipe, string list> =
    // Tomlyn 2.x: the generic Deserialize<TomlTable> replaces the old Toml.ToModel.
    let table = TomlSerializer.Deserialize<TomlTable>(tomlText, TomlSerializerOptions())
    match tryGet table "entry" with
    | Some(:? TomlTableArray as entries) ->
        let decodedEntries =
            entries
            |> Seq.mapi decodeEntry
            |> Seq.toList
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

let load (recipePath: string) : Result<Recipe, string list> =
    if File.Exists recipePath |> not then Error [ $"recipe file not found: {recipePath}" ]
    else File.ReadAllText recipePath |> decodeRecipe
