module Xantham.Decoder.Runtime

open System
open System.Collections.Frozen
open System.Collections.Generic
open Xantham
open Xantham.Decoder
open Xantham.Decoder.Types.Graph

module Map =
    let inline choose (fn: 'Key -> 'Item -> 'Outcome option) (map: Map<'Key, 'Item>) = Map.fold (fun acc key item -> match fn key item with Some value -> Map.add key value acc | None -> acc) (Map []) map

/// <summary>
/// Runtime configuration for a <see cref="T:XanthamTree"/>.
/// Wraps the underlying <c>Decoder.Settings</c> and may be extended with
/// runtime-only options in the future.
/// </summary>
/// <category index="6">Runtime</category>
type Settings = {
    /// Decoder-layer settings controlling how the input JSON is read.
    Decoder: Decoder.Settings
}

type Settings with
    /// <summary>Create default <c>Settings</c> for the given input file path.</summary>
    static member Init filePath = {
        Decoder = Decoder.Settings.Create filePath
    }
    /// <summary>
    /// Create <c>Settings</c> for the given input file and apply a
    /// transformation function to customise the defaults.
    /// </summary>
    /// <param name="fn">Transform applied to the default settings.</param>
    /// <param name="fileName">Path to the input JSON file.</param>
    static member Create fn fileName : Settings =
        Settings.Init(fileName) |> fn


/// <summary>
/// In-memory representation of a decoded Xantham JSON file, optimised for
/// generator access. Holds frozen lookups for types, exports, top-level
/// declarations, and provides lazy access to the resolved object graph
/// (<c>ArenaInterner</c>) and a dependency graph.
/// </summary>
/// <param name="settings">Runtime settings carrying the input file path and decoder options.</param>
/// <category index="6">Runtime</category>
type XanthamTree(settings: Settings) =
    let fileName = settings.Decoder.InputFile
    let decodedResult =
        match Decoder.read fileName with
        | Ok result -> result
        | Error error -> failwith error
    let typeMap = decodedResult.TypeMap.ToFrozenDictionary()
    let keyExportMap = decodedResult.ExportTypeMap.ToFrozenDictionary()
    let exportMap =
        decodedResult.ExportMap
        |> Map.map (fun _ value ->
            value
            |> Seq.map (fun export -> keyExportMap[export])
            |> Seq.toArray
            )
        |> _.ToFrozenDictionary()
    let functionExportMap =
        keyExportMap
        |> Seq.toArray
        |> Array.filter _.Value.IsFunction
        |> Array.Parallel.collect (fun (KeyValue(_, value)) ->
            match value with
            | TsExportDeclaration.Function functionExport ->
                functionExport.ToArray()
                |> Array.map (fun innerFunction -> KeyValuePair(innerFunction.SignatureKey, functionExport))
            | _ -> failwith "Unreachable"
            )
        |> _.ToFrozenDictionary()
    let libSet = decodedResult.LibEsExports.ToFrozenSet()
    let tryFindMatchExport (key: TypeKey) =
        match keyExportMap.TryGetValue(key) with
        | true, value -> ValueSome value
        | false, _ -> ValueNone
    let tryFindMatchType (key: TypeKey) =
        match typeMap.TryGetValue(key) with
        | true, value -> ValueSome value
        | false, _ -> ValueNone
    let findMatchExport key = tryFindMatchExport key |> ValueOption.get
    let findMatchType key = tryFindMatchType key |> ValueOption.get
    let tryFindMatchExportOrType key =
        tryFindMatchExport key
        |> ValueOption.map Ok
        |> ValueOption.orElseWith(fun () -> findMatchType key |> Error |> ValueSome)
    let findMatchExportOrType key = tryFindMatchExportOrType key |> ValueOption.get
    let topLevelKeys =
        decodedResult.TopLevelExports
        |> List.map (fun key -> KeyValuePair(key, findMatchExportOrType key))
        |> _.ToFrozenDictionary()
    let isLibEsExport (key: TypeKey) = libSet.Contains(key)
    
    let arenaInterner = lazy (
        let interner = ArenaInterner.ArenaInterner.create decodedResult
        for exportKey in keyExportMap.Keys do
            interner.ResolveExport exportKey
            |> ignore
        interner
    )
    
    /// <summary>Frozen lookup of all decoded structural types, keyed by <c>TypeKey</c>.</summary>
    member this.TypeMap = typeMap
    /// <summary>Frozen lookup from source module path to the array of exports declared in that module.</summary>
    member this.ExportMap = exportMap
    /// <summary>Frozen lookup of all exported declarations, keyed by their <c>TypeKey</c>.</summary>
    member this.KeyExportMap = keyExportMap
    /// <summary>Frozen lookup of declarations exported at the top level of the input.
    /// Each entry is either an <c>Ok export</c> or an <c>Error structural-type</c>.</summary>
    [<Obsolete("Better to extract all the exports from the ExportMap with the module name of your source.")>]
    member this.TopLevelExports = topLevelKeys
    /// <summary>Frozen lookup from a function signature key to the (possibly overloaded) function export it belongs to.</summary>
    member this.SignatureFunctionMap = functionExportMap
    /// <summary>Set of <c>TypeKey</c>s belonging to the TypeScript <c>lib.es*</c> standard library.</summary>
    member this.LibEsExports = libSet
    /// <summary>Predicate: is the given <c>TypeKey</c> a TypeScript <c>lib.es*</c> export?</summary>
    member this.IsLibEsExport = isLibEsExport
    /// <summary>
    /// Tag a <c>TypeKey</c> with the kind of construct it identifies, returning a
    /// <c>CodeKey</c> for ergonomic dispatch on type-vs-export kinds without
    /// re-walking the underlying <c>TsType</c>/<c>TsExportDeclaration</c>.
    /// </summary>
    /// <param name="key">The <c>TypeKey</c> to classify.</param>
    member this.Codify (key: TypeKey) =
        match findMatchExportOrType key with
        | Ok value ->
            match value with
            | TsExportDeclaration.Variable _ -> CodeKey.ExportCodeKey.Variable key
            | TsExportDeclaration.Interface _ -> CodeKey.ExportCodeKey.Interface  key
            | TsExportDeclaration.TypeAlias _ -> CodeKey.ExportCodeKey.TypeAlias  key
            | TsExportDeclaration.Class _ -> CodeKey.ExportCodeKey.Class  key
            | TsExportDeclaration.Enum _ -> CodeKey.ExportCodeKey.Enum  key
            | TsExportDeclaration.Module _ -> CodeKey.ExportCodeKey.Module  key
            | TsExportDeclaration.Function _ -> CodeKey.ExportCodeKey.Function  key
            |> CodeKey.CodeKey.Export
        | Error value ->
            match value with
            | TsType.GlobalThis -> CodeKey.TypeCodeKey.GlobalThis |> CodeKey.CodeKey.Type
            | TsType.Array _ -> CodeKey.TypeCodeKey.Array key |> CodeKey.CodeKey.Type
            | TsType.Conditional _ -> CodeKey.TypeCodeKey.Conditional key |> CodeKey.CodeKey.Type
            | TsType.Primitive _ -> CodeKey.TypeCodeKey.Primitive key |> CodeKey.CodeKey.Type
            | TsType.EnumCase _ -> CodeKey.TypeCodeKey.EnumCase key |> CodeKey.CodeKey.Type
            | TsType.Union _ -> CodeKey.TypeCodeKey.Union key |> CodeKey.CodeKey.Type
            | TsType.Intersection _ -> CodeKey.TypeCodeKey.Intersection key |> CodeKey.CodeKey.Type
            | TsType.Literal _ -> CodeKey.TypeCodeKey.Literal key |> CodeKey.CodeKey.Type
            | TsType.TypeReference _ -> CodeKey.TypeCodeKey.TypeReference key |> CodeKey.CodeKey.Type
            | TsType.TypeParameter _ -> CodeKey.TypeCodeKey.TypeParameter key |> CodeKey.CodeKey.Type
            | TsType.ReadOnly _ -> CodeKey.TypeCodeKey.ReadOnly key |> CodeKey.CodeKey.Type
            | TsType.Tuple _ -> CodeKey.TypeCodeKey.Tuple key |> CodeKey.CodeKey.Type
            | TsType.Index _ -> CodeKey.TypeCodeKey.Index key |> CodeKey.CodeKey.Type
            | TsType.Predicate _ -> CodeKey.TypeCodeKey.Predicate key |> CodeKey.CodeKey.Type
            | TsType.TypeLiteral _ -> CodeKey.TypeCodeKey.TypeLiteral key |> CodeKey.CodeKey.Type
            | TsType.TemplateLiteral _ -> CodeKey.TypeCodeKey.TemplateLiteral key |> CodeKey.CodeKey.Type
            | TsType.Optional _ -> CodeKey.TypeCodeKey.Optional key |> CodeKey.CodeKey.Type
            | TsType.Substitution _ -> CodeKey.TypeCodeKey.Substitution key |> CodeKey.CodeKey.Type
            | TsType.IndexedAccess _ -> CodeKey.TypeCodeKey.IndexAccess key |> CodeKey.CodeKey.Type
            | TsType.Interface _ -> CodeKey.ExportCodeKey.Interface  key |> CodeKey.CodeKey.Export
            | TsType.Class _ -> CodeKey.ExportCodeKey.Class  key |> CodeKey.CodeKey.Export
            | TsType.Enum _ -> CodeKey.ExportCodeKey.Enum  key |> CodeKey.CodeKey.Export
    /// <summary>
    /// Build a dependency graph over the decoded type/export maps.
    /// Conditional <c>Check</c>/<c>Extends</c> branches are excluded from the edges.
    /// </summary>
    member this.GetDependencyGraph() = Graph.create false decodedResult
    /// <summary>
    /// Lazily resolve the underlying decoded result into an
    /// <see cref="T:Xantham.Decoder.ArenaInterner.ArenaInterner"/> object graph.
    /// The first call materialises the interner; subsequent calls return the cached value.
    /// </summary>
    member this.GetArenaInterner() = arenaInterner.Value
    /// <summary>
    /// Default constructor for a XanthamTree.
    /// </summary>
    new (fileName: string) =
        XanthamTree(Settings.Init(fileName))

/// <summary>
/// Create a <see cref="T:XanthamTree"/> from the given Xantham JSON file path
/// using default decoder settings.
/// </summary>
/// <param name="fileName">Path to the input JSON file.</param>
let create (fileName: string) = XanthamTree(fileName)

/// <summary>
/// Create a <see cref="T:XanthamTree"/> from the given Xantham JSON file path
/// with a transformation function applied to the default <see cref="T:Settings"/>.
/// </summary>
/// <param name="fn">Transform applied to the default settings.</param>
/// <param name="fileName">Path to the input JSON file.</param>
let createWith (fn: Settings -> Settings) (fileName: string) = XanthamTree(Settings.Create fn fileName)