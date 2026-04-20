module Xantham.Decoder.Runtime

open System
open System.Collections.Frozen
open System.Collections.Generic
open Xantham
open Xantham.Decoder
open Xantham.Decoder.Types.Graph

module Map =
    let inline choose (fn: 'Key -> 'Item -> 'Outcome option) (map: Map<'Key, 'Item>) = Map.fold (fun acc key item -> match fn key item with Some value -> Map.add key value acc | None -> acc) (Map []) map

type Settings = {
    Decoder: Decoder.Settings
}

type Settings with
    static member Init filePath = {
        Decoder = Decoder.Settings.Create filePath
    }
    static member Create fn fileName : Settings =
        Settings.Init(fileName) |> fn
        

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
    
    member this.TypeMap = typeMap
    member this.ExportMap = exportMap
    member this.KeyExportMap = keyExportMap
    [<Obsolete("Better to extract all the exports from the ExportMap with the module name of your source.")>]
    member this.TopLevelExports = topLevelKeys
    member this.SignatureFunctionMap = functionExportMap
    member this.LibEsExports = libSet
    member this.IsLibEsExport = isLibEsExport
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
    member this.GetDependencyGraph() = Graph.create false decodedResult
    member this.GetArenaInterner() = arenaInterner.Value
    /// <summary>
    /// Default constructor for a XanthamTree.
    /// </summary>
    new (fileName: string) =
        XanthamTree(Settings.Init(fileName))

/// Creates a utility type with common handling operations and optimized access
/// using FrozenDictionaries and FrozenSets for the result of a decoded xantham json file.
let create (fileName: string) = XanthamTree(fileName)
/// Creates a utility type with common handling operations and optimized access
/// using FrozenDictionaries and FrozenSets for the result of a decoded xantham json file.
let createWith (fn: Settings -> Settings) (fileName: string) = XanthamTree(Settings.Create fn fileName)