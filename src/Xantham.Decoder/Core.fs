namespace Xantham.Decoder
open Thoth.Json.Net
open Xantham
open System.IO

/// <summary>
/// Diagnostics for the Decoder.
/// </summary>
module private Diagnostics =
    open Schema
    let healthCheck (encodedResult: EncodedResult) =
        let typeMap = encodedResult.Types
        let exportStore = encodedResult.ExportedDeclarations
        let trueTypeKeys =
            typeMap
            |> Map.toArray
            |> Array.Parallel.collect (fun (_, value) -> Utils.getKeys value)
        let trueTypeMissingKeys =
            typeMap.Keys
            |> Set
            |> Set.difference (Set trueTypeKeys)
        let foundKeysInNodeStore =
            trueTypeMissingKeys
            |> Set.intersect (Set exportStore.Keys)
        let unemittedKeys =
            Set.difference
                trueTypeMissingKeys
                foundKeysInNodeStore
            |> Set.toList
            |> List.sort
        let trueTypeMissingKeys = trueTypeMissingKeys |> Set.toList |> List.sort
        let foundKeysInNodeStore = foundKeysInNodeStore |> Set.toList |> List.sort
        
        {| MissingTypeKeys = trueTypeMissingKeys; FoundKeysInNodeStore = foundKeysInNodeStore; UnemittedKeys = unemittedKeys |}
    let inline isHealthy (healthCheck: {| FoundKeysInNodeStore: TypeKey list; MissingTypeKeys: TypeKey list; UnemittedKeys: TypeKey list |}) =
        healthCheck.UnemittedKeys |> List.isEmpty
    let printHealthCheck (healthCheck: {| FoundKeysInNodeStore: TypeKey list; MissingTypeKeys: TypeKey list; UnemittedKeys: TypeKey list |}) =
        let missingTypeKeys = {| Keys = healthCheck.MissingTypeKeys; Length = healthCheck.MissingTypeKeys.Length |}
        let foundKeysInNodeStore = {| Keys = healthCheck.FoundKeysInNodeStore; Length = healthCheck.FoundKeysInNodeStore.Length |}
        if healthCheck.UnemittedKeys |> List.isEmpty then
            [
                "✔️ All TypeKeys are accounted for."
                if missingTypeKeys.Length > 0 then
                    $"   %i{missingTypeKeys.Length} Type Keys are found in the NodeStore: %A{missingTypeKeys.Keys}"
                    ""
                    "   Generation using this type-set should still be safe."
            ]
            |> String.concat "\n"
        else
            let unemittedKeys = {| Keys = healthCheck.UnemittedKeys; Length = healthCheck.UnemittedKeys.Length |}
            [
                $"❌ Missing %i{unemittedKeys.Length} TypeKeys: %A{unemittedKeys.Keys}"
                $"   %i{missingTypeKeys.Length} Type Keys were missing from the TypeStore: %A{missingTypeKeys.Keys}"
                $"   %i{foundKeysInNodeStore.Length} Type Keys are found in the NodeStore: %A{foundKeysInNodeStore.Keys}"
                ""
                "   Generation using this type-set may cause errors."
            ]
            |> String.concat "\n"
        |> System.Console.WriteLine
        

module Decoder =
    /// <summary>
    /// Settings for the Decoder.
    /// </summary>
    type Settings = {
        /// <summary>
        /// The path to the <c>.json</c> file to decode.
        /// </summary>
        InputFile: string
        /// <summary>
        /// Whether to perform a health check on the decoded data and determine
        /// if there are any missing or dangling TypeKeys. Defaults to <c>true</c> in debug builds.
        /// </summary>
        PerformHealthCheck: bool
        /// <summary>
        /// Whether to compress the output TypeMap before passing to consumers.
        /// Defaults to <c>true</c>.
        /// </summary>
        Compress: bool
        /// <summary>
        /// Whether to sanitize the typemap by replacing cyclical keys with obj.
        /// Defaults to <c>true</c>.
        /// </summary>
        Sanitize: bool
    }
    type Settings with
        static member Create(inputFile: string, ?performHealthCheck: bool, ?compress: bool, ?sanitize: bool) =
            let performHealthCheck =
                defaultArg
                    performHealthCheck
                    #if DEBUG
                    true
                    #else
                    false
                    #endif
            let compress = defaultArg compress true
            let sanitize = defaultArg sanitize true
            {
                InputFile = inputFile
                PerformHealthCheck = performHealthCheck
                Compress = compress
                Sanitize = sanitize
            }
    let private getExportSource = function
        | TsExportDeclaration.Variable { Source = source } 
        | TsExportDeclaration.Interface { Source = source } 
        | TsExportDeclaration.TypeAlias { Source = source } 
        | TsExportDeclaration.Class { Source = source } 
        | TsExportDeclaration.Enum { Source = source } 
        | TsExportDeclaration.Module { Source = source } -> source.Value
        | TsExportDeclaration.Function funs -> funs.ValueOrHead.Source.Value
    /// <summary>
    /// Decode a xantham produced <c>.json</c> file with the settings provided.
    /// </summary>
    let readWithSettings (settings: Settings) =
        File.ReadAllText(settings.InputFile)
        |> Decode.fromString Schema.EncodedResult.decode
        // |> Decode.Auto.fromString<Schema.EncodedResult>
        |> Result.map (
            if settings.Compress then Utils.compress else id
            >> if settings.Sanitize then Utils.sanitize else id
            >> fun result ->
                if settings.PerformHealthCheck then Diagnostics.healthCheck result |> Diagnostics.printHealthCheck
                {
                    TypeMap = result.Types
                    ExportTypeMap = result.ExportedDeclarations
                    ExportMap =
                        result.ExportedDeclarations
                        |> Seq.map (fun (KeyValue(key, value)) -> key, value)
                        |> Seq.groupBy (snd >> getExportSource)
                        |> Seq.map (fun (key, value) -> key, value |> Seq.map fst |> Set.ofSeq)
                        |> Map
                    TopLevelExports = result.TopLevelExports |> List.distinct
                    LibEsExports = result.LibEsExports
                }
           )
    /// <summary>
    /// Decode a xantham produced <c>.json</c> file.
    /// This is a proxy function that creates a <c>Settings</c> object using the default values and
    /// the supplied <c>fileName</c> which is then passed to <c>readWithSettings</c>.
    /// </summary>
    let read fileName =
        Settings.Create(fileName)
        |> readWithSettings