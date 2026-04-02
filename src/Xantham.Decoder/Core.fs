namespace Xantham.Decoder
open Thoth.Json.Net
open Xantham
open Xantham.Internal
open System.IO

type TopLevelKeys = TypeKey array

module private Diagnostics =
    let healthCheck (processedResult: {| Types: TypeMap; Exports: ExportArray; TopLevelExports: ExportArray |}) =
        let typeMap = processedResult.Types
        let trueTypeKeys =
            typeMap
            |> Map.fold (fun acc _ -> Utils.getKeys >> (@) acc) []
            |> List.append (
                processedResult.Exports
                |> Seq.append processedResult.TopLevelExports
                |> Seq.map Utils.getKeysFromExport
                |> Seq.collect id
                |> Seq.toList
                )
        let trueTypeMissingKeys =
            typeMap.Keys
            |> Set
            |> Set.difference (Set trueTypeKeys)
        let trueTypeMissingKeys = trueTypeMissingKeys |> Set.toList |> List.sort
        trueTypeMissingKeys
    let inline isHealthy (healthCheck: TypeKey list) =
        healthCheck |> List.isEmpty
    let printHealthCheck (healthCheck: TypeKey list ) =
        if healthCheck |> List.isEmpty then
            [
                "✔️ All TypeKeys are accounted for."
            ]
            |> String.concat "\n"
        else
            let unemittedKeys = {| Keys = healthCheck; Length = healthCheck.Length |}
            [
                $"❌ Missing %i{unemittedKeys.Length} TypeKeys: %A{unemittedKeys.Keys}"
                ""
                "   Generation using this type-set may cause errors."
            ]
            |> String.concat "\n"
        |> System.Console.WriteLine

module Decoder =
    let readWithOptions options =
        let fileName = options.File
        let typeRemap = options.Remap
        let duplicateRemap = options.DuplicateRemap
        File.ReadAllText(fileName)
        |> Decode.Auto.fromString<EncodedResult>
        |> Result.map(Post.Result.processResult >> fun result ->
                #if DEBUG
                Diagnostics.healthCheck result
                |> Diagnostics.printHealthCheck
                #endif
                {
                    TypeMap = result.Types
                    Exports = result.Exports |> Array.append result.TopLevelExports
                    LibSet = Set [] // todo - reinstate
                    PrimaryExports = result.TopLevelExports
                    DuplicateMap = Map.empty
                }
            )
    let read fileName =
        readWithOptions { File = fileName; Remap = ValueNone; DuplicateRemap = ValueNone }