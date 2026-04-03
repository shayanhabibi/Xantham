namespace Xantham.Decoder
open Thoth.Json.Net
open Xantham
open System.IO

// Json Typing
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
    let read fileName =
        File.ReadAllText(fileName)
        |> Decode.Auto.fromString<Schema.EncodedResult>
        |> Result.map(Utils.compress >> fun result ->
                #if DEBUG
                Diagnostics.healthCheck result
                |> Diagnostics.printHealthCheck
                #endif
                
                {
                    TypeMap = result.Types
                    ExportTypeMap = result.ExportedDeclarations
                    ExportMap =
                        result.ExportedDeclarations
                        |> Seq.map (fun kv ->
                            match kv.Value with
                            | TsExportDeclaration.Variable { Source = source } 
                            | TsExportDeclaration.Interface { Source = source } 
                            | TsExportDeclaration.TypeAlias { Source = source } 
                            | TsExportDeclaration.Class { Source = source } 
                            | TsExportDeclaration.Enum { Source = source } 
                            | TsExportDeclaration.Module { Source = source } -> source.Value, kv.Value
                            | TsExportDeclaration.Function funs ->
                                funs.ValueOrHead.Source.Value, kv.Value
                            )
                        |> Map
                }
            )