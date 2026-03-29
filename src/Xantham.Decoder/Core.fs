namespace Xantham.Decoder
open Thoth.Json.Net
open Xantham
open System.IO

// Json Typing
type private DuplicateMap = Map<TypeKey, TsAstNode array>
type private TopLevelKeys = TypeKey array
type private GlueNodeMap = Map<TypeKey, TsAstNode>
type private EncodedResult = GlueNodeMap * DuplicateMap * TopLevelKeys

module Diagnostics =
    let healthCheck (encodedResult: TypeMap * NodeMap * TopLevelKeys) =
        let typeMap, nodeStore, _ = encodedResult
        let trueTypeKeys =
            typeMap
            |> Map.fold (fun acc _ -> Utils.getKeys >> (@) acc) []
        let trueTypeMissingKeys =
            typeMap.Keys
            |> Set
            |> Set.difference (Set trueTypeKeys)
        let foundKeysInNodeStore =
            trueTypeMissingKeys
            |> Set.intersect (Set nodeStore.Keys)
        let unemittedKeys =
            Set.difference
                trueTypeMissingKeys
                foundKeysInNodeStore
            |> Set.toList
            |> List.sort
        let trueTypeMissingKeys = trueTypeMissingKeys |> Set.toList |> List.sort
        let foundKeysInNodeStore = foundKeysInNodeStore |> Set.toList |> List.sort
        
        {| MissingTypeKeys = trueTypeMissingKeys; FoundKeysInNodeStore = foundKeysInNodeStore; UnemittedKeys = unemittedKeys |}
        
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
    let private mapAstToType (node: TsAstNode) =
        match node with
        | TsAstNode.Alias node -> TsType.TypeAlias node |> Ok
        | TsAstNode.Tuple node -> TsType.Tuple node |> Ok
        | TsAstNode.Interface node -> TsType.Interface node |> Ok
        | TsAstNode.Variable node -> TsType.Variable node |> Ok
        | TsAstNode.Primitive node -> TsType.Primitive node |> Ok
        | TsAstNode.Predicate node -> TsType.Predicate node |> Ok
        | TsAstNode.Literal node -> TsType.Literal node |> Ok
        | TsAstNode.TypeLiteral node -> TsType.TypeLiteral node |> Ok
        | TsAstNode.TypeParameter node -> TsType.TypeParameter node |> Ok
        | TsAstNode.IndexAccessType node -> TsType.IndexedAccess node |> Ok
        | TsAstNode.FunctionDeclaration node -> TsType.Function node |> Ok
        | TsAstNode.TypeReference node -> TsType.TypeReference node |> Ok
        | TsAstNode.Index node -> TsType.Index node |> Ok
        | TsAstNode.Array node -> node |> TsType.TypeReference |> TsType.Array |> Ok
        | TsAstNode.Class node -> TsType.Class node |> Ok
        | TsAstNode.Conditional node -> TsType.Conditional node |> Ok
        | TsAstNode.Union node -> TsType.Union node |> Ok
        | TsAstNode.Intersection node -> TsType.Intersection node |> Ok
        | TsAstNode.Module node -> TsType.Module node |> Ok
        | TsAstNode.Enum node -> TsType.Enum node |> Ok
        | TsAstNode.Property node -> NodeStore.Property node |> Error
        | TsAstNode.Parameter node -> NodeStore.Parameter node |> Error
        | TsAstNode.Method node -> NodeStore.Method node |> Error
        | TsAstNode.Constructor node -> NodeStore.Constructor node |> Error
        | TsAstNode.EnumCase node -> TsType.EnumCase node |> Ok
        | TsAstNode.SubstitutionType node -> NodeStore.SubstitutionType node |> Error
        | TsAstNode.Optional node -> TsType.Optional node |> Ok
        | TsAstNode.ConstructSignature node -> NodeStore.ConstructSignature node |> Error
        | TsAstNode.IndexSignature node -> NodeStore.IndexSignature node |> Error
        | TsAstNode.GetAccessor node -> NodeStore.GetAccessor node |> Error
        | TsAstNode.SetAccessor node -> NodeStore.SetAccessor node |> Error
        | TsAstNode.CallSignature node -> node |> NodeStore.CallSignature |> Error
        | TsAstNode.GlobalThis -> TsType.GlobalThis |> Ok
        | TsAstNode.TemplateLiteral tsTemplateLiteralType -> TsType.TemplateLiteral tsTemplateLiteralType |> Ok
        
    let read fileName =
        File.ReadAllText(fileName)
        |> Decode.Auto.fromString<EncodedResult>
        |> Result.map(function
            | map, duplicateMap, topLevelKeys ->
                let types, others =
                    map
                    |> Map.map(fun _ -> mapAstToType)
                    |> Map.partition(fun _ -> _.IsOk)
                let types =
                    types
                    |> Map.map(fun _ -> function Ok value -> value | _ -> failwith "Unreachable")
                let others =
                    others
                    |> Map.map(fun _ -> function Error value -> value | _ -> failwith "Unreachable")
                #if DEBUG
                Diagnostics.healthCheck (types, others, topLevelKeys)
                |> Diagnostics.printHealthCheck
                #endif
                {
                    TypeMap = types
                    LibSet = Set [] // todo - reinstate
                    NodeMap = others
                    TopLevelKeys = topLevelKeys |> Array.toList
                }
            )