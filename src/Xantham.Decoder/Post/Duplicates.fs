module Xantham.Decoder.Post.Duplicates

open System
open Xantham
open Xantham.Decoder
open Xantham.Internal

let private identityPriority (identity: TsIdentityKey) =
    match identity with
    | TsIdentityKey.Declaration _ -> 0
    | TsIdentityKey.Symbol _ -> 1
    | TsIdentityKey.Transient _ -> 2
    
let private referenceRemap (duplicates: EncodedDuplicateResult) =
    match duplicates.Results with
    | [| { Node = TsAstNode.TypeReference _ } 
         { Node = (TsAstNode.Array _ | TsAstNode.Tuple _) } as winner |]
    | [| { Node = (TsAstNode.Array _ | TsAstNode.Tuple _) } as winner
         { Node = TsAstNode.TypeReference _ } |] -> winner |> Ok
    | duplicates -> Error duplicates

let private firstPassMergeFunctions (duplicates: EncodedDuplicateResult) =
    let funcs, nonFuncs =
        duplicates.Results
        |> Array.partition _.Node.IsFunctionDeclaration
    let funcsMerged =
        funcs
        |> Array.map (_.Node >> function
            | TsAstNode.FunctionDeclaration node -> node
            | _ -> failwith "Expected only function declarations"
            )
        |> TsOverloadableConstruct.Create
    nonFuncs
    |> Seq.sortBy (_.Id >> identityPriority)
    |> Seq.toArray
    |> fun result ->
        { duplicates with Duplicates = result },
        TsExportDeclaration.Function funcsMerged
        |> ValueSome
        // { funcs[0] with Node = TsAstNode.FunctionDeclaration funcsMerged }
    
let private secondPassMergeMembers (duplicates: EncodedDuplicateResult) =
    let results = duplicates.Results
    let winner = results[0]
    let inline merger
        (predicateForAcceptableNodes: TsAstNode -> bool)
        (mapToMembers: TsAstNode -> TsMember list)
        (reconstitution: 'T -> TsMember list -> TsAstNode)
        (value: 'T when 'T:(member Members: TsMember list)) =
        let mergeableMembers, unmergeableMembers =
            results[1..]
            |> Array.partition (_.Node >> predicateForAcceptableNodes)
            ||> (fun mergeable unmergeable ->
                mergeable
                |> Array.map (_.Node >> mapToMembers)
                , unmergeable)
        mergeableMembers
        |> Array.fold (fun acc members ->
            Set.union acc (Set members)
            ) (Set value.Members)
        |> Set.toList
        |> fun members ->
            [|
                { winner with Node = reconstitution value members }
                yield! unmergeableMembers
            |]
        |> fun nodes ->
            { duplicates with Duplicates = nodes }
    match winner.Node with
    | TsAstNode.TypeLiteral tsLiteral when results.Length > 1 ->
        tsLiteral
        |> merger
            _.IsTypeLiteral
            (function
                | TsAstNode.TypeLiteral { Members = members } -> members
                | _ -> [])
            (fun tsLiteral members -> TsAstNode.TypeLiteral { tsLiteral with Members = members })
    | TsAstNode.Interface tsInterface when results.Length > 1 ->
        tsInterface
        |> merger
            _.IsInterface
            (function
                | TsAstNode.Interface { Members = members } -> members
                | _ -> [])
            (fun tsInterface members -> TsAstNode.Interface { tsInterface with Members = members })
    | TsAstNode.Class tsClass when results.Length > 1 ->
        tsClass
        |> merger
            _.IsClass
            (function
                | TsAstNode.Class { Members = members } -> members
                | _ -> [])
            (fun tsClass members -> TsAstNode.Class { tsClass with Members = members })
    | _ -> duplicates
        

type MergeResult =
    | ExportOnly of key: TypeKey * node: TsExportDeclaration
    | Both of key: TypeKey * node: TsAstNode * export: TsExportDeclaration
    | NodeOnly of key: TypeKey * node: TsAstNode


let private mergeDuplicates (duplicates: EncodedDuplicateResult) =
    if
        duplicates.Results
        |> Array.exists _.Node.IsFunctionDeclaration
    then
        firstPassMergeFunctions duplicates
    else duplicates, ValueNone
    ||> fun duplicates fn ->
        match duplicates with
        | { Duplicates =[||] } as duplicates -> duplicates, fn
        | _ -> secondPassMergeMembers duplicates, fn

let private flattenDuplicate (duplicates: EncodedDuplicateResult) =
    if duplicates.Results.Length > 0 then
        duplicates.Key, ValueSome duplicates.Results[0].Node
    else duplicates.Key, ValueNone

let resolveDuplicates (duplicates: EncodedDuplicateResult array) =
    duplicates
    |> Seq.map (
        mergeDuplicates
        >> fun (duplicates, fn) ->
            match flattenDuplicate duplicates, fn with
            | (key, ValueSome node), ValueSome export ->
                MergeResult.Both(key, node, export)
            | (key, ValueNone), ValueSome export ->
                MergeResult.ExportOnly(key, export)
            | (key, ValueSome node), ValueNone ->
                MergeResult.NodeOnly(key, node)
            | _ -> failwith "Should not reduce duplicates to a null case"
        )
    |> Seq.toArray
    