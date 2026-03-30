module Main

open System.Collections.Generic
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Node.Api
open Fable.Mocha
open Fable.Core.Testing

let createTestReader (fileName: string) =
    let filePath = path.join(__SOURCE_DIRECTORY__, $"/TypeFiles/{fileName}.d.ts")
    let reader = TypeScriptReader.create filePath
    reader

let runReader (reader: TypeScriptReader) =
    let exports =
        Internal.initialise reader
        |> Internal.getAndPrepareExports
    Internal.runReader reader
    |> Internal.assembleResults
    |> Internal.exciseDuplicateKeys
    |> fun split ->
        let splitMap = Dictionary<TypeKey, TsAstNode * TsAstNode array>()
        let orderedDuplicates = split.DuplicateGroups |> Internal.sortResultGroups
        let mergedDuplicates =
            orderedDuplicates
            |> Internal.mergeOverloads
            |> Internal.mergeMembersIntoWinner
        mergedDuplicates
        |> Internal.filterConflictDuplicatesOnly
        |> Seq.iter (fun group -> splitMap.Add(group.Key, (group.Winner.Node, group.Losers |> Array.map _.Node)) )
        
        
        Internal.resolveDuplicates (Seq.map Internal.prune mergedDuplicates)
        |> Seq.append split.NonDuplicates
        |> Seq.sortBy _.Key
        |> Seq.map (fun ir -> KeyValuePair(ir.Key, ir.Node))
        |> Dictionary

let tests =
    testList "Tests" [
        testCase "Test members emitted" <| fun _ ->
            let result =
                createTestReader "basic"
                |> runReader
            "Should contain interfaces with members"
            |> Expect.exists result (fun kv ->
                match kv.Value with
                | TsAstNode.Interface { Members = members } when members.Length > 0 -> true
                | _ -> false
                )
            
        testCase "Have a alias and a interface" <| fun _ ->
            let result =
                createTestReader "basic"
                |> runReader
                |> Seq.filter (fun kv -> kv.Value.IsInterface || kv.Value.IsAlias)
                |> Seq.length
            ""
            |> Expect.equal result 2
        testCase "Merge members for interfaces" <| fun _ ->
            let result =
                createTestReader "merge"
                |> runReader
                |> Seq.find _.Value.IsInterface
                |> _.Value
                |> function
                    | TsAstNode.Interface interfaceType -> interfaceType
                    | _ -> failwith "Should be an interface"
            ""
            |> Expect.hasLength result.Members 2
        testCase "Two interfaces total 2 props" <| fun _ ->
            let result =
                createTestReader "merge"
                |> runReader
                |> fun data ->
                    Internal.writeOutput (path.join(__SOURCE_DIRECTORY__, "output.json")) [||] (Dictionary()) data
                    data
                |> Seq.filter _.Value.IsInterface
                |> Seq.toArray
            ""
            |> Expect.hasLength result 2
        testCase "Extending interface" <| fun _ ->
            let reader =
                createTestReader "extends"
            let result = runReader reader
            result
            |> Seq.pick (function
                | KeyValue(_, TsAstNode.Interface { Heritage = { Extends = [ extend ] } }) ->
                    Some extend.Type
                | _ -> None
                )
            |> fun typeKey ->
                "Contains extended key"
                |> Expect.exists result (_.Key >> (=) typeKey)
        testCase "Multiple extensions of the same interface" <| fun _ ->
            let reader =
                createTestReader "multiple-extends"
            let result = runReader reader
            result
            |> Seq.choose (function
                | KeyValue(_, TsAstNode.Interface { Heritage = { Extends = [ extend ] } }) -> Some extend.Type
                | _ -> None
                )
            |> Seq.toArray
            |> fun typeKeys ->
                let resultKeys =
                    result.Keys
                    |> Seq.toArray
                "Contains all extended keys"
                |> Expect.containsAll resultKeys typeKeys
        
    ]

Mocha.runTests tests |> ignore