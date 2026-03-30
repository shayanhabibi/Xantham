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
        testCase "Test 1" <| fun _ ->
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
            
    ]

Mocha.runTests tests |> ignore