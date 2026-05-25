module Xantham.TypeScript.Tests.Structural

open System.Collections.Generic
open Xantham.Mocha
open Xantham.Fable
open TypeScript

testSuite "LM - Lookup Map" <| fun _ ->
    testSuite "LM1 - Each DU case has a constructor value" <| fun _ ->
        let inline makeArrTestCase (name: string) (arr: (_ * (obj -> 'T)) array) =
            testCase name <| fun _ ->
                let reflectedNames =
                    FSharp.Reflection.FSharpType.GetUnionCases typeof<'T>
                    |> Array.map _.Name
                let getCaseName = Fable.Core.Reflection.getCaseName
                arr
                |> Seq.map (snd >> funApply {||} >> getCaseName)
                |> Flip.Expect.containsAll reflectedNames $"Not every {name} has a constructor"
                    
        let inline makeDictTestCase (name: string) (map: Dictionary<_, obj -> 'T>) =
            testCase name <| fun _ ->
                let reflectedNames =
                    FSharp.Reflection.FSharpType.GetUnionCases typeof<'T>
                    |> Array.map _.Name
                let getCaseName = Fable.Core.Reflection.getCaseName
                map.Values
                |> Seq.map (funApply {||} >> getCaseName)
                |> Flip.Expect.containsAll reflectedNames $"Not every {name} has a constructor"
        makeDictTestCase "DeclarationFileNodes" Internal.declarationFileNodes
        makeDictTestCase "MemberDeclaration" Internal.memberDeclarationKindSetMap
        makeDictTestCase "TypeDeclaration" Internal.typeDeclarationKindSetMap
        makeDictTestCase "TopLevelStatements" Internal.topLevelStatements
        makeDictTestCase "TopLevelExportSymbolDeclarations" Internal.topLevelExportDeclarations
        makeDictTestCase "TopLevelLocalSymbolDeclarations" Internal.topLevelLocalDeclarations
        makeDictTestCase "TypeNodeKind" Internal.typeNodeKindSetMap
        makeDictTestCase "JSDocKind" Internal.jsDocKindSetMap
        makeDictTestCase "ModuleExport" Internal.moduleExportSetMap
        makeDictTestCase "Modifier" Internal.modifierSetMap
        makeDictTestCase "LiteralTokenNode" Internal.literalTokenNodeKindSet
        makeArrTestCase "TypeFlagLiteral" Internal.typeFlagLiteralKindSet
        makeArrTestCase "TypeFlagObject" Internal.typeFlagObjectKindSet
        makeArrTestCase "TypeFlagPrimaryKind" Internal.typeFlagPrimaryKindSet
    testSuite "LM2 - Each DU/Enum construction map has no duplicate keys" <| fun _ ->
        let inline makeArrTestCase (name: string) (arr: (_ * _) array) =
            testCase name <| fun _ ->
                let length = arr |> Array.length
                arr
                |> Seq.map (fst >> fun x -> ((^T or SRTPFlagToStringArray):(static member ToStringArray: ^T -> string array) x))
                |> Seq.distinct
                |> Flip.Expect.hasLength length $"Not every {name} has a unique key"
        let inline makeDictTestCase (name: string) (dict: Dictionary<Ts.SyntaxKind, _>) =
            testCase name <| fun _ ->
                let length = dict.Keys |> Seq.length
                dict.Keys
                |> Seq.map _.Name
                |> Seq.distinct
                |> Flip.Expect.hasLength length $"Not every {name} has a unique key"
        makeDictTestCase "DeclarationFileNodes" Internal.declarationFileNodes
        makeDictTestCase "MemberDeclaration" Internal.memberDeclarationKindSetMap
        makeDictTestCase "TypeDeclaration" Internal.typeDeclarationKindSetMap
        makeDictTestCase "TopLevelStatements" Internal.topLevelStatements
        makeDictTestCase "TopLevelExportSymbolDeclarations" Internal.topLevelExportDeclarations
        makeDictTestCase "TopLevelLocalSymbolDeclarations" Internal.topLevelLocalDeclarations
        makeDictTestCase "TypeNodeKind" Internal.typeNodeKindSetMap
        makeDictTestCase "JSDocKind" Internal.jsDocKindSetMap
        makeDictTestCase "ModuleExport" Internal.moduleExportSetMap
        makeDictTestCase "Modifier" Internal.modifierSetMap
        makeDictTestCase "LiteralTokenNode" Internal.literalTokenNodeKindSet
        makeArrTestCase "TypeFlagLiteral" Internal.typeFlagLiteralKindSet
        makeArrTestCase "TypeFlagObject" Internal.typeFlagObjectKindSet
        makeArrTestCase "TypeFlagPrimaryKind" Internal.typeFlagPrimaryKindSet
    testSuite "LM3" <| fun _ -> () // Seems redundant
    testSuite "LM4 - All Subset maps fall under the master Superset (DeclarationFileNodes)" <| fun _ ->
        let inline makeDictTestCase (name: string) (nameSuperSet: string) (dict: Dictionary<Ts.SyntaxKind, _>) (superSetDict: Dictionary<Ts.SyntaxKind, _>) =
            testCase $"{name} subset of {nameSuperSet}" <| fun _ ->
                let subset =
                    dict.Keys
                    |> Seq.map _.Name
                    |> Seq.distinct
                let superset =
                    superSetDict.Keys
                    |> Seq.map _.Name
                    |> Seq.distinct
                superset
                |> Flip.Expect.containsAll subset $"Not every {name} key is in {nameSuperSet}"
        makeDictTestCase "MemberDeclaration" "DeclarationFileNodes" Internal.memberDeclarationKindSetMap Internal.declarationFileNodes
        makeDictTestCase "TypeDeclaration" "DeclarationFileNodes" Internal.typeDeclarationKindSetMap Internal.declarationFileNodes
        makeDictTestCase "TopLevelStatements" "DeclarationFileNodes" Internal.topLevelStatements Internal.declarationFileNodes
        makeDictTestCase "TopLevelExportSymbolDeclarations" "DeclarationFileNodes" Internal.topLevelExportDeclarations Internal.declarationFileNodes
        makeDictTestCase "TopLevelLocalSymbolDeclarations" "DeclarationFileNodes" Internal.topLevelLocalDeclarations Internal.declarationFileNodes
        makeDictTestCase "TypeNodeKind" "DeclarationFileNodes" Internal.typeNodeKindSetMap Internal.declarationFileNodes
        // makeDictTestCase "JSDocKind" "DeclarationFileNodes" Internal.jsDocKindSetMap Internal.declarationFileNodes
        makeDictTestCase "ModuleExport" "DeclarationFileNodes" Internal.moduleExportSetMap Internal.declarationFileNodes
        makeDictTestCase "Modifier" "DeclarationFileNodes" Internal.modifierSetMap Internal.declarationFileNodes
        makeDictTestCase "LiteralTokenNode" "DeclarationFileNodes" Internal.literalTokenNodeKindSet Internal.declarationFileNodes
        
                
