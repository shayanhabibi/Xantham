module Xantham.TypeScript.Tests.Structural

open System.Collections.Generic
open Xantham.Mocha
open Xantham.TypeScript
open Xantham.Fable
open TypeScript

// ─────────────────────────────────────────────────────────────────────────────
// LM · LOOKUP-MAP ↔ DU STRUCTURAL PROOFS
//
// Unlike the corpus-driven proofs in `Program.test.fs`, these are
// *corpus-independent*: the classifier in `XanTagKind.fs`'s `Internal` module is a
// set of `Dictionary<Ts.SyntaxKind, obj -> 'DU>` maps (plus the `Array.find`-based
// `typeFlag*KindSet` tables). The LM proofs hold by construction or they don't —
// they fail fast and pinpoint a drift the moment a DU case is added without its map
// row (or a `SyntaxKind` is mapped twice, or a sub-map escapes the master set).
//
// Each carries a stable ID (`LM-n`) catalogued in `src/Xantham.TypeScript/README.md`:
//   LM-1 · every DU case is reachable from its map (no dead constructor).
//   LM-2 · every map's keys are distinct (a SyntaxKind resolves to one case).
//   LM-4 · every sub-map's keys are a subset of `declarationFileNodes` (the master
//          node vocabulary that ND-8 keys on).
// LM-3 (walk cross-check) and LM-5 (constructor-wiring) from the backlog are not yet
// implemented — `LM-3` below is a deliberate placeholder.
// ─────────────────────────────────────────────────────────────────────────────

testSuite "LM · Lookup-Map ↔ DU Structure" <| fun _ ->
    testSuite "LM-1 · Each DU case has a constructor value" <| fun _ ->
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
    testSuite "LM-2 · Each DU/Enum construction map has no duplicate keys" <| fun _ ->
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
    testSuite "LM-3" <| fun _ -> () // Reserved — backlog walk cross-check, not yet implemented
    testSuite "LM-4 · All subset maps fall under the master superset (DeclarationFileNodes)" <| fun _ ->
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
open type Ts.SymbolFlags
ptestSuite "Generation" <| fun _ ->
    testCase "Generate Symbol interfaces" <| fun _ ->
        let symbolFlagMap = Dictionary [
            let inline (==>) a b = KeyValuePair(a,b)
            FunctionScopedVariable ==> "Parameter"
            BlockScopedVariable ==> "Variable"
            Property ==> nameof Property 
            EnumMember ==> nameof EnumMember 
            Function ==> nameof Function
            Class ==> nameof Class
            Interface ==> nameof Interface
            ConstEnum ==> nameof ConstEnum
            RegularEnum ==> "TypeEnum"
            ValueModule ==> nameof ValueModule
            NamespaceModule ==> nameof NamespaceModule
            TypeLiteral ==> nameof TypeLiteral
            ObjectLiteral ==> nameof ObjectLiteral
            Method ==> nameof Method
            Constructor ==> nameof Constructor
            GetAccessor ==> nameof GetAccessor
            SetAccessor ==> nameof SetAccessor
            Signature ==> nameof Signature
            TypeParameter ==> nameof TypeParameter
            TypeAlias ==> nameof TypeAlias
        ]
        let composites = Dictionary [
            let inline (==>) a b = KeyValuePair(a,b)
            Enum ==> nameof Enum
            Value ==> nameof Value
            Type ==> nameof Type
            Namespace ==> nameof Namespace
            Module ==> nameof Module
            Accessor ==> nameof Accessor
            ModuleMember ==> nameof ModuleMember
            ExportHasLocal ==> nameof ExportHasLocal
            PropertyOrAccessor ==> nameof PropertyOrAccessor
            ClassMember ==> nameof ClassMember
        ]
        let exclusions = Dictionary [
            let inline (==>) a b = KeyValuePair(a,b)
            FunctionScopedVariable ==> ParameterExcludes
            BlockScopedVariable ==> BlockScopedVariableExcludes
            Property ==> PropertyExcludes
            EnumMember ==> EnumMemberExcludes
            Function ==> FunctionExcludes
            Class ==> ClassExcludes
            Interface ==> InterfaceExcludes
            RegularEnum ==> RegularEnumExcludes
            ConstEnum ==> ConstEnumExcludes
            ValueModule ==> ValueModuleExcludes
            NamespaceModule ==> NamespaceModuleExcludes
            Method ==> MethodExcludes
            GetAccessor ==> GetAccessorExcludes
            SetAccessor ==> SetAccessorExcludes
            TypeParameter ==> TypeParameterExcludes
            TypeAlias ==> TypeAliasExcludes
        ]
        let duplicatesTag = "Duplicates"
        let optionalTag = Optional, nameof Optional
        let transientTag = Transient, nameof Transient
        let symbolInterfaceName = "ISymbol"
        let symbolInterface = $"""module Symbol =
"""
        let inheritsInterface = sprintf "        inherit I%s"
        let inherits = sprintf "        inherit %s"
        let makeType = sprintf "    type %s ="
        let makeInterface = sprintf "    type I%s ="
        [
            symbolInterface
            "    // Tags"
            for _, name in [ optionalTag; transientTag ] do
                makeInterface name
                inherits symbolInterfaceName
            "    /// <summary>Indicates this can merge with declarations of its own kind.</summary>"
            makeInterface duplicatesTag + " interface end"
            ""
            let makeComposites = fun isTransient ->
                [
                    "    // Composites"
                    for KeyValue(_, name) in composites do
                        makeInterface name
                        if isTransient then
                            inherits symbolInterfaceName
                            snd transientTag |> inheritsInterface
                        else
                            name
                            |> sprintf "Transient.I%s"
                            |> inherits
                        snd optionalTag |> inheritsInterface
                    ""
                ]
                |> if isTransient then List.map (sprintf "    %s") else id
            let makeFlags = fun isTransient ->
                [
                    "    // Flags"
                    for KeyValue(symFlag, name) in symbolFlagMap do
                        name
                        |> makeInterface
                        if exclusions.ContainsKey symFlag && exclusions[symFlag].HasFlag symFlag |> not then
                            inheritsInterface duplicatesTag
                        if isTransient then
                            inherits symbolInterfaceName
                            snd transientTag |> inheritsInterface
                        else
                            name
                            |> sprintf "Transient.I%s"
                            |> inherits
                        snd optionalTag |> inheritsInterface
                        for KeyValue(compositeFlag, compositeName) in composites do
                            if compositeFlag.HasFlag symFlag then
                                inheritsInterface compositeName
                    ""
                ]
                |> if isTransient then List.map (sprintf "    %s") else id
            let makeSymbols = fun isTransient ->
                [
                    "    // Symbols"
                    for KeyValue(symFlag, name) in symbolFlagMap do
                        name |> makeType
                        if not isTransient then
                            name
                            |> sprintf "Transient.%s"
                            |> inherits
                        if exclusions.ContainsKey symFlag then
                            inheritsInterface name
                            for KeyValue(inheritSymFlag, inheritName) in symbolFlagMap do
                                if inheritName <> name && exclusions[symFlag].HasFlag inheritSymFlag |> not then
                                    inheritsInterface inheritName
                        nameof Optional
                        |> inheritsInterface
                    ""
                ]
                |> if isTransient then List.map (sprintf "    %s") else id
            let makeDiscriminatedUnion = fun isTransient ->
                [
                    "    // Discriminated Union"
                    "    [<RequireQualifiedAccess>]"
                    "    type Kind ="
                    for KeyValue(_, name) in symbolFlagMap do
                        $"        | %s{name} of %s{name}"
                    if not isTransient then
                        "        | Transient of Transient.Kind"
                    else
                        "        /// <summary>No known declarations or solid types associated with symbol. Contributions are helpful if you can determine the concrete type/declaration from the symbol.</summary>"
                        "        | Unknown of ISymbol"
                    ""
                ]
                |> if isTransient then List.map (sprintf "    %s") else id
            "    [<RequireQualifiedAccess>]"
            "    module Transient ="
            yield! makeComposites true
            yield! makeFlags true
            yield! makeSymbols true
            yield! makeDiscriminatedUnion true
            yield! makeComposites false
            yield! makeFlags false
            yield! makeSymbols false
            yield! makeDiscriminatedUnion false
            ""
            "    [<Erase>]"
            "    type LocalTableSymbol = LocalTableSymbol of Ts.Symbol interface IWrappable<Ts.Symbol>"
            "    [<Erase>]"
            "    type ExportTableSymbol = ExportTableSymbol of Ts.Symbol interface IWrappable<Ts.Symbol>"
        ]
        |> String.concat "\n"
        |> printfn "%s"