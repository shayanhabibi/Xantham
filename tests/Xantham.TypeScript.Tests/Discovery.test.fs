module Discovery

open System
open Xantham.TypeScript.Types.Node
open Xantham.TypeScript.Types.Symbol
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha

let inline edgeCases (runner: Spec.RunnerContext) : unit =
    runner.testSuite "Edge cases" <| fun _ ->
        let symbolFlagCombinations = HashSet<Ts.SymbolFlags>()
        let cases = Dictionary<string array, HashSet<string array>>()
        let compositeDeclarationSymbols = ResizeArray<Ts.Symbol>()
        let inline ifNotEmptyThen (fn: 'T -> unit when 'T :> 'U seq) (collection: 'T) =
            if Seq.isEmpty collection then () else
            fn collection
        beforeTests "Prepare collections" <| fun _ ->
            symbolFlagCombinations.Clear()
            cases.Clear()
            compositeDeclarationSymbols.Clear()
        afterTests "Print collections" <| fun _ ->
            symbolFlagCombinations
            |> ifNotEmptyThen (
                Seq.map (_.ToStringArray() >> Array.sort)
                >> Seq.sort
                >> Seq.toArray
                >> Utils.trace
                )
            cases
            |> ifNotEmptyThen (
                Seq.map (fun (KeyValue(syntaxKinds, symbolFlags)) ->
                    symbolFlags
                    |> Seq.toArray
                    |> Array.sort,
                    syntaxKinds
                    )
                >> Seq.sort
                >> Seq.toArray
                >> Utils.trace
                )
        afterTests "Cleanup collections" <| fun _ ->
            compositeDeclarationSymbols.Clear()
            cases.Clear()
            symbolFlagCombinations.Clear()
        runner.testCase "Collect composite symbol flags" <| fun _ ctx ->
            ctx.Symbols.Value
            |> Array.filter (_.flags.ToStringArray().Length.Equals(1) >> not)
            |> Array.iter (_.flags >> symbolFlagCombinations.Add >> ignore)
        runner.testCase "Find unknown symbol flag <-> symbol declaration kind compositions" <| fun _ ctx ->
            // ---
            // Symbols merge declarations, and this can be common sense & documented for the most part,
            // but some such as merging a parameter and a type parameter can be surprising.
            // To this effect, we collect all symbols that have declarations of different *kinds*, and
            // associate it with the symbol flags that elicited the different *kinds*.
            // ---
            // An internal map is kept in source code for tracking; this can be used in structural tests with
            // reflection to ensure any wrappers account for all cases.
            // The test will only fail if a new combination is discovered. This combination should be output
            // to the console once all fixtures have run.
            let mutable fail = false
            ctx.Symbols.Value
            |> Array.map (function
                | symbol when symbol.flags.HasFlag(Ts.SymbolFlags.Alias) -> ctx.Checker.getAliasedSymbol symbol
                | symbol -> ctx.Checker.getMergedSymbol symbol
                )
            |> Array.distinctBy ts.getSymbolId
            |> Array.iter (fun symbol ->
               symbol.declarations
               |> Option.filter (_.AsArray.Length.Equals(1) >> not)
               |> Option.filter (_.AsArray >> fun decls ->
                   compositeDeclarationSymbols.Add symbol
                   match Internal.knownSymbolNodeCombinations.TryGetValue symbol.flags with
                   | true, set ->
                       decls
                       |> Array.map _.kind
                       |> Array.distinct
                       |> Array.except set
                       |> Array.isEmpty
                       |> not
                   | _ -> false
                   )
               |> Option.map (
                   _.AsArray
                   >> Array.distinctBy _.kind
                   >> Array.map _.kind.Name
                   )
               |> Option.iter (fun kinds ->
                   let kinds =
                       kinds
                       |> Array.sort
                   let symbolFlags = symbol.flags.ToStringArray() |> Array.sort
                   match cases.TryGetValue(kinds) with
                   | true, set -> set.Add symbolFlags |> ignore
                   | _ -> cases.Add(kinds, HashSet [symbolFlags])
                   fail <- true
                   )
               )
            Expect.isFalse fail "Found new unknown symbol flag/kind combinations"

type FlagSymbolCounter<'T when 'T :> Enum> = {
    name: string
    filter: Ts.Type -> bool
    flagMap: Ts.Type -> 'T
    symbolCount: Dictionary<'T, int>
    count: Dictionary<'T, int>
    identity: Dictionary<'T, string>
    subCounters: ResizeArray<FlagSymbolCounter<'T>>
    guaranteedCompositions: Dictionary<'T, 'T>
    symbolFlags: Dictionary<'T, Dictionary<Ts.SymbolFlags, int>>
    symbolKind: Dictionary<string array, Dictionary<string, int>>
}
type FlagSymbolCounterResult = {
    flag: int
    name: string
    symbolCount: int
    count: int
    percent: float
}

module FlagSymbolCounter =
    let create (name: string) filter flagMap (identity: ('T * string) array) =
        {
            name = name
            filter = filter
            flagMap = flagMap
            symbolCount =
                identity
                |> Array.map (fst >> fun flag -> flag, 0)
                |> Array.map KeyValuePair
                |> Dictionary
            count =
                identity
                |> Array.map (fst >> fun flag -> flag, 0)
                |> Array.map KeyValuePair
                |> Dictionary
            identity =
                identity
                |> Array.map KeyValuePair
                |> Dictionary
            subCounters = ResizeArray()
            guaranteedCompositions =
                identity
                |> Array.map (fst >> fun flag -> flag, enum -1)
                |> Array.map KeyValuePair
                |> Dictionary
            symbolFlags = Dictionary()
            symbolKind = Dictionary()
        }
    let addSymbolFlags (counter: FlagSymbolCounter<'T>) (symbolFlags: Ts.SymbolFlags) (flags: 'T) =
        match counter.symbolFlags.TryGetValue(flags) with
        | false, _ -> counter.symbolFlags[flags] <- Dictionary [ KeyValuePair(symbolFlags, 1) ]
        | true, dict ->
            match dict.TryGetValue(symbolFlags) with
            | true, count -> dict[symbolFlags] <- count + 1
            | _ -> dict.Add(symbolFlags, 1)
    let addSymbolKind program (counter: FlagSymbolCounter<'T>) (symbol: Ts.Symbol) (flags: 'T) =
        let symbolKind = Symbol.createKind program symbol
        let symbolKindName = symbolKind.ToString()
        let flagArray = Array.sort [|
            for KeyValue(flag, name) in counter.identity do
                if flags.HasFlag flag then name
        |]
        match counter.symbolKind.TryGetValue(flagArray) with
        | false, _ ->
            counter.symbolKind[flagArray] <- Dictionary [ KeyValuePair(symbolKindName, 1) ]
        | true, dict ->
            match dict.TryGetValue(symbolKindName) with
            | true, count -> dict[symbolKindName] <- count + 1
            | _ -> dict.Add(symbolKindName, 1)
    let addSubCounter (counter: FlagSymbolCounter<'T>) (subCounter: FlagSymbolCounter<'T>) =
        counter.subCounters.Add subCounter
    let populateSubCounters (counter: FlagSymbolCounter<'T>) =
        counter.identity
        |> Seq.filter (fun (KeyValue(flag, name)) -> counter.symbolCount[flag] <> 0 && counter.symbolCount[flag] <> counter.count[flag] && counter.subCounters.AsArray |> Array.exists _.name.Equals(name) |> not)
        |> Seq.toArray
        |> Array.map (fun (KeyValue(flag, name)) ->
            create name (counter.flagMap >> _.HasFlag(flag)) counter.flagMap (counter.identity |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray)
            )
        |> Array.iter (addSubCounter counter)
    let getFullResults (counter: FlagSymbolCounter<'T>) =
        counter.identity
        |> Seq.map (fun (KeyValue(flag: 'T, name)) ->
            {
                flag = unbox flag
                name = name
                symbolCount = counter.symbolCount[flag]
                count = counter.count[flag]
                percent = float counter.symbolCount[flag] / float counter.count[flag] * 10.0
            })
        |> Seq.toArray
    let getThinResults (counter: FlagSymbolCounter<'T>) =
        counter.identity
        |> Seq.choose (fun (KeyValue(flag, name)) ->
            let symbolCount = counter.symbolCount[flag]
            let count = counter.count[flag]
            if count <> 0 then Some {
                flag = unbox flag
                name = name
                symbolCount = symbolCount
                count = count
                percent = float symbolCount / float count * 10.0
            } else None
            )
        |> Seq.toArray
    let getCompositions (counter: FlagSymbolCounter<'T>) =
        counter.guaranteedCompositions
        |> Seq.choose (fun compFlag ->
            if compFlag.Value = unbox -1 then None else
            let flagNameArray = [|
                for KeyValue(flag, name) in counter.identity do
                    if compFlag.Value.HasFlag flag then name
            |]
            if flagNameArray.Length < 2 then None else
            (counter.identity[compFlag.Key], flagNameArray)
            |> Some
            )
        |> Seq.toArray
    let getSymbolFlags (counter: FlagSymbolCounter<'T>) =
        counter.symbolFlags
        |> Seq.map (fun symbolFlagEntry ->
            let flagNameArray = [|
                for KeyValue(flag, name) in counter.identity do
                    if symbolFlagEntry.Key.HasFlag flag then name
            |]
            let totalCount = symbolFlagEntry.Value |> Seq.sumBy _.Value
            let symbolFlagArray = [|
                for KeyValue(symbolFlag, count) in symbolFlagEntry.Value do
                    symbolFlag.ToStringArray()
                    |> Array.sort, count
            |]
            flagNameArray, totalCount, symbolFlagArray
            )
    let getSymbolKind (counter: FlagSymbolCounter<'T>) =
        counter.symbolKind
        |> Seq.map (fun symbolKindEntry ->
            let totalCount = symbolKindEntry.Value |> Seq.sumBy _.Value
            let symbolKindArray = [|
                for KeyValue(symbolKind, count) in symbolKindEntry.Value do
                    symbolKind, count
            |]
            symbolKindEntry.Key, totalCount, symbolKindArray
            )
    let printSymbolFlags (counter: FlagSymbolCounter<'T>) =
        getSymbolFlags counter
        |> Seq.map (fun (flagNameArray, totalCount, symbolFlagArray) ->
            [
                sprintf "[ %A ]" flagNameArray
                for symbolFlags, count in Array.sortByDescending snd symbolFlagArray do
                    sprintf "    (%i/%i) %A" count totalCount symbolFlags
            ]
            |> String.concat "\n"
            )
        |> String.concat "\n\n"
        |> printfn "%s"
    let printSymbolKind (counter: FlagSymbolCounter<'T>) =
        getSymbolKind counter
        |> Seq.map (fun (flagNameArray, totalCount, symbolKindArray) ->
            [
                sprintf "[ %A ]" flagNameArray
                for symbolKind, count in Array.sortByDescending snd symbolKindArray do
                    sprintf "    SymbolKind: (%i/%i) %s" count totalCount symbolKind
            ]
            |> String.concat "\n"
            )
        |> String.concat "\n\n"
        |> printfn "%s"
    let incCount (counter: FlagSymbolCounter<'T>) (flag: 'T) = counter.count[flag] <- counter.count[flag] + 1
    let incSymbolCount (counter: FlagSymbolCounter<'T>) (flag: 'T) = counter.symbolCount[flag] <- counter.symbolCount[flag] + 1
    let incBothCounts (counter: FlagSymbolCounter<'T>) flag =
        incCount counter flag
        incSymbolCount counter flag
    let private printHeader = fun length header -> sprintf "|%-*s|%-10s|%-10s|%-10s|%-10s|" length header "Symbols" "Count" "Sym:Count%" "Type"
    let private printRow length flagName symCount count typ = sprintf "|%-*s|%-10i|%-10i|%-9.2f%%|%-10s|" length flagName symCount count (float symCount / float count * 100.0) typ
    let printResults (counter: FlagSymbolCounter<'T>) =
        let printResult length (result: FlagSymbolCounterResult) =
            match result with
            | { name = name; symbolCount = symCount; count = count } when symCount = count && symCount > 0 ->
                printRow length name symCount count (String.replicate 10 "+")
            | { name = name; symbolCount = 0; count = 0 } ->
                printRow length name 0 0 "N/A"
            | { name = name; symbolCount = 0; count = count } ->
                printRow length name 0 count ""
            | { name = name; symbolCount = symCount; count = count; percent = percent } ->
                printRow length name symCount count (max percent 1. |> int |> String.replicate |> funApply "-")
        let length = counter.identity.Values |> Seq.maxBy _.Length |> _.Length
        let name = counter.name
        getFullResults counter
        |> Array.sortBy _.flag
        |> Array.sortByDescending _.count
        |> Array.sortByDescending _.percent
        |> Array.map (printResult length)
        |> Array.insertAt 0 (printHeader length name)
        |> String.concat "\n"
    let printSubCounters (counter: FlagSymbolCounter<'T>) =
        let printResult length (result: FlagSymbolCounterResult) =
            match result with
            | { name = name; symbolCount = symCount; count = count; percent = percent } when symCount = count && symCount > 0 ->
                printRow length name symCount count (String.replicate 10 "+")
            | { name = name; symbolCount = 0; count = count } ->
                printRow length name 0 count ""
            | { name = name; symbolCount = symCount; count = count; percent = percent } ->
                printRow length name symCount count (max percent 1. |> int |> String.replicate |> funApply "-")
        let length = counter.identity.Values |> Seq.maxBy _.Length |> _.Length
        counter.subCounters.AsArray
        |> Array.mapi (fun idx ->
            getThinResults 
            >> Array.sortBy _.flag
            >> Array.map (printResult length)
            >> Array.insertAt 0 (printHeader length counter.subCounters[idx].name)
            >> String.concat "\n"
            )
        |> String.concat "\n\n"
        
let inline symbolLessTypes (runner: Spec.RunnerContext) : unit =
    runner.testSuite "Type Investigation" <| fun _ ->
        let flags = [|
            Ts.TypeFlags.Any, "Any"
            Ts.TypeFlags.Unknown, "Unknown"
            Ts.TypeFlags.Undefined, "Undefined"
            Ts.TypeFlags.Null, "Null"
            Ts.TypeFlags.Void, "Void"
            Ts.TypeFlags.String, "String"
            Ts.TypeFlags.Number, "Number"
            Ts.TypeFlags.BigInt, "BigInt"
            Ts.TypeFlags.Boolean, "Boolean"
            Ts.TypeFlags.ESSymbol, "ESSymbol"
            Ts.TypeFlags.StringLiteral, "StringLiteral"
            Ts.TypeFlags.NumberLiteral, "NumberLiteral"
            Ts.TypeFlags.BigIntLiteral, "BigIntLiteral"
            Ts.TypeFlags.BooleanLiteral, "BooleanLiteral"
            Ts.TypeFlags.UniqueESSymbol, "UniqueESSymbol"
            Ts.TypeFlags.EnumLiteral, "EnumLiteral"
            Ts.TypeFlags.Enum, "Enum"
            Ts.TypeFlags.NonPrimitive, "NonPrimitive"
            Ts.TypeFlags.Never, "Never"
            Ts.TypeFlags.TypeParameter, "TypeParameter"
            Ts.TypeFlags.Object, "Object"
            Ts.TypeFlags.Index, "Index"
            Ts.TypeFlags.TemplateLiteral, "TemplateLiteral"
            Ts.TypeFlags.StringMapping, "StringMapping"
            Ts.TypeFlags.Substitution, "Substitution"
            Ts.TypeFlags.IndexedAccess, "IndexedAccess"
            Ts.TypeFlags.Conditional, "Conditional"
            Ts.TypeFlags.Union, "Union"
            Ts.TypeFlags.Intersection, "Intersection"
        |]
        let objectFlags = [|
            Ts.ObjectFlags.Class, "Class"
            Ts.ObjectFlags.Interface, "Interface"
            Ts.ObjectFlags.Reference, "Reference"
            Ts.ObjectFlags.Tuple, "Tuple"
            Ts.ObjectFlags.Anonymous, "Anonymous"
            Ts.ObjectFlags.Mapped, "Mapped"
            Ts.ObjectFlags.Instantiated, "Instantiated"
            Ts.ObjectFlags.ObjectLiteral, "ObjectLiteral"
            Ts.ObjectFlags.EvolvingArray, "EvolvingArray"
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties, "ObjectLiteralPatternWithComputedProperties"
            Ts.ObjectFlags.ReverseMapped, "ReverseMapped"
            Ts.ObjectFlags.JsxAttributes, "JsxAttributes"
            Ts.ObjectFlags.JSLiteral, "JSLiteral"
            Ts.ObjectFlags.FreshLiteral, "FreshLiteral"
            Ts.ObjectFlags.ArrayLiteral, "ArrayLiteral"
            Ts.ObjectFlags.ContainsSpread, "ContainsSpread"
            Ts.ObjectFlags.ObjectRestType, "ObjectRestType"
            Ts.ObjectFlags.InstantiationExpressionType, "InstantiationExpressionType"
        |]
        let typeFlags = FlagSymbolCounter.create "TypeFlags" (fun _ -> true) _.flags flags
        let objectFlags = FlagSymbolCounter.create "ObjectFlags" _.flags.HasFlag(Ts.TypeFlags.Object) (fun typ -> typ :?> Ts.ObjectType |> _.objectFlags) objectFlags
        
        afterTests "Print typeFlags" <| fun _ ->
            [
                box typeFlags
                box objectFlags
            ]
            |> List.iter (unbox<FlagSymbolCounter<Enum>> >> fun typeFlags ->
                FlagSymbolCounter.printResults typeFlags
                |> printfn "%s\n"
                
                FlagSymbolCounter.printSubCounters typeFlags
                |> printfn "%s\n"
                
                // FlagSymbolCounter.getCompositions typeFlags
                // |> Utils.trace
                
                FlagSymbolCounter.printSymbolFlags typeFlags
                FlagSymbolCounter.printSymbolKind typeFlags
            )
            
        let inline iterIdentity program (block: FlagSymbolCounter<'T>) (typ: #Ts.Type) =
            let filter = block.filter
            let flagMap = block.flagMap
            if filter typ |> not then () else
            let inline inc (mapping: _ -> Dictionary<_, int>) flag =
                let section = mapping block
                section[flag] <- section[flag] + 1
            for KeyValue(flag, _) in block.identity do
                if flagMap typ |> _.HasFlag(flag) then
                    block.guaranteedCompositions[flag] <- block.guaranteedCompositions[flag] &&& flagMap typ
                    inc _.count flag
                    if typ.getCanonicalSymbol().IsSome then
                        FlagSymbolCounter.addSymbolFlags
                            block
                            (typ.getCanonicalSymbol().Value.flags)
                            (flagMap typ)
                        FlagSymbolCounter.addSymbolKind
                            program
                            block
                            (typ.getCanonicalSymbol().Value)
                            (flagMap typ)
                        inc _.symbolCount flag
        afterEachTests " " <| fun _ ->
            FlagSymbolCounter.populateSubCounters typeFlags
            FlagSymbolCounter.populateSubCounters objectFlags
        runner.testCase "TypeFlags & ObjectFlags iteration" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.iter ( iterIdentity ctx.Program typeFlags )
            ctx.Types.Value
            |> Array.iter ( iterIdentity ctx.Program objectFlags )
        runner.testCase "SubCounters iteration" <| fun _ ctx ->
            typeFlags.subCounters.AsArray
            |> Array.iter (fun subCounter ->
                ctx.Types.Value |> Array.iter ( iterIdentity ctx.Program subCounter ) )
            objectFlags.subCounters.AsArray
            |> Array.iter (fun subCounter ->
                ctx.Types.Value |> Array.iter ( iterIdentity ctx.Program subCounter ) )
        let flock = HashSet<string array>()
        runner.ptestCase "TypeFlags & ObjectFlags" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> Array.map (fun typ ->
                typ.flags.ToStringArray()
                |> Array.map (sprintf "Ts.TypeFlags.%s")
                |> Array.append (
                    typ :?> Ts.ObjectType
                    |> _.objectFlags.ToStringArray()
                    |> Array.map (sprintf "Ts.ObjectFlags.%s")
                    |> Array.sort
                    )
                |> Array.sort
                )
            |> Array.sort
            |> Array.iter (flock.Add >> ignore)
        afterTests "Print flock" <| fun _ ->
            flock
            |> Seq.toArray
            |> NonEmptyArray.create
            |> Option.iter (NonEmptyArray.iter Utils.trace)

type SyntaxKindSymbolCounter = {
    name: string
    filter: Ts.SyntaxKind -> bool
    map: Ts.Node -> Ts.Node
    symbolCount: Dictionary<Ts.SyntaxKind, int>
    count: Dictionary<Ts.SyntaxKind, int>
    identity: Dictionary<Ts.SyntaxKind, string>
    documentationTags: Dictionary<Ts.SyntaxKind, Dictionary<Ts.JSDocSyntaxKind, int>>
}

type SyntaxKindSymbolCounterResult = {
    kind: int
    name: string
    symbolCount: int
    count: int
    percent: float
}
module SyntaxKindSymbolCounter =
    let create (name: string) filter flagMap (identity: (Ts.SyntaxKind * string) array) =
        {
            name = name
            filter = filter
            map = flagMap
            symbolCount =
                identity
                |> Array.map (fst >> fun flag -> flag, 0)
                |> Array.map KeyValuePair
                |> Dictionary
            count =
                identity
                |> Array.map (fst >> fun flag -> flag, 0)
                |> Array.map KeyValuePair
                |> Dictionary
            identity =
                identity
                |> Array.map KeyValuePair
                |> Dictionary
            documentationTags =
                identity
                |> Array.map (fst >> fun flag -> flag, Dictionary())
                |> Array.map KeyValuePair
                |> Dictionary
        }
    let getFullResults (counter: SyntaxKindSymbolCounter) =
        counter.identity
        |> Seq.map (fun (KeyValue(flag: Ts.SyntaxKind, name)) ->
            {
                kind = unbox flag
                name = name
                symbolCount = counter.symbolCount[flag]
                count = counter.count[flag]
                percent = float counter.symbolCount[flag] / float counter.count[flag] * 10.0
            })
        |> Seq.toArray
    let getThinResults (counter: SyntaxKindSymbolCounter) =
        counter.identity
        |> Seq.choose (fun (KeyValue(flag, name)) ->
            let symbolCount = counter.symbolCount[flag]
            let count = counter.count[flag]
            if count <> 0 then Some {
                flag = unbox flag
                name = name
                symbolCount = symbolCount
                count = count
                percent = float symbolCount / float count * 10.0
            } else None
            )
        |> Seq.toArray
    let getCompositions (counter: FlagSymbolCounter<'T>) =
        counter.guaranteedCompositions
        |> Seq.choose (fun compFlag ->
            if compFlag.Value = unbox -1 then None else
            let flagNameArray = [|
                for KeyValue(flag, name) in counter.identity do
                    if compFlag.Value.HasFlag flag then name
            |]
            if flagNameArray.Length < 2 then None else
            (counter.identity[compFlag.Key], flagNameArray)
            |> Some
            )
        |> Seq.toArray
    let incCount (counter: SyntaxKindSymbolCounter) (flag: Ts.SyntaxKind) = counter.count[flag] <- counter.count[flag] + 1
    let incSymbolCount (counter: SyntaxKindSymbolCounter) (flag: Ts.SyntaxKind) = counter.symbolCount[flag] <- counter.symbolCount[flag] + 1
    let incBothCounts (counter: SyntaxKindSymbolCounter) flag =
        incCount counter flag
        incSymbolCount counter flag
    let addNodeDocumentation (counter: SyntaxKindSymbolCounter) (node: Ts.Node) =
        let dict = counter.documentationTags[node.kind]
        
        ts.getAllJSDocTags(node, fun _ -> true).AsArray
        |> Array.map _.kind
        |> Array.iter (fun tagKind ->
            match dict.TryGetValue tagKind with
            | true, counter -> dict[tagKind] <- counter + 1
            | _ -> dict[tagKind] <- 1
            )
    let getNodeDocumentationResults (counter: SyntaxKindSymbolCounter) =
        counter.documentationTags
        |> Seq.map (fun (KeyValue(kind, dict)) ->
            let counts = [|
                for KeyValue(tagKind, count) in dict do
                    tagKind.Name, count
            |]
            kind.Name, counts |> Array.sortByDescending snd
            )
        |> Seq.toArray
        
    let private printHeader = fun length header -> sprintf "|%-*s|%-10s|%-10s|%-10s|%-10s|" length header "Symbols" "Count" "Sym:Count%" "Type"
    let private printRow length flagName symCount count typ = sprintf "|%-*s|%-10i|%-10i|%-9.2f%%|%-10s|" length flagName symCount count (float symCount / float count * 100.0) typ
    let printResults (counter: SyntaxKindSymbolCounter) =
        let printResult length (result: SyntaxKindSymbolCounterResult) =
            match result with
            | { name = name; symbolCount = symCount; count = count } when symCount = count && symCount > 0 ->
                printRow length name symCount count (String.replicate 10 "+")
            | { name = name; symbolCount = 0; count = 0 } ->
                printRow length name 0 0 "N/A"
            | { name = name; symbolCount = 0; count = count } ->
                printRow length name 0 count ""
            | { name = name; symbolCount = symCount; count = count; percent = percent } ->
                printRow length name symCount count (max percent 1. |> int |> String.replicate |> funApply "-")
        let length = counter.identity.Values |> Seq.maxBy _.Length |> _.Length
        let name = counter.name
        getFullResults counter
        |> Array.filter (_.percent >> JS.isNaN >> not)
        |> Array.sortByDescending _.count
        |> Array.sortByDescending _.percent
        |> Array.map (printResult length)
        |> Array.insertAt 0 (printHeader length name)
        |> String.concat "\n"
 
let inline symbolDiscovery (runner: Spec.RunnerContext) : unit = runner.testSuite "Node Symbol Discovery" <| fun _ ->
    let kinds = [|
        Ts.SyntaxKind.Unknown, "Unknown"
        Ts.SyntaxKind.EndOfFileToken, "EndOfFileToken"
        Ts.SyntaxKind.SingleLineCommentTrivia, "SingleLineCommentTrivia"
        Ts.SyntaxKind.MultiLineCommentTrivia, "MultiLineCommentTrivia"
        Ts.SyntaxKind.NewLineTrivia, "NewLineTrivia"
        Ts.SyntaxKind.WhitespaceTrivia, "WhitespaceTrivia"
        Ts.SyntaxKind.ShebangTrivia, "ShebangTrivia"
        Ts.SyntaxKind.ConflictMarkerTrivia, "ConflictMarkerTrivia"
        Ts.SyntaxKind.NonTextFileMarkerTrivia, "NonTextFileMarkerTrivia"
        Ts.SyntaxKind.NumericLiteral, "NumericLiteral"
        Ts.SyntaxKind.BigIntLiteral, "BigIntLiteral"
        Ts.SyntaxKind.StringLiteral, "StringLiteral"
        Ts.SyntaxKind.JsxText, "JsxText"
        Ts.SyntaxKind.JsxTextAllWhiteSpaces, "JsxTextAllWhiteSpaces"
        Ts.SyntaxKind.RegularExpressionLiteral, "RegularExpressionLiteral"
        Ts.SyntaxKind.NoSubstitutionTemplateLiteral, "NoSubstitutionTemplateLiteral"
        Ts.SyntaxKind.TemplateHead, "TemplateHead"
        Ts.SyntaxKind.TemplateMiddle, "TemplateMiddle"
        Ts.SyntaxKind.TemplateTail, "TemplateTail"
        Ts.SyntaxKind.OpenBraceToken, "OpenBraceToken"
        Ts.SyntaxKind.CloseBraceToken, "CloseBraceToken"
        Ts.SyntaxKind.OpenParenToken, "OpenParenToken"
        Ts.SyntaxKind.CloseParenToken, "CloseParenToken"
        Ts.SyntaxKind.OpenBracketToken, "OpenBracketToken"
        Ts.SyntaxKind.CloseBracketToken, "CloseBracketToken"
        Ts.SyntaxKind.DotToken, "DotToken"
        Ts.SyntaxKind.DotDotDotToken, "DotDotDotToken"
        Ts.SyntaxKind.SemicolonToken, "SemicolonToken"
        Ts.SyntaxKind.CommaToken, "CommaToken"
        Ts.SyntaxKind.QuestionDotToken, "QuestionDotToken"
        Ts.SyntaxKind.LessThanToken, "LessThanToken"
        Ts.SyntaxKind.LessThanSlashToken, "LessThanSlashToken"
        Ts.SyntaxKind.GreaterThanToken, "GreaterThanToken"
        Ts.SyntaxKind.LessThanEqualsToken, "LessThanEqualsToken"
        Ts.SyntaxKind.GreaterThanEqualsToken, "GreaterThanEqualsToken"
        Ts.SyntaxKind.EqualsEqualsToken, "EqualsEqualsToken"
        Ts.SyntaxKind.ExclamationEqualsToken, "ExclamationEqualsToken"
        Ts.SyntaxKind.EqualsEqualsEqualsToken, "EqualsEqualsEqualsToken"
        Ts.SyntaxKind.ExclamationEqualsEqualsToken, "ExclamationEqualsEqualsToken"
        Ts.SyntaxKind.EqualsGreaterThanToken, "EqualsGreaterThanToken"
        Ts.SyntaxKind.PlusToken, "PlusToken"
        Ts.SyntaxKind.MinusToken, "MinusToken"
        Ts.SyntaxKind.AsteriskToken, "AsteriskToken"
        Ts.SyntaxKind.AsteriskAsteriskToken, "AsteriskAsteriskToken"
        Ts.SyntaxKind.SlashToken, "SlashToken"
        Ts.SyntaxKind.PercentToken, "PercentToken"
        Ts.SyntaxKind.PlusPlusToken, "PlusPlusToken"
        Ts.SyntaxKind.MinusMinusToken, "MinusMinusToken"
        Ts.SyntaxKind.LessThanLessThanToken, "LessThanLessThanToken"
        Ts.SyntaxKind.GreaterThanGreaterThanToken, "GreaterThanGreaterThanToken"
        Ts.SyntaxKind.GreaterThanGreaterThanGreaterThanToken, "GreaterThanGreaterThanGreaterThanToken"
        Ts.SyntaxKind.AmpersandToken, "AmpersandToken"
        Ts.SyntaxKind.BarToken, "BarToken"
        Ts.SyntaxKind.CaretToken, "CaretToken"
        Ts.SyntaxKind.ExclamationToken, "ExclamationToken"
        Ts.SyntaxKind.TildeToken, "TildeToken"
        Ts.SyntaxKind.AmpersandAmpersandToken, "AmpersandAmpersandToken"
        Ts.SyntaxKind.BarBarToken, "BarBarToken"
        Ts.SyntaxKind.QuestionToken, "QuestionToken"
        Ts.SyntaxKind.ColonToken, "ColonToken"
        Ts.SyntaxKind.AtToken, "AtToken"
        Ts.SyntaxKind.QuestionQuestionToken, "QuestionQuestionToken"
        Ts.SyntaxKind.BacktickToken, "BacktickToken"
        Ts.SyntaxKind.HashToken, "HashToken"
        Ts.SyntaxKind.EqualsToken, "EqualsToken"
        Ts.SyntaxKind.PlusEqualsToken, "PlusEqualsToken"
        Ts.SyntaxKind.MinusEqualsToken, "MinusEqualsToken"
        Ts.SyntaxKind.AsteriskEqualsToken, "AsteriskEqualsToken"
        Ts.SyntaxKind.AsteriskAsteriskEqualsToken, "AsteriskAsteriskEqualsToken"
        Ts.SyntaxKind.SlashEqualsToken, "SlashEqualsToken"
        Ts.SyntaxKind.PercentEqualsToken, "PercentEqualsToken"
        Ts.SyntaxKind.LessThanLessThanEqualsToken, "LessThanLessThanEqualsToken"
        Ts.SyntaxKind.GreaterThanGreaterThanEqualsToken, "GreaterThanGreaterThanEqualsToken"
        Ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,  "GreaterThanGreaterThanGreaterThanEqualsToken"
        Ts.SyntaxKind.AmpersandEqualsToken, "AmpersandEqualsToken"
        Ts.SyntaxKind.BarEqualsToken, "BarEqualsToken"
        Ts.SyntaxKind.BarBarEqualsToken, "BarBarEqualsToken"
        Ts.SyntaxKind.AmpersandAmpersandEqualsToken, "AmpersandAmpersandEqualsToken"
        Ts.SyntaxKind.QuestionQuestionEqualsToken, "QuestionQuestionEqualsToken"
        Ts.SyntaxKind.CaretEqualsToken, "CaretEqualsToken"
        Ts.SyntaxKind.Identifier, "Identifier"
        Ts.SyntaxKind.PrivateIdentifier, "PrivateIdentifier"
        Ts.SyntaxKind.BreakKeyword, "BreakKeyword"
        Ts.SyntaxKind.CaseKeyword, "CaseKeyword"
        Ts.SyntaxKind.CatchKeyword, "CatchKeyword"
        Ts.SyntaxKind.ClassKeyword, "ClassKeyword"
        Ts.SyntaxKind.ConstKeyword, "ConstKeyword"
        Ts.SyntaxKind.ContinueKeyword, "ContinueKeyword"
        Ts.SyntaxKind.DebuggerKeyword, "DebuggerKeyword"
        Ts.SyntaxKind.DefaultKeyword, "DefaultKeyword"
        Ts.SyntaxKind.DeleteKeyword, "DeleteKeyword"
        Ts.SyntaxKind.DoKeyword, "DoKeyword"
        Ts.SyntaxKind.ElseKeyword, "ElseKeyword"
        Ts.SyntaxKind.EnumKeyword, "EnumKeyword"
        Ts.SyntaxKind.ExportKeyword, "ExportKeyword"
        Ts.SyntaxKind.ExtendsKeyword, "ExtendsKeyword"
        Ts.SyntaxKind.FalseKeyword, "FalseKeyword"
        Ts.SyntaxKind.FinallyKeyword, "FinallyKeyword"
        Ts.SyntaxKind.ForKeyword, "ForKeyword"
        Ts.SyntaxKind.FunctionKeyword, "FunctionKeyword"
        Ts.SyntaxKind.IfKeyword, "IfKeyword"
        Ts.SyntaxKind.ImportKeyword, "ImportKeyword"
        Ts.SyntaxKind.InKeyword, "InKeyword"
        Ts.SyntaxKind.InstanceOfKeyword, "InstanceOfKeyword"
        Ts.SyntaxKind.NewKeyword, "NewKeyword"
        Ts.SyntaxKind.NullKeyword, "NullKeyword"
        Ts.SyntaxKind.ReturnKeyword, "ReturnKeyword"
        Ts.SyntaxKind.SuperKeyword, "SuperKeyword"
        Ts.SyntaxKind.SwitchKeyword, "SwitchKeyword"
        Ts.SyntaxKind.ThisKeyword, "ThisKeyword"
        Ts.SyntaxKind.ThrowKeyword, "ThrowKeyword"
        Ts.SyntaxKind.TrueKeyword, "TrueKeyword"
        Ts.SyntaxKind.TryKeyword, "TryKeyword"
        Ts.SyntaxKind.TypeOfKeyword, "TypeOfKeyword"
        Ts.SyntaxKind.VarKeyword, "VarKeyword"
        Ts.SyntaxKind.VoidKeyword, "VoidKeyword"
        Ts.SyntaxKind.WhileKeyword, "WhileKeyword"
        Ts.SyntaxKind.WithKeyword, "WithKeyword"
        Ts.SyntaxKind.ImplementsKeyword, "ImplementsKeyword"
        Ts.SyntaxKind.InterfaceKeyword, "InterfaceKeyword"
        Ts.SyntaxKind.LetKeyword, "LetKeyword"
        Ts.SyntaxKind.PackageKeyword, "PackageKeyword"
        Ts.SyntaxKind.PrivateKeyword, "PrivateKeyword"
        Ts.SyntaxKind.ProtectedKeyword, "ProtectedKeyword"
        Ts.SyntaxKind.PublicKeyword, "PublicKeyword"
        Ts.SyntaxKind.StaticKeyword, "StaticKeyword"
        Ts.SyntaxKind.YieldKeyword, "YieldKeyword"
        Ts.SyntaxKind.AbstractKeyword, "AbstractKeyword"
        Ts.SyntaxKind.AccessorKeyword, "AccessorKeyword"
        Ts.SyntaxKind.AsKeyword, "AsKeyword"
        Ts.SyntaxKind.AssertsKeyword, "AssertsKeyword"
        Ts.SyntaxKind.AssertKeyword, "AssertKeyword"
        Ts.SyntaxKind.AnyKeyword, "AnyKeyword"
        Ts.SyntaxKind.AsyncKeyword, "AsyncKeyword"
        Ts.SyntaxKind.AwaitKeyword, "AwaitKeyword"
        Ts.SyntaxKind.BooleanKeyword, "BooleanKeyword"
        Ts.SyntaxKind.ConstructorKeyword, "ConstructorKeyword"
        Ts.SyntaxKind.DeclareKeyword, "DeclareKeyword"
        Ts.SyntaxKind.GetKeyword, "GetKeyword"
        Ts.SyntaxKind.InferKeyword, "InferKeyword"
        Ts.SyntaxKind.IntrinsicKeyword, "IntrinsicKeyword"
        Ts.SyntaxKind.IsKeyword, "IsKeyword"
        Ts.SyntaxKind.KeyOfKeyword, "KeyOfKeyword"
        Ts.SyntaxKind.ModuleKeyword, "ModuleKeyword"
        Ts.SyntaxKind.NamespaceKeyword, "NamespaceKeyword"
        Ts.SyntaxKind.NeverKeyword, "NeverKeyword"
        Ts.SyntaxKind.OutKeyword, "OutKeyword"
        Ts.SyntaxKind.ReadonlyKeyword, "ReadonlyKeyword"
        Ts.SyntaxKind.RequireKeyword, "RequireKeyword"
        Ts.SyntaxKind.NumberKeyword, "NumberKeyword"
        Ts.SyntaxKind.ObjectKeyword, "ObjectKeyword"
        Ts.SyntaxKind.SatisfiesKeyword, "SatisfiesKeyword"
        Ts.SyntaxKind.SetKeyword, "SetKeyword"
        Ts.SyntaxKind.StringKeyword, "StringKeyword"
        Ts.SyntaxKind.SymbolKeyword, "SymbolKeyword"
        Ts.SyntaxKind.TypeKeyword, "TypeKeyword"
        Ts.SyntaxKind.UndefinedKeyword, "UndefinedKeyword"
        Ts.SyntaxKind.UniqueKeyword, "UniqueKeyword"
        Ts.SyntaxKind.UnknownKeyword, "UnknownKeyword"
        Ts.SyntaxKind.UsingKeyword, "UsingKeyword"
        Ts.SyntaxKind.FromKeyword, "FromKeyword"
        Ts.SyntaxKind.GlobalKeyword, "GlobalKeyword"
        Ts.SyntaxKind.BigIntKeyword, "BigIntKeyword"
        Ts.SyntaxKind.OverrideKeyword, "OverrideKeyword"
        Ts.SyntaxKind.OfKeyword, "OfKeyword"
        Ts.SyntaxKind.QualifiedName, "QualifiedName"
        Ts.SyntaxKind.ComputedPropertyName, "ComputedPropertyName"
        Ts.SyntaxKind.TypeParameter, "TypeParameter"
        Ts.SyntaxKind.Parameter, "Parameter"
        Ts.SyntaxKind.Decorator, "Decorator"
        Ts.SyntaxKind.PropertySignature, "PropertySignature"
        Ts.SyntaxKind.PropertyDeclaration, "PropertyDeclaration"
        Ts.SyntaxKind.MethodSignature, "MethodSignature"
        Ts.SyntaxKind.MethodDeclaration, "MethodDeclaration"
        Ts.SyntaxKind.ClassStaticBlockDeclaration, "ClassStaticBlockDeclaration"
        Ts.SyntaxKind.Constructor, "Constructor"
        Ts.SyntaxKind.GetAccessor, "GetAccessor"
        Ts.SyntaxKind.SetAccessor, "SetAccessor"
        Ts.SyntaxKind.CallSignature, "CallSignature"
        Ts.SyntaxKind.ConstructSignature, "ConstructSignature"
        Ts.SyntaxKind.IndexSignature, "IndexSignature"
        Ts.SyntaxKind.TypePredicate, "TypePredicate"
        Ts.SyntaxKind.TypeReference, "TypeReference"
        Ts.SyntaxKind.FunctionType, "FunctionType"
        Ts.SyntaxKind.ConstructorType, "ConstructorType"
        Ts.SyntaxKind.TypeQuery, "TypeQuery"
        Ts.SyntaxKind.TypeLiteral, "TypeLiteral"
        Ts.SyntaxKind.ArrayType, "ArrayType"
        Ts.SyntaxKind.TupleType, "TupleType"
        Ts.SyntaxKind.OptionalType, "OptionalType"
        Ts.SyntaxKind.RestType, "RestType"
        Ts.SyntaxKind.UnionType, "UnionType"
        Ts.SyntaxKind.IntersectionType, "IntersectionType"
        Ts.SyntaxKind.ConditionalType, "ConditionalType"
        Ts.SyntaxKind.InferType, "InferType"
        Ts.SyntaxKind.ParenthesizedType, "ParenthesizedType"
        Ts.SyntaxKind.ThisType, "ThisType"
        Ts.SyntaxKind.TypeOperator, "TypeOperator"
        Ts.SyntaxKind.IndexedAccessType, "IndexedAccessType"
        Ts.SyntaxKind.MappedType, "MappedType"
        Ts.SyntaxKind.LiteralType, "LiteralType"
        Ts.SyntaxKind.NamedTupleMember, "NamedTupleMember"
        Ts.SyntaxKind.TemplateLiteralType, "TemplateLiteralType"
        Ts.SyntaxKind.TemplateLiteralTypeSpan, "TemplateLiteralTypeSpan"
        Ts.SyntaxKind.ImportType, "ImportType"
        Ts.SyntaxKind.ObjectBindingPattern, "ObjectBindingPattern"
        Ts.SyntaxKind.ArrayBindingPattern, "ArrayBindingPattern"
        Ts.SyntaxKind.BindingElement, "BindingElement"
        Ts.SyntaxKind.ArrayLiteralExpression, "ArrayLiteralExpression"
        Ts.SyntaxKind.ObjectLiteralExpression, "ObjectLiteralExpression"
        Ts.SyntaxKind.PropertyAccessExpression, "PropertyAccessExpression"
        Ts.SyntaxKind.ElementAccessExpression, "ElementAccessExpression"
        Ts.SyntaxKind.CallExpression, "CallExpression"
        Ts.SyntaxKind.NewExpression, "NewExpression"
        Ts.SyntaxKind.TaggedTemplateExpression, "TaggedTemplateExpression"
        Ts.SyntaxKind.TypeAssertionExpression, "TypeAssertionExpression"
        Ts.SyntaxKind.ParenthesizedExpression, "ParenthesizedExpression"
        Ts.SyntaxKind.FunctionExpression, "FunctionExpression"
        Ts.SyntaxKind.ArrowFunction, "ArrowFunction"
        Ts.SyntaxKind.DeleteExpression, "DeleteExpression"
        Ts.SyntaxKind.TypeOfExpression, "TypeOfExpression"
        Ts.SyntaxKind.VoidExpression, "VoidExpression"
        Ts.SyntaxKind.AwaitExpression, "AwaitExpression"
        Ts.SyntaxKind.PrefixUnaryExpression, "PrefixUnaryExpression"
        Ts.SyntaxKind.PostfixUnaryExpression, "PostfixUnaryExpression"
        Ts.SyntaxKind.BinaryExpression, "BinaryExpression"
        Ts.SyntaxKind.ConditionalExpression, "ConditionalExpression"
        Ts.SyntaxKind.TemplateExpression, "TemplateExpression"
        Ts.SyntaxKind.YieldExpression, "YieldExpression"
        Ts.SyntaxKind.SpreadElement, "SpreadElement"
        Ts.SyntaxKind.ClassExpression, "ClassExpression"
        Ts.SyntaxKind.OmittedExpression, "OmittedExpression"
        Ts.SyntaxKind.ExpressionWithTypeArguments, "ExpressionWithTypeArguments"
        Ts.SyntaxKind.AsExpression, "AsExpression"
        Ts.SyntaxKind.NonNullExpression, "NonNullExpression"
        Ts.SyntaxKind.MetaProperty, "MetaProperty"
        Ts.SyntaxKind.SyntheticExpression, "SyntheticExpression"
        Ts.SyntaxKind.SatisfiesExpression, "SatisfiesExpression"
        Ts.SyntaxKind.TemplateSpan, "TemplateSpan"
        Ts.SyntaxKind.SemicolonClassElement, "SemicolonClassElement"
        Ts.SyntaxKind.Block, "Block"
        Ts.SyntaxKind.EmptyStatement, "EmptyStatement"
        Ts.SyntaxKind.VariableStatement, "VariableStatement"
        Ts.SyntaxKind.ExpressionStatement, "ExpressionStatement"
        Ts.SyntaxKind.IfStatement, "IfStatement"
        Ts.SyntaxKind.DoStatement, "DoStatement"
        Ts.SyntaxKind.WhileStatement, "WhileStatement"
        Ts.SyntaxKind.ForStatement, "ForStatement"
        Ts.SyntaxKind.ForInStatement, "ForInStatement"
        Ts.SyntaxKind.ForOfStatement, "ForOfStatement"
        Ts.SyntaxKind.ContinueStatement, "ContinueStatement"
        Ts.SyntaxKind.BreakStatement, "BreakStatement"
        Ts.SyntaxKind.ReturnStatement, "ReturnStatement"
        Ts.SyntaxKind.WithStatement, "WithStatement"
        Ts.SyntaxKind.SwitchStatement, "SwitchStatement"
        Ts.SyntaxKind.LabeledStatement, "LabeledStatement"
        Ts.SyntaxKind.ThrowStatement, "ThrowStatement"
        Ts.SyntaxKind.TryStatement, "TryStatement"
        Ts.SyntaxKind.DebuggerStatement, "DebuggerStatement"
        Ts.SyntaxKind.VariableDeclaration, "VariableDeclaration"
        Ts.SyntaxKind.VariableDeclarationList, "VariableDeclarationList"
        Ts.SyntaxKind.FunctionDeclaration, "FunctionDeclaration"
        Ts.SyntaxKind.ClassDeclaration, "ClassDeclaration"
        Ts.SyntaxKind.InterfaceDeclaration, "InterfaceDeclaration"
        Ts.SyntaxKind.TypeAliasDeclaration, "TypeAliasDeclaration"
        Ts.SyntaxKind.EnumDeclaration, "EnumDeclaration"
        Ts.SyntaxKind.ModuleDeclaration, "ModuleDeclaration"
        Ts.SyntaxKind.ModuleBlock, "ModuleBlock"
        Ts.SyntaxKind.CaseBlock, "CaseBlock"
        Ts.SyntaxKind.NamespaceExportDeclaration, "NamespaceExportDeclaration"
        Ts.SyntaxKind.ImportEqualsDeclaration, "ImportEqualsDeclaration"
        Ts.SyntaxKind.ImportDeclaration, "ImportDeclaration"
        Ts.SyntaxKind.ImportClause, "ImportClause"
        Ts.SyntaxKind.NamespaceImport, "NamespaceImport"
        Ts.SyntaxKind.NamedImports, "NamedImports"
        Ts.SyntaxKind.ImportSpecifier, "ImportSpecifier"
        Ts.SyntaxKind.ExportAssignment, "ExportAssignment"
        Ts.SyntaxKind.ExportDeclaration, "ExportDeclaration"
        Ts.SyntaxKind.NamedExports, "NamedExports"
        Ts.SyntaxKind.NamespaceExport, "NamespaceExport"
        Ts.SyntaxKind.ExportSpecifier, "ExportSpecifier"
        Ts.SyntaxKind.MissingDeclaration, "MissingDeclaration"
        Ts.SyntaxKind.ExternalModuleReference, "ExternalModuleReference"
        Ts.SyntaxKind.JsxElement, "JsxElement"
        Ts.SyntaxKind.JsxSelfClosingElement, "JsxSelfClosingElement"
        Ts.SyntaxKind.JsxOpeningElement, "JsxOpeningElement"
        Ts.SyntaxKind.JsxClosingElement, "JsxClosingElement"
        Ts.SyntaxKind.JsxFragment, "JsxFragment"
        Ts.SyntaxKind.JsxOpeningFragment, "JsxOpeningFragment"
        Ts.SyntaxKind.JsxClosingFragment, "JsxClosingFragment"
        Ts.SyntaxKind.JsxAttribute, "JsxAttribute"
        Ts.SyntaxKind.JsxAttributes, "JsxAttributes"
        Ts.SyntaxKind.JsxSpreadAttribute, "JsxSpreadAttribute"
        Ts.SyntaxKind.JsxExpression, "JsxExpression"
        Ts.SyntaxKind.JsxNamespacedName, "JsxNamespacedName"
        Ts.SyntaxKind.CaseClause, "CaseClause"
        Ts.SyntaxKind.DefaultClause, "DefaultClause"
        Ts.SyntaxKind.HeritageClause, "HeritageClause"
        Ts.SyntaxKind.CatchClause, "CatchClause"
        Ts.SyntaxKind.AssertClause, "AssertClause"
        Ts.SyntaxKind.AssertEntry, "AssertEntry"
        Ts.SyntaxKind.ImportTypeAssertionContainer, "ImportTypeAssertionContainer"
        Ts.SyntaxKind.PropertyAssignment, "PropertyAssignment"
        Ts.SyntaxKind.ShorthandPropertyAssignment, "ShorthandPropertyAssignment"
        Ts.SyntaxKind.SpreadAssignment, "SpreadAssignment"
        Ts.SyntaxKind.EnumMember, "EnumMember"
        Ts.SyntaxKind.SourceFile, "SourceFile"
        Ts.SyntaxKind.Bundle, "Bundle"
        Ts.SyntaxKind.JSDocTypeExpression, "JSDocTypeExpression"
        Ts.SyntaxKind.JSDocNameReference, "JSDocNameReference"
        Ts.SyntaxKind.JSDocMemberName, "JSDocMemberName"
        Ts.SyntaxKind.JSDocAllType, "JSDocAllType"
        Ts.SyntaxKind.JSDocUnknownType, "JSDocUnknownType"
        Ts.SyntaxKind.JSDocNullableType, "JSDocNullableType"
        Ts.SyntaxKind.JSDocNonNullableType, "JSDocNonNullableType"
        Ts.SyntaxKind.JSDocOptionalType, "JSDocOptionalType"
        Ts.SyntaxKind.JSDocFunctionType, "JSDocFunctionType"
        Ts.SyntaxKind.JSDocVariadicType, "JSDocVariadicType"
        Ts.SyntaxKind.JSDocNamepathType, "JSDocNamepathType"
        Ts.SyntaxKind.JSDoc, "JSDoc"
        Ts.SyntaxKind.JSDocText, "JSDocText"
        Ts.SyntaxKind.JSDocTypeLiteral, "JSDocTypeLiteral"
        Ts.SyntaxKind.JSDocSignature, "JSDocSignature"
        Ts.SyntaxKind.JSDocLink, "JSDocLink"
        Ts.SyntaxKind.JSDocLinkCode, "JSDocLinkCode"
        Ts.SyntaxKind.JSDocLinkPlain, "JSDocLinkPlain"
        Ts.SyntaxKind.JSDocTag, "JSDocTag"
        Ts.SyntaxKind.JSDocAugmentsTag, "JSDocAugmentsTag"
        Ts.SyntaxKind.JSDocImplementsTag, "JSDocImplementsTag"
        Ts.SyntaxKind.JSDocAuthorTag, "JSDocAuthorTag"
        Ts.SyntaxKind.JSDocDeprecatedTag, "JSDocDeprecatedTag"
        Ts.SyntaxKind.JSDocClassTag, "JSDocClassTag"
        Ts.SyntaxKind.JSDocPublicTag, "JSDocPublicTag"
        Ts.SyntaxKind.JSDocPrivateTag, "JSDocPrivateTag"
        Ts.SyntaxKind.JSDocProtectedTag, "JSDocProtectedTag"
        Ts.SyntaxKind.JSDocReadonlyTag, "JSDocReadonlyTag"
        Ts.SyntaxKind.JSDocOverrideTag, "JSDocOverrideTag"
        Ts.SyntaxKind.JSDocCallbackTag, "JSDocCallbackTag"
        Ts.SyntaxKind.JSDocOverloadTag, "JSDocOverloadTag"
        Ts.SyntaxKind.JSDocEnumTag, "JSDocEnumTag"
        Ts.SyntaxKind.JSDocParameterTag, "JSDocParameterTag"
        Ts.SyntaxKind.JSDocReturnTag, "JSDocReturnTag"
        Ts.SyntaxKind.JSDocThisTag, "JSDocThisTag"
        Ts.SyntaxKind.JSDocTypeTag, "JSDocTypeTag"
        Ts.SyntaxKind.JSDocTemplateTag, "JSDocTemplateTag"
        Ts.SyntaxKind.JSDocTypedefTag, "JSDocTypedefTag"
        Ts.SyntaxKind.JSDocSeeTag, "JSDocSeeTag"
        Ts.SyntaxKind.JSDocPropertyTag, "JSDocPropertyTag"
        Ts.SyntaxKind.JSDocThrowsTag, "JSDocThrowsTag"
        Ts.SyntaxKind.JSDocSatisfiesTag, "JSDocSatisfiesTag"
        Ts.SyntaxKind.SyntaxList, "SyntaxList"
        Ts.SyntaxKind.NotEmittedStatement, "NotEmittedStatement"
        Ts.SyntaxKind.PartiallyEmittedExpression, "PartiallyEmittedExpression"
        Ts.SyntaxKind.CommaListExpression, "CommaListExpression"
        Ts.SyntaxKind.SyntheticReferenceExpression, "SyntheticReferenceExpression"
        Ts.SyntaxKind.Count, "Count"
        Ts.SyntaxKind.DeferKeyword, "DeferKeyword"
        Ts.SyntaxKind.JSDocImportTag, "JSDocImportTag"
        Ts.SyntaxKind.NotEmittedTypeElement, "NotEmittedTypeElement"
    |]
    
    let syntaxKindCounter = SyntaxKindSymbolCounter.create "SyntaxKind" (fun _ -> true) id kinds
    let syntaxKindCounterEmbedded = SyntaxKindSymbolCounter.create "SyntaxKindEmbedded" (fun _ -> true) id kinds
    let syntaxTypeCounter = SyntaxKindSymbolCounter.create "SyntaxType" (fun _ -> true) id kinds
    afterTests "Adjust embedded syntax kind counter; we're only interested in real differences" <| fun _ ->
        syntaxKindCounter
        |> SyntaxKindSymbolCounter.getFullResults
        |> Array.filter (function
            | { count = count; symbolCount = symbolCount } when count = symbolCount && symbolCount > 0 -> true
            | _ -> false
            )
        |> Array.iter (fun result ->
            syntaxKindCounterEmbedded.count[enum result.kind] <- 0
            syntaxKindCounterEmbedded.symbolCount[enum result.kind] <- 0
            )
    afterTests "Print typeFlags" <| fun _ ->
        SyntaxKindSymbolCounter.printResults syntaxKindCounter
        |> printfn "%s\n"
        
        "This demonstrates what percentage of nodes (that don't ALWAYS return a symbol when the checker acts on the node\n \
        itself) have the symbol embedded/inlined in the node. This is useful for understanding when a node that has an\n \
        optional identifier (like a parameter) has a symbol and where to access if unable to use the checker."
        |> printfn "\n%s"
        SyntaxKindSymbolCounter.printResults syntaxKindCounterEmbedded
        |> printfn "%s\n"
        
        SyntaxKindSymbolCounter.printResults syntaxTypeCounter
        |> printfn "%s\n"
        
        SyntaxKindSymbolCounter.getNodeDocumentationResults syntaxKindCounter
        |> Seq.filter (snd >> Array.isEmpty >> not)
        |> Seq.map (fun (kind, documentation) ->
            [
                sprintf "SyntaxKind: %s" kind
                for kind, count in documentation do
                    sprintf "\t%s: %d" kind count
            ]
            |> String.concat "\n"
            )
        |> String.concat "\n\n"
        |> printfn "%s"
        SyntaxKindSymbolCounter.getNodeDocumentationResults syntaxKindCounter
        |> Seq.filter (snd >> Array.isEmpty >> not)
        |> Seq.collect (snd >> Array.map fst)
        |> Seq.distinct
        |> Seq.toArray
        |> Array.iter (printfn "%s")
    afterTests "Generate Patterns" <| fun _ ->
        let makePattern name counter =
            let results = SyntaxKindSymbolCounter.getFullResults counter
            let matching, partial = results |> Array.partition (fun result -> result.symbolCount > 0 && result.symbolCount = result.count)
            let empty, partial = partial |> Array.partition (fun result -> result.symbolCount = 0)
            let _, empty = empty |> Array.partition (fun result -> JS.isNaN result.percent)
            let inline push spaces (str: string) = str.PadLeft(str.Length + spaces, ' ')
            let hasMatching = Array.isEmpty matching |> not
            let hasPartial = Array.isEmpty partial |> not
            let hasEmpty = Array.isEmpty empty |> not
            [
                push 4 $"module {name} ="
                if hasMatching then
                    push 8 "let private alwaysMask ="
                    yield!
                        matching
                        |> Array.mapi (fun i result ->
                            result.name
                            |> if i = 0 then sprintf "Ts.SyntaxKind.%s"
                                else sprintf "||| Ts.SyntaxKind.%s"
                            |> push 12)
                if hasPartial then
                    push 8 "let private someMask ="
                    yield!
                        partial
                        |> Array.mapi (fun i result ->
                            result.name
                            |> if i = 0 then sprintf "Ts.SyntaxKind.%s"
                                else sprintf "||| Ts.SyntaxKind.%s"
                            |> push 12)
                if hasEmpty then
                    push 8 "let private neverMask ="
                    yield!
                        empty
                        |> Array.mapi (fun i result ->
                            result.name
                            |> if i = 0 then sprintf "Ts.SyntaxKind.%s"
                                else sprintf "||| Ts.SyntaxKind.%s"
                            |> push 12)
                [
                    if hasMatching then "Always"
                    if hasPartial then "Some"
                    "None"
                ]
                |> String.concat "|"
                |> sprintf "let (|%s|) (node: Ts.Node) ="
                |> push 8
                [
                    if hasMatching then "node.kind &&& alwaysMask = node.kind then Always()"
                    if hasPartial then "node.kind &&& someMask = node.kind then Some()"
                ]
                |> String.concat ("\n" + push 12 "elif ")
                |> sprintf "if %s"
                |> push 12
                if hasMatching || hasPartial then
                    push 12 "else None()"
                else push 12 "None()"
            ]
            |> String.concat "\n"
        (makePattern "HasSymbol" syntaxKindCounter, makePattern "HasSymbolEmbedded" syntaxKindCounterEmbedded, makePattern "HasType" syntaxTypeCounter)
        |||> printfn "module SyntaxKind =\n%s\n%s\n%s"
    runner.testCase "SyntaxKind Discovery" <| fun _ ctx ->
        ctx.Nodes
        |> Array.iter (fun node ->
            SyntaxKindSymbolCounter.addNodeDocumentation syntaxKindCounter node
            if ctx.Checker.getSymbolAtLocation node |> Option.isSome then
                SyntaxKindSymbolCounter.incBothCounts syntaxKindCounter node.kind
            else SyntaxKindSymbolCounter.incCount syntaxKindCounter node.kind
            
            if node?symbol |> Option.ofObj |> Option.isSome then
                SyntaxKindSymbolCounter.incBothCounts syntaxKindCounterEmbedded node.kind
            else SyntaxKindSymbolCounter.incCount syntaxKindCounterEmbedded node.kind
            
            if try ctx.Checker.getTypeAtLocation node |> Option.ofObj |> Option.isSome
               with _ -> false
            then SyntaxKindSymbolCounter.incBothCounts syntaxTypeCounter node.kind
            else SyntaxKindSymbolCounter.incCount syntaxTypeCounter node.kind
            )
            
let inline modifierDiscovery (runner: Spec.RunnerContext) = runner.testSuite "Modifier Discovery" <| fun _ ->
    let modifiers = HashSet<string>()
    let modifierKeywords = HashSet<Node.ModifierKeyword>()
    afterTests "Print modifier list" <| fun _ ->
        modifiers
        |> Seq.toArray
        |> printfn "%A"
        modifierKeywords
        |> Seq.toArray
        |> Array.map _.ToString()
        |> printfn "%A"
    runner.testCase "Modifier Discovery" <| fun _ ctx ->
        ctx.Nodes
        |> Array.filter ts.canHaveModifiers
        |> Array.choose (ts.getModifiers >> Option.map _.AsArray)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.collect id
            >> Array.map _.kind.Name
            >> Array.distinct
            >> Array.iter (modifiers.Add >> ignore)
            )
    runner.testCase "Modifier wrapper" <| fun _ ctx ->
        ctx.Nodes
        |> Array.filter ts.canHaveModifiers
        |> Array.choose (ts.getModifiers >> Option.map _.AsArray)
        |> Chain.Expect.skipIfEmpty
        |> Option.iter (
            Array.collect id
            >> Array.map Node.ModifierKeyword.create
            >> Array.distinct
            >> Array.iter (modifierKeywords.Add >> ignore)
            )

let inline documentationDiscovery (runner: Spec.RunnerContext) = runner.testSuite "Documentation Discovery" <| fun _ ->
    let properties = ResizeArray<string>()
    let tagNames = HashSet<string * string>()
    afterTests "Print properties" <| fun _ ->
        properties.AsArray
        |> printfn "%A"
        tagNames
        |> Seq.toArray
        |> Array.iter (printfn "%A")
    runner.testCase "Documentation Discovery" <| fun _ ctx ->
        ctx.Symbols.Value
        |> Array.collect (_.getDocumentationComment(Some ctx.Checker).AsArray >> Array.collect (JS.Constructors.Object.getOwnPropertyNames >> _.AsArray))
        |> Array.distinct
        |> Array.iter (properties.Add >> ignore)
        ctx.Nodes
        |> Array.choose (Node.JSDoc.collectForNode ctx.Program)
        |> Array.collect _.Values
        |> Array.map (fun tag -> tag.ToString(), Node.JSDoc.getTag tag)
        |> Array.iter (tagNames.Add >> ignore)
        ctx.Nodes
        |> Array.choose (Node.JSDoc.collectForNode ctx.Program)
        |> Array.collect _.Values
        |> Array.choose (fun jsDoc ->
            Node.JSDoc.getComment ctx.Program jsDoc
            |> Option.filter (_.Values >> Array.exists (_.IsText >> not) )
            |> Option.map (fun c -> jsDoc.ToString(), c)
            )
        |> Array.iter (fun (tag, comments) ->
            printfn "JSDocTag: %s\n%A" tag comments
            )
