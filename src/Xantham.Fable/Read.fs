[<AutoOpen>]
module Xantham.Fable.Main

open System.Collections.Generic
open System.Text.RegularExpressions
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop
open Fable.Core
open Node
open Thoth.Json
open TypeScript
open Xantham
open System.Collections
open Xantham.Fable.Reading
open Xantham.Fable.Reading.Entry
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer

module Internal =
    open Glutinum.Chalk
    type IRResult<'T> = {
        Identity: IdentityKey
        Key: TypeKey
        Node: 'T
    }
    type IRExportResult = IRResult<TsExportDeclaration>
    type IRRTypeResult = IRResult<TsType>
    type IRResultDuplicates<'T> = {
        Key: TypeKey
        Results: IRResult<'T> array
    }
    module Log =
        let prefix = chalk.redBright.Invoke "[CIRCREF]"
        let inline healthCheckError<'T> (typeKey: TypeKey) (typ: 'T) (o: obj) = Log.emit $"{prefix} - { chalk.yellowBright.Invoke typeof<'T>.Name}: {o} references itself [{chalk.yellowBright.Invoke typeKey}]"
            
    let rec healthCheckType (typKey: TypeKey) (node: TsType) =
        match node with
        | TsType.TemplateLiteral node ->
            if node.Types |> List.contains typKey then
                Log.healthCheckError typKey node node
        | TsType.GlobalThis -> ()
        | TsType.Tuple tsTuple ->
            tsTuple.Types
            |> List.iter (function
                | TsTupleElement.Variadic key
                | TsTupleElement.Fixed { Type = key }
                | TsTupleElement.FixedLabeled(_, { Type = key }) ->
                    if key = typKey then
                        Log.healthCheckError typKey tsTuple tsTuple
                )
        | TsType.Interface tsInterface -> ()
        | TsType.Primitive typeKindPrimitive -> ()
        | TsType.Predicate tsTypePredicate -> ()
        | TsType.Literal tsLiteral -> ()
        | TsType.TypeLiteral tsTypeLiteral -> ()
        | TsType.TypeParameter tsTypeParameter ->
            if Some typKey = tsTypeParameter.Constraint then Log.healthCheckError typKey tsTypeParameter tsTypeParameter.Name
            elif Some typKey = tsTypeParameter.Default then Log.healthCheckError typKey tsTypeParameter tsTypeParameter.Name
        | TsType.Index tsIndex -> if typKey = tsIndex.Type then Log.healthCheckError typKey tsIndex tsIndex
        | TsType.TypeReference tsTypeReference ->
            if typKey = tsTypeReference.Type then Log.healthCheckError typKey tsTypeReference tsTypeReference
            elif Some typKey = tsTypeReference.ResolvedType then Log.healthCheckError typKey tsTypeReference tsTypeReference
        | TsType.Enum tsEnumType -> ()
        | TsType.EnumCase tsEnumCase -> ()
        | TsType.Substitution tsSubstitutionType -> ()
        | TsType.Conditional tsConditionalType ->
            if typKey = tsConditionalType.Check then Log.healthCheckError typKey tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.Extends then Log.healthCheckError typKey tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.True then Log.healthCheckError typKey tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.False then Log.healthCheckError typKey tsConditionalType tsConditionalType
        | TsType.Class tsClass -> ()
        | TsType.Union tsTypeUnion ->
            if tsTypeUnion.Types |> List.contains typKey then
                Log.healthCheckError typKey tsTypeUnion tsTypeUnion
        | TsType.Intersection tsTypeIntersection ->
            if tsTypeIntersection.Types |> List.contains typKey then
                Log.healthCheckError typKey tsTypeIntersection tsTypeIntersection
        | TsType.Optional tsTypeReference -> if typKey = tsTypeReference.Type then Log.healthCheckError typKey None tsTypeReference
        | TsType.IndexedAccess tsIndexAccessType ->
            if typKey = tsIndexAccessType.Object || typKey = tsIndexAccessType.Index
            then Log.healthCheckError typKey tsIndexAccessType tsIndexAccessType
        | TsType.ReadOnly tsType
        | TsType.Array tsType ->
            match tsType with
            | TsType.TypeReference { ResolvedType = Some key }
            | TsType.TypeReference { Type = key } when key = typKey -> Log.healthCheckError typKey tsType tsType
            | _ -> ()

    let private unknownKey = TypeKindPrimitive.Unknown.TypeKey
    
    /// Priority for choosing the winning entry when multiple IdentityKeys map to the same TypeKey.
    /// Lower number = higher priority.
    let identityPriority (identity: IdentityKey) =
        match identity with
        | IdentityKey.DeclarationPosition _ -> 0
        | IdentityKey.Symbol _              -> 1
        | IdentityKey.AliasSymbol _         -> 2
        | IdentityKey.Id _                  -> 3
        
    let private finaliseAssembly (results: IRResult<'T> array) =
        let duplicates, nonDuplicates =
            results
            |> Array.groupBy _.Key
            |> Array.map (fun (key, values) -> key, values |> Array.distinctBy _.Node)
            |> Array.partition (snd >> _.Length >> (<) 1)
        let duplicates =
            duplicates
            |> Array.map (fun (key, values) ->
                {
                    Key = key
                    Results = values |> Array.sortBy (_.Identity >> identityPriority)
                }
            )
        let nonDuplicates =
            nonDuplicates
            |> Array.map (snd >> Array.exactlyOne)
        {| DuplicateGroups = duplicates; NonDuplicates = nonDuplicates |}
    let assembleTypes (reader: TypeScriptReader) =
        Array.ofSeq <| seq {
            let keys =
                reader.signalCache.Values
                |> Seq.choose (fun kv -> if kv.Builder.Value.IsSome then Some kv.Key else None)
                |> Set
            for kv in reader.signalCache do
                let identityKey = kv.Key
                let { Key = typeKey; Builder = builder } = kv.Value
                match builder.Value with
                | ValueNone when keys.Contains typeKey |> not ->
                    let error (pos: obj) = (chalk.redBright.Invoke "[MISSREF]" + $" - TypeKey {chalk.yellowBright.Invoke typeKey} - Missing type builder value - {chalk.yellow.Invoke pos}") |> Log.emit
                    match identityKey with
                    | IdentityKey.DeclarationPosition(file, _, _) ->
                        error file
                    | IdentityKey.Symbol sym ->
                        error sym.name
                    | _ -> error $"%A{identityKey}"
                // another entry will have the filled out value
                | ValueNone -> ()
                | ValueSome builder ->
                    let tsType = builder.Build()
                    healthCheckType typeKey tsType
                    { Identity = identityKey; Key = typeKey; Node = tsType }
        }
        |> finaliseAssembly
    let assembleExports (reader: TypeScriptReader) =
        Array.ofSeq <| seq {
            let keys =
                reader.exportCache.Values
                |> Seq.choose (fun kv -> if kv.Builder.Value.IsSome then Some kv.RefKey else None)
                |> Set
            for kv in reader.exportCache do
                let identityKey = kv.Key
                let { RefKey = typeKey; Builder = builder } = kv.Value
                match builder.Value with
                | ValueNone when keys.Contains typeKey |> not -> 
                    let error (pos: obj) = (chalk.redBright.Invoke "[MISSREF]" + $" - RefKey {chalk.yellowBright.Invoke typeKey} - Missing export builder value - {chalk.yellow.Invoke pos}") |> Log.emit
                    match identityKey with
                    | IdentityKey.DeclarationPosition(file, _, _) ->
                        error file
                    | IdentityKey.Symbol sym ->
                        error sym.name
                    | _ -> error $"%A{identityKey}"
                | ValueNone -> ()
                | ValueSome builder ->
                    let tsExport = builder.Build()
                    { Identity = identityKey; Key = typeKey; Node = tsExport }
        }
        |> finaliseAssembly
    let assembleResults (reader: TypeScriptReader) =
        assembleTypes reader, assembleExports reader

    open Schema
    let trimTypeReferenceArrayTupleDuplicates (result: EncodedResult) =
        result.DuplicateTypes
        |> Map.partition (fun key duplicates ->
            match duplicates with
            | [ { Value = TsType.TypeReference _ }; { Value = comp } ]
            | [ { Value = comp }; { Value = TsType.TypeReference _ } ] -> comp.IsArray || comp.IsTuple
            | _ -> false
            )
        ||> fun reduced duplicates -> (
            reduced
            |> Map.fold (fun acc key value ->
                acc
                |> Map.change key (function
                    | None ->
                        match value with
                        | [ { Value = TsType.TypeReference _ }; { Value = comp } ] 
                        | [ { Value = comp }; { Value = TsType.TypeReference _ } ] -> Some comp
                        | [ { Value = value } ] -> Some value
                        | _ -> failwith "should be unreachable"
                    | Some _ -> failwith "Should be unreachable"
                    )
                ) result.Types
            , duplicates)
        ||> fun nonDupes dupes ->
            {
                result with
                    Types = nonDupes
                    DuplicateTypes = dupes
            }

    let private mergeableTypes (typ: TsType) =
        typ.IsInterface
        || typ.IsTypeLiteral
    let mergeTypes (result: EncodedResult) =
        result.DuplicateTypes
        |> Map.map (fun _ duplicates ->
            if duplicates.Length < 2 || duplicates[0].Value |> mergeableTypes |> not then duplicates else
            let winner = duplicates[0]
            let mergeable, nonmergeable =
                duplicates[1..]
                |> List.partition (_.Value >> mergeableTypes)

            let mergedWinner =
                match winner.Value with
                | TsType.Interface iface ->
                    { winner with
                        Value =
                            mergeable
                            |> List.fold (fun iface -> function
                                    | { Value = TsType.Interface otherface } ->
                                        { iface with TsInterface.Members = iface.Members @ otherface.Members }
                                    | _ -> iface
                                ) iface
                            |> TsType.Interface }
                | TsType.TypeLiteral typeLiteral ->
                    { winner with
                        Value =
                            mergeable
                            |> List.fold (fun typeLiteral -> function
                                | { Value = TsType.TypeLiteral otherTypeLiteral } ->
                                    { typeLiteral with TsTypeLiteral.Members = otherTypeLiteral.Members @ typeLiteral.Members }
                                | _ -> typeLiteral
                                ) typeLiteral
                            |> TsType.TypeLiteral }
                | _ -> winner
            mergedWinner :: nonmergeable
            )
        |> Map.partition (fun _ -> List.length >> (<) 1)
        ||> fun duplicates reduced ->
            {
                result with
                    Types =
                        reduced
                        |> Map.fold (fun acc key value ->
                            acc
                            |> Map.change key (function
                                | None -> value |> List.exactlyOne |> _.Value |> Some
                                | Some _ -> failwith "Should be unreachable"
                                )
                            ) result.Types
                    DuplicateTypes = duplicates
            }
    let private mergeableExports (exportedType: TsExportDeclaration) =
        exportedType.IsInterface
        || exportedType.IsFunction
    let mergeExports (result: EncodedResult) =
        result.DuplicateExports
        |> Map.map (fun _ duplicates ->
            if duplicates.Length < 2 || not(mergeableExports duplicates[0].Value) then duplicates else
            let winner = duplicates[0]
            let mergeable, nonmergeable =
                duplicates[1..]
                |> List.partition (_.Value >> mergeableExports)

            let mergedWinner =
                match winner.Value with
                | TsExportDeclaration.Interface iface ->
                    { winner with
                        Value =
                            mergeable
                            |> List.fold (fun iface -> function
                                    | { Value = TsExportDeclaration.Interface otherface } ->
                                        { iface with TsInterface.Members = iface.Members @ otherface.Members }
                                    | _ -> iface
                                ) iface
                            |> TsExportDeclaration.Interface }
                | TsExportDeclaration.Function func ->
                    { winner with
                          Value =
                              mergeable
                              |> List.fold (fun func -> function
                                  | { Value = TsExportDeclaration.Function otherFunc } ->
                                      TsOverloadableConstruct.Combine func otherFunc
                                  | _ -> func
                                  ) func
                              |> TsExportDeclaration.Function }
                | _ -> winner
            mergedWinner :: nonmergeable
            )
        |> Map.partition (fun _ -> List.length >> (<) 1)
        ||> fun duplicates reduced ->
            {
                result with
                    ExportedDeclarations =
                        reduced
                        |> Map.fold (fun acc key value ->
                            acc
                            |> Map.change key (function
                                | None -> value |> List.exactlyOne |> _.Value |> Some
                                | Some _ -> failwith "Should be unreachable"
                                )
                            ) result.ExportedDeclarations
                    DuplicateExports = duplicates
            }
    let writeOutput (destination: string)
                    (result: EncodedResult)  =
        let destination =
            if destination = null then
                path.join(__SOURCE_DIRECTORY__, "output.json")
            elif destination.EndsWith(".json") then
                destination
            else path.join(destination, "output.json")
        #if DEBUG
        // let json = Encode.Auto.toString(1, result)
        let json = EncodedResult.encode result |> Encode.toString 1
        #else
        // let json = Encode.Auto.toString(result)
        let json = EncodedResult.encode result |> Encode.toString 1
        #endif
        fs.writeFile(destination, json, None, callback = ignore)

    let initialise (reader: TypeScriptReader) =
        tagPrimitives reader

    let getAndPrepareExports (reader: TypeScriptReader) =
        reader
        |> _.program.getSourceFile(reader.entryFile).Value
        |> getDeclarations reader
        |> Array.apply (pushToStack reader)

    let runReader (reader: TypeScriptReader) =
        let mutable stackEntry = Unchecked.defaultof<XanthamTag>
        while reader.stack.TryPop(&stackEntry) do
            Dispatcher.dispatch reader stackEntry
        reader
    
    let selectAndMergeWinnersInDuplicates (result: EncodedResult) =
        let mergeWinnersInto (duplicates: Map<TypeKey, DuplicateEncoding<_> list>) (nonDupes: Map<TypeKey, _>) =
            duplicates
            |> Map.fold (fun acc key duplicates ->
                acc
                |> Map.change key (function
                    | None -> duplicates |> List.head |> _.Value |> Some
                    | Some value -> Some value
                    )
                ) nonDupes
            
        { result with
            Types = mergeWinnersInto result.DuplicateTypes result.Types
            ExportedDeclarations = mergeWinnersInto result.DuplicateExports result.ExportedDeclarations }
open Schema
let read (reader: TypeScriptReader) =
    let exportTags =
        Internal.initialise reader
        |> Internal.getAndPrepareExports
    let exportTagIdentities =
        exportTags
        |> Array.map _.IdentityKey
    let typeResults, exportResults =
        Internal.runReader reader
        |> Internal.assembleResults
    let typeIdentities =
        typeResults.NonDuplicates
        |> Array.map _.Identity
        |> Array.append (
            exportResults.NonDuplicates
            |> Array.map _.Identity
            )
        |> Array.append exportTagIdentities
        |> Array.distinct
    {
        ExportedDeclarations =
            exportResults.NonDuplicates
            |> Array.map (fun ir -> ir.Key, ir.Node)
            |> Map
        Types =
            typeResults.NonDuplicates
            |> Array.map (fun ir -> ir.Key, ir.Node)
            |> Map
        DuplicateExports =
            exportResults.DuplicateGroups
            |> Array.map (fun group ->
                group.Key,
                group.Results
                |> Array.map (fun ir ->
                    {
                        Identity =
                            match ir.Identity with
                            | IdentityKey.Id typeKey -> Type typeKey
                            | IdentityKey.AliasSymbol symbol -> Symbol symbol.name
                            | IdentityKey.Symbol symbol -> Symbol symbol.name
                            | IdentityKey.DeclarationPosition(file, pos, endPos) ->
                                DeclarationFile(file, int pos, int endPos)
                        Value = ir.Node
                    })
                |> Array.toList
                )
            |> Map
        DuplicateTypes =
            typeResults.DuplicateGroups
            |> Array.map (fun group ->
                group.Key,
                group.Results
                |> Array.map (fun ir ->
                    {
                        Identity =
                            match ir.Identity with
                            | IdentityKey.Id typeKey -> Type typeKey
                            | IdentityKey.AliasSymbol symbol -> Symbol symbol.name
                            | IdentityKey.Symbol symbol -> Symbol symbol.name
                            | IdentityKey.DeclarationPosition(file, pos, endPos) ->
                                DeclarationFile(file, int pos, int endPos)
                        Value = ir.Node
                    })
                |> Array.toList
                )
            |> Map
        TopLevelExports =
            exportTagIdentities
            |> Array.map (fun key ->
                reader.exportCache[key].RefKey
                )
            |> Array.distinct
            |> Array.toList
        LibEsExports =
            typeIdentities
            |> Array.filter reader.libCache.Contains
            |> Array.choose (fun key ->
                match reader.exportCache.TryGetValue(key) with
                | true, export -> Some export.RefKey
                | _ ->
                match reader.signalCache.TryGetValue(key) with
                | true, signal -> Some signal.Key
                | _ -> None
                )
            |> Array.toList
    }
    |> Internal.trimTypeReferenceArrayTupleDuplicates
    |> Internal.mergeExports
    |> Internal.selectAndMergeWinnersInDuplicates
let write (outputDestination: string) (result: EncodedResult) =
    Internal.writeOutput outputDestination result
let readAndWrite (outputDestination: string) (reader: TypeScriptReader) =
    read reader
    |> write outputDestination