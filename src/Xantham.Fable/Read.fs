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
    type IRResult = {
        Identity: IdentityKey
        Key: TypeKey
        Node: TsAstNode
    }
    type IRResultDuplicates = {
        Key: TypeKey
        Results: IRResult array
    }
    [<MeasureAnnotatedAbbreviation>] type IRResultDuplicates<[<Measure>] 'U> = IRResultDuplicates
    type [<Measure>] idPrioritySorted
    let inline prune (typ: IRResultDuplicates<_>): IRResultDuplicates = unbox typ
    let inline plant<[<Measure>] 'u> (typ: IRResultDuplicates): IRResultDuplicates<'u> = unbox typ
    let inline graft<[<Measure>] 'n>(typ: IRResultDuplicates<_>): IRResultDuplicates<'n> = unbox typ
    let inline (|IRResultDuplicates|) value = prune value
    module Log =
        let prefix = chalk.redBright.Invoke "[CIRCREF]"
        let inline healthCheckError<'T> (typ: 'T) (o: obj) = Log.emit $"{prefix} - { chalk.yellowBright.Invoke typeof<'T>.Name}: {o} references itself"
            
    let rec healthCheckNode (typKey: TypeKey) (node: TsAstNode) =
        match node with
        | TsAstNode.Alias alias ->
            if alias.Type = typKey then
                Log.healthCheckError alias alias.Name
            alias.TypeParameters
            |> List.map (snd >> TsAstNode.TypeParameter)
            |> List.iter (healthCheckNode typKey)
        | TsAstNode.TemplateLiteral node ->
            if node.Types |> List.contains typKey then
                Log.healthCheckError node node
        | TsAstNode.GlobalThis -> ()
        | TsAstNode.Tuple tsTuple ->
            tsTuple.Types
            |> List.iter (function
                | TsTupleElement.Variadic key
                | TsTupleElement.Fixed { Type = key }
                | TsTupleElement.FixedLabeled(_, { Type = key }) ->
                    if key = typKey then
                        Log.healthCheckError tsTuple tsTuple
                )
        | TsAstNode.Interface tsInterface -> ()
        | TsAstNode.Variable tsVariable ->
            if typKey = tsVariable.Type then Log.healthCheckError tsVariable tsVariable.Name
        | TsAstNode.Primitive typeKindPrimitive -> ()
        | TsAstNode.Predicate tsTypePredicate -> ()
        | TsAstNode.Literal tsLiteral -> ()
        | TsAstNode.TypeLiteral tsTypeLiteral -> ()
        | TsAstNode.Property tsProperty -> if typKey = tsProperty.Type then Log.healthCheckError tsProperty tsProperty.Name
        | TsAstNode.Parameter tsParameter -> if typKey = tsParameter.Type then Log.healthCheckError tsParameter tsParameter.Name
        | TsAstNode.TypeParameter tsTypeParameter ->
            if Some typKey = tsTypeParameter.Constraint then Log.healthCheckError tsTypeParameter tsTypeParameter.Name
            elif Some typKey = tsTypeParameter.Default then Log.healthCheckError tsTypeParameter tsTypeParameter.Name
        | TsAstNode.IndexAccessType tsIndexAccessType ->
            if typKey = tsIndexAccessType.Object then Log.healthCheckError tsIndexAccessType tsIndexAccessType
            elif typKey = tsIndexAccessType.Index then Log.healthCheckError tsIndexAccessType tsIndexAccessType
        | TsAstNode.FunctionDeclaration tsFunction -> for tsFunction in tsFunction.Values do if typKey = tsFunction.Type then Log.healthCheckError tsFunction tsFunction.Name
        | TsAstNode.Method tsMethod -> for tsMethod in tsMethod.Values do if typKey = tsMethod.Type then Log.healthCheckError tsMethod tsMethod.Name
        | TsAstNode.Constructor tsConstructor -> ()
        | TsAstNode.ConstructSignature tsConstructSignature -> ()
        | TsAstNode.IndexSignature tsIndexSignature -> if typKey = tsIndexSignature.Type then Log.healthCheckError tsIndexSignature tsIndexSignature
        | TsAstNode.Index tsIndex -> if typKey = tsIndex.Type then Log.healthCheckError tsIndex tsIndex
        | TsAstNode.TypeReference tsTypeReference ->
            if typKey = tsTypeReference.Type then Log.healthCheckError tsTypeReference tsTypeReference
            elif Some typKey = tsTypeReference.ResolvedType then Log.healthCheckError tsTypeReference tsTypeReference
        | TsAstNode.Array tsTypeReference ->
            if typKey = tsTypeReference.Type then Log.healthCheckError [] tsTypeReference
            elif Some typKey = tsTypeReference.ResolvedType then Log.healthCheckError [] tsTypeReference
        | TsAstNode.Enum tsEnumType -> ()
        | TsAstNode.EnumCase tsEnumCase -> ()
        | TsAstNode.SubstitutionType tsSubstitutionType -> ()
        | TsAstNode.Conditional tsConditionalType ->
            if typKey = tsConditionalType.Check then Log.healthCheckError tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.Extends then Log.healthCheckError tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.True then Log.healthCheckError tsConditionalType tsConditionalType
            elif typKey = tsConditionalType.False then Log.healthCheckError tsConditionalType tsConditionalType
        | TsAstNode.Class tsClass -> ()
        | TsAstNode.Union tsTypeUnion ->
            if tsTypeUnion.Types |> List.contains typKey then
                Log.healthCheckError tsTypeUnion tsTypeUnion
        | TsAstNode.Intersection tsTypeIntersection ->
            if tsTypeIntersection.Types |> List.contains typKey then
                Log.healthCheckError tsTypeIntersection tsTypeIntersection
        | TsAstNode.Optional tsTypeReference -> if typKey = tsTypeReference.Type then Log.healthCheckError None tsTypeReference
        | TsAstNode.GetAccessor tsGetAccessor -> if typKey = tsGetAccessor.Type then Log.healthCheckError tsGetAccessor tsGetAccessor
        | TsAstNode.SetAccessor tsSetAccessor -> if typKey = tsSetAccessor.ArgumentType then Log.healthCheckError tsSetAccessor tsSetAccessor
        | TsAstNode.CallSignature tsCallSignature -> for tsCallSignature in tsCallSignature.Values do if typKey = tsCallSignature.Type then Log.healthCheckError tsCallSignature tsCallSignature
        | TsAstNode.Module tsModule -> ()

    let private unknownKey = TypeKindPrimitive.Unknown.TypeKey

    /// Scan a built node for TypeKey fields set to Unknown (-14).
    /// Returns a list of (fieldName, value) pairs for any Unknown fields found.
    let private findUnknownFields (node: TsAstNode) : string list =
        let u = unknownKey
        match node with
        | TsAstNode.Alias a ->
            [ if a.Type = u then "Alias.Type" ]
        | TsAstNode.Variable v ->
            [ if v.Type = u then "Variable.Type" ]
        | TsAstNode.Property p ->
            [ if p.Type = u then "Property.Type" ]
        | TsAstNode.Method m ->
            [ for m in m.Values do if m.Type = u then "Method.Type" ]
        | TsAstNode.FunctionDeclaration f ->
            [ for f in f.Values do if f.Type = u then "FunctionDeclaration.Type" ]
        | TsAstNode.GetAccessor g ->
            [ if g.Type = u then "GetAccessor.Type" ]
        | TsAstNode.SetAccessor s ->
            [ if s.ArgumentType = u then "SetAccessor.ArgumentType" ]
        | TsAstNode.CallSignature cs ->
            [ for cs in cs.Values do if cs.Type = u then "CallSignature.Type" ]
        | TsAstNode.ConstructSignature cs ->
            [ for cs in cs.Values do if cs.Type = u then "ConstructSignature.Type" ]
        | TsAstNode.IndexSignature idx ->
            [ if idx.Type = u then "IndexSignature.Type" ]
        | TsAstNode.Index idx ->
            [ if idx.Type = u then "Index.Type" ]
        | TsAstNode.TypeReference tr ->
            [ if tr.Type = u then "TypeReference.Type"
              if tr.TypeArguments |> List.exists ((=) u) then "TypeReference.TypeArguments[]"
              if tr.ResolvedType = Some u then "TypeReference.ResolvedType" ]
        | TsAstNode.Array tr ->
            [ if tr.Type = u then "Array.Type"
              if tr.ResolvedType = Some u then "Array.ResolvedType" ]
        | TsAstNode.Optional tr ->
            [ if tr.Type = u then "Optional.Type" ]
        | TsAstNode.Union un ->
            [ if un.Types |> List.exists ((=) u) then "Union.Types[]" ]
        | TsAstNode.Intersection it ->
            [ if it.Types |> List.exists ((=) u) then "Intersection.Types[]" ]
        | TsAstNode.Conditional ct ->
            [ if ct.Check = u then "Conditional.Check"
              if ct.Extends = u then "Conditional.Extends"
              if ct.True = u then "Conditional.True"
              if ct.False = u then "Conditional.False" ]
        | TsAstNode.IndexAccessType ia ->
            [ if ia.Object = u then "IndexAccessType.Object"
              if ia.Index = u then "IndexAccessType.Index" ]
        | TsAstNode.TypeParameter tp ->
            [ if tp.Constraint = Some u then "TypeParameter.Constraint"
              if tp.Default = Some u then "TypeParameter.Default" ]
        | TsAstNode.Parameter p ->
            [ if p.Type = u then "Parameter.Type" ]
        | TsAstNode.TemplateLiteral tl ->
            [ if tl.Types |> List.exists ((=) u) then "TemplateLiteral.Types[]" ]
        | _ -> []

    let assembleResults (reader: TypeScriptReader) = Array.ofSeq <| seq {
         for kv in reader.signalCache do
             let identity = kv.Key
             let { Key = typeKey; Builder = builder } = kv.Value
             match builder.Value with
             | ValueNone ->
                 let error (pos: obj) = (chalk.redBright.Invoke "[MISSREF]" + $" - TypeKey {chalk.yellowBright.Invoke typeKey} - Missing builder value.\n         - {chalk.dim.yellowBright.Invoke pos}") |> Log.emit
                 match identity with
                 | IdentityKey.DeclarationPosition(file, pos, endPos) ->
                     match reader.program.getSourceFile(file) with
                     | Some file ->
                         let start = file.getLineAndCharacterOfPosition(pos)
                         let endPos = file.getLineAndCharacterOfPosition(endPos)
                         error $"%A{file.fileName} ({start.line},{start.character}) ({endPos.line},{endPos.character})"
                     | None ->
                         error $"{file} ({pos},{endPos})"
                 | IdentityKey.Symbol sym ->
                     error $"{sym.name}"
                 | IdentityKey.Id _ ->
                     // Anonymous types with no symbol or declaration — complex generic internals.
                     // These are expected for highly generic signatures (splitProps, mergeProps, etc.)
                     // and have no meaningful identity to report.
                     error $"%A{identity}"
                 | _ -> error $"%A{identity}"
             | ValueSome builder ->
                 let builtNode = builder.Build()
                 healthCheckNode typeKey builtNode
                 let unknownFields = findUnknownFields builtNode
                 if unknownFields.Length > 0 then
                     Log.emit <| chalk.dim.Invoke(
                         chalk.redBright.Invoke "[STUBREF]" + $" - TypeKey {chalk.yellowBright.Invoke typeKey} - Unknown fields: " + $"""{String.concat ", " unknownFields}"""
                         )
                 { Identity = identity
                   Key = typeKey
                   Node = builtNode }
        }

    /// Priority for choosing the winning entry when multiple IdentityKeys map to the same TypeKey.
    /// Lower number = higher priority.
    let identityPriority (identity: IdentityKey) =
        match identity with
        | IdentityKey.DeclarationPosition _ -> 0
        | IdentityKey.Symbol _              -> 1
        | IdentityKey.AliasSymbol _         -> 2
        | IdentityKey.Id _                  -> 3
    let sortResultGroups (groups: IRResultDuplicates seq) =
        groups
        |> Seq.map (fun group ->
            { group with Results = group.Results |> Array.sortBy (fun ir -> identityPriority ir.Identity) }
            |> graft<idPrioritySorted>)
    let mergeOverloads (groups: IRResultDuplicates<idPrioritySorted> seq): IRResultDuplicates<idPrioritySorted> seq =
        groups
        |> Seq.map (fun (IRResultDuplicates group) ->
            let sorted = group.Results
            let winner = sorted[0]
            if sorted |> Array.exists (fun ir -> ir.Node <> winner.Node) |> not then plant group else
            // merge
            let isOverloadable, nonOverloadable =
                match winner.Node with
                | TsAstNode.FunctionDeclaration _
                | TsAstNode.Method _
                | TsAstNode.Constructor _
                | TsAstNode.ConstructSignature _
                | TsAstNode.CallSignature _ ->
                    sorted
                    |> Array.partition (function
                        | { Node = node } when node <> winner.Node -> true
                        | _ -> false
                        )
                    |> (function
                        | [||], _ -> ValueNone, sorted
                        | values, sorted ->
                            values
                            |> Array.map _.Node
                            |> ValueSome, sorted |> Array.filter (fun { Node = node } -> node <> winner.Node))
                | _ -> ValueNone, sorted
            isOverloadable
            |> ValueOption.map (fun overloads ->
                let inline combine
                        (a: TsOverloadableConstruct<'T>)
                        (recreate: TsOverloadableConstruct<'T> -> TsAstNode)
                        (b: TsAstNode -> TsOverloadableConstruct<'T> option) =
                    seq { yield a; yield! overloads |> Seq.choose b }
                    |> TsOverloadableConstruct.Create
                    |> recreate
                match winner.Node with
                | TsAstNode.FunctionDeclaration node ->
                    function TsAstNode.FunctionDeclaration node -> Some node | _ -> None
                    |> combine node  TsAstNode.FunctionDeclaration
                | TsAstNode.Method node ->
                    function TsAstNode.Method node -> Some node | _ -> None
                    |> combine node  TsAstNode.Method
                | TsAstNode.Constructor node ->
                    function TsAstNode.Constructor node -> Some node | _ -> None
                    |> combine node  TsAstNode.Constructor
                | TsAstNode.ConstructSignature node ->
                    function TsAstNode.ConstructSignature node -> Some node | _ -> None
                    |> combine node  TsAstNode.ConstructSignature
                | TsAstNode.CallSignature node ->
                    function TsAstNode.CallSignature node -> Some node | _ -> None
                    |> combine node  TsAstNode.CallSignature
                | _ -> failwith "Should be unreachable"
                )
            |> ValueOption.map (fun merged ->
                { Key = group.Key; Results = Array.append [| { winner with Node = merged } |] nonOverloadable }
                |> plant
                )
            |> ValueOption.defaultValue (plant group)
            )
    let mergeMembersIntoWinner (groups: IRResultDuplicates<idPrioritySorted> seq): IRResultDuplicates<idPrioritySorted> seq =
        groups
        |> Seq.map (fun (IRResultDuplicates group) ->
            let sorted = group.Results
            let winner = sorted[0]
            match winner.Node with
            | TsAstNode.Interface tsInterface when sorted.Length > 1 ->
                let mergeAbleMembers, unmergeableMembers =
                    sorted[1..]
                    |> Array.partition (fun result ->
                        result.Node.IsInterface
                        && result.Identity.IsDeclarationPosition)
                    ||> (fun mergeable unmergeable ->
                        mergeable
                        |> Array.map (function
                            | { Node = TsAstNode.Interface nestedInterface } ->
                                nestedInterface.Members
                            | _ -> []
                            ), unmergeable
                        )
                mergeAbleMembers
                |> Array.fold (fun acc members ->
                    Set.union acc (Set members)
                    ) (Set tsInterface.Members)
                |> Set.toList
                |> fun members ->
                    [| { winner with Node = TsAstNode.Interface { tsInterface with Members = members } }
                       yield! unmergeableMembers |]
            | TsAstNode.Class tsClass when sorted.Length > 1 ->
                let mergeAbleMembers, unmergeableMembers =
                    sorted[1..]
                    |> Array.partition (fun result ->
                        result.Node.IsClass
                        && result.Identity.IsDeclarationPosition)
                    ||> (fun mergeable unmergeable ->
                        mergeable
                        |> Array.map (function
                            | { Node = TsAstNode.Class nestedClass } ->
                                nestedClass.Members
                            | _ -> []
                            ), unmergeable
                        )
                mergeAbleMembers
                |> Array.fold (fun acc members ->
                    Set.union acc (Set members)
                    ) (Set tsClass.Members)
                |> Set.toList
                |> fun members ->
                    [| { winner with Node = TsAstNode.Class { tsClass with Members = members } }
                       yield! unmergeableMembers |]
            | _ -> sorted
            |> fun sorted ->
                { group with Results = sorted }
                |> plant
            )
    /// For each group of entries sharing a TypeKey, select the highest-priority winner.
    /// Only logs groups where the winner's node differs from at least one other entry (genuine
    /// conflicts). Same-builder duplicates (lib overloads, Id shadows) are silently discarded.
    let resolveDuplicates (groups: IRResultDuplicates seq) : IRResult seq =
        seq {
            for group in groups do
                let sorted = group.Results |> Array.sortBy (fun ir -> identityPriority ir.Identity)
                let winner = sorted[0]
                let hasConflict = sorted |> Array.exists (fun ir -> ir.Node <> winner.Node)
                if hasConflict then
                    Log.emit <| chalk.redBright.Invoke "[COLLIDE]" + " - " + $"TypeKey {chalk.yellowBright.Invoke group.Key} - Duplicate builder values for the same key."
                    let inline emit (s: obj) = Log.emit $"  {s}"
                    for ir in sorted do
                        if ir.Identity = winner.Identity then
                            chalk.yellowBright.Invoke "[EMIT]" + $" - %A{ir.Identity}"
                            |> emit
                            $"%A{ir.Node}"
                            |> String.splitLines
                            |> Array.iter emit
                        else
                            chalk.gray.bold.Invoke "[DISC]" + $" - %A{chalk.gray.Invoke ir.Identity}"
                            |> emit
                            $"%A{chalk.dim.Invoke ir.Node}"
                            |> String.splitLines
                            |> Array.iter emit
                    emit <| chalk.dim.Invoke "----------------------------------------------------------------"
                winner
        }

    let exciseDuplicateKeys (results: IRResult array) =
        let count =
            results
            |> Seq.countBy _.Key
            |> Seq.map KeyValuePair
            |> Dictionary
        let duplicateGroups = seq {
            for { Key = key } in results |> Seq.distinctBy _.Key do
            if count[key] > 1 then
                { Key = key
                  Results = results |> Array.filter (fun ir -> ir.Key = key) }
        }
        let nonDuplicates = seq {
            for ir in results do
            if count[ir.Key] = 1 then ir
        }
        {| DuplicateGroups = duplicateGroups; NonDuplicates = nonDuplicates |}

        
    let filterConflictDuplicatesOnly (groups: IRResultDuplicates<idPrioritySorted> seq) =
        seq {
            for IRResultDuplicates group in groups do
                let sorted = group.Results
                let winner = sorted[0]
                let hasConflict = sorted |> Array.exists (fun ir -> ir.Node <> winner.Node)
                if hasConflict then
                    {| Key = group.Key
                       Losers = sorted |> Array.filter (fun ir -> ir.Node <> winner.Node)
                       Winner = winner |}
        }
    
    let writeOutput (destination: string)
                    (exports: XanthamTag array)
                    (duplicates: Dictionary<TypeKey, TsAstNode * TsAstNode array>)
                    (result: Dictionary<TypeKey, TsAstNode>)  =
        let destination =
            if destination = null then
                path.join(__SOURCE_DIRECTORY__, "output.json")
            elif destination.EndsWith(".json") then
                destination
            else path.join(destination, "output.json")
        let resultTuple =
            [| for kv in result do kv.Key, kv.Value |],
            [| for kv in duplicates do kv.Key, kv.Value |],
            [|
                for export in exports do
                    match
                        GuardedData.TypeSignal.get export
                        |> _.Value
                    with
                    | typeKey when typeKey <> TypeKey.Unknown -> typeKey
                    | _ -> ()
            |]
        #if DEBUG
        let json = Encode.Auto.toString(1, resultTuple)
        #else
        let json = Encode.Auto.toString(resultTuple)
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

let readAndWrite (outputDestination: string) (reader: TypeScriptReader) =
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
        |> Seq.iter (fun group ->
            splitMap.Add(group.Key, (group.Winner.Node, group.Losers |> Array.map _.Node))
            )
        
        Internal.resolveDuplicates (Seq.map Internal.prune mergedDuplicates)
        |> Seq.append split.NonDuplicates
        |> Seq.sortBy _.Key
        |> Seq.map (fun ir -> KeyValuePair(ir.Key, ir.Node))
        |> Dictionary
        |> Internal.writeOutput outputDestination exports splitMap
