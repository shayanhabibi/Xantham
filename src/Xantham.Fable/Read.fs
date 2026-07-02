[<AutoOpen>]
module Xantham.Fable.Main

open Fable.Core.DynamicExtensions
open Node
open Thoth.Json
open Xantham
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
        | TsType.TypeQuery ({ Type = key } as typeQuery) ->
            if typKey = key then Log.healthCheckError typKey typeQuery typeQuery

    let private unknownKey = TypeKindPrimitive.Unknown.TypeKey
    
    /// Priority for choosing the winning entry when multiple IdentityKeys map to the same TypeKey.
    /// Lower number = higher priority.
    let identityPriority (identity: IdentityKey) =
        match identity with
        | IdentityKey.DeclarationPosition _ -> 0
        | IdentityKey.Symbol _              -> 1
        | IdentityKey.AliasSymbol _         -> 2
        | IdentityKey.Id _                  -> 3
        
    // NOTE: non-private (was `private`) only so the encoder identity/dedup
    // isolation suite (tests/Xantham.Fable.Tests/EncoderMergeDedup.fs) can drive
    // it on CONSTRUCTED IRResult values without a live ts-morph reader. Behaviour
    // is unchanged.
    let finaliseAssembly (results: IRResult<'T> array) =
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
                #if !RELEASE
                path.join(__SOURCE_DIRECTORY__, "output.json")
                #else
                "./output.json"
                #endif
            elif destination.EndsWith(".json") then
                destination
            else path.join(destination, "output.json")
        #if DEBUG
        // let json = Encode.Auto.toString(1, result)
        let json = EncodedResult.encode result |> Encode.toString 1
        #else
        // let json = Encode.Auto.toString(result)
        let json = EncodedResult.encode result |> Encode.toString 0
        #endif
        fs.writeFile(destination, json, None, callback = (function
            | Some error -> failwith $"%A{error}"
            | None -> ()))

    let initialise (reader: TypeScriptReader) =
        tagPrimitives reader

    /// Seeds the stack from EVERY entry file, keeping the (entry file -> its export
    /// tags) association for provenance. Identity-keyed caches make cross-entry
    /// dedup intrinsic: a declaration reached from two entries registers once.
    let getAndPrepareExports (reader: TypeScriptReader) =
        reader.entryFiles
        |> Array.map (fun entryFile ->
            match reader.program.getSourceFile entryFile with
            | Some sourceFile ->
                entryFile,
                sourceFile
                |> getDeclarations reader
                |> Array.apply (pushToStack reader)
            | None -> failwith $"Entry file is not part of the program: {entryFile}")

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

    // ── Structural union interning (encoder identity, derived not minted) ─────────
    //
    // ROOT FIX. TypeKey.create() mints union identity by ALLOCATION (a monotonic
    // decrement counter), so every occurrence of a structurally-identical synthesized
    // union (e.g. `string | number`) gets a BRAND-NEW key — the IR is a multiset of
    // identities, not a set of types (measured: 781 of 1583 union keys redundant, 49%).
    //
    // Union member identities are NOT available at mint time (TypeKey.create runs in
    // tryGetOrRegisterStore, before the union body is dispatched) nor at union dispatch
    // (instrumented: members read -14/Unknown there; they settle reactively AFTER, as
    // the stack drains). The first point ALL union member keys are settled is assembly
    // (STypeUnionBuilder.Build reads each member signal's final .Value). So the
    // structural interning is DEFERRED to here — still encoder-side, still identity-
    // rooted (it rewrites which keys exist in the IR the decoder receives), NOT a
    // downstream generator bandaid.
    //
    // SCOPE: UNIONS ONLY. Unions are location-independent (`string|number` is the same
    // type anywhere → safe to share one identity; the decoder's internedUnions already
    // interns them by this exact sorted-member signature, so this COMPOSES with it).
    // TypeLiterals are deliberately NOT interned (location-dependent: hoisted under a
    // named owner; interning them collapses distinct-owner literals — tried & reverted).
    let internStructuralUnions (result: EncodedResult) =
        // Signature of a union = its sorted member-key list (mirrors decoder internedUnions).
        let unionSignature (TsTypeUnion members) = List.sort members
        // ── reference rewriter ─────────────────────────────────────────────────────
        // Build the closures that rewrite EVERY TypeKey-bearing position for a given
        // remap. Enumerated against Common.Types.fs (every field typed TypeKey /
        // TypeKey list / TypeKey option). Returned as a record so the fixpoint driver
        // can apply successive remaps.
        let applyRemap (remap: Map<TypeKey, TypeKey>) (result: EncodedResult) =
            let remapKey (key: TypeKey) =
                match Map.tryFind key remap with
                | Some canonical -> canonical
                | None -> key
            let remapInlinedTypeParam ((key, tp): InlinedTsTypeParameter) : InlinedTsTypeParameter =
                remapKey key,
                { tp with
                    Constraint = tp.Constraint |> Option.map remapKey
                    Default = tp.Default |> Option.map remapKey }
            let remapTypeRef (r: TsTypeReference) =
                { r with
                    Type = remapKey r.Type
                    TypeArguments = r.TypeArguments |> List.map remapKey
                    ResolvedType = r.ResolvedType |> Option.map remapKey }
            let remapParam (p: TsParameter) = { p with Type = remapKey p.Type }
            let remapTupleElemType (e: TsTupleElementType) = { e with Type = remapKey e.Type }
            let remapMember = function
                | TsMember.Method m ->
                    m.ToList()
                    |> List.map (fun v -> { v with TsMethod.Type = remapKey v.Type; Parameters = v.Parameters |> List.map remapParam; TypeParameters = v.TypeParameters |> List.map remapInlinedTypeParam })
                    |> TsOverloadableConstruct.Create
                    |> TsMember.Method
                | TsMember.Property p -> TsMember.Property { p with Type = remapKey p.Type }
                | TsMember.GetAccessor g -> TsMember.GetAccessor { g with Type = remapKey g.Type }
                | TsMember.SetAccessor s -> TsMember.SetAccessor { s with ArgumentType = remapKey s.ArgumentType }
                | TsMember.CallSignature c ->
                    c.ToList()
                    |> List.map (fun v -> { v with TsCallSignature.Type = remapKey v.Type; Parameters = v.Parameters |> List.map remapParam; TypeParameters = v.TypeParameters |> List.map remapInlinedTypeParam })
                    |> TsOverloadableConstruct.Create
                    |> TsMember.CallSignature
                | TsMember.IndexSignature i ->
                    TsMember.IndexSignature { i with Type = remapKey i.Type; Parameters = i.Parameters |> List.map remapParam }
                | TsMember.ConstructSignature c ->
                    c.ToList()
                    |> List.map (fun v -> { v with TsConstructSignature.Type = remapKey v.Type; Parameters = v.Parameters |> List.map remapParam; TypeParameters = v.TypeParameters |> List.map remapInlinedTypeParam })
                    |> TsOverloadableConstruct.Create
                    |> TsMember.ConstructSignature
            let rec remapType (typ: TsType) : TsType =
                match typ with
                | TsType.GlobalThis
                | TsType.Primitive _
                | TsType.Literal _ -> typ
                | TsType.Union (TsTypeUnion members) -> members |> List.map remapKey |> TsTypeUnion |> TsType.Union
                | TsType.Intersection (TsTypeIntersection members) -> members |> List.map remapKey |> TsTypeIntersection |> TsType.Intersection
                | TsType.Conditional c -> TsType.Conditional { Check = remapKey c.Check; Extends = remapKey c.Extends; True = remapKey c.True; False = remapKey c.False }
                | TsType.Interface i -> TsType.Interface { i with Members = i.Members |> List.map remapMember; TypeParameters = i.TypeParameters |> List.map remapInlinedTypeParam; Heritage = { Extends = i.Heritage.Extends |> List.map remapTypeRef } }
                | TsType.Class c -> TsType.Class { c with Members = c.Members |> List.map remapMember; TypeParameters = c.TypeParameters |> List.map remapInlinedTypeParam; Constructors = c.Constructors |> List.map (fun ctor -> { ctor with Parameters = ctor.Parameters |> List.map remapParam }); Heritage = { Implements = c.Heritage.Implements |> Option.map remapTypeRef; Extends = c.Heritage.Extends |> List.map remapTypeRef } }
                | TsType.Enum e -> TsType.Enum { e with Members = e.Members |> List.map (fun m -> { m with Parent = remapKey m.Parent }) }
                | TsType.EnumCase ec -> TsType.EnumCase { ec with Parent = remapKey ec.Parent }
                | TsType.IndexedAccess ia -> TsType.IndexedAccess { Object = remapKey ia.Object; Index = remapKey ia.Index }
                | TsType.TypeReference r -> TsType.TypeReference (remapTypeRef r)
                | TsType.Array t -> TsType.Array (remapType t)
                | TsType.ReadOnly t -> TsType.ReadOnly (remapType t)
                | TsType.TypeParameter tp -> TsType.TypeParameter { tp with Constraint = tp.Constraint |> Option.map remapKey; Default = tp.Default |> Option.map remapKey }
                | TsType.Tuple t -> TsType.Tuple { t with Types = t.Types |> List.map (function TsTupleElement.FixedLabeled(l, e) -> TsTupleElement.FixedLabeled(l, remapTupleElemType e) | TsTupleElement.Variadic k -> TsTupleElement.Variadic (remapKey k) | TsTupleElement.Fixed e -> TsTupleElement.Fixed (remapTupleElemType e)) }
                | TsType.Index i -> TsType.Index { i with Type = remapKey i.Type }
                | TsType.Predicate p -> TsType.Predicate { p with Type = remapKey p.Type }
                | TsType.TypeLiteral tl -> TsType.TypeLiteral { tl with Members = tl.Members |> List.map remapMember }
                | TsType.TemplateLiteral t -> TsType.TemplateLiteral { t with Types = t.Types |> List.map remapKey }
                | TsType.Optional r -> TsType.Optional (remapTypeRef r)
                | TsType.Substitution s -> TsType.Substitution { Base = remapKey s.Base; Constraint = remapKey s.Constraint }
                | TsType.TypeQuery q -> TsType.TypeQuery { q with Type = remapKey q.Type }
            let rec remapExport (export: TsExportDeclaration) : TsExportDeclaration =
                match export with
                | TsExportDeclaration.Variable v -> TsExportDeclaration.Variable { v with Type = remapKey v.Type }
                | TsExportDeclaration.Interface i -> TsExportDeclaration.Interface { i with Members = i.Members |> List.map remapMember; TypeParameters = i.TypeParameters |> List.map remapInlinedTypeParam; Heritage = { Extends = i.Heritage.Extends |> List.map remapTypeRef } }
                | TsExportDeclaration.TypeAlias a -> TsExportDeclaration.TypeAlias { a with Type = remapKey a.Type; TypeParameters = a.TypeParameters |> List.map remapInlinedTypeParam }
                | TsExportDeclaration.Class c -> TsExportDeclaration.Class { c with Members = c.Members |> List.map remapMember; TypeParameters = c.TypeParameters |> List.map remapInlinedTypeParam; Constructors = c.Constructors |> List.map (fun ctor -> { ctor with Parameters = ctor.Parameters |> List.map remapParam }); Heritage = { Implements = c.Heritage.Implements |> Option.map remapTypeRef; Extends = c.Heritage.Extends |> List.map remapTypeRef } }
                | TsExportDeclaration.Enum e -> TsExportDeclaration.Enum { e with Members = e.Members |> List.map (fun m -> { m with Parent = remapKey m.Parent }) }
                | TsExportDeclaration.Module m -> TsExportDeclaration.Module { m with Exports = m.Exports |> List.map remapExport }
                | TsExportDeclaration.Function f ->
                    f.ToList()
                    |> List.map (fun v -> { v with TsFunction.Type = remapKey v.Type; Parameters = v.Parameters |> List.map remapParam; TypeParameters = v.TypeParameters |> List.map remapInlinedTypeParam; SignatureKey = remapKey v.SignatureKey })
                    |> TsOverloadableConstruct.Create
                    |> TsExportDeclaration.Function
            let remapDuplicates remapNode (dups: Map<TypeKey, DuplicateEncoding<'a> list>) =
                // Drop redundant keys (entry now lives at the canonical key), and rewrite
                // both the surviving group keys and the bodies. The decoder ignores the
                // duplicate maps, but keeping them self-consistent avoids dangling references
                // and keeps the IR honestly free of the redundant union identities.
                dups
                |> Map.toList
                |> List.choose (fun (key, group) ->
                    if Map.containsKey key remap then None
                    else Some(key, group |> List.map (fun d -> { d with Value = remapNode d.Value })))
                |> Map.ofList
            {
                result with
                    Types =
                        result.Types
                        |> Map.toList
                        |> List.choose (fun (key, typ) ->
                            if Map.containsKey key remap then None     // redundant union — dropped (key now canonical)
                            else Some(key, remapType typ))
                        |> Map.ofList
                    ExportedDeclarations = result.ExportedDeclarations |> Map.map (fun _ -> remapExport)
                    DuplicateTypes = result.DuplicateTypes |> remapDuplicates remapType
                    DuplicateExports = result.DuplicateExports |> remapDuplicates remapExport
                    TopLevelExports = result.TopLevelExports |> List.map remapKey
                    LibEsExports = result.LibEsExports |> List.map remapKey
                    EntryExports = result.EntryExports |> Map.map (fun _ -> List.map remapKey)
            }
        // ── fixpoint driver ────────────────────────────────────────────────────────
        // Compute the union remap from the CURRENT Types map, apply it, and repeat.
        // Remapping a union's member keys can make two previously-distinct unions become
        // structurally identical (a new collision), so iterate until no remap is produced.
        // Terminates: each pass strictly shrinks the droppable-union count.
        //
        // EXPORT-KEY PROTECTION (correctness, not optimisation): a union key can ALSO be
        // an ExportedDeclarations key — a NAMED type alias whose body is a union, e.g.
        // `type ResponseFormatTextConfig = A | B | C` lives at key 4086 in BOTH Types
        // (the union body) and ExportedDeclarations (the alias). prerenderTypeAliases
        // resolves the alias via `ResolveType exportKey` → typeMap[exportKey], so dropping
        // such a key from Types makes the export resolution dangle (KeyNotFoundException).
        // So an export-keyed union is NEVER droppable; it stays as its own entry. It is
        // PREFERRED as the canonical for its signature, so anonymous synthesized twins
        // fold onto the named declaration rather than the reverse.
        let exportKeys = result.ExportedDeclarations.Keys |> Set.ofSeq
        let computeRemap (result: EncodedResult) =
            let unionEntries =
                result.Types
                |> Map.toList
                |> List.choose (function key, TsType.Union u -> Some(key, u) | _ -> None)
            // canonical per signature: prefer an export-keyed member (named declaration);
            // otherwise the minimum key. Deterministic (unique ints, stable min).
            let canonicalOf =
                unionEntries
                |> List.groupBy (fun (_, u) -> unionSignature u)
                |> List.map (fun (signature, group) ->
                    let keys = group |> List.map fst
                    let canonical =
                        match keys |> List.filter exportKeys.Contains with
                        | [] -> List.min keys
                        | exported -> List.min exported
                    signature, canonical)
                |> Map.ofList
            unionEntries
            |> List.choose (fun (key, u) ->
                let canonical = canonicalOf[unionSignature u]
                // Never drop a key that is itself an export key (must keep its Types entry),
                // and never remap a key onto itself.
                if key = canonical || exportKeys.Contains key then None else Some(key, canonical))
            |> Map.ofList
        let rec loop (result: EncodedResult) =
            let remap = computeRemap result
            if Map.isEmpty remap then result
            else loop (applyRemap remap result)
        loop result
open Schema
let read (reader: TypeScriptReader) =
    let exportTagsPerEntry =
        Internal.initialise reader
        |> Internal.getAndPrepareExports
    let exportTags = exportTagsPerEntry |> Array.collect snd
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
        EntryExports =
            exportTagsPerEntry
            |> Array.map (fun (entryFile, tags) ->
                entryFile,
                tags
                |> Array.map (fun tag -> reader.exportCache[tag.IdentityKey].RefKey)
                |> Array.distinct
                |> Array.toList)
            |> Map.ofArray
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
    |> Internal.internStructuralUnions
let write (outputDestination: string) (result: EncodedResult) =
    Internal.writeOutput outputDestination result
let readAndWrite (outputDestination: string) (reader: TypeScriptReader) =
    read reader
    |> write outputDestination