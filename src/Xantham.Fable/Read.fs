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
    let rec healthCheckNode (typKey: TypeKey) (node: TsAstNode) =
        match node with
        | TsAstNode.Alias alias ->
            if alias.Type = typKey then
                Log.error $"Alias {alias.Name} points to itself"
            alias.TypeParameters
            |> List.map TsAstNode.TypeParameter
            |> List.iter (healthCheckNode typKey)
        | TsAstNode.TemplateLiteral node ->
            if node.Types |> List.contains typKey then
                Log.error $"TemplateLiteral {node} points to itself"
        | TsAstNode.GlobalThis -> ()
        | TsAstNode.Tuple tsTuple ->
            tsTuple.Types
            |> List.iter (function
                | TsTupleElement.Variadic key
                | TsTupleElement.Fixed { Type = key }
                | TsTupleElement.FixedLabeled(_, { Type = key }) ->
                    if key = typKey then
                        Log.error $"Tuple {tsTuple} points to itself"
                )
        | TsAstNode.Interface tsInterface -> ()
        | TsAstNode.Variable tsVariable ->
            if typKey = tsVariable.Type then Log.error $"Variable {tsVariable.Name} points to itself"
        | TsAstNode.Primitive typeKindPrimitive -> ()
        | TsAstNode.Predicate tsTypePredicate -> ()
        | TsAstNode.Literal tsLiteral -> ()
        | TsAstNode.TypeLiteral tsTypeLiteral -> ()
        | TsAstNode.Property tsProperty -> if typKey = tsProperty.Type then Log.error $"Property {tsProperty.Name} points to itself"
        | TsAstNode.Parameter tsParameter -> if typKey = tsParameter.Type then Log.error $"Parameter {tsParameter.Name} points to itself"
        | TsAstNode.TypeParameter tsTypeParameter ->
            if Some typKey = tsTypeParameter.Constraint then Log.error $"TypeParameter {tsTypeParameter.Name} points to itself"
            elif Some typKey = tsTypeParameter.Default then Log.error $"TypeParameter {tsTypeParameter.Name} points to itself"
        | TsAstNode.IndexAccessType tsIndexAccessType ->
            if typKey = tsIndexAccessType.Object then Log.error $"IndexAccessType {tsIndexAccessType} points to itself"
            elif typKey = tsIndexAccessType.Index then Log.error $"IndexAccessType {tsIndexAccessType} points to itself"
        | TsAstNode.FunctionDeclaration tsFunction -> if typKey = tsFunction.Type then Log.error $"FunctionDeclaration {tsFunction.Name} points to itself"
        | TsAstNode.Method tsMethod -> if typKey = tsMethod.Type then Log.error $"Method {tsMethod.Name} points to itself"
        | TsAstNode.Constructor tsConstructor -> ()
        | TsAstNode.ConstructSignature tsConstructSignature -> ()
        | TsAstNode.IndexSignature tsIndexSignature -> if typKey = tsIndexSignature.Type then Log.error $"IndexSignature {tsIndexSignature} points to itself"
        | TsAstNode.Index tsIndex -> if typKey = tsIndex.Type then Log.error $"Index {tsIndex} points to itself"
        | TsAstNode.TypeReference tsTypeReference ->
            if typKey = tsTypeReference.Type then Log.error $"TypeReference {tsTypeReference} points to itself"
            elif Some typKey = tsTypeReference.ResolvedType then Log.error $"TypeReference {tsTypeReference} points to itself"
        | TsAstNode.Array tsTypeReference ->
            if typKey = tsTypeReference.Type then Log.error $"Array {tsTypeReference} points to itself"
            elif Some typKey = tsTypeReference.ResolvedType then Log.error $"Array {tsTypeReference} points to itself"
        | TsAstNode.Enum tsEnumType -> ()
        | TsAstNode.EnumCase tsEnumCase -> ()
        | TsAstNode.SubstitutionType tsSubstitutionType -> ()
        | TsAstNode.Conditional tsConditionalType ->
            if typKey = tsConditionalType.Check then Log.error $"Conditional {tsConditionalType} points to itself"
            elif typKey = tsConditionalType.Extends then Log.error $"Conditional {tsConditionalType} points to itself"
            elif typKey = tsConditionalType.True then Log.error $"Conditional {tsConditionalType} points to itself"
            elif typKey = tsConditionalType.False then Log.error $"Conditional {tsConditionalType} points to itself"
        | TsAstNode.Class tsClass -> ()
        | TsAstNode.Union tsTypeUnion ->
            if tsTypeUnion.Types |> List.contains typKey then
                Log.error $"Union {tsTypeUnion} points to itself"
        | TsAstNode.Intersection tsTypeIntersection ->
            if tsTypeIntersection.Types |> List.contains typKey then
                Log.error $"Intersection {tsTypeIntersection} points to itself"
        | TsAstNode.Optional tsTypeReference -> if typKey = tsTypeReference.Type then Log.error $"Optional {tsTypeReference} points to itself"
        | TsAstNode.GetAccessor tsGetAccessor -> if typKey = tsGetAccessor.Type then Log.error $"GetAccessor {tsGetAccessor} points to itself"
        | TsAstNode.SetAccessor tsSetAccessor -> if typKey = tsSetAccessor.ArgumentType then Log.error $"SetAccessor {tsSetAccessor} points to itself"
        | TsAstNode.CallSignature tsCallSignature -> if typKey = tsCallSignature.Type then Log.error $"CallSignature {tsCallSignature} points to itself"
        | TsAstNode.Module tsModule -> ()

    type IRResult = {
        Identity: IdentityKey
        Key: TypeKey
        Node: TsAstNode
    }

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
            [ if m.Type = u then "Method.Type" ]
        | TsAstNode.FunctionDeclaration f ->
            [ if f.Type = u then "FunctionDeclaration.Type" ]
        | TsAstNode.GetAccessor g ->
            [ if g.Type = u then "GetAccessor.Type" ]
        | TsAstNode.SetAccessor s ->
            [ if s.ArgumentType = u then "SetAccessor.ArgumentType" ]
        | TsAstNode.CallSignature cs ->
            [ if cs.Type = u then "CallSignature.Type" ]
        | TsAstNode.ConstructSignature cs ->
            [ if cs.Type = u then "ConstructSignature.Type" ]
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
                 match identity with
                 | IdentityKey.DeclarationPosition(file, pos, endPos) ->
                     match reader.program.getSourceFile(file) with
                     | Some file ->
                         let start = file.getLineAndCharacterOfPosition(pos)
                         let endPos = file.getLineAndCharacterOfPosition(endPos)
                         Log.error $"[{typeKey}] Missing builder for: %A{file.fileName} ({start.line},{start.character}) ({endPos.line},{endPos.character})"
                     | None ->
                         Log.error $"[{typeKey}] Missing builder for: {file} ({pos},{endPos})"
                 | IdentityKey.Symbol sym ->
                     Log.error $"[{typeKey}] Missing builder for: {sym.name}"
                 | IdentityKey.Id _ ->
                     // Anonymous types with no symbol or declaration — complex generic internals.
                     // These are expected for highly generic signatures (splitProps, mergeProps, etc.)
                     // and have no meaningful identity to report.
                     Log.warn $"[{typeKey}] Missing builder for: %A{identity}"
                 | _ -> Log.error $"[{typeKey}] Missing builder for: %A{identity}"
             | ValueSome builder ->
                 let builtNode = builder.Build()
                 healthCheckNode typeKey builtNode
                 let unknownFields = findUnknownFields builtNode
                 if unknownFields.Length > 0 then
                     Log.warn $"""[{typeKey}] %A{identity}: Unknown TypeKey in fields: {String.concat ", " unknownFields}"""
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

    /// For each group of entries sharing a TypeKey, select the highest-priority winner.
    /// Only logs groups where the winner's node differs from at least one other entry (genuine
    /// conflicts). Same-builder duplicates (lib overloads, Id shadows) are silently discarded.
    let resolveDuplicates (groups: {| Key: TypeKey; Results: IRResult array |} seq) : IRResult seq =
        seq {
            for group in groups do
                let sorted = group.Results |> Array.sortBy (fun ir -> identityPriority ir.Identity)
                let winner = sorted[0]
                let hasConflict = sorted |> Array.exists (fun ir -> ir.Node <> winner.Node)
                if hasConflict then
                    Log.warn $"| Duplicate entries for key %A{group.Key}"
                    for ir in sorted do
                        Log.debug $" * %A{ir.Identity} ->"
                        Log.log $" %A{ir.Node}"
                    Log.success $"| Winner: %A{winner.Identity}"
                    Log.debug $" {winner.Node}"
                    Log.info "_______________________________________"
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
                {| Key = key
                   Results = results |> Array.filter (fun ir -> ir.Key = key) |}
        }
        let nonDuplicates = seq {
            for ir in results do
            if count[ir.Key] = 1 then ir
        }
        {| DuplicateGroups = duplicateGroups; NonDuplicates = nonDuplicates |}

    let writeOutput (destination: string) (exports: XanthamTag array) (duplicates: Dictionary<TypeKey, TsAstNode array>) (result: Dictionary<TypeKey, TsAstNode>)  =
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
        let json = Encode.Auto.toString(4, resultTuple)
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

    let read (reader: TypeScriptReader) : Dictionary<TypeKey, TsAstNode> =
        tagPrimitives reader
        |> _.program.getSourceFile(reader.entryFile).Value
        |> getDeclarations reader
        |> Array.apply (pushToStack reader)
        |> ignore

        let mutable stackEntry = Unchecked.defaultof<XanthamTag>
        while reader.stack.TryPop(&stackEntry) do
            Dispatcher.dispatch reader stackEntry

        let results = assembleResults reader
        let split = exciseDuplicateKeys results
        let resolved = resolveDuplicates split.DuplicateGroups
        Seq.append split.NonDuplicates resolved
        |> Seq.map (fun ir -> KeyValuePair(ir.Key, ir.Node))
        |> Dictionary

let readAndWrite (outputDestination: string) (reader: TypeScriptReader) =
    let exports =
        Internal.initialise reader
        |> Internal.getAndPrepareExports
    Internal.runReader reader
    |> Internal.assembleResults
    |> Internal.exciseDuplicateKeys
    |> fun split ->
        let splitMap = Dictionary<TypeKey, TsAstNode array>()
        split.DuplicateGroups
        |> Seq.iter (fun group ->
            splitMap.Add(group.Key, group.Results |> Array.map _.Node)
            )
        Internal.resolveDuplicates split.DuplicateGroups
        |> Seq.append split.NonDuplicates
        |> Seq.sortBy _.Key
        |> Seq.map (fun ir -> KeyValuePair(ir.Key, ir.Node))
        |> Dictionary
        |> Internal.writeOutput outputDestination exports splitMap
