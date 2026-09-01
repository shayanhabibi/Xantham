/// <summary>
/// Construct a graph of dependencies between types.
/// </summary>
module Xantham.Decoder.Types.Graph

open System.Collections.Concurrent
open System.Collections.Frozen
open System.Collections.Generic
open FSharp.Control
open Xantham
open Xantham.Decoder

/// <summary>
/// A dependency graph of types extracted from a <c>DecodedResult</c>.
/// Provides reverse-lookup, cycle detection, and degree tracking for
/// downstream ordering or topological analysis.
/// </summary>
/// <category index="5">Dependency Graph</category>
type Graph = {
    /// For each <c>TypeKey</c>, the set of keys that depend on it (i.e. its dependents).
    Dependents: FrozenDictionary<TypeKey, FrozenSet<TypeKey>>
    /// Cycles in the graph as <c>(typeKey, cycleParticipant)</c> pairs:
    /// each entry indicates a key that participates in a dependency cycle with another key.
    Cycles: FrozenDictionary<TypeKey, TypeKey>
    /// In-degree per <c>TypeKey</c> — the count of outgoing dependencies from each node.
    Degrees: FrozenDictionary<TypeKey, int>
}

module Graph =
    [<Struct>]
    type private GraphNode = {
        TypeKey: TypeKey
        Dependencies: Set<TypeKey>
    }
    let private consumeTypeReference (typeReference: TsTypeReference) = 
        Set [
            typeReference.Type
            yield! typeReference.TypeArguments
            if typeReference.ResolvedType.IsSome then typeReference.ResolvedType.Value
        ]
    let private consumeMember = function
        | TsMember.CallSignature callSignatures ->
            callSignatures.Values
            |> Array.map (fun callSignature ->
                seq {
                    callSignature.Type
                    yield!
                        callSignature.Parameters
                        |> Seq.map _.Type
                }
                |> Set.ofSeq)
            |> Set.unionMany
        | TsMember.Method tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun tsOverloadableConstruct ->
                tsOverloadableConstruct.Type
                |> List.singleton
                |> List.append (tsOverloadableConstruct.Parameters |> List.map _.Type)
                |> Set.ofList
                )
            |> Set.unionMany
        | TsMember.Property tsProperty ->
            Set [ tsProperty.Type ]
        | TsMember.GetAccessor tsGetAccessor ->
            Set [ tsGetAccessor.Type ]
        | TsMember.SetAccessor tsSetAccessor ->
            Set [ tsSetAccessor.ArgumentType ]
        | TsMember.IndexSignature tsIndexSignature ->
            Set [ tsIndexSignature.Type; yield! tsIndexSignature.Parameters |> List.map _.Type ]
        | TsMember.ConstructSignature tsOverloadableConstruct ->
            tsOverloadableConstruct.Values
            |> Array.map (fun constructSignature ->
                constructSignature.Type
                |> List.singleton
                |> List.append (constructSignature.Parameters |> List.map _.Type)
                |> Set.ofList
                )
            |> Set.unionMany
    let private consumeTypeParameter (typeParameter: TsTypeParameter) =
        Set [
            if typeParameter.Constraint.IsSome then typeParameter.Constraint.Value
            if typeParameter.Default.IsSome then typeParameter.Default.Value
        ]
    let private consumeInlineTypeParameter (typeParameter: InlinedTsTypeParameter) =
        Set [
            fst typeParameter
            if (snd typeParameter).Constraint.IsSome then (snd typeParameter).Constraint.Value
            if (snd typeParameter).Default.IsSome then (snd typeParameter).Default.Value
        ]

    /// <summary>
    /// Build a <see cref="T:Graph"/> from a <c>DecodedResult</c>.
    /// </summary>
    /// <param name="includeCheckExtendsConditional">
    /// When <c>true</c>, conditional types contribute their <c>Check</c>/<c>Extends</c> branches
    /// as edges; when <c>false</c>, only the <c>True</c>/<c>False</c> branches are followed.
    /// </param>
    /// <param name="decodedResult">The decoded type/export maps to walk.</param>
    let create includeCheckExtendsConditional (decodedResult: DecodedResult) =
        let dependents = ConcurrentDictionary<TypeKey, ConcurrentBag<TypeKey>>()
        let degrees = ConcurrentDictionary<TypeKey, int>()
        // Compute the set of TypeKey dependencies for a given TsType.
        // Lifted out of the taskSeq below because F# can't statically
        // compile a state machine when its body contains `let rec`
        // (FS3511). Returning Set<TypeKey> here keeps the per-key Node
        // construction in the taskSeq cheap.
        let rec getDependencies tsType =
            match tsType with
            | TsType.GlobalThis
            | TsType.Primitive _
            | TsType.Enum _
            | TsType.Literal _
            | TsType.Predicate _
            | TsType.EnumCase _ -> Set.empty
            | TsType.Conditional tsConditionalType ->
                Set [
                    if includeCheckExtendsConditional then
                        tsConditionalType.Check
                        tsConditionalType.Extends
                    tsConditionalType.True
                    tsConditionalType.False
                ]
            | TsType.Interface tsInterface ->
                let extends =
                    tsInterface.Heritage.Extends
                    |> List.map consumeTypeReference
                let members =
                    tsInterface.Members
                    |> List.map consumeMember
                let typeArguments =
                    tsInterface.TypeParameters
                    |> List.map consumeInlineTypeParameter
                extends @ members @ typeArguments
                |> Set.unionMany
            | TsType.Class tsClass ->
                let extends =
                    tsClass.Heritage.Extends
                    |> List.map consumeTypeReference
                    |> List.append (
                        tsClass.Heritage.Implements
                        |> Option.toList
                        |> List.map consumeTypeReference
                        )
                let members =
                    tsClass.Members
                    |> List.map consumeMember
                let typeArguments =
                    tsClass.TypeParameters
                    |> List.map consumeInlineTypeParameter
                let constructors =
                    tsClass.Constructors
                    |> List.map (_.Parameters >> List.map _.Type >> Set)
                extends @ members @ typeArguments @ constructors
                |> Set.unionMany
            | TsType.Union tsTypeUnion -> Set tsTypeUnion.Types
            | TsType.Intersection tsTypeIntersection -> Set tsTypeIntersection.Types
            | TsType.IndexedAccess tsIndexAccessType ->
                Set [ tsIndexAccessType.Object; tsIndexAccessType.Index ]
            | TsType.TypeReference tsTypeReference -> consumeTypeReference tsTypeReference
            | TsType.Array tsType -> getDependencies tsType
            | TsType.TypeParameter tsTypeParameter -> consumeTypeParameter tsTypeParameter
            | TsType.ReadOnly tsType -> getDependencies tsType
            | TsType.Tuple tsTuple ->
                tsTuple.Types
                |> List.map _.Type
                |> Set
            | TsType.Index tsIndex -> Set [ tsIndex.Type ]
            | TsType.TypeLiteral tsTypeLiteral ->
                tsTypeLiteral.Members
                |> List.map consumeMember
                |> Set.unionMany
            | TsType.TemplateLiteral tsTemplateLiteralType -> Set tsTemplateLiteralType.Types
            | TsType.Optional tsTypeReference -> consumeTypeReference tsTypeReference
            | TsType.Substitution tsSubstitutionType ->
                Set [ tsSubstitutionType.Base; tsSubstitutionType.Constraint ]
            | TsType.TypeQuery tsTypeQuery -> Set [ tsTypeQuery.Type ]
        taskSeq {
            for kv in decodedResult.TypeMap do
                { TypeKey = kv.Key; Dependencies = getDependencies kv.Value }
        }
        |> TaskSeq.iter (fun node ->
            let degree = node.Dependencies.Count
            degrees.AddOrUpdate(node.TypeKey, degree, fun _ oldDegree -> oldDegree + degree)
            |> ignore
            for key in node.Dependencies do
                dependents.GetOrAdd(key, ConcurrentBag())
                |> _.Add(node.TypeKey)
            )
        |> _.Wait()
        let dependents =
            seq {
                for kv in dependents do
                    KeyValuePair(kv.Key, kv.Value.ToFrozenSet())
            }
            |> _.ToFrozenDictionary()
        let cycles =
            seq {
                for kv in dependents do
                    let key = kv.Key
                    for nestedKv in dependents do
                        if nestedKv.Value.Contains(key) && kv.Value.Contains(nestedKv.Key) then
                            yield KeyValuePair(key, nestedKv.Key)
            }
            |> _.ToFrozenDictionary()
        {
            Dependents = dependents
            Cycles = cycles
            Degrees = degrees.ToFrozenDictionary()
        }
        