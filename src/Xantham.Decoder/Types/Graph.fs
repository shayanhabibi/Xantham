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

type Graph = {
    Dependents: FrozenDictionary<TypeKey, FrozenSet<TypeKey>>
    Cycles: FrozenDictionary<TypeKey, TypeKey>
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

    let create includeCheckExtendsConditional (decodedResult: DecodedResult) =
        let dependents = ConcurrentDictionary<TypeKey, ConcurrentBag<TypeKey>>()
        let degrees = ConcurrentDictionary<TypeKey, int>()
        taskSeq {
            for kv in decodedResult.TypeMap do
                let rec matcher = function
                    | TsType.GlobalThis 
                    | TsType.Primitive _ 
                    | TsType.Enum _ 
                    | TsType.Literal _
                    | TsType.Predicate _
                    | TsType.EnumCase _ ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set.empty
                        }
                    | TsType.Conditional tsConditionalType ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set [
                                if includeCheckExtendsConditional then
                                    tsConditionalType.Check
                                    tsConditionalType.Extends
                                tsConditionalType.True
                                tsConditionalType.False
                            ]
                            
                        }
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
                        {
                            TypeKey = kv.Key
                            Dependencies =
                                extends @ members @ typeArguments
                                |> Set.unionMany
                        }
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
                        {
                            TypeKey = kv.Key
                            Dependencies =
                                extends @ members @ typeArguments @ constructors
                                |> Set.unionMany
                        }
                    | TsType.Union tsTypeUnion ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set tsTypeUnion.Types
                        }
                    | TsType.Intersection tsTypeIntersection ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set tsTypeIntersection.Types
                        }
                    | TsType.IndexedAccess tsIndexAccessType ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set [ tsIndexAccessType.Object; tsIndexAccessType.Index ]
                        }
                    | TsType.TypeReference tsTypeReference ->
                        {
                            TypeKey = kv.Key
                            Dependencies = consumeTypeReference tsTypeReference
                        }
                    | TsType.Array tsType -> matcher tsType
                    | TsType.TypeParameter tsTypeParameter ->
                        {
                            TypeKey = kv.Key
                            Dependencies = consumeTypeParameter tsTypeParameter
                        }
                    | TsType.ReadOnly tsType -> matcher tsType
                    | TsType.Tuple tsTuple ->
                        {
                            TypeKey = kv.Key
                            Dependencies =
                                tsTuple.Types
                                |> List.map _.Type
                                |> Set
                        }
                    | TsType.Index tsIndex ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set [ tsIndex.Type ]
                        }
                    | TsType.TypeLiteral tsTypeLiteral ->
                        {
                            TypeKey = kv.Key
                            Dependencies =
                                tsTypeLiteral.Members
                                |> List.map consumeMember
                                |> Set.unionMany
                        }
                    | TsType.TemplateLiteral tsTemplateLiteralType ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set tsTemplateLiteralType.Types
                        }
                    | TsType.Optional tsTypeReference ->
                        {
                            TypeKey = kv.Key
                            Dependencies = consumeTypeReference tsTypeReference
                        }
                    | TsType.Substitution tsSubstitutionType ->
                        {
                            TypeKey = kv.Key
                            Dependencies = Set [ tsSubstitutionType.Base; tsSubstitutionType.Constraint ]
                        }
                matcher kv.Value
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
        