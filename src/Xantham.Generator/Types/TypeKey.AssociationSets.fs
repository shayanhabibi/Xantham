[<AutoOpen>]
module Xantham.Generator.TypeKeyAssociationSets

open System.Collections.Generic
open Xantham
open Xantham.Decoder.Runtime

module TypeKey =
    type Association = TypeKey * Set<TypeKey>
    type AssociationMap = Map<TypeKey, Set<TypeKey>>

    module AssociationMap =
        let private shallowCollectParameters (value: TsParameter list) = value |> Seq.map _.Type
        let private shallowCollectMembers = function
            | TsMember.Method method -> method.ToSeq() |> Seq.collect (fun method ->
                seq {
                    yield! method.Parameters |> shallowCollectParameters
                    method.Type
                }
                )
            | TsMember.Property tsProperty -> seq { tsProperty.Type }
            | TsMember.GetAccessor tsGetAccessor -> seq { tsGetAccessor.Type }
            | TsMember.SetAccessor tsSetAccessor -> seq { tsSetAccessor.ArgumentType }
            | TsMember.CallSignature tsOverloadableConstruct -> tsOverloadableConstruct.ToSeq() |> Seq.collect (fun callSignature ->
                seq {
                    yield! callSignature.Parameters |> shallowCollectParameters
                    callSignature.Type
                }
                )
            | TsMember.IndexSignature tsIndexSignature -> seq { tsIndexSignature.Type }
            | TsMember.ConstructSignature tsOverloadableConstruct -> tsOverloadableConstruct.ToSeq() |> Seq.collect (fun constructSignature ->
                seq {
                    yield! constructSignature.Parameters |> shallowCollectParameters
                    constructSignature.Type
                })

        let rec private shallowCollectTypes = function
            | TsType.TypeReference ref -> seq {
                ref.Type
                yield! ref.TypeArguments
                if ref.ResolvedType.IsSome then ref.ResolvedType.Value
                }
            | TsType.GlobalThis -> Seq.empty
            | TsType.Conditional tsConditionalType -> seq {
                    tsConditionalType.Check
                    tsConditionalType.Extends
                    tsConditionalType.True
                    tsConditionalType.False
                }
            | TsType.Interface tsInterface -> seq {
                    yield!
                        tsInterface.Members
                        |> Seq.collect shallowCollectMembers
                    yield!
                        tsInterface.Heritage.Extends
                        |> Seq.collect (fun heritage -> seq {
                            heritage.Type
                            yield! heritage.TypeArguments
                            if heritage.ResolvedType.IsSome then heritage.ResolvedType.Value
                        })
                    yield!
                        tsInterface.TypeParameters
                        |> Seq.map fst
                }
            | TsType.Class tsClass -> seq {
                    yield!
                        tsClass.Members
                        |> Seq.collect shallowCollectMembers
                    yield!
                        tsClass.Heritage.Extends
                        |> Seq.collect (fun heritage -> seq {
                            heritage.Type
                            yield! heritage.TypeArguments
                            if heritage.ResolvedType.IsSome then heritage.ResolvedType.Value
                        })
                    if tsClass.Heritage.Implements.IsSome then yield! seq {
                        tsClass.Heritage.Implements.Value.Type
                        yield! tsClass.Heritage.Implements.Value.TypeArguments
                        if tsClass.Heritage.Implements.Value.ResolvedType.IsSome then tsClass.Heritage.Implements.Value.ResolvedType.Value
                    }
                    yield!
                        tsClass.TypeParameters
                        |> Seq.map fst
                }
            | TsType.Primitive _ -> Seq.empty
            | TsType.Enum _ -> Seq.empty
            | TsType.EnumCase _ -> Seq.empty
            | TsType.Union tsTypeUnion -> tsTypeUnion.Types
            | TsType.Intersection tsTypeIntersection -> tsTypeIntersection.Types
            | TsType.Literal _ -> Seq.empty
            | TsType.IndexedAccess tsIndexAccessType -> seq {
                    tsIndexAccessType.Object
                    tsIndexAccessType.Index
                }
            | TsType.Array tsType -> shallowCollectTypes tsType
            | TsType.TypeParameter tsTypeParameter -> seq {
                    if tsTypeParameter.Default.IsSome then tsTypeParameter.Default.Value
                    if tsTypeParameter.Constraint.IsSome then tsTypeParameter.Constraint.Value
                }
            | TsType.ReadOnly tsType -> shallowCollectTypes tsType
            | TsType.Tuple tsTuple -> seq {
                    for tsTupleKind in tsTuple.Types do
                        match tsTupleKind with
                        | FixedLabeled(_, { Type = typ }) 
                        | Variadic typ 
                        | Fixed { Type = typ } -> typ
                }
            | TsType.Index tsIndex -> seq { tsIndex.Type }
            | TsType.Predicate tsTypePredicate -> seq { tsTypePredicate.Type }
            | TsType.TypeLiteral tsTypeLiteral -> tsTypeLiteral.Members |> Seq.collect shallowCollectMembers
            | TsType.TemplateLiteral tsTemplateLiteralType -> tsTemplateLiteralType.Types
            | TsType.Optional tsTypeReference -> seq {
                    tsTypeReference.Type
                    if tsTypeReference.ResolvedType.IsSome then tsTypeReference.ResolvedType.Value
                    yield! tsTypeReference.TypeArguments
                }
            | TsType.Substitution tsSubstitutionType ->  seq {
                    tsSubstitutionType.Base
                    tsSubstitutionType.Constraint
                }
        let rec private shallowCollectExports = function
            | TsExportDeclaration.Variable { Type = typeReference } -> seq { typeReference }
            | TsExportDeclaration.Interface tsInterface -> seq {
                    yield!
                        tsInterface.Members
                        |> Seq.collect shallowCollectMembers
                    yield!
                        tsInterface.Heritage.Extends
                        |> Seq.collect (fun heritage -> seq {
                            heritage.Type
                            yield! heritage.TypeArguments
                            if heritage.ResolvedType.IsSome then heritage.ResolvedType.Value
                        })
                    yield!
                        tsInterface.TypeParameters
                        |> Seq.map fst
                }
            | TsExportDeclaration.Class tsClass -> seq {
                    yield!
                        tsClass.Members
                        |> Seq.collect shallowCollectMembers
                    yield!
                        tsClass.Heritage.Extends
                        |> Seq.collect (fun heritage -> seq {
                            heritage.Type
                            yield! heritage.TypeArguments
                            if heritage.ResolvedType.IsSome then heritage.ResolvedType.Value
                        })
                    if tsClass.Heritage.Implements.IsSome then yield! seq {
                        tsClass.Heritage.Implements.Value.Type
                        yield! tsClass.Heritage.Implements.Value.TypeArguments
                        if tsClass.Heritage.Implements.Value.ResolvedType.IsSome then tsClass.Heritage.Implements.Value.ResolvedType.Value
                    }
                    yield!
                        tsClass.TypeParameters
                        |> Seq.map fst
                }
            | TsExportDeclaration.TypeAlias tsTypeAlias -> seq {
                    tsTypeAlias.Type
                    yield!
                        tsTypeAlias.TypeParameters
                        |> Seq.map fst
                }
            | TsExportDeclaration.Enum _ -> Seq.empty
            | TsExportDeclaration.Module tsModule -> tsModule.Exports |> Seq.collect shallowCollectExports
            | TsExportDeclaration.Function tsOverloadableConstruct ->
                tsOverloadableConstruct.ToSeq()
                |> Seq.collect (fun tsOverloadableConstruct -> seq {
                    tsOverloadableConstruct.Type
                    yield! tsOverloadableConstruct.Parameters |> shallowCollectParameters
                    yield! tsOverloadableConstruct.TypeParameters |> Seq.map fst
                    tsOverloadableConstruct.SignatureKey
                })

