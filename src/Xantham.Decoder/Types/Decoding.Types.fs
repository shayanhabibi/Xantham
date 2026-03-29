namespace Xantham.Decoder

open Xantham
/// <summary>
/// Represents 'non' types, like properties, methods, constructors, etc.
/// </summary>
[<RequireQualifiedAccess>]
type NodeStore =
    | Property of TsProperty
    | Parameter of TsParameter
    | Method of TsMethod
    | Constructor of TsConstructor
    | SubstitutionType of TsSubstitutionType
    | ConstructSignature of TsConstructSignature
    | IndexSignature of TsIndexSignature
    | GetAccessor of TsGetAccessor
    | SetAccessor of TsSetAccessor
    | CallSignature of TsCallSignature

type TypeMap = Map<TypeKey, TsType>
type NodeMap = Map<TypeKey, NodeStore>

type LibSet = TypeKey Set

type DecodedResult = {
    TypeMap: TypeMap
    LibSet: LibSet
    NodeMap: NodeMap
    TopLevelKeys: TypeKey list
}


