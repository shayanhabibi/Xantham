namespace Xantham.Decoder

open Xantham
/// <summary>
/// Represents 'non' types, like properties, methods, constructors, etc.
/// </summary>
[<RequireQualifiedAccess>]
type NodeStore =
    | Property of TsProperty
    | Parameter of TsParameter
    | Method of TsOverloadableConstruct<TsMethod>
    | Constructor of TsOverloadableConstruct<TsConstructor>
    | SubstitutionType of TsSubstitutionType
    | ConstructSignature of TsOverloadableConstruct<TsConstructSignature>
    | IndexSignature of TsIndexSignature
    | GetAccessor of TsGetAccessor
    | SetAccessor of TsSetAccessor
    | CallSignature of TsOverloadableConstruct<TsCallSignature>

type TypeMap = Map<TypeKey, TsType>
type NodeMap = Map<TypeKey, NodeStore>

type LibSet = TypeKey Set

type DecodedResult = {
    TypeMap: TypeMap
    LibSet: LibSet
    NodeMap: NodeMap
    TopLevelKeys: TypeKey list
}


