namespace Xantham.Decoder

open Xantham

type TypeMap = Map<TypeKey, TsType>

type LibSet = TypeKey Set

type DecodedResult = {
    TypeMap: TypeMap
    LibSet: LibSet
    TopLevelKeys: TypeKey list
}


