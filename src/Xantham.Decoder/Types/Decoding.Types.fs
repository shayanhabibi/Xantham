namespace Xantham.Decoder

open Xantham

type TypeMap = Map<TypeKey, TsType>
type ExportArray = TsExportDeclaration array
type LibSet = TypeKey Set
type DuplicateMap = Map<TypeKey, TsAstNode * TsAstNode array>
type DuplicateResult = {
    Key: TypeKey
    Winner: TsAstNode
    Results: TsAstNode array
}
type DuplicateRemapFun = DuplicateResult -> TsAstNode
type RemapFun = TypeKey -> TsAstNode -> TsAstNode
type RuntimeOptions = {
    File: string
    Remap: RemapFun voption
    DuplicateRemap: DuplicateRemapFun voption
}

type DecodedResult = {
    TypeMap: TypeMap
    Exports: ExportArray
    LibSet: LibSet
    PrimaryExports: ExportArray
    DuplicateMap: DuplicateMap
}


