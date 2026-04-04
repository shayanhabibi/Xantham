namespace Xantham.Decoder

open Xantham

type TypeMap = Map<TypeKey, TsType>
type ExportTypeMap = Map<TypeKey, TsExportDeclaration>
type ExportMap = Map<string, Set<TypeKey>>

type LibSet = TypeKey Set

type CompressionResult = {
    Types: TypeMap
    Exports: ExportTypeMap
    TopLevelExports: ExportTypeMap
}

type DecodedResult = {
    TypeMap: TypeMap
    ExportTypeMap: ExportTypeMap
    ExportMap: ExportMap
    TopLevelExports: TypeKey list
    LibEsExports: TypeKey list
}


