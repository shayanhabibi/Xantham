namespace Xantham.Decoder

open Xantham

type TypeMap = Map<TypeKey, TsType>
type ExportMap = Map<string, TsExportDeclaration>
type ExportTypeMap = Map<TypeKey, TsExportDeclaration>

type LibSet = TypeKey Set

type DecodedResult = {
    TypeMap: TypeMap
    ExportTypeMap: ExportTypeMap
    ExportMap: ExportMap
}


