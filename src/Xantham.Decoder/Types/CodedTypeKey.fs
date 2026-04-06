module Xantham.Decoder.CodeKey

open Xantham

[<Struct>]
type TypeCodeKey =
    | GlobalThis
    | Conditional of TypeKey
    | Primitive of TypeKey
    | EnumCase of TypeKey
    | Union of TypeKey
    | Intersection of TypeKey
    | Literal of TypeKey
    | IndexAccess of TypeKey
    | TypeReference of TypeKey
    | Array of TypeKey
    | TypeParameter of TypeKey
    | ReadOnly of TypeKey
    | Tuple of TypeKey
    | Index of TypeKey
    | Predicate of TypeKey
    | TypeLiteral of TypeKey
    | TemplateLiteral of TypeKey
    | Optional of TypeKey
    | Substitution of TypeKey

[<Struct>]
type ExportCodeKey =
    | Variable of TypeKey
    | Interface of TypeKey
    | TypeAlias of TypeKey
    | Class of TypeKey
    | Enum of TypeKey
    | Module of TypeKey
    | Function of TypeKey

[<Struct>]
type CodeKey =
    | Export of export: ExportCodeKey
    | Type of typeCode: TypeCodeKey