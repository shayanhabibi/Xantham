/// <summary>
/// Discriminated unions tagging a <c>TypeKey</c> with the kind of construct it identifies.
/// Used by consumers (e.g. generators) that need to dispatch on the structural kind of
/// a type or export without re-walking the full <c>TsType</c>/<c>TsExportDeclaration</c>.
/// </summary>
module Xantham.Decoder.CodeKey

open Xantham

/// <summary>
/// A <c>TypeKey</c> tagged with the <c>TsType</c> case it identifies.
/// </summary>
/// <category index="2">Code Key Tagging</category>
[<Struct>]
type TypeCodeKey =
    /// The synthetic <c>globalThis</c> reference type.
    | GlobalThis
    /// A conditional type (<c>T extends U ? X : Y</c>).
    | Conditional of conditional: TypeKey
    /// A primitive type (<c>string</c>, <c>number</c>, <c>boolean</c>, etc.).
    | Primitive of primitive: TypeKey
    /// A single case of an enum.
    | EnumCase of enumCase: TypeKey
    /// A union type (<c>A | B</c>).
    | Union of union: TypeKey
    /// An intersection type (<c>A &amp; B</c>).
    | Intersection of intersection: TypeKey
    /// A literal type (string, number, boolean, or bigint literal).
    | Literal of literal: TypeKey
    /// An indexed access type (<c>T[K]</c>).
    | IndexAccess of indexAccess: TypeKey
    /// A reference to another type (named, with optional type arguments).
    | TypeReference of typeReference: TypeKey
    /// An array type (<c>T[]</c> or <c>Array&lt;T&gt;</c>).
    | Array of array: TypeKey
    /// A type parameter binding (<c>T</c>).
    | TypeParameter of typeParameter: TypeKey
    /// A <c>readonly</c>-modified type.
    | ReadOnly of readOnly: TypeKey
    /// A tuple type (fixed, labeled, or variadic).
    | Tuple of tuple: TypeKey
    /// A <c>keyof</c> index type (<c>keyof T</c>).
    | Index of index: TypeKey
    /// A type predicate (<c>x is T</c>).
    | Predicate of predicate: TypeKey
    /// An anonymous type literal (<c>{ x: number }</c>).
    | TypeLiteral of typeLiteral: TypeKey
    /// A template literal type (<c>`prefix-${T}`</c>).
    | TemplateLiteral of templateLiteral: TypeKey
    /// An optional type wrapper.
    | Optional of optional: TypeKey
    /// A substitution type used in conditional/distributive type checking.
    | Substitution of substitution: TypeKey

/// <summary>
/// A <c>TypeKey</c> tagged with the <c>TsExportDeclaration</c> case it identifies.
/// </summary>
/// <category index="2">Code Key Tagging</category>
[<Struct>]
type ExportCodeKey =
    /// An exported variable declaration.
    | Variable of variable: TypeKey
    /// An exported interface declaration.
    | Interface of ``interface``: TypeKey
    /// An exported type alias declaration.
    | TypeAlias of typeAlias: TypeKey
    /// An exported class declaration.
    | Class of ``class``: TypeKey
    /// An exported enum declaration.
    | Enum of enum: TypeKey
    /// An exported module/namespace declaration.
    | Module of ``module``: TypeKey
    /// An exported function declaration (potentially overloaded).
    | Function of func: TypeKey

/// <summary>
/// Outer envelope distinguishing whether a <c>TypeKey</c> identifies an exported
/// declaration (<see cref="T:ExportCodeKey"/>) or a structural type
/// (<see cref="T:TypeCodeKey"/>).
/// </summary>
/// <category index="2">Code Key Tagging</category>
[<Struct>]
type CodeKey =
    /// The key identifies an exported declaration.
    | Export of export: ExportCodeKey
    /// The key identifies a structural type only.
    | Type of typeCode: TypeCodeKey
