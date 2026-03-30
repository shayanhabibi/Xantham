namespace Xantham
#if FABLE_COMPILER
open Fable.Core
[<Erase>]
type TypeKey = TypeKey of int
module TypeKey =
    let inline createWith (i: int) = TypeKey i

#else
type TypeKey = System.Int32
module TypeKey =
    let inline createWith (i: int) = i
#endif

/// <summary>
/// Indicates a type can be a member of a <c>TsOverloadableConstruct</c> collection.
/// </summary>
type IOverloadable = interface end

type TsOverloadableConstruct<'T when 'T:>IOverloadable and 'T : equality and 'T : comparison> =
    | NoOverloads of 'T
    | Overloaded of 'T Set
    member this.ValueOrHead =
        match this with
        | NoOverloads value -> value
        | Overloaded values -> Set.maxElement values
    member inline this.ToList() =
        match this with
        | NoOverloads value -> [ value ]
        | Overloaded values -> Set.toList values
    member inline this.ToSeq() =
        match this with
        | NoOverloads value -> seq { value }
        | Overloaded values -> values
    member inline this.ToArray() =
        match this with
        | NoOverloads value -> [| value |]
        | Overloaded values -> values |> Set.toArray
    static member inline Create(value: 'T when 'T :> IOverloadable) = NoOverloads value
    static member inline Create(values: 'T seq when 'T :> IOverloadable) =
        if values |> Seq.isEmpty then failwith "Invalid TsOverloadableConstruct; require at least one value"
        elif Seq.length values = 1 then NoOverloads (values |> Seq.head)
        else Overloaded (values |> Set.ofSeq)
    static member Create(overloads: TsOverloadableConstruct<'T> seq) =
        overloads |> Seq.collect _.ToSeq()
        |> function
            | values when Seq.length values > 1 -> Overloaded (values |> Set.ofSeq)
            | values when Seq.isEmpty values -> failwith "Invalid TsOverloadableConstruct; require at least one value"
            | values -> NoOverloads (values |> Seq.head)
    member this.Values = this.ToArray()

[<RequireQualifiedAccess>]
type TsComment =
    | Summary of string list
    | Returns of string
    | Param of name: string * content: string option
    | Deprecated of string option
    | Remarks of string
    | DefaultValue of string
    | Example of string
    | TypeParam of typeName: string * content: string option
    | Throws of string
[<RequireQualifiedAccess>]
type TsLiteral =
    | String of value: string
    | Int of value: int
    | Float of value: float
    | Bool of value: bool
    | BigInt of value: bigint
    | Null

/// <summary>
/// <para>Represents a single enum case in TypeScript.</para>
/// <para>Reflects:<br/><c>EnumMember</c> in the TS AST and literal enum values in the TS type system.</para>
/// </summary>
/// <example>
/// <code lang="ts">
/// enum Color { Red = 0, Green = 1 }
/// // GlueEnumCase for "Red" with value 0
/// </code>
/// </example>
type TsEnumCase = {
    Source: string option
    FullyQualifiedName: string list
    Name: string
    Value: TsLiteral
    Documentation: TsComment list
}

/// <summary>
/// <para> Represents a TypeScript `enum` type.</para>
/// <para>Reflects: `EnumDeclaration` and the resulting enum type.</para>
/// </summary>
/// <example>
/// <code lang="ts">
/// export enum Direction { Up, Down }
/// </code>
/// </example>
type TsEnumType = {
    Source: string option
    FullyQualifiedName: string list
    Name: string
    Members: TsEnumCase list
    Documentation: TsComment list
}

/// <summary>
/// <para>Represents a top-level or module-scoped <c>var/let/const</c> in TypeScript along with its resolved type.</para>
/// <para>Reflects: <c>VariableDeclaration</c> and its <c>Type</c>.</para>
/// </summary>
/// <example>
/// <code lang="ts">
/// export const answer: number = 42;
/// </code>
/// </example>
type TsVariable = {
    Source: string option
    FullyQualifiedName: string list
    Name: string
    Type: TypeKey
    Documentation: TsComment list
}
    
/// <summary>
/// <para>Represents a function/method parameter.</para>
/// <para>Reflects: `ParameterDeclaration` (including optional `?` and rest `...` syntax).</para>
/// </summary>
/// <example>
/// <code lang="ts">
/// function f(x: number, y?: string, ...rest: boolean[]) {}
/// </code>
/// </example>
type TsParameter = {
    Name: string
    IsOptional: bool
    IsSpread: bool
    Type: TypeKey
    Documentation: TsComment list
}

/// <summary>
/// <para>Represents a generic type parameter with optional constraint and default.</para>
/// <para>Reflects: `TypeParameterDeclaration` and `TypeParameter` (constraint `extends`, default value).</para>
/// <example>
/// <code lang="ts">
/// type Box&lt;T extends object = {}> = { value: T }
/// </code>
/// </example>
/// </summary>
type TsTypeParameter = {
    Name: string
    Constraint: TypeKey option
    Default: TypeKey option
    Documentation: TsComment list
}

/// <summary>
/// A tuple to be used by types which inline <c>TsTypeParameter</c>s rather than referencing by TypeKey.
/// The tuple is used to preserve information for the container type that inlines them.
/// </summary>
type InlinedTsTypeParameter = TypeKey * TsTypeParameter

/// Represents a method-like member (class or interface) or signature-like method.
///
/// Reflects: `MethodDeclaration` (class), `MethodSignature` (interface), and function-like members
/// including optional `?` and `static` modifiers.
/// Example (TS):
/// ```ts
/// interface I { foo(x: number): string }
/// class C { static bar(): void {} }
/// ```
type TsMethod = {
    Name: string
    Parameters: TsParameter list
    Type: TypeKey
    IsOptional: bool
    IsStatic: bool
    Documentation: TsComment list
} with interface IOverloadable

/// Represents a call signature in a type literal or interface.
///
/// Reflects: `CallSignatureDeclaration`.
/// Example (TS):
/// ```ts
/// interface FnLike { (x: number, y: number): number }
/// ```
type TsCallSignature = {
    Documentation: TsComment list
    Parameters: TsParameter list
    Type: TypeKey
} with interface IOverloadable

/// Represents a construct signature (the type of `new` for a value).
///
/// Reflects: `ConstructSignatureDeclaration`.
/// Example (TS):
/// ```ts
/// interface CtorLike { new (x: number): Date }
/// ```
type TsConstructSignature = {
    Type: TypeKey
    Parameters: TsParameter list
} with interface IOverloadable

/// Represents a class constructor declaration.
///
/// Reflects: `ConstructorDeclaration`.
/// Example (TS):
/// ```ts
/// class C { constructor(readonly x: number) {} }
/// ```
type TsConstructor = {
    Documentation: TsComment list
    Parameters: TsParameter list
} with interface IOverloadable
        

[<RequireQualifiedAccess>]
type TsAccessor =
    | ReadOnly
    | WriteOnly
    | ReadWrite

/// Represents a property (class or interface) with additional modifiers.
///
/// Reflects: `PropertyDeclaration` (class) and `PropertySignature` (interface), including
/// `readonly`, `?` optional, `static`, and visibility modifiers.
/// Example (TS):
/// ```ts
/// interface P { readonly name?: string }
/// class C { private static count: number }
/// ```
type TsProperty = {
    Name: string
    Type: TypeKey
    IsStatic: bool
    IsOptional: bool
    IsPrivate: bool
    Accessor: TsAccessor
    Documentation: TsComment list
}

/// Represents a `get` accessor.
///
/// Reflects: `GetAccessorDeclaration`.
/// Example (TS):
/// ```ts
/// class C { get value(): number { return 0 } }
/// ```
type TsGetAccessor = {
    Name: string
    Type: TypeKey
    IsStatic: bool
    IsPrivate: bool
}

/// Represents a `set` accessor.
///
/// Reflects: `SetAccessorDeclaration`.
/// Example (TS):
/// ```ts
/// class C { set value(v: number) {} }
/// ```
type TsSetAccessor = {
    Name: string
    Documentation: TsComment list
    ArgumentType: TypeKey
    IsStatic: bool
    IsPrivate: bool
}

/// Represents an index signature (property access by key type) on an object type.
///
/// Reflects: `IndexSignatureDeclaration` like `[key: string]: number` and readonly variants.
/// Example (TS):
/// ```ts
/// interface MapLike { readonly [k: string]: number }
/// ```
type TsIndexSignature = {
    Parameters: TsParameter list
    Type: TypeKey
    IsReadOnly: bool
}

/// Represents a top-level (or exported) function declaration with its type and generics.
///
/// Reflects: `FunctionDeclaration` and its `Signature`.
/// Example (TS):
/// ```ts
/// export function sum<T extends number>(a: T, b: T): T { return (a + b) as T }
/// ```
type TsFunction = {
    Source: string option
    FullyQualifiedName: string list
    Documentation: TsComment list
    IsDeclared: bool
    Name: string
    Type: TypeKey
    Parameters: TsParameter list
    TypeParameters: InlinedTsTypeParameter list
} with interface IOverloadable

/// Represents a conditional type `T extends U ? X : Y`.
///
/// Reflects: `ConditionalType`.
/// Example (TS):
/// ```ts
/// type IfNumber<T> = T extends number ? 'num' : 'other'
/// ```
type TsConditionalType = {
    Check: TypeKey
    Extends: TypeKey
    True: TypeKey
    False: TypeKey
}

[<RequireQualifiedAccess>]
type TsMember =
    | Method of TsOverloadableConstruct<TsMethod>
    | Property of TsProperty
    | GetAccessor of TsGetAccessor
    | SetAccessor of TsSetAccessor
    | CallSignature of TsOverloadableConstruct<TsCallSignature>
    | IndexSignature of TsIndexSignature
    | ConstructSignature of TsOverloadableConstruct<TsConstructSignature>

    /// <summary>
    /// For SRTP
    /// </summary>
    member inline this.AsGlueMember = this
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsMethod =
        match this with
        | TsMember.Method value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsProperty =
        match this with
        | TsMember.Property value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsGetAccessor =
        match this with
        | TsMember.GetAccessor value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsSetAccessor =
        match this with
        | TsMember.SetAccessor value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsCallSignature =
        match this with
        | TsMember.CallSignature value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsIndexSignature =
        match this with
        | TsMember.IndexSignature value -> ValueSome value
        | _ -> ValueNone
    /// <summary>
    /// For SRTP
    /// </summary>
    member this.IfIsConstructSignature =
        match this with
        | TsMember.ConstructSignature value -> ValueSome value
        | _ -> ValueNone
    /// A member inside classes/interfaces/type literals; each case mirrors the
    /// corresponding TypeScript declaration or signature node.

/// Represents a reference to a named type with optional type arguments.
///
/// Reflects: `TypeReference` (e.g., `Foo<Bar>`), `ExpressionWithTypeArguments` in heritage clauses,
/// and similar reference forms. `ResolvedType` may hold a concrete resolved key when available.
/// Example (TS):
/// ```ts
/// interface Box<T> { value: T }
/// type IntBox = Box<number>
/// ```
type TsTypeReference = {
    Type: TypeKey
    TypeArguments: TypeKey list
    ResolvedType: TypeKey option
}

/// Represents `extends` relationships for interfaces.
/// Reflects: `HeritageClause` with `extends` entries on interfaces.
type TsInterfaceHeritage = {
    Extends: TsTypeReference list
}

/// Represents `extends` and `implements` relationships for classes.
/// Reflects: `HeritageClause` with `extends`/`implements` on classes.
type TsClassHeritage = {
    Implements: TsTypeReference option
    Extends: TsTypeReference list
}

/// Represents a TypeScript `interface` type with its members and generics.
///
/// Reflects: `InterfaceDeclaration` and its computed `InterfaceType`.
/// Example (TS):
/// ```ts
/// export interface IterableLike<T> extends Iterable<T> { length: number }
/// ```
type TsInterface = {
    Source: string option
    FullyQualifiedName: string list
    Enumerable: bool
    Name: string
    Members: TsMember list
    TypeParameters: InlinedTsTypeParameter list
    Documentation: TsComment list
    Heritage: TsInterfaceHeritage
}

/// Represents a type literal `{ ... }` with inline members.
///
/// Reflects: `TypeLiteralNode` and the corresponding anonymous object type.
/// Example (TS):
/// ```ts
/// type Point = { x: number; y: number; (x: number): string }
/// ```
type TsTypeLiteral = {
    Members: TsMember list
}

/// Represents an indexed access type, where a property type is selected by an index type.
///
/// Reflects: `IndexedAccessType`.
/// Example (TS):
/// ```ts
/// type Foo<T, K extends keyof T> = T[K]
/// ```
type TsIndexAccessType = {
    Object: TypeKey
    Index: TypeKey
}

/// Represents a named alias for a type, possibly generic.
///
/// Reflects: `TypeAliasDeclaration`.
/// Example (TS):
/// ```ts
/// export type ReadonlyList<T> = ReadonlyArray<T>
/// ```
type TsTypeAlias = {
    Source: string option
    FullyQualifiedName: string list
    Name: string
    Type: TypeKey
    TypeParameters: InlinedTsTypeParameter list
    Documentation: TsComment list
}

/// Represents a `namespace` or `module` declaration and the collected types within it.
///
/// Reflects: `ModuleDeclaration`, `ModuleBlock`, and `NamespaceExportDeclaration`.
/// Example (TS):
/// ```ts
/// declare namespace MyLib { const version: string }
/// ```
type TsModule = {
    Source: string option
    FullyQualifiedName: string list
    Name: string
    IsNamespace: bool
    IsRecursive: bool
    Types: TypeKey list
}

/// Represents a substitution type: a type parameter substituted with a concrete type
/// under its constraint context.
///
/// Reflects: `SubstitutionType` in the TS type system.
type TsSubstitutionType = {
    Base: TypeKey
    Constraint: TypeKey
}

/// Represents a TypeScript `class` with constructors, members, generics and heritage.
///
/// Reflects: `ClassDeclaration` and its computed `InterfaceType` for instance side.
/// Example (TS):
/// ```ts
/// export class C<T> implements Iterable<T> { constructor(public value: T) {} length = 0 }
/// ```
type TsClass = {
    Source: string option
    FullyQualifiedName: string list
    Enumerable: bool
    Name: string
    Constructors: TsConstructor list
    Members: TsMember list
    TypeParameters: InlinedTsTypeParameter list
    Heritage: TsClassHeritage
}

/// Represents a union type.
///
/// Reflects: `UnionType` and `UnionTypeNode`.
/// Example (TS): `type A = string | number`
type TsTypeUnion = TsTypeUnion of TypeKey list with
    member inline this.Types = match this with TsTypeUnion t -> t
/// Represents an intersection type.
///
/// Reflects: `IntersectionType` and `IntersectionTypeNode`.
/// Example (TS): `type B = { x: number } & { y: number }`
type TsTypeIntersection = TsTypeIntersection of TypeKey list with
    member inline this.Types = match this with TsTypeIntersection t -> t

/// Represents a tuple element's type and modifiers.
///
/// Reflects: tuple element nodes including optional `?` and rest `...` (variadic) labels.
type TsTupleElementType = {
    Type: TypeKey
    IsOptional: bool
    IsRest: bool
}
type TsTupleElement =
    | FixedLabeled of string * TsTupleElementType
    | Variadic of TypeKey
    | Fixed of TsTupleElementType

    member this.Type =
        match this with
        | FixedLabeled (_, { Type = value })
        | Variadic value
        | Fixed { Type = value } -> value
    member this.IsOptional =
        match this with
        | Fixed { IsOptional = value }
        | FixedLabeled (_, { IsOptional = value }) -> value
        | _ -> false
    member this.IsRest =
        match this with
        | FixedLabeled (_, { IsRest = true })
        | Fixed { IsRest = true }
        | Variadic _ -> true
        | _ -> false
        

/// Represents a tuple type and its structure.
///
/// Reflects: `TupleType` and `TupleTypeNode` including readonly tuples.
/// Example (TS):
/// ```ts
/// type T = readonly [x: number, y?: string, ...rest: boolean[]]
/// ```
type TsTuple = {
    IsReadOnly: bool
    FixedLength: int
    MinRequired: int
    Types: TsTupleElement list
}

/// Represents the index type (key type) of an object for mapped/indexed operations.
///
/// Reflects: `IndexType` used by the checker for `keyof` and index queries.
type TsIndex = {
    Type: TypeKey
}

/// Represents a template literal type with string segments and interpolated types.
///
/// Reflects: `TemplateLiteralType` at the checker layer (TypeFlags.TemplateLiteral).
/// Example (TS): `` `on${string}` `` where texts = ["on", ""] and types = [string]
type TsTemplateLiteralType = {
    Texts: string list
    Types: TypeKey list
}

/// Represents a type predicate or assertion signature like `x is Foo` or `asserts x is Foo`.
///
/// Reflects: `TypePredicateNode` and `TypePredicate`.
/// Example (TS):
/// ```ts
/// function isNum(x: unknown): x is number { return typeof x === 'number' }
/// function assertIsNum(x: unknown): asserts x is number { if (typeof x !== 'number') throw new Error() }
/// ```
type TsTypePredicate = {
    ParameterName: string
    Type: TypeKey
    IsAssertion: bool
}

and [<RequireQualifiedAccess>] TsType =
    | GlobalThis
    | Conditional of TsConditionalType
    | Interface of TsInterface
    | Class of TsClass
    | Variable of TsVariable
    | Primitive of TypeKindPrimitive
    | Enum of TsEnumType
    | EnumCase of TsEnumCase
    | TypeAlias of TsTypeAlias
    | Function of TsOverloadableConstruct<TsFunction>
    | Union of TsTypeUnion
    | Intersection of TsTypeIntersection
    | Literal of TsLiteral
    | IndexedAccess of TsIndexAccessType
    | Module of TsModule
    | TypeReference of TsTypeReference
    | Array of TsType
    | TypeParameter of TsTypeParameter
    | ReadOnly of TsType
    | Tuple of TsTuple
    | Index of TsIndex
    | Predicate of TsTypePredicate
    | TypeLiteral of TsTypeLiteral
    | TemplateLiteral of TsTemplateLiteralType
    | Optional of TsTypeReference

    /// Discriminated union of all high-level TypeScript type shapes the reader can produce.
    /// Each case mirrors a concrete TS type or declaration form (see individual types above).
and [<RequireQualifiedAccess>] TypeKindPrimitive =
    | Any
    | Unknown
    | Never
    | Void
    | Undefined
    | Null
    | String
    | Integer
    | Number
    | Boolean
    | BigInt
    | ESSymbol
    | NonPrimitive
    /// Maps directly to TypeScript primitive types and some special built-ins
    /// (e.g., `any`, `unknown`, `never`, `void`, `undefined`, `null`, `symbol`).
    member this.TypeKey =
        match this with
        | String -> TypeKey.createWith -1
        | Integer -> TypeKey.createWith -2
        | Number -> TypeKey.createWith -3
        | Any -> TypeKey.createWith -7
        | Unknown -> TypeKey.createWith -14
        | Never -> TypeKey.createWith -12
        | Void -> TypeKey.createWith -9
        | Undefined -> TypeKey.createWith -8
        | Null -> TypeKey.createWith -5
        | Boolean -> TypeKey.createWith -4
        | BigInt -> TypeKey.createWith -3
        | ESSymbol -> TypeKey.createWith -11
        | NonPrimitive -> TypeKey.createWith -10

type [<RequireQualifiedAccess>] TsAstNode =
    | TemplateLiteral of TsTemplateLiteralType
    | GlobalThis
    | Tuple of TsTuple
    | Interface of TsInterface
    | Variable of TsVariable
    | Primitive of TypeKindPrimitive
    | Predicate of TsTypePredicate
    | Literal of TsLiteral
    | TypeLiteral of TsTypeLiteral
    | Property of TsProperty
    | Parameter of TsParameter
    | TypeParameter of TsTypeParameter
    | IndexAccessType of TsIndexAccessType
    | FunctionDeclaration of TsOverloadableConstruct<TsFunction>
    | Method of TsOverloadableConstruct<TsMethod>
    | Alias of TsTypeAlias
    | Constructor of TsOverloadableConstruct<TsConstructor>
    | ConstructSignature of TsOverloadableConstruct<TsConstructSignature>
    | IndexSignature of TsIndexSignature
    | Index of TsIndex
    | TypeReference of TsTypeReference
    | Array of TsTypeReference
    | Enum of TsEnumType
    | EnumCase of TsEnumCase
    | SubstitutionType of TsSubstitutionType
    | Conditional of TsConditionalType
    | Class of TsClass
    | Union of TsTypeUnion
    | Intersection of TsTypeIntersection
    | Optional of TsTypeReference
    | GetAccessor of TsGetAccessor
    | SetAccessor of TsSetAccessor
    | CallSignature of TsOverloadableConstruct<TsCallSignature>
    | Module of TsModule


