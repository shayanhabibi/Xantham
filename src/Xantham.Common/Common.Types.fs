namespace Xantham
#if FABLE_COMPILER
open Thoth.Json
open Fable.Core
open Fable.Core.JsInterop
/// <summary>
/// In Fable, this is an array. In DotNet, this is a list.
/// </summary>
type listOrArray<'T> = 'T array
/// <summary>
/// A unique identifier for a type.
/// </summary>
[<Erase>]
type TypeKey = TypeKey of int
module TypeKey =
    let inline createWith (i: int) = TypeKey i
    let encode: Encoder<TypeKey> = unbox >> Encode.int
    let decode: Decoder<TypeKey> = Decode.int >> unbox
#else
open Thoth.Json.Net
[<AutoOpen>]
module private Operators =
    let inline (==>) x y = x, y
/// <summary>
/// In Fable, this is an array. In DotNet, this is a list.
/// </summary>
type listOrArray<'T> = 'T list
/// <summary>
/// A unique identifier for a type.
/// </summary>
type TypeKey = System.Int32
module TypeKey =
    let inline createWith (i: int) = i
    let encode: Encoder<TypeKey> = Encode.int
    let decode: Decoder<TypeKey> = Decode.int
#endif

type ConditionalExport =
    | Types of ExportValue
    | Default of ExportValue
    | Browser of ExportValue
    | Development of ExportValue
    | Production of ExportValue
    | NodeAddons of ExportValue
    | Node of ExportValue
    | Import of ExportValue
    | Require of ExportValue
    | ModuleSync of ExportValue
    | Module of ExportValue
    | ESNext of ExportValue
    | Unknown of key: string * value: ExportValue

and ExportValue =
    | String of string
    | Conditional of ConditionalExport list
#if FABLE_COMPILER
[<Erase>]
#endif
[<Struct>]
type ExportPath = ExportPath of string with
    member inline this.Value = let (ExportPath value) = this in value
    static member inline Create value = ExportPath value
    
type Export = Map<ExportPath, ExportValue>
    
[<Struct>]
type PackageId = PackageId of name: string * version: string

[<Struct>]
type SubModuleId = SubModuleId of packageId: PackageId * subModuleName: string

[<Struct>]
type Package = {
    Name: string
    Version: string
    Json: Export voption
    SubModules: SubModuleId listOrArray
    Entry: SubModuleId listOrArray
} with member inline this.Key = PackageId(this.Name, this.Version)

[<Struct>]
type SubModule = {
    Package: PackageId
    Name: string
    Path: string
} with member inline this.Key = SubModuleId(this.Package, this.Name)

[<Struct>]
type SubModuleRelation = {
    Dependent: SubModuleId
    Dependency: SubModuleId
}

[<Struct>]
type ExportPoint = {
    Name: string
    SubModule: SubModuleId
}

/// <summary>
/// Can be absent on a declaration in two scenarios:<br/>
/// 1. When the declaration is not exported from the module.<br/>
/// 2. When the declaration is a default library type<br/>
/// <br/>
/// In the case of (1), the declaration still exists in our type hierarchy
/// as it may be a structural member or contributor to some type.
/// </summary>
/// <remarks>
/// This provides the names with which a declaration is exported per submodule.
/// This will allow you to correctly generate import statements, as the imported
/// name does not have to be related to the declaration name.<br/><br/>
/// A declaration which has no associated export collection, and contains a file path
/// in its fully qualified name, is a declaration that is not explicitly exported.
/// A declaration which has no associated export collection, and does not contain
/// a file path in its fully qualified name, is a default library declaration.
/// </remarks>
[<Struct>]
type ExportCollection = {
    Canonical: ExportPoint
    Aliases: ExportPoint listOrArray
}

[<Struct>]
type Source =
    | LibEs of fileName: string
    /// <summary>
    /// Declarations without export collections are considered entirely internal.
    /// They are not exported from any module, so we can safely assume their exclusive origin.
    /// </summary>
    /// <param name="subModuleId"></param>
    | PackageInternal of subModuleId: SubModuleId
    | Package of exportCollection: ExportCollection

[<Struct>]
type Metadata = {
    Source: Source
}

/// <summary>
/// Indicates a type can be a member of a <c>TsOverloadableConstruct</c> collection.
/// </summary>
/// <category>Auxiliary</category>
type IOverloadable = interface end

/// <summary>
/// Represents a non-empty collection of overloads of a type.
/// </summary>
/// <remarks>
/// Constructors ensure that the collection is never empty.
/// </remarks>
/// <category>Container</category>
type TsOverloadableConstruct<'T when 'T:>IOverloadable and 'T : equality and 'T : comparison> =
    | NoOverloads of 'T
    | Overloaded of 'T Set
    /// <summary>
    /// Selects the value if there is only one signature (no overloads), or the <i>max</i> element of the
    /// overload set if there are multiple signatures.
    /// </summary>
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
    static member Combine (lhs: TsOverloadableConstruct<'T>) (rhs: TsOverloadableConstruct<'T>) =
        match lhs, rhs with
        | NoOverloads l, NoOverloads r when l <> r ->
            Set [| l; r |] |> Overloaded
        | NoOverloads l, NoOverloads _ -> NoOverloads l
        | Overloaded l, NoOverloads r -> Overloaded (Set.add r l)
        | Overloaded l, Overloaded r -> Overloaded (Set.union l r)
        | NoOverloads l, Overloaded r -> Overloaded (Set.add l r)
    /// <summary>
    /// Alias for <c>_.ToArray()</c>.
    /// </summary>
    member this.Values = this.ToArray()

/// <summary>
/// DU of TS comment tag types.
/// </summary>
/// <category>Auxiliary</category>
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

/// <summary>
/// DU of TS literal types.
/// </summary>
/// <category>Type Representation</category>
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
/// <category>Type Representation</category>
type TsEnumCase = {
    Parent: TypeKey
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
/// <category>Type Representation</category>
type TsEnumType = {
    Metadata: Metadata
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
/// <category>Declaration Representation</category>
type TsVariable = {
    Metadata: Metadata
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
/// <category>Declaration Representation</category>
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
/// <category>Type Representation</category>
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
/// <category>Auxiliary</category>
type InlinedTsTypeParameter = TypeKey * TsTypeParameter

/// <summary>
/// <para>Represents a method-like member (class or interface) or signature-like method.</para>
/// <para>Reflects: `MethodDeclaration` (class), `MethodSignature` (interface), and function-like members
/// including optional `?` and `static` modifiers.</para>
/// <example><code lang="ts">
/// interface I { foo(x: number): string }
/// class C { static bar(): void {} }
/// </code></example>
/// </summary>
/// <category>Declaration Representation</category>
type TsMethod = {
    Name: string
    Parameters: TsParameter list
    Type: TypeKey
    TypeParameters: InlinedTsTypeParameter list
    IsOptional: bool
    IsStatic: bool
    Documentation: TsComment list
} with interface IOverloadable

/// <summary>
/// <para>Represents a call signature in a type literal or interface.</para>
/// <para>Reflects: `CallSignatureDeclaration`.</para>
/// <example><code lang="ts">
/// interface FnLike { (x: number, y: number): number }
/// </code></example>
/// </summary>
/// <category>Declaration Representation</category>
type TsCallSignature = {
    Documentation: TsComment list
    Parameters: TsParameter list
    Type: TypeKey
    TypeParameters: InlinedTsTypeParameter list
} with interface IOverloadable

/// <summary>
/// <para>Represents a construct signature (the type of `new` for a value).</para>
/// <para>Reflects: `ConstructSignatureDeclaration`.</para>
/// <example><code lang="ts">
/// interface CtorLike { new (x: number): Date }
/// </code></example>
/// </summary>
/// <category>Declaration Representation</category>
type TsConstructSignature = {
    Type: TypeKey
    Parameters: TsParameter list
    TypeParameters: InlinedTsTypeParameter list
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
    Metadata: Metadata
    FullyQualifiedName: string list
    Documentation: TsComment list
    IsDeclared: bool
    Name: string
    Type: TypeKey
    Parameters: TsParameter list
    TypeParameters: InlinedTsTypeParameter list
    /// <summary>
    /// The type key or signature key of the function's signature.
    /// This key should return a type literal with call signatures.
    /// </summary>
    SignatureKey: TypeKey
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
    Metadata: Metadata
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
    Metadata: Metadata
    FullyQualifiedName: string list
    Name: string
    Type: TypeKey
    TypeParameters: InlinedTsTypeParameter list
    Documentation: TsComment list
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
    Metadata: Metadata
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

/// <summary>
/// Represents a type query like `typeof Foo` or `typeof Foo.bar`.
/// The type query is resolved to the type of the referenced entity.
/// </summary>
/// <remarks>
/// The name is provided so that enum like type query unions can have
/// names inferred from the query appropriately.<br/>
/// This is the cheapest schema representation.
/// </remarks>
type TsTypeQuery = {
    FullyQualifiedName: string list
    Type: TypeKey
}
/// <summary>
/// Represents a <c>namespace</c> or <c>module</c> declaration and the collected types within it.
/// 
/// Reflects: <c>ModuleDeclaration</c>, <c>ModuleBlock</c>, and <c>NamespaceExportDeclaration</c>.
/// </summary>
/// <example>
/// <code lang="ts">
/// declare namespace MyLib { const version: string }
/// </code>
/// </example>
and TsModule = {
    Metadata: Metadata
    FullyQualifiedName: string list
    Name: string
    IsNamespace: bool
    IsRecursive: bool
    Exports: TsExportDeclaration list
}
and [<RequireQualifiedAccess>] TsType =
    | GlobalThis
    | Conditional of TsConditionalType
    | Interface of TsInterface
    | Class of TsClass
    | Primitive of TypeKindPrimitive
    | Enum of TsEnumType
    | EnumCase of TsEnumCase
    | Union of TsTypeUnion
    | Intersection of TsTypeIntersection
    | Literal of TsLiteral
    | IndexedAccess of TsIndexAccessType
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
    | Substitution of TsSubstitutionType
    | TypeQuery of TsTypeQuery

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

and [<RequireQualifiedAccess>] TsExportDeclaration =
    | Variable of TsVariable
    | Interface of TsInterface
    | TypeAlias of TsTypeAlias
    | Class of TsClass
    | Enum of TsEnumType
    | Module of TsModule
    | Function of TsOverloadableConstruct<TsFunction>

and [<RequireQualifiedAccess>] TsAstNode =
    | TemplateLiteral of TsTemplateLiteralType
    | GlobalThis
    | Tuple of TsTuple
    | Interface of TsInterface
    | Variable of TsVariable
    | Primitive of TypeKindPrimitive
    | Predicate of TsTypePredicate
    | Literal of TsLiteral
    | TypeLiteral of TsTypeLiteral
    | TypeParameter of TsTypeParameter
    | IndexAccessType of TsIndexAccessType
    | FunctionDeclaration of TsOverloadableConstruct<TsFunction>
    | Alias of TsTypeAlias
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
    | Module of TsModule
    | TypeQuery of TsTypeQuery
    member this.IsExport =
        match this with
        | Class _
        | Interface _
        | Variable _
        | FunctionDeclaration _
        | Alias _
        | Enum _
        | Module _ -> true
        | TemplateLiteral _
        | GlobalThis 
        | Tuple _
        | Primitive _
        | Predicate _
        | Literal _
        | TypeLiteral _
        | TypeParameter _
        | IndexAccessType _
        | Index _
        | TypeReference _
        | Array _
        | EnumCase _
        | SubstitutionType _
        | Conditional _
        | Union _
        | Intersection _
        | TypeQuery _
        | Optional _ -> false
    member this.IsType =
        match this with
        | FunctionDeclaration _
        | Alias _
        | Module _
        | Variable _ -> false
        | Class _
        | Interface _
        | Enum _
        | TemplateLiteral _
        | GlobalThis 
        | Tuple _
        | Primitive _
        | Predicate _
        | Literal _
        | TypeLiteral _
        | TypeParameter _
        | IndexAccessType _
        | Index _
        | TypeReference _
        | Array _
        | EnumCase _
        | SubstitutionType _
        | Conditional _
        | Union _
        | Intersection _
        | TypeQuery _
        | Optional _ -> true
    member this.ToType() =
        match this with
        | FunctionDeclaration _
        | Alias _
        | Module _
        | Variable _ -> ValueNone
        | Class v -> TsType.Class v |> ValueSome
        | Interface v -> TsType.Interface v |> ValueSome
        | Enum v -> TsType.Enum v |> ValueSome
        | TemplateLiteral v -> TsType.TemplateLiteral v |> ValueSome
        | GlobalThis -> TsType.GlobalThis |> ValueSome
        | Tuple v -> TsType.Tuple v |> ValueSome
        | Primitive v -> TsType.Primitive v |> ValueSome
        | Predicate v -> TsType.Predicate v |> ValueSome
        | Literal v -> TsType.Literal v |> ValueSome
        | TypeLiteral v -> TsType.TypeLiteral v |> ValueSome
        | TypeParameter v -> TsType.TypeParameter v |> ValueSome
        | IndexAccessType v -> TsType.IndexedAccess v |> ValueSome
        | Index v -> TsType.Index v |> ValueSome
        | TypeReference v -> TsType.TypeReference v |> ValueSome
        | Array v -> (TsType.TypeReference >> TsType.Array >> ValueSome) v
        | EnumCase v -> TsType.EnumCase v |> ValueSome
        | SubstitutionType v -> TsType.Substitution v |> ValueSome
        | Conditional v -> TsType.Conditional v |> ValueSome
        | Union v -> TsType.Union v |> ValueSome
        | Intersection v -> TsType.Intersection v |> ValueSome
        | Optional v -> TsType.Optional v |> ValueSome
        | TypeQuery v -> TsType.TypeQuery v |> ValueSome
    member this.ToExportDeclaration() =
        match this with
        | Class v -> TsExportDeclaration.Class v |> ValueSome
        | Interface v -> TsExportDeclaration.Interface v |> ValueSome
        | Variable v -> TsExportDeclaration.Variable v |> ValueSome
        | FunctionDeclaration v -> TsExportDeclaration.Function v |> ValueSome
        | Alias v -> TsExportDeclaration.TypeAlias v |> ValueSome
        | Enum v -> TsExportDeclaration.Enum v |> ValueSome
        | Module v -> TsExportDeclaration.Module v |> ValueSome
        | TemplateLiteral _
        | GlobalThis 
        | Tuple _
        | Primitive _
        | Predicate _
        | Literal _
        | TypeLiteral _
        | TypeParameter _
        | IndexAccessType _
        | Index _
        | TypeReference _
        | Array _
        | EnumCase _
        | SubstitutionType _
        | Conditional _
        | Union _
        | Intersection _
        | TypeQuery _
        | Optional _ -> ValueNone
    member this.ToTypeExportDeclaration() =
        this.ToType(), this.ToExportDeclaration()

module Schema =
    type TsIdentityKey =
        | DeclarationFile of file: string * startPos: int * endPos: int
        | Symbol of name: string
        | Type of TypeKey
    type DuplicateEncoding<'T> = {
        Identity: TsIdentityKey
        Value: 'T
    }
    type PackageMap = {
        Packages: Map<PackageId, Package>
        SubModuleRelations: SubModuleRelation listOrArray
        SubModules: Map<SubModuleId, SubModule>
    }
    type EncodedResult = {
        PackageMap: PackageMap
        ExportedDeclarations: Map<TypeKey, TsExportDeclaration>
        Types: Map<TypeKey, TsType>
        DuplicateExports: Map<TypeKey, DuplicateEncoding<TsExportDeclaration> list>
        DuplicateTypes: Map<TypeKey, DuplicateEncoding<TsType> list>
        TopLevelExports: TypeKey list
        LibEsExports: TypeKey list
    }

(* =================================== *)
(* ===== END OF TYPE DEFINITIONS ===== *)
(* =================================== *)
        
        
(* =================================== *)
(* ==== THOTH ENCODERS & DECODERS ==== *)
(* =================================== *)

[<AutoOpen>]
module private Utils =
    let inline mock<'T> = Unchecked.defaultof<'T>
    module Decode =
        let listOrArray decoder path value =
            #if FABLE_COMPILER
            Decode.array decoder path value
            #else
            Decode.list decoder path value
            #endif
    module Encode =
        let listOrArray =
            #if FABLE_COMPILER
            Encode.array
            #else
            Encode.list
            #endif

module ConditionalExport =
    let create key =
        match key with
        | "types" | "Types" -> Types
        | "default" | "Default" -> Default
        | "browser" | "Browser" -> Browser
        | "development" | "Development" -> Development
        | "production" | "Production" -> Production
        | "node-addons" | "NodeAddons" -> NodeAddons
        | "node" | "Node" -> Node
        | "import" | "Import" -> Import
        | "require" | "Require" -> Require
        | "module" | "Module" -> Module
        | "module-sync" | "ModuleSync" -> ModuleSync
        | "esnext" | "ESNext" -> ESNext 
        | key -> fun value -> Unknown(key, value)

module private ExportValueImpl =
    let rec encode (value: ExportValue) =
        match value with
        | ExportValue.String value -> Encode.string value
        | ExportValue.Conditional values -> encodeConditionals values
    and encodeConditionals (value: ConditionalExport list) =
        value
        |> List.map (function
            | Types value -> nameof ConditionalExport.Types, encode value
            | Default exportValue -> nameof ConditionalExport.Default, encode exportValue
            | Browser exportValue -> nameof ConditionalExport.Browser, encode exportValue
            | Development exportValue -> nameof ConditionalExport.Development, encode exportValue
            | Production exportValue -> nameof ConditionalExport.Production, encode exportValue
            | NodeAddons exportValue -> nameof ConditionalExport.NodeAddons, encode exportValue
            | Node exportValue -> nameof ConditionalExport.Node, encode exportValue
            | Import exportValue -> nameof ConditionalExport.Import, encode exportValue
            | Require exportValue -> nameof ConditionalExport.Require, encode exportValue
            | ModuleSync exportValue -> nameof ConditionalExport.ModuleSync, encode exportValue
            | Module exportValue -> nameof ConditionalExport.Module, encode exportValue
            | ESNext exportValue -> nameof ConditionalExport.ESNext, encode exportValue
            | Unknown(key, value) -> key, encode value
            )
        |> Encode.object
    let rec decode: Decoder<ExportValue> = fun path value ->
        Decode.oneOf [
            Decode.string |> Decode.map ExportValue.String
            decodeConditionals |> Decode.map ExportValue.Conditional
        ] path value
    and decodeConditionals: Decoder<ConditionalExport list> =
        Decode.keyValuePairs decode
        |> Decode.map (List.map (fun (key, value) -> ConditionalExport.create key value))
module ExportValue =
    let encode = ExportValueImpl.encode
    let decode: Decoder<_> = ExportValueImpl.decode

module ExportPath =
    let encode (value: ExportPath) = value.Value |> Encode.string
    let decode: Decoder<ExportPath> = Decode.string |> Decode.map ExportPath.Create

module Export =
    let encode (value: Export) = Encode.map ExportPath.encode ExportValue.encode value
    let decode: Decoder<Export> = Decode.map' ExportPath.decode ExportValue.decode

module PackageId =
    let encode (PackageId(name, version)) =
        Encode.object [
            "Name", Encode.string name
            "Version", Encode.string version
        ]
    let decode: Decoder<PackageId> = Decode.object <| fun get ->
        let name = get.Required.Field "Name" Decode.string
        let version = get.Required.Field "Version" Decode.string
        PackageId(name, version)

module SubModuleId =
    let encode (SubModuleId(packageId, subModuleName)) =
        Encode.object [
            "PackageId", PackageId.encode packageId
            "SubModuleName", Encode.string subModuleName
        ]
    let decode: Decoder<SubModuleId> = Decode.object <| fun get ->
        let packageId = get.Required.Field "PackageId" PackageId.decode
        let subModuleName = get.Required.Field "SubModuleName" Decode.string
        SubModuleId(packageId, subModuleName)

module Package =
    let encode (package: Package) =
        Encode.object [
            nameof mock<Package>.Name, Encode.string package.Name
            nameof mock<Package>.Version, Encode.string package.Version
            nameof mock<Package>.SubModules, package.SubModules |> Seq.map SubModuleId.encode |> Encode.seq
            nameof mock<Package>.Entry, package.Entry |> Seq.map SubModuleId.encode |> Encode.seq
            if package.Json.IsSome then
                nameof mock<Package>.Json, package.Json.Value |> Export.encode
        ]
    let decode: Decoder<Package> = Decode.object <| fun get ->
        {
            Name = get.Required.Field (nameof mock<Package>.Name) Decode.string
            Json = get.Optional.Field (nameof mock<Package>.Json) Export.decode |> Option.toValueOption
            Version = get.Required.Field (nameof mock<Package>.Version) Decode.string
            SubModules = get.Required.Field (nameof mock<Package>.SubModules) (Decode.listOrArray SubModuleId.decode)
            Entry = get.Required.Field (nameof mock<Package>.Entry) (Decode.listOrArray SubModuleId.decode)
        }

module SubModule =
    let encode (value: SubModule) =
        Encode.object [
            nameof mock<SubModule>.Package, PackageId.encode value.Package
            nameof mock<SubModule>.Name, Encode.string value.Name
            nameof mock<SubModule>.Path, Encode.string value.Path
        ]
    let decode: Decoder<SubModule> = Decode.object <| fun get ->
        {
            Package = get.Required.Field (nameof mock<SubModule>.Package) PackageId.decode
            Name = get.Required.Field (nameof mock<SubModule>.Name) Decode.string
            Path = get.Required.Field (nameof mock<SubModule>.Path) Decode.string
        }

module SubModuleRelation =
    let encode (value: SubModuleRelation) =
        Encode.object [
            nameof mock<SubModuleRelation>.Dependent, SubModuleId.encode value.Dependent
            nameof mock<SubModuleRelation>.Dependency, SubModuleId.encode value.Dependency
        ]
    let decode: Decoder<SubModuleRelation> = Decode.object <| fun get ->
        {
            Dependent = get.Required.Field (nameof mock<SubModuleRelation>.Dependent) SubModuleId.decode
            Dependency = get.Required.Field (nameof mock<SubModuleRelation>.Dependency) SubModuleId.decode
        }

module ExportPoint =
    let encode (value: ExportPoint) =
        Encode.object [
            nameof mock<ExportPoint>.SubModule, SubModuleId.encode value.SubModule
            nameof mock<ExportPoint>.Name, Encode.string value.Name
        ]
    let decode: Decoder<ExportPoint> = Decode.object <| fun get ->
        {
            Name = get.Required.Field (nameof mock<ExportPoint>.Name) Decode.string
            SubModule = get.Required.Field (nameof mock<ExportPoint>.SubModule) SubModuleId.decode
        }

module ExportCollection =
    let encode (value: ExportCollection) =
        Encode.object [
            nameof mock<ExportCollection>.Canonical, ExportPoint.encode value.Canonical 
            nameof mock<ExportCollection>.Aliases, value.Aliases |> Seq.map ExportPoint.encode |> Encode.seq
        ]
    let decode: Decoder<ExportCollection> = Decode.object <| fun get ->
        {
            Canonical = get.Required.Field (nameof mock<ExportCollection>.Canonical) ExportPoint.decode
            Aliases = get.Required.Field (nameof mock<ExportCollection>.Aliases) (Decode.listOrArray ExportPoint.decode)
        }

module Source =
    let encode (value: Source) =
        match value with
        | Source.LibEs fileName -> Encode.string fileName
        | Source.PackageInternal subModuleId -> SubModuleId.encode subModuleId
        | Source.Package exportCollection -> ExportCollection.encode exportCollection
    let decode: Decoder<Source> = Decode.oneOf [
        Decode.string |> Decode.map Source.LibEs
        SubModuleId.decode |> Decode.map Source.PackageInternal
        ExportCollection.decode |> Decode.map Source.Package
    ]

module Metadata =
    let encode (value: Metadata) =
        Encode.object [
            nameof mock<Metadata>.Source, Source.encode value.Source
        ]
    let decode: Decoder<Metadata> = Decode.object <| fun get ->
        {
            Source = get.Required.Field (nameof mock<Metadata>.Source) Source.decode
        }

module TsOverloadableConstruct =
    let encode (encoder: Encoder<'T>) (value: TsOverloadableConstruct<'T>) =
        match value with
        | NoOverloads foo -> Encode.list [ encoder foo ]
        | Overloaded foo ->
            foo
            |> Set.toList
            |> List.map encoder
            |> Encode.list
    let decode (decoder: Decoder<'T>): Decoder<TsOverloadableConstruct<'T>> =
        Decode.list decoder
        |> Decode.andThen(fun values ->
            match values with
            | [ values ] -> Decode.succeed (NoOverloads values)
            | values -> Decode.succeed (Overloaded (Set.ofList values))
            )
module TsComment =
    let encode (value: TsComment) =
        Encode.object [
            match value with
            | TsComment.Summary l ->
                nameof TsComment.Summary,
                l |> List.map Encode.string |> Encode.list
            | TsComment.Returns s ->
                nameof TsComment.Returns,
                Encode.string s
            | TsComment.Param(name, content) ->
                nameof TsComment.Param, Encode.string name
                "Content", content |> Encode.option Encode.string
            | TsComment.Deprecated stringOption ->
                nameof TsComment.Deprecated, stringOption |> Encode.option Encode.string
            | TsComment.Remarks s ->
                nameof TsComment.Remarks, Encode.string s
            | TsComment.DefaultValue s ->
                nameof TsComment.DefaultValue, Encode.string s
            | TsComment.Example s ->
                nameof TsComment.Example, Encode.string s
            | TsComment.TypeParam(typeName, content) ->
                nameof TsComment.TypeParam, Encode.string typeName
                "Content", content |> Encode.option Encode.string
            | TsComment.Throws s ->
                nameof TsComment.Throws, Encode.string s
        ]
    let decode: Decoder<TsComment> = Decode.oneOf [
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Summary) (Decode.list Decode.string)
            |> TsComment.Summary
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Returns) Decode.string
            |> TsComment.Returns
        Decode.object <| fun get ->
            let param = get.Required.Field (nameof TsComment.Param) Decode.string
            let content = get.Required.Field "Content" (Decode.option Decode.string)
            TsComment.Param(param, content)
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Deprecated) (Decode.option Decode.string)
            |> TsComment.Deprecated 
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Remarks) Decode.string
            |> TsComment.Remarks
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.DefaultValue) Decode.string
            |> TsComment.DefaultValue
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Example) Decode.string
            |> TsComment.Example
        Decode.object <| fun get ->
            let typeName = get.Required.Field (nameof TsComment.TypeParam) Decode.string
            let content = get.Required.Field "Content" (Decode.option Decode.string)
            TsComment.TypeParam(typeName, content)
        Decode.object <| fun get ->
            get.Required.Field (nameof TsComment.Throws) Decode.string
            |> TsComment.Throws
    ]
module TsLiteral =
    let encode (value: TsLiteral) =
        match value with
        | TsLiteral.String s -> Encode.object [ "String", Encode.string s ]
        | TsLiteral.Int i -> Encode.object [ "Int", Encode.int i ]
        | TsLiteral.Float f -> Encode.object [ "Float", Encode.float f ]
        | TsLiteral.Bool b -> Encode.object [ "Bool", Encode.bool b ]
        | TsLiteral.BigInt bi -> Encode.object [ "BigInt", Encode.bigint bi ]
        | TsLiteral.Null -> Encode.object [ "Null", Encode.bool true ]
    let decode: Decoder<TsLiteral> = Decode.oneOf [
        Decode.object <| fun get -> get.Required.Field "String" Decode.string |> TsLiteral.String
        Decode.object <| fun get -> get.Required.Field "Int" Decode.int |> TsLiteral.Int
        Decode.object <| fun get -> get.Required.Field "Float" Decode.float |> TsLiteral.Float
        Decode.object <| fun get -> get.Required.Field "Bool" Decode.bool |> TsLiteral.Bool
        Decode.object <| fun get -> get.Required.Field "BigInt" Decode.bigint |> TsLiteral.BigInt
        Decode.object <| fun get -> get.Required.Field "Null" Decode.bool |> fun _ -> TsLiteral.Null
    ]

module TsEnumCase =
    let encode (value: TsEnumCase) =
        Encode.object [
            "Parent" ==> TypeKey.encode value.Parent
            "FullyQualifiedName" ==> (value.FullyQualifiedName |> List.map Encode.string |> Encode.list)
            "Name" ==> Encode.string value.Name
            "Value" ==> TsLiteral.encode value.Value
            "Documentation" ==> (value.Documentation |> List.map TsComment.encode |> Encode.list)
        ]
    let decode: Decoder<TsEnumCase> =
        Decode.object <| fun get -> {
            Parent = get.Required.Field "Parent" TypeKey.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Name = get.Required.Field "Name" Decode.string
            Value = get.Required.Field "Value" TsLiteral.decode
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsEnumType =
    let encode (value: TsEnumType) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName" ==> (value.FullyQualifiedName |> List.map Encode.string |> Encode.list)
            "Name", Encode.string value.Name
            "Members", value.Members |> List.map TsEnumCase.encode |> Encode.list
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsEnumType> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsEnumType>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Name = get.Required.Field "Name" Decode.string
            Members = get.Required.Field "Members" (Decode.list TsEnumCase.decode)
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsVariable =
    let encode (value: TsVariable) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Name", Encode.string value.Name
            "Type", TypeKey.encode value.Type
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsVariable> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsVariable>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Name = get.Required.Field "Name" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsParameter =
    let encode (value: TsParameter) =
        Encode.object [
            "Name", Encode.string value.Name
            "IsOptional", Encode.bool value.IsOptional
            "IsSpread", Encode.bool value.IsSpread
            "Type", TypeKey.encode value.Type
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsParameter> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            IsOptional = get.Required.Field "IsOptional" Decode.bool
            IsSpread = get.Required.Field "IsSpread" Decode.bool
            Type = get.Required.Field "Type" TypeKey.decode
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsTypeParameter =
    let encode (value: TsTypeParameter) =
        Encode.object [
            "Name", Encode.string value.Name
            "Constraint", value.Constraint |> Encode.option TypeKey.encode
            "Default", value.Default |> Encode.option TypeKey.encode
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsTypeParameter> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            Constraint = get.Required.Field "Constraint" (Decode.option TypeKey.decode)
            Default = get.Required.Field "Default" (Decode.option TypeKey.decode)
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module InlinedTsTypeParameter =
    let encode ((key, param): InlinedTsTypeParameter) =
        Encode.object [
            "Key", TypeKey.encode key
            "Param", TsTypeParameter.encode param
        ]
    let decode: Decoder<InlinedTsTypeParameter> =
        Decode.object <| fun get ->
            let key = get.Required.Field "Key" TypeKey.decode
            let param = get.Required.Field "Param" TsTypeParameter.decode
            (key, param)

module TsMethod =
    let encode (value: TsMethod) =
        Encode.object [
            "Name", Encode.string value.Name
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
            "Type", TypeKey.encode value.Type
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
            "IsOptional", Encode.bool value.IsOptional
            "IsStatic", Encode.bool value.IsStatic
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsMethod> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
            Type = get.Required.Field "Type" TypeKey.decode
            TypeParameters =
                get.Optional.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
                |> Option.defaultValue []
            IsOptional = get.Required.Field "IsOptional" Decode.bool
            IsStatic = get.Required.Field "IsStatic" Decode.bool
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsCallSignature =
    let encode (value: TsCallSignature) =
        Encode.object [
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
            "Type", TypeKey.encode value.Type
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
        ]
    let decode: Decoder<TsCallSignature> =
        Decode.object <| fun get -> {
            TsCallSignature.Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
            Type = get.Required.Field "Type" TypeKey.decode
            TypeParameters =
                get.Optional.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
                |> Option.defaultValue []
        }

module TsConstructSignature =
    let encode (value: TsConstructSignature) =
        Encode.object [
            "Type", TypeKey.encode value.Type
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
        ]
    let decode: Decoder<TsConstructSignature> =
        Decode.object <| fun get -> {
            Type = get.Required.Field "Type" TypeKey.decode
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
            TypeParameters =
                get.Optional.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
                |> Option.defaultValue []
        }

module TsConstructor =
    let encode (value: TsConstructor) =
        Encode.object [
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
        ]
    let decode: Decoder<TsConstructor> =
        Decode.object <| fun get -> {
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
        }

module TsAccessor =
    let encode (value: TsAccessor) =
        match value with
        | TsAccessor.ReadOnly -> Encode.string "ReadOnly"
        | TsAccessor.WriteOnly -> Encode.string "WriteOnly"
        | TsAccessor.ReadWrite -> Encode.string "ReadWrite"
    let decode: Decoder<TsAccessor> =
        Decode.string |> Decode.andThen (function
            | "ReadOnly" -> Decode.succeed TsAccessor.ReadOnly
            | "WriteOnly" -> Decode.succeed TsAccessor.WriteOnly
            | "ReadWrite" -> Decode.succeed TsAccessor.ReadWrite
            | other -> Decode.fail $"Unknown TsAccessor: {other}")

module TsProperty =
    let encode (value: TsProperty) =
        Encode.object [
            "Name", Encode.string value.Name
            "Type", TypeKey.encode value.Type
            "IsStatic", Encode.bool value.IsStatic
            "IsOptional", Encode.bool value.IsOptional
            "IsPrivate", Encode.bool value.IsPrivate
            "Accessor", TsAccessor.encode value.Accessor
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsProperty> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            IsStatic = get.Required.Field "IsStatic" Decode.bool
            IsOptional = get.Required.Field "IsOptional" Decode.bool
            IsPrivate = get.Required.Field "IsPrivate" Decode.bool
            Accessor = get.Required.Field "Accessor" TsAccessor.decode
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsGetAccessor =
    let encode (value: TsGetAccessor) =
        Encode.object [
            "Name", Encode.string value.Name
            "Type", TypeKey.encode value.Type
            "IsStatic", Encode.bool value.IsStatic
            "IsPrivate", Encode.bool value.IsPrivate
        ]
    let decode: Decoder<TsGetAccessor> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            IsStatic = get.Required.Field "IsStatic" Decode.bool
            IsPrivate = get.Required.Field "IsPrivate" Decode.bool
        }

module TsSetAccessor =
    let encode (value: TsSetAccessor) =
        Encode.object [
            "Name", Encode.string value.Name
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
            "ArgumentType", TypeKey.encode value.ArgumentType
            "IsStatic", Encode.bool value.IsStatic
            "IsPrivate", Encode.bool value.IsPrivate
        ]
    let decode: Decoder<TsSetAccessor> =
        Decode.object <| fun get -> {
            Name = get.Required.Field "Name" Decode.string
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
            ArgumentType = get.Required.Field "ArgumentType" TypeKey.decode
            IsStatic = get.Required.Field "IsStatic" Decode.bool
            IsPrivate = get.Required.Field "IsPrivate" Decode.bool
        }

module TsIndexSignature =
    let encode (value: TsIndexSignature) =
        Encode.object [
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
            "Type", TypeKey.encode value.Type
            "IsReadOnly", Encode.bool value.IsReadOnly
        ]
    let decode: Decoder<TsIndexSignature> =
        Decode.object <| fun get -> {
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
            Type = get.Required.Field "Type" TypeKey.decode
            IsReadOnly = get.Required.Field "IsReadOnly" Decode.bool
        }

module TsFunction =
    let encode (value: TsFunction) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
            "IsDeclared", Encode.bool value.IsDeclared
            "Name", Encode.string value.Name
            "Type", TypeKey.encode value.Type
            "Parameters", value.Parameters |> List.map TsParameter.encode |> Encode.list
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
            "SignatureKey", TypeKey.encode value.SignatureKey
        ]
    let decode: Decoder<TsFunction> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsFunction>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
            IsDeclared = get.Required.Field "IsDeclared" Decode.bool
            Name = get.Required.Field "Name" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            Parameters = get.Required.Field "Parameters" (Decode.list TsParameter.decode)
            TypeParameters = get.Required.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
            SignatureKey = get.Required.Field "SignatureKey" TypeKey.decode
        }

module TsConditionalType =
    let encode (value: TsConditionalType) =
        Encode.object [
            "Check", TypeKey.encode value.Check
            "Extends", TypeKey.encode value.Extends
            "True", TypeKey.encode value.True
            "False", TypeKey.encode value.False
        ]
    let decode: Decoder<TsConditionalType> =
        Decode.object <| fun get -> {
            Check = get.Required.Field "Check" TypeKey.decode
            Extends = get.Required.Field "Extends" TypeKey.decode
            True = get.Required.Field "True" TypeKey.decode
            False = get.Required.Field "False" TypeKey.decode
        }

module TsMember =
    let encode (value: TsMember) =
        Encode.object [
            match value with
            | TsMember.Method m ->
                "Method", TsOverloadableConstruct.encode TsMethod.encode m
            | TsMember.Property p ->
                "Property", TsProperty.encode p
            | TsMember.GetAccessor g ->
                "GetAccessor", TsGetAccessor.encode g
            | TsMember.SetAccessor s ->
                "SetAccessor", TsSetAccessor.encode s
            | TsMember.CallSignature cs ->
                "CallSignature", TsOverloadableConstruct.encode TsCallSignature.encode cs
            | TsMember.IndexSignature is ->
                "IndexSignature", TsIndexSignature.encode is
            | TsMember.ConstructSignature cs ->
                "ConstructSignature", TsOverloadableConstruct.encode TsConstructSignature.encode cs
        ]
    let decode: Decoder<TsMember> = Decode.oneOf [
        Decode.object <| fun get ->
            get.Required.Field "Method" (TsOverloadableConstruct.decode TsMethod.decode)
            |> TsMember.Method
        Decode.object <| fun get ->
            get.Required.Field "Property" TsProperty.decode
            |> TsMember.Property
        Decode.object <| fun get ->
            get.Required.Field "GetAccessor" TsGetAccessor.decode
            |> TsMember.GetAccessor
        Decode.object <| fun get ->
            get.Required.Field "SetAccessor" TsSetAccessor.decode
            |> TsMember.SetAccessor
        Decode.object <| fun get ->
            get.Required.Field "CallSignature" (TsOverloadableConstruct.decode TsCallSignature.decode)
            |> TsMember.CallSignature
        Decode.object <| fun get ->
            get.Required.Field "IndexSignature" TsIndexSignature.decode
            |> TsMember.IndexSignature
        Decode.object <| fun get ->
            get.Required.Field "ConstructSignature" (TsOverloadableConstruct.decode TsConstructSignature.decode)
            |> TsMember.ConstructSignature
    ]

module TsTypeReference =
    let encode (value: TsTypeReference) =
        Encode.object [
            "Type", TypeKey.encode value.Type
            "TypeArguments", value.TypeArguments |> List.map TypeKey.encode |> Encode.list
            "ResolvedType", value.ResolvedType |> Encode.option TypeKey.encode
        ]
    let decode: Decoder<TsTypeReference> =
        Decode.object <| fun get -> {
            Type = get.Required.Field "Type" TypeKey.decode
            TypeArguments = get.Required.Field "TypeArguments" (Decode.list TypeKey.decode)
            ResolvedType = get.Required.Field "ResolvedType" (Decode.option TypeKey.decode)
        }

module TsInterfaceHeritage =
    let encode (value: TsInterfaceHeritage) =
        Encode.object [
            "Extends", value.Extends |> List.map TsTypeReference.encode |> Encode.list
        ]
    let decode: Decoder<TsInterfaceHeritage> =
        Decode.object <| fun get -> {
            Extends = get.Required.Field "Extends" (Decode.list TsTypeReference.decode)
        }

module TsClassHeritage =
    let encode (value: TsClassHeritage) =
        Encode.object [
            "Implements", value.Implements |> Encode.option TsTypeReference.encode
            "Extends", value.Extends |> List.map TsTypeReference.encode |> Encode.list
        ]
    let decode: Decoder<TsClassHeritage> =
        Decode.object <| fun get -> {
            Implements = get.Required.Field "Implements" (Decode.option TsTypeReference.decode)
            Extends = get.Required.Field "Extends" (Decode.list TsTypeReference.decode)
        }

module TsInterface =
    let encode (value: TsInterface) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Enumerable", Encode.bool value.Enumerable
            "Name", Encode.string value.Name
            "Members", value.Members |> List.map TsMember.encode |> Encode.list
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
            "Heritage", TsInterfaceHeritage.encode value.Heritage
        ]
    let decode: Decoder<TsInterface> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsInterface>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Enumerable = get.Required.Field "Enumerable" Decode.bool
            Name = get.Required.Field "Name" Decode.string
            Members = get.Required.Field "Members" (Decode.list TsMember.decode)
            TypeParameters = get.Required.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
            Heritage = get.Required.Field "Heritage" TsInterfaceHeritage.decode
        }

module TsTypeLiteral =
    let encode (value: TsTypeLiteral) =
        Encode.object [
            "Members", value.Members |> List.map TsMember.encode |> Encode.list
        ]
    let decode: Decoder<TsTypeLiteral> =
        Decode.object <| fun get -> {
            Members = get.Required.Field "Members" (Decode.list TsMember.decode)
        }

module TsIndexAccessType =
    let encode (value: TsIndexAccessType) =
        Encode.object [
            "Object", TypeKey.encode value.Object
            "Index", TypeKey.encode value.Index
        ]
    let decode: Decoder<TsIndexAccessType> =
        Decode.object <| fun get -> {
            Object = get.Required.Field "Object" TypeKey.decode
            Index = get.Required.Field "Index" TypeKey.decode
        }

module TsTypeAlias =
    let encode (value: TsTypeAlias) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Name", Encode.string value.Name
            "Type", TypeKey.encode value.Type
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
            "Documentation", value.Documentation |> List.map TsComment.encode |> Encode.list
        ]
    let decode: Decoder<TsTypeAlias> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsTypeAlias>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Name = get.Required.Field "Name" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            TypeParameters = get.Required.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
            Documentation = get.Required.Field "Documentation" (Decode.list TsComment.decode)
        }

module TsSubstitutionType =
    let encode (value: TsSubstitutionType) =
        Encode.object [
            "Base", TypeKey.encode value.Base
            "Constraint", TypeKey.encode value.Constraint
        ]
    let decode: Decoder<TsSubstitutionType> =
        Decode.object <| fun get -> {
            Base = get.Required.Field "Base" TypeKey.decode
            Constraint = get.Required.Field "Constraint" TypeKey.decode
        }

module TsClass =
    let encode (value: TsClass) =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Enumerable", Encode.bool value.Enumerable
            "Name", Encode.string value.Name
            "Constructors", value.Constructors |> List.map TsConstructor.encode |> Encode.list
            "Members", value.Members |> List.map TsMember.encode |> Encode.list
            "TypeParameters", value.TypeParameters |> List.map InlinedTsTypeParameter.encode |> Encode.list
            "Heritage", TsClassHeritage.encode value.Heritage
        ]
    let decode: Decoder<TsClass> =
        Decode.object <| fun get -> {
            Metadata = get.Required.Field (nameof mock<TsClass>.Metadata) Metadata.decode
            FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
            Enumerable = get.Required.Field "Enumerable" Decode.bool
            Name = get.Required.Field "Name" Decode.string
            Constructors = get.Required.Field "Constructors" (Decode.list TsConstructor.decode)
            Members = get.Required.Field "Members" (Decode.list TsMember.decode)
            TypeParameters = get.Required.Field "TypeParameters" (Decode.list InlinedTsTypeParameter.decode)
            Heritage = get.Required.Field "Heritage" TsClassHeritage.decode
        }

module TsTypeUnion =
    let encode (TsTypeUnion types) =
        types |> List.map TypeKey.encode |> Encode.list
    let decode: Decoder<TsTypeUnion> =
        Decode.list TypeKey.decode |> Decode.map TsTypeUnion

module TsTypeIntersection =
    let encode (TsTypeIntersection types) =
        types |> List.map TypeKey.encode |> Encode.list
    let decode: Decoder<TsTypeIntersection> =
        Decode.list TypeKey.decode |> Decode.map TsTypeIntersection

module TsTupleElementType =
    let encode (value: TsTupleElementType) =
        Encode.object [
            "Type", TypeKey.encode value.Type
            "IsOptional", Encode.bool value.IsOptional
            "IsRest", Encode.bool value.IsRest
        ]
    let decode: Decoder<TsTupleElementType> =
        Decode.object <| fun get -> {
            Type = get.Required.Field "Type" TypeKey.decode
            IsOptional = get.Required.Field "IsOptional" Decode.bool
            IsRest = get.Required.Field "IsRest" Decode.bool
        }

module TsTupleElement =
    let encode (value: TsTupleElement) =
        match value with
        | TsTupleElement.FixedLabeled(label, elem) ->
            Encode.object [
                "FixedLabeled", Encode.string label
                "Element", TsTupleElementType.encode elem
            ]
        | TsTupleElement.Variadic key ->
            Encode.object [ "Variadic", TypeKey.encode key ]
        | TsTupleElement.Fixed elem ->
            Encode.object [ "Fixed", TsTupleElementType.encode elem ]
    let decode: Decoder<TsTupleElement> = Decode.oneOf [
        Decode.object <| fun get ->
            let label = get.Required.Field "FixedLabeled" Decode.string
            let elem = get.Required.Field "Element" TsTupleElementType.decode
            TsTupleElement.FixedLabeled(label, elem)
        Decode.object <| fun get ->
            get.Required.Field "Variadic" TypeKey.decode |> TsTupleElement.Variadic
        Decode.object <| fun get ->
            get.Required.Field "Fixed" TsTupleElementType.decode |> TsTupleElement.Fixed
    ]

module TsTuple =
    let encode (value: TsTuple) =
        Encode.object [
            "IsReadOnly", Encode.bool value.IsReadOnly
            "FixedLength", Encode.int value.FixedLength
            "MinRequired", Encode.int value.MinRequired
            "Types", value.Types |> List.map TsTupleElement.encode |> Encode.list
        ]
    let decode: Decoder<TsTuple> =
        Decode.object <| fun get -> {
            IsReadOnly = get.Required.Field "IsReadOnly" Decode.bool
            FixedLength = get.Required.Field "FixedLength" Decode.int
            MinRequired = get.Required.Field "MinRequired" Decode.int
            Types = get.Required.Field "Types" (Decode.list TsTupleElement.decode)
        }

module TsIndex =
    let encode (value: TsIndex) =
        Encode.object [ "Type", TypeKey.encode value.Type ]
    let decode: Decoder<TsIndex> =
        Decode.object <| fun get -> { Type = get.Required.Field "Type" TypeKey.decode }

module TsTemplateLiteralType =
    let encode (value: TsTemplateLiteralType) =
        Encode.object [
            "Texts", value.Texts |> List.map Encode.string |> Encode.list
            "Types", value.Types |> List.map TypeKey.encode |> Encode.list
        ]
    let decode: Decoder<TsTemplateLiteralType> =
        Decode.object <| fun get -> {
            Texts = get.Required.Field "Texts" (Decode.list Decode.string)
            Types = get.Required.Field "Types" (Decode.list TypeKey.decode)
        }

module TsTypePredicate =
    let encode (value: TsTypePredicate) =
        Encode.object [
            "ParameterName", Encode.string value.ParameterName
            "Type", TypeKey.encode value.Type
            "IsAssertion", Encode.bool value.IsAssertion
        ]
    let decode: Decoder<TsTypePredicate> =
        Decode.object <| fun get -> {
            ParameterName = get.Required.Field "ParameterName" Decode.string
            Type = get.Required.Field "Type" TypeKey.decode
            IsAssertion = get.Required.Field "IsAssertion" Decode.bool
        }

module TsTypeQuery =
    let encode (value: TsTypeQuery) =
        Encode.object [
            nameof value.FullyQualifiedName, value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            nameof value.Type, TypeKey.encode value.Type
        ]
    let decode: Decoder<TsTypeQuery> =
        Decode.object <| fun get -> {
            FullyQualifiedName = get.Required.Field (nameof mock<TsTypeQuery>.FullyQualifiedName) (Decode.list Decode.string)
            Type = get.Required.Field (nameof mock<TsTypeQuery>.Type) TypeKey.decode
        }

module TypeKindPrimitive =
    let encode (value: TypeKindPrimitive) =
        match value with
        | TypeKindPrimitive.Any -> Encode.string "Any"
        | TypeKindPrimitive.Unknown -> Encode.string "Unknown"
        | TypeKindPrimitive.Never -> Encode.string "Never"
        | TypeKindPrimitive.Void -> Encode.string "Void"
        | TypeKindPrimitive.Undefined -> Encode.string "Undefined"
        | TypeKindPrimitive.Null -> Encode.string "Null"
        | TypeKindPrimitive.String -> Encode.string "String"
        | TypeKindPrimitive.Integer -> Encode.string "Integer"
        | TypeKindPrimitive.Number -> Encode.string "Number"
        | TypeKindPrimitive.Boolean -> Encode.string "Boolean"
        | TypeKindPrimitive.BigInt -> Encode.string "BigInt"
        | TypeKindPrimitive.ESSymbol -> Encode.string "ESSymbol"
        | TypeKindPrimitive.NonPrimitive -> Encode.string "NonPrimitive"
    let decode: Decoder<TypeKindPrimitive> =
        Decode.string |> Decode.andThen (function
            | "Any" -> Decode.succeed TypeKindPrimitive.Any
            | "Unknown" -> Decode.succeed TypeKindPrimitive.Unknown
            | "Never" -> Decode.succeed TypeKindPrimitive.Never
            | "Void" -> Decode.succeed TypeKindPrimitive.Void
            | "Undefined" -> Decode.succeed TypeKindPrimitive.Undefined
            | "Null" -> Decode.succeed TypeKindPrimitive.Null
            | "String" -> Decode.succeed TypeKindPrimitive.String
            | "Integer" -> Decode.succeed TypeKindPrimitive.Integer
            | "Number" -> Decode.succeed TypeKindPrimitive.Number
            | "Boolean" -> Decode.succeed TypeKindPrimitive.Boolean
            | "BigInt" -> Decode.succeed TypeKindPrimitive.BigInt
            | "ESSymbol" -> Decode.succeed TypeKindPrimitive.ESSymbol
            | "NonPrimitive" -> Decode.succeed TypeKindPrimitive.NonPrimitive
            | other -> Decode.fail $"Unknown TypeKindPrimitive: {other}")

module TsType =
    let rec encode (value: TsType) : JsonValue =
        Encode.object [
            match value with
            | TsType.GlobalThis -> "GlobalThis", Encode.bool true
            | TsType.Conditional c -> "Conditional", TsConditionalType.encode c
            | TsType.Interface i -> "Interface", TsInterface.encode i
            | TsType.Class c -> "Class", TsClass.encode c
            | TsType.Primitive p -> "Primitive", TypeKindPrimitive.encode p
            | TsType.Enum e -> "Enum", TsEnumType.encode e
            | TsType.EnumCase ec -> "EnumCase", TsEnumCase.encode ec
            | TsType.Union u -> "Union", TsTypeUnion.encode u
            | TsType.Intersection i -> "Intersection", TsTypeIntersection.encode i
            | TsType.Literal l -> "Literal", TsLiteral.encode l
            | TsType.IndexedAccess ia -> "IndexedAccess", TsIndexAccessType.encode ia
            | TsType.TypeReference tr -> "TypeReference", TsTypeReference.encode tr
            | TsType.Array inner -> "Array", encode inner
            | TsType.TypeParameter tp -> "TypeParameter", TsTypeParameter.encode tp
            | TsType.ReadOnly inner -> "ReadOnly", encode inner
            | TsType.Tuple t -> "Tuple", TsTuple.encode t
            | TsType.Index i -> "Index", TsIndex.encode i
            | TsType.Predicate p -> "Predicate", TsTypePredicate.encode p
            | TsType.TypeLiteral tl -> "TypeLiteral", TsTypeLiteral.encode tl
            | TsType.TemplateLiteral tl -> "TemplateLiteral", TsTemplateLiteralType.encode tl
            | TsType.Optional tr -> "Optional", TsTypeReference.encode tr
            | TsType.Substitution s -> "Substitution", TsSubstitutionType.encode s
            | TsType.TypeQuery tsTypeQuery -> nameof TsType.TypeQuery, TsTypeQuery.encode tsTypeQuery
        ]
    let rec decode: Decoder<TsType> =
        fun path value ->
            Decode.oneOf [
                Decode.object (fun get -> get.Required.Field "GlobalThis" Decode.bool |> fun _ -> TsType.GlobalThis)
                Decode.object (fun get -> get.Required.Field "Conditional" TsConditionalType.decode |> TsType.Conditional)
                Decode.object (fun get -> get.Required.Field "Interface" TsInterface.decode |> TsType.Interface)
                Decode.object (fun get -> get.Required.Field "Class" TsClass.decode |> TsType.Class)
                Decode.object (fun get -> get.Required.Field "Primitive" TypeKindPrimitive.decode |> TsType.Primitive)
                Decode.object (fun get -> get.Required.Field "Enum" TsEnumType.decode |> TsType.Enum)
                Decode.object (fun get -> get.Required.Field "EnumCase" TsEnumCase.decode |> TsType.EnumCase)
                Decode.object (fun get -> get.Required.Field "Union" TsTypeUnion.decode |> TsType.Union)
                Decode.object (fun get -> get.Required.Field "Intersection" TsTypeIntersection.decode |> TsType.Intersection)
                Decode.object (fun get -> get.Required.Field "Literal" TsLiteral.decode |> TsType.Literal)
                Decode.object (fun get -> get.Required.Field "IndexedAccess" TsIndexAccessType.decode |> TsType.IndexedAccess)
                Decode.object (fun get -> get.Required.Field "TypeReference" TsTypeReference.decode |> TsType.TypeReference)
                Decode.object (fun get -> get.Required.Field "Array" decode |> TsType.Array)
                Decode.object (fun get -> get.Required.Field "TypeParameter" TsTypeParameter.decode |> TsType.TypeParameter)
                Decode.object (fun get -> get.Required.Field "ReadOnly" decode |> TsType.ReadOnly)
                Decode.object (fun get -> get.Required.Field "Tuple" TsTuple.decode |> TsType.Tuple)
                Decode.object (fun get -> get.Required.Field "Index" TsIndex.decode |> TsType.Index)
                Decode.object (fun get -> get.Required.Field "Predicate" TsTypePredicate.decode |> TsType.Predicate)
                Decode.object (fun get -> get.Required.Field "TypeLiteral" TsTypeLiteral.decode |> TsType.TypeLiteral)
                Decode.object (fun get -> get.Required.Field "TemplateLiteral" TsTemplateLiteralType.decode |> TsType.TemplateLiteral)
                Decode.object (fun get -> get.Required.Field "Optional" TsTypeReference.decode |> TsType.Optional)
                Decode.object (fun get -> get.Required.Field "Substitution" TsSubstitutionType.decode |> TsType.Substitution)
                Decode.object (fun get -> get.Required.Field (nameof TsType.TypeQuery) TsTypeQuery.decode |> TsType.TypeQuery)
            ] path value

module TsModule =
    let rec encode (value: TsModule) : JsonValue =
        Encode.object [
            nameof value.Metadata ==> Metadata.encode value.Metadata
            "FullyQualifiedName", value.FullyQualifiedName |> List.map Encode.string |> Encode.list
            "Name", Encode.string value.Name
            "IsNamespace", Encode.bool value.IsNamespace
            "IsRecursive", Encode.bool value.IsRecursive
            "Exports", value.Exports |> List.map encodeExport |> Encode.list
        ]
    and encodeExport (value: TsExportDeclaration) : JsonValue =
        Encode.object [
            match value with
            | TsExportDeclaration.Variable v -> "Variable", TsVariable.encode v
            | TsExportDeclaration.Interface i -> "Interface", TsInterface.encode i
            | TsExportDeclaration.TypeAlias ta -> "TypeAlias", TsTypeAlias.encode ta
            | TsExportDeclaration.Class c -> "Class", TsClass.encode c
            | TsExportDeclaration.Enum e -> "Enum", TsEnumType.encode e
            | TsExportDeclaration.Module m -> "Module", encode m
            | TsExportDeclaration.Function f -> "Function", TsOverloadableConstruct.encode TsFunction.encode f
        ]
    and decode: Decoder<TsModule> =
        fun path value ->
            (Decode.object <| fun get -> {
                Metadata = get.Required.Field (nameof mock<TsModule>.Metadata) Metadata.decode
                FullyQualifiedName = get.Required.Field "FullyQualifiedName" (Decode.list Decode.string)
                Name = get.Required.Field "Name" Decode.string
                IsNamespace = get.Required.Field "IsNamespace" Decode.bool
                IsRecursive = get.Required.Field "IsRecursive" Decode.bool
                Exports = get.Required.Field "Exports" (Decode.list decodeExport)
            }) path value
    and decodeExport: Decoder<TsExportDeclaration> =
        fun path value ->
            Decode.oneOf [
                Decode.object (fun get -> get.Required.Field "Variable" TsVariable.decode |> TsExportDeclaration.Variable)
                Decode.object (fun get -> get.Required.Field "Interface" TsInterface.decode |> TsExportDeclaration.Interface)
                Decode.object (fun get -> get.Required.Field "TypeAlias" TsTypeAlias.decode |> TsExportDeclaration.TypeAlias)
                Decode.object (fun get -> get.Required.Field "Class" TsClass.decode |> TsExportDeclaration.Class)
                Decode.object (fun get -> get.Required.Field "Enum" TsEnumType.decode |> TsExportDeclaration.Enum)
                Decode.object (fun get -> get.Required.Field "Module" decode |> TsExportDeclaration.Module)
                Decode.object (fun get -> get.Required.Field "Function" (TsOverloadableConstruct.decode TsFunction.decode) |> TsExportDeclaration.Function)
            ] path value

module TsExportDeclaration =
    let encode = TsModule.encodeExport
    let decode = TsModule.decodeExport

[<AutoOpen>]
module SchemaThoth =
    open Schema
    
    module Schema =
        module TsIdentityKey =
            let encode (value: TsIdentityKey) =
                match value with
                | DeclarationFile(file, startPos, endPos) ->
                    Encode.object [ "DeclarationFile", Encode.string file; "StartPos", Encode.int startPos; "EndPos", Encode.int endPos ]
                | Symbol name -> Encode.object [ "Symbol", Encode.string name ]
                | Type i -> Encode.object [ "Type", TypeKey.encode i ]
            let decode: Decoder<TsIdentityKey> = Decode.oneOf [
                Decode.object <| fun get ->
                    let file = get.Required.Field "DeclarationFile" Decode.string
                    let startPos = get.Required.Field "StartPos" Decode.int
                    let endPos = get.Required.Field "EndPos" Decode.int
                    DeclarationFile(file, startPos, endPos)
                Decode.object <| fun get -> get.Required.Field "Symbol" Decode.string |> Symbol
                Decode.object <| fun get -> get.Required.Field "Type" TypeKey.decode |> Type
            ]

        module DuplicateEncoding =
            let encode (encoder: Encoder<'T>) (value: DuplicateEncoding<'T>) =
                Encode.object [
                    "Identity", TsIdentityKey.encode value.Identity
                    "Value", encoder value.Value
                ]
            let inline decode<'T> (decoder: Decoder<'T>): Decoder<DuplicateEncoding<'T>> =
                Decode.object <| fun get -> {
                    Identity = get.Required.Field "Identity" TsIdentityKey.decode
                    Value = get.Required.Field "Value" decoder
                }
        module PackageMap =
            module Packages =
                let encode (value: Map<PackageId, Package>) = Encode.map PackageId.encode Package.encode value
                let decode: Decoder<Map<PackageId, Package>> = Decode.map' PackageId.decode Package.decode
            module SubModules =
                let encode (value: Map<SubModuleId, SubModule>) = Encode.map SubModuleId.encode SubModule.encode value
                let decode: Decoder<Map<SubModuleId, SubModule>> = Decode.map' SubModuleId.decode SubModule.decode
            let encode (value: PackageMap) = Encode.object [
                nameof value.Packages, Packages.encode value.Packages
                nameof value.SubModules, SubModules.encode value.SubModules
                nameof value.SubModuleRelations,
                value.SubModuleRelations
                #if FABLE_COMPILER
                |> Array.map
                #else
                |> List.map
                #endif
                    SubModuleRelation.encode
                |> Encode.listOrArray
            ]
            let decode: Decoder<PackageMap> = Decode.object <| fun get -> {
                Packages = get.Required.Field (nameof mock.Packages) Packages.decode
                SubModuleRelations = get.Required.Field (nameof mock.SubModuleRelations) (Decode.listOrArray SubModuleRelation.decode)
                SubModules = get.Required.Field (nameof mock.SubModules) SubModules.decode
            }
        module EncodedResult =
            let encode (value: EncodedResult) =
                Encode.object [
                    nameof value.PackageMap,
                    value.PackageMap
                    |> PackageMap.encode
                    
                    "ExportedDeclarations",
                    value.ExportedDeclarations
                    |> Encode.map TypeKey.encode TsExportDeclaration.encode
                    
                    "Types",
                    value.Types
                    |> Encode.map TypeKey.encode TsType.encode
                    
                    "DuplicateExports",
                    value.DuplicateExports
                    |> Encode.map TypeKey.encode (
                        List.map (DuplicateEncoding.encode TsExportDeclaration.encode)
                        >> Encode.list
                    )
                    
                    "DuplicateTypes",
                    value.DuplicateTypes
                    |> Encode.map TypeKey.encode (
                        List.map (DuplicateEncoding.encode TsType.encode)
                        >> Encode.list
                    )
                    
                    "TopLevelExports",
                    value.TopLevelExports
                    |> List.map TypeKey.encode
                    |> Encode.list
                    
                    "LibEsExports",
                    value.LibEsExports
                    |> List.map TypeKey.encode
                    |> Encode.list
                ]
            let decode: Decoder<EncodedResult> = Decode.object <| fun get -> {
                PackageMap = get.Required.Field (nameof mock.PackageMap) PackageMap.decode
                ExportedDeclarations = get.Required.Field "ExportedDeclarations" (Decode.map' TypeKey.decode TsExportDeclaration.decode)
                Types = get.Required.Field "Types" (Decode.map' TypeKey.decode TsType.decode)
                DuplicateExports = get.Required.Field "DuplicateExports" (Decode.map' TypeKey.decode (Decode.list (DuplicateEncoding.decode TsExportDeclaration.decode)))
                DuplicateTypes = get.Required.Field "DuplicateTypes" (Decode.map' TypeKey.decode (Decode.list (DuplicateEncoding.decode TsType.decode)))
                TopLevelExports = get.Required.Field "TopLevelExports" (Decode.list TypeKey.decode)
                LibEsExports = get.Required.Field "LibEsExports" (Decode.list TypeKey.decode)
            }