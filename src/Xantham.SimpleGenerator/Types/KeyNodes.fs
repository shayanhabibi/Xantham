namespace Xantham.SimpleGenerator

open Xantham


[<Struct>]
type KeyEnumCase = {
    Name: NameKey
    Value: LiteralKey
}

[<Struct>]
type KeyEnum = {
    Qualifiers: NameKey array
    Name: NameKey
    Source: SourceKey
    Members: EnumCaseKey array
}

[<Struct>]
type KeyInterface = {
    Qualifiers: NameKey array
    Name: NameKey
    Heritage: TypeReferenceKey array
    Source: SourceKey
    TypeParameters: TypeParameterKey array
    Members: MemberKey array
}

[<Struct>]
type KeyClass = {
    Qualifiers: NameKey array
    Name: NameKey
    Heritage: TypeReferenceKey array
    Source: SourceKey
    TypeParameters: TypeParameterKey array
    Members: MemberKey array
    Constructors: ConstructorKey array
}

[<Struct>]
type KeyVariable = {
    Qualifiers: NameKey array
    Source: SourceKey
    Name: NameKey
    Type: MasterKey
}

[<Struct>]
type KeyParameter = {
    Name: NameKey
    Type: MasterKey
    IsOptional: bool
    IsSpread: bool
}

[<Struct>]
type KeyTypeParameter = {
    Name: NameKey
    Constraint: MasterKey voption
    Default: MasterKey voption
}

[<Struct>]
type KeyMethod = {
    Name: NameKey
    Parameters: ParameterKey array
    Type: MasterKey
    IsStatic: bool
    IsOptional: bool
}

[<Struct>]
type KeyCallSignature = {
    Parameters: ParameterKey array
    Type: MasterKey
}

[<Struct>]
type KeyConstructSignature = {
    Parameters: ParameterKey array
    Type: MasterKey
}

[<Struct>]
type KeyConstructor = {
    Parameters: ParameterKey array
}

[<Struct>]
type KeyProperty = {
    Name: NameKey
    Type: MasterKey
    Accessor: TsAccessor
    IsStatic: bool
    IsPrivate: bool
    IsOptional: bool
}

[<Struct>]
type KeyGetAccessor = {
    Name: NameKey
    Type: MasterKey
    IsStatic: bool
    IsPrivate: bool
}

[<Struct>]
type KeySetAccessor = {
    Name: NameKey
    Type: MasterKey
    IsStatic: bool
    IsPrivate: bool
}

[<Struct>]
type KeyIndexSignature = {
    Parameters: ParameterKey array
    Type: MasterKey
    IsReadonly: bool
}

[<Struct>]
type KeyFunction = {
    Qualifiers: NameKey array
    Source: SourceKey
    Name: NameKey
    Parameters: ParameterKey array
    Type: MasterKey
    TypeParameters: TypeParameterKey array
}

[<Struct>]
type KeyConditional = {
    Check: MasterKey
    Extends: MasterKey
    True: MasterKey
    False: MasterKey
}

[<Struct>]
type KeyTypeReference = {
    Type: MasterKey
    TypeArguments: MasterKey array
    ResolvedType: MasterKey voption
}

[<Struct>]
type KeyTypeLiteral = {
    Members: MemberKey array
}

[<Struct>]
type KeyIndexAccess = {
    Index: MasterKey
    Object: MasterKey
}

[<Struct>]
type KeyModule = {
    Qualifiers: NameKey array
    Source: SourceKey
    Types: MasterKey array
    Name: NameKey
    IsNamespace: bool
    IsRecursive: bool
}

[<Struct>]
type KeyUnion = {
    Types: MasterKey array
}

[<Struct>]
type KeyIntersection = {
    Types: MasterKey array
}

[<Struct>]
type KeyTupleElement = {
    Name: NameKey voption
    Type: MasterKey
    IsOptional: bool
    IsRest: bool
    IsVariadic: bool
}

[<Struct>]
type KeyTuple = {
    Elements: TupleElementKey array
    MinRequired: int
    FixedLength: int 
    IsReadOnly: bool
}

[<Struct>]
type KeyIndex = {
    Type: MasterKey
}

[<Struct>]
type KeyPredicate = {
    Type: MasterKey
    IsAssertion: bool
}

[<Struct>]
type KeyTypeAlias = {
    Qualifiers: NameKey array
    Source: SourceKey
    Name: NameKey
    Type: MasterKey
    TypeParameters: TypeParameterKey array
}

/// Members can be indexed separately since they should never be really referenced
/// directly 'as a type'. They are still given master keys, but through a single
/// union that contains all member types.
[<Struct; RequireQualifiedAccess>]
type MemberBuilder =
    | Method of MethodKey
    | CallSignature of CallSignatureKey
    | ConstructSignature of ConstructSignatureKey
    | Property of PropertyKey
    | GetAccessor of GetAccessorKey
    | SetAccessor of SetAccessorKey
    | IndexSignature of IndexSignatureKey

[<Struct; RequireQualifiedAccess>]
type MasterBuilder =
    | Global
    | Array of MasterKey
    | TypeKey of TypeKey
    | Name of NameKey
    | Source of SourceKey
    | Parameter of ParameterKey
    | TypeParameter of TypeParameterKey
    | Literal of LiteralKey
    | Primitive of PrimitiveKey
    | EnumCase of EnumCaseKey
    | Enum of EnumKey
    | Interface of InterfaceKey
    | Class of ClassKey
    | Documentation of DocumentationKey
    | Member of MemberKey
    | HashType of HashTypeKey
    | Constructor of ConstructorKey
    | TupleElement of TupleElementKey
    | Variable of VariableKey
    | Function of FunctionKey
    | Conditional of ConditionalKey
    | TypeReference of TypeReferenceKey
    | TypeLiteral of TypeLiteralKey
    | IndexAccess of IndexAccessKey
    | Index of IndexKey
    | Module of ModuleKey
    | Union of UnionKey
    | Intersection of IntersectionKey
    | Predicate of PredicateKey
    | TypeAlias of TypeAliasKey
    | Tuple of TupleKey
    | Cyclic of CyclicKey
