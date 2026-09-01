module Xantham.TypeScript.Type

open Fable.Core
open TypeScript

[<RequireQualifiedAccess>]
type TypeValidationError =
    /// <summary>
    /// This is a type error object without a symbol.
    /// </summary>
    | SymbolessErrorType
    /// <summary>
    /// This is a type object that still has a symbol (which is also be an unknown symbol kind).
    /// The storage of the information is despite the compiler diagnostic error for parsing the type.
    /// </summary>
    | ErrorType

[<RequireQualifiedAccess>]
type PrimitiveSingleton=
    | Any of Ts.Type
    | Unknown of Ts.Type
    | String of Ts.Type
    | Number of Ts.Type
    | Boolean of Ts.Type
    | BigInt of Ts.Type
    | ESSymbol of Ts.Type
    | Void of Ts.Type
    | Undefined of Ts.Type
    | Null of Ts.Type
    | Never of Ts.Type
    | NonPrimitive of Ts.Type
    interface INeverSymbol
    interface IFastUnionUnwrappable<Ts.Type>

type StringLiteralType =
    inherit IErasedWrapper<Ts.StringLiteralType>
    inherit INeverSymbol

type NumberLiteralType =
    inherit IErasedWrapper<Ts.NumberLiteralType>
    inherit INeverSymbol

type BigIntLiteralType =
    inherit IErasedWrapper<Ts.BigIntLiteralType>
    inherit INeverSymbol

type LiteralType =
    inherit IErasedWrapper<Ts.LiteralType>
    inherit INeverSymbol

[<RequireQualifiedAccess>]
type PrimitiveLiteral =
    | String of StringLiteralType
    | Number of NumberLiteralType
    | BigInt of BigIntLiteralType
    | Boolean of LiteralType
    interface INeverSymbol
    interface IFastUnionUnwrappable<Ts.LiteralType>
    
type UniqueESSymbol =
    inherit IAlwaysSymbol
    inherit ICanHaveSymbol<Symbol.Property>
    inherit ICanHaveSymbol<Symbol.Method>
    inherit IErasedWrapper<Ts.UniqueESSymbolType>

type EnumMember = EnumMember of PrimitiveLiteral with
    interface IAlwaysSymbol
    interface IInlinedProgram
    interface IAlwaysSymbol<Symbol.EnumMember>
    interface IFastUnionUnwrappable<PrimitiveLiteral>

[<RequireQualifiedAccess>]
type Literal =
    | UniqueESSymbol of UniqueESSymbol
    | PrimitiveLiteral of PrimitiveLiteral
    | EnumMember of EnumMember
    interface ICanHaveSymbol

type StringMappingType =
    inherit IErasedWrapper<Ts.StringMappingType>
    inherit IAlwaysSymbol
    inherit IAlwaysSymbol<Symbol.TypeAlias>

[<RequireQualifiedAccess>]
type StringMapping =
    | Capitalize of StringMappingType
    | Lowercase of StringMappingType
    | Uppercase of StringMappingType
    | Uncapitalize of StringMappingType
    interface IAlwaysSymbol
    interface IAlwaysSymbol<Symbol.TypeAlias>
    interface IFastUnionUnwrappable<Ts.StringMappingType>

type TypeParameter = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.TypeParameter>
type Intersection = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.IntersectionType>
type Union = 
    inherit IInlinedTypeChecker
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.UnionType>
type TemplateLiteral = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.TemplateLiteralType>
type Index = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.IndexType>
    
[<RequireQualifiedAccess>]
type InstantiablePrimitive =
    | Index of Index
    | StringMapping of StringMapping
    | TemplateLiteral of TemplateLiteral
    interface ICanHaveSymbol
type Substitution = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.SubstitutionType>
type Conditional = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.ConditionalType>
type IndexedAccess = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.IndexedAccessType>
    
[<RequireQualifiedAccess>]
type InstantiableNonPrimitive =
    | Conditional of Conditional
    | Substitution of Substitution
    | TypeParameter of TypeParameter
    | IndexedAccess of IndexedAccess
    interface ICanHaveSymbol
    interface IFastUnionUnwrappable<Ts.InstantiableType>
    
[<RequireQualifiedAccess>]
type Instantiable =
    | Primitive of InstantiablePrimitive
    | NonPrimitive of InstantiableNonPrimitive
    interface INeverSymbol

[<RequireQualifiedAccess>]
type Primitive =
    | Singleton of PrimitiveSingleton
    | Literal of Literal
    interface ICanHaveSymbol


type Interface = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.InterfaceType>

type Class = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.InterfaceType>

type PureTypeReference = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.TypeReference>
    
type InterfaceReference = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.TypeReference>
    
type ClassReference = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.TypeReference>
    
type ArrayReference = 
    inherit IAlwaysSymbol
    inherit IErasedWrapper<Ts.TypeReference>
    
type TupleReference = 
    inherit INeverSymbol
    inherit IErasedWrapper<Ts.TypeReference>

[<RequireQualifiedAccess>]
type TypeReference =
    | Pure of PureTypeReference
    | Array of ArrayReference
    | Tuple of TupleReference
    | Interface of InterfaceReference
    | Class of ClassReference
    interface ICanHaveSymbol
    interface IFastUnionUnwrappable<Ts.TypeReference>

type ObjectType = inherit IErasedWrapper<Ts.ObjectType>
    
type ObjectRest = inherit ObjectType
type InstantiationExpression = inherit ObjectType
type AnonymousType = inherit ObjectType
type InstantiatedAnonymousType = inherit ObjectType

[<RequireQualifiedAccess>]
type Anonymous =
    | ObjectRest of ObjectRest
    | InstantiationExpression of InstantiationExpression
    | Anonymous of AnonymousType
    | Instantiated of InstantiatedAnonymousType
    interface IAlwaysSymbol
    interface IFastUnionUnwrappable<Ts.ObjectType>

type MappedType = inherit ObjectType
type InstantiatedMappedType = inherit ObjectType

[<RequireQualifiedAccess>]
type Mapped =
    | Mapped of MappedType
    | Instantiated of InstantiatedMappedType
    interface IAlwaysSymbol<Symbol.TypeLiteral>
    interface IFastUnionUnwrappable<Ts.ObjectType>

[<RequireQualifiedAccess>]
type Structural =
    | Mapped of Mapped
    | Union of Union
    | Intersection of Intersection
    | Anonymous of Anonymous
    | TypeReference of TypeReference
    | Interface of Interface
    | Class of Class
    interface ICanHaveSymbol

type Enum = 
    inherit ICanHaveSymbol<Symbol.TypeEnum>
    inherit ICanHaveSymbol<Symbol.ConstEnum>
    inherit IAlwaysSymbol<Symbol.IEnum>
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.EnumType>

[<RequireQualifiedAccess>]
type Kind =
    | Primitive of Primitive
    | Structural of Structural
    | Instantiable of Instantiable
    | Enum of Enum
    interface ICanHaveSymbol
