module Xantham.SimpleGenerator.KeyNodeHashing

open Xantham
open Xantham.Decoder

module PrimitiveKey =
    let create (value: TypeKindPrimitive): PrimitiveKey =
        value.GetHashCode() |> LanguagePrimitives.Int32WithMeasure<primitiveKey>

module HashTypeKey =
    let create (key: TypeKey): HashTypeKey =
        key |> LanguagePrimitives.Int32WithMeasure<hashTypeKey>

module CyclicKey =
    let inline toHashTypeKey (key: CyclicKey): HashTypeKey = int key |> LanguagePrimitives.Int32WithMeasure<hashTypeKey>
    let inline fromHashTypeKey (key: HashTypeKey): CyclicKey = int key |> LanguagePrimitives.Int32WithMeasure<cyclicKey>
    let create (key: TypeKey): CyclicKey =
        key
        |> HashTypeKey.create
        |> fromHashTypeKey

module NameKey =
    let inline create (value: ^T): NameKey =
        Name.get value
        |> _.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<nameKey>
    let inline createFromString (value: string): NameKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<nameKey>
    let inline createAndPassString (value: ^T): string * NameKey =
        let name =
            Name.get value
        name,
        name |> createFromString
    
    let moduleMagicKeyString = "__MODULE__"
    let typarTransientString = "Typar"
    let moduleMagicKey = createFromString moduleMagicKeyString
    let typarTransientKey = createFromString typarTransientString
    
module TypeReferenceKey =
    let inline create (value: KeyTypeReference): TypeReferenceKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<typeReferenceKey>

module TypeParameterKey =
    let inline create (value: KeyTypeParameter): TypeParameterKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<typeParameterKey>

module SourceKey =
    let inline createFrom (value: ShapeSourceMaybe<_>): SourceKey =
        value.Source
        |> Option.toValueOption
        |> _.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<sourceKey>
    let inline createFromString (value: string): SourceKey =
        (ValueSome value).GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<sourceKey>
    let inline create (value: string option): SourceKey =
        value
        |> Option.toValueOption
        |> _.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<sourceKey>
    let nullKey =  create None

module MethodKey =
    let inline create (value: KeyMethod): MethodKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<methodKey>

module MemberKey =
    let inline create (value: MemberBuilder): MemberKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<memberKey>

module ParameterKey =
    let inline create (value: KeyParameter): ParameterKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<parameterKey>

module CallSignatureKey =
    let inline create (value: KeyCallSignature): CallSignatureKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<callSignatureKey>

module ConstructSignatureKey =
    let inline create (value: KeyConstructSignature): ConstructSignatureKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<constructSignatureKey>

module PropertyKey =
    let inline create (value: KeyProperty): PropertyKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<propertyKey>

module GetAccessorKey =
    let inline create (value: KeyGetAccessor): GetAccessorKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<getAccessorKey>

module SetAccessorKey =
    let inline create (value: KeySetAccessor): SetAccessorKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<setAccessorKey>

module IndexSignatureKey =
    let inline create (value: KeyIndexSignature): IndexSignatureKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<indexSignatureKey>

module ConstructorKey =
    let inline create (value: KeyConstructor): ConstructorKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<constructorKey>

module MasterKey =
    let inline create (value: MasterBuilder): MasterKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<masterKey>

module ClassKey =
    let inline create (value: KeyClass): ClassKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<classKey>

module GlobalKey =
    let inline create (): MasterKey =
        MasterBuilder.Global
        |> MasterKey.create

module ConditionalKey =
    let inline create (value: KeyConditional): ConditionalKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<conditionalKey>

module InterfaceKey =
    let inline create (value: KeyInterface): InterfaceKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<interfaceKey>
        
module VariableKey =
    let inline create (value: KeyVariable): VariableKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<variableKey>

module EnumKey =
    let inline create (value: KeyEnum): EnumKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<enumKey>

module EnumCaseKey =
    let inline create (value: KeyEnumCase): EnumCaseKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<enumCaseKey>

module TypeAliasKey =
    let inline create (value: KeyTypeAlias): TypeAliasKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<typeAliasKey>
        
module FunctionKey =
    let inline create (value: KeyFunction): FunctionKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<functionKey>

module UnionKey =
    let inline create (value: KeyUnion): UnionKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<unionKey>

module IntersectionKey =
    let inline create (value: KeyIntersection): IntersectionKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<intersectionKey>

module LiteralKey =
    let create (value: TsLiteral): LiteralKey =
        match value with
        | TsLiteral.Float floatValue ->
            if System.Double.IsInteger floatValue then
                int floatValue
                |> TsLiteral.Int
            else value
        | value -> value
        |> _.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<literalKey>

module IndexAccessKey =
    let inline create (value: KeyIndexAccess): IndexAccessKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<indexAccessKey>

module ModuleKey =
    let inline create (value: KeyModule): ModuleKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<moduleKey>

module TupleKey =
    let inline create (value: KeyTuple): TupleKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<tupleKey>

module TupleElementKey =
    let inline create (value: KeyTupleElement): TupleElementKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<tupleElementKey>
        
module IndexKey =
    let inline create (value: KeyIndex): IndexKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<indexKey>
        
module PredicateKey =
    let inline create (value: KeyPredicate): PredicateKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<predicateKey>

module TypeLiteralKey =
    let inline create (value: KeyTypeLiteral): TypeLiteralKey =
        value.GetHashCode()
        |> LanguagePrimitives.Int32WithMeasure<typeLiteralKey>

