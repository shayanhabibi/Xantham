[<AutoOpen>]
module Xantham.SimpleGenerator.Measures
open Xantham.Decoder

(* ==================================
  | We store all hashes as integers,|
  | with an associated measure to   |
  | provide static type safety.     |
   =============================== *)
[<Measure>] type nameKey; type NameKey = int<nameKey>
[<Measure>] type sourceKey; type SourceKey = int<sourceKey>
[<Measure>] type parameterKey; type ParameterKey = int<parameterKey>
[<Measure>] type typeParameterKey; type TypeParameterKey = int<typeParameterKey>
[<Measure>] type literalKey; type LiteralKey = int<literalKey>
[<Measure>] type primitiveKey; type PrimitiveKey = int<primitiveKey>
[<Measure>] type enumCaseKey; type EnumCaseKey = int<enumCaseKey>
[<Measure>] type enumKey; type EnumKey = int<enumKey>
[<Measure>] type interfaceKey; type InterfaceKey = int<interfaceKey>
[<Measure>] type classKey; type ClassKey = int<classKey>
[<Measure>] type documentationKey; type DocumentationKey = int<documentationKey>
[<Measure>] type memberKey; type MemberKey = int<memberKey>
[<Measure>] type hashTypeKey; type HashTypeKey = int<hashTypeKey>
[<Measure>] type constructorKey; type ConstructorKey = int<constructorKey>
[<Measure>] type tupleElementKey; type TupleElementKey = int<tupleElementKey>
[<Measure>] type variableKey; type VariableKey = int<variableKey>
[<Measure>] type indexSignatureKey; type IndexSignatureKey = int<indexSignatureKey>
[<Measure>] type functionKey; type FunctionKey = int<functionKey>
[<Measure>] type conditionalKey; type ConditionalKey = int<conditionalKey>
[<Measure>] type typeReferenceKey; type TypeReferenceKey = int<typeReferenceKey>
[<Measure>] type typeLiteralKey; type TypeLiteralKey = int<typeLiteralKey>
[<Measure>] type indexAccessKey; type IndexAccessKey = int<indexAccessKey>
[<Measure>] type moduleKey; type ModuleKey = int<moduleKey>
[<Measure>] type unionKey; type UnionKey = int<unionKey>
[<Measure>] type intersectionKey; type IntersectionKey = int<intersectionKey>
[<Measure>] type predicateKey; type PredicateKey = int<predicateKey>
[<Measure>] type typeAliasKey; type TypeAliasKey = int<typeAliasKey>
[<Measure>] type methodKey; type MethodKey = int<methodKey>
[<Measure>] type callSignatureKey; type CallSignatureKey = int<callSignatureKey>
[<Measure>] type constructSignatureKey; type ConstructSignatureKey = int<constructSignatureKey>
[<Measure>] type propertyKey; type PropertyKey = int<propertyKey>
[<Measure>] type getAccessorKey; type GetAccessorKey = int<getAccessorKey>
[<Measure>] type setAccessorKey; type SetAccessorKey = int<setAccessorKey>
[<Measure>] type tupleKey; type TupleKey = int<tupleKey>
[<Measure>] type masterKey; type MasterKey = int<masterKey>
[<Measure>] type cyclicKey; type CyclicKey = int<cyclicKey>
[<Measure>] type indexKey; type IndexKey = int<indexKey>
