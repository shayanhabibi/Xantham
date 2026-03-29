namespace rec Xantham.SimpleGenerator

open System.Collections.Frozen
open System.Collections.Generic
open Microsoft.Extensions.Logging
open Xantham
open Xantham.Decoder
open Xantham.Decoder.Runtime
open Xantham.SimpleGenerator.KeyNodeHashing
open FSharp.Logf

(*
Types, members, properties, names etc are hashed, and composed into structs which
are further hashed to produce unique identifiers for potentially common patterns.

The hashes are used as keys to retrieve the data. The data is pre-computed and stored
in a dictionary with all relevant information and functions prepared for whatever
circumstance the data is needed for.

Paths can be composed separately.

Only unique and relevant information is used to produce the hashes.
As an example, documentation for all objects is ignored, and must be stored separately, as
a value in a dictionary.

This should ultimately prevent recomputation of common objects/patterns and improve performance.
*)

type ShapeDocumentation<^T when ^T:(member Documentation: TsComment list)> = ^T

module Prelude =
    module Primitive =
        let anyKey = PrimitiveKey.create TypeKindPrimitive.Any
        let unknownKey = PrimitiveKey.create TypeKindPrimitive.Unknown
        let neverKey = PrimitiveKey.create TypeKindPrimitive.Never
        let voidKey = PrimitiveKey.create TypeKindPrimitive.Void
        let undefinedKey = PrimitiveKey.create TypeKindPrimitive.Undefined
        let nullKey = PrimitiveKey.create TypeKindPrimitive.Null
        let stringKey = PrimitiveKey.create TypeKindPrimitive.String
        let integerKey = PrimitiveKey.create TypeKindPrimitive.Integer
        let numberKey = PrimitiveKey.create TypeKindPrimitive.Number
        let booleanKey = PrimitiveKey.create TypeKindPrimitive.Boolean
        let bigIntKey = PrimitiveKey.create TypeKindPrimitive.BigInt
        let esSymbolKey = PrimitiveKey.create TypeKindPrimitive.ESSymbol
        let nonPrimitiveKey = PrimitiveKey.create TypeKindPrimitive.NonPrimitive
        let primitives =
            Set [
                anyKey
                unknownKey
                neverKey
                voidKey
                undefinedKey
                nullKey
                stringKey
                integerKey
                numberKey
                booleanKey
                bigIntKey
                esSymbolKey
                nonPrimitiveKey
            ]
        module Master =
            let anyKey = MasterBuilder.Primitive Primitive.anyKey |> MasterKey.create
            let unknownKey = MasterBuilder.Primitive Primitive.unknownKey |> MasterKey.create
            let neverKey = MasterBuilder.Primitive Primitive.neverKey |> MasterKey.create
            let voidKey = MasterBuilder.Primitive Primitive.voidKey |> MasterKey.create
            let undefinedKey = MasterBuilder.Primitive Primitive.undefinedKey |> MasterKey.create
            let nullKey = MasterBuilder.Primitive Primitive.nullKey |> MasterKey.create
            let stringKey = MasterBuilder.Primitive Primitive.stringKey |> MasterKey.create
            let integerKey = MasterBuilder.Primitive Primitive.integerKey |> MasterKey.create
            let numberKey = MasterBuilder.Primitive Primitive.numberKey |> MasterKey.create
            let booleanKey = MasterBuilder.Primitive Primitive.booleanKey |> MasterKey.create
            let bigIntKey = MasterBuilder.Primitive Primitive.bigIntKey |> MasterKey.create
            let esSymbolKey = MasterBuilder.Primitive Primitive.esSymbolKey |> MasterKey.create
            let nonPrimitiveKey = MasterBuilder.Primitive Primitive.nonPrimitiveKey |> MasterKey.create
            let primitives =
                Set [
                    anyKey
                    unknownKey
                    neverKey
                    voidKey
                    undefinedKey
                    nullKey
                    stringKey
                    integerKey
                    numberKey
                    booleanKey
                    bigIntKey
                    esSymbolKey
                    nonPrimitiveKey
                    Union.Master.variant1
                    Union.Master.variant2
                ]
                
    module Literal =
        let trueKey = LiteralKey.create (TsLiteral.Bool true)
        let falseKey = LiteralKey.create (TsLiteral.Bool false)
        let nullKey = LiteralKey.create TsLiteral.Null
        module Master =
            let trueKey = MasterBuilder.Literal Literal.trueKey |> MasterKey.create
            let falseKey = MasterBuilder.Literal Literal.falseKey |> MasterKey.create
            let nullKey = MasterBuilder.Literal Literal.nullKey |> MasterKey.create
    module Union =
        let variant1 =
            { KeyUnion.Types = [| Literal.Master.trueKey; Literal.Master.falseKey |] }
            |> UnionKey.create
        let variant2 =
            { KeyUnion.Types = [| Literal.Master.falseKey; Literal.Master.trueKey |] }
            |> UnionKey.create
        let booleans = Set [ variant1; variant2 ]
        module Master =
            let variant1 = MasterBuilder.Union Union.variant1 |> MasterKey.create
            let variant2 = MasterBuilder.Union Union.variant2 |> MasterKey.create
            let booleans = Set [ variant1; variant2 ]
    module Name =
        let moduleMagicKey = NameKey.moduleMagicKey
        let typarTransientKey = NameKey.typarTransientKey
module KeyResolution =
    let getName ctx key =
        ctx.keyCache.nameKeys
        |> Dictionary.tryItem key
        |> ValueOption.defaultWith(fun _ -> failwithf "Could not find name for key %A" key)
    let getNamePascalCase ctx = getName ctx >> Name.Pascal.create
    let getNameCamelCase ctx = getName ctx >> Name.Camel.create
    let createNameKey ctx value = 
        let key = NameKey.createFromString value
        let name = value
        key |> Dictionary.Flip.tryAddKeyPassthrough ctx.keyCache.nameKeys name
    let inline createQualifierKeys ctx (value: ^T when ^T:(member FullyQualifiedName: string list)) =
        let length = value.FullyQualifiedName.Length
        if length <= 1 then [||] else
        value.FullyQualifiedName
        |> Seq.truncate (value.FullyQualifiedName.Length - 1)
        |> Seq.map (createNameKey ctx)
        |> Seq.toArray
    
    [<Struct>]
    type VisitationContext<^T, ^KeyType, ^TypeKey
        when ^KeyType: struct and ^TypeKey: struct> = {
        keyType: ^KeyType
        typeKey: ^TypeKey
        masterKey: MasterKey
        masterBuilder: MasterBuilder
        documentation: TsComment array voption
        visitationFlags: VisitationFlags
    }
    
    [<Struct>]
    type LiteralAssociations = {
        Associations: MasterKey seq
        Mapping: LiteralKey seq
    }
    /// <summary>
    /// Visitation policies clearly define the rules/behavior for inheriting/associating keys
    /// and literals or the literals associated to their nested types.
    /// </summary>
    type LiteralAssociationPolicy<^T, ^KeyType, ^TypeKey when ^KeyType: struct and ^TypeKey: struct> =
        KeyResolutionContext -> VisitationContext<^T, ^KeyType, ^TypeKey> -> LiteralAssociations
    module LiteralAssociationPolicy =
        let private empty = { Associations = Seq.empty; Mapping = Seq.empty }
        let typeReference: LiteralAssociationPolicy<TsTypeReference, KeyTypeReference, TypeReferenceKey> = fun ctx visitCtx ->
            { Associations = [
                visitCtx.keyType.ResolvedType
                |> ValueOption.defaultValue visitCtx.keyType.Type
            ]; Mapping = List.empty }
        let typeParameter: LiteralAssociationPolicy<TsTypeParameter, KeyTypeParameter, TypeParameterKey> = fun ctx visitCtx ->
            { Associations =
                  visitCtx.keyType.Constraint
                  |> ValueOption.orElse visitCtx.keyType.Default
                  |> ValueOption.toArray
              Mapping = List.empty }
        let parameter: LiteralAssociationPolicy<TsParameter, KeyParameter, ParameterKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let method: LiteralAssociationPolicy<TsMethod, KeyMethod, MethodKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let callSignature: LiteralAssociationPolicy<TsCallSignature, KeyCallSignature, CallSignatureKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let constructSignature: LiteralAssociationPolicy<TsConstructSignature, KeyConstructSignature, ConstructSignatureKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let property: LiteralAssociationPolicy<TsProperty, KeyProperty, PropertyKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let getAccessor: LiteralAssociationPolicy<TsGetAccessor, KeyGetAccessor, GetAccessorKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let setAccessor: LiteralAssociationPolicy<TsSetAccessor, KeySetAccessor, SetAccessorKey> = fun ctx visitCtx ->
            empty
        let indexSignature: LiteralAssociationPolicy<TsIndexSignature, KeyIndexSignature, IndexSignatureKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let constructor: LiteralAssociationPolicy<TsConstructor, KeyConstructor, ConstructorKey> = fun ctx visitCtx ->
            empty
        let ``function``: LiteralAssociationPolicy<TsFunction, KeyFunction, FunctionKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let typeAlias: LiteralAssociationPolicy<TsTypeAlias, KeyTypeAlias, TypeAliasKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let enum: LiteralAssociationPolicy<TsEnumType, KeyEnum, EnumKey> = fun ctx visitCtx ->
            {
                Associations =
                    visitCtx.keyType.Members
                    |> Array.map (
                        MasterBuilder.EnumCase
                        >> MasterKey.create
                        )
                Mapping = Seq.empty
            }
        let enumCase: LiteralAssociationPolicy<TsEnumCase, KeyEnumCase, EnumCaseKey> = fun ctx visitCtx ->
            empty
        let variable: LiteralAssociationPolicy<TsVariable, KeyVariable, VariableKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let ``interface``: LiteralAssociationPolicy<TsInterface, KeyInterface, InterfaceKey> = fun ctx visitCtx ->
            empty
        let ``class``: LiteralAssociationPolicy<TsClass, KeyClass, ClassKey> = fun ctx visitCtx ->
            empty
        let conditional: LiteralAssociationPolicy<TsConditionalType, KeyConditional, ConditionalKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.True; visitCtx.keyType.False ]; Mapping = Seq.empty }
        let union: LiteralAssociationPolicy<TsTypeUnion, KeyUnion, UnionKey> = fun ctx visitCtx ->
            { Associations = visitCtx.keyType.Types; Mapping = Seq.empty }
        let intersection: LiteralAssociationPolicy<TsTypeIntersection, KeyIntersection, IntersectionKey> = fun ctx visitCtx ->
            empty
        let indexAccess: LiteralAssociationPolicy<TsIndexAccessType, KeyIndexAccess, IndexAccessKey> = fun ctx visitCtx ->
            match IndexAccess.tryResolveToMember ctx visitCtx.keyType with
            | ValueSome memberKey ->
                ctx.cache.members
                |> Dictionary.tryItem memberKey
                |> ValueOption.bind (function
                    | MemberBuilder.CallSignature callSignatureKey ->
                        ctx.cache.callSignatureKeys
                        |> Dictionary.tryItem callSignatureKey
                        |> ValueOption.map _.Type
                    | MemberBuilder.Method i ->
                        ctx.cache.methodKeys
                        |> Dictionary.tryItem i
                        |> ValueOption.map _.Type
                    | MemberBuilder.ConstructSignature i ->
                        ctx.cache.constructSignatureKeys
                        |> Dictionary.tryItem i
                        |> ValueOption.map _.Type
                    | MemberBuilder.Property i ->
                        ctx.cache.propertyKeys
                        |> Dictionary.tryItem i
                        |> ValueOption.map _.Type
                    | MemberBuilder.GetAccessor i ->
                        ctx.cache.getAccessorKeys
                        |> Dictionary.tryItem i
                        |> ValueOption.map _.Type
                    | MemberBuilder.SetAccessor i -> ValueNone
                    | MemberBuilder.IndexSignature i ->
                        ctx.cache.indexSignatureKeys
                        |> Dictionary.tryItem i
                        |> ValueOption.map _.Type
                    )
                |> ValueOption.map Seq.singleton
                |> ValueOption.defaultValue Seq.empty
                |> Seq.toList
                |> fun associations ->
                    { Associations = associations; Mapping = Seq.empty }
            | ValueNone -> empty
        let moduleDeclaration: LiteralAssociationPolicy<TsModule, KeyModule, ModuleKey> = fun ctx visitCtx ->
            empty
        let tuple: LiteralAssociationPolicy<TsTuple, KeyTuple, TupleKey> = fun ctx visitCtx ->
            empty
        let tupleElement: LiteralAssociationPolicy<TsTupleElement, KeyTupleElement, TupleElementKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Mapping = Seq.empty }
        let index: LiteralAssociationPolicy<TsIndex, KeyIndex, IndexKey> = fun ctx visitCtx ->
            ctx.cache.memberIndex
            |> Dictionary.tryItem visitCtx.masterKey
            |> ValueOption.map (
                Array.choose (
                    Member.tryNameKey ctx
                    >> ValueOption.bind (
                        Dictionary.Flip.tryItem ctx.cache.nameKeys
                        >> ValueOption.map (
                            TsLiteral.String
                            >> ctx.createLiteralKey
                            )
                        )
                    >> ValueOption.toOption
                    )
                )
            |> ValueOption.filter (Array.isEmpty >> not)
            |> ValueOption.map (fun literals ->
                { Associations = Seq.empty; Mapping = literals }
                )
            |> ValueOption.defaultValue empty
        let predicate: LiteralAssociationPolicy<TsTypePredicate, KeyPredicate, PredicateKey> = fun ctx visitCtx ->
            empty
        let typeLiteral: LiteralAssociationPolicy<TsTypeLiteral, KeyTypeLiteral, TypeLiteralKey> = fun ctx visitCtx ->
            empty
        
        /// <summary>
        /// Applies a given policy to a given context and returns a new context with the visitation flags updated.
        /// </summary>
        /// <param name="policy"></param>
        /// <param name="ctx"></param>
        /// <param name="visitCtx"></param>
        let apply<'T, 'KeyType, 'TypeKey when 'KeyType: struct and 'TypeKey: struct> (policy: LiteralAssociationPolicy<'T, 'KeyType, 'TypeKey>) (ctx: KeyResolutionContext) (visitCtx: VisitationContext<'T, 'KeyType, 'TypeKey>): VisitationContext<'T, 'KeyType, 'TypeKey> =
            let associations = policy ctx visitCtx
            ctx.cache.literalAssociations.RegisterAssociations visitCtx.masterKey associations.Associations
            let literalFlags =
                if associations.Associations |> Seq.isEmpty then VisitationFlags.None else
                associations.Associations
                |> Seq.fold (fun acc other ->
                    acc ||| (
                        ctx.cache.visitationFlags
                        |> Dictionary.tryItem other
                        |> ValueOption.defaultValue VisitationFlags.None
                        &&& VisitationFlags.WouldYieldLiteral
                        )
                    ) VisitationFlags.None
            let literalFlags =
                if associations.Mapping |> Seq.isEmpty then literalFlags else
                associations.Mapping
                |> Seq.fold (fun acc literalKey ->
                    match ctx[literalKey] with
                    | TsLiteral.Int _ | TsLiteral.BigInt _ -> acc ||| VisitationFlags.WouldYieldInt
                    | TsLiteral.Bool _ -> acc ||| VisitationFlags.WouldYieldBool
                    | TsLiteral.String _ -> acc ||| VisitationFlags.WouldYieldString
                    | TsLiteral.Float _ -> acc ||| VisitationFlags.WouldYieldFloat
                    | TsLiteral.Null -> acc ||| VisitationFlags.WouldYieldNull
                    ) literalFlags
                |> fun flags ->
                    ctx.cache.literalAssociations.RegisterLiterals visitCtx.masterKey associations.Mapping
                    |> ignore
                    flags
            addFlag literalFlags visitCtx
    
    /// <summary>
    /// IR for <c>MemberAssociationPolicy</c>.
    /// </summary>
    [<Struct>]
    type MemberAssociations = {
        Associations: MasterKey seq
        Members: MemberKey seq
    }
    /// <summary>
    /// A policy that determines which members of a given type should be associated with a given key.
    /// </summary>
    type MemberAssociationPolicy<'T, 'KeyType, 'TypeKey when 'KeyType: struct and 'TypeKey: struct> =
        KeyResolutionContext -> VisitationContext<'T, 'KeyType, 'TypeKey> -> MemberAssociations
    /// <summary>
    /// A policy that determines which members of a given type should be associated with a given key.
    /// </summary>
    module MemberAssociationPolicy =
        let private empty = { Associations = Seq.empty; Members = Seq.empty }
        let typeReference: MemberAssociationPolicy<TsTypeReference, KeyTypeReference, TypeReferenceKey> = fun ctx visitCtx ->
            {
                Associations = [
                    visitCtx.keyType.ResolvedType
                    |> ValueOption.defaultValue visitCtx.keyType.Type
                ]
                Members = Seq.empty
            }
        let typeParameter: MemberAssociationPolicy<TsTypeParameter, KeyTypeParameter, TypeParameterKey> = fun ctx visitCtx ->
            { Associations =
                visitCtx.keyType.Constraint
                |> ValueOption.orElse visitCtx.keyType.Default
                |> ValueOption.toArray
              Members = Seq.empty }
        let parameter: MemberAssociationPolicy<TsParameter, KeyParameter, ParameterKey> = fun ctx visitCtx ->
            empty
        let method: MemberAssociationPolicy<TsMethod, KeyMethod, MethodKey> = fun ctx visitCtx ->
            empty
        let callSignature: MemberAssociationPolicy<TsCallSignature, KeyCallSignature, CallSignatureKey> = fun ctx visitCtx ->
            empty
        let constructSignature: MemberAssociationPolicy<TsConstructSignature, KeyConstructSignature, ConstructSignatureKey> = fun ctx visitCtx ->
            empty
        let property: MemberAssociationPolicy<TsProperty, KeyProperty, PropertyKey> = fun ctx visitCtx ->
            empty
        let getAccessor: MemberAssociationPolicy<TsGetAccessor, KeyGetAccessor, GetAccessorKey> = fun ctx visitCtx ->
            empty
        let setAccessor: MemberAssociationPolicy<TsSetAccessor, KeySetAccessor, SetAccessorKey> = fun ctx visitCtx ->
            empty
        let indexSignature: MemberAssociationPolicy<TsIndexSignature, KeyIndexSignature, IndexSignatureKey> = fun ctx visitCtx ->
            empty
        let constructor: MemberAssociationPolicy<TsConstructor, KeyConstructor, ConstructorKey> = fun ctx visitCtx ->
            empty
        let ``function``: MemberAssociationPolicy<TsFunction, KeyFunction, FunctionKey> = fun ctx visitCtx ->
            empty
        let typeAlias: MemberAssociationPolicy<TsTypeAlias, KeyTypeAlias, TypeAliasKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Members = Seq.empty }
        let enum: MemberAssociationPolicy<TsEnumType, KeyEnum, EnumKey> = fun ctx visitCtx ->
            empty
        let enumCase: MemberAssociationPolicy<TsEnumCase, KeyEnumCase, EnumCaseKey> = fun ctx visitCtx ->
            empty
        let variable: MemberAssociationPolicy<TsVariable, KeyVariable, VariableKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Members = Seq.empty }
        let ``interface``: MemberAssociationPolicy<TsInterface, KeyInterface, InterfaceKey> = fun ctx visitCtx ->
            { Associations = Seq.empty; Members = visitCtx.keyType.Members }
        let ``class``: MemberAssociationPolicy<TsClass, KeyClass, ClassKey> = fun ctx visitCtx ->
            { Associations = Seq.empty; Members = visitCtx.keyType.Members }
        let conditional: MemberAssociationPolicy<TsConditionalType, KeyConditional, ConditionalKey> = fun ctx visitCtx ->
            let trueSet =
                visitCtx.keyType.True
                |> Dictionary.Flip.tryItem ctx.cache.memberIndex
                |> ValueOption.map Set
                |> ValueOption.defaultValue Set.empty
            let falseSet =
                visitCtx.keyType.False
                |> Dictionary.Flip.tryItem ctx.cache.memberIndex
                |> ValueOption.map Set
                |> ValueOption.defaultValue Set.empty
            {
                Associations = Seq.empty
                Members = Set.intersect trueSet falseSet |> Set.toArray
            }
        let union: MemberAssociationPolicy<TsTypeUnion, KeyUnion, UnionKey> = fun ctx visitCtx ->
            {
                Associations = Seq.empty
                Members =
                    visitCtx.keyType.Types
                    |> Array.choose (
                        Dictionary.Flip.tryItem ctx.cache.memberIndex
                        >> ValueOption.map Set.ofArray
                        >> ValueOption.toOption
                        )
                    |> Set.tryIntersectMany
                    |> Set.toArray
            }
        let intersection: MemberAssociationPolicy<TsTypeIntersection, KeyIntersection, IntersectionKey> = fun ctx visitCtx ->
            // todo - check
            { Associations = visitCtx.keyType.Types; Members = Seq.empty }
        let indexAccess: MemberAssociationPolicy<TsIndexAccessType, KeyIndexAccess, IndexAccessKey> = fun ctx visitCtx ->
            match IndexAccess.tryResolveToMember ctx visitCtx.keyType with
            | ValueSome memberKey ->
                { Associations = Seq.empty; Members = Seq.singleton memberKey }
            | ValueNone -> empty
        let moduleDeclaration: MemberAssociationPolicy<TsModule, KeyModule, ModuleKey> = fun ctx visitCtx ->
            // todo
            empty
        let tuple: MemberAssociationPolicy<TsTuple, KeyTuple, TupleKey> = fun ctx visitCtx ->
            empty
        let tupleElement: MemberAssociationPolicy<TsTupleElement, KeyTupleElement, TupleElementKey> = fun ctx visitCtx ->
            { Associations = [ visitCtx.keyType.Type ]; Members = Seq.empty }
        let index: MemberAssociationPolicy<TsIndex, KeyIndex, IndexKey> = fun ctx visitCtx ->
            empty
        let predicate: MemberAssociationPolicy<TsTypePredicate, KeyPredicate, PredicateKey> = fun ctx visitCtx ->
            empty
        let typeLiteral: MemberAssociationPolicy<TsTypeLiteral, KeyTypeLiteral, TypeLiteralKey> = fun ctx visitCtx ->
            { Associations = Seq.empty; Members = visitCtx.keyType.Members }
        let apply<'T, 'KeyType, 'TypeKey when 'KeyType: struct and 'TypeKey: struct> (policy: MemberAssociationPolicy<'T, 'KeyType, 'TypeKey>) (ctx: KeyResolutionContext) (visitCtx: VisitationContext<'T, 'KeyType, 'TypeKey>): VisitationContext<'T, 'KeyType, 'TypeKey> =
            let policyResult = policy ctx visitCtx
            let associatedKeys =
                policyResult.Associations
                |> Seq.collect (Dictionary.Flip.tryItem ctx.cache.memberIndex >> ValueOption.defaultValue Array.empty)
                |> Seq.append policyResult.Members
                |> Seq.distinct
                |> Seq.toArray
            match associatedKeys with
            | [||] -> visitCtx
            | members ->
                members
                |> Dictionary.Flip.tryAdd ctx.cache.memberIndex visitCtx.masterKey
                { visitCtx with visitationFlags = visitCtx.visitationFlags ||| VisitationFlags.HasMembers }
    /// <summary>
    /// IR for <c>VisitationFlagInheritancePolicy</c>.
    /// </summary>
    [<Struct>]
    type VisitationFlagInheritanceTargets = {
        Keys: MasterKey seq
        FlagMask: VisitationFlags
    }
    /// <summary>
    /// A policy that determines which nested types of a given type should have their
    /// visitation flags inherited/concatenated into the parent type's visitation flags.
    /// </summary>
    type VisitationFlagInheritancePolicy<'T, 'KeyType, 'TypeKey when 'KeyType: struct and 'TypeKey: struct> =
        KeyResolutionContext -> VisitationContext<'T, 'KeyType, 'TypeKey> -> VisitationFlagInheritanceTargets
    /// <summary>
    /// A policy that determines which nested types of a given type should have their
    /// visitation flags inherited/concatenated into the parent type's visitation flags.
    /// </summary>
    module VisitationFlagInheritancePolicy =
        /// <summary>
        /// The default mask is a simple exclusion of flags that are irrelevant for the purpose of inheritance.
        /// This includes <c>HasName</c>, <c>HasDocs</c>, <c>IsObsolete</c>.
        /// </summary>
        [<Literal>]
        let private defaultMask = ~~~(
            VisitationFlags.HasName
            ||| VisitationFlags.HasDocs
            ||| VisitationFlags.IsObsolete
            )
        let private empty = { Keys = Seq.empty; FlagMask = defaultMask }
        let private nullOp = fun _ _ -> empty
        let private singleton target = { Keys = Seq.singleton target; FlagMask = defaultMask }
        let typeReference: VisitationFlagInheritancePolicy<TsTypeReference, KeyTypeReference, TypeReferenceKey> = fun ctx visitCtx ->
            visitCtx.keyType.ResolvedType
            |> ValueOption.defaultValue visitCtx.keyType.Type
            |> singleton
        let typeParameter: VisitationFlagInheritancePolicy<TsTypeParameter, KeyTypeParameter, TypeParameterKey> = fun ctx visitCtx ->
            { Keys =
                visitCtx.keyType.Constraint
                |> ValueOption.orElse visitCtx.keyType.Default
                |> ValueOption.toArray
              FlagMask = defaultMask }
        let parameter: VisitationFlagInheritancePolicy<TsParameter, KeyParameter, ParameterKey> = nullOp
        let method: VisitationFlagInheritancePolicy<TsMethod, KeyMethod, MethodKey> = nullOp
        let callSignature: VisitationFlagInheritancePolicy<TsCallSignature, KeyCallSignature, CallSignatureKey> = nullOp
        let constructSignature: VisitationFlagInheritancePolicy<TsConstructSignature, KeyConstructSignature, ConstructSignatureKey> = nullOp
        let property: VisitationFlagInheritancePolicy<TsProperty, KeyProperty, PropertyKey> = nullOp
        let getAccessor: VisitationFlagInheritancePolicy<TsGetAccessor, KeyGetAccessor, GetAccessorKey> = nullOp
        let setAccessor: VisitationFlagInheritancePolicy<TsSetAccessor, KeySetAccessor, SetAccessorKey> = nullOp
        let indexSignature: VisitationFlagInheritancePolicy<TsIndexSignature, KeyIndexSignature, IndexSignatureKey> = nullOp
        let constructor: VisitationFlagInheritancePolicy<TsConstructor, KeyConstructor, ConstructorKey> = nullOp
        let ``function``: VisitationFlagInheritancePolicy<TsFunction, KeyFunction, FunctionKey> = nullOp
        let typeAlias: VisitationFlagInheritancePolicy<TsTypeAlias, KeyTypeAlias, TypeAliasKey> = fun ctx visitCtx ->
            // todo - check
            empty
            // visitCtx.keyType.Type
            // |> singleton
        let enum: VisitationFlagInheritancePolicy<TsEnumType, KeyEnum, EnumKey> = nullOp
        let enumCase : VisitationFlagInheritancePolicy<TsEnumCase, KeyEnumCase, EnumCaseKey> = nullOp
        let variable: VisitationFlagInheritancePolicy<TsVariable, KeyVariable, VariableKey> = nullOp
        let ``interface``: VisitationFlagInheritancePolicy<TsInterface, KeyInterface, InterfaceKey> = nullOp
        let ``class``: VisitationFlagInheritancePolicy<TsClass, KeyClass, ClassKey> = nullOp
        let conditional: VisitationFlagInheritancePolicy<TsConditionalType, KeyConditional, ConditionalKey> = nullOp
        let union: VisitationFlagInheritancePolicy<TsTypeUnion, KeyUnion, UnionKey> = nullOp
        let intersection: VisitationFlagInheritancePolicy<TsTypeIntersection, KeyIntersection, IntersectionKey> = fun ctx visitCtx ->
            { Keys = visitCtx.keyType.Types; FlagMask = defaultMask }
        let indexAccess: VisitationFlagInheritancePolicy<TsIndexAccessType, KeyIndexAccess, IndexAccessKey> = nullOp
        let moduleDeclaration: VisitationFlagInheritancePolicy<TsModule, KeyModule, ModuleKey> = nullOp
        let tuple: VisitationFlagInheritancePolicy<TsTuple, KeyTuple, TupleKey> = nullOp
        let tupleElement: VisitationFlagInheritancePolicy<TsTupleElement, KeyTupleElement, TupleElementKey> = nullOp
        let index: VisitationFlagInheritancePolicy<TsIndex, KeyIndex, IndexKey> = nullOp
        let predicate: VisitationFlagInheritancePolicy<TsTypePredicate, KeyPredicate, PredicateKey> = nullOp
        let typeLiteral: VisitationFlagInheritancePolicy<TsTypeLiteral, KeyTypeLiteral, TypeLiteralKey> = nullOp
        let apply<'T, 'KeyType, 'TypeKey when 'KeyType: struct and 'TypeKey: struct> (policy: VisitationFlagInheritancePolicy<'T, 'KeyType, 'TypeKey>) (ctx: KeyResolutionContext) (visitCtx: VisitationContext<'T, 'KeyType, 'TypeKey>): VisitationContext<'T, 'KeyType, 'TypeKey> =
            let { Keys = targets; FlagMask = mask } = policy ctx visitCtx
            let flags =
                targets
                |> Seq.fold (fun acc ->
                    Dictionary.Flip.tryItem ctx.cache.visitationFlags
                    >> function
                        | ValueSome flags -> acc ||| (flags &&& mask)
                        | ValueNone -> acc
                    ) visitCtx.visitationFlags
            addFlag flags visitCtx

    let inline private makeVisitationContextBase
            ctx
            (makeFn: KeyResolutionContext -> ^T -> ^KeyType)
            (keyFn: ^KeyType -> ^TypeKey)
            (converter: ^TypeKey -> MasterBuilder)
            (docMapping: (^T -> TsComment list) voption) = fun value ->
        let keyType = makeFn ctx value
        let typeKey = keyFn keyType
        let masterBuilder = converter typeKey
        let masterKey = MasterKey.create masterBuilder
        (masterKey, masterBuilder)
        ||> Dictionary.Flip.tryAddOrGet ctx.cache.masters
        |> function
            | builder when masterBuilder <> builder ->
                logfe ctx.Logger "Key %A{key} discovered with different builder: %A{builder} when we calculated %A{masterBuilder} for keyType %A{keyType}" masterKey builder masterBuilder keyType
            | _ -> ()
        let documentation =
            match docMapping with
            | ValueSome docMapping ->
                let documentation = docMapping value
                ctx.addDocumentation documentation masterKey
                |> ignore
                documentation |> Array.ofList |> ValueSome
            | ValueNone -> ValueNone
        {
            keyType = keyType
            typeKey = typeKey
            masterKey = masterKey
            masterBuilder = masterBuilder
            documentation = documentation
            visitationFlags =
                if documentation.IsSome
                then VisitationFlags.HasDocs
                else VisitationFlags.None
        }

    let inline private makeDocVisitationContext<^T, ^KeyType, ^TypeKey
        when ShapeDocumentation<^T>
        and ^KeyType: struct and ^TypeKey: struct>
        ctx
        (makeFn: KeyResolutionContext -> ^T -> ^KeyType)
        (keyFn: ^KeyType -> ^TypeKey)
        (converter: ^TypeKey -> MasterBuilder): _ -> VisitationContext<^T, ^KeyType, ^TypeKey> =
        makeVisitationContextBase ctx makeFn keyFn converter (ValueSome _.Documentation)
    let inline private makeVisitationContext<^T, ^KeyType, ^TypeKey
        when ^KeyType: struct and ^TypeKey: struct>
        ctx
        (makeFn: KeyResolutionContext -> ^T -> ^KeyType)
        (keyFn: ^KeyType -> ^TypeKey)
        (converter: ^TypeKey -> MasterBuilder): _ -> VisitationContext<^T, ^KeyType, ^TypeKey> =
        makeVisitationContextBase ctx makeFn keyFn converter ValueNone
    
    let addFlag<'A, 'B, 'C when 'B: struct and 'C: struct> (flag: VisitationFlags) (visitCtx: VisitationContext<'A, 'B, 'C>): VisitationContext<'A, 'B, 'C> =
        { visitCtx with visitationFlags = visitCtx.visitationFlags ||| flag }
    let addFlagIf (flag: VisitationFlags) (predicate: VisitationContext<'A, 'B, 'C> -> bool) (visitCtx: VisitationContext<'A, 'B, 'C>) =
        if predicate visitCtx then addFlag flag visitCtx else visitCtx
    let inline registerFlags (ctx: KeyResolutionContext) (value: VisitationContext<'A, 'B, 'C>) =
        ctx.cache.visitationFlags
        |> Dictionary.Enum.add value.masterKey value.visitationFlags
        value
        
    (* ========================================================================================
    ===========================================================================================
    What follows is the implementation of our key resolution policies and visitations.
    Each type lists the policies that apply to it, and a function that converts the
    original GlueNode into a KeyType.
    The visitor function creates a VisitationContext for the node, and then applies the
    policies listed.
    
    Extra VisitationFlags are applied depending on the node state, and then the visitation is
    finalised by calling registerFlags.
    ===========================================================================================
    =========================================================================================== *)
    module TypeReference =
        let private literalPolicy = LiteralAssociationPolicy.typeReference
        let private memberPolicy = MemberAssociationPolicy.typeReference
        let private flagInheritancePolicy = VisitationFlagInheritancePolicy.typeReference
        let make (ctx: KeyResolutionContext) (value: TsTypeReference) =
            {
                KeyTypeReference.Type =
                    value.Type
                    |> ctx.visitType
                TypeArguments =
                    value.TypeArguments
                    |> List.toArray
                    |> Array.map ctx.visitType
                ResolvedType =
                    value.ResolvedType
                    |> Option.toValueOption
                    |> ValueOption.map ctx.visitType
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createTypeReferenceKey MasterBuilder.TypeReference value 
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.HasTypeParameters (_.keyType.TypeArguments >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.IsNeverTyped (fun visitCtx ->
                visitCtx.keyType.ResolvedType
                |> ValueOption.defaultValue visitCtx.keyType.Type
                |> (=) Prelude.Primitive.Master.neverKey)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module TypeParameter =
        let literalPolicy = LiteralAssociationPolicy.typeParameter
        let memberPolicy = MemberAssociationPolicy.typeParameter
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.typeParameter
        let make (ctx: KeyResolutionContext) (value: TsTypeParameter) =
            {
                KeyTypeParameter.Name = createNameKey ctx value.Name
                Constraint =
                    value.Constraint
                    |> Option.toValueOption
                    |> ValueOption.map ctx.visitType
                Default =
                    value.Default
                    |> Option.toValueOption
                    |> ValueOption.map ctx.visitType
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createTypeParameterKey MasterBuilder.TypeParameter value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsConstrained (_.keyType.Constraint >> ValueOption.isSome)
            |> addFlagIf VisitationFlags.IsGeneric (_.keyType >> fun { Default = d; Constraint = c } -> not (d.IsSome || c.IsSome))
            |> addFlag VisitationFlags.IsTypeParameter
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Parameter =
        let literalPolicy = LiteralAssociationPolicy.parameter
        let memberPolicy = MemberAssociationPolicy.parameter
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.parameter
        let make (ctx: KeyResolutionContext) (value: TsParameter) =
            {
                KeyParameter.Name = createNameKey ctx value.Name
                Type = ctx.visitType value.Type 
                IsOptional = value.IsOptional
                IsSpread = value.IsSpread
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createParameterKey MasterBuilder.Parameter value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> addFlagIf VisitationFlags.ParamArray _.keyType.IsSpread
            |> addFlagIf VisitationFlags.MaybeParamObject (fun visitCtx ->
                visitCtx.keyType.Type
                |> Dictionary.Flip.tryItem ctx.cache.visitationFlags
                |> ValueOption.exists _.HasFlag(VisitationFlags.HasMembers)
                )
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Method =
        let private literalPolicy = LiteralAssociationPolicy.method
        let private memberPolicy = MemberAssociationPolicy.method
        let private flagInheritancePolicy = VisitationFlagInheritancePolicy.method
        let make (ctx: KeyResolutionContext) (value: TsMethod) =
            {
                KeyMethod.Name = createNameKey ctx value.Name
                Parameters =
                    value.Parameters
                    |> List.toArray
                    |> Array.map (Parameter.visit ctx >> _.typeKey)
                Type = ctx.visitType value.Type
                IsStatic = value.IsStatic
                IsOptional = value.IsOptional
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createMethodKey (MemberBuilder.Method >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module CallSignature =
        let literalPolicy = LiteralAssociationPolicy.callSignature
        let memberPolicy = MemberAssociationPolicy.callSignature
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.callSignature
        let make (ctx: KeyResolutionContext) (value: TsCallSignature) =
            {
                KeyCallSignature.Parameters =
                    value.Parameters
                    |> List.toArray
                    |> Array.map (Parameter.visit ctx >> _.typeKey)
                Type = ctx.visitType value.Type
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createCallSignatureKey (MemberBuilder.CallSignature >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module ConstructSignature =
        let literalPolicy = LiteralAssociationPolicy.constructSignature
        let memberPolicy = MemberAssociationPolicy.constructSignature
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.constructSignature
        let make (ctx: KeyResolutionContext) (value: TsConstructSignature) =
            {
                KeyConstructSignature.Parameters =
                    value.Parameters
                    |> List.toArray
                    |> Array.map (Parameter.visit ctx >> _.typeKey)
                Type = ctx.visitType value.Type
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createConstructSignatureKey (MemberBuilder.ConstructSignature >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Property =
        let literalPolicy = LiteralAssociationPolicy.property
        let memberPolicy = MemberAssociationPolicy.property
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.property
        let make (ctx: KeyResolutionContext) (value: TsProperty) =
            {
                KeyProperty.Name = createNameKey ctx value.Name
                Type = ctx.visitType value.Type
                Accessor = value.Accessor
                IsStatic = value.IsStatic
                IsPrivate = value.IsPrivate
                IsOptional = value.IsOptional
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createPropertyKey (MemberBuilder.Property >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module GetAccessor =
        let literalPolicy = LiteralAssociationPolicy.getAccessor
        let memberPolicy = MemberAssociationPolicy.getAccessor
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.getAccessor
        let make (ctx: KeyResolutionContext) (value: TsGetAccessor) =
            {
                KeyGetAccessor.Name = createNameKey ctx value.Name
                Type = ctx.visitType value.Type
                IsStatic = value.IsStatic
                IsPrivate = value.IsPrivate
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createGetAccessorKey (MemberBuilder.GetAccessor >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module SetAccessor =
        let literalPolicy = LiteralAssociationPolicy.setAccessor
        let memberPolicy = MemberAssociationPolicy.setAccessor
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.setAccessor
        let make (ctx: KeyResolutionContext) (value: TsSetAccessor) =
            {
                KeySetAccessor.Name = createNameKey ctx value.Name
                Type = ctx.visitType value.ArgumentType
                IsStatic = value.IsStatic
                IsPrivate = value.IsPrivate
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createSetAccessorKey (MemberBuilder.SetAccessor >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module IndexSignature =
        let literalPolicy = LiteralAssociationPolicy.indexSignature
        let memberPolicy = MemberAssociationPolicy.indexSignature
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.indexSignature
        let make (ctx: KeyResolutionContext) (value: TsIndexSignature) =
            {
                KeyIndexSignature.Parameters =
                    value.Parameters
                    |> List.toArray
                    |> Array.map (Parameter.visit ctx >> _.typeKey)
                Type = ctx.visitType value.Type
                IsReadonly = value.IsReadOnly
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createIndexSignatureKey (MemberBuilder.IndexSignature >> ctx.createMemberKey >> MasterBuilder.Member) value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Constructor =
        let literalPolicy = LiteralAssociationPolicy.constructor
        let memberPolicy = MemberAssociationPolicy.constructor
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.constructor
        let make (_ctx: KeyResolutionContext) (value: TsConstructor) =
            {
                KeyConstructor.Parameters =
                    value.Parameters
                    |> List.toArray
                    |> Array.map (Parameter.visit _ctx >> _.typeKey)
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createConstructorKey MasterBuilder.Constructor value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Member =
        let inline private makeMemberKey ctx (memberBuilder: MemberBuilder) =
            let memberKey = MemberKey.create memberBuilder
            (memberKey, memberBuilder)
            ||> Dictionary.Flip.tryAdd ctx.keyCache.members
            memberKey
        let visit (ctx: KeyResolutionContext) (value: TsMember) =
            match value with
            | TsMember.Method m -> Method.visit ctx m |> _.typeKey |> MemberBuilder.Method |> makeMemberKey ctx
            | TsMember.CallSignature c -> CallSignature.visit ctx c |> _.typeKey |> MemberBuilder.CallSignature |> makeMemberKey ctx
            | TsMember.ConstructSignature c -> ConstructSignature.visit ctx c |> _.typeKey |> MemberBuilder.ConstructSignature |> makeMemberKey ctx
            | TsMember.Property p -> Property.visit ctx p |> _.typeKey |> MemberBuilder.Property |> makeMemberKey ctx
            | TsMember.GetAccessor g -> GetAccessor.visit ctx g |> _.typeKey |> MemberBuilder.GetAccessor |> makeMemberKey ctx
            | TsMember.SetAccessor s -> SetAccessor.visit ctx s |> _.typeKey |> MemberBuilder.SetAccessor |> makeMemberKey ctx
            | TsMember.IndexSignature i -> IndexSignature.visit ctx i |> _.typeKey |> MemberBuilder.IndexSignature |> makeMemberKey ctx
        let tryNameKey (ctx: KeyResolutionContext) (key: MemberKey) =
            match
                ctx.cache.members
                |> Dictionary.item key
            with
            | MemberBuilder.ConstructSignature _
            | MemberBuilder.IndexSignature _
            | MemberBuilder.CallSignature _ -> ValueNone
            | MemberBuilder.Method i ->
                ctx.cache.methodKeys
                |> Dictionary.item i
                |> _.Name
                |> ValueSome
            | MemberBuilder.Property i ->
                ctx.cache.propertyKeys
                |> Dictionary.item i
                |> _.Name
                |> ValueSome
            | MemberBuilder.GetAccessor i ->
                ctx.cache.getAccessorKeys
                |> Dictionary.item i
                |> _.Name
                |> ValueSome
            | MemberBuilder.SetAccessor i ->
                ctx.cache.setAccessorKeys
                |> Dictionary.item i
                |> _.Name
                |> ValueSome

    module Function =
        let literalPolicy = LiteralAssociationPolicy.``function``
        let memberPolicy = MemberAssociationPolicy.``function``
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.``function``
        
        let make (ctx: KeyResolutionContext) (value: TsFunction) =
            {
                KeyFunction.Source = ctx.createSourceKey value
                Qualifiers = createQualifierKeys ctx value
                Name = createNameKey ctx value.Name
                Parameters = value.Parameters |> List.toArray |> Array.map (Parameter.visit ctx >> _.typeKey)
                Type = ctx.visitType value.Type
                TypeParameters = value.TypeParameters |> List.toArray |> Array.map (snd >> TypeParameter.visit ctx >> _.typeKey)
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createFunctionKey MasterBuilder.Function value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> addFlagIf VisitationFlags.HasTypeParameters (_.keyType.TypeParameters >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.IsSymbolMember (fun _ -> value.Name.StartsWith("[Symbol"))
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module TypeAlias =
        let literalPolicy = LiteralAssociationPolicy.typeAlias
        let memberPolicy = MemberAssociationPolicy.typeAlias
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.typeAlias
        let make (ctx: KeyResolutionContext) (value: TsTypeAlias) =
            {
                KeyTypeAlias.Source = ctx.createSourceKey value
                Qualifiers = createQualifierKeys ctx value
                Name = createNameKey ctx value.Name
                Type = ctx.visitType value.Type
                TypeParameters =
                    value.TypeParameters
                    |> List.toArray
                    |> Array.map (snd >> TypeParameter.visit ctx >> _.typeKey)
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createTypeAliasKey MasterBuilder.TypeAlias value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.HasTypeParameters (_.keyType.TypeParameters >> Array.isEmpty >> not)
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Enum =
        let literalPolicy = LiteralAssociationPolicy.enum
        let memberPolicy = MemberAssociationPolicy.enum
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.enum
        let make (ctx: KeyResolutionContext) (value: TsEnumType) =
            {
                KeyEnum.Name = createNameKey ctx value.Name
                Qualifiers = createQualifierKeys ctx value
                Source = ctx.createSourceKey value
                Members = value.Members |> List.toArray |> Array.map (EnumCase.visit ctx >> _.typeKey)
            }
        let visit ctx value =
            let registerMembers (visitCtx: VisitationContext<TsEnumType, KeyEnum, EnumKey>) =
                visitCtx.keyType.Members
                |> Array.iter (Dictionary.Flip.tryAddKeyPassthrough ctx.keyCache.enumCaseToEnumKeys visitCtx.typeKey >> ignore)
                visitCtx
            makeDocVisitationContext ctx make ctx.createEnumKey MasterBuilder.Enum value
            |> registerMembers
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module EnumCase =
        let literalPolicy = LiteralAssociationPolicy.enumCase
        let memberPolicy = MemberAssociationPolicy.enumCase
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.enumCase
        let make (ctx: KeyResolutionContext) (value: TsEnumCase) =
            {
                KeyEnumCase.Name = createNameKey ctx value.Name
                Value = value.Value |> ctx.createLiteralKey
            }
        let visit ctx value: VisitationContext<_, _, _> =
            makeDocVisitationContext ctx make ctx.createEnumCaseKey MasterBuilder.EnumCase value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Variable =
        let literalPolicy = LiteralAssociationPolicy.variable
        let memberPolicy = MemberAssociationPolicy.variable
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.variable
        let make (ctx: KeyResolutionContext) (value: TsVariable) =
            {
                KeyVariable.Source = ctx.createSourceKey value
                Qualifiers = createQualifierKeys ctx value
                Name = createNameKey ctx value.Name
                Type = ctx.visitType value.Type
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createVariableKey MasterBuilder.Variable value
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Interface =
        let literalPolicy = LiteralAssociationPolicy.``interface``
        let memberPolicy = MemberAssociationPolicy.``interface``
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.``interface``
        
        let make (ctx: KeyResolutionContext) (value: TsInterface) =
            {
                KeyInterface.Name = createNameKey ctx value.Name
                Qualifiers = createQualifierKeys ctx value
                Heritage =
                    value.Heritage.Extends
                    |> List.toArray
                    |> Array.map (TypeReference.visit ctx >> _.typeKey)
                Source = ctx.createSourceKey value
                TypeParameters = value.TypeParameters |> List.toArray |> Array.map (snd >> TypeParameter.visit ctx >> _.typeKey)
                Members = value.Members |> List.toArray |> Array.map (Member.visit ctx)
            }
        let visit ctx value =
            makeDocVisitationContext ctx make ctx.createInterfaceKey MasterBuilder.Interface value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.HasMembers (_.keyType.Members >> Array.isEmpty >> not) 
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.HasTypeParameters (_.keyType.TypeParameters >> Array.isEmpty >> not)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Class =
        let literalPolicy = LiteralAssociationPolicy.``class``
        let memberPolicy = MemberAssociationPolicy.``class``
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.``class``
        let make (ctx: KeyResolutionContext) (value: TsClass) =
            {
                KeyClass.Name = createNameKey ctx value.Name
                Qualifiers = createQualifierKeys ctx value
                Heritage =
                    value.Heritage.Implements
                    |> Option.toArray
                    |> Array.append (List.toArray value.Heritage.Extends)
                    |> Array.map (TypeReference.visit ctx >> _.typeKey)
                Source = ctx.createSourceKey value
                TypeParameters = value.TypeParameters |> List.toArray |> Array.map (snd >> TypeParameter.visit ctx >> _.typeKey)
                Members = value.Members |> List.toArray |> Array.map (Member.visit ctx)
                Constructors = value.Constructors |> List.toArray |> Array.map (Constructor.visit ctx >> _.typeKey)
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createClassKey MasterBuilder.Class value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.HasMembers (_.keyType.Members >> Array.isEmpty >> not)
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.HasSource (_.keyType.Source >> (<>) SourceKey.nullKey)
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlagIf VisitationFlags.HasTypeParameters (_.keyType.TypeParameters >> Array.isEmpty >> not)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Conditional =
        let literalPolicy = LiteralAssociationPolicy.conditional
        let memberPolicy = MemberAssociationPolicy.conditional
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.conditional
        let make (ctx: KeyResolutionContext) (value: TsConditionalType) =
            {
                KeyConditional.Check = ctx.visitType value.Check
                Extends = ctx.visitType value.Extends
                True = ctx.visitType value.True
                False = ctx.visitType value.False
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createConditionalKey MasterBuilder.Conditional value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Union =
        let literalPolicy = LiteralAssociationPolicy.union
        let memberPolicy = MemberAssociationPolicy.union
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.union
        let make (ctx: KeyResolutionContext) (value: TsTypeUnion) =
            { KeyUnion.Types = value.Types |> List.toArray |> Array.map ctx.visitType }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createUnionKey MasterBuilder.Union value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> fun visitCtx ->
                if visitCtx.keyType.Types
                    |> Array.forall (
                        Dictionary.Flip.tryItem ctx.cache.visitationFlags
                        >> ValueOption.exists (
                            (&&&) VisitationFlags.WouldYieldLiteral
                            >> (<>) VisitationFlags.None)
                        )
                then visitCtx.keyType.Types
                    |> Array.fold (fun acc ->
                        Dictionary.Flip.item ctx.cache.visitationFlags
                        >> (&&&) VisitationFlags.WouldYieldLiteral
                        >> fun flags -> addFlag flags acc
                        ) visitCtx
                elif visitCtx.keyType.Types
                    |> Array.forall (
                        Dictionary.Flip.tryItem ctx.cache.memberIndex
                        >> ValueOption.exists (Array.isEmpty >> not)
                        )
                then addFlag VisitationFlags.HasMembers visitCtx
                else visitCtx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Intersection =
        let literalPolicy = LiteralAssociationPolicy.intersection
        let memberPolicy = MemberAssociationPolicy.intersection
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.intersection
        let make (ctx: KeyResolutionContext) (value: TsTypeIntersection) =
            { KeyIntersection.Types = value.Types |> List.toArray |> Array.map ctx.visitType }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createIntersectionKey MasterBuilder.Intersection value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module IndexAccess =
        let literalPolicy = LiteralAssociationPolicy.indexAccess
        let memberPolicy = MemberAssociationPolicy.indexAccess
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.indexAccess
        let make (ctx: KeyResolutionContext) (value: TsIndexAccessType) =
            {
                KeyIndexAccess.Index = ctx.visitType value.Index
                Object = ctx.visitType value.Object
            }
        let tryResolveToMember (ctx: KeyResolutionContext) (keyType: KeyIndexAccess) =
            let findMemberWithNameKey nameKey =
                ctx.cache.memberIndex
                |> Dictionary.tryItem keyType.Object
                |> ValueOption.bind (
                    Array.tryFind (Member.tryNameKey ctx >> ValueOption.exists ((=) nameKey))
                    >> Option.toValueOption
                    )
            if Dictionary.Enum.has keyType.Index VisitationFlags.WouldYieldString ctx.cache.visitationFlags then
                keyType.Index
                |> ctx.cache.literalAssociations.GetLiteralKeysAsArray
                |> Array.head
                |> Dictionary.Flip.item ctx.cache.literalKeys
                |> function
                    | TsLiteral.String s ->
                        let indexKey = NameKey.createFromString s
                        findMemberWithNameKey indexKey
                    | _ -> ValueNone
            else ValueNone
            
        let visit ctx value =
            makeVisitationContext ctx make ctx.createIndexAccessKey MasterBuilder.IndexAccess value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
                
        let resolve ctx = visit ctx >> _.masterKey
    module ModuleDeclaration =
        let literalPolicy = LiteralAssociationPolicy.moduleDeclaration
        let memberPolicy = MemberAssociationPolicy.moduleDeclaration
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.moduleDeclaration
        let make (ctx: KeyResolutionContext) (value: TsModule) =
            {
                KeyModule.Source = ctx.createSourceKey value
                Qualifiers = createQualifierKeys ctx value
                Types = value.Types |> List.toArray |> Array.map ctx.visitType
                Name = createNameKey ctx value.Name
                IsNamespace = value.IsNamespace
                IsRecursive = value.IsRecursive
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createModuleKey MasterBuilder.Module value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlag VisitationFlags.HasName
            |> addFlagIf VisitationFlags.IsFullyQualified (_.keyType.Qualifiers >> Array.isEmpty >> not)
            |> addFlag VisitationFlags.HasSource
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Tuple =
        let literalPolicy = LiteralAssociationPolicy.tuple
        let memberPolicy = MemberAssociationPolicy.tuple
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.tuple
        let make (ctx: KeyResolutionContext) (value: TsTuple) =
            {
                KeyTuple.Elements = value.Types |> List.toArray |> Array.map (TupleElement.visit ctx >> _.typeKey)
                MinRequired = value.MinRequired
                FixedLength = value.FixedLength
                IsReadOnly = value.IsReadOnly
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createTupleKey MasterBuilder.Tuple value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module TupleElement =
        let literalPolicy = LiteralAssociationPolicy.tupleElement
        let memberPolicy = MemberAssociationPolicy.tupleElement
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.tupleElement
        let make (ctx: KeyResolutionContext) (value: TsTupleElement) =
            match value with
            | TsTupleElement.FixedLabeled (name, element) ->
                {
                    KeyTupleElement.Name = createNameKey ctx name |> ValueSome
                    Type = ctx.visitType element.Type
                    IsOptional = element.IsOptional
                    IsRest = element.IsRest
                    IsVariadic = false
                }
            | TsTupleElement.Fixed element ->
                {
                    KeyTupleElement.Name = ValueNone
                    Type = ctx.visitType element.Type
                    IsOptional = element.IsOptional
                    IsRest = element.IsRest
                    IsVariadic = false
                }
            | TsTupleElement.Variadic typeKey ->
                {
                    KeyTupleElement.Name = ValueNone
                    Type = ctx.visitType typeKey
                    IsOptional = false
                    IsRest = false
                    IsVariadic = true
                }
        let visit ctx value: VisitationContext<_, _, _> =
            makeVisitationContext ctx make ctx.createTupleElementKey MasterBuilder.TupleElement value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> addFlagIf VisitationFlags.MaybeParamObject (
                _.keyType.Type
                >> Dictionary.Flip.tryItem ctx.cache.memberIndex
                >> ValueOption.exists (Array.isEmpty >> not)
                )
            |> addFlagIf VisitationFlags.ParamArray (_.keyType >> fun { IsRest = isRest; IsVariadic = isVariadic } -> isRest || isVariadic)
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Index =
        let literalPolicy = LiteralAssociationPolicy.index
        let memberPolicy = MemberAssociationPolicy.index
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.index
        let make (ctx: KeyResolutionContext) (value: TsIndex) =
            { KeyIndex.Type = ctx.visitType value.Type }
        let visit ctx value=
            makeVisitationContext ctx make ctx.createIndexKey MasterBuilder.Index value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
 
        let resolve ctx = visit ctx >> _.masterKey
    module Predicate =
        let literalPolicy = LiteralAssociationPolicy.predicate
        let memberPolicy = MemberAssociationPolicy.predicate
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.predicate
        let make (ctx: KeyResolutionContext) (value: TsTypePredicate) =
            {
                KeyPredicate.Type = ctx.visitType value.Type
                IsAssertion = value.IsAssertion
            }
        let visit ctx value =
            makeVisitationContext ctx make ctx.createPredicateKey MasterBuilder.Predicate value
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module TypeLiteral =
        let literalPolicy = LiteralAssociationPolicy.typeLiteral
        let memberPolicy = MemberAssociationPolicy.typeLiteral
        let flagInheritancePolicy = VisitationFlagInheritancePolicy.typeLiteral
        let make (ctx: KeyResolutionContext) (value: TsTypeLiteral) =
            { KeyTypeLiteral.Members = value.Members |> List.toArray |> Array.map (Member.visit ctx) }
        let visit ctx value =
            let visitCtx = makeVisitationContext ctx make ctx.createTypeLiteralKey MasterBuilder.TypeLiteral value
            visitCtx
            |> LiteralAssociationPolicy.apply literalPolicy ctx
            |> MemberAssociationPolicy.apply memberPolicy ctx
            |> VisitationFlagInheritancePolicy.apply flagInheritancePolicy ctx
            |> registerFlags ctx
        let resolve ctx = visit ctx >> _.masterKey
    module Literal =
        let visit (ctx: KeyResolutionContext) (value: TsLiteral) =
            let literalKey = ctx.createLiteralKey value
            let masterKey = MasterBuilder.Literal literalKey |> ctx.createMasterKey
            match value with
            | TsLiteral.String _ -> ctx.cache.visitationFlags |> Dictionary.Enum.add masterKey VisitationFlags.WouldYieldString
            | TsLiteral.BigInt _
            | TsLiteral.Int _ -> ctx.cache.visitationFlags |> Dictionary.Enum.add masterKey VisitationFlags.WouldYieldInt
            | TsLiteral.Float _ -> ctx.cache.visitationFlags |> Dictionary.Enum.add masterKey VisitationFlags.WouldYieldFloat
            | TsLiteral.Bool _ -> ctx.cache.visitationFlags |> Dictionary.Enum.add masterKey VisitationFlags.WouldYieldBool
            | TsLiteral.Null -> ctx.cache.visitationFlags |> Dictionary.Enum.add masterKey VisitationFlags.WouldYieldNull
            ctx.cache.literalAssociations.RegisterLiteralsKeyPassthrough
                [ literalKey ]
                masterKey
        let resolve = visit
    module Primitive =
        let visit (_ctx: KeyResolutionContext) (value: TypeKindPrimitive) =
            PrimitiveKey.create value
            |> MasterBuilder.Primitive
            |> MasterKey.create
        let resolve = visit
    let glueTypeVisitor (ctx: KeyResolutionContext) glueType =
        match glueType with
        | TsType.Literal l -> Literal.resolve ctx l
        | TsType.GlobalThis -> GlobalKey.create()
        | TsType.Conditional glueConditionalType -> Conditional.resolve ctx glueConditionalType
        | TsType.Interface glueInterface -> Interface.resolve ctx glueInterface
        | TsType.Class glueClass -> Class.resolve ctx glueClass
        | TsType.Variable glueVariable -> Variable.resolve ctx glueVariable
        | TsType.Primitive typeKindPrimitive -> Primitive.resolve ctx typeKindPrimitive
        | TsType.Enum glueEnumType -> Enum.resolve ctx glueEnumType
        | TsType.EnumCase glueEnumCase -> EnumCase.resolve ctx glueEnumCase
        | TsType.TypeAlias glueTypeAlias -> TypeAlias.resolve ctx glueTypeAlias
        | TsType.Function glueFunctionDeclaration -> Function.resolve ctx glueFunctionDeclaration
        | TsType.Union glueTypeUnion -> Union.resolve ctx glueTypeUnion
        | TsType.Intersection glueTypeIntersection -> Intersection.resolve ctx glueTypeIntersection 
        | TsType.IndexedAccess glueIndexAccessType -> IndexAccess.resolve ctx glueIndexAccessType 
        | TsType.Module glueModuleDeclaration -> ModuleDeclaration.resolve ctx glueModuleDeclaration 
        | TsType.TypeReference glueTypeReference -> TypeReference.resolve ctx glueTypeReference 
        | TsType.Array glueType ->
            glueTypeVisitor ctx glueType |> MasterBuilder.Array |> ctx.createMasterKey
        | TsType.TypeParameter glueTypeParameter -> TypeParameter.resolve ctx glueTypeParameter 
        | TsType.ReadOnly glueType -> glueTypeVisitor ctx glueType 
        | TsType.Tuple glueTuple -> Tuple.resolve ctx glueTuple 
        | TsType.Index glueIndex -> Index.resolve ctx glueIndex 
        | TsType.Predicate glueTypePredicate -> Predicate.resolve ctx glueTypePredicate 
        | TsType.TypeLiteral glueTypeLiteral -> TypeLiteral.resolve ctx glueTypeLiteral
        | TsType.TemplateLiteral tsTemplateLiteralType -> failwith "todo"
        | TsType.Optional tsTypeReference -> TypeReference.resolve ctx tsTypeReference

    let nodeStoreVisitor (ctx: KeyResolutionContext) nodeStore =
        match nodeStore with
        | NodeStore.CallSignature callSignature -> CallSignature.resolve ctx callSignature
        | NodeStore.Property glueProperty -> Property.resolve ctx glueProperty
        | NodeStore.Parameter glueParameter -> Parameter.resolve ctx glueParameter
        | NodeStore.Method glueMethod -> Method.resolve ctx glueMethod
        | NodeStore.Constructor glueConstructor -> Constructor.resolve ctx glueConstructor
        | NodeStore.ConstructSignature glueConstruct -> ConstructSignature.resolve ctx glueConstruct
        | NodeStore.IndexSignature glueIndexSignature -> IndexSignature.resolve ctx glueIndexSignature
        | NodeStore.GetAccessor glueGetAccessor -> GetAccessor.resolve ctx glueGetAccessor
        | NodeStore.SetAccessor glueSetAccessor -> SetAccessor.resolve ctx glueSetAccessor
        | NodeStore.SubstitutionType glueSubstitutionType -> failwith "todo"

    let visitor (ctx: KeyResolutionContext) (result: Result<TsType, NodeStore>) =
        match result with
        | Ok glueType -> glueTypeVisitor ctx glueType
        | Error nodeStore -> nodeStoreVisitor ctx nodeStore

module Resolve =
    module SRTP =
        module Name =
            type HasNameKey<^KeyType when ^KeyType:(member Name: NameKey)> = ^KeyType
            type MaybeHasNameKey<^KeyType when ^KeyType:(member Name: NameKey voption)> = ^KeyType
            let inline tryGetName<^KeyType, ^TypeKey when HasNameKey<^KeyType>>
                ([<InlineIfLambda>] keyTypeDictMapping: KeyCache -> Dictionary<^TypeKey, ^KeyType>)
                (ctx: KeyResolutionContext)  =
                Dictionary.Flip.tryItem (keyTypeDictMapping ctx.cache)
                >> ValueOption.bind (
                    _.Name
                    >> Dictionary.Flip.tryItem ctx.cache.nameKeys
                    )
            let inline tryGetMaybeName<^KeyType, ^TypeKey when MaybeHasNameKey<^KeyType>>
                ([<InlineIfLambda>] keyTypeDictMapping: KeyCache -> Dictionary<^TypeKey, ^KeyType>)
                (ctx: KeyResolutionContext)  =
                Dictionary.Flip.tryItem (keyTypeDictMapping ctx.cache)
                >> ValueOption.bind _.Name
                >> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.nameKeys)
    let tryGetMasterKey (ctx: KeyResolutionContext) (key: TypeKey) =
        let hashKey = HashTypeKey.create key
        ctx.cache.typeToMasterKeys
        |> Dictionary.tryItem hashKey
    let tryGetMasterBuilder (ctx: KeyResolutionContext) =
        tryGetMasterKey ctx
        >> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
    /// Identical to <c>tryGetMasterKey</c> but will resolve
    /// <c>MasterBuilder.TypeKey</c>, <c>MasterBuilder.Cyclic</c> and <c>MasterBuilder.HashType</c>
    /// recursively.
    let rec tryResolveMasterBuilder (ctx: KeyResolutionContext) =
        let rec resolve ctx: MasterBuilder -> MasterBuilder voption = function
            | MasterBuilder.TypeKey typeKey ->
                tryResolveMasterBuilder ctx typeKey
            | MasterBuilder.Cyclic cyclicKey ->
                cyclicKey
                |> MasterBuilder.Cyclic
                |> MasterKey.create
                |> Dictionary.Flip.tryItem ctx.cache.cyclicRemaps
                |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
                |> ValueOption.bind (resolve ctx)
            | MasterBuilder.HashType hashTypeKey ->
                Dictionary.tryItem hashTypeKey ctx.cache.typeToMasterKeys
                |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
                |> ValueOption.bind (resolve ctx)
            | passThrough -> ValueSome passThrough
        tryGetMasterBuilder ctx
        >> ValueOption.bind (resolve ctx)
    let tryGetDocumentation (ctx: KeyResolutionContext) (key: TypeKey) =
        tryGetMasterKey ctx key
        |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.documentation)
    let tryGetName (ctx: KeyResolutionContext) =
        let inline getNameFrom key mapping = SRTP.Name.tryGetName mapping ctx key
        let inline getMaybeNameFrom key mapping = SRTP.Name.tryGetMaybeName mapping ctx key
        tryGetMasterKey ctx
        >> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
        >> ValueOption.bind (
            let rec impl = function
                | MasterBuilder.Class c -> getNameFrom c _.classKeys
                | MasterBuilder.TypeKey i -> tryGetName ctx i
                | MasterBuilder.Name i -> Dictionary.tryItem i ctx.cache.nameKeys
                | MasterBuilder.Parameter i -> getNameFrom i _.parameterKeys
                | MasterBuilder.TypeParameter i -> getNameFrom i _.typeParameterKeys
                | MasterBuilder.EnumCase i -> getNameFrom i _.enumCaseKeys
                | MasterBuilder.Enum i -> getNameFrom i _.enumKeys
                | MasterBuilder.Interface i -> getNameFrom i _.interfaceKeys
                | MasterBuilder.Member i ->
                    Dictionary.tryItem i ctx.cache.members
                    |> ValueOption.bind (function
                        | MemberBuilder.Property p -> getNameFrom p _.propertyKeys
                        | MemberBuilder.Method i -> getNameFrom i _.methodKeys
                        | MemberBuilder.GetAccessor i -> getNameFrom i _.getAccessorKeys
                        | MemberBuilder.SetAccessor i -> getNameFrom i _.setAccessorKeys
                        | MemberBuilder.CallSignature _ | MemberBuilder.IndexSignature _
                        | MemberBuilder.ConstructSignature _ -> ValueNone)
                | MasterBuilder.HashType i ->
                    Dictionary.tryItem i ctx.cache.typeToMasterKeys
                    |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
                    |> ValueOption.bind impl
                | MasterBuilder.TupleElement i ->
                    getMaybeNameFrom i _.tupleElementKeys
                | MasterBuilder.Variable i -> getNameFrom i _.variableKeys
                | MasterBuilder.Function i -> getNameFrom i _.functionKeys
                | MasterBuilder.Module i -> getNameFrom i _.moduleKeys
                | MasterBuilder.TypeAlias i -> getNameFrom i _.typeAliasKeys
                | MasterBuilder.Cyclic i ->
                    CyclicKey.toHashTypeKey i
                    |> Dictionary.Flip.tryItem ctx.cache.typeToMasterKeys
                    |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.masters)
                    |> ValueOption.bind impl
                | MasterBuilder.Conditional _
                | MasterBuilder.Constructor _ | MasterBuilder.TypeLiteral _
                | MasterBuilder.IndexAccess _ | MasterBuilder.Index _
                | MasterBuilder.Tuple _ | MasterBuilder.Union _
                | MasterBuilder.Intersection _ | MasterBuilder.Predicate _
                | MasterBuilder.Array _ | MasterBuilder.Source _
                | MasterBuilder.Literal _ | MasterBuilder.Primitive _
                | MasterBuilder.Documentation _ | MasterBuilder.Global 
                | MasterBuilder.TypeReference _ -> ValueNone
            impl)
            
[<AutoOpen>]
module KeyCacheExtensions =
    type KeyCache with
        member inline this.Item(key: ClassKey) = Dictionary.item key this.classKeys
        member inline this.Item(key: EnumCaseKey) = Dictionary.item key this.enumCaseKeys
        member inline this.Item(key: EnumKey) = Dictionary.item key this.enumKeys
        member inline this.Item(key: FunctionKey) = Dictionary.item key this.functionKeys
        member inline this.Item(key: PropertyKey) = Dictionary.item key this.propertyKeys
        member inline this.Item(key: MethodKey) = Dictionary.item key this.methodKeys
        member inline this.Item(key: GetAccessorKey) = Dictionary.item key this.getAccessorKeys
        member inline this.Item(key: SetAccessorKey) = Dictionary.item key this.setAccessorKeys
        member inline this.Item(key: TypeParameterKey) = Dictionary.item key this.typeParameterKeys
        member inline this.Item(key: VariableKey) = Dictionary.item key this.variableKeys
        member inline this.Item(key: ModuleKey) = Dictionary.item key this.moduleKeys
        member inline this.Item(key: TypeAliasKey) = Dictionary.item key this.typeAliasKeys
        member inline this.Item(key: IndexSignatureKey) = Dictionary.item key this.indexSignatureKeys
        member inline this.Item(key: CallSignatureKey) = Dictionary.item key this.callSignatureKeys
        member inline this.Item(key: ConstructSignatureKey) = Dictionary.item key this.constructSignatureKeys
        member inline this.Item(key: ParameterKey) = Dictionary.item key this.parameterKeys
        member inline this.Item(key: InterfaceKey) = Dictionary.item key this.interfaceKeys
        member inline this.Item(key: ConditionalKey) = Dictionary.item key this.conditionalKeys
        member inline this.Item(key: MemberKey) = Dictionary.item key this.members
        member inline this.Item(key: TypeReferenceKey) = Dictionary.item key this.typeReferenceKeys
        member inline this.Item(key: IndexKey) = Dictionary.item key this.indexKeys
        member inline this.Item(key: NameKey) = Dictionary.item key this.nameKeys
        member inline this.Item(key: PredicateKey) = Dictionary.item key this.predicateKeys
        member inline this.Item(key: TypeLiteralKey) = Dictionary.item key this.typeLiteralKeys
        member inline this.Item(key: TupleElementKey) = Dictionary.item key this.tupleElementKeys
        member inline this.Item(key: TupleKey) = Dictionary.item key this.tupleKeys
        member inline this.Item(key: UnionKey) = Dictionary.item key this.unionKeys
        member inline this.Item(key: IntersectionKey) = Dictionary.item key this.intersectionKeys
        member inline this.Item(key: IndexAccessKey) = Dictionary.item key this.indexAccessKeys
        member inline this.Item(key: HashTypeKey) = Dictionary.item key this.typeToMasterKeys
        member inline this.Item(key: MasterKey) = Dictionary.item key this.masters
        member inline this.Item(key: LiteralKey) = Dictionary.item key this.literalKeys
            
[<AutoOpen>]
module KeyResolutionContextExtensions =
    type KeyResolutionContext with
        member this.Item(key: LiteralKey) = this.cache[key]
        member this.Item(key: MasterKey) = this.cache[key]
        member this.Item(key: HashTypeKey) = this.cache[key]
        member this.Item(key: ClassKey) = this.cache[key]
        member this.Item(key: EnumCaseKey) = this.cache[key]
        member this.Item(key: EnumKey) = this.cache[key]
        member this.Item(key: FunctionKey) = this.cache[key]
        member this.Item(key: PropertyKey) = this.cache[key]
        member this.Item(key: MethodKey) = this.cache[key]
        member this.Item(key: GetAccessorKey) = this.cache[key]
        member this.Item(key: SetAccessorKey) = this.cache[key]
        member this.Item(key: TypeParameterKey) = this.cache[key]
        member this.Item(key: VariableKey) = this.cache[key]
        member this.Item(key: ModuleKey) = this.cache[key]
        member this.Item(key: TypeAliasKey) = this.cache[key]
        member this.Item(key: IndexSignatureKey) = this.cache[key]
        member this.Item(key: CallSignatureKey) = this.cache[key]
        member this.Item(key: ConstructSignatureKey) = this.cache[key]
        member this.Item(key: ParameterKey) = this.cache[key]
        member this.Item(key: InterfaceKey) = this.cache[key]
        member this.Item(key: ConditionalKey) = this.cache[key]
        member this.Item(key: MemberKey) = this.cache[key]
        member this.Item(key: TypeReferenceKey) = this.cache[key]
        member this.Item(key: IndexKey) = this.cache[key]
        member this.Item(key: NameKey) = this.cache[key]
        member this.Item(key: PredicateKey) = this.cache[key]
        member this.Item(key: TypeLiteralKey) = this.cache[key]
        member this.Item(key: TupleElementKey) = this.cache[key]
        member this.Item(key: TupleKey) = this.cache[key]
        member this.Item(key: UnionKey) = this.cache[key]
        member this.Item(key: IntersectionKey) = this.cache[key]
        member this.Item(key: IndexAccessKey) = this.cache[key]
 
        static member Create(
            getGlueFun: TypeKey -> Result<TsType, NodeStore>,
            ?logger: ILogger,
            ?resolveTypeFun: KeyResolutionContext -> Result<TsType, NodeStore> -> MasterKey,
            ?keyCache: KeyCache
            ) =
            // IMPORTANT
            // Ensure primitive types are registered and other known types are added to the cache
            let keyCache = defaultArg keyCache KeyCache.Empty
            [
                LiteralKey.create (TsLiteral.Int 128), TsLiteral.Int 128
                LiteralKey.create (TsLiteral.Int 129), TsLiteral.Int 129
                Prelude.Literal.falseKey, TsLiteral.Bool false
                Prelude.Literal.trueKey, TsLiteral.Bool true
                Prelude.Literal.nullKey, TsLiteral.Null
            ]
            |> List.iter (fun (key, literal) ->
                Dictionary.tryAdd key literal keyCache.literalKeys
                let builder = MasterBuilder.Literal key
                let key = builder |> MasterKey.create
                Dictionary.tryAdd key builder keyCache.masters
                )
            [
                Prelude.Primitive.anyKey, Prelude.Primitive.Master.anyKey
                Prelude.Primitive.unknownKey, Prelude.Primitive.Master.unknownKey
                Prelude.Primitive.neverKey, Prelude.Primitive.Master.neverKey 
                Prelude.Primitive.voidKey, Prelude.Primitive.Master.voidKey 
                Prelude.Primitive.undefinedKey, Prelude.Primitive.Master.undefinedKey 
                Prelude.Primitive.nullKey, Prelude.Primitive.Master.nullKey 
                Prelude.Primitive.stringKey, Prelude.Primitive.Master.stringKey 
                Prelude.Primitive.integerKey, Prelude.Primitive.Master.integerKey 
                Prelude.Primitive.numberKey, Prelude.Primitive.Master.numberKey 
                Prelude.Primitive.booleanKey, Prelude.Primitive.Master.booleanKey 
                Prelude.Primitive.bigIntKey, Prelude.Primitive.Master.bigIntKey 
                Prelude.Primitive.esSymbolKey, Prelude.Primitive.Master.esSymbolKey 
                Prelude.Primitive.nonPrimitiveKey, Prelude.Primitive.Master.nonPrimitiveKey 
            ]
            |> List.iter (fun (key, masterKey) ->
                let builder = MasterBuilder.Primitive key
                Dictionary.tryAdd masterKey builder keyCache.masters
                )
            [
                Prelude.Union.variant1, Prelude.Union.Master.variant1
                Prelude.Union.variant2, Prelude.Union.Master.variant2
            ]
            |> List.iter (fun (key, masterKey) ->
                let primitiveKey =
                    Prelude.Primitive.booleanKey
                    |> MasterBuilder.Primitive
                Dictionary.tryAdd masterKey primitiveKey keyCache.masters
                )
            [
                Prelude.Name.moduleMagicKey, NameKey.moduleMagicKeyString
                Prelude.Name.typarTransientKey, NameKey.typarTransientString
            ]
            |> List.iter (fun (key, name) ->
                Dictionary.tryAdd key name keyCache.nameKeys)
            let resolveTypeFun = defaultArg resolveTypeFun KeyResolution.visitor
            #if DEBUG
            // In debug mode we will default to a console logger.
            let logger = defaultArg logger (
                LoggerFactory.Create(fun builder ->
                    builder.AddConsole().SetMinimumLevel(LogLevel.Information)
                    |> ignore).CreateLogger()
                )
            #else
            // In release mode, we will default to a no-op logger.
            let logger = defaultArg logger Abstractions.NullLogger.Instance
            #endif
            {
                keyCache = keyCache
                getGlueFun = getGlueFun
                resolveTypeFun = resolveTypeFun
                logger = logger
            }

        static member inline CreateAndResolve(xanthamTree: XanthamTree) =
            let ctx = KeyResolutionContext.Create(xanthamTree.TryGetTypeForKey)
            for key in xanthamTree.ExportsDict.Keys.AsSpan() do
                ctx.visitType key
                |> ignore
            for key in xanthamTree.TypeDict.Keys.AsSpan() do
                ctx.visitType key
                |> ignore
            for key in xanthamTree.LibSet do
                HashTypeKey.create key
                |> Dictionary.Flip.tryItem ctx.cache.typeToMasterKeys
                |> ValueOption.iter (fun masterKey ->
                    ctx.cache.visitationFlags
                    |> Dictionary.Enum.add masterKey VisitationFlags.IsEsLib
                    )
            ctx

