// ReSharper disable FSharpInterpolatedString

namespace Xantham.SimpleGenerator

open System.Collections.Frozen
open System.Collections.Generic
open Microsoft.Extensions.Logging
open Xantham
open Xantham.Decoder
open FSharp.Logf
open Xantham.SimpleGenerator.KeyNodeHashing

type VisitationFlags =
    | None = 0
    | WouldYieldInt = ((<<<) 1 0)
    | WouldYieldBool = ((<<<) 1 1)
    | WouldYieldNull = ((<<<) 1 2)
    | WouldYieldString = ((<<<) 1 3)
    | WouldYieldFloat = ((<<<) 1 4)
    | MaybeParamObject  = ((<<<) 1 5)
    | ParamArray  = ((<<<) 1 6)
    | IsObsolete  = ((<<<) 1 7)
    | HasSource = ((<<<) 1 8)
    | HasMembers = ((<<<) 1 9)
    | HasName = ((<<<) 1 10)
    | HasTypeParameters = ((<<<) 1 11)
    | IsTypeParameter = ((<<<) 1 12)
    | IsGeneric = ((<<<) 1 13)
    | IsConstrained = ((<<<) 1 14)
    | IsSymbolMember = ((<<<) 1 15)
    | IsFullyQualified = ((<<<) 1 16)
    | IsEsLib = ((<<<) 1 17)
    | IsNeverTyped = ((<<<) 1 18)
    | HasDocs = (1 <<< 19)
    | ContainsCyclicReference = (1 <<< 20)
module VisitationFlags =
    [<Literal>]
    let HasDocumentation = VisitationFlags.IsObsolete ||| VisitationFlags.HasDocs
    [<Literal>]
    let WouldYieldLiteral: VisitationFlags =
        VisitationFlags.WouldYieldInt
        ||| VisitationFlags.WouldYieldBool
        ||| VisitationFlags.WouldYieldNull
        ||| VisitationFlags.WouldYieldString
        ||| VisitationFlags.WouldYieldFloat
    [<Literal>]
    let RequiresAttributes =
        VisitationFlags.ParamArray
        ||| VisitationFlags.MaybeParamObject
        ||| VisitationFlags.IsObsolete
        
    let toStringArray (flags: VisitationFlags) =
        [|
            if flags.HasFlag(VisitationFlags.WouldYieldInt) then nameof VisitationFlags.WouldYieldInt 
            if flags.HasFlag(VisitationFlags.WouldYieldBool) then nameof VisitationFlags.WouldYieldBool 
            if flags.HasFlag(VisitationFlags.WouldYieldNull) then nameof VisitationFlags.WouldYieldNull 
            if flags.HasFlag(VisitationFlags.WouldYieldString) then nameof VisitationFlags.WouldYieldString 
            if flags.HasFlag(VisitationFlags.WouldYieldFloat) then nameof VisitationFlags.WouldYieldFloat 
            if flags.HasFlag(WouldYieldLiteral) then nameof WouldYieldLiteral
            if flags.HasFlag(VisitationFlags.MaybeParamObject ) then nameof VisitationFlags.MaybeParamObject  
            if flags.HasFlag(VisitationFlags.ParamArray ) then nameof VisitationFlags.ParamArray  
            if flags.HasFlag(VisitationFlags.IsObsolete ) then nameof VisitationFlags.IsObsolete  
            if flags.HasFlag(VisitationFlags.HasSource) then nameof VisitationFlags.HasSource 
            if flags.HasFlag(VisitationFlags.HasMembers) then nameof VisitationFlags.HasMembers 
            if flags.HasFlag(VisitationFlags.HasName) then nameof VisitationFlags.HasName 
            if flags.HasFlag(VisitationFlags.HasTypeParameters) then nameof VisitationFlags.HasTypeParameters
            if flags.HasFlag(VisitationFlags.IsTypeParameter) then nameof VisitationFlags.IsTypeParameter
            if flags.HasFlag(VisitationFlags.IsGeneric) then nameof VisitationFlags.IsGeneric
            if flags.HasFlag(RequiresAttributes) then nameof RequiresAttributes
            if flags.HasFlag(HasDocumentation) then nameof HasDocumentation
            if flags.HasFlag(VisitationFlags.IsSymbolMember) then nameof VisitationFlags.IsSymbolMember
            if flags.HasFlag(VisitationFlags.IsFullyQualified) then nameof VisitationFlags.IsFullyQualified
            if flags.HasFlag(VisitationFlags.IsEsLib) then nameof VisitationFlags.IsEsLib
            if flags.HasFlag(VisitationFlags.IsNeverTyped) then nameof VisitationFlags.IsNeverTyped
            if flags.HasFlag(VisitationFlags.ContainsCyclicReference) then nameof VisitationFlags.ContainsCyclicReference
        |]
[<AutoOpen>]
module VisitationFlagDictionaryExtensions =
    module Dictionary =
        module Enum =
            let inline has (key: MasterKey) (value: VisitationFlags) (dict: Dictionary<MasterKey, VisitationFlags>) =
                Dictionary.tryItem key dict
                |> ValueOption.exists _.HasFlag(value)
            let inline hasMask (key: MasterKey) (mask: VisitationFlags) (dict: Dictionary<MasterKey, VisitationFlags>) =
                Dictionary.tryItem key dict
                |> ValueOption.exists ((&&&) mask >> int >> (<>) 0)
            let inline add (key: MasterKey) (value: VisitationFlags) (dict: Dictionary<MasterKey, VisitationFlags>) =
                let addResult: VisitationFlags = Dictionary.tryAddOrGet key value dict
                if addResult <> value then
                    dict[key] <- addResult ||| value
            let inline remove (key: MasterKey) (value: VisitationFlags) (dict: Dictionary<MasterKey, VisitationFlags>) =
                dict
                |> Dictionary.tryItem key
                |> ValueOption.iter (fun flags ->
                    dict[key] <- flags ^^^ (flags &&& value))
            let inline set (key: MasterKey) (value: VisitationFlags) (dict: Dictionary<MasterKey, VisitationFlags>) =
                dict
                |> Dictionary.tryItem key
                |> function
                    | ValueSome _ -> dict[key] <- value
                    | ValueNone -> dict.Add(key, value)
        module Flip =
            module Enum =
                let inline has dict key value = Enum.has key value dict
                let inline add dict key value = Enum.add key value dict
                let inline remove dict key value = Enum.remove key value dict
                let inline set dict key value = Enum.set key value dict

/// <summary>
/// Simple dictionary abstraction that tracks what types are associated with what literals
/// </summary>
type LiteralAssociations(ref: Dictionary<LiteralKey, TsLiteral>) =
    let literalAssociations = Dictionary<MasterKey, LiteralKey array>()
    let keyAssociations = Dictionary<MasterKey, MasterKey array>()
    let getKeyAssociations (masterKey: MasterKey) =
        Dictionary.tryItem masterKey keyAssociations
        |> ValueOption.defaultValue [||]
    let getLiteralKeys (masterKey: MasterKey) =
        Dictionary.tryItem masterKey literalAssociations
        |> ValueOption.defaultValue [||]
    let rec collectAssociations (masterKey: MasterKey) =
        [|
            masterKey
            yield!
                getKeyAssociations masterKey
                |> Array.collect collectAssociations
        |]
    let getAllAssociatedLiteralKeys (masterKey: MasterKey) =
        collectAssociations masterKey
        |> Array.collect getLiteralKeys
        |> ValueSome
        |> ValueOption.filter (Array.isEmpty >> not)
    let createLiteralAssociationEntry (masterKey: MasterKey) (literals: LiteralKey seq) =
        if Seq.isEmpty literals then ()
        else
            literals
            |> Seq.toArray
            |> Dictionary.Flip.tryAdd literalAssociations masterKey
    let createKeyAssociationEntry (masterKey: MasterKey) (associatedKeys: MasterKey seq) =
        associatedKeys
        |> Seq.filter ((<>) masterKey)
        |> Seq.filter (fun key ->
            // no point adding an association if the
            // keys aren't associated with literals themselves
            literalAssociations.ContainsKey key 
            || keyAssociations.ContainsKey key
            )
        |> ValueSome
        |> ValueOption.filter (Seq.isEmpty >> not)
        |> ValueOption.iter (Seq.toArray >> Dictionary.Flip.tryAdd keyAssociations masterKey)
    member this.RegisterLiterals (key: MasterKey) (literals: LiteralKey seq) =
        createLiteralAssociationEntry key literals
        literals
    member this.RegisterLiteralsKeyPassthrough (literalKeys: LiteralKey seq) (key: MasterKey) =
        createLiteralAssociationEntry key literalKeys
        key
    member this.RegisterKeysAndLiterals (key: MasterKey) (associatedKeys: MasterKey seq) (literals: LiteralKey seq) =
        createLiteralAssociationEntry key literals
        createKeyAssociationEntry key associatedKeys
        literals
    member this.RegisterAssociations (key: MasterKey) (associatedKeys: MasterKey seq) =
        createKeyAssociationEntry key associatedKeys
    member this.RegisterAssociationsKeyPassthrough (associatedKeys: MasterKey seq) (key: MasterKey) =
        createKeyAssociationEntry key associatedKeys
        key
    member this.GetLiteralKeys (key: MasterKey) = getAllAssociatedLiteralKeys key
    member this.GetLiteralKeysAsArray (key: MasterKey) =
        getAllAssociatedLiteralKeys key
        |> ValueOption.defaultValue Array.empty
    member this.ResolveLiterals (key: MasterKey) =
        this.GetLiteralKeysAsArray key
        |> Array.map (fun key -> ref.Item key)
    member this.ToFrozenDictionary()=
        struct {|
             Literals = ref.ToFrozenDictionary()
             LiteralAssociations = literalAssociations.ToFrozenDictionary()
             KeyAssociations = keyAssociations.ToFrozenDictionary()
         |}
    
type KeyCache = {
    documentation: Dictionary<MasterKey, TsComment array>
    visitationFlags: Dictionary<MasterKey, VisitationFlags>
    cycles: HashSet<CyclicKey>
    sourceKeys: Dictionary<SourceKey, string>
    cyclicRemaps: Dictionary<MasterKey, MasterKey>
    masters: Dictionary<MasterKey, MasterBuilder>
    members: Dictionary<MemberKey, MemberBuilder>
    memberIndex: Dictionary<MasterKey, MemberKey array>
    seenKeys: HashSet<HashTypeKey>
    nameKeys: Dictionary<NameKey, string>
    typeToMasterKeys: Dictionary<HashTypeKey, MasterKey>
    parameterKeys: Dictionary<ParameterKey, KeyParameter>
    typeParameterKeys: Dictionary<TypeParameterKey, KeyTypeParameter>
    typeReferenceKeys: Dictionary<TypeReferenceKey, KeyTypeReference>
    methodKeys: Dictionary<MethodKey, KeyMethod>
    callSignatureKeys: Dictionary<CallSignatureKey, KeyCallSignature>
    constructSignatureKeys: Dictionary<ConstructSignatureKey, KeyConstructSignature>
    propertyKeys: Dictionary<PropertyKey, KeyProperty>
    getAccessorKeys: Dictionary<GetAccessorKey, KeyGetAccessor>
    setAccessorKeys: Dictionary<SetAccessorKey, KeySetAccessor>
    indexSignatureKeys: Dictionary<IndexSignatureKey, KeyIndexSignature>
    constructorKeys: Dictionary<ConstructorKey, KeyConstructor>
    functionKeys: Dictionary<FunctionKey, KeyFunction>
    typeAliasKeys: Dictionary<TypeAliasKey, KeyTypeAlias>
    enumKeys: Dictionary<EnumKey, KeyEnum>
    enumCaseKeys: Dictionary<EnumCaseKey, KeyEnumCase>
    enumCaseToEnumKeys: Dictionary<EnumCaseKey, EnumKey>
    variableKeys: Dictionary<VariableKey, KeyVariable>
    interfaceKeys: Dictionary<InterfaceKey, KeyInterface>
    classKeys: Dictionary<ClassKey, KeyClass>
    conditionalKeys: Dictionary<ConditionalKey, KeyConditional>
    unionKeys: Dictionary<UnionKey, KeyUnion>
    intersectionKeys: Dictionary<IntersectionKey, KeyIntersection>
    indexAccessKeys: Dictionary<IndexAccessKey, KeyIndexAccess>
    moduleKeys: Dictionary<ModuleKey, KeyModule>
    tupleKeys: Dictionary<TupleKey, KeyTuple>
    tupleElementKeys: Dictionary<TupleElementKey, KeyTupleElement>
    indexKeys: Dictionary<IndexKey, KeyIndex>
    predicateKeys: Dictionary<PredicateKey, KeyPredicate>
    typeLiteralKeys: Dictionary<TypeLiteralKey, KeyTypeLiteral>
    literalKeys: Dictionary<LiteralKey, TsLiteral>
    literalAssociations: LiteralAssociations
}

type FrozenKeyCache = {
    documentation: FrozenDictionary<MasterKey, TsComment array>
    visitationFlags: FrozenDictionary<MasterKey, VisitationFlags>
    sourceKeys: FrozenDictionary<SourceKey, string>
    cyclicRemaps: FrozenDictionary<MasterKey, MasterKey>
    masters: FrozenDictionary<MasterKey, MasterBuilder>
    members: FrozenDictionary<MemberKey, MemberBuilder>
    memberIndex: FrozenDictionary<MasterKey, MemberKey array>
    nameKeys: FrozenDictionary<NameKey, Name>
    typeToMasterKeys: FrozenDictionary<HashTypeKey, MasterKey>
    parameterKeys: FrozenDictionary<ParameterKey, KeyParameter>
    typeParameterKeys: FrozenDictionary<TypeParameterKey, KeyTypeParameter>
    typeReferenceKeys: FrozenDictionary<TypeReferenceKey, KeyTypeReference>
    methodKeys: FrozenDictionary<MethodKey, KeyMethod>
    callSignatureKeys: FrozenDictionary<CallSignatureKey, KeyCallSignature>
    constructSignatureKeys: FrozenDictionary<ConstructSignatureKey, KeyConstructSignature>
    propertyKeys: FrozenDictionary<PropertyKey, KeyProperty>
    getAccessorKeys: FrozenDictionary<GetAccessorKey, KeyGetAccessor>
    setAccessorKeys: FrozenDictionary<SetAccessorKey, KeySetAccessor>
    indexSignatureKeys: FrozenDictionary<IndexSignatureKey, KeyIndexSignature>
    constructorKeys: FrozenDictionary<ConstructorKey, KeyConstructor>
    functionKeys: FrozenDictionary<FunctionKey, KeyFunction>
    typeAliasKeys: FrozenDictionary<TypeAliasKey, KeyTypeAlias>
    enumKeys: FrozenDictionary<EnumKey, KeyEnum>
    enumCaseKeys: FrozenDictionary<EnumCaseKey, KeyEnumCase>
    variableKeys: FrozenDictionary<VariableKey, KeyVariable>
    interfaceKeys: FrozenDictionary<InterfaceKey, KeyInterface>
    classKeys: FrozenDictionary<ClassKey, KeyClass>
    conditionalKeys: FrozenDictionary<ConditionalKey, KeyConditional>
    unionKeys: FrozenDictionary<UnionKey, KeyUnion>
    intersectionKeys: FrozenDictionary<IntersectionKey, KeyIntersection>
    indexAccessKeys: FrozenDictionary<IndexAccessKey, KeyIndexAccess>
    moduleKeys: FrozenDictionary<ModuleKey, KeyModule>
    tupleKeys: FrozenDictionary<TupleKey, KeyTuple>
    tupleElementKeys: FrozenDictionary<TupleElementKey, KeyTupleElement>
    indexKeys: FrozenDictionary<IndexKey, KeyIndex>
    predicateKeys: FrozenDictionary<PredicateKey, KeyPredicate>
    typeLiteralKeys: FrozenDictionary<TypeLiteralKey, KeyTypeLiteral>
    literalKeys: FrozenDictionary<LiteralKey, TsLiteral>
    literalAssociations: LiteralAssociations   
} 


type KeyCache with
    static member Empty =
        let literalKeys = Dictionary<LiteralKey, TsLiteral>()
        {
            documentation = Dictionary<MasterKey, TsComment array>()
            visitationFlags = Dictionary<MasterKey, VisitationFlags>()
            cycles = HashSet<CyclicKey>()
            sourceKeys = Dictionary<SourceKey, string>()
            cyclicRemaps = Dictionary<MasterKey, MasterKey>()
            masters = Dictionary<MasterKey, MasterBuilder>()
            members = Dictionary<MemberKey, MemberBuilder>()
            memberIndex = Dictionary<MasterKey, MemberKey array>()
            seenKeys = HashSet<HashTypeKey>()
            nameKeys = Dictionary<NameKey, string>()
            typeToMasterKeys = Dictionary<HashTypeKey, MasterKey>()
            parameterKeys = Dictionary<ParameterKey, KeyParameter>()
            typeParameterKeys = Dictionary<TypeParameterKey, KeyTypeParameter>()
            typeReferenceKeys = Dictionary<TypeReferenceKey, KeyTypeReference>()
            methodKeys = Dictionary<MethodKey, KeyMethod>()
            callSignatureKeys = Dictionary<CallSignatureKey, KeyCallSignature>()
            constructSignatureKeys = Dictionary<ConstructSignatureKey, KeyConstructSignature>()
            propertyKeys = Dictionary<PropertyKey, KeyProperty>()
            getAccessorKeys = Dictionary<GetAccessorKey, KeyGetAccessor>()
            setAccessorKeys = Dictionary<SetAccessorKey, KeySetAccessor>()
            indexSignatureKeys = Dictionary<IndexSignatureKey, KeyIndexSignature>()
            constructorKeys = Dictionary<ConstructorKey, KeyConstructor>()
            functionKeys = Dictionary<FunctionKey, KeyFunction>()
            typeAliasKeys = Dictionary<TypeAliasKey, KeyTypeAlias>()
            enumKeys = Dictionary<EnumKey, KeyEnum>()
            enumCaseKeys = Dictionary<EnumCaseKey, KeyEnumCase>()
            enumCaseToEnumKeys = Dictionary<EnumCaseKey, EnumKey>()
            variableKeys = Dictionary<VariableKey, KeyVariable>()
            interfaceKeys = Dictionary<InterfaceKey, KeyInterface>()
            classKeys = Dictionary<ClassKey, KeyClass>()
            conditionalKeys = Dictionary<ConditionalKey, KeyConditional>()
            unionKeys = Dictionary<UnionKey, KeyUnion>()
            intersectionKeys = Dictionary<IntersectionKey, KeyIntersection>()
            indexAccessKeys = Dictionary<IndexAccessKey, KeyIndexAccess>()
            moduleKeys = Dictionary<ModuleKey, KeyModule>()
            tupleKeys = Dictionary<TupleKey, KeyTuple>()
            tupleElementKeys = Dictionary<TupleElementKey, KeyTupleElement>()
            indexKeys = Dictionary<IndexKey, KeyIndex>()
            predicateKeys = Dictionary<PredicateKey, KeyPredicate>()
            typeLiteralKeys = Dictionary<TypeLiteralKey, KeyTypeLiteral>()
            literalKeys = literalKeys
            literalAssociations = LiteralAssociations(literalKeys)
        }
type KeyResolutionContext = private {
    keyCache: KeyCache
    getGlueFun: TypeKey -> Result<TsType, NodeStore>
    resolveTypeFun: KeyResolutionContext -> Result<TsType, NodeStore> -> MasterKey
    logger: ILogger
} with
    member this.Logger = this.logger
    member this.cache = this.keyCache

module KRLogging =
    let inline logVisitedPreComputedValue (ctx: KeyResolutionContext) (key: TypeKey) (value: MasterKey) =
        logft ctx.Logger "KeyCache.visitType already computed for %i{typeKey} as %A{masterKey}" key value
    let inline logCyclicKeyCreation (ctx: KeyResolutionContext) (key: TypeKey) (masterKey: MasterKey) =
        logfw ctx.Logger
            "KeyCache.visitType has seen this typeKey %i{typeKey} but does not have a result; creating a cyclic key %A{cyclicKey}"
            key masterKey
    let inline logCyclicRemap (ctx: KeyResolutionContext) (key: TypeKey) (maybeCyclic: MasterKey) (result: MasterKey) =
        logfi ctx.Logger " \
KeyCache.visitType completed a visitation which was originally not seen for the typeKey %i{typeKey}, but already had a \
result %A{cyclicKey} after the visitation completed. This indicates the visitation completed resolution of a cyclic key. \
The resolved key of %A{masterKey} will be mapped from the cyclic master key." key maybeCyclic result
    let inline logVisitation (ctx: KeyResolutionContext) (key: TypeKey) (result: MasterKey) =
        logfd ctx.Logger "KeyCache.visitType for typeKey %i{typeKey} created master key %A{masterKey}" key result
    let inline logKeyCreation
        (name: string)
        (ctx: KeyResolutionContext)
        (keyValue: 'Y)
        (key: 'X) =
        logft ctx.Logger "KeyCache.%s{keyType}: Registered %A{keyValue} as %A{key}" name keyValue key
        
[<AutoOpen>]
module Helpers =
    let inline makeKeyCacheFn<'Key, 'Value>
        (ctx: KeyResolutionContext)
        ([<InlineIfLambda>] logger: KeyResolutionContext -> 'Value -> 'Key -> unit)
        ([<InlineIfLambda>] fn: 'Value -> 'Key)
        (cache: Dictionary<'Key, 'Value>) =
        fun value ->
            let key = fn value
            logger ctx value key
            Dictionary.Flip.tryAddKeyPassthrough cache value key

type KeyResolutionContext with
    member inline internal ctx.addDocumentation (comments: TsComment list) (key: MasterKey) =
        if comments.IsEmpty then key
        else
        let comments = comments |> Array.ofList
        let addVisitationFlags (comments: TsComment array) =
            if comments |> Array.exists _.IsDeprecated then
                ctx.cache.visitationFlags
                |> Dictionary.Enum.add key (VisitationFlags.HasDocs ||| VisitationFlags.IsObsolete)
            else 
                ctx.cache.visitationFlags
                |> Dictionary.Enum.add key VisitationFlags.HasDocs 
        ctx.cache.documentation
        |> Dictionary.tryItem key
        |> function
            | ValueSome existing when existing = comments -> ()
            | ValueSome [||] ->
                addVisitationFlags comments
                ctx.cache.documentation[key] <- comments
            | ValueSome existing ->
                let newComments =
                    Array.concat [ existing; comments ]
                    |> Array.distinct
                addVisitationFlags newComments
                ctx.cache.documentation[key] <- newComments
            | ValueNone ->
                ctx.cache.documentation.Add(key, comments)
                addVisitationFlags comments
        key
    member ctx.visitType (key: TypeKey) =
        let hashTypeKey = HashTypeKey.create key
        if not <| ctx.cache.seenKeys.Add hashTypeKey then
            // Branch runs if we have already seen this typeKey
            ctx.cache.typeToMasterKeys
            |> Dictionary.tryItem hashTypeKey
            |> function
                // We should already have the result.
                | ValueSome value ->
                    KRLogging.logVisitedPreComputedValue ctx key value
                    // return the computed masterkey result.
                    value
                // If not, we are dealing with a cyclic key.
                | ValueNone ->
                    // IMPORTANT
                    // Ensure that the cyclic master key is associated with the literals and members and whatever other
                    // indexes we have created in the `else` branch of this method
                    let cyclicKey = CyclicKey.create key
                    let masterKey =
                        MasterBuilder.Cyclic cyclicKey
                        |> MasterKey.create

                    KRLogging.logCyclicKeyCreation ctx key masterKey
                    // Identify the cyclic key in the cycles set
                    ctx.cache.cycles.Add cyclicKey |> ignore
                    // Associate the cyclic master builder with the cyclic master key
                    ctx.cache.masters.Add(masterKey, MasterBuilder.Cyclic cyclicKey)
                    // Associate the original type key with the cyclic master key
                    ctx.cache.typeToMasterKeys.Add(hashTypeKey, masterKey)
                    // return the cyclic master key.
                    masterKey
        else
            // Branch runs if we have not yet seen this typekey
            let result =
                // Resolve the typekey to a masterKey
                ctx.getGlueFun key
                |> ctx.resolveTypeFun ctx
            
            // On first visitation of a typekey, and on completion of the resolver,
            // we should still not see any keys associated with the (since we just completed it).
            ctx.cache.typeToMasterKeys
            |> Dictionary.tryItem hashTypeKey
            |> function
                // If we find a masterkey already associated with the typekey we just resolved,
                // then we must have incurred a visitation of the current typekey while
                // we were resolving it to a masterkey.
                | ValueSome maybeCyclic ->
                    // The cyclic key is the master key we have already associated with the typekey.
                    // We will now remap the cyclic key to the master key we just created.
                    
                    // IMPORTANT
                    // Associate the cyclic master key with the literals and members and whatever other indexes we have created
                    // If we create more indexes, then we need to add their associations here if relevant.
                    KRLogging.logCyclicRemap ctx key maybeCyclic result
                    // We only need to change the result if the cyclic key is different to the masterkey.
                    if maybeCyclic <> result then
                        // Remap the cyclic key to the master key we just created.
                        ctx.cache.cyclicRemaps.Add(maybeCyclic, result)
                        // Map literal associations to cyclic master key
                        ctx.cache.literalAssociations.RegisterAssociations maybeCyclic [ result ]
                        // Add member index to cyclic master key
                        ctx.cache.memberIndex
                        |> Dictionary.tryItem result
                        |> ValueOption.iter (fun members -> ctx.cache.memberIndex.Add(maybeCyclic, members))
                        // Add documentation to cyclic master key
                        ctx.cache.documentation
                        |> Dictionary.tryItem result
                        |> ValueOption.iter (fun docs -> ctx.cache.documentation.Add(maybeCyclic, docs))
                        // Add visitation flags to cyclic master key
                        ctx.cache.visitationFlags
                        |> Dictionary.tryItem result
                        |> ValueOption.iter (fun flags -> ctx.cache.visitationFlags.Add(maybeCyclic, flags ||| VisitationFlags.ContainsCyclicReference))
                        // Return the cyclic master key, so that the hash of members/types etc which reference
                        // the cyclic key will be the same whether the cycle has been resolved or not.
                        maybeCyclic
                    else result
                | ValueNone ->
                    KRLogging.logVisitation ctx key result
                    ctx.cache.typeToMasterKeys.Add(hashTypeKey, result)
                    result
    member inline ctx.createSourceKey  (value: ShapeSourceMaybe<_>)=
            let key = SourceKey.createFrom value
            match value.Source with
            | Some source ->
                KRLogging.logKeyCreation "SourceKey" ctx source key
                Dictionary.Flip.tryAddKeyPassthrough ctx.cache.sourceKeys source key
            | None -> key
    member ctx.createNameKey(value: string) =
        KRLogging.logKeyCreation "NameKey" ctx value (NameKey.create {| Name = value |})
        let key = NameKey.createFromString value
        let name = value
        ctx.cache.nameKeys
        |> Dictionary.tryAddKeyPassthrough key name
    member ctx.createNameKey(value: Name) =
        let key = NameKey.createFromString value.ValueOrSource
        let value = Name.valueOrSource value
        ctx.cache.nameKeys
        |> Dictionary.tryAddKeyPassthrough key value
    member ctx.createMasterKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "MasterKey") MasterKey.create ctx.cache.masters
    member ctx.createMemberKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "MemberKey") MemberKey.create ctx.cache.members
    member ctx.createTypeReferenceKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TypeReferenceKey") TypeReferenceKey.create ctx.cache.typeReferenceKeys
    member ctx.createTypeParameterKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TypeParameterKey") TypeParameterKey.create ctx.cache.typeParameterKeys
    member ctx.createParameterKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ParameterKey") ParameterKey.create ctx.cache.parameterKeys
    member ctx.createMethodKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "MethodKey") MethodKey.create ctx.cache.methodKeys
    member ctx.createCallSignatureKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "CallSignatureKey") CallSignatureKey.create ctx.cache.callSignatureKeys
    member ctx.createLiteralKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "LiteralKey") LiteralKey.create ctx.cache.literalKeys
    member ctx.createConstructSignatureKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ConstructSignatureKey") ConstructSignatureKey.create ctx.cache.constructSignatureKeys
    member ctx.createPropertyKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "PropertyKey") PropertyKey.create ctx.cache.propertyKeys
    member ctx.createGetAccessorKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "GetAccessorKey") GetAccessorKey.create ctx.cache.getAccessorKeys
    member ctx.createSetAccessorKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "SetAccessorKey") SetAccessorKey.create ctx.cache.setAccessorKeys
    member ctx.createIndexSignatureKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "IndexSignatureKey") IndexSignatureKey.create ctx.cache.indexSignatureKeys
    member ctx.createConstructorKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ConstructorKey") ConstructorKey.create ctx.cache.constructorKeys
    member ctx.createFunctionKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "FunctionKey") FunctionKey.create ctx.cache.functionKeys
    member ctx.createTypeAliasKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TypeAliasKey") TypeAliasKey.create ctx.cache.typeAliasKeys
    member ctx.createEnumKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "EnumKey") EnumKey.create ctx.cache.enumKeys
    member ctx.createEnumCaseKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "EnumCaseKey") EnumCaseKey.create ctx.cache.enumCaseKeys
    member ctx.createVariableKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "VariableKey") VariableKey.create ctx.cache.variableKeys
    member ctx.createInterfaceKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "InterfaceKey") InterfaceKey.create ctx.cache.interfaceKeys
    member ctx.createClassKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ClassKey") ClassKey.create ctx.cache.classKeys
    member ctx.createConditionalKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ConditionalKey") ConditionalKey.create ctx.cache.conditionalKeys
    member ctx.createUnionKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "UnionKey") UnionKey.create ctx.cache.unionKeys
    member ctx.createIntersectionKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "IntersectionKey") IntersectionKey.create ctx.cache.intersectionKeys
    member ctx.createIndexAccessKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "IndexAccessKey") IndexAccessKey.create ctx.cache.indexAccessKeys
    member ctx.createModuleKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "ModuleKey") ModuleKey.create ctx.cache.moduleKeys
    member ctx.createTupleKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TupleKey") TupleKey.create ctx.cache.tupleKeys
    member ctx.createTupleElementKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TupleElementKey") TupleElementKey.create ctx.cache.tupleElementKeys
    member ctx.createIndexKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "IndexKey") IndexKey.create ctx.cache.indexKeys
    member ctx.createPredicateKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "PredicateKey") PredicateKey.create ctx.cache.predicateKeys
    member ctx.createTypeLiteralKey =
        makeKeyCacheFn ctx (KRLogging.logKeyCreation "TypeLiteralKey") TypeLiteralKey.create ctx.cache.typeLiteralKeys
    member ctx.hasVisitationFlag (key: MasterKey) flag =
        ctx.cache.visitationFlags
        |> Dictionary.Enum.has key flag
    member ctx.hasVisitationMask (key: MasterKey) mask =
        ctx.cache.visitationFlags
        |> Dictionary.Enum.hasMask key mask
    member ctx.notHasVisitationFlag (key: MasterKey) = ctx.hasVisitationFlag key >> not
    member ctx.notHasVisitationMask key = ctx.hasVisitationMask key >> not
    