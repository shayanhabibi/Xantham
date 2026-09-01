module Xantham.TypeScript.Collections

open System.Collections.Generic
open System.ComponentModel
open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham.Annotations
open Xantham.Fable

/// Factory that creates the wrapped version of the object, and provides the method
/// to navigate to its unique key.
type TypeWrapper<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType> = {
    IntermediateFn: Ts.Program -> ^RawType -> ^ProgramIntermediateType
    WrapFn: ^ProgramIntermediateType -> ^WrappedType
    KeyFn: ^ProgramIntermediateType -> ^KeyType
}

/// The factory for creating Symbol.Kind from a Symbol.
type SymbolTypeWrapper = TypeWrapper<SymbolKey, Ts.Symbol, ISymbol, Symbol.Kind>
/// The factory for creating Node.Kind from a Node.
type NodeTypeWrapper = TypeWrapper<NodeKey, Ts.Node, INode, Node.Kind>
/// The factory for creating Type.Kind from a Type.
type TypeTypeWrapper = TypeWrapper<TypeKey, Ts.Type, IType, Type.Kind>

/// Composite factory that stores the methods for wrapping and handling the 3 underlying types.
type TypeWrappers = {
    Symbol: SymbolTypeWrapper
    Node: NodeTypeWrapper
    Type: TypeTypeWrapper
}

type TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType> = TypeWrappers -> TypeWrapper<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType>
    
/// A utility container that holds the underlying value, and the wrapped value for an object.
type TypeWrapValue<^ProgramIntermediateType, ^WrappedType> = {
    Wrapper: ^WrappedType
    Value: ^ProgramIntermediateType
}

type SymbolValueMap = TypeWrapValue<ISymbol, Symbol.Kind>
type NodeValueMap = TypeWrapValue<INode, Node.Kind>
type TypeValueMap = TypeWrapValue<IType, Type.Kind>

/// Uniquely tags integers that are representative of the positional index for a function handler
type [<Measure>] awaitingIndex
/// Uniquely tags integers that are representative of the positional index for a value in the array
type [<Measure>] indexedIndex

/// Representation of an array [0*; 1*; 2?] where:
/// 0 - Is cached value
/// 1 - State
/// 2 - Index if state
/// Stores the pointer to the value, and the state of the index. When paired with its
/// key, it forms a 'Slot'. (ie this is the value of a 'slot').
[<TypeScriptTaggedUnion("1")>]
type SlotValue<^CachedValue> =
    | [<CompiledValue 0>] Pending of ``0``: ^CachedValue
    | [<CompiledValue 1>] Awaiting of ``0``: ^CachedValue * ``2``: int<awaitingIndex>
    | [<CompiledValue 2>] Indexed of ``0``: ^CachedValue * ``2``: int<indexedIndex>

/// Abbrev of a SlotIndex where the value is a IdentityValueMap type.
type TypeWrapSlotValue<^ProgramIntermediateType, ^WrappedType> = SlotValue<TypeWrapValue<^ProgramIntermediateType, ^WrappedType>>

/// Slots are keys and their indexes
type [<Erase>] Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType> =
    | Slot of key: ^KeyType * index: TypeWrapSlotValue<^ProgramIntermediateType, ^WrappedType>

type SymbolSlot = Slot<SymbolKey, ISymbol, Symbol.Kind>
type NodeSlot = Slot<NodeKey, INode, Node.Kind>
type TypeSlot = Slot<TypeKey, IType, Type.Kind>

/// <summary>
/// Stores the collective information regarding a symbol/node/type and its related objects.
/// </summary>
type CompositeKeyIndex = {
    UID: int
    mutable SymbolKey: SymbolKey option
    NodeKeys: HashSet<NodeKey>
    TypeKeys: HashSet<TypeKey>
    NodeTypePairs: Dictionary<NodeKey, TypeKey>
}

type CompositeKeyIndexError =
    | NodeAssociationAlreadyExists of NodeKey * TypeKey
    | SymbolKeyAlreadyExists of SymbolKey

/// Represents a dictionary mapping keys to slot indexes.
type SlotMap<^KeyType, ^ProgramWrappedIntermediate, ^WrappedType when ^KeyType :> IIdentityKey<^KeyType>> =
    Dictionary<^KeyType, TypeWrapSlotValue<^ProgramWrappedIntermediate, ^WrappedType>>

type SymbolSlotMap = SlotMap<SymbolKey, ISymbol, Symbol.Kind>
type NodeSlotMap = SlotMap<NodeKey, INode, Node.Kind>
type TypeSlotMap = SlotMap<TypeKey, IType, Type.Kind>

type CompositeCollection = {
    Wrappers: TypeWrappers
    SymbolKeys: SymbolSlotMap
    NodeKeys: NodeSlotMap
    TypeKeys: TypeSlotMap
    Indexes: ResizeArray<CompositeKeyIndex>
    Awaiters: ResizeArray<(CompositeKeyIndex -> unit) option>
}

type CompositeKeyCollectionError =
    | ConflictingIndexAssociations of CompositeKeyIndex array
    | IndexError of CompositeKeyIndexError

type PackageCollection = private {
    Packages: Dictionary<Packages.PackageId, HashSet<NodeKey>>
    Index: Dictionary<NodeKey, Packages.SubModuleId>
    PackageDependencies: Dictionary<NodeKey, HashSet<NodeKey>>
    mutable MustComputeCircularDependencies: bool
    mutable CircularDependencies: Map<NodeKey, HashSet<NodeKey>>
}

[<Erase>]
type ArrayIndexValue<^ValueType> =
    | ArrayIndexValue of position: int * value: ^ValueType
    member inline this.AsTuple = unbox<int * ^ValueType> this
    member inline this.Index = fst this.AsTuple
    member inline this.Value = snd this.AsTuple
    static member Create(index, value) = ArrayIndexValue(index, value)

/// <summary>
/// KeySearch is a type that represents the information provided from searching for a key in the collection.
/// When used in a collection to represent a composite key search, the optionality of the keysearch reflects
/// whether the composite key contained the key type or not. The optionality of its internal value is reflective
/// of whether the key is already registered in the value collection.
/// </summary>
[<Erase>]
type KeySearch<^Key> =
    | KeySearch of key: ^Key option * index: ArrayIndexValue<CompositeKeyIndex> option * awaitFn: ArrayIndexValue<CompositeKeyIndex -> unit> option
    member inline this.AsTuple = unbox<^Key option * ArrayIndexValue<CompositeKeyIndex> option * ArrayIndexValue<CompositeKeyIndex -> unit> option> this
    member inline this.Key = this.AsTuple |> fun (key,_,_) -> key
    member inline this.Index = this.AsTuple |> fun (_,index,_) -> index
    member inline this.Awaiter = this.AsTuple |> fun (_,_,awaiter) -> awaiter
    static member inline Create<^KeyType, ^U, ^W when ^KeyType :> IIdentityKey<^KeyType>> (
        key: Slot<^KeyType, ^U, ^W>,
        index: ArrayIndexValue<CompositeKeyIndex>
        ) = KeySearch(Some key, Some index, None)
    static member inline Create<^KeyType, ^U, ^W when ^KeyType :> IIdentityKey<^KeyType>> (
        key: Slot<^KeyType, ^U, ^W>,
        awaiter: ArrayIndexValue<CompositeKeyIndex -> unit>
        ) = KeySearch(Some key, None, Some awaiter)
    static member inline Create<^KeyType, ^U, ^W when ^KeyType :> IIdentityKey<^KeyType>> (key: Slot<^KeyType, ^U, ^W>) =
        KeySearch(Some key, None, None)
    static member inline Create<^KeyType, ^U, ^W when ^KeyType :> IIdentityKey<^KeyType>>(): KeySearch<Slot<^KeyType, ^U, ^W>> = KeySearch(None, None, None)

type CompositeKeySearchQueryObject<^Value> =
    abstract Symbol: ^Value with get,set
    abstract Node: ^Value with get,set
    abstract Type: ^Value with get,set


[<Erase>]
type CompositeKeySearch =
    | CompositeKeySearch of
        symbol: KeySearch<SymbolSlot> option *
        node: KeySearch<NodeSlot> option *
        type': KeySearch<TypeSlot> option
    member this.AsTuple = unbox<KeySearch<SymbolSlot> option * KeySearch<NodeSlot> option * KeySearch<TypeSlot> option> this
    member this.AsArray = unbox<KeySearch<Slot<int, obj, obj>> option array> this
    member inline this.Symbol = let symbol, _, _ = this.AsTuple in symbol
    member inline this.Node = let _, node, _ = this.AsTuple in node
    member inline this.Type = let _, _, type' = this.AsTuple in type'
    static member Create(?symbol, ?node, ?type') = CompositeKeySearch(symbol, node, type')

module TypeWrapper =
    let inline createIntermediate wrapper program underlyingValue =
        wrapper.IntermediateFn program underlyingValue 
    let inline createWrapper wrapper intermediateValue =
        wrapper.WrapFn intermediateValue
    let inline getKey wrapper intermediateValue =
        wrapper.KeyFn intermediateValue
    let inline createWrapperFromRaw wrapper program underlyingValue =
        createIntermediate wrapper program underlyingValue
        |> createWrapper wrapper
    let inline getKeyFromRaw wrapper program underlyingValue =
        createIntermediate wrapper program underlyingValue
        |> getKey wrapper
    let inline createTypeWrapValue wrapper intermediate = {
        Wrapper = createWrapper wrapper intermediate
        Value = intermediate
    }
    let inline createTypeWrapValueFromRaw wrapper program underlyingValue =
        createIntermediate wrapper program underlyingValue
        |> createTypeWrapValue wrapper

module TypeWrapperPath =
    let Symbol: TypeWrappers -> SymbolTypeWrapper = _.Symbol
    let Node: TypeWrappers -> NodeTypeWrapper = _.Node
    let Type: TypeWrappers -> TypeTypeWrapper = _.Type

module TypeWrappers =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let symbolTypeKey = SymbolTypeKey.create<TypeWrappers> "TypeWrappers"
        
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type HelperFunctions<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType> =
        abstract createIntermediate: wrappers: TypeWrappers -> program: Ts.Program -> underlyingValue: ^RawType -> ^ProgramIntermediateType
        abstract createWrapper: wrappers: TypeWrappers -> intermediate: ^ProgramIntermediateType -> ^WrappedType
        abstract getKey: wrappers: TypeWrappers -> intermediate: ^ProgramIntermediateType -> ^KeyType
        abstract initSlot: wrappers: TypeWrappers -> intermediate: ^ProgramIntermediateType -> Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        
    let inline makeProgramIntermediate (path: TypeWrapperPath<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType>) wrapper program underlyingValue =
        TypeWrapper.createIntermediate (path wrapper) program underlyingValue 
    let inline makeWrapper (path: TypeWrapperPath<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType>) wrapper intermediateValue =
        TypeWrapper.createWrapper (path wrapper) intermediateValue
    let inline getKeyType (path: TypeWrapperPath<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType>) wrapper intermediateValue =
        TypeWrapper.getKey (path wrapper) intermediateValue
    let inline initSlot (path: TypeWrapperPath<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType>) wrapper intermediateValue =
        Slot(
            getKeyType path wrapper intermediateValue,
            SlotValue.Pending {
                Wrapper = makeWrapper path wrapper intermediateValue
                Value = intermediateValue
            }
        )
        
    [<EditorBrowsable(EditorBrowsableState.Never); AutoOpen>]
    module Internal =
        let inline makeHelperFunctions (path: TypeWrapperPath<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType>): HelperFunctions<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType> =
            { new HelperFunctions<^UnderlyingValue, ^ProgramIntermediate, ^KeyType, ^WrappedType> with
                    member this.createIntermediate wrapper program underlyingValue =
                        makeProgramIntermediate path wrapper program underlyingValue
                    member this.getKey wrappers intermediate =
                        getKeyType path wrappers intermediate
                    member this.createWrapper wrappers intermediate =
                        makeWrapper path wrappers intermediate
                    member this.initSlot wrappers intermediate =
                        initSlot path wrappers intermediate }
    
    let toSymbolWrapper = _.Symbol
    let toNodeWrapper = _.Node
    let toTypeWrapper = _.Type
    
    let inline create symbolWrapper nodeWrapper typeWrapper = {
        Symbol = symbolWrapper
        Node = nodeWrapper
        Type = typeWrapper
    }
    
    let Symbol: HelperFunctions<SymbolKey, Ts.Symbol, ISymbol, Symbol.Kind> = makeHelperFunctions TypeWrapperPath.Symbol
    let Node: HelperFunctions<NodeKey, Ts.Node, INode, Node.Kind> = makeHelperFunctions TypeWrapperPath.Node
    let Type: HelperFunctions<TypeKey, Ts.Type, IType, Type.Kind> = makeHelperFunctions TypeWrapperPath.Type

module SlotValue =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    module Implementation =
        let inline createPending (cachedValue: ^CachedValue) =
            [| cachedValue; !! 0; !! null |] |> unbox<SlotValue<^CachedValue>>
        let inline createAwaiting (cachedValue: ^CachedValue) (awaitingIndex: int) =
            [| cachedValue; !! 1; !! awaitingIndex |] |> unbox<SlotValue<^CachedValue>>
        let inline createIndexed (cachedValue: ^CachedValue) (index: int) =
            [| cachedValue; !! 2; !! index |] |> unbox<SlotValue<^CachedValue>>
        let inline setIndexToValue (index: SlotValue<_>) (indexToSet: int) (value: obj) =
            (unbox<ResizeArray<obj>> index)[indexToSet] <- value
        let inline setTagValue (index: SlotValue<_>) (tagValue: int) = setIndexToValue index 1 tagValue
        let inline setTagToPending (slotIndex: SlotValue<_>) = setTagValue slotIndex 0
        let inline setTagToAwaiting (slotIndex: SlotValue<_>) = setTagValue slotIndex 1
        let inline setTagToIndexed (slotIndex: SlotValue<_>) = setTagValue slotIndex 2
        let inline setIndexValue (index: SlotValue<_>) indexValue = setIndexToValue index 2 indexValue
        let inline getCachedValue<^CachedType> (index: SlotValue<_>): ^CachedType = (unbox<ResizeArray<obj>> index)[0] |> unbox
        let inline setPending (index: SlotValue<_>) = setTagToPending index
        let inline setAwaiting (awaitingIndex: int) (index: SlotValue<_>) =
            setTagToAwaiting index
            setIndexValue index awaitingIndex
        let inline setIndexed (positionIndex: int) (index: SlotValue<_>) =
            setTagToIndexed index
            setIndexValue index positionIndex
    let inline create<^CacheValue> cacheValue: SlotValue<^CacheValue> = Implementation.createPending cacheValue
    let inline isPending<^CacheValue>: SlotValue<^CacheValue> -> bool = _.IsPending
    let inline isAwaiting<^CacheValue>: SlotValue<^CacheValue> -> bool = _.IsAwaiting
    let inline isIndexed<^CacheValue>: SlotValue<^CacheValue> -> bool = _.IsIndexed
    let inline isPendingOrAwaiting<^CacheValue>: SlotValue<^CacheValue> -> bool = isIndexed >> not
    let inline setIndex (positionalIndex: int<indexedIndex>) (slotIndex: SlotValue<^CacheValue>)  =
        match slotIndex with
        | Indexed(_, idx) when idx = positionalIndex  -> Ok None
        | Indexed(_, idx) -> Result.Error idx
        | _ ->
            match slotIndex with
            | Awaiting(_, fnIndex) -> Some fnIndex |> Ok
            | _ -> Ok None
            |> fun result ->
                // mutable op must come after all conditions
                Implementation.setIndexed (int positionalIndex) slotIndex
                result
            
    let inline setAwaiting (positionalIndex: int<awaitingIndex>) (slotIndex: SlotValue<^CacheValue>) =
        match slotIndex with
        | Awaiting(_, fnIndex) when fnIndex <> positionalIndex -> Result.Error fnIndex
        | Awaiting _ -> Ok None
        | Indexed(_, idx) -> Result.Ok (Some idx)
        | Pending _ ->
            Implementation.setAwaiting (int positionalIndex) slotIndex
            Ok None
    let inline cache<^CachedValue> (slotIndex: SlotValue<^CachedValue>) =
        (unbox<Array<^CachedValue>> slotIndex)[0]

module TypeWrapSlotValue =
    /// Alias for SlotValue.create
    let inline create<^ProgramIntermediateType, ^WrapperType>
        (valueMap: TypeWrapValue<^ProgramIntermediateType, ^WrapperType>)
        : TypeWrapSlotValue<^ProgramIntermediateType, ^WrapperType> =
        SlotValue.create valueMap
    let inline rawCreate underlying wrapper = create { Wrapper = wrapper; Value = underlying }
    let inline createFrom fn underlying = fn underlying |> rawCreate
    /// Alias for SlotValue.isPending
    let inline isPending<^Wrapper, ^Underlying>: TypeWrapSlotValue<^Underlying, ^Wrapper> -> bool = SlotValue.isPending
    /// Alias for SlotValue.isAwaiting
    let inline isAwaiting<^Wrapper, ^Underlying>: TypeWrapSlotValue<^Underlying, ^Wrapper> -> bool = _.IsAwaiting
    /// Alias for SlotValue.isIndexed
    let inline isIndexed<^Wrapper, ^Underlying>: TypeWrapSlotValue<^Underlying, ^Wrapper> -> bool = _.IsIndexed
    /// Alias for SlotValue.isPendingOrAwaiting
    let inline isPendingOrAwaiting<^Wrapper, ^Underlying>: TypeWrapSlotValue<^Underlying, ^Wrapper> -> bool = isIndexed >> not
    /// Alias for SlotValue.setIndex
    let inline setIndex (positionalIndex: int<indexedIndex>) (slotIndex: TypeWrapSlotValue<^Underlying, ^Wrapper>)  =
        SlotValue.setIndex positionalIndex slotIndex
    /// Alias for SlotValue.setAwaiting
    let inline setAwaiting (positionalIndex: int<awaitingIndex>) (slotIndex: TypeWrapSlotValue<^Underlying, ^Wrapper>)  =
        SlotValue.setAwaiting positionalIndex slotIndex
    /// Alias for SlotValue.cache
    let inline cache<^Wrapper, ^Underlying> (slotIndex: TypeWrapSlotValue<^Underlying, ^Wrapper>) = SlotValue.cache slotIndex

module CompositeKeySearchQuery =
    let inline objectFromSearch (map: KeySearch<int> -> ^Value) (search: CompositeKeySearch) =
        jsOptions<CompositeKeySearchQueryObject<^Value>>(fun o ->
            o.Symbol <- !!(!!search.Symbol |> Option.map map)
            o.Node <- !!(!!search.Node |> Option.map map)
            o.Type <- !!(!!search.Type |> Option.map map)
        )
    let inline bindObjectFromSearch (map: KeySearch<int> -> ^Value option) (search: CompositeKeySearch) =
        jsOptions<CompositeKeySearchQueryObject<^Value>>(fun o ->
            o.Symbol <- !!(!!search.Symbol |> Option.bind map)
            o.Node <- !!(!!search.Node |> Option.bind map)
            o.Type <- !!(!!search.Type |> Option.bind map)
        )
    let inline fromSearch (map: KeySearch<int> -> ^Value) (search: CompositeKeySearch) =
        [|
            !!search.Symbol |> Option.map map
            !!search.Node |> Option.map map
            !!search.Type |> Option.map map
        |]
    let inline bindFromSearch (map: KeySearch<int> -> ^Value option) (search: CompositeKeySearch) =
        [|
            !!search.Symbol |> Option.bind map
            !!search.Node |> Option.bind map
            !!search.Type |> Option.bind map
        |]

type CompositeKeySearch with
    member inline this.Indexes = CompositeKeySearchQuery.bindFromSearch _.Index this
    member inline this.Awaiters = CompositeKeySearchQuery.bindFromSearch _.Awaiter this
    member inline this.IndexPositions = this |> CompositeKeySearchQuery.bindFromSearch (_.Index >> Option.map _.Index)
    member inline this.IndexValues = this |> CompositeKeySearchQuery.bindFromSearch (_.Index >> Option.map _.Value)
    member inline this.FirstIndexValue =
        this
        |> CompositeKeySearchQuery.bindFromSearch (
            _.Index
            >> Option.map _.Value
            )
        |> Array.tryPick id
    
module ArrayIndexValue =
    let inline (|Value|) (ArrayIndexValue(_, value)) = value
    let inline (|Index|) (ArrayIndexValue(index, _)) = index
    let inline create index value = ArrayIndexValue(index, value)
    let inline index (ArrayIndexValue(index, _)) = index
    let inline value (ArrayIndexValue(_, value)) = value
    let inline indexEquals (ArrayIndexValue(index1, _)) (ArrayIndexValue(index2, _)) = index1 = index2
    let inline valueEquals (ArrayIndexValue(_, value1)) (ArrayIndexValue(_, value2)) = value1 = value2
    module Flip =
        let inline flip ([<InlineIfLambda>] fn: ^A -> ^B -> ^C) (arg1: ^B) (arg2: ^A) = fn arg2 arg1
        let inline create value index = flip create value index

module CompositeKeyIndexError =
    let inline symbolKeyConflict symbolKey = CompositeKeyIndexError.SymbolKeyAlreadyExists symbolKey
    let inline nodeAssociationConflict nodeKey typeKey = CompositeKeyIndexError.NodeAssociationAlreadyExists(nodeKey, typeKey)
    module Flip =
        let inline nodeAssociationConflict typeKey nodeKey = nodeAssociationConflict nodeKey typeKey

module Slot =
    let inline key(Slot(key, _)) = key
    let inline slotIndex (Slot(_, index)) = index
    let inline mapSlotIndex<^Key, ^Underlying, ^Wrapper, ^Value> ([<InlineIfLambda>] fn: TypeWrapSlotValue<^Underlying, ^Wrapper> -> ^Value) (Slot(_: ^Key, index)): ^Value = fn index
    let inline mapSlotKey<^Key, ^Underlying, ^Wrapper, ^Value> ([<InlineIfLambda>] fn: ^Key -> ^Value) (Slot(key, _) : Slot<^Key, ^Underlying, ^Wrapper>) = fn key
    let inline rawCreate (key: ^Key) (index: TypeWrapSlotValue<^U, ^W>) = Slot(key, index)
    let inline createPending typeWrapper (intermediate: ^PrograrmIntermediate) =
        Slot(
            TypeWrapper.getKey typeWrapper intermediate,
            TypeWrapper.createTypeWrapValue typeWrapper intermediate
            |> SlotValue.Pending
            )
    let inline createAwaiting typeWrapper (awaitingIndex: int<awaitingIndex>) (intermediate: ^ProgramIntermediate) =
        Slot(
            TypeWrapper.getKey typeWrapper intermediate,
            TypeWrapper.createTypeWrapValue typeWrapper intermediate
            |> SlotValue.Implementation.createAwaiting
            |> funApply (int awaitingIndex)
            )
    let inline createIndexed typeWrapper (index: int<indexedIndex>) (intermediate: ^ProgramIntermediate) =
        Slot(
            TypeWrapper.getKey typeWrapper intermediate,
            TypeWrapper.createTypeWrapValue typeWrapper intermediate
            |> SlotValue.Implementation.createIndexed
            |> funApply (int index)
            )
    let inline setAwaiting (awaitingIndex: int<awaitingIndex>) (slot: Slot<^Key, _, _>) =
        slotIndex slot
        |> SlotValue.setAwaiting awaitingIndex
    let inline setAwaitingFromArrayIndexValue (awaiting: ArrayIndexValue<CompositeKeyIndex -> unit>) (slot: Slot<^Key, _, _>) =
        slotIndex slot
        |> SlotValue.setAwaiting (awaiting.Index |> LanguagePrimitives.Int32WithMeasure)
    let inline setIndexed (index: int<indexedIndex>) (slot: Slot<^Key, _, _>) =
        slotIndex slot
        |> SlotValue.setIndex index
    let inline setIndexedFromArrayIndexValue (index: ArrayIndexValue<CompositeKeyIndex>) (slot: Slot<^Key, _, _>) =
        slotIndex slot
        |> SlotValue.setIndex (index.Index |> LanguagePrimitives.Int32WithMeasure)
    /// Checks if a slot is pending (incl. awaiting).
    let inline isPendingOrAwaiting<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, bool>
                                                                      SlotValue.isPending
    /// Checks if a slot is pending (excl. awaiting).
    let inline isPending<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, bool> SlotValue.isPending
    /// Checks if a slot is awaiting an index.
    let inline isAwaiting<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, bool> SlotValue.isAwaiting
    /// Checks if a slot is indexed.
    let inline isIndexed<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, bool> SlotValue.isIndexed 
    /// Retrieves the awaited function position from a slot if it is awaiting an index.
    let inline tryAwaiting<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, _> <| function
        | SlotValue.Awaiting(_, index) -> Some index
        | _ -> None
    /// Retrieves the index from a slot if it contains one.
    let inline tryIndex<^Key, ^Underlying, ^Wrapper> = mapSlotIndex<^Key, ^Underlying, ^Wrapper, _> <| function
        | SlotValue.Indexed(_, index) -> Some index
        | _ -> None
    /// Checks if a slot key is equal to the given key.
    let inline equals<^Key, ^Underlying, ^Wrapper when ^Key:equality> (key: ^Key): Slot<^Key, ^Underlying, ^Wrapper> -> bool = mapSlotKey<^Key, ^Underlying, ^Wrapper, bool> ((=) key)
    /// <summary>
    /// Binary pattern matching slots which are completed and not completed.
    /// </summary>
    /// <param name="slot"></param>
    let inline (|IsPending|IsIndexed|IsAwaiting|) slot =
        if isIndexed slot then IsIndexed
        elif isAwaiting slot then IsAwaiting
        else IsPending
    let inline (|IsPendingOrAwaiting|_|) slot = isPendingOrAwaiting slot
    /// <summary>
    /// Exhaustive pattern match for slots which are awaiting an index, pending (but not awaiting),
    /// and containing an index.
    /// </summary>
    let inline (|Awaiting|Pending|Indexed|) (Slot(key: ^A, index)) =
        match index with
        | SlotValue.Awaiting(_, fnIndex) -> Awaiting(key, fnIndex)
        | SlotValue.Pending _ -> Pending(key)
        | SlotValue.Indexed(_, idx) -> Indexed(key, idx)

module rec KeySearch =
    let inline key<'T>: KeySearch<'T> -> _ = _.Key
    let inline awaiter<'T>: KeySearch<'T> -> _ = _.Awaiter
    let inline index<'T>: KeySearch<'T> -> _ = _.Index
    let inline initEmpty<'Key>: KeySearch<'Key> = unbox {||}
    let inline rawCreate<'T> (key: 'T) index awaiter =
        KeySearch(Some key, index, awaiter)
    let inline withKey (key: 'Key) (value: KeySearch<'Key>) =
        rawCreate key (index value) (awaiter value)
    let inline withIndex (index: ArrayIndexValue<CompositeKeyIndex>) (value: KeySearch<'Key>) =
        rawCreate (key value).Value (Some index) (awaiter value)
    let inline withAwaiter (awaiter: ArrayIndexValue<CompositeKeyIndex -> unit>) (value: KeySearch<'Key>) =
        rawCreate (key value).Value (index value) (Some awaiter)
    let inline requiresAction (value: KeySearch<'Key>) = value.Index.IsNone
    let inline isIndexed (value: KeySearch<'Key>) = value.Index.IsSome
    let inline isAwaiting (value: KeySearch<'Key>) = value.Awaiter.IsSome
    let inline isPendingOrAwaiting (value: KeySearch<'Key>) = key value |> Option.isSome && not (isIndexed value)
    let inline isPending (value: KeySearch<'Key>) = isPendingOrAwaiting value && not (isAwaiting value)
    let inline isNotFound (value: KeySearch<'Key>) = key value |> Option.isNone
    let inline (|Awaiting|Pending|Indexed|NotFound|) (value: KeySearch<^Key>) =
        if isIndexed value then Indexed(key value |> Option.get, index value |> Option.get)
        elif isAwaiting value then Awaiting(key value |> Option.get, awaiter value |> Option.get)
        elif key value |> Option.isSome then Pending(key value |> Option.get)
        else NotFound()
    let inline feedbackIndex (value: KeySearch<'Key>) (index: CompositeKeyIndex) =
        key value
    module Option =
        let inline hasInput (search: KeySearch<^Key> option) = Option.isSome search
        let inline isNotFound (search: KeySearch<^Key> option) = search |> Option.map KeySearch.isNotFound |> Option.defaultValue false
        let inline requiresAction<'Key>: KeySearch<'Key> option -> _ = Option.map KeySearch.requiresAction >> Option.defaultValue false
        let inline isIndexed<'Key>: KeySearch<'Key> option -> _ = Option.exists KeySearch.isIndexed
        let inline isAwaiting<'Key>: KeySearch<'Key> option -> _ = Option.exists KeySearch.isAwaiting
        let inline isPending<'Key>: KeySearch<'Key> option -> _ = Option.exists KeySearch.isPending
        let inline isPendingOrAwaiting<'Key>: KeySearch<'Key> option -> _ = Option.exists KeySearch.isPendingOrAwaiting
        let inline key<^T>: KeySearch<^T> option -> _ = Option.bind KeySearch.key
        let inline unsafeKey<^T>: KeySearch<^T> option -> ^T = Option.bind KeySearch.key >> Option.defaultWith (fun () -> failwith "KeySearch.key returned None")
        let inline awaiter<^T>: KeySearch<^T> option -> _ = Option.bind KeySearch.awaiter
        let inline unsafeAwaiter<^T>: KeySearch<^T> option -> ArrayIndexValue<CompositeKeyIndex -> unit> = Option.bind KeySearch.awaiter >> Option.defaultWith (fun () -> failwith "KeySearch.awaiter returned None")
        let inline index<^T>: KeySearch<^T> option -> _ = Option.bind KeySearch.index
        let inline unsafeIndex<^T>: KeySearch<^T> option -> ArrayIndexValue<CompositeKeyIndex> = Option.bind KeySearch.index >> Option.defaultWith (fun () -> failwith "KeySearch.index returned None")
        let inline initEmpty<'Key>: KeySearch<'Key> option = None
        let inline (|NoInput|NotFound|Pending|Awaiting|Indexed|) (value: KeySearch<^Key> option) =
            if not <| hasInput value then NoInput()
            elif isIndexed value then Indexed(key value |> Option.get, index value |> Option.get)
            elif isAwaiting value then Awaiting(key value |> Option.get, awaiter value |> Option.get)
            elif isPending value then Pending(key value |> Option.get)
            else NotFound()
    type SRTPHelper =
        static member inline Create(key: Slot<^Key, ^U, ^W>, value) = KeySearch.Create(key, awaiter = value)
        static member inline Create(key: Slot<^Key, ^U, ^W>, index) = KeySearch.Create(key, index = index)
        static member inline Create(key: Slot<^Key, ^U, ^W>, ()) = KeySearch.Create(key)
    let inline srtpCreate (key: Slot<^Key, ^U, ^W>) value =
        ((^T or SRTPHelper):(static member Create: Slot<^Key, ^U, ^W> * ^T -> KeySearch<Slot<^Key, ^U, ^W>>) (key, value))
        
type KeySearch<^Key> with
    member inline this.isNotFound = KeySearch.isNotFound this
    member inline this.isPending = KeySearch.isPending this
    member inline this.isAwaiting = KeySearch.isAwaiting this
    member inline this.isIndexed = KeySearch.isIndexed this
    member inline this.requiresAction = KeySearch.requiresAction this
    member inline this.isPendingOrAwaiting = KeySearch.isPendingOrAwaiting this
    
module TypeWrapValue =
    /// Creates an object containing a wrapper and a direct reference to the underlying value.
    let inline create underlyingValue wrapper = { Wrapper = wrapper; Value = underlyingValue }
    /// Gets the wrapper value for a given identity value map.
    let inline get { Wrapper = wrapper } = wrapper
    let inline wrapper map = get map
    /// Gets the underlying value for a given identity value map.
    let inline value { Value = value } = value
    /// Checks if the identity value map underlying values are equal
    let inline equalValue { Value = value1 } { Value = value2 } = value1 = value2
    /// Checks if the identity value maps wrapper values are equal.
    let inline equalWrapper { Wrapper = wrapper1 } { Wrapper = wrapper2 } = wrapper1 = wrapper2
    /// Checks if an identity value maps underlying value is equal to a given value.
    let inline valueEquals comp { Value = value } = comp = value
    /// Checks if an identity value maps wrapper value is equal to a given value.
    let inline wrapperEquals comp { Wrapper = wrapper } = comp = wrapper
    type SRTPHelper =
        static member inline equal (map1: TypeWrapValue<'U, _>, map2: TypeWrapValue<'U, _>) = equalValue map1 map2
        static member inline equal (map: TypeWrapValue<'UnderlyingValue, _>, value: 'UnderlyingValue) = valueEquals value map
        static member inline equal (value: 'UnderlyingValue, map: TypeWrapValue<'UnderlyingValue, _>) = valueEquals value map
        static member inline equal (map: TypeWrapValue<'T, 'Wrapper>, value: 'Wrapper) = wrapperEquals value map
        static member inline equal (value: 'Wrapper, map: TypeWrapValue<'T, 'Wrapper>) = wrapperEquals value map
    /// Performs generic equality checks between identity value maps and/or values.
    let inline srtpEqual l r = ((^T or SRTPHelper):(static member equal: ^T * ^U -> bool) (l,r))
    /// Creates an identity value map from a given underlying value using the
    /// given function to create its wrapper.
    let inline createFromConstructor fn underlyingValue = { Wrapper = fn underlyingValue; Value = underlyingValue }

module CompositeKeyIndex =
    module Internal =
        let mutable private uidCounter = 0
        /// Provides a unique index identifier for a given composite key index that is independent
        /// of the internal values.
        let getNextUid() =
            uidCounter <- uidCounter + 1
            uidCounter
        /// Checks if index has a symbol key
        let inline hasSymbolKey (index: CompositeKeyIndex) = index.SymbolKey.IsSome
        /// Checks if index has a specific node key stored
        let inline nodeKeyExists (nodeKey: NodeKey) (index: CompositeKeyIndex) =
            index.NodeKeys.Contains nodeKey
        /// Checks if index has a specific type key stored
        let inline typeKeyExists (typeKey: TypeKey) (index: CompositeKeyIndex) =
            index.TypeKeys.Contains typeKey
        /// Checks if index has a specific symbol key
        let inline symbolKeyExists (symbolKey: SymbolKey) (index: CompositeKeyIndex) =
            index.SymbolKey.IsSome && (index.SymbolKey.Value = symbolKey)
        /// Adds the provided type key to the index type key array without any other action.
        let inline putTypeKey (typeKey: TypeKey) (index: CompositeKeyIndex) =
            index.TypeKeys.Add(typeKey)
        /// Adds the provided node key to the index node key array without any other action.
        let inline putNodeKey (nodeKey: NodeKey) (index: CompositeKeyIndex) =
            index.NodeKeys.Add(nodeKey)
        /// Sets the index symbol key to the given value.
        let inline putSymbolKey (symbolKey: SymbolKey) (index: CompositeKeyIndex) =
            index.SymbolKey <- Some symbolKey
        /// Checks if the index has any associations for the provided node key.
        let inline associationForNodeExists (nodeKey: NodeKey) (index: CompositeKeyIndex) =
            index.NodeTypePairs.ContainsKey nodeKey
        /// Checks if the index has any associations for the provided type key.
        let inline associationForTypeExists (typeKey: TypeKey) (index: CompositeKeyIndex) =
            index.NodeTypePairs.ContainsValue typeKey
        /// Adds the provided association to the association array, without any other action.
        let inline putNodeTypeAssociationPair (nodeKey: NodeKey) (typeKey: TypeKey) (index: CompositeKeyIndex) =
            index.NodeTypePairs.Add(nodeKey, typeKey)
           
    /// Adds the provided type key to the index type key array if it was not present. Returns true if successful.
    let inline addTypeKey (typeKey: TypeKey) (index: CompositeKeyIndex) =
        Internal.putTypeKey typeKey index
    /// Adds the provided node key to the index node key array if it was not present. Returns true if successful.
    let inline addNodeKey (nodeKey: NodeKey) (index: CompositeKeyIndex) =
        Internal.putNodeKey nodeKey index
    let inline addAndIgnoreTypeKey typeKey index = addTypeKey typeKey index |> ignore
    let inline addAndIgnoreNodeKey nodeKey index = addNodeKey nodeKey index |> ignore
    /// Adds the provided node key to the index node key array if it was not present, and returns the index object.
    let inline includeNodeKey nodeKey index =
        addAndIgnoreNodeKey nodeKey index
        index
    /// Adds the provided type key to the index type key array if it was not present, and returns the index object.
    let inline includeTypeKey typeKey index =
        addAndIgnoreTypeKey typeKey index
        index
    /// Gets all associated node keys for the provided type key.
    let inline tryGetAssociationsForType (typeKey: TypeKey) (index: CompositeKeyIndex) =
        index.NodeTypePairs
        |> Seq.choose (fun kv -> if kv.Value = typeKey then Some kv.Key else None)
        |> NonEmptyArray.create
    /// Gets the associated type key, if one exists.
    let inline tryGetAssociationForNode (nodeKey: NodeKey) (index: CompositeKeyIndex) =
        match index.NodeTypePairs.TryGetValue nodeKey with
        | true, value -> Some value
        | _ -> None
    /// Adds the provided association to the association array if it was not present. Returns true if successful.
    /// Returns false if the association already exists. Returns an error with the existing type key associated to the
    /// node key if the node already has an association.
    let inline addNodeTypeAssociationPair nodeKey typeKey index =
        match tryGetAssociationForNode nodeKey index with
        | Some key when key <> typeKey ->
            Result.Error(key)
        | Some _ ->
            Ok false
        | None ->
            addAndIgnoreTypeKey typeKey index
            addAndIgnoreNodeKey nodeKey index
            Internal.putNodeTypeAssociationPair nodeKey typeKey index
            Ok true
    let inline includeNodeTypeAssociationPair nodeKey typeKey index =
        addNodeTypeAssociationPair nodeKey typeKey index
        |> Result.map (fun _ -> index)

    let setSymbolKey symbolKey index = Internal.putSymbolKey symbolKey index
    
    let initFromCompositeKey compositeKey =
        let maybeNodeKey = CompositeKey.node compositeKey
        let maybeTypeKey = CompositeKey.type' compositeKey
        {
            UID = Internal.getNextUid()
            SymbolKey = CompositeKey.symbol compositeKey
            NodeKeys = HashSet [ if maybeNodeKey.IsSome then maybeNodeKey.Value ]
            TypeKeys = HashSet [ if maybeTypeKey.IsSome then maybeTypeKey.Value ]
            NodeTypePairs =
                match maybeNodeKey, maybeTypeKey with
                | Some nodeKey, Some typeKey -> Dictionary [ KeyValuePair(nodeKey, typeKey) ]
                | _ -> Dictionary()
        }
        
    let getCompositeKeysForType (typeKey: TypeKey) (index: CompositeKeyIndex) =
        tryGetAssociationsForType typeKey index
        |> Option.map (
            NonEmptyArray.map (fun nodeKey ->
                CompositeKey.Create(
                    ?symbol = index.SymbolKey,
                    node = nodeKey,
                    typ = typeKey
                    )
                )
            )
        |> Option.orElseWith (fun () ->
            if index.TypeKeys.Contains typeKey then
                CompositeKey.Create(?symbol = index.SymbolKey, typ = typeKey)
                |> NonEmptyArray.singleton
                |> Some
            else None
            )
    let getCompositeKeyForNode (nodeKey: NodeKey) (index: CompositeKeyIndex) =
        tryGetAssociationForNode nodeKey index
        |> Option.map (fun typeKey ->
            CompositeKey.Create(
                ?symbol = index.SymbolKey,
                node = nodeKey,
                typ = typeKey
                )
            )
        |> Option.orElseWith (function
            | _ when index.NodeKeys.Contains nodeKey -> CompositeKey.Create(?symbol = index.SymbolKey, node = nodeKey) |> Some
            | _ -> None)
    let getCompositeKeysForIndex (index: CompositeKeyIndex) =
        let isolatedNodeKeys =
            index.NodeKeys
            |> Seq.except index.NodeTypePairs.Keys
            |> Seq.map (fun nodeKey ->
                CompositeKey.Create(node = nodeKey, ?symbol = index.SymbolKey)
                )
            |> Seq.toArray
        let isolatedTypeKeys =
            index.TypeKeys
            |> Seq.except index.NodeTypePairs.Values
            |> Seq.map (fun typeKey ->
                CompositeKey.Create(typ = typeKey, ?symbol = index.SymbolKey)
                )
            |> Seq.toArray
        index.NodeTypePairs
        |> Seq.map (fun kv ->
            CompositeKey.Create(
                ?symbol = index.SymbolKey,
                node = kv.Key,
                typ = kv.Value
                )
            )
        |> Seq.append isolatedNodeKeys
        |> Seq.append isolatedTypeKeys
        |> NonEmptyArray.create
    let includeCompositeKey (compositeKey: CompositeKey) (index: CompositeKeyIndex) =
        let inline makeAssociation (nodeKey: NodeKey) (typeKey: TypeKey) =
            addAndIgnoreTypeKey typeKey index
            addAndIgnoreNodeKey nodeKey index
            match addNodeTypeAssociationPair nodeKey typeKey index with
            | Ok _ -> Ok()
            | Error typeKey ->
                CompositeKeyIndexError.NodeAssociationAlreadyExists(nodeKey, typeKey)
                |> Result.Error
        let inline handleSymbolKey (symbolKey: SymbolKey option) =
            if symbolKey <> index.SymbolKey then
                symbolKey
                |> Option.iter (setSymbolKey >> funApply index)
        match compositeKey, index with
        | { Symbol = Some symbolKey }, { SymbolKey = Some indexSymbolKey } when symbolKey <> indexSymbolKey ->
            CompositeKeyIndexError.SymbolKeyAlreadyExists(indexSymbolKey)
            |> Result.Error
        | { Type = Some typeKey; Node = Some nodeKey; Symbol = symbolKey }, _ ->
            handleSymbolKey symbolKey
            makeAssociation nodeKey typeKey
        | { Symbol = symbolKey; Type = typeKey; Node = nodeKey }, _ ->
            handleSymbolKey symbolKey
            typeKey
            |> Option.iter (addAndIgnoreTypeKey >> funApply index)
            nodeKey
            |> Option.iter (addAndIgnoreNodeKey >> funApply index)
            Ok()
            

module CompositeKeySearch =
    // TODO - if keysearch has input, but not found, then we will not have keys accessible without the composite key that
    //      founded the keysearch.
    let inline private validateIndexAssociations (index: CompositeKeyIndex) (query: CompositeKeySearch) =
        match query.Node, query.Type with
        | KeySearch.Option.Indexed _, KeySearch.Option.Indexed _ -> Ok query
        | KeySearch.Option.NoInput , _ | _, KeySearch.Option.NoInput -> Ok query
        | _ ->
            match
                CompositeKeyIndex.addNodeTypeAssociationPair
                    (KeySearch.Option.key query.Node |> Option.get |> Slot.key)
                    (KeySearch.Option.key query.Type |> Option.get |> Slot.key)
                    index
            with
            | Ok _ -> Ok query
            | Error typeKey ->
                CompositeKeyIndexError.NodeAssociationAlreadyExists(
                    KeySearch.Option.key query.Node |> Option.get |> Slot.key,
                    typeKey
                    )
                |> CompositeKeyCollectionError.IndexError
                |> Result.Error
            
    let inline private validateIndexes (compositeKey: CompositeKey) (query: CompositeKeySearch) =
        query.Indexes
        |> Array.choose id
        |> Array.distinctBy ArrayIndexValue.index
        |> function
            // if there is no indexes for the query, then the query is still valid.
            | [||] -> Ok query
            // If the symbolkey is indexed, and we only have one unique index object, then we are good.
            | [| _ |] when query.Symbol |> KeySearch.Option.isIndexed || compositeKey.symbolKey.IsNone -> Ok query
            // If the symbolkey is not indexed, but the unique index object already has a symbolkey, then the symbolkey
            // in the index is different to the query. Invalid query.
            | [| ArrayIndexValue.Value { SymbolKey = Some key } |] ->
                CompositeKeyIndexError.SymbolKeyAlreadyExists key
                |> CompositeKeyCollectionError.IndexError
                |> Result.Error
            // If the query raises more than one index, then it is invalid.
            | arr ->
                arr
                |> Array.map ArrayIndexValue.value
                |> CompositeKeyCollectionError.ConflictingIndexAssociations
                |> Result.Error
    /// Checks if the composite key search is valid: has no indexes/one common index; associations that would be made
    /// are not conflicting with existing associations.
    let validate (compositeKey: CompositeKey) (query: CompositeKeySearch) =
        // if the indexes are valid for the query (none or one unique), then we
        // check (if present) that there are no association invalidations
        validateIndexes compositeKey query
        |> Result.bind (
            _.FirstIndexValue
            >> Option.map (
                validateIndexAssociations
                >> funApply query
                )
            >> Option.defaultValue (Result.Ok query)
            )
    /// Checks if the composite key search requires some action to be completed (ie not all input keys have been indexed.)
    let requiresAction (query: CompositeKeySearch) =
        query.AsArray
        |> Array.exists KeySearch.Option.requiresAction
    /// Checks if the composite key search requires initiation (implies action is required) - no index exists for the query.
    let requiresInitiation (query: CompositeKeySearch) =
        query.AsArray
        |> Array.forall (Option.map KeySearch.requiresAction >> Option.defaultValue true)
        
    let inline symbol (query: CompositeKeySearch) = query.Symbol
    let inline node (query: CompositeKeySearch) = query.Node
    let inline type' (query: CompositeKeySearch) = query.Type
    
    type HelperFunctions<^Key, ^U, ^W> =
        abstract isSome: CompositeKeySearch -> bool
        abstract isNone: CompositeKeySearch -> bool
        abstract exists: predicate: (KeySearch<Slot<^Key, ^U, ^W>> -> bool) -> search: CompositeKeySearch -> bool
        abstract map: fn: (KeySearch<Slot<^Key, ^U, ^W>> -> 'Output) -> search: CompositeKeySearch -> 'Output option
        abstract requiresAction: CompositeKeySearch -> bool
        abstract hasInput: CompositeKeySearch -> bool
        abstract isIndexed: CompositeKeySearch -> bool
        abstract isAwaiting: CompositeKeySearch -> bool
        abstract isPending: CompositeKeySearch -> bool
        abstract isPendingOrAwaiting: CompositeKeySearch -> bool
        abstract key: CompositeKeySearch -> ^Key option
        abstract tryAwaiter: CompositeKeySearch -> ArrayIndexValue<CompositeKeyIndex -> unit> option
        abstract tryIndex: CompositeKeySearch -> ArrayIndexValue<CompositeKeyIndex> option
        
    let inline private make<^Key, ^U, ^W> ([<InlineIfLambda>] mapping: CompositeKeySearch -> KeySearch<Slot<^Key, ^U, ^W>> option) =
        {
            new HelperFunctions<^Key, ^U, ^W> with
                member _.isSome search = mapping search |> Option.isSome
                member _.isNone search = mapping search |> Option.isNone
                member _.exists predicate search = mapping search |> Option.exists predicate
                member _.map fn search = mapping search |> Option.map fn
                member _.requiresAction search = mapping search |> KeySearch.Option.requiresAction
                member _.hasInput search = mapping search |> KeySearch.Option.hasInput
                member _.isIndexed search = mapping search |> KeySearch.Option.isIndexed
                member _.isAwaiting search = mapping search |> KeySearch.Option.isAwaiting
                member _.isPending search = mapping search |> KeySearch.Option.isPending
                member _.isPendingOrAwaiting search = mapping search |> KeySearch.Option.isPendingOrAwaiting
                member _.key search = mapping search |> KeySearch.Option.key |> Option.map Slot.key
                member _.tryAwaiter search = mapping search |> Option.bind KeySearch.awaiter
                member _.tryIndex search = mapping search |> Option.bind KeySearch.index
        }
    /// Shortcut functions mapped to the symbol of the composite key search.
    let Symbol = make _.Symbol
    /// Shortcut functions mapped to the node of the composite key search.
    let Node = make _.Node
    /// Shortcut functions mapped to the type of the composite key search.
    let Type = make _.Type
    
    /// Checks if the composite key search requires an association to be made to the index (implies action is required).
    let requiresAssociation (query: CompositeKeySearch) =
        Node.hasInput query && Type.hasInput query
        && (Node.requiresAction query || Type.requiresAction query)

module SlotMap =
    module Implementation =
        let inline map<^Key, ^UnderlyingValue, ^Wrapped, ^Value> (fn: Dictionary<^Key, TypeWrapSlotValue<^UnderlyingValue, ^Wrapped>> -> ^Value)
            (map: Dictionary<^Key, TypeWrapSlotValue<^UnderlyingValue,^Wrapped>>) : ^Value = fn map
    let inline init<^Key, ^UnderlyingValue, ^Wrapped when ^Key :> IIdentityKey<^Key> and ^Key:equality >: SlotMap<^Key, ^UnderlyingValue, ^Wrapped> = Dictionary []
    let inline keys (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.Keys
    let inline values (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.Values
    let inline containsKey (key: ^Key) (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.ContainsKey(key)
    let inline containsValue value (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.ContainsValue(value)
    let inline get key (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.Item(key)
    let inline tryGet key (map: SlotMap<^Key, ^U, ^W>): TypeWrapSlotValue<^U, ^W> option = map |> Implementation.map (fun map ->
        match map.TryGetValue key with
        | true, value -> Some value
        | _ -> None
        )
    let inline add (key: ^Key) value (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.Add(key, value)
    let inline tryAdd (key: ^Key) value (map: SlotMap<^Key, ^U, ^W>) = map |> Implementation.map _.TryAdd(key, value)

module CompositeCollection =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let symbolTypeKey = SymbolTypeKey.create<CompositeCollection> "CompositeCollection"
    let init wrappers = {
        Wrappers = wrappers
        SymbolKeys = SlotMap.init
        NodeKeys = SlotMap.init
        TypeKeys = SlotMap.init
        Indexes = ResizeArray()
        Awaiters = ResizeArray()
    }
    module private Internal =
        let inline queryImpl ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^Key, ^U, ^W>) key collection =
            mapping collection
            |> SlotMap.tryGet key
            |> Option.map (function
                | SlotValue.Pending _ as slot -> KeySearch.srtpCreate (Slot.rawCreate key slot) ()
                | SlotValue.Awaiting(_, idx) as slot ->
                    collection.Awaiters[int idx]
                    |> Option.defaultWith (fun () -> failwithf "Awaiters array index %i is out of bounds" (int idx))
                    |> ArrayIndexValue.create (int idx)
                    |> KeySearch.srtpCreate (Slot.rawCreate key slot)
                | SlotValue.Indexed(_, idx) as slot ->
                    let idx = int idx
                    collection.Indexes[idx]
                    |> ArrayIndexValue.create idx
                    |> KeySearch.srtpCreate (Slot.rawCreate key slot)
                )
            |> Option.defaultValue KeySearch.initEmpty<Slot<^Key, ^U, ^W>>
        let inline initSlot typeWrapperPath input collection  =
            TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
        let inline putSlot ([<InlineIfLambda>] slotMapPath: CompositeCollection -> SlotMap<^T, ^U, ^W>) collection input =
            (slotMapPath collection)
            |> SlotMap.add (Slot.key input) (Slot.slotIndex input)
        let inline includeSlot ([<InlineIfLambda>] slotMapPath: CompositeCollection -> SlotMap<^T, ^U, ^W>) collection input =
            putSlot slotMapPath collection input
            input
        let inline putPendingKey typeWrapperPath slotMapPath input collection =
            TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
            |> putSlot slotMapPath collection
        let inline includePendingKey typeWrapperPath slotMapPath input collection =
            TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
            |> includeSlot slotMapPath collection
        let inline putIndexedKey typeWrapperPath slotMapPath input collection idx =
            let slot = TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
            Slot.setIndexed (LanguagePrimitives.Int32WithMeasure idx) slot
            |> ignore
            putSlot slotMapPath collection slot
        let inline includeIndexedKey typeWrapperPath slotMapPath input collection idx =
            let slot = TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
            Slot.setIndexed (LanguagePrimitives.Int32WithMeasure idx) slot
            |> ignore
            includeSlot slotMapPath collection slot
            
        /// Finds the next available index for an awaiter handler and inserts it, and returns the index position
        let inline putAwaitHandlerImpl awaiter (collection: CompositeCollection) =
            let idx = 
                collection.Awaiters.AsArray
                |> Array.tryFindIndex _.IsNone
                |> Option.defaultWith(fun () ->
                    let nextIdx = collection.Awaiters.Count
                    collection.Awaiters.AddRange(Seq.replicate 5 None)
                    nextIdx
                    )
            collection.Awaiters[idx] <- Some awaiter
            LanguagePrimitives.Int32WithMeasure<awaitingIndex> idx
        let inline removeAwaitHandlerImpl (index: int<awaitingIndex>) (collection: CompositeCollection) =
            let success = collection.Awaiters[int index] |> Option.isSome
            collection.Awaiters[int index] <- None
            success
        let inline popAwaitHandlerImpl (index: int<awaitingIndex>) collection =
            let awaiter = collection.Awaiters[int index]
            removeAwaitHandlerImpl index collection |> ignore
            awaiter
        /// Adds the index object to the collection and returns the positional index.
        let inline addIndexImpl  (collection: CompositeCollection) (index: CompositeKeyIndex)=
            let idx = collection.Indexes.Count
            collection.Indexes.Add(index)
            LanguagePrimitives.Int32WithMeasure<indexedIndex> idx
            
        let inline addPendingSlotImpl
            (typeWrapperPath: TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrapperType>)
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrapperType>)
            (input: ^ProgramIntermediateType)
            (collection: CompositeCollection) =
            let key = TypeWrappers.getKeyType typeWrapperPath collection.Wrappers input
            match queryImpl mapping key collection with
            | KeySearch.Pending slot -> Ok(slot)
            | KeySearch.NotFound _ ->
                includePendingKey typeWrapperPath mapping input collection
                |> Ok
            | KeySearch.Indexed(_, index) ->
                Result.Error(Choice1Of2 index)
            | KeySearch.Awaiting(_, awaiter) ->
                Result.Error(Choice2Of2 awaiter)

        let inline addAwaitingHandlerImpl
            (typeWrapperPath: TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType>)
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrappedType>)
            (input: ^ProgramIntermediateType)
            handler
            (collection: CompositeCollection) =
            let key = TypeWrappers.getKeyType typeWrapperPath collection.Wrappers input
            match queryImpl mapping key collection with
            | KeySearch.Pending slot ->
                let idx = putAwaitHandlerImpl handler collection
                Slot.setAwaiting idx slot
                |> Result.map (Option.map (failwithf "%A") >> Option.defaultValue slot)
            | KeySearch.NotFound _ ->
                let slot = TypeWrappers.initSlot typeWrapperPath collection.Wrappers input
                putAwaitHandlerImpl handler collection
                |> Slot.setAwaiting
                |> funApply slot
                |> ignore
                slot
                |> includeSlot mapping collection
                |> Ok
            | KeySearch.Indexed(slot, index) ->
                index.Value
                |> handler
                Ok slot
            | KeySearch.Awaiting(_, awaiter) ->
                Result.Error(LanguagePrimitives.Int32WithMeasure awaiter.Index)
        let inline addIndexedSlotImpl
            (typeWrapperPath: TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType>)
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrappedType>)
            (input: ^ProgramIntermediateType)
            (collection: CompositeCollection)
            (idx: int<indexedIndex>) =
            let key = TypeWrappers.getKeyType typeWrapperPath collection.Wrappers input
            match queryImpl mapping key collection with
            | KeySearch.Pending slot ->
                Slot.setIndexed idx slot
                |> Result.map (fun _ -> slot)
                |> Result.mapError (fun _ -> slot)
            | KeySearch.NotFound _ ->
                Slot.createIndexed (typeWrapperPath collection.Wrappers) idx input
                |> includeSlot mapping collection
                |> Ok
            | KeySearch.Awaiting(slot, awaiter) ->
                if not <| removeAwaitHandlerImpl (LanguagePrimitives.Int32WithMeasure awaiter.Index) collection then failwith "Await handler not found"
                match Slot.setIndexed idx slot with
                | Ok(Some _) ->
                    collection.Indexes[int idx]
                    |> awaiter.Value 
                    Ok slot
                | _ -> Result.Error(slot)
            | KeySearch.Indexed(slot, existingIndex) ->
                if existingIndex.Value <> collection.Indexes[int idx]
                then Result.Error(slot)
                else Ok slot
        let inline tryCacheFromKey
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrappedType>)
            (key: ^KeyType)
            collection =
            mapping collection
            |> SlotMap.tryGet key
            |> Option.map SlotValue.cache
        let inline tryCacheFromInput
            (typeWrapperPath: TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType>)
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrappedType>)
            (input: ^ProgramIntermediateType)
            collection =
            TypeWrappers.getKeyType typeWrapperPath collection.Wrappers input
            |> tryCacheFromKey mapping
            |> funApply collection
        let inline tryValueFromKey
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^T, ^U, ^W>)
            key collection =
                tryCacheFromKey mapping key collection
                |> Option.map TypeWrapValue.value
        let inline tryWrapperFromKey
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^T, ^U, ^W>)
            key collection =
                tryCacheFromKey mapping key collection
                |> Option.map TypeWrapValue.wrapper
        let inline tryWrapperFromInput
            (typeWrapperPath: TypeWrapperPath<^T, ^UV, ^U, ^W>)
            ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^T, ^U, ^W>)
            (input: ^U) collection =
                tryCacheFromInput typeWrapperPath mapping input collection
                |> Option.map TypeWrapValue.wrapper
                
    type HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, ^ErrorType> =
        Result<Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType>, ^ErrorType>
    type AddAwaiterResult<^KeyType, ^ProgramIntermediateType, ^WrappedType> = HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, int<awaitingIndex>>
    type AddIndexedResult<^KeyType, ^ProgramIntermediateType, ^WrappedType> = HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType>>
    type Handler = CompositeKeyIndex -> unit
    type AddPendingResult<^KeyType, ^ProgramIntermediateType, ^WrappedType> = HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, Choice<ArrayIndexValue<CompositeKeyIndex>, ArrayIndexValue<Handler>>>
    type SetIndexByKeyResult<^KeyType, ^ProgramIntermediateType, ^WrappedType> = HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType>> option
    type SetAwaiterByKeyResult<^KeyType, ^ProgramIntermediateType, ^WrappedType> = HelperResult<^KeyType, ^ProgramIntermediateType, ^WrappedType, int<awaitingIndex>> option
    
    /// <summary>
    /// Functions premapped to the different key types (type/node/symbol) commonly used.
    /// </summary>
    type Helper<
        ^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType
            when ^KeyType :> IIdentityKey<^KeyType>
            and ^KeyType:equality
    > = {
        /// <summary>
        /// Query searches the collection for the key; use pattern matching to discriminate the result between awaiting,
        /// not found, pending etc.
        /// </summary>
        query: ^KeyType -> CompositeCollection -> KeySearch<Slot<^KeyType, ^ProgramIntermediateType, ^WrappedType>>
        /// <summary>
        /// Adds the element to the collection as a pending value. If the element was already registered
        /// as awaiting the result, or already indexed with the result, then the index and value of that slot type are
        /// returned as an error.
        /// On success (or if no change was made), then an Ok result is returned.
        /// </summary>
        addPending: ^ProgramIntermediateType -> CompositeCollection -> AddPendingResult<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        /// <summary>
        /// Adds the element to the collection with a handler which will run when the index for that value is completed.
        /// </summary>
        addAwaiter: ^ProgramIntermediateType -> Handler -> CompositeCollection -> AddAwaiterResult<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        /// <summary>
        /// Adds a value with its index position.
        /// </summary>
        addIndexed: ^ProgramIntermediateType -> int<indexedIndex> -> CompositeCollection -> AddIndexedResult<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        setAwaiterByKey: ^KeyType -> Handler -> CompositeCollection -> SetAwaiterByKeyResult<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        /// <summary>Returns None if the key does not exist in the collection. Returns Ok if the index is set to the
        /// given position. Returns an error if the index was at a different position already. Will automatically handle
        /// awaiting index fulfillment.</summary>
        setIndexedByKey: ^KeyType -> int<indexedIndex> -> CompositeCollection -> SetIndexByKeyResult<^KeyType, ^ProgramIntermediateType, ^WrappedType>
        /// Convenience function: addPending >> ignorer; return input
        register: CompositeCollection -> ^ProgramIntermediateType -> ^ProgramIntermediateType
        /// Convenience function: addPending >> getWrapper
        registerToWrapper: CompositeCollection -> ^ProgramIntermediateType -> ^WrappedType
        registerToKey: CompositeCollection -> ^ProgramIntermediateType -> ^KeyType
        getMap: CompositeCollection -> ^KeyType -> TypeWrapValue<^ProgramIntermediateType, ^WrappedType> option
        getWrapper: CompositeCollection -> ^KeyType -> ^WrappedType option
        getValue: CompositeCollection -> ^KeyType -> ^ProgramIntermediateType option
    }
    let inline private makeHelper
        (typeWrapperPath: TypeWrapperPath<^KeyType, ^RawType, ^ProgramIntermediateType, ^WrappedType>)
        ([<InlineIfLambda>] mapping: CompositeCollection -> SlotMap<^KeyType, ^ProgramIntermediateType, ^WrappedType>) =
        {
            query = Internal.queryImpl mapping
            addAwaiter = Internal.addAwaitingHandlerImpl typeWrapperPath mapping
            addIndexed = fun input index collection -> Internal.addIndexedSlotImpl typeWrapperPath mapping input collection index
            addPending = Internal.addPendingSlotImpl typeWrapperPath mapping
            setAwaiterByKey = fun key handler collection ->
                Internal.tryValueFromKey mapping key collection
                |> Option.map (
                    Internal.addAwaitingHandlerImpl typeWrapperPath mapping
                    >> funApply2 handler collection
                    )
            setIndexedByKey = fun key index collection ->
                Internal.tryValueFromKey mapping key collection
                |> Option.map (
                    Internal.addIndexedSlotImpl typeWrapperPath mapping
                    >> funApply2 collection index
                    )
            register = fun collection input ->
                Internal.addPendingSlotImpl typeWrapperPath mapping input collection |> ignore
                input
            registerToWrapper = fun collection input ->
                Internal.tryWrapperFromInput typeWrapperPath mapping input collection
                |> Option.defaultWith (fun () ->
                    match Internal.addPendingSlotImpl typeWrapperPath mapping input collection with
                    | Result.Ok slot ->
                        slot
                        |> Slot.slotIndex
                        |> SlotValue.cache
                        |> TypeWrapValue.wrapper
                    | e -> failwithf $"Impossible state: %A{e}"
                    )
            registerToKey = fun collection input ->
                Internal.addPendingSlotImpl typeWrapperPath mapping input collection |> ignore
                TypeWrapper.getKey (typeWrapperPath collection.Wrappers) input
            getMap = fun collection key ->
                Internal.tryCacheFromKey mapping key collection
            getWrapper = fun collection key ->
                Internal.tryWrapperFromKey mapping key collection
            getValue = fun collection key ->
                Internal.tryValueFromKey mapping key collection
        }
    let Symbol: Helper<SymbolKey, Ts.Symbol, ISymbol, Symbol.Kind> = makeHelper TypeWrapperPath.Symbol _.SymbolKeys
    let Node: Helper<NodeKey, Ts.Node, INode, Node.Kind> = makeHelper TypeWrapperPath.Node _.NodeKeys
    let Type: Helper<TypeKey, Ts.Type, IType, Type.Kind> = makeHelper TypeWrapperPath.Type _.TypeKeys
    let query (compositeKey: CompositeKey) collection =
        CompositeKeySearch.Create(
            ?symbol = (compositeKey.Symbol |> Option.map (Symbol.query >> funApply collection))
            , ?node = (compositeKey.Node |> Option.map (Node.query >> funApply collection))
            , ?type' = (compositeKey.Type |> Option.map (Type.query >> funApply collection))
            )
        |> CompositeKeySearch.validate compositeKey
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type QuerySRTPHelper =
        static member inline query(key, collection) = Symbol.query key collection
        static member inline query(key, collection) = Node.query key collection
        static member inline query(key, collection) = Type.query key collection
        static member inline query(key,collection) = query key collection
        
    let inline srtpQuery key collection =
        ((^T or QuerySRTPHelper):(static member query: ^T * CompositeCollection -> ^Value) (key, collection))
    
    let registerCompositeKey (compositeKey: CompositeKey) (collection: CompositeCollection) =
        let inline action idx map (helper: Helper<^A, ^B, ^C, ^D>) =
            map compositeKey
            |> Option.iter (
                helper.setIndexedByKey
                >> funApply2 idx collection
                >> Option.defaultWith(fun () ->
                    failwith "CompositeKey contained keys that were not cached. \
                            This occurs if the source of the key was not given to \
                            the composite key collection in isolation before being \
                            composed into a composite key.")
                >> Result.mapError (failwithf "key %A not found in collection")
                >> ignore
                )
        let inline compositeAction idx =
            action idx _.symbolKey Symbol
            action idx _.nodeKey Node
            action idx _.typeKey Type
            Ok()
        srtpQuery compositeKey collection
        |> Result.bind (function
            | search when CompositeKeySearch.requiresInitiation search ->
                CompositeKeyIndex.initFromCompositeKey compositeKey
                |> Internal.addIndexImpl collection
                |> compositeAction
            | search when CompositeKeySearch.requiresAction search ->
                search.Indexes
                |> Array.pick (Option.map ArrayIndexValue.index)
                |> LanguagePrimitives.Int32WithMeasure<indexedIndex>
                |> compositeAction
            | _ -> Ok() 
            )

module PackageCollection =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let symbolTypeKey = SymbolTypeKey.create<PackageCollection> "PackageCollection"
    
    let init () = {
        Packages = Dictionary()
        Index = Dictionary()
        PackageDependencies = Dictionary()
        MustComputeCircularDependencies = false
        CircularDependencies = Map<NodeKey, NodeKey HashSet> [  ]
    }
    
    module private Internal =
    
        let inline bitwiseOrCompute (collection: PackageCollection) (value: bool) =
            collection.MustComputeCircularDependencies <- collection.MustComputeCircularDependencies || value
            collection
            
        let inline addPackageIdToPackages (sourceFileKey: NodeKey) (Packages.SubModuleId(name, version, _)) (collection: PackageCollection) =
            match collection.Packages.TryGetValue <| Packages.PackageId(name, version) with
            | true, packages -> packages.Add(sourceFileKey) |> ignore
            | _ -> collection.Packages.Add(Packages.PackageId(name, version), HashSet [ sourceFileKey ])
            collection
            
        let inline addPackageIdToIndex (sourceFileKey: NodeKey) (subModuleId: Packages.SubModuleId) (collection: PackageCollection) =
            match collection.Index.TryGetValue sourceFileKey with
            | true, package when package <> subModuleId ->
                Result.Error package
            | true, _ -> Ok collection
            | false, _ ->
                collection.Index.Add(sourceFileKey, subModuleId)
                Ok collection
                
        let inline computeCircularDependencies (collection: PackageCollection) =
            collection.PackageDependencies
            |> Seq.choose (fun (KeyValue(key, set)) ->
                let orderedValues =
                    set |> Seq.filter ((<) key) |> Seq.toArray
                if Array.isEmpty orderedValues then None else
                Some(key, orderedValues)
                )
            |> Seq.sortBy fst
            |> Seq.collect (fun (key, values) ->
                    values
                    |> Seq.filter (fun value ->
                        match collection.PackageDependencies.TryGetValue value with
                        | true, dependencies -> dependencies.Contains(key)
                        | _ -> false
                        )
                    |> Seq.collect (fun value -> seq { key, value; value, key })
                )
            |> Seq.groupBy fst
            |> Seq.map (fun (key, values) -> key, Seq.map snd values |> HashSet)
            |> Map.ofSeq
            |> fun map ->
                collection.MustComputeCircularDependencies <- false
                collection.CircularDependencies <- map
        
    let inline hasPackageId (sourceFileKey: NodeKey) (collection: PackageCollection) = collection.Index.ContainsKey sourceFileKey
    
    let inline hasDependencies (dependee: NodeKey) (collection: PackageCollection) = collection.PackageDependencies.ContainsKey dependee
    
    let addDependency (dependee: NodeKey) (dependency: NodeKey) (collection: PackageCollection) =
        match collection.PackageDependencies.TryGetValue dependee with
        | true, dependencies ->
            dependencies.Add(dependency)
            |> Internal.bitwiseOrCompute collection
        | false, _ ->
            collection.PackageDependencies.Add(dependee, HashSet [ dependency ])
            Internal.bitwiseOrCompute collection true
            
            
    let addPackageId (sourceFileKey: NodeKey) (subModuleId: Packages.SubModuleId) (collection: PackageCollection) =
        Internal.addPackageIdToIndex sourceFileKey subModuleId collection
        |> Result.map (Internal.addPackageIdToPackages sourceFileKey subModuleId)
        
    let tryGetPackageId (sourceFileKey: NodeKey) (collection: PackageCollection) =
        match collection.Index.TryGetValue sourceFileKey with
        | true, packageId -> Some packageId
        | _ -> None
        
    let getModulesFromSubModuleId (Packages.SubModuleId(name, version, _) as subModule) (collection: PackageCollection) =
        match collection.Packages.TryGetValue <| Packages.PackageId(name, version) with
        | true, packages ->
            packages
            |> Seq.sort
            |> Seq.choose (tryGetPackageId >> funApply collection)
            |> NonEmptyArray.create
            |> Option.get
        | _ ->
            NonEmptyArray.singleton subModule

    let getModules (Packages.PackageId(name, version)) (collection: PackageCollection) =
        Packages.SubModuleId(name, version, Measures.annotate "")
        |> getModulesFromSubModuleId
        |> funApply collection
        
    let tryGetDependencies (dependee: NodeKey) (collection: PackageCollection) =
        match collection.PackageDependencies.TryGetValue dependee with
        | true, dependencies -> Some dependencies
        | _ -> None
        
    let getCircularDependencies (collection: PackageCollection) =
        if collection.MustComputeCircularDependencies then Internal.computeCircularDependencies collection
        collection.CircularDependencies

            
