module Xantham.Generator.Types.ObservableHashSet

open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.ComponentModel
open SignalsDotnet.Configuration

type ObservableHashSet<'T>(?values: seq<'T>) =
    let values = defaultArg values Seq.empty
    let inner = HashSet<'T>(values)
    let collectionChanged = Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()
    let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
    let notify action (value: 'T) =
        propertyChanged.Trigger(null, PropertyChangedEventArgs("Count"))
        collectionChanged.Trigger(null, NotifyCollectionChangedEventArgs(action, value))
    interface INotifyCollectionChanged with
        [<CLIEvent>]
        member _.CollectionChanged = collectionChanged.Publish
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member _.PropertyChanged = propertyChanged.Publish
    member _.Add(item: 'T) =
        let value = inner.Add(item)
        if value then notify NotifyCollectionChangedAction.Add item
        value
    member _.Remove(item: 'T) =
        let value = inner.Remove(item)
        if value then notify NotifyCollectionChangedAction.Remove item
        value
    member _.Contains(item: 'T) = inner.Contains(item)
    member _.Count = inner.Count
    interface IEnumerable<'T> with
        member _.GetEnumerator() = (inner :> IEnumerable<'T>).GetEnumerator()
    interface IEnumerable with
        member _.GetEnumerator() = (inner :> IEnumerable).GetEnumerator()

open SignalsDotnet

type SignalHashSet<'T> = CollectionSignal<ObservableHashSet<'T>>

[<Struct>]
type SignalSeq<'T, 'U when 'T:>INotifyCollectionChanged and 'T:> INotifyPropertyChanged and 'T: not struct> =
    {
        Source: CollectionSignal<'T>
        Func: 'T seq -> 'U
    }
    static member inline op_Implicit(source: CollectionSignal<'T>): SignalSeq<'T, 'T seq> = { Source = source; Func = id }
    static member inline Create(source: CollectionSignal<'T>): SignalSeq<'T, 'T seq> = { Source = source; Func = id }
module CollectionSignal =
    module Seq =
        let inline filter (f: 'T -> bool) collection =
            { collection with Func = collection.Func >> Seq.filter f }
        let inline map (f: 'T -> 'U) (collection: SignalSeq<_, _>) =
            { Source = collection.Source; Func = collection.Func >> Seq.map f }
        let choose (f: 'T -> 'U option) (collection: SignalSeq<_, _>) =
            { Source = collection.Source; Func = collection.Func >> Seq.choose f }
        let indexOf (f: _ -> bool) collection =
            { Source = collection.Source; Func = collection.Func >> Seq.findIndex f }
        let inline private compute (fn: _ -> 'T) (collection: SignalSeq<_, _>): IReadOnlySignal<_> =
            Signal.Computed(fun () ->
                seq collection.Source.Value
                |> collection.Func
                |> fn)
        let exec (collection: SignalSeq<_, 'T>): IReadOnlySignal<'T> = Signal.Computed(fun () -> seq collection.Source.Value |> collection.Func)
        let toList (collection: SignalSeq<_, _>) = compute Seq.toList collection
        let toArray (collection: SignalSeq<_, _>) = compute Seq.toArray collection
        let toSet (collection: SignalSeq<_, _>) = compute Set.ofSeq collection

module SignalHashSet =
    /// <summary>
    /// Default SignalHashSet.
    /// </summary>
    let empty<'T>: SignalHashSet<'T> = SignalHashSet<'T>()
    /// <summary>
    /// SignalHashSet with weak signal subscriptions that reduces memory leaks.
    /// </summary>
    let weakEmpty<'T>: SignalHashSet<'T> = SignalHashSet(
        collectionChangedConfiguration = (fun _ -> CollectionChangedSignalConfiguration(true, id))
        )
    /// <summary>
    /// Adds the value to the set
    /// </summary>
    let add (value: 'T) (set: SignalHashSet<'T>) = set.Value.Add(value)
    /// <summary>
    /// Removes the value from the set
    /// </summary>
    let remove (value: 'T) (set: SignalHashSet<'T>) = set.Value.Remove(value)
    /// <summary>
    /// Checks in-sync whether the value is currently in the set.
    /// </summary>
    /// <remarks>
    /// See <c>SignalHashSet.Compute.contains</c> and <c>SignalHashSet.Compute.containsValue</c> for reactive alternatives.
    /// </remarks>
    let contains (value: 'T) (set: SignalHashSet<'T>) = set.Value.Contains(value)
    /// <summary>
    /// Checks in-sync the current set size.
    /// </summary>
    /// <remarks>
    /// See <c>SignalHashSet.Compute.count</c> for reactive alternatives.
    /// </remarks>
    let count (set: SignalHashSet<'T>) = set.Value.Count
    module Compute =
        let containsValue (value: 'T) (set: SignalHashSet<'T>) =
            Signal.Computed(fun () -> set.Value.Contains(value))
        let contains (value: IReadOnlySignal<'T>) (set: SignalHashSet<'T>) =
            Signal.Computed(fun () ->
                let value = value.Value
                set.Value.Contains(value))
        let count (set: SignalHashSet<'T>) = Signal.Computed(fun () -> set.Value.Count)
        let values (set: SignalHashSet<'T>) = Signal.Computed(fun () -> seq set.Value)
        let map (f: 'T -> 'U) (set: SignalHashSet<'T>) =
            Signal.Computed(fun () ->
                set.Value
                |> Seq.map f)
        let choose (f: 'T -> 'U option) (set: SignalHashSet<'T>) =
            Signal.Computed(fun () ->
                set.Value
                |> Seq.choose f)
        let filter (f: 'T -> bool) (set: SignalHashSet<'T>) =
            Signal.Computed(fun () ->
                set.Value
                |> Seq.filter f)
        let toList (set: SignalHashSet<'T>) = Signal.Computed(fun () -> set.Value |> Seq.toList)
