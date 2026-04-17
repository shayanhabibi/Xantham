namespace FSharp.SignalsDotnet

open System.Collections.Generic
open System.ComponentModel
open SignalsDotnet

type EventSignal = ISignal<R3.Unit>
type Signal<'T> = SignalsDotnet.Signal<'T>
type IReadOnlySignal<'T> = SignalsDotnet.IReadOnlySignal<'T>
type ISignal<'T> = SignalsDotnet.ISignal<'T>
type Effect = SignalsDotnet.Effect
type DictionarySignal<'K, 'V> = SignalsDotnet.DictionarySignal<'K, 'V>

module Effect =
    let inline create ([<InlineIfLambda>] fn: unit -> unit) = new Effect(fn)
    let inline batch ([<InlineIfLambda>] fn: unit -> unit) = Effect.AtomicOperation(fn)
    let inline createBatch ([<InlineIfLambda>] fn: unit -> unit) = create(fun () -> batch fn)
    let inline dispose (effect: Effect) = effect.Dispose()
    
    let inline doWith ([<InlineIfLambda>] fn: unit -> unit) =
        use effect = create fn
        effect
        
module Signal =
    let inline batch ([<InlineIfLambda>] fn: unit -> unit) = Effect.AtomicOperation(fn)
    let inline effect ([<InlineIfLambda>] fn: unit -> unit) = new Effect(fn)
    
    let createEvent(): EventSignal = Signal.CreateEvent()
    let track (eventSignal: EventSignal) = eventSignal.Track()
    let trigger (eventSignal: EventSignal) = eventSignal.Invoke()
    
    let inline init<'T> = Signal<'T>()
    let inline create (value: 'T) = Signal(value)
    let inline compute ([<InlineIfLambda>] fn: unit -> 'T): IReadOnlySignal<'T> = Signal.Computed(fn)
    let inline computeValue (value: 'T): IReadOnlySignal<'T> = Signal.Computed(fun () -> value)
    let inline linkWith ([<InlineIfLambda>] fn: unit -> 'T): ISignal<'T> = Signal.Linked(fn)
    let inline linkWithValue (value: 'T): ISignal<'T> = Signal.Linked(fun () -> value)
    let inline set (value: 'T) (signal: ISignal<'T>) =
        signal.Value <- value
        signal
        
    let inline map ([<InlineIfLambda>] fn: 'T -> 'U) (signal: IReadOnlySignal<'T>) = Signal.Computed(fun () -> fn signal.Value)
    let inline link ([<InlineIfLambda>] fn: 'T -> 'U) (signal: IReadOnlySignal<'T>) = Signal.Linked(fun () -> fn signal.Value)
    
    let inline map2
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>) =
        Signal.Computed(fun () -> fn signal1.Value signal2.Value)
        
    let inline map3
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V -> 'W)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>)
        (signal3: IReadOnlySignal<'V>) =
        Signal.Computed(fun () -> fn signal1.Value signal2.Value signal3.Value)
        
    let inline map4
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V -> 'W -> 'X)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>)
        (signal3: IReadOnlySignal<'V>)
        (signal4: IReadOnlySignal<'W>) =
        Signal.Computed(fun () -> fn signal1.Value signal2.Value signal3.Value signal4.Value)
        
    let inline link2
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>) =
        Signal.Linked(fun () -> fn signal1.Value signal2.Value)
    let inline link3
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V -> 'W)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>)
        (signal3: IReadOnlySignal<'V>) =
        Signal.Linked(fun () -> fn signal1.Value signal2.Value signal3.Value)
    let inline link4
        ([<InlineIfLambda>] fn: 'T -> 'U -> 'V -> 'W -> 'X)
        (signal1: IReadOnlySignal<'T>)
        (signal2: IReadOnlySignal<'U>)
        (signal3: IReadOnlySignal<'V>)
        (signal4: IReadOnlySignal<'W>) =
        Signal.Linked(fun () -> fn signal1.Value signal2.Value signal3.Value signal4.Value)

module DictionarySignal =
    let inline tryAdd (key: 'K) (value: 'V) (dict: IDictionary<'K, 'V>) =
        dict.TryAdd(key, value) |> ignore
        dict
    let inline tryGet (key: 'K) (dict: IDictionary<'K, 'V>) =
        match dict.TryGetValue(key) with
        | true, value -> Some value
        | _ -> None
    let inline tryGetV (key: 'K) (dict: IDictionary<'K, 'V>) =
        match dict.TryGetValue(key) with
        | true, value -> ValueSome value
        | _ -> ValueNone
    let inline contains (key: 'K) (dict: IDictionary<'K, 'V>) = dict.ContainsKey(key)
    let inline tryRemove (key: 'K) (dict: IDictionary<'K, 'V>) = dict.Remove(key) |> ignore; dict
    let inline getOrAdd (key: 'K) (value: 'V) (dict: IDictionary<'K, 'V>) =
        tryGetV key dict
        |> ValueOption.defaultWith(fun () -> 
            tryAdd key value dict
            |> ignore
            value)
    let inline getOrAddWith (key: 'K) (value: 'K -> 'V) (dict: IDictionary<'K, 'V>): 'V =
        tryGetV key dict
        |> ValueOption.defaultWith(fun () ->
            let value = value key
            tryAdd key value dict
            |> ignore
            value)
    let inline getOrAddWithThen (key: 'K) (value: 'K -> 'V) (thenThunk: 'K ->  unit) (dict: IDictionary<'K, 'V>) =
        tryGetV key dict
        |> ValueOption.defaultWith(fun () ->
            let value = value key
            tryAdd key value dict
            |> ignore
            thenThunk key
            value)
    let inline getOrDefault (key: 'K) (defaultValue: 'V) (dict: IDictionary<'K, 'V>) =
        tryGetV key dict
        |> ValueOption.defaultValue defaultValue
    let inline addOrReplace (key: 'K) (value: 'V) (dict: IDictionary<'K, 'V>) =
        dict[key] <- value
    let inline init<'K, 'V> = DictionarySignal<'K, 'V>()