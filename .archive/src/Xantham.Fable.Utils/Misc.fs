[<AutoOpen>]
module Xantham.Fable.AutoOpenUtils

open Fable.Core

/// <summary>
/// Apply args to a function. Used for sugar in long pipe chains.
/// <example><code lang="fsharp">
/// fn |> funApply args
/// // Same as
/// fn args
/// 
/// // Example use case
/// (* Some calc *)
/// |> (+) 1
/// |> Array.skip
/// |> funApply arr
/// // Same as
/// (* Some calc *)
/// |> (+) 1
/// |> fun count -> array.skip count arr
/// </code></example>
/// </summary>
/// <param name="args"></param>
/// <param name="fn"></param>
let inline funApply args fn = fn args
let inline funApply2 arg1 arg2 fn = fn arg1 arg2
let inline funApply3 arg1 arg2 arg3 fn = fn arg1 arg2 arg3

[<ImportDefault("node:util")>]
type Utils =
    [<CompiledName("inspect"); ParamObject(1)>]
    static member inline Inspect(object: obj, ?showHidden: bool, ?depth: int, ?colors: bool, ?customInspect: bool, ?showProxy: bool, ?maxArrayLength: int, ?maxStringLength: int, ?breakLength: int, ?compact: int, ?sorted: bool, ?getters: bool, ?numericSeparator: bool): string = JS.undefined

module Utils =
    let inspectTo (depth: int) o = Utils.Inspect(o, depth = depth, colors = true, numericSeparator = true)
    let inspect o = Utils.Inspect(o, colors = true, numericSeparator = true)
    let traceTo (depth: int) o = JS.console.dir(o, {| depth = depth; colors = true |}) 
    let traceInf o = JS.console.dir(o, {| depth = null; colors = true |})
    let trace o = traceTo 3 o
    
    
type System.Collections.Generic.List<'T> with
    [<Emit "$0">]
    member inline this.AsArray: 'T array = unbox this

[<RequireQualifiedAccess>]
module Seq =
    let apply (voidFun: 'a -> unit) (sequence: 'T when 'T :> 'a seq): 'T =
        for x in sequence do voidFun x
        sequence
    let revApply (voidFun: 'a -> unit) (sequence: 'T when 'T :> 'a seq): 'T =
        if Seq.length sequence > 0
        then for x in [ Seq.length sequence - 1 .. 0 ] do voidFun (Seq.item x sequence)
        sequence

[<RequireQualifiedAccess>]
module Array =
    let apply voidFun (arr: _ array) =
        for x in arr do voidFun x
        arr
    let revApply voidFun (arr: _ array) =
        if arr.Length > 0
        then for x in [ arr.Length - 1 .. 0 ] do voidFun arr[x]
        arr
    
[<Erase>]
type NonEmptyArray<'T> = private NonEmptyArray of obj array with
    [<Emit "$0">]
    member inline this.Values = unbox<'T array> this
    member inline this.Value = Array.head this.Values
    member inline this.Length = this.Values.Length
    static member inline op_Implicit(this: NonEmptyArray<'T>): 'T seq = this.Values
    interface System.Collections.IEnumerable with
        [<Emit("$0")>]
        member this.GetEnumerator() = this.Values.GetEnumerator()
    interface System.Collections.Generic.IEnumerable<'T> with
        [<Emit("$0")>]
        member this.GetEnumerator() = this.Values.GetEnumerator() :?> System.Collections.Generic.IEnumerator<'T>
    
module NonEmptyArray =
    let inline map (fn: 'T -> 'U) (this: NonEmptyArray<'T>): NonEmptyArray<'U> = Array.map fn this.Values |> unbox
    let inline vcreate (values: 'T seq) =
        match Seq.toArray values with
        | [||] -> ValueNone
        | arr -> unbox<NonEmptyArray<'T>> arr |> ValueSome
    let inline create (values: 'T seq) =
        vcreate values
        |> ValueOption.toOption
    let inline choose (fn: 'T -> 'U option) (this: NonEmptyArray<'T>): NonEmptyArray<'U> option =
        this.Values |> Array.choose fn |> create
    let inline tryPick (fn: 'T -> 'U option) (this: NonEmptyArray<'T>): 'U option =
        this.Values |> Array.tryPick fn
    let inline iter (fn: 'T -> unit) (this: NonEmptyArray<'T>) = this.Values |> Array.iter fn
    let inline filter (fn: 'T -> bool) (this: NonEmptyArray<'T>) = this.Values |> Array.filter fn |> create
    let inline distinct (this: NonEmptyArray<'T>) = this.Values |> Array.distinct |> unbox<NonEmptyArray<'T>>
    let inline distinctBy (fn: 'T -> 'a when 'a:equality) (this: NonEmptyArray<'T>) : NonEmptyArray<'T> = this.Values |> Array.distinctBy fn |> unbox<NonEmptyArray<'T>>
    let inline popHead (this: NonEmptyArray<'T>) = this.Value, this.Values |> Array.tail |> create
    let inline head (this: NonEmptyArray<'T>) = this.Value
    let inline tail (this: NonEmptyArray<'T>) = this.Values |> Array.tail |> create
    let inline appendOne (value: 'T) (this: NonEmptyArray<'T>) = this.Values |> Array.append [| value |] |> unbox<NonEmptyArray<'T>>
    let inline addOne (value: 'T) (this: NonEmptyArray<'T>) = unbox<ResizeArray<'T>> this |> _.Add(value)
    let inline singleton (value: 'T) = [| value |] |> unbox<NonEmptyArray<'T>>
    let inline append (this: NonEmptyArray<'T>) (other: NonEmptyArray<'T>) = this.Values |> Array.append other.Values |> unbox<NonEmptyArray<'T>>
    let inline length (this: NonEmptyArray<'T>) = this.Values.Length
    let inline exists predicate (this: NonEmptyArray<'T>) = this.Values |> Array.exists predicate
    let inline collectArrays mapping (this: NonEmptyArray<'T>) = this.Values |> Array.collect mapping |> create
    let collect (mapping: 'T -> 'U NonEmptyArray option) (this: NonEmptyArray<'T>) = create [|
        for value in this.Values do
            match mapping value with
            | Some arr -> yield! arr.Values
            | None -> ()
    |]
    let inline apply (fn: 'T -> unit) (this: NonEmptyArray<'T>): NonEmptyArray<'T> =
        this.Values |> Array.iter fn
        this
    let inline revApply (fn: 'T -> unit) (this: NonEmptyArray<'T>): NonEmptyArray<'T> =
        if this.Values.Length > 0
        then for x in [ this.Values.Length - 1 .. 0 ] do fn this.Values[x]
        this
    
    
    
[<RequireQualifiedAccess>]
module String =
    let replace (oldValue: string) (newValue: string) (input: string) =
        input.Replace(oldValue, newValue)

    let inline remove (character: char) (text: string) = text.Trim(character)
    module remove =
        let singleQuote = remove '''
        let doubleQuote = remove '"'

    module Casing =
        let capitalize (text: string) =
            (string text[0]).ToUpper() + text[1..]
        let lowerAll (text: string) = text.ToLower()
        let lowerFirst (text: string) = (string text[0]).ToLower() + text[1..]

    let splitLines (text: string) =
        JS.Constructors
            .RegExp.Create(
                "\r\n|\r|\n"
                ).Split(text)

    let normalizePath = replace "\\" "/"

[<RequireQualifiedAccess>]
module Enum =
    let inline mask (flag: ^T when ^T :> System.Enum) = (&&&) flag
    let inline hasFlag (flagToCheck: ^T when ^T :> System.Enum) (input: ^T when ^T:> System.Enum) =
        input.HasFlag flagToCheck
    let inline hasMask (mask: ^T when ^T :> System.Enum) (input: ^T when ^T:> System.Enum) =
        mask &&& input |> (<>) (enum 0)
    /// <summary>
    /// Checks if the input enum has the given flag.
    /// </summary>
    /// <param name="flagToCheck"></param>
    /// <param name="input"></param>
    let inline (|HasFlag|_|) (flagToCheck: ^T when ^T :> System.Enum) (input: ^T when ^T:> System.Enum) = hasFlag flagToCheck input
    /// <summary>
    /// Checks if the input enum has any matches with the given mask.
    /// </summary>
    /// <param name="mask"></param>
    /// <param name="input"></param>
    let inline (|HasMatch|_|) (mask: ^T when ^T :> System.Enum) (input: ^T when ^T:> System.Enum) = hasMask mask input
    let inline (|Match|_|) (mask: ^T when ^T :> System.Enum) (input: ^T when ^T:> System.Enum) =
        let match' = mask &&& input
        if match' <> enum 0 then Some match' else None