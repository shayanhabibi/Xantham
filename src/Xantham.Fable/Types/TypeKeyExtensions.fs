[<AutoOpen>]
module Xantham.Fable.Types.TypeKeyExtensions

open Xantham
open TypeScript
open Fable.Core

type Ts.Type with
    [<EmitProperty "id">]
    member inline this.TypeKey: TypeKey = jsNative

type TypeKey with
    member inline this.Value: int = unbox this
    static member inline String = TypeKey -1 // TypeKindPrimitive.String.TypeKey // -1
    static member inline Int = TypeKey -2// TypeKindPrimitive.Integer.TypeKey // -2
    static member inline Float = TypeKey -3//TypeKindPrimitive.Number.TypeKey // -3
    static member inline Bool = TypeKey -4//TypeKindPrimitive.Boolean.TypeKey // -4
    static member inline Null = TypeKey -5//TypeKindPrimitive.Null.TypeKey // -5
    static member inline Unit = TypeKey -6
    static member inline Any = TypeKey -7
    static member inline Undefined = TypeKey -8
    static member inline Void = TypeKey -9
    static member inline Object = TypeKey -10
    static member inline Symbol = TypeKey -11
    static member inline Never = TypeKey -12
    static member inline Discard = TypeKey -13
    static member inline Unknown = TypeKey -14
    static member inline TemplateLiteral = TypeKey -15
    static member inline ConstructorType = TypeKey -16

module TypeKey =
    /// <summary>
    /// A counter for generating unique TypeKeys.
    /// </summary>
    let mutable private keyNum = TypeKey.ConstructorType.Value - 1
    /// <summary>
    /// Performs a thunk with the <c>keyNum</c> counter, returns the result, and decrements the counter.
    /// </summary>
    /// <param name="thunk"></param>
    let inline private withKeyNumThenDecr (thunk: int -> 'T) =
        let key = thunk keyNum
        keyNum <- keyNum - 1
        key
    let createWith = TypeKey
    /// <summary>
    /// Creates a unique generated typekey.
    /// </summary>
    let create() = withKeyNumThenDecr TypeKey
