/// Consumer-side evidence for the erased union representation used by the D2 investigation.
module Xantham.Generator.RunGate.ErasedCases

open Fable.Core
open Fable.Core.JsInterop

type private LeftCall =
    abstract left: int

    [<Emit("$0($1)")>]
    abstract Invoke: int -> int

type private RightCall =
    abstract right: string

    [<Emit("$0($1)")>]
    abstract Invoke: int -> int

[<RequireQualifiedAccess; Erase>]
type private Calls =
    | [<Fickl.Core.CaseTest("$0.left !== undefined")>] Left of LeftCall
    | [<Fickl.Core.CaseTest("$0.right !== undefined")>] Right of RightCall

    [<Emit("$0($1)")>]
    member _.Invoke(value: int) : int = jsNative

type private Constructor =
    [<EmitConstructor>]
    abstract Create: int -> obj

[<RequireQualifiedAccess; Erase>]
type private ConstructorOrText =
    | [<Fickl.Core.CaseTest("typeof $0 === 'function'")>] Constructor of Constructor
    | [<Fickl.Core.CaseTest("typeof $0 === 'string'")>] Text of string

[<RequireQualifiedAccess; Erase>]
type private Allocation =
    | [<Fickl.Core.CaseTest("$0 === $0")>] Values of obj array
    | Missing

let run check =
    let left: LeftCall = emitJsExpr () "Object.assign(x => x + 10, { left: 1 })"
    let right: RightCall = emitJsExpr () "Object.assign(x => x * 2, { right: 'twice' })"
    let wrapped = Calls.Right right
    check "erased case construction retains the callable value" (emitJsExpr (wrapped, right) "$0 === $1")
    check "shared Invoke calls either erased arm" (wrapped.Invoke 5 = 10 && (Calls.Left left).Invoke 5 = 15)

    let describe =
        function
        | Calls.Left value -> string (value.Invoke 3)
        | Calls.Right value -> value.right + string (value.Invoke 3)

    check "incoming untagged callable matches its left case" (describe (unbox left) = "13")
    check "incoming untagged callable matches its right case" (describe (unbox right) = "twice6")
    check "erased case testers use the predicate" (wrapped.IsRight && not wrapped.IsLeft)
    check "erased construction adds no tag" (emitJsExpr wrapped "Object.keys($0).join(',') === 'right'")
    check "repeated predicate input preserves allocation identity" (Allocation.Values [||]).IsValues

    let mutable evaluations = 0

    let next () =
        evaluations <- evaluations + 1
        Calls.Left left

    check "erased case tester evaluates its input once" ((next ()).IsLeft && evaluations = 1)

    let constructor: Constructor =
        emitJsExpr () "class { constructor(value) { this.value = value; } }"

    let wrappedConstructor = ConstructorOrText.Constructor constructor
    check "erased constructor case retains identity" (emitJsExpr (wrappedConstructor, constructor) "$0 === $1")

    match wrappedConstructor with
    | ConstructorOrText.Constructor value ->
        check "extracted erased constructor remains constructable" ((value.Create 7)?value = 7)
    | ConstructorOrText.Text _ -> check "constructor matched its case" false

    check "erased constructor predicate rejects a text arm" (not (ConstructorOrText.Text "text").IsConstructor)
