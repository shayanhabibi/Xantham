module Xantham.Generator.RunGate.RecursiveCallbacks

open Fable.Core
open Fable.Core.JsInterop
open NestedNameLab.Recursive

let run check =
    let mutable next: Value = U2.Case1 "done"

    let thunk =
        { new Thunk with
            member _.Invoke() = next
        }

    check "recursive callable is a JavaScript function" (emitJsExpr thunk "typeof $0 === 'function'")
    check "recursive callable returns its leaf" (emitJsExpr (thunk.Invoke()) "$0 === 'done'")
    next <- U2.Case2 thunk
    check "recursive union retains function identity" (emitJsExpr (thunk, thunk.Invoke()) "$0 === $1")
    check "JavaScript invokes the recursive callable directly" (emitJsExpr thunk "$0() === $0")

    let generic =
        { new GenericThunk<int> with
            member _.Invoke() = U2.Case1 42
        }

    check "generic recursive callback preserves its argument type" (emitJsExpr (generic.Invoke()) "$0 === 42")

    let imported: Thunk = emitJsExpr () "(() => 'native')"
    check "native function implements the recursive callback" (emitJsExpr (imported.Invoke()) "$0 === 'native'")
