module Xantham.Generator.RunGate.GeneratedErasedCases

open Fable.Core.JsInterop
open ErasedExclusiveLab

let run check =
    let left = Exports.left ()
    let right = Exports.right ()
    let wrapped = Calls.RightCall right
    check "generated erased case preserves identity" (emitJsExpr (wrapped, right) "$0 === $1")

    check
        "generated erased case adds no properties"
        (emitJsExpr wrapped "Object.keys($0).sort().join(',') === 'left,right'")

    check "generated shared Invoke calls both arms" (wrapped.Invoke 5. = 10. && (Calls.LeftCall left).Invoke 5. = 15.)
    check "generated predicates reject explicit undefined" (wrapped.IsRightCall && not wrapped.IsLeftCall)

    let describe =
        function
        | Calls.LeftCall value -> value.Invoke 3.
        | Calls.RightCall value -> value.Invoke 3.

    check "incoming callable matches left" (describe (Exports.incomingLeft ()) = 13.)
    check "incoming callable matches right" (describe (Exports.incomingRight ()) = 6.)
    let data = RightData.Create "data"
    let mixed = Mixed.RightData data

    match mixed with
    | Mixed.RightData value -> check "mixed data arm preserves identity" (emitJsExpr (value, data) "$0 === $1")
    | Mixed.LeftCall _ -> check "mixed data arm matches" false

    let text: TextCall = emitJsExpr () "Object.assign(x => x + '!', { right: 'text' })"

    match DifferentCalls.TextCall text with
    | DifferentCalls.TextCall value -> check "distinct signature remains on its arm" (value.Invoke "hello" = "hello!")
    | DifferentCalls.LeftCall _ -> check "text callable matches" false

    let constructor = Exports.``constructor`` ()

    match constructor with
    | Constructors.RightConstructor value ->
        check "generated constructor extraction preserves identity" (emitJsExpr (value, constructor) "$0 === $1")
        check "generated constructor remains constructable" ((value.Create 7.).value = 7.)
    | Constructors.LeftConstructor _ -> check "incoming constructor matches" false

    let generic: Generic<string> = Exports.generic ()
    let genericMethod = Exports.genericMethod ()

    check
        "shared generic call binds its own parameter"
        (genericMethod.Invoke "hello" = "hello" && genericMethod.Invoke 42 = 42)

    check "shared widened overload remains callable" ((Exports.overloaded ()).Invoke "b" = 2.)
    check "generic alias application retains shared Invoke" (generic.Invoke "hello" = "hello!")

    match generic with
    | Generic.RightGeneric value -> check "generic arm retains Invoke" (value.Invoke "arm" = "arm!")
    | Generic.LeftGeneric _ -> check "generic arm matches" false

    let escaped = Exports.escaped ()
    check "escaped property predicates preserve dollar placeholders" (escaped.IsLeft0 && not escaped.IsRight)

    match escaped with
    | Escaped.Left0 value -> check "escaped arm remains callable" (value.Invoke 4. = 5.)
    | Escaped.Right _ -> check "escaped arm matches" false
