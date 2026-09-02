/// The run gate's checks: each one exercises a generated binding and reads the JavaScript
/// side back through `emitJsExpr` or a fixture runtime hook, so the claim under test is what
/// the erasure *did*, not what the F# type said. Node exits non-zero when any check failed,
/// after printing all of them.
module Xantham.Generator.RunGate.Program

open Fable.Core
open Fable.Core.JsInterop

let mutable private failures: string list = []
let mutable private passed = 0

let private check (claim: string) (condition: bool) =
    if condition then
        passed <- passed + 1
    else
        failures <- failures @ [ claim ]

let private equal (claim: string) (expected: 'T) (actual: 'T) =
    if expected = actual then
        passed <- passed + 1
    else
        failures <- failures @ [ $"{claim}: expected {expected}, got {actual}" ]

/// The JSON of a value as JavaScript sees it: the shape an erased type really has.
let private json (value: obj) : string = emitJsExpr value "JSON.stringify($0)"

/// `[<Global>]` reaches the global it names, and `[<Global>]` + `[<EmitConstructor>]` news the
/// global class rather than emitting the name as a call.
let private globals () =
    let root = GlobalsLab.Exports.registry
    equal "a global value binds to globalThis.registry" "root" root.label
    equal "and its optional member reads back as Some" (Some 3.0) root.size
    equal "a mutable global reads through [<Global>]" 41.0 GlobalsLab.Exports.counter
    equal "a global function is called by name" true (GlobalsLab.Exports.ping "up")
    equal "with its optional parameter passed through" false (GlobalsLab.Exports.ping ("up", 0.0))
    equal "and omitted when absent" false (GlobalsLab.Exports.ping "down")

    let widget = GlobalsLab.Widget.Create "w"
    equal "a ParamObject Create with an omitted optional is the bare literal" """{"label":"w"}""" (json widget)

    let gadget = GlobalsLab.Exports.Gadget widget

    check
        "[<Global; EmitConstructor>] constructs an instance of the global class"
        (emitJsExpr gadget "$0 instanceof globalThis.Gadget")

    equal "the constructor argument arrived" "w" gadget.widget.label
    let spun = gadget.spin(2.0).spin(3.0)
    equal "a `this`-returning method chains on the same instance" true (obj.ReferenceEquals(gadget, spun))
    equal "and the runtime saw both calls" 5.0 (emitJsExpr gadget "$0.turns")
    equal "a static on a global class reads off the global name" 9.0 GlobalsLab.Gadget.SPEED

/// `[<Import>]` bindings land on the module's exports; a tagged union case is the tagged object
/// the JavaScript side reads, and a tagged object built by JavaScript matches the F# case.
let private imports () =
    equal "an imported const reads its export" "0.1.0-lab" PhaseBLab.Exports.version
    equal "an imported overload picks the number arm" 3.0 (PhaseBLab.Exports.round 2.6)
    equal "and the string arm" "2.50" (PhaseBLab.Exports.round ("2.5", 2.0))

    equal
        "an imported object's members read through"
        1.0
        (PhaseBLab.Exports.defaults.duration
         |> Option.defaultValue 0.0
         |> fun d -> d / 1000.0)

    equal "a namespace re-export's members are callable" 1.0 (PhaseBLab.Exports.utils.clamp.Invoke(5.0, 0.0, 1.0))
    equal "a StringEnum member reads as its compiled name" (Some PhaseBLab.TimeUnit.Ms) PhaseBLab.Exports.defaults.unit

    let mutable ticks: (float * float option) list = []

    let options =
        PhaseBLab.TimerOptions.Create(
            labels = [| "a"; "b" |],
            duration = 5.0,
            unit = PhaseBLab.TimeUnit.S,
            onTick = System.Action<float, float option>(fun progress count -> ticks <- ticks @ [ progress, count ])
        )

    equal
        "a ParamObject Create with a StringEnum and a callback is the literal (the callback aside)"
        """{"labels":["a","b"],"duration":5,"unit":"s"}"""
        (json options)

    let timer = PhaseBLab.Exports.Timer options
    let timerClass: obj = import "Timer" "phase-b-lab"
    check "[<Import; EmitConstructor>] news the imported class" (emitJsExpr (timer, timerClass) "$0 instanceof $1")
    equal "a readonly property reads before play" 0.0 timer.progress
    let chained = timer.play().seek(2.0, true).tween [| 1.0; 2.0; 3.0 |]
    equal "chained methods return the instance" true (obj.ReferenceEquals(timer, chained))
    equal "a callback in the options object was invoked with the declared arguments" [ 1.0, Some 1.0 ] ticks

    equal
        "and every call arrived with the arguments the declaration promised - a rest parameter spread"
        """[["play"],["seek",2,true],["tween",1,2,3]]"""
        (emitJsExpr timer "JSON.stringify($0.calls)")

    let fresh = PhaseBLab.Exports.createTimer ()

    equal
        "an omitted optional parameter is not passed as undefined-shaped junk"
        true
        (emitJsExpr fresh "$0.options.duration === 1000")

    PhaseBLab.Exports.configure (PhaseBLab.ConfigureSettings.Create(fps = 60.0))
    // `configured` is a `let` export the runtime assigns: a live binding, read after the call.
    let settings: obj = import "configured" "phase-b-lab"
    equal "a synthesized ParamObject reaches the function as the literal" """{"fps":60}""" (json settings)

/// A class static reaches the property on the constructor object: not the constructor, not an
/// instance member of the same name, and - through a subclass - the base's static that
/// JavaScript inherits down the constructor chain.
let private statics () =
    equal "a const-like static reads off the constructor object" 100.0 StaticsLab.Counter.MAX
    equal "and a settable one reads the same way" 7.0 StaticsLab.Counter.tick
    equal "a static factory runs the static, not the constructor" 3.0 (StaticsLab.Counter.from 3.0).value
    equal "a static overload picks the number arm" 4.0 (StaticsLab.Counter.``of`` 4.0).value
    equal "and the string arm reaches the same JavaScript static" 4.0 (StaticsLab.Counter.``of`` "abcd").value
    equal "a subclass carries the static JavaScript inherits for it" 100.0 StaticsLab.Doubling.MAX
    equal "a static on a generic declaration is reachable once instantiated" 0.0 StaticsLab.Box<float>.EMPTY

    // `Clash.json` is the one collision F# admits: a static method beside an instance method of
    // the same name. Both have to reach their own JavaScript half.
    let clash = StaticsLab.Exports.Clash()
    equal "an instance method survives beside the static of its name" 42.0 (clash.json ())
    equal "and the static of that name reaches the constructor object's" 10.0 (StaticsLab.Clash.json 5.0).status

/// The one claim in the flags lab a compile gate cannot make: that F# `bigint` *is* the native
/// JavaScript `BigInt` after Fable's compile. The fixture runtime throws on anything whose
/// `typeof` is not `"bigint"`, so every call here is the assertion; `emitJsExpr` reads the
/// runtime's own verdict back for the values that never cross the boundary.
let private bigints () =
    let two = 2I
    check "an F# bigint literal is a native JavaScript BigInt" (emitJsExpr two "typeof $0 === \"bigint\"")

    // `total` reduces with `+` over `0n`, which TypeErrors on a mixed operand: a float that
    // merely printed like an integer would not survive the call, let alone sum correctly.
    equal "an array of bigint arrives as bigints and sums" 6I (FlagsLab.Exports.total [| 1I; 2I; 3I |])

    equal
        "a bigint round-trips through JavaScript with no float precision to lose"
        9007199254740993I
        (FlagsLab.Exports.total [| 9007199254740993I |])

    let ledger = FlagsLab.Exports.ledger 10I
    equal "a bigint-typed member reads back as the value JavaScript holds" 10I ledger.balance
    equal "and a bigint parameter reaches a method that adds it to one" 15I (ledger.credit 5I)
    check "what came back is still a BigInt, not a number" (emitJsExpr (ledger.balance) "typeof $0 === \"bigint\"")

    // The neighbours, at runtime: a template literal really is a string on the way in and out,
    // and `symbol` really is a symbol - which is why `obj` is where the binding had to stop.
    equal
        "a template literal parameter is a string JavaScript can call string methods on"
        "onsave"
        (FlagsLab.Exports.normalize "save")

    equal "and an intrinsic mapping is the string its transform produced" "LOUD" (FlagsLab.Exports.shout "loud")

    equal
        "the value behind the `unique symbol` binding is a real symbol"
        "symbol"
        (FlagsLab.Exports.describe FlagsLab.Exports.brandTag)

/// A constructor object is an interface of its own, and `[<EmitConstructor>]` on an *abstract*
/// member `new`s the object the member was read off (§4.4) - so a `typeof X` member is a
/// working constructor rather than an `obj` the consumer has to escape out of.
let private constructorObjects () =
    // The `interface` + `declare const` spelling: the export's own value type is the object.
    let widget = CtorLab.Exports.Widget.Create "hello"
    let widgetClass: obj = import "Widget" "ctor-lab"

    check
        "EmitConstructor on an abstract member news the object it is read off"
        (emitJsExpr (widget, widgetClass) "$0 instanceof $1")

    equal "the constructor argument arrived" "hello" widget.label
    equal "and the instance side still reaches its own JavaScript" "hello:2" (widget.resize 2.0).label
    equal "a property of the constructor object is the class's static" "widget" CtorLab.Exports.Widget.DEFAULT_LABEL

    // `typeof Gauge` at a member position - the construct the whole ServiceWorkerGlobalScope
    // constructor table is made of. `$0` is `scope.Gauge`, so this is `new scope.Gauge(3)`.
    let gauge = CtorLab.Exports.scope.Gauge.Create 3.0
    let gaugeClass: obj = import "Gauge" "ctor-lab"
    check "a typeof member news the class it names" (emitJsExpr (gauge, gaugeClass) "$0 instanceof $1")
    equal "with the argument the declaration promised" 3.0 gauge.size
    equal "and the class's static reads off the same object" "px" CtorLab.Exports.scope.Gauge.UNIT

    // An interface whose only members are construct signatures, generic and overloaded.
    equal "a generic construct signature news with its argument" "hi" (CtorLab.Exports.parcels.Create "hi").value
    equal "and the nullary overload reaches the same constructor" "empty" (CtorLab.Exports.parcels.Create()).value

let private taggedUnions () =
    let circle = PhaseBLab.Shape.Circle 2.0
    equal "a tagged-union case erases to the tagged object" """{"kind":"circle","radius":2}""" (json circle)
    equal "and JavaScript reads it off the tag" (System.Math.PI * 4.0) (PhaseBLab.Exports.area circle)
    let rect = PhaseBLab.Shape.RoundRect(2.0, 3.0, 1.0)

    equal
        "a multi-field case carries its CompiledName tag"
        """{"kind":"round-rect","width":2,"height":3,"radius":1}"""
        (json rect)

    equal "and the JavaScript side agrees on its arm" 6.0 (PhaseBLab.Exports.area rect)

    match PhaseBLab.Exports.makeRoundRect (4.0, 5.0, 0.5) with
    | PhaseBLab.Shape.RoundRect(width, height, radius) ->
        equal
            "a JavaScript-built tagged object matches the F# case with its fields"
            (4.0, 5.0, 0.5)
            (width, height, radius)
    | PhaseBLab.Shape.Circle radius -> check $"a JavaScript-built round-rect matched Circle {radius}" false

[<EntryPoint>]
let main _ =
    globals ()
    imports ()
    statics ()
    bigints ()
    constructorObjects ()
    taggedUnions ()

    match failures with
    | [] ->
        printfn $"run gate: {passed} checks passed"
        0
    | failed ->
        for claim in failed do
            eprintfn $"run gate FAILED: {claim}"

        eprintfn $"run gate: {failed.Length} of {passed + failed.Length} checks failed"
        1
