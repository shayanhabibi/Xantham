/// The run gate's checks: each one exercises a generated binding and reads the JavaScript
/// side back through `emitJsExpr` or a fixture runtime hook, so the claim under test is what
/// the erasure *did*, not what the F# type said. Node exits non-zero when any check failed,
/// after printing all of them.
module Xantham.Generator.RunGate.Program

open System
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

    // The write half of the same binding. `Exports` carries `[<Global("globalThis")>]`, so the
    // assignment is `globalThis.counter = 55` rather than the call `counter(55)`.
    GlobalsLab.Exports.counter <- 55.0
    equal "and an assignment lands on globalThis" 55.0 (emitJsExpr () "globalThis.counter")
    equal "which the binding reads back" 55.0 GlobalsLab.Exports.counter
    GlobalsLab.Exports.counter <- 41.0
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
            onTick = PhaseBLab.TickCallback(fun progress count -> ticks <- ticks @ [ progress, count ])
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

    PhaseBLab.Exports.configure (PhaseBLab.Configure.Settings.Create(fps = 60.0))
    // `configured` is a `let` export the runtime assigns: a live binding, read after the call.
    let settings: obj = import "configured" "phase-b-lab"
    equal "a synthesized ParamObject reaches the function as the literal" """{"fps":60}""" (json settings)

/// A class static reaches the property on the constructor object: not the constructor, not an
/// instance member of the same name, and - through a subclass - the base's static that
/// JavaScript inherits down the constructor chain.
let private statics () =
    equal "a const-like static reads off the constructor object" 100.0 StaticsLab.Counter.MAX
    equal "and a settable one reads the same way" 7.0 StaticsLab.Counter.tick

    // The declaration carries `[<Import("Counter", "statics-lab")>]`, so the assignment is
    // `Counter.tick = 12` on the constructor object itself. A per-member `[<Import>]` would
    // compile it to the call `Counter.tick(12)` and TypeError here.
    let counterClass: obj = import "Counter" "statics-lab"
    StaticsLab.Counter.tick <- 12.0
    equal "a settable static writes the property on the constructor object" 12.0 (unbox<float> counterClass?tick)
    equal "which the binding reads back" 12.0 StaticsLab.Counter.tick
    equal "and the subclass sees the write JavaScript inherits for it" 12.0 StaticsLab.Doubling.tick
    StaticsLab.Counter.tick <- 7.0
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

/// §4.4's is-a relation after erasure. F# proves the `inherit` legal; only running it proves the
/// relation costs nothing - that an upcast to an inherited base is the identical object, that the
/// members the flattening redeclared are all still on it, and that a class whose F# type gained an
/// `inherit` keeps the JavaScript prototype chain it always had.
let private heritage () =
    let derived = InheritLab.Derived.Create(extra = true, name = "leaf", at = 2.0)

    equal
        "a Create on an inheriting interface still emits every member, inherited ones included"
        """{"extra":true,"name":"leaf","at":2}"""
        (json derived)

    let asBase = derived :> InheritLab.Base
    equal "an upcast to the inherited base reads the base's member" "leaf" asBase.name
    check "and it is the same object: both interfaces are erased" (emitJsExpr (derived, asBase) "$0 === $1")

    let both = InheritLab.Both.Create(label = "x", volume = 1.0, pitch = 2.0)
    equal "a diamond upcasts down one arm" 1.0 (both :> InheritLab.Loud).volume
    equal "and down the other" 2.0 (both :> InheritLab.Pitched).pitch

    let tagged = InheritLab.Tagged.Create(tag = "t", value = "v")
    equal "a generic base upcasts at the argument the inherit applied" "v" (tagged :> InheritLab.Box<string>).value

    let leaf = InheritLab.Exports.Leaf 3.0
    let nodeClass: obj = import "Node" "inherit-lab"
    equal "an EmitConstructor subclass ran its base constructor" 3.0 leaf.id

    check
        "and is an instance of the JavaScript base its F# type inherits"
        (emitJsExpr (leaf, nodeClass) "$0 instanceof $1")

    equal "which the F# upcast agrees with" 3.0 (leaf :> InheritLab.Node).id

/// A declaration nested in a module under the owner that reaches it (§4.4, wave six lane AD).
/// The nesting is an F# spelling and nothing else: the JavaScript object crossing the boundary
/// carries no trace of it, and a nested StringEnum is still the bare string its `CompiledName`
/// names.
let private nestedNames () =
    let retry =
        NestedNameLab.Widget.Options.Retry.Create(3.0, NestedNameLab.Widget.Options.Retry.Backoff.Linear)

    equal
        "a ParamObject Create inside two nested modules is still the bare object literal"
        """{"attempts":3,"backoff":"linear"}"""
        (json retry)

    equal "and the module path reaches JavaScript as nothing at all" "linear" (NestedNameLab.Exports.backoffOf retry)

    let built = NestedNameLab.Exports.defaultRetry ()
    equal "a JavaScript-built object reads through the nested declaration" 3.0 built.attempts

    equal
        "including a StringEnum three modules deep"
        NestedNameLab.Widget.Options.Retry.Backoff.Exponential
        built.backoff

    // The owner refers forward into the module holding its inline shape, and a `ParamObject`
    // Create at that depth is still a bare object literal. Wave six lane AD proposed the form;
    // this is the golden it landed as.
    let options = NestedNameLab.Widget.Options.Create(retry = retry, label = "w")
    let metrics = NestedNameLab.Widget.Metrics.Create(hits = 1.0)
    let widget = NestedNameLab.Widget.Create(options = options, metrics = metrics)

    equal
        "a nested module's type is the object literal its Create built"
        """{"retry":{"attempts":3,"backoff":"linear"},"label":"w"}"""
        (json options)

    equal
        "and the owner carries it under the property the reference named"
        """{"options":{"retry":{"attempts":3,"backoff":"linear"},"label":"w"},"metrics":{"hits":1}}"""
        (json widget)

    NestedNameLab.Exports.configure (NestedNameLab.Configure.Settings.Create true)

    equal
        "an import binds under a nested parameter type"
        true
        (emitJsExpr () "globalThis.__nestedNameLabConfigure.verbose")

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

/// The workarounds of docs/fable5-workarounds.md, each read back through
/// `tests/fixtures/fable-workaround-lab/index.js`. Where the document says the direct F#
/// spelling misbehaves, the misbehaviour is checked beside the workaround: a workaround is worth
/// what the failure it avoids is worth, and only running both settles which is which. The FABLE
/// "Cannot type test (evals to false)" warnings this module produces are that failure, reported
/// by the compiler that causes it.
let private workarounds () =
    // 1. An erased union over two interface arms. Fable can type-test a primitive and cannot
    // type-test an erased interface, so the `Ok` test folds to `false` and the match collapses
    // to its other branch - unconditionally, for every value.
    let asOk = FableWorkaroundLab.Exports.run false

    let branch =
        match asOk with
        | U2.Case1 _ -> "Err"
        | U2.Case2 _ -> "Ok"

    equal "a U2 over two interfaces matches one arm whatever the value is" "Err" branch

    let readOutcome (outcome: FableWorkaroundLab.Outcome) =
        if emitJsExpr outcome "\"value\" in $0" then
            Choice1Of2(unbox<FableWorkaroundLab.Ok> outcome)
        else
            Choice2Of2(unbox<FableWorkaroundLab.Err> outcome)

    match readOutcome (FableWorkaroundLab.Exports.run false) with
    | Choice1Of2 ok -> equal "discriminating in JavaScript reaches the arm the value has" "yes" ok.value
    | Choice2Of2 _ -> check "discriminating in JavaScript reaches the Ok arm" false

    match readOutcome (FableWorkaroundLab.Exports.run true) with
    | Choice2Of2 err -> equal "and the other arm" "no" err.reason
    | Choice1Of2 _ -> check "discriminating in JavaScript reaches the Err arm" false

    // 2. `:?` against an interface this run declares. The emitted object carries no F# type, so
    // the test is a compile-time `false` however the value was built.
    let shapes = FableWorkaroundLab.Exports.shapes ()

    check
        "a downcast to an inheriting interface is false for a value that is one"
        (not (shapes[1] :? FableWorkaroundLab.Circle))

    let asCircle (shape: FableWorkaroundLab.Shape) : FableWorkaroundLab.Circle option =
        if emitJsExpr shape "\"radius\" in $0" then
            Some !!shape
        else
            None

    equal "narrowing on the member the extension adds reaches it" (Some 2.0) (asCircle shapes[1] |> Option.map _.radius)
    equal "and refuses a value that only satisfies the base" None (asCircle shapes[0] |> Option.map _.radius)

    // 3. A settable static and a mutable global, both writable through the binding: the
    // attribute sits on the declaration, and Fable compiles `<-` under it as a property write.
    equal "a settable static reads through the binding" 100.0 FableWorkaroundLab.Budget.limit
    let budgetClass: obj = import "Budget" "fable-workaround-lab"
    FableWorkaroundLab.Budget.limit <- 250.0
    equal "and an assignment reaches the constructor object it is read off" 250.0 (unbox<float> budgetClass?limit)
    equal "which the binding reads back" 250.0 FableWorkaroundLab.Budget.limit
    FableWorkaroundLab.Budget.limit <- 100.0

    GlobalsLab.Exports.counter <- 55.0
    equal "a mutable global is written the same way, through globalThis" 55.0 (emitJsExpr () "globalThis.counter")
    GlobalsLab.Exports.counter <- 41.0

    // 4. `string | null` and `value?: string` are the same F# type, and `None` is `undefined`.
    let slots = FableWorkaroundLab.Exports.slots ()
    equal "a present string reads as Some" (Some "a") slots[0].value
    equal "an explicit null reads as None" None slots[1].value
    equal "and an absent property reads as None too" None slots[2].value
    equal "though JavaScript still tells the two apart" "null" (FableWorkaroundLab.Exports.describe slots[1])
    equal "as it does the absent one" "absent" (FableWorkaroundLab.Exports.describe slots[2])

    equal
        "None omits the property rather than writing null"
        "absent"
        (FableWorkaroundLab.Exports.describe (FableWorkaroundLab.Slot.Create()))

    // The value that is `None` to F# and `null` to JavaScript: the only thing that reaches a
    // declaration whose type is `string | null` with the null it declares.
    let asNull: string option = emitJsExpr () "null"

    equal
        "a null passed through the option reaches the property as null"
        "null"
        (FableWorkaroundLab.Exports.describe (FableWorkaroundLab.Slot.Create(?value = asNull)))

    let explicitNull: FableWorkaroundLab.Slot = !! createObj [ "value" ==> (null: obj) ]

    equal
        "and a hand-built literal is the same, for a member with no Create"
        "null"
        (FableWorkaroundLab.Exports.describe explicitNull)

    equal "which F# still reads back as None" None explicitNull.value

    // 5. Implementing a generated interface. An object expression is a class instance: the
    // members sit on the prototype, so JavaScript that enumerates or serialises finds nothing.
    let viaObjectExpression =
        { new FableWorkaroundLab.Listener with
            member _.name
                with get () = "oe"
                and set _ = ()

            member _.notify count = $"oe:{count}"
        }

    equal
        "an object expression carries no own enumerable property"
        "{}||oe:1"
        (FableWorkaroundLab.Exports.invite viaObjectExpression)

    let viaLiteral: FableWorkaroundLab.Listener =
        !!{|
            name = "lit"
            notify = System.Func<float, string>(fun count -> $"lit:{count}")
        |}

    equal
        "an anonymous record is the plain object the declaration described"
        """{"name":"lit"}|name,notify|lit:1"""
        (FableWorkaroundLab.Exports.invite viaLiteral)

    // Wave four lane O: `Listener.notify` is carried into Create as a delegate parameter, so the
    // literal above is reached under the typechecker rather than through the unchecked cast.
    let viaCreate =
        FableWorkaroundLab.Listener.Create("cr", (fun count -> $"cr:{count}"))

    equal
        "a Create carrying a method is that same plain object, type-checked"
        """{"name":"cr"}|name,notify|cr:1"""
        (FableWorkaroundLab.Exports.invite viaCreate)

    equal "and the delegate is reachable as the method it stands for" "cr:2" (viaCreate.notify 2.0)

    // 6. Equality. `=` on a generated interface is Fable's structural `equals`, which walks the
    // JavaScript object.
    let first = FableWorkaroundLab.Exports.fresh ()
    let second = FableWorkaroundLab.Exports.fresh ()
    check "= holds between two distinct objects with the same fields" (first = second)
    check "identity is obj.ReferenceEquals, and it separates them" (not (obj.ReferenceEquals(first, second)))

    let threw =
        try
            FableWorkaroundLab.Exports.cyclic () = FableWorkaroundLab.Exports.cyclic ()
            |> ignore

            false
        with _ ->
            true

    check "and the same = does not terminate on a self-referencing object" threw

/// A consumer's class over an entrypoint the lab's ambient module exports. Declared here because
/// `inherit` is a source construct: this type existing at all is the check the interface form
/// could not pass (FS0946), and its behaviour is what the checks below read.
type private Bench(label: string) =
    inherit AmbientModuleLab.Workbench(label)

    override this.run(payload) = $"derived:{this.label}:{payload.label}"

/// An ambient module declaration binds to the specifier it quotes rather than to the package the
/// rest of the file imports from, and a renamed re-export binds under the exported name.
let private ambientModules () =
    let payload = AmbientModuleLab.Exports.connect "socket"
    equal "a renamed re-export reaches the export, not the module-local name" "socket" payload.label
    equal "a function imported from a specifier reaches that module" 6.0 (AmbientModuleLab.Exports.measure payload)

    let hammer = AmbientModuleLab.Exports.Hammer 4.0
    let hammerClass: obj = import "Hammer" "ambient-lab:tools"

    check
        "a class imported from a specifier news that module's class"
        (emitJsExpr (hammer, hammerClass) "$0 instanceof $1")

    equal "and its method runs" "socket:4" (hammer.strike payload)
    equal "a static dots off the imported name" 12.0 AmbientModuleLab.Hammer.LIMIT

    // `declare module "ambient-lab:runtime" { export = AmbientLabRuntime }`. Nothing puts the
    // namespace on `globalThis`, so a `[<Global>]` binding to it would read `undefined`.
    equal "an `export =` namespace's members read through the specifier" "1.4.0" AmbientModuleLab.Exports.version
    check "and the namespace itself is no global" (emitJsExpr () "globalThis.AmbientLabRuntime === undefined")

/// The entrypoint form: an `[<AbstractClass>]` under the specifier's import, which a consumer
/// derives from. Fable compiles the `inherit` to `extends` and the constructor to `super(...)`,
/// so the derived object is the module's class at runtime.
let private entrypointClasses () =
    let bench = Bench "vice"
    let workbench: obj = import "Workbench" "ambient-lab:tools"

    check "a derived class extends the module's class in JavaScript" (emitJsExpr (bench, workbench) "$0 instanceof $1")

    equal "the base constructor's assignment reads back off the instance" "vice" bench.label
    equal "and the JavaScript prototype carries the same value" "vice" (emitJsExpr bench "$0.label")

    let payload = AmbientModuleLab.Exports.connect "socket"
    equal "the override is what F# calls" "derived:vice:socket" (bench.run payload)

    equal
        "and what JavaScript calls through the base's own method name"
        "derived:vice:socket"
        (emitJsExpr (bench, payload) "$0.run($1)")

    // `class Snag extends Error`: the base is the compiler library's, and `Error` binds to `exn`,
    // so the class form carries `inherit exn` and F# sees an exception. `errorClasses` below is
    // where that is exercised; here it is the JavaScript object that is under test.
    let snag = AmbientModuleLab.Exports.Snag "torn"
    check "a class over a lib base is still the module's class" (emitJsExpr snag "$0 instanceof Error")
    equal "and its base constructor ran" "torn" snag.message

    // The guard: a class no specifier exports, and one whose base this run declares, keep the
    // object-literal Create the entrypoint form has no room for.
    let anvil = AmbientModuleLab.Anvil.Create 9.0
    equal "a global abstract class keeps its ParamObject Create" 9.0 anvil.mass

    let vise =
        AmbientModuleLab.Vise.Create(2.0, 5.0, (fun (p: AmbientModuleLab.Payload) -> p.label))

    equal "and so does a class whose base this run declares" 2.0 vise.jaw

/// A consumer's subclass over the hook lab's entrypoint, implementing one hook and declining the
/// other. Declared here rather than inside the check for the reason `Retry` is: `inherit` and
/// `interface … with` are source constructs, and this type compiling is half of what the
/// emission claims.
type private Handled(label: string) =
    inherit HookInterfaceLab.Station(label)

    override this.run(signal) = $"run:{this.label}:{signal.label}"

    interface HookInterfaceLab.Station.IFetchHandler with
        member this.fetch(signal) = $"fetch:{this.label}:{signal.label}"

/// The negative of the same claim: a subclass providing no hook at all.
type private Unhandled(label: string) =
    inherit HookInterfaceLab.Station(label)

    override this.run(signal) = $"run:{this.label}:{signal.label}"

/// A subclass opting into both hooks, so a check can read what Fable emits for a class carrying
/// more than one interface implementation at once.
type private HandledBoth(label: string) =
    inherit HookInterfaceLab.Station(label)

    override this.run(signal) = $"run:{this.label}:{signal.label}"

    interface HookInterfaceLab.Station.IFetchHandler with
        member this.fetch(signal) = $"fetch:{this.label}:{signal.label}"

    interface HookInterfaceLab.Station.IAlarmHandler with
        member this.alarm() = $"alarm:{this.label}"

/// A hook whose interface carries its owner's type parameter.
type private Forwarder(seed: string) =
    inherit HookInterfaceLab.Relay<string>(seed)

    interface HookInterfaceLab.Relay.IForwardHandler<string> with
        member _.forward value = $"forward:{value}"

/// An optional method of an entrypoint class, emitted as an interface a subclass opts into. The
/// platform reads a hook off the instance, so what the checks measure is what
/// `typeof instance.fetch` answers for a subclass that implemented it and one that did not.
let private optionalHooks () =
    let signal = HookInterfaceLab.Signal.Create "s"
    let handled = Handled "one"

    check "an implemented hook is present by property access" (emitJsExpr handled "typeof $0.fetch === \"function\"")

    equal
        "and it dispatches to the implementation the subclass gave"
        "fetch:one:s"
        (emitJsExpr (handled, signal) "$0.fetch($1)")

    check
        "a hook the same subclass declined is absent"
        (emitJsExpr handled "typeof $0.alarm === \"undefined\" && !(\"alarm\" in $0)")

    check "the hook is no own property of the instance" (emitJsExpr handled "Object.keys($0).indexOf(\"fetch\") === -1")

    let unhandled = Unhandled "two"

    check
        "a subclass implementing no hook interface carries no hook"
        (emitJsExpr unhandled "typeof $0.fetch === \"undefined\" && typeof $0.alarm === \"undefined\"")

    equal
        "while the mandatory slot it overrode still dispatches"
        "run:two:s"
        (emitJsExpr (unhandled, signal) "$0.run($1)")

    equal "and the imported base constructor assigned its own member" "two" unhandled.label

    let forwarder = Forwarder "seed"

    equal
        "a hook interface carrying its owner's type parameter dispatches under the hook's name"
        "forward:x"
        (emitJsExpr forwarder "$0.forward(\"x\")")

    equal "and the base constructor's argument arrived" "seed" forwarder.seed

    let asData =
        HookInterfaceLab.Station.IFetchHandler.Create(fun (s: HookInterfaceLab.Signal) -> s.label)

    equal
        "the hook interface's Create is the object literal a handler map is"
        "s"
        (emitJsExpr (asData, signal) "$0.fetch($1)")

    // A subclass opting into both hooks at once, formerly proven only against `Probes.HookedBench`
    // (wave six lane AA, now `hook-interface-lab`). A platform that discovers hooks by reading
    // `instance.fetch` walks the prototype chain and finds it; one that enumerates own keys finds
    // an instance carrying only what the base constructor assigned.
    let both = HandledBoth "vice"
    let signalSocket = HookInterfaceLab.Signal.Create "socket"

    check
        "an interface-implemented hook is reachable by property access on a class implementing more than one"
        (emitJsExpr both "typeof $0.fetch === \"function\"")

    equal
        "and it dispatches to the implementation the subclass gave"
        "fetch:vice:socket"
        (emitJsExpr (both, signalSocket) "$0.fetch($1)")

    check "and by the `in` operator, which walks the same chain" (emitJsExpr both "\"fetch\" in $0")

    check
        "the hook is no own property: own-key enumeration does not discover it"
        (emitJsExpr both "Object.keys($0).indexOf(\"fetch\") === -1")

    check
        "nor does hasOwnProperty, nor getOwnPropertyNames on the instance"
        (emitJsExpr
            both
            "!Object.prototype.hasOwnProperty.call($0, \"fetch\") && Object.getOwnPropertyNames($0).indexOf(\"fetch\") === -1")

    equal
        "the instance's own keys are what the base constructor assigned"
        "label"
        (emitJsExpr both "Object.keys($0).join(\",\")")

    check
        "the prototype's methods are non-enumerable, so Object.keys on it discovers nothing either"
        (emitJsExpr both "Object.keys(Object.getPrototypeOf($0)).length === 0")

    check
        "which the `in` operator agrees with for a subclass that omits the interface"
        (emitJsExpr unhandled "!(\"fetch\" in $0)")

    let bareTwo = Unhandled "plain"
    check "a subclass that omits the interface carries no hook" (emitJsExpr bareTwo "typeof $0.fetch === \"undefined\"")

    equal
        "and the override it did make still dispatches"
        "run:plain:socket"
        (emitJsExpr (bareTwo, signalSocket) "$0.run($1)")

    // The member name Fable emits for an interface implementation on a class. A mangled name is a
    // method no platform will look up.
    equal
        "an interface member on a class is emitted under its declared name, unmangled, once per interface"
        "constructor,run,fetch,alarm"
        (emitJsExpr both "Object.getOwnPropertyNames(Object.getPrototypeOf($0)).join(\",\")")

    equal "and the second interface's member dispatches under that name" "alarm:vice" (emitJsExpr both "$0.alarm()")

    check
        "and the class overriding an abstract base member emits that name too"
        (emitJsExpr unhandled "Object.getOwnPropertyNames(Object.getPrototypeOf($0)).indexOf(\"run\") >= 0")

/// A class renamed by a name clash: its statics bind through the *export* name and are declared
/// on the type the instance side took.
let private renamedStatics () =
    equal "a renamed class's static reads off the selector its export name spells" 7.0 StaticsCollisionLab.Depot2.LIMIT

    let depot = StaticsCollisionLab.Depot2.``open`` "a"
    equal "and its static method reaches the same object" "a" depot.slot

/// Wave six's remaining probe, over the hand-written forms in `Probes.fs` that no lab golden yet
/// carries. The optional-hook and nested-name probes wave six proposed alongside this one moved
/// onto `hook-interface-lab` and `nested-name-lab` once lanes AA and AD landed; see `optionalHooks`
/// and `nestedNames` above.
let private probes () =
    let payload = AmbientModuleLab.Exports.connect "socket"

    // Two forms `nested-name-lab` (lane AD) does not carry: a nested inline shape holding a field
    // of its own owner's type, and an import bound from inside the nested module rather than at
    // the file's top level. See `Probes.fs`'s header for why these stay hand-written.
    let options =
        Probes.Widget.Options.Create(depth = 2.0, retry = Probes.Widget.Options.Retry.Create(attempts = 3.0))

    let widget = Probes.Widget.Create(label = "w", options = options)
    widget.options.owner <- Some widget
    equal "the nested type's reference back to its owner reads through" "w" (emitJsExpr widget "$0.options.owner.label")

    equal
        "an import declared inside the nested module reaches the specifier's export"
        6.0
        (Probes.Widget.Exports.measure payload)

/// Lane CP, wave fourteen: proves the assumption lane CN's exclusive-arm fold (batch two) rests
/// on, over the two hand-written forms in `Probes.fs`'s probes 17-18. Fable accepts a type
/// carrying two `[<ParamObject; Emit("$0")>]` `Create` statics distinguished only by which
/// parameters are required, in both shapes the corpus holds today - the pair where each arm has
/// a required member of its own (`SearchOptions`), and the pair where one arm's distinguishing
/// member is optional in source (`ContainerLikeOptions`) - and each overload's emitted object
/// literal holds exactly the arguments passed at that call site.
let private paramObjectOverloads () =
    let byQuery = Probes.SearchOptions.Create(query = "hello", shared = 1.0)
    equal "the query arm's object carries the query key" """{"query":"hello","shared":1}""" (json byQuery)

    let byMessages =
        Probes.SearchOptions.Create(messages = [| "a"; "b" |], shared = 2.0)

    equal
        "the messages arm's object carries the messages key, and not the query key"
        """{"messages":["a","b"],"shared":2}"""
        (json byMessages)

    let byImage =
        Probes.ContainerLikeOptions.Create(enableInternet = true, image = "img")

    equal
        "the required-arm object carries only its own key beside the shared one"
        """{"enableInternet":true,"image":"img"}"""
        (json byImage)

    let bySnapshot =
        Probes.ContainerLikeOptions.Create(enableInternet = true, snapshot = "snap")

    equal
        "the optional-arm object carries only its own key too, though its distinguishing member is optional in source"
        """{"enableInternet":true,"snapshot":"snap"}"""
        (json bySnapshot)

    let bare = Probes.ContainerLikeOptions.Create(enableInternet = true)

    equal
        "omitting both arms' distinguishing members still resolves, to the optional arm"
        """{"enableInternet":true}"""
        (json bare)

/// A consumer's class over the entrypoint the error lab's ambient module exports. Declared here
/// for the reason `Bench` is: `inherit` is a source construct, and this type compiling at all is
/// what the flattened form could not reach. `Fault` inherits `exn`, so `Retry` is an F# exception
/// and the checks below raise it.
type private Retry(message: string) =
    inherit ErrorClassLab.Fault(message)

    override this.describe(detail) = $"retry:{this.message}:{detail}"

/// A class an ambient module exports that extends `Error`. The binding inherits `exn`, so a
/// consumer raises it and catches it by type - the two operations the flattened interface form
/// admits no spelling of at all.
let private errorClasses () =
    let faultClass: obj = import "Fault" "error-lab:faults"

    // The imported constructor's own instance, raised and caught by the type it was declared
    // under. `raise` typechecks because the binding derives `exn`, and the catch is a type test.
    let imported = ErrorClassLab.Exports.Fault "torn"

    let caught =
        try
            raise imported
        with :? ErrorClassLab.Fault as fault ->
            fault.message

    equal "an entrypoint class over Error is raised and caught by its own type" "torn" caught

    // The consumer's subclass, which is the shape a real API asks for.
    let derived = Retry "stalled"

    let byBase =
        try
            raise derived
        with :? ErrorClassLab.Fault as fault ->
            fault.describe "twice"

    equal "and a consumer's subclass is caught as the base it derives" "retry:stalled:twice" byBase

    let byOwnType =
        try
            raise derived
        with
        | :? Retry as retry -> $"own:{retry.message}"
        | :? ErrorClassLab.Fault -> "base"

    equal "the subclass's own type is what the narrower handler matches" "own:stalled" byOwnType

    // The JavaScript side of the same object: the `inherit exn` is erased at the import, so what
    // is thrown is the module's class, and the platform's `catch (e) { e instanceof Fault }` sees
    // it as one.
    check "the raised object is the module's class in JavaScript" (emitJsExpr (derived, faultClass) "$0 instanceof $1")
    check "and an Error, which is what a JavaScript catch tests for" (emitJsExpr derived "$0 instanceof Error")

    check
        "an F# raise reaches a JavaScript catch as the same object"
        (emitJsExpr
            (derived, faultClass)
            "(() => { try { throw $0 } catch (e) { return e instanceof $1 && e.message === \"stalled\" } })()")

    // The base constructor ran, so the JavaScript properties the flattening declared read back.
    equal "the JavaScript message property reads through the binding" "stalled" derived.message
    equal "and the runtime's own assignment does too" "Fault" derived.name
    check "the retryable flag the class assigns is on the instance" derived.retryable

    // The negative: an entrypoint with no base inherits nothing, so it is a plain class. A
    // `raise` of it would not typecheck, which is the claim; what runs here is that the class
    // form still reaches its member.
    let runner = ErrorClassLab.Exports.Runner "plain"
    equal "an entrypoint with no base is still the module's class" "base:plain:once" (runner.run "once")
    check "and is no exception" (emitJsExpr runner "!($0 instanceof Error)")

    // `Error` in a reference position now reads as `exn` rather than `obj`, so what comes back is
    // catchable without a cast.
    let reason = ErrorClassLab.Exports.reason imported

    equal
        "a returned Error is an exn the consumer can raise"
        "torn"
        (try
            raise reason
         with e ->
             e.Message)

/// Lane AE's design gate. Every claim reads the arity JavaScript saw, so a curried chain fails
/// twice over: `length` reads 1, and the call returns a function where a string was declared.
let private callbackFunctionForms () =
    equal
        "an F# function of arity 0 crosses as a 0-argument function"
        "0:none"
        (Probes.CallbackFunctions.callNone (fun () -> "none"))

    equal "arity 1 crosses as a 1-argument function" "1:got:1" (Probes.CallbackFunctions.callOne (fun a -> $"got:{a}"))
    equal "arity 2 crosses uncurried" "2:got:1:2" (Probes.CallbackFunctions.callTwo (fun a b -> $"got:{a}:{b}"))

    equal
        "arity 3 crosses uncurried"
        "3:got:1:2:3"
        (Probes.CallbackFunctions.callThree (fun a b c -> $"got:{a}:{b}:{c}"))

    let mutable sawVoid = 0.0
    equal "a unit-returning callback keeps its arity" 1.0 (Probes.CallbackFunctions.callVoid (fun a -> sawVoid <- a))
    equal "and the runtime's call reached it" 7.0 sawVoid

    equal
        "a named callback abbreviation crosses uncurried"
        "2:1.5|2"
        (Probes.CallbackFunctions.callNamed (fun value digits -> $"{value}|{digits}"))

    let built =
        Probes.Handlers.Create(onTick = (fun a b -> $"tick:{a}:{b}"), onDone = (fun _ -> ()))

    equal "a callback in a ParamObject literal crosses uncurried" "2:tick:1:2:1" (Probes.CallbackFunctions.fire built)

    let options =
        Probes.Options.Create(label = "b", transform = (fun a b -> $"t:{a}:{b}"), finish = (fun () -> ()))

    equal
        "a method-shaped ParamObject parameter crosses uncurried"
        "b:2:t:1:2:0"
        (Probes.CallbackFunctions.build options)

    let fromJs = Probes.CallbackFunctions.handlers
    equal "a callback read off an interface member applies with all its arguments" "js:1:2" (fromJs.onTick 1.0 2.0)
    // Measured, not wanted. Reading a function-typed member back out hands F# a curry wrapper of
    // length 1 rather than the JavaScript function itself, so a consumer that passes the value on
    // to JavaScript passes a unary function. `Func` reads back as the function JavaScript holds.
    equal
        "reading a function-typed member back hands F# a unary curry wrapper"
        1.0
        (emitJsExpr fromJs.onTick "$0.length")

    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    let factory = Probes.CallbackFunctions.factory

    // A function-typed *property*: the value is read off the member and applied at the call site.
    // The wrapper applies correctly from F#; its arity is what is lost.
    equal "a function-typed property reads back as a unary curry wrapper too" 1.0 (emitJsExpr factory.pair "$0.length")
    equal "and applies with all its arguments" "pair:1:2" (attempt (fun () -> factory.pair 1.0 2.0))
    equal "a function-typed member of arity 0 applies" "ready" (attempt factory.ready)

    // The same function, reached through a method call rather than a property read. Fable wraps
    // nothing here and compiles the call site curried anyway, so the application throws. This is
    // the position that refuses the conversion.
    let made = factory.make 5.0
    equal "a callback returned from a method arrives at its declared arity" 2.0 (emitJsExpr made "$0.length")

    check
        "but the call site applies it one argument at a time, and throws"
        ((attempt (fun () -> made 1.0 2.0)).StartsWith "threw:")

    equal "a callback returned from a method applies at arity 1" "one:5:1" (attempt (fun () -> factory.makeOne 5.0 1.0))
    equal "a callback returned from a method applies at arity 0" "none:5" (attempt (factory.makeNone 5.0))

    check
        "and throws at arity 3 for the same reason"
        ((attempt (fun () -> factory.makeThree 5.0 1.0 2.0 3.0)).StartsWith "threw:")

/// The emission itself, position by position, on the generated golden rather than a hand-written
/// mirror. Each callback below is either an F# function type or a delegate according to the rule
/// `Shape.Spec.callbackRef` applies, and every claim reads the `length` of the function JavaScript
/// received beside the result of calling it with all its arguments at once.
let private callbackGoldenForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    // Parameter position. Arity 0 and 1 convert; arity 2 and 3 keep the delegate.
    equal
        "a converted callback of arity 0 crosses as a 0-argument function"
        "0:none"
        (CallbackFunctionLab.Exports.callNone (fun () -> "none"))

    equal
        "a converted callback of arity 1 crosses as a 1-argument function"
        "1:got:1"
        (CallbackFunctionLab.Exports.callOne (fun a -> $"got:{a}"))

    equal
        "a retained delegate of arity 2 crosses at its declared arity"
        "2:got:1:2"
        (CallbackFunctionLab.Exports.callTwo (CallbackFunctionLab.CallTwo.Callback(fun a b -> $"got:{a}:{b}")))

    equal
        "a retained delegate of arity 3 crosses at its declared arity"
        "3:got:1:2:3"
        (CallbackFunctionLab.Exports.callThree (CallbackFunctionLab.CallThree.Callback(fun a b c -> $"got:{a}:{b}:{c}")))

    // The unit-returning arm, which rendered `Action` before the conversion.
    let mutable sawVoid = 0.0

    equal
        "a converted unit-returning callback keeps its arity"
        1.0
        (CallbackFunctionLab.Exports.callVoid (fun a -> sawVoid <- a))

    equal "and the runtime's call reached it" 7.0 sawVoid
    let mutable sawVoidTwo = 0.0

    equal
        "a retained Action of arity 2 keeps its arity"
        2.0
        (CallbackFunctionLab.Exports.callVoidTwo (
            CallbackFunctionLab.CallVoidTwo.Callback(fun a b -> sawVoidTwo <- a + b)
        ))

    equal "and the runtime's call reached it with both arguments" 15.0 sawVoidTwo

    equal
        "a named callback abbreviation of arity 2 keeps its delegate"
        "2:1.5|2"
        (CallbackFunctionLab.Exports.callNamed (CallbackFunctionLab.Formatter(fun value digits -> $"{value}|{digits}")))

    // A ParamObject literal, mixing both spellings in one object.
    let built =
        CallbackFunctionLab.Handlers.Create(
            onTick = CallbackFunctionLab.Handlers.OnTick(fun a b -> $"tick:{a}:{b}"),
            onDone = (fun _ -> ())
        )

    equal
        "a ParamObject literal mixing a delegate and a converted optional crosses at both arities"
        "2:tick:1:2:1"
        (CallbackFunctionLab.Exports.fire built)

    let options =
        CallbackFunctionLab.Options.Create(
            label = "b",
            transform = Func<float, float, string>(fun a b -> $"t:{a}:{b}"),
            finish = (fun () -> ())
        )

    equal
        "a method-shaped ParamObject parameter crosses at its declared arity in both spellings"
        "b:2:t:1:2:0"
        (CallbackFunctionLab.Exports.build options)

    // Read-back. A delegate arrives as the function JavaScript holds; a converted callback of
    // arity 0 or 1 arrives as a wrapper whose length is that same arity.
    let fromJs = CallbackFunctionLab.Exports.handlers
    equal "a retained delegate read off an interface member keeps its arity" 2.0 (emitJsExpr fromJs.onTick "$0.length")
    equal "and invokes with all its arguments" "js:1:2" (fromJs.onTick.Invoke(1.0, 2.0))

    match fromJs.onDone with
    | None -> check "a converted optional member reads back present" false
    | Some onDone ->
        equal "a converted optional member reads back at arity 1" 1.0 (emitJsExpr onDone "$0.length")
        onDone 3.0
        check "and applies" true

    let factory = CallbackFunctionLab.Exports.factory
    equal "a retained delegate property keeps its arity" 2.0 (emitJsExpr factory.pair "$0.length")
    equal "and invokes with all its arguments" "pair:1:2" (attempt (fun () -> factory.pair.Invoke(1.0, 2.0)))
    equal "a converted property of arity 0 reads back at arity 0" 0.0 (emitJsExpr factory.ready "$0.length")
    equal "and applies" "ready" (attempt factory.ready)

    // Member returns. This is where the curried spelling threw: a converted return is measured
    // here at arity 0 and 1, and the delegate carries arity 2 and 3.
    let made = factory.make 5.0
    equal "a retained delegate returned from a method keeps its arity" 2.0 (emitJsExpr made "$0.length")
    equal "and invokes with all its arguments" "made:5:1:2" (attempt (fun () -> made.Invoke(1.0, 2.0)))
    equal "a converted return of arity 1 applies" "one:5:1" (attempt (fun () -> factory.makeOne 5.0 1.0))
    equal "a converted return of arity 0 applies" "none:5" (attempt (factory.makeNone 5.0))

    equal
        "a retained delegate returned from a method invokes at arity 3"
        "three:5:1:2:3"
        (attempt (fun () -> (factory.makeThree 5.0).Invoke(1.0, 2.0, 3.0)))

    // Nesting. A callback whose own return is a callback keeps whichever level the rule refuses,
    // and the two levels need not agree.
    equal
        "a converted callback over a retained delegate crosses at both declared arities"
        "1:2:made:5:1:2"
        (attempt (fun () ->
            CallbackFunctionLab.Exports.callNesting (fun seed ->
                CallbackFunctionLab.CallNesting.Outer.Result(fun a b -> $"made:{seed}:{a}:{b}"))))

    equal
        "a retained delegate over a converted callback crosses at both declared arities"
        "1:1:one:5:1"
        (attempt (fun () ->
            CallbackFunctionLab.Exports.callNestingOne (
                CallbackFunctionLab.CallNestingOne.Outer(fun seed -> (fun a -> $"one:{seed}:{a}"))
            )))

    let driven =
        CallbackFunctionLab.Factory.Create(
            make = (fun seed -> CallbackFunctionLab.Factory.Make.Result(fun a b -> $"made:{seed}:{a}:{b}")),
            makeOne = Func<float, float -> string>(fun seed -> (fun a -> $"one:{seed}:{a}")),
            makeNone = Func<float, unit -> string>(fun seed -> (fun () -> $"none:{seed}")),
            makeThree =
                (fun seed -> CallbackFunctionLab.Factory.MakeThree.Result(fun a b c -> $"three:{seed}:{a}:{b}:{c}")),
            ready = (fun () -> "ready"),
            pair = CallbackFunctionLab.Factory.Pair(fun a b -> $"pair:{a}:{b}")
        )

    equal
        "a Factory built in F# crosses outward at every declared arity"
        "1:2:made:5:1:2:0:ready:2:pair:1:2"
        (attempt (fun () -> CallbackFunctionLab.Exports.drive driven))

/// Lane AK. The tupled function type, in the same positions, against the same runtime. Fable
/// compiles an F# tuple to a JavaScript array, so each claim below reads the `length` of the
/// function JavaScript received beside the result of calling it with all its arguments at once.
let private callbackTupledForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    let attemptNum (f: unit -> float) =
        try
            $"%g{f ()}"
        with e ->
            $"threw: {e.Message}"

    // Measured, not wanted. A tupled parameter crosses as a one-argument JavaScript function
    // taking an array, so the runtime's `fn(1, 2)` binds the array slot to `1` and the
    // destructuring reads `undefined`. The value is wrong and nothing throws.
    equal
        "a tupled function of arity 2 crosses as a unary function over an array"
        "1:got:undefined:undefined"
        (attempt (fun () -> Probes.CallbackTuples.callTwo (fun (a, b) -> $"got:{a}:{b}")))

    equal
        "and arity 3 the same way"
        "1:got:undefined:undefined:undefined"
        (attempt (fun () -> Probes.CallbackTuples.callThree (fun (a, b, c) -> $"got:{a}:{b}:{c}")))

    let mutable sawVoidTwo = 0.0

    equal
        "a tupled unit-returning callback crosses at arity 1"
        "1"
        (attemptNum (fun () -> Probes.CallbackTuples.callVoidTwo (fun (a, b) -> sawVoidTwo <- a + b)))

    check "and the arguments the runtime passed arrived as undefined" (Double.IsNaN sawVoidTwo)

    equal
        "a named tupled abbreviation crosses at arity 1 too"
        "1:undefined|undefined"
        (attempt (fun () -> Probes.CallbackTuples.callNamed (fun (value, digits) -> $"{value}|{digits}")))

    let built =
        Probes.TupledHandlers.Create(onTick = (fun (a, b) -> $"tick:{a}:{b}"), onDone = (fun _ -> ()))

    equal
        "a tupled callback in a ParamObject literal crosses at arity 1"
        "1:tick:undefined:undefined:1"
        (attempt (fun () -> Probes.CallbackTuples.fire built))

    let options =
        Probes.TupledOptions.Create(label = "b", transform = (fun (a, b) -> $"t:{a}:{b}"), finish = (fun () -> ()))

    equal
        "a tupled method-shaped ParamObject parameter crosses at arity 1"
        "b:1:t:undefined:undefined:0"
        (attempt (fun () -> Probes.CallbackTuples.build options))

    // The read-back direction inverts. Fable inserts no curry wrapper around a tupled member, so
    // the arity JavaScript holds survives the read - and the F# application passes the tuple as a
    // single array argument, so the JavaScript function sees one argument where it declared two.
    let fromJs = Probes.CallbackTuples.handlers

    equal
        "a tupled callback read off an interface member keeps the arity JavaScript holds"
        2.0
        (emitJsExpr fromJs.onTick "$0.length")

    equal
        "but applying it passes the tuple as one array argument"
        "js:1,2:undefined"
        (attempt (fun () -> fromJs.onTick (1.0, 2.0)))

    let factory = Probes.CallbackTuples.factory

    equal "a tupled function-typed property keeps the arity JavaScript holds" 2.0 (emitJsExpr factory.pair "$0.length")

    equal
        "and applies as one array argument the same way"
        "pair:1,2:undefined"
        (attempt (fun () -> factory.pair (1.0, 2.0)))

    equal "a tupled function-typed member of arity 0 applies" "ready" (attempt factory.ready)
    let made = factory.make 5.0

    equal "a tupled callback returned from a method keeps the arity JavaScript holds" 2.0 (emitJsExpr made "$0.length")

    equal "and applies as one array argument" "made:5:1,2:undefined" (attempt (fun () -> made (1.0, 2.0)))

    equal
        "a tupled callback returned from a method applies as one array argument at arity 3"
        "three:5:1,2,3:undefined:undefined"
        (attempt (fun () -> (factory.makeThree 5.0) (1.0, 2.0, 3.0)))

/// Lane AK. Arity 0 and 1, where the curried and tupled spellings coincide, and the nesting a
/// conversion of that slice alone produces.
let private callbackMixedForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    equal
        "a unary function returning a delegate crosses at arity 1 over a 2-argument delegate"
        "1:2:made:5:1:2"
        (attempt (fun () ->
            Probes.CallbackMixed.callNesting (fun seed ->
                System.Func<float, float, string>(fun a b -> $"made:{seed}:{a}:{b}"))))

    // Measured, not wanted. Where a function type's own return is a function type, Fable flattens
    // the two levels into one JavaScript function of the summed arity, so the runtime's first
    // application returns the result rather than the inner callback. Arity does not rescue this:
    // it fails with both levels unary. This is the rule the conversion below obeys - a function
    // type is emitted only where its return is not one.
    check
        "a function type returning a curried function flattens into one JavaScript function"
        ((attempt (fun () -> Probes.CallbackMixed.callNestingCurried (fun seed a b -> $"made:{seed}:{a}:{b}")))
            .StartsWith
            "threw:")

    check
        "and flattens with both levels unary too"
        ((attempt (fun () -> Probes.CallbackMixed.callNestingOne (fun seed a -> $"one:{seed}:{a}"))).StartsWith "threw:")

    let mixed =
        Probes.MixedFactory.Create(
            make = (fun seed -> System.Func<float, float, string>(fun a b -> $"made:{seed}:{a}:{b}")),
            ready = (fun () -> "ready"),
            pair = System.Func<float, float, string>(fun a b -> $"pair:{a}:{b}")
        )

    equal
        "a ParamObject literal mixing function types and delegates crosses at every declared arity"
        "1:2:made:5:1:2:0:ready:2:pair:1:2"
        (attempt (fun () -> Probes.CallbackMixed.drive mixed))

    equal
        "a delegate returning a unary function crosses at arity 1 twice"
        "1:1:one:5:1"
        (attempt (fun () ->
            Probes.CallbackMixed.callNestingOneMixed (
                System.Func<float, float -> string>(fun seed -> (fun a -> $"one:{seed}:{a}"))
            )))

    equal
        "a delegate returning a unary delegate crosses at arity 1 twice"
        "1:1:one:5:1"
        (attempt (fun () ->
            Probes.CallbackMixed.callNestingOneDelegate (
                System.Func<float, System.Func<float, string>>(fun seed ->
                    System.Func<float, string>(fun a -> $"one:{seed}:{a}"))
            )))

    equal
        "a delegate returning a 2-argument delegate crosses at both declared arities"
        "1:2:made:5:1:2"
        (attempt (fun () ->
            Probes.CallbackMixed.callNestingDelegate (
                System.Func<float, System.Func<float, float, string>>(fun seed ->
                    System.Func<float, float, string>(fun a b -> $"made:{seed}:{a}:{b}"))
            )))

    // Read-back at arity 1, which lane AE left open: its curry-wrapper finding was measured at
    // arity 2, where a wrapper of length 1 loses an argument. At arity 1 there is none to lose.
    let fromJs = Probes.CallbackFunctions.handlers

    match fromJs.onDone with
    | None -> check "an optional function-typed member read back is present" false
    | Some onDone ->
        equal "an optional function-typed member reads back at arity 1" 1.0 (emitJsExpr onDone "$0.length")
        onDone 3.0
        check "and applies" true

/// Lane AM. The same rule inside an erased union arm. `U2` unwraps to whichever arm was supplied,
/// so each claim reads the `length` of the function JavaScript received beside the result of
/// calling it with all its arguments at once, and the non-callback arm of the same union is read
/// beside it.
let private callbackUnionArmForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    // Parameter position: the converted arm at arity 0 and 1, the retained delegate at arity 2.
    equal
        "a converted callback arm of arity 0 crosses as a 0-argument function"
        "0:none"
        (attempt (fun () -> CallbackFunctionLab.Exports.callUnionNone (U2.Case2(fun () -> "none"))))

    equal
        "a converted callback arm of arity 1 crosses as a 1-argument function"
        "1:got:1"
        (attempt (fun () -> CallbackFunctionLab.Exports.callUnionOne (U2.Case2(fun a -> $"got:{a}"))))

    equal
        "a retained delegate arm of arity 2 crosses at its declared arity"
        "2:got:1:2"
        (attempt (fun () ->
            CallbackFunctionLab.Exports.callUnionTwo (
                U2.Case2(CallbackFunctionLab.CallUnionTwo.Listener(fun a b -> $"got:{a}:{b}"))
            )))

    equal
        "the non-callback arm of the same union crosses as its own value"
        "text:plain"
        (attempt (fun () -> CallbackFunctionLab.Exports.callUnionOne (U2.Case1 "plain")))

    equal
        "a named union abbreviation carries its converted arm at arity 1"
        "1:named:1"
        (attempt (fun () -> CallbackFunctionLab.Exports.callUnionNamed (U2.Case2(fun a -> $"named:{a}"))))

    // Return position, where the bare converted callback threw above arity 1.
    let returnedOne = CallbackFunctionLab.Exports.makeUnionOne 5.0

    equal
        "a converted callback arm returned from a function arrives at arity 1"
        1.0
        (emitJsExpr returnedOne "$0.length")

    equal
        "and the match reaches it and applies it"
        "one:5:1"
        (attempt (fun () ->
            match returnedOne with
            | U2.Case1 text -> $"text:{text}"
            | U2.Case2 callback -> callback 1.0))

    let returnedTwo = CallbackFunctionLab.Exports.makeUnionTwo 5.0

    equal "a retained delegate arm returned from a function arrives at arity 2" 2.0 (emitJsExpr returnedTwo "$0.length")

    equal
        "and invokes with all its arguments"
        "two:5:1:2"
        (attempt (fun () ->
            match returnedTwo with
            | U2.Case1 text -> $"text:{text}"
            | U2.Case2 callback -> callback.Invoke(1.0, 2.0)))

    // Read-back off a member. The member's type is the union, so the read carries no function
    // type and Fable inserts no curry wrapper: the arity JavaScript holds survives at both arities.
    let fromJs = CallbackFunctionLab.Exports.unionHandlers

    equal "a converted callback arm read off a member arrives at arity 1" 1.0 (emitJsExpr fromJs.one "$0.length")

    equal
        "and applies"
        "js1:1"
        (attempt (fun () ->
            match fromJs.one with
            | U2.Case1 text -> $"text:{text}"
            | U2.Case2 callback -> callback 1.0))

    equal "a retained delegate arm read off a member arrives at arity 2" 2.0 (emitJsExpr fromJs.two "$0.length")

    equal
        "and invokes with both its arguments"
        "js2:1:2"
        (attempt (fun () ->
            match fromJs.two with
            | U2.Case1 text -> $"text:{text}"
            | U2.Case2 callback -> callback.Invoke(1.0, 2.0)))

    equal
        "the non-callback arm of a union-typed member reads back as its value"
        "text:plain"
        (attempt (fun () ->
            match fromJs.text with
            | U2.Case1 text -> $"text:{text}"
            | U2.Case2 callback -> callback 1.0))

    // The outward direction: a union-typed member built in F# and read by JavaScript.
    let built =
        CallbackFunctionLab.UnionHandlers.Create(
            one = U2.Case2(fun a -> $"one:{a}"),
            two = U2.Case2(CallbackFunctionLab.UnionHandlers.Two(fun a b -> $"two:{a}:{b}")),
            text = U2.Case1 "plain"
        )

    equal
        "a ParamObject literal carries a converted arm, a delegate arm and a non-callback arm"
        "1:one:1|2:two:1:2|text:plain"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireUnion built))

    // The corpus shape exactly: the callback arm beside an object arm. The arm is read out by a
    // JavaScript discrimination rather than by a match, because a type test on the object arm is
    // the erased-interface case `workarounds` above measures.
    equal
        "a converted callback arm beside an object arm crosses at arity 1"
        "1:got:1"
        (attempt (fun () -> CallbackFunctionLab.Exports.callUnionObject (U2.Case2(fun a -> $"got:{a}"))))

    equal
        "and the object arm of the same union carries its method at arity 1"
        "object:1:obj:1"
        (attempt (fun () ->
            CallbackFunctionLab.Exports.callUnionObject (
                U2.Case1(CallbackFunctionLab.ListenerObject.Create(fun a -> $"obj:{a}"))
            )))

    let objectUnion = CallbackFunctionLab.Exports.objectUnion

    equal "a callback arm read back beside an object arm arrives at arity 1" 1.0 (emitJsExpr objectUnion "$0.length")

    equal
        "and applies with its argument"
        "js:1"
        (attempt (fun () ->
            if emitJsExpr objectUnion "typeof $0 === \"function\"" then
                (unbox<float -> string> objectUnion) 1.0
            else
                "the object arm"))

/// Lane AQ. The two positions lane AM left unreached: a function-typed union arm one level
/// deeper than a direct union child (behind an array, behind an `option`), and a `U2<...>`
/// nested inside a delegate's own type parameter (`Action<string, U2<UnionListenerObject,
/// (float -> unit)>>`, the shape `Action<'Type, U2<(obj -> unit), EventListenerObject<Event>>,
/// ...>` carries in `@cloudflare/workers-types`).
let private callbackUnionNestingForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    // Array of union: parameter direction, a `ParamObject` literal carrying a converted arm
    // beside a non-callback arm.
    let arrayHandlers =
        CallbackFunctionLab.ArrayUnionHandlers.Create(steps = [| U2.Case2(fun a -> $"js1:{a}"); U2.Case1 "plain" |])

    equal
        "a converted callback arm behind an array in a ParamObject literal crosses at arity 1"
        "1:js1:1|text:plain"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireArrayUnion arrayHandlers))

    // Array of union: read-back direction, the array built in JavaScript.
    let fromJsArray = CallbackFunctionLab.Exports.arrayUnionHandlers

    equal
        "and a converted callback arm behind an array reads back at arity 1"
        "1:js1:1|text:plain"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireArrayUnion fromJsArray))

    // Option of union: parameter direction, present and absent.
    let optionSome =
        CallbackFunctionLab.OptionUnionHandlers.Create(step = U2.Case2(fun a -> $"js1:{a}"))

    equal
        "a converted callback arm behind an option in a ParamObject literal crosses at arity 1"
        "1:js1:1"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireOptionUnion optionSome))

    let optionNone = CallbackFunctionLab.OptionUnionHandlers.Create()

    equal
        "and an absent option carrying the same union reports its absence"
        "none"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireOptionUnion optionNone))

    // Option of union: read-back direction, both built in JavaScript.
    let fromJsSome = CallbackFunctionLab.Exports.optionUnionHandlersSome
    let fromJsNone = CallbackFunctionLab.Exports.optionUnionHandlersNone

    equal
        "and a converted callback arm behind an option reads back at arity 1"
        "1:js1:1"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireOptionUnion fromJsSome))

    equal
        "and the JavaScript-built absent option reports its absence too"
        "none"
        (attempt (fun () -> CallbackFunctionLab.Exports.fireOptionUnion fromJsNone))

    // `U2<...>` nested inside a delegate's own type parameter. `register` is a two-argument
    // void callback, so it converts to `Action` by the arity rule; its second parameter is
    // itself the union under test. JavaScript calls `register` once with the function arm and
    // once with the object arm, and the F# lambda records what it received directly - no
    // round trip through JavaScript is needed to read the arity off a parameter.
    //
    // Discriminating by pattern-matching `U2.Case1`/`U2.Case2` here hits the same pre-existing
    // erased-union limitation lane AM recorded: the interface arm's runtime type test folds to
    // `false` at compile time, so the match always takes the function arm. Discriminating in
    // JavaScript with `typeof`, as the fixture's other object-arm checks already do, sidesteps it.
    let mutable seenFn = ""
    let mutable seenObj = ""

    CallbackFunctionLab.Exports.addListener (
        CallbackFunctionLab.AddListener.Register(fun kind listener ->
            if emitJsExpr listener "typeof $0 === \"function\"" then
                let fn = unbox<float -> unit> listener
                let arity = emitJsExpr fn "$0.length"
                seenFn <- $"{kind}:{arity}"
            else
                let obj = unbox<CallbackFunctionLab.UnionListenerObject> listener
                let arity = emitJsExpr obj.handleEvent "$0.length"
                seenObj <- $"{kind}:obj:{arity}")
    )

    equal "the function arm nested inside the delegate's own type parameter keeps arity 1" "fn-arm:1" seenFn

    equal "and the object arm nested the same way carries its method at arity 1" "obj-arm:obj:1" seenObj

// Lane BA. The same callback positions again, with the retained delegate spelled as a *named* F#
// delegate rather than `System.Func`/`System.Action`. Naming the delegate is what would let a
// parameter carry a name across the boundary, and the feature rests on the named form behaving as
// the framework one does; each claim below reads the `length` of the function JavaScript received
// beside the result of calling it with all its arguments at once. Arity 4 runs against
// `index.js`'s runtime-only `callFour`/`makeFour`, with a hand-written `Func` control beside it.

/// A callback of arity 2 whose parameters carry names.
type TickHandler = delegate of x: float * y: float -> string

type TripleHandler = delegate of a: float * b: float * c: float -> string

type QuadHandler = delegate of a: float * b: float * c: float * d: float -> string

/// The unit-returning arm, which renders `Action` today.
type VoidHandler = delegate of x: float * y: float -> unit

/// The named-abbreviation position: the lab's own `Formatter`.
type FormatterHandler = delegate of value: float * digits: float -> string

/// A named delegate whose return is a converted callback - the outer level of a nesting.
type SeedHandler = delegate of seed: float -> (float -> string)

/// The lab's `Handlers` with its arity 2 member named.
[<Interface>]
type DelegateHandlers =
    abstract onTick: TickHandler with get, set
    abstract onDone: (float -> unit) option with get, set

    [<ParamObject; Emit("$0")>]
    static member Create(onTick: TickHandler, ?onDone: (float -> unit)) : DelegateHandlers = jsNative

/// The lab's `Options`, whose method member the ParamObject pass binds as a callback-typed Create
/// parameter.
[<Interface>]
type DelegateOptions =
    abstract label: string with get, set
    abstract transform: a: float * b: float -> string
    abstract finish: unit -> unit

    [<ParamObject; Emit("$0")>]
    static member Create(label: string, transform: TickHandler, finish: (unit -> unit)) : DelegateOptions = jsNative

/// The lab's `Factory`, every member of arity 2 or wider named.
[<Interface>]
type DelegateFactory =
    abstract make: seed: float -> TickHandler
    abstract makeOne: seed: float -> (float -> string)
    abstract makeNone: seed: float -> (unit -> string)
    abstract makeThree: seed: float -> TripleHandler
    abstract ready: (unit -> string)
    abstract pair: TickHandler

    [<ParamObject; Emit("$0")>]
    static member Create
        (
            make: (float -> TickHandler),
            makeOne: Func<float, (float -> string)>,
            makeNone: Func<float, (unit -> string)>,
            makeThree: (float -> TripleHandler),
            ready: (unit -> string),
            pair: TickHandler
        ) : DelegateFactory =
        jsNative

[<Erase>]
type NamedDelegates =

    [<Import("callTwo", "callback-function-lab")>]
    static member callTwo(callback: TickHandler) : string = jsNative

    [<Import("callThree", "callback-function-lab")>]
    static member callThree(callback: TripleHandler) : string = jsNative

    [<Import("callFour", "callback-function-lab")>]
    static member callFour(callback: QuadHandler) : string = jsNative

    /// The framework delegate at the same arity, so arity 4 has its own control.
    [<Import("callFour", "callback-function-lab")>]
    static member callFourFunc(callback: Func<float, float, float, float, string>) : string = jsNative

    [<Import("callVoidTwo", "callback-function-lab")>]
    static member callVoidTwo(callback: VoidHandler) : float = jsNative

    [<Import("callNamed", "callback-function-lab")>]
    static member callNamed(formatter: FormatterHandler) : string = jsNative

    [<Import("fire", "callback-function-lab")>]
    static member fire(handlers: DelegateHandlers) : string = jsNative

    [<Import("handlers", "callback-function-lab")>]
    static member handlers: DelegateHandlers = jsNative

    [<Import("build", "callback-function-lab")>]
    static member build(options: DelegateOptions) : string = jsNative

    [<Import("factory", "callback-function-lab")>]
    static member factory: DelegateFactory = jsNative

    [<Import("makeFour", "callback-function-lab")>]
    static member makeFour(seed: float) : QuadHandler = jsNative

    [<Import("makeFour", "callback-function-lab")>]
    static member makeFourFunc(seed: float) : Func<float, float, float, float, string> = jsNative

    [<Import("callNesting", "callback-function-lab")>]
    static member callNesting(outer: float -> TickHandler) : string = jsNative

    [<Import("callNestingOne", "callback-function-lab")>]
    static member callNestingOne(outer: SeedHandler) : string = jsNative

    [<Import("drive", "callback-function-lab")>]
    static member drive(factory: DelegateFactory) : string = jsNative

/// Lane BA's measurement, position by position against `callbackGoldenForms` above.
let private callbackNamedDelegateForms () =
    let attempt (f: unit -> string) =
        try
            f ()
        with e ->
            $"threw: {e.Message}"

    // Parameter position, at every arity the framework delegate is retained for.
    equal
        "a named delegate of arity 2 crosses at its declared arity"
        "2:got:1:2"
        (NamedDelegates.callTwo (TickHandler(fun a b -> $"got:{a}:{b}")))

    equal
        "a named delegate of arity 3 crosses at its declared arity"
        "3:got:1:2:3"
        (NamedDelegates.callThree (TripleHandler(fun a b c -> $"got:{a}:{b}:{c}")))

    equal
        "a named delegate of arity 4 crosses at its declared arity"
        "4:got:1:2:3:4"
        (NamedDelegates.callFour (QuadHandler(fun a b c d -> $"got:{a}:{b}:{c}:{d}")))

    equal
        "and the framework delegate reads the same at arity 4"
        "4:got:1:2:3:4"
        (NamedDelegates.callFourFunc (Func<float, float, float, float, string>(fun a b c d -> $"got:{a}:{b}:{c}:{d}")))

    // The unit-returning arm, which renders `Action`.
    let mutable sawVoid = 0.0

    equal
        "a named unit-returning delegate of arity 2 keeps its arity"
        2.0
        (NamedDelegates.callVoidTwo (VoidHandler(fun a b -> sawVoid <- a + b)))

    equal "and the runtime's call reached it with both arguments" 15.0 sawVoid

    equal
        "a named delegate in the abbreviation position crosses at its declared arity"
        "2:1.5|2"
        (NamedDelegates.callNamed (FormatterHandler(fun value digits -> $"{value}|{digits}")))

    // A ParamObject literal, and the method-shaped Create parameter beside it.
    let built =
        DelegateHandlers.Create(onTick = TickHandler(fun a b -> $"tick:{a}:{b}"), onDone = (fun _ -> ()))

    equal
        "a named delegate in a ParamObject literal crosses at its declared arity"
        "2:tick:1:2:1"
        (NamedDelegates.fire built)

    let options =
        DelegateOptions.Create(label = "b", transform = TickHandler(fun a b -> $"t:{a}:{b}"), finish = (fun () -> ()))

    equal
        "a named delegate as a method-shaped ParamObject parameter crosses at its declared arity"
        "b:2:t:1:2:0"
        (NamedDelegates.build options)

    // Read-back: JavaScript to F#.
    let fromJs = NamedDelegates.handlers
    equal "a named delegate read off an interface member keeps its arity" 2.0 (emitJsExpr fromJs.onTick "$0.length")
    equal "and invokes with all its arguments" "js:1:2" (fromJs.onTick.Invoke(1.0, 2.0))

    let factory = NamedDelegates.factory
    equal "a named delegate property keeps its arity" 2.0 (emitJsExpr factory.pair "$0.length")
    equal "and invokes with all its arguments" "pair:1:2" (attempt (fun () -> factory.pair.Invoke(1.0, 2.0)))

    let made = factory.make 5.0
    equal "a named delegate returned from a method keeps its arity" 2.0 (emitJsExpr made "$0.length")
    equal "and invokes with all its arguments" "made:5:1:2" (attempt (fun () -> made.Invoke(1.0, 2.0)))

    equal
        "a named delegate returned from a method invokes at arity 3"
        "three:5:1:2:3"
        (attempt (fun () -> (factory.makeThree 5.0).Invoke(1.0, 2.0, 3.0)))

    let madeFour = NamedDelegates.makeFour 5.0
    equal "a named delegate returned from a method keeps arity 4" 4.0 (emitJsExpr madeFour "$0.length")

    equal
        "and invokes with all four arguments"
        "four:5:1:2:3:4"
        (attempt (fun () -> madeFour.Invoke(1.0, 2.0, 3.0, 4.0)))

    equal
        "the framework delegate reads back the same at arity 4"
        "four:5:1:2:3:4"
        (attempt (fun () -> (NamedDelegates.makeFourFunc 5.0).Invoke(1.0, 2.0, 3.0, 4.0)))

    // Nesting, both ways round.
    equal
        "a converted callback over a named delegate crosses at both declared arities"
        "1:2:made:5:1:2"
        (attempt (fun () -> NamedDelegates.callNesting (fun seed -> TickHandler(fun a b -> $"made:{seed}:{a}:{b}"))))

    equal
        "a named delegate over a converted callback crosses at both declared arities"
        "1:1:one:5:1"
        (attempt (fun () -> NamedDelegates.callNestingOne (SeedHandler(fun seed -> (fun a -> $"one:{seed}:{a}")))))

    // Outward: a whole object of named delegates built in F#.
    let driven =
        DelegateFactory.Create(
            make = (fun seed -> TickHandler(fun a b -> $"made:{seed}:{a}:{b}")),
            makeOne = Func<float, float -> string>(fun seed -> (fun a -> $"one:{seed}:{a}")),
            makeNone = Func<float, unit -> string>(fun seed -> (fun () -> $"none:{seed}")),
            makeThree = (fun seed -> TripleHandler(fun a b c -> $"three:{seed}:{a}:{b}:{c}")),
            ready = (fun () -> "ready"),
            pair = TickHandler(fun a b -> $"pair:{a}:{b}")
        )

    equal
        "a factory of named delegates built in F# crosses outward at every declared arity"
        "1:2:made:5:1:2:0:ready:2:pair:1:2"
        (attempt (fun () -> NamedDelegates.drive driven))

    // What the names buy. The delegate declares `x` and `y`; every lambda below binds `a` and `b`,
    // so the two are told apart by inspection. On the F# side the declared names are the ones
    // `Invoke` accepts as named arguments. In JavaScript the parameters are the binders of
    // whichever lambda made the function, so the declared names are an F#-side affordance.
    let named = TickHandler(fun a b -> $"named:{a}:{b}")
    equal "the delegate's parameter names are usable at the F# call site" "named:1:2" (named.Invoke(x = 1.0, y = 2.0))

    let parametersOf (fn: obj) =
        let source: string = emitJsExpr fn "$0.toString()"
        source.Substring(0, source.IndexOf "=>")

    let builtParameters = parametersOf named

    check
        "and are absent from the emitted JavaScript"
        (not (builtParameters.Contains "x" || builtParameters.Contains "y"))

    check "which carries the lambda's binders instead" (builtParameters.Contains "a" && builtParameters.Contains "b")

    check
        "a delegate read back from JavaScript carries the runtime's own parameter names"
        ((parametersOf fromJs.onTick).Contains "a")

/// Lane BB: the generator's own named delegates, read off `delegate-name-lab`'s golden. Where
/// `callbackNamedDelegateForms` measured a hand-written declaration, these are the declarations
/// `synthesize-anonymous` minted and `shape-callbacks` wrote.
let private generatedDelegateForms () =
    let arity (fn: obj) : float = emitJsExpr fn "$0.length"

    // Parameter position, at both arities and unit-returning.
    equal
        "a generated delegate of arity 2 crosses at its declared arity"
        "2:got:1:2"
        (DelegateNameLab.Exports.callTwo (DelegateNameLab.CallTwo.Handler(fun x y -> $"got:{x}:{y}")))

    equal
        "a generated delegate of arity 3 crosses at its declared arity"
        "3:got:1:2:z"
        (DelegateNameLab.Exports.callThree (DelegateNameLab.CallThree.Handler(fun x y label -> $"got:{x}:{y}:{label}")))

    let mutable sawVoid = 0.0

    equal
        "a generated unit-returning delegate keeps its arity"
        "2"
        (DelegateNameLab.Exports.callVoidTwo (DelegateNameLab.CallVoidTwo.Handler(fun x y -> sawVoid <- x + y)))

    equal "and the runtime's call reached it with both arguments" 3.0 sawVoid

    equal
        "a delegate declared from a named alias crosses at its declared arity"
        "2:n:3:4"
        (DelegateNameLab.Exports.callNamed (DelegateNameLab.TickHandler(fun x y -> $"n:{x}:{y}")))

    // A ParamObject literal built in F#, read by JavaScript.
    let built =
        DelegateNameLab.EventTarget.Create(
            onTick = DelegateNameLab.EventTarget.OnTick(fun x y -> $"tick:{x}:{y}"),
            onDrag = DelegateNameLab.EventTarget.OnDrag(fun x y -> $"drag:{x}:{y}"),
            onDone = (fun () -> ())
        )

    equal
        "two generated delegates in one ParamObject literal keep their arities"
        "2:tick:1:2|2:drag:3:4|0"
        (DelegateNameLab.Exports.fire built)

    // The same members built in JavaScript, read back into F#.
    let fromJs = DelegateNameLab.Exports.target
    equal "a generated delegate read off an interface member keeps its arity" 2.0 (arity fromJs.onTick)
    equal "and invokes with all its arguments" "tick12" (fromJs.onTick.Invoke(1.0, 2.0))
    equal "and so does the second member of the same shape" "drag12" (fromJs.onDrag.Invoke(1.0, 2.0))

    // Return position, in both directions.
    let factory = DelegateNameLab.Exports.factory
    equal "a generated delegate read off a property keeps its arity" 2.0 (arity factory.pair)
    equal "and invokes with all its arguments" "pair12" (factory.pair.Invoke(1.0, 2.0))

    equal
        "a generated delegate returned from a method invokes with all its arguments"
        "made312"
        (factory.make(3.0).Invoke(1.0, 2.0))

    let driven =
        DelegateNameLab.Factory.Create(
            pair = DelegateNameLab.Factory.Pair(fun x y -> $"pair:{x}:{y}"),
            make = (fun seed -> DelegateNameLab.Factory.Make.Result(fun x y -> $"made:{seed}:{x}:{y}"))
        )

    equal
        "a factory built in F# crosses outward at both declared arities"
        "2:pair:1:2|2:made:9:1:2"
        (DelegateNameLab.Exports.drive driven)

    // The nesting rule: the outer level is retained and named, the inner one converts.
    equal
        "a generated delegate over a converted callback crosses at both declared arities"
        "1:1:one:7:5"
        (DelegateNameLab.Exports.callNesting (
            DelegateNameLab.CallNesting.Outer(fun seed -> (fun x -> $"one:{seed}:{x}"))
        ))

/// A case folded from arms sharing a tag value agrees on the tag alone, so it carries no field.
/// What the fold claims is that the tag still reaches JavaScript from a payload-free case.
let private foldedTaggedCases () =
    equal
        "a folded case carrying no field still erases to the tagged object"
        """{"state":"done"}"""
        (json SharedTagLab.Terminal.Done)

    equal
        "and a sibling case the fold left alone carries its own tag"
        """{"state":"pending"}"""
        (json SharedTagLab.Terminal.Pending)

    match SharedTagLab.Terminal.Done with
    | SharedTagLab.Terminal.Done -> check "a folded case matches itself on the tag" true
    | SharedTagLab.Terminal.Pending -> check "a folded case matched Pending" false

/// A union written inline at a member position, declared under a name of its own. What the mint
/// claims is the read: `workarounds` item 1 measures an erased union over interface arms
/// reaching one arm for every value, and a case list keyed on the tag reaches the arm the value
/// carries.
let private mintedTaggedCases () =
    equal
        "a case minted from an inline union erases to the tagged object"
        """{"type":"fetch","url":"/x"}"""
        (json (SharedTagLab.Wide.Event.Fetch "/x"))

    equal
        "and an arm carrying the tag alone erases to it alone"
        """{"type":"queue"}"""
        (json SharedTagLab.Wide.Event.Queue)

    let above: SharedTagLab.Wide.Event =
        emitJsExpr () """({ type: "jsrpc", methodName: "ping" })"""

    match above with
    | SharedTagLab.Wide.Event.Jsrpc methodName ->
        equal "a JavaScript-built object reaches the case its tag names" "ping" methodName
    | _ -> check "a JavaScript-built jsrpc reached another case" false

    // Three arms, below the erased-union cap: the arm an erased union would have written under
    // its own name is reachable here because the tag is what the match reads.
    let below: SharedTagLab.Narrow.Event = emitJsExpr () """({ type: "email" })"""

    match below with
    | SharedTagLab.Narrow.Event.Email -> check "a union claimed below the cap matches on its tag too" true
    | _ -> check "a JavaScript-built email reached another case" false

/// §4.10's pure index signatures, resolved to `Xantham.Fable.Core.Record`/`ReadonlyRecord`
/// rather than a minted interface. Every property below is one of those types; reading and
/// writing through the F# indexer (`.[key]` / `.[key] <- value`) round-trips against the
/// underlying JavaScript object, and one write is cross-checked by a direct `emitJsExpr`
/// property read to confirm the indexer compiles to the same property access an index
/// signature's own type would.
let private recordIndex () =
    let cache = RecordIndexLab.Cache.Create(entries = emitJsExpr () "({ a: 1, b: 2 })")

    equal "a string-keyed Record reads a value off the object literal it was built from" 1.0 cache.entries.["a"]

    cache.entries.["c"] <- 3.0

    equal "and a write through Record's indexer lands as a plain property" 3.0 (emitJsExpr (cache.entries) "$0.c")

    let grid =
        RecordIndexLab.Grid.Create(rows = emitJsExpr () "({ 1: \"x\", 2: \"y\" })")

    equal "a numeric-keyed Record reads the same way" "x" grid.rows.[1.0]

    let frozen = RecordIndexLab.Frozen.Create(values = emitJsExpr () "({ ok: true })")

    equal "a readonly index signature reads through ReadonlyRecord's indexer" true frozen.values.["ok"]

    RecordIndexLab.Exports.tag (emitJsExpr () "({ x: 1.0 })")

    check
        "an imported function taking Record<string,'T> receives the plain object literal, no wrapper"
        (emitJsExpr
            ()
            "globalThis.__recordIndexLabTagCalls.length === 1 && globalThis.__recordIndexLabTagCalls[0].x === 1")

[<EntryPoint>]
let main _ =
    globals ()
    imports ()
    ambientModules ()
    entrypointClasses ()
    errorClasses ()
    statics ()
    bigints ()
    constructorObjects ()
    heritage ()
    taggedUnions ()
    foldedTaggedCases ()
    mintedTaggedCases ()
    nestedNames ()
    optionalHooks ()
    renamedStatics ()
    workarounds ()
    probes ()
    paramObjectOverloads ()
    callbackFunctionForms ()
    callbackGoldenForms ()
    callbackTupledForms ()
    callbackMixedForms ()
    callbackUnionArmForms ()
    callbackUnionNestingForms ()
    callbackNamedDelegateForms ()
    generatedDelegateForms ()
    recordIndex ()

    match failures with
    | [] ->
        printfn $"run gate: {passed} checks passed"
        0
    | failed ->
        for claim in failed do
            eprintfn $"run gate FAILED: {claim}"

        eprintfn $"run gate: {failed.Length} of {passed + failed.Length} checks failed"
        1
