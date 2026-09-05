/// Hand-written mirrors of forms wave six proposes the generator emit, held here so the run gate
/// can measure what Fable does with them before a lane is built on the answer. Nothing generates
/// this file; a form that lands in the generator moves into a lab fixture's golden and out of
/// here.
///
/// The optional-hook probe (opt-in interfaces for lifecycle hooks) and the nested-name probe's
/// object-literal checks moved onto `hook-interface-lab` and `nested-name-lab` once lanes AA and
/// AD landed those goldens; `tests/Xantham.Generator.RunGate/Program.fs`'s `optionalHooks` and
/// `nestedNames` carry the checks now. What remains below are two forms neither golden carries:
/// a nested inline shape holding a field of its own owner's type, and an import bound from inside
/// a nested module rather than at the file's top level.
///
/// `module rec` is the header every generated golden carries, and the probe below depends on it.
module rec Xantham.Generator.RunGate.Probes

open Fable.Core
open Fable.Core.JsInterop

// A nested inline shape named under a module that shares its owner's name.

/// Owner of an inline shape. Lane AD names the shape `Widget.Options`, which puts a module beside
/// this type and a forward reference into it.
[<Interface>]
type Widget =
    abstract label: string with get, set
    abstract options: Widget.Options with get, set

    [<ParamObject; Emit("$0")>]
    static member Create(label: string, options: Widget.Options) : Widget = jsNative

module Widget =

    /// The inline shape, referencing its owner back across the module boundary.
    [<Interface>]
    type Options =
        abstract depth: float with get, set
        abstract owner: Widget option with get, set
        abstract retry: Options.Retry option with get, set

        [<ParamObject; Emit("$0")>]
        static member Create(depth: float, ?owner: Widget, ?retry: Options.Retry) : Options = jsNative

    /// A second level of nesting, for an inline shape inside an inline shape.
    module Options =

        [<Interface>]
        type Retry =
            abstract attempts: float with get, set

            [<ParamObject; Emit("$0")>]
            static member Create(attempts: float) : Retry = jsNative

    /// An import bound from inside the nested module, where every other generated import sits at
    /// the file's top level.
    [<Erase>]
    type Exports =
        [<Import("measure", "ambient-lab:tools")>]
        static member measure(payload: AmbientModuleLab.Payload) : float = jsNative

// Probes 4-8 - a callback as an F# function type, in every position the corpus uses.
//
// Lane AE's design gate. `D5` chose `System.Func`/`System.Action` for guaranteed arity at the
// boundary; these mirror what the shape tier would emit instead, against the same runtime
// (`tests/fixtures/callback-function-lab/index.js`), which reports the `length` of the function it
// received. A curried chain reads as length 1 and returns a function where a string was declared.

/// A named callback type, which the shape tier abbreviates.
type Formatter = float -> float -> string

/// Probe 6 - a callback as an interface member, required and optional.
[<Interface>]
type Handlers =
    abstract onTick: (float -> float -> string) with get, set
    abstract onDone: (float -> unit) option with get, set

    [<ParamObject; Emit("$0")>]
    static member Create(onTick: float -> float -> string, ?onDone: float -> unit) : Handlers = jsNative

/// Probe 7 - a callback as a `ParamObject` Create parameter, which is what a method member binds.
[<Interface>]
type Options =
    abstract label: string with get, set
    abstract transform: a: float * b: float -> string
    abstract finish: unit -> unit

    [<ParamObject; Emit("$0")>]
    static member Create(label: string, transform: float -> float -> string, finish: unit -> unit) : Options = jsNative

/// Probe 8 - a callback returned from a member, at arity 2 and at arity 0.
[<Interface>]
type Factory =
    abstract make: seed: float -> (float -> float -> string)
    abstract makeOne: seed: float -> (float -> string)
    abstract makeNone: seed: float -> (unit -> string)
    abstract makeThree: seed: float -> (float -> float -> float -> string)
    abstract ready: (unit -> string)
    abstract pair: (float -> float -> string)

[<Erase>]
type CallbackFunctions =

    /// Probes 4 and 5 - a callback in parameter position, at arities 0 to 3.
    [<Import("callNone", "callback-function-lab")>]
    static member callNone(callback: unit -> string) : string = jsNative

    [<Import("callOne", "callback-function-lab")>]
    static member callOne(callback: float -> string) : string = jsNative

    [<Import("callTwo", "callback-function-lab")>]
    static member callTwo(callback: float -> float -> string) : string = jsNative

    [<Import("callThree", "callback-function-lab")>]
    static member callThree(callback: float -> float -> float -> string) : string = jsNative

    [<Import("callVoid", "callback-function-lab")>]
    static member callVoid(callback: float -> unit) : float = jsNative

    [<Import("callNamed", "callback-function-lab")>]
    static member callNamed(formatter: Formatter) : string = jsNative

    [<Import("fire", "callback-function-lab")>]
    static member fire(handlers: Handlers) : string = jsNative

    [<Import("handlers", "callback-function-lab")>]
    static member handlers: Handlers = jsNative

    [<Import("build", "callback-function-lab")>]
    static member build(options: Options) : string = jsNative

    [<Import("factory", "callback-function-lab")>]
    static member factory: Factory = jsNative

    [<Import("callVoidTwo", "callback-function-lab")>]
    static member callVoidTwo(callback: float -> float -> unit) : float = jsNative

// Probes 9-13 - a callback as a *tupled* F# function type, in the same positions.
//
// Lane AK. A tuple is the other function type F# offers, and Fable compiles an F# tuple to a
// JavaScript array, so the question is whether `(float * float) -> string` crosses as a
// 2-argument JavaScript function or as a 1-argument function taking an array. Arities 0 and 1
// have no tupled spelling distinct from the curried one, so probes 4 and 5 above cover them.

/// A named tupled callback type, in the spelling the shape tier would abbreviate.
type TupledFormatter = (float * float) -> string

/// A tupled callback as an interface member, required and optional.
[<Interface>]
type TupledHandlers =
    abstract onTick: ((float * float) -> string) with get, set
    abstract onDone: (float -> unit) option with get, set

    [<ParamObject; Emit("$0")>]
    static member Create(onTick: (float * float) -> string, ?onDone: float -> unit) : TupledHandlers = jsNative

/// A tupled callback as a `ParamObject` Create parameter, which is what a method member binds.
[<Interface>]
type TupledOptions =
    abstract label: string with get, set
    abstract transform: a: float * b: float -> string
    abstract finish: unit -> unit

    [<ParamObject; Emit("$0")>]
    static member Create(label: string, transform: (float * float) -> string, finish: unit -> unit) : TupledOptions =
        jsNative

/// A tupled callback returned from a method, and read off a function-typed member.
[<Interface>]
type TupledFactory =
    abstract make: seed: float -> ((float * float) -> string)
    abstract makeThree: seed: float -> ((float * float * float) -> string)
    abstract ready: (unit -> string)
    abstract pair: ((float * float) -> string)

[<Erase>]
type CallbackTuples =

    [<Import("callTwo", "callback-function-lab")>]
    static member callTwo(callback: (float * float) -> string) : string = jsNative

    [<Import("callThree", "callback-function-lab")>]
    static member callThree(callback: (float * float * float) -> string) : string = jsNative

    [<Import("callVoidTwo", "callback-function-lab")>]
    static member callVoidTwo(callback: (float * float) -> unit) : float = jsNative

    [<Import("callNamed", "callback-function-lab")>]
    static member callNamed(formatter: TupledFormatter) : string = jsNative

    [<Import("fire", "callback-function-lab")>]
    static member fire(handlers: TupledHandlers) : string = jsNative

    [<Import("handlers", "callback-function-lab")>]
    static member handlers: TupledHandlers = jsNative

    [<Import("build", "callback-function-lab")>]
    static member build(options: TupledOptions) : string = jsNative

    [<Import("factory", "callback-function-lab")>]
    static member factory: TupledFactory = jsNative

// Probes 14-16 - the arity 0 and 1 slice, where the curried and tupled spellings coincide and
// there is no arity for either to lose. Lane AK measures the positions lane AE's table left open:
// a nested callback whose inner level keeps its delegate, and a function-typed member read back
// at arity 1.

/// A `Factory` whose arity 0 and 1 members are function types and whose arity 2 members keep the
/// delegate: the mixed form a partial conversion produces.
[<Interface>]
type MixedFactory =
    abstract make: seed: float -> System.Func<float, float, string>
    abstract ready: (unit -> string)
    abstract pair: System.Func<float, float, string>

    [<ParamObject; Emit("$0")>]
    static member Create
        (make: float -> System.Func<float, float, string>, ready: unit -> string, pair: System.Func<float, float, string>)
        : MixedFactory =
        jsNative

[<Erase>]
type CallbackMixed =

    /// A unary function returning a delegate: the outer level converted, the inner retained.
    [<Import("callNesting", "callback-function-lab")>]
    static member callNesting(outer: float -> System.Func<float, float, string>) : string = jsNative

    /// The same nesting with the inner level a curried function too.
    [<Import("callNesting", "callback-function-lab")>]
    static member callNestingCurried(outer: float -> (float -> float -> string)) : string = jsNative

    /// Both levels at arity 1, where no spelling differs.
    [<Import("callNestingOne", "callback-function-lab")>]
    static member callNestingOne(outer: float -> (float -> string)) : string = jsNative

    [<Import("drive", "callback-function-lab")>]
    static member drive(factory: MixedFactory) : string = jsNative

    /// A delegate returning a unary function: the outer level retained, the inner converted.
    [<Import("callNestingOne", "callback-function-lab")>]
    static member callNestingOneMixed(outer: System.Func<float, float -> string>) : string = jsNative

    /// Both levels retained.
    [<Import("callNestingOne", "callback-function-lab")>]
    static member callNestingOneDelegate(outer: System.Func<float, System.Func<float, string>>) : string = jsNative

    [<Import("callNesting", "callback-function-lab")>]
    static member callNestingDelegate(outer: System.Func<float, System.Func<float, float, string>>) : string = jsNative
