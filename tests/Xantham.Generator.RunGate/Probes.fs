/// Hand-written mirrors of forms wave six proposes the generator emit, held here so the run gate
/// can measure what Fable does with them before a lane is built on the answer. Nothing generates
/// this file; a form that lands in the generator moves into a lab fixture's golden and out of
/// here.
///
/// `module rec` is the header every generated golden carries, and probe 3 depends on it.
module rec Xantham.Generator.RunGate.Probes

open Fable.Core
open Fable.Core.JsInterop

// Probes 1 and 2 - an optional lifecycle hook as an opt-in interface.

/// One interface per optional hook, carrying the hook as its only member. Lane AA's proposed
/// replacement for the settable `jsNative` property an optional method renders as today.
[<Interface>]
type IFetchHandler =
    abstract fetch: payload: AmbientModuleLab.Payload -> string

/// A second hook, so the measurement covers a subclass opting into more than one.
[<Interface>]
type IAlarmHandler =
    abstract alarm: unit -> string

/// A consumer's entrypoint subclass that opts into both hooks.
type HookedBench(label: string) =
    inherit AmbientModuleLab.Workbench(label)

    override this.run(payload) = $"hooked:{this.label}:{payload.label}"

    interface IFetchHandler with
        member this.fetch(payload) = $"fetch:{this.label}:{payload.label}"

    interface IAlarmHandler with
        member this.alarm() = $"alarm:{this.label}"

/// A consumer's entrypoint subclass that declines the hook. The negative of probe 1: the platform
/// must read this instance as carrying no `fetch`.
type BareBench(label: string) =
    inherit AmbientModuleLab.Workbench(label)

    override this.run(payload) = $"bare:{this.label}:{payload.label}"

// Probe 3 - an inline shape named under a module that shares its owner's name.

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
