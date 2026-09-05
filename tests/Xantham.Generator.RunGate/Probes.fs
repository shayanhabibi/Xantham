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
