module Xantham.Types.Signal

// ---------------------------------------------------------------------------
// Tracking scope — module-level mutable, single active scope at a time.
// F# doesn't support `let private mutable` at module level; the binding is
// effectively internal since no .fsi exposes it.
// ---------------------------------------------------------------------------
let mutable internal collector: (IEvent<unit> -> unit) option = None

/// <summary>
/// A reactive signal that holds a value and notifies dependents when it changes.
/// </summary>
/// <typeparam name="a">The type of value held by the signal.</typeparam>
/// <remarks>
/// <para>
/// <c>Signal&lt;'a&gt;</c> is a unified type covering both <em>source</em> signals (plain
/// mutable values) and <em>computed</em> signals (values derived from other signals via a
/// thunk). Consumers need not distinguish between the two — the same <c>.Value</c> /
/// <c>.Invalidated</c> surface works for both.
/// </para>
///
/// <para><b>Source signal</b> — holds a plain value, updated imperatively:</para>
/// <code lang="fsharp">
/// let count = Signal.source 0
/// count.Set 1
/// printfn "%d" count.Value   // 1
/// </code>
///
/// <para>
/// <b>Computed signal (explicit deps)</b> — re-evaluates when any listed
/// invalidation stream fires:
/// </para>
/// <code lang="fsharp">
/// let a = Signal.source 2
/// let b = Signal.source 3
/// let sum = Signal.computed (fun () -> a.Value + b.Value) [ a.Invalidated; b.Invalidated ]
/// printfn "%d" sum.Value   // 5
/// a.Set 10
/// printfn "%d" sum.Value   // 13
/// </code>
///
/// <para>
/// <b>Computed signal (auto-tracked)</b> — deps are discovered by running the
/// thunk once; any signal whose <c>.Value</c> is accessed during that run is registered
/// automatically:
/// </para>
/// <code lang="fsharp">
/// let a = Signal.source 2
/// let b = Signal.source 3
/// let sum = Signal.auto (fun () -> a.Value + b.Value)
/// printfn "%d" sum.Value   // 5
/// </code>
///
/// <para>
/// <b>Derived signals and effects</b> — combinators for transforming and
/// observing signals:
/// </para>
/// <code lang="fsharp">
/// let doubled = Signal.map ((*) 2) count
/// let labelledSum = Signal.map2 (sprintf "%d + %d") a b
///
/// let sub =
///     Signal.effect
///         (fun () -> printfn "count is now %d" count.Value)
///         [ count.Invalidated ]
/// count.Set 5   // prints "count is now 5"
/// sub.Dispose() // stop listening
/// </code>
///
/// <para>
/// <b>Dirty propagation</b> — invalidation is transitive and lazy. Setting a
/// source signal marks all downstream computed signals dirty immediately, but their
/// thunks do not re-run until <c>.Value</c> is next accessed:
/// </para>
/// <code lang="fsharp">
/// let x   = Signal.source 1
/// let y   = Signal.auto (fun () -> x.Value * 2)
/// let z   = Signal.auto (fun () -> y.Value + 1)
/// x.Set 5
/// // y and z are dirty, but neither thunk has run yet
/// printfn "%d" z.Value   // pulls z → pulls y → 5*2+1 = 11
/// </code>
/// </remarks>
type Signal<'a> private (compute: unit -> 'a, upstream: IEvent<unit> option) =
    // Source signals start clean and pre-cached; computed signals start dirty.
    let mutable dirty = upstream.IsSome
    let mutable cache: 'a option = if upstream.IsNone then Some (compute()) else None
    let changed = Event<unit>()

    // Wire dirty-propagation from upstream invalidation stream.
    do
        match upstream with
        | None     -> ()
        | Some inv ->
            inv.Add(fun () ->
                if not dirty then
                    dirty <- true
                    changed.Trigger())

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    static member private Merge(deps: IEvent<unit> list) : IEvent<unit> =
        match deps with
        | []           -> Event<unit>().Publish        // inert — never fires
        | [single]     -> single
        | head :: tail -> List.fold Event.merge head tail

    // -----------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------

    /// <summary>Creates a source signal with an initial value.</summary>
    /// <param name="value">The initial value.</param>
    static member Source(value: 'a) : Signal<'a> =
        Signal<'a>((fun () -> value), None)

    /// <summary>
    /// Creates a computed signal whose thunk is re-evaluated whenever any stream
    /// in <paramref name="deps"/> fires.
    /// </summary>
    /// <param name="compute">Thunk that produces the signal's value.</param>
    /// <param name="deps">
    /// Upstream invalidation streams — typically the <c>.Invalidated</c> properties
    /// of other signals.
    /// </param>
    static member Computed(compute: unit -> 'a, deps: IEvent<unit> list) : Signal<'a> =
        Signal<'a>(compute, Some (Signal<'a>.Merge deps))

    /// <summary>
    /// Creates a computed signal, discovering dependencies automatically by
    /// dry-running <paramref name="thunk"/> once inside a tracking scope.
    /// </summary>
    /// <param name="thunk">
    /// Thunk that produces the signal's value. Every <c>Signal.Value</c> access
    /// during the initial dry-run registers that signal as a dependency.
    /// </param>
    /// <remarks>
    /// Only signals whose <c>.Value</c> is accessed during the dry-run are
    /// registered. Conditional dependencies — signals read only in some branches —
    /// are tracked only if that branch executes during the initial run.
    /// </remarks>
    static member Auto(thunk: unit -> 'a) : Signal<'a> =
        let mutable deps: IEvent<unit> list = []
        let prev = collector
        collector <- Some (fun ev -> deps <- ev :: deps)
        try
            thunk() |> ignore
        finally
            collector <- prev
        Signal<'a>.Computed(thunk, deps)

    // -----------------------------------------------------------------------
    // Members
    // -----------------------------------------------------------------------

    /// <summary>
    /// Fires when this signal becomes dirty — i.e. when any upstream dependency
    /// changes and the cached value may no longer be current.
    /// </summary>
    /// <remarks>
    /// Subscribing to this event is how downstream computed signals and
    /// <c>Signal.effect</c> handlers learn they need to re-evaluate.
    /// The event fires at most once per dirty transition; subsequent upstream
    /// changes while already dirty are suppressed.
    /// </remarks>
    member _.Invalidated : IEvent<unit> = changed.Publish

    /// <summary>
    /// Returns the current value of the signal, re-computing and caching it if dirty.
    /// </summary>
    /// <remarks>
    /// Reading <c>.Value</c> inside a <c>Signal.auto</c> tracking scope registers
    /// this signal as a dependency of the signal being constructed.
    /// </remarks>
    member this.Value : 'a =
        collector |> Option.iter (fun report -> report this.Invalidated)
        if dirty then
            let v = compute()
            cache <- Some v
            dirty <- false
            v
        else
            cache.Value

    /// <summary>
    /// Sets the value of a source signal.
    /// </summary>
    /// <param name="v">The new value.</param>
    /// <exception cref="System.InvalidOperationException">
    /// Thrown when called on a computed signal.
    /// </exception>
    /// <remarks>
    /// Equality-checked via <c>obj.Equals</c>: calling <c>Set</c> with a value equal
    /// to the current one is a no-op and does not fire <c>Invalidated</c>.
    /// </remarks>
    member _.Set(v: 'a) =
        match upstream with
        | Some _ ->
            raise (System.InvalidOperationException "Cannot set a computed signal")
        | None ->
            let differs =
                match cache with
                | Some c -> not ((box v).Equals(box c))
                | None   -> true   // defensive; source signals are always initialised
            if differs then
                cache <- Some v
                changed.Trigger()

    /// <summary>
    /// Retrofits this source signal with a reactive thunk, converting it into a
    /// self-updating signal without replacing the signal object.
    /// </summary>
    /// <param name="thunk">
    /// Value-producing thunk. Any <c>.Value</c> access on another signal during
    /// each run is auto-tracked as a dependency, exactly as with <c>Signal.auto</c>.
    /// </param>
    /// <remarks>
    /// <para>
    /// Use this when a signal was created as a <c>pending</c> placeholder —
    /// because other signals already depend on it — and a thunk becomes available
    /// later that should drive its value going forward.
    /// </para>
    /// <para>
    /// The thunk is run immediately to seed the value, then re-run whenever any
    /// tracked dependency fires. Before each re-run, the previous subscriptions are
    /// disposed so that shifting dependency sets (conditional branches in the thunk)
    /// are handled correctly.
    /// </para>
    /// <para>
    /// Equality-checked on each <c>Set</c>: the thunk re-running does not fire
    /// <c>Invalidated</c> unless the produced value actually differs.
    /// </para>
    /// </remarks>
    member this.FulfillWith(thunk: unit -> 'a) : unit =
        let mutable subs: System.IDisposable list = []
        let rec run () =
            subs |> List.iter _.Dispose()
            let mutable deps: IEvent<unit> list = []
            let prev = collector
            collector <- Some (fun ev -> deps <- ev :: deps)
            let v = thunk()
            collector <- prev
            this.Set v
            subs <- deps |> List.map (fun dep -> Observable.subscribe (fun () -> run()) dep)
        run()

type PendingSignal<'A> = Signal<'A voption>

// ---------------------------------------------------------------------------
// Curried module-level API
// ---------------------------------------------------------------------------

/// <summary>
/// Curried constructor functions and combinators for <see cref="T:Xantham.Fable.Types.Signal.Signal`1"/>.
/// </summary>
[<RequireQualifiedAccess>]
module Signal =

    /// <summary>Creates a source signal holding <paramref name="value"/>.</summary>
    /// <param name="value">The initial value.</param>
    let source (value: 'a) : Signal<'a> = Signal<'a>.Source value
    
    /// <summary>Creates a source signal initialised to <c>ValueNone</c>.</summary>
    let pending<'a>(): Signal<'a voption> = source ValueNone

    /// <summary>Sets <paramref name="signal"/> to <c>ValueSome <paramref name="value"/></c>.</summary>
    /// <param name="value">The value to store.</param>
    /// <param name="signal">The pending signal to fill.</param>
    let fill<'a> (value: 'a) (signal: Signal<'a voption>) = signal.Set(ValueSome value)

    /// <summary>
    /// Retrofits <paramref name="signal"/> with a reactive thunk, seeding it immediately
    /// and re-running whenever any auto-tracked dependency changes.
    /// </summary>
    /// <param name="thunk">
    /// Value-producing thunk. Signal reads inside the thunk are auto-tracked as dependencies.
    /// </param>
    /// <param name="signal">
    /// An existing source signal — typically one created with <c>Signal.pending</c> that other
    /// signals already depend on and which cannot simply be replaced.
    /// </param>
    /// <remarks>
    /// See <see cref="M:Xantham.Fable.Types.Signal.Signal`1.FulfillWith"/> for full semantics.
    /// </remarks>
    let fulfillWith (thunk: unit -> 'a) (signal: Signal<'a>) : unit = signal.FulfillWith thunk

    /// <summary>
    /// Variant of <see cref="M:fulfillWith"/> for <c>Signal&lt;'a voption&gt;</c>: wraps the
    /// thunk result in <c>ValueSome</c> before setting.
    /// </summary>
    /// <param name="thunk">Thunk producing the unwrapped value.</param>
    /// <param name="signal">The pending signal to fulfill.</param>
    let fulfillWithSome (thunk: unit -> 'a) (signal: Signal<'a voption>) : unit =
        signal.FulfillWith(fun () -> thunk() |> ValueSome)

    /// <summary>
    /// Creates a computed signal that re-evaluates <paramref name="thunk"/> whenever
    /// any stream in <paramref name="deps"/> fires.
    /// </summary>
    /// <param name="thunk">Value-producing thunk.</param>
    /// <param name="deps">
    /// Explicit list of upstream invalidation streams
    /// (e.g. <c>[ a.Invalidated; b.Invalidated ]</c>).
    /// </param>
    let computed (thunk: unit -> 'a) (deps: IEvent<unit> list) : Signal<'a> = Signal<'a>.Computed(thunk, deps)

    /// <summary>
    /// Creates a computed signal, auto-discovering dependencies by running
    /// <paramref name="thunk"/> once in a tracking scope.
    /// </summary>
    /// <param name="thunk">
    /// Value-producing thunk. Any <c>.Value</c> access during the dry-run registers
    /// the accessed signal as a dependency.
    /// </param>
    let auto (thunk: unit -> 'a) : Signal<'a> = Signal<'a>.Auto thunk

    /// <summary>
    /// Transforms a signal's value with <paramref name="f"/>, returning a new
    /// computed signal that updates whenever the source changes.
    /// </summary>
    /// <param name="f">Mapping function.</param>
    /// <param name="s">Source signal.</param>
    let map (f: 'a -> 'b) (s: Signal<'a>) : Signal<'b> =
        Signal<'b>.Auto(fun () -> f s.Value)

    /// <summary>
    /// Combines two signals with a binary function, returning a computed signal
    /// that updates whenever either source changes.
    /// </summary>
    /// <param name="f">Combining function.</param>
    /// <param name="a">First source signal.</param>
    /// <param name="b">Second source signal.</param>
    let map2 (f: 'a -> 'b -> 'c) (a: Signal<'a>) (b: Signal<'b>) : Signal<'c> =
        Signal<'c>.Auto(fun () -> f a.Value b.Value)

    /// <summary>
    /// Runs <paramref name="action"/> immediately, then re-runs it each time any
    /// stream in <paramref name="deps"/> fires.
    /// </summary>
    /// <param name="action">Side-effecting action to run.</param>
    /// <param name="deps">
    /// Upstream invalidation streams that trigger re-runs
    /// (e.g. <c>[ count.Invalidated ]</c>).
    /// </param>
    /// <returns>
    /// A disposable that, when disposed, cancels all future re-runs.
    /// </returns>
    let effect (action: unit -> unit) (deps: IEvent<unit> list) : System.IDisposable =
        action()
        let subscriptions = deps |> List.map (fun ev -> Observable.subscribe (fun () -> action()) ev)
        { new System.IDisposable with
            member _.Dispose() = subscriptions |> List.iter _.Dispose() }
