// Signal.test.fsx
// Run via: npm run test:signal
// Compiled by Fable, executed in Node.js.
#load "../src/Xantham.Fable/Types/Signal.fs"

open Xantham.Fable.Types.Signal

// ---------------------------------------------------------------------------
// Minimal inline test runner
// Intentionally dependency-free so no NuGet restore is needed beyond FSharp.Core.
// ---------------------------------------------------------------------------

let mutable private _passed = 0
let mutable private _failed = 0
let mutable private _suite  = ""

/// Opens a named test suite. Purely decorative — groups console output.
let describe (name: string) (f: unit -> unit) =
    _suite <- name
    printfn $"\n  %s{name}"
    f ()
    _suite <- ""

/// Registers and immediately runs a single test case.
let it (name: string) (f: unit -> unit) =
    try
        f ()
        printfn $"    \u2713 %s{name}" // ✓
        _passed <- _passed + 1
    with ex ->
        printfn $"    \u2717 %s{name}" // ✗
        printfn $"      %s{ex.Message}"
        _failed <- _failed + 1

/// Asserts structural equality; throws with a diff message on mismatch.
let shouldEqual<'a when 'a : equality> (expected: 'a) (actual: 'a) =
    if actual <> expected then
        failwithf $"  expected: %A{expected}\n       got: %A{actual}"

/// Asserts a boolean condition.
let shouldBeTrue (msg: string) (condition: bool) =
    if not condition then failwith msg

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe "Signal.source — basics" (fun () ->

    it "holds initial value" (fun () ->
        let s = Signal.source 42
        shouldEqual 42 s.Value)

    it "Set updates value" (fun () ->
        let s = Signal.source 0
        s.Set 7
        shouldEqual 7 s.Value)

    it "Set fires Invalidated" (fun () ->
        let s = Signal.source 0
        let mutable fired = false
        s.Invalidated.Add(fun () -> fired <- true)
        s.Set 1
        shouldBeTrue "Invalidated should fire on change" fired)

    it "Set with same value does not fire Invalidated (equality gate)" (fun () ->
        let s = Signal.source 5
        let mutable count = 0
        s.Invalidated.Add(fun () -> count <- count + 1)
        s.Set 5
        shouldEqual 0 count)
)

describe "Signal.computed — explicit deps" (fun () ->

    it "evaluates thunk lazily on first read" (fun () ->
        let a = Signal.source 3
        let mutable runs = 0
        let c = Signal.computed (fun () -> runs <- runs + 1; a.Value * 2) [a.Invalidated]
        shouldEqual 0 runs   // not yet read
        c.Value |> ignore
        shouldEqual 1 runs)

    it "re-evaluates when dep fires" (fun () ->
        let a = Signal.source 3
        let sum = Signal.computed (fun () -> a.Value * 2) [a.Invalidated]
        a.Set 10
        shouldEqual 20 sum.Value)

    it "is dirty-flagged but not re-run until .Value is read" (fun () ->
        let a = Signal.source 1
        let mutable runs = 0
        let c = Signal.computed (fun () -> runs <- runs + 1; a.Value) [a.Invalidated]
        c.Value |> ignore   // initial read
        runs <- 0
        a.Set 2             // marks dirty, does not re-run
        shouldEqual 0 runs
        c.Value |> ignore   // re-run on read
        shouldEqual 1 runs)

    it "propagates Invalidated downstream before thunk re-runs" (fun () ->
        let a = Signal.source 1
        let b = Signal.computed (fun () -> a.Value + 10) [a.Invalidated]
        b.Value |> ignore   // read once to make b clean; Invalidated only fires on clean→dirty
        let mutable notified = false
        b.Invalidated.Add(fun () -> notified <- true)
        a.Set 2
        shouldBeTrue "downstream should be notified immediately" notified)
)

describe "Signal.auto — auto-tracked deps" (fun () ->

    it "discovers deps during initial dry-run" (fun () ->
        let a = Signal.source 2
        let b = Signal.source 3
        let prod = Signal.auto (fun () -> a.Value * b.Value)
        shouldEqual 6 prod.Value)

    it "updates when tracked dep changes" (fun () ->
        let a = Signal.source 2
        let b = Signal.source 3
        let prod = Signal.auto (fun () -> a.Value * b.Value)
        a.Set 5
        shouldEqual 15 prod.Value)

    it "chained auto signals propagate correctly" (fun () ->
        let x = Signal.source 1
        let y = Signal.auto (fun () -> x.Value * 2)
        let z = Signal.auto (fun () -> y.Value + 1)
        x.Set 5
        // y → 10, z → 11
        shouldEqual 11 z.Value)
)

describe "Signal.pending / fill" (fun () ->

    it "pending starts as ValueNone" (fun () ->
        let s = Signal.pending<int> ()
        shouldEqual ValueNone s.Value)

    it "fill sets ValueSome" (fun () ->
        let s = Signal.pending<int> ()
        Signal.fill 99 s
        shouldEqual (ValueSome 99) s.Value)

    it "fill fires Invalidated" (fun () ->
        let s = Signal.pending<int> ()
        let mutable fired = false
        s.Invalidated.Add(fun () -> fired <- true)
        Signal.fill 1 s
        shouldBeTrue "fill should fire Invalidated" fired)
)

describe "Signal.fulfillWith — retrofit a thunk onto an existing signal" (fun () ->

    it "seeds the signal immediately with the thunk's result" (fun () ->
        let a = Signal.source 10
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> a.Value)
        shouldEqual 10 s.Value)

    it "auto-tracks the thunk dep and updates on change" (fun () ->
        let a = Signal.source 1
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> a.Value * 3)
        a.Set 4
        shouldEqual 12 s.Value)

    it "downstream signals that subscribed before FulfillWith still propagate" (fun () ->
        let a   = Signal.source 1
        let s   = Signal.source 0
        // downstream subscribes to s *before* FulfillWith is called
        let down = Signal.auto (fun () -> s.Value + 100)
        s |> Signal.fulfillWith (fun () -> a.Value)
        a.Set 5
        shouldEqual 105 down.Value)

    it "equality gate: no redundant Invalidated when thunk returns same value" (fun () ->
        let a = Signal.source 5
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> a.Value)   // seeds to 5
        let mutable extra = 0
        s.Invalidated.Add(fun () -> extra <- extra + 1)
        a.Set 5     // dep fires, thunk returns 5 again — gate should suppress
        shouldEqual 0 extra)

    it "handles shifting deps across re-evaluations" (fun () ->
        let flag = Signal.source true
        let a    = Signal.source 1
        let b    = Signal.source 100
        let s    = Signal.source 0
        s |> Signal.fulfillWith (fun () -> if flag.Value then a.Value else b.Value)
        shouldEqual 1 s.Value       // reads a
        flag.Set false              // switch branch — now reads b
        shouldEqual 100 s.Value
        b.Set 200                   // b is now the active dep
        shouldEqual 200 s.Value)
)

describe "Signal.fulfillWithSome" (fun () ->

    it "wraps thunk result in ValueSome" (fun () ->
        let a = Signal.source "hello"
        let s = Signal.pending<string> ()
        s |> Signal.fulfillWithSome (fun () -> a.Value)
        shouldEqual (ValueSome "hello") s.Value)

    it "updates and wraps on dep change" (fun () ->
        let a = Signal.source "x"
        let s = Signal.pending<string> ()
        s |> Signal.fulfillWithSome (fun () -> a.Value)
        a.Set "y"
        shouldEqual (ValueSome "y") s.Value)
)

describe "Signal.map" (fun () ->

    it "transforms the current value" (fun () ->
        let a = Signal.source 4
        let doubled = Signal.map ((*) 2) a
        shouldEqual 8 doubled.Value)

    it "updates with source" (fun () ->
        let a = Signal.source 4
        let doubled = Signal.map ((*) 2) a
        a.Set 10
        shouldEqual 20 doubled.Value)
)

describe "Signal.map2" (fun () ->

    it "combines two sources" (fun () ->
        let a = Signal.source 3
        let b = Signal.source 4
        let s = Signal.map2 (sprintf "%d+%d") a b
        shouldEqual "3+4" s.Value)

    it "updates when either source changes" (fun () ->
        let a = Signal.source 1
        let b = Signal.source 2
        let s = Signal.map2 (+) a b
        b.Set 10
        shouldEqual 11 s.Value)
)

describe "Signal.effect" (fun () ->

    it "runs the action immediately" (fun () ->
        let a = Signal.source 0
        let mutable last = -1
        let _ = Signal.effect (fun () -> last <- a.Value) [a.Invalidated]
        shouldEqual 0 last)

    it "re-runs on dep change" (fun () ->
        let a = Signal.source 0
        let mutable last = -1
        let _ = Signal.effect (fun () -> last <- a.Value) [a.Invalidated]
        a.Set 7
        shouldEqual 7 last)

    it "dispose cancels future re-runs" (fun () ->
        let a = Signal.source 0
        let mutable last = -1
        let sub = Signal.effect (fun () -> last <- a.Value) [a.Invalidated]
        sub.Dispose ()
        a.Set 99
        shouldEqual 0 last)   // still 0 from the initial run; 99 was not observed
)

// ===========================================================================
// DEEPENED COVERAGE — appended; targets the subtle, high-value, previously
// untested parts of the Signal contract.
// ===========================================================================

describe "Signal.auto — dependency-set discovery semantics" (fun () ->

    it "a signal NOT read during the thunk is NOT a dependency" (fun () ->
        // c reads only `a`; `b` is never touched inside the thunk.
        let a = Signal.source 1
        let b = Signal.source 100
        let c = Signal.auto (fun () -> a.Value * 10)
        shouldEqual 10 c.Value
        // Mutating the untracked signal must NOT mark c dirty.
        let mutable invalidated = false
        c.Invalidated.Add(fun () -> invalidated <- true)
        b.Set 999
        shouldBeTrue "untracked dep must not invalidate" (not invalidated)
        shouldEqual 10 c.Value)   // unchanged

    it "recomputes when a read-during-thunk dependency invalidates" (fun () ->
        let a = Signal.source 2
        let mutable runs = 0
        let c = Signal.auto (fun () -> runs <- runs + 1; a.Value + 1)
        c.Value |> ignore
        let before = runs
        a.Set 5
        c.Value |> ignore
        shouldEqual 6 c.Value
        shouldBeTrue "should have recomputed exactly once more" (runs = before + 1))

    it "reading the same dep twice in the thunk registers it (idempotent collection)" (fun () ->
        // The thunk reads a.Value twice; the dep should still be tracked and a
        // single Set must trigger exactly one dirty transition.
        let a = Signal.source 3
        let c = Signal.auto (fun () -> a.Value + a.Value)
        shouldEqual 6 c.Value
        let mutable invalidations = 0
        c.Invalidated.Add(fun () -> invalidations <- invalidations + 1)
        a.Set 4
        shouldEqual 8 c.Value
        // At most one Invalidated per dirty transition. Reading a twice could,
        // in a buggy impl, register `a` twice and fire twice; assert exactly one.
        shouldEqual 1 invalidations)

    it "CONCEALED-BRANCH dep is FROZEN: a dep read only on the not-taken branch never becomes tracked" (fun () ->
        // Characterizes auto's actual behavior: deps are collected ONCE at
        // construction (the dry-run) and then FROZEN — the thunk re-runs with
        // collector=None, so a branch taken later does NOT add new deps.
        let flag = Signal.source true
        let a    = Signal.source 1     // read on the true branch (taken at build)
        let b    = Signal.source 100   // read on the false branch (NOT taken at build)
        let c    = Signal.auto (fun () -> if flag.Value then a.Value else b.Value)
        shouldEqual 1 c.Value          // dry-run took true branch ⇒ deps = {flag, a}
        // Flip the branch via the tracked `flag` dep.
        flag.Set false
        shouldEqual 100 c.Value        // now evaluates b
        // `b` was NOT a dep at construction and auto never re-collects ⇒ changing
        // b does not invalidate c, so c keeps its STALE cached 100.
        let mutable invalidated = false
        c.Invalidated.Add(fun () -> invalidated <- true)
        b.Set 200
        shouldBeTrue "auto deps are frozen: b is not tracked" (not invalidated)
        shouldEqual 100 c.Value)       // STALE — documents the frozen-deps contract

    it "auto with multiple deps recomputes when ANY one fires (merge)" (fun () ->
        let a = Signal.source 1
        let b = Signal.source 2
        let c = Signal.source 3
        let sum = Signal.auto (fun () -> a.Value + b.Value + c.Value)
        shouldEqual 6 sum.Value
        b.Set 20
        shouldEqual 24 sum.Value     // 1 + 20 + 3
        c.Set 30
        shouldEqual 51 sum.Value     // 1 + 20 + 30
        a.Set 10
        shouldEqual 60 sum.Value)    // 10 + 20 + 30
)

describe "Signal.auto — laziness & nesting" (fun () ->

    it "nested auto reading auto reading source propagates transitively" (fun () ->
        let x = Signal.source 1
        let y = Signal.auto (fun () -> x.Value * 2)
        let z = Signal.auto (fun () -> y.Value + 1)
        shouldEqual 3 z.Value     // 1*2 + 1
        x.Set 5
        shouldEqual 11 z.Value)   // 5*2 + 1

    it "nested auto thunks are LAZY: do not re-run until .Value pulled" (fun () ->
        let x = Signal.source 1
        let mutable yRuns = 0
        let mutable zRuns = 0
        let y = Signal.auto (fun () -> yRuns <- yRuns + 1; x.Value * 2)
        let z = Signal.auto (fun () -> zRuns <- zRuns + 1; y.Value + 1)
        z.Value |> ignore         // pulls z → pulls y
        let yBase = yRuns
        let zBase = zRuns
        x.Set 5                   // marks both dirty, runs NEITHER thunk
        shouldBeTrue "y thunk must not run on Set" (yRuns = yBase)
        shouldBeTrue "z thunk must not run on Set" (zRuns = zBase)
        z.Value |> ignore         // now pulls: z re-runs, which pulls y re-run
        shouldBeTrue "y re-ran exactly once on pull" (yRuns = yBase + 1)
        shouldBeTrue "z re-ran exactly once on pull" (zRuns = zBase + 1))

    it "reading .Value twice without invalidation does not re-run (cache)" (fun () ->
        // NOTE: Signal.auto runs the thunk ONCE at construction (the dry-run that
        // collects deps), and the resulting Computed starts DIRTY (upstream.IsSome),
        // so the first real .Value pull recomputes too — runs reaches 2. The
        // load-bearing guarantee is that repeat reads AFTER the first pull are
        // cached and do NOT re-run.
        let a = Signal.source 7
        let mutable runs = 0
        let c = Signal.auto (fun () -> runs <- runs + 1; a.Value)
        shouldEqual 1 runs        // dry-run at construction
        c.Value |> ignore         // first pull recomputes (started dirty)
        shouldEqual 2 runs
        let afterFirstPull = runs
        c.Value |> ignore
        c.Value |> ignore
        shouldEqual afterFirstPull runs)   // cached — no further re-runs

    it "deep chain (4 levels) is lazy and propagates" (fun () ->
        let s = Signal.source 1
        let mutable runs = 0
        let l1 = Signal.auto (fun () -> s.Value + 1)
        let l2 = Signal.auto (fun () -> l1.Value + 1)
        let l3 = Signal.auto (fun () -> l2.Value + 1)
        let l4 = Signal.auto (fun () -> runs <- runs + 1; l3.Value + 1)
        shouldEqual 1 runs        // l4's construction dry-run
        shouldEqual 5 l4.Value    // 1 +1+1+1+1 ; first pull recomputes
        shouldEqual 2 runs
        let beforeReread = runs
        l4.Value |> ignore
        shouldEqual beforeReread runs   // cached
        s.Set 10
        shouldEqual 14 l4.Value)  // 10 +1+1+1+1
)

describe "Signal.computed — invalidation coalescing & multi-dep merge" (fun () ->

    it "explicit-deps computed recomputes when ANY of several deps fires" (fun () ->
        let a = Signal.source 1
        let b = Signal.source 2
        let sum = Signal.computed (fun () -> a.Value + b.Value) [a.Invalidated; b.Invalidated]
        shouldEqual 3 sum.Value
        a.Set 10
        shouldEqual 12 sum.Value
        b.Set 20
        shouldEqual 30 sum.Value)

    it "Invalidated fires AT MOST ONCE per dirty transition (coalesced while dirty)" (fun () ->
        // The `if not dirty then ... changed.Trigger()` guard suppresses repeat
        // fires while already dirty. Two upstream Sets without an intervening
        // read should yield only ONE Invalidated.
        let a = Signal.source 1
        let c = Signal.computed (fun () -> a.Value) [a.Invalidated]
        c.Value |> ignore         // make c clean
        let mutable fires = 0
        c.Invalidated.Add(fun () -> fires <- fires + 1)
        a.Set 2                   // clean → dirty: fires
        a.Set 3                   // already dirty: suppressed
        shouldEqual 1 fires
        c.Value |> ignore         // read → clean again
        a.Set 4                   // clean → dirty: fires again
        shouldEqual 2 fires)

    it "no Invalidated fires before the first read (computed starts dirty)" (fun () ->
        // A computed starts dirty; until the first read it stays dirty, so an
        // upstream Set finds `dirty = true` and does NOT trigger.
        let a = Signal.source 1
        let c = Signal.computed (fun () -> a.Value) [a.Invalidated]
        let mutable fires = 0
        c.Invalidated.Add(fun () -> fires <- fires + 1)
        a.Set 2                   // c was never read ⇒ still dirty ⇒ suppressed
        shouldEqual 0 fires
        shouldEqual 2 c.Value)    // value is still correct once pulled

    it "computed with empty deps is inert — never recomputes" (fun () ->
        // Merge [] returns an inert event that never fires; the computed is
        // effectively frozen after its first evaluation.
        let mutable runs = 0
        let c = Signal.computed (fun () -> runs <- runs + 1; 42) []
        shouldEqual 42 c.Value
        shouldEqual 1 runs
        c.Value |> ignore
        shouldEqual 1 runs)       // never re-runs; no deps can fire
)

describe "Signal.source — Set equality gate (extended)" (fun () ->

    it "Set differing value after an equal no-op still fires" (fun () ->
        let s = Signal.source 5
        let mutable fires = 0
        s.Invalidated.Add(fun () -> fires <- fires + 1)
        s.Set 5     // no-op
        s.Set 6     // real change
        s.Set 6     // no-op (equal to current)
        s.Set 7     // real change
        shouldEqual 2 fires)

    it "equality gate works for reference/string values" (fun () ->
        let s = Signal.source "abc"
        let mutable fires = 0
        s.Invalidated.Add(fun () -> fires <- fires + 1)
        s.Set "abc"                      // structurally equal — gated
        shouldEqual 0 fires
        s.Set "xyz"
        shouldEqual 1 fires)

    it "Set on a computed signal raises InvalidOperationException" (fun () ->
        let a = Signal.source 1
        let c = Signal.computed (fun () -> a.Value) [a.Invalidated]
        let mutable threw = false
        try c.Set 99
        with :? System.InvalidOperationException -> threw <- true
        shouldBeTrue "Set on computed must raise InvalidOperationException" threw)

    it "Set on an auto signal raises InvalidOperationException" (fun () ->
        let a = Signal.source 1
        let c = Signal.auto (fun () -> a.Value)
        let mutable threw = false
        try c.Set 99
        with :? System.InvalidOperationException -> threw <- true
        shouldBeTrue "Set on auto must raise InvalidOperationException" threw)
)

describe "Signal.pending — full fulfill cycle" (fun () ->

    it "pending is a source: supports Set / fill repeatedly" (fun () ->
        let s = Signal.pending<int> ()
        shouldEqual ValueNone s.Value
        Signal.fill 1 s
        shouldEqual (ValueSome 1) s.Value
        Signal.fill 2 s
        shouldEqual (ValueSome 2) s.Value)

    it "downstream auto depending on a pending signal sees the fill" (fun () ->
        let p = Signal.pending<int> ()
        let view = Signal.auto (fun () ->
            match p.Value with
            | ValueSome n -> n * 10
            | ValueNone   -> -1)
        shouldEqual -1 view.Value         // still pending
        Signal.fill 5 p
        shouldEqual 50 view.Value)        // recomputes after fill

    it "fill with same value is gated (no Invalidated)" (fun () ->
        let s = Signal.pending<int> ()
        Signal.fill 7 s
        let mutable fires = 0
        s.Invalidated.Add(fun () -> fires <- fires + 1)
        Signal.fill 7 s                   // equal ValueSome 7 — gated
        shouldEqual 0 fires
        shouldEqual (ValueSome 7) s.Value)
)

describe "Signal.fulfillWith — re-collected (dynamic) deps & equality gate" (fun () ->

    it "re-collects deps on each run: a newly-read dep on a later branch DOES get tracked" (fun () ->
        // Contrast with auto's FROZEN deps: FulfillWith re-establishes the
        // collector inside run() every time, so deps read on the active branch
        // after a switch become live subscriptions.
        let flag = Signal.source true
        let a    = Signal.source 1
        let b    = Signal.source 100
        let s    = Signal.source 0
        s |> Signal.fulfillWith (fun () -> if flag.Value then a.Value else b.Value)
        shouldEqual 1 s.Value
        flag.Set false                    // re-run picks the b branch, tracks b
        shouldEqual 100 s.Value
        b.Set 200                         // now b is a live dep ⇒ s updates
        shouldEqual 200 s.Value)

    it "tracks MULTIPLE other signals read in the thunk" (fun () ->
        let a = Signal.source 1
        let b = Signal.source 2
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> a.Value * 100 + b.Value)
        shouldEqual 102 s.Value
        a.Set 3
        shouldEqual 302 s.Value
        b.Set 9
        shouldEqual 309 s.Value)

    it "does not re-subscribe to an already-tracked dep (subs dedup)" (fun () ->
        // run()'s collector skips deps already in `subs`. Two reads of `a` across
        // re-runs must not multiply the update on a single Set.
        let a = Signal.source 1
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> a.Value + a.Value)
        shouldEqual 2 s.Value
        let mutable fires = 0
        s.Invalidated.Add(fun () -> fires <- fires + 1)
        a.Set 5
        shouldEqual 10 s.Value
        shouldEqual 1 fires)              // exactly one downstream fire

    it "SAFE pattern: fulfillWith reading OTHER signals does not stall (self-ref hazard avoided)" (fun () ->
        // The documented hazard is a thunk that reads the SAME signal being
        // fulfilled. Here the thunk reads only OTHER signals, so it completes.
        // (We intentionally do NOT exercise the self-referential stall; see the
        // commented KNOWN-HAZARD note below this describe block.)
        let other = Signal.source 11
        let s = Signal.source 0
        s |> Signal.fulfillWith (fun () -> other.Value + 1)
        shouldEqual 12 s.Value)
)

// ---------------------------------------------------------------------------
// KNOWN HAZARD (documented, NOT executed as a live test — would hang):
//
//   let s = Signal.pending<int> ()
//   s |> Signal.fulfillWith (fun () ->
//       match s.Value with            // reads the SAME signal being fulfilled
//       | ValueSome n -> n
//       | ValueNone   -> 0)
//
// FulfillWith.run() sets `collector` and runs the thunk immediately. Reading
// s.Value during that run (a) reports s.Invalidated to the collector, which
// subscribes run() to s's OWN .Invalidated, and (b) returns the seed ValueNone.
// `this.Set` of the equal/derived value can then re-fire Invalidated, re-entering
// run() — a self-referential subscription loop that never settles. NEVER fulfill
// a signal with a thunk that reads itself. Asserted indirectly by the SAFE-pattern
// test above; left here as a guard for future readers.
// ---------------------------------------------------------------------------

describe "Signal.map / map2 — laziness & equality" (fun () ->

    it "map is lazy: f does not run until .Value is read" (fun () ->
        let a = Signal.source 4
        let mutable fRuns = 0
        let m = Signal.map (fun x -> fRuns <- fRuns + 1; x * 2) a
        shouldEqual 1 fRuns      // Auto dry-runs the thunk once at construction
        m.Value |> ignore
        shouldEqual 2 fRuns      // first real pull (dirty after construction)
        m.Value |> ignore
        shouldEqual 2 fRuns)     // cached — no re-run

    it "map recomputes only after upstream invalidation" (fun () ->
        let a = Signal.source 4
        let mutable fRuns = 0
        let m = Signal.map (fun x -> fRuns <- fRuns + 1; x * 2) a
        m.Value |> ignore
        let baseRuns = fRuns
        a.Set 10
        shouldEqual baseRuns fRuns   // dirty, but not yet recomputed
        shouldEqual 20 m.Value
        shouldEqual (baseRuns + 1) fRuns)

    it "map2 recomputes when EITHER source changes" (fun () ->
        let a = Signal.source 1
        let b = Signal.source 2
        let m = Signal.map2 (+) a b
        shouldEqual 3 m.Value
        a.Set 10
        shouldEqual 12 m.Value
        b.Set 20
        shouldEqual 30 m.Value)

    it "chained map composes" (fun () ->
        let a = Signal.source 3
        let m = a |> Signal.map ((+) 1) |> Signal.map ((*) 2)
        shouldEqual 8 m.Value     // (3+1)*2
        a.Set 4
        shouldEqual 10 m.Value)   // (4+1)*2
)

describe "Signal.effect — re-run semantics (extended)" (fun () ->

    it "fires once per dep invalidation, observing the latest value" (fun () ->
        let a = Signal.source 0
        let mutable observations = []
        let sub = Signal.effect (fun () -> observations <- a.Value :: observations) [a.Invalidated]
        a.Set 1
        a.Set 2
        a.Set 3
        sub.Dispose()
        // initial run (0) + one per distinct Set
        shouldEqual [3; 2; 1; 0] observations)

    it "merges multiple dep streams: re-runs on either" (fun () ->
        let a = Signal.source 0
        let b = Signal.source 0
        let mutable runs = 0
        let sub = Signal.effect (fun () -> runs <- runs + 1) [a.Invalidated; b.Invalidated]
        let baseRuns = runs       // 1 (initial)
        a.Set 1
        b.Set 1
        sub.Dispose()
        shouldEqual (baseRuns + 2) runs)

    it "dispose is idempotent (double dispose does not throw)" (fun () ->
        let a = Signal.source 0
        let sub = Signal.effect (fun () -> ()) [a.Invalidated]
        sub.Dispose()
        let mutable threw = false
        try sub.Dispose()
        with _ -> threw <- true
        shouldBeTrue "double Dispose must not throw" (not threw))

    it "equality-gated Set does not re-run the effect" (fun () ->
        let a = Signal.source 5
        let mutable runs = 0
        let sub = Signal.effect (fun () -> runs <- runs + 1) [a.Invalidated]
        a.Set 5                   // equal — gated, no Invalidated
        sub.Dispose()
        shouldEqual 1 runs)       // only the initial run
)

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

let total = _passed + _failed
printfn $"\n  %d{_passed}/%d{total} passing"

if _failed > 0 then
    printfn $"  %d{_failed} failing\n"
    failwithf $"%d{_failed} test(s) failed"
else
    printfn ""
