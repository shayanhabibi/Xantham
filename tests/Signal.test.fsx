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
    printfn "\n  %s" name
    f ()
    _suite <- ""

/// Registers and immediately runs a single test case.
let it (name: string) (f: unit -> unit) =
    try
        f ()
        printfn "    \u2713 %s" name   // ✓
        _passed <- _passed + 1
    with ex ->
        printfn "    \u2717 %s" name   // ✗
        printfn "      %s" ex.Message
        _failed <- _failed + 1

/// Asserts structural equality; throws with a diff message on mismatch.
let shouldEqual<'a when 'a : equality> (expected: 'a) (actual: 'a) =
    if actual <> expected then
        failwithf "  expected: %A\n       got: %A" expected actual

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

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

let total = _passed + _failed
printfn "\n  %d/%d passing" _passed total
if _failed > 0 then
    printfn "  %d failing\n" _failed
    failwithf "%d test(s) failed" _failed
else
    printfn ""
