#nowarn FS3535


namespace Xantham.Mocha

open System
open System.Runtime.CompilerServices
open Fable.Core.Testing
open Fable.Core

(*
Code source from Zaid-ajaj `Fable.Mocha`.

Adjustments made so output works with IDE integrations, and click and run capabilities.
*)

[<StringEnum; RequireQualifiedAccess>]
type State =
    | Failed
    | Passed
    | Pending
type [<Interface; Discardable>] ITimeoutable<'T> =
    abstract timeout: unit -> int
    abstract timeout: int -> 'T
    abstract timeout: string -> 'T
type [<Interface; Discardable>] ISlowable<'T> =
    abstract slow: unit -> int
    abstract slow: int -> 'T
    abstract slow: string -> 'T
type [<Interface; Discardable>] ISkippable =
    abstract skip: unit -> unit
type [<Interface; Discardable>] IState =
    abstract state: State option with get,set
    abstract isPending: unit -> bool
    abstract isFailed: unit -> bool
    abstract isPassed: unit -> bool
type [<Interface; Discardable>] IRetryable<'T> =
    abstract retries: unit -> int
    abstract retries: int -> 'T
    abstract currentRetry: unit -> int
    abstract currentRetry: int -> 'T
type [<Interface>] IBailable<'T> =
    abstract bail: unit -> bool
    abstract bail: bool -> 'T
    

type [<Interface; Discardable>] Suite =
    abstract ctx: Context with get,set
    abstract suites: Suite array with get,set
    abstract tests: Test array with get,set
    abstract pending: bool with get,set
    abstract file: string option with get,set
    abstract root: bool with get,set
    abstract delayed: bool with get,set
    abstract parent: Suite option with get,set
    abstract title: string with get,set
    inherit ITimeoutable<Suite>
    inherit IRetryable<Suite>
    inherit ISlowable<Suite>
    inherit IBailable<Suite>
    abstract isPending: unit -> bool
    abstract clone: unit -> Suite
    abstract beforeAll: (Context -> unit) -> Suite
    abstract beforeAll: title: string * fn: (Context -> unit) -> Suite
    abstract afterAll: (Context -> unit) -> Suite
    abstract afterAll: title: string * fn: (Context -> unit) -> Suite
    abstract beforeEach: (Context -> unit) -> Suite
    abstract beforeEach: title: string * fn: (Context -> unit) -> Suite
    abstract afterEach: (Context -> unit) -> Suite
    abstract afterEach: title: string * fn: (Context -> unit) -> Suite
    abstract addSuite: Suite -> Suite
    abstract addTest: Test -> Suite
    abstract dispose: unit -> unit
    abstract fullTitle: unit -> string
    abstract titlePath: unit -> string array
    abstract total: unit -> int
    abstract eachTest: fn: (Test -> unit) -> Suite
    abstract run: unit -> unit
and [<Interface; Discardable>] Runnable =
    abstract id: string with get,set
    abstract title: string with get,set
    abstract fn: (unit -> unit) with get,set
    abstract body: string
    abstract async: bool with get,set
    abstract sync: bool with get,set
    abstract timedOut: bool with get,set
    abstract pending: bool with get,set
    abstract duration: int option with get,set
    abstract parent: Suite option with get,set
    abstract timer: obj option with get,set
    abstract ctx: Context option with get,set
    abstract callback: (exn option -> unit) option with get,set
    abstract allowUncaught: bool option with get,set
    abstract file: string option with get,set
    abstract resetTimeout: unit -> unit
    inherit ITimeoutable<Runnable>
    inherit IState
    inherit ISlowable<Runnable>
    inherit ISkippable
    inherit IRetryable<Runnable>
    
and [<Interface; Discardable>] Test =
    inherit Runnable
    abstract ``type``: string
and [<Interface; Discardable>] Hook = inherit Runnable

and [<Interface; Discardable>] Context =
    abstract test: Runnable option
    abstract currentTest: Test option with get,set
    abstract runnable: unit -> Runnable
    abstract runnable: Runnable -> Context
    inherit ITimeoutable<Context>
    inherit ISkippable
    inherit ISlowable<Context>

module Mocha =
    let [<Emit("describe($0, function() { $1(this) })")>] describe (name: string) (f: Suite -> unit): Suite = jsNative
    let [<Emit("describe.only($0, function() { $1(this) })")>] describeOnly (name: string) (f: Suite -> unit): Suite = jsNative
    let [<Emit("describe.skip($0, function() { $1(this) })")>] describeSkip (name: string) (f: Suite -> unit): Suite = jsNative
    let [<Emit("it($0, function() { $1(this) })")>] it (name: string) (f: Context -> unit): Test = jsNative
    let [<Emit("it.skip($0, function() { $1(this) })")>] itSkip (msg: string) (f: Context -> unit): Test = jsNative
    let [<Emit("it.only($0, function() { $1(this) })")>] itOnly (msg: string) (f: Context -> unit): Test = jsNative
    let [<Emit("before($0, function() { $1(this) })")>] before (msg: string) (f: Context -> unit): Hook = jsNative
    let [<Emit("beforeEach($0, function() { $1(this) })")>] beforeEach (msg: string) (f: Context -> unit): Hook = jsNative
    let [<Emit("after($0, function() { $1(this) })")>] after (msg: string) (f: Context -> unit): Hook = jsNative
    let [<Emit("afterEach($0, function() { $1(this) })")>] afterEach (msg: string) (f: unit -> unit): Hook = jsNative

type Accuracy = { absolute: float; relative: float }

module Accuracy =
  let inline areCloseLhs a b = abs(a-b)
  let inline areCloseRhs m a b = m.absolute + m.relative * max (abs a) (abs b)
  let inline areClose m a b = areCloseLhs a b <= areCloseRhs m a b
  let low = {absolute=1e-6; relative=1e-3}
  let medium = {absolute=1e-8; relative=1e-5}
  let high = {absolute=1e-10; relative=1e-7}
  let veryHigh = {absolute=1e-12; relative=1e-9}

[<RequireQualifiedAccess>]
module Runtime =
    [<Emit "this">]
    let inline getTestCase(): Context = jsNative

[<AutoOpen>]
module Test =
    let inline testSuite name ([<InlineIfLambda>] f: Suite -> unit) = Mocha.describe name f |> ignore
    let inline ptestSuite name ([<InlineIfLambda>] f: _ -> unit) = Mocha.describeSkip name f |> ignore
    let inline ftestSuite name ([<InlineIfLambda>] f: _ -> unit) = Mocha.describeOnly name f |> ignore
    let inline beforeTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.before name f |> ignore
    let inline beforeEachTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.beforeEach name f |> ignore
    let inline afterTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.after name f |> ignore
    let inline afterEachTests name ([<InlineIfLambda>] f: _ -> unit) : unit = Mocha.afterEach name f |> ignore
    let inline testCase name ([<InlineIfLambda>] f: _ -> unit) = Mocha.it name f |> ignore
    let inline ptestCase name ([<InlineIfLambda>] f: _ -> unit) = Mocha.itSkip name f |> ignore
    let inline ftestCase name ([<InlineIfLambda>] f: _ -> unit) = Mocha.itOnly name f |> ignore
    let inline failtest msg = failwith msg
    let inline failtestf fmt msg = failwithf fmt msg
    module Expecto =
        let inline testSuite name ([<InlineIfLambda>] f: _ -> unit): Suite = Mocha.describe name f 
        let inline ptestSuite name ([<InlineIfLambda>] f: _ -> unit): Suite = Mocha.describeSkip name f 
        let inline ftestSuite name ([<InlineIfLambda>] f: _ -> unit): Suite = Mocha.describeOnly name f 
        let inline beforeTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.before name f |> ignore
        let inline beforeEachTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.beforeEach name f |> ignore
        let inline afterTests name ([<InlineIfLambda>] f: _ -> unit): unit = Mocha.after name f |> ignore
        let inline afterEachTests name ([<InlineIfLambda>] f: _ -> unit) : unit = Mocha.afterEach name f |> ignore
        let inline testCase name ([<InlineIfLambda>] f: _ -> unit): Test = Mocha.it name f 
        let inline ptestCase name ([<InlineIfLambda>] f: _ -> unit): Test = Mocha.itSkip name f 
        let inline ftestCase name ([<InlineIfLambda>] f: _ -> unit): Test = Mocha.itOnly name f 
        let inline failtest msg = failwith msg
        let inline failtestf fmt msg = failwithf fmt msg
        let inline testList name (tests: Test list): Test =
            testSuite name <| fun suite ->
                for test in tests do
                    if test.``type`` |> Option.ofObj |> Option.exists ((=) "test") then
                        suite.addTest(test)
                    else suite.addSuite(unbox test)
                    |> ignore
            |> unbox<Test>

[<RequireQualifiedAccess>]
module Env =
    [<Emit("new Function(\"try {return this===window;}catch(e){ return false;}\")")>]
    let isBrowser : unit -> bool = jsNative
    let insideBrowser = isBrowser()
    [<Emit("typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope")>]
    let insideWorker :  bool = jsNative

module Nib =
    open Scriptorium.Nib.Assertion
    let inline stopPrn assertion: Assertion<_, _> = fun state -> if state.Stopped then state else assertion state
    let assertForEach (inner: Assertion<_, _>): Assertion<_, _> =
        stopPrn <| fun state ->
            let initialSubject = state.Subject
            let mutable counter = -1
            let finalState =
                initialSubject
                |> Seq.fold (fun state x ->
                    counter <- counter + 1
                    let initialTags = state.Tags
                    let nextState =
                        {
                            Subject = x
                            Errors = state.Errors
                            Tags = initialTags @ [ $"[%i{counter}]" ]
                            Stopped = state.Stopped
                        }
                        |> inner
                    { nextState with Tags = initialTags }
                    ) (unbox state)
            {
                Subject = initialSubject
                Errors = finalState.Errors
                Tags = finalState.Tags
                Stopped = finalState.Stopped
            }
    type SRTPHelper =
        static member inline forEach(inner: Assertion<_, _>, state: AssertionState<_>) = assertForEach inner <| state
        static member inline forEach(inner: Assertion<_, _>, subject: 'a when 'a :> 'b seq) = assertThat subject (assertForEach inner)
    let popTag: Assertion<_, _> = fun state -> { state with Tags = state.Tags |> List.tail }
    let withTag tagName assertion =
        tag tagName >> assertion >> popTag
    let projectedAssertion projection test printer =
        assertion (projection >> test) (fun orig -> projection orig |> printer orig)
    let insideTuple projection assertion =
        inside (fun x -> (x, projection x)) assertion
    let inline projectedInside projection assertion = insideTuple projection assertion
    let focusTuple projection =
        focus (fun x -> (x, projection x))
    let inline assertThat assertion subject =
        assertThat subject assertion
    let inline foreach<^T, ^Result, ^A, ^B when (^T or SRTPHelper):(static member forEach: Assertion<^A, ^B> * ^T -> ^Result)> (inner: Assertion<^A, ^B>) (subject: ^T) =
        ((^T or SRTPHelper):(static member forEach: Assertion<^A, ^B> * ^T -> ^Result) (inner, subject))
    let inline projectedForEach projection inner subject =
        foreach (insideTuple projection inner) subject
    let inline all predicate = foreach (satisfy predicate)
    let inline exists predicate = assertion (Seq.exists predicate) (sprintf "Expected collection to contain at least one element satisfying the predicate, but got none.\n\n%A")
    let inline hasLength length =
        projectedAssertion Seq.length _.Equals(length) (fun collection length -> sprintf "Expected collection to have length %i, but got %i.\n\n%A" length (Seq.length collection) collection)
    let inline withSome<'T>: Assertion<'T option,'T> = Option.value
    let equal expected = isEqualTo expected
    let notEqual expected = isNotEqualTo expected
    let isNotNaN = satisfy (not << Double.IsNaN)
    let isNaN = satisfy Double.IsNaN
    let isZero = isEqualTo 0
    let isNonZero = isNotEqualTo 0
    let inline skipIfError<'T>: Assertion<'T> = fun state ->
        if state.Errors.Length > 0
        then
            Runtime.getTestCase().skip()
            { state with Stopped = true }
        else state
    let isNotEmpty<'a, 'b when 'a :> 'b seq>: Assertion<'a> = satisfy (Seq.isEmpty >> not)
    let isEmpty<'a, 'b when 'a :> 'b seq>: Assertion<'a> = satisfy Seq.isEmpty
    let hasLengthGreaterThan length = inside Seq.length (tag "length" >> isGreaterThan length)
    let hasLengthGreaterOrEqual length = inside Seq.length (tag "length" >> isGreaterOrEqual length)
    let hasLengthLessThan length = inside Seq.length (tag "length" >> isLessThan length)
    let hasLengthLessOrEqual length = inside Seq.length (tag "length" >> isLessOrEqual length)
    let ifTrueThen predicate assertion =
        fun state ->
            if predicate state.Subject then
                let nextState =
                    state
                    |> assertion
                { state with Errors = nextState.Errors; Stopped = nextState.Stopped }
            else state
    let ifTrueThenOrElse predicate trueBranch falseBranch: Assertion<_, _> =
        fun state ->
            if predicate state.Subject then
                let nextState =
                    state
                    |> trueBranch
                { state with Errors = nextState.Errors; Stopped = nextState.Stopped }
            else
                let nextState =
                    state
                    |> falseBranch
                { state with Errors = nextState.Errors; Stopped = nextState.Stopped }
    let apply (f: 'a -> unit): Assertion<'a> = fun state -> f state.Subject; state
    let branchInsideResult (ifOk: Assertion<'a, 'b>) (ifError: Assertion<'c, 'd>): Assertion<Result<'a, 'c>> =
        stopPrn <| fun state ->
            match state.Subject with
            | Ok x ->
                let next =
                    {
                        Subject = x
                        Errors = state.Errors
                        Stopped = state.Stopped
                        Tags = state.Tags
                    }
                    |> withTag "ok" ifOk
                { state with Errors = next.Errors; Stopped = next.Stopped }
            | Error x ->
                let next =
                    {
                        Subject = x
                        Errors = state.Errors
                        Stopped = state.Stopped
                        Tags = state.Tags
                    }
                    |> withTag "error" ifError
                { state with Errors = next.Errors; Stopped = next.Stopped }
    let inline private ingestState (subject: AssertionState<'a>) (state: AssertionState<'b>) =
        { subject with Errors = state.Errors; Stopped = state.Stopped }
    let inline private injectSubjectAssertAndIngest (subject: 'a) (assertion: Assertion<'a, 'b>) (state: AssertionState<'c>) =
        {
            Subject = subject
            Errors = state.Errors
            Stopped = state.Stopped
            Tags = state.Tags
        }
        |> assertion
        |> ingestState state
    let inline private mapAssertionAndIngest (mapping: 'c -> 'a) (assertion: Assertion<'a, 'b>) (state: AssertionState<'c>) =
        state
        |> injectSubjectAssertAndIngest (mapping state.Subject) assertion
    let branchInside
        (projection: 'a -> Choice<'b, 'c>)
        (assertion: Assertion<'b, _>)
        (assertion2: Assertion<'c, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of2 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of2 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
    let branchInsideOption
        (projection: 'a -> 'b option)
        (assertion: Assertion<'b, _>): Assertion<'a> =
        stopPrn <| fun state ->
            projection state.Subject
            |> Option.map (injectSubjectAssertAndIngest >> fun fn -> fn (withTag "some" assertion) state)
            |> Option.defaultValue state
    let projectedBranchInside
        (branchProjection: 'a -> Choice<'b, 'c>)
        (assertion: Assertion<'a * 'b, _>)
        (assertion2: Assertion<'a * 'c, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match branchProjection state.Subject with
            | Choice1Of2 x -> injectSubjectAssertAndIngest (state.Subject,x) (withTag "choice1" assertion) state
            | Choice2Of2 x -> injectSubjectAssertAndIngest (state.Subject,x) (withTag "choice2" assertion2) state
        
    let branchInside3
        (projection: 'a -> Choice<'ca, 'cb, 'cc>)
        (assertion: Assertion<'ca, _>)
        (assertion2: Assertion<'cb, _>)
        (assertion3: Assertion<'cc, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of3 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of3 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | Choice3Of3 x -> injectSubjectAssertAndIngest x (withTag "choice3" assertion3) state
    let projectedBranchInside3
        (projection: 'a -> Choice<'ca, 'cb, 'cc>)
        (assertion: Assertion<'a * 'ca, _>)
        (assertion2: Assertion<'a * 'cb, _>)
        (assertion3: Assertion<'a * 'cc, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of3 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice1" assertion) state
            | Choice2Of3 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice2" assertion2) state
            | Choice3Of3 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice3" assertion3) state
    let branchInside4
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd>)
        (assertion: Assertion<'ca, _>)
        (assertion2: Assertion<'cb, _>)
        (assertion3: Assertion<'cc, _>)
        (assertion4: Assertion<'cd, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of4 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of4 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | Choice3Of4 x -> injectSubjectAssertAndIngest x (withTag "choice3" assertion3) state
            | Choice4Of4 x -> injectSubjectAssertAndIngest x (withTag "choice4" assertion4) state
    let projectedBranchInside4
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd>)
        (assertion: Assertion<'a * 'ca, _>)
        (assertion2: Assertion<'a * 'cb, _>)
        (assertion3: Assertion<'a * 'cc, _>)
        (assertion4: Assertion<'a * 'cd, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of4 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice1" assertion) state
            | Choice2Of4 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice2" assertion2) state
            | Choice3Of4 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice3" assertion3) state
            | Choice4Of4 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice4" assertion4) state
    let branchInside5
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce>)
        (assertion: Assertion<'ca, _>)
        (assertion2: Assertion<'cb, _>)
        (assertion3: Assertion<'cc, _>)
        (assertion4: Assertion<'cd, _>)
        (assertion5: Assertion<'ce, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of5 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of5 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | Choice3Of5 x -> injectSubjectAssertAndIngest x (withTag "choice3" assertion3) state
            | Choice4Of5 x -> injectSubjectAssertAndIngest x (withTag "choice4" assertion4) state
            | Choice5Of5 x -> injectSubjectAssertAndIngest x (withTag "choice5" assertion5) state
    let projectedBranchInside5
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce>)
        (assertion: Assertion<'a * 'ca, _>)
        (assertion2: Assertion<'a * 'cb, _>)
        (assertion3: Assertion<'a * 'cc, _>)
        (assertion4: Assertion<'a * 'cd, _>)
        (assertion5: Assertion<'a * 'ce, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of5 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice1" assertion) state
            | Choice2Of5 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice2" assertion2) state
            | Choice3Of5 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice3" assertion3) state
            | Choice4Of5 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice4" assertion4) state
            | Choice5Of5 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice5" assertion5) state
    let branchInside6
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce, 'cf>)
        (assertion: Assertion<'ca, _>)
        (assertion2: Assertion<'cb, _>)
        (assertion3: Assertion<'cc, _>)
        (assertion4: Assertion<'cd, _>)
        (assertion5: Assertion<'ce, _>)
        (assertion6: Assertion<'cf,_>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of6 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of6 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | Choice3Of6 x -> injectSubjectAssertAndIngest x (withTag "choice3" assertion3) state
            | Choice4Of6 x -> injectSubjectAssertAndIngest x (withTag "choice4" assertion4) state
            | Choice5Of6 x -> injectSubjectAssertAndIngest x (withTag "choice5" assertion5) state
            | Choice6Of6 x -> injectSubjectAssertAndIngest x (withTag "choice6" assertion6) state
    let projectedBranchInside6
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce, 'cf>)
        (assertion: Assertion<'a * 'ca, _>)
        (assertion2: Assertion<'a * 'cb, _>)
        (assertion3: Assertion<'a * 'cc, _>)
        (assertion4: Assertion<'a * 'cd, _>)
        (assertion5: Assertion<'a * 'ce, _>)
        (assertion6: Assertion<'a * 'cf,_>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice1" assertion) state
            | Choice2Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice2" assertion2) state
            | Choice3Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice3" assertion3) state
            | Choice4Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice4" assertion4) state
            | Choice5Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice5" assertion5) state
            | Choice6Of6 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice6" assertion6) state
    let branchInside7
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce, 'cf, 'cg>)
        (assertion: Assertion<'ca, _>)
        (assertion2: Assertion<'cb, _>)
        (assertion3: Assertion<'cc, _>)
        (assertion4: Assertion<'cd, _>)
        (assertion5: Assertion<'ce, _>)
        (assertion6: Assertion<'cf,_>)
        (assertion7: Assertion<'cg, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of7 x -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Choice2Of7 x -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | Choice3Of7 x -> injectSubjectAssertAndIngest x (withTag "choice3" assertion3) state
            | Choice4Of7 x -> injectSubjectAssertAndIngest x (withTag "choice4" assertion4) state
            | Choice5Of7 x -> injectSubjectAssertAndIngest x (withTag "choice5" assertion5) state
            | Choice6Of7 x -> injectSubjectAssertAndIngest x (withTag "choice6" assertion6) state
            | Choice7Of7 x -> injectSubjectAssertAndIngest x (withTag "choice7" assertion7) state
    let projectedBranchInside7
        (projection: 'a -> Choice<'ca, 'cb, 'cc, 'cd, 'ce, 'cf, 'cg>)
        (assertion: Assertion<'a * 'ca, _>)
        (assertion2: Assertion<'a * 'cb, _>)
        (assertion3: Assertion<'a * 'cc, _>)
        (assertion4: Assertion<'a * 'cd, _>)
        (assertion5: Assertion<'a * 'ce, _>)
        (assertion6: Assertion<'a * 'cf,_>)
        (assertion7: Assertion<'a * 'cg, _>): Assertion<'a> =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Choice1Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice1" assertion) state
            | Choice2Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice2" assertion2) state
            | Choice3Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice3" assertion3) state
            | Choice4Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice4" assertion4) state
            | Choice5Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice5" assertion5) state
            | Choice6Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice6" assertion6) state
            | Choice7Of7 x -> injectSubjectAssertAndIngest (state.Subject, x) (withTag "choice7" assertion7) state
    let branchInsideFor projection assertion assertion2 =
        stopPrn <| fun state ->
            match projection state.Subject with
            | Some (Choice1Of2 x) -> injectSubjectAssertAndIngest x (withTag "choice1" assertion) state
            | Some (Choice2Of2 x) -> injectSubjectAssertAndIngest x (withTag "choice2" assertion2) state
            | None -> state

[<RequireQualifiedAccess>]
module Expect =
    let inline skipWithMsg msg =
        let ctx = Runtime.getTestCase()
        let runnable = ctx.runnable()
        runnable.title <- runnable.title + " || [SKIPPED] " + msg
        ctx.skip()
    let inline skip () = Runtime.getTestCase().skip()
    let inline equal (actual: 'a) (expected: 'a) msg : unit =
        if actual = expected || not (Env.isBrowser()) then
            Assert.AreEqual(actual, expected, msg)
        else
            let valueType = actual.GetType()
            let primitiveTypes = [ typeof<int>; typeof<bool>; typeof<double>; typeof<string>; typeof<decimal>; typeof<Guid> ]
            let errorMsg =
                if List.contains valueType primitiveTypes then
                    sprintf "<span style='color:black'>Expected:</span> <br /><div style='margin-left:20px; color:crimson'>%s</div><br /><span style='color:black'>Actual:</span> </br ><div style='margin-left:20px;color:crimson'>%s</div><br /><span style='color:black'>Message:</span> </br ><div style='margin-left:20px; color:crimson'>%s</div>" (string expected) (string actual) msg
                else
                    sprintf "<span style='color:black'>Expected:</span> <br /><div style='margin-left:20px; color:crimson'>%A</div><br /><span style='color:black'>Actual:</span> </br ><div style='margin-left:20px;color:crimson'>%A</div><br /><span style='color:black'>Message:</span> </br ><div style='margin-left:20px; color:crimson'>%s</div>" expected actual msg

            raise (Exception(errorMsg))
    let notEqual actual expected msg : unit =
        Assert.NotEqual(actual, expected, msg)
    let private isNull' cond =
        match box cond with
        | null -> true
        | _ -> false
    let isNull cond = equal (isNull' cond) true
    let isNotNull cond = notEqual (isNull' cond) true
    let isNotNaN cond msg = if Double.IsNaN cond then failwith msg
    let isNotInfinity cond msg = if Double.IsInfinity cond then failwith msg
    let isTrue cond = equal cond true
    let isFalse cond = equal cond false
    let isZero cond = equal cond 0
    let isEmpty (x: 'a seq) msg = if not (Seq.isEmpty x) then failwithf "%s. Should be empty." msg
    let pass() = equal true true "The test passed"
    let passWithMsg (message: string) = equal true true message
    let exists (x: 'a seq) (a: 'a -> bool) msg = if not (Seq.exists a x) then failwith msg
    let all (x: 'a seq) (a: 'a -> bool) msg = if not (Seq.forall a x) then failwith msg
    /// Expect the passed sequence not to be empty.
    let isNonEmpty (x: 'a seq) msg = if Seq.isEmpty x then failwithf "%s. Should not be empty." msg
    /// Expects x to be not null nor empty
    let isNotEmpty (x: 'a seq) msg =
        isNotNull x msg
        isNonEmpty x msg
    /// Expects x to be a sequence of length `number`
    let hasLength x number msg = equal (Seq.length x) number (sprintf "%s. Expected %A to have length %i" msg x number)
    /// Expects x to be Result.Ok
    let isOk x message =
        match x with
        | Ok _ -> passWithMsg message
        | Error x' -> failwithf "%s. Expected Ok, was Error(\"%A\")." message x'
    /// Expects the value to be a Result.Ok value and returns it or fails the test
    let wantOk x message =
        match x with
        | Ok x' ->
            passWithMsg message
            x'
        | Error x' -> failwithf "%s. Expected Ok, was Error(\"%A\")." message x'
    let stringContains (subject: string) (substring: string) message =
        if not (subject.Contains(substring))
        then failwithf "%s. Expected subject string '%s' to contain substring '%s'." message subject substring
        else passWithMsg message

    /// Expects x to be Result.Error
    let isError x message =
        match x with
        | Error _ -> passWithMsg message
        | Ok x' -> failwithf "%s. Expected Error _, was Ok(%A)." message x'
    let isSome x message =
        match x with
        | Some _ -> passWithMsg message
        | None -> failwithf "%s. Expected Some _, was None." message
    /// Expects the value to be a Some x value and returns x or fails the test
    let wantSome x message =
        match x with
        | Some x' ->
            passWithMsg message
            x'
        | None -> failwithf "%s. Expected Some _, was None." message
    /// Expects the value to be a Result.Error value and returns it or fails the test
    let wantError (x: Result<'a, 'b>) (message: string) =
        match x with
        | Error value ->
            passWithMsg message
            value
        | Ok value -> failwithf "%s. Expected Error _, was Ok(%A)." message value
    let isNone x message =
        match x with
        | None -> passWithMsg message
        | Some x' -> failwithf "%s. Expected None, was Some(%A)." message x'
    let private throws' f =
        try f ()
            None
        with exn ->
            Some exn
    /// Expects the passed function to throw an exception
    let throws f msg =
        match throws' f with
        | None -> failwithf "%s. Expected f to throw." msg
        | Some _ -> ()
    /// Expects the passed function to throw, then calls `cont` with the exception
    let throwsC f cont =
        match throws' f with
        | None -> failwithf "Expected f to throw."
        | Some exn -> cont exn
    let passes f =
        match throws' f with
        | None -> ()
        | Some exn -> raise exn
    /// Expects the `actual` sequence to contain all elements from `expected`
    /// It doesn't take into account the number of occurrences and the order of elements.
    /// Calling this function will enumerate both sequences; they have to be finite.
    let containsAll (actual : _ seq) (expected : _ seq) message =
        let actualEls, expectedEls = List.ofSeq actual, List.ofSeq expected
        let matchingEls =
            actualEls
            |> List.filter (fun a -> expectedEls |> List.contains a)

        let extraEls =
            actualEls
            |> List.filter (fun a -> not (matchingEls |> List.contains a))
        let missingEls =
            expectedEls
            |> List.filter (fun e -> not (matchingEls |> List.contains e))

        if List.isEmpty missingEls then
            ()
        else
            sprintf
                "%s. Sequence `actual` does not contain all `expected` elements. Missing elements from `actual`: %A. Extra elements in `actual`: %A"
                message
                missingEls
                extraEls
            |> failtest

    /// Expects `actual` and `expected` (that are both floats) to be within a
    /// given `accuracy`.
    let floatClose accuracy actual expected message =
        if Double.IsInfinity actual then
            failtestf "%s. Expected actual to not be infinity, but it was." message
        elif Double.IsInfinity expected then
            failtestf "%s. Expected expected to not be infinity, but it was." message
        elif Accuracy.areClose accuracy actual expected |> not then
            failtestf
                "%s. Expected difference to be less than %.20g for accuracy {absolute=%.20g; relative=%.20g}, but was %.20g. actual=%.20g expected=%.20g"
                message (Accuracy.areCloseRhs accuracy actual expected)
                accuracy.absolute accuracy.relative
                (Accuracy.areCloseLhs actual expected)
                actual expected

    /// Expects `actual` to be less than `expected` or to be within a
    /// given `accuracy`.
    let floatLessThanOrClose accuracy actual expected message =
        if actual>expected then floatClose accuracy actual expected message

    /// Expects `actual` to be greater than `expected` or to be within a
    /// given `accuracy`.
    let floatGreaterThanOrClose accuracy actual expected message =
        if actual<expected then floatClose accuracy actual expected message

module Flip =
    [<RequireQualifiedAccess>]
    module Expect =
        let inline skipIfEmpty (collection: 'a when 'a :> 'b seq) = if collection :> 'b seq |> Seq.isEmpty then Expect.skip()
        let inline skipIf (condition: 'a -> bool) (input: 'a) = if condition input then Expect.skip()
        let inline equal expected msg actual = Expect.equal actual expected msg
        let inline notEqual expected msg actual = Expect.notEqual actual expected msg
        let inline isNull msg cond = Expect.isNull cond msg
        let inline isNotNull msg cond = Expect.isNotNull cond msg
        let inline isNotNaN msg cond = Expect.isNotNaN cond msg
        let inline isNotInfinity msg cond = Expect.isNotInfinity cond msg
        let inline isTrue msg cond = Expect.isTrue cond msg
        let inline isFalse msg cond = Expect.isFalse cond msg
        let inline isZero msg cond = Expect.isZero cond msg
        let inline isEmpty msg x = Expect.isEmpty x msg
        let inline exists a msg x = Expect.exists x a msg
        let inline all a msg x = Expect.all x a msg
        let inline isNonEmpty msg x = Expect.isNonEmpty x msg
        let inline isNotEmpty msg x = Expect.isNotEmpty x msg
        let inline hasLength number msg x = Expect.hasLength x number msg
        let inline isOk msg x = Expect.isOk x msg
        let inline wantOk msg x = Expect.wantOk x msg
        let inline stringContains subString msg subject = Expect.stringContains subject subString msg
        let inline isError msg x = Expect.isError x msg
        let inline isSome msg x = Expect.isSome x msg
        let inline wantSome msg x = Expect.wantSome x msg
        let inline wantError msg x = Expect.wantError x msg
        let inline isNone msg x = Expect.isNone x msg
        let inline throws msg f = Expect.throws f msg
        let inline throwsC cont f = Expect.throwsC f cont
        let inline containsAll expected msg actual = Expect.containsAll actual expected msg
        let inline floatClose accuracy expected msg actual = Expect.floatClose accuracy actual expected msg
        let inline floatLessThanOrClose accuracy expected msg actual = Expect.floatLessThanOrClose accuracy actual expected msg
        let inline floatGreaterThanOrClose accuracy expected msg actual = Expect.floatGreaterThanOrClose accuracy actual expected msg
            
module Chain =
    [<RequireQualifiedAccess>]
    module Expect =
        let inline skipIfEmpty (collection: 'a when 'a :> 'b seq) = if collection :> 'b seq |> Seq.isEmpty then Expect.skip(); None else Some collection
        let inline skipIf (condition: 'a -> bool) (input: 'a) = if condition input then Expect.skip(); None else Some input
        let inline equal expected msg actual = Expect.equal actual expected msg; actual
        let inline notEqual expected msg actual = Expect.notEqual actual expected msg; actual
        let inline isNull msg actual = Flip.Expect.isNull msg actual; actual
        let inline isNotNull msg actual = Flip.Expect.isNotNull msg actual; actual
        let inline isNotNaN msg actual = Flip.Expect.isNotNaN msg actual; actual
        let inline isNotInfinity msg actual = Flip.Expect.isNotInfinity msg actual; actual
        let inline isTrue msg actual = Expect.isTrue actual msg; actual
        let inline isFalse msg actual = Expect.isFalse actual msg; actual
        let inline isZero msg actual = Expect.isZero actual msg; actual
        let inline isEmpty msg x = Expect.isEmpty x msg; x
        let inline exists a msg x = Flip.Expect.exists a msg x; x
        let inline all a msg x = Flip.Expect.all a msg x; x
        let inline isNonEmpty msg x = Expect.isNonEmpty x msg; x
        let inline isNotEmpty msg x = Expect.isNotEmpty x msg; x
        let inline hasLength number msg x = Expect.hasLength x number msg; x
        let inline isOk msg x = Expect.isOk x msg; x
        let inline stringContains subString msg subject = Expect.stringContains subject subString msg; subject
        let inline isError msg x = Expect.isError x msg; x
        let inline isSome msg x = Flip.Expect.isSome msg x; x
        let inline wantSome msg x = Flip.Expect.wantSome msg x
        let inline wantOk msg x = Flip.Expect.wantOk msg x
        let inline isNone msg x = Flip.Expect.isNone msg x; x
        let inline containsAll expected msg actual = Flip.Expect.containsAll  expected msg actual ; actual