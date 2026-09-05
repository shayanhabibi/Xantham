# Lane AK — callbacks of one argument or none become F# function types

Wave seven, batch 2. Branch `worktree-gen-wave7-ak`, forked from `worktree-generator-wave-seven`
at `d3dc55b`.

Lane AE measured the **curried** spelling, found a method's return of arity 2 or more throws, and
concluded that D5 stands. The conclusion was drawn too broadly: currying is one of two function
types F# offers. This lane measured the other one — **tupled** — and the arity slice where the two
coincide.

## The three spellings, position by position

Measured under Fable 5.2 against `tests/fixtures/callback-function-lab/index.js`, which reports
`fn.length` for every function it receives beside the result of calling it with all its arguments
at once. Each cell is the arity JavaScript saw, and whether the call worked.

| Position | `Func`/`Action` | Curried (AE) | Tupled (AK) |
| --- | --- | --- | --- |
| parameter, arity 0 | 0, works | 0, works | *no tupled spelling* |
| parameter, arity 1 | 1, works | 1, works | *no tupled spelling* |
| parameter, arity 2 | 2, works | 2, works | **1, arguments read `undefined`, no throw** |
| parameter, arity 3 | 3, works | 3, works | **1, arguments read `undefined`, no throw** |
| parameter, `unit` return, arity 1 | 1, works | 1, works | *no tupled spelling* |
| parameter, `unit` return, arity 2 | 2, works | *unmeasured by AE* | **1, arguments read `NaN`** |
| named abbreviation, arity 2 | 2, works | 2, works | **1, arguments read `undefined`** |
| `ParamObject` Create, from a property | 2, works | 2, works | **1, arguments read `undefined`** |
| `ParamObject` Create, from a method | 2, works | 2, works | **1, arguments read `undefined`** |
| interface member, read back, arity 1 | 1, works | *unmeasured by AE* | 1, works |
| interface member, read back, arity 2 | 2, works | **1, arity lost** | 2, **application passes one array** |
| function-typed property, read back, arity 0 | 0, works | 0, works | 0, works |
| function-typed property, read back, arity 2 | 2, works | **1, arity lost** | 2, **application passes one array** |
| method return, arity 0 | 0, works | 0, works | 0, works |
| method return, arity 1 | 1, works | 1, works | *no tupled spelling* |
| method return, arity 2 | 2, works | 2, **throws `TypeError`** | 2, **application passes one array** |
| method return, arity 3 | 3, works | 3, **throws `TypeError`** | 3, **application passes one array** |

**Tupled is worse than curried, and worse in a different way.** Fable compiles an F# tuple to a
JavaScript array, so a tupled callback crosses as a *one-argument* function over an array. In
parameter, `ParamObject` and abbreviation position the runtime's `fn(1, 2)` binds the array slot to
`1`, the destructuring reads `undefined`, and **nothing throws** — a silently wrong value rather
than curried's `TypeError`. Read-back inverts: Fable inserts no curry wrapper, so `.length` survives
at 2, but the F# application passes the tuple as a single array argument and JavaScript sees one
argument where it declared two.

So the tupled conversion the brief proposed was **not** made. What was made is the slice below.

## What converted, and the rule

At arity 0 and 1 the curried and tupled spellings coincide — there is one function type and no
arity for either to lose. **1,250 of the corpus's 1,615 delegate occurrences sit in that slice.**
Two further measurements were needed before it could be taken, and both are new:

- **A function type may not have a function type as its return.** Fable flattens `A -> (B -> C)`
  into one JavaScript function of the summed arity, so the runtime's first application returns the
  result rather than the inner callback. Arity does not rescue it: it fails with both levels unary.
  A delegate over a function (`Func<float, float -> string>`), a delegate over a delegate, and a
  function over a delegate all cross at both declared arities, so the constraint falls on the outer
  level alone and the inner one converts on its own terms.
- **Reading a function-typed member back at arity 1** hands over a wrapper of length 1, which is
  the arity JavaScript holds. Lane AE's curry-wrapper finding was taken at arity 2, where a wrapper
  of length 1 loses an argument; at arity 1 there is none to lose.

`Shape.Spec.callbackRef` is the whole decision, and every site that builds a callback reference
goes through it:

```
| _, FsFunc _     -> delegate, TR055 "its return is itself a callback"
| [], _           -> FsFunc(FsUnit, returns)
| [ argument ], _ -> FsFunc(argument, returns)
| _               -> delegate, TR055 $"it takes {n} arguments"
```

`Model.FsTypeRef` gains `FsFunc of FsTypeRef * FsTypeRef`. It renders **always parenthesised**:
`abstract handler: (float -> string) with get, set` is a property where the bare spelling is a
method, and `abstract make: seed: float -> (float -> string)` is a one-parameter method returning a
callback where the bare spelling takes two parameters.

### Positions that retained the delegate

Every retained site raises `TR055` with its reason — `it takes 2 arguments` (and 3, 4, 5, 7), or
`its return is itself a callback`. Corpus totals: `@cloudflare/workers-types` 180 symbols, `animejs`
120, `solid-js` 30, `type-fest` 0.

Nothing in the corpus retains for a reason other than those two. Optional parameters and
`[<ParamArray>]` rest parameters inside a callback signature convert normally, because the rule
reads the shaped parameter list rather than the TypeScript one —
`setTimeout(callback: ('Args -> unit), msDelay: float option, [<ParamArray>] args: 'Args)` is in the
regenerated cloudflare golden.

## Where the measurement lives

The lab's golden is now **linked into the run gate**, which lane AE deliberately left undone. The
delegate arm and the converted arm are therefore measured on the generated file rather than on a
hand-written mirror, so a change to the emission is measured rather than asserted about.
`Probes.fs` keeps only the arms that have no generated equivalent: the tupled spelling, the curried
spelling, and the two nesting forms the golden does not carry.

- `tests/fixtures/callback-function-lab/` gains `callVoidTwo` (an arity-2 void callback, so the
  `Action<A, B>` arm has more than one argument to guarantee), `callNesting`, `callNestingOne` and
  `drive` — the last takes a `Factory` built in F#, so its callback members cross outward rather
  than back, which is where a `ParamObject` literal puts them.
- `tests/Xantham.Generator.RunGate/Program.fs` — `callbackGoldenForms` (the emission),
  `callbackTupledForms` (the tupled spelling), `callbackMixedForms` (the nesting rule).
- `tests/Xantham.Generator.Tests/Shape.test.fs` — two new cases pinning the retention rule at the
  pass level, and four updated to the converted spelling.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` — the lab's block is four cases: what converts,
  what retains, what the nesting rule does, and that every retained site names its reason.

## Files touched in `src/`

`Model.fs` (`FsFunc`), `Render.fs` (`printTypeIn`, `qualifyRef`), `Shape/Spec.fs` (`callbackRef`,
`delegateRef`, `typeSpelling`, `typeVarsOf`), `Shape/Callbacks.fs`, `Shape/Arity.fs` (`mapRef`),
`Shape/Overloads.fs` (`normalize`), `Shape/ParamObjects.fs` (`parameterFor`, which now returns its
findings). `Findings.fs` is untouched — `TR055` was pre-declared at dispatch and is used as
declared.

## Measurements

Delegate occurrences, counted with a bracket-aware parse over the goldens so a nested `Func` counts.
The dispatch baseline of 1,245 counted `Func<` alone and saw neither `Action` nor nesting; that same
measure reads 1,245 before and 293 after.

| Fixture | Before | After | `TR055` symbols |
| --- | --- | --- | --- |
| `@cloudflare/workers-types` | 795 | 207 | 180 |
| `animejs` | 542 | 117 | 120 |
| `solid-js` | 271 | 41 | 30 |
| `type-fest` | 7 | 0 | 0 |
| **corpus total** | **1,615** | **365** | **330** |

Corpus tiers, summed over every manifest:

| | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| before | 490 | 1525 | 783 | 193 |
| after | 479 | 1540 | 783 | 193 |

`TR055` is the only finding whose count moved anywhere in the corpus; nothing widened or escaped
that did not before. The eleven symbols that left `exact` are the ones that acquired a `TR055`, and
the four extra symbols overall are the lab's new exports.

Gate: **460 generator tests, 90 wire tests, run gate 230 checks**, up from 456 / 90 / 179. Compile
gate green — the four npm goldens compile against `Fable.Core` 5.2.0 with the converted spelling.
`git diff --stat --ignore-cr-at-eol`: 65 files, +1408 / -1189.

## Left undone

Nothing this lane's brief asked for. Two observations for whoever picks callbacks up next:

- A function type inside an erased union (`U2<(obj -> unit), EventListenerObject<Event>>`) occurs in
  the cloudflare golden and compiles. The erasure is a no-op on the value, so the arity measured in
  bare position carries over, but no check exercises a union arm specifically.
- `TR055` at 330 corpus sites is a large ergonomic row. It is the honest report of a retained
  delegate, but a manager composing wave seven's manifests should expect it.
