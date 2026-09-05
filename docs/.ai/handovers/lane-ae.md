# Lane AE — callbacks as F# function types: the design gate refused

Wave seven, batch 1. Branch `worktree-gen-wave7-ae`, forked from `worktree-generator-wave-seven`
at `f708e34`.

**The corpus was not converted.** The design gate the brief set — "if any position curries, stop"
— failed in two of the five positions, one of them fatally. `D5` in
`docs/.ai/plans/generator-type-mapping.md:762` stands, and now stands on measured evidence rather
than on the assertion that "curried lambdas work well in Fable 5".

## The gate, position by position

Measured under Fable 5.2 against `tests/fixtures/callback-function-lab/index.js`, which reports
`fn.length` for every function it receives beside the result of calling it with all its arguments
at once. A curried chain is visible twice over: the length reads 1, and the call returns a function
where a string was declared.

The F# under test is hand-written in `tests/Xantham.Generator.RunGate/Probes.fs` (probes 4-8),
mirroring what the shape tier would emit if `FsDelegate` rendered as a function type. Probes.fs is
where a form is measured before a lane is built on it, which is exactly the question here.

| Position | Arity | JavaScript sees the declared arity | F# can consume it |
| --- | --- | --- | --- |
| parameter | 0, 1, 2, 3 | yes | yes |
| parameter, `unit` return (the `Action` arm) | 1 | yes | yes |
| parameter, through a named abbreviation | 2 | yes | yes |
| `ParamObject` Create, from a function-typed property | 2 | yes | yes |
| `ParamObject` Create, from a method member | 2, 0 | yes | yes |
| interface member, **read back from JavaScript** | 2 | **no — length 1** | yes |
| member return, function-typed property | 0 | yes | yes |
| **method return** | 0, 1 | yes | yes |
| **method return** | **2, 3** | yes | **no — throws** |

### The two failures

**A method's return of arity ≥ 2 throws.** `abstract make: seed: float -> (float -> float ->
string)` compiles the call site `(factory.make 5.0) 1.0 2.0` to `factory.make(5)(1)(2)`. The value
JavaScript handed over is a genuine 2-ary function (`.length` reads 2 — Fable inserts no wrapper on
a method's return), so the second application lands on a string and throws
`TypeError: factory.make(...)(...) is not a function`. Arities 0 and 1 pass because curried and
uncurried application coincide there. Arity 3 throws for the same reason as arity 2.

This is not a widening or an ergonomic cost. It is a binding that type-checks, compiles, and fails
at run time on the first call — the worst failure mode a generated binding has, and one no compile
gate would catch. `@cloudflare/workers-types` and `solid-js` both return callbacks from methods.

**Reading a function-typed member back hands F# a unary curry wrapper.** `abstract pair: (float ->
float -> string)` reads back with `.length` 1 rather than 2. Applying it from F# works — the
wrapper forwards correctly, and `factory.pair 1.0 2.0` returns `pair:1:2`. What is lost is the
value: a consumer who reads the member and passes it *on* to JavaScript passes a unary function.
`Func<float, float, string>` reads back as the function JavaScript actually holds, so this is a
regression the conversion would introduce, not a pre-existing one.

Both are now asserted positively in the run gate rather than left as failures, so the measurement
is pinned: a Fable release that fixes either one turns the gate red and says so.

## Why this settles D5 rather than deferring it

D5 named curried emission "a candidate config toggle later". The measurement says a toggle would
be a switch between a binding that works and a binding that throws, for the same declarations —
`@cloudflare/workers-types` has methods returning callbacks, so the toggle could not be recommended
under any setting. What could be defended is a **hybrid**: function types in parameter and
`ParamObject` position, where all four arities pass cleanly, and delegates retained on member
returns and on function-typed members read back. That is a real design, and every position it needs
is already measured green above. It is not this lane's to land, because the brief gated the whole
conversion on every position passing, and it does not.

Whoever picks the hybrid up should know that `Factory.Create` in the golden already shows the case
that compounds: a method whose return is itself a function synthesises
`Func<float, Func<float, float, string>>`, so a hybrid has to decide the nesting rule, not just the
outermost position.

## What landed

- `tests/fixtures/callback-function-lab/` — the lab. `index.d.ts` declares a callback in each of
  the five positions at arities 0 to 3; `index.js` is the runtime that reports arity.
- `tests/Xantham.Generator.RunGate/Probes.fs` — probes 4 to 8, the function-typed mirror.
- `tests/Xantham.Generator.RunGate/Program.fs` — `callbackFunctionForms ()`, 23 checks.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` — the lab's `fixtureTests` block, pinning what
  the delegate emission writes in each of the same positions.
- `tests/Xantham.Generator.Tests/golden/callback-function-lab/` — the generated golden.

Not touched: `Shape/Spec.fs`, `Shape/Callbacks.fs`, `Model.fs`, `Render.fs`, `Shape/Arity.fs`,
`Shape/Overloads.fs`, `Shape/ParamObjects.fs`. The lane's owned emission sites are unchanged, so it
merges into any other wave-seven lane without contest.

The lab's golden was **not** linked into the run gate's `.fsproj`. Linking it would compile the
delegate form the gate is not measuring; the probes carry the function-typed form the gate is.
A hybrid lane should link it and move the checks off `Probes.fs` at that point.

## Findings

`TR055` / `TR.CallbackKeptAsDelegate` is the lane's case and **fires nowhere**. It reports a
conversion attempted and refused, and no conversion was attempted. The Pipeline test asserts its
absence on the lab so the row does not quietly acquire a meaning later. The case, its
`FindingCodes.table` row and its `Findings.test.fs` line were pre-declared at dispatch and are
untouched — one dead row, which is the cost the dispatch plan budgeted for.

## Measurements

Delegate members, counted as `Func<` occurrences in the goldens (the metric the dispatch baseline
of 1,245 was taken with):

| Fixture | Before | After |
| --- | --- | --- |
| `@cloudflare/workers-types` | 559 | 559 |
| `animejs` | 515 | 515 |
| `solid-js` | 169 | 169 |
| `type-fest` | 2 | 2 |
| **corpus total** | **1,245** | **1,245** |
| `callback-function-lab` | — | 24 (new) |

Corpus tiers, summed over every manifest: `exact 464  ergonomic 1505  widened 774  escape 191`
before, unchanged after but for the new lab's own manifest
(`exact 11  ergonomic 3  widened 0  escape 0`). No finding count moved anywhere in the corpus; the
lane changed no emission.

Run gate: 179 checks pass, up from 156. Full `dotnet fsi build.fsx -- test` green.

## Left undone

The hybrid above. Nothing else — the lane's question was answered, and answered against the
conversion.
