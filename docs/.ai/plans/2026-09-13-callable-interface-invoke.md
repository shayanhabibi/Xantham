# Overloaded generic callable interfaces as `Invoke` members: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use `superpowers:subagent-driven-development`
> to implement this plan task by task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> REQUIRED READING before starting: `.claude/rules/generator-fixtures.md` (lab first, run
> everything, read almost none of it), `.claude/rules/comments.md`, and
> `docs/.ai/handovers/orphan-delegates.md` for how this was found.

**Goal:** A named object type whose whole content is call signatures, where those signatures
declare type parameters of their own, is declared as an F# interface carrying one `Invoke`
member per signature, instead of one delegate carrying every signature's type parameters on
its head.

**Why:** F# methods carry their own type parameters. Hoisting a signature's parameters onto a
declaration head is the workaround for a delegate having nowhere else to put them, and it is
what makes an overloaded generic callable unwritable. The `Invoke` form has no rank-2 problem
to work around.

## The input that motivates it

`solid-js/store`'s `SetStoreFunction<T>`: nine call signatures, no members, no construct
signatures. `K1` is declared as `K1 extends KeyOf<W<T>>` in one signature and
`K1 extends MutableKeyOf<W<T>>` in another.

Today it renders as nothing at all. `declaresInterface` (`Shape/Interfaces.fs:11`) excludes it
because its members, index infos and construct signatures are all empty, so it routes to
`shape-callbacks`, which builds one delegate from the first signature (`TR031`) and asks
`aliasTypeParams` (`Shape/Spec.fs:2411`) for the head. `aliasTypeParams` groups by
*(name, constraint)*, deliberately keeping a slot per bound, so the head carries `'K1` twice.
`Arity.unwritableHead` (`Shape/Arity.fs:194`) compares names, raises `RA007`, and drops the
declaration. `audit-coverage` then reports `AC001`.

Consumers currently read:

```fsharp
type StoreReturn<'T> = 'T * Action<obj, obj, obj, obj, obj, obj, obj, obj>
static member createStore<'T> ([<ParamArray>] __0: obj) : 'T * obj = jsNative
```

## The form already exists

`tests/Xantham.Generator.Tests/golden/callable-hybrid-lab/CallableHybridLab.fs` carries both
halves, and that lab is in the run gate (`RunGate/Program.fs`), so the shape is proven at
runtime rather than only compiled:

```fsharp
abstract Invoke: x: float -> float
abstract Invoke: x: float * y: float -> float
abstract Invoke<'T>: value: 'T -> 'T
```

`Shape/Interfaces.fs:265` builds `FsInvoke` members and raises `SI008`. Nothing new is needed in
render.

## Tasks

### Task 1 — the routing predicate

- [ ] Add a predicate to `Shape/Spec.fs`, beside `isPureCallback`. It holds for a type with more
      than one call signature where hoisting every signature's type parameters yields two entries
      sharing a name. Derive it from the same grouping `aliasTypeParams` uses, so the two agree by
      construction rather than by restatement — the `#84` review's `declarationOf` finding is the
      cost of two copies of one rule.
- [ ] Unit tests in `tests/Xantham.Generator.Tests/Shape.test.fs` over `Build.shapeModel`: one
      signature (false), two signatures with no type parameters (false), two sharing a name under
      one bound (false — `aliasTypeParams` collapses those), two sharing a name under two bounds
      (true).

### Task 2 — route it

- [ ] `declaresInterface` (`Shape/Interfaces.fs:11`) admits a type the predicate holds for.
- [ ] `shapeCallbacks` (`Shape/Callbacks.fs`) skips it.
- [ ] Confirm exactly one pass declares each such type. Two declarations of one name is the
      failure mode; `declaredOnce` in `Interfaces.fs` guards its own pass only.

### Task 3 — the lab

- [ ] `tests/fixtures/callable-overloads-lab/` — hand-authored, tracked, named `*-lab` so git,
      the compile gate and the run gate pick it up. Register one `fixtureTests` block in
      `Pipeline.test.fs`.
- [ ] Pin the positives and the negatives: an interface with two generic call signatures sharing
      a parameter name under different bounds (takes the `Invoke` form); one with a single generic
      signature (stays a delegate); one with several non-generic signatures (stays a delegate,
      `TR031` as today); one with members beside its call signatures (the existing hybrid path,
      unchanged).
- [ ] Add run-gate checks proving a consumer can call each `Invoke` overload. Requires the golden
      linked in `RunGate.fsproj` and checks in its `Program.fs` — a two-place addition.

### Task 4 — measure

- [ ] `dotnet fsi build.fsx -- findings` before and after. **`AC001` must fall from 8 to 7**, and
      `solid-js` must lose its `AC001` and `RA007` entirely.
- [ ] `TR031` is 37 corpus-wide. Every one is a site keeping only the first overload, so expect
      movement well beyond solid-js. Report the count and the tier counts per package.
- [ ] Expect several of `SetStoreFunction`'s nine overloads to collapse: their parameters
      (`Part<W<T>, K1>`, `W<T>[K1]`) widen to `obj`, so `dedupe-overloads` will merge the
      indistinguishable ones and raise `DO001`. Two or three surviving overloads is success; the
      goal is a typed callable, not nine of them.
- [ ] Confirm `type StoreReturn<'T> = 'T * SetStoreFunction<'T>` and that `createStore` returns
      the same, rather than `obj`.
- [ ] Full gate before handing over: `dotnet build Xantham.slnx`, then
      `dotnet fsi build.fsx -- test --update --run-gate`.

## Watch for

- **Arity at the reference site.** The whole point is that the head keeps arity 1 (`'T`), so
  `StoreReturn<T>` applies it with one argument. If any signature's parameters still reach the
  head, the reference mismatches and widens to `obj` again — the same outcome under a new name.
- **`RA007` should stop firing for this input rather than be weakened.** It is load-bearing: on
  `master` it did not exist and this declaration would have emitted an uncompilable duplicate
  head. Leave the rule alone.
- **Fable emit for a generic `Invoke`** reached through an imported interface. `callable-hybrid-lab`
  proves the simple cases at runtime; the run-gate checks in Task 3 are what extend that.
- Do not open a large golden or a `symbols.jsonl`. Grep them.

## Out of scope

- `type-fest`'s `PositiveInfinity` / `NegativeInfinity` (the other two `AC001`s). They carry
  `RE001` — the compiler could not answer `getDeclaredTypeOfSymbol` — so the cause is upstream of
  shaping and needs its own investigation.
- The four `exclusive-arms-lab` `AC001`s. Those are a deliberate fold that `Pipeline.test.fs:4315`
  pins; the arms' members survive under `AnchoredOptions`. `audit-coverage` keys on names, so it
  reports a fold as a drop. Worth fixing in `audit-coverage`, separately.
- The conservative hook guard in `Shape/Anonymous.fs`. `drop-orphan-delegates` retracts after the
  fact, so the prediction no longer has to be exact.
