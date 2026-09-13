# Callable-interface Invoke handover — overloaded-generic callables (4f3d695, 58c4938, 1212837, bc648dc)

Branch `worktree-gen-callable-invoke`, on `f5ff1a2`. Gated on the composed tree: `dotnet build
Xantham.slnx` 0 errors, `dotnet fsi build.fsx -- test --update --run-gate` — 766 + 90 Expecto,
run gate 453 checks, all green.

## The routing predicate (4f3d695, 58c4938)

`hasIncompatibleOverloadedTypeParameters` (`Shape/Spec.fs:190`) holds for a named callable whose
call signatures share a type-parameter name under two different bounds. Two passes read it, with
opposite signs: `declaresInterface` (`Shape/Interfaces.fs:23`) admits such a type, and
`shapeCallbacks` (`Shape/Callbacks.fs:63`) skips it. The pair is what keeps exactly one pass
declaring the name. `shape-interfaces` then renders an interface with one `Invoke` member per
signature, instead of one delegate head `aliasTypeParams` cannot write.

`callable-overloads-lab` (1212837, bc648dc) pins the tuple-return case. Its `Coalesce<T>` carries
its own `T` inside a call-signature bound (`<U extends T>`), mirroring `SetStoreFunction<T>`,
whose every call-signature bound reads `T` (`store.d.ts:78`); `makeCoalescer<T>(): [T,
Coalesce<T>]` mirrors `createStore<T>(): StoreReturn<T>`. The generated
`static member makeCoalescer<'T> () : 'T * Coalesce<'T>` keeps the reference at arity 1 and
unwidened, and two run-gate checks confirm the emitted `Invoke` reaches through the imported
interface at runtime.

What widens at a return position is a *concrete* application. Task 3's round 2 measured
`makeCoalescer(): [string, Coalesce<string>]` rendering `string * (obj -> obj)` under `TR031` and
`TR013` and generalised that to the outer type parameter itself. The goldens settle
it: applying the declaration's own free parameter keeps the name, at `createStore` and at
`makeCoalescer` alike.

## `AC001`: 8 before, 6 after

Per-fixture breakdown after: `exclusive-arms-lab` 4, `type-fest` 2. `setter-lab` and `solid-js`
both fall to zero. The plan's "out of scope" section predicts `exclusive-arms-lab`'s four and
`type-fest`'s two `PositiveInfinity`/`NegativeInfinity` pair (`RE001`) as the remainder — six, not
the brief's stated four. `setter-lab`'s extra point drops through `RA001` (`RA.GenericAliasDropped`), a different code
from the `RA007` the brief cites, and that `RA001` disappears from the corpus between baseline
and this run. The `RA002` that disappears alongside it is `solid-js`'s, on `Exports` for
`Store.SetStoreFunction` (`f5ff1a2:tests/Xantham.Generator.Tests/golden/solid-js/symbols.jsonl`
line 347); `setter-lab` carried one `RA001` and three `RA006` at baseline. The predicate's reach
extends one step past the brief's floor.

**Target "`AC001` must fall to 7" — exceeded, not met at the stated number.** It falls to 6.

## `RA007`: 1 before (solid-js), 0 after

`solid-js` carries zero `RA007` findings in this run. `SetStoreFunction` now reaches
`repair-arity` as an interface with typed `Invoke` members, past the point where a two-bound
type-parameter clash would trigger the drop.

## `TR031`: 37 before, 36 after

Per-package after: `animejs` 18 (flat), `callback-overload-lab` 2 (flat), `setter-lab` 2 (down
from 3), `solid-js` 13 (down from 14), `callable-overloads-lab` 1 (new fixture, not in baseline).
Movement stays local to `solid-js`'s `SetStoreFunction` and its `setter-lab` sibling; the
"movement well beyond solid-js" the brief anticipates does not appear in this run.

## `SetStoreFunction`: 9 call signatures, 5 surviving `Invoke` overloads

`dedupe-overloads` raises `DO001` four times against `SetStoreFunction` (0 before, 4 after —
`solid-js` first gains a `DO001` finding in this run). Five `Invoke` members remain in
`SolidJs.fs:2513`; three of the five are parameterised over `'K1`..`'K6` per signature, and the
remaining two (`SolidJs.fs:2520`, `:2522`) carry none. The brief's stated goal is two or three
survivors; five clear the "typed callable, not nine of them" bar without reaching that count.

Corpus `DO001` rises by five, not four: `solid-js` 0 -> 4 on `Store.SetStoreFunction.Invoke`, and
`setter-lab` 2 -> 3 on `DivergentBound.Invoke`, whose two same-arity signatures merge once both
reach `Invoke`. Corpus total 12 -> 17.

## `StoreReturn` — half the second target

`createStore` reads correctly:

```
static member createStore<'T> ([<ParamArray>] __0: obj) : 'T * SetStoreFunction<'T> = jsNative
```

`StoreReturn` does not move:

```
type StoreReturn<'T> = 'T * Action<obj, obj, obj, obj, obj, obj, obj, obj>
```

Both derive from the same source, `StoreReturn<T> = [get: Store<T>, set: SetStoreFunction<T>]`
in `store.d.ts`. `Store.StoreReturn` carries `TR031` ("callback with 9 overloads shaped from the
first") and `TR055`, both tagged `"pass": "shape-aliases"`. The path: `Shape/Aliases.fs:218`
resolves an abbreviation's right side through `typeRefIgnoringSelf`, which reaches the shared
reference code in `Shape/Spec.fs`; the tuple's second element arrives at `objectRef`'s
pure-callback branch (`Spec.fs:1802`) and shapes into a delegate through `delegateRef`
(`Spec.fs:2149`, the raiser of `TR031` outside `shape-callbacks`). That branch sits ahead of
`objectRef`'s named-instantiation lookup (`Spec.fs:1808`). `createStore`'s return reaches a
naming path and keeps `SetStoreFunction<'T>`.

**Target "confirm `StoreReturn` reads the same as `createStore`'s return" — unmet.**
`createStore` moved; the `StoreReturn` alias did not.

**Next.** The candidate fix orders `objectRef`'s named-instantiation lookup ahead of its
pure-callback branch, starting at `Shape/Spec.fs:1802`. It reaches every reference to a pure
callable across the corpus (`TR031` stands at 36), and needs its own lab plus a before/after
`findings` measurement under `.claude/rules/generator-fixtures.md`.

## Corpus movement

`git diff --stat f5ff1a2..HEAD` touches three fixtures: `callable-overloads-lab` (new),
`setter-lab`, `solid-js`. Every fixture outside those three stays byte-identical.

Tier counts per moved package, before -> after:

| package | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| `solid-js` | 53 -> 53 | 163 -> 163 | 91 -> 84 | 43 -> 42 |
| `setter-lab` | 0 -> 0 | 0 -> 0 | 4 -> 5 | 1 -> 0 |
| `callable-overloads-lab` (new) | 4 | 5 | 2 | 0 |

Per-pass totals that moved:

- `solid-js`: `shape-callbacks` 227 -> 156, `shape-interfaces` 811 -> 1012,
  `synthesize-paramobjects` 157 -> 158, `dedupe-overloads` 0 -> 4, `repair-arity` 14 -> 12,
  `drop-orphan-delegates` 2 -> 10, `audit-coverage` 1 -> 0.
- `setter-lab`: `shape-callbacks` 21 -> 16, `shape-interfaces` 14 -> 17,
  `synthesize-paramobjects` 1 -> 2, `dedupe-overloads` 2 -> 3, `repair-arity` 4 -> 3,
  `audit-coverage` 1 -> 0.

Two of those movements sit outside the targets the plan set:

- **`drop-orphan-delegates` 2 -> 10 on `solid-js`.** Eight delegates,
  `Store.SetStoreFunction.Setter` through `Setter8`, become orphans once the call signatures
  reach `Invoke` members, and `DD001` drops all eight. `module SetStoreFunction` leaves
  `SolidJs.fs` with them.
- **`dedupe-overloads` 2 -> 3 on `setter-lab`.** `DivergentBound`'s two same-arity call
  signatures both reach `Invoke`, and the second merges into the first under `DO001`.
