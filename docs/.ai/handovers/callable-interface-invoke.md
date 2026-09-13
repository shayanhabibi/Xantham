# Callable-interface Invoke handover — overloaded-generic callables (4f3d695..6d3824b)

Branch `worktree-gen-callable-invoke`, on `f5ff1a2`. Gated on the composed tree: `dotnet build
Xantham.slnx` 0 errors, `dotnet fsi build.fsx -- test --update --run-gate` — 766 + 90 Expecto,
run gate 453 checks, all green.

## The routing predicate (4f3d695, 58c4938)

`Shape/Interfaces.fs`'s overloaded-type-parameter predicate routes a named callable whose call
signatures share a type-parameter name under two different bounds to `shape-interfaces`. There
it renders as an interface with one `Invoke` member per signature, instead of one delegate head
`aliasTypeParams` cannot write. `callable-overloads-lab` (1212837, bc648dc) pins the tuple-return
case; two run-gate checks confirm the emitted `Invoke` reaches through the imported interface at
runtime.

## `AC001`: 8 before, 6 after

Per-fixture breakdown after: `exclusive-arms-lab` 4, `type-fest` 2. `setter-lab` and `solid-js`
both fall to zero. The plan's "out of scope" section predicts `exclusive-arms-lab`'s four and
`type-fest`'s two `PositiveInfinity`/`NegativeInfinity` pair (`RE001`) as the remainder — six, not
the brief's stated four. `setter-lab`'s extra point drops through `RA001` (`RA.GenericAliasDropped`), a different code
from the `RA007` the brief names; its `RA001` and companion `RA002` both disappear from the
corpus between baseline and this run. The predicate's reach extends one step past the brief's
floor.

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
`SolidJs.fs:2513`, each still parameterised over `'K1`..`'K6` per signature. The brief's stated
goal is two or three survivors; five clear the "typed callable, not nine of them" bar without
reaching that count.

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
in `store.d.ts`. `symbols.jsonl` tags `Store.StoreReturn`'s findings with `"pass": "shape-aliases"` — a pass
separate from `shape-callbacks`/`shape-interfaces`, reachable only through `Shape/Aliases.fs`.
That file shapes a tuple member's inline callable type on its own path, independent of the new
routing predicate.

**Target "confirm `StoreReturn` reads the same as `createStore`'s return" — unmet.**
`createStore` moved; the `StoreReturn` alias did not.

## Fixture diff

`git diff --stat f5ff1a2..HEAD` touches three fixtures: `callable-overloads-lab` (new),
`setter-lab`, `solid-js`. Every fixture outside those three stays byte-identical.
