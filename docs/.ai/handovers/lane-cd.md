# Lane CD - indexed access over an operand whose own keys confine the index

Wave thirteen, branch `worktree-gen-wave13-cd`, forked at `522450b` (batch one composed).

`indexedAccessRef` resolved `T[K]` through one path and widened on every other: where `K` was a
key variable the signature bound as `typekeyof<'T,'R>`, the access read as the `'R` that idiom
introduced, and everything else went to `obj` under `TR020`. `model.TypeVars` holds only the
type parameters a declaration bound, so `EventMap[Type]` resolved inside the generic
`EventTarget<EventMap>` and the *same* access widened in every class that instantiated
`EventMap` with a concrete interface - which is where the corpus writes it.

The access now resolves to the union of the value types the index can select, wherever the
operand's own keys confine the index.

## What was built

- **`Shape/Spec.fs`**, a new section above the type-reference section:

  ```fsharp
  type private KeySet =
      | AllKeysOf of operand: int
      | Named of keys: string list

  val private operandShape : ShapeModel -> int -> int -> TypeFacts option
  val private keySetOf     : ShapeModel -> int -> int -> KeySet option
  val internal indexedAccessValues : ShapeModel -> TypeFacts -> int list option
  ```

  `operandShape` reads a type parameter as its bound, which is the tightest thing true of every
  instantiation. `keySetOf` reads an index type as `keyof T` where the checker deferred it, as a
  literal or a finite union of literals where it did not, and follows a key variable to its
  bound the same way an operand is followed. Both stop after `BoundReach` = 3 links.

  `indexedAccessValues` returns the selected value type ids under two licences, and `None`
  otherwise:

  - `AllKeysOf operand` where `operand` is the access's own object type - `EventMap[keyof
    EventMap]`. Every value the operand carries, members and index signatures alike.
  - `Named keys` where every key is a member the operand declares. A key the operand does not
    declare returns `None` for the whole access rather than for that arm.

- **`indexedAccessRef`** takes `ctx` and `self` (it recurses now) and hands the value ids to
  `erasedUnionRef`, which is what decides the arity, deduplicates the arms after mapping, and
  raises `TR036` where the result is wider than `ErasedUnionArity`. A resolved access carries no
  finding of its own.

- **`tests/fixtures/indexed-access-lab/`**, hand-authored and tracked, registered in
  `Pipeline.test.fs` with three cases. It pins `Concrete[Type]` under `Type extends keyof
  Concrete` in parameter, callback-parameter and return position; `EventMap[keyof EventMap]`
  over a parameter bounded by `Record<string, WorkerEvent>`; a twelve-arm value union, so the
  `TR036` hand-off is pinned rather than discovered; and the two shapes that stay widened.

- **One existing assertion updated.** `keyof-overload-lab`'s "a key-set bound reaches the
  signature erased" case asserted `abstract find<'K>: selector: 'K -> obj`. `HtmlTags[K]` now
  reads `U2<Div, Span>`. The test's subject - that the bound `K` was taken over is nowhere in
  the F# signature, which is what makes the two overloads collide under `DO005` - is unchanged,
  and `DO005` still fires on the same owner.

## Findings

No new case, no change to `Findings.fs` or `FindingCodes.table`.

## Measurements

`dotnet fsi build.fsx -- findings`, before and after.

| | before (`522450b`) | after | corpus only |
| --- | ---: | ---: | ---: |
| generator tests | 472 | 477 | |
| wire tests | 90 | 90 | |
| run-gate checks | 300 | 300 | |
| exact | 530 | 530 | 530 |
| ergonomic | 1633 | 1658 | 1633 |
| widened | 776 | 779 | 776 |
| escape | 195 | 195 | 195 |
| total findings | 16,870 | 16,906 | 16,863 |

"corpus only" subtracts `indexed-access-lab`'s own 43 findings and its tier counts (0 exact / 25
ergonomic / 3 widened / 0 escape). **No existing fixture's tier counts moved at all**: every
recovered owner already carried some other widening, so the per-symbol grade did not change.

### Per-key delta, every code that moved

| code | before | after | of which lab | corpus delta |
| --- | ---: | ---: | ---: | ---: |
| `TR020` IndexedAccessNoForm | 69 | 53 | 3 | **-19** |
| `TR036` UnionTooWide | 9 | 10 | 1 | **0** |
| `TR009` UnknownToObj | 182 | 194 | - | +12 |
| `TR026` ConstrainedArgumentWidened | 8 | 0 | - | -8 |
| `TR044` ArgumentNotASubtypeOfConstraint | 8 | 16 | - | +8 |
| `TR045` ConditionalTypeDeferred | 186 | 185 | - | -1 |
| `TR046` ConditionalResolvedToBranch | 64 | 65 | - | +1 |
| `SP001` | 1704 | 1728 | 24 | 0 |
| `SP002` | 652 | 655 | 3 | 0 |
| `SY004` | 781 | 783 | 2 | 0 |
| `TP002` | 310 | 319 | 9 | 0 |
| `TR055` | 321 | 322 | 1 | 0 |

`TR018` 59, `TR023` 136, `TR037` 15, `DO001` 5, `RT001` 3, `DT002` 17, `DT003` 1, `DT004` 1: all
unchanged. Net corpus movement is **-7 findings**.

By fixture, the only goldens that moved are `@cloudflare/workers-types` (`TR020` 22 → 5, `TR009`
159 → 171, `TR026` 8 → 0, `TR044` 2 → 10), `keyof-overload-lab` (`TR020` 2 → 0) and `type-fest`
(`TR045` 155 → 154, `TR046` 31 → 32). `animejs` 13, `solid-js` 22, `type-fest` 8,
`array-shape-lab` 1 and `keyof-lab` 1 `TR020` are unchanged.

### Real versus re-keyed

**Nineteen `TR020` recoveries, none of them re-keyed to `TR036`.** The arity hand-off the
dispatch priced did not fire anywhere in the corpus: no resolved union reached ten arms. The
only site in the tree over the cap is the lab's deliberate one.

Arity of every union the resolution produced:

| arms | sites | where |
| ---: | ---: | --- |
| 1 | 3 | `EventTarget.dispatchEvent(event)` → `Event`; `type-fest.ArrayIndices`, inside a conditional's check position and so not spelled in the golden; lab `Target.dispatchEvent(event)` → `WorkerEvent` |
| 2 | 2 | `keyof-overload-lab.Finder.find()` → `U2<Div, Span>` |
| 3 | 5 | lab, `WorkerEventMap` |
| 4 | 16 | `@cloudflare`, the four event maps (five value ids deduplicating to four for the `WorkerGlobalScope` family) |
| 12 | 2 | lab `onWide`, over the cap → `TR036` |

Two secondary movements land on owners the recovery reached, both consequences of the access
now carrying a real type rather than `obj`. Neither is a re-key of the access loss itself:

- **`TR026` → `TR044`, 8 owners.** `{global, WorkerGlobalScope, ServiceWorkerGlobalScope,
  WebSocket} × {add,remove}EventListener(handler)`. The handler is
  `EventListenerOrEventListenerObject<EventMap[Type]>`; the argument used to be `obj`, which
  reported as a constrained argument widened. It is now `U4<FetchEvent, QueueEvent<obj>,
  PromiseRejectionEvent, ScheduledEvent>`, which is a real type and is not an F# subtype of the
  `Event` bound, so the same loss reports under the argument-subtype key instead. The type
  written at the position is strictly tighter than before; the finding count is flat.
- **`TR009` +12.** `{global, WorkerGlobalScope, ServiceWorkerGlobalScope} × {add,remove}
  EventListener × {(handler), (handler)(event)}`. `WorkerGlobalScopeEventMap.queue` is
  `QueueEvent<unknown>`, and the `unknown` inside it was invisible while the whole access was
  `obj`. This is a loss that was always there being named for the first time, not a new one.

`type-fest.ArrayIndices` is a third-order gain: a conditional that was deferred (`TR045`,
widened) now resolves to its sole inhabited branch (`TR046`, ergonomic), because the indexed
access in its check position resolved.

### `git diff --stat` over the goldens

```
 .../workers-types/Cloudflare.WorkersTypes.fs       | 24 +++++++++---------
 .../golden/@cloudflare/workers-types/manifest.json |  4 ++--
 .../golden/@cloudflare/workers-types/symbols.jsonl | 12 +++++------
 .../golden/keyof-overload-lab/KeyofOverloadLab.fs  |  2 +-
 .../golden/keyof-overload-lab/manifest.json        |  5 ++---
 .../golden/keyof-overload-lab/symbols.jsonl        |  2 +-
 .../golden/type-fest/manifest.json                 |  4 ++--
 .../golden/type-fest/symbols.jsonl                 |  2 +-
 8 files changed, 27 insertions(+), 28 deletions(-)
```

Plus the new `golden/indexed-access-lab/` - 243 lines of binding, 40 of manifest, 28 of
symbols.

`dotnet build Xantham.slnx` succeeds; `dotnet fsi build.fsx -- test` exits 0 with the run gate.

## The 50 corpus sites still widened

Classified from an instrumented run over the three npm rungs that carry them, printing the
object and index type flags at every decline. Each cluster is a different reason.

### solid-js, 22 - the index is a conditional type

19 of the 22 are one alias and its propagation:

```ts
export type NoInfer<T extends any> = [T][T extends any ? 0 : never];
```

The operand is the tuple `[T]` (which reaches the shape as an object carrying `Array`'s 42
members) and the index is the conditional `T extends any ? 0 : never`. A conditional index
enumerates no key set, so the access declines. This is `solid-js/types/reactive/signal.d.ts:143`
and it reaches `on`, `createMemo`, `createEffect`, `createComputed`, `createRenderEffect`,
`For.Props.Children` and `Index.Props.Children` through their parameter types. The remaining
three index a bounded type parameter by `number` (2) and by `any` (1).

**Not recoverable by this lane's technique**, and probably not worth recovering: the idiom
exists to defeat inference, and its F# reading is the operand's element type. That would be a
`NoInfer`-shaped special case in `Model.LibBindings` territory, not a key-set resolution.

### animejs, 13 - the operand carries no members

```ts
// The operand is declared in lib.dom.d.ts; the run carries no members for it.
export declare function query<K extends keyof HTMLElementTagNameMap>(selector: K): HTMLElementTagNameMap[K] | null;
```

Every one of the 13 is `Element.querySelector` / `querySelectorAll` / `closest` and
`addEventListener` / `removeEventListener` reached through `DrawableSVGGeometry`. The index is
exactly the shape this lane resolves - a type parameter whose bound is the expanded key union of
the operand. **The operand is in the type table as an object type with zero members.** The
resolve tier does not follow the members of a lib-declared interface, so there is nothing to
select from and the access declines correctly on the data it has.

This is the one cluster where the shape is recoverable and the blocker is elsewhere. Recovering
it means either following lib interface members in the resolve tier, or reading the tag-name
maps out of `BrowserBindingTable.generated.fs`, which already carries the element names the
values would resolve to. **Left for whoever owns the resolve tier's follow policy** - it is not
a `Shape/` change and it would move a lot more than 13 findings.

### @cloudflare/workers-types, 5 - nested access over an opaque result

```ts
export declare function runModel<Name extends keyof ModelMap>(name: Name, input: ModelMap[Name]["inputs"]): ModelMap[Name]["outputs"];
```

`Ai.run()` 1, `Ai.run(inputs)` 3, `Ai.Run.Inputs.requests` 1 - `AiModelList[Name]["inputs"]` at
`index.d.ts:11851`. The outer operand is itself an indexed access and declares no keys, so
`"inputs"` selects nothing. The recon already graded these a decline rather than a defect, and
this lane agrees: nothing in F# names a literal index into an opaque result. Pinned in the lab
as `runModel`.

### type-fest 8, array-shape-lab 1, keyof-lab 1

Not probed. `keyof-lab`'s is `values<T>(source: T): T[keyof T][]`, where `T` is unconstrained
and so stands for nothing - the correct decline.

## Worklist

- **animejs's 13, above.** The largest recoverable cluster left under `TR020`, and it is a
  resolve-tier question rather than a shape one.
- **The 8 `TR044` sites.** `U4<FetchEvent, …>` is not an F# subtype of `Event`, so the argument
  to `EventListenerObject<'T>` still reports a loss. Every arm *is* an `Event` subtype
  individually; if the erased union carried the constraint, these would be exact. That is a
  `Fable.Core` `U`-type question, not a mapping one.
- **`EventMap[Type]` where the operand is generic and the index is a bound key variable** still
  reads as `typekeyof<'EventMap,'R>` and is untouched. The two regimes do not overlap: the
  `typekeyof` arm is tested first, and the corpus proves it, since `EventTarget<'EventMap>`'s
  own `addEventListener` still renders `'R` while every instantiation of it now renders the
  value union.
