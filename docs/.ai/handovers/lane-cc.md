---
category: Generator
audience: managing agent
title: Handover - lane CC, wave thirteen (remeasure TR018)
---

# Lane CC — `TR018` remeasured, 82 sites re-attributed

Read-only. Base `a008490` (wave thirteen integration fork), branch `worktree-gen-wave13-cc`.
Nothing under `src/` or `tests/` touched; `git status` carries this file alone. Reproducers were
written outside the repository (`%TEMP%/xantham-cc/`) and run through
`dotnet run --project src/Xantham.Cli -- generate <dir> -o <dir>/out` with `XANTHAM_TSGO_EXE`
pointed at the main checkout's pinned compiler.

## Verdict

- **`animejs`'s `*.then(callback)(self)` cluster is nine sites, not eight, and it is a mechanism
  the recon never named: TypeScript's uninhabited-intersection reduction.** Recoverable. The
  current message is false at all nine. One predicate, one finding case, one lab.
- **`@cloudflare/workers-types`'s four `Workflow*.Config.retries` owners are a deferred
  conditional operand over the declaration's own type parameter.** Floor — same refusal as cause
  B, and already pinned by the tracked `hoist-conditional-lab` fixture, which the recon predates.
- **The other 69 sites are cause A residue (28) and cause B family (41).** Neither is new work
  this lane can open: cause A is the recon's own Lane A, priced separately and partly answered by
  lane CA's dedup; cause B is contract.

**Open an implementation lane for the nine, in batch two, sequenced after CA and CB.** It needs
one pre-declared finding case (§4). Do not open one against the other 73.

## 1. Corrected site table, superseding the recon's

`TR018` is 82 corpus-wide, counted over `golden/**/symbols.jsonl`. The recon's 194 and its
per-fixture split are both dead: causes C and D landed between the recon and now, as `TR050`
(`TR.IntersectionCallableFlattened`, 44 corpus sites) and `TR049`
(`TR.EmptyIntersectionOperandReduced`, 61 corpus sites).

| fixture | sites | A | B1 | B2 | E |
| --- | ---: | ---: | ---: | ---: | ---: |
| `animejs` | 33 | 24 | | | 9 |
| `@cloudflare/workers-types` | 21 | | 7 | 14 | |
| `solid-js` | 14 | 4 | 10 | | |
| `type-fest` | 8 | | 8 | | |
| `hoist-conditional-lab` | 3 | | | 3 | |
| `brand-lab` | 1 | | 1 | | |
| `intersection-empty-lab` | 1 | | 1 | | |
| `intersection-lab` | 1 | | 1 | | |
| **total** | **82** | **28** | **28** | **17** | **9** |

Causes, with A and B carried over from the recon and B split by operand kind:

| # | cause | sites | verdict |
| --- | --- | ---: | --- |
| A | shared property flattened to a cross-product union of intersections | 28 | recon's Lane A, priced there; partly a duplicate-reporting artefact (§5) |
| B1 | bare type-parameter operand | 28 | floor |
| B2 | deferred conditional or mapped-type operand over a type parameter | 17 | floor |
| E | uninhabited intersection (unit-typed property collision) | 9 | **recoverable** |

Owners per cause are listed in §6.

Two further corrections to the recon's §9: `TR019` (`TR.IntersectionNotDeclared`) now fires at
**3** sites, not zero — `LoopbackDurableObjectNamespace.get()`, `.getByName()`, and
`hoist-conditional-lab`'s `condSeed` — so the guard's other arm is reachable. And every one of
the 82 carries the identical rendered message, so a site count is not a loss count anywhere in
this key.

## 2. Cluster E — uninhabited intersection (9 sites, animejs). Recoverable.

### Mechanism

`animejs` declares, on four classes, the standard "make this not thenable" idiom:

```ts
then(callback?: Callback<this & { then: null }>): Promise<any>;
```

`then` is declared by both operands. The flattened property is
`((cb?) => Promise<any>) & null`, and `null` is a unit type, so TypeScript marks the whole
intersection **uninhabited** and resolves it to `never`. Verified against the pinned compiler
directly:

```ts
interface A { then(): void }
type B = A & { then: null };
declare const b: B;
const x: number = b;          // no error: B is never
type C = A & { then: string };
declare const c: C;
const y: number = c;          // TS2322: C is not assignable to number
```

The type still arrives carrying `TypeFlags.Intersection`, with an empty member list and empty
index infos, so `intersectionRef` falls through to the fallback and reports
`IntersectionOverNonObject`. **The message is false**: both operands are object types and both
carry members. This is the same class of mis-keying cause C was.

### Reproducer (3 lines)

```ts
export declare class Timer {
    then(callback?: (self: this & { then: null }) => any): Promise<any>;
}
```

→ **1 `TR018`** on `Timer.then(callback)(self)`, renders `self: obj`.

The alias-position form is the same mechanism and costs the same three lines:

```ts
export interface A { then(): void }
export type B = A & { then: null };
export declare const b: B;
```

→ **2 `TR018`**, on `B` and on `b`; `type B = obj`.

### Negatives

Three, each differing in one operand, all measured:

1. `this & { paused: boolean }` on the same class — no name collision — → **0 `TR018`**; the
   intersection flattens and `SI003` synthesizes the declaration.
2. `play(callback?: (self: this & { then: null }) => any)` — the collided name is not a member of
   the class — → **0 `TR018`**.
3. `this & { then: string }` — the collided property is not unit-typed, so TypeScript does not
   empty the intersection — → **1 `TR018`**, but on `Timer.Then.Callback.Self.then`, the member,
   not the whole type. That is cause A, and it is the boundary: only a unit-typed collision moves
   the loss from the property up to the whole intersection.

Replacing `this` with the class name (`Timer & { then: null }`) changes nothing, and removing the
`Callback<T>` indirection changes nothing. The `this` type and the indexed-access alias are both
incidental.

### Why it is recoverable, and that the operands are reachable

The correct F# answer at all nine sites is the class: `self: Timer`, `self: Timeline`,
`self: JSAnimation`, `self: WAAPIAnimation`, `self: CallbackArgument`. The dropped operand
contributes one property typed `null`, whose only purpose is to break TypeScript's `await`
protocol — a protocol Fable's F# surface does not participate in. Today all nine render `obj`.

The operands are already in the type table for an uninhabited intersection, so no resolve-tier
change is needed. Measured:

```ts
export type Good = string & { __brand: "x" };
export type Bad = string & { __brand: "x" } & { __brand: "y" };
export declare const good: Good;
export declare const bad: Bad;
```

`Bad` is `never` by the same reduction (`"x" & "y"` is empty), yet it renders `string<Bad>` with
`SA001` and no `TR018` — `brandedPrimitive` read its `IntersectionMembers` and its constituents'
facts. `operandsOf` will answer for the cluster-E shapes too.

### The predicate a lane would write

`intersectionRef`'s fallback already carries the family: `reducedOperand` drops operands that
constrain nothing and maps to the one that remains. Cluster E is the same shape one step out —
drop the operands that *empty* the intersection rather than the ones that are empty. Concretely,
where every operand carries `TypeFlags.Object`, at least one carries members, and the intersection
carries none, partition the operands by whether every property they declare is unit-typed; where
exactly one non-unit operand remains, map to it.

Stated over unit-typed properties rather than over `null` specifically, because the reduction TS
performs is over unit types generally — the `Bad` probe above is the literal-typed case of the
same rule, and it must keep reaching `brandedPrimitive` first.

## 3. Cluster B2 — deferred conditional operand (17 sites). Floor.

### Mechanism

`@cloudflare/workers-types` declares, at `experimental/index.d.ts:16001`:

```ts
export type WorkflowStepContext<Delay = WorkflowDelayDuration | number> = {
  config: {
    retries?: { limit: number; backoff?: WorkflowBackoff }
      & (Delay extends WorkflowDelayFunction ? {} : { delay: WorkflowDelayDuration | number });
    ...
  };
};
```

The second operand is a conditional deferred on the alias's own parameter. Its members are
answered again at each instantiation, so the checker computes none for the intersection, and the
fallback reports `TR018` accurately.

### Reproducer (4 lines)

```ts
export type Step<Delay = number> = {
    config: { retries?: { limit: number } & (Delay extends Function ? {} : { delay: number }) };
};
export declare const step: Step;
```

→ **1 `TR018`** on `Step.Config.retries`, the fixture's owner shape exactly.

### Negatives

- The same declaration with the conditional written over a concrete type
  (`number extends Function ? {} : { delay: number }`) resolves eagerly → **0 `TR018`**. The
  deferral is the trigger, not the conditional form.
- The same declaration with the conditional's true branch non-empty
  (`Delay extends Function ? { d: string } : { delay: number }`) → **1 `TR018`**. It is not a
  cause-D `X & {}` shape, and `reducedOperand` correctly declines it.
- `{} & (Delay extends Function ? {} : { delay: number })` → **0 `TR018`**, `TR049` plus `TR045`
  (`TR.ConditionalTypeDeferred`). This is the probe that shows the deferred operand *is* in the
  type table; the recovery machinery can reach it, and declines for a reason rather than for want
  of facts.

### Why it is floor

`retries` is a property a consumer constructs. Emitting the readable operand's members alone —
`limit` and `backoff` without `delay` — would let a consumer omit a property that is required at
the instantiation the conditional resolves to. The under-approximation is sound in return
position and unsound in argument position, and a member of a param-object bag is both. The same
argument retires the equivalent partial flatten for B1.

### Already pinned

`tests/fixtures/hoist-conditional-lab` is this exact construct, reduced from `three`'s
`Node<TNodeType>`, with a control that differs in the deferred operand alone. It is tracked, it
carries 3 of the 82, and it postdates the recon. **A lane acting here needs no new lab.**

The four `Workflow*` owners are four hoisted copies of one source declaration —
`WorkflowStepContext.Config` reached bare, through `ctx`, through `ctx.ctx`, and through
`rollback.ctx.ctx`. One loss, reported four times.

## 4. Finding case a cluster-E lane would need

Naming it, not adding it. The manager pre-declares it at dispatch.

| Case | Tier | Raised where |
| --- | --- | --- |
| `TR.UninhabitedIntersectionReduced of property: string` | ergonomic | `intersectionRef`, the new branch, carrying the property whose collision emptied the intersection |

Ergonomic rather than exact: the mapping keeps the operand a consumer can use and drops one the
consumer could never have supplied. It sits beside `TR049` (`EmptyIntersectionOperandReduced`,
exact) and `TR050` (`IntersectionCallableFlattened`, ergonomic), which are the two branches this
one joins.

Where the lane chooses to relabel without recovering, the case is
`TR.IntersectionUninhabited` at widened tier instead. Recommend the recovery: nine sites all sit
on the `self` parameter of `animejs`'s completion callbacks, which is the package's most-used
surface, and `obj` there costs a cast at every call.

## 5. Cause A residue (28 sites) — attribution confirmed, not re-derived

`animejs`'s 24 are `LayoutSpecificAnimationParams.delay?: number | FunctionValue` intersected with
`TimerOptions.delay?: TweenParamValue`, where
`TweenParamValue = number | string | FunctionValue | EasingParam | TweakRegister`. **Non-identical
unions**, so the flattened property becomes a cross-product union whose primitive-and-object arms
each report. Confirmed by measurement rather than by reading:

```ts
export type Fn = (i?: number) => number;
export type A = { delay?: number | Fn };
export type B = { delay?: number | string };
export type P = A & B;
export declare const p: P;
```

→ **2 `TR018`** on `P.delay`, plus `TR035`. The identical-union control — `B` declaring
`number | Fn` as well — gives **0 `TR018`**, so the recon's identical-operand slice has landed and
lane R3 is right about it.

Two consequences the next lane should hold:

- The 24 are **4 losses**: `LayoutAnimationParams.delay`, `.duration`, `AutoLayoutParams.delay`,
  `.duration`, each reported six times, once per cross-product arm, all six with the same rendered
  message. They accumulate in `erasedUnionRef`, which is the function lane CA deduplicates by
  *(owner, rendered message)*. **Lane CA should expect `TR018` to fall by roughly 20 on `animejs`
  as a side effect of its `TR037` work**, and should report that rather than treat it as an
  unexplained diff.
- `solid-js`'s 4 are `ResourceOptions<T | I, S> & { initialValue: I }`, where the shared property's
  operands are type parameters. Cause A in shape, cause B in what stops it. They stay after CA.

## 6. Owners, by cause

**E — uninhabited intersection (9).** `animejs`: `Timer.then(callback)(self)`,
`Timeline.then(...)`, `JSAnimation.then(...)`, `WAAPIAnimation.then(...)`,
`CallbackArgument.then(...)`, `CallbackArgument.Head.then(...)`,
`CallbackArgument.Head.Parent.then(...)`, `CallbackArgument.Head.Prev.then(...)`,
`CallbackArgument.Head2.then(...)`. All nine resolve to one of the four
`then(callback?: Callback<this & { then: null }>)` declarations in `animejs`'s `dist/modules`.

**B2 — deferred conditional or mapped operand (17).** `@cloudflare/workers-types`: `Fetcher`,
`DurableObjectStub`, `LoopbackServiceStub`, `LoopbackDurableObjectClass`, `RpcStub`,
`DurableObjectNamespace.get()`, `.getByName()`, `DurableObjectFacets.get()`,
`WorkerStub.getEntrypoint()`, `RpcStubConstructor.Create()`, `WorkflowStepContext.Config.retries`,
`WorkflowRollbackContext.Ctx.Config.retries`, `WorkflowRollbackHandler.Ctx.Ctx.Config.retries`,
`WorkflowStepRollbackOptions.Rollback.Ctx.Ctx.Config.retries`. `hoist-conditional-lab`:
`CondExtensions.toVar()`, `CondNode`, `CondVarNode`.

**B1 — bare type-parameter operand (28).** `@cloudflare/workers-types`: `XOR` ×2,
`EventContext.env`, `PagesFunction.Context.env`, `EventPluginContext.env`,
`PagesPluginFunction.Context.env`, `Ai.run(inputs)`. `solid-js`: `ParentProps`,
`ParentComponent(props)`, `VoidProps`, `VoidComponent(props)`, `FlowProps`,
`FlowComponent(props)`, `PropsWithChildren`, `lazy()`, `MatchProps.children(item)`,
`MatchProps.children(item)()` — the last two through `NonNullable<T> = T & {}`, where
`reducedOperand` declines by design because the operand stands over a type parameter.
`type-fest`: `Opaque`, `Tagged`, `InvariantOf`, `Except`, `SetOptional`, `SetRequired`,
`SetReadonly`, `FixedLengthArray`. Labs: `brand-lab`'s `Counted`, `intersection-empty-lab`'s
`Counted`, `intersection-lab`'s `merge(base)`.

**A — cross-product union of intersections (28).** `animejs`:
`LayoutAnimationParams.delay` ×6, `.duration` ×6, `AutoLayoutParams.delay` ×6, `.duration` ×6.
`solid-js`: `CreateResource.Options.initialValue` ×2, `CreateResource.Options3.initialValue` ×2.

## 7. What to do with `docs/.ai/plans/generator-tr018-recon.md`

Mark it **superseded in part**, by section, rather than retiring it — its cause A and cause B
mechanisms are still the best statement of what those constructs do.

| section | state |
| --- | --- |
| §1 provenance, §2 population | superseded — 194 → 82, and the per-fixture split with it |
| §3 the guard and its four causes | superseded — the counts are dead and the taxonomy is incomplete; causes E and B2 are the residue it never saw |
| §4 cause A | mechanism stands; counts superseded; its identical-operand slice has landed |
| §5 cause B | mechanism stands and is still the floor; broaden "type-parameter operand" to cover a deferred conditional or mapped type over one |
| §6 cause C | **landed**, as `TR050`; its C1 reproducer now yields 0 `TR018` (measured) |
| §7 cause D | **landed**, as `TR049` |
| §8 recommendation, shape-of-win table | superseded — Lanes C and D are done, and the table's floor of 39 is now 73 out of 82 |
| §9 first bullet, "`TR019` fires nowhere" | superseded — 3 sites |

The floor line the recon closes on survives with a different number: **73 of the 82 are contract,
not backlog**, once cause A is priced on its own as that document already asks.

## 8. What this lane did not settle

- Whether lane CA's dedup collapses `animejs`'s 24 cause-A sites to 4 is a prediction from the
  message-identity measurement, not something this lane ran. CA holds the measurement.
- The four `Workflow*.Config.retries` owners are four hoistings of one declaration. Why the
  anonymous hoister mints a fresh `Config` per access path rather than reusing
  `WorkflowStepContext`'s is a hoisting question, not a `TR018` one, and is not attributed here.
