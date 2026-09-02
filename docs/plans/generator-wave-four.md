---
category: Generator
audience: managing agent
title: Plan - generator wave four
integration-branch: worktree-generator-wave-four
---

# Generator wave four — dispatch plan

Wave three closed at `96bf9d9` (`docs/plans/generator-architecture.md`, "Wave three, closed").
The corpus stands at **28 fixtures and 14,803 findings**. This wave is scoped by two things wave
three did not have: the user's dispositions on `docs/fable5-workarounds.md`, and the decision that
**`@cloudflare/workers-types` is the litmus test and `@types/three` is not**.

Read `.claude/rules/generator-fixtures.md` first — the "For managing agent" section is the
protocol this plan assumes.

## What the user settled

`docs/fable5-workarounds-user-response.md` answers all six documented Fable-5 losses. Four are
closed as unresolvable and **carry no work in this wave or any later one**:

| § | loss | disposition |
|---|---|---|
| 1 | `null`, `undefined` and an absent property are one F# value | consumer's to handle on the JS side. Keep the finding; assign no work. |
| 2 | `=` on a generated interface is a deep JavaScript comparison | same. |
| 4 | an erased union arm that is an interface is never selected | known limitation of erased unions. Helpers are a later question. |
| 5 | a downcast to a generated interface is `false` | known limitation. Consumers `:?>` after establishing the type themselves. |

Two carry work, and both are lanes below:

- **§3** — an interface with a function-typed abstract member gets no `[<ParamObject>]` `Create`.
  *"Create the param objects helper anyway."* The missing `this` inside the function is a later
  question; an object expression compiling to a class works well enough. → **lane O**.
- **§6** — a settable static and a mutable global bind read-only. *"The one issue that we can
  actually easily do something about, and SHOULD do something about."* The user supplied the
  emission to target. → **lane N**.

## What the litmus change costs the old priority list

The architecture doc's wave-four list ranked `alignOperands` second, on the grounds that it
decides the `@types/three` rung. It does not decide anything else: `SY002` stands at **45 on
`three` and 1 on `@cloudflare/workers-types`**. With `three` deprioritised the lane loses its
justification, and it is **not dispatched**. `three` remains unenrolled, is not re-measured this
wave, and no lane may cite the rung as a reason.

`@cloudflare/workers-types` at HEAD: **exact 252, ergonomic 1,031, widened 374, escape 115**.
Its widened tier has never been attributed. That is lane Q.

## Before dispatch — done, in one commit

**Finding cases pre-declared.** Codes are positional, so this is the one append point no pattern
removes. Seven cases are appended and `Findings.test.fs` is updated in the same commit. Each lane
**raises the cases named below and edits `Findings.fs` no further**; a case that turns out unused
costs one dead union case, which is cheaper than a mid-wave renumbering.

| key | case | tier | lane |
|---|---|---|---|
| `TR049` | `TypeReference.EmptyIntersectionOperandReduced` | exact | P |
| `TR050` | `TypeReference.IntersectionCallableFlattened of signatures: int` | ergonomic | P |
| `TR051` | `TypeReference.IntersectionOperandsIdentical` | exact | P |
| `SC006` | `ShapeClasses.StaticSettable` | exact | N |
| `SE003` | `ShapeExports.MutableValueReadOnly` | widened | N |
| `SP002` | `SynthesizeParamObjects.MethodMemberAsCreateParameter` | ergonomic | O |
| `SP003` | `SynthesizeParamObjects.CreateNotSynthesized of reason: string` | widened | O |

A tier is not positional. A lane that proves its pre-declared tier wrong — the run gate says the
setter does not round-trip, say — changes the attribute and the `Findings.test.fs` line in place
and reports it. It does not move, rename or delete the case.

## Batch 1 — four lanes, disjoint files

```
[ N | O | P | Q ]  →  merge  →  batch 2
```

Lane Q writes no source. Lane N owns `Shape/Classes.fs` and `Shape/Exports.fs`, lane O owns
`Shape/ParamObjects.fs`, lane P owns `Shape/Spec.fs`. `Spec.fs` is shared by every pass, so lane
P is the only lane in it this batch. Goldens are regenerated over the composed tree, never merged
by hand.

### Lane N — a settable static and a mutable global bind settable

`docs/fable5-workarounds.md` §6, and the user's §6 answer, which supplies the target emission:

```fsharp
[<Import("Budget", "fable-workaround-lab")>]
type Budget(spent: float) =
    member val spent: float = spent
    static member val limit: float = JS.undefined with get, set

[<Global("globalThis")>]
type Globals =
    static member val counter: float = JS.undefined with get, set
```

so that `Budget.limit <- 5.` and `Globals.counter <- 5.` compile to `Budget.limit = 5.0` and
`globalThis.counter = 5.0`.

- Owns `Shape/Classes.fs` and `Shape/Exports.fs`. Labs: `statics-lab` (`Counter.tick`),
  `globals-lab` (`declare var counter`), `fable-workaround-lab` (`Budget.limit`) — all three
  already carry the construct, so **write no new lab**; extend the run gate instead.
- Findings: raises `SC006`, `SE003`. `SC003` should fall to the statics this cannot reach; if it
  falls to zero, say so rather than retiring the case.
- The run gate is the deliverable: an assignment followed by a read-back, against `index.js`.
  A claim that the setter works, unproven by the run gate, is not accepted.
- Also updates `docs/fable5-workarounds.md` §6 to what now holds.
- Done: `SC003` before/after, `SE003` count, run-gate checks added, `findings` diff.

### Lane O — `Create` for an interface that carries a method

`docs/fable5-workarounds.md` §3. `Shape/ParamObjects.fs` disqualifies an interface the moment any
member is an `FsMethod`, which is **1,245 abstract members across the 1,397 interface types in the
goldens**. The user's answer is to synthesize the `Create` anyway, carrying the method in as a
function-typed parameter.

- Owns `Shape/ParamObjects.fs`. Lab: **`paramobject-method-lab`** (new; register it in
  `Pipeline.test.fs`). `fable-workaround-lab`'s `Listener { name; notify(count): string }` is the
  minimal in-tree case and the run gate already reaches it.
- Findings: raises `SP002` on each method carried in, `SP003` with a reason where an interface
  still gets none. `FsIndexer` and `FsConstructor` keep their existing exclusions and
  `CreateParameterBudget` (24) still applies — both are reasons `SP003` should now state.
- The delegate receives no `this`. That is a documented later question, not this lane's; say so
  in the finding message (pre-declared) and in the lab, and do not attempt a fix.
- Done: `SP001` before/after, `SP003` reasons ranked by count, compile gate green, run gate
  exercising a `Create` that carries a method.

### Lane P — `TR018` causes D, C and the identical-operand slice of A

`docs/plans/generator-tr018-recon.md` §8, which prices all three and supplies a reproducer each.
`TR018` stands at 194. Do them in the recon's order and stop before the rest of cause A.

1. **Cause D** (28 sites) — reduce `X & {}` to `X`. Cheapest. `brand-lab`'s
   `Counted = string & { count: number }` must keep its brand; the object operand there is not
   empty, and the lab already pins it. → `TR049`.
2. **Cause C** (30 sites) — read `CallSignatures` alongside `Members` and `IndexInfos`, so an
   intersection of callables at a member position reaches the overload machinery it already
   reaches at an export position. `intersection-lab`'s `Cancelable` must not move. → `TR050`.
3. **The identical-operand slice of cause A** (24 sites, all `workers-types`) — where every
   operand of the flattened property is the same type, that type is the answer. One equality
   test, no distribution. → `TR051`.

- Owns `Shape/Spec.fs`. Labs: **`intersection-empty-lab`** (D), **`intersection-callable-lab`**
  (C). The A slice is pinned by `workers-types` and needs no lab of its own.
- **Cause B (39 sites) is not work.** A type-parameter operand has no members until it is
  instantiated and the mapping declines to invent them. Leave the behaviour; `intersection-lab`
  pins it as intended.
- **The rest of cause A is out of scope.** Distribution over union operands is a type-level
  computation on the checker's behalf, and `TR035` is downstream of it. It is priced separately.
- Report `TR035` and `TR036` alongside `TR018`: a change to intersection flattening moves union
  arms, and the recon's §9 names that as the untested interaction.
- Done: `TR018` at or near 112, `TR019` (which fires at 2 sites) re-read, `TR035`/`TR036` before
  and after, goldens reviewed in aggregate.

### Lane Q — reconnaissance: the `@cloudflare/workers-types` widened and escape tiers

Read-only. No source change, no golden change, no finding raised. The output is
**`docs/plans/generator-cloudflare-recon.md`**, in the shape of
`docs/plans/generator-tr023-recon.md` and `generator-tr018-recon.md` — both of which priced a
wave correctly and are the format to copy.

Cloudflare is now the litmus, and its widened tier is unattributed. Cover, in size order, every
widened key at 15 sites or more that no existing recon covers:

| key | sites | case |
|---|---:|---|
| `TR037` | 56 | `TemplateLiteralToString` |
| `TR023` | 43 | `NotAmongGeneratedDeclarations` — cause 2 only; causes 1A/1B closed in wave three |
| `TR040` | 40 | `ObjectTypeToObj` |
| `TR035` | 23 | `UnionWithObjArm` |
| `TR020` | 22 | `IndexedAccessNoForm` |
| `TR047` | 22 | `ObjectWithoutMembers` |
| `DO001` | 19 | `OverloadDropped` |
| `TR036` | 17 | `UnionTooWide` |

Plus the **115 escape-tier symbols**, which nothing has ever attributed: what drives them, and in
what proportion (`TR003`, `TR008`, `HG001`, `SE001`, `AC001` are the candidate keys).

For each: a named cause, a reproducer of six declarative lines or fewer quoted in full, a count,
and a verdict of **defect** or **the mapping declining to lie**. Close with a cost-ordered
recommendation and a "shape of the win" table, as both prior recons do. Residual must be zero —
every site attributed, or the unattributed remainder stated with its count.

- Do **not** open the cloudflare golden or load its manifest's `symbols` array. Work from
  `dotnet fsi build.fsx -- findings --key <key>`, targeted `grep` into the `.d.ts`, and scratch
  labs run outside the repository, exactly as the `TR018` recon did.
- `TR009` (156, `UnknownToObj`) and `TR006` (297, `StringLiteralToString`) are settled decisions
  and are **out of scope**. So is anything under §§1, 2, 4, 5 of the workarounds document.

## Batch 2 — priced by batch 1

Not dispatched yet. The candidates, in the order they currently stand:

1. **`FollowDepth`.** `RT001` reports `@cloudflare/workers-types` truncating 1,772 types at depth
   12, against 261 before wave three. Nothing is wrong today and the headroom is thin. Raising the
   cutoff is a corpus-wide behaviour change and needs a lane that owns `Resolve.fs` alone and
   measures it. Wave three handed this up rather than acting on it.
2. **Whatever lane Q prices.** This is the point of running it in batch 1.
3. **`TR018` cause A proper** (the remaining ~73 sites), measured against `TR035` as well as
   against `TR018`.
4. **`MB002`** (`SymbolKeyedMemberDropped`, 9 on cloudflare, 12 corpus-wide). Wave three verified
   an `Emit` route exists.

Not scheduled, and here so nobody re-derives them:

- **`alignOperands` / `SY002`.** A `three` concern (45 there, 1 on cloudflare). Deprioritised with
  the rung.
- **The pass-prefix nit** (`XX:TY001` finding keys, `generator-architecture.md`, "Easy Nits").
  It renumbers every published key and touches `Findings.fs`, `Findings.test.fs`,
  `Pipeline.test.fs` and `Shape.test.fs`. It is a wave of its own, or a quiet solo lane; never
  concurrent with lanes that raise findings.
- **`TR032`, `MB003`, `SP001`, `TR008`, `TR006`, `TR009`, `SI005`.** Settled.

## What every lane reports

Per `.claude/rules/generator-fixtures.md`: `dotnet fsi build.fsx -- findings` before and after,
tier counts, the finding codes raised, `git diff --stat`, and whether the compile gate and run
gate passed. A lane that reports only "green" cannot be composed. A large-fixture movement the
lane's small evidence does not account for is **handed back with a pointer, not chased**.
