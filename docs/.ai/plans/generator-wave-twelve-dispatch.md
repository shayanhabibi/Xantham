---
category: Generator
audience: managing agent
title: Dispatch - generator wave twelve
integration-branch: worktree-generator-wave-twelve
---

# Generator wave twelve — named callback delegates

One item, two lanes, sequenced. A multi-argument callback rendered as
`System.Func<float, float, string>`, which guarantees arity at the Fable boundary and tells a
consumer nothing about what the arguments mean. It now renders as a named delegate declaration
carrying the parameter names TypeScript spelled.

`worktree-generator-wave-twelve` forks wave eleven at `6141eb2`. Waves ten and eleven are
complete and gated; nothing here disturbs the alpha packaging or the `FollowDepth` change.

## Why a proof lane came first

D5 chose `System.Func` for a measured reason, and D5a exists because two plausible alternatives
broke: curried F# function types throw `TypeError` at arity 2 or more, and tupled ones read
`undefined` at runtime without throwing — the worse failure, because nothing reports it. Whether
a *custom named* delegate behaves as `System.Func` does had never been measured. With 361
retained sites at stake and silence as the known failure mode, the assumption was worth a lane
before any generator code moved.

### Lane BA — the boundary holds

A named delegate behaves identically to `System.Func`/`System.Action` in **every** position, at
arities 2, 3 and 4, in both directions: parameter position, the `unit`-returning `Action` arm,
named-abbreviation position, `ParamObject` `Create` literal, method-shaped `ParamObject`
parameter, interface member read back from JavaScript, function-typed property, method return,
a factory of delegates crossing outward, and both nestings. Each claim reads `fn.length` as
JavaScript saw it, beside a hand-written `System.Func` control at the same arity. 26 run-gate
checks pin it.

The working spelling is `type TickHandler = delegate of x: float * y: float -> string`.

**The names are an F#-side affordance.** `handler.Invoke(x = 1.0, y = 2.0)` compiles, so the
declaration gives named arguments, IntelliSense and a readable signature. The names do not reach
JavaScript: a `TickHandler` declared `x, y` emits `(a_13, b_11)`, and a delegate read back from
the runtime carries the runtime's own binder names. Pinned as three checks rather than left as
prose, so no later reader has to guess which half was bought.

### Lane BB — landed

108 delegate declarations across the corpus: 80 minted from anonymous callbacks and reported
under `SY006`, plus 28 already-named callback aliases that now render `delegate of` where they
rendered `Func`/`Action`. Positional `Func<…>`/`Action<…>` in the goldens falls **411 → 230**.

Naming reuses `Anonymous.claim` — the same nesting, sanitising and uniquifying that names
anonymous shapes, recording `SY004` and `SY005` as it always has. Ordering rides the existing
`module rec` plus `DeclOrders`/`order-declarations`; no new ordering mechanism was introduced.
Placement follows the existing `declOrigins`/`emittingGroup` rule, so a delegate a shipped group
references is declared where that group can see it. Shapes are hash-consed on checker type id,
which is why 361 sites need 108 declarations.

`bind-free-type-params` was extended to callbacks, so a hoisted delegate is generic over the
type parameters it reads rather than closing over them.

**Three classes stay inline, deliberately.** Method-member types — 80 of the 131 remaining
positional sites, all `Create` overloads — have no reference position to name. Rank-2 signatures
cannot be named without losing the quantifier. And applications of generic callback aliases were
*tried* and reverted: naming them widened all nine `ExportedHandler` members to `obj`, so the
inline spelling is the better one. A measurement that says "leave it alone" is worth as much as
one that says "change it".

## Pre-declared finding case

Appended in `1a2f537`, before dispatch.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `SY.CallbackDelegateNamed of declaredAs: string` | `SY006` | exact | Lane BB |

## Gate and corpus

| | wave eleven `6141eb2` | composed |
| --- | ---: | ---: |
| generator tests | 467 | **469** |
| wire tests | 90 | 90 |
| run gate checks | 257 | **297** |
| exact | 495 | **513** |
| ergonomic | 1552 | **1616** |
| widened | 782 | **790** |
| escape | 193 | **195** |
| total findings | 17,947 | **17,928** |
| `TR055` | 361 | **325** |
| `SY006` | 0 | **80** |
| `TR002` / `TR003` / `RT001` | 0 / 0 / 3 | 0 / 0 / 3 |
| exit code | 0 | 0 |

### The tier rise is re-attribution, and it was checked rather than accepted

`widened` and `escape` both rose, which is the shape a new loss would take. Every finding code
was diffed between the two trees. **Not one widened or escape code rose**, and a widened one
(`TP006`) fell:

- Rose, all Exact or Ergonomic: `SY006` +80, `SY004` +68, `MB001` +34, `MB006` +6, `TP002` +4,
  `SP001` +2, `MB003` +1, `SP002` +1.
- Fell: `TR032` −167, `TR055` −36, `TR009` −5, `TR008` −3, `TP006` −2, `RA006` −1, `TR013` −1.

Total findings fall by 19. `TR032`'s −167 and `TR055`'s −36 come from the same mechanism: a
shape used at *n* sites is now reported once, at its declaration, instead of once per site. The
tier counts grade symbols rather than findings, so 94 new declaration rows add rows to every
bucket while the losses they carry move to more precise owners — `For.Props` widened →
ergonomic as the new `For.Props.Children` takes the widening.

## For the next worklist

1. **The fidelity queue**, deferred from wave ten and still priced: `TR037` 54, `TR036` 72,
   `TR023` 137, `TR018` 82, and group emission ordering. Lane R3's dispatch order in
   `docs/.ai/handovers/lane-r3.md` stands.
2. **The 131 positional sites that remain.** 80 are `Create` overload method-member types; the
   rest are rank-2 signatures and generic alias applications. Each has a recorded reason above,
   so this is a floor with causes rather than a backlog.
3. **`callback-function-lab`'s arity-4 exports are runtime-only.** Lane BA added `callFour` and
   `makeFour` to `index.js` without declaring them in `index.d.ts`, to avoid editing an exact
   `TR055` reason list in `Pipeline.test.fs` outside its ownership. Declaring them needs that
   one test line.
4. **Record the margin, not the frontier count** — carried from wave eleven.
