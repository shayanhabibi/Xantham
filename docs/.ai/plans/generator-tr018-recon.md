---
category: Generator
title: Plan - TR018 reconnaissance
---

# `TR018` — reconnaissance

`TypeReference.IntersectionOverNonObject` stands at **194 sites** after wave three's lanes 1A and
1B. This document attributes every one of them to a named cause, reduces each cause to a
reproducer of six declarative lines or fewer, and says for each whether it is a defect or the
mapping declining to lie.

**Nothing here is a code change.** One source site is cited; it was not touched.

The headline is the answer the `TR023` recon asked for in its §9: **`TR018` is a distinct defect,
not the residue of `TR023` cause 1.** Lane 1B moved `TR023` by 86 sites and moved `TR018` by
zero — the two key populations are byte-identical before and after at owner-and-symbol
granularity. The overlap the recon suspected does not exist.

---

## 1. Provenance

| | |
|---|---|
| baseline | `worktree-gen-member-shape` @ lane 1B, over `worktree-generator-wave-three` |
| corpus | `golden/**/manifest.json`, 22 fixtures |
| population | 194 `TR018` findings |
| grades | 194 `widened`, one disposition |
| passes | `shape-interfaces` 154, `shape-aliases` 34, `shape-callbacks` 4, `shape-exports` 2 |
| labs | 8 hand-authored packages of 2-6 declarative lines, run through a scratch harness calling `Pipeline.generate GeneratorConfig.Default` outside the repository; none committed |
| compiler | `node_modules/@typescript/typescript-win32-x64/lib/tsc.exe`, borrowed from the main checkout |

**Residual: zero.** All 194 are attributed. The attribution is by owning declaration, cross-read
against each owner's `.d.ts` source line and confirmed by a reproducer per cause; four owners
(`Timer`, `Timeline`, `JSAnimation`, `WAAPIAnimation`, one site each) are attributed by the shape
of the owner rather than by an individual reading.

---

## 2. Where the 194 are

By fixture:

| fixture | sites |
|---|---:|
| `animejs` | 106 |
| `@cloudflare/workers-types` | 47 |
| `solid-js` | 25 |
| `type-fest` | 14 |
| `brand-lab` | 1 |
| `intersection-lab` | 1 |

99 distinct owning declarations, with a long tail: `AutoLayoutParams` and
`LayoutAnimationParams` carry 23 each, `Utils` 12, and 79 owners carry one site apiece.

Split by where the finding lands, the population divides cleanly in two:

| position | sites |
|---|---:|
| a member's type (`Owner.member`) | 154 |
| an alias's whole body (`Owner`) | 40 |

---

## 3. The guard, and its four causes

`intersectionRef` (`Shape/Spec.fs:642`) reaches its fallback when the intersection carries no
declaration name, and picks its message from one test:

```fsharp
let reason =
    if facts.Members.IsEmpty && facts.IndexInfos.IsEmpty then
        TypeReference.IntersectionOverNonObject
    else
        TypeReference.IntersectionNotDeclared
```

The `else` arm, `TR019`, fires at **zero sites in the corpus**. Every intersection that reaches
this fallback reaches it with an empty member set, so `TR018`'s message — "intersection over a
non-object operand has no members to flatten" — is the only one anyone reads, and it covers four
distinct situations, one of which it describes accurately.

| # | cause | sites | share | verdict |
|---|---|---:|---:|---|
| A | a shared property flattened to *union & union* | 97 | 50.0% | defect, recoverable |
| B | a type-parameter operand | 39 | 20.1% | designed refusal |
| C | callable operands with no properties | 30 | 15.5% | defect, mis-keyed |
| D | the `(X & {})` autocomplete idiom | 28 | 14.4% | defect, cheap |

Per fixture:

| cause | `animejs` | `workers-types` | `solid-js` | `type-fest` | labs |
|---|---:|---:|---:|---:|---:|
| A | 66 | 31 | | | |
| B | | 16 | 13 | 8 | 2 |
| C | 30 | | | | |
| D | 10 | | 12 | 6 | |

---

## 4. Cause A — a shared property flattened to *union & union* (97, 50.0%)

### Mechanism

`A & B` where both operands declare the same property. The checker types the flattened property
as the intersection of the two declared types, and where those are unions the result is
`U₁ & U₂` — an intersection whose operands are unions. `deriveFacts` asks for structure only when
every constituent carries `TypeFlags.Object` (`Resolve.fs:306`), so the facts arrive with no
members, and the property renders `obj`.

The correct answer is computable at every site measured: `(number | (() => number)) & (number |
string)` distributes to `number`, and where the two declarations are textually identical — every
one of `workers-types`' 24 — the answer is the operand itself.

### Reproducer

**A1 — two declarations of one property (4 lines).**

```ts
export type A = { delay?: number | (() => number) };
export type B = { delay?: number | string };
export type Params = A & B;
export declare const params: Params;
```

→ **2 `TR018`** on `Params.delay`, in 37 lines. `A.delay` renders `U2<float, Func<float>>` and
`B.delay` renders `U2<string, float>`; `Params.delay` renders `obj`.

**A2 — the negative (4 lines).** The same construct where the property is not a union.

```ts
export type A = { name: string };
export type B = { name: string };
export type Both = A & B;
export declare const both: Both;
```

→ **0 findings**, and `Both` inherits both operands and redeclares `name: string`. The union is
the trigger, not the sharing.

### Where they are

`animejs`' `LayoutAnimationParams` (23) and `AutoLayoutParams` (23) intersect four and five
option bags that each declare `delay`, `duration`, `ease` and the `on*` callbacks;
`workers-types`' `EmailDestinations2/3/4` and `EmailMessageBuilder2/3/4` (4 each) are the hoisted
arms of `{ to?: … } & ({ to: … } | { cc: … } | { bcc: … })`, where the two declarations of `to`
are the same union. `RoleScopedChatInput.role` is one declaration merged with another that spells
its literal union differently.

---

## 5. Cause B — a type-parameter operand (39, 20.1%)

### Mechanism

`P & { … }` inside a generic declaration. A type parameter has no members until it is
instantiated, and `Resolve.fs:306` declines the round trip deliberately: asking would also drag a
primitive operand's apparent members — `String`'s whole prototype — into the table for nothing.

This is **the one situation the message describes accurately**, and `tests/fixtures/intersection-lab`
already pins it as intended behaviour: *"Not flattened: a type-parameter operand has no members to
read, so it widens loudly."*

### Reproducer

**B1 (2 lines).**

```ts
export type Props<P = {}> = P & { readonly children: string };
export declare function render<P>(props: Props<P>): void;
```

→ **2 `TR018`**, one on the alias and one on `render(props)`, alongside `SA002`; `Props<'P>`
renders as the erased phantom `[<Erase>] type Props<'P> = private Props__ of obj`.

### Where they are

`solid-js`' `ParentProps` / `VoidProps` / `FlowProps` family and their `Component<…>` wrappers
(13); `type-fest`'s `Opaque`, `InvariantOf`, `Tagged`, `SetOptional`, `SetRequired`,
`SetReadonly`, `Except`, `FixedLengthArray` (8); `workers-types`' `XOR`, `Fetcher`,
`DurableObjectStub`, the loopback stubs, and `Ai.run`'s `AiModels[Name]["inputs"] & AiOptions`
(16); `brand-lab`'s `Counted` and `intersection-lab`'s `merge` (1 each).

**These 39 are not work.** They are the mapping declining to invent members it cannot see.

---

## 6. Cause C — callable operands with no properties (30, 15.5%)

### Mechanism

Both operands *are* object types, the structure *was* derived, and it carries call signatures with
no properties and no index infos. The guard tests `Members` and `IndexInfos` alone, so a hybrid of
two function types is reported as an intersection over a non-object operand — which is false about
every one of these 30 sites.

The overload machinery that would render them already exists: at an export position the same
construct lands as two overloads (see the negative below).

### Reproducer

**C1 — at a member position (6 lines).**

```ts
declare function round(value: number, length: number): number;
export type Chained = (length: number) => number;
export interface Utils {
    round: typeof round & Chained;
}
export declare const utils: Utils;
```

→ **1 `TR018`** on `Utils.round`, which renders `abstract round: obj with get, set`, in 23 lines.

**C2 — the negative, at an export position (3 lines).** The same intersection as a value export.

```ts
declare function round(value: number, length: number): number;
export type Chained = (length: number) => number;
export declare const roundPad: typeof round & Chained;
```

→ **0 findings**, and `roundPad` renders as two overloads — `(value: float, length: float)` and
`(length: float)`. The signatures are reachable; the member position does not reach them.

### Where they are

All 30 are in `animejs`: `Utils`' twelve chainable helpers (`clamp`, `damp`, `lerp`, …), each
`typeof numberUtils.x & ChainedX`; the `CallbackArgument` family (11), whose `refresh`, `revert`,
`stretch` and `then` are declared by more than one of the three intersected classes; and
`DrawableSVGGeometry.setAttribute`, where the entry package narrows `Element.setAttribute` with
its own overload:

```ts
export type DrawableSVGGeometry = SVGGeometryElement & {
    setAttribute(name: "draw", value: `${number} ${number}`): void;
    draw: `${number} ${number}`;
};
```

---

## 7. Cause D — the `(X & {})` autocomplete idiom (28, 14.4%)

### Mechanism

`"in" | "out" | (string & {})` is the standard TypeScript spelling for "these literals, or any
string, and keep the literals in autocomplete". The object operand is empty, so
`brandedPrimitive` finds no marker to carry and the intersection falls through to the fallback
with an empty member set.

The cost is not the arm: **the whole union widens**. `type Ease = obj` replaces `type Ease =
string`. `X & {}` reduces to `X` for every `X`.

### Reproducer

**D1 (2 lines).**

```ts
export type Ease = "in" | "out" | (string & {});
export declare const ease: Ease;
```

→ **1 `TR018`** plus 2 `TR006`, and `type Ease = obj` where `string` is the whole answer.

### Where they are

`solid-js` declares `type Element = Node | ArrayElement | (string & {}) | number | boolean | null
| undefined` in `jsx.d.ts`, and every one of its 12 sites is a type carrying `JSX.Element`:
`JSXElement`, `ResolvedJSXElement`, `ResolvedChildren`, `ValidComponent`, and the `children`
member of the `Match` / `Show` / `ErrorBoundary` props. `animejs` spells it three times
(`EasingParam`, `WAAPIEasingParam`, `TweenComposition`) and pays for it at ten sites, because
seven further aliases carry one of those in a union. `type-fest`'s `LiteralUnion<Literal, Base> =
Literal | (Base & Record<never, never>)` is the same idiom under a name, and `PackageJson` reads
it at `cpu`, `os` and `homepage`.

---

## 8. Recommendation

Three lanes and one non-lane, in cost order.

### Lane D — reduce `X & {}` to `X`. **Cheapest, do first.**

**Recovers 28 sites (14.4%)** and, more to the point, replaces `obj` with the operand's own type
at 28 reference positions — three of them whole exported aliases in `solid-js`'s public surface.
The test is one predicate over the intersection's operands: exactly one carries members, the rest
carry none. It is the smallest lane in this document and the only one whose correct answer needs
no new machinery.

**Risk:** `brand-lab`'s `Counted = string & { count: number }` must keep its brand. The object
operand there is not empty, so the predicate separates them, and the lab already pins it.

### Lane C — reach the call signatures at a member position.

**Recovers 30 sites (15.5%)**, all `animejs`, and turns twelve `abstract round: obj` into the
overloaded delegates the same construct already renders at an export position. The guard's test
should read `CallSignatures` alongside `Members` and `IndexInfos`; where signatures are present
the site is a callable, and `isPureCallback` is the existing route.

**Risk:** hybrids that carry both properties and signatures already flatten correctly today
(`intersection-lab`'s `Cancelable`), so the change must widen what is *recognised* rather than
what is *reported*.

### Lane A — distribute an intersection over its union operands.

**Recovers up to 97 sites (50.0%)**, and it is the largest and the least certain. The reduction is
real — `U₁ & U₂` distributes, and the identical-operand case (`workers-types`' 24) collapses to
one side — but it is a type-level computation the generator would be performing on the checker's
behalf, and the union cap (`TR035`) is downstream of it. Price it on its own before enrolling it.

**Do first, and separately:** the identical-operand slice. Where every operand of the flattened
property is the same type, the answer is that type, and no distribution is needed. That is
`workers-types`' 24 sites for one equality test.

### Not a lane — cause B (39 sites)

`P & { … }` has no members until it is instantiated, and the finding says so. Leaving it is the
mapping working. What is worth changing is the *message*, not the behaviour: `TR018` currently
reads as a defect report at 39 sites where it is a contract.

### The shape of the win

| after | `TR018` remaining |
|---|---:|
| today | 194 |
| Lane D | 166 |
| + Lane C | 136 |
| + the identical-operand slice of A | 112 |
| + the rest of Lane A | 39 |

39 is the floor without changing what the resolve tier asks the checker for: the type-parameter
operands, which are a contract rather than a loss.

---

## 9. What this document does not cover

- **`TR019`** (`IntersectionNotDeclared`, the guard's other arm) fires nowhere. Whether it is
  reachable at all, and whether the two arms want to be one case, is a question for whoever opens
  the guard.
- **Whether cause A's distribution interacts with the union cap.** `TR035` (union too wide) stands
  at 87 across the corpus, and a distributed intersection produces the cross product of its
  operands' arms. Lane A must be measured against `TR035`, not only against `TR018`.
- **The four `animejs` sites attributed by owner shape** — `Timer`, `Timeline`, `JSAnimation` and
  `WAAPIAnimation`, each one site, each on the parameter of `then(callback)`. They are grouped
  with cause C because their owners' shared members are, but no reproducer isolates them.
