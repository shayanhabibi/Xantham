---
category: Generator
title: Plan - Default Lib Bindings
index: 2
---

# Can we out-bind Fable.Browser?

**Question.** Can the Wire-based generator, driven by the TypeScript 7 checker over
`lib.dom.d.ts` and friends, produce F# bindings of *higher fidelity* than the
hand-written [Fable.Browser](https://github.com/fable-compiler/fable-browser) packages?

**Answer.** Yes on fidelity — decisively, and on axes a hand-written binding cannot
reach at DOM scale. No on shippability *today*: the generated DOM binding does not
compile, for one reason (heritage is flattened rather than emitted as `inherit`), and
that one reason accounts for 1,698 of its 1,700 compile errors. The gap between "higher
fidelity" and "usable" is a bounded, well-understood work item, not an open research
problem.

This document separates three things the reader should not conflate:

1. what the generator produces **today**,
2. what the current plan already knows it leaves open (§4 of
   [Type Mapping](generator-type-mapping.md)),
3. what is reachable **beyond** that and would push fidelity past the status quo of
   any hand-written binding.

---

## 1. The experiment

`lib.dom.d.ts` is not an npm package, so it was re-hosted as one. A probe package
(`dom-probe`) whose `index.d.ts` is

```ts
/// <reference no-default-lib="true"/>
/// <reference lib="es2015" />
/// <reference lib="es2018.asynciterable" />
… the body of lib.dom.d.ts …
```

is typechecked with `--lib es2015,es2018.asynciterable` so the compiler's own DOM lib
does not collide with the copy under test. `Grouping.classify` tests `EntryPackage`
first by path prefix, so the re-hosted lib classifies as the shipping entry package and
the full pipeline runs over it. A driver script replicates `Bootstrap.start` with
`CompilerOptions.Lib` set, then folds `Harvest → Resolve → Shape → Render`.

### 1.1 It runs end to end

| Measure | Value |
|---|---|
| Input | `lib.dom.d.ts`, 45,125 lines |
| Wall clock | **9.7 s** |
| Globals harvested | 2,021 |
| Types resolved | 27,106 |
| Declarations shaped | 1,906 |
| F# emitted | 314,976 lines / 17.5 MB |

Getting this far required one generator fix, included with this document: the shape
tier recursed forever on `lib.dom.d.ts` through the cycle
`typeRef → unionRef → erasedUnionRef → typeRef → objectRef → typeRef`. A 1 GB stack did
not help, which proved the recursion cyclic rather than merely deep. `Shape/Spec.fs` now
carries a thread-static `Descent` path set: a type id already on the current reference
descent widens to `obj` with a `Widened` finding instead of being followed again. The
full suite (120 generator tests, 84 wire tests) is unchanged — every committed golden is
byte-identical.

### 1.2 It does not yet compile

Compiled against `Fable.Core` 5.2.0 and `Xantham.Fable.Core`, the emitted `DomProbe.fs`
yields **1,700 errors**:

| Cause | Count |
|---|---|
| Missing subtype relation (heritage flattened, `Inherits = []`) | 1,698 |
| FS1212 optional-argument ordering (the `createElementNS` overloads) | 2 |

The heritage errors are all of one shape. `Shape/Interfaces.fs` flattens base members into the
derived interface and emits an `Ergonomic` finding —
*"base members flattened into the interface (the is-a relation is not emitted)"* — with
`Inherits = []` hard-coded. That is fine until a generic constraint needs the relation:
`NodeListOf<TNode extends Node>` and `HTMLCollectionOf<TElement extends Element>` cannot
be satisfied by types that are structurally complete but formally unrelated. Top failing
pairs: `ChildNode → Node` (322), `obj → Node` (308), `obj → Element` (298),
`HTMLElement → Element` (298).

So: fidelity is already there; the type *lattice* is not.

---

## 2. Where the generated binding is already better

Numbers below compare the 1,906 generated declarations against Fable.Browser `main`
(585 hand-written non-companion types).

### 2.1 Coverage

**1,042 `lib.dom` interfaces have no Fable.Browser counterpart at all.** Fable.Browser
is split into hand-curated packages (`Fable.Browser.Dom`, `.Event`, `.Css`, `.Gamepad`,
…) and stops where its authors stopped. The generator's coverage is whatever the lib
file contains, by construction. Fable.Browser also has 23 members commented out behind
`// TODO`; the generator has no such category — a thing it cannot map becomes a
*recorded finding*, never a silent absence.

### 2.2 Freshness

**147 Fable.Browser names do not exist anywhere in the current lib.** They are bindings
to a web platform that no longer exists: the entire `SVGPathSeg*` family, `NodeSelector`,
`Position` / `PositionError` (superseded by `GeolocationPosition`), `WebGLObject`,
`WindowTimers`. Hand-written bindings decay against a moving spec; generated ones are
re-derived from whatever TypeScript ships.

### 2.3 Nullability

The generator emits **32,222 `option`s**, one per nullable position the checker reports.
Fable.Browser emits **zero**, and instead marks 641 types `[<AllowNullLiteral>]`,
pushing every null check onto the call site untyped. This is the single largest
day-to-day correctness difference, and it is not something a human can plausibly
maintain by hand across 45k lines.

### 2.4 Mutability

`readonly` is honoured. `lib.dom` has 3,730 `readonly` properties; Fable.Browser marks
3,280 of its 5,238 members `with get, set` regardless. Concretely, `activeElement` is
generated as get-only `Element option`; in Fable.Browser it is settable and non-null,
both wrong.

### 2.5 Literal unions and construction

- **246 `[<StringEnum>]`** unions vs roughly 59 hand-written.
- **781 `[<ParamObject>]` `Create` members** for options bags, which Fable.Browser mostly
  models as mutable classes or `obj`.

### 2.6 Documentation

Full MDN JSDoc is carried through, including `@deprecated`. Fable.Browser carries almost
none.

### 2.7 Outright corrections

Spot-checking turned up plain bugs the generator does not reproduce: `cancelBubble` is a
`bool` property, bound as a *method* in Fable.Browser; `activeElement` as above.

---

## 3. Where it is currently worse, or merely equal

These are the honest debits, and each is a named item in §4 of the type-mapping plan or
a finding tier in the manifest.

### 3.1 No `inherit` (the blocker)

Covered in §1.2. Only 8 of 1,906 generated types carry heritage. Fable.Browser models
the DOM hierarchy properly, which is why its `Element` behaves like an `Element`.

### 3.2 The keyof regime does not fire over the DOM's closed maps

`lib.dom` encodes the most valuable typing in the whole platform in
`HTMLElementTagNameMap` and the `*EventMap` family:

```ts
createElement<K extends keyof HTMLElementTagNameMap>(tagName: K, …): HTMLElementTagNameMap[K];
addEventListener<K extends keyof WindowEventMap>(type: K, listener: (ev: WindowEventMap[K]) => any, …): void;
```

Today `T[keyof T]` widens (type-mapping §4.10). The output contains zero occurrences of
`keyof<` or `typekeyof`, and `createElement` degrades to `tagName: 'K … -> obj`. The cost
shows up in the findings as **2,730 "indexed access has no F# form"** and **2,632
"overload dropped: identical after widening"** — the second number is the map collapsing:
once the key type widens, every per-tag overload becomes the same signature and all but
one is discarded. Fable.Browser hand-writes a subset of these and gets typed
`document.createElement("canvas")` where the generator gets `obj`.

### 3.3 Literal-dispatch overloads

`getContext("2d")` is the canonical case. The `"2d"` literal widens, the overload set
becomes ambiguous, and the useful return type is lost. Fable.Browser hand-wrote
`getContext_2d`.

### 3.4 Intersections of object types

`window` is declared `Window & typeof globalThis` and widens to `obj` —
*"intersection object types has no F# form yet"*. The single most-used name in the DOM is
currently untyped.

### 3.5 The long tail

| Finding | Count |
|---|---|
| `any → obj` escapes | 16,593 |
| Anonymous `__type` shapes widened | 724 |
| Unmapped template literal types | 436 |
| Types beyond the resolve depth cutoff | 289 |
| Symbol-keyed members dropped | 72 |

`any` is irreducible — TypeScript said nothing, so neither can we. The other four are
tractable.

---

## 4. What future work would buy, ranked

The generator is incomplete, and the ceiling is well above the status quo. Ranked by
fidelity gained per unit of work:

1. **Emit `inherit` for named heritage.** Unblocks compilation outright (1,698 of 1,700
   errors) and restores the DOM's subtype lattice. F# single inheritance vs TypeScript's
   multiple `extends` needs a policy — most likely: the first base becomes `inherit`,
   remaining bases stay flattened with the existing `Ergonomic` finding. This is the
   difference between "a research artefact" and "a package".

2. **Close the keyof / indexed-access regime over *concrete* maps.** When the map type is
   a closed interface of literal keys — exactly `HTMLElementTagNameMap`, exactly
   `*EventMap` — `T[K]` can be resolved per key and the method expanded into a real
   overload set, or into a phantom-typed key. This is the single largest fidelity prize in
   the file, and unlike everything else on this list it is something a hand-written
   binding **cannot sustain**: ~400 tags and ~600 events that change every release.
   Winning here is how the generated binding stops being merely broader than
   Fable.Browser and becomes categorically better.

3. **Literal-overload synthesis**, which subsumes `getContext` and the rest of the
   string-dispatch family, and would remove much of the 2,632 dropped-overload count.

4. **Object-intersection flattening**, so `window` stops being `obj`.

5. The tail: template literals, raising the resolve depth cutoff, hash-consing anonymous
   `__type` shapes so they get names instead of `obj`, symbol-keyed members, and a sharper
   policy for `any` at variance-safe positions.

Items 1 and 4 are mechanical. Item 2 is the design work. Items 3 and 5 are increments.

---

## 5. Verdict

On the axes a generator is structurally good at — coverage, freshness, nullability,
mutability, documentation, literal unions, options-bag ergonomics — the generated binding
already beats Fable.Browser by margins that are not close (1,042 missing interfaces, 147
dead names, 32,222 vs 0 `option`s). On the axes that require the generator to *understand*
the type system rather than transcribe it — heritage, keyof maps, literal dispatch,
intersections — it currently loses, and item 1 loses hard enough to prevent compilation.

None of the four losses is a wall. Fixing the first makes the binding shippable; fixing
the second makes it something no one could have written by hand.
