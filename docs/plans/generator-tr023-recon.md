---
category: Generator
title: Plan - TR023 reconnaissance
---

# `TR023` — reconnaissance

`TypeReference.NotAmongGeneratedDeclarations` is the second-largest non-designed finding in the
corpus: **727 sites**, all of them a reference widened to `obj` because the shape tier could not
name what it was pointing at. This document attributes every one of the 727 to a named cause,
reduces each cause to a reproducer of a dozen declarative lines or fewer, and says for each
whether it is worth fixing, what fixing it would take, and roughly what it would recover.

**Nothing here is a code change.** Five source sites are cited; none was touched.

The headline: **509 of the 727 (70.0%) come from one defect with one shape** — the generator does
not recognise an array when the array arrives under some name other than `Array`. That defect also
costs 2,164 lines of committed golden, including 42% of the entire `solid-js` binding.

---

## 1. Provenance

Every number came from a script over the artefacts, never from reading them. No manifest, no
`symbols` array and no golden `.fs` was opened whole or paged through, per
`.claude/rules/generator-fixtures.md`; the incantations are in
[appendix A](#appendix-a--how-each-number-was-taken).

| | |
|---|---|
| baseline | `worktree-gen-tr023-recon` @ `8ded872`, the wave's pre-dispatch commit |
| corpus | the goldens under `tests/Xantham.Generator.Tests/golden/` — 18 rendered `.fs` files, 53,476 lines |
| population | 727 `TR023` findings, extracted from `golden/**/manifest.json` and cross-checked against `dotnet fsi build.fsx -- findings --key TR023` |
| grades | 615 `widened`, 112 `escape` — the same finding case reached from two dispositions |
| labs | 13 hand-authored packages of 1-6 declarative lines each, run through a scratch harness calling `Pipeline.generate GeneratorConfig.Default` outside the repository; none committed |
| compiler | `node_modules/@typescript/typescript-win32-x64/lib/tsc.exe`, borrowed from the main checkout |
| Fable surface | pinned `Fable.Core` 5.2.0, interrogated through `fcs_referenced_symbols` against `Xantham.Generator.CompileGate.fsproj` |

**Residual: zero.** All 727 are attributed. Nothing was left in an "other" bucket and nothing was
forced into a bucket it did not fit — the three causes are distinguishable from the finding's own
`shown` string plus the owning declaration, not by eyeballing.

---

## 2. Where the 727 are

By fixture:

| fixture | sites | share |
|---|---:|---:|
| `type-fest` | 351 | 48.3% |
| `@cloudflare/workers-types` | 148 | 20.4% |
| `solid-js` | 118 | 16.2% |
| `animejs` | 101 | 13.9% |
| `inherit-lab` | 3 | |
| `lib-lab` | 3 | |
| `keyof-lab` | 2 | |
| `ansi-regex` | 1 | |

By owning declaration the distribution is extremely concentrated. **Eight declarations carry 618 of
the 727 (85.0%)**:

| owner | fixture | sites |
|---|---|---:|
| `ReadonlyTuple` | `type-fest` | 319 |
| `DrawableSVGGeometry` | `animejs` | 86 |
| `EmailDestinations2To` | `@cloudflare/workers-types` | 38 |
| `EmailDestinations2To2` | `@cloudflare/workers-types` | 38 |
| `ArrayElement` | `solid-js` | 38 |
| `SplitPropsItem` | `solid-js` | 38 |
| `SplitPropsResultItem` | `solid-js` | 38 |
| `StructuredCloneable` | `type-fest` | 23 |

The remaining ~109 sites are spread over 60-odd owners at 1-8 each.

There are only **166 distinct `shown` names** across the 727 sites, and 30 of them are the members
of `Array` (`at`, `concat`, `map`, `filter`, …), accounting for 509 sites on their own.

---

## 3. The three causes

| # | cause | sites | share |
|---|---|---:|---:|
| 1 | a compiler-lib type's members are reached structurally, and each member's *own* type fails to resolve | 595 | 81.8% |
| 2 | a compiler-lib type **name** has no F# binding to point at | 95 | 13.1% |
| 3 | an object type with **no members** can never be given a declaration name | 37 | 5.1% |

Causes 1 and 2 are both "the compiler lib", but they are not the same bug and do not have the same
fix: cause 2 is a *missing row in a table*, cause 1 is a *wrong turn at `Resolve.fs:403`* that
happens even for lib types that do have bindings.

---

## 4. Cause 1 — a lib type's members are reached structurally (595, 81.8%)

### The mechanism

`Resolve.fs:403` short-circuits any type whose origin group is not `Ship`:

```fsharp
if GeneratorConfig.disposition ctx.Config origin <> Ship && not isAnonymousShape then
    // Identity only (O7): the shape tier renders references to this group by
    // templated name or widens them, and either way nothing reads its members.
```

The comment above `isAnonymousShape` (`Resolve.fs:395`) states the premise honestly: *"The O7
shortcut below rests on the group's types having names to be referenced by."* The escape hatch
covers mapped types and checker-invented `__`-prefixed names.

The premise fails for a **member's** type. When the entry package reaches into a lib type
structurally — by `extends Array<T>`, by intersecting with it, or by a mapped type over it — the
shape tier walks that type's members, and for member `at` it resolves the type *of `at`*. That
type's symbol is the **method symbol**, whose name is `at`, not `Array`. `at` is not a `__` name
and its type is not `Mapped`, so `isAnonymousShape` is false, the shortcut fires, and the method
arrives at the shape tier as identity only: no call signatures, no members, nothing.

`objectRef` (`Spec.fs:539`) then walks its ladder and misses every rung. `isPureCallback`
(`Spec.fs:120`) requires a non-empty `CallSignatures` — the stripped method has none.
`arrayElement` (`Spec.fs:127`) matches only `SymbolName = Some("Array" | "ReadonlyArray")`.
`isTuple` (`Spec.fs:269`), `instantiationOf`, `libBinding`, `globalThis` and `isConstructorObject`
all miss. The last arm, at `Spec.fs:599`, fires — reporting the *member's* name:

```fsharp
| (Ship | Widen), _ ->
    let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
    FsObj, [ Finding.make owner (TypeReference.NotAmongGeneratedDeclarations shown) ]
```

Hence 30 findings named `at`, `concat`, `entries`, … per array-shaped declaration.

### By lib family

| family | sites | where |
|---|---:|---|
| `Array` / `ReadonlyArray` members | **509** | `ReadonlyTuple` 319, the two `EmailDestinations2To*` 76, the three `solid-js` owners 114 |
| DOM `Element` / `Node` members | 79 | `animejs` `DrawableSVGGeometry` |
| `ErrorConstructor.isError` | 4 | `@cloudflare/workers-types` |
| `Promise` `then` / `catch` / `finally` | 3 | `inherit-lab` `Deferred` |

The 509 array-shaped sites are the single largest block in the whole key, and they are one
recognizer away from disappearing.

### Reproducers

**1a — the `extends` form (2 lines).** Exactly `solid-js`'s `ArrayElement`.

```ts
export interface ArrayElement extends Array<string> {}
export declare const value: ArrayElement;
```

→ **38 `TR023`**, and a 377-line `ArrayElement` declaration in a 391-line file, every member `obj`.
The correct rendering is `type ArrayElement = string[]`.

**1b — the intersection form (2 lines).**

```ts
export type Tagged = readonly string[] & { readonly tag: "t" };
export declare const value: Tagged;
```

→ **29 `TR023`**, 276 lines under `Tagged`.

**1c — the mapped/union form (3 lines).** This is `type-fest`'s `ReadonlyTuple`, reduced from its
11-way digit expansion to 3.

```ts
type TupleOf<Length extends 0 | 1 | 2, Fill> = [[], [Fill], [Fill, Fill]][Length];
export type ReadonlyTuple<Element, Length extends 0 | 1 | 2> = Readonly<TupleOf<Length, Element>>;
export declare const value: ReadonlyTuple<string, 2>;
```

→ **87 `TR023`** — 29 member names × 3 union arms — and 272 lines under `ReadonlyTuple`. In
`type-fest` the same construct has 11 arms: 29 × 11 = 319, the largest single owner in the corpus.
**The multiplier is union arity**, not anything about `Readonly`.

**1d — the array-inside-a-union-member form (5 lines).** This is `@cloudflare/workers-types`'
`EmailDestinations`.

```ts
export interface Address { readonly a: string }
export type Destinations = { to?: string | Address | (string | Address)[] } & (
  | { to: string | Address | (string | Address)[] }
  | { cc: string }
);
export declare const value: Destinations;
```

→ **76 `TR023`** (38 × 2 hoisted `To` declarations), 791 lines.

**1e — the DOM form (4 lines).** This is `animejs`' `DrawableSVGGeometry`.

```ts
export type DrawableSVGGeometry = SVGGeometryElement & {
    draw: string;
};
export declare const shape: DrawableSVGGeometry;
```

→ **87 `TR023`** (86 in the golden under the same owner) and a **1,488-line** declaration, against
1,486 in the golden. This one is *not* array-shaped, so no array recognizer will help it.

**1f — the control (3 lines).** The decisive negative. Identical construct, entry-group base:

```ts
interface Base { at(index: number): string; readonly length: number }
export interface Derived extends Base {}
export declare const value: Derived;
```

→ **0 `TR023`**, 25 lines, `at` rendered as a delegate. This proves the trigger is the *group
disposition* at `Resolve.fs:403`, not `extends`-flattening, not method members, and not anything
about how `Derived` is written. Move the same declarations into the entry package and the bug
vanishes.

**1g — the non-reproducer, recorded so nobody chases it.**

```ts
export type Frozen = Readonly<string[]>;
export type Plain = readonly string[];
export type Mutable = string[];
export declare const a: Frozen;
```

→ **0 findings**, 22 lines, all three render `string[]`. `Readonly<T[]>` over a *concrete* operand
is already handled. Only the deferred/generic operand (1c) fails, because the checker cannot
collapse it before the generator sees it.

---

## 5. Cause 2 — a lib type name has no F# binding (95, 13.1%)

Here the `shown` name really is a type name, the ladder reached `libBinding`, and
`Naming.LibBindings` / `Naming.BrowserBindings` had no row. This is the *designed* failure mode of
the mapping — but it splits three ways, and only one of the three is work.

| sub-case | sites | verdict |
|---|---:|---|
| **no target exists in the pinned packages** | 54 | not generator work |
| **deliberate refusal** — `seq`-shaped names | 30 | not work; already documented |
| **a target exists and the row is missing** | 11 | small, real, cheap |

**No target (54).** 47 host/DOM names — `Animation` 7, `WebAssembly.Module` 8, `ResizeObserver` 3,
`DOMMatrix` 3, `DOMPoint` 3, `DOMRect*`, `GPU*`, `FileSystem*Handle`, `VideoFrame`, `CryptoKey`,
`ImageBitmap`, `StylePropertyMap`, … Every one of the 28 distinct host names was checked against
`BrowserBindingTable.generated.fs`: **none is present**, because that table is generated from what
the pinned `Fable.Browser.*` packages actually export. Plus 7 ECMAScript names with the same
problem — `BigUint64Array` 3, `RegExp` 2, `Float16Array` 2. Confirmed against pinned `Fable.Core`
5.2.0: there is no `JS.RegExp` (only `JS.RegExpConstructor`), no `JS.BigUint64Array` and no
`JS.Float16Array`. **These 54 cannot be fixed by editing the generator.** They move only if the
pinned Fable surface grows.

**Deliberate refusal (30).** `IterableIterator` 20, `Iterable` 9, `AsyncIterableIterator` 1. The
table says why, at `Model.fs:222-226`:

> `seq`-shaped names (`Iterable`, `Iterator`) are absent on purpose — Fable.Core binds only the
> async ones, and pretending `seq<'T>` interoperates with a JS iterable is exactly the kind of
> claim this table exists not to make.

That reasoning is intact. These 30 are the mapping being honest and should stay.

**Missing row (11).** `Error` 8 — Fable compiles a JS `Error` to `System.Exception`, so `exn` is a
defensible target with a loss note — and the boxed primitive wrappers `Boolean` / `Number` /
`String` at 1 each (`bool` / `float` / `string`). This is the only recoverable slice, and it is
1.5% of the key.

### Reproducers

**2a — the ECMAScript form (1 line).**

```ts
export declare function each(): IterableIterator<string>;
```

→ **1 `TR023`**: `IterableIterator is not among the generated declarations; widened to obj`.

**2b — the host form (2 lines).**

```ts
export declare const matrix: DOMMatrix;
export declare const err: Error;
```

→ **2 `TR023`**, one naming `DOMMatrix` (no target exists) and one naming `Error` (target exists,
row missing). 18 lines out.

---

## 6. Cause 3 — an object type with no members (37, 5.1%)

`Anonymous.needsName` (`Shape/Anonymous.fs:44`) requires, at `:61`:

```fsharp
&& not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
```

A shape with neither members nor index infos therefore never claims a declaration name, so when
`objectRef` later looks it up in `model.DeclNames` it is not there, and the fallback fires. The
`shown` name is `__type` (31 sites) or the empty declaration's own name (6 sites).

This is **almost entirely a reporting problem, not a mapping problem** — the rendering is already
what it should be:

- **9 sites are pure noise.** The owner already carries `SA002` and renders as a designed erased
  phantom (`type DeepPartial<'T> = private DeepPartial__ of obj`). `TR023` is a second report of
  the same already-explained, already-intended outcome.
- **22 further `__type` sites** are anonymous `{}` literals and member-less mapped expansions.
  `obj` is the right answer for `{}`; the finding just names the wrong thing.
- **6 named empty declarations.** Four — `TestController`, `TraceItemConnectEventInfo`,
  `TraceItemCustomEventInfo`, `SqlStorageStatement` — render as `type X = obj`. That is a *nominal*
  loss (the name stops being a distinct type), not a semantic one, and it deserves its own finding
  case rather than being reported as "a declaration went missing". `DurableObjectClass<'_T>`
  renders as an erased phantom instead, being generic. `Env` is **the one site in the whole key
  where `TR023` is exactly right**: `interface Env {}` produces no declaration head anywhere in the
  rendered file, so the reference really is to something that is not among the generated
  declarations.

### Reproducers

**3a — all three shapes at once (6 lines).**

```ts
export interface Empty {}
export type Flags<T> = { [K in keyof T]: boolean };
export interface Choice {
    logprobs?: {} | null;
}
export declare const value: Empty;
```

→ **3 `TR023`** (2 × `__type`, 1 × `Empty`) alongside `SA002`, in 27 lines.

**3b — the minimum (2 lines).**

```ts
export interface Empty {}
export declare const value: Empty;
```

→ **1 `TR023`** naming `Empty`, 18 lines.

**3c — the `SA002` double report (3 lines).** `type-fest`'s `XOR`; the noise slice.

```ts
export type Without<T, U> = { [P in Exclude<keyof T, keyof U>]?: never };
export type XOR<T, U> = (T & Without<U, T>) | (U & Without<T, U>);
export declare const value: Without<{ a: 1 }, { b: 2 }>;
```

→ **2 × `SA002`** and **1 × `TR023`** naming `__type`, on the same symbol, in 28 lines.

---

## 7. What cause 1 costs in committed golden

Measured by `awk` over each rendered `.fs`, splitting on `^type ` / `^and `, never printing the
file:

| declaration | golden lines | file total | share of file |
|---|---:|---:|---:|
| `solid-js` `ArrayElement` + `SplitPropsItem` + `SplitPropsResultItem` | 375 + 375 + 373 = **1,123** | 2,672 | **42.0%** |
| `animejs` `DrawableSVGGeometry` | **1,486** | 5,697 | **26.1%** |
| `workers-types` `EmailDestinations2To` + `…2To2` | 378 + 379 = **757** | 31,108 | 2.4% |
| `type-fest` `ReadonlyTuple` | **284** | 11,955 | 2.4% |

**3,650 lines of 53,476 corpus-wide (6.8%)** are the rendered form of cause 1. The array-shaped
subset alone — the part one recognizer removes — is **2,164 lines**. Nearly half of `solid-js`'s
binding is three copies of a widened `Array`.

---

## 8. Recommendation

Two lanes, strictly ordered, plus two non-lanes.

### Lane 1A — recognise array-shaped operands as arrays. **Do this first.**

**Recovers 509 sites (70.0% of the key) and ~2,164 golden lines.**

`arrayElement` (`Spec.fs:127`) matches on the symbol name alone:

```fsharp
match facts.SymbolName, facts.TypeArguments with
| Some("Array" | "ReadonlyArray"), [ element ] -> Some element
```

which misses all three real-world spellings: `Readonly<T[]>` over a deferred operand (the symbol is
`__type`), `interface X extends Array<T> {}` (the symbol is the interface's own name), and
`readonly T[] & { … }` (the symbol is the intersection's). A **structural** recognizer — a numeric
index signature, plus `length: number`, plus a member set that is `Array`'s — catches all three,
and `interface ArrayElement extends Array<Element> {}` becomes `type ArrayElement = Element[]`
instead of 375 lines of `obj`.

It depends on nothing, it is confined to one predicate and its call sites, and reproducers 1a, 1b,
1c and 1d all become one-liners of correct output. The three affected goldens shrink hard, which is
the point — but it means the lane's diff is mostly golden churn and should be reviewed as such.
`arrayElement`'s doc comment ("recognized by identity so the check holds for every group
disposition") is the claim being revised, so revise the comment with it.

**Risk:** a structural recognizer can false-positive on an entry-package interface that merely
looks like an array. Pin the negative with a lab — an entry-group interface with a numeric index
signature and a `length` but no `Array` ancestry must *not* collapse.

### Lane 1B — resolve a member symbol's type structurally. **Second, after 1A.**

**Recovers the remaining 86 cause-1 sites** — the DOM intersections (79), `ErrorConstructor.isError`
(4) and `Promise`'s `then` / `catch` / `finally` (3) — and ~1,486 golden lines, nearly all of them
`animejs`.

Extend `isAnonymousShape` (`Resolve.fs:395`) so that a symbol whose `SymbolFlags` say it is a
*member* (Method / Property / Signature) rather than a *type declaration* (Interface / Class /
TypeAlias / Enum) is resolved structurally regardless of its group. The stripped method then
arrives with its call signatures intact, `isPureCallback` matches, and it renders as a `Func<…>`
delegate — which lab 1f already proves is exactly what happens once the shortcut does not fire.

Second, not first, because 1A removes 70% of the surface this would otherwise churn through, and
because 1B is the riskier of the two: it deepens the type table on lib-heavy rungs (a runtime cost
worth measuring on `@cloudflare/workers-types`) and at some sites it will convert `TR023` into a
*different* finding rather than into `exact`. Do it against a corpus 1A has already shrunk.

### Not a lane — cause 2 (95 sites)

84 of the 95 are correct behaviour: 54 have no target in the pinned Fable packages, and 30 are a
documented, deliberate refusal. The recoverable 11 (`Error` → `exn` with a loss note, and
`Boolean` / `Number` / `String` → primitives) are four table rows in `Model.fs:180-221` and belong
as a rider on some other lane, not a lane of their own. **Do not send an agent at "the compiler-lib
binding gap"** — it will spend its budget rediscovering that `Fable.Browser.*` does not export
`DOMMatrix`.

### Not a mapping fix — cause 3 (37 sites)

A reporting change, and a small one:

1. Suppress `TR023` where the owning symbol already carries `SA002` — 9 sites of pure double
   reporting.
2. Give member-less object types their own finding case, graded `Widened`, so `{}` and empty
   interfaces stop being reported as "a declaration is missing" when no declaration was ever going
   to exist. That covers 27 of the other 28 — `Env` stays, correctly — and makes the key's count
   mean one thing again.

Worth doing *with* 1A, in the same lane: it costs a case in `Findings.fs` and a predicate, and
leaving it makes the post-1A `TR023` count harder to read.

### The shape of the win

| after | `TR023` remaining |
|---|---:|
| today | 727 |
| Lane 1A | 218 |
| Lane 1A + the cause-3 reporting fix | 182 |
| + Lane 1B | 96 |
| + the 11 recoverable table rows | 85 |

85 is the floor without changing the pinned Fable surface: 84 sites of the mapping declining to
lie, plus `Env`. That is a key worth keeping.

---

## 9. What this document does not cover

**`TR018` (232, "intersection over a non-object operand has no members to flatten")** was offered
as a stretch and is **not** covered. `TR023` used the full budget: 727 sites, seven reproducers
built and run, plus a control and a non-reproducer built to eliminate two wrong hypotheses, plus
the golden cost measured per declaration. A complete `TR023` beats two half-answers — and there is
a concrete reason to think the two keys overlap: lab 1d emitted 4 `TR018` alongside its 76 `TR023`,
and lab 1e's DOM intersection is `TR018`'s exact shape. **Re-measure `TR018` after 1A and 1B land.**
Part of it may be the same defect wearing a different finding case, and reconnoitring it now would
price work that is about to move.

---

## Appendix A — how each number was taken

Per `.claude/rules/generator-fixtures.md`: run everything, read almost none of it. The largest
single read of generated output anywhere in this work was a lab manifest's key counter.

| number | how |
|---|---|
| the 727 population | `python` over `golden/**/manifest.json`, emitting one tuple per `TR023` finding — `(fixture, shown, owner, file, pass, grade)` — into a scratch JSON; cross-checked against `dotnet fsi build.fsx -- findings --key TR023` |
| distribution by fixture, owner and `shown` | `collections.Counter` over that tuple list |
| the cause split | `shown = "__type"`, or `shown` naming a known empty declaration → cause 3; otherwise initial-uppercase (a type name) → cause 2, initial-lowercase (a member name) → cause 1. Every one of the 166 distinct `shown` names was inspected in the counter, so the split is enumerated rather than sampled |
| the lib-family split of cause 1 | owning declaration cross-referenced against the `shown` names appearing under it |
| lab findings counts | `python` over each lab's `manifest.json`, printing a finding-key counter and a `TR023`-`shown` counter — never the `symbols` array |
| lab line counts | `wc -l` on the rendered file; per-declaration counts by the `awk` below |
| per-declaration golden lines | `awk '/^(type\|and) /{cur=$2} cur{c[cur]++} END{for(n in c) print c[n], n}' <file>` piped through `sort -rn \| head` — emits `lines<TAB>name`, never the file |
| corpus total lines | `find golden -name '*.fs' \| xargs grep -c '' \| awk -F: '{s+=$2} END{print s}'` |
| browser-table membership | `grep -c '"<Name>"' src/Xantham.Generator/BrowserBindingTable.generated.fs` for each of the 28 distinct host names |
| the pinned Fable surface | `fcs_referenced_symbols` for `RegExp`, `Uint64Array` and `Fable.Core.JS.` against `tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj`. Note that the *generator* project does not reference `Fable.Core`, so querying it returns an empty — and misleading — result; ask the compile gate |
| lab runs | scratch `.fsx` `#r`-ing the built generator, setting `XANTHAM_TSGO_EXE`, calling `Pipeline.generate GeneratorConfig.Default <labDir>` and writing `rendered.Files` outside the repository |

The 13 labs live outside the worktree and are not committed. Every one of them is quoted in full
above, so each is two lines of transcription to rebuild.
