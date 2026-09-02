# Fable.Core binding gaps

Places where a generated binding widens because **`Fable.Core` ships no name to bind to**, not
because Fable's compilation model loses something and not because F# has no form.

`docs/fable5-workarounds.md` sorts every loss into three causes and covers only the second:

1. **F# has no form.** Rank-2 types, structural subtyping, singleton literal types.
2. **Fable 5's compilation model.** What the emitted JavaScript or the Fable runtime does.
3. **The generator has not got to it.** A gap, not a limit.

This document is a fourth: the mapping is expressible, the generator would emit it, and **the
destination type does not exist in the pinned library**. The distinction matters because the
answer is different in kind — a name that Fable.Core adds, or that this repository binds itself in
`src/Xantham.Fable.Core`, closes one of these outright. Nothing about Fable's compilation model
has to change.

Every claim is verified against the pins this repository builds on: `Fable.Core` **5.2.0**, the
`fable` tool 5.0.0, `fable-library-js` 5.0.0. The inventory method is
`fcs_nuget_types Fable.Core` against `tests/Xantham.Generator.CompileGate`, which resolves the
exact assembly the compile gate compiles against.

Counts are corpus-wide over 28 fixtures at `b993a75`, from `dotnet fsi build.fsx -- findings`.

**Answer inline, in the `(ANSWER)` blocks, as you did for `fable5-workarounds.md`.**

---

## 1. The synchronous iteration protocol has no binding

**Incidence: 28 sites** — 27 of `@cloudflare/workers-types`' 43 `TR023`, plus 1 elsewhere.
`TR023` stands at 143 corpus-wide; this is its largest single named cause.

### What Fable.Core ships

`Fable.Core.JS` carries the **asynchronous** half of the iteration protocol and none of the
synchronous half:

| name | in `Fable.Core.JS` 5.2.0 |
|---|---|
| `AsyncIterable`, `AsyncIterable<'T>` | yes |
| `AsyncIterator<'T>` | yes |
| `AsyncGenerator<'T>` | yes |
| `IteratorResult<'T>` | yes |
| `Iterable`, `Iterable<'T>` | **no** |
| `Iterator<'T>` | **no** |
| `IterableIterator<'T>` | **no** |
| `Generator<'T>` | **no** |
| `AsyncIterableIterator<'T>` | **no** (but see below) |

`Fable.Core.Py.Iterable` and `Fable.Core.Py.Iterator` do exist — they are the Python interop
surface and are unreachable from a JavaScript binding. A metadata search for the bare name finds
them and is misleading.

### What TypeScript declares

```ts
export interface Headers {
    entries(): IterableIterator<[string, string]>;
    keys(): IterableIterator<string>;
}
```

### What the binding emits

```fsharp
[<Interface>]
type Headers =
    abstract entries: unit -> obj
    abstract keys: unit -> obj
```

with `TR023 NotAmongGeneratedDeclarations` at each site — "a named type is not among the
declarations this run generated; widened to obj".

### Where they are

`IterableIterator` 20 sites, `Iterable` 7, `AsyncIterableIterator` 1. On
`@cloudflare/workers-types` these are the `Headers`, `FormData`, `URLSearchParams` and
`CacheStorage` surfaces — the iteration methods of every collection type the Workers runtime
exposes. A consumer gets `obj` where the `.d.ts` promised a sequence.

`AsyncIterableIterator` is the near miss: `JS.AsyncIterable` and `JS.AsyncIterator` both exist, so
the type is expressible as one of them with a documented loss, rather than absent outright.

[//]: # (ANSWER)
We will have to generate or hand roll our own bindings for these constructs and ship them with Xantham.Fable.Core.
We will submit a PR at a later time to Fable for a bunch of different constructs once the generator is working.
[//]: # (ANSWER END)

---

## 2. `symbol` has no binding

**Incidence: 11 sites** — `TR041 SymbolNoBinding` 5, `TR042 UniqueSymbolNoBinding` 6.

### What Fable.Core ships

No `Symbol` type in `Fable.Core.JS`. The module carries `Object`, `Math`, `Date`, `JSON`, `Map`,
`Set`, `WeakMap`, `WeakSet`, `Promise`, `RegExp`, `ArrayBuffer`, `DataView` and the `TypedArray`
family, and no `Symbol`.

### What TypeScript declares

```ts
export declare const tag: unique symbol;
export interface Keyed { [tag]: string }
```

### What the binding emits

`obj` for the symbol type, and the symbol-keyed member is dropped entirely
(`MB002 SymbolKeyedMemberDropped`, 12 sites) because F# has no form for it.

The two halves are different problems and only the first is a binding gap. A `Symbol` type would
let `unique symbol` and `symbol` bind; the *keyed member* still needs `EmitIndexer` or a similar
route, which wave three verified exists but did not take.

[//]: # (ANSWER)
Same as before; we'll generate or hand roll our own bindings for this.
[//]: # (ANSWER END)

---

## 3. `BigUint64Array` is missing from an otherwise complete typed-array family

**Incidence: 1 site**, on `@cloudflare/workers-types`.

### What Fable.Core ships

Every typed array except one: `Int8Array`, `Uint8Array`, `Uint8ClampedArray`, `Int16Array`,
`Uint16Array`, `Int32Array`, `Uint32Array`, `Float32Array`, `Float64Array`, `BigInt64Array` — and
**no `BigUint64Array`**, though `BigInt64Array` is present.

This reads as an oversight upstream rather than a decision. One site today, and the cost of the
gap is that the whole family cannot be bound by rule — a table with one hole needs a special case.

[//]: # (ANSWER)
Same as previous.
We can shadow the types that we are handrolling/generating for these types that should exist in Fable.Core.JS
through Xantham.Fable.Core such that they are reachable through the Fable.Core.JS namespace/module.
[//]: # (ANSWER END)

---

## 4. A bound name that carries no arity

**Incidence: 132 sites** — `TR024 LibExtraTypeArgumentsDropped` 118 on
`@cloudflare/workers-types`, 14 corpus-wide as `TR025 LibBindingLoss`.

Not a missing name: the name exists and binds, and the *arity* is lost. `Naming.LibBindings` maps
a compiler-lib name to a `Fable.Core.JS` name, and where the destination takes fewer type
parameters than the source the extra arguments are dropped.

`docs/plans/generator-architecture.md` (O7) already records the conclusion this forces — the
destination of a group redirection has to carry arity, not just a name. Listed here because it is
the same class of question and the answer may be the same: bind it upstream, bind it in
`src/Xantham.Fable.Core`, or accept the loss.

[//]: # (ANSWER)
Bind it in Xantham.Fable.Core
[//]: # (ANSWER END)

---

## How an entry gets added

An agent that finds a mapping blocked on a name `Fable.Core` does not ship **adds a section here
rather than only raising a finding**, because a finding says a site widened and this document says
what would close it. The bar for an entry:

- The gap is a **missing name**, not a compilation-model loss (that is `fable5-workarounds.md`)
  and not a form F# lacks.
- The absence is verified against the pinned assembly — `fcs_nuget_types Fable.Core` against
  `tests/Xantham.Generator.CompileGate`, or a compile error quoted with its code. A metadata or
  text search is not sufficient: `Fable.Core.Py` shadows several JS names and will mislead.
- A count from `dotnet fsi build.fsx -- findings`, and the finding key that reports it.
- The TypeScript, the F# emitted for it, and an empty `(ANSWER)` block.
