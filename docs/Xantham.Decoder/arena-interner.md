---
title: ArenaInterner
category: Xantham.Decoder
categoryindex: 2
index: 6
---

# `ArenaInterner` — the lazy resolved object graph

`ArenaInterner` lifts the flat, key-addressed `DecodedResult` into an
in-memory object graph in which every `TypeKey` reference is replaced with a
`Lazy<ResolvedType>`. Following a reference becomes "force a lazy"; no map
lookup is needed at the call site.

## Why a second representation?

The wire format is two flat maps keyed by `TypeKey`:

```
TypeMap          : Map<TypeKey, TsType>
ExportTypeMap    : Map<TypeKey, TsExportDeclaration>
```

References between types are *also* `TypeKey` values, which means every
dereference inside a generator has to:

1. Carry both maps (or close over them).
2. Look up the key.
3. Pattern match on the resulting `TsType` / `TsExportDeclaration`.

That's fine for one or two hops, but rendering walks the type graph
recursively. The arena representation removes the ceremony:

* Every `TypeKey` reference becomes a `Lazy<ResolvedType>`.
* Forcing a lazy returns the resolved record, with its own outgoing
  references already represented as lazies.
* Resolution is memoised in a per-`TypeKey` dictionary, so repeated
  references through different paths return the *same* instance.

Combined with `[<ReferenceEquality>]` on the resolved record types, this
gives generators identity-based sharing detection essentially for free —
you can recognise that two members reference the same `TypeParameter`
without carrying keys around.

## How cycles are handled

TypeScript declarations are routinely self-referential:

```ts
interface Node { parent?: Node; children: Node[] }
type Json = string | number | boolean | null | Json[] | { [k: string]: Json }
```

Under the arena representation, every outgoing reference is a `Lazy<_>` and
construction of a node never forces those lazies. By the time anything is
forced, the entire graph (including the cyclic partner) has already been
shelled and inserted into the cache, so re-entrance is impossible.

You don't need to write any cycle detection in your generator. Walk the
graph with normal pattern matching; lazy boundaries break the recursion for
you.

## Active pattern

The module exposes `(|Resolve|)` for ergonomic forcing:

```fsharp
match someLazyResolvedType with
| Resolve t ->
    // t is a fully forced ResolvedType
    ...
```

This is purely sugar over `.Value`, but it composes with nested patterns —
e.g. `Resolve (ResolvedType.Union { Members = members })`.

## When to use it

**Prefer `ArenaInterner` when:**

* Rendering functions should not have to thread global maps around.
* Identity / structural sharing between types is meaningful to your output.
* The generator pattern-matches recursively on `ResolvedType` (much more
  ergonomic than repeated map lookups).

**Stay with `DecodedResult` maps when:**

* You only need a small slice of the graph. `ArenaInterner.create` shells
  every entry in the export map at construction time, so startup cost
  scales with the size of the input even when you only plan to read a
  handful of types.
* You need `TypeKey` identity preserved on every node — the resolved graph
  drops keys in favour of object identity, which matters if your output
  uses keys as stable cross-references.
* Startup time is important and the input is large.

## Construction

`XanthamTree` lazily creates the interner the first time you ask for it:

```fsharp
let interner = tree.GetArenaInterner()
```

Subsequent calls return the cached value. To go directly:

```fsharp
let interner = ArenaInterner.ArenaInterner.create decodedResult
```

In both cases, every key in `ExportTypeMap` is shelled eagerly into the
cache. Nested types are still deferred; only the outer envelope of each
exported declaration is materialised on construction.

## Diagnostics

`QualifiedNamePartDiagnostic` is a `[<Flags>]` enum surfaced when a name part
contains characters that would break a generated F# qualified name:

* `ContainsQuotationMarks`
* `ContainsSlash`
* `ContainsPeriod`

Generators can use this to decide whether to backtick-wrap, replace, or skip
a name segment.
