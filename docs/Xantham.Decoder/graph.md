---
title: Dependency Graph
category: Xantham.Decoder
categoryindex: 2
index: 5
---

# Dependency Graph

`Xantham.Decoder.Types.Graph` builds a dependency view over the decoded
type/export maps. It is the analytical counterpart to the resolved object
graph: while [`ArenaInterner`](arena-interner.html) gives you a navigable
graph for *rendering*, `Graph` gives you a graph for *analysis* —
topological ordering, cycle detection, and reachability.

## What's in a `Graph`

The `Graph` record has two frozen lookups:

| Field | Type | Meaning |
|-------|------|---------|
| `Dependents` | `FrozenDictionary<TypeKey, FrozenSet<TypeKey>>` | For each `TypeKey`, the set of keys it depends on. |
| `Cycles` | `FrozenDictionary<TypeKey, TypeKey>` | Pairs `(typeKey, cycleParticipant)` — keys that participate in a cycle, mapped to the partner that closes it. |

A `TypeKey` not present in `Cycles` is acyclic with respect to its
dependencies.

## Building one

The most direct path is from a `XanthamTree`:

```fsharp
let graph = tree.GetDependencyGraph()
```

Internally this calls `Graph.create excludeConditionalBranches decodedResult`
with `excludeConditionalBranches = false` — meaning the
`Check` and `Extends` arms of TypeScript conditional types are *not* traced as
dependency edges. Including them is occasionally useful for very thorough
reachability analysis, but for the common case of "what would I have to
declare before this type" they only add noise.

If you need the alternative, call `Graph.create` directly:

```fsharp
let graphAll = Graph.create true decodedResult
```

## Typical uses

**Topological emit order.** Start from the leaves of `Dependents` and emit
upward. Any key that appears as a value (a dependency) of an unemitted key
must be emitted first.

**Cycle reporting.** Iterate `Cycles` to surface mutually-recursive type
definitions to the user, or to decide where to break the cycle in generated
output (e.g. by switching to interface-style declarations).

**Reachability filtering.** Walk `Dependents` from a chosen entry set to
compute the transitive closure of types actually needed by a generation
target — useful when the input contains a large standard library you don't
want to emit wholesale.

## Notes

* The graph is built over both `TypeMap` and `ExportTypeMap` keys; it does
  not distinguish between structural types and exported declarations.
* Edges are recorded against the pre-compression / post-sanitization key
  space — so behaviour depends on the `Decoder.Settings` used to load the
  tree.
* The graph is computed eagerly when `GetDependencyGraph()` is called; cache
  the result if you intend to query it more than once.
