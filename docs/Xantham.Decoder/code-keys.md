---
title: Code Keys
category: Xantham.Decoder
categoryindex: 2
index: 4
---

# Code Keys — tagged dispatch over `TypeKey`

A raw `TypeKey` is just an opaque address into the type or export map. It
does not, by itself, tell a generator whether the construct it points at is a
union, a class, a function export, or a primitive.

The `CodeKey` family solves this by tagging a `TypeKey` with the kind of
construct it identifies, so generators can pattern match without re-walking
the underlying `TsType` / `TsExportDeclaration`.

## The three layers

```text
CodeKey
 ├── Export of ExportCodeKey  ── Variable | Interface | TypeAlias | Class | Enum | Module | Function
 └── Type   of TypeCodeKey    ── GlobalThis | Conditional | Primitive | Union | …
```

* **`TypeCodeKey`** — every structural `TsType` case that isn't itself a
  declaration (e.g. `Union`, `Tuple`, `TypeReference`, `TemplateLiteral`).
  `GlobalThis` is the one nullary case; everything else carries the original
  `TypeKey`.
* **`ExportCodeKey`** — every top-level declaration kind. `Interface`,
  `Class`, and `Enum` are reachable through *both* paths (the structural map
  and the export map); `Codify` always normalizes them to `ExportCodeKey`.
* **`CodeKey`** — the umbrella DU joining the two halves.

## How to obtain one

The recommended path is `XanthamTree.Codify`:

```fsharp
let kind = tree.Codify someKey
match kind with
| CodeKey.Export (ExportCodeKey.Class classKey)     -> ...
| CodeKey.Export (ExportCodeKey.Function funcKey)   -> ...
| CodeKey.Type   (TypeCodeKey.Union unionKey)       -> ...
| CodeKey.Type    TypeCodeKey.GlobalThis            -> ...
| _ -> ...
```

`Codify` resolves the key against both maps in a single pass: if it finds an
export it categorises against `ExportCodeKey`; otherwise it categorises the
structural type. `TsType.Interface`, `TsType.Class`, and `TsType.Enum` are
folded into the `Export` branch even when the key is reached via the
structural map.

## Why a separate type

* **No re-walk.** Pattern matching on a `CodeKey` is constant time;
  re-fetching the construct from a map and matching its outer DU is not.
* **Narrowing.** A function that only operates on, e.g., functions can take
  `ExportCodeKey` (or even `ExportCodeKey * ...`) and refuse anything else at
  the type level.
* **Stable identity.** The `TypeKey` payload is preserved on every case, so
  the `CodeKey` can be passed around without losing the address — handy when
  the next step is to actually look the construct up in the map.

## Dispatch tables

A common pattern is to build a frozen dictionary of generators keyed by
case constructor:

```fsharp
let renderType =
    function
    | TypeCodeKey.Union k        -> renderUnion k
    | TypeCodeKey.Intersection k -> renderIntersection k
    | TypeCodeKey.Tuple k        -> renderTuple k
    | TypeCodeKey.Primitive k    -> renderPrimitive k
    // …
```

Because every `TypeCodeKey` / `ExportCodeKey` case is a single `TypeKey`
payload, the matched key flows straight through into the renderer.
