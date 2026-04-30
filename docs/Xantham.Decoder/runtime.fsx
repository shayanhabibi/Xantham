(**
---
title: Runtime
category: Xantham.Decoder
categoryindex: 2
index: 1
---

# `XanthamTree` — the runtime façade

`XanthamTree` is the entry point most generators want to start from. It owns
the file-load, decode, freeze, and (lazily) the arena interning steps, and
exposes them through a small set of frozen lookups.

## Constructing a tree

The simplest case takes a path to a Xantham JSON file:
*)

(*** condition: prepare ***)
#I "../../src/Xantham.Decoder/bin/Release/net10.0"
#r "Xantham.Decoder.dll"
open Xantham.Decoder
(** *)

let tree = Runtime.create "./schema.json"

(**
For more control over decoder behaviour (compression, sanitization, health
check), use `createWith` and adjust the `Decoder.Settings`:
*)

let custom =
    "./schema.json"
    |> Runtime.createWith (fun s ->
        { s with
            Decoder =
                { s.Decoder with
                    Compress = false
                    Sanitize = true
                    PerformHealthCheck = true } })

(**
`Runtime.Settings.Create` and `Runtime.Settings.Init` are the two underlying
factory methods — `Init` produces defaults; `Create` lets you transform them.

## What's on the tree

Each member is a `FrozenDictionary` or `FrozenSet`, so lookups are allocation-
free and thread-safe.

| Member | Purpose |
|--------|---------|
| `tree.TypeMap` | Every structural `TsType` decoded from the input, keyed by `TypeKey`. |
| `tree.KeyExportMap` | Every `TsExportDeclaration`, keyed by `TypeKey`. |
| `tree.ExportMap` | All declarations, grouped by their source module path. |
| `tree.SignatureFunctionMap` | Look up a function export by any of its overloaded signature keys. |
| `tree.LibEsExports` | The set of `TypeKey`s belonging to the TS standard library (`lib.es*.d.ts`). |
| `tree.IsLibEsExport` | Predicate convenience for `LibEsExports`. |

The deprecated `tree.TopLevelExports` is preserved for backward compatibility;
prefer `ExportMap` keyed by your source module.

## Coding a key

`tree.Codify key` takes any `TypeKey` and returns a `CodeKey` that tags the
key with the kind of construct it identifies:
*)

let codeKey = tree.Codify (Xantham.TypeKey.createWith 59)
match codeKey with
| Xantham.Decoder.CodeKey.CodeKey.Export exp ->
    printfn "exported declaration: %A" exp
| Xantham.Decoder.CodeKey.CodeKey.Type typ ->
    printfn "structural type: %A" typ

(**
This avoids re-walking the underlying `TsType` / `TsExportDeclaration` every
time a generator needs to dispatch on category. See
[Code Keys](code-keys.html) for the full taxonomy.

## Crossing into the resolved graph

When a generator wants to walk types as a graph rather than as flat maps, ask
the tree for its arena interner. The first call materialises the entire export
shell; subsequent calls return the cached value:
*)

let interner = tree.GetArenaInterner()

(**
See [ArenaInterner](arena-interner.html) for what the resolved graph looks
like and when to prefer it over raw maps.

## Building a dependency graph

`tree.GetDependencyGraph()` returns a `Graph` over the type/export maps with
conditional `Check`/`Extends` branches excluded from the edge set. See
[Dependency Graph](graph.html).
*)

let graph = tree.GetDependencyGraph()
