(**
---
title: Decoder
category: Xantham.Decoder
categoryindex: 1
index: 2
---

# `Decoder` — JSON to `DecodedResult`

`Decoder` is the layer below `XanthamTree`. It is responsible only for
reading a Xantham JSON file off disk and applying the standard compression
and sanitization passes before producing a `DecodedResult`.

You usually don't need to call into `Decoder` directly — `XanthamTree` does it
for you — but it's exposed for tooling and tests.

## The simple path
*)

(*** condition: prepare ***)
#I "../../src/Xantham.Decoder/bin/Release/net10.0"
#r "Xantham.Decoder.dll"
open Xantham.Decoder
(** *)

let result = Decoder.read "./schema.json"

match result with
| Ok decoded ->
    printfn
        "Loaded %i types and %i exported declarations."
        decoded.TypeMap.Count
        decoded.ExportTypeMap.Count
| Error e -> eprintfn "Decode failed: %s" e

(**
`Decoder.read` is a proxy that constructs a default `Settings` and calls
`readWithSettings`.

## Settings

`Decoder.Settings` controls the three optional passes the decoder runs:

| Field | Default | Effect |
|-------|---------|--------|
| `InputFile` | (required) | Path to the `.json` file produced by the extractor. |
| `PerformHealthCheck` | `true` in DEBUG, `false` otherwise | Walks the type map, verifies that every referenced `TypeKey` is reachable, and prints a report. |
| `Compress` | `true` | Collapses redundant entries in the type/export maps. |
| `Sanitize` | `true` | Replaces cyclic `TypeKey`s with `obj` so generators don't need to detect cycles themselves. |

Construct settings with the `Create` static method:
*)

let settings =
    Decoder.Settings.Create(
        inputFile = "./schema.json",
        performHealthCheck = true,
        compress = true,
        sanitize = false)

let decoded = Decoder.readWithSettings settings

(**
## `DecodedResult`

The shape returned on success:

* `TypeMap : Map<TypeKey, TsType>` — every structural type in the input.
* `ExportTypeMap : Map<TypeKey, TsExportDeclaration>` — every exported
  declaration.
* `ExportMap : Map<string, Set<TypeKey>>` — exports grouped by source module
  path.
* `TopLevelExports : TypeKey list` — exports declared at the top level of the
  input.
* `LibEsExports : TypeKey list` — exports that originate from the TS standard
  library files.

The wire format itself (`Schema.EncodedResult`) lives in `Xantham.Common`.

## Health check

When enabled, the health check answers the question *"are all referenced
`TypeKey`s present in the type map?"*. It distinguishes:

* **Missing keys** — referenced but absent from `TypeMap`.
* **Found-in-node-store** — missing from `TypeMap` but present as an export
  declaration. Generation is still safe in this case.
* **Unemitted keys** — missing from both. Generation against this dataset is
  likely to fail.

The report is printed to stdout. The status emojis (`✔️` / `❌`) are part of
the printed output, not the return value.
*)
