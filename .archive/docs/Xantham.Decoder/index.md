---
title: Xantham.Decoder
category: Xantham.Decoder
categoryindex: 1
index: 0
---

# Xantham.Decoder

`Xantham.Decoder` is the .NET-side counterpart of `Xantham.Fable`. It reads the
JSON wire format produced by the extractor and materialises it into strongly
typed F# structures suitable for downstream generators.

The decoder is intentionally thin: it does not generate F# code itself.
Its job is to:

1. Deserialize the JSON IR (`Schema.EncodedResult`) using `Thoth.Json.Net`.
2. Apply optional compression and sanitization passes over the type/export
   maps.
3. Produce a `DecodedResult` of frozen, key-addressable maps.
4. Optionally lift those maps into an in-memory object graph (`ArenaInterner`)
   in which `TypeKey` references become `Lazy<ResolvedType>` values, breaking
   cycles at the lazy boundary.

## Pipeline at a glance

    JSON file
       │
       │  Decoder.read / Decoder.readWithSettings
       ▼
    DecodedResult                ─── frozen TypeMap / ExportTypeMap / ExportMap
       │
       │  XanthamTree(settings)
       ▼
    XanthamTree                  ─── lookup-friendly façade for generators
       │
       │  tree.GetArenaInterner()
       ▼
    ArenaInterner / ResolvedType ─── lazy object graph, identity-preserving

## Page index

* [Runtime](runtime.html) — `XanthamTree`, the main façade for generators.
* [Decoder](decoder.html) — JSON read pipeline, settings, health check.
* [Names &amp; Casing](names.html) — `Name`, units-of-measure for casing,
  normalization helpers.
* [Code Keys](code-keys.html) — `CodeKey` / `TypeCodeKey` / `ExportCodeKey`
  tagged dispatch over `TypeKey`s.
* [Dependency Graph](graph.html) — `Graph.create` and the dependency view.
* [ArenaInterner](arena-interner.html) — lazy resolved object graph.

## API reference

The full XML-doc generated reference is available under the
[Reference](../reference/index.html) section, organised by the same `category`
groups that appear in this guide.
