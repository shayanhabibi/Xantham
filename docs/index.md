# Xantham

> A schema-driven TypeScript → F# bindings generator.

Xantham is a hard fork of [Glutinum](https://github.com/glutinum-org/cli) that tackles the TypeScript-to-.NET bindings problem differently. Instead of a single end-to-end pipeline it separates concerns into **extract**, **encode/decode**, and **generate** phases across Fable and .NET boundaries. The common JSON schema is the only hand-off point; generators consume `Xantham.Decoder` and never touch the extraction layer.

---

## Glutinum vs Xantham

Both tools read `.d.ts` files and emit F# bindings. They make different trade-offs.

| | Glutinum | Xantham |
|---|---|---|
| **Try it now** | [Web playground](https://glutinum.net) — browser, no install | CLI only — requires Node.js + .NET SDK |
| **Setup** | Zero | Moderate |
| **Input scope** | One `.d.ts` entry point | Entire type graph, recursively across all referenced files |
| **Import statements** | Not generated — wired by hand | Generated automatically; provenance is tracked through the full type tree |
| **Output style** | Single built-in format | User-defined — bring your own strategy (string concat, Fabulous.AST, SyntaxOak, …) |
| **Encoder replaceability** | N/A — monolithic | TSC can be swapped (e.g. TSGO) without touching decoders or generators |
| **Decoder reuse** | N/A | Any consumer takes a dependency on `Xantham.Decoder` independently |
| **Complexity** | Low | Higher — three distinct phases |

**Choose Glutinum** when you need a quick answer for a single file and want zero friction.

**Choose Xantham** when you need the full picture: multi-file type graphs, generated imports, and control over how the output looks.

---

## How It Works

### Glutinum

<div class="mermaid">
flowchart LR
    A[.d.ts Input] --> B[TypeScript Compiler API]
    subgraph Fable / Glutinum
        B --> C[GlueAST]
        C --> D[FSharpAST]
        D --> E[String Concatenation]
    end
    E --> F[F# Bindings]
</div>

### Xantham

<div class="mermaid">
flowchart LR
    input[".d.ts entry point"] --> tsc
    subgraph Fable / Xantham.Fable
        tsc["TypeScript Compiler API"]
        crawler["Recursive AST Crawler"]
        thoth_enc["Thoth Encoder"]
        tsc --> crawler
        crawler --> thoth_enc
    end
    subgraph Common
        schema["JSON Schema"]
    end
    subgraph .NET / Xantham.Decoder
        thoth_dec["Thoth Decoder"]
        util["Utility Layer"]
        thoth_dec --> util
    end
    subgraph Userland Generators
        gen1["Xantham.SimpleGenerator — Fabulous.AST"]
        gen2["Your Generator — String Concat / SyntaxOak / …"]
    end
    schema -.-> thoth_enc
    schema -.-> thoth_dec
    thoth_enc --> json["JSON"]
    json --> thoth_dec
    util --> types["F# Types"]
    types --> gen1
    types --> gen2
    gen1 --> out["F# Bindings"]
    gen2 --> out
</div>

Xantham's extractor follows every type reference recursively. By the time the decoder receives the schema it has the complete type graph — all the information needed to resolve cross-file references and emit accurate `open` / import statements.

---

## Modules

| Module | Role |
|--------|------|
| **Xantham.Common** | Shared discriminated union schema (`Common.Types.fs`) — the contract between extractor and generators. Included directly as a source file in both `Xantham.Fable` and `Xantham.Decoder`; not a separate assembly. |
| **Xantham.Fable** | TypeScript extractor compiled to JS via Fable. Crawls `.d.ts` files via ts-morph, merges overloads by `TypeKey`, and emits JSON conforming to the common schema. |
| **Xantham.Fable.Core** | Minimal Fable bindings stub. Provides F# representations of TypeScript type-system idioms (`keyof`, indexed access types, heterogeneous property unions). |
| **Xantham.Decoder** | .NET library. Decodes the JSON schema into strongly-typed F# structures and provides a utility layer for generator consumption. |
| **Xantham.Generator** | Core rendering library — path system, type-ref model, and F# AST helpers. |

---

## Current Status

| Component                           |     Status     | Notes                                                                                       |
|-------------------------------------|:--------------:|---------------------------------------------------------------------------------------------|
| **Schema** (`Xantham.Common`)       |     🟡 RC      | Near-stable; breaking changes require coordinated updates across all layers.                |
| **Reader** (`Xantham.Fable`)        |     🟡 RC      | Handles large hierarchies (e.g. solid-js). Stack-based traversal avoids JS stack overflows. |
| **Decoder** (`Xantham.Decoder`)     |     🟡 RC      | Review documentation and testing.                                                           |
| **Generator** (`Xantham.Generator`) | 🟠 In progress | Structural output works.                                                                    |
