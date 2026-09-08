# Xantham

> A TypeScript → F# bindings generator, built on the TypeScript 7 compiler's own API server.


> [!NOTE]
> For consuming the cli tool [get started here.](https://shayanhabibi.github.io/Xantham/xantham-cli/)

The whole project now sits on **`Xantham.TypeScript.Wire`** — a .NET client that runs the Go `tsc`
binary as `tsc --api`, speaks its msgpack protocol over stdio, and reads the binary AST it returns
in place, without JSON in the middle. The compiler's own schema generates the API surface, the
kinds, the child slots and the typed node layer, so the client tracks upstream rather than
paraphrasing it.

---

## Xantham.TypeScript.Wire

A standalone NuGet package, usable on its own with no dependency on the rest of Xantham.

- **The full API surface**, generated from the compiler's shipped schema: 142 synchronous calls,
  the same set again as `Async`, and typed records for every parameter and response.
- **The binary AST, read in place.** A node is a struct over the blob and an index, not an object
  graph, and the typed layer gives each one a tag — `Node<FunctionDeclaration>` — so narrowing is a
  compile-time question.
- **A batching mailbox** that collects overlapping calls into one `batchRequests` round trip.
- **A virtual filesystem**, so the compiler can be pointed at sources that exist only in memory.

Targets `net10.0`, `net8.0` and `netstandard2.1`. See the
[package README](src/Xantham.TypeScript.Wire/README.md) to get started.

---

## Repository layout

| Path                                  | Role                                                                       |
|---------------------------------------|----------------------------------------------------------------------------|
| `src/Xantham.TypeScript.Wire`         | The client. Published to NuGet, usable on its own.                         |
| `src/Xantham.Generator`               | The bindings generator: Harvest → Resolve → Shape → Render over Wire.      |
| `src/Xantham.Fable.Core`              | The support library generated bindings open (erased `keyof`, brands).      |
| `src/Xantham.Cli`                | The cli tool that ships the generator for consumption.                     |
| `tests/Xantham.TypeScript.Wire.Tests` | Expecto suite against the root pinned `typescript` 7.x package.            |
| `tests/Xantham.Generator.Tests`       | Expecto suite plus the golden corpus the generator is pinned against.      |
| `tests/Xantham.Generator.CompileGate` | Compiles the committed goldens as F# on every build.                       |
| `tools/tsc-ast`                       | Vendors upstream compiler sources and emits the AST and enum F# layers.    |
| `tools/proto-gen`                     | Emits the protocol F# layers from the shipped `typescript` schema.         |
| `tools/session-gen`                   | Emits the session layer over the protocol surface.                         |
| `tools/browser-gen`                   | Emits the generator's DOM binding table from the `Fable.Browser.*` family. |
| `build.fsx`                           | The build pipeline.                                                        |

---

## Current Status

| Component | Status | Notes                                                                                                                        |
|-----------|:------:|------------------------------------------------------------------------------------------------------------------------------|
| **Wire** (`Xantham.TypeScript.Wire`) | 🟢 Shipped | Generated from the compiler's own schema; packaged for NuGet.                                                                |
| **Generator** (`Xantham.Generator`) | 🟡 Alpha | Ships as the `Xantham.Cli` dotnet tool at `0.1.0-alpha.1`.                                                                   |
| **Support** (`Xantham.Fable.Core`) | 🟡 Alpha | Erased `keyof`/`typekeyof` and brand helpers, revived from the archive. Packaged as `Xantham.Fable.Core` at `0.1.0-alpha.1`. |
| **cli** (`Xantham.Cli`) | 🟡 Alpha | The `Xantham.Cli` dotnet tool at `0.1.0-alpha.1`.                                                                            |

Generated bindings target **Fable 5.x only**, and depend on `Fable.Core` plus the
`Fable.Browser.*` family. Every committed golden is compiled against those packages on each
build, so a binding that does not compile fails the build rather than a review.

The generator's progress is tracked as a ladder of real npm packages (`ansi-regex`, `animejs`,
`@cloudflare/workers-types`, ...), each pinned by version, generated into a committed golden,
and accompanied by a `manifest.json` grading every symbol `Exact`, `Ergonomic`, `Widened` or
`Escape`. 
---

## See the Docs

[Docs are generated from the source.](https://shayanhabibi.github.io/Xantham)
