# Xantham

[![NuGet](https://img.shields.io/nuget/v/Xantham.TypeScript.Wire?label=nuget%20Xantham.TypeScript.Wire)](https://www.nuget.org/packages/Xantham.TypeScript.Wire)

> A TypeScript → F# bindings generator, built on the TypeScript 7 compiler's own API server.

Xantham is a hard fork of [Glutinum](https://github.com/glutinum-org/cli). It is mid-rebuild.

The whole project now sits on **`Xantham.TypeScript.Wire`** — a .NET client that runs the Go `tsc`
binary as `tsc --api`, speaks its msgpack protocol over stdio, and reads the binary AST it returns
in place, without JSON in the middle. The compiler's own schema generates the API surface, the
kinds, the child slots and the typed node layer, so the client tracks upstream rather than
paraphrasing it.

The earlier design — a Fable extractor crawling the TypeScript 5 JavaScript compiler API, a common
JSON schema as the hand-off point, and a .NET decoder plus generator — has been retired. See
[`.archive/`](.archive/README.md) below.

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

### Documentation

- [Navigating the AST](docs/wire-navigation.md) — tags, `Node<'Tag>`, views and the escape hatches.
- [The hand-written register](docs/wire-hand-written.md) — every fact transcribed from upstream
  rather than derived from its schema, and how to update each one.
- [The wire protocol](docs/plans/tsgo-protocol.md) — framing, error model and the binary AST
  format, verified against live byte traces.
- [Remaining work](docs/plans/wire-remaining-work.md) — what is still outstanding, in phases.
- [Generator architecture](docs/plans/generator-architecture.md) — the nano-pass pipeline, its
  decisions, and what each phase landed.
- [Type mapping](docs/plans/generator-type-mapping.md) — how each TypeScript construct becomes
  F#, and what it costs when it cannot.

---

## Repository layout

| Path | Role |
|------|------|
| `src/Xantham.TypeScript.Wire` | The client. Published to NuGet, usable on its own. |
| `src/Xantham.Generator` | The bindings generator: Harvest → Resolve → Shape → Render over Wire. |
| `src/Xantham.Fable.Core` | The support library generated bindings open (erased `keyof`, brands). |
| `tests/Xantham.TypeScript.Wire.Tests` | Expecto suite against the root pinned `typescript` 7.x package. |
| `tests/Xantham.Generator.Tests` | Expecto suite plus the golden corpus the generator is pinned against. |
| `tests/Xantham.Generator.CompileGate` | Compiles the committed goldens as F# on every build. |
| `tools/tsc-ast` | Vendors upstream compiler sources and emits the AST and enum F# layers. |
| `tools/proto-gen` | Emits the protocol F# layers from the shipped `typescript` schema. |
| `tools/session-gen` | Emits the session layer over the protocol surface. |
| `tools/browser-gen` | Emits the generator's DOM binding table from the `Fable.Browser.*` family. |
| `build.fsx` | The build pipeline. |
| `.archive/` | **Obsolete pre-Wire work. Nothing in here is live.** |

## `.archive/`

Everything under [`.archive/`](.archive/README.md) is retired: `Xantham.Common`, `Xantham.Fable`,
`Xantham.Decoder`, `Xantham.Generator` and their tests and docs, the superseded plans, and the old
build system. It is kept as a record of obstacles already met, not as a source of truth — it does
not build, is not referenced by the solution, and should not be read as a description of how
Xantham works today.

---

## Current Status

| Component | Status | Notes |
|-----------|:------:|-------|
| **Wire** (`Xantham.TypeScript.Wire`) | 🟢 Shipped | Generated from the compiler's own schema; packaged for NuGet. |
| **Generator** (`Xantham.Generator`) | 🟡 In progress | Phases A–C landed; phase D (erased idioms) is most of the way through. Not yet packaged. |
| **Support** (`Xantham.Fable.Core`) | 🟡 In progress | Erased `keyof`/`typekeyof` and brand helpers, revived from the archive. Not yet packaged. |

Generated bindings target **Fable 5.x only**, and depend on `Fable.Core` plus the
`Fable.Browser.*` family. Every committed golden is compiled against those packages on each
build, so a binding that does not compile fails the build rather than a review.

The generator's progress is tracked as a ladder of real npm packages (`ansi-regex`, `animejs`,
`@cloudflare/workers-types`, ...), each pinned by version, generated into a committed golden,
and accompanied by a `manifest.json` grading every symbol `Exact`, `Ergonomic`, `Widened` or
`Escape`. See [the architecture plan](docs/plans/generator-architecture.md) for where each
phase stands.

---

## See the Docs

[Docs are generated from the source.](https://shayanhabibi.github.io/Xantham)
