# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Does

**Xantham** is a TypeScript-to-F# bindings generator. It reads `.d.ts` declaration files and outputs F# binding code. The pipeline is:

1. **Extract** (Fable/JavaScript): Crawls TypeScript AST via the TSC API and serializes it as JSON
2. **Transport**: Common JSON schema (defined in `Xantham.Common`)
3. **Decode & Generate** (.NET): Deserializes JSON into typed F# structures, then runs a user-provided generator

## Commands

```bash
# Compile F# to JS (Fable), then run the extractor
npm run prestart        # dotnet fable --cwd src/Xantham.Fable
npm start               # node src/Xantham.Fable/Program.fs.js

# Development watch mode
npm run watch           # Fable watch + node --watch

# Fable extractor tests (Mocha, Fable.Mocha)
npm test                # pretest: dotnet fable -c Debug --cwd tests/Xantham.Fable.Tests -o dist/tests → mocha tests/Xantham.Fable.Tests/dist/tests
npm run watch-test      # Fable watch + mocha --watch

# Signal unit tests (Fable → Node.js, no project file needed)
npm run test:signal     # dotnet fable --noCache tests/Signal.test.fsx && node tests/Signal.test.fs.js
```

## Project Structure

| Project | Purpose |
|---------|---------|
| `Xantham.Common` | Shared discriminated union schema (`Common.Types.fs`) — the contract between extractor and generators. Defines `IOverloadable` marker interface and `TsOverloadableConstruct<'T>` (`NoOverloads of 'T \| Overloaded of 'T Set`) wrapping the five overloadable constructs: `TsFunction`, `TsMethod`, `TsCallSignature`, `TsConstructSignature`, `TsConstructor`. `TsMember.Method/CallSignature/ConstructSignature` and `TsExportDeclaration.Function` carry `TsOverloadableConstruct<_>` payloads. The two primary output DUs are `TsType` (all structural/semantic types: Interface, Class, Primitive, Enum, EnumCase, Union, Intersection, Literal, TypeReference, Array, Tuple, TypeParameter, TypeLiteral, Conditional, etc.) and `TsExportDeclaration` (top-level exported declarations: Variable, Interface, TypeAlias, Class, Enum, Module, Function). `TsAstNode` still exists as a flat catch-all DU mirroring both `TsType` and `TsExportDeclaration` cases but is no longer the primary reader output. The wire format output type is `Schema.EncodedResult` (also in `Common.Types.fs`): `{ ExportedDeclarations: Map<TypeKey, TsExportDeclaration>; Types: Map<TypeKey, TsType>; DuplicateExports: Map<TypeKey, DuplicateEncoding<TsExportDeclaration> list>; DuplicateTypes: Map<TypeKey, DuplicateEncoding<TsType> list>; TopLevelExports: TypeKey list; LibEsExports: TypeKey list }`. |
| `Xantham.Fable` | TypeScript extractor — compiled to JS via Fable, uses TSC API (ts-morph) |
| `Xantham.Fable.Core` | Minimal Fable bindings stub |
| `Xantham.Decoder` | .NET library for decoding the JSON schema and providing path utilities |
| `Xantham.Generator` | Core rendering library — path system, type-ref model, and F# AST helpers for building generators |
| `Xantham.Fable.Tests` | Fable/Mocha integration tests for the extractor pipeline; fixtures in `tests/Xantham.Fable.Tests/TypeFiles/*.d.ts` |

`Xantham.Common/Common.Types.fs` is included directly as a `<Compile>` item in both `Xantham.Fable.fsproj` and `Xantham.Decoder.fsproj` — it is not compiled as a separate assembly.

## Key Dependencies

- **Fable 5.0.0-rc.#** — F# to JavaScript compiler (for `Xantham.Fable`)
- **Thoth.Json 10.4.1** (Fable) / **Thoth.Json.Net 12.0.0** (.NET) — JSON serialization
- **Fabulous.AST 2.0.0-pre06** — AST-based F# code generation
- **Fable.Mocha 2.17.0** — Testing framework to use for any Fable tests
- **Expecto** - Testing framework to use for any dotnet tests

## Development Notes

- All projects target `net10.0`
- Test fixtures are actual `.d.ts` files in `tests/Xantham.Fable.Tests/TypeFiles/`
- `tests/Signal.test.fsx` is a standalone Fable script (no `.fsproj`) that unit-tests `Signal.fs` via `#load`. Fable outputs `tests/Signal.test.fs.js` (strips the `x`); run with `npm run test:signal`. Computed signals start dirty — always read `.Value` at least once before subscribing to `.Invalidated` in tests, otherwise the `if not dirty` guard suppresses the first notification.
