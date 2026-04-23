# AGENTS.md

## Project Overview
Xantham is a TypeScript-to-F# bindings generator that separates concerns into three phases:
1. Extract (Fable/JavaScript) - Crawls TypeScript AST and serializes as JSON
2. Transport - Common JSON schema 
3. Decode & Generate (.NET) - Deserializes JSON into F# structures and runs generators

## Key Commands
- `npm run prestart` - Compile Fable extractor
- `npm start` - Run extractor against .d.ts file 
- `npm run watch` - Development watch mode (Fable + Node)
- `npm run test:signal` - Run standalone Fable tests
- `dotnet build` - Build .NET projects
- `dotnet test` - Run .NET tests

## Project Structure
- `Xantham.Fable` - TypeScript extractor (Fable/F# to JS)
- `Xantham.Decoder` - .NET library for decoding JSON schema
- `Xantham.Generator` - Core rendering library
- `Xantham.Common` - Shared schema definitions (included directly, not compiled separately)
- `tests/Xantham.Fable.Tests` - Fable/Mocha integration tests with .d.ts fixtures

## Development Workflow
1. Extract: `npm run prestart && npm start <path/to/index.d.ts>`
2. Decode & Generate: `dotnet run --project src/Xantham.SimpleGenerator`
3. Watch mode: `npm run watch` for development

## Important Details
- All projects target `net10.0`
- Test fixtures are actual `.d.ts` files in `tests/Xantham.Fable.Tests/TypeFiles/`
- `Xantham.Common/Common.Types.fs` is included directly in both Fable and Decoder projects
- Generated code uses Thoth.Json for serialization
- Fable 5.0.0-rc.# is used for F# to JS compilation
- Fable.Mocha is used for Fable tests, Expecto for .NET tests
- Computed signals start dirty - always read `.Value` before subscribing to `.Invalidated` in tests

## Key Files
- `src/Xantham.Fable/Program.fs.js` - Entry point for extractor
- `src/Xantham.Common/Common.Types.fs` - Core schema definitions
- `src/Xantham.SimpleGenerator/Program.fs` - Example generator implementation
- `tests/Signal.test.fsx` - Standalone Fable test script

## Architecture Notes
- Schema is the single hand-off point between extractor and generators
- Generators consume `Xantham.Decoder` and never touch the extraction pipeline
- Overload support added via `IOverloadable` / `TsOverloadableConstruct<'T>`
- Stack-based traversal avoids JS stack overflows