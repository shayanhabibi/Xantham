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

# Signal unit tests (Fable → Node.js, no project file needed)
npm run test:signal     # dotnet fable --noCache Signal.test.fsx && node Signal.test.fs.js

# Build/run the .NET projects
dotnet build            # Build all projects
dotnet run --project src/Xantham.SimpleGenerator

# Run tests
dotnet test src/Xantham.SimpleGenerator.Tests
dotnet run --project src/Xantham.SimpleGenerator.Tests
```

## Project Structure

| Project | Purpose |
|---------|---------|
| `Xantham.Common` | Shared discriminated union schema (`Common.Types.fs`) — the contract between extractor and generators. Defines `IOverloadable` marker interface and `TsOverloadableConstruct<'T>` (`NoOverloads of 'T \| Overloaded of 'T Set`) wrapping the five overloadable constructs: `TsFunction`, `TsMethod`, `TsCallSignature`, `TsConstructSignature`, `TsConstructor`. `TsMember.Method/CallSignature/ConstructSignature` and `TsType.Function` / `TsAstNode.FunctionDeclaration/Method/Constructor/ConstructSignature/CallSignature` all carry `TsOverloadableConstruct<_>` payloads. |
| `Xantham.Fable` | TypeScript extractor — compiled to JS via Fable, uses TSC API (ts-morph) |
| `Xantham.Fable.Core` | Minimal Fable bindings stub |
| `Xantham.Decoder` | .NET library for decoding the JSON schema and providing path utilities |
| `Xantham.SimpleGenerator` | Example generator using Fabulous.AST for F# code output |
| `Xantham.SimpleGenerator.Tests` | Tests using real `.d.ts` fixtures with Expecto |

`Xantham.Common/Common.Types.fs` is included directly as a `<Compile>` item in both `Xantham.Fable.fsproj` and `Xantham.Decoder.fsproj` — it is not compiled as a separate assembly.

## Architecture Details

### Xantham.Fable (Extractor)

- **`Bootstrap.fs`** — Creates the TypeScript compiler program with bundler module resolution
- **`Types.fs`** — Builder record types (`TsXxxBuilder`) with `ref<TypeKey>` slots for deferred resolution, and `TsAstNodeBuilder` DU wrapping them.
- **`Types/Symbol.fs`** — `SymbolTypeKey<'T>`: a zero-overhead erased wrapper around a JS Symbol used as a typed property key. `SymbolTypeKey.create<'T>` allocates a named symbol; two symbols with the same name are distinct. Used throughout as the key mechanism for all tag/tracer property bags.
- **`Types/Tracer.fs`** — Proxied tracer infrastructure. `Tracer<'T>` stores `{ Value: 'T }` on the target JS object under `TRACER_TAG` (O(1), no dictionary). `TRACER_PROXY` stamps the type parameter so `TYPE_Valid`/`TYPE_Invalid` can verify the wrapper at runtime. Key types: `CyclicTracer<'T>` (self-referential tracer), `GuardedTracer<'T,'G>` (tracer + mutable Guard), `KeyedGuardedTracer` (adds Key). `IdentityKey` DU unifies all identity representations: `Id TypeKey | AliasSymbol | Symbol | DeclarationPosition`. `GuardTracer = Tracer<IdentityKey>` stored on TypeScript objects via `TRACER_GUARD`; `module GuardTracer` provides `fromNode`/`fromType`/`fromSymbol` smart constructors and an active pattern for direct match on `IdentityKey` cases. `Tracer.Data` sub-module provides named-slot property bag access on any tracer object.
- **`Types/XanTag.fs`** — Tag system built on `Tracer.fs`. `XanTagKind` classifies any `Ts.Node` or `Ts.Type` into one of its sub-DUs (`TypeDeclaration`, `TypeNode`, `JSDocTags`, etc.). `XanthamTag = GuardedTracer<XanTagKind, GuardTracer>`: the tag object is a tracer stored on the node/type via `TRACER_TAG`; its `Guard` property holds a `GuardTracer` stored on the underlying symbol/type via `TRACER_GUARD`. `TagState<'T> = Visited of 'T | Unvisited of 'T` tracks first-visit status; `.Value` uses `emitJsExpr "$0.fields[0]"` for O(1) unwrap. `XanthamTag.Create` has three SRTP constraints dispatching over `Ts.Node`/`Ts.Type`/`Ts.Symbol` via `GuardTracerSRTPCreator` (guard creation + existence check) and `XanTagKind` (classification). Returns `(TagState<XanthamTag> * TagState<GuardTracer>)`. `XanthamTag` exposes a dual property bag: tag bag (`Get/Set/Has/Clear/GetOrInit`) on the tag object and keyed bag (`KeyedGet/...`) on the Guard object.
- **`Types/GuardedData.fs`** — Named data-slot accessors (`SummaryContent`, `Documentation`, `AstNodeBuilder`, `TypeSignal`) over `XanthamTag`'s property bags. Each module provides `get/has/set/clear/getOrSetWith/getOrMapSet` for the tag bag and an identical `Keyed` sub-module for the guard-partitioned bag.
- **`Types/ModuleMap.fs`** — Resolves file paths to module names by traversing import graphs and falling back to `package.json` / `node_modules` path.
- **`Types/Signal.fs`** — Fable-compatible reactive signal library. `Signal<'a>` covers source signals (`.Set(v)`) and computed signals (explicit deps or auto-tracked via `Signal.auto`). Module-level `collector` enables auto-tracking. `Signal.pending` creates a `Signal<'a voption>` initialised to `ValueNone`; `Signal.fill` sets it imperatively; `Signal.fulfillWith` / `Signal.fulfillWithSome` retrofit an existing source signal with an auto-tracked thunk — useful when the signal was pre-allocated as a placeholder that other signals already depend on.
- **`Types/ReactiveBuilders.fs`** — Signal-based mirror of the builder types. `Signal<TypeKey>` (aliased `TypeSignal`) replaces `ref<TypeKey>`; `Signal<SXxx voption>` replaces `ref<TsXxxBuilder voption>`; docs collapse to `TsComment list`. Each `SXxx` type's `.Build()` returns the final `TsXxx` type directly (no Builder-suffixed intermediate). `STsAstNodeBuilder` DU mirrors `TsAstNodeBuilder`. Active patterns in `Patterns.Builder` (original) and `Patterns.SBuilder` (reactive). `TypeStore` wraps `STsAstNodeBuilder` with reactive projections. Five builder types implement `IOverloadable`: `SMethodBuilder`, `SCallSignatureBuilder`, `SConstructSignatureBuilder`, `SConstructorBuilder`, and the function builder — allowing `STsAstNodeBuilder.Build()` to produce `TsOverloadableConstruct<'T>` wrapped results for those cases.
- **`Read.fs`** — Stack-based traversal engine (replaces tail recursion since JS has no TCO). Stack entries: `TypeNode`, `Declaration`, `TypeKind`, `JsDocTag`, `DelayedFunc`. After traversal, `readAndWrite` runs a three-phase duplicate-resolution pipeline: (1) group results by `TypeKey`; (2) `mergeOverloads` — collects overloadable duplicate entries (`FunctionDeclaration`, `Method`, `Constructor`, `ConstructSignature`, `CallSignature`) and wraps them into `TsOverloadableConstruct<'T>`; (3) `filterConflictDuplicatesOnly` — logs warnings only for genuine node conflicts, silently discards same-builder duplicates.
- **`Reader.fs`** — Reader composition and utilities
- **`Readers/Router.fs`** — Routes AST nodes to the appropriate domain reader
- **`Readers/Prelude.fs`** — Shared reader state: global stack, `typeScriptReader` reference, `RefKey`/`RefMember` helpers for deferred node registration, `TypeStore.Create` helpers, `addToMemory`/`tryGetOrRegisterStore` for the signal cache.
- **`Readers/*.fs`** — 21+ modules, one per TypeScript construct (Alias, Enum, Property, Tuple, UnionNIntersection, Conditional, TypeLiteral, etc.)
- **`Reading/Prelude.fs`** — Higher-level reading helpers: `libCacheMemoryHandler` (adds lib-file types to `libCache` to suppress noise in missing-builder logs), `TypeStore.Create.XanTag`, `addToMemory`, `tagPrimitives`, `tryGetOrRegisterStore`.
- **`Reading/Entry.fs`** — Entry points: `getDeclarations` (enumerates exports/statements as `XanthamTag array`), `setDeclarationsNodeBuilderSignal`, `pushDeclarationsToStack`.
- **`Reading/Dispatcher.fs`** — Dispatches a `XanthamTag` to the appropriate sub-reader based on `XanTagKind`. Pattern: `Ignore` → skip; `tryGetOrRegisterStore` returns `None` (already cached) → skip; otherwise dispatch.
- **`Reading/TypeReference.fs`** — Builds `STypeReferenceBuilder` for both node-path (`fromNode`, via `TypeNode.TypeReference`) and type-path (`fromType`, via `TypeFlagObject.Reference/Instantiated`). **Important**: `fromType` must always build the TypeReference directly — do NOT add a `signalCache.ContainsKey` guard, as the tag was just registered by `tryGetOrRegisterStore` and the check would always be true, creating a self-referential `fulfillWith` that permanently stalls the builder at `ValueNone`. `resolveTypeBase` and `getTypeSymbol` check `signalCache.ContainsKey(guard.Value)` **regardless of the guard's Visited/Unvisited state** — see the shared-symbol hazard note below.

The separation of `TypeNodeKind` (syntactic) vs `TypeKind` (semantic) is important: `Foo<'A>` is a `TypeReference` syntactically but resolves to a `TypeLiteral { bar: 'A' }` after type checking.

#### XanthamTag invariant
`node[TRACER_TAG]` is always a plain imprinted wrapper `{ Value: XanTagKind, [TRACER_PROXY]: "XanTagKind" }` — never the `XanTagKind` DU directly. `tag.Value: XanTagKind`; `tag.Value.Value: U2<Ts.Type, Ts.Node>` (the underlying TS object). The `Guard` property on the tag points to the symbol/type/node object on which the `GuardTracer` is stored under `TRACER_GUARD`.

#### TypeLiteral signature handling
`Ts.Signature.getDeclaration()` can return `CallSignatureDeclaration`, `FunctionTypeNode`, `MethodSignature`, `MethodDeclaration`, `ConstructorTypeNode`, or `ConstructorDeclaration` — not only the explicit signature declaration kinds. `Readers/TypeLiteral.fs` handles all of these. `FunctionTypeNode` and `ConstructorTypeNode` are built inline (they are not `Declaration` nodes and cannot go through `RefMember.fromDeclaration`).

### Xantham.Decoder

- **`Types/Path.fs`** — Strongly-typed path derivation system for contextual type resolution
- **`Core.fs`** — Thoth.Json.Net decoding of the JSON schema
- **`Runtime.fs`** — Decoded tree management

### Xantham.SimpleGenerator (Example Generator)

- `Generator/TypeResolver.fs` coordinates rendering
- `Generator/*.fs` — 27 type-specific renderers
- Uses Fabulous.AST for AST-based F# code generation (as opposed to string concatenation)

## Key Dependencies

- **Fable 5.0.0-rc.3** — F# to JavaScript compiler (for `Xantham.Fable`)
- **ts-morph 27.0.2** / **TypeScript 5.9.3** — TypeScript compiler API access
- **Thoth.Json 10.4.1** (Fable) / **Thoth.Json.Net 12.0.0** (.NET) — JSON serialization
- **Fabulous.AST 2.0.0-pre06** — AST-based F# code generation
- **Expecto** / **FsUnit** — Testing framework for `.Tests` project
- **FsToolkit.ErrorHandling** — Railway-oriented programming utilities

## Development Notes

- All projects target `net10.0`
- The `FABLE_COMPILER` constant is defined in `Xantham.Fable.fsproj` — use `#if FABLE_COMPILER` guards when sharing code between Fable and .NET
- `TypeScript.fs` (the TSC API binding) is auto-generated via ts2fable — do not manually edit it; use `TypeScript.Extensions.fs` for extensions
- The stack-based traversal in `Read.fs` is intentional to handle large `.d.ts` files (like solid-js) without stack overflows
- Test fixtures are actual `.d.ts` files in `src/Xantham.SimpleGenerator.Tests/Tests/*/`
- `Ts.SyntaxKind` values are sequential integers, **not** flags — never use bitwise OR for membership checks; use `set [...].Contains(node.kind)` instead
- `emitJsExpr "$0.fields[0]"` on DU `.Value` members relies on Fable 5's internal layout (single-payload cases store their value at `fields[0]`). Verified against Fable 5.0.0-rc.3.
- `Types/ReactiveBuilders.fs` is compiled after `Types.fs` in the fsproj (it depends on both `Signal.fs` and `Types.fs`)
- `Types/Reader.fs` is compiled after `ReactiveBuilders.fs` per the current fsproj ordering
- `TagState<'T>` (in `XanTag.fs`) is the canonical first-visit indicator — it replaced the old `ColorWrapper` erased union. `Unvisited` on first encounter, `Visited` on repeats; `.Value` is a zero-cost `fields[0]` unwrap. **Guard `TagState` semantics are counter-intuitive**: `XanthamTag.Create` returns `TagState.Visited(guard)` when the guard was *freshly created* (first time that symbol/type was seen), and `TagState.Unvisited(guard)` when the guard *pre-existed* on the object. This is the opposite of what the name suggests for the container — `seenGuard=true` (guard existed before) → `TagState.Unvisited`; `seenGuard=false` (guard is new) → `TagState.Visited`.
- `Signal.test.fsx` at the repo root is a standalone Fable script (no `.fsproj`) that unit-tests `Signal.fs` via `#load`. Fable outputs `Signal.test.fs.js` (strips the `x`); run with `npm run test:signal`. Computed signals start dirty — always read `.Value` at least once before subscribing to `.Invalidated` in tests, otherwise the `if not dirty` guard suppresses the first notification.
- **`Signal.fulfillWith` self-reference hazard**: `FulfillWith` runs the thunk immediately. If `signal.FulfillWith(fun () -> signal.Value)` is called (the signal subscribes to its own value), the thunk reads `ValueNone`, sets the signal to `ValueNone` (no-op), and subscribes to `signal.Invalidated` — but nothing ever fires it. The signal is permanently stalled. This can happen accidentally when `store.Builder` and `GuardedData.AstNodeBuilder.getOrSetDefault tag` refer to the same object (which they always do when the TypeStore was created from this tag). Always ensure `fulfillWith` thunks read from a *different* signal than the one being fulfilled.
- **`Read.fs` missing-builder diagnostics**: The final loop in `read` logs `Missing builder for: <identity>` for any signalCache entry whose builder is still `ValueNone`. Entries in `libCache` (TypeScript built-in lib types) are suppressed. The ~4 remaining `Id N` entries (anonymous types with no symbol/declaration from complex generics) are expected and do not affect output correctness — they are skipped, not emitted as JSON.
- **`isFromEs5Lib` / `libCacheMemoryHandler`**: `isFromEs5Lib` matches all TypeScript built-in lib files via `fn.Contains("/lib/lib.") && fn.EndsWith(".d.ts")`, not just `lib.es5.d.ts`. `libCacheMemoryHandler` handles `IdentityKey.Symbol`, `IdentityKey.AliasSymbol`, and `IdentityKey.DeclarationPosition` cases. It is called from `tryGetOrRegisterStore` for every newly registered entry (not just from `addToMemory`).
- **Shared-symbol hazard in `resolveTypeBase` / `getTypeSymbol`**: Two distinct `Ts.Type` objects can share the same underlying symbol (e.g. `Array<string>` and `Array<T>` both resolve to the `Array` symbol). `GuardTracer.fromType` stores the guard on the *symbol*, so the second type's `CreateXanthamTag` returns `TagState.Unvisited` for the guard (pre-existing). The original match `| _, TagState.Visited ident when signalCache.ContainsKey(ident.Value)` missed this case — since the guard is Unvisited, the check failed and the target type was incorrectly pushed to the stack. When dispatched, `tryGetOrRegisterStore` found the shared IdentityKey entry (registered for the first type) and wrote that type's Key into the target's TypeSignal, creating a self-referential TypeReference cycle. **Fix**: always check `signalCache.ContainsKey(guard.Value)` regardless of Visited/Unvisited; when the cached key equals the outer type's own TypeKey, return `TypeSignal.ofKey(target.TypeKey)` to point to the generic declaration instead.
- **`findUnknownFields` false positives**: The `unknownKey` sentinel in `Read.fs` (`TypeKindPrimitive.Unknown.TypeKey`) is also a legitimate type value — any entry whose type field IS the `unknown` TS primitive will trigger "Unknown TypeKey in fields". This produces false-positive warnings for `unknown[]` element types and `() => unknown` call-signature return types. These are not real errors.
- **Overload merging pipeline**: After all signals resolve, `readAndWrite` runs `mergeOverloads` before `resolveDuplicates`. It groups duplicate-key entries by overloadability (the five `IOverloadable` types), wraps them into `TsOverloadableConstruct.Create` (producing `NoOverloads` or `Overloaded Set`), and replaces the winner entry. Identity priority ordering is `DeclarationPosition > Symbol > AliasSymbol > Id`. Non-overloadable duplicates fall through to standard single-winner conflict resolution. `healthCheckNode` and `findUnknownFields` both iterate overloads via `.Values` to cover all variants. Consumer code (Decoder, SimpleGenerator) uses `.Values` / `.ToList()` / `.ToSeq()` to process all overloads, or `.ValueOrHead` for backward-compatible single-value access.

## XanTagKind Dispatch: Case Priority & Rationale

This section documents the cost/benefit decision for each `XanTagKind` dispatch case. Cases removed from `XanTagKind` are noted separately.

### TypeDeclaration cases

| Case | Priority | Rationale |
|------|----------|-----------|
| `Interface` | Critical | Primary structural type — emits `TsInterface` with properties, methods, call/construct/index signatures, heritage, type params |
| `TypeAlias` | Critical | The other top-level structural type — emits `TsTypeAlias`; needed for union, intersection, mapped, conditional, template literal aliases |
| `FunctionDeclaration` | Critical | Top-level functions are first-class exports |
| `VariableStatement` | Critical | `const`/`let` declarations are frequent export forms, especially for class-like values |
| `PropertySignature` | Critical | Members of interfaces/type literals — most common child node |
| `MethodSignature` | Critical | Method members of interfaces — required for overload support |
| `Parameter` | Critical | Required by every function, method, constructor, and call signature |
| `Enum` | High | Enums appear frequently in TypeScript APIs; emit `TsEnum` |
| `TypeParameter` | High | Generic type params on interfaces, aliases, functions, classes — required for correct generic bindings |
| `HeritageClause` | High | `extends`/`implements` — needed for inheritance chains (`Array<T>`, `EventTarget`, etc.) |
| `Class` | High | Class declarations appear as values (constructor + prototype); emit `TsClass` |
| `Constructor` | High | Constructor signatures on classes |
| `CallSignature` | High | `{ (x: T): R }` — call signatures on interfaces/type literals |
| `ConstructSignature` | High | `{ new(x: T): R }` — construct signatures (factory patterns) |
| `IndexSignature` | High | `{ [key: string]: T }` — index signatures |
| `Module` / `Namespace` | High | `declare module "..."` / `namespace Foo` — ambient modules and namespaces are the primary grouping mechanism in `.d.ts` files |
| `GetAccessor` | Medium | `get foo(): T` — read-only computed property; can be approximated as a property if deferred |
| `SetAccessor` | Medium | `set foo(v: T)` — write-only property; less common in `.d.ts` files |
| `Property` | Medium | Class instance/static properties (distinct from `PropertySignature` which is interface-only) |
| `Method` | Medium | Class method declarations (distinct from `MethodSignature`) |
| `NamespaceExportDeclaration` | Ignore | `export as namespace Foo` — UMD global marker; carries zero type information; relevant only if emitting global bindings (deferred to a later phase) |

### TypeFlagObject cases

| Case | Priority | Rationale |
|------|----------|-----------|
| `Anonymous` | High | Anonymous object types are extremely common (e.g., inline `{ x: number }` type literals) |
| `Interface` | High | Named interface types resolved from the checker — distinct from `TypeDeclaration.Interface` (syntactic); this is the semantic object |
| `Class` | High | Class static-side and instance-side types from the checker |
| `Mapped` | Medium-High | Mapped types (`{ [K in keyof T]: T[K] }`) — complex but appear in many utility types |
| `Tuple` | Medium-High | Typed tuples `[A, B, C]` — common in function return types and rest parameters |
| `Instantiated` | Medium | Generic type instantiation result — may surface as a distinct type when the checker resolves an applied generic |
| `Reference` | Implemented | Wired to `TypeReference.fromType`; handles all `ObjectFlags.Reference` types. If `target.TypeKey == typeReference.TypeKey` (uninstantiated bare generic), forwards to the symbol declaration instead to avoid a self-referential TypeReference entry. |
| `EvolvingArray` | Ignore | Internal checker type for narrowing `let arr = []` — never appears in `.d.ts` declaration files |

### Removed XanTagKind cases

These DU types still exist but are no longer cases of `XanTagKind` (so they are never dispatched):

**`ModulesAndExports`** — All cases intentionally ignored:
- Import nodes (`ImportDeclaration`, `ImportClause`, etc.): zero information loss — the checker resolves imports transparently via `getSymbolAtLocation` / `getTypeAtLocation`.
- Export nodes (`ExportDeclaration`, `ExportSpecifier`, etc.): zero information loss — `Entry.getDeclarations` uses `checker.getExportsOfModule` which follows all export chains recursively and returns the original declaration symbols.
- `NamespaceExport` (`export as namespace Foo`): UMD global name metadata only; no type structure.

**`IdentifierNameNode`** — All cases dead code in practice:
- `Identifier`, `QualifiedName`, `ComputedPropertyName`: always accessed inline as children of their parent nodes (e.g., a `PropertySignature` reads `.name` directly). Pushing them to the stack independently adds cost with no benefit.
- `ComputedPropertyName` specifically should be handled inline when reading property/method names — not via a separate dispatch.

**`Modifiers`** — Entire case is dead code:
- All modifiers (`public`, `private`, `protected`, `readonly`, `abstract`, `static`, `optional`, etc.) are read directly from the parent node's `modifiers[]` array during that node's dispatch. There is no scenario where a modifier node needs its own stack frame or signal.