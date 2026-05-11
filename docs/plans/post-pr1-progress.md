# Post-PR1 Progress Report — verify-cloudflare-sdk-pipeline

> Handover note for Shayan. Covers work on `speakez-xantham` and the
> consuming driver `Fidelity.CloudEdge/generators/xantham` since PR #1
> (`speakeztech/source-refactor`) merged at commit `791ef9a`.

- **Branch:** `verify-cloudflare-sdk-pipeline` (HEAD `dc7a650`)
- **Window:** 2026-05-11, post-merge of PR #1
- **Status:** all 178 generator tests pass; 13 decoder Identifier tests pass

## Why this work exists

The acid test for Xantham's structural correctness is generating the full
Cloudflare TypeScript SDK and getting it to compile cleanly through F#.
We exercise three packages simultaneously through the
Fidelity.CloudEdge driver:

- `@cloudflare/dynamic-workflows`
- `@cloudflare/workers-types`
- `agents`

"Closeout" means all three at zero errors at the same time. A per-SDK
zero is a snapshot, not a finish line.

## Error count progression

Real, uncapped error counts (build with `/p:OtherFlags="--maxerrors:10000"`
to defeat the F# compiler's default `--maxerrors:200` truncation).

| SDK | Pre-PR1 baseline | Current | Δ |
|---|---:|---:|---:|
| dynamic-workflows | 18 | 6 | −12 |
| workers-types | 1,376 | 408 | −968 |
| agents | 3,932 | 1,498 | −2,434 |
| **Total** | **5,326** | **1,912** | **−3,414 (−64%)** |

The dominant remaining error code is FS0039 ("type or namespace not
defined"); FS0033, FS0001, FS0663, FS0698, FS0887 round out the long tail.

## What landed in `speakez-xantham` since PR #1 (6 commits)

All on `verify-cloudflare-sdk-pipeline`. Listed in commit order.

### `6121aa6` — generator: migrate to source-attribution DU

Generator side of the source-attribution model PR #1 introduced. The
generator was still reading the old `Source : QualifiedNamePart option`
in some hot paths; this commit consumes the new `Source` DU
(`LibEs of fileName | PackageInternal of LazySubModule | Package of
ResolvedExportCollection`) throughout `Render.fs`, `RenderScope.*`,
`SyntheticPathAssignment.fs`, and `TypeRefRender.Paths.fs`.

Eleven files touched (193 insertions, 113 deletions). Tests for mocking
helpers were updated to construct sources via the DU.

This unblocked everything downstream — most subsequent fixes pattern-match
`Source` to decide whether substitution applies.

### `22b570b` — preserve typar args in heritage / generic-interface TypeReferences

Bug in `RenderScope.Prelude.fs`: when a `TypeReference` appeared as a
heritage target or as a generic interface application, its `TypeArguments`
were dropped. The generated F# rendered `inherit Foo` instead of
`inherit Foo<'T>`, producing cascading FS0033/FS0663 errors at the use
sites of the heritage type.

Fix preserves the typar arguments through the prerender path so the
emitted heritage carries the right generic arity.

### `7983be3` — prune unused typars from TypeAlias.Alias declarations

When the cycle-break pass collapses `type X<'T> = U2<X, A>` to
`type X<'T> = obj`, the typar `'T` remains declared but unused — F# then
rejects with FS0035 ("type abbreviation has unused declared typars").

Fix walks the resolved alias body and filters declared typars to only
those actually appearing as `Intrinsic` atoms with a leading quote. Atom-
walking only — earlier resolved-graph walking attempts regressed by
preserving typars cycle-break should have eliminated.

### `e41629b` — add lib.dom/lib.es substitutions to obj for Fable-unavailable types

Extends `LibEsDefaults.substitutions` (in `Types/Generator.fs`) with
mappings to `obj` for TypeScript built-ins that Fable doesn't model as
distinct F# types (`PropertyKey`, `RegExp`, `URL`, `AbortSignal`,
`WebSocket`, `EventTarget`, `Event`, `Blob`, `File`, `FormData`, etc.).

These types previously generated FS0039 at every reference site. Mapping
to `obj` preserves runtime correctness (they pass through Fable as opaque
JS handles) and resolves the reference.

### `04d6910` — extend lib.dom obj substitutions to Streams/Crypto/Encoding/Request*

Same pattern as `e41629b`, broader scope: `Request`, `Response`,
`RequestInit`, `RequestInfo`, `ResponseInit`, the streaming abstractions
(`QueuingStrategy`, `UnderlyingSource/Sink`, `Transformer`,
`ReadableByteStreamController`, `ReadableStream/WritableStream/Transform
Stream DefaultController`, `ByteLengthQueuingStrategy`,
`CountQueuingStrategy`), text encoding (`TextEncoder/Decoder/Stream`
variants), `Crypto/SubtleCrypto/CryptoKey/CryptoKeyPair/JsonWebKey`,
event types (`EventInit` variants, `ErrorEvent`, `PromiseRejectionEvent`),
`DOMException`, `URLSearchParams`, and the `WebAssembly.*` module
(`Module`, `Instance`, `Memory`, `Table`, `Global`, `Tag`, `Exception`).

Cleared ~338 FS0039 errors in one pass.

### `dc7a650` — substitute lib.es TypeAliases at TypeAliasRemap registration site

`Iterator`, `Generator`, `AsyncIterator`, `AsyncGenerator`, `AsyncIterable`
needed the same arity-3-to-1 collapse `IterableIterator` already had.
These appear as `TypeAlias` in the resolved tree rather than
`Interface/Class/Enum`, so the `resolvedTypePreludeInterceptor` couldn't
see them — `ResolvedType` has no `TypeAlias` case (aliases resolve to
their body, losing identity at TypeReference resolution time).

Fix moves the substitution check into `prerenderTypeAliases` (in
`Generator/RenderScope.Prelude.fs`) where `ResolvedExport.TypeAlias`
still carries the alias's `Name` and `Source`. When the source is
`LibEs _` and the alias name is in the substitution table, register
the canonical substituted ref in `TypeAliasRemap` for both the
declaration's `TypeKey` and the body's `TypeKey`.

Also adds an `Enum` case to `tryLookupSubstitution` (currently unused at
runtime, included for completeness).

## What landed in `Fidelity.CloudEdge/generators/xantham` since PR #1

One substantive commit plus output regenerations.

### `ff963b0` — fix(verify): inline Xantham.Fable.Core Library.fs so substrate intrinsics resolve

The verify .fsproj files (`Verify.Agents.fsproj`,
`Verify.WorkersTypes.fsproj`, `Verify.DynamicWorkflows.fsproj`)
referenced only `Fable.Core 4.3.0`. Xantham emits substrate primitives
defined in `Xantham.Fable.Core/Library.fs` — `proptypekey<'T,
'ReturnType>` (TS `T[K]`), `keyof<'T>` (TS `keyof T`), `typekeyof`,
`proptypelock`. With no reference to that library, every emission
of those types produced FS0039 (404 hits for `proptypekey` in agents
alone, plus 12 in workers-types and 6 in agents for `keyof`).

The natural fix would have been a `<ProjectReference>` to
`Xantham.Fable.Core.fsproj`, but `speakez-xantham/Directory.Build.props`
sets `IsExcludedFromRootBuild=true` for Fable projects (and
`Directory.Build.targets` then clears `CompileDependsOn`) so the
referenced project never produces a DLL. The exclusion is intentional:
the comment notes it exists to prevent naive AI tooling from triggering
unwanted Fable builds.

Compromise: include `Library.fs` directly as a `<Compile>` item via
`<Link>`, which respects the exclusion while making the substrate types
visible. Also bumps `Fable.Core` to `5.0.0-beta.4` (what
`Xantham.Fable.Core` targets).

### Regeneration commits (no driver source change)

Twenty regen commits, each tagged with the Xantham change that
prompted it (e.g. `regen after LibExDefaults change`, `regenerate after
broadened lib.es eligibility`). These keep `output/*.fs` and
`verify/*.wrapped.fs` aligned with the current generator. No functional
changes; they exist so cross-SDK error counts in the verify pipeline
reflect the active code.

### Driver shrinkage

`generators/xantham/Program.fs` is now 52 lines. Earlier in the May 5
work, the driver carried custom Cloudflare interceptors for `Typescript`-
parent pruning, `babel/typescript` source filtering, and a private
lib.es substitution table. Commit `92d5312` (pre-PR1, on the
`source-refactor` branch via the merge) pulled all three into Xantham's
default `LibEsDefaults` / path interceptors. The current driver is now
just `Decoder.Runtime.create → prerenderTypeAliases → processExports →
RootModule.collectModules → renderRoot`, with `GeneratorContext.Empty`.

This matches the boundary rule stated in the project notes: general
TS→F# patterns belong in Xantham; only implementation-specific policy
should live in a consumer.

## Remaining FS0039 buckets (top 15, agents)

```
244  'Type'
112  'Code'
 54  'JSONSchema'
 54  'CloudflareAgents.JsonSchemaTyped.Decoder.TestsFixturesAgentsNodeModulesJsonSchemaTypedDraft202012'
 46  'Method'
 36  'Item'
 32  'Props'
 28  'TypeName'
 26  'State'
 26  'Invoke'
 22  'Optout'
 22  'Optin'
 20  'SpecificationVersion'
 20  '_Lit87'
 20  'Brand'
```

Three patterns dominate:

1. **Free typars at use sites** (`SendResultT`, `Extra`, `INPUT`,
   `Output`, `ZodType`, `Value` family — collectively ~350 errors).
   `'Env`, `'Event`, `'T`, `'R` referenced from a member position when
   the parent type doesn't declare the typar. Earlier use-site
   propagation attempts moved the error rather than fixing it; the fix
   shape is structural — lift typars to declaring type rather than
   use site.

2. **Path-navigation residuals** (synthetic `_Lit*` references like
   `_Lit87`, `_Lit70`, plus `Method`, `Type`, `Item` properties of
   inner literal scopes). References to interned synthetic literals
   from call sites where the FIRST encounter's grafted path doesn't
   navigate cleanly. Open question: does `scope.PathContext` accumulate
   depth (member, parameter, etc.) that the docs' "transient resolution"
   semantics doesn't expect to handle? The cached `TypeRefRender`
   carries the first scope's grafted path, and anchoring against the
   call-site anchor may not match the actual emission location.

3. **Declaration-merged augmentations across packages**
   (`JSONSchema`, `StandardSchemaV1`, `ZodType` — module-shaped references
   to types that are augmented by the consumer package but emitted in
   the original lib's scope). Map collision in `interner.ExportMap`
   (multiple exports sharing a `Source` bucket) suspected; a previous
   iterate-all attempt regressed by +424 cascade and was reverted.

## Outstanding questions for review

These are worth a look before more fixes land:

- **scope.PathContext depth.** When prerender threads a synthetic
  literal through nested property scopes, the grafted path can become
  unexpectedly deep (e.g. `DispatchWorkflow.LoadRunner.LoadRunner.Invoke`).
  Is that an intentional consequence of the path system as documented,
  or a sign the property-scope graft is over-applying? Bears on the
  path-navigation residuals bucket.

- **Map vs MultiMap for `interner.ExportMap`.** Several
  declaration-merge cases (lib.dom `Request` augmented by
  workers-types, JSONSchema-typed augmentations across packages) hit
  a Source-bucket collision. A naive flip to a multi-map cascaded
  +424 errors when tried in isolation; presumably consumers expect
  one entry per Source.

- **`TypeAlias` identity in `ResolvedType`.** The lib.es alias
  substitution had to be done at `prerenderTypeAliases` because
  `ResolvedType` has no `TypeAlias` case (aliases resolve to their
  body). Is preserving alias identity through resolution something
  the design wants to retain, or is the registration-site workaround
  the intended pattern?

## Reproduction

From either repo:

```bash
# Rebuild Xantham + regenerate all three SDKs
cd /home/hhh/repos/Fidelity.CloudEdge/generators/xantham
dotnet build Driver.fsproj --no-incremental
dotnet run --project Driver.fsproj --no-build -- output/dynamic-workflows.json output/dynamic-workflows.fs
dotnet run --project Driver.fsproj --no-build -- output/agents.json output/agents.fs
dotnet run --project Driver.fsproj --no-build -- output/workers-types.json output/workers-types.fs

# Re-wrap and verify (uncapped error counts)
cd /home/hhh/repos/Fidelity.CloudEdge/generators/xantham/verify
for proj in Verify.DynamicWorkflows Verify.WorkersTypes Verify.Agents; do
    echo "=== $proj ==="
    dotnet build "$proj.fsproj" /p:OtherFlags="--maxerrors:10000" 2>&1 | tail -3 | head -2
done
```

Generator + Decoder tests:

```bash
cd /home/hhh/repos/speakez-xantham/tests/Xantham.Generator.Tests
dotnet run -- --colours 0 2>&1 | tail -3

cd /home/hhh/repos/speakez-xantham/tests/Xantham.Decoder.Tests
dotnet run -- --colours 0 --filter-test-list "Identifier" 2>&1 | tail -3
```
