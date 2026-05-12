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

| SDK | Pre-PR1 baseline | Pre-Literal-fix | Current | Δ vs baseline |
|---|---:|---:|---:|---:|
| dynamic-workflows | 18 | 6 | 6 | −12 |
| workers-types | 1,376 | 408 | 401 | −975 |
| agents | 3,932 | 1,498 | 1,537 | −2,395 |
| **Total** | **5,326** | **1,912** | **1,944** | **−3,382 (−64%)** |

The dominant remaining error code is FS0039 ("type or namespace not
defined"); FS0001, FS0033, FS0663, FS0698, FS0887 round out the long tail.

Note the "Pre-Literal-fix" column: the most recent commit
(see *Synthetic inner-literal anchor* below) is a principled structural
fix that trades ~290 FS0039 "not defined" errors for ~310 FS0001 latent
type-mismatch errors that had been hiding behind the not-defined ones.
Net total goes up by 32, but the underlying bug count is lower and the
error picture is now honest — the FS0001s are real semantic issues that
need a separate pass.

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

### Synthetic inner-literal anchor — graft `scope.PathContext` in the `Literal` branch

The `ResolvedType.Literal` arm of `prerender` in `RenderScope.Prelude.fs`
was producing the wrong root path for property-level single-literal
types — the TS pattern `type: "image"` inside an inline record like:

```ts
{ type: "image"; data: string; mimeType: string; ... }
```

The encoder represents `"image"` as a singular `ResolvedType.Literal
(TsLiteral.String "image")` (not wrapped in a `Union`), and the generator
emits it as a one-case `[<StringEnum>] type Method = | [<CompiledName
"image">] Image` — fine in isolation. The problem was *where* that
one-case union got anchored. The two sibling branches did this
correctly:

* `ResolvedType.TypeLiteral` (the parent record case at line 564–580):
  computes `rootPath = TransientPath.toTransientModulePath
  scope.PathContext |> TransientTypePath.graft`. The graft pulls in
  the parent's accumulated path including the property name
  (`appendNameToPathContext` ran in `Property.render` before recursing).
* `ResolvedType.Union` *LiteralLike* branch (line 246–268): same graft
  when `path = Anchored`.

The `ResolvedType.Literal` arm at line 319–332 instead used the bare
`TransientTypePath.Anchored` for `Root`. That placeholder means
"I am exactly the anchor" — so the inner literal got anchored at the
parent record's path. Two distinct anchors collided at the same `TypePath`
in `RootModule.collectModules`: the parent record `_Lit70` and the
inner literal `_Lit70.Method`.

In aggregate this manifested as 518+ FS0039 errors in agents
(`'Type'` ×244, `'Code'` ×112, `'Method'` ×46, `'Item'` ×36,
`'TypeName'` ×28, `'State'` ×26, `'Invoke'` ×26, plus tail entries).

Fix: graft `scope.PathContext` in the `Literal` arm, identical to the
TypeLiteral branch. Output now emits

```fsharp
module rec _Lit70 =
    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Method = | [<CompiledName("resources/templates/list")>] ResourcesTemplatesList
```

at `Agents.Agent._Lit70` (sibling of the parent record `type _Lit70`),
which F# accepts because `type X` and `module rec X` coexist at the
same scope.

#### Empirical impact

|  | Before fix | After fix | Δ |
|---|---:|---:|---:|
| FS0039 `'Type'` (agents) | 244 | 78 | −166 |
| FS0039 `'Code'` (agents) | 112 | 34 | −78 |
| FS0039 `'Method'` (agents) | 46 | 0 | −46 |
| FS0039 (total, all three SDKs) | ~2,646 | ~2,346 | ~−300 |
| FS0001 (agents) | 276 | 370 | +94 |
| FS0698 (agents) | 120 | 146 | +26 |
| Total errors (all three) | 1,912 | 1,944 | +32 |

#### Why the net went up

The fix is principled but exposes a latent layer that was hiding
behind FS0039. Previously, every `_Lit70.Method` reference failed early
with "type 'Method' is not defined" — F# stopped checking. With the
type now defined, F# proceeds to type-check the *uses*, which surfaces
real type-mismatch issues that have nothing to do with the inner-literal
anchor itself. Two patterns dominate the FS0001 increase:

* **Erased-union narrowing** — call sites where a `U18<…, _Lit7, …>`
  value is passed to a parameter typed as a specific `_Lit7`. F# rejects
  this even though `_Lit7` is one of the union's erased cases. The
  generator emits the broader union somewhere upstream of where the
  narrowed type is expected. Sample:

  ```
  The type 'U18<Agents.AddMcpServerOptions._Lit5, _Lit28, _Lit54,
              _Lit56, _Lit64, _Lit66, _Lit68, _Lit69, _Lit70, _Lit71,
              _Lit72, _Lit73, _Lit74, _Lit75, McpAgent._Lit63,
              McpAgent._Lit65, McpAgent._Lit66, McpAgent._Lit69>'
  is not compatible with the type 'Agents.AddMcpServerOptions._Lit7'
  ```

* **`Typescript.SubmitEvent._Lit*` path leak** — a separate path-
  pruning bug where the `Typescript` parent prune doesn't fire on
  member paths nested inside synthetic literals. Sample:

  ```
  The type 'AgentContext' is not compatible with the type
  'Typescript.SubmitEvent._Lit1353'
  ```

Both are real generator bugs that were latent before this fix. The
"+32" net is the cost of making them visible; the underlying bug
count went down even though the surface count went up. The error
picture is now honest — actionable categories instead of a wall of
not-defined errors that all trace to one root cause.

#### Why this took time to find

Five investigative dead ends before the trace pointed at the right
branch:

1. *Map-collision in `interner.ExportMap`*: ruled out by direct
   inspection of how the synthetic literals' ResolvedType reaches
   the lookup site.
2. *Cache-race on shared body identity*: that turned out to be a
   different bug (pattern 4, the `Record<K, V>` collapse — encoder-
   side).
3. *Per-scope path-recomputation*: tried wrapping the cache-hit
   branch in a per-call-site grafted path. Reverted as drift from
   Shayan's design (one entry per `ResolvedType` in
   `Choice<ResolvedType, ResolvedExport>`-keyed dictionary).
4. *Iterate-all in synthetic emission*: regressed +424 errors.
5. *Resolved-graph typar walking*: regressed +196 errors versus
   atom-only walk.

The break came from adding `eprintfn` traces at
`anchorPreludeAnchorScope`'s Transient branch and at
`Render.Transient.TypeLiteral.render` to confirm whether the inner
literal was actually being registered with the parent's TypeStore.
The traces showed two distinct scope IDs — the property's
`appendNameToPathContext`-derived scope was registering correctly,
but the *anchor* was reading a different scope. From there, examining
which `ResolvedType` kind reached the anchor labeled with
`agents.Agent._Lit70` revealed it was `Literal (Str …)` — not the
record. Cross-referencing the prerender branch for `ResolvedType.Literal`
against `ResolvedType.TypeLiteral` and `ResolvedType.Union LiteralLike`
made the missing graft obvious.

The Literal-arm omission is consistent with how rare single-literal
property types are: most TS authors write `type: "image" | "audio"`
(a Union LiteralLike) rather than `type: "image"` alone. The
`agents` SDK happens to use single-literal property typing heavily
through `@modelcontextprotocol/sdk`'s discriminated record shapes
(`{ type: "image"; data: string; mimeType: string }` and friends),
which is why this bucket showed up so prominently in agents and not
in dynamic-workflows or workers-types.

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

## Remaining FS0039 buckets (top 15, agents — post Literal-anchor fix)

```
 80  'Type'                   ← residual after Literal-anchor fix
 54  'JSONSchema'             ← declaration-merge (pattern 3)
 54  'CloudflareAgents.JsonSchemaTyped.Decoder.TestsFixturesAgentsNodeModulesJsonSchemaTypedDraft202012'
 38  'Code'                   ← residual
 34  'Item'                   ← residual
 30  'Props'                  ← typar
 30  'Optout'                 ← inner literal (different position)
 28  'TypeName'               ← residual
 24  'Invoke'                 ← residual
 22  'Optin'
 20  'Output'                 ← typar
 20  '_Lit87'                 ← path-navigation residual
 20  'Brand'
 18  'State'                  ← typar
 18  'Input'                  ← typar
```

The `'Type'`, `'Code'`, `'Item'`, `'TypeName'`, `'Invoke'` residuals
(~200 errors remaining out of the ~518 in this family pre-fix) are
positions where the inner literal sits inside *nested* synthetic
contexts (literal in array literal, literal in tuple literal, etc.)
where the property-name graft alone isn't enough — the parent scope
itself has a deeper transient context that needs a different anchor
strategy. Worth a follow-up but smaller than the bucket pre-fix.

In workers-types the top buckets are now dominated by pattern 4
(`ModuleImports` ×134, `WebAssembly` ×134 — the `Record<K, V>`
collapse, flagged below) and a separate `GetWithMetadata` ×28 leak
in `IncomingRequestCfPropertiesTLSClientAuthPlaceholder`.

Three patterns dominate:

1. **Free typars at use sites** (`SendResultT`, `Extra`, `INPUT`,
   `Output`, `ZodType`, `Value` family — collectively ~350 errors).
   `'Env`, `'Event`, `'T`, `'R` referenced from a member position when
   the parent type doesn't declare the typar. Earlier use-site
   propagation attempts moved the error rather than fixing it; the fix
   shape is structural — lift typars to declaring type rather than
   use site.

2. **Path-navigation residuals** (synthetic `_Lit*` references like
   `_Lit87`). After the Literal-anchor fix described above, the
   `Method`/`Type`/`Code`/`Item` property bucket dropped from ~518 to
   ~200. The remaining ~200 are positions where the inner literal sits
   inside *nested* synthetic contexts (literal-in-array, literal-in-tuple,
   nested-inline-shape) where the property-name graft alone isn't
   enough — the parent scope itself has a deeper transient context
   that needs a different anchor strategy. The encoder's deduplication
   of structurally-identical inner literals across distinct outer
   contexts is also a factor (one resolved-type entry winning the
   render cache for several call sites).

3. **Declaration-merged augmentations across packages**
   (`JSONSchema`, `StandardSchemaV1`, `ZodType` — module-shaped references
   to types that are augmented by the consumer package but emitted in
   the original lib's scope). Map collision in `interner.ExportMap`
   (multiple exports sharing a `Source` bucket) suspected; a previous
   iterate-all attempt regressed by +424 cascade and was reverted.

4. **`Record<K, V>` body collapse** (workers-types: 132 `'ModuleImports'`
   + 132 `'CloudflareWorkersTypes.Cloudflare.WorkersTypes.WebAssembly'`
   = ~66 unique sites; also tied to several `'Type'`/`'Item'` bucket
   entries). The encoder's mapped-type expansion drops the value-type
   typar — `Record<string, ImportValue>`, `Record<string, unknown>`,
   `Record<string, T>` for any `T` all resolve to body
   `{[key: string]: any}` (Type field `-7`). The decoder's
   reference-equality interning then merges those bodies into a single
   `ResolvedType`. 19+ TypeAlias declarations end up sharing one body
   identity in workers-types alone:

   ```
   Pick, Record, Partial, Omit, Readonly, Without,
   Cloudflare.Exports, WebAssembly.{Exports,Imports,ModuleImports}
   (with declaration-merge duplicates),
   PublicKeyCredentialClientCapabilities,
   "cloudflare:pipelines".PipelineRecord, Params,
   FlagshipEvaluationContext, AiModelListType
   ```

   When the generator caches a `TypeRefRender` for the shared
   `ResolvedType` (during prerender of the first alias's body), every
   anonymous `Record<string, X>` reference site cache-hits and emits
   the cached path. `WebAssembly.ModuleImports` happens to win the
   cache race, so e.g. `interface IncomingRequestCfPropertiesBase
   extends Record<string, unknown>` lands as
   `inherit WebAssembly.ModuleImports`.

   **Fix surface:** encoder-side. The mapped-type expansion in the
   Xantham.Fable encoder needs to preserve the value-type slot
   (`Type: 39139` for `ImportValue`, not `Type: -7` for `any`).
   Generator-side mitigations exist (refuse to cache shared-body
   aliases; emit `obj` on collision) but each papers over the
   encoder issue rather than fixing it.

   **Why we didn't try a surface-level substitution.** The obvious
   next move was to extend the lib.es/lib.dom substitution table —
   add `WebAssembly.{Exports, Imports, ModuleImports}` (the three
   not yet covered; the other seven `WebAssembly.*` types are
   already `obj`-substituted) plus the TS utility types
   (`Pick`, `Record`, `Partial`, `Omit`, `Readonly`, `Without`) so
   none of the collision participants can win the cache race.
   On its face this is principled — those types have no useful
   F# representation anyway, and `Cloudflare.Exports` /
   `"cloudflare:pipelines".PipelineRecord` etc. are bag-of-
   properties shapes whose typed value adds nothing over `obj`.
   F# users consuming a Workers binding don't reach for
   `WebAssembly.ModuleImports` as a typed value — they call
   `WebAssembly.instantiate(env.MY_WASM, imports)` and treat the
   result as opaque JS, same as the seven already-substituted
   `WebAssembly.*` types.

   The catch is heritage. The TS pattern

   ```ts
   interface IncomingRequestCfPropertiesBase
     extends Record<string, unknown> { ... }
   ```

   currently emits `inherit WebAssembly.ModuleImports` (wrong path,
   but a path). If the substitution table covers `Record` and the
   WebAssembly aliases, the emission becomes `inherit obj` inside
   an interface body — F# rejects with FS0887 ("'obj' is not
   interface type"). So a surface fix at the substitution layer
   trades FS0039 (this bucket) for FS0887 in heritage positions
   for every `interface X extends Record<...>` in the SDK. That's
   a different error category but not a smaller one — likely
   ~66 sites move from FS0039 to FS0887.

   The structurally correct emission for heritage of a Record-
   shaped type would be **no heritage at all** plus an index
   signature member on the inheriting interface, or alternatively
   replacing the heritage with `inherit obj()` only when the
   target is a class body (not an interface). Both of those are
   generator-side decisions that depend on whether the inherit
   target's resolved body is a TypeLiteral with only an index
   signature — a structural test the generator can do today.

   **Where this lands:** the encoder fix preserves the typar and
   makes `Pick<T, K>`, `Record<K, V>`, `Omit<T, K>`, etc. resolve
   to distinct interned bodies, which removes the cache-race
   premise and lets each alias's own emission decision stand.
   That's the clean fix. A generator-side band-aid would have to
   handle the Record-in-heritage case alongside the substitution
   to avoid FS0887 fallout. Worth flagging together so the encoder
   fix doesn't get scoped narrowly to one bucket.

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
