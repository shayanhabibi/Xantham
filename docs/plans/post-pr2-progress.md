# Post-PR2 Progress Report

> Handover note for Shayan. Picks up after PR #2
> (`prevent-silent-drops-in-export-map`) merged at commit `494a248`.
> The post-PR1 doc (`docs/plans/post-pr1-progress.md`) covers the
> earlier era and the rationale that fed into PR #2.

- **Branch:** `master` (PR #2 merged direct to master)
- **Window:** 2026-05-13, post-merge of PR #2
- **Status:** 178 generator tests pass; 13 decoder Identifier tests pass

# Baseline (post-PR2, after driver migration)

| SDK | Pre-PR1 baseline | End of PR1 era | Post-PR2 | Δ vs baseline |
|---|---:|---:|---:|---:|
| dynamic-workflows | 18 | 6 | 6 | −12 |
| workers-types | 1,376 | 401 | 370 | −1,006 |
| agents | 3,932 | 1,460¹ | 1,460 | −2,472 |
| **Total** | **5,326** | **1,867** | **1,836** | **−3,490 (−65%)** |

¹ End-of-PR1-era agents count was 1,441 (per post-pr1-progress.md). The
1,460 post-PR2 number reflects the cascade Shayan predicted: previously-
dropped exports now visible expose latent inner-literal anchor bugs in
Interface/Class bodies. Workers-types' −31 win dominates the net, so
the overall is still down.

# What PR #2 brought in (Shayan)

Two structural fixes plus one bug catch, plus encoder ergonomic work.

## `ExportMap` is now multi-valued

`interner.ExportMap` went from `Map<Source, LazyResolvedExport>` to
`Map<Source, LazyResolvedExport list>` at
[`Arena.Interner.fs:469`](../../src/Xantham.Decoder/Types/Arena.Interner.fs#L469).
Construction at line ~989 now groups by Source and accumulates the
list rather than letting `Map.ofSeq` last-wins-drop duplicates. Doc-
comment invariant: *"Lists should be singletons where
`Source.IsPackage`"* — multi-entry buckets only appear under
`Source.LibEs` / `Source.PackageInternal`.

For `zod/v3/types.d.ts` (71 exports → previously 1 surviving), the V3
ZodType class with 3 typars and 44 members is now rendered. The
`'ZodType' FS0033 ×104` bucket in agents — referenced with 3 type
arguments against a previously-emitted `type ZodType = | False | True`
String-enum-shaped collapse — is cleared.

## `Record<K, V>` body preserves typars

`TypeFlagObject.Mapped` no longer collapses generic mapped types to
`{[key: string]: any}`. At
[`TypeFlagObject.fs:217`](../../src/Xantham.Fable/Reading/Dispatch/TypeFlagObject.fs#L217)
the fallback path now routes `nameType` / `constraintType` /
`typeParameter.getConstraint` as the IndexSignature Parameter Type,
and `templateType` as the value Type. The principle is stated cleanly
in the PR thread: *"encoder transmits, doesn't decide"* — the
encoded `IndexSignature` Parameter Type slot carries the real K
(typar, template-literal type, conditional, etc.); the value Type
slot carries the real V; the generator decides what to do with each
shape.

Net effect: `Record<string, ImportValue>` and `Record<string,
unknown>` now intern to distinct `ResolvedType` identities (different
content), eliminating the render-cache race that mis-routed
`extends Record<...>` heritages to `WebAssembly.ModuleImports`. The
`'ModuleImports' ×134` + `'WebAssembly' ×134` family in workers-types
went from 268 → 10 (only one qualified-path variant remains).

## Commutative-union element sort

`STypeUnionBuilder` / `STypeIntersectionBuilder` annotations document a
new commutative-vs-ordered contract; the runtime enforcement is the
decoder-side sort in `Utils.compressWithMap` (around lines 305/310 of
`Arena.Interner.fs`). `"text" | "audio"` and `"audio" | "text"` now
intern to the same `ResolvedType`. This closes the SyntheticPathAssignment
content-vs-reference identity gap documented in the post-PR1 doc
("Outstanding questions for review" → SyntheticPathAssignment
coverage gap).

## Bug catch: `TypeFlagObject.Instantiated → ReverseMapped`

`TypeFlagObject.Instantiated of Ts.ObjectType` was bound to
`Ts.ObjectFlags.Instantiated` but the dispatch was clearly meant for
`Ts.ObjectFlags.ReverseMapped`. Renamed the case + fixed the bit
flag. New typed extension wrappers `MappedType` / `ReverseMappedType`
in `Utils/TypeScript.Extensions.fs` give strong-typed access to
declaration / typeParameter / constraintType / nameType / templateType
fields instead of raw `Ts.ObjectType`.

## Encoder ergonomic: package-name entry

The encoder driver previously required a path to a specific `.d.ts`
inside a package. New module `Xantham.Fable.Temp.Directory`
(`Utils/Xantham.Directory.fs`) creates a per-run scratch `.xantham/run_*/temp.d.ts`
that imports all of a package's declaration files; the TS compiler walks
out via normal module resolution. CLI accepts a package name
(`xantham solid-js`) and resolves entry from `package.json` +
`node_modules`. Test fixtures migrated from a flat
`TypeFiles/packages/<name>/` layout to `TypeFiles/node_modules/<name>/`
with proper `package.json` files. Final commits tighten cleanup with
parallel-safe `try .../with _ -> ()` guards on `.xantham/run_*` race
conditions.

# Driver migration (post-merge)

PR #2's `ExportMap` shape change required one direct fix in
speakez-xantham (the sole direct consumer of the old shape).

## `b6505d3` — generator: iterate list-valued ExportMap in processExports

`processExports` at
[`RenderScope.Anchored.fs:748`](../../src/Xantham.Generator/Generator/RenderScope.Anchored.fs#L748)
was `Map.iter (fun _ x -> ... x.Value)`. With `ExportMap` now
list-valued, the inner needs a `List.iter`. Build was failing at
RenderScope.Anchored.fs:750 (`"'List<_>' does not define the member
'Value'"`) until this iteration adjustment:

```fsharp
interner.ExportMap
|> Map.iter (fun _ lazyExports ->
    lazyExports
    |> List.iter (fun lazyExport ->
        registerAnchorFromExport ctx lazyExport.Value))
```

That's the only generator-side migration required by PR #2.

# Fidelity.CloudEdge driver: scripts under source control

Earlier sessions accumulated `/tmp/wrap-verify.sh` ad-hoc helpers
that got purged between machine states. PR `Fidelity.CloudEdge#?`
moves these into a stable `generators/xantham/scripts/` folder:

- `regen-all.sh` — rebuild Driver + regen all three SDK outputs into
  `output/*.fs`.
- `wrap-all.sh` — wrap all three into `verify/*.wrapped.fs` (handles
  the dynamic-workflows special-case where the SDK name's hyphen
  breaks the default `${name^}` capitalisation).
- `wrap-verify.sh <name> <fs-path> [<module-name>]` — single-SDK
  wrapper with optional module-name override.
- `verify-all.sh [counts|hist|fs0039]` — build all three verify
  projects with `--maxerrors:10000` and print counts / FS-code
  histogram / FS0039 `'name'` buckets.
- `README.md` documenting the typical workflow and the `--maxerrors:200`
  default-truncation gotcha.

# Where the remaining error count comes from

## Top FS0039 buckets, post-PR2

### workers-types (370 total, 332 FS0039)

```
 28  'Type'
 28  'GetWithMetadata'
 28  'CloudflareWorkersTypes.Cloudflare.WorkersTypes.IncomingRequestCfPropertiesTLSClientAuthPlaceholder'
 14  'FunctionCall'
 10  'Stream'
 10  'CloudflareWorkersTypes.Cloudflare.WorkersTypes.WebAssembly'
 10  'CloudflareWorkersTypes.Cloudflare.WorkersTypes.VectorizeModule._Lit7'
 10  'CAPTURING_PHASE'
 10  'BUBBLING_PHASE'
 10  'BigUint64Array'
 10  'AT_TARGET'
  8  'Success', 'RequestPriority', 'NONE', 'Invoke', 'Done', 'CompressionFormat'
  6  'ToolChoice', 'Timeout', 'Role'
```

`'ModuleImports' ×134` is gone (Record collapse fixed). Remaining
`'WebAssembly' ×10` is the qualified-path variant referencing
`WebAssembly.VectorizeModule._Lit7` and similar nested
synthetic-literal paths.

### agents (1,460 total, 1,202 FS0039 — sample top 20)

```
 78  'Type'                       ← residual inner-literal in synthetic _LitN
 44  'Code'                       ← residual inner-literal
 38  'CloudflareAgents.JsonSchemaTyped.Interface'  ← NEW: PR2 cascade
 36  'Item'                       ← residual inner-literal
 30  'Optin'                      ← residual inner-literal
 28  'TypeName'                   ← residual inner-literal
 26  'State'                      ← typar
 26  'Invoke'                     ← residual inner-literal
 22  'Optout'                     ← residual inner-literal
 22  'Flat'                       ← NEW: previously-dropped export now visible
 20  '_Lit85'                     ← path-navigation residual
 20  'Brand'                      ← typar
 16  'Success', '_parse', 'NeedsApproval'  ← NEW: previously-dropped
 14  'Input'                      ← typar
 14  'CloudflareAgents.JsonSchemaTyped.Decoder.TestsFixturesAgentsNodeModulesJsonSchemaTypedDraft202012.JSONSchemaModule.Number'
 14  'CloudflareAgents.JsonSchemaTyped.Decoder.TestsFixturesAgentsNodeModulesJsonSchemaTypedDraft202012.JSONSchemaModule.Integer'
 12  'Values'                     ← residual
 12  'SpecificationVersion'       ← residual
```

The `'JsonSchemaTyped.Interface'` ×38 bucket is the most visible new
cascade. See investigation below.

## Today's investigation: the `'JsonSchemaTyped.Interface'` cascade

Sample errors:

```
agents.wrapped.fs(16080,61): error FS0039: The type 'UnevaluatedProperties'
  is not defined in 'CloudflareAgents.JsonSchemaTyped.Interface'.
agents.wrapped.fs(16081,56): error FS0039: The type 'UnevaluatedItems'
  is not defined in 'CloudflareAgents.JsonSchemaTyped.Interface'.
agents.wrapped.fs(16093,53): error FS0039: The type 'PropertyNames'
  is not defined in 'CloudflareAgents.JsonSchemaTyped.Interface'.
```

The source-side shape, from `json-schema-typed`'s declKey 47870 in the
encoded JSON:

```
TypeAlias name="Interface" FQN=["__type"]
  Source.Canonical = json-schema-typed/draft_2020_12.d.ts:JSONSchema
  TypeParameters = [Value, SchemaType]
```

This is `type JSONSchema.Interface<Value, SchemaType> = { ... }` —
a big mapped/structural type alias whose body is a TypeLiteral with
~12 property-typed string-union literals (`unevaluatedProperties`,
`unevaluatedItems`, `propertyNames`, `patternProperties`,
`additionalProperties`, `prefixItems`, `dependencies`,
`dependentRequired`, `dependentSchemas`, `definitions`, …).

### What works and what doesn't

Traced `anchorPreludeExportScope` for the export `Interface`. It
processes 12 children. Trace at the Transient anchor branch with
`anchorPath` ending in `Interface`:

```
[TRACE-T] anchorPath=json-schema-typed.Interface -> finalPath=json-schema-typed.Interface.type
[TRACE-T] anchorPath=json-schema-typed.Interface -> finalPath=json-schema-typed.Interface.allOf
[TRACE-T] anchorPath=json-schema-typed.Interface -> finalPath=json-schema-typed.Interface.else
[TRACE-T] anchorPath=json-schema-typed.Interface -> finalPath=json-schema-typed.Interface   ← collision
[TRACE-T] anchorPath=json-schema-typed.Interface -> finalPath=json-schema-typed.Interface   ← collision
... (many bare-Interface entries) ...
```

Two patterns:

* **Correctly nested** — property name carried through (`.type`,
  `.allOf`, `.else`). These inner literals have `Root =
  AnchoredAndMoored "propertyName"`; the anchor pass anchors to
  `<Interface>.<PropertyPascal>` correctly.

* **Collided with parent** — `finalPath=json-schema-typed.Interface`
  (no segment after). These inner literals have `Root = Anchored`
  (bare); anchoring against parent gives the parent's path itself.
  `Dictionary.tryAdd` only keeps the first entry, so the parent type
  wins and these inner literals never get emitted at their intended
  path.

The failing names (`UnevaluatedProperties`, `UnevaluatedItems`,
`PropertyNames`) **don't appear in the Transient-branch trace at
all** — they don't reach the `Root = Transient` branch. They likely
go through the Synthetic-Anchored branch
([`RenderScope.Anchored.fs:465-491`](../../src/Xantham.Generator/Generator/RenderScope.Anchored.fs#L465-L491))
because their inner Union is in `ctx.SyntheticPaths` (multi-position
synthetic across multiple JSONSchema variants — Object, String,
Number, Array all reference the same `"unevaluatedProperties"` literal).

### The shape of the bug

When a one-case-string-union inner literal is:

1. **Multi-position** (referenced from multiple parent bodies — same
   `"unevaluatedProperties"` string used as a literal type in Interface,
   Object, String, …),
2. **SyntheticPathAssignment-eligible** (Union case satisfies
   `isSyntheticLiteral`),
3. **Buildable to a stable concrete path** via the
   `<parent>._LitN` formula in `SyntheticPathAssignment.fs`'s
   `buildSyntheticPath` …

… SyntheticPathAssignment assigns it ONE concrete path, e.g.
`AiModels._Lit156`, as a **sibling** of the parents that reference it.
References from each parent use the cached `ConcretePath` ref, which
localises differently depending on the call-site anchor:

* From `<X>._Lit47`, common prefix is `<X>`; localised reference =
  `_Lit156` (which fails — `_Lit156` doesn't exist at this scope).
* From `<JsonSchemaTyped>.Interface`, common prefix is
  `JsonSchemaTyped`; localised reference = `_Lit156.<…>`.

Yet the actual emitted reference is the *transient-style*
`Interface.UnevaluatedProperties` — two segments where the FIRST is
the parent name and the SECOND is the Pascal'd property name.

This suggests the cache-hit branch at
[`RenderScope.Prelude.fs:91-93`](../../src/Xantham.Generator/Generator/RenderScope.Prelude.fs#L91-L93)
(SyntheticPath-assigned + Root = Anchored) isn't being taken; the
cache hit is taking the *transient* path at
[`RenderScope.Prelude.fs:75-78`](../../src/Xantham.Generator/Generator/RenderScope.Prelude.fs#L75-L78)
which stores the cached transient path under the call-site's scope.
The cached transient was computed during the FIRST encounter at a
DIFFERENT parent's scope, so its rootPath reflects that other
parent's PathContext — not the current call site's.

Without more focused tracing this is speculation, but the
observable contradiction (referenced as `Interface.<X>`, never
emitted at any sibling path either) is consistent with two distinct
bugs interacting:

* The inner Union has *both* a cached transient ref (from a path
  through one parent) AND a SyntheticPath-assigned concrete path
  (from `SyntheticPathAssignment.run`). The cache-hit logic picks the
  transient one because of evaluation order — the first prerender
  happens before SyntheticPathAssignment can mark the type, or the
  cache-hit dispatch doesn't re-check `SyntheticPaths` membership.
* The Transient cache-hit's `tryAdd` step
  (`RenderScopeStore.tryAdd`) stores the cached transient under the
  new scope's TypeStore, with the OLD scope's grafted form — so the
  inner literal anchors at the wrong path.

### Sibling cascade buckets

The same pattern likely drives the other "new" buckets:

* `'Flat' ×22`, `'_parse' ×16`, `'NeedsApproval' ×16`,
  `'Success' ×16` — all are previously-dropped exports' inner
  literals.
* `'JsonSchemaTyped.Decoder.…JSONSchemaModule.Number' ×14` and
  `'…Integer' ×14` — similar collision shape, JSONSchema's Number /
  Integer per-type variants whose inner literals collide with the
  parent module path.

# Outstanding questions for review

## Resolved by PR #2

These were the largest items in the post-PR1 doc's "Outstanding
questions"; PR #2 addressed them.

* **Map vs MultiMap for `interner.ExportMap`** — resolved via list-
  valued map (above).
* **`Record<K, V>` body collapse** — resolved encoder-side via
  faithful MappedType transmission (above).
* **SyntheticPathAssignment content-vs-reference gap** — resolved
  via commutative-union element sort (above).

## Still open from post-PR1, unchanged by PR #2

* **`TypeAlias` identity in `ResolvedType`** — registration-site
  workaround in `prerenderTypeAliases` stands as the intended
  pattern. No PR #2 change here.

## New, opened by PR #2's surface changes

### Inner-literal anchor cascade for SyntheticPath-assigned literals referenced via cache-hit

The largest single observable cascade. ~38 errors in the
`JsonSchemaTyped.Interface` bucket alone; similar shape in the
`Flat`/`_parse`/`NeedsApproval`/`Number`/`Integer` buckets and
contributing to the 'Type'/'Code'/'Method'/'Item' family residuals.

The shape: inner one-case-string-union literal `X` is referenced from
multiple parent bodies. `SyntheticPathAssignment.run` assigns it one
concrete path `<sibling>._LitN`. But each call site emits a reference
that says `<parent>.X` (two segments, parent + Pascal'd property
name). Neither path resolves — `_LitN` isn't emitted under each
parent, and the type isn't navigable via the parent's name in F#'s
scope resolution.

The clean fix surface is one of:

1. **Cache-hit re-check** — at
   [`RenderScope.Prelude.fs:72-94`](../../src/Xantham.Generator/Generator/RenderScope.Prelude.fs#L72-L94)
   the cache-hit dispatch routes by the cached `Root` kind alone. If
   the resolvedType is in `ctx.SyntheticPaths`, route to the
   synthetic-cache-hit branch (line 91-93) regardless of what kind
   the cached Root has. Stops the transient cache from winning the
   race for a type that should be referenced via its concrete
   sibling path.

2. **Promote-on-second-encounter** — when the Transient cache hit
   sees a multi-position transient that doesn't have a concrete path
   yet, run `SyntheticPathAssignment.assignSynthetics` retroactively
   for that single ResolvedType and re-route to the concrete-cache-hit
   branch. The risk (raised in the post-PR1 doc): the "FIRST
   encounter wins" semantics changes from "ConcretePath at first
   encounter" to "ConcretePath promoted on second encounter," which
   could cascade in unexpected ways.

3. **Inner-literal emission across all body kinds** —
   `Interface`/`Class` body emissions get a *companion*
   `module rec X` containing the inner literal types referenced as
   `X.<PropertyPascal>`, mirroring what the `_LitN` synthetic-anchored
   pattern does for TypeLiteral bodies. Avoids the cross-parent
   sharing race entirely by emitting one copy per consumer.

(3) is what Shayan flagged as the "Migration work remaining item 5"
in PR #2's progress note. (1) and (2) are encoder/decoder/generator
plumbing fixes; (3) is a clean generator-side approach but produces
N copies of the same one-case enum per N consumers.

### `Source.LibEs` residual fallback

PR #2 added a `Log.error` to the residual fallback in
[`TypeDeclaration.fs:420`](../../src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs#L420)
— "Invariant: declaration is neither lib-es, nor has an export
collection, nor has a submodule id." Any hits during a verify run
mean a declaration the encoder doesn't have a SourceTag for and
isn't a lib type. Per Shayan's progress note: likely project-
reference or ambient module that
`SeedExportPoints` / `GetExportCollection` extensions aren't
reaching. Worth deciding whether to widen those extensions or
convert the fallback to an explicit `Source` DU case for
"unknown-but-declared."

### Tier 2-4 mapped-type renderer

The encoder now transmits `nameType` faithfully — IndexSignature
Parameter Type slot can carry a `TemplateLiteralType`,
`ConditionalType`, etc. The generator currently passes everything
through to `obj`. The four-tier scheme Shayan sketched in PR #2's
thread is the follow-on:

* Tier 1: pure key transformation (template literals like
  `\`get${Capitalize<K & string>}\``) → SRTP-like key constructor
  with `[<Emit>]`.
* Tier 2: `as K extends U ? never : K` → predicate function + option
  return on `tryGet`.
* Tier 3: combined transform+filter → same as tier 2 with a
  composed predicate.
* Tier 4: unsupported / opaque-bail.

Most Cloudflare SDK use is tier 1 (`Pick`/`Omit`/`Partial`-shaped).
The agents SDK has at least one tier 2 (per Shayan).

# Reproduction

```bash
# Regenerate all three SDKs and verify
cd /home/hhh/repos/Fidelity.CloudEdge/generators/xantham
./scripts/regen-all.sh         # rebuild Driver + regen
./scripts/wrap-all.sh          # wrap into verify/*.wrapped.fs
./scripts/verify-all.sh        # counts ("N Error(s)" per SDK)
./scripts/verify-all.sh hist   # FS-code histogram per SDK
./scripts/verify-all.sh fs0039 # FS0039 'name' buckets per SDK (top 20)
```

```bash
# Generator + Decoder tests
cd /home/hhh/repos/speakez-xantham/tests/Xantham.Generator.Tests
dotnet run -- --colours 0 2>&1 | tail -3

cd /home/hhh/repos/speakez-xantham/tests/Xantham.Decoder.Tests
dotnet run -- --colours 0 --filter-test-list "Identifier" 2>&1 | tail -3
```
