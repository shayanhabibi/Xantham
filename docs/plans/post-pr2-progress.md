# Post-PR2 Progress Report

> Handover note for Shayan. Picks up after PR #2
> (`prevent-silent-drops-in-export-map`) merged at commit `494a248`.
> The post-PR1 doc (`docs/plans/post-pr1-progress.md`) covers the
> earlier era and the rationale that fed into PR #2.

- **Branch:** `master` (PR #2 merged direct to master)
- **Window:** 2026-05-13, post-merge of PR #2
- **Status:** 178 generator tests pass; 13 decoder Identifier tests pass

# Baseline (post-PR2 era)

| SDK | Pre-PR1 baseline | End of PR1 era | Post-PR2 | After bool-collapse | Δ vs baseline |
|---|---:|---:|---:|---:|---:|
| dynamic-workflows | 18 | 6 | 6 | 6 | −12 |
| workers-types | 1,376 | 401 | 370 | 352 | −1,024 |
| agents | 3,932 | 1,460¹ | 1,460 | 1,417 | −2,515 |
| **Total** | **5,326** | **1,867** | **1,836** | **1,775** | **−3,551 (−67%)** |

¹ End-of-PR1-era agents count was 1,441 (per post-pr1-progress.md). The
1,460 post-PR2 number reflects the cascade Shayan predicted: previously-
dropped exports now visible expose latent inner-literal anchor bugs in
Interface/Class bodies. Workers-types' −31 win dominates the net, so
the overall is still down.

The "After bool-collapse" column reflects the
*"Collapse boolean-only literal sub-Union to F# `bool`"* fix described
below under "Investigation and follow-on fix." Net −61 from post-PR2,
−92 from end of PR1.

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

# Generator-side changes after PR #2 merge

Two commits on master, listed in order.

## `b6505d3` — generator: iterate list-valued ExportMap in processExports

PR #2's `ExportMap` shape change required one direct fix in
speakez-xantham (the sole direct consumer of the old shape).

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

## `<bool-collapse>` — collapse boolean-only literal sub-Union to F# `bool`

In `RenderScope.Prelude.fs:269`'s mixed-Union branch (literals + other
types), when every `TsLiteral` in the LiteralLike subset is a
`TsLiteral.Bool _`, substitute `ResolvedType.Primitive
TypeKindPrimitive.Boolean` for the synthesized sub-Union of
`(false, true)`. Eliminates the synthesized one-or-two-case StringEnum
wrapper for the common TS pattern `boolean | OtherType` (json-schema-
typed's ~12 keyword properties, agents inherits via re-export).

The synthesized sub-Union had a path-mismatch bug at the heart of the
`JsonSchemaTyped.Interface` cascade — TypeRef stored 2-segment path
`<PropertyName>.Literals`, body emitted at bare-Anchored (the parent's
own path), collision with parent record. The bool collapse removes
the synthesized type altogether, sidestepping the mismatch.

Net effect: total errors 1,836 → 1,775 (−61), 178 generator tests pass.
See "Investigation and follow-on fix" below for the full trace and
fix derivation.

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

## Top FS0039 buckets, post-bool-collapse

### workers-types (352 total, 518 FS0039 grep matches)

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

`'ModuleImports' ×134` is gone from PR #2 (Record collapse fixed).
Remaining `'WebAssembly' ×10` is the qualified-path variant
referencing `WebAssembly.VectorizeModule._Lit7` and similar nested
synthetic-literal paths.

### agents (1,417 total — sample top 20)

```
 80  'Type'                       ← residual inner-literal in synthetic _LitN
 48  'Code'                       ← residual inner-literal
 34  'Item'                       ← residual inner-literal
 32  'Optin'                      ← residual inner-literal
 28  'TypeName'                   ← residual inner-literal
 26  'State'                      ← typar
 26  'Invoke'                     ← residual inner-literal
 22  'Optout'                     ← residual inner-literal
 22  'Flat'                       ← previously-dropped export now visible
 20  '_Lit85'                     ← path-navigation residual
 20  'Brand'                      ← typar
 18  'Input'                      ← typar
 16  'StringIterator'             ← new surface from bool-collapse
 14  '_parse'                     ← previously-dropped
 12  'Values'                     ← residual
 12  'SpecificationVersion'       ← residual
 12  'Success'                    ← previously-dropped
 12  'Inclusive'                  ← residual
 10  'Origin', 'McpServer'        ← typar / previously-dropped
```

The `'JsonSchemaTyped.Interface' ×38` bucket that dominated the
initial post-PR2 snapshot is gone, along with the JSONSchemaModule
`'Number' ×14` and `'Integer' ×14` siblings (all three were the same
bool-only mixed-Union pattern). New top buckets are residuals from
other inner-literal patterns; see "Investigation and follow-on fix"
below.

## Investigation and follow-on fix: the `'JsonSchemaTyped.Interface'` cascade

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

### Deeper dive: encoder shares one Union ResolvedType across all the failing properties

Inspecting `Interface`'s body TypeLiteral (encoded TypeKey 47870) in
`agents.json`:

```
propertyNames:           TypeKey=47981 kind=Union
unevaluatedItems:        TypeKey=47981 kind=Union  (same!)
unevaluatedProperties:   TypeKey=47981 kind=Union  (same!)
```

All three properties resolve to the **same** Union 47981, which has
three elements: `Literal(18)` = `false`, `Literal(20)` = `true`,
`TypeLiteral(47980)` = the JSONSchema body. That is, the TS source
shape is `boolean | JSONSchema` for each property — the literal
strings `"unevaluatedProperties"` etc. don't appear at the type
level. Whatever's named `Interface.UnevaluatedProperties` in the
F# output is the **boolean sub-Union** wrapped as a string-enum,
which the generator names per-property.

That naming happens at line 269-284 of
`Generator/RenderScope.Prelude.fs` (the "mixed Union" branch — Union
with literals + others). It creates a fresh `ResolvedType.Union`
containing just the literal-like elements, wraps it in
`LazyContainer.CreateTypeKeyDummy<ResolvedType>` (note: NEW
ResolvedType value, NOT shared with the parent Union — so each
property's mixed-Union processing creates its own sub-Union value).
The sub-Union goes through the LiteralLike branch (line 217+) with
`lazyResolvedType.Raw = DummyTypeKey`, which sets
`path = AnchoredAndMoored "Literals"`.

`createTransientPath` (`Types/RenderScope.Prelude.fs:295`) then
grafts the property's PathContext onto the leaf, producing a stored
path of `Moored(<PropertyName>, "Literals")` — TWO segments. The
TypeRef registered in scope.TypeStore points at this two-segment
path.

But `renderUnionLiterals` (`Render.Transient.fs:89`) sets
`Metadata.Path = TransientTypePath.Anchored` (bare) and
`Name = ValueNone`. After anchoring, the body's emission location is
the **parent's path itself** (bare Anchored anchors to the anchor's
own TypePath). The Name fallback in `anchorTypeDefn` then reads the
parent's leaf as the type Name.

### The shape of the mismatch

| | Reference (use site) | Emission (body location) |
|---|---|---|
| Stored path | `Moored(<PropertyName>, "Literals")` (2-seg) | (none on `LiteralUnionRender`) |
| Anchored against `<parent>.Interface` | `<parent>.Interface.<PropertyName>.Literals` | `<parent>.Interface` (collision with parent record) |
| F# output | `Interface.<PropertyName>` after localise drops the trailing `.Literals` | parent record wins the path; sub-Union body never emits |

So references say `Interface.UnevaluatedProperties` (with the
`.Literals` segment somehow stripped by the localise pass), and the
body never emits anywhere — FS0039.

### Attempted fix (reverted)

Tried adding the same `scopedPath` derivation
(`TransientPath.toTransientModulePath scopeStore.PathContext |> TransientTypePath.graft`)
to `renderUnionLiterals` that `Members.renderFromMembersAndFunctions`
already uses for TypeLikeRender. Hypothesis: align the body's
emission location with the TypeRef's stored path.

Result: massive regression. **Total errors 1,836 → 2,358 (+522)**.
- workers-types: 370 → 639 (+269)
- agents: 1,460 → 1,713 (+253)
- 82 new FS0037 (Duplicate definition of type) errors across both SDKs

The fix double-grafts: `createTransientPath` already grafts
`scope.PathContext` into the TypeStore-stored path. Adding the same
graft to the LiteralUnionRender's Metadata.Path produces a body whose
anchored emission location matches `<PropertyName>` (single segment)
— but the TypeRef expects `<PropertyName>.Literals` (two segments).
Three properties referencing the same shared Union 47981 (whose
mixed-branch path-creation runs once via cache-hit, then re-runs
fresh per property) end up trying to emit three distinct sub-Unions
all at `<parent>.UnevaluatedProperties`, `<parent>.UnevaluatedItems`,
`<parent>.PropertyNames` — but at the EMISSION level, multiple
ResolvedTypes resolved through to the same identity in some cases,
triggering FS0037.

### What the right fix needs to do

The root cause is the asymmetric handling of `scope.PathContext`:

1. `createTransientPath` (TypeRef registration) **grafts** scope.PathContext into the stored path
2. `renderUnionLiterals` (body emission) uses bare `Anchored` — **doesn't graft**
3. `Members.renderFromMembersAndFunctions` (TypeLikeRender body emission) **grafts** scope.PathContext

So (1) and (3) graft; (2) doesn't. To fix (2), either:

* Make (2) graft AND change (1) so the leaf `"Literals"` isn't
  appended (because the property-name from PathContext already
  captures the identity — no need to suffix `.Literals`). Path in
  TypeStore becomes single-segment matching what (2) would produce.
* OR — leave (2) and (1) unchanged but change the LiteralUnionRender
  emission to recognize "I'm at bare Anchored, my actual location is
  on the TypeRef's stored path." This needs a way to read the
  stored path from `scope.TypeStore[resolvedType]` at render time
  and use it as the Metadata.Path. The render is in a `lazy` so the
  TypeStore is fully populated by then — feasible.
* OR — restructure the "mixed Union" branch so the sub-Union doesn't
  need its own emitted type. Could emit the booleans as a `U2<bool>`
  erased union inline at each call site, avoiding the named type
  entirely.

The third option is structurally simplest but changes the output's
F# shape (no more `Interface.UnevaluatedProperties` type — just
`option<U3<bool, JSONSchema, …>>` or similar). The first option
requires refactoring `createTransientPath` for the AnchoredAndMoored
case. The second option adds a TypeStore lookup to the LiteralUnion
render path.

### Landed: option (3), bool-only collapse

Took the third option of the three. For the specific shape that
dominated the `JsonSchemaTyped.Interface` cascade —
`boolean | OtherType` — *every* literal in the mixed-Union branch's
LiteralLike subset is a `TsLiteral.Bool`. `false | true` is
structurally F# `bool`. Collapse to the primitive in the mixed
branch:

```fsharp
let allLiteralsAreBool =
    not (List.isEmpty literals)
    && literals
       |> List.forall (function
           | ResolvedTypeLiteralLike.Literal (TsLiteral.Bool _) -> true
           | _ -> false)
seq {
    if allLiteralsAreBool then
        ResolvedType.Primitive TypeKindPrimitive.Boolean
    elif not <| List.isEmpty literals then
        // ... existing sub-Union synthesis for non-bool literals
        |> ResolvedType.Union
    ...
}
```

No sub-Union created → no path-mismatch trigger → references just say
`bool` directly. The named `Interface.UnevaluatedProperties`-style
type disappears from the output; the property's type becomes
`option<U2<bool, JSONSchema>>` (or whatever) at every call site.

**Empirical impact:**

| | Pre-fix | Post-fix | Δ |
|---|---:|---:|---:|
| `'JsonSchemaTyped.Interface'` ×38 | 38 | 0 | **−38** |
| `'JSONSchemaModule.Number'` ×14 | 14 | 0 | **−14** |
| `'JSONSchemaModule.Integer'` ×14 | 14 | 0 | **−14** |
| workers-types total | 370 | 352 | −18 |
| agents total | 1,460 | 1,417 | −43 |
| **Grand total** | **1,836** | **1,775** | **−61** |

178 generator tests pass.

The other histograms shifted modestly: agents FS0039 1,834 → 1,782
(−52), FS0033 350 → 322 (−28), FS0001 130 → 136 (+6); workers-types
FS0663 34 → 22 (−12), FS0698 20 → 12 (−8). Structurally down across
the board, no FS-code regressions.

### Non-bool mixed unions still on the path-mismatch path

Mixed unions with non-bool literals (e.g. `"foo" | "bar" | OtherType`)
still synthesize a sub-Union and still hit the path-mismatch bug
documented above. The blast radius is much smaller because the bool
case was the dominant trigger in agents (json-schema-typed re-export);
remaining surface is in workers-types `'Type'`/`'GetWithMetadata'`
families and a handful of other agents buckets.

Option (1) or (2) from the previous section is still worth pursuing
for the residual cases. Option (1) is cleaner architecturally (fixes
the asymmetry itself) but invasive (touches `createTransientPath`).
Option (2) is more localized (lookup TypeStore at render time) but
needs the LiteralUnionRender to read the scope's TypeStore — which
isn't currently threaded into the renderer.

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

### Non-bool mixed-Union path mismatch (residual)

The investigation under "Investigation and follow-on fix" above
identified the actual root cause of the
`'JsonSchemaTyped.Interface' ×38` cascade: asymmetric handling of
`scope.PathContext` between TypeRef registration and body emission.
The bool-collapse fix addressed the dominant manifestation (bool-only
literal subsets in mixed Unions). The general asymmetry still applies
to mixed Unions where the literal subset contains non-bool values
(e.g. `"foo" | "bar" | OtherType`).

Recap:

1. `createTransientPath` (`Types/RenderScope.Prelude.fs:295`) **grafts**
   `scope.PathContext` into the TypeRef's stored path
2. `Members.renderFromMembersAndFunctions`
   (`Generator/Render.Transient.fs:260`) **grafts** `scope.PathContext`
   into the TypeLikeRender's Metadata.Path
3. `renderUnionLiterals` (`Generator/Render.Transient.fs:89`) uses
   bare `TransientTypePath.Anchored` — **doesn't graft**

For non-bool mixed Unions, the sub-Union's stored TypeRef points at
`<PropertyName>.Literals` (2-seg) while the body emits at the parent's
own path (collision). Three candidate fixes documented at "What the
right fix needs to do" above:

- (1) Make `renderUnionLiterals` graft AND change `createTransientPath`
  to not append `"Literals"` for the DummyTypeKey case — symmetric
  1-segment paths.
- (2) Make `renderUnionLiterals` look up `scope.TypeStore[resolvedType]`
  at render time and use the stored path as Metadata.Path. Localised
  fix, doesn't touch shared infrastructure.
- (3) For each non-bool mixed Union, find a structural reduction
  similar to bool-collapse. Harder because non-bool string unions
  don't have a primitive equivalent.

The bool-collapse used a variant of (3) for the specific bool case.
For arbitrary string-literal unions, (1) or (2) is the path forward.

Smaller blast radius than pre-bool-fix; remaining residuals are
spread across many buckets rather than concentrated in one.

### Inner-literal emission across Interface/Class body kinds (Shayan's item 5)

Independent of the mixed-Union path mismatch. Some Interface/Class
bodies in agents (e.g. `Flat`, `_parse`, `NeedsApproval`,
`StringIterator`) contain string-union property values whose inner
types need `module rec <ParentType>` companions emitted as siblings.
The post-PR1 Literal-arm fix covers the `ResolvedType.Literal`
prerender arm; the `TypeLiteral` arm grafts via its own branch; the
`Union LiteralLike` branch grafts. But for properties of `Interface`
and `Class` bodies — entered via `Interface.render` / `Class.render`
at `Render.TypeShapes.fs` — the inner literals' emission isn't being
surfaced into the parent's anchor scope companion.

Per Shayan's PR #2 progress note: "lift inner-literal anchor across
all body kinds. The Literal and TypeLiteral arms of prerender now
graft `scope.PathContext`. Union LiteralLike already does. The
remaining gap is Interface and Class bodies that contain string-
union literals as property types."

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
