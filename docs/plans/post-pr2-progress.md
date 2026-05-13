# Post-PR2 Progress Report

> Handover note for Shayan. Picks up after PR #2
> (`prevent-silent-drops-in-export-map`) merged at commit `494a248`.
> The post-PR1 doc (`docs/plans/post-pr1-progress.md`) covers the
> earlier era and the rationale that fed into PR #2.

- **Branch:** `verify-cloudflare-sdk-pipeline` on
  `https://github.com/houstonhaynes/speakez-xantham`
- **Window:** 2026-05-13, post-merge of PR #2
- **Status:** 178 generator tests pass; 13 decoder Identifier tests pass

## Late-session snapshot (2026-05-13 evening)

Three encoder-side fixes plus one generator-side fix landed after
the multi-emission work. The verify pipeline now runs end-to-end
with the current encoder + decoder (was blocked earlier in the
session by an encoder regression that broke workers-types
encoding). All four are documented inline below; this is the
short index.

| Commit | Layer | Symptom |
|---|---|---|
| `3ae5680` | encoder | `T[]` syntactic nodes collided with the `Array` Interface TypeKey; structural `SType.Array` was discarded in duplicate resolution, leaving `ResizeArray` bare at use sites |
| `a4681bc` | generator | `Render.Collection.combine` preserved t1's TypeParameters when merging two TypeDefns; a synthetic Intersection (empty typars) that landed at the same module+name as a real Class silently dropped the Class's typars (`type ZodType =` instead of `type ZodType<'Output, 'Def, 'Input> =`) |
| `103d3ea` | encoder | `Variable.readDeclaration` fulfilled `xanTag.Builder` verbatim with `innerBuilderSignal.Value`; for declaration-merged interface+namespace pairs (e.g. `Cloudflare: Cloudflare`), the inner builder is `ValueNone`, leaving the variable's builder unfulfilled and causing `[MISSREF]` at assemble-time + `KeyNotFoundException` in the decoder |

End-of-session verify counts (current encoder + above fixes):

| SDK | Counts |
|---:|---:|
| dynamic-workflows | 5 |
| workers-types | 220 |
| agents | 1,291 |
| **Total** | **1,516** |

The agents count is +80 vs the May-11-JSON baseline (1,211) — the
encoder-fresh JSON surfaces issues the May 11 artifact was hiding,
specifically multi-emission residue and constraint cascades that
the bare-generic encoding bug was masking. The T[] fix alone
dropped agents FS0033 "use-site dropped typars" from 320 → 120
(−200) — real structural progress; the +80 net reflects the
other latent buckets becoming visible. Workers-types and
dynamic-workflows both dropped slightly.

The next concrete bucket — IndexSignature / method-return
self-reference paths (`_Lit214.Item`, `_Lit162.Type`, etc.) — is
described at the bottom of this doc under
"IndexSignature / method-return self-reference paths (next big
bucket)". I scoped the architectural fix but stopped short of
landing it; the seam map is captured for you to pick up.

# Baseline (post-PR2 era)

| SDK | Pre-PR1 baseline | End of PR1 era | Post-PR2 | After bool-collapse | After path-keyed-anchors | After any/unknown-constraint-drop | After visited-pair-keying | After empty-string-collapse | After multi-valued TypeStore | After extended-constraint-drop | Δ vs baseline |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| dynamic-workflows | 18 | 6 | 6 | 6 | 6 | 6 | 6 | 6 | 6 | 6 | −12 |
| workers-types | 1,376 | 401 | 370 | 352 | 358 | 358 | 354 | 337 | 285 | 258 | −1,118 |
| agents | 3,932 | 1,460¹ | 1,460 | 1,417 | 1,407 | 1,405 | 1,314 | 1,314 | 1,341 | 1,220 | −2,712 |
| **Total** | **5,326** | **1,867** | **1,836** | **1,775** | **1,771** | **1,769** | **1,674** | **1,657** | **1,632** | **1,484** | **−3,842 (−72%)** |

¹ End-of-PR1-era agents count was 1,441 (per post-pr1-progress.md). The
1,460 post-PR2 number reflects the cascade Shayan predicted: previously-
dropped exports now visible expose latent inner-literal anchor bugs in
Interface/Class bodies. Workers-types' −31 win dominates the net, so
the overall is still down.

The "After bool-collapse" column reflects the
*"Collapse boolean-only literal sub-Union to F# `bool`"* fix described
below under "Investigation and follow-on fix." Net −61 from post-PR2,
−92 from end of PR1.

The "After path-keyed-anchors" column reflects the structural fix to
the `RenderScope.Anchors` dict — keyed by `TypePath` instead of
`ResolvedType`, allowing the same `ResolvedType` to anchor at
multiple distinct paths when reached from different parents within
one export. Net −4 from bool-collapse, −96 from end of PR1.

The "After any/unknown-constraint-drop" column reflects dropping
`'T :> Bar` typar constraints where `Bar` resolves through to a
`Primitive Any`/`Unknown` (TS encoder collapsing TS `any`/`unknown`
or alias chains bottoming at those primitives). Net −2 from
path-keyed-anchors, −98 from end of PR1.

The "After visited-pair-keying" column reflects fixing the cycle-
prevention tracker introduced with path-keyed-anchors. The initial
`HashSet<ResolvedType>` was too aggressive — it prevented the same
shared literal from being processed at *different* anchorPaths,
defeating the path-keyed dict's purpose. Re-keyed to
`HashSet<ResolvedType * AnchorPath>` so cycles are caught (same rt
at same parent) but multi-parent emissions proceed. Net **−95
from any/unknown-constraint-drop, −193 from end of PR1**. This is
where the path-keyed-anchors refactor's intended impact actually
shows up.

The "After empty-string-collapse" column reflects redirecting the
`ResolvedType.Literal (TsLiteral.String "")` arm to the `String`
primitive prerender. The empty-string literal is structurally
indistinguishable from `string` and carries no useful constraint — TS
uses it as a placeholder default (lib.dom's
`IncomingRequestCfPropertiesTLSClientAuthPlaceholder` types 16 cert
fields as `""`). The encoder interns one shared `Literal (String "")`
and the downstream path-assignment first-wins on `TryAdd(rt, path)`,
so only one of 16 fields' anchored types ever emitted. Collapsing
the literal to the primitive eliminates the synthesis entirely;
each property's reference resolves to bare `string`. Net **−17 from
visited-pair-keying, −210 from end of PR1**.

The "After multi-valued TypeStore" column reflects the structural
refactor described in [`docs/plans/multi-valued-typestore.md`](multi-valued-typestore.md).
`TypeStore` becomes `Dictionary<ResolvedType, HashSet<TransientTypePath>>`;
Literal-typed rts accumulate every per-reference path within an
export scope; the anchor pass emits one body per path. Multi-emission
restricted to Literals because TypeLiteral extension hung agents at
60s+ via Zod V3's `DeepPartialInternal` recursive type alias (full
trace in the design doc). Net **−25 from empty-string-collapse,
−235 from end of PR1**. Categorical FS0039 wins: `CAPTURING_PHASE`,
`BUBBLING_PHASE`, `AT_TARGET`, `Stream` cleared; `Code`, `Optin`,
`Type`, `GetWithMetadata` reduced. agents +27 is *predicted inflation*
from clearing conflating issues, exposing previously-masked Zod V4
method-parameter brand-type buckets (`SuperRefine`, `Prefault`,
`Pipe`, `Overwrite`, `Catch`); forensic finding for those is notated
in the design doc for follow-on encoder/typar-rendering work.

Also landed in this window: the long-standing warning backlog
documented in [`memory: project_warning_backlog`](#) is now closed
out — build emits **0 warnings, 0 errors** on `verify-cloudflare-sdk-pipeline`
after the cleanup pass (NU1605 FSharp.Core downgrade, FS0988 empty
main, FS3511 state machine, FS0025 incomplete pattern, FS1104 ×3
`@`-identifiers, FS0044 ×6 + FS0040 deprecated/recursive in test
mocks). Test fixtures (output.json, package-lock.json) untracked
from git per the existing `tests/**/output.json` / `package-lock.json`
gitignore rules; `.xantham/` added to gitignore for decoder per-run
temp.d.ts directories.

The "After extended-constraint-drop" column reflects broadening
`Render.TypeParameter.isVacuousResolved` (formerly `isAnyLikeResolved`)
to cover ALL resolved-type kinds F# can't represent as a typar bound:
`Primitive _` (any kind, not just Any/Unknown), `Union`, `Intersection`,
`Tuple`, `Literal`, `Array`, `Index` (keyof), `IndexedAccess` (T[K]),
`TypeQuery` (typeof), `TemplateLiteral`, `Conditional`, `Predicate`,
`Substitution`, `EnumCase`. Transparent wrappers (`Optional`,
`ReadOnly`, `TypeReference` with `ResolvedType`) walk through. **Kept
out:** `TypeLiteral` (some Zod V3/V4 generics rely on inline-object
bounds; adding it could re-trigger DeepPartialInternal-style issues),
`Interface`/`Class`/`Enum`/`TypeParameter`/`GlobalThis` (real F#
constraint targets).

Net **−148 from multi-valued TypeStore, −383 from end of PR1**.
Categorical changes:
- **FS0663**: wt 34→0 (cleared), agents 206→48 (−158)
- **FS0698 (sealed)**: wt 20→0 (cleared), agents 150→54 (−96)
- **FS0660**: wt 14→0 (cleared), agents 72→10 (−62)
- **FS0033/FS0001 inflated** (predicted): agents 242→350 (+108) and
  186→200 (+16) — downstream type-mismatch errors previously masked
  by the constraint errors now surface.

178 generator tests pass; build stays at 0 warnings, 0 errors.

The earlier history-comment in `Render.TypeParameter.fs` recorded that
a *much broader* filter (any `Intrinsic` target at the rendered-atom
level) regressed +31 by breaking V3 ZodType emission. That broader
filter matched on rendered `TypeRefAtom.Intrinsic` (catching
`proptypekey<X>`, `obj`, etc. as bounds), which leaked load-bearing
constraints. This refactor stays at the resolved-type level so
those rendered constructs are not implicated.

## Investigated, not fixed: bare-generic return types (~206 FS0033 errors)

The dominant remaining FS0033 bucket is bare-generic emission of
`T[]`-style return types: `ResizeArray<_> expects 1 type argument(s)
but is given 0` ×192 (agents) + `IReadOnlyList<_> expects 1...` ×14
(agents). Affects every `T[]` method return inside synthesised
TypeLiteral bodies for Array<T>'s methods (`push`, `unshift`,
`toSpliced`, `toSorted`, `toReversed`, `with`, etc.) and similar
`readonly T[]` callbacks.

Same family appears in the bucket-list under FS0039 as bare-generic
references on class self-returns (`abstract Create: ... -> Server`
where Server has 3 typars, ditto Protocol, ExperimentalServerTasks,
Client, ZodSetInternals, ZodPromiseInternals, ZodOptionalInternals,
ZodNullableInternals, ZodLazyInternals, ZodDefaultInternals,
ZodCatchInternals).

Empirical observation:
- Parameter types of the SAME shape render correctly. Example
  contrast in workers-types (`Array<T>` synthesised TypeLiteral):
  - `abstract concat: [<ParamArray>] items: ResizeArray<ConcatArray<'T>> -> ResizeArray`
  - The parameter is parameterised correctly (`ResizeArray<ConcatArray<'T>>`)
  - The return is bare `ResizeArray`.
- `IterableIterator<T>` (a regular TypeReference) renders with
  args; `T[]` (`ResolvedType.Array`) on the same return position
  renders bare.

Hypothesis (untested): the issue is downstream of the
`ResolvedType.Array` prerender in `RenderScope.Prelude.fs:507`
where `(lift Intrinsic.array, [innerPrerender])` is constructed
correctly. Args appear to survive `anchor`/`localise` passes
(both preserve `Prefix(prefix, args)` shape). Suspects: the
self-reference collapse in `Render.TypeAlias.fs`
(`mapAtomsWithPrefixCollapse` drops args when the prefix rewrites
to `obj`), or a similar collapse applied to method-return-type
positions specifically. Requires a debug-log instrumented run
to isolate.

Forensic notes for Shayan (deferred):
- Encoded TS shape for these methods uses `TsType.Array (T)` for `T[]`
  syntax. The encoder DOES NOT collapse this to `TypeReference Array<T>`.
- The TypeRefRender for `TypeParameter T` is a Widget atom carrying
  `'T` as a LongIdent.
- Constructing the outer `Prefix(Intrinsic "ResizeArray", [Widget 'T])`
  via the SRTPHelper dispatch resolves to `createPrefix` and produces
  a correct `Prefix_(...)` molecule. The args list is non-empty.
- Yet the final rendered F# has bare `ResizeArray`. So args are being
  stripped between this construction and the emission.

Scope:
- This bucket is structurally similar to but distinct from the
  multi-emission and constraint-drop work landed in this branch.
- Likely requires a debug stream (the file-based logger pattern from
  multi-valued-typestore.md investigation) to identify the exact
  stripping site.
- Deferred to a separate investigation pass.

### Follow-up investigation: TypeAliasRemap is the proximate cause (and the half-fix that doesn't hold)

Using the file-based debug-stream pattern, the proximate cause of
the bare-generic emission was identified:

`RenderScope.Prelude.fs:685-688` — after every prerender path, if
the rt's TypeKey is in `ctx.TypeAliasRemap`, the registered render
is REPLACED with the remap target ref:

```fsharp
|> function
    | Registered { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Raw) ->
        ctx.TypeAliasRemap[lazyResolvedType.Raw]
        |> TypeRefRender.orNullable nullable
    | Registered ref -> ref
```

For generic substitutions like `Array → ResizeArray` (arity 1) and
`ReadonlyArray → IReadOnlyList` (arity 1), the remap target is a
**bare intrinsic atom** built via `intrinsicRef`
(`Types/Generator.fs:205`). When the body's natural prerender
produces `Prefix(ResizeArray, [T])`, the remap discards it and
returns just the bare `ResizeArray` atom — losing the typar arg.

**Naive fix (tried, reverted +40 regression):** Skip
`addTypeAliasRemap` for the bodyTypeKey when the substitution
has arity > 0. The body's natural Array-arm prerender produces
`Prefix(ResizeArray, [T])` directly without the remap clobber.

Tested empirically: dropped some bare cases (32 → ~32 — the count
held but byte-count grew by ~85KB indicating many newly-correct
emissions). But agents FS0039 went UP by ~76 because cross-context
references (where the typar 'T isn't in the current scope) now
emit `ResizeArray<'T>` with a dangling `'T` — same shape, different
failure mode.

**The real shape of the problem:** the remap is doing two things at
once. (a) For LOCAL references within the alias's own context
(where typars are in scope), the args need to be preserved.
(b) For CROSS-CONTEXT references (from sites without those typars
in scope), the args need to be stripped so 'T doesn't dangle.

The current remap collapses both to (b). The naive fix collapses
both to (a). Neither is correct.

A proper fix needs to discriminate at the reference site whether
the typars are in scope. Information not easily available at the
remap point. Either:
- Defer the remap decision to a later pass where context is known.
- Eagerly substitute typars at the body-alias-registration time
  with an `obj` placeholder for cross-context cases, while
  preserving the original Prefix for local-context cases (would
  need two cached versions per remapped rt).

Reverted to keep baseline numbers stable. The file-based
investigation pattern is documented and reusable; deferring to a
focused pass with deeper encoder/renderer cooperation.

### Root cause and source fix: `T[]` collides with `Array` Interface TypeKey

The TypeAliasRemap clobber turned out to be a downstream symptom.
The actual root cause is in the encoder, and the bare-generic
emission for `T[]` returns has a clean source fix.

**Forensic trail (file-based instrumentation at three layers):**

1. Logged the final `prerender` return for any `Atom(Intrinsic
   "ResizeArray")` — fired only 2 times across the whole agents
   regen. So bare-ResizeArray is not a direct prerender return.

2. Logged `Method.render`'s `returnType` when it's a bare
   ResizeArray atom — fired **88 times in agents**. All 88 have
   `method.Type.Value = ResolvedType.Interface(Array)` directly
   (NOT `ResolvedType.Array` or `ResolvedType.TypeReference`).

3. The 88 fires bucket cleanly:

   | Method | Count | `method.Type.Raw` |
   |---|---|---|
   | `with`, `toSpliced`, `toSorted`, `toReversed`, `splice`, `slice`, `reverse`, `concat` | 11 each | **65** |

   `8 methods × 11 synthetic TypeLiteral copies = 88`. TypeKey 65
   is the Array Interface itself. The method's `Type` field points
   to the bare Array Interface — no TypeReference wrapper, no
   typar application.

**Why this happens (encoder-side):**

`Reading/Dispatch/TypeNode.fs` dispatches `TypeNode.ArrayType`
(syntactic `T[]`) and calls `setTypeKeyFromNode arrayTypeNode`,
which evaluates `ctx.checker.getTypeFromTypeNode arrayTypeNode |>
_.TypeKey`. TS's TypeChecker returns the same `Ts.Type` for `T[]`
and the `Array<T>` Interface declaration — they share `type.id`.
So the SType.Array structural form (built from the syntactic node)
and the Interface declaration (built from the Array symbol) both
register TypeStore entries at the same TypeKey.

The duplicate-resolution pass picks one winner. The Interface wins
on identity priority (its declaration position is more authoritative
than the synthetic SType.Array). All references to TypeKey 65
resolve to the Interface — losing the element-type application.

When the Array Interface's `resolvedTypePreludeInterceptor` then
substitutes `Array → ResizeArray` (arity 1) and returns the bare
intrinsic atom, the use site has no remaining `<T>` to apply, and
emits bare `ResizeArray`.

**Fix at source (Xantham.Fable encoder):**

`Reading/Prelude.fs` already has a `usesGeneratedKey` list of tag
kinds that need a fresh TypeKey for exactly this reason —
TypeChecker collapsing distinct syntactic nodes onto a shared
semantic TypeKey. The list includes `UnionType`, `IntersectionType`,
`TypeQuery`, `TypePredicate`, `TypeReference` (with args). It was
missing `ArrayType`.

Added `XanTagKind.TypeNode (TypeNode.ArrayType _) -> true` so each
`T[]` syntactic node gets its own TypeStore entry distinct from
the Array Interface. With this change, the SType.Array entry no
longer collides with the Interface — references to `T[]` resolve
to the structural Array form, which goes through
`ResolvedType.Array` in the generator and builds
`Prefix(ResizeArray, [T])` correctly.

Companion change in `Reading/Dispatch/TypeNode.fs`:
- Set TypeSignal to `ctx.signalCache[xanTag.IdentityKey].Key` (the
  generated key), so parents embed the structural-entry key in
  their Type fields. (`setTypeKeyFromNode` would have embedded the
  semantic key, defeating the purpose.) Same pattern as `UnionType`.
- Force-push the semantic Array type so its declaration dispatcher
  (`TypeFlagObject.Interface`) still registers an entry at the
  semantic key for direct Array Interface references. Same pattern
  as `UnionType` / `IntersectionType`.

**Encoder MISSREF blocker — also fixed (`Variable.readDeclaration`):**

Re-encoding workers-types with the current encoder triggered a
`[MISSREF] TypeKey -N` at `workers-types/index.ts:466:22` (the
`export declare const Cloudflare: Cloudflare;` line). The decoder
then threw `KeyNotFoundException` during `compressResult` because
the dangling generated TypeKey was referenced from members but
had no entry in the types map.

Root cause: declaration-merged interface + namespace pairs share
a name. The variable's type annotation `: Cloudflare` references
the merged identity; neither the Interface nor the Namespace
dispatcher populates the VariableDeclaration's TypeStore Builder
under the generated key. `Variable.readDeclaration` was fulfilling
`xanTag.Builder` with the inner type's builder via
`Signal.fulfillWith(fun () -> innerBuilderSignal.Value)` — if
the inner is `ValueNone`, the variable's builder stays
`ValueNone`, surfaces as MISSREF at assemble-time, and downstream
the decoder choked.

Fix: in `Variable.readDeclaration`, fall back to
`SType.Primitive NonPrimitive` (renders to F# `obj`) when the
inner builder is `ValueNone`. Same safe placeholder used elsewhere
for unresolvable references. Eliminates the MISSREF; downstream
references to the variable's type resolve to `obj` rather than
crashing the decoder.

**Verification status (post-MISSREF-fix):**

End-to-end verify pipeline runs cleanly. Re-encoded all three
SDKs with the current encoder + my fixes; counts:

| SDK | Before encoder regen | After |
|---|---|---|
| agents | 1211 | 1291 |
| workers-types | 223 | 220 |
| dynamic-workflows | 6 | 5 |

The T[] encoder fix took effect: agents FS0033 "use-site dropped
typars" dropped from 320 → 120 (−200). But total agents went up
by 80 because the fresh encoder also surfaces additional FS0039
multi-emission residue (Item: 38→92, Type: 30→40, etc.) and
FS0001 type-mismatch cases that were previously masked. Net
across the three SDKs: 1440 → 1516 (+76), but with the
structural fixes truly applied — the May 11 JSON had been masking
both the T[] bug AND the surfacing of other issues.

### Sibling fix: declaration drops typars when synthetic `TypeDefn` lands at the same module+name

The same FS0033 bucket has a second sub-pattern: the *declaration*
emits without its typars even though the body references them.
Largest concentration: `type ZodType =` (Zod V3 class) at agents
line 17011, with body methods using `'Def`, `'Output`, `'Input`
that don't resolve. 74 of 84 "does not expect any type arguments,
but here is given N" errors in agents trace to this single
declaration.

**Forensic trail:**

1. Logged `Class.render` / `Interface.render` for shape name
   `"ZodType"`: each fired exactly **once**, both with 3 typars
   (V3 Class: `Output,Def,Input`; V4 Classic Interface:
   `Output,Input,Internals`). So the named declarations carry
   their typars correctly out of the shape-render layer.

2. Logged the `module'.Types.TryAdd` collision site in
   `Render.Collection.fs:286` for any `"ZodType"` name. Three
   modules report adds/merges — V3Types, V4ClassicSchemas,
   V4CoreSchemas. V3Types saw two entries:

   ```
   MODULE-ADD   V3Types ZodType shape=TypeDefn[0]inh=0   ← first
   MODULE-MERGE V3Types ZodType existing=[0]inh=0 incoming=[3]inh=0
   ```

   The first entry has **zero typars and zero inheritance**.
   The incoming real Class has 3 typars. The merge guard at
   line 127 (`when t1.Inheritance = t2.Inheritance`) passes
   because both Inheritance lists are empty. Merge fires, and
   the `{ t1 with Constructors = ...; Members = ...; Functions = ... }`
   reshaping preserves `t1.TypeParameters` (empty). The real
   class's 3 typars are silently dropped.

   The V4ClassicSchemas case shows the *correct* behaviour by
   coincidence: existing `[3]inh=1` vs incoming `[0]inh=0`, so
   the Inheritance equality guard *fails*, the fallback
   `| _ -> primary` returns `t1` (the 3-typar entry), and the
   synthetic 0-typar entry is discarded. Typars preserved.

3. Source of the synthetic 0-typar entry:
   `Render.Transient.fs:274-285` — `Members.renderFromMembersAndFunctions`
   produces a `Transient.TypeRender.TypeDefn` with
   `Name = ValueNone`, `TypeParameters = []`, `Inheritance = []`,
   `IsClass = false`. Used by `Intersection.render` to flatten
   `A & B` into a single TypeLiteral-like shape. When the
   resulting Transient is anchored via `RenderScope.Anchored.fs:147-183`,
   the `Name = ValueNone` fallback at lines 160-165 reads the
   leaf segment of the anchor path. If that path tail happens
   to be `"ZodType"` (e.g. an Intersection authored against
   Zod V3 `ZodType` lands its anchor at the Class's own path),
   the synthetic gets named `"ZodType"` and lands at module
   `V3Types` — colliding with the real Class.

**Fix at source (this branch):** `Render.Collection.fs` `combine`
function — when merging two `TypeDefn`s, preserve the side with
non-empty `TypeParameters` rather than always taking `t1`'s. The
synthetic's empty typar list no longer wins when merged with a
real declaration's populated list.

```fsharp
let typeParameters =
    match t1.TypeParameters, t2.TypeParameters with
    | [], rhs -> rhs
    | lhs, _ -> lhs
{ t1 with
      TypeParameters = typeParameters
      Constructors = ...
      Members = ...
      Functions = ... }
```

Mirrored in the `TypeAlias.TypeDefn` arm.

**Effect on agents FS0033:**
- "does not expect any type arguments": **84 → 10** (−74)
- "expects N args but given 0": 266 → 320 (+54, downstream
  exposure)

The bare-generic bucket goes 350 → 330 in agents. Net agents
total +2 (1209 → 1211) — the synthetic-merge fix corrects the
declarations, exposing other latent issues (e.g. inheritance
clauses and constructor-return positions that drop typars at the
USE site for classes that previously had no typars; the same
encoder-side `T[]` bug for those classes). Workers-types
unchanged (the pattern doesn't trigger there). Conceptual win
even if the count moves slightly: the typar information now
flows correctly through the data structures.

**Open follow-on for Shayan:**
- The deeper question is *why* an Intersection.render anchors at
  the same path as the Class declaration whose body it derives
  from. The synthetic should anchor at a sibling path (e.g.
  `ZodType.<intersection-tag>` or a `_Lit*` slot), not at the
  Class's own path. Investigating that would prevent the
  collision rather than just resolving it on merge.
- Inheritance equality could be a weak guard — two entities
  that *should not* merge (a class declaration and a synthetic
  TypeLiteral) just happened to both have empty `Inheritance`.
  A stronger guard might check `IsClass` agreement or whether
  both sides carry a Source.

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

## `<path-keyed-anchors>` — switch `RenderScope.Anchors` from ResolvedType-keyed to TypePath-keyed

Tackles the shared-multi-position-literal-within-one-export cascade
that the bool-collapse fix sidestepped for the specific bool subset.

**The bug shape**: `RenderScope.Anchors : Dictionary<ResolvedType,
TypePath * Render>` deduped emissions by `ResolvedType`. When the
same `ResolvedType` (e.g. the shared string-literal `"object"`)
was reached from multiple parents within one export's anchor pass —
e.g. _Lit79 contains _Lit246 and _Lit247, both with property
`type: "object"` — `Dictionary.tryAdd` accepted only the FIRST
parent's emission. Body emitted at `<parent>._Lit246.Type`;
references from _Lit247 expected `<parent>._Lit247.Type` and
failed FS0039.

**The fix**: change `Anchors` to
`Dictionary<TypePath, Render>` — keyed by the anchored emission
path rather than the source `ResolvedType`. Multiple emissions of
the same `ResolvedType` at distinct paths now coexist; only the
PATH itself is deduped (so the same final location can't be claimed
twice).

Cycle prevention moves from the `anchors.ContainsKey` filter
(which previously doubled as the duplicate-emission dedup) to a
separate `visited: HashSet<ResolvedType>` threaded through the
`anchor` recursion. Allocated fresh per top-level anchor pass.

`Render.Collection.fs` consumers updated: `x.Anchors.Values |>
... |> Array.map (fun (typePath, render) -> ...)` becomes
`x.Anchors |> ... |> Array.map (fun (KeyValue(typePath, render)) ->
...)` — the path was previously paired with the render in the
value tuple, now it's the key.

**Empirical impact:**

| | Pre-fix | Post-fix | Δ |
|---|---:|---:|---:|
| workers-types | 352 | 358 | +6 |
| agents | 1,417 | 1,407 | −10 |
| **Total** | **1,775** | **1,771** | **−4** |

178 generator tests pass. Specific references previously failing
(e.g. `_Lit247.Type` at agents.wrapped.fs:2872) now resolve. Some
sibling buckets (`'Brand' ×20`, `'StringIterator' ×16`) cleared
entirely. New FS0033/FS0663 cases surfaced (+38 combined) — likely
the duplicate emissions exposed typar-constraint shapes that
weren't reachable before, similar to the cascade pattern Shayan
predicted for PR #2.

Net is small. The structural change is sound (the asymmetry was
real), but most of the cascade was already handled by other
mechanisms (e.g. the synthetic-anchored branch uses ConcretePath
refs that don't depend on the parent's anchor).

### Landed follow-up: drop `Any`/`Unknown`-resolved constraints (narrow filter)

After the broader Intrinsic-target filter regressed (next section),
re-attempted with a narrower filter on the underlying
`LazyResolvedType` rather than the rendered `TypeRefRender`:

```fsharp
let rec private isAnyLikeResolved (rt: ResolvedType) =
    match rt with
    | ResolvedType.Primitive TypeKindPrimitive.Any
    | ResolvedType.Primitive TypeKindPrimitive.Unknown -> true
    | ResolvedType.Optional opt -> isAnyLikeResolved opt.Type.Value
    | ResolvedType.ReadOnly inner -> isAnyLikeResolved inner
    | ResolvedType.TypeReference tr ->
        match tr.ResolvedType with
        | Some resolved -> isAnyLikeResolved resolved.Value
        | None -> false
    | _ -> false
let isVacuousLazyConstraint (lazyTy: LazyResolvedType) =
    isAnyLikeResolved lazyTy.Value
```

Walks the resolved-type graph through transparent wrappers
(`Optional`, `ReadOnly`, `TypeReference`-with-`ResolvedType`) so an
alias chain like `AgentContext → DurableObjectState → Primitive
Any` is caught even though the prerendered `TypeRefRender` is a
`ConcretePath` to the alias name. Applied in
`Render.TypeParameter.render` (the prelude builder, before
prerendering the constraint) and in the function-export-specific
TypeParameterRender construction at
`RenderScope.Anchored.fs:725` (which builds an Anchored typar
directly, bypassing TypeParameter.render).

**Empirical impact:**

| | Pre-attempt | Post-attempt | Δ |
|---|---:|---:|---:|
| FS0663 (agents) | 216 | 198 | **−18** ✓ |
| FS0698 (agents) | 158 | 146 | **−12** ✓ |
| FS0660 (agents) | 78 | 72 | **−6** ✓ |
| FS0039 (agents) | 1,738 | 1,770 | **+32** (unmasked) |
| **Agents total** | **1,407** | **1,405** | **−2** |

Net −2. The intended FS0663/FS0698/FS0660 drop (−36) is real;
FS0039 increase (+32) is references that previously got masked by
the early constraint failure now becoming visible. 178 generator
tests pass.

Modest delta but structurally correct — the `'T :> obj` /
`'T :> obj option` constraints are vacuous-or-unsatisfiable in F#
and shouldn't be emitted.

### Attempted follow-up (broader Intrinsic-target filter, reverted)

The path-keyed-anchors fix re-exposed the FS0663 / FS0698 cascade
documented in post-pr1-progress.md: `'T :> AgentContext` where
`AgentContext` resolves to `obj option` (sealed), plus `'T :> obj`
and other Intrinsic-target constraints F# rejects.

Tried lifting a `isVacuousPreludeConstraint` helper to the
top-level `Render` module checking `TypeRefKind.Atom
(TypeRefAtom.Intrinsic _)`, threaded through three call sites:
`Render.TypeParameter.render`, both
`Render.{Transient,Concrete}.anchorTypeParameters`, and the
function-export-specific construction in
`registerAnchorFromExport`.

Result: total **1,771 → 1,802 (+31)** regression.

| Code | Pre-attempt | Post-attempt | Δ |
|---|---:|---:|---:|
| FS0663 (agents) | 216 | 144 | **−72** ✓ |
| FS0698 (agents) | 158 | 118 | **−40** ✓ |
| FS0660 (agents) | 78 | 42 | **−36** ✓ |
| FS0033 (agents) | 348 | 444 | **+96** ✗ |
| FS0001 (agents) | 134 | 134 | 0 |
| FS0661 (agents) | 6 | 24 | **+18** ✗ |
| FS0039 (agents) | 1,738 | 1,810 | **+72** ✗ |
| WT FS0033 | 96 | 120 | +24 |
| WT FS0663 | 34 | 26 | −8 |

The constraint-drop did clear the intended FS0663/FS0698/FS0660
cascade (−148 combined). But the typars-now-unconstrained exposed
typar-arity mismatches elsewhere (FS0033 +96 +24 = +120,
FS0661 +18, FS0039 +72) — including breaking V3 ZodType emission
(`type ZodType =` with no typars but body uses `'Def`).

Reverted. The check needs to be narrower than "any Intrinsic
constraint":

* Drop only `obj` (Intrinsic.obj) — vacuous, can't break anything
  downstream because no typar can be inferred to satisfy it.
* Leave `string`/`int`/other primitive-name intrinsics alone —
  these turn out to be load-bearing somehow (probably as
  type-arg constraints F# DOES allow despite the FS0698
  appearance, or constraints encoded as Intrinsic that aren't
  actually emitted as `:>` in F#).

The narrower variant is a follow-up. Diff stayed clean on the
revert; no code changes from this attempt landed.

## `<visited-pair-keying>` — fix cycle-prevention tracker to allow per-anchor emissions

The path-keyed-anchors refactor was supposed to enable a single
shared `ResolvedType` (e.g. the literal `"object"` used as a TS
discriminator across many JSON Schema variants) to emit at distinct
paths — one under each parent that references it. The dict became
`Dictionary<TypePath, Render>` exactly so paths could differ.

But the cycle-prevention tracker I added alongside the refactor was
`HashSet<ResolvedType>`, keyed only by ResolvedType. The recursion's
guard `if visited.Add resolvedType then ... else skip` meant the
SECOND parent's recursion would silently skip the shared literal —
defeating the entire point of switching to path-keyed. The earlier
modest +/-4 result reflected this self-defeating behavior.

The fix: re-key the visited tracker by `(ResolvedType, AnchorPath)`.
Same `(rt, parent)` pair still terminates cycles. Different parents
of the same rt → process again, anchor at the new parent's
location. Path-keyed dict accepts the second entry (different path)
and the body emits at both locations.

```fsharp
let rec anchor
    (ctx: GeneratorContext)
    (visited: HashSet<ResolvedType * AnchorPath>)
    anchors anchorPath resolvedType =
    if visited.Add (resolvedType, anchorPath) then
        GeneratorContext.Prelude.tryGet ctx resolvedType
        |> ValueOption.iter (anchorPreludeAnchorScope ctx visited
                              (Some anchors) anchorPath resolvedType-renderScope)
```

`HashSet<ResolvedType * AnchorPath>` uses F#'s default tuple equality
(combines each side). `ResolvedType` is `[<ReferenceEquality>]`,
`AnchorPath` is a structural DU — the combined hash works
correctly for set membership.

**Empirical impact:**

| | Pre-fix | Post-fix | Δ |
|---|---:|---:|---:|
| FS0039 agents | 1,770 | 1,610 | **−160** |
| FS0033 agents | 348 | 266 | **−82** |
| FS0001 agents | 134 | 168 | +34 |
| **agents** | **1,405** | **1,314** | **−91** |
| FS0039 workers-types | 506 | 498 | −8 |
| **workers-types** | **358** | **354** | **−4** |
| **Total** | **1,769** | **1,674** | **−95** |

Top FS0039 bucket changes in agents:
- `'Type' ×82 → 38` (−44)
- `'Brand' ×20 → 0` (cleared)
- `'StringIterator' ×16 → 0` (cleared)
- `'State' ×26 → 12` (−14)
- `'_Lit85' ×20 → 0` (cleared)
- `'Code' ×48 → 42` (−6)

The FS0001 +34 is type-mismatch errors newly exposed where the
previously-missing inner type now resolves, but its actual shape
doesn't match expected at the use site (the predicted layered
cascade Shayan flagged in PR #2's progress note).

178 generator tests pass.

This is what the path-keyed-anchors refactor was always supposed
to do — the previous +/-4 result was an unintended self-block from
my over-aggressive cycle guard.

## `<empty-string-collapse>` — collapse `""` literal to `string` primitive

`src/Xantham.Generator/Generator/RenderScope.Prelude.fs:348`. The
`ResolvedType.Literal (TsLiteral.String "")` arm now redirects to
the `String` primitive prerender instead of producing a synthesized
single-case StringEnum:

```fsharp
| ResolvedType.Literal (TsLiteral.String "") ->
    ResolvedType.Primitive TypeKindPrimitive.String
    |> LazyContainer.CreateFromValue
    |> prerender ctx scope
    |> RenderScope.createRootless resolvedType
    |> addOrReplaceScope ctx resolvedType
```

**Why this fix and not a broader interning fix.** The encoder
interns one shared `ResolvedType.Literal (String "")` across every
TS site that types a property as `""`. Downstream, the path-assignment
pre-pass writes into `scope.TypeStore` via `TryAdd(resolvedType, path)`
— first-wins. For the lib.dom `IncomingRequestCfPropertiesTLSClientAuthPlaceholder`
interface (16 of 17 cert fields typed `""`), this meant 15 of 16
property-derived anchored types were silently dropped; their
references (`CertNotAfter`, `CertFingerprintSHA256`, …) dangled
and produced 16 FS0039 errors per consumer of the type. Workers-types,
agents (via re-export), and dynamic-workflows all paid.

The right fix at the storage layer would be either (a) keying
`TypeStore` by `(rt, path)` so all 16 paths survive interning, or
(b) emitting one canonical type and having all 16 properties
reference it. (a) is structurally invasive — `TypeStore` is read by
many downstream renderers that assume the rt-key invariant —
and (b) requires reworking the property-name-derived path
assignment. Neither is local. But the *semantic* observation is
that `""` is a vacuous constraint: a property typed "must always
be the empty string" is just `string` from a binding consumer's
perspective — the value-shape doesn't constrain anything useful.
Collapsing at the Literal-arm sidesteps the entire interning-vs-
multiplicity problem because there's nothing left to dedup.

Narrower than collapsing all string literals: `"NONE"`, `"0"`,
`"json"`, `"json-schema-typed"` etc. all encode meaningful value
constraints (sentinel return values, discriminator tags). Those
remain as single-case StringEnums.

**Empirical impact:**

| | Pre-fix | Post-fix | Δ |
|---|---:|---:|---:|
| workers-types | 354 | 337 | **−17** |
| agents | 1,314 | 1,314 | 0 |
| dynamic-workflows | 6 | 6 | 0 |
| **Total** | **1,674** | **1,657** | **−17** |

All 16 TLS placeholder properties (`certNotAfter`, `certNotBefore`,
`certFingerprintSHA256`, `certFingerprintSHA1`, `certIssuerSKI`,
`certSKI`, `certIssuerSerial`, `certSerial`, `certSubjectDNLegacy`,
`certIssuerDNLegacy`, `certSubjectDNRFC2253`, `certIssuerDNRFC2253`,
`certSubjectDN`, `certIssuerDN`, `certSubjectDN`, …) now emit as
`abstract certX: string with get, set`. The remaining 2 in that
interface (`certRevoked: "0"` and `certPresented: "0"`) are
exhibiting the same pattern under a `"0"` literal — `certRevoked`
still dangles. That's a smaller bucket and would need either the
storage-layer fix above or a different surgical move (e.g. a
property-aliasing pass) — out of scope for this fix.

178 generator tests pass.

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

### IndexSignature / method-return self-reference paths (next big bucket)

After landing the three encoder fixes this session, the dominant
remaining FS0039 bucket in agents is path references like
`_Lit214.Item`, `_Lit122.Code`, `_Lit197.Catch`,
`WorkflowName.Item.Key` etc. — `<Type>.<Member>` paths where the
parent is an emitted Interface/Class/TypeLiteral but no companion
nested module exists to host the referenced inner name.

Concrete example (agents.wrapped.fs ~line 1986):

```fsharp
type _Lit214 =
    abstract Item: k: string ->
        U2<proptypekey<proptypekey<'T, _Lit214.Item>, _Lit214.Item>, option<obj>>
```

The `_Lit214.Item` reference is the *return type of `_Lit214`'s
own `Item` index signature*, used recursively from inside the
return type itself. F# can't resolve it because `_Lit214` is an
interface (not a module) — there's nothing at `.Item` as a type.

Top buckets in agents this shape produces:
- `'Item' ×92` — index signatures
- `'Type' ×40`, `'Code' ×32`, `'TypeName' ×30`, `'Invoke' ×28`,
  `'Optout' ×24`, `'Optin' ×22`, `'Flat' ×22` — method/property
  return-type self-references

Compare with the cases that *work* today, e.g. `_Lit162.Code`
(agents.wrapped.fs ~line 278):

```fsharp
module rec _Lit162 =
    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Code = | [<CompiledName("invalid_value")>] InvalidValue

type _Lit162 =
    ...
    abstract code: _Lit162.Code with get, set
```

The literal-string-union case produces a *companion module* with
a nested `type Code` alias — and consumers of `_Lit162.Code`
resolve cleanly. The architecture supports this pattern; it's
just not extended to IndexSignature returns and method-return
self-references.

**Architectural fix path (for Shayan):**

The clean fix is a *pre-pass* that walks Interface/Class/TypeLiteral
resolved types BEFORE the Render-lazy force phase, identifies
IndexSignature members (and method-returns that contain
self-referencing path atoms), and synthesizes companion
`ResolvedExport.TypeAlias` entries anchored at
`<parent>.<MemberName>`. Those get picked up by the standard
anchor-registration pass and emitted into the parent's module
namespace.

The seams I traced while scoping this:

1. **`IndexSignature.render`** (`Render.Member.fs:181-218`) —
   currently produces only a `MemberRender.Method` at member path
   `Anchored.Moored "Item"`. Doesn't emit a companion TypeAlias
   for the return type. Would need a parallel emission, but a
   `MemberRender` value is single-valued — composing with an
   additional TypeAlias requires either a richer return shape or
   a side-channel.

2. **`ctx.AnchorRenders`** (`Types/Generator.fs:26,426`) — the
   dictionary that drives final emission. Keyed by
   `Choice<ResolvedType, ResolvedExport>`. Adding a synthetic
   anchor entry needs a *key*. A fabricated `ResolvedExport.TypeAlias`
   built around the IndexSignature's return type would work but
   creating one requires (a) a fresh `TypeKey`, (b) a `Source`
   inherited from the parent, (c) routing through `pipeTypeAlias`
   for path interception. Easier said than done from inside
   `Render.Member.fs`.

3. **The lazy-forcing model** (`RenderScope.Anchored.fs:541-569`,
   `:571+`) — `anchorPreludeExportScope` iterates `TypeStore` per
   export; `registerAnchorFromExport` runs per export and forces
   the `Render` lazy via the `Anchors = anchorPreludeExportScope`
   binding. Adding to `ctx.AnchorRenders` from *inside* a `Render`
   lazy risks ordering bugs — the iteration that ought to pick up
   the new anchor may already have finished. Hence the need for
   a pre-pass, not in-place injection during member render.

4. **`prerenderFromGraph`** (`RenderScope.Prelude.fs:768-...`) —
   the top-level pre-render entry point. Already does one pre-pass
   (`prerenderTypeAliases`) to seed the `TypeAliasRemap`. A second
   pre-pass to seed synthetic companion exports for IndexSignatures
   would be parallel in shape.

5. **`TypeLikeRender.renderInterface` / `renderAbstractClass`**
   (`TypeRender.Render.fs:589, :628`) — final emission. The
   `members @ functions` list could be augmented at this layer
   with companion type aliases, but by emission time the anchor
   dictionary is already locked in. So the fix must land earlier.

**Why the same pattern recurs for non-IndexSignature members:**
the `'Type'` / `'Code'` / `'Invoke'` etc. buckets are method
returns where the resolved type internally embeds an Atom whose
path is `<Self>.<MethodName>` — the encoder is treating the
method's signature as if its return type were a nested type alias
of the same name. Same architectural fix applies: synthesize a
nested module with type aliases for those self-referencing names.

**Investigation overhead estimate:** ~4–8 hours of focused work
to land cleanly, including the synthesis-key fabrication, the
pre-pass plumbing, and validation that it doesn't regress the
existing companion-module pattern (e.g. `_Lit162.Code`).
Probably worth Shayan's design judgment on whether the synthesis
should live at the encoder layer (richer `ResolvedExport` shapes)
or the generator layer (synthetic anchors only).

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
