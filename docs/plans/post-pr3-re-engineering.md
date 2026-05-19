# Post-PR3 Re-Engineering Notes

> Working document. Companion to `post-pr3-progress.md` (the status snapshot)
> and the design docs in `docs/Xantham.Generator/`. This doc focuses on the
> *principled re-engineering* happening at specific composition points in
> the generator pipeline — what's landed, what was attempted and revealed
> deeper structure, and what the staged path forward looks like.

## Why a separate doc

`post-pr3-progress.md` tracks the project state. This doc captures **design
findings** that surfaced as the 12 SDKs were driven through the verify
pipeline, where established design idioms (`atom/molecule` from
[typeref-render.md](../Xantham.Generator/typeref-render.md), the Path
system, ArenaInterner identity) needed extension or restoration to handle
shapes the original framing didn't yet commit to.

The work here stays inside Shayan's architecture. The Pilot/Farscape ethos
applies: there are two cleanly-modeled concerns (what a type *is* vs. how
it's *referenced*), and we fill in composition rules at the boundary where
those concerns meet new TS-source idioms.

## What's landed (the principled fixes)

| Fix | Principle restored / extended | Commit |
|---|---|---|
| Backtick-free identifier renamer | Consumer-facing F# names should be clean; original TS name persists via Fable interop attribute (`[<EmitMethod>]` / `[<EmitProperty>]` / `[<CompiledName>]`) | `e5feed6` |
| Self-ref `TsType.TypeQuery` cycle break | Cycle-break belongs at decoder layer where the lazy graph forces; downstream rendering shouldn't trip on un-broken self-refs | `2f22b15` |
| `Source.UnknownDeclared` codec + consumer-site exhaustiveness | Decoder-side DU additions need wiring through every consumer match — the case being defined is a contract, not a marker | `5098692`/`12fb376`/`4342c19`/`2f22b15` |
| Leading-dot member rename + camel-context keyword pass | `Identifier.toSafe` is the right layer for source-text-to-F#-identifier translation; case-pipeline ordering must keep the second-pass keyword guard | `5916873` |
| StringEnum case-name disambiguation | Union-case identity is per-emission, not per-source-literal; collisions get numeric-suffix dedup with `[<CompiledName>]` preserving the source value | `42df1f0` |
| `Implements` separation; one `inherit` per class; module-as-type vs interface collision; `AllowNullLiteral` heritage discipline | `TypeLikeRender` needs separate channels for class-extends vs class-implements; the emission rules for each are distinct in F# | (combined) |
| Lib-substitution additions (Intl.\*, ArrayLike, EventListenerOrEventListenerObject, +others) | The `LibEsDefaults.substitutions` table is the documented seam for naming-divergence between TS lib.es and F# stdlib equivalents | (combined) |
| `TypeAliasRemap` molecule preservation | The remap is an identity-stabilisation cache, not a wholesale-replacement rule. Per atom/molecule composition, it stores the canonical *atom* for an alias and references compose `Prefix(canonical, args)` molecules; the final-pass replacement must preserve `Prefix` wrappings | `7b3bafa` |
| `Prefix(obj/exn, args)` collapse | Non-generic intrinsics can't carry typar args; cycle-break/substitution that lowers a Prefix head to `obj` should drop the args | (with the above) |
| `obj`/`exn` filtering from interface inheritance and class implements | F# rejects `inherit obj` / `interface obj with`; cycle-break artifacts at heritage targets need to be filtered, not emitted | (with the above) |
| Synthetic-reference site audit map | Single helper `createAssignedSyntheticRef` documents the universe of 8 sites (3 prerender branches, cache-hit, anchor pass, decl emission, merge logic, missing TemplateLiteral) so Phase B's typar emission isn't discovered through error cascades | `c769b68` |
| **Phase B — synthetic-typar capture** | Multi-position interned literals (`_LitN` from Union/Intersection/TypeLiteral) hoist captured enclosing-scope typars onto the synthetic declaration and emit `Prefix(_LitN, [typars])` at every reference site; `ctx.SyntheticTypars` is the agreement point between helper and decl emission | `0ef73ac` |
| **Parser-bail unmask** | Bare-`_` source typar → `Anon` (F# rejects `'_` in declaration position); captured-typar dedup-by-name at decl + ref emission so duplicate captures from cross-scope walks don't produce `<'S, 'S>`. Removes parser-mask cascade that hid downstream errors | `39f0053` |
| **Phase C — `CycleBrokenPaths` metadata** | Tracks alias TypePaths whose body collapsed to bare `obj`/`exn`. Consulted at early `remap`, final-pass remap, and TypeReference branch's Prefix collapse — drops args when target is cycle-broken so `obj<T>` is never emitted | (with parser unmask) |
| **Phase E1 — Heritage arity reconciliation** | New `Interface.reconcileHeritageArity` reconciles heritage TypeRefRenders against the parent's declared arity. Walks through nested TypeReferences via `resolvedKindArity`; consults `ctx.TypeAliasArity` for TypeLiteral-bodied alias parents | (Phase E1) |
| **Phase E2 — `TypeAliasArity` context field** | Records every alias's TS-source declared typar count (keyed by both decl + body TypeKey). Populated by `prerenderTypeAliases`. Drives heritage reconciliation, TypeReference arity logic for non-Interface/Class inner, and final-pass remap Prefix truncation | (Phase E2) |
| **Phase E3 — Class/Interface ref auto-pad** | Cached `TypeRef` for a generic Interface/Class is now `Prefix(path, [obj × arity])` instead of bare atom. Constraints, heritage clauses, and other paths that bypass the TypeReference arity reconciler get padded args automatically. Reduces verify errors by 22% / 17% in one change | (Phase E3) |
| **Phase E4 — Alias free-typar hoist + arity propagation** | `Render.TypeAlias.fs reconcileTyparList` walks the alias body's ResolvedType graph and hoists every reachable `ResolvedType.TypeParameter` onto the declaration as a synthetic typar (no constraint/default). `prerenderTypeAliases` precomputes the same hoist-count and stores `declared + hoisted` in `ctx.TypeAliasArity`, so use sites of these aliases arity-pad correctly via the existing reconciler. Eliminates the `'T not defined` cohort and 94% of remaining verify errors in one connected change | (Phase E4) |
| **Phase E5 — Walk-through-target + TypeLiteral-bodied hoist + cycle-broken skip + Prefix-no-re-wrap** | Four-part cleanup: (a) collectFreeTypars walks `TypeReference.Type.Value` so typars inside referenced-but-empty-args synthetic bodies are seen; (b) TypeLiteral/Intersection-bodied alias declarations also receive hoisted typars (previously only Alias/Union branches did); (c) cycle-broken aliases (`type X = obj`) skip hoisting — keeping typars would conflict with use-site arg-dropping; (d) empty-args TypeReference branch no longer wraps Prefix-shaped prefix in another Prefix (avoids `Foo<A><B>` parser bail) | (Phase E5) |
| **Phase E6 — Union/Tuple/Function: drop args, expose upstream substitution gap** | Both empty-args and args-present `TypeReference` branches now skip wrapping when the prefix is a Union/Tuple/Function molecule (`Foo<A><B>` / `U4<A,B,C,D><E,F,G,H>` / `(A * B)<C>` / `(A -> B)<C>` are all invalid F#). The previous wrapping behavior produced parser-bail errors (FS0010/FS1242) that masked the deeper alias-body substitution gap — `'T not defined`, `'F not defined`, etc. — which is the next principled target. Verify count rises (561 → 13,475) but this is honest unmasking, not regression | (Phase E6) |
| **Phase F — Typar atoms as `Intrinsic`; render-layer substitution at body and Union/Tuple/Function sites** | Three connected pieces: (a) `ResolvedType.TypeParameter` and Phase B helper render typars as `Intrinsic_ "'T"` atoms instead of opaque `Widget_` atoms — same emitted text but downstream passes can identify and substitute; (b) `Render.TypeAlias.fs` Alias and Union variants now substitute body typars not in the alias's kept-typar list with `obj` via `substituteForHeritage`; (c) empty-args and args-present `TypeReference` branches with Union/Tuple/Function prefix invoke the same substitution with `Set.empty` (no caller scope). Heritage substitution observed firing ~57× per SDK in practice. Remaining cohort (`'T not defined` etc.) comes from member-type emissions (method params/returns, property types) that don't yet route through the substitution layer — that's Phase G | (Phase F) |

## The principle, restated

**`typeref-render.md` is the spine.** The render layer keeps three concerns
separate:

- **Atom** — a leaf name (intrinsic, concrete path, transient path, widget).
- **Molecule** — composition. `Prefix(prefix, args)` is the way generic
  application is represented.
- **`TypeRefRender { Kind; Nullable }`** — the unit that flows through
  prerender / anchor / localise / lowering.

Any caching, remapping, or substitution mechanism added on top of this must
**compose with** the atom/molecule split, not replace it. The smart
constructors (`RenderScopeStore.TypeRefRender.create`'s SRTP overloads) and
the four-pass pipeline (prerender → addOrReplaceScope → anchor → render)
all rely on this invariant.

Where the previous-era implementation deviated, the fixes above are
**restorations**, not refactors. They put the composition back where the
doc says it should be.

## What's open — and why each one is deeper than a one-keystroke fix

### Synthetic-typar capture — Phase B LANDED (`0ef73ac`), with caveats

The capture is now live. `ctx.SyntheticTypars` holds captured enclosing-scope
typars per synthetic; `createAssignedSyntheticRef` consults it to emit
`Prefix(_LitN, [typars])`; `Members.renderFromMembersAndFunctions` hoists
the same list onto the declaration. Walker traverses transparently through
named-type containers (Interface/Class/Enum), which is counterintuitive but
correct — narrower walks (boundary-stopping) capture fewer typars and end
up emitting refs whose typars aren't in scope at any reference site (verified
empirically).

The cache-hit / anchor-pass concerns from the original framing turned out
not to be problems: since the helper is the single emission point, the
cache stores whatever it emits and downstream cache hits return the same
Prefix shape automatically.

**Verified result (after `0ef73ac`):** verify pipeline 9,155 → 2,744 raw,
2,812 → 141 distinct. **70% / 95% reduction.** Parser-bail on `'_` was
masking deeper FS0037/FS0033 cohorts — see the parser-unmask section below.

**Open caveats:**
- Captured typar names sometimes collide across cross-scope captures
  (multiple methods declaring `<S>` → captured list has two distinct
  TypeParameter records named `S`). Decl-side and ref-side dedup by name
  collapses them; F# binds body refs by name. Semantically lossy when
  distinct `S`s would have meant distinct types.
- `_` source typars (TS "don't care" convention) collapse to `Anon` —
  multiple `_` typars in one body all bind to one declaration entry.

### TypeAlias cycle-break loses arity — Phase C LANDED (`39f0053`)

`CycleBrokenPaths: HashSet<TypePath>` is now populated by
`Render.TypeAlias.fs markCycleBrokenIfErased` when the alias body resolves
to bare `obj`/`exn`. Consulted at three sites: early `remap`, final-pass
remap (Prefix collapse), and the TypeReference branch's Prefix-construction.
When the head's atom matches a cycle-broken path, args are dropped — F#
gets `obj` instead of `obj<T>`.

**Caveat:** Phase C catches *alias* cycle-break (where the alias's body
resolves to obj). It does NOT catch *class* cycle-break (where a class's
heritage is broken). The class case is the FS0001 Zod cohort below, still
open.

### Class-shape lost through cycle-break (FS0001 Zod cohort)

**The problem.** TS `class ZodString extends ZodType<...> {...}`
cycle-breaks the parent to `obj()` in F# emission:

```fsharp
type ZodString private () =
    inherit obj()
    ...
```

ZodString no longer satisfies the `T :> SomeType` constraint declared on
`ZodArray<T when T :> SomeType>`. F# emits FS0001 on every
`ZodArray<ZodString>` use site (~150 errors in agents).

**The principled options.**

1. **Preserve the class hierarchy** through cycle-break. Cycle-break
   currently produces structural-info loss; an alternative is
   forward-only cycle-break (emit a stub at the cycle point but keep the
   original supertype links for typecheck purposes). Substantial encoder
   /decoder work.
2. **Elide constraints at use sites** when the typar argument's resolved
   class inherits `obj` (i.e. cycle-broken parent). Localised generator
   fix, but loses semantic info. Aligned with the spirit of "cycle-break
   loses information; downstream emission compensates by not asking
   F# to verify what we can't represent."
3. **Drop typar constraints from declarations** that target sealed or
   structurally-shallow shapes. Even more aggressive than (2); affects
   all consumers.

(2) is the smallest principled extension. Pairs naturally with the
`CycleBrokenPaths` infrastructure described under TypeAlias-cycle-break.

### `IndexSignature` self-reference cohort

This is the deep architectural item from
`post-pr2-progress.md`. The TS-source idiom:

```typescript
interface Foo {
    [K in keyof EventMap]: T[K]
}
```

The `T[K]` is an `IndexedAccess` whose body references the parameter
typar `K`. The encoder dispatches the lookup, but the resolved graph
loses the binding between K's declaration and the `T[K]` use.

In the F# output, references like `Foo._Lit32Module.Item.Key` dangle —
`Item` and `Key` are synthetic names that should have been generated
under the right scope, but the dispatch routed elsewhere.

**Where the fix lives.** Encoder-side. The dispatcher in
`Xantham.Fable/Reading/Dispatch/TypeFlagObject.fs` needs to thread the
parameter-typar context through `IndexedAccess` resolution.

Lower priority than the synthetic-typar + cycle-break stack because
the manifestation is many sites of one structural defect; once the upper
items land, the residual cohort is the natural next target.

### `NoInfer` / TS 5.4+ intrinsics

Currently 0 errors across all 12 SDKs — earlier fixes (renamer +
cycle-break) coincidentally resolved the visible manifestations. Worth
monitoring as new SDKs are added; the encoder's dispatcher path for
`NoInfer`-as-intrinsic remains the latent risk
(`post-pr3-progress.md §2 in the original doc body covers this).

### `UnknownDeclared` classifier scope

The Babel.Types.X cohort — encoder classifier puts some declarations
into `Source.UnknownDeclared` that should attribute to known packages.
Local fixes in
`src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs:467` (the
fallback flip) can revert to `LibEs` for the unattributable case, or
the classifier can be made smarter.

Adjacent to the encoder-side IndexSignature work; same code-base
neighborhood.

## Staged path forward

Phases A–C have landed. The recurring shape that drove them: **a
render-time decision (cycle-break, synthetic-typar-capture) needs
metadata that flows through every downstream emission path**. The
infrastructure that delivers this (`SyntheticPaths`, `SyntheticTypars`,
`CycleBrokenPaths`) is now in place.

Phases D–F (open) shift the work into the encoder, where the root causes
of the dominant remaining cohort live (declaration-merge winner,
alias-body substitution, heritage arity).

A staged approach that doesn't bunch all the surgery in one commit:

### Phase A — single synthetic-reference helper ✓ LANDED `c769b68`

Audited every place a synthetic-reference is emitted; documented the
universe of 8 sites in `createAssignedSyntheticRef`'s comment block.
Comment-only change; no behavior shift. Preparation for Phase B.

### Phase B — turn on `SyntheticTypars` ✓ LANDED `0ef73ac`

Helper consults `SyntheticTypars`, emits `Prefix(path, [typar-refs])`.
`Members.renderFromMembersAndFunctions` hoists the same list onto the
decl side. Walker traverses transparently through named-type containers
(empirically correct over boundary-stopping). Cache-hit and anchor-pass
concerns from the original framing didn't materialise because the helper
is the single emission point.

### Phase C — `CycleBrokenPaths` metadata ✓ LANDED `39f0053`

`Render.TypeAlias.fs markCycleBrokenIfErased` populates the set when an
alias body resolves to bare `obj`/`exn`. Consulted at:

- The early `remap` in `prerender` (drop args from the Prefix molecule
  when the remap target is cycle-broken)
- The final-pass remap (same logic at exit)
- The TypeReference branch's `Prefix` construction (drop args when the
  prefix is a ConcretePath atom in `CycleBrokenPaths`)

The "constraint emission" and "heritage rendering" bullets from the
original Phase C framing turned out to overlap with the above three
sites; constraints go through `ctx.PreludeGetTypeRef = prerender`, so
they pick up the same drop behavior. Heritage is the open issue — see
Phase E below for the not-yet-handled case.

### Phase D (open) — Encoder-side TypeAlias duplicate winner selection

Driven by errors like `type Schedule = U4<Schedule._Lit2<'T>, ...>` —
F# rejects because `Schedule` is declared without `<'T>` but the body
uses `'T`. Root cause: TS source has **two** `Schedule` declarations
(`agent-tool-types-CM_50fcV.d.ts:1764: type Schedule<T = string>` AND
`schedule.d.ts:82: type Schedule = z.infer<...>`). The encoder's
`selectAndMergeWinnersInDuplicates` (`Read.fs:377`) picks the non-generic
one as winner; the body's `'T` references survive into the generated
binding but the declaration is non-generic.

**Fix surface:** `src/Xantham.Fable/Read.fs` `selectAndMergeWinnersInDuplicates`
needs to either prefer the generic-arity-higher declaration when both are
real (vs cherry-picking by file position), or merge declared typar lists
across duplicates.

### Phase E — Arity reconciliation across all reference sites ✓ LANDED

Three connected pieces shipped together:

1. **Heritage arity reconciliation** (`Render.TypeShapes.fs`):
   `Interface.reconcileHeritageArity` truncates Prefix args when the
   parent's declared arity is 0; walks nested TypeReferences to find
   the named target.
2. **`TypeAliasArity` context field** (`Types/Generator.fs`,
   `RenderScope.Prelude.fs prerenderTypeAliases`): records alias
   declared typar count keyed by both decl + body TypeKey. Consulted
   wherever the arity reconciler needs to know the alias's TS-source
   declared count (TypeLiteral-bodied aliases don't expose declared
   arity through `Interface`/`Class` cases).
3. **Class/Interface ref auto-pad** (`createConcretePaddedRef` in
   `RenderScope.Prelude.fs`): cached `TypeRef` for any generic
   Interface/Class is `Prefix(path, [obj × arity])` instead of bare
   atom. This catches constraint emission, heritage without explicit
   args, and any other site that bypasses the TypeReference arity
   reconciler.

The declaration's own emission uses the type's path + declared
TypeParameters list directly, so the auto-padding affects only
references — declarations remain correctly shaped.

**Verified result:** 17,008 → 13,217 raw (-22%), 3,007 → 2,490 distinct
(-17%). Per-SDK: agents -1,327, ai-chat -1,283, voice -1,366, think
-1,105.

### Phase F (open) — `IndexSignature` self-reference + `UnknownDeclared` classifier

Original Phase D. Encoder-deep work in `src/Xantham.Fable/Reading/Dispatch/`.
Threading typar context through `IndexedAccess` resolution; tightening the
`UnknownDeclared` fallback. Becomes clearer once D and E strip the
consumer-side noise dominating the histogram today.

### Decoder-side alias-body typar substitution (attempted, reverted)

Tried in this session at `src/Xantham.Decoder/Types/Arena.Interner.fs`
`buildFromTypeReference`. The walker (`substituteResolvedType`,
`substituteMember`, `buildAliasSubstitution`) correctly produces
self-contained substituted bodies per call site, with TypeKey-keyed
memoization to avoid creating fresh ResolvedType instances for cyclic
references within one substitution.

**Why reverted.** Even with memoization, the substituted bodies have
fresh `[<ReferenceEquality>]` outer-record identities at every level. The
generator's anchor pass (`RenderScope.Anchored.fs:479`) uses a `visited`
HashSet keyed by `(ResolvedType, AnchorPath, TransientTypePath)` — when
the same conceptual body is reached through different call-site
substitutions, the visited set doesn't dedup and the anchor walk
recurses to stack overflow.

**Path forward** (one of three):
1. Change the anchor pass's `visited` key from ResolvedType-identity to
   TypeKey (the underlying lazy data is stable across substitutions).
2. Hash-cons the substituted ResolvedTypes by structural identity.
3. Move substitution upstream to the encoder where TS's own type
   instantiation produces self-contained types that share TypeKeys
   naturally — `src/Xantham.Fable/Reading/TypeReference.fs` `fromType`,
   using `checker.getTypeAtLocation` or `checker.instantiateType`.

Option 3 is most architecturally aligned (TS already does the work,
we'd capture it). Option 1 is the smallest direct change.

## Notes on metaphor and architecture evolution

The Pilot/Farscape ethos invites this kind of incremental fill-in:
clean structural model + use-site composition rules. Each iteration on
the SDK topography surfaces a new use-site idiom (alias-with-typars,
synthetic-with-captured-typars, cycle-broken-classes-as-constraints)
that the original framing didn't yet commit to. The right move each
time is to **find the principled extension within the existing
abstractions**, not retreat to per-case patches.

Where this doc lands a finding without an attendant fix, it's because
the integration surface is wider than a one-keystroke change and
deserves a staged commit sequence — not because the fix is
fundamentally unclear.

## Open file pointers (for next-session resumption)

- `src/Xantham.Fable/Read.fs:377` — `selectAndMergeWinnersInDuplicates`.
  Phase D entry point: bias winner selection toward generic-arity-higher
  declaration, or merge typars across duplicates.
- `src/Xantham.Generator/Generator/Render.TypeShapes.fs` — `Class.render`
  and `Interface.render` heritage processing (`shape.Heritage.Extends |>
  List.map toTypeRef`). Phase E entry point: arity-reconcile heritage
  refs against the parent's declared typar count.
- `src/Xantham.Generator/Generator/RenderScope.Prelude.fs:481-494` — the
  existing arity reconciler that needs to be invoked at heritage sites.
- `src/Xantham.Fable/Reading/TypeReference.fs` `fromType` — decoder-side
  substitution alternative (option 3 above): set `ResolvedType` to the
  TS-substituted instantiation rather than `ValueNone`.
- `src/Xantham.Generator/Generator/RenderScope.Anchored.fs:479` — the
  anchor pass's `visited` HashSet; key change here would unlock the
  decoder substitution work (option 1).
- `src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs:467` — Phase F
  `UnknownDeclared` fallback flip site.
- `src/Xantham.Fable/Reading/Dispatch/TypeFlagObject.fs` — Phase F
  IndexSignature dispatch.

## Current measurable state (for delta-tracking)

After Phase E6 (Union/Tuple/Function arg-drop, exposing upstream
substitution gap): **13,475 raw / 2,802 distinct** verify errors across
12 runtime SDKs.

This is honest unmasking of the alias-body substitution gap that Phase
E5's masking-via-wrapping hid. The dominant unmasked cohort is uniform
across the 4 SDKs sharing `@types/node`:
- `'T not defined` (170-190 each) — alias body has free typars not in
  use-site scope
- `'F not defined` (88-108 each) — same pattern
- `'EventEmitter not defined` (70 each) — encoder `this`-as-typar bug;
  TS `prependListener<K>(...): this` returning `this` was encoded as a
  TypeParameter named after the enclosing class
- `'Options not defined in ...Crypto.GenerateKeyPairSyncModule.Invoke`
  (64 each) — synthetic submodule path resolution gap

The principled next-step targets, in upstream-to-downstream order:
1. **Alias-body typar substitution** (decoder/encoder) — substitute
   alias typars in body at use sites. Attempted earlier; trips over
   anchor pass's reference-identity cycle detection. Needs either
   TypeKey-keyed dedup in anchor or hash-cons of substituted bodies.
2. **`this`-as-typar encoding** (encoder) — TS source `): this` from a
   method should encode as a self-reference / the enclosing-class type,
   not as a TypeParameter named after the class.
3. **Synthetic submodule path resolution** — `Crypto.GenerateKeyPairSyncModule.Invoke.Options`
   member should resolve; the path is constructed but lookup fails.

Trajectory across the session:
- Pre-PR3 baseline: 9,155 raw / 2,817 distinct (parser-bail masking
  ~6× more downstream errors)
- After Phase B (synthetic-typar capture, `0ef73ac`): 2,744 raw / 141
  distinct (still parser-bail masked)
- After parser-bail unmask + Phase C (`39f0053`): 17,109 raw / 3,032
  distinct (honest, no masking)
- After Phase E1–E3 (auto-pad refs): 13,217 / 2,490 (-3,791 / -517)
- After Phase E4 (alias-hoist + arity propagation):
  829 / 207 (-12,388 raw from E3)
- After Phase E5 (parser-wrap masking): 561 / 51 (-268 raw / -156
  distinct from E4)
- After Phase E6 (this commit, unmask via arg-drop): **13,475 / 2,802**
  — honest unmasking exposing the upstream alias-body substitution gap

Per-SDK after Phase E6 (honest, no masking):

| SDK | Raw | Distinct |
|---|---:|---:|
| Agents | 2,394 | 485 |
| AiChat | 2,391 | 502 |
| Codemode | 277 | 66 |
| Containers | 721 | 175 |
| DynamicWorkflows | 38 | 9 |
| Puppeteer | 27 | 11 |
| Sandbox | 713 | 178 |
| Shell | 717 | 172 |
| Think | 2,578 | 506 |
| Voice | 2,502 | 480 |
| WorkerBundler | 715 | 172 |
| WorkersTypes | 401 | 102 |

DynamicWorkflows (38) and Puppeteer (27) are essentially unaffected
by the unmask — they don't depend heavily on `@types/node`. The other
SDKs all show the same alias-body free-typar cohort:
- `@types/node` aliases are referenced through Promise / EventEmitter
  / Listener types whose bodies have free typars
- Without substitution at use sites, the free typars leak to F# as
  undefined typars (FS0039)

The principled fix is alias-body substitution at the decoder level —
that's the next bite, and it brings most of this 13,475 number back
down. Until then, this measurement IS the honest state.

178 generator tests pass; 28/29 decoder tests pass (one pre-existing
fixture failure unrelated). See `reference_verify_pipeline.md` in memory
for run instructions.
