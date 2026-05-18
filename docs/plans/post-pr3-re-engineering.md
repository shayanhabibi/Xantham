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
| **`TypeAliasRemap` molecule preservation** | The remap is an identity-stabilisation cache, not a wholesale-replacement rule. Per atom/molecule composition, it stores the canonical *atom* for an alias and references compose `Prefix(canonical, args)` molecules; the final-pass replacement must preserve `Prefix` wrappings | (latest pushed) |
| `Prefix(obj/exn, args)` collapse | Non-generic intrinsics can't carry typar args; cycle-break/substitution that lowers a Prefix head to `obj` should drop the args | (with the above) |
| `obj`/`exn` filtering from interface inheritance and class implements | F# rejects `inherit obj` / `interface obj with`; cycle-break artifacts at heritage targets need to be filtered, not emitted | (with the above) |

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

### Synthetic-typar capture

**The problem.** TS inline callbacks and similar inline shapes (e.g.
`(value: T, index: number) => U` inside a method `foo<T, U>(cb: ...)`)
intern as `ResolvedType.TypeLiteral` with bodies that reference typars
declared by the *enclosing* method or class. The encoder doesn't carry
the typar-context as part of the synthetic's identity; the decoder's
resolved graph stores the typar references as `ResolvedType.TypeParameter`
nodes pointing back at the enclosing declaration.

The generator currently emits these synthetics as un-typar'd type
declarations:

```fsharp
type _Lit18 =
    abstract Invoke: value: 'T * index: float * array: ResizeArray<'T> -> bool
```

F# rejects this — `'T` is unbound at the type-declaration scope (FS0039).

**The principled answer.** Per `paths.md`, the path system assigns
synthetic literals concrete paths via `SyntheticPathAssignment`. Per
`typeref-render.md`, generic application is a `Prefix` molecule. Combining
the two: when a synthetic's body uses typars from an enclosing scope, the
synthetic's declaration must hoist those typars and every reference site
must compose `Prefix(synthetic-path, [typar-refs])`.

**The attempt.** Implemented in three layers:

1. New `GeneratorContext.SyntheticTypars: DictionaryImpl<ResolvedType, TypeParameter list>` paired with the existing `SyntheticPaths`.
2. `SyntheticPathAssignment` walks each synthetic's body collecting
   `ResolvedType.TypeParameter` references in first-appearance order.
3. `Render.Transient.fs.Members.render` consults the dictionary and
   emits the captured typars on `Transient.TypeLikeRender.TypeParameters`.
4. `prerender`'s three `SyntheticPaths` consultation sites wrap the
   atom ref in `Prefix(path, [typar-refs])` via a new
   `createAssignedSyntheticRefWithTypars` helper.

**What worked.** Synthetic declarations correctly emitted typars
(including constraints):

```fsharp
type _Lit1<'TArgs, 'R when 'TArgs :> Types.Node.Array<option<obj>>> = ...
type _Lit18<'T> =
    abstract Invoke: value: 'T * index: float * array: ResizeArray<'T> -> bool
```

**What didn't.** Reference uniformity broke. Histogram went 9,155 → 11,083
raw, with FS0037 (duplicate definitions) jumping 4–114 → 832 and FS0033
shifting from "expects 0 given N" patterns to "expects N given 0" patterns
(`_Lit93<_,_,_>` expects 3 given 0).

**Why.** Synthetic-reference emission has more paths than the three
`SyntheticPaths` consultation sites I instrumented:

- **Cache-hit paths** (`prerender` lines ~87–109) — when a synthetic's
  render is already cached, the cached `TypeRef` is returned via `remap`
  without re-running the typar-capture wrap.
- **Anchoring/localisation passes** — `RenderScope.Anchored.fs` walks
  refs converting Transient atoms to Concrete; if it encounters a
  synthetic-without-typars (cached early) it doesn't know to add typars.
- **The synthetic-vs-real-class merge logic** in `Render.Collection.fs`
  intentionally prefers `TypeParameters = []` from one side or non-empty
  from the other; the rule was designed assuming the synthetic side was
  always empty. With synthetics now carrying typars, this rule needs
  refinement.
- **Path-localisation** (`anchorAndLocalise`) walks atoms; molecules
  carrying typar args at synthetic paths may not localise the way
  atoms do.

The pattern: **the typar-aware reference shape needs to flow through every
emission and every cache-touch path, not just the three SyntheticPaths
consultations.** That's a coordinated multi-site change, not a single
landing.

**Reverted.** The structural pieces (`SyntheticTypars` dictionary, the
walking-and-capture logic, the typar emission on declarations) are
correct in isolation and worth reviving when the integration surface is
addressed. For now the working tree is back at the molecule-preservation
state.

### TypeAlias cycle-break loses arity

**The problem.** `type ZodTypeAny = ZodType<any, any, any>` is a TS alias
with 0 declared typars (fully-applied). The body (`ZodType<any,any,any>`)
cycle-breaks to `obj` during render. Result: `type ZodTypeAny = obj`.
But the encoder produces TypeReference applications like
`ZodTypeAny<option<obj>, ZodTypeDef, 'Output>` (3 args) — apparently
preserving the original `ZodType<...>` shape at some call sites. F#
rejects because the *declaration* is non-generic.

**Detected attempts and walls.** Tried two angles:

- *TypeAliasArity* — pad 0-arg references with `obj`. Reference path
  bypasses the arity reconciler entirely (consumed by `remap` directly).
- *ObjAliasPaths* — register aliases that cycle-break to `obj` so Prefix
  construction at use sites drops args. Detection at remap-setup time
  sees only the un-broken body (`TypeReference` to ZodType), not the
  collapse-to-`obj` outcome (which only happens later at render time).

**Why it's deeper.** The cycle-break is a render-time decision driven by
`RenderingAliasTargetRefs` — a set that's empty at setup and populated
as alias bodies are entered. By the time we know the body collapsed to
`obj`, references may have already been rendered with args. Working
backwards requires either:

- A two-pass architecture (render aliases first, collect cycle-break
  outcomes, then run references), or
- A persistent metadata trail (`CycleBrokenPaths: HashSet<TypePath>`
  populated during render; reference emission re-checks before final
  emit).

The second is a smaller architectural addition but still touches
multiple emission paths. Same shape as the synthetic-typar surface.

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

The recurring shape across the open items is: **a render-time decision
(cycle-break, synthetic-typar-capture, class-shape loss) needs metadata
that flows through every downstream emission path**, not just the three
or four sites where the decision originates.

A staged approach that doesn't bunch all the surgery in one commit:

### Phase A — single synthetic-reference helper

Audit every place a synthetic-reference is emitted (prerender's three
`SyntheticPaths` sites, the cache-hit `remap`, the anchor pass's
synthetic handling, the merge logic in `Render.Collection.fs`). Route
each through a single helper function. Helper currently does the same
thing the inline code does (atom-only return); the value of the audit
is **knowing the universe of sites** rather than discovering them
post-hoc through error cascades.

This is preparation, not improvement. Likely zero net error-count
change. But it sets up Phase B to land cleanly.

### Phase B — turn on `SyntheticTypars`

Once Phase A's helper is the single emission point, switch it to
consult `SyntheticTypars` and emit `Prefix(path, [typar-refs])`. The
declaration-emission piece (already verified in the attempt) layers
back in. Expect a cascade of cleanups and reveal unmasked issues — the
shape that the molecule-preservation commit already prepared for.

### Phase C — `CycleBrokenPaths` metadata

Add a `HashSet<TypePath>` populated during alias rendering when the
body collapses to bare `obj`/`exn`. Consult it at:

- Prefix-construction sites (drop args when head is a known cycle-broken
  alias)
- Constraint emission (drop constraints when target is a known
  cycle-broken alias)
- Heritage rendering (drop heritage when target is known cycle-broken)

Addresses TypeAlias-cycle-break-loses-arity and FS0001 Zod cohort in
one connected change.

### Phase D — encoder-side IndexSignature and `UnknownDeclared`

Move to `src/Xantham.Fable/Reading/Dispatch/`. Threading typar context
through `IndexedAccess`; tightening the `UnknownDeclared` fallback.
These are encoder-deep but their structural shape becomes clearer once
A–C have stripped the consumer-side noise.

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

- `src/Xantham.Generator/Generator/RenderScope.Prelude.fs` —
  `prerender`, the three `SyntheticPaths` consultations
  (~lines 238, 328, 624), the `remap` cache-hit handler (~lines 41–55),
  the molecule-preserving final pass (~lines 684–706).
- `src/Xantham.Generator/Generator/SyntheticPathAssignment.fs` —
  Phase A/B target. The walking/capture logic for `SyntheticTypars`
  is preserved in this conversation's transcript.
- `src/Xantham.Generator/Generator/Render.Transient.fs` —
  `Members.render`, `renderFromMembersAndFunctions`. The Phase B
  decl-emission piece.
- `src/Xantham.Generator/Generator/Render.Collection.fs` —
  `mergeRenders`. Phase B may need refinement here for synthetic-vs-real
  typar-list merging.
- `src/Xantham.Generator/Generator/Render.TypeAlias.fs` —
  `pruneUnusedTypars`, `breakSelfReference`. Phase C populates
  `CycleBrokenPaths` here when the body collapses to bare obj/exn.
- `src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs:467` — Phase D
  `UnknownDeclared` fallback flip site.
- `src/Xantham.Fable/Reading/Dispatch/TypeFlagObject.fs` — Phase D
  IndexSignature dispatch.

## Current measurable state (for delta-tracking)

9,155 raw / 2,817 distinct verify errors across 12 runtime SDKs
(commit `<head>` at time of writing). 178 generator tests pass; 28/29
decoder tests pass (one pre-existing fixture failure unrelated). See
`reference_verify_pipeline.md` in memory for run instructions.
