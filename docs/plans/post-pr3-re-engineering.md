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
| **Phase E6 — Union/Tuple/Function: drop args, expose upstream substitution gap** | Both empty-args and args-present `TypeReference` branches now skip wrapping when the prefix is a Union/Tuple/Function molecule (`Foo<A><B>` / `U4<A,B,C,D><E,F,G,H>` / `(A * B)<C>` / `(A -> B)<C>` are all invalid F#). The previous wrapping behavior produced parser-bail errors (FS0010/FS1242) that masked the deeper alias-body substitution gap — `'T not defined`, `'F not defined`, etc. — which is the next principled target. Verify count rises but this is honest unmasking, not regression | (Phase E6) |
| **Phase F — Typar atoms as `Intrinsic`; render-layer substitution at body and Union/Tuple/Function sites** | Three connected pieces: (a) `ResolvedType.TypeParameter` and Phase B helper render typars as `Intrinsic_ "'T"` atoms instead of opaque `Widget_` atoms — same emitted text but downstream passes can identify and substitute; (b) `Render.TypeAlias.fs` Alias and Union variants now substitute body typars not in the alias's kept-typar list with `obj` via `substituteForHeritage`; (c) empty-args and args-present `TypeReference` branches with Union/Tuple/Function prefix invoke the same substitution with `Set.empty` (no caller scope). Heritage substitution observed firing ~57× per SDK in practice. Remaining cohort (`'T not defined` etc.) comes from member-type emissions (method params/returns, property types) that don't yet route through the substitution layer — that's Phase G | (Phase F) |
| **Phase G — Boundary-stop free-typar walkers (synthetic + alias)** | Both `SyntheticPathAssignment.collectFreeTypars` and `Render.TypeAlias.collectBodyTypars` were walking transparently through named-type containers and through `TypeReference.Type.Value`, picking up typars from the target's own declaration scope and hoisting them onto synthetic/alias declarations. A synthetic `_Lit102` whose body uses zero typars was being declared as `_Lit102<'T,'S,'This,'U,'A,'D,'Arr,'InnerArr,'Depth,'Array>` because the walker followed `ResizeArray<U2<float,string>>` into `Array<T>`'s body and collected `Array`'s `T` plus typars from its iterator methods. Boundary-stop both walkers at `Interface`/`Class` and at `tr.Type.Value` — walk only `tr.TypeArguments` (carrying typars from the OUTER scope). The earlier "transparent walk is empirically better" hypothesis (Phase B doc) didn't hold once Phase F's render-layer substitution was in place: over-capture was producing constraint-mismatched refs that no use-site scope could satisfy. Largest single-phase reduction in synthetic-typar cohorts | (Phase G) |
| **Phase H — Encoder-side alias instantiation via TS's `getTypeOfSymbol`** | At the `TypeReferenceNode` dispatch in `src/Xantham.Fable/Reading/Dispatch/TypeNode.fs`, when the target's instantiated `Ts.Type.aliasSymbol` is set (TS's marker for an alias application), bypass `TypeReference.fromNode` (which emits an un-substituted `TypeReference + TypeArguments`) and emit a `STypeLiteralBuilder` populated by `TypeFlagObject.buildSubstitutedMembersFromType`. That helper enumerates properties via `getPropertiesOfType` and reads each property's TYPE via `ctx.checker.getTypeOfSymbol(propSym)` — the TS checker performs the typar substitution at that call, so the captured property types are already substituted at the use site (`Foo<string>`'s `x: T` becomes `x: string`). Lossy on member-shape distinction (method-vs-property is collapsed to property of function type — the substituted type for a method symbol is the signature's function type) but correct on type content. Closes the `'Value not defined` and `'S not defined` cohorts at instantiated alias sites; WorkersTypes effectively at zero. Surfaces a new FS0033 arity-mismatch cohort (`NonSharedBuffer<_,_,_>`, `ZodTypeAny<_,_,_,_,_,_>`, `OmitKeys<_,_>`, `AnyZodObject<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>`) — fewer error categories, more per-category repetition. Surviving `'T` / `'Output` / `'EventEmitter` cohorts are non-alias `this`-as-typar bugs from `@types/node/events.d.ts` and downstream Zod inheritance | (Phase H) |
| **Phase I — Method/signature typar shadowing in free-typar walkers** | All three walkers (`SyntheticPathAssignment.collectFreeTypars`, `Render.TypeAlias.collectBodyTypars`, `RenderScope.Prelude prerenderTypeAliases` walkT) thread a `shadowed: HashSet<TypeParameter>` set; when walking into `Member.Method` / `Member.CallSignature` / `Member.ConstructSignature`, the signature's declared `TypeParameters` extend the shadowed set for the recursive walks into its parameters and return type. Pre-Phase H this didn't matter because alias bodies referenced named-type targets that the walkers boundary-stopped at; Phase H now inlines substituted bodies as `TypeLiteral`s, so the walkers enter method bodies and see method-level typars as if they were free. Shadowing prevents the bogus hoist onto the surrounding alias/synthetic declaration. Small measured impact at landing because the DOMINANT remaining FS0033 cohort (`NonSharedBuffer<_,_,_>`, etc.) traces to a *different* root cause — see Phase J/K. The shadowing fix is principled in its own right — walkers should track scope correctly regardless of whether the immediate impact is large | (Phase I) |
| **Phase J — `this`-as-typar routing to the constraint** | TS represents `this` (in `methodName(): this` etc.) as a polymorphic `TypeParameter` with `isThisType = true` (internal flag) and a `constraint` pointing at the enclosing class. The encoder's `TypeFlagPrimary.TypeParameter` branch read the typar's symbol name as the F# typar name — TS sets that to the *class* symbol — producing orphan typars like `'EventEmitter`, `'Uint8Array` at every `this`-typed position. Fix: detect `isThisType` via `Fable.Core.JsInterop` (`typ?isThisType = true`) and route the xanTag's signals through to the constraint type via `routeToType`, so `this` emits as a reference to the enclosing class rather than a phantom typar. Eliminates the FS0039 `'EventEmitter is not defined` cohort and reduces the FS0033 `NonSharedBuffer<_,_,_>` cohort by one typar slot. Surviving FS0033 in that cohort traces to method-level typars inside the substituted body — addressed by Phase K | (Phase J) |
| **Phase K — Type-level signature typeParameters in `S{Call,Construct}SignatureBuilder`** | The encoder's `callSigToMemberSlot` and `constructSigToMemberSlot` (in `Reading/Dispatch/TypeFlagObject.fs`) were setting `TypeParameters = [||]` with the comment "TS Signature objects don't expose typeParameters as nodes; the declaration-side reads do." That's true about *nodes* — but signatures DO expose typars at the *type* level via `signature.getTypeParameters()`, each a `Ts.TypeParameter`. With Phase H emitting substituted-method properties as function types whose call signatures carry the method-level typars, the empty array left the synthesized typars invisible to the decoder. Downstream walkers (post-Phase I) shadowed zero typars at the call sig and the method-level typars leaked into the surrounding alias's hoisted typar list. New `getTypeParamSlotsFromSignature` helper mirrors the pattern in `TypeDeclaration.getTypeParamSlots` but operates on the type-level `Ts.TypeParameter` array — tag each, push to stack, weave the resulting `SType.TypeParameter` builder into the `InlinedSTypeParameterBuilder` shape downstream consumers expect. **Eliminates** the bulk of the FS0033 cohort introduced by Phase H | (Phase K) |
| **Phase L — Constraint propagation for hoisted typars** | `Render.TypeAlias.fs reconcileTyparList` and the `Intersection`/`TypeLiteral` TypeDefn branch were constructing extra typar entries (those hoisted by the body walker) as bare records with `Constraint = ValueNone; Default = ValueNone`. When the alias body uses those typars in constrained positions — e.g. as a synthetic's typar with `:> SomeInterface`, or as a method-level typar passed through a constrained call signature — F#'s constraint inference *requires* the same bounds on the alias declaration's typar list, and emits FS0001 missing-constraint errors at the declaration site if they're absent. Fix: replace the bare-record construction with `TypeParameter.render ctx scopeStore` so each hoisted typar carries its full `TypeParameterRender` (constraint, default, atom) into the declaration. `Render.Transient.fs renderFromMembersAndFunctions` was already using `TypeParameter.render` for synthetic declarations; the comment was clarified to call out the constraint-propagation invariant explicitly. The principle: the body walker's hoisting selects *which* typars belong on the declaration; the render layer's `TypeParameter.render` is the canonical way to project a `TypeParameter` to its declared form. The two should compose, not duplicate | (Phase L) |
| **Phase M — TS lib.es5 utility-type aliases and iterator types in the substitution table** | The render layer's `LibEsDefaults.substitutions` table is the documented seam between TS lib.es type names and their F# equivalents. Eighteen TS lib.es5 utility-type aliases (`Omit`, `Pick`, `Partial`, `Required`, `Readonly`, `Exclude`, `Extract`, `NonNullable`, `ReturnType`, `Parameters`, `ConstructorParameters`, `InstanceType`, `Awaited`, `ThisParameterType`, `OmitThisParameter`, `ThisType`, `NoInfer`; plus the four string-transform intrinsics `Uppercase`/`Lowercase`/`Capitalize`/`Uncapitalize`) had no entries — references emitted as bare path-rooted names (`Omit<X, K>`) which F# couldn't resolve. At HERITAGE positions these were the dominant pattern: `interface X extends Partial<Omit<Y, K>>` produced `inherit option<Omit<obj, obj>>` after cycle-break (Partial → option<…>; Omit args broken to obj). The existing heritage filter (`isObjOrExnIntrinsic` in `TypeRender.Render.fs`) only catches bare `obj`/`exn` atoms, not Prefix molecules — so cycle-broken-but-still-named aliases survived as orphan references, producing paired FS0039 (`'Omit' is not defined`) + FS0887 (`'obj' is not an interface type`) at every site. Phase H closes this at NON-heritage TypeNode positions via `getTypeOfSymbol`, but heritage refs route through `Class.exprWithTypeArgsToRefSlot` (a different path that Phase H does not touch). Fix: substitute the utility-type aliases to `obj` arity 0 (string for the four string-transform intrinsics). At heritage, refs become bare `obj` and the existing filter drops them; at member-position they collapse to `obj` (lossy on structural intent, compilation-correct). Same pattern extended to TS 5.x iterator additions (`SetIterator`, `MapIterator`, `ArrayIterator`, `RegExpStringIterator`, `RegExpMatchArray`, `RegExpExecArray`, `TemplateStringsArray`, `ReadonlySetLike`, `WeakKey`, `PromisifyFn`) whose F# bindings either don't exist or have divergent arity. The principle: when an F# binding diverges from TS — by name, arity, or absence — the substitution table is the seam, not a per-site special case. Phase H handles the "TS computes a structural body" case; Phase M handles the "TS-name has no F# binding" case at every position including heritage | (Phase M) |

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

### Synthetic-typar capture — Phase B LANDED (`0ef73ac`), revised by Phase G

The capture is live. `ctx.SyntheticTypars` holds captured enclosing-scope
typars per synthetic; `createAssignedSyntheticRef` consults it to emit
`Prefix(_LitN, [typars])`; `Members.renderFromMembersAndFunctions` hoists
the same list onto the declaration.

**Phase B's original walker was transparent through all containers** —
Interface/Class members, TypeReference's `Type.Value`. The original
rationale was union-of-all-captures: collect every typar reachable so
each reference site finds at least the typars it needs. **Phase G
revisits this:** transparent walking picks up typars from the *target's*
declaration scope (e.g. walking `Array<T>`'s body collects `Array`'s
`T` and its iterator method typars), and those typars are bound at the
*target's* declaration, not at the synthetic's enclosing scope. The
ref `_LitN<'T, ...>` then carries args no reference site can satisfy.

**Post-Phase G discipline:** boundary-stop at named-type declarations
(Interface/Class) and at `tr.Type.Value`. Walk only `tr.TypeArguments`
for TypeReference — those carry typars from the synthetic's *outer*
scope (the host export's typar list), which is what we want.

The cache-hit / anchor-pass concerns from the original framing turned out
not to be problems: since the helper is the single emission point, the
cache stores whatever it emits and downstream cache hits return the same
Prefix shape automatically.

Verified at landing (`0ef73ac`); parser-bail on `'_` was masking
deeper FS0037/FS0033 cohorts — see the parser-unmask section below
for what was subsequently exposed.

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

The result was a large single-change reduction across the four
SDKs whose synthetic literals carried the heaviest hoisted-typar
load (agents, ai-chat, voice, think); per-SDK numbers live in
the status doc.

### Phase F (open) — `IndexSignature` self-reference + `UnknownDeclared` classifier

Original Phase D. Encoder-deep work in `src/Xantham.Fable/Reading/Dispatch/`.
Threading typar context through `IndexedAccess` resolution; tightening the
`UnknownDeclared` fallback. Becomes clearer once D and E strip the
consumer-side noise dominating the histogram today.

### Decoder-side alias-body typar substitution (attempted twice, reverted)

**First attempt:** earlier session at `src/Xantham.Decoder/Types/Arena.Interner.fs`
`buildFromTypeReference`. Walker (`substituteResolvedType`,
`substituteMember`, `buildAliasSubstitution`) correctly produced
self-contained substituted bodies per call site, with TypeKey-keyed
memoization to avoid fresh ResolvedType instances for cyclic
references within one substitution.

**Why reverted (first time).** Even with memoization, the substituted
bodies had fresh `[<ReferenceEquality>]` outer-record identities at
every level. The generator's anchor pass uses a `visited` HashSet
keyed by `(ResolvedType, AnchorPath, TransientTypePath)` — when the
same conceptual body is reached through different call-site
substitutions, the visited set doesn't dedup and the anchor walk
recurses to stack overflow.

**Second attempt (this session):** retried with the three-option plan
in mind:

- Added `aliasByBodyKey: ConcurrentDictionary<TypeKey, TypeAlias>` to
  bridge body-keys to alias declarations (so `buildFromTypeReference`
  can know whether the target is an alias and look up its declared
  typars).
- Added `substitutedBodyCache: ConcurrentDictionary<(TypeKey, TypeKey list), Lazy<ResolvedType>>`
  — hash-cons keyed by `(alias body TypeKey, args TypeKey list)`.
  Same `(alias, args)` returns the same `Lazy<ResolvedType>` across
  all call sites. The Lazy boundary prevents factory re-entry during
  cyclic construction.
- Implemented `substituteType` / `substituteLazy` / `substituteMember` /
  `substituteTupleElement` walkers with per-call memo keyed by
  input ResolvedType reference identity (so cycles within one
  substitution stabilise).
- Routed `buildFromTypeReference` to populate `ResolvedType` with the
  substituted body when target is a registered alias with declared
  typars and args are present.

**What went wrong this time.** Three cascading issues:

1. **Per-substitution memo wasn't enough.** Substituted bodies still
   had fresh outer-record identities ACROSS substitutions — the
   memo dictionary scopes to one `substituteType` call. The
   `substitutedBodyCache` shared identity at the OUTER substituted
   body level (one ResolvedType per `(alias, args)` tuple), but
   inner TypeReferences inside a substituted body were constructed
   via `substituteLazyMemo` and produced fresh wrapper LazyResolvedType
   instances. When the generator's `prerenderTypeAliases` walker
   traversed the substituted body and followed `tr.ResolvedType.Value`,
   it kept generating fresh ResolvedTypes through chained lazies
   until stack overflow.
2. **Boundary-stopping `prerenderTypeAliases` walker** at TypeReference
   (matching the discipline already applied to `collectFreeTypars` and
   `collectBodyTypars`) got past that crash, but then the next walker
   downstream — path-flattening in `NamePath.flattenModule` — hit its
   own unbounded recursion through cycles the substituted body
   exposed. Each substituted body has TypeReferences whose Type field
   stays unchanged (original alias body) and whose ResolvedType field
   recursively re-enters substitution lookups. The path graph
   accumulates synthetic refs without termination.
3. **The cache-key vs structural-key mismatch is fundamental.**
   Substituted args carried over the ORIGINAL typar's TypeKey via
   `LazyResolvedType.Data` (we preserve `Data` to keep identity
   stable). So a use site of `Foo<string>` and a substituted-inner
   `Foo<T>` (with T → string applied) produce DIFFERENT cache keys
   (`(Foo, [string TypeKey])` vs `(Foo, [T TypeKey])`) even though
   they should resolve to the same substituted body. Without
   per-step re-keying — which requires assigning TypeKeys to
   substituted Values, which the encoder owns and we don't —
   composition across substituted layers fragments identity.

Reverted. The one principled holdover kept: boundary-stopping the
`prerenderTypeAliases` walker at TypeReference and Interface/Class —
symmetric to the discipline applied to the other two free-typar
walkers, valid on its own even without substitution.

**Path forward — landed via Phase H (encoder-side).** The first
option of three listed below was implemented and landed. The other
two remain available for future iterations or for the residual
non-alias cohorts (Interface methods returning `this`, etc.) that
Phase H does not address.

1. **Encoder-side substitution via TS's compiler API — LANDED**
   in `src/Xantham.Fable/Reading/Dispatch/TypeNode.fs` and
   `Reading/Dispatch/TypeFlagObject.fs`. The
   `TypeReferenceNode` dispatch checks
   `getTypeFromTypeNode(node).aliasSymbol` — TS sets this on
   instantiated alias applications. When present, the dispatch
   emits an `STypeLiteralBuilder` via the new
   `buildSubstitutedMembersFromType` helper, which enumerates
   properties via `getPropertiesOfType` and reads each property
   type via `getTypeOfSymbol(propSym)` (TS substitutes typars at
   that call). The tag's TypeKey is set via the standard
   `signalCache[xanTag.IdentityKey].Key` pattern (same as
   `TypeReference.fromNode` uses) — the substituted TypeLiteral
   gets a stable encoder-assigned TypeKey, the surrounding
   infrastructure handles it like any other interned literal.
   **Result:** distinct count −511 (−19%); WorkersTypes from 86
   to 1 distinct; `'Value` / `'S` typar-cohorts eliminated at
   instantiated alias sites. Trade-off: lossy on method-vs-property
   distinction (collapses to property of function type — TS's
   substituted type for a method symbol IS its function-typed
   signature, so this is type-accurate but member-shape-lossy) and
   surfaces a new FS0033 arity-mismatch cohort downstream where
   generator's arity reconciler doesn't yet know about the typars
   the encoder substitution introduces.
2. **Hash-cons substituted ResolvedTypes by structural identity** —
   not pursued; was the path the reverted decoder attempt would
   have needed. Layering a structural-equality wrapper
   (`type StructurallyEq = { Inner: ResolvedType }` with custom
   `Equals`/`GetHashCode` doing deep comparison) would let a
   decoder-level cache key on shape, making inner-vs-outer
   substituted refs converge on the same identity. Substantial:
   needs structural hash/eq on the whole ResolvedType DU including
   lazies and nested records. **Now redundant** for the alias-
   instantiation use case (Phase H covers it via TS-managed
   identity) but could still apply to other substitution-like
   scenarios that don't go through TS's instantiation path.
3. **Change anchor pass + `prerenderTypeAliases` + path-flatten
   visited keys to TypeKey** — not pursued. Substituted bodies
   would need synthetic TypeKeys, which means a decoder-side
   TypeKey allocator and threading TypeKey through every
   visited-set site. **Made irrelevant** by Phase H: the encoder
   now assigns real TypeKeys to substituted literals via TS's
   normal interning, so no synthetic allocation needed.

Notes from the reverted decoder attempt are preserved for whoever
picks up the next layer (the residual non-alias cohorts: `this`-as-
typar, synthetic submodule paths). The lazy-cache pattern from that
attempt is correct in shape for cases where TS's instantiation isn't
the right substitution — its failures were at the COMPOSITION
boundary where one substituted body references another substituted
body. Phase H side-steps composition entirely by using TS's own
substituted property types, which are already self-consistent.

**Phase H downstream cleanup needed.** The new FS0033 cohorts
(`NonSharedBuffer<_,_,_> expects 3 given 0`, `ZodTypeAny<_,_,_,_,_,_>
expects 6 given 3`, etc.) come from the generator's arity reconciler
not knowing that the substituted TypeLiteral's declared typars
exceed what historical use sites pass. Two paths to address:
- Extend `ctx.TypeAliasArity` (or an equivalent map) to capture the
  encoder-substituted body's effective typar count and apply the
  existing pad/truncate logic to references.
- At the encoder, when emitting the substituted TypeLiteral, ALSO
  emit appropriate use-site adjustments — though this loops back
  into "encoder needs to know about use-site arity rules," which
  is a generator concern.

The first path is cleaner — keep the encoder dumb-and-correct
about substitution; let the generator's existing arity machinery
extend to cover the new typar counts.

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

## Residual cohorts and the principled next steps

With Phases H/I/J/K/L landed, the alias-instantiation gap, the
`this`-as-typar bug, the empty-call-signature-typars gap, and
the hoisted-typar constraint-erasure gap are closed at their
sources. Remaining residue:

- **Non-alias instantiation gap** (FS0039 `'T`, `'Output`, `'F`
  cohorts): the encoder substitution in Phase H fires only when
  `instantiated.aliasSymbol` is set (TS's marker for alias
  application). Class/Interface instantiations
  (`extends ZodType<X, Y, Z>`) and conditional/mapped type
  instantiations don't get the substituted-body treatment. They
  flow through the existing `TypeReference.fromNode` /
  `TypeReference.fromType` path which preserves `Type +
  TypeArguments` separately. Method-level typars in those instantiated
  bodies still leak as orphan typars.
- **Synthetic submodule path resolution** (FS0039
  `_LitN.Invoke.Options` and similar): the path is constructed but
  lookup fails. Companion `module _LitN = ...` either isn't being
  emitted or is emitted at a path the reference doesn't resolve to.
  Likely a path-anchoring discrepancy between declaration site and
  reference site.
- **Residual FS0001 missing constraints** (where the constraint
  the body uses isn't directly attached to the typar at the call
  site Phase L can see — e.g. constraints inferred through nested
  function/method positions further down a substituted body). Phase
  L handles the common case of "the hoisted typar already carries
  its constraint at its `TypeParameter` record"; deeper inference
  through method positions inside substituted bodies remains.

The principled next-step targets, in upstream-to-downstream order:
1. **Extend Phase H's substitution to non-alias instantiations**
   (`Reading/Dispatch/TypeFlagObject.fs` `TypeFlagObject.Reference`
   branch). The current path calls `TypeReference.fromType` for
   `Object.Reference`-flagged types; route through
   `buildSubstitutedMembersFromType` instead when the instantiated
   target's generic arguments differ from declaration defaults.
2. **Synthetic submodule path resolution** — investigate why
   `_LitN.Invoke.Options` lookup fails despite the synthetic existing
   at a sibling path. Likely involves the path interceptors in
   `Path.Interceptors`.

The session's measurement trajectory and per-SDK breakdown live in
[`post-pr3-progress.md`](./post-pr3-progress.md) — this document
covers the *engineering*, that one covers the *status*.
