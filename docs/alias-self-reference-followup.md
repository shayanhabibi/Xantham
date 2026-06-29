# Tracked follow-up: residual `type X = X` self-cyclic aliases

**Status:** Open. Lower-priority follow-up to the coordinated alias-self-reference fix
(extractor `SAliasBuilder.Type` decouple + generator `prerenderTypeAliases` dual-register),
which took IR-level self-references 332 → 0 and FS0953 232 → 170 with all 438 tests passing.

## What remains

~6 type aliases still render as `type X = X` (or `X = wrapper<X>`) in the
`@cloudflare/workers-types` output. Examples and their IR body shapes:

| Alias | IR body | Self-ref channel |
|:------|:--------|:-----------------|
| `AiModelListType` | `TypeReference{ Type:538, ResolvedType:13237 }`, export key `13237` | `ResolvedType == exportKey` |
| `ResponseInputMessageContentList` | `TypeReference{ Type:206(Array), ResolvedType:13984 }`, export `13984` | `ResolvedType == exportKey` |
| `MainModule` | `TypeReference{ Type:1083, ResolvedType:38 }`, export `38` → `GlobalProp<MainModule, option<MainModule>>` | self-ref nested **inside type arguments** |
| `AiImageClassificationOutput` | `Array{ TypeReference{ Type:14670, ResolvedType:null } }` | self-ref via **`Type`** (not `ResolvedType`); `ResolvedType` already null |
| `AiTextToImageOutput` | `TypeReference{ Type:331, TypeArguments:[14603], ResolvedType:14601 }`, export `14601` | `ResolvedType == exportKey` + arity-doubling when stripped |

## Why it is not a one-line generator fix

It is **multi-mechanism**, and the layer ownership is the **generator** (the encoder
cannot distinguish a self-cyclic alias from a benign transparent one such as
`type StringMap = Map<string,string>` — their IR is identical, both carrying
`ResolvedType == exportKey`; nulling `ResolvedType` in the encoder breaks 7 encoder
tests that legitimately follow it to the resolved structure).

A first attempt (guard in `Render.TypeAlias.fs` `resolveInnerRef`: detect when the
prerendered body ref is a `ConcretePath` equal to the alias's own `path`, then
re-render with the body `TypeReference.ResolvedType` stripped to force the structural
`Type`) fixed ~4 of the cases but:
- did **not** catch the `Type`-channel case (`AiImageClassificationOutput`, where
  `ResolvedType` is already null and the cycle is via `Type`);
- did **not** catch the type-argument-nested case (`MainModule`);
- **introduced a regression** — stripping `ResolvedType` exposed an arity-doubling bug
  in the `TypeReference{ Type; TypeArguments }` prerender path
  (`ReadableStream<X><X>` for `AiTextToImageOutput`).

So a clean fix must (a) detect self-reference across all three channels (`ResolvedType`,
`Type`, and type-argument-nested), and (b) resolve the underlying arity-doubling in the
prerender path before stripping is safe. That is its own scoped piece of work in
`src/Xantham.Generator/Generator/RenderScope.Prelude.fs` (the
`ResolvedType.TypeReference` cases ~253-300) + `Render.TypeAlias.fs` `resolveInnerRef`.

## Root-cause analysis (ready to implement in two ordered steps)

A read-only investigation pinned the two mechanisms. **The fixes are ordered: the
arity-doubling (step 1) must land before the self-reference detector (step 2), because
forcing the structural `Type` path for a self-ref triggers the doubling.**

### Step 1 — arity-doubling (`ReadableStream<X><X>`), must land first — IMPLEMENTED

Implemented in `RenderScope.Prelude.fs`: a guard before the `TypeReference` branch-3
prefix build — when the inner `Type` is itself a `ResolvedType.TypeReference` already
carrying non-empty `TypeArguments` (an already-instantiated generic application), render
the inner type alone and drop the redundant outer `TypeArguments`. Precise, no false
positives (a legitimate `Promise<X>` has a bare `Interface`/`Class` as its `Type`).
Verified: `><` doubling 0; legitimate generics intact; Phase-B errors unchanged at 3204;
185 generator + 248 Fable tests pass. NOTE: in isolation this converts a *malformed*
`ReadableStream<X><X>` into a (still-FS0953) `type X = X` self-reference — so the error
count is flat; its value is removing a genuinely malformed construct and unblocking step 2.

### Step 1 — original analysis

Root: in `src/Xantham.Generator/Generator/RenderScope.Prelude.fs` branch 3 of the
`prerender` `TypeReference` cases (~lines 259-305, prefix build at 296-302). For
`AiTextToImageOutput`, the IR `Type` is **already instantiated**:
`TypeReference{ Type:331, TypeArguments:[14603], ResolvedType:14601 }` where `Type:331` is
itself `ReadableStream<Uint8Array<ArrayBuffer>>` (already applied), and `TypeArguments` is
*also* populated. Branch 3 prerenders the already-applied `Type` to a `Prefix_` molecule,
then wraps it again with `TypeArguments` → `Ast.AppPrefix(prefix, args)` renders
`ReadableStream<...><...>`. The `alignedArguments` arity logic (261-289) doesn't catch
this because it checks arg *count*, not whether the inner `Type` is *already applied*.

- **Deepest owner (encoder):** `src/Xantham.Fable/Reading/TypeReference.fs` `fromNode`
  (~184-236) — `resolveBase` returned an already-instantiated `ReadableStream<...>` as
  `Type` while also emitting `TypeArguments`, a redundant double-instantiation. Invariant:
  `TypeReference.Type` should be the bare generic definition when `TypeArguments` is
  populated, never a self-instantiated `Prefix`-shaped reference.
- **Practical generator-layer fix (unblocks step 2):** in branch 3, before building
  `Prefix_(prefix, postfixArguments)`, if `prefix.Kind` is already
  `TypeRefKind.Molecule (TypeRefMolecule.Prefix _)` (i.e. the inner `Type` already
  resolved to a generic application), render `prefix` alone and drop the redundant
  `TypeArguments`.

### Step 2 — VERDICT: the residual splits across two layers (deferred)

Direct IR inspection settled where Step 2 belongs. The ~12 residual self-cyclic aliases
are NOT one problem — they split into:

**(A) Encoder-corrupt bodies — no generator fix can recover them.** For the `Array<X>`
transparent aliases (`ResponseInput`, `ResponseInputMessageContentList`,
`ResponseFunctionCallOutputItemList`), the IR body is
`TypeReference{ Type=206, TypeArguments=[<real element>], ResolvedType=<own exportKey> }`,
and **`Type=206` resolves to `Array{ TypeReference{ Type=84 } }`** — i.e. `Type` is an
*already-instantiated, spurious* `Array<wrong>` (key 84 is an unrelated type — the
`WorkflowInstance` ghost seen in earlier attempts), while the *real* element sits
separately in `TypeArguments`. The correct output (`ResizeArray<ResponseInputItem>`) is
NOT reconstructible from this body — the `Type` field is already wrong. This is an
**encoder defect**: an `Array<X>`/transparent-alias body must emit a BARE `Array` as
`Type` (with the element in `TypeArguments`), never a pre-instantiated `Array<spurious>`.
Root site to investigate: `src/Xantham.Fable/Reading/TypeReference.fs` `fromNode`/
`resolveBase` (how an alias body that is `Array<…>` gets its `Type` vs `TypeArguments`) —
the same already-instantiated-`Type` family as the Step-1 arity-doubling root.

**(B) Generator-fixable — faithful body, only the self-`ResolvedType`.** For the
`Record<…>`-style aliases (e.g. `FlagshipEvaluationContext`: `Type=538` is the
`TypeLiteral`/index-signature object, `TypeArguments=[string, value-union]`, only
`ResolvedType==exportKey` is spurious), the body IS faithful — rendering `Type`+
`TypeArguments` while ignoring the self-`ResolvedType` yields the correct output. But the
clean implementation is blocked by the circular dependency below.

CONCLUSION: Step 2 is a TWO-layer fix (encoder for group A, generator for group B), not a
single generator change. Four generator-only attempts failed because they tried to render
group-A's corrupt bodies faithfully. Deferred as its own scoped work — ~12 aliases of
thousands, lower impact than the hoisted-nested-types (#3, ~278 errors) and lib.es items.

### Step 2 — circular-dependency note (the group-B generator blocker)

Three implementation attempts (top-level `ResolvedType` strip; recursive `path` strip;
`TypeRefRender.replace aliasRef` over the registered body render) all fail for the same
**architectural** reason, not a missing detail:

The `TypeAliasRemap` is required so *cross-references* to an alias render as the alias
name. But it **also fires during the alias's own body render**, collapsing the body to
the alias name. Therefore `PreludeRenders[body]` holds the *already-collapsed* self-ref
(`= aliasRef`), NOT a structural render — so `replace aliasRef newRef.TypeRef structural`
is a no-op (substitutes aliasRef→aliasRef). The only way to obtain the un-collapsed
structural form is to re-render the body with the remap suppressed, which re-runs
`prerender` on the corrupt IR and re-exposes arity-doubling (`ResizeArray<W><Item>`).

**A clean fix must break this circular dependency**: render an alias's OWN body with the
remap suppressed *for that alias's key specifically* (active for all others), with the
Step-1 arity guard protecting against the re-instantiation doubling. That is a more
invasive change to `prerenderTypeAliases` / the prelude topological pass (where the
alias's own body render is produced) than a `resolveInnerRef` guard. Deferred as its own
scoped piece of work; the residual is ~12 aliases of thousands and lower-impact than the
hoisted-nested-types and lib.es items.

### Step 2 — complete self-reference detector (3 channels)

The current remap (keyed on whole `ResolvedType` instances) only reliably catches the
top-level `ResolvedType` field. The self-reference appears in **three positions**, all of
which a complete detector in `prerender`'s `TypeReference` cases must check (carrying the
current alias's own resolved instance, recursing through wrappers):

| Channel | Example | Self-ref location |
|:--------|:--------|:------------------|
| 1. `ResolvedType` field | `AiTextToImageOutput` (`ResolvedType:14601 == self`) | top-level `ResolvedType` (line 253 trigger) |
| 2. `Type` field, nested in `Array`/`Optional`/`ReadOnly` | `AiImageClassificationOutput` (`Array{ TypeReference{ Type → self }}`) | `innerResolvedType` / `Type`, recurse through wrappers (line 306) |
| 3. nested in `TypeArguments` | `MainModule` (`Conditional` with `TypeArguments:[1084, 38]`, self=38, plus `ResolvedType:38`) | each `alignedArguments` entry, recursively (incl. `Conditional`/`Union` substructure) |

On a self-match, render the node as a `RefOnly` reference to the alias name (do not
re-expand). Mirror how `prerenderTypeAliases` line ~500 already registers the export-key
instance for channel 1; extend registration/checking to the `Type`-field and
nested-`TypeArguments` instances.

Key files: `RenderScope.Prelude.fs` (prerender 253-305, remap 25-29 & 442-446,
prerenderTypeAliases 472-501); `TypeRefRender.Render.fs:59-64` (`Ast.AppPrefix`, where the
double-wrap surfaces); `Render.TypeAlias.fs:26-36` (`resolveInnerRef` self-protection).

## Separately: anonymous-literal-union aliases

A distinct, pre-existing limitation (not introduced by any of this work): aliases whose
body is a union of **anonymous** object literals (`{...} | {...}` with no named member to
promote), e.g. `AiCfBaaiBgeSmallEnV15Input`, `AiCfMetaM2M10012BInput`, render as
`U2<self, self>` because the alias name is the only available name for the anonymous
members. The principled fix is to give anonymous union members anchored transient names
(a hoisting concern shared with the dropped-hoisted-types item). Tracked separately.

---

## Separate item: #3 dropped/duplicate nested literal types (278 "not defined in Module")

**Status:** root-caused, fix-in-progress (correct direction found; path-grafting detail remains).

Interface properties with literal types (e.g. `certPresented: "0"`, `certRevoked: "0"`,
`certIssuerDN: ""`) each hoist to a nested StringEnum in the owner module. The
`RenderScopeStore.TypeStore` (`RenderScope.Prelude.fs:177`, `Dictionary<ResolvedType,
TransientTypePath>`) correctly DEDUPS these by resolved value — `certPresented` and
`certRevoked` genuinely share `ResolvedType` (IR Type key 12744 = `Literal "0"`), so one
emitted type is right. **The bug:** the member REFERENCE (`createTransientPath` at
`RenderScope.Prelude.fs:228-232`) returns the member's OWN path (`CertRevoked`), but
`TryAdd` kept only the first member's path (`CertPresented`) — so `CertRevoked` dangles
(278 "not defined in 'Owner'").

**Correct direction (confirmed):** the reference must point at the path the TypeStore
KEPT (the canonical/first path for that resolved value), not the member's own un-emitted
path. **Naive attempt failed:** returning `createTransientPath stored` produced
double-grafted garbage (`Owner.CertNotBefore.CertIssuerDN`) because the returned transient
path is re-anchored relative to the current member's context downstream. The remaining
work is to return the canonical path so it localises to the single absolute
`Owner.CertPresented`, not a re-prefixed compound. Target: 278 -> ~0 with no anchoring
regression. Touches the transient->anchored path localisation
(`TransientTypePath.anchor` / `localisePaths`).
