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

### Step 1 — arity-doubling (`ReadableStream<X><X>`), must land first

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
