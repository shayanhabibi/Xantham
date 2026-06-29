# Path-system pass — findings (2026-06-28)

A dedicated pass on the two biggest remaining error classes: (A) `typescript`-source
global placement and (B) dropped/duplicate hoisted nested types (#3). Understand+design was
run as a multi-agent workflow; implementation was driven inline with build→regenerate→
compile→test verification, reverting anything not clean.

## Fix A — `typescript`-source global placement — LANDED (clean)

**Root cause.** 36 of the surface's own globals (`Response`, `Request`, `RequestInit`,
`WebSocket`, `QueuingStrategy`, `Body`, `ResponseInit`, the ReadableStream family, …) are
attributed by the TS checker to a `typescript` source. The blanket source-ignore gate in
`registerAnchorFromExport` dropped their *definitions* to ref-only (`Choice1Of2`), while
references already resolved to the bare top-level name — so every reference dangled.

**Fix.** Thread the package's top-level exports (excluding lib.es internals) from the
decoder interner (`ArenaInterner.TopLevelExports: HashSet<ResolvedExport>`) into
`GeneratorContext`, and relax the six ignore gates: emit the full definition when the
export is one of these top-level globals. The plan's discriminator was *wrong* —
`TopLevelExports` has 938 entries (the whole surface), not 37; the correct gate is
`source = typescript ∧ in TopLevelExports ∧ ∉ LibEsExports` = exactly the 36 globals. The
plan's fixpoint drain loop was unnecessary (every export is already iterated).

**Bonus root-cause fix.** Emitting these globals surfaced a *latent crash*: the
arity-alignment warning in `prerender` did `printfn "...%A..." innerResolvedTypeValue`, and
`%A` on a cyclic `ResolvedType` overflows the stack via `ResolvedType.ToString()`. Changed
to `eprintfn` (off stdout) with the bounded type key instead of `%A`. This also removed the
`%A` debug dumps that were polluting generated stdout.

**Result.** No crash, 6+ globals defined at top level, no FS0037 duplicate-def explosion,
185 generator + 5 decoder tests pass. Net −10 errors (2868→2858). The full ~340 payoff is
**masked** because the global bodies surface sub-problem B (`Response.Type` nested refs) and
duplicate members from declaration merging (`Response` emits `url`/`type`/`status` twice).

## Fix B — hoisted nested types (#3) — NOT LANDABLE cleanly; root cause pinned

**The real root cause (definitively traced).** `RenderScopeStore.TypeStore` dedups nested
hoisted types **by `ResolvedType`**, and the entire render cache (`GeneratorContext.Prelude`
/ `tryGet`) is *also* keyed by `ResolvedType` — so two properties that share a `ResolvedType`
have structurally ONE render. Instrumentation of all 177 dedup-loser events shows the
sharing is broad and semantically heterogeneous:
- `certPresented`/`certRevoked` → same key (both the `"0"` literal) — redirect would be OK.
- **`pop`/`push`/`length`/`concat` → same key** (all collapse to one ResolvedType) — redirect
  is **semantically wrong** (`pop` ≠ `length`). `filter.predicate`/`every.predicate` likewise.

References are named **per-property** (`Owner.Pop`, `Owner.Length`, …), but emission emits
**one type per ResolvedType** (the first writer), so the other names dangle → FS0039.

**Why the plan's canonical-redirect fails.** Redirecting a loser's reference to the winner's
name is only valid when they truly are the same type. Across the general dedup set they are
not, so redirecting exploded the count (+1046) — confirmed by an implemented attempt
(`CanonicalTransientPath_` atom resolving against the owner anchor), then reverted. Two
earlier framings (return canonical relative path; keep owner-portion swap leaf) double-graft
because the stored "canonical" path already carries the *winner member's* path-context
segment (`PathContext` is extended with the member name before the property type renders),
and `TransientTypePath.anchor` is non-idempotent (always prepends the handed anchor's trace).

**What a correct fix requires (architectural, not surgical).** One of:
1. **Name-keyed emission/cache** — emit one type per distinct hoisted *name/path*, not per
   `ResolvedType`. Requires the render cache to stop collapsing by `ResolvedType` for
   transient/hoisted types (large change to `GeneratorContext.Prelude`).
2. **Encoder-side distinct keys** — give each hoisted property-type its own key so they
   don't structurally collapse (moves the fix to the owning layer; matches the layer-
   ownership rule, since identity is the encoder's job).
3. **Selective redirect** — redirect only when winner and loser are *genuinely* the same
   named-enum type (e.g. both string-literal enums with identical cases), leaving distinct
   structural hoists alone. Narrower, but needs a same-emitted-shape predicate.

Recommend (2) — it is the principled layer for type identity — evaluated as its own task.
Three generator-only attempts confirm this is not a surgical generator fix.
