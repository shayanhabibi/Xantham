# Post-PR3 Progress Report

> **Framing superseded.** This doc was originally written as a
> handover note framed around upstream/downstream boundaries
> ("Shayan's domain," "file back upstream," etc.). That framing
> is no longer in effect — **all of Xantham (encoder, decoder,
> generator, schema, docs) is fair to fix in this fork**. Any
> remaining "Shayan to X" / "for upstream" / "follow-up PR by"
> language below should be read as "the work that needs to
> happen here." The historical narrative is preserved as a
> snapshot of the decision points at the time; the *recommendations*
> within it should be reinterpreted under the fully-open scope.
>
> **Target set correction.** The doc originally referred to the
> Cloudflare SDK set as 13 packages including a `cloudflare`
> aggregate. That was wrong. The bare `cloudflare` npm package
> is the management REST client, handled separately via
> Hawaii + OpenAPI spec on the FCE side — NOT a Xantham target.
> The actual Xantham target set is **12 runtime SDKs**:
> `agents`, `@cloudflare/ai-chat`, `@cloudflare/codemode`,
> `@cloudflare/containers`, `@cloudflare/dynamic-workflows`,
> `@cloudflare/puppeteer`, `@cloudflare/sandbox`,
> `@cloudflare/shell`, `@cloudflare/think`, `@cloudflare/voice`,
> `@cloudflare/worker-bundler`, `@cloudflare/workers-types`.

> **Original framing (preserved):** Picks up after PR #3
> (`structured-logging-encoder`) merged at commit `3850dd3`.
> The post-PR2 doc (`docs/plans/post-pr2-progress.md`) covers
> the immediately-preceding era — the four source fixes that
> landed before this PR, ending with the encoder MISSREF
> fallback in `Variable.readDeclaration` that PR #3 replaced
> with the proper symbol-routing fix.

- **Branch:** `verify-cloudflare-sdk-pipeline` on
  `https://github.com/houstonhaynes/speakez-xantham`
- **Window:** 2026-05-15, post-merge of PR #3
- **Status:** 178 generator tests pass

> **Update 2026-05-16:** the original "file back upstream" decision
> on `Source.UnknownDeclared` was reversed once the goal shifted
> from "minimise consumer churn" to "get all Cloudflare runtime
> SDKs generating end-to-end so we can hard-count and classify the
> remaining errors." A walkthrough of subsequent work is in
> [§ "Update — work landed since this doc was written"](#update--work-landed-since-this-doc-was-written)
> at the bottom; the historical snapshot above is preserved as a
> capture of the decision point.

## What PR #3 brought in

Seven commits, summarised in the PR description:

> Variable Declarations: The fallback given to variable
> declarations was investigated in the context of merged
> interfaces and namespaces and has been resolved. The fallback
> is removed, and the declaration now routes correctly to the
> namespace.

The largest commits:

| Commit | Layer | What |
|---|---|---|
| `5ccf2ba` | encoder | Structured file logging in the encoder (operational; not behavioural) |
| `512a6ec` | encoder | **Replaced `Variable.readDeclaration` fallback with proper symbol routing.** `forwardToSymbolDeclaration` in `TypeFlagObject.fs` now walks *all* declarations of a symbol (not just the first usable one) and combines their builders via `Signal.computed`. Avoids stalls where the first declaration pointed back at the tag itself. |
| `e97bd70` | encoder | Memory conservation in symbol routing |
| `c1fffac` | encoder | Pass all encoder & decoder fixtures |

The replaced fallback is the one landed in post-pr2 as
`103d3ea` — `xanTag.Builder <- ValueSome (SType.Primitive NonPrimitive)`
when the inner builder is `ValueNone`. The new approach routes
to the right declaration (namespace + interface merge) and
preserves the actual semantic shape rather than collapsing to
`obj`. Conceptually cleaner.

## Compatibility break: `Source.UnknownDeclared` is incomplete

PR #3 surfaced a new fallback DU case for declarations the
encoder can't attribute to a known package or submodule:

```fsharp
[<Struct>]
type Source =
    | LibEs of fileName: string
    | PackageInternal of subModuleId: SubModuleId
    | Package of exportCollection: ExportCollection
    | UnknownDeclared of fileName: string   // NEW in e97bd70
```

It was introduced by commit `e97bd70 fix(encoder): memory
conservation in symbol routing to declarations`, which also
flipped the unattributable-declaration fallback in
`src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs:467` from
`Source.LibEs` to the new `Source.UnknownDeclared`:

```diff
- // Pre-PR3 fallback (e97bd70~1):
- sourceTag.Guard.Source.fileName
- |> Node.Api.path.basename
- |> Source.LibEs
+ // PR #3 fallback:
+ let source =
+     sourceTag.Guard.Source.fileName
+     |> Node.Api.path.basename
+     |> Source.UnknownDeclared
+ xanTag.trace (fun log tagId -> log.logft "..." tagId (source.ToString()))
+ source
```

**The DU case was added but not wired up.** The codec
(`Source.encode` / `Source.decode` in
`Xantham.Common/Common.Types.fs`) and several downstream `match`
statements on `Source` / `ArenaInterner.Source` weren't extended.
Running the new encoder against the agents fixture throws at JSON
serialisation time:

```
Exception { message: 'Match failure: Xantham.Source' }
```

Sites the new DU case breaks (verified by inspection — six
sites whose matches were exhaustive at three cases pre-PR3):

| File | Site |
|---|---|
| `src/Xantham.Common/Common.Types.fs` | `Source.encode` (line 1220) and `Source.decode` (line 1225) — the codec for the on-disk JSON shape |
| `src/Xantham.Decoder/Core.fs` | `ExportMap` builder's `getSource kv.Value` match (line 132) — runtime crash on agents during decode |
| `src/Xantham.Generator/Generator/TypeRefRender.Paths.fs` | `sourceToQualifiedNamePart` (line 25) — module-path qualifier resolution |
| `src/Xantham.Generator/Generator/Render.Collection.fs` | `tryRenderMetadataImport`'s `sourcePackageName` (line 68) — `[<Import>]` from-clause resolution |
| `src/Xantham.Generator/Generator/Render.fs` | `Customisation.Interceptors.IgnorePathRender.Source` (line 30) |
| `src/Xantham.Generator/Types/Generator.fs` | default `InterceptorIgnorePathRender.Source` (line 362) — same shape as `Render.fs` |

`src/Xantham.Decoder/Types/Arena.Interner.fs:982` (the
`buildSourceFromMetadata` mapping into the resolver-side DU)
*was* updated in `e97bd70` to include the case. Everything else
on the consumer side wasn't.

**Original (since-reversed) decision: file this back upstream
rather than patch locally.** A few candidate exhaustiveness fixes
were trialled in-session (extending the codec to
`{ "UnknownDeclared": <fileName> }`, returning `None` for the
qualifier, etc.) and *do* stop the crashes. But they don't address
what `UnknownDeclared` *means* — none of the downstream sites have
a meaningful answer for "what package does this declaration belong
to" because the case deliberately encodes "I don't know." Returning
`None` for the qualifier produces dangling path references (see the
`Types ×692` bucket below). That wasn't a usable end state on its
own; that observation still stands.

The pre-PR3 fallback to `Source.LibEs` was sub-optimal (it
misattributed unknown declarations as part of the TS standard
library) but every downstream site had a defined behaviour for
it and the output compiled. The PR #3 `Source.UnknownDeclared`
fallback as left in `e97bd70` was strictly worse from the
consumer's perspective until the wiring was completed.

**Update — superseded.** The "file back upstream" framing has
been retired (see top banner). The exhaustiveness patches were
landed locally in commits `5098692` (codec) / `12fb376` and
`4342c19` (consumer-site matches) / `2f22b15` (final pass and
the self-ref TypeQuery cycle break that unblocked the broader
pipeline). The remaining `UnknownDeclared` work (classifier
scope — making sure declarations that should attribute to known
packages don't fall through) lives in the encoder
(`src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs`) and is
in scope for this fork's continued work — not deferred to anyone
upstream.

The original three-option recommendation below is preserved as
the analysis snapshot. **Read it under the corrected scope:**
every option (revert the fallback flip, complete the wiring, or
refine the classifier) is an in-scope fix for this fork to
choose between, not a choice to defer.

Three options:

* (a) **revert the fallback flip in `TypeDeclaration.fs:467`** so
  unattributable declarations stay on `Source.LibEs`. Keep the
  `Source.UnknownDeclared` case defined for future use (no
  consumer impact until anything actually constructs it). The
  rest of `e97bd70` (the symbol-routing rewrite and the
  signal-memory work) stays.
* (b) **complete the wiring**: codec encode/decode for
  round-trip survival, plus a deliberate choice at each of the
  six consumer sites for what `UnknownDeclared` should
  semantically map to. (This is what landed in `5098692`–`2f22b15`
  as a conservative default — codec round-trips verbatim, qualifier
  returns `None`, metadata import returns `ValueNone`.) The
  dangling-reference symptom is still present and falls under
  the classifier scope question.
* (c) **rethink**: maybe what's needed isn't a new DU case but a
  refinement of when the fallback fires. If most of the agents
  declarations landing in this case *should* have proper
  `Source.Package` attribution (which is what the inconsistency
  in the `Types ×692` cohort below suggests), the right fix is
  in the classifier, not in adding a fallback case. This is the
  remaining live option for fully resolving the cohort.

## Verify counts after PR #3 (with in-session exhaustiveness probes, since reverted)

| SDK | Pre-PR3 (post-pr2 close) | Post-PR3 + exhaustiveness probes | Δ |
|---:|---:|---:|---:|
| dynamic-workflows | 5 | **2** | −3 |
| workers-types | 220 | 230 | +10 |
| agents | 1,291 | **2,539** | **+1,248** |
| **Total** | **1,516** | **2,771** | **+1,255** |

These counts are with the six exhaustiveness extensions trialled
in-session (and then reverted from the working tree at the time
this snapshot was taken, when the framing was still "file upstream").
They represented the floor of what the consumer side could produce
*given the new fallback as designed* — not a state any consumer
should land at without further work. Without the codec extension,
the pipeline couldn't even serialise; with it, downstream emits
dangling references for the `UnknownDeclared` declarations.
*(The exhaustiveness extensions were subsequently landed locally
in `5098692`+; see the Update section at the bottom.)*

The dynamic-workflows drop is the only clean win. Workers-types
ticks up slightly. **Agents nearly doubled.** The hypothesis below
explains why, but worth flagging up front: a 24-hour gap and a
PR with ~1,700 added lines that drove a 1,250-error regression
in the largest SDK is more churn than the previous PRs (PR #2
was net-neutral after dust settled; PRs #1 and earlier were
mostly net-positive).

## Where the agents regression went

Agents FS code distribution moved sharply:

| Code | Pre-PR3 | Post-PR3 | Δ |
|---:|---:|---:|---:|
| FS0039 (undefined name) | 1,790 | **3,978** | +2,188 |
| FS0033 (typar mismatch) | 132 | 422 | +290 |
| FS0001 (type mismatch) | 332 | 306 | −26 |
| FS0887 (abstract/concrete) | 34 | 118 | +84 |
| FS0698 (invalid constraint) | 60 | 52 | −8 |
| FS0663 (constraint inconsistency) | 54 | 46 | −8 |

Top FS0039 buckets in agents, post-PR3:

```
1464  'NoInfer'
 692  'Types'
  92  'Item'         (unchanged — IndexSignature self-ref bucket)
  42  'Type'
  32  'Code'
  30  'TypeName'
  30  'Invoke'
  24  'Optout'
  22  'Optin'
  20  'IteratorObject'   (new)
  20  'Flat'
  18  'Input'
  16  'StringIterator'   (new)
```

The pre-existing IndexSignature self-reference cohort (`Item` / `Type` /
`Code` / `TypeName` / etc.) is essentially unchanged — the
architectural fix proposed at the bottom of post-pr2 still applies
unmodified.

Two new cohorts dominate the post-PR3 delta:

### 1. `NoInfer ×1464` (largest single bucket)

`NoInfer<T>` is a TypeScript intrinsic added in TS 5.4 (essentially
the identity transform that opts out of typar inference at a
position). At line 6392 of agents.wrapped.fs:

```fsharp
type AgentWorkflowEvent = option<NoInfer>
```

`NoInfer` is referenced bare — no `<T>` argument, and no F# type
named `NoInfer` exists. The generator's substitution table has no
entry for it, and the encoder doesn't lower it to its inner type.

Pre-PR3, NoInfer references resolved via the
`fix: fix missing ref builder for intrinsic type NoInfer (#51)`
patch (`0c92526`, 2026-05-04). PR #3's symbol-routing rework
likely changed which dispatcher fires for intrinsic-type
references and the NoInfer-specific handling no longer takes
effect.

### 2. `Types ×692` (path-qualifier loss)

Inconsistent attribution: `Babel.Types.Comment` resolves (line
14582), but `Types.BabelTraverse.NodePath` (line 14448) doesn't.
Same source file, same module hierarchy, two references to
Babel types render with and without the `Babel.` prefix. The
inconsistency points at `UnknownDeclared` being applied to *some*
Babel declarations (path renders without the package qualifier)
while others get `Source.Package` (path renders with it).

My `sourceToQualifiedNamePart` fix returns `None` for
`UnknownDeclared` — which is correct given there's no resolvable
package — but the effect is that anything landing in
`UnknownDeclared` loses its module-qualifier and the rendered
reference becomes `Types.X` instead of `Babel.Types.X`. Other
references to the *same* underlying type still work because they
went through different attribution.

The right fix is in the encoder classifier: declarations that
obviously belong to a known package shouldn't land in
`UnknownDeclared` at all. `UnknownDeclared` should be the
fallback-of-last-resort for genuinely unattributable declarations,
not a sink for cases PR #3's new routing couldn't classify. The
classifier lives in
`src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs` and is
in scope for this fork's continued work.

## Open questions for review

1. **`Source.UnknownDeclared` wiring.** The primary blocker
   originally (see "Compatibility break" above). Six consumer
   sites either need behaviour defined or the fallback flip needs
   to revert to `Source.LibEs`. **Update — superseded**: the
   exhaustiveness wiring was landed locally in `5098692`–`2f22b15`
   with conservative defaults. The remaining live question is the
   classifier-scope one (item 3 below).

2. **`NoInfer` re-handling.** Pre-PR3 the intrinsic was
   short-circuited somewhere in the encoder's dispatch
   (per `#51`). Where, and what changed in `512a6ec` /
   `e97bd70` to bypass it? Either restore the intrinsic-type
   lowering, or add `"NoInfer"` to `LibEsDefaults.substitutions`
   in `Xantham.Generator/Types/Generator.fs` with a `Target = T`
   pass-through (probably the cleaner option since NoInfer is
   conceptually identity).

3. **`UnknownDeclared` scope.** What declarations land there
   post-PR3? Spot-check: at minimum some Babel declarations
   from agents do (the `Types ×692` cohort below shows a
   `Babel.Types.X` reference resolving in one site and a
   bare `Types.X` reference dangling in another, in the same
   file). Audit the encoder paths that classify into
   `UnknownDeclared` and confirm they're genuinely
   unattributable rather than a routing miss. A diagnostic
   counter (log/print) at the assignment site would help
   distinguish "this is a real fallback" from "this is a
   routing regression."

4. **Are dispatcher debug-chain calls the only change to
   `Signal` semantics?** PR #3's `_.chainDebug(callingTag)`
   insertions in `getSignalsFromTypeNodeOption` and
   `getTypeParamSlots` thread a calling-tag context through
   builder signals. Worth confirming this is purely
   diagnostic — no ordering / fulfillment behaviour change.

## Honest take

The pattern from PR #1 → PR #2 → PR #3 is encoder-layer work
increasing in scope and surfacing a wider blast radius across
the rest of the pipeline. PR #1 was schema additions; PR #2 was
the ExportMap drop fix plus the mapped-type encoder rework that
required ~10 generator-side patches in the
verify-cloudflare-sdk-pipeline branch to land cleanly; PR #3
added `UnknownDeclared` as a new fallback and the new
symbol-routing semantics, which required six exhaustiveness
patches just to *run*, and the fully-running output regressed
agents by ~1,250 errors at the time the original snapshot was
taken.

The substrate-flexibility hypothesis (Xantham as a durable
multi-consumer analysis layer) holds. Under the corrected
scope framing (see top banner), iteration cost across layers is
just normal in-fork work — none of it is downstream-of-upstream
churn. The post-PR2 fixes that landed in earlier sessions
(`3ae5680` T[] TypeKey collision, `a4681bc` typar merge,
`103d3ea` MISSREF fallback) were validated against the
then-current encoder; PR #3 changed the encoder substantially
enough that some of that validation isn't directly transferable —
re-validate as the affected paths are touched.

The next concrete bucket — IndexSignature / method-return
self-reference paths (`Item` ×N, etc.) — described at the bottom
of post-pr2 is *still the right next architectural target* once
`NoInfer` and the `UnknownDeclared` classifier-scope question are
addressed; the regression doesn't change its priority.

# Reproduction

```bash
# Rebuild encoder
cd /home/hhh/repos/speakez-xantham
npm run prestart

# Re-encode all three SDK fixtures
for fix in workers-types dynamic-workflows agents; do
  case $fix in
    workers-types) pkg="@cloudflare/workers-types" ;;
    dynamic-workflows) pkg="@cloudflare/dynamic-workflows" ;;
    agents) pkg="agents" ;;
  esac
  cd tests/Xantham.Decoder.Tests/fixtures/$fix
  node /home/hhh/repos/speakez-xantham/src/Xantham.Fable/output/Program.fs.js "$pkg" \
    -o /home/hhh/repos/Fidelity.CloudEdge/generators/xantham/output/$fix.json
  cd -
done

# Regenerate F# bindings and wrap
cd /home/hhh/repos/Fidelity.CloudEdge/generators/xantham
./scripts/regen-all.sh
./scripts/wrap-all.sh
./scripts/verify-all.sh
```

---

# Update — work landed since this doc was written

Window: 2026-05-15 evening → 2026-05-16. Seven commits on
`verify-cloudflare-sdk-pipeline` past `e65df55`:

| Commit | Layer | What |
|---|---|---|
| `15471bb` | FCE driver | Handle expanded Cloudflare runtime SDK set (originally framed as 3 → 13; **correction:** the actual target set is 12 runtime SDKs — see the top banner; the bare `cloudflare` aggregate is not a Xantham target) |
| `dd16eb8` | encoder | Source-side correction |
| `5098692` | common | `Source.UnknownDeclared` codec wired into `Common.Types.Source.encode/decode` (`{ "UnknownDeclared": <fileName> }` envelope) |
| `12fb376` `4342c19` | decoder, generator | Exhaustive `Source` match across the six consumer sites enumerated above |
| `2f22b15` | decoder, generator | **Self-ref `TsType.TypeQuery` cycle break** in `Arena.Interner.resolve` (10 of the runtime SDKs hung past 30 min before this) + final `UnknownDeclared` exhaustiveness |
| `e5feed6` | decoder, generator | **Backtick-free identifier renamer.** `Identifier.toSafe` replaces `NormalizeIdentifierBackticks`; `renderAbstractWithName` switched to `[<EmitMethod>]` (CompiledName is FS0755 on abstract methods) |

## Decisions reversed

**"File `Source.UnknownDeclared` back upstream."** Reversed. The
goal shifted from "minimise consumer-side churn" to "get all
runtime SDKs generating end-to-end so we have hard error counts
to classify." Patching exhaustiveness locally was the faster path
to that goal, and (under the now-corrected scope framing — see
top banner) every layer of Xantham is in scope for this fork
anyway.

The local fix for each of the six consumer sites is a
conservative default: codec round-trips the case verbatim; the
qualifier resolver returns `None`; metadata-import lookup
returns `ValueNone`; ignore-path decisions return `false`. None
of these change behaviour for `LibEs`/`PackageInternal`/`Package`
inputs; they only define what happens for `UnknownDeclared`
inputs, which without the patches threw `Match failure:
Xantham.Source` at JSON serialisation or render time.

The dangling-path concern documented in the original snapshot
(`Babel.Types.X` vs bare `Types.X` in agents) is still real. It's
a classifier problem in the encoder (`UnknownDeclared` being
applied to declarations that *should* attribute to known
packages), not a consumer-side wiring problem. The classifier
lives in `src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs`
and is in scope for continued work in this fork.

## What unblocked the broader runtime-SDK pipeline

PR #3 made the Cloudflare runtime SDK fixtures decodable end-to-end
*in principle*, but most of them hung past 30 minutes in the
generator. Heartbeat instrumentation localised it to
`Interface.MessageEvent` member #5 (the `source` property).
Root cause: `TsType.TypeQuery` self-references — the encoder
logged `[CIRCREF]` warnings but still emitted them, and
`Arena.Interner.resolve` had no break for `tq.Type = typeKey`
self-references. Fixed in `2f22b15` with a self-ref short-circuit
that returns `ResolvedType.Primitive NonPrimitive` for the
self-pointing case:

```fsharp
let resolvedValue =
    match typeMap[typeKey] with
    | TsType.TypeQuery tq when tq.Type = typeKey ->
        ResolvedType.Primitive TypeKindPrimitive.NonPrimitive
    | t -> buildFrom (isLibEs typeKey) t
```

Plus a defence-in-depth guard at the `RenderScope.Prelude.TypeQuery`
branch (`+18 lines`) that liftNullables to `obj` when the
prerender-side cycle isn't caught upstream.

After these, all runtime SDKs generated F# end-to-end. Total
verify errors at that point: **666 raw**, compared with the
1,516 the original snapshot reports for a smaller 3-SDK set after
the in-session exhaustiveness probes. Current state (post the
`e5feed6` renamer) is **556 raw / ~170 distinct across the 12
runtime SDKs** — see [[reference_verify_pipeline]] for the
breakdown.

## Backtick-free identifier renamer

`e5feed6` is a Xantham-wide redesign of the
"how do we render TS identifiers that collide with F# syntax"
question. Before this commit the answer was Fantomas's
`NormalizeIdentifierBackticks`, which wraps any unsafe identifier
in `` ` ` `` backticks. That works at the compile level but
leaks into every consumer's call site — `widget.``fixed``()`
instead of `widget.fixed_()`.

The redesign moves to a **rename-with-Fable-interop-attribute**
pattern that the existing `Name.Modified(original, renamed)` DU
already supports:

| Case | Source | F# emitted | Interop attribute |
|---|---|---|---|
| F# keyword | `fixed` | `fixed_` | `[<EmitMethod("fixed")>]` |
| Bare `_` | `_` | `anon` | `[<EmitProperty("_")>]` (property) / none (parameter) |
| Empty-after-sanitize | `"\n"` | `Newline` | `[<CompiledName("\n")>]` |
| Leading digit | `2fa` | `_2fa` | `[<CompiledName("2fa")>]` |

Key implementation notes:

1. **`Identifier.toSafe`** replaces `NormalizeIdentifierBackticks` in
   `Name.fs`'s `Normalization.normalize`. It composes
   `sanitizeOrName` (which keeps the casing-boundary chars `.` `/`
   `-` so pascal/camel-case can fold them) with the rename rules
   above.
2. **F# keyword list** is current keywords only. Reserved
   future-use words (`event`, `method`, `mixin`, etc.) pass through
   unchanged — the F# compiler accepts them today, and renaming
   them mass-renames real TS API surface (e.g. `event` parameter
   names) for no compile gain.
3. **Abstract-method attribute is `EmitMethod`, not `CompiledName`.**
   F# rejects `[<CompiledName>]` on `abstract X: ... -> T`
   (FS0755). EmitMethod is Fable's purpose-built attribute for the
   same role and is accepted in this position. Properties continue
   to use `EmitProperty` (already in place pre-edit). The
   `[<CompiledName>]` is still used on union/enum cases where F#
   does accept it.
4. **`Name.Modified` propagation** drives the attribute emission
   automatically via `AttributesBuilderBase.MakeAttributeIfModified`
   — no per-site decoration needed for individual renames.

## Current state — verify counts

Raw and distinct (file:line:col-deduplicated) error counts after
the post-PR3 work landed (parser-bail unmask through walker
boundary-stop). **Cloudflare row dropped** — the bare `cloudflare`
package is the management REST client, not a Xantham target (see
top banner). Target set is **12 runtime SDKs**.

| SDK | Raw | Distinct |
|---|---:|---:|
| Agents | 1,648 | 472 |
| AiChat | 1,568 | 468 |
| Codemode | 235 | 60 |
| Containers | 708 | 170 |
| DynamicWorkflows | 29 | 7 |
| Puppeteer | 25 | 10 |
| Sandbox | 716 | 175 |
| Shell | 707 | 167 |
| Think | 1,580 | 486 |
| Voice | 1,670 | 482 |
| WorkerBundler | 700 | 165 |
| WorkersTypes | 269 | 86 |
| **Total** | **9,855** | **2,748** |

The post-PR3 work peeled successive layers of masking and over-
capture: parser-bail unmask exposed real downstream errors,
arity reconciliation absorbed most of the use-site noise, and
boundary-stopping the three free-typar walkers (synthetic in
`SyntheticPathAssignment`, alias-body in `Render.TypeAlias`,
arity-precomputation in `prerenderTypeAliases`) eliminated the
synthetic-typar over-capture cohort. See
`docs/plans/post-pr3-re-engineering.md` for the principle behind
each phase.

The early post-PR3 totals (615 raw / 549 distinct in the table
that previously occupied this slot) were under parser-bail masking
— ~6× as much real downstream noise was hidden by an upstream
parser failure. That table reflected what the F# compiler *saw*
before bailing, not what was actually wrong.

The remaining histogram is dominated by:

* **FS0039 typar-not-defined** (~1,100 across SDKs) — the
  dominant remaining cohort. Manifestations like `'T not defined`,
  `'Value not defined`, `'Output not defined`, `'EventEmitter
  not defined`, `'Options not defined in ...GenerateKeyPairSync...`.
  Root cause: alias-body use-site instantiation gap. When TS
  source has `type Foo<T> = { x: T }` and the consumer site uses
  `Foo<string>`, the encoder/decoder doesn't substitute `T → string`
  in the body — the body is captured as a structural literal with
  `x: T` and emitted at the use site with orphan `T`. TS's own
  compiler API does this substitution implicitly via
  `getTypeOfSymbol` on the substituted property symbol, but the
  encoder reads property types through `valueDeclaration` (i.e.
  source declaration), not through the substituted type chain.
  Closing this requires the encoder's property-reading path to
  switch from declaration-based to type-based at instantiation
  sites — a substantial refactor in `TypeFlagObject` dispatch.
  See post-pr3-re-engineering.md for the three architectural
  options enumerated; none is a one-keystroke change.
* **FS0033 arity mismatches** (~165 Think alone) — surviving
  use sites where a cycle-broken alias target still receives args
  (e.g. `ZodTypeAny<obj, ZodTypeDef, 'Output>` where `ZodTypeAny`
  collapsed to `obj`). Phase C / Phase G fallback drops args at
  remap and at non-remap Prefix sites, but the dominant
  ZodTypeAny cohort routes through a path where the head-atom's
  path doesn't match `CycleBrokenPaths` (target-key vs body-key
  mismatch). The principled fix requires cycle-broken metadata
  to flow through the TypeAliasRemap chain consistently.
* **FS0001 missing constraints** (~163 Think) — type aliases
  with many declared typars whose bodies reference synthetics
  with constrained typars; F# infers the constraints and rejects
  the alias declaration that lacks them. Downstream of the
  substitution gap above.

For the complete current roadmap including the additional
generator-side categories (`.node` member names, StringEnum
collisions, AllowNullLiteral discipline, etc.) see
[[project_pathway_full_sdk_compilation]] in memory.

## Open questions — current status

| Original § | Status |
|---|---|
| §1 `Source.UnknownDeclared` wiring | **Resolved locally** in `5098692`/`12fb376`/`4342c19`/`2f22b15`. Six consumer sites have defined behaviour. The qualifier-loss / classifier-routing question (§3 below) is the live remainder. |
| §2 `NoInfer` re-handling | **Still open.** Largest remaining FS0039 bucket. Untouched in subsequent commits. |
| §3 `UnknownDeclared` scope (Babel attribution) | **Still open.** Manifests as path-qualifier loss for some references in agents et al. Lives in the encoder classifier (`src/Xantham.Fable/Reading/Dispatch/TypeDeclaration.fs`) — in scope for this fork. |
| §4 Dispatcher debug-chain semantic equivalence | **Untouched** — no behavioural regressions observed that point at it, so deprioritised. |

New open questions surfaced since:

5. **F# keyword set completeness.** `Identifier.fsharpKeywords` is
   the current best-guess of what F# rejects today. The
   bake-in is empirical (errors emerged from real verify
   output and were added one by one — `const`, `constraint`,
   etc.). If new TS APIs surface a collision with a word not in
   the set, the symptom is FS0010 on the generated code. A
   future audit against the official F# spec keyword list
   would tighten this.

6. **`L46818`-class structural emission bugs.** The cohort
   that this session's renamer work targeted included one
   structural bug: a `module rec Typescript =` emitted at
   column 0 mid-file in agents.wrapped.fs (when it should have
   been nested under the parent module). This is a Generator
   indentation/scope bug, not a name-escape issue. Out of scope
   for the renamer work but still a real defect.
