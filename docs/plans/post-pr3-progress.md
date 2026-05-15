# Post-PR3 Progress Report

> Handover note for Shayan. Picks up after PR #3
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

## What PR #3 brought in

Seven commits, summarised by Shayan in the PR description:

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

**Decision: file this back upstream rather than patch locally.**
A few candidate exhaustiveness fixes were trialled in-session
(extending the codec to `{ "UnknownDeclared": <fileName> }`,
returning `None` for the qualifier, etc.) and *do* stop the
crashes. But they don't address what `UnknownDeclared` *means* —
none of the downstream sites have a meaningful answer for "what
package does this declaration belong to" because the case
deliberately encodes "I don't know." Returning `None` for the
qualifier produces dangling path references (see the `Types ×692`
bucket below). That's not a usable end state.

The pre-PR3 fallback to `Source.LibEs` was sub-optimal (it
misattributed unknown declarations as part of the TS standard
library) but every downstream site had a defined behaviour for
it and the output compiled. The PR #3 `Source.UnknownDeclared`
fallback is strictly worse from the consumer's perspective until
the wiring is complete — and the design choices for what each
consumer site should *do* with an unknown attribution belong with
the author of the case, not the downstream user.

Recommendation for follow-up PR by Shayan: pick one of —

* (a) **revert the fallback flip in `TypeDeclaration.fs:467`** so
  unattributable declarations stay on `Source.LibEs`. Keep the
  `Source.UnknownDeclared` case defined for future use (no
  consumer impact until anything actually constructs it). The
  rest of `e97bd70` (the symbol-routing rewrite and the
  signal-memory work) stays.
* (b) **complete the wiring**: codec encode/decode for
  round-trip survival, plus a deliberate choice at each of the
  six consumer sites for what `UnknownDeclared` should
  semantically map to. Probably needs design discussion — the
  obvious answer of "treat as LibEs" defeats the purpose of
  having a distinct case; "treat as none" produces the dangling
  references we're seeing.
* (c) **rethink**: maybe what's needed isn't a new DU case but a
  refinement of when the fallback fires. If most of the agents
  declarations landing in this case *should* have proper
  `Source.Package` attribution (which is what the inconsistency
  in the `Types ×692` cohort below suggests), the right fix is
  in the classifier, not in adding a fallback case.

## Verify counts after PR #3 (with in-session exhaustiveness probes, since reverted)

| SDK | Pre-PR3 (post-pr2 close) | Post-PR3 + exhaustiveness probes | Δ |
|---:|---:|---:|---:|
| dynamic-workflows | 5 | **2** | −3 |
| workers-types | 220 | 230 | +10 |
| agents | 1,291 | **2,539** | **+1,248** |
| **Total** | **1,516** | **2,771** | **+1,255** |

These counts are with the six exhaustiveness extensions trialled
in-session (and then reverted from the working tree, given the
"file back upstream" decision). They represent the floor of
what the consumer side can produce *given the new fallback as
designed* — not a state any consumer should be expected to
land. Without the codec extension, the pipeline can't even
serialise; with it, downstream emits dangling references for
the `UnknownDeclared` declarations.

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

The right fix is upstream: declarations that obviously belong to
a known package shouldn't land in `UnknownDeclared` at all.
`UnknownDeclared` should be the fallback-of-last-resort for
genuinely unattributable declarations, not a sink for cases
PR #3's new routing couldn't classify.

## Open questions for review

1. **`Source.UnknownDeclared` wiring.** The primary blocker
   (see "Compatibility break" above). Six downstream sites
   either need behaviour defined or the fallback flip needs to
   revert to `Source.LibEs`. Filed back to Shayan for resolution.

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

The pattern from PR #1 → PR #2 → PR #3 is encoder-side work
increasing in scope and surfacing a wider blast radius
downstream. PR #1 was schema additions; PR #2 was the
ExportMap drop fix plus the mapped-type encoder rework that
required ~10 generator-side patches in the verify-cloudflare-sdk-pipeline
branch to land cleanly; PR #3 added `UnknownDeclared` as a
new fallback and the new symbol-routing semantics, which
required six exhaustiveness patches just to *run*, and the
fully-running output regressed agents by ~1,250 errors.

The substrate-flexibility hypothesis (Xantham as a durable
multi-consumer analysis layer) holds, but the cost of each
encoder iteration on downstream consumers is meaningful. The
post-PR2 fixes that landed earlier this session
(`3ae5680` T[] TypeKey collision, `a4681bc` typar merge,
`103d3ea` MISSREF fallback) were validated against a stable
encoder; PR #3 then changed the encoder substantially enough
that some of that validation isn't directly transferable.

The next concrete bucket — IndexSignature / method-return
self-reference paths (`Item` ×92, etc.) — described at the
bottom of post-pr2 is *still the right next architectural
target* once `NoInfer` and the `UnknownDeclared` scope
question are addressed; the regression doesn't change its
priority.

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
