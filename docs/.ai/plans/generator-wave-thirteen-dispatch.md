---
category: Generator
audience: managing agent
title: Dispatch - generator wave thirteen
integration-branch: worktree-generator-wave-thirteen
---

# Generator wave thirteen — the fidelity queue, opened

Wave twelve's closing worklist put the fidelity queue first and left it priced. This wave spends
that price. Every mechanism below was re-verified against the current tree before dispatch: the
lines lane R3 cites in `docs/.ai/handovers/lane-r3.md` are still present, and the two recon
documents it rests on — `generator-cloudflare-recon.md` and `generator-tr018-recon.md` — still
describe what the code does.

`worktree-generator-wave-thirteen` forks `master` at `ffacc3d`, which is wave twelve merged. The
alpha packaging, the `FollowDepth` constant wave eleven settled at 20, and the delegate
declarations wave twelve landed are all untouched.

## Lanes

Three lanes in batch one, one held for batch two. Lane R3's dispatch order is followed, with its
first three entries collapsed into a single lane: all three land in `erasedUnionRef` or in the
constant that function reads, and two agents inside one function is a merge chosen rather than
suffered.

### Lane CA — the cheap three

Branch `worktree-gen-wave13-ca`, forked from this integration branch. Three changes, three
commits, each measured on its own.

1. **Deduplicate `erasedUnionRef`'s findings by *(owner, rendered message)*.** The function
   accumulates every arm's findings inside the per-arm map while `List.distinct` collapses the
   arms themselves, so a union whose fourteen arms all map to `string` reports fourteen identical
   findings against one rendered type. Keying on the owner alone is wrong: `BodyInit`-shaped
   owners carry several genuinely distinct messages that must all survive. Emits no golden line;
   the whole change is manifest-visible.
2. **`ErasedUnionArity` 4 → 9.** `Fable.Core` 5.2.0 ships `U2`–`U9`, and `Render.fs` already
   builds the type name from the arity, so the renderer needs no work. This reopens D4's recorded
   decision, which is the change's real cost.
3. **`Model.LibBindings` gains `AsyncIterableIterator` → `JS.AsyncIterable`,** with a loss note.
   The table already carries `AsyncIterable`, `AsyncIterator` and `AsyncGenerator`; this is the
   family's missing fourth name. `Error` → `exn` landed in an earlier wave and needs nothing.

Owns `Shape/Spec.fs` (`erasedUnionRef` and `ErasedUnionArity`), `Model.fs`, the doc comments in
`Render.fs`, the `UnionTooWide(5, 4)` literal in `Findings.test.fs`, and D4's record in
`docs/.ai/plans/generator-type-mapping.md`. No new finding case.

### Lane CB — a tagged union refused on a shared tag says so

Branch `worktree-gen-wave13-cb`, forked from this integration branch. `taggedUnionShape` requires
every arm's tag value to be distinct and returns `None` where two collide. `DT.ArmNotPlainData` is
raised only on the success path, so the refusal is silent: the union falls through to
`erasedUnionRef` and the manifest carries no `DT` finding for it under any name.

Two outcomes are acceptable, in this order of preference: merge the arms sharing a value into one
case carrying the members they agree on, or refuse loudly. **Refusing silently is what the lane
exists to end.** A DU case per arm where two arms share a tag is not legal F#, so the merge has to
be real or not attempted.

`taggedUnionShape` has exactly one call site, `Shape/TaggedUnions.fs:34`, which is the seam: it
can return a refusal reason rather than a bare `None`, and the pass raises the finding.

Owns `Shape/TaggedUnions.fs` and `taggedUnionShape` (`Shape/Spec.fs:195-240`).

**Out of scope, deliberately: recon §5.3, the inline discriminated union at member position.**
`detectTaggedUnions` iterates `model.DeclNames`, so a union written inline has no name to be
offered under. Reaching it means minting one through `Anonymous.claim` inside a pass that names
nothing today — a wave-sized change for one recorded site. It stays on the worklist.

### Lane CC — remeasure `TR018` before anyone implements against it

Branch `worktree-gen-wave13-cc`. Read-only. Lane R3 found the `TR018` recon's site list stale: its
causes C and D landed in an earlier wave and took the cheap half of the count with them. Two
clusters in the residue are territory the document never covered.

- **`animejs`'s eight `*.then(callback)(self)` sites** look cause-C-shaped but survived the
  cause-C fix. A fresh reproducer decides whether they share that mechanism.
- **`@cloudflare/workers-types`'s four `Workflow*.Config.retries` owners**, which the original
  recon never saw.

The remainder is very likely cause B — an intersection generic over a type parameter — which is a
floor rather than a backlog, and `type-fest`'s eight are confirmed unrecoverable. The lane's
deliverable is a verdict per cluster with a reproducer under ten lines for anything it calls
recoverable, written to `docs/.ai/handovers/lane-cc.md`. It opens no implementation lane itself.

Owns nothing under `src/` or `tests/`.

### Held for batch two — Lane CD, indexed access over a concrete operand

`TR020` 69, recon lane Q3, `indexedAccessRef`. Sequenced after CA: an event map with more than
nine entries currently lands on `obj` through `TR036`, so once the cap is 9 those sites move
between keys rather than down. Measuring Q3 before CA merges would price it wrong.

## Ownership map

| File | Lane |
| --- | --- |
| `Shape/Spec.fs` — `erasedUnionRef`, `ErasedUnionArity` | CA |
| `Shape/Spec.fs` — `taggedUnionShape` | CB |
| `Shape/TaggedUnions.fs` | CB |
| `Model.fs`, `Render.fs`, `Findings.test.fs` | CA |
| `Findings.fs` | neither — pre-declared below |

CA and CB share `Shape/Spec.fs` and are dispatched in parallel regardless: the two regions sit
about 1,400 lines apart and merge by hunk. This is the one exception to "give each agent a
different pass file" this wave takes, recorded so the merge is expected rather than discovered.

## Pre-declared finding cases

Appended on the integration branch before dispatch, so lane CB never has to come back for one. A
case that turns out unused costs one dead row.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `DT.TagValueShared of tag: string * value: string` | `DT003` | ergonomic | Lane CB |
| `DT.ArmsMergedOnSharedTag of tag: string * value: string` | `DT004` | ergonomic | Lane CB |

Both are graded ergonomic to match `DT001`, which describes the same outcome: the union survives,
as an erased union rather than a discriminated one. Where the fallback then exceeds the arity cap,
`TR036` records that loss separately and keeps the two apart.

## What every lane owes back

Fifteen lines to the managing agent; the full report to `docs/.ai/handovers/lane-<id>.md` on the
lane's own branch. Counts, not verdicts:

- `dotnet fsi build.fsx -- findings` before and after, and the per-key delta for every code that
  moved — not only the targeted one.
- Tier counts, run-gate check count, generator and wire test counts.
- `git diff --stat` over the regenerated goldens.
- Branch and commit SHA.
- Anything a large fixture did that the lab fixture does not account for. **Report it and stop**
  rather than chasing it.

Lane CA additionally owes the **arity histogram of the sites `UnionTooWide` widens today** — how
many are arity 5, 6, 7, 8, 9, and how many above 9 — measured before the constant moves. Raising
the cap reopens a decision that was recorded with a reason, and the histogram is what lets the
threshold be chosen on evidence rather than on the largest number `Fable.Core` happens to ship.

## Gate and corpus

Baseline, measured on this branch at the fork, reproducing wave twelve's recorded numbers exactly:

| | wave twelve `ffacc3d` | composed |
| --- | ---: | ---: |
| generator tests | 469 | |
| wire tests | 90 | |
| run gate checks | 297 | |
| exact | 513 | |
| ergonomic | 1616 | |
| widened | 790 | |
| escape | 195 | |
| total findings | 17,928 | |
| `TR018` | 82 | |
| `TR020` | 69 | |
| `TR023` | 137 | |
| `TR036` | 72 | |
| `TR037` | 54 | |
| `DO001` | 5 | |
| `DT001` / `DT002` | 2 / 14 | |
| `RT001` | 3 | |
| exit code | 0 | 0 |

Composed at `01b0db4`, gated once over the merged tree with
`dotnet fsi build.fsx -- test --update`: every stage ok, run gate 300 checks, exit 0, and
**regeneration moved not one golden line** — the three branches composed without interacting.

| | wave twelve `ffacc3d` | composed |
| --- | ---: | ---: |
| generator tests | 469 | 472 |
| wire tests | 90 | 90 |
| run gate checks | 297 | 300 |
| exact | 513 | 530 |
| ergonomic | 1616 | 1633 |
| widened | 790 | **776** |
| escape | 195 | 195 |
| total findings | 17,928 | **16,870** |
| `TR018` | 82 | 59 |
| `TR020` | 69 | 69 |
| `TR023` | 137 | 136 |
| `TR036` | 72 | **9** |
| `TR037` | 54 | 15 |
| `DT002` / `DT003` / `DT004` | 14 / 0 / 0 | 17 / 1 / 1 |
| `DO001` / `RT001` | 5 / 3 | 5 / 3 |

### Composition was checked, not assumed

Each lane's independently measured delta survives composition exactly. Lane CA alone reported
tiers 527/1630/762/195 at 16,826 findings; lane CB alone reported +3 exact, +3 ergonomic, +14
widened and +44 findings, all of them its new fixture's own. The composed tree is 530/1633/776/195
at 16,870 — CA's numbers plus CB's deltas, to the unit. No interaction between branches to find.

### The headline number is mostly re-keying, and says so

Findings fall by 1,058. That is overwhelmingly lane CA's dedup removing duplicate *reports* of
losses already counted, not losses recovered: the change emits no golden line at all. The fidelity
gains are `TR036` 72 → 9, `TR023` 137 → 136, and the tier movement — widened falls by 14 with
nothing entering escape.

`TR018`'s fall from 82 to 59 is the same dedup reaching a key nobody targeted, predicted by lane CC
before it happened. **Lane CC's corrected site table was therefore stale on arrival**: the standing
figure is 59 corpus-wide and 13 on animejs. Any lane opened against CC's table re-reads it against
this tree.

## Outcomes, and what carries forward

1. **The threshold is settled at 9, and the way past it is closed.** Arity 5 holds 32 of the 69
   widened sites, so the first step off 4 buys nearly half the win; the step from 8 to 9 buys one
   site. The remaining 9 sit at arity 10, 11 and 14, above anything `Fable.Core` ships.

   Uncapping to 100 was directed, scoped and then withdrawn on cost. It would have meant
   generating `U10`-`U100` into `Xantham.Fable.Core` under the `Fable.Core` namespace - 5,005 union
   cases and 5,005 `op_ErasedCast` overloads - and every `!^` in consumer code would resolve
   against one overload per case of the target type. The lane was stopped before it wrote anything.
   **The arity ceiling is what `Fable.Core` ships**, and the nine sites above it widen to `obj`.

   The proof obligation that lane would have carried is worth recording even though it went
   unspent: a `U10` this repository declares is erased by the `[<Erase>]` attribute, which is a
   general mechanism, so it *should* behave as `U9` does - but an erased union that is not actually
   erased fails silently at runtime, which is the failure mode wave twelve's lane BA exists
   because of. Any future attempt at this must prove `U10` at the run gate beside a `U9` control
   before generating the other ninety.
2. **Lane CB's fold is built and reaches no corpus site,** because the managing agent scoped recon
   §5.3 out. `TailStream.EventType` arrives as an inline union at callback-parameter position and
   `detectTaggedUnions` iterates `model.DeclNames`, so §5.3 is not a separate nicety — it is the
   gate on `DT003`/`DT004` ever firing. It is repriced and goes back on the worklist as the lane
   that makes CB's work reachable, not as one recorded site.
3. **`TR018` is 73-out-of-82 contract.** Lane CC's remeasurement closed it rather than opening it.
   The one recoverable cluster is nine sites: `this & { then: null }` is TypeScript's
   uninhabited-intersection reduction to `never`, arriving flagged Intersection with no members,
   and the current message is false at all nine. A lane for those nine needs
   `TR.UninhabitedIntersectionReduced of property: string` pre-declared; `TR058` is the next free
   code.
4. **The recon's `Log` reproducer does not reach the pass.** The checker distributes the
   intersection into arms flagged `Intersection` while `isObjectMember` tests `TypeFlags.Object`;
   their members are populated, so only the flag rejects them. Admitting intersections there moves
   discriminated-union detection corpus-wide and is unpriced. `shared-tag-lab` pins today's
   behaviour as the standing reproducer.
5. **Narrow the pattern before grepping a large `symbols.jsonl`.** Lane CB spent avoidable context
   on three very long lines. The rule permits the grep; briefs should ask for a narrowed pattern.

## Batch two

| Lane | Work | Owns | Pre-declared case |
| --- | --- | --- | --- |
| CD | `TR020` 69, indexed access over a concrete operand (recon lane Q3) | `indexedAccessRef`, `Shape/Spec.fs` | none |
| CE | The nine uninhabited-intersection sites | `intersectionRef`, `Shape/Spec.fs` | `TR058` |
| CF | Recon §5.3, inline discriminated unions at member position | `Shape/TaggedUnions.fs`, `Shape/Anonymous.fs` | none — `DT003`/`DT004` already exist |

`indexedAccessRef` and `intersectionRef` sit far apart in `Shape/Spec.fs` and merge by hunk, as
CA and CB did this wave. CD was held out of batch one because raising the arity cap moves its sites
between keys rather than down; that raise has now landed, so `TR020` 69 is a true baseline.

---

## Batch two, composed and gated

Composed at `93abc84`: **304 run-gate checks, every stage ok, exit 0.**

| | wave twelve `ffacc3d` | batch one | batch two |
| --- | ---: | ---: | ---: |
| run gate checks | 297 | 300 | **304** |
| exact | 513 | 530 | 536 |
| ergonomic | 1616 | 1633 | 1665 |
| widened | 790 | 776 | 792 |
| escape | 195 | 195 | 201 |
| total findings | 17,928 | 16,870 | 16,994 |
| `TR018` | 82 | 59 | **53** |
| `TR020` | 69 | 69 | **53** |
| `TR023` | 137 | 136 | 136 |
| `TR036` | 72 | 9 | 10 |
| `TR037` | 54 | 15 | 15 |
| `TR058` | — | 0 | 10 |
| `DT002` | 14 | 17 | 24 |

### The tier rise is four new lab fixtures, and it was checked

`widened` rose 2 and `escape` rose 6 against batch one, which is the shape a new loss takes. Per
fixture, it is not one:

| fixture | exact | ergonomic | widened | escape |
| --- | ---: | ---: | ---: | ---: |
| `@cloudflare/workers-types` | +3 | +5 | **−5** | 0 |
| `animejs` | +11 | +3 | **−14** | 0 |
| `solid-js` | 0 | +7 | **−7** | 0 |
| `phase-b-lab` | +1 | 0 | **−1** | 0 |
| `nominal-lab` | +1 | 0 | 0 | 0 |
| `indexed-access-lab` *(new)* | 0 | +25 | +3 | 0 |
| `shared-tag-lab` *(new)* | +7 | +6 | +25 | 0 |
| `uninhabited-intersection-lab` *(new)* | 0 | +3 | +1 | +6 |

**Every existing fixture's widened count fell — 27 between them — and not one existing fixture's
escape count moved at all.** The whole of the +2 widened and +6 escape belongs to lab fixtures that
did not exist at the start of the wave, and a lab exists precisely to carry hard cases. The honest
headline for the corpus is: **widened −27, escape unchanged.**

### Composition found something no lane could

The `@cloudflare` manifest conflicted between lanes CD and CF. Resolved by taking one side and
regenerating, and the regeneration was **not** a no-op: `shape-interfaces` settles at 7,984
findings with 502 widened, against the 7,989 and 507 the taken side carried. Five widened symbols
exist on neither branch alone — lane CD resolves indexed accesses that lane CF's newly named unions
then answer for. That is the case for gating a batch as a batch rather than per lane.

## Batch three - lane CH, composed and gated

An object whose whole content is one index signature references
`Xantham.Fable.Core.Record<'Key,'Value>` rather than minting an interface. `MB004` falls 140 to
63; `TR059` reads 221, counted per reference site where `MB004` counted per declaration. Composed
totals: tiers 535/1603/797/200 over 3,135 symbols, 17,040 findings, `SY004` 726. Tests 490, wire
90, run gate 309 with five new `recordIndex()` checks. Gate green, tree clean.

### The widened column rose and no mapping got worse

Widened reads 792 to 797 and three symbols reach escape. Both read as regressions and neither is
one. Sixty-seven declarations vanish; five were widened, four were escape, and every one carried
`MB004`, `SP003` and `SY004` - a minted anonymous type. Their marks reattached to the sites
referencing them.

The escape three are exact. `DynamicDispatchOptions.Outbound`, `DispatchNamespace.Get.Args` and
`animejs`'s `Scope.Data` each owned a `TR008` for an `any`-valued index signature; inlined as
`Record<string, obj>`, that `TR008` lands on `DynamicDispatchOptions`, `DispatchNamespace` and
`Scope`. The fourth, `solid-js`'s `SharedConfig.Resources`, moved nothing, because `SharedConfig`
was already escape. The widened five reach twelve sites the same way -
`FunctionDefinition.parameters` read `RequestInitCfProperties.Base` and now reads
`Record<string, obj>`, the same `obj` written where the caller can see it.

One declaration serves many references, so re-attribution raises a count while improving the
surface: five widened declarations become twelve widened sites. **The widened column is not
comparable across this merge.**

The lane reported the net `+5` and set it aside as below its effort budget. The gross is twelve
and the escape movement went unreported; both were attributed at merge, from the per-symbol tiers
in `symbols.jsonl` across the fork and the lane tip.

### The third sighting was confirmed

Lane CH was briefed to expect lane CF's rule again and met it. A real declaration keeps its name
and only a `synthesize-anonymous` claim does not, tested through `SymbolName` syntheticity.
Generic index signatures needed no `FreeTypeParams.fs` change but did need two empirical
exclusions: unused declared type parameters, and self or mutual recursion, which would otherwise
reach FS0953. `Record` is always written fully qualified, so no collision path exists.

## Carried forward

1. **`isObjectMember` rejects intersection arms.** `TailStream.EventType`'s three sites are all
   that stand between lane CB's shared-tag fold and the site that motivated it. The checker
   distributes the intersection into arms flagged `Intersection` while the pass tests
   `TypeFlags.Object`; their members are populated, so only the flag rejects them. Admitting
   intersections moves discriminated-union detection corpus-wide and is unpriced.
2. **The resolve tier does not follow lib interface members.** `animejs`'s 13 remaining `TR020`
   sites are lane CD's own shape over `lib.dom`'s `HTMLElementTagNameMap`, which reaches the type
   table carrying zero members. Resolve-tier follow policy, not a `Shape/` change.
3. **The CLI emits less than the harness.** `dotnet run --project src/Xantham.Cli -- generate` over
   `@cloudflare/workers-types` produces 26k lines where the harness produces 30k. The CLI is the
   user-facing entry point, so this is a candidate defect rather than a probing caveat.
4. **`TR023` 136 is a floor.** Lane CA took the one recoverable row; `Iterable`,
   `IterableIterator` and `BigUint64Array` are verified absent from the pinned `Fable.Core`.

5. **Exclusive-arm object unions mint one interface per arm.** A union whose arms differ only in
   which members are `?: never` is emitted as a `U2` alias over two minted interfaces with
   identical shared members. `ContainerStartupOptions` is
   `U2<Container.Start.Options, Container.Start.Options2>`, seven shared members written twice;
   `AiSearchSearchRequest` is `U2<AiSearchSearchRequest2, AiSearchSearchRequest3>`. Sixteen types
   in the `@cloudflare` golden carry a `never`-typed member, rendered
   `abstract image: unit option with get, set` and `?image: unit` in `Create`.

   **Fold the arms into one interface and carry the exclusivity on `Create` instead.** One name,
   every member of the union present, exclusive members optional, and one
   `[<ParamObject; Emit("$0")>]` `Create` overload per arm holding that arm's required members and
   omitting the members that arm declares `never`. Call sites drop the `!^` cast into an arm and
   the `unit` members leave the surface.

   Exclusivity then holds at construction rather than for the life of the value: both setters stay
   reachable on a folded instance. That is the trade being asked for.

   Four things to settle before dispatch:
   - **Arms must differ by a required parameter.** `Container.Start.Options2`'s
     `containerSnapshot` is optional in the source, so its overload holds no required member the
     other lacks and F# cannot separate the two. `AiSearchSearchRequest`'s `query`/`messages` pair
     can be separated; the container pair needs a decision.
   - **Shared members must agree.** Fold where arms carry the same member set modulo `never` with
     matching types on the shared ones. Disagreement widens, so it declines.
   - **Fable must accept overloaded `[<ParamObject; Emit("$0")>]` statics.** Run-gate check
     required, not assumed.
   - `D1Response.error: unit option` has no sibling arm. A lone `never` member comes from some
     other mechanism and this item does not cover it.

   Needs a new `Ergonomic` case at `TR060`; `TR059` belongs to lane CH.

## What this wave says about the generator

Two lanes independently found the same boundary. Wave twelve's lane BB reverted naming
*applications of generic callback aliases* because it widened nine members to `obj`; lane CF had to
guard against renaming `ResponseInputContent`, a union another declaration already answers for by
member set. Both are the same rule discovered twice, in different passes: **a shape another
declaration answers for should not be given a second name.** Lane CH is briefed to expect it a
third time, for index-signature objects a package has already named.

The arity threshold looked like the wave's central question and was not. Lane CF measured that
gating an inline union claim on "only above the cap" claims exactly one union corpus-wide, and lane
CD found that no resolved indexed access in the whole corpus reaches ten arms. Width is rarely the
thing that decides a mapping; provenance is.
