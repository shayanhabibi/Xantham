---
category: Generator
audience: managing agent
title: Dispatch - generator wave fourteen
integration-branch: worktree-generator-wave-fourteen
---

# Generator wave fourteen — the escape column, and a user-facing defect

`worktree-generator-wave-fourteen` forks `master` at `35c42f9`, which is wave thirteen composed
through batch three. The worklist is `docs/.ai/plans/generator-wave-fourteen.md`; this document is
the triage verdict over it and the briefs that carry it.

## Baseline, re-measured on this branch at the fork

`dotnet fsi build.fsx -- findings`, summed across all 55 fixtures, reproduces the worklist's
recorded numbers to the unit:

| | at the fork |
| --- | ---: |
| exact | 535 |
| ergonomic | 1603 |
| widened | 797 |
| escape | 200 |
| generator tests | 490 |
| wire tests | 90 |
| run gate checks | 309 |

Targeted keys: `TR008` 574, `TR006` 302, `TR031` 61, `TR020` 53, `TR059` 221.

Per-fixture distribution of the three keys this wave touches, measured rather than assumed:

| key | distribution |
| --- | --- |
| `TR008` 574 | `animejs` 350, `@cloudflare/workers-types` 166, `solid-js` 42, labs 16 |
| `TR006` 302 | `@cloudflare/workers-types` 227, `shared-tag-lab` 27, `animejs` 20, `literal-overload-lab` 8, `solid-js` 6, rest single digits |
| `TR031` 61 | `animejs` 34, `solid-js` 19, `setter-lab` 7, `intersection-callable-lab` 1 |

**`TR008` is `animejs` before it is `@cloudflare`.** The worklist's escape-*symbol* counts put
`@cloudflare` first at 110 symbols against `animejs`'s 54; the *finding* counts invert that, 350 to
166. Lane CJ is briefed on the finding distribution, because that is where the sites are.

## Triage

The worklist offers seven items. This wave takes six of them and defers one.

| Item | Verdict | Lane | Batch |
| --- | --- | --- | --- |
| 3 — CLI emits less than the harness | Taken. A user-facing entry point that under-emits is a defect. | CI | one |
| 5 — recon: `TR008` reachability | Taken. Read-only; nothing gates on it. | CJ | one |
| 6 — recon: `TR006` split | Taken. Read-only; nothing gates on it. | CK | one |
| 7 — `TR031` callback overloads | Taken. | CL | one |
| 1 — `isObjectMember` rejects intersection arms | Taken **as recon only**. The worklist says price the radius before taking it, so this wave prices it and does not spend it. | CM | one |
| 4 — exclusive-arm object union fold | Taken, in two parts. Its stated prerequisite is proven first. | CP, then CN | one, then two |
| 2 — resolve tier does not follow lib interface members | Deferred to batch two. | CO | two |

The worklist's own sequencing note is honoured: items 1 and 4 both alter how union arms are read
and must not share a batch undeliberately. In batch one item 1 is **read-only**, so it writes
nothing that item 4's probe could collide with. The implementation halves are separated by a batch
boundary.

### Why item 4 splits into two lanes

Wave thirteen's carried-forward item 5 lists four things to settle before dispatch, and one of them
is not a design question but an empirical one: *Fable must accept overloaded
`[<ParamObject; Emit("$0")>]` statics*, recorded as "Run-gate check required, not assumed." The
whole fold rests on it. Discovering it fails from inside a large implementation lane wastes the
lane; proving it in isolation costs one small lab.

**Lane CP proves it in batch one. Lane CN implements the fold in batch two, and is dispatched only
if CP comes back green.** If CP comes back red the fold is not available in the form the user asked
for, and that is a finding to report rather than a lane to run.

### What is declined, and stays declined

`TR009` 219, `SA002` 197, `TR045` 185, `TR023` 136, `TR032` 5131 and `MB003` 3839, and uncapping
erased-union arity. The worklist's reasoning for each is unchanged and no lane opens against any of
them. `TR032` and `MB003` are the two largest residues in the corpus and are correct mappings; the
headline number does not attract a lane.

## Pre-declared finding cases

Appended on this integration branch in one commit before any lane forks, gated green. Three cases;
a case that turns out unused costs one dead row.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `TR.ExclusiveArmsFolded of arms: int` | `TR060` | ergonomic | Lane CN (batch two) |
| `TR.ExclusiveArmsNotFoldable of arms: int` | `TR061` | ergonomic | Lane CN (batch two) |
| `TR.CallbackOverloadsNotSeparable of overloads: int` | `TR062` | widened | Lane CL |

`TR060` is the code wave thirteen reserved for the fold, and the reservation is kept so its
carried-forward note stays true. `TR061` is the fold's decline: arms that separate on no required
parameter leave the union erased, which is today's behaviour and is graded ergonomic to match
`DT001`, where the union also survives in erased form. `TR062` separates *cannot* from *did not* at
`TR031`'s sites; it is graded widened because `TR031` is.

**No lane edits `Findings.fs` further.** A case discovered mid-task is a request back to the
managing agent.

## Batch one

Six lanes, dispatched in parallel. Three write no code at all.

### Lane CI — the CLI under-emits, or no longer does

Branch `worktree-gen-wave14-ci`. **Re-verify before diagnosing.** Wave thirteen measured
`dotnet run --project src/Xantham.Cli -- generate` over `@cloudflare/workers-types` at 26k lines
against the harness's 30k, and has not measured it since; three merges have landed on top. If the
gap has closed, say so with the two line counts and stop — that is a complete and valuable result.

If it reproduces, the deliverable is the **mechanism**: which stage of `Pipeline.fs` the CLI enters
differently from the harness, expressed as the specific option, path or default that diverges. Fix
it where the fix is contained. Where it is not, write the mechanism up and stop rather than
reshaping the pipeline.

Owns `src/Xantham.Cli/`, and `Pipeline.fs` only if the divergence is there. Adds no finding case.
No lab fixture: the reproducer is a CLI invocation, not a `.d.ts`.

### Lane CJ — how much of `TR008` is reachable

Branch `worktree-gen-wave14-cj`. **Read-only.** Owns nothing under `src/` or `tests/`.

574 sites. `any` is unconstrained in the general case, so the question is not whether `any` can be
typed — it is **at how many sites something else in the declaration already constrains it**: a
sibling overload, a `keyof` over the same operand, a call signature that consumes the position, a
neighbouring member that fixes the shape.

Start at `animejs`, which holds 350 of the 574. Report per-cluster attribution with a reproducer
under ten lines for anything called recoverable, and an **explicit decline for the remainder** with
the reason. Wave nine's lane R3 is the model. This lane opens no implementation lane itself; it
prices one.

Writes only `docs/.ai/handovers/lane-cj.md`.

### Lane CK — split `TR006`'s 302 before either half is priced

Branch `worktree-gen-wave14-ck`. **Read-only.** Owns nothing under `src/` or `tests/`.

`TR056` already keeps a literal where one separates an overload, 34 times, so the retention
mechanism exists and the question is where else it pays. **Split the 302 on one line: a
single-literal member is a different case from a literal-union member.** The latter is a
discriminated-union candidate and may belong with the tagged-union passes rather than with literal
retention.

Report the two counts, the reason each site falls on its side of the line, and a separate price for
each half. 227 of the 302 are in `@cloudflare/workers-types`; `shared-tag-lab`'s 27 are a lab's own
and should be reported apart from the corpus figure.

Writes only `docs/.ai/handovers/lane-ck.md`.

### Lane CL — a callback's later overloads

Branch `worktree-gen-wave14-cl`. Lab fixture: **`callback-overload-lab`**.

`TR031` fires from exactly two sites and they are not the same problem:

- `Shape/Spec.fs:1850`, in `delegateRef` — a callback at **member or parameter position**. This is
  the recoverable half: F# carries overloads on a delegate-typed member.
- `Shape/Callbacks.fs:25`, in the named-declaration pass — a callback that is its own declaration,
  emitted `FsDelegateType`. A `type X = delegate of ...` holds one signature.

**Establish the split of the 61 across those two sites first and report it**, before implementing.
The counts by fixture are `animejs` 34, `solid-js` 19, `setter-lab` 7,
`intersection-callable-lab` 1.

Recover the member-position half. Where overloads separate on no F# form — same arity and parameter
types differing only in return type is the obvious case — raise `TR062` rather than leaving `TR031`
to describe both outcomes. Where the named-declaration half stays as it is, say so and leave
`TR031` on it; that is a floor, not a backlog, and reporting it as one is the deliverable.

**Do not mint `X2`, `X3` names for a declaration's later overloads.** A shape another declaration
answers for should not be given a second name; three lanes across two waves have met that rule and
it is a property of the generator.

Owns `Shape/Callbacks.fs` and `delegateRef` in `Shape/Spec.fs`. May add run-gate checks.

### Lane CM — price admitting intersection arms, do not admit them

Branch `worktree-gen-wave14-cm`. **Read-only.** Owns nothing under `src/` or `tests/`.

`isObjectMember` tests `TypeFlags.Object`. The checker distributes an intersection into arms flagged
`Intersection` whose members are populated, so only the flag rejects them.
`TailStream.EventType`'s three sites are all that stand between lane CB's shared-tag fold and the
site that motivated it, and `shared-tag-lab` pins today's behaviour as the standing reproducer.

**The lane's whole deliverable is the blast radius, as a number.** Admitting intersections moves
discriminated-union detection corpus-wide. Establish, without committing a behaviour change to any
branch that merges:

1. How many sites corpus-wide reach `isObjectMember` with an `Intersection`-flagged type today.
2. Which of them would become discriminated-union candidates, and which would then be *refused* by
   `taggedUnionShape` — a refusal now raises `DT003`/`DT004` rather than falling silent.
3. Whether the three `TailStream.EventType` sites actually fold once admitted, or merely reach the
   pass and get refused for a second reason.

A throwaway local experiment is the right way to answer 1 and 2; **it is not committed and the
branch carries only the write-up.** Report a recommended verdict — take it, or decline it with the
reason — and the evidence for it.

Writes only `docs/.ai/handovers/lane-cm.md`.

### Lane CP — prove overloaded `ParamObject` statics, or sink the fold

Branch `worktree-gen-wave14-cp`. Lab fixture: **`paramobject-overload-lab`**.

Batch two's lane CN rests on a single unproven assumption. Prove or disprove it, and nothing else.

**The question.** Does Fable accept a type carrying two or more `[<ParamObject; Emit("$0")>]`
`Create` statics separated only by which parameters are required, and does each emit a JavaScript
object literal holding exactly the arguments passed at that call site?

Hand-write the F# — this lane proves a Fable capability, not a generator pass. The shape to prove
is the one lane CN will generate:

```fsharp
type Options =
    abstract query: string option with get, set
    abstract messages: string array option with get, set
    abstract shared: int with get, set

type Options with
    [<ParamObject; Emit("$0")>]
    static member Create(query: string, shared: int) : Options = jsNative
    [<ParamObject; Emit("$0")>]
    static member Create(messages: string array, shared: int) : Options = jsNative
```

`AiSearchSearchRequest`'s `query`/`messages` pair is the real motivating shape.

**A compile is not the proof.** Add run-gate checks asserting each overload's emitted object has
the keys that overload was given and does not have the other arm's key. An erased or attributed
construct that compiles and misbehaves at runtime is the failure mode wave twelve's lane BA exists
because of.

Also settle, and report as a plain yes or no with the compiler's own words on a no:

- Whether F# separates the overloads when one arm's distinguishing member is **optional in source**.
  `Container.Start.Options2`'s `containerSnapshot` is that case, and wave thirteen flagged it as
  possibly unfoldable. If F# cannot separate them, `TR061` is the answer for that pair and lane CN
  needs to know before it starts.

Report **feasible** or **not feasible** in the first line of the handover. Owns
`tests/fixtures/paramobject-overload-lab/`, its `Pipeline.test.fs` registration, and the run-gate
two-place addition.

## Ownership map

| File | Lane |
| --- | --- |
| `src/Xantham.Cli/` | CI |
| `Shape/Callbacks.fs` | CL |
| `Shape/Spec.fs` — `delegateRef` (~1850) | CL |
| `tests/fixtures/callback-overload-lab/` | CL |
| `tests/fixtures/paramobject-overload-lab/` | CP |
| `Findings.fs` | nobody — pre-declared above |
| nothing under `src/` or `tests/` | CJ, CK, CM |

Only CI, CL and CP write code, and they touch disjoint files. **Two collisions are expected rather
than discovered**: `Pipeline.test.fs`'s `fixtureTests` block and the run gate's two-place addition
are append points CL and CP may both reach. The resolution for both is to keep both sides.

## What every lane owes back

**Fifteen lines to the managing agent.** The full report goes to
`docs/.ai/handovers/lane-<id>.md` on the lane's own branch, where it survives the session and costs
nothing to leave unread. Counts, not verdicts:

- `dotnet fsi build.fsx -- findings` before and after, and the per-key delta for **every** code that
  moved, not only the targeted one.
- Tier counts, run-gate check count, generator and wire test counts.
- `git diff --stat` over the regenerated goldens.
- Branch name and commit SHA.
- Anything a large fixture did that the lab fixture does not account for. **Report it and stop.**

Read-only lanes owe the same discipline against a smaller surface: their handover file, their
branch, their SHA, and no `src/` diff at all.

### Four clauses this wave enforces, because wave thirteen paid for each

- **Report the gross, not the net.** A lane owes per-symbol attribution of every tier movement it
  causes. Lane CH gave `+5` where the gross was twelve to widened and three to escape, and the
  escape movement went unreported. Netting is not reporting.
- **A tier column can rise because the mapping improved.** A lane that removes declarations says so
  explicitly, because one declaration's mark becomes many sites' marks and the column is then not
  comparable across the merge.
- **A completion notification is not evidence of completion.** Every lane commits as soon as its
  work is coherent. This manager checks that a branch carries commits before believing it landed.
- **A shape another declaration answers for gets no second name.** Met by lane BB, lane CF and lane
  CH in three different passes. Lane CL is briefed on it directly.

Unchanged and non-negotiable: goldens are regenerated, never hand-merged. Batches gate as batches.
Merge messages go to a file and are applied with `git commit -F`, because a message containing
backticks passed with `-m` is mangled by the shell. **Never merge into `master`** — the integration
branch is handed to the user, who lands it.

### Model constraint

`opus` on the Agent tool's `model` enum resolves to the parent session's context variant, and this
parent runs a 1M-context model, so dispatching `opus` breaches the standing constraint that no
teammate exceeds Opus 5 on a standard context window. **Every lane in this wave is dispatched on
`sonnet`.** Wave thirteen breached this on four lanes before it was caught. Consulting Fable 5.1
requires the user's permission and a stated reason; none has been requested.

## Batch two, provisional

| Lane | Work | Owns | Gated on |
| --- | --- | --- | --- |
| CN | Item 4, the exclusive-arm fold | `Shape/Interfaces.fs`, `Shape/ParamObjects.fs`, `erasedUnionRef` | Lane CP returning feasible |
| CO | Item 2, resolve tier follows lib interface members | `Resolve.fs` | nothing |

Plus whatever lanes CJ, CK and CM price into existence. Batch two is written after batch one
composes, against the numbers batch one leaves behind rather than against these.

---

## Batch one, composed and gated

Composed at `5dc7127`: **517 generator tests, 90 wire tests (1 skipped by design), run gate 323
checks, every stage ok, exit 0.** Eight lanes, all dispatched on `sonnet`.

| | fork `8d3a4fa` | composed |
| --- | ---: | ---: |
| exact | 535 | 543 |
| ergonomic | 1603 | 1620 |
| widened | 797 | **794** |
| escape | 200 | 202 |
| total findings | 17,040 | 17,122 |
| generator tests | 490 | 517 |
| run gate checks | 309 | 323 |
| `TR031` | 61 | **35** |
| `TR062` | 0 | 1 |
| `SI001` | 11 | **1** |
| `SI008` | 0 | 17 |
| `TR050` | 44 | **30** |
| `DT002` / `DT003` / `DT004` | 24 / 1 / 1 | 26 / 2 / 3 |
| `DO001` | 5 | 10 |
| `HG003` | 0 | 1 |

### The honest headline is widened -8, and the rise is four new labs

Per fixture, the corpus totals decompose without remainder:

| fixture | exact | ergonomic | widened | escape |
| --- | ---: | ---: | ---: | ---: |
| `@cloudflare/workers-types` | 0 | +2 | **-2** | **+1** |
| `animejs` | 0 | +1 | **-1** | 0 |
| `solid-js` | 0 | +1 | **-1** | 0 |
| `shared-tag-lab` | 0 | +2 | **-1** | 0 |
| `intersection-callable-lab` | 0 | +2 | **-2** | 0 |
| `intersection-lab` | 0 | +1 | **-1** | 0 |
| `callable-hybrid-lab` *(new)* | +6 | +6 | +3 | 0 |
| `paramobject-overload-lab` *(new)* | +2 | +2 | 0 | 0 |
| `callback-overload-lab` *(new)* | 0 | 0 | +2 | 0 |
| `dom-shadow-lab` *(new)* | 0 | 0 | 0 | +1 |

**Every existing fixture's widened count fell, eight between them, and no existing fixture gained
an exact or lost one.** The +5 widened and +1 escape in the corpus totals belong entirely to four
labs that did not exist at the start of the wave, and a lab exists to carry hard cases.

**The one escape symbol on an existing fixture is a mapping improvement.** Lane CS's newly named
ten-case union exposes a `TR.AnyToObj` that the unnamed arm carried invisibly; the loss is not new,
its owner is. `TR008` reads 574 to 575 for that reason alone.

### Composition found two defects that no lane could

Both were invisible on every branch taken alone, and both were found by regenerating and gating the
merged tree rather than by re-reading a lane's report.

1. **`HG.NothingHarvested` embedded an absolute path in a golden.** The case carried `ctx.EntryFile`
   whole, so `dom-shadow-lab/symbols.jsonl` recorded the worktree that generated it and differed on
   every machine. Latent since the case was written: no committed fixture raised `HG003` until lane
   CQ's lab, whose whole point is total shadowing. One golden of 58 was affected; the other 57
   already spell files relative. Fixed by `underPackage` at `abecfbd`, and the golden reads
   `index.d.ts`.
2. **`signatureKey`'s arity.** Lane CL gave it a type-parameter argument to stop two overloads
   differing only in a type parameter's source name erasing to one CLR signature. Lane CR added the
   `FsInvoke` case against the one-argument shape it forked from. Both branches gated green alone;
   the textual merge does not type-check, and `FS0001` at `Overloads.fs:167` is the whole symptom.
   Passing `c.TypeParameters c.Parameters` rather than `[]` is what extends CL's fix to CR's
   construct - a generic `Invoke` overload set would otherwise reach the `FS0438` CL closed, and an
   empty list would have compiled and been silently wrong.

Regeneration over the composed tree then moved three manifests that neither lane produced alone:
`animejs` 1,968 findings to 1,935 with widened 169 to 155, `solid-js` 463 to 511, and
`intersection-callable-lab`'s last widened symbol to ergonomic. **`TR050` fell 44 to 30 in the
composition**, not on either branch.

### What each lane settled

| Lane | Outcome |
| --- | --- |
| CI | **Item 3 is not a defect.** The gap reproduces at 25,933 lines against 30,235 and is the documented contract: `Lib = None` loads the DOM, and a global type library's DOM-redeclaring names then belong to the compiler lib. The CLI with an equivalent `--config` is byte-identical to the golden. |
| CJ | **`TR008` 574 is a floor.** All 574 attributed across eight clusters; 295 are one `animejs` alias, `Callback<T> = { method(self: T): any }["method"]`, whose return the library discards. |
| CK | **`TR006`'s literal-union half does not exist.** `namedUnionByMembers` names a pure literal union before the raise site can see it. Split A 271 / B 0 / C 31. |
| CL | `TR031` 61 to 35. Residual reconciled 17 genuine floor / 16 recoverable and scoped out. |
| CM | Priced item 1 at 75 sites and recommended taking it; lane CS then took it. |
| CP | Overloaded `ParamObject` statics are **feasible**, proved at the run gate. |
| CQ | The CLI names a package whose declarations the default lib absorbs. |
| CR | All eleven callable hybrids reach their call signatures through `Invoke`. |
| CS | Intersection arms admitted; `TailStream.EventType` folds into a ten-case union. |

### Carried to wave fifteen

1. **`Invoke` lifts lane CL's named-declaration floor, and neither lane could see it.** CR was
   scoped to call signatures *beside* properties, CL to call signatures *alone*. Between them sits
   a named alias with several call signatures - `AnimatableProperty = Setter & Getter` keeps its
   setter and drops its getter - which an interface carrying overloaded `Invoke` members would
   hold. The trade is real and needs pricing: `type X = Func<...>` accepts a lambda literal and an
   `Invoke` interface does not.
2. **Lane CL's 16 recoverable sites.** `animejs`'s `LayoutAnimationParams` and `AutoLayoutParams`
   reach `shapeMembers` as an `Intersection` reporting zero call signatures, so `isPureCallback`
   declines and only `intersectionRef` finds the two signatures, by which point `typeRef` must
   return one type. Recoverable, and it restructures the shape-decision entry point.
3. **A named alias whose whole definition is one string literal.** `type DurableObjectRoutingMode =
   "primary-only"` widens to `string` and the single legal value is lost. Distinct from a
   single-literal member, and hiding inside lane CK's bucket A of 271.
4. **A discarded callback return reads `obj`, not `unit`.** 295 `animejs` sites make an F# caller
   produce a value the library throws away. An ergonomics question rather than a fidelity one, and
   it wants stating as its own item before anyone prices it.
5. **`claim`'s naming walk renamed two `@cloudflare` symbols.** Deterministic rather than unstable,
   and `Shape/Ordering.fs` runs after naming so it is not the fix point. Pinning it is a
   collision-policy redesign.
6. **`unionRef`'s self-name lookup misses `ConcreteBranch`** (`Spec.fs:1946`), found by lane CK.
7. **Item 4, the exclusive-arm fold, is unspent and now unblocked.** Lane CP proved the capability;
   `TR060` and `TR061` are declared and unused. **Probe 18 carries a caveat the lane understated:**
   the container pair separates on `unit`-typed placeholders rather than on required parameters, so
   an arm whose distinguisher is optional in source folds only by keeping the `never` members on
   the surface - which is the cleanup item 4 exists to get. `AiSearchSearchRequest`'s pair has no
   such problem.

### A process note

Lane CL gated with `--quick` and its `Overloads.fs` edit reached the integration branch unformatted;
the composed gate's `format` stage caught it. `--quick` skips `format`, and a lane that never runs
the unflagged gate has not been gated. The brief already says so and the lane did it anyway, so the
check belongs at the merge rather than in the instruction.
