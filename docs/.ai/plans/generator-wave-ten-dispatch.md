---
category: Generator
audience: managing agent
title: Dispatch - generator wave ten
integration-branch: worktree-generator-wave-ten
---

# Generator wave ten — dispatch

This wave was re-scoped by the user before dispatch. `generator-wave-ten.md` inventories the
fidelity queue; the user's ruling is that the fidelity queue does not block an alpha and the
alpha's blockers do. Wave ten therefore ships **packaging, consumer documentation and the one
correctness risk in the queue**, and the `Shape/Spec.fs` fidelity work becomes wave eleven,
the first post-alpha wave, where real consumer bindings will reprice it.

`worktree-generator-wave-ten` forks `master` at `cb6256b`. Wave nine is already merged into
`master`, so no rebase is owed. Both lanes fork `cb6256b` directly; this wave declares no
finding case, so there is no prep commit.

## Baseline, re-measured on the integration branch

Measured at `cb6256b` before dispatch:

Gate: **467 generator tests, 90 wire tests** (1 skipped by design under `XANTHAM_TSGO_EXE`),
**run gate 257 checks**. `dotnet fsi build.fsx -- test` **exited 0** and left `git status`
clean.

Corpus over all 50 fixtures: `exact 495, ergonomic 1552, widened 786, escape 193`. Every
residue count in the worklist reproduces exactly: `TR032` 5298, `MB003` 3836, `TR023` 137,
`TR018` 82, `TR036` 72, `TR037` 54, `RT001` 7.

## Premises verified before pricing

Every alpha blocker was confirmed against the tree rather than taken on report:

- **The pack stage ships one project.** `build.fsx:35` and `:48` filter `Spec.srcProjects` to
  `_.Name.EndsWith("Wire")`, so `dotnet pack` reaches `Xantham.TypeScript.Wire` alone.
- **`Xantham.Fable.Core` carries no package identity.** Its `.fsproj` sets `TargetFrameworks`
  and `GenerateDocumentationFile` and nothing else — no `PackageId`, `Version`, `Authors` or
  `Description`. Its `README.md` is included as `Content`, which does not place it at the
  package root. Every generated file opens this namespace, so a consumer compiles nothing
  until it is on NuGet.
- **`Xantham.Cli` carries identity but no version.** `PackAsTool`, `ToolCommandName xantham`
  and `PackageId Xantham.Cli` are all set; `Version` is absent, and the pack filter excludes it.
- **No consumer documentation exists.** `xantham generate` and `xantham.json` appear nowhere
  under `docs/`, in `README.md`, or in any package README. `README.md:83-84` and
  `docs/index.md:107-108` still describe the generator and the support library as
  "Not yet packaged".
- **`RT001` 7 hides ~2,061 unresolved types**, 1,815 of them in `@cloudflare/workers-types`.

One premise moved in the lane's favour. Lane R3 priced the frontier sample as needing a debug
hook it had no read-only way to build, believing the frontier ids carry nothing to attribute.
They carry the wire's own answer: the cutoff branch at `Resolve.fs:746` already holds the whole
`TypeResponse` for every unfollowed type — `Flags`, `ObjectFlags`, `Target`, `TypeParameters`,
`ObjectType`, `IndexType`, `CheckType`. The sample is a dump of what is in hand, not
instrumentation of the walk, and lane R4 is briefed against the site rather than the estimate.

One premise did **not** hold as stated. The stray `src/Xantham.Generator/Measures.fs` and
`src/Xantham.Cli/xantham-out/` are untracked in the user's main checkout and exist in no commit,
so no lane worktree can see or remove them. The repository-side half is real and is lane P1's:
`xantham-out/` is not in `.gitignore`, which is what lets a generation run leave that directory
behind. Removing the two local files is the user's own `git clean`, and is reported rather than
performed.

## What decided the batch structure

**Two concurrent lanes, per the user's direction.** Lane P1 owns the release surface —
`build.fsx`, the two `.fsproj` files, and every consumer-facing document. Lane R4 owns
`Resolve.fs`. The two share no file.

**Packaging and documentation are one lane, not two.** The documentation is downstream of the
packaging decisions it has to state: the package ids, the version a consumer installs, the tool
command name, and what `pack` actually emits. A separate docs lane would either block on P1 or
document a package that did not exist yet.

**The fidelity queue is not dispatched.** Worklist items 1–4 (`TR037` 54, `TR036` 72,
`TR023` 137, `TR018` 82) are sequenced through `Shape/Spec.fs` and improve bindings that already
compile. They are alpha-plus. Lane R3's dispatch order in `docs/.ai/handovers/lane-r3.md` stands
as written and needs no re-derivation when wave eleven opens it.

**The `TR058` case is not pre-declared.** A case was declared and reverted when the wave was
re-scoped; wave eleven declares it, since the lane that raises it now sits in that wave. `TR057`
remains the maximum in the `TR` prefix.

## Decisions the worklist handed the manager

### Item 6 — the thirteen `keyof`-bound overloads: declined

Recovering them costs a measured **235 `StringEnum` cases across four bounds in `animejs` alone**
(lane AP, `docs/.ai/handovers/lane-ap.md`), against 13 overloads returned. That trade makes the
binding worse to read everywhere to recover a cluster in one place. `DO005` 14 stands as the
recorded loss it already is, with its cause recorded in three measurements. Reversible if a
consumer arrives who needs those overloads; until then the count is not a target.

### Item 7 — the two small items: one folded in, one re-priced

**Group emission ordering is deferred to wave eleven.** `Render.fs` writes `groups/*.fs` in
filesystem order, correct for `nested-dep-lab` by coincidence; a shipped group sorting before
the group it depends on breaks the gate with no rule to point at. It is a latent gate break with
no live failure, and it lands in `Render.fs`, which wave eleven's fidelity lane also opens.

**`inline` and demand-driven resolve leaves the small-items bullet.** Wave five priced it as the
prerequisite for shipping large groups: it has to resolve what a package references rather than
what its group contains, and its scoping has to cover `Unclassified` shapes — one `Date` under
`esnext` put 37 anonymous lib declarations into a consumer's module. That is wave-sized. It has
ridden five worklists inside a bullet that prices it as an offcut, which is why it keeps being
deferred as one. It stands as its own worklist entry from here.

## Standing clauses every lane carries

0. **Verify the base SHA before the first edit, and halt on mismatch.** Your worktree path is in
   your brief. Run `git -C <your worktree> rev-parse HEAD`, confirm `cb6256b`, and prefix every
   later command with that path. A lane that inherits the shell's working directory works on
   another wave's branch — wave nine lost a dispatch to exactly this.
1. Read `.claude/rules/generator-fixtures.md` first.
2. Commit as work becomes coherent, not once at the end.
3. Fast loop while iterating (`--quick --update --no-run-gate --filter`); full
   `dotnet fsi build.fsx -- test` before the final commit.
4. **Trust `build.fsx`'s exit code.** A passing pipeline that exits 0 is green.
5. **Never end a turn waiting on a background run. Commit first, then wait.** An unfinished build
   is a reason to commit, not a reason to pause. Both halves are load-bearing: wave eight lost a
   lane that stalled with nothing committed, and wave nine's lane survived the same stall only
   because it had committed first.
6. **Verify your premise before acting on it.** Disproving an item closes it — a completed lane,
   not a failed one.
7. Finding codes come from `FindingCodes.table`. A case you find you need mid-task is a request
   back to the manager, not a local edit.
8. Full report to `docs/.ai/handovers/lane-<id>.md` on your own branch; return **at most fifteen
   lines**.
9. Report measurements, not verdicts: `findings` output before and after, tier counts, finding
   codes added or moved, `git diff --numstat`.
10. Never `git push`, never open a PR, never merge into `master` or into the integration branch,
    and **never publish a package to NuGet**. Publishing is the user's step.
11. Hand back what you cannot explain. An unexplained large-fixture diff reported early is cheap.
12. Do not open a large golden or a fixture's `symbols.jsonl`. Run everything, read almost none
    of it.

## Lanes

| Lane | Items | Model | Branch | Worktree | Owns |
| --- | --- | --- | --- | --- | --- |
| P1 | packaging, consumer docs, ignore rule | Opus 5 | `worktree-gen-wave10-p1` | `.claude/worktrees/gen-wave10-p1` | `build.fsx`, `Xantham.Fable.Core.fsproj`, `Xantham.Cli.fsproj`, `README.md`, `docs/**`, `.gitignore` |
| R4 | FollowDepth frontier sample | Sonnet 5 | `worktree-gen-wave10-r4` | `.claude/worktrees/gen-wave10-r4` | `Resolve.fs`, `docs/.ai/plans/generator-followdepth-recon.md` |

Lane P1 takes Opus 5: it is the release surface, its output is what a consumer meets first, and
a wrong package identity is expensive to withdraw. Lane R4 takes Sonnet 5 — it returns
measurements against a site this document names. Neither lane exceeds Opus 5 on the standard
context window.

Lane R4 writes under `docs/.ai/`, lane P1 under `docs/` proper and `README.md`. The only file
both could reach is `docs/index.md`; it is P1's.

### Lane P1 — packaging, consumer documentation, ignore rule

Three things in order. Packaging first, because the documentation states what packaging decided.

**1. Make the pack stage ship what a consumer needs.** `build.fsx:35` and `:48` filter
`Spec.srcProjects` to names ending `Wire`. Replace that filter with an explicit publishable set:
`Xantham.TypeScript.Wire`, `Xantham.Fable.Core`, `Xantham.Cli`. `Xantham.Generator` stays
unpacked — the CLI is a `PackAsTool` package and bundles its referenced assemblies, so nothing
a consumer runs needs the generator as a separate package. Confirm that claim by inspecting the
built `.nupkg`, not by assuming it.

Give `Xantham.Fable.Core` a package identity: `PackageId`, `Version`, `Authors` and
`Description`, matching the shape `Xantham.TypeScript.Wire.fsproj` already uses. Its `README.md`
is currently `Content Include`, which does not reach the package root — pack it the way the Wire
project packs its own. Give `Xantham.Cli` a `Version`; its identity is otherwise complete.

Use **`0.1.0-alpha.1`** for both new packages and leave `Xantham.TypeScript.Wire` at `0.2.0`.
That version is the manager's assumption, flagged to the user; if they name another, it is a
one-line change in each `.fsproj`.

Then check the `bump` command (`build.fsx:446`), which takes every `srcProjects` relative path.
Confirm it reaches the two newly versioned projects rather than silently skipping a project whose
version it did not previously find.

Gate this half on `dotnet fsi build.fsx -- pack` emitting three `.nupkg` files under `bin/`, and
on the full `dotnet fsi build.fsx -- test` staying green — you are editing the pipeline that runs
it. **Do not publish anything to NuGet.**

**2. Write the consumer documentation.** There is none: `xantham generate` and `xantham.json`
appear in no document in this repository. A consumer needs one page that takes them from nothing
to a compiling binding:

- install the tool (`dotnet tool install`), with the package id and version this lane just set;
- install `typescript@7` **in their own project** — `Tsc.locate` walks parent directories for
  `node_modules`, so the pin has to be theirs, and this is the step that silently fails without
  it;
- write `xantham.json`, against the schema the CLI emits from its own config record
  (`src/Xantham.Cli/Schema.fs`) — describe how to get the schema rather than transcribing it, so
  the document cannot drift from the record;
- run `xantham generate`, and what the exit codes mean;
- reference `Xantham.Fable.Core` and `Fable.Core` 5.2.0 from the consuming project, and compile.

Read `docs/wire-usage.md` first and follow its shape; this page is the generator's equivalent and
should not invent a second house style. Say plainly what the alpha does not do — the refused
rungs (`@types/three`, `typescript`) and the recorded losses — so a consumer meets the boundary
in the documentation rather than in their own build.

Then correct the status tables that call this work unpackaged: `README.md:82-84` and
`docs/index.md:106-108`.

**3. Close the ignore gap.** `src/Xantham.Cli/xantham-out/` is not in `.gitignore`, which is why
a generation run leaves a `Three.fs` and a `manifest.json` behind in a working checkout. Add the
rule. Note that `.gitignore` un-ignores the `*lab*` fixture family — read the file before adding
to it, and do not disturb that pattern. Two such files exist untracked in the user's main
checkout; they are not in any commit, you cannot see them from your worktree, and removing them
is not your lane.

### Lane R4 — the FollowDepth frontier sample

A sampling lane. Return numbers. A repair is not in scope, and neither is a verdict on whether
`FollowDepth` should change — the user's framing is that this is the queue's one correctness
risk rather than a recorded loss, and the answer decides whether a cheap constant raise settles
it.

`RT001` reads 7 corpus-wide because the finding fires once per fixture, batched, on the synthetic
`<type-table>` symbol. Behind those 7 sit roughly **2,061 unresolved types, 1,815 of them in
`@cloudflare/workers-types`** — up from 1,772 four waves ago. Each is a member that widened to
`obj` without anything naming which member. Nothing watches this number and nothing has ever
attributed it.

The cutoff branch is `Resolve.fs:746`, inside `resolveTypeTable`'s `walk`. Every type it declines
to follow is already a materialised `TypeResponse`: it carries `Id`, `Flags`, `ObjectFlags`,
`Target`, `TypeParameters`, `ObjectType`, `IndexType` and `CheckType`. Your dump is of what that
branch already holds — lane R3's read that these ids carry nothing to attribute is wrong, and it
is why this item was priced as unaffordable for four waves.

Gate the dump behind an environment variable (`XANTHAM_FRONTIER_DUMP=<path>`, or a name that
matches the conventions already in the tree), so the default path is byte-identical. Prove that:
the corpus tier counts and finding counts must be unchanged at the end of your lane, `495 / 1552
/ 786 / 193` and `RT001` 7.

Then answer, for the 1,815:

- the distribution over `Flags` and `ObjectFlags` — what kind of type is stuck;
- whether one utility type is ballooning, which shows as `Target` concentrated on a few ids, or
  whether these are many independent types;
- what `FollowDepth` at 16 and at 20 does to the frontier count and to wall-clock time. Does the
  count converge, or does the wall simply move?

The third question is the one the user is buying: a constant raise is cheap if the count
converges and worthless if it does not.

Write the analysis to `docs/.ai/plans/generator-followdepth-recon.md`, including a named sample
of stuck types a later lane can chase, and the ≤15-line summary to
`docs/.ai/handovers/lane-r4.md`. Keep the hook if it is clean and gated; the point of the item
is that this number grows with nothing watching it. You own `Resolve.fs`. Do not open `Shape/`,
`build.fsx`, or anything under `docs/` outside the two files named here.

## Outcomes

Measured on the composed integration branch after both merges.

| Item | Lane | Outcome |
| --- | --- | --- |
| packaging | P1 | **Shipped.** `Spec.publishable` replaces the `EndsWith("Wire")` filter at both sites. `pack` emits three `.nupkg`: Wire `0.2.0`, `Xantham.Fable.Core` and `Xantham.Cli` at `0.1.0-alpha.1`. |
| consumer docs | P1 | **Shipped.** `docs/generator-usage.md`, 237 lines, install through compile, with the alpha boundary stated. |
| ignore rule | P1 | **Shipped.** `xantham-out/` ignored unanchored; the `*lab` fixture re-includes verified intact. |
| frontier sample | R4 | **Answered, against the hypothesis.** Raising `FollowDepth` recovers nothing. |

### Gate and corpus: unchanged, as designed

| | base `cb6256b` | composed |
| --- | ---: | ---: |
| generator tests | 467 | 467 |
| wire tests | 90 | 90 |
| run gate checks | 257 | 257 |
| exact / ergonomic / widened / escape | 495 / 1552 / 786 / 193 | 495 / 1552 / 786 / 193 |
| exit code | 0 | 0 |

Neither lane was meant to move a count. Lane P1 touched no generator source; lane R4's dump is
a no-op with `XANTHAM_FRONTIER_DUMP` unset. A moved count here would have been the defect.

### The frontier answer

The item was bought to decide whether a cheap constant raise settles the queue's one correctness
risk. **It does not.** Measured on `@cloudflare/workers-types`:

| `FollowDepth` | unique frontier type ids |
| ---: | ---: |
| 12 | 2,371 |
| 16 | 2,392 |
| 20 | 2,397 |

The stuck set does not shrink as the cutoff rises — it grows slightly and flattens. Following
deeper exposes about as many unresolved types as it resolves, so the wall moves and the count
stays. `FollowDepth` is not the lever.

What the frontier is made of points at the lever instead. 74.5% of entries carry `Object`, and
77.8% of those are `Anonymous | Instantiated | CouldContainTypeVariables` — the signature of a
generic utility type applied to arguments, not a hand-declared interface. Of the 783
`Reference`-flagged entries carrying a `Target`, 44% point at a single target id, and all of
them at just 22 ids. A few utility types instantiated across many argument combinations produce
this frontier; many independent types do not.

So the repair to price is **normalising or memoising utility-type instantiation on its target
and arguments**, not a constant. That is a wave-sized item and it belongs on the next worklist
in those terms.

Two things are open, both recorded by the lane rather than hidden. Target id 65 — the single
largest concentration point — has no declaration name, because checker ids are assigned per
session and a name needs a further wire lookup that a sampling pass had no scope for. And lane
R3's carried estimate of ~1,815 does not reconcile against the 2,371 measured here; the earlier
number was never produced by this method, so the two are not comparable rather than one being
wrong.

### The wave's cost in process

**No lane needed manager intervention, and no lane stalled.** Both halt gates fired clean at
`cb6256b`, both lanes committed as they went, and neither ended a turn waiting on a background
run. The clauses wave nine wrote against — the inherited working directory and the background
stall — cost nothing this wave. Both were gated rather than merely stated, which is what wave
nine's own retrospective asked for.

**Lane P1 found a defect the brief did not name.** `pack-Xantham.Fable.Core` failed
`NETSDK1005`: the run gate's `dotnet fable` restores the support package for `netstandard2.1`
alone, and `--no-build` implies `--no-restore`. Each pack stage now restores first. A lane that
had reported "packaging done" without packing would have shipped this to the user.

**One premise correction paid for an item four waves deferred.** Lane R3 priced the frontier
sample as unaffordable because it read the stuck ids as carrying nothing to attribute. Checking
that read against `Resolve.fs` before dispatch, rather than carrying it forward, turned a
refused item into a one-lane answer. A carried estimate is worth re-reading at the site when the
item it blocks keeps deferring.

**Left as found, outside every brief.** `README.md` and `docs/index.md` carry eight links to
`docs/plans/…`; that directory does not exist and the files live at `docs/.ai/plans/`. Lane P1
reported them rather than widening its lane.
