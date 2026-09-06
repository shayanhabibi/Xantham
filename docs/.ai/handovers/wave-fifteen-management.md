---
category: Generator
audience: incoming managing agent
title: Wave fifteen — management handover
integration-branch: worktree-generator-wave-fifteen
---

# Wave fifteen — management handover

You are taking over managing this wave. Read this before the worklist, then read
`docs/.ai/plans/generator-wave-fifteen.md` and `generator-wave-fifteen-dispatch.md`.

**The wave has not delivered what it was opened to deliver.** It was opened to publish Fable
bindings for the compiler's own lib — the DOM half as `Xantham.Fable.Browser`, the ECMAScript half
into `Xantham.Fable.Core`. Neither exists. What the wave found instead is a generator bug that
blocks both, located precisely and not fixed.

## State

Integration branch `worktree-generator-wave-fifteen`. Every merge below was gated with
`XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test` in the foreground, exit 0, tree clean.

| | at item zero | now |
| --- | ---: | ---: |
| exact | 543 | 545 |
| ergonomic | 1623 | 1630 |
| widened | 793 | **789** |
| escape | 206 | 206 |
| generator tests | 522 | 532 |
| run gate checks | 323 | 323 |

Seven symbols moved, all of them better, none worse, verified per symbol against the baseline at
each merge. **Widened fell 0.5%.**

## What shipped

1. **Item zero — the support package shadows into `Fable.Core.JS`.** `master` `fd2a10b` was red:
   it consolidated `Brand.fs` and `Record.fs` into one `[<AutoOpen>]` module and dropped the
   `Xantham.Fable.Core` namespace, while the generator still emitted `open Xantham.Fable.Core`.
   Generated output now carries no Xantham-named token. `Naming.SupportBindings` handles the
   collision case: a package declaring `Record` keeps the unqualified name and the support
   package's is reached as `JS.Record`, repaired in `repair-arity` rather than widened to `obj`.
2. **`TR036` closed, 6 of 6.** A file needing erased-union arity above nine declares that `U<n>`
   at its own footer, one per distinct arity, in `Fable.Core`'s shape.
3. **`NoInfer` reaches a type.** `TR020` 53 to 36. Default emits `JS.NoInfer<'T>`;
   `resolveNoInfer` in `xantham.json` resolves it to the bare operand.
4. **`HG003` names what it discarded.** An entry whose declarations are classified away now
   reports the in-scope count and where they went, instead of reporting only that it found nothing.
5. **Item 1f — the compiler lib ships when configured.** `Harvest.harvestGlobals` admits the
   `CompilerLib` group when its disposition is `Ship`. Gated end to end by `lib-ship-lab` on
   `lib.scripthost.d.ts`.
6. **Frontier counters.** `XANTHAM_RESOLVE_COUNTERS` reports expansions and table size.

## What did not ship

- No `src/Xantham.Fable.Browser`, no DOM binding, no ECMAScript surface.
- `tools/browser-gen` untouched; it still intersects the upstream `Fable.Browser.*` packages.
- `TR023` 136 and `TR024` 147 unmoved. `fable-binding-gaps.md` still unanswered.

## The blocker, stated precisely

**A generic method whose return type applies its enclosing interface to that method's own fresh
type parameter costs the resolve tier disproportionately.** Not self-reference — three probe
packages of the same length, measured idle:

| probe | methods | frontier expansions | table size |
| --- | ---: | ---: | ---: |
| returns `Foo<T>` and `this` | 5 | 24 | 24 |
| returns `Foo<U>`, `Foo<S>` | 5 | **39,908** | 28,644 |
| both | 10 | **62,932** | 44,500 |

`Array<T>` in `lib.es5.d.ts` has this shape — `map<U>`, `filter<S>`, `reduce<U>` — so the
ECMAScript libs never complete, and `lib.dom` through the shipped path was killed at 171s and
6,025MB. The probe sources are in `docs/.ai/handovers/lane-de.md`.

The walk bounds depth (`FollowDepth = 20`, `Resolve.fs:73`) and does not bound width.

### Five attempts, and what each broke

Do not repeat these. Four of the five changed *identity* — memoizing so two references to `Foo<U>`
collapse to one table entry — and every one moved output.

| Attempt | Key | Broke |
| --- | --- | --- |
| 1 | checker's raw `Target` | merged unrelated declarations: `hoist-conditional-lab`'s `CondNode`/`DirectNode`, `solid-js`'s `Computation`, `@cloudflare`'s `File`, a `chain-lab` case |
| 2 | `getTargetOfType` dedup | doubled arity on multi-heritage interfaces, same fixtures |
| 3 | declaration name + argument symbol ids | merged members across unrelated declarations |
| 4 | as 3, plus a second round-trip confirming argument openness | resolved a `this` type through a hoisted intersection to a concrete substitution where the golden deliberately renders `obj` |
| 5 | **frontier width bound** — the depth mechanism copied | in flight as lane DI when this was written |

**The identity the frontier needs is not one the checker's responses carry.** Three of the four
memoization attempts died on `hoist-conditional-lab`. A fifth key is not the next thing to try.

Attempt 5 is different in kind: a width bound changes no identity and fails safe, because the
mechanism it copies puts excess frontier types in `notFollowed`, records one finding for the
frontier, and lets references widen to `obj` under their own owner. Its honest cost is that it
**caps** the blowup rather than removing it — `lib.dom` may complete carrying more `obj` than it
should, and how much is the number that decides whether it is acceptable.

## Mistakes I made, and what they cost

Written plainly so you do not repeat them.

1. **I gave a performance lane a wall-clock target on a shared machine.** Lane DE measured the
   reproducer at 45.8s under three concurrent lanes; it is 12.9s idle. Lane DG had no stable
   signal, drifted twice into an output-correctness metric it *could* measure, and produced
   nothing across two dispatches. **~605k tokens.** The fix was a deterministic count, which
   should have been in the brief from the start. Any performance work on this box needs a counter,
   never a duration.
2. **I reported the DOM half as ready to ship when it was not.** Lane DA measured `lib.dom.d.ts`
   completing in under a minute, and I relayed that as a result. DA had copied the DOM text into a
   synthetic package with `"lib": []`, so no ECMAScript lib loaded and `T[]` never resolved to
   `Array<T>` — the run never reached the costly shape. Lane DF found the real path does not
   complete. **I should have read DA's configuration before believing its number.** A measurement
   taken through a workaround measures the workaround.
3. **I priced an item from a document instead of a measurement.** The worklist said `NoInfer`
   would reach about 20 of `solid-js`'s `TR045` sites. It reaches none. Harmless here because the
   lane measured it and said so, but the estimate came from reading `fable-utility-types.md`
   rather than the corpus.
4. **I let three lanes park on background monitors before making it a standing check.** DB, DD and
   DG each reported completion while stopped mid-flight with uncommitted work — one had two
   untracked directories that would have been silently dropped. A brief saying "never `--quick`,
   never a background monitor" does not prevent it. **Check `git log` and `git status` on the lane
   branch before believing any completion notification.** That check is now the only thing that
   has reliably caught it.
5. **I narrated a dispatch I had not made.** I told the user a fresh lane was going out and did
   not call the tool; it sat idle until they asked. Say what you have done, not what you are about
   to do.
6. **I sent a brief long enough to be truncated.** Lane DI's first launch arrived as the numbers
   table with no task attached, and it searched the wrong worktree. Keep briefs compact enough to
   survive, and state the worktree path in the first line.

## Costs

Approximately **2.3M subagent tokens** across nine lane dispatches, for a 0.5% movement in the
widened column and a located-but-unfixed blocker.

| Lane | Item | Tokens | Outcome |
| --- | --- | ---: | --- |
| DA | 1a recon | ~252k | measured; DOM number later invalidated by its own config |
| DB | `U10`+ footer | ~218k | shipped |
| DC | `NoInfer` | ~253k | shipped |
| DD | `HG003` | ~176k | shipped |
| DE | blowup diagnosis | ~223k | diagnosed to 31 lines |
| DF | 1f ship path | ~228k | shipped |
| DG | frontier fix | **~605k** | nothing |
| DH | frontier fix | ~309k | counters shipped, two negative results |
| DI | width bound | ~39k+ | in flight |

**DG alone cost more than any lane that shipped.** Two dispatches, no change to `Resolve.fs`
either time, and a second report arguing against a premise that was never in its brief.

## What to do next

Ranked.

1. **Let lane DI finish.** If the width bound holds with no golden movement, DOM ships and the
   ECMAScript half probably follows. This is the only cheap path left.
2. **If DI fails, stop dispatching lanes at this problem.** Five approaches will have failed. The
   resolve tier's identity model needs a deliberate redesign — how a type reference is keyed when
   its arguments are open — and that is its own wave with a written design first, not a lane brief.
3. **Either way, the wave's shipped items are sound and can be handed to the user.** They are
   independent of the blocker.
4. **Wave sixteen still carries wave fourteen's six items**, listed in the worklist. None were
   touched.

## Standing rules this wave paid for

- Verify a lane landed by reading its branch, not its notification.
- Performance targets are counts, never durations, on this machine.
- Read a measurement's configuration before believing its number.
- Tier movement is per symbol and gross; a finding-key delta is a different quantity and three
  lanes have now offered one instead.
- Compose a batch and regenerate goldens before gating — wave thirteen found symbols that existed
  on neither branch alone; this wave's batch one found none, which is worth knowing either way.
- Briefs lead with the change. Lanes briefed to establish and report will report and build nothing.
