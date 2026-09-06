---
category: Generator
audience: managing agent
title: Worklist - generator wave fourteen
integration-branch: worktree-generator-wave-fourteen
---

# Generator wave fourteen — worklist

An inventory, not a plan. Triage the items below, decide what the wave takes, and write the lane
briefs that structure it. Wave thirteen's record is
`docs/.ai/plans/generator-wave-thirteen-dispatch.md`; read its Outcomes, Batch three and Carried
forward sections before pricing anything here.

Read `.claude/rules/generator-fixtures.md` first.

## Baselines, measured on the composed wave thirteen tree

Wave thirteen is on `master`. Gate: **490 generator tests, 90 wire tests** (1 skipped by design
under `XANTHAM_TSGO_EXE`), **run gate 309 checks**. `build.fsx` exits its pipeline's real code and
the gate leaves the tree clean.

Corpus, all 55 fixtures: `exact 535, ergonomic 1603, widened 797, escape 200` over 3,135 symbols
and 17,040 findings.

Residues counted from the committed `symbols.jsonl`:

`TR032` 5131, `MB003` 3839, `SP001` 1754, `SY004` 726, `SP002` 662, `MB001` 650, `TR008` 574,
`TP002` 335, `TR055` 322, `TR006` 302, `TR059` 221, `TR009` 219, `SA002` 197, `TR045` 185,
`TR024` 147, `TR023` 136, `SP003` 134, `SI003` 121, `SI004` 89, `SI005` 81, `SY006` 80, `TR046` 65,
`MB004` 63, `TR031` 61, `TR049` 58, `TR018` 53, `TR020` 53, `TR040` 48, `TR050` 44, `MB006` 43,
`TR056` 34, `SI006` 31, `TR047` 29, `RA003` 29, `TP008` 29, `TR035` 27, `TR025` 27, `DT002` 24,
then a tail below 20.

Tier mass concentrates in three fixtures. Escape symbols: `@cloudflare/workers-types` 110,
`animejs` 54, `solid-js` 18, everything else in single digits. Widened symbols:
`@cloudflare/workers-types` 387, `type-fest` 202, `solid-js` 67, `shared-tag-lab` 25, `animejs` 18.

## The two numbers that decide this wave

**`TR008` is the escape tier.** 574 of the 585 escape-graded findings are `TR.AnyToObj`. Every
other escape code is in single digits: `AC001` 3, `HG005` 2, `TR054` 2, `RE001` 2, `HG001` 1,
`GE003` 1. No wave moves the escape column without moving `any`, and nothing else in that column
is worth a lane.

**`TR006` is the largest widened mass that is not already a declared floor.** 302 sites of
`TR.StringLiteralToString`. Above it sit `TR009` 219, where `unknown` maps to `obj` because that is
what `unknown` is, and `SA002` 197 with `TR045` 185, which are §4.11 phantom and conditional
machinery. `TR023` 136 was verified a floor in wave thirteen. All three are declined below.

## Carried from wave thirteen

1. **`isObjectMember` rejects intersection arms.** `TailStream.EventType`'s three sites are all that
   stand between lane CB's shared-tag fold and the site that motivated it. The checker distributes
   the intersection into arms flagged `Intersection` while the pass tests `TypeFlags.Object`; their
   members are populated, so only the flag rejects them. The edit is small and the blast radius is
   not, because admitting intersections moves discriminated-union detection corpus-wide. Price the
   radius before taking it.
2. **The resolve tier does not follow lib interface members.** `animejs`'s 13 remaining `TR020`
   sites are lane CD's own shape over `lib.dom`'s `HTMLElementTagNameMap`, which reaches the type
   table carrying zero members. Resolve-tier follow policy, not a `Shape/` change. Whatever it
   costs is paid once and every lib-typed position benefits.
3. **The CLI emits less than the harness.** `dotnet run --project src/Xantham.Cli -- generate` over
   `@cloudflare/workers-types` produced 26k lines where the harness produced 30k, measured in wave
   thirteen and not since. **The CLI is the user-facing entry point, so a divergence here is a
   defect in the product rather than a caveat about probing.** Re-verify against the current tree
   first; the number may have moved under three merges. Highest-value item on this list if it
   reproduces.
4. **Exclusive-arm object unions mint one interface per arm.** A union whose arms differ only in
   which members are `?: never` costs three names: one interface per arm plus the `U2` alias over
   them. Fold to one interface, exclusive members optional, one `[<ParamObject; Emit("$0")>]`
   `Create` overload per arm. Requested directly by the user. **Constrained: F# separates overloads
   on required parameters, and `Container.Start.Options2`'s `containerSnapshot` is optional in
   source, so that pair may not be foldable even though `AiSearchSearchRequest`'s `query`/`messages`
   pair is.** Needs `TR060`. Full statement in wave thirteen's Carried forward item 5.

## Priced from this wave's measurement

5. **Recon: how much of `TR008` is reachable.** 574 sites, 110 escape symbols in `@cloudflare` and
   54 in `animejs`. `any` is unconstrained in the general case, so the recon question is not whether
   `any` can be typed but **at how many sites something else in the declaration already constrains
   it** — a sibling overload, a `keyof` over the same operand, a call signature that consumes the
   position. Report per-site attribution and an explicit decline for the remainder. A measurement
   lane, not an implementation lane; wave nine's R3 is the model.
6. **Recon: `TR006`, 302 string literals widened to `string`.** `TR056` already keeps a literal
   where one is needed to separate an overload, 34 times, so the retention mechanism exists and the
   question is where else it pays. A single-literal member is a different case from a
   literal-union member; the latter is a discriminated-union candidate and may belong with the
   tagged-union passes instead. Split the 302 on that line before pricing either half.
7. **`TR031`, 61 callbacks shaped from their first overload.** A callback declared with two or more
   overloads is emitted from the first and the rest are dropped. F# can carry overloads on a
   delegate-typed member. More mechanical than items 5 and 6, and it needs no recon lane first.

## What not to chase

- **`TR009` 219.** `unknown` maps to `obj` because that is what `unknown` is. Recorded as D8.
- **`SA002` 197 and `TR045` 185.** Phantom computation and deferred conditionals, §4.11. F# defers
  no type. These are `type-fest`'s 202 widened symbols and they are the mapping working as designed.
  Waves twelve and thirteen both declined this and the reasoning has not changed.
- **`TR023` 136.** Verified a floor in wave thirteen; `Iterable`, `IterableIterator` and
  `BigUint64Array` are absent from the pinned `Fable.Core`.
- **`TR032` 5131 and `MB003` 3839.** The two largest residues in the corpus are `undefined` hoisted
  to `option` and optional members read as `option`. Both are correct mappings graded ergonomic.
  Their size is a property of TypeScript rather than a defect, and the headline number should not
  attract a lane.
- **Uncapping erased-union arity.** Rejected in wave thirteen as D4 and the reasoning stands: every
  `!^` cast resolves against one `op_ErasedCast` overload per case, so `U10`–`U100` adds cost to
  every cast in the corpus. Nine is a ceiling, not a preference.

## Clauses that change how this wave is dispatched

Wave thirteen's still hold. Four are worth restating because wave thirteen paid for each of them:

- **Report the gross, not the net.** Lane CH gave its tier movement as `+5` and set the residue
  aside as below its effort budget. The gross was twelve symbols to widened and three to escape,
  and the escape movement went unreported. It resolved to re-attribution rather than regression,
  but that was established at merge by the managing agent, not by the lane. A lane owes per-symbol
  attribution of every tier movement it causes.
- **A tier column can rise because the mapping improved.** When a minted declaration stops minting,
  the findings it owned reattach to every site that referenced it, so one declaration's mark becomes
  many sites' marks. Wave thirteen's widened column is not comparable across the lane CH merge. Any
  lane that removes declarations must say so explicitly.
- **A completion notification is not evidence of completion.** Lane CH reported completed while
  stopped mid-flight, with 36 files uncommitted and an empty transcript. Check that a lane branch
  carries commits before believing the lane landed.
- **A shape another declaration answers for should not be given a second name.** Wave twelve's lane
  BB, wave thirteen's lane CF and lane CH all met this rule, in three different passes. Treat it as
  a property of the generator rather than a lane-local surprise.

Unchanged and non-negotiable: goldens are regenerated, never hand-merged. Batches are gated as
batches, because wave thirteen found five widened symbols that existed on neither lane alone. Merge
messages are written to a file and applied with `git commit -F`; a message containing backticks
passed with `-m` is mangled by the shell. Never merge into `master` — the integration branch is
handed to the user, who lands it.

**Model constraint.** The Agent tool's `model` enum exposes `sonnet`, `opus`, `haiku` and `fable`,
and `opus` resolves to the parent session's context variant. Where the parent runs on a 1M-context
model, dispatching `opus` breaches the standing constraint that no teammate exceeds Opus 5 on a
standard context window. **Only `sonnet` and `haiku` are guaranteed compliant.** Wave thirteen
breached this on four lanes before it was caught. Consulting Fable 5.1 requires the user's
permission and a stated reason.

## Shape of the wave, if it helps

Item 3 should not wait: a user-facing entry point that emits less than the test harness is a defect,
and it is cheap to re-verify. Items 5 and 6 are recon and produce no binding change, so they can run
alongside anything. Items 1, 4 and 7 are implementation and touch `Shape/`; items 1 and 4 both alter
how union arms are read, so they should not share a batch without being composed deliberately.
