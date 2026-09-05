---
category: Generator
audience: managing agent
title: Dispatch - generator wave eight
integration-branch: worktree-generator-wave-eight
---

# Generator wave eight — dispatch

Triage of `generator-wave-eight.md`, the batch structure it produced, and the decisions the
worklist handed to the manager. Every lane's handover sits in `docs/.ai/handovers/`.

`worktree-generator-wave-eight` forks `master` at `ce59075`. Wave seven is already merged into
`master`, so no rebase is owed.

## Baseline, measured on `master` at `ce59075`

Gate: 460 generator tests, 90 wire tests, run gate 230 checks. Corpus over all 49 fixtures:
`exact 479, ergonomic 1540, widened 783, escape 193`.

Residues: `TR008` 569, `TR055` 357, `TR023` 137, `TR018` 82, `TR036` 72, `TR037` 54, `DO001` 19,
`SY005` 7, `AC001` 3. `MB001` 610.

## What decided the batch structure

**Item 10 was already live on the integration branch.** The worktree held 47 F# files reported as
modified with a byte-empty content diff under `--ignore-cr-at-eol` — fantomas' CRLF output against
a `.gitattributes` demanding LF. `git merge` refuses to run against that tree, so the wave could
not be integrated at all until it was fixed. It became a prerequisite rather than a queue item,
and the tree was cleaned before dispatch.

**Items 9 and 10 are one lane, not two.** Both are defects in the repository's own tooling and
both land in the build script and its sibling dotfiles. Splitting them buys nothing and costs a
merge.

**Items 2 and 3 are one lane, not two.** The worklist priced them as the same subject matter
landing in `Shape/Spec.fs` and `Shape/Overloads.fs`, to be run in sequence and never
concurrently. A single lane doing item 2 then item 3 is that sequence, with no merge between.

**Item 4 is answered before item 2 is dispatched**, because item 2 mints more derived names. Its
measurement needs no build — the goldens are committed — so it ran as read-only recon alongside
batch 0 rather than consuming a slot in batch 1.

## Decisions the worklist handed to the manager

### Item 13 — positional `obj` provenance: struck

Deferred in waves five, six and seven. The reason each time was that no consumer joins checker
facts to body operations, and the per-symbol table already addresses a declaration site uniquely.
Lane AG's `NodeHandle` removes the implementation cost, but implementation cost was never the
reason for the deferral. Building the field now serves no consumer.

Struck from the queue. Should a consumer arrive, the field is a `NodeHandle` that Harvest already
holds and the work is small; that is the note this entry leaves behind, and it does not need to
be re-listed every wave to stay true.

### Items 11 and 12 — the fidelity queue and the small items: carried

Untouched, deliberately. Wave eight carries five measured items plus two tooling defects; opening
`TR023` at 137 or the `Fable.Core` binding gaps alongside them would exceed what one integration
can compose.

## Pre-declared finding cases

Appended on the integration branch before dispatch, so that no lane discovers a case mid-task.
Current maxima before this wave were `TR056` and `DO002`.

| Case | Code | Tier | Owner |
| --- | --- | --- | --- |
| `TR.BareNullToObj` | `TR057` | widened | Lane AN, item 5 |
| `DO.OverloadsDistinguishedByLiteralUnion` | `DO003` | exact | Lane AO, item 2 |
| `DO.ExportFunctionOverloadDropped` | `DO004` | widened | Lane AO, item 3 |

Each is unraised at declaration. A case a lane's evidence turns out not to justify stays unraised
and costs one dead row — that outcome is accepted in advance, and for lane AN it is a likely one.

## Dispatch rules every lane carries

Repeated into each brief rather than referenced, because a lane starts cold:

1. Read `.claude/rules/generator-fixtures.md` before anything else.
2. Verify the branch base before starting, and confirm it. Every lane's worktree was created by
   the manager from the integration branch and its SHA checked before the brief was written.
3. Commit as the work becomes coherent, not once at the end.
4. Run the fast loop while iterating; run `dotnet fsi build.fsx -- test` before the final commit.
5. Do not trust `build.fsx`'s exit code until lane AL reports on item 9. Read the summary line.
6. Write the full report to `docs/.ai/handovers/lane-<id>.md` and return at most fifteen lines.
7. Never `git push`, never open a PR, never merge into `master` or into the integration branch.
8. A lane that measures and refuses has succeeded. Say so in the brief, and price refusal as an
   available outcome.

### Item 4 — a retained literal derived from a URL: left uncapped

Answered by recon R2 from the committed goldens, before item 2 was dispatched. No code changed.

The corpus holds **10** derived names in total, across three fixtures — four in
`@cloudflare/workers-types` under `KVNamespace`, three in `animejs` under `DrawableSVGGeometry`,
three in `literal-overload-lab` under `Store`. The longest is 26 characters
(`HttpWwwW3Org1998MathMathML`). One name exceeds 24 characters and none exceeds 32. The name that
prompted the item, `HttpWwwW3Org1999Xhtml`, is 21 characters: the qualified path reads long, the
identifier does not.

Truncation collisions within a scope, at caps of 24, 32 and 40: **zero at every cap**. The three
URL-derived names under `DrawableSVGGeometry` diverge at characters 13 to 16, on the year segment,
well before any cap reaches them.

So the hash tail of option (c) guards nothing measurable, and the cap of option (b) would truncate
exactly one name in the corpus. Both add a mechanism that must itself be tested and that yields
identifiers no consumer can read back to their literal. All ten names are single-case
`[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]` unions reached through completion rather
than typed by hand, so length is nearly free where it occurs.

R2's caveat is recorded and it argues the same way: the corpus contains no pair of literals that
differ only past character 24, so it cannot demonstrate that truncation is safe in general — only
that nothing here truncates. A cap shipped on that evidence would be an untested mechanism
answering a hypothetical.

Left uncapped. The threshold that would change this answer is a fixture producing either a name
past roughly 32 characters or two literals in one scope sharing a long prefix; the measurement
above is what a future wave re-runs rather than re-deriving.

---

# Outcomes

Recorded as each batch merged. Every lane's handover sits in `docs/.ai/handovers/`.

## Batch 0 — the tooling prerequisite

| Item | Lane | Outcome |
| --- | --- | --- |
| 9 | AL | **Defect survived alpha 3.** Pre-fix a failing pipeline exits 0, post-fix 1, passing 0. |
| 10 | AL | **Clean tree after a full run.** `.editorconfig` gains `end_of_line = lf`. |
| — | prep | `TR057`, `DO003`, `DO004` declared. Gate held at 460 / 90 / 230. |

Lane AL produced both edits and then stalled three times waiting on a background build
without committing, across roughly 118 tool calls. The managing agent committed the lane's
working tree and carried out the verification. There is a symmetry worth keeping: an agent that
cannot trust an exit code falls back to polling a long build and loses its turn to the poll,
which is the recurring cost item 9 removes.

## Batch 1

| Item | Lane | Outcome |
| --- | --- | --- |
| 1 | AM | **Measured safe, no `src/` change.** Run gate 230 → 249. `TR055` 357 → 360. |
| 5 | AN | **Closed on a corrected premise.** `TR014` already records the site. `TR057` unraised. |
| 4 | R2 | **Closed from the goldens.** Ten names, longest 26 characters, zero collisions at any cap. |

**Item 5's worklist premise was wrong, and disproving it was the lane's result.** A bare
`x: null` was described as recording no absence fact. It records a widening fact: the type
reaches `typeRefOnPath`'s catch-all and the site carries `TR014` with `flags=Null`. The absence
alphabet has no entry for it, and the site is not silent, so a second finding would duplicate
one site. Corpus population is one, and it is the lab's own declaration.

**Item 1's read-back result is stronger than wave seven's.** Lane AK measured that a
function-typed member read back hands F# a curry wrapper of length 1. Through a union member it
does not: the member's declared type is the union, so the arity JavaScript holds survives.

Left flagged by lane AM, unmeasured: a union arm under an array or an `option`, and `U2<...>`
nested inside a delegate's type parameter.

## Batch 1 boundary, measured over the composed tree at `50fb712`

Composition introduced nothing. Every count each lane reported independently survives:

Gate: run gate **249** checks, all stages ok, and `git status` clean after a full run — the
first wave in which that last clause is true. Corpus over 49 fixtures:
`exact 488, ergonomic 1544, widened 783, escape 193`.

`DO001` 19, `DO002` 4, `TR055` 360, `TR056` 32, `TR014` 1. `TR057`, `DO003` and `DO004` stand at
zero: one settled unraised, two awaiting lane AO.

## Batch 2

| Item | Lane | Outcome |
| --- | --- | --- |
| 2 | AO | **Unreachable in the corpus. Raised nothing.** `DO003` stands at zero and a test asserts it. |
| 3 | AO | **Option (b), the loss is recorded.** `DO001` 19 → 18, `DO004` 0 → 3. |

**Item 2 was declined on two independent measurements**, not on the manager's suspicion. None of
the nineteen `DO001` sites is separated by a literal union; and a probe over all 230 overloaded
members in the corpus found exactly one parameter position taking two or more literals, the lab's
own `Choice.pick`, which `synthesize-anonymous` already names. The boundary is now exact:
`isLiteralUnion` declines a name only where a member is not a literal, so `"a" | "b" | string`
cannot reach the collision — the checker subsumes it — while `"a" | "b" | (string & {})` can. The
lab's `Blend` pins that, and the mechanism lane AF rejected stays rejected.

**Item 3 took the recorded loss.** Extending retention to export functions repairs nothing,
because neither animejs site is literal-separated. `DO004` reports the drop instead;
`ExportConstructor` keeps `DO001`.

The split of the nineteen, which is the measurement that decided it: export functions 2
(`$`, `mapRange`), `DrawableSVGGeometry` keyof type parameter 13, `Ai.run` 1, `AutoRAG.aiSearch`
1, `BrowserRun.quickAction` 1, lab `Widen.scan` 1.

`DO001` reconciles per fixture rather than in aggregate: animejs 15 → 13 with two moving to
`DO004`, the lab 1 → 2 gaining `Blend` and contributing one `DO004` for `emit`, and
`@cloudflare/workers-types` unchanged at 3 because its three sites are members rather than
exported functions.

# Carried into wave nine

1. **`BrowserRun.quickAction` is a literal-separated loss that lane AF's mechanism misses**, and
   it is the wave's one newly found defect. `BrowserRunContentOptions` and
   `BrowserRunMarkdownOptions` are distinct type ids that render as an abbreviation pair, so
   `literalErasedKey` never groups them while `normalize` collides them. The general shape:
   retention groups by type id, dedupe compares normalised F# signatures, and the two disagree.
   Repair belongs in `Shape/Spec.fs`.
2. **Two callback positions lane AM did not measure** — a union arm under an array or an
   `option`, and `U2<...>` nested inside a delegate's type parameter.
3. The fidelity queue and the small items, items 11 and 12, carried untouched for a fourth wave.

## What this wave establishes about its own method

**Three of the five priced items closed with no generator code**, and none of the three was
skipped. Item 4 was answered from the committed goldens, item 5 by disproving the worklist's
premise, and item 2 by measuring that the shape it repairs does not occur. The wave changed
generator behaviour in exactly one place, item 3, and that change records a loss rather than
repairing one.

**A worklist premise is a hypothesis.** Item 5 asserted that a bare `x: null` records no absence
fact; it records `TR014`. Item 2 assumed the collision was live; it is not. Both were written
from real observations of wave seven and both were wrong about what followed. Lanes were briefed
to measure the premise before acting on it, and that is what produced the wave's results.
