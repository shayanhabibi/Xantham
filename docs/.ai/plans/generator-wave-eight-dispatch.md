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
