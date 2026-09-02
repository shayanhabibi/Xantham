---
paths:
  - "src/Xantham.Generator/**"
  - "tests/Xantham.Generator.Tests/**"
  - "tests/Xantham.Generator.CompileGate/**"
  - "tests/Xantham.Generator.RunGate/**"
  - "tests/fixtures/**"
---

# Working the generator: small fixtures, measured large ones

The generator is gated against real npm packages, and those goldens are enormous — the
`@cloudflare/workers-types` binding is 30k lines and its manifest is 78k. An agent that reads one
has spent its context on output it did not need to see, and has less left for the change it was
sent to make.

Two costs look alike here and only one is real. **Running** against a large fixture is a build and
a test run: it costs no context and it is the project's actual safety net. **Reading** the result
is pure context burn. So the rule is not "avoid the large fixtures" — it is *run everything, read
almost none of it*.

## Prove the feature on a lab fixture

When given a feature task on the generator, **build the small evidence first**:

1. A hand-authored lab fixture under `tests/fixtures/<name>-lab/`, tracked in git, pinning the
   construct under the live compiler. `intersection-lab`, `statics-lab`, `brand-lab` and
   `generics-lab` are the models. **Name it `<feature>-lab` and it is tracked, gated and
   resolvable with no further edit anywhere**: `.gitignore` un-ignores the `*lab` family by
   pattern, the compile gate globs every golden, and the run gate's `register.mjs` reads the
   package name out of each fixture's own `package.json`. A lab under any other name is ignored
   by git and will vanish silently.
2. Per-pass unit tests in `tests/Xantham.Generator.Tests`.

Both are the deliverable a reviewer actually reads, so keep them **surgical**: the fewest
declarations that demonstrate the construct, each of its negatives, and nothing else. A lab that
also exercises three neighbouring features tells you nothing when it goes red. Write them before
the behaviour change where the task allows it — a lab that fails for the right reason first is
worth more than one written to match what the code already did.

## The inner loop is three flags

`build.fsx` carries the loop, so you do not have to reconstruct it out of `dotnet test` and a
shell one-liner. Iterating on a pass:

```
dotnet fsi build.fsx -- test --quick --update --no-run-gate --filter "<suite>"
```

- `--quick` skips setup, `--update` regenerates the goldens before asserting against them
  (it runs the suite twice: once writing, once checking), `--no-run-gate` drops the Fable run
  gate, which is much the slowest step.
- `--filter` narrows to one suite by name, where you know which one you are moving.

Every one of those flags removes something that is real safety on the way out and pure latency on
the way in. **Drop them all and run `dotnet fsi build.fsx -- test` before you commit.** A branch
handed over having only ever run the fast loop has not been gated.

## Large fixtures: regenerate, gate, measure - do not load

The npm rungs (`@cloudflare/workers-types`, `animejs`, `type-fest`, `solid-js`, and any rung added
later) are **still regenerated and still gated on every change**. Nothing here relaxes that:

- `dotnet build Xantham.slnx` - the compile gate compiles the committed goldens against
  Fable.Core and the `Fable.Browser.*` family. This is what decides whether a generated binding is
  legal F#, and it decides it by compiling, not by being read.
- `dotnet fsi build.fsx -- test` - Expecto suites plus the Fable run gate.

Unless the managing agent or the user directs otherwise:

- **Do not open a large golden binding file, and do not page through its diff.**
- **Do not load a whole manifest.** The per-symbol `symbols` array is the part that is 78k lines
  long, and it is never the thing you need.
- Per-symbol findings are **aggregated, never read**, and `build.fsx` does the aggregating:

```
dotnet fsi build.fsx -- findings                       # every fixture: tiers, then counts by key
dotnet fsi build.fsx -- findings --fixture animejs     # one fixture
dotnet fsi build.fsx -- findings --key TR014           # one finding across the corpus
```

Run it before your change and after it. The two outputs, diffed, are the measurement that
justifies the change - and they cost a page of context rather than a corpus.

### Report measures, not contents

What a large fixture is *for* is the numbers it produces. Report those:

- Did it compile? Did the run gate pass?
- `git diff --stat` — how many files, how many lines moved.
- Finding counts by key, before and after — the `findings` output above. The finding your change
  targets falling, and the new ones it introduces rising, is the justification for the change.
- Tier counts (exact / ergonomic / widened / escape) and golden line counts where they moved.
- On failure: the first several **distinct** compiler error codes, deduplicated, with one
  representative site each — not the raw error stream. A broken golden yields thousands of
  near-identical errors, so the build caps them at 25 (`MaxCompilerErrors` in
  `Directory.Build.props`); raise it only if you have a reason to believe the tail differs.

Targeted `grep` for the construct you changed, and a bounded sample of representative diff hunks
(`git diff -- <path> | head -n 200`), satisfy the repository's "read the diff before committing
it" rule. You are reviewing it in aggregate plus samples; the managing agent does the
large-fixture judgment at integration.

### Hand back what you cannot explain

If a large fixture moves in a way your small evidence does not account for — a diff in a package
you did not target, a count that moved the wrong way, a hunk you do not understand — **report it
to the managing agent with the pointer and stop**. Do not spend context chasing it. An unexplained
diff handed over early is cheap; one discovered at merge time, under three other branches, is not.

### Reduce bugs to a reproducer

A bug found in a large fixture is written up as the smallest `.d.ts` that reproduces it, quoted in
full. A citation into a huge generated file is nearly worthless to whoever picks the work up; a
five-line reproducer is the whole handoff.

## Working alongside other branches

Generator work is usually dispatched several branches at a time, and the passes themselves merge
cleanly — `Shape.fs` auto-merged on every branch of the last wave despite being the file all of
them edited. What conflicts is the handful of **append-only lists**. Most have since been turned
into patterns or made to read the tree, and the ones that remain need care:

- **Finding codes are positional.** A manifest key is its union's `Prefix` plus the case's 1-based
  declaration position, so `SI005` means "the fifth case of the `ShapeInterfaces` union" and
  nothing else. **Append your cases to the end of the union, never insert or reorder.** Two
  branches that each append are both correct in isolation and both renumbered by the merge —
  which is why `Findings.test.fs` snapshots key *and* case name together, and why that snapshot
  failing after a merge is the system working. Expect to renumber your citations at integration:
  `Pipeline.test.fs` and `Shape.test.fs` pin codes as bare string literals, deliberately, because
  those codes are published in every manifest.
- **The run gate is still a two-place addition** — link the golden in
  `Xantham.Generator.RunGate.fsproj` and add the checks to its `Program.fs`. Both are deliberate
  (a check is a judgement about behaviour, not a file listing), so both can conflict. Only add a
  lab to the run gate if it has runtime behaviour to prove; the compile gate already covers "does
  it compile", and covers it automatically.
- **Goldens are regenerated, not merged.** Generation is deterministic, so a conflict in a golden
  or a manifest is resolved by taking either side and re-running with `--update`, never by hand.
- Say in your handover which finding codes you added and which large-fixture counts moved. That
  is what the managing agent composes; a branch that reports only "green" cannot be composed.

## Asking for more

This is a default, not a wall. **If you believe you need to load something larger to do the task
properly, ask the managing agent or the user — with the exact reasoning**: what you are trying to
establish, what you already tried to establish it with, and why the aggregate did not settle it.
A specific request is normally granted. Silently loading 30k lines, or silently shipping a change
you could not verify, are both worse than asking.
