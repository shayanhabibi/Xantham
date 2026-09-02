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
   `generics-lab` are the models.
2. Per-pass unit tests in `tests/Xantham.Generator.Tests`.

Both are the deliverable a reviewer actually reads, so keep them **surgical**: the fewest
declarations that demonstrate the construct, each of its negatives, and nothing else. A lab that
also exercises three neighbouring features tells you nothing when it goes red. Write them before
the behaviour change where the task allows it — a lab that fails for the right reason first is
worth more than one written to match what the code already did.

## Large fixtures: regenerate, gate, measure — do not load

The npm rungs (`@cloudflare/workers-types`, `animejs`, `type-fest`, `solid-js`, and any rung added
later) are **still regenerated and still gated on every change**. Nothing here relaxes that:

- `dotnet build Xantham.slnx` — the compile gate compiles the committed goldens against
  Fable.Core and the `Fable.Browser.*` family. This is what decides whether a generated binding is
  legal F#, and it decides it by compiling, not by being read.
- `dotnet fsi build.fsx -- test` — Expecto suites plus the Fable run gate.
- `XANTHAM_UPDATE_GOLDEN=1 dotnet test tests/Xantham.Generator.Tests` — regenerate.

Unless the managing agent or the user directs otherwise:

- **Do not open a large golden binding file, and do not page through its diff.**
- **Do not load a whole manifest.** Its header — `counts` and the per-pass totals — is the summary
  worth reading and is the first ~87 lines; the `symbols` array after it is the part that is 78k
  lines long. `sed -n '1,/"symbols"/p' <manifest>` gets the header and stops.
- Per-symbol findings are **aggregated, never read**. A short script over the JSON counting
  `key`/`message` pairs gives the before/after profile:
  `python -c "import json,collections; d=json.load(open(F)); c=collections.Counter((f['key'],f['message']) for s in d['symbols'] for f in s.get('findings',[])); print(*c.most_common(25),sep='\n')"`

### Report measures, not contents

What a large fixture is *for* is the numbers it produces. Report those:

- Did it compile? Did the run gate pass?
- `git diff --stat` — how many files, how many lines moved.
- Finding counts by key, before and after. The finding your change targets falling, and the new
  ones it introduces rising, is the measurement that justifies the change.
- Tier counts (exact / ergonomic / widened / escape) and golden line counts where they moved.
- On failure: the first several **distinct** compiler error codes, deduplicated, with one
  representative site each — not the raw error stream.

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

## Asking for more

This is a default, not a wall. **If you believe you need to load something larger to do the task
properly, ask the managing agent or the user — with the exact reasoning**: what you are trying to
establish, what you already tried to establish it with, and why the aggregate did not settle it.
A specific request is normally granted. Silently loading 30k lines, or silently shipping a change
you could not verify, are both worse than asking.
