# Lane CA — cheap three

Wave thirteen, branch `worktree-gen-wave13-ca`, forked integration branch
`worktree-generator-wave-thirteen` at `a008490`. Three small, independent changes
to `erasedUnionRef` and its neighbours, three commits, each measured on its own
before the next started.

## Commits

1. `01e5072` — fix(shape): dedupe erasedUnionRef's per-arm findings alongside its arms
2. `92545fb` — feat(shape): raise ErasedUnionArity from 4 to 9, one U-type per Fable.Core arity
3. `1b0bff4` — feat(model): bind AsyncIterableIterator to JS.AsyncIterable in LibBindings

Final commit on branch: `1b0bff4`.

## Change 1 — dedupe erasedUnionRef's findings

`erasedUnionRef` (`src/Xantham.Generator/Shape/Spec.fs`) maps a union's member ids
to F# type references, accumulating each arm's findings into a mutable list while
`List.distinct` collapses the *arms* themselves. The findings list was never
deduplicated the same way: a union whose fourteen arms all widen or lose fidelity
identically reported fourteen identical findings for what renders as one type.

Fix: apply `List.distinct` to the accumulated findings list too, after arm mapping.
`Finding` is a record over `Pass`, `Symbol` (the owner), and `Kind` (an F# DU behind
an interface, structurally equatable), so this dedupes on the full `(owner, message)`
pair, not owner alone — a `BodyInit`-shaped owner whose several arms produce
genuinely distinct messages keeps every one of them.

No golden source-text line moved; every diff line was a count already present in the
same manifest/symbols line, confirming the change is "arm-accumulated finding count,
not new/removed content."

### Measurements (change 1)

Baseline (fork point, matches wave-thirteen dispatch table): TR037 54, TR018 82,
TR023 137, TR036 72, DO001 5, TR020 69, RT001 3. Tiers exact 513 / ergonomic 1616 /
widened 790 / escape 195. Total findings 17,928.

After change 1: TR037 15, TR018 59, TR023 137, TR036 69, DO001 5, TR020 69, RT001 3.
Tiers unchanged (513/1616/790/195 — dedup only removes duplicate *findings*, not
symbols, so tier assignment per symbol is untouched). Total findings 16,887.

Per-key deltas, every code that moved (not only the targeted TR037):

| key | before | after | why |
|---|---:|---:|---|
| TR037 (TemplateLiteralToString) | 54 | 15 | targeted: duplicate template-literal arm findings collapse |
| TR018 (IntersectionOverNonObject) | 82 | 59 | side effect: duplicate arm resolutions inside erasedUnionRef were raising the same TR018 once per duplicate arm |
| TR036 (UnionTooWide) | 72 | 69 | side effect: same mechanism, a handful of duplicate arm sets |
| TR023, DO001, TR020, RT001 | unchanged | unchanged | not raised inside erasedUnionRef's per-arm loop |

The TR018 drop is corroborated independently by lane CC's read-only remeasurement
(reported to the managing agent mid-task, not discovered by me): animejs's
`LayoutAnimationParams`/`AutoLayoutParams` `.delay`/`.duration`/`.ease` sites were
4 real intersection-over-non-object losses reported 24 times through this exact
duplication path. My own measurement (isolated via `git stash` to the change-1-only
tree) puts animejs's TR018 at 13 after change 1, down from a baseline of 33 — the
remaining 13 are the genuine distinct sites (some of the 24 lane CC counted are on
`AutoLayoutParams` itself, some on its dependents, so 13 vs. 4 is the corpus's full
picture, not just the one interface lane CC's reproducer used). Lane CC's successor
should treat corpus-wide TR018 59 (animejs 13) as its corrected baseline, not 82.

`git diff --stat` over goldens for this commit: 13 files (manifests + symbols.jsonl
under `@cloudflare/workers-types`, `animejs`, `intersection-empty-lab`,
`literal-overload-lab`, `solid-js`, `type-fest`), 95 insertions / 93 deletions — all
counts within existing lines, no `.fs` binding file touched.

Full gate before commit: `dotnet fsi build.fsx -- test` exit 0 (generator tests
469/469, wire tests 90/90, run gate 297/297 checks); `dotnet build Xantham.slnx`
succeeded.

## Change 2 — ErasedUnionArity 4 → 9

### Arity histogram (measured before moving the constant)

Built by grepping every `TR.UnionTooWide` line across
`tests/Xantham.Generator.Tests/golden/**/symbols.jsonl` (post change-1 tree, so the
69 sites above are the base population) and counting the `"arms"` field:

| arity | sites |
|---:|---:|
| 5 | 32 |
| 6 | 15 |
| 7 | 6 |
| 8 | 6 |
| 9 | 1 |
| above 9 (10, 11, 14) | 9 (4 at 10, 3 at 11, 2 at 14) |

Total 69, matching the corpus-wide TR036 count measured after change 1. Raising the
cap to 9 (one `U_n` type for every arity `Fable.Core` 5.2.0 ships, `U2`-`U9`)
recovers 60 of 69 sites; the remaining 9 (arity 10/11/14) still widen to `obj` and
would need `Fable.Core` to ship a wider `U_n`, which it does not.

### What moved

- `Shape/Spec.fs:356` — `ErasedUnionArity` literal 4 → 9, doc comment restated.
- `Render.fs:153` doc comment, `Model.fs` `FsErasedUnion` doc comment — both restated
  the old threshold in prose; `Render.printType` itself needed no code change since
  it already builds `U{arms.Length}<...>` from the arm count, not the constant.
- `Findings.test.fs` — the `UnionTooWide(5, 4)` example changed to `(10, 9)` (its
  payload/message assertions updated to match) so the unit test still demonstrates
  an over-cap case rather than becoming stale/misleading against the new constant.
- `Shape.test.fs` — the "a union wider than the erased arity still widens to obj"
  test used six arms, which now fits under the new cap and stopped testing what it
  claimed to test. Rewrote it with ten distinct arms (unrelated to the Findings.test.fs
  literal, this is a different scenario test) so it still exercises the widen path.
  This was necessary, not optional — the fast loop's regenerate-goldens step failed
  the first attempt at this change with an `Expect.equal` mismatch (`FsErasedUnion`
  where the test expected `FsObj`), which is exactly this test.
- `docs/.ai/plans/generator-type-mapping.md` §4.5(4) D4 record — threshold and the
  histogram evidence for it.

### Measurements (change 2)

TR036 69 → 9 (60 sites recovered). No other key moved. Tiers: exact 513 → 527 (+14),
ergonomic 1616 → 1630 (+14), widened 790 → 762 (−28), escape 195 (unchanged). Total
findings 16,887 → 16,825. (Tier counts move by fewer than the 60 arm-level sites
recovered because tiers are counted per top-level symbol, and several recovered
`UnionTooWide` sites shared a symbol with another finding already setting its tier.)

`git diff --stat` over goldens: 15 files (source `.fs` plus manifest/symbols.jsonl
for `@cloudflare/workers-types`, `animejs`, `lab`, `solid-js`, `type-fest`), 177
insertions / 165 deletions. This is the one change of the three whose golden diff
moves actual source text (`obj` → `U5<...>`/`U6<...>`/etc. at reference sites), which
is the intended effect, not a surprise.

No nine-arm (or wider, within cap) erased union in a nested position is constructed
anywhere in the corpus at this cap — the histogram's own arity-9 count is 1, and it
is a top-level union, not nested. Nothing here exercises the newly-widened boundary
at a nested position; noted per the brief, not chased further.

Full gate before commit: `dotnet fsi build.fsx -- test` exit 0 (469/469, 90/90, run
gate 297/297); `dotnet build Xantham.slnx` succeeded — the compile gate compiles the
regenerated `U5`-`U9` sites against the pinned `Fable.Core` 5.2.0, confirmed via
`fcs_nuget_types Fable.Core` against the compile-gate project that `U2`-`U9` all
exist there before committing.

## Change 3 — Model.LibBindings: AsyncIterableIterator → JS.AsyncIterable

Added one row to `Model.LibBindings.table` (`src/Xantham.Generator/Model.fs`,
next to `AsyncIterable`/`AsyncIterator`/`AsyncGenerator`/`IteratorResult`):

```fsharp
"AsyncIterableIterator",
("JS.AsyncIterable",
 1,
 Some "AsyncIterableIterator reads as JS.AsyncIterable; its next/return/throw methods are not on it")
```

Verified against the pinned `Fable.Core` 5.2.0 (the same assembly
`tests/Xantham.Generator.CompileGate` references) via `fcs_nuget_types` /
`fcs_nuget_members`, not by memory or a text search of the package:

- `Fable.Core` ships no `AsyncIterableIterator` or `IterableIterator` name of any
  kind.
- `JS.AsyncIterable<'T>` exists and has exactly one member, `asyncIterator() ->
  JS.AsyncIterator<'T>` — it does not carry `next`/`return`/`throw` directly, hence
  the loss note.
- `BigUint64Array` is absent from the assembly too (confirmed, unrelated to this
  row — not added, per the brief's explicit instruction not to guess a binding that
  is not there).

### Measurements (change 3)

One site moves corpus-wide: `@cloudflare/workers-types`' `ReadableStream.values()`
was `TR023` (`NotAmongGeneratedDeclarations`, widened to `obj`, tier widened). It is
now two findings on the same site: `TR024` (`LibExtraTypeArgumentsDropped` — the
TypeScript declaration passes 3 type arguments, `AsyncIterableIterator<T, TReturn,
TNext>`, against the bound arity of 1, so the extras are dropped per the existing
arity rule) and `TR025` (`LibBindingLoss`, the loss note), both tier ergonomic.

TR023: 137 → 136. No other key moved. Tiers are unchanged at the symbol level
(exact 527 / ergonomic 1630 / widened 762 / escape 195): `ReadableStream` was
already tier `escape` before this change, driven by other findings on the same
symbol (`TR008 AnyToObj` on `cancel`), so trading its `values()` finding from
widened to two ergonomic ones does not move the symbol's own worst tier. Total
findings ticks up by one (16,825 → 16,826): one widened finding became two
ergonomic ones, which is the intended trade — the site now compiles to
`JS.AsyncIterable<'R>` instead of `obj`.

`git diff --stat` over goldens: 3 files under `@cloudflare/workers-types`
(`Cloudflare.WorkersTypes.fs`, `manifest.json`, `symbols.jsonl`), 5 insertions / 5
deletions.

Full gate before commit: `dotnet fsi build.fsx -- test` exit 0 (469/469, 90/90, run
gate 297/297); `dotnet build Xantham.slnx` succeeded — the compile gate compiles
`ReadableStream.values: ?options: ReadableStreamValuesOptions -> JS.AsyncIterable<'R>`
against the pinned `Fable.Core`.

## Cumulative summary, fork point → final commit

| | fork (`a008490`) | final (`1b0bff4`) |
|---|---:|---:|
| generator tests | 469 | 469 |
| wire tests | 90 | 90 |
| run gate checks | 297 | 297 |
| exact | 513 | 527 |
| ergonomic | 1616 | 1630 |
| widened | 790 | 762 |
| escape | 195 | 195 |
| total findings | 17,928 | 16,826 |
| TR018 | 82 | 59 |
| TR020 | 69 | 69 (unowned by this lane, held for lane CD) |
| TR023 | 137 | 136 |
| TR036 | 72 | 9 |
| TR037 | 54 | 15 |
| DO001 | 5 | 5 |
| RT001 | 3 | 3 |
| exit code | 0 | 0 |

`git diff --stat` over goldens, fork point → final commit: 19 files changed, 239
insertions(+), 227 deletions(-) (listed per-change above; no file outside
`tests/Xantham.Generator.Tests/golden/` and the source/test/doc files named in each
change's section moved).

## Anything unexplained

Nothing. Every per-key movement above is attributed to a specific mechanism in one
of the three changes; none required editing `Findings.fs` (no new finding case was
needed, as instructed); no large-fixture diff appeared that the corresponding small
measurement (histogram, `fcs_nuget_members` lookup, isolated per-key delta) did not
account for.

One flag for the managing agent, not a stop-and-report item: TR018's corpus-wide
baseline for lane CC's successor is now **59** (animejs **13**), not the wave's
opening baseline of 82 (animejs 33) — lane CC should price its remaining reproducers
against the post-CA number.
