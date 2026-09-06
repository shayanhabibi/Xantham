---
category: Generator
audience: managing agent
title: Lane DA — pointing the generator at the compiler's own lib directory
---

# Lane DA — recon for wave fifteen item 1a

Read-only measurement lane. No generator source touched, no fixture, no golden, nothing under
`tests/` changed. Branch `worktree-gen-wave15-da`, forked from `worktree-generator-wave-fifteen`.
Driver script lived under `.claude/worktrees/gen-wave15-da/.scratch/` (temp package dirs plus
`xantham.json` for each), deleted before this commit; nothing under `.scratch/` is tracked or
committed. Driven with `dotnet run --project src/Xantham.Cli -- generate <dir> -o <dir>` directly,
no `build.fsx` stage — this is not a fixture, so `tools/xantham-fixtures.fsx` was never invoked.
Compiler resolved via the ordinary parent-directory walk in `Tsc.locate` (the worktree lives
nested inside the main checkout, so it reached the main checkout's `node_modules` unaided; no
`XANTHAM_TSGO_EXE` export was needed).

Pinned compiler: `typescript` `7.1.0-dev.20260830.1`, lib files at
`node_modules/@typescript/typescript-win32-x64/lib/lib.*.d.ts`, 108 files, 75,313 lines combined,
0 have a `package.json` of their own.

## Headline answer: "the shipped compiler-lib package" is not one file

`Bootstrap.entryFile`/`Pipeline.run` want a package directory with a `package.json` and one entry
`.d.ts`. The compiler's `lib` directory is not shaped like that — it is 108 flat files stitched
together at compile time by `/// <reference lib="X" />` directives that the compiler resolves
against its own bundled copies, never against a user package. That has one large, non-obvious
consequence discovered here, not assumed going in:

**`Harvest.harvestGlobals` cannot see anything reached only through a `lib="X"` reference.** It
calls `getSymbolsInScope` at the entry file's position 0, then keeps only symbols whose *first
declaration* (`Grouping.classify`, `Model.fs:1275`) resolves to a path under the package
directory. A `/// <reference lib="dom" />` line loads the compiler's real, external
`lib.dom.d.ts` — outside any package directory — every time, regardless of `xantham.json`'s
`"lib"` setting. So an entry file that is *itself* only reference directives (`lib.d.ts`,
`lib.esnext.full.d.ts`, or any other aggregator) harvests **nothing** — not "widened", not
"escaped", zero declarations and zero findings, tier profile `escape 1` for a single
`HG.NothingHarvested` (`HG003`) marking the whole run. Verified on two different aggregator
entries (`lib.d.ts`'s own ES5+DOM+webworker+scripthost defaults, and `lib.esnext.full.d.ts`'s
full ES-through-esnext plus full DOM), both with `"lib": []` in `xantham.json` to suppress the
implicit default load. Both came back empty in under 11 seconds — this is not a slow failure,
it fails immediately and completely.

The only way tried here that gets real output is to make the lib content **physically present
under the package directory** — i.e., copy `.d.ts` bodies into the temp package rather than
reference them by name. That is a recon workaround, not a recommendation; a real implementation
of item 1b/1c should decide deliberately how the compiler-lib group gets its own `Ship`
disposition without literally duplicating 75k lines of upstream text into a synthetic entry.

## What actually ran, and what happened

### `lib.dom.d.ts` alone, copied as the entry (`"lib": []`)

45,125 input lines. **Completed twice, byte-identical both times** (generated `.fs`, `manifest.json`
and `symbols.jsonl` diffed to zero) — the naming walk terminates and is stable for this input.
10.96s and 43.35s across the two timed runs (machine was shared with other concurrent lanes'
`dotnet` processes throughout; wall time is not a clean signal here, but neither run showed
runaway growth).

- **Output: 333,937 lines** — about 11x the largest existing fixture golden
  (`@cloudflare/workers-types` at ~30k).
- **Tiers: exact 1204, ergonomic 1545, widened 297, escape 526** (3,572 symbols).
- **Top finding keys** (full list captured in `.scratch`, deleted; the top 18 below account for
  the overwhelming majority):

  | key | name | count |
  | --- | --- | --- |
  | TR032 | `TR.NullableHoistedToOption` | 39,344 |
  | TR008 | `TR.AnyToObj` | 16,781 |
  | TR007 | `TR.NumericLiteralToFloat` | 6,494 |
  | MB001 | `MB.OptionalParameterAsOption` | 4,644 |
  | TP002 | `TP.ConstraintDropped` | 3,173 |
  | MB006 | `MB.OptionalParameterFromUnion` | 2,712 |
  | MB003 | `MB.OptionalMemberAsOption` | 2,507 |
  | DO005 | `DO.KeyofConstrainedOverloadDropped` | 1,946 |
  | TR036 | `TR.UnionTooWide` (erased-union arity over 9) | 1,513 |
  | SP001 | `SP.ParamObjectSynthesized` | 980 |
  | SP002 | `SP.MethodMemberAsCreateParameter` | 954 |
  | SI005 | `SI.BaseInherited` | 805 |
  | TR056 | `TR.StringLiteralKeptForOverload` | 786 |
  | SI004 | `SI.ConstructorObjectDeclared` | 706 |
  | TR044 | `TR.ArgumentNotASubtypeOfConstraint` | 605 |
  | TR026 | `TR.ConstrainedArgumentWidened` | 604 |
  | SP003 | `SP.CreateNotSynthesized` | 411 |
  | TR023 | `TR.NotAmongGeneratedDeclarations` | 318 |

  **`TR036` at 1,513 is worth flagging directly against item 3's directive.** Item 3 sizes
  erased-union-arity-over-nine at 6 findings across the *entire current 55-fixture corpus* and
  says explicitly not to price it. `lib.dom.d.ts` alone produces 1,513 — two and a half orders of
  magnitude more than the corpus that sized the decision. The directive ("do it, do not price it")
  may still be the right call, but whoever picks up item 3 should know the DOM lib is where nearly
  all of it will land, not the existing fixtures.
- `xantham: 1668 declarations in this package share a name with a default-lib declaration` — this
  warning only appears when `"lib"` is *not* suppressed (first attempt, before adding
  `"lib": []`); once suppressed it goes away, confirming the shadow is exactly what the CLI
  warning describes and not a separate bug.

### Reference-only "whole `lib.*.d.ts`" entries — both fail immediately

- `lib.d.ts` copied as entry (default ES5+DOM+webworker.importscripts+scripthost, `"lib"` unset,
  i.e. real default-lib behaviour): `HG003`, 0 declarations, ~11s.
- `lib.esnext.full.d.ts` copied as entry (full ESNext + full DOM, `"lib": []`): `HG003`, 0
  declarations, ~2s.

Both are legitimate ways a consumer might reasonably try to point the generator at "the whole
lib set", and both come back silently empty rather than with a diagnostic that says why. That is
the sharpest single finding of this lane: **a plausible, natural first attempt at item 1a produces
zero output and a passing exit code**, not a crash and not an escape-tier warning proportional to
the problem. `HG.NothingHarvested`'s message text ("declares neither a module nor any ambient
global — nothing harvested") is accurate but easy to misread as "this package is empty" rather
than "everything this package pointed at lives somewhere the harvester will not follow".

### Concatenating all 108 lib files as one literal entry — does not complete

Built by `cat`-ing every `lib.*.d.ts` into one file (75,313 lines) so every declaration
physically resolves under the package directory, with `"lib": []`. **Killed after ~22 minutes**,
resident memory past 10.9 GB and still climbing, no output written. This machine runs several
concurrent wave-fifteen lanes' `dotnet` processes; letting it run further risked the shared box,
not just this lane, so it was terminated rather than run to exhaustion or OOM.

To find out whether DOM or ECMAScript was driving that, a second, smaller run isolated the
ECMAScript files alone: every `lib.es*.d.ts`, `lib.decorators*.d.ts` and `lib.scripthost.d.ts`
concatenated, 14,489 lines — **a third the size of `lib.dom.d.ts` alone, which finished in
well under a minute.** This one climbed past 6.8 GB and was still rising after 165 seconds, so it
was killed too, before producing output.

**That is the actual, load-bearing finding: the ECMAScript half (item 1b), not the DOM half (item
1c), is where this technique blows up, and it blows up on roughly a third of the input DOM
processes cleanly.** DOM's own declarations are numerous but structurally flat — mostly
interfaces with plain members plus a lot of overloads DO005 already resolves cheaply. The
ECMAScript core (`Array`, `Promise`, `Iterator`, `Map`/`Set`, the `Intl` formatters) is smaller in
line count but far heavier in generic and overload structure: deeply chained generic methods
(`Array<T>.map`/`reduce`/`sort` with several generic overloads apiece), the ES2015+ iterator
protocol threading through multiple lib years, and `Awaited<T>`-style recursive conditional
types. Something in that shape is not scaling the way DOM's shape does. This lane did not
diagnose which pass — that is real diagnostic work, out of scope for a read-only recon — but item
1b should not be estimated from item 1c's numbers; they are not the same problem.

## Answering the wave's six questions

1. **Does it complete at all?** Not as "the whole `lib.*.d.ts` set" via any technique tried here.
   DOM alone completes and completes identically twice. ECMAScript alone, and ECMAScript+DOM
   together, did not complete inside a ~20 minute, multi-gigabyte budget on a shared machine, and
   were killed rather than run to a conclusion. Whether they terminate at all, and in what time,
   is still open — this lane could not safely find out further.
2. **`@cloudflare/workers-types`, `lib.dom.d.ts`, `lib.es*.d.ts`, `lib.*.d.ts` — do DOM and
   ECMAScript need checking separately, not assumed together?** Yes, confirmed directly: they
   behave completely differently under this generator. DOM completes fast and cleanly; the
   ECMAScript core is the one that does not complete. `@cloudflare/workers-types` was not
   re-measured in this lane (it is an existing fixture and out of scope for a recon that targets
   the *compiler's own* lib, not a dependent package), but its `TR023` count in the wave-fifteen
   baseline is exactly the finding this item is meant to close, so it is downstream of whichever
   half — or both — end up shippable.
3. **Does the naming walk terminate and stay stable?** For `lib.dom.d.ts`: yes, twice, byte-for-
   byte identical output across two independent runs. For the full or ECMAScript-only inputs: not
   established — those runs never reached the point where naming would have run to completion.
4. **Tier profile.** See the DOM table above: `exact 1204 / ergonomic 1545 / widened 297 /
   escape 526` over 3,572 symbols, `TR032`/`TR008`/`TR007` dominate exactly as they do in the
   existing 55-fixture corpus (same three keys top the wave-fifteen baseline), so DOM's shape of
   loss is continuous with what is already known, not a new distribution.
5. **What breaks that would not have been guessed?**
   - **Reference-chain entries fail silently, with exit 0 and a clean but easy-to-miss escape
     finding, not a hard error.** A user pointing the generator at any of the compiler's own
     "aggregator" lib files gets nothing and has to know to look at `manifest.json`'s single
     `HG003` line to find out why.
   - **The compiler-lib group's own `Grouping.classify` (`Model.fs:1275`) already exists and
     already tags these declarations as `CompilerLib` today** — every existing fixture that
     references a DOM or ES name gets that name classified into a group whose disposition
     defaults to `Widen` for exactly this reason (item 1's own premise). What this lane adds is
     that making that group `Ship`-able is not simply "point Harvest at the lib directory" — the
     harvester's notion of "ours" is physical-path-based and a `lib="X"` reference never produces
     a physical path inside any user package, so shipping this group needs its own resolution
     path, not entry-package impersonation.
   - **The ECMAScript half is architecturally the harder of the two**, opposite to what the
     wave-fifteen dispatch language's ordering (1b before 1c, "ECMAScript half... Closes
     `fable-binding-gaps.md` entries 1-4" first) might suggest is the easier warm-up. Sizing 1b as
     smaller than 1c because DOM's raw finding count (`TR023`'s 136) looks bigger than
     ECMAScript's named causes (29 + 5) would be a mistake this lane's numbers argue against.
   - **`TR036` (erased-union arity) scales with the DOM lib far past its priced size** — see above.
6. Not answered: interface merging across files, `globalThis` declarations, self-referential
   generics as *specific* mechanisms — this lane did not get far enough into the ECMAScript or
   combined runs to observe them in isolation, and did not open `symbols.jsonl` beyond
   `grep`-ing finding keys (per the fixtures rule). The memory growth pattern (steady, not a
   step, not oscillating) is consistent with unbounded work proportional to something in the
   input rather than a single pathological declaration, but that is an inference, not a
   measurement — the next lane should treat it as a hypothesis to check, not a fact.

## What the next lane should do differently

- **Do not re-run the full or ECMAScript-only concatenation without a hard, external time or
  memory bound** (a wrapper that kills the process past a threshold, not a human watching
  `tasklist`). This machine runs other lanes' `dotnet` processes concurrently; an unbounded retry
  risks the whole batch, not just this branch.
- **Price item 1b and item 1c as separate problems.** They do not share a scaling story, and item
  1c (DOM) already has a clean, fast, stable measurement to build from; item 1b does not, and
  needs an actual profiling pass (which pass, which file, which construct) before anyone sizes it.
- **A real "shipped compiler-lib package" cannot be entry-package impersonation.** Whatever ships
  it needs `Grouping.classify`'s existing `CompilerLib` tag to carry a `Ship` disposition through
  the normal per-package resolution path, not a synthetic package that copies lib text so the
  harvester misclassifies it as `EntryPackage`. That decision belongs to whoever designs items
  1d/1f, and this lane's workaround should not be read as a proposed implementation.

## Provenance

Compiler: `node_modules/@typescript/typescript-win32-x64` (Windows x64, `typescript`
`7.1.0-dev.20260830.1`, borrowed from the main checkout, matching `AGENTS.md`'s pinned ground
truth). All measurements taken directly against `Xantham.Cli`'s `generate` command; no generator
source, test, or fixture file was modified. Every temp package directory and generated output
lived under this worktree's `.scratch/`, deleted before this commit — nothing described here is
reproducible from a file in the tree; reproduce by copying the cited `.d.ts` files into a fresh
package directory with `"lib": []` in `xantham.json` and running the CLI as shown at the top of
this file.
