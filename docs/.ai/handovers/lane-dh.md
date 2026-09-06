---
category: Generator
audience: managing agent
title: Lane DH — frontier width instrumented, two memoization fixes tried and rejected
---

# Lane DH — generator wave fifteen, resolve-tier frontier width

Branch `worktree-gen-wave15-dh`. Problem statement: a generic method's return type applying the
enclosing interface's own fresh type parameter (`Foo<T>.map<U>(...): Foo<U>`) costs the resolve
tier disproportionately, distinct from plain self-reference (`Foo<T>.push(...): Foo<T>` costs
nothing extra).

## Step 1 — counts (the only success metric this lane reports against)

Added `XANTHAM_RESOLVE_COUNTERS`-gated instrumentation to `resolveTypeTable` in
`src/Xantham.Generator/Resolve.fs`: a mutable counter incremented by `fresh.Length` each time the
breadth-first walk admits a generation's frontier, printed against the final `Map.count table`
once the walk closes. Gated behind a lazily-read env var, so it costs nothing on a normal run.

Measured on `.probe/half1` (5 methods, self/no-fresh-parameter returns only — `push`, `concat`,
`slice`, `sort`, `splice`), `.probe/half2` (5 methods, each returning the enclosing interface
applied to a fresh method-level type parameter — `every`, `map`, `filter`, `reduce`,
`reduceRight`), and `.probe/full` (the union, 10 methods), against `Foo<T>` with no `lib`
dependency (same fixture shape DE's recon used):

| fixture | frontier-expansions | table-size |
|---|---|---|
| half1 | 24 | 24 |
| half2 | 39908 | 28644 |
| full | 62932 | 44500 |

Reproduced twice (once during the fix attempts, once again after reverting to confirm the
counters alone move nothing) with identical numbers both times — the resolve tier's behavior is
deterministic here, not a noisy-machine artifact. half2 alone, five methods each returning `Foo`
applied to a fresh type parameter, produces roughly 1000x half1's frontier width. This is the
"only success metric" the brief asks for, and it did not move: **no fix shipped**, so these
numbers are also the current, unfixed baseline.

## Step 2 — fix attempted and rejected (twice), reverted

Diagnosis (inherited from DE, confirmed here): `deriveFacts` memoizes on `TypeResponse.Id`, a
checker id assigned per response rather than per declaration. A generic method's own return type
reapplying the enclosing declaration to a fresh, still-open type parameter gets a brand-new
checker id at every occurrence, so the frontier never recognizes it as already-derived — bounded
only by `FollowDepth = 20` (a depth cutoff, not a width bound), which is why the growth is
exponential-ish rather than unbounded.

Both attempts tried the same strategy: memoize derived structural facts (`Members`,
`IndexInfos`, `CallSignatures`, `ConstructSignatures`, `BaseTypes`) across type-table entries,
keyed on something more stable than the transient response id, restricted to entries where every
type argument is an open (unbound) type parameter.

**Attempt 1** — key = `(Origin, SymbolParent, declaration name, argument-symbol ids)`, borrowing
canonical `Members`/etc. from the first occurrence seen. Broke 6 tests. Most notably,
`hoist-conditional-lab`'s golden went from clean to
`type DirectVarNode<'TNodeType, 'TNode, 'TNodeType, 'TNode>` — a doubled, duplicated
type-parameter list — with members merged in from `DirectExtensions<'TNodeType>` and
`DirectVarNodeInterface<'TNode>`, two structurally parallel but unrelated declarations that
happen to reuse the same type-parameter names. Separately, half2's own manifest picked up new
findings not present in either the unfixed baseline or a correct fix: `widened` went from 1 to 6,
with new `TR013`/`RA004` findings — the key under-collapsed relative to what output-neutrality
requires, in the opposite direction from Attempt 1's over-collapse on `hoist-conditional-lab`.

**Attempt 2** — refined the key by additionally verifying, via `getSymbolOfType` on each type
argument, that every argument really is an open type parameter (not just flagged as one), and
recording the declaration's own type-parameter symbol ids as part of the key. This closed the
duplication seen in Attempt 1. But re-testing `hoist-conditional-lab` specifically revealed a
second, independent corruption: `DirectExtensions<'TNodeType>`'s `toVar` member rendered
returning a concrete `DirectVarNode<'TNodeType, DirectExtensions<'TNodeType>>` substitution,
where the golden deliberately renders `obj` (a widen). That fixture's `toVar` member returns a
`this`-type through a hoisted, conditional-type-flattened-to-intersection alias — a case the
recognizer is supposed to decline to resolve by design, contrasted deliberately in the fixture
against a plain-intersection control case. Attempt 2's memoization caused the recognizer to
resolve it anyway.

Both attempts fail for the same reason, not two unrelated reasons: `hoist-conditional-lab` is
built specifically to reuse the type-parameter names `TNodeType`/`TNode` across two structurally
parallel but semantically unrelated declaration families, to catch exactly this class of
collapse-by-name-or-openness bug. Under two materially different key designs — one keying on
declaration name plus argument-symbol ids, one additionally confirming argument openness via a
second checker round-trip — both still corrupt or misresolve this fixture. That means
type-parameter symbol ids returned by the checker are not stable/interned per declaration across
repeated queries in the way a borrow-and-alias memoization key requires, and the difficulty is
structural to "share derived `Members` across occurrences of a possibly-different declaration
with the same shape", not a matter of finding a better key. This is the same failure class the
task brief's two pre-stated dead ends (raw `Target` collisions; `getTargetOfType`-alone arity
doubling) already ruled out, reproduced here under different specific designs.

Both attempts are fully reverted. `src/Xantham.Generator/Resolve.fs` now carries only the
counters from Step 1 — confirmed via `git diff`, which shows a single, additive 21-line diff
against the pre-existing file, and via an empty `XANTHAM_UPDATE_GOLDEN=1` diff against every
golden fixture. Full suite: 532/532 passed, twice (once before the golden-regen check, once
after). `XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test` (the full gate) also passed clean in
the foreground.

Per the task's explicit permission — "if the frontier cannot be closed without moving output,
stop and report the numbers, that is a real result; silent revert is not acceptable" — this lane
stops here rather than shipping a fix that risks a third undetected corruption on some other
fixture. The counters are a real, committed, output-neutral contribution; the fix is not.

## Step 3 — output neutrality

Confirmed output-neutral. `git diff --stat` shows only `src/Xantham.Generator/Resolve.fs`
changed, +21/-0 lines, no other file touched. No golden fixture moved. No reproducer fixture was
added — there is no fix for it to guard, and adding one now would only cost the suite's runtime
for no benefit.

## Step 4 — `lib.dom` through the compiler-lib `ship` path

Tested, since the brief asks this be reported either way regardless of whether the frontier
closed. Built a scratch package (`{"lib": ["dom"], "groups": {"typescript/lib": "ship"}}`,
entry package with an empty `index.d.ts`) and ran `xantham generate` against it under a
PowerShell wrapper bounding wall-clock (180s) and working-set (6000MB), killing the process tree
on either bound.

**Result: it does not complete.** The run was killed at 171.1s, having reached 6025MB peak
working set (the memory bound, not the time bound, is what tripped). This is consistent with the
unbounded-frontier-width bug above: `lib.dom.d.ts` contains multiple generic, self-referential
interfaces in this exact shape (collection-like and event-target-like interfaces returning
themselves applied to fresh type parameters), so shipping it in full (rather than the widen path
DA/DE's earlier recon used) hits the same frontier blowup, at a scale five orders of magnitude
past `.probe/half2`. Do not attempt to ship DOM through the compiler-lib path, and do not run any
ECMAScript-lib test through it either, until the frontier-width bug above is actually closed —
per the brief's own instruction, this was not attempted.

One process-cleanup note for whoever reuses this: the kill left a `tsc.exe` child (the
TypeScript language-service host the generator spawns) resident after the wrapper's own
`Kill($true)` on the parent `dotnet.exe` — `Process.Kill($true)` does not appear to reliably take
the compiler's own child process with it. Recovered via `Get-CimInstance Win32_Process` filtered
on the worktree path, `Stop-Process -Force` (not `taskkill /F` directly — Git Bash's path
translation mangles `/F` into a drive-letter argument; use PowerShell's own `Stop-Process` or
`taskkill` invoked with `//F` to suppress the translation). No other lane's process was touched.
All scratch files (`.scratch/dom-ship-lab`, `.scratch/bound-run.ps1`) deleted before this commit;
nothing under `.scratch/` is tracked.

## Recommendation for the next lane

- Treat "type-parameter symbol id stability" as the open question, not "which key". Before
  attempting a third memoization design, verify empirically (with a debugger or the
  `XANTHAM_FRONTIER_DUMP` hook already in `Resolve.fs`) whether the checker's type-parameter
  symbol ids are stable across repeated `getSymbolOfType` calls for the *same* declaration's
  parameter, across different instantiation paths. Both attempts here assumed they were close
  enough to usable and both were wrong on `hoist-conditional-lab` specifically.
- Consider whether the fix belongs at resolve time at all. `Shape/FreeTypeParams.fs`'s
  `freeTypeParams` is exact-identity-sensitive to borrowed `Members`/`TypeArguments`, which is
  why borrowing structural facts at resolve time is fragile — a render-time approach that
  recognizes "this member returns the enclosing declaration applied to a still-open parameter"
  structurally, without needing the two occurrences' checker ids or symbol ids to agree, may be
  more robust than resolve-tier memoization.
- A frontier **width** bound (independent of the existing `FollowDepth` depth cutoff) is still
  worth adding regardless of whether a memoization fix ever lands — it is the second, independent
  mitigation DE's recon already flagged, and this lane's numbers (half2 alone producing ~1000x
  half1's frontier) make the case for it concretely.
- Do not re-attempt `hoist-conditional-lab` as a "clean up the collision, then retry" side quest.
  It is doing its job: catching exactly the class of bug both this lane's attempts and the two
  pre-stated dead ends fall into. Any real fix has to survive it, not route around it.

## Provenance

Compiler: same pinned `typescript` build used by other wave-fifteen lanes, via the repo's
`node_modules/@typescript/typescript-win32-x64`. Every measurement ran directly against a
Release build of `src/Xantham.Cli`. `.probe/` (half1/half2/half3/full fixtures and their outputs)
and `.scratch/` (the DOM-ship scratch package and the bounded-run wrapper) both deleted before
this commit; neither is tracked. No leftover `dotnet.exe`/`tsc.exe` process from this lane
remained on the shared box at handover time.
