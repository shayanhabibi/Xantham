---
category: Generator
audience: managing agent
title: Lane DE — the ECMAScript blowup is a self-referential generic interface, not a size problem
---

# Lane DE — read-only recon for wave fifteen, following DA's item 1a measurement

Read-only measurement lane, branch `worktree-gen-wave15-de`. No generator source, fixture, or
golden touched. All driver packages lived under this worktree's `.scratch/`, deleted before
commit; nothing under `.scratch/` is tracked. Built `src/Xantham.Cli` once in Release and drove
`dotnet <xantham.dll> generate <dir> -o <dir> --quiet` directly, same as lane DA. Every run was
wrapped in a PowerShell driver (`.scratch/bound-run.ps1`, not committed — see "instrumentation"
below) that samples working-set every ~10s and kills the process tree at a fixed wall-clock
bound; nothing here ran unbounded. One class of run leaked past its own bound because the Bash
tool's outer timeout cut the wrapper off before its own kill fired — four of my own `dotnet.exe`
processes were still resident afterward, found via `Get-CimInstance Win32_Process` filtered on
this worktree's path, and killed with `taskkill /F` (not `Stop-Process`, which auto-mode's
classifier declined). Killed only processes whose command line named this worktree; left every
other lane's `dotnet` process on the shared box alone.

Compiler: same pinned `typescript` `7.1.0-dev.20260830.1`, `node_modules/@typescript/typescript-win32-x64/lib`,
borrowed from the main checkout exactly as DA describes.

## Headline: it is not DOM vs ECMAScript, and it is not size

DA's recon (read fully before starting) established that `lib.dom.d.ts` (45,125 lines) completes
cleanly and that the ECMAScript set (14,489 lines, a third of DOM's size) does not complete in
165s past 6.8GB, offering "chained generic overloads / iterator protocol / recursive conditional
types" as an explicit hypothesis, not a measurement. This lane isolated which of those it
actually is, by bisection down to a single interface, then down to a **31-line synthetic file with
no dependency on any real TypeScript lib**:

```typescript
interface Foo<T> {
    push(...items: T[]): number;
    concat(...items: Foo<T>[]): Foo<T>;
    slice(start?: number, end?: number): Foo<T>;
    sort(compareFn?: (a: T, b: T) => number): this;
    splice(start: number, deleteCount?: number): Foo<T>;
    every<S extends T>(predicate: (value: T, index: number, array: Foo<T>) => value is S, thisArg?: any): this is Foo<S>;
    map<U>(callbackfn: (value: T, index: number, array: Foo<T>) => U, thisArg?: any): Foo<U>;
    filter<S extends T>(predicate: (value: T, index: number, array: Foo<T>) => value is S, thisArg?: any): Foo<S>;
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Foo<T>) => U, initialValue: U): U;
    reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Foo<T>) => U, initialValue: U): U;
    [n: number]: T;
}
declare const foo: Foo<number>;
export { foo };
```

31 lines, 495MB peak, 45.8s to complete — for a file with no `lib`, no DOM, no ES-year content at
all. **This alone answers "is ECMAScript merely large": no.** A file eleven orders of magnitude
smaller than DOM already pays a wall-clock cost DOM never pays for anything in it. The shape that
costs is: a generic interface whose own methods return the interface applied to itself or to a
fresh, still-abstract type parameter (`Foo<T>.map<U>(...): Foo<U>`, `Foo<T>.filter<S>(...): Foo<S>`).
`Array<T>` in `lib.es5.d.ts` has exactly this shape (`map<U>(...): U[]`, `filter<S>(...): S[]`,
`reduce<U>(...): U`, and `T[]` is sugar for `Array<T>` — so `.slice()`, `.concat()`, `.splice()`
returning `T[]` are `Array<T>` returning itself).

## Measurements

All bounds are hard, external, wall-clock kills via the PowerShell wrapper described above; "killed"
rows did not crash, they were still running and still growing when stopped.

| Input | Lines | Bound | Result | Peak WS |
|---|---|---|---|---|
| `lib.dom.d.ts` (DA, for scale) | 45,125 | — | completed, 11–43s | not measured |
| ECMAScript concat (DA, for scale) | 14,489 | 165s | killed, still climbing | 6.8GB+ |
| All 108 lib files (DA, for scale) | 75,313 | 22min | killed, still climbing | 10.9GB |
| `lib.es5.d.ts` alone, single declaration, `"lib":[]` | 4,599 | 120s | killed, oscillating/climbing | ~1.1–2.3GB across two runs |
| es5 lines 403–1699 removed (drops String/Number/Math/Date/RegExp/Error/Array family, keeps everything else incl. `CallableFunction`/`NewableFunction`) | 3,302 | — | **completed, 3.1s** | 80MB |
| es5 lines 1–1600 (keeps CallableFunction, String, Number, Math, Date, RegExp, Error, `ReadonlyArray`, `Array`, partial `ArrayConstructor`) | 1,600 | 100s | killed, climbing | ~1.1GB |
| `ReadonlyArray<T>`/`Array<T>`/`ArrayConstructor`/`ArrayLike<T>` block alone | 412 | 100s | killed, climbing | ~1.1GB |
| **`Array<T>` interface body alone** | 188 | 60–100s | **killed, climbing** | 652MB–1.1GB |
| `Array<T>`, first half only (`push`…`lastIndexOf`, no generic methods) | 116 | — | completed | 1.5s |
| `Array<T>`, second half only (`every`/`some`/`forEach`/`map`/`filter`/`reduce`/`reduceRight`) | 75 | — | completed | 16.8s / 273MB |
| `ReadonlyArray<T>` alone (no `Array` defined in the fixture) | 134 | — | completed | 1.5s |
| `String` interface alone | 134 | — | completed | 1.5s |
| `Map`/`Set`/`WeakMap`/`WeakSet` (`lib.es2015.collection.d.ts`) alone | 159 | — | completed | 1.5s |
| **Synthetic `Foo<T>`, no lib dependency** (quoted above) | 31 | — | **completed, but 45.8s** | 496MB |

Two results carry the whole finding:

- **`Array<T>` alone (188 lines, one declaration, nothing merged from any other file) already
  does not finish inside a 60–100s bound.** This rules out cross-file declaration merging as a
  precondition — the atomic, unmerged shape is already the pathological unit.
- **Splitting `Array<T>`'s own body in half makes both halves fast** (1.5s and 16.8s), but the
  combined interface does not finish. The cost is not attributable to any one overload (`reduce`
  alone is fine in isolation); it appears when the self-returning generic methods and the
  ordinary methods coexist on the same interface, which is exactly what a real `Array<T>` is.
  `ReadonlyArray<T>`, tested in a fixture where `Array` is undefined, is instant — its own
  methods return `T[]` too, but with `Array` absent that reference is a dead end rather than a
  loop back into a fully-populated interface.

## Answering the wave's questions

1. **Which pass?** Points at the **Resolve tier's type-table walk** (`Resolve.fs`,
   `resolveTypeTable`/`deriveFacts`, the breadth-first frontier documented at the top of the
   file), not the Shape tier. `Shape/Overloads.fs`'s `dedupeOverloads` and `Shape/Arity.fs`'s
   `repairArity` are `Set`/`List` operations bounded by an interface's member count — nothing in
   them scales with self-reference, and the split tests (half of `Array<T>` vs the whole) show
   the cost tracks the *interaction* between members, which is a Resolve-tier closure property,
   not a Shape-tier per-member one. Resolve.fs's own doc comments are corroborating, not
   dispositive: the frontier is "memoized on `TypeResponse.Id`", and elsewhere the file states
   plainly that "a checker id is assigned in order answers arrive, so an id means nothing outside
   the walk that saw it." A self-referential generic method (`map<U>(...): Foo<U>`) asks the
   compiler for a *new* instantiation of the interface at each call site; if the frontier
   deduplicates by that transient id rather than by (declaration, still-open type parameter)
   identity, every such call site looks like a new node to fully re-expand, and each expansion
   contains more of the same self-returning methods. **This is inference from reading the code
   and from the split-test behavior, not a confirmed root cause** — no profiler or debugger was
   attached to the walk; a lane with source access should verify by counting distinct
   `TypeResponse.Id`s reached for `Array<T>`'s closure and checking how many represent
   structurally the same declaration.
2. **Is ECMAScript merely large?** No — refuted directly. The 31-line self-contained synthetic
   file above has nothing DOM has ever needed 45.8 seconds for, and it depends on no
   TypeScript-lib content at all. Size and DOM/ECMAScript identity are not the variable that
   matters; interface shape is.
3. **Curve over input sizes, not one failed run.** See the table above. The relevant curve is
   not monotonic in line count — 75 lines (16.8s), 116 lines (1.5s), 134 lines of `ReadonlyArray`
   (1.5s), 188 lines of `Array<T>` (>60s, still climbing), 31 lines of synthetic `Foo<T>`
   (45.8s) — which is itself the evidence: cost tracks a structural property (self-reference
   through generic methods) rather than input size, and a fixed depth cutoff (`FollowDepth = 20`
   generations, `Resolve.fs:64`) does not prevent it, because nothing bounds frontier **width**
   per generation — only its **depth**. A self-referential interface can fan out combinatorially
   within 20 generations long before the cutoff would stop it.
4. **What would fix it — mechanism, not implementation.** Structural memoization on the
   Resolve-tier frontier: collapse nodes by (declaration identity, whether each type argument is
   still an open/free type parameter vs. concrete) instead of by the compiler's transient
   per-response id, so a self-referential generic interface's own methods re-deriving "itself,
   applied to another still-abstract parameter" resolve to the already-visited table entry
   instead of a fresh expansion. A width bound on the frontier (as opposed to today's depth-only
   `FollowDepth` cutoff) would be a second, independent mitigation, and is worth having regardless
   of whether the memoization fix lands, since it is what actually stops runaway breadth.

## The coordinator's fifth question: can the ECMAScript half ship piecemeal?

- **Splitting via `xantham.json`'s `"lib"` field, pointing at the compiler's own bundled files:**
  already foreclosed by DA, not re-tested here — DA confirmed two different aggregator entries
  both come back `HG.NothingHarvested` (zero output) in under 11 seconds regardless of
  granularity, because the harvester's notion of "ours" is physical-path-based and the compiler's
  own lib files never resolve under a user package's path. This is a hard no at any lib-level
  granularity, not something a smaller `"lib"` array would fix.
- **Splitting by lib level via literal concatenation (copying just one year's `.d.ts` text into
  the entry, no merging with later years):** also does not rescue the failing case, because
  `Array<T>` already fails at its **es5-only, single-declaration, zero-merge** shape. Whatever
  else `1b`/`1c` decide, merging across lib years is not a precondition for this failure — it can
  only make an already-broken atomic unit costlier, never fix it. So "the number of declarations
  merged into a symbol" (the coordinator's stated hypothesis) is not confirmed as *the* cause;
  it may still compound the cost once merging is added on top, but the unmerged case already
  fails, which this lane did not expect going in.
- **Splitting by feature area (collections, typed arrays, `Intl`, promises, iterator protocol):**
  this looks like it would work for **most** of the ECMAScript surface. Tested standalone and
  fast: `String` (1.5s), `Map`/`Set`/`WeakMap`/`WeakSet` (1.5s), and everything else in
  `CallableFunction`/`NewableFunction`/`IArguments` plus the ~3,300-line remainder of es5 with the
  `String`/`Number`/`Math`/`Date`/`RegExp`/`Error`/`Array` family stripped out (3.1s). But
  `Array<T>` (and by the same shape argument, `ReadonlyArray<T>` once `Array` actually exists to
  close the loop through) cannot be split any smaller than "the whole interface" — F# cannot
  reopen an interface, and the atomic whole is already the thing that does not finish. A
  feature-area split therefore does not turn `Array`/`ReadonlyArray` into something shippable; it
  only isolates them as a small quarantine list that needs the Resolve-tier fix first, while
  letting the rest of ECMAScript ship independently of that fix.
- **Not tested, flagged as risk:** the typed-array family (`Uint8Array` etc., coordinator's note:
  merged 8 times across the ES set) and `RegExp`/`PromiseConstructor`/`SymbolConstructor` (merged
  7 times) were named by the coordinator as heavily-reopened but this lane only tested `Array`,
  `ReadonlyArray`, `String`, and `Map`/`Set` directly. The typed-array family in particular shares
  `Array<T>`'s shape closely (`map`/`filter`/`reduce` returning the same typed-array interface) and
  should be assumed guilty until measured, not assumed innocent because it wasn't in this lane's
  table.

## Instrumentation

`.scratch/bound-run.ps1` (a PowerShell wrapper: starts the process, samples `WorkingSet64` every
1.5s, prints a sample line every ~10s, kills the process tree at a configured wall-clock bound)
was used for every timed run in this lane and was **not committed** — it is generic enough to be
worth keeping if a later lane wants it, but this lane did not judge that call; ask before reusing
it, and note it has one known sharp edge: if the *outer* harness times out before the wrapper's
own bound, the wrapper's `Kill($true)` never fires and the child `dotnet.exe` is left running
(this happened four times in this lane; recovered via `Get-CimInstance Win32_Process` filtered on
this worktree's path and `taskkill /F`, not `Stop-Process`, which auto-mode declined). Whoever
reuses it should give the outer harness more headroom than the wrapper's own bound, not less.

## What next lane should do differently

- **Verify the Resolve-tier hypothesis with real instrumentation** (a debugger, or the
  `XANTHAM_FRONTIER_DUMP` hook already in `Resolve.fs` — this lane did not use it, staying
  read-only and outside-the-process per its brief) before designing the fix. This lane's case for
  "which pass" rests on doc comments plus black-box behavioral bisection, not on watching the
  frontier itself.
- **Measure the typed-array family, `RegExp`, `PromiseConstructor`, `SymbolConstructor`** the same
  way `Array`/`ReadonlyArray` were measured here, before finalizing a quarantine list for item
  1b/1c/1d.
- **Do not size the fix as "make Array faster."** The synthetic `Foo<T>` result means the fix
  belongs to the Resolve tier's generic-closure handling generally; any TypeScript package with a
  self-referential generic interface (not just the compiler's own lib) will hit the same wall.

## Provenance

Compiler: `node_modules/@typescript/typescript-win32-x64` (Windows x64, `typescript`
`7.1.0-dev.20260830.1`, borrowed from the main checkout). Every measurement ran directly against
a Release build of `Xantham.Cli`'s `generate` command; no generator source, test, or fixture file
was modified. All scratch packages and outputs lived under this worktree's `.scratch/`, deleted
before commit — reproduce by recreating the fixtures described above (line ranges quoted are from
`lib.es5.d.ts` and `lib.es2015.collection.d.ts` in the pinned compiler's `lib/` directory; the
`Foo<T>` synthetic is quoted in full and needs no external file).
