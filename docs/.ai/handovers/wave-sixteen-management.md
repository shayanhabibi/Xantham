---
category: Generator
audience: incoming managing agent
title: Wave sixteen — management handover
integration-branch: worktree-generator-wave-sixteen
---

# Wave sixteen — management handover

Wave fifteen closed on a resolve-tier defect it located and did not fix, after ten lane
dispatches and roughly 2.6M subagent tokens. Wave sixteen was run by one agent with no lanes.
It fixed the defect, then took the two blockers that stood between the fix and a compiling
compiler-lib binding.

## State

Branch `worktree-generator-wave-sixteen`, forked from `master` at `a4ac799` (wave fifteen's
close). Two commits, each gated by `XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test` in the
foreground with exit 0 and a clean tree:

| Commit | Change |
| --- | --- |
| `1790322` | resolve: an instantiation is derived as identity where nothing reads its members |
| `e8c0f36` | emit: a shipped group's aliases and hoisted shapes are written into its own file; a backticked member key is dropped under `MB007` |

Gate at the tip: 538 generator tests, 90 wire (1 skipped by design), run gate 323 checks, format
clean.

## The defect, and the fix

Wave fifteen's statement held: a generic method whose return type applies its enclosing
interface to the method's own type parameter (`Array<T>.map<U>(...): Array<U>`) cost the resolve
tier disproportionately. The mechanism is that the checker clones a signature's type parameter
every time it instantiates the signature, so `Array<U>` is a new type id every generation, and
the walk derived each one's members - the same forty methods under a fresh parameter - and
followed them. `lib.dom` carried a second shape on top: it applies `Array<T>` to hundreds of
element types, each an interned but distinct instantiation dragging Array's closure in.

Six wave-fifteen attempts re-keyed the frontier so that two instantiations would share derived
facts; every one corrupted `hoist-conditional-lab`, because member type ids differ per
instantiation. The fix does not share facts. It reads what the shape tier reads:

- A reference position renders an instantiation as the declaration applied to its arguments
  (`instantiationOf`, `Shape/Spec.fs`), so it needs identity - target and arguments - only.
- Members of an instantiation are read only where it stands as a seed or as an operand of a
  union or an intersection (the tagged-union and exclusive-arm passes).

So `Resolve.fs` keeps three registers per walk: the type parameters signatures declared, the ids
read structurally, and the instantiations derived as identity. An instantiation over a signature
parameter is identity everywhere. Any other instantiation is identity unless registered
structural, and one a later generation reads as an operand is re-derived in full
(`instantiation-operand-lab` pins that path).

Measured with `XANTHAM_RESOLVE_COUNTERS`, idle machine:

| Input | Before | After |
| --- | --- | --- |
| `.probe/half2` (5 generic methods, DE's probe) | 14,308 expansions, width cutoff | 40 expansions, closes |
| `lib.dom` shipped whole | killed at 171s / 6GB; bounded: 78.6% widened | closes at generation 13, 36s, 821MB |
| `lib.esnext` shipped whole | never completed | closes at generation 16, 5.5s, 229MB |

`lib.dom` tiers after: exact 481, ergonomic 1613, widened 456, escape 633 (the escape column
is `TR008 any -> obj`). `lib.esnext`: exact 101, ergonomic 256, widened 322, escape 111.

Corpus movement: seven goldens unmoved, `@cloudflare/workers-types` lost its 1,848-type
depth-cutoff finding (widened 383 -> 382) and its hoisted `KVNamespaceListResult` arms now list
parameters in declared order. Nothing regressed. `FollowWidth` rose from 4096 to 32768; the
widest legitimate generation seen live is 10,218.

Two rules that were tried first and rejected, so the next agent does not repeat them:

- **Every instantiation as identity.** Corpus-neutral except two places: `callable-hybrid-lab`'s
  exported value rendered as `Boxed<float>` rather than a method, and `solid-js`'s `Resource`
  lost its `DT001` finding because the tagged-union pass could no longer read the arm. The
  second is a diagnostic regression that would become an output regression on a plain-data
  generic arm. Rejected.
- **Only instantiations over a signature parameter as identity.** Fixes the probe; `lib.dom`
  still grows 1.5x per generation through concrete `Array<X>` instantiations. Not enough alone.

## The two blockers past the fix

With the walk closing, the shipped compiler-lib output was compiled against Fable.Core 5.2.0
and the support package in a scratch project. `lib.esnext` had exactly two error classes:

1. `RegExpConstructor`'s legacy property named `` $` `` - a backtick, which no F# identifier can
   carry. Dropped under `MB007 UnspellableMemberDropped`, beside the symbol-keyed drop.
2. `groups/TypeScript.Lib.fs` referred to 83 names in the entry module, which compiles after
   it: every union alias (`PropertyKey`, `ArrayBufferLike`) and every shape hoisted out of a lib
   interface (`NumberFormatOptions.UnitDisplay`) was placed in the entry module because a union
   has no `Origin`. The planner (`Pipeline.groupModules`) now places an unowned alias with the
   export it is the declared type of and a hoisted name with the declaration at its root.

`lib.esnext` compiles clean: 16,216 lines, zero errors. `lib.dom` compiles clean: 334,599 lines,
zero errors, about four and a half minutes of `fsc`.

## What did not ship

- No `src/Xantham.Fable.Browser`, no ECMAScript surface in `Xantham.Fable.Core`. The generated
  output exists and compiles as a scratch project; wrapping it as packages, shadowing names into
  `Fable.Core`/`Fable.Browser.*` (worklist items 1b, 1c, 1e) and rewriting `tools/browser-gen`
  (1d) are untouched.
- The DOM/ECMAScript split into two packages. A run of `"lib": ["dom"]` loads the ES libs too,
  so today both surfaces come out of one `groups/TypeScript.Lib.fs`. Splitting by source file
  needs a planner rule the way the hoisted-name rule was added.
- `TR023` 136 and `TR024` 147 in the corpus are unmoved: the corpus still maps lib names through
  `libBinding`, not through a shipped package.

## How to reproduce the lib runs

```
XANTHAM_TSGO_EXE=<repo>/node_modules/@typescript/typescript-win32-x64/lib/tsc.exe
XANTHAM_RESOLVE_COUNTERS=1
dotnet src/Xantham.Cli/bin/Release/net10.0/xantham.dll generate <pkg> -o <pkg>/out
```

`<pkg>` holds `index.d.ts` (`export {};`), `package.json`, and
`xantham.json` = `{ "groups": { "typescript/lib": "ship" }, "lib": ["esnext"] }` or `["dom"]`.
Keep `<pkg>` outside the repository tree: the gate's format stage sweeps every `.fs` under it.
Compile `out/groups/TypeScript.Lib.fs` then `out/<Module>.fs` against `Fable.Core` 5.2.0 and
`src/Xantham.Fable.Core`.

## Compiler-library packaging after this handover

Commit `0ed43ee` replaced the split library design with one `namespace rec TypeScript.Lib`
file containing `Es` and `Dom` modules: the two families reference each other. The current
[packing script](../../../tools/lib-pack/pack-libs.ps1) generates that file from a clean
producer and compiles it for `netstandard2.1` and `net8.0` while packing `Xantham.Fable.Lib`.

The tracked projects under `src/xantham-libs/` came from diagnostic commit `666ce0a`
("push buggy gen'd files to get second opinion from houston"). They retain the superseded
split output. The September 6, 2026 solution baseline spent 7m12s there and reported 50
errors across both DOM target frameworks, including missing ES types. Their two solution
entries have been removed; the snapshot files remain intact. The maintained `lib-ship-lab`
golden still participates in the ordinary compile gate.

The September 6 check regenerated the combined output with TypeScript
`7.1.0-dev.20260902.1` and `lib: ["esnext", "dom"]`. The 73,424-line core compiled for
`netstandard2.1` and `net8.0` with zero warnings/errors and no references to the producer module.
The packing recipe now selects both library families explicitly; `lib` replaces the compiler's
default set, so selecting DOM alone omits the ECMAScript declarations.

Global scope is reusable in this clean producer because every program source is a compiler
default library or an empty source/empty export. An application-enriched DOM scope remains a
separate dependency problem; the ownership fixture retains that reproduction. The earlier
334,599-line DOM-only measurement used a different input profile. NuGet packing and publishing
are separate from the generation/compilation check reported here.

## Standing rules, unchanged

Counts, never durations, for performance work. Read a measurement's configuration before
believing it. Compose then regenerate goldens before gating. Full gate, never `--quick`, before
a commit. Never merge into `master`.
