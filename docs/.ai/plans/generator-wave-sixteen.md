---
category: Generator
audience: managing agent
title: Worklist - generator wave sixteen
integration-branch: worktree-generator-wave-sixteen
---

# Generator wave sixteen — the resolve tier closes on the compiler lib

Wave fifteen's worklist (`generator-wave-fifteen.md`) stands; this document records what wave
sixteen took from it and what remains. Read `docs/.ai/handovers/wave-sixteen-management.md`
first: it carries the fix, the measurements and the rejected designs.

## Taken

| Item | Outcome |
| --- | --- |
| Resolve tier's identity model (wave fifteen's opener) | Shipped, `1790322`. No re-keying: an instantiation is identity where nothing reads its members, structural where a union, an intersection or a seed does. `lib.dom` and `lib.esnext` close. |
| Unspellable member key | Shipped, `e8c0f36`. `MB007`. |
| Shipped group's file self-contained | Shipped, `e8c0f36`. Unowned aliases and hoisted shapes are placed with their owner's group. `lib.esnext` compiles clean against Fable.Core. |

## Remaining, in order

1. **1b/1c as packages.** `groups/TypeScript.Lib.fs` from a `"lib": ["dom"]` ship run carries
   both surfaces. Add a planner rule splitting the compiler-lib group by source file family
   (`lib.dom*.d.ts` against `lib.es*.d.ts`) into two modules, then wrap each as a project:
   ECMAScript into `src/Xantham.Fable.Core`, DOM into `src/Xantham.Fable.Browser`. Gate each
   by compiling, as the corpus goldens are gated.
2. **1e, shadowing.** `Naming.SupportBindings` lists six names; a shipped lib surface makes it
   several hundred. Measure the real collision set against what the corpus declares before
   choosing between the collision rule and the reserved-name rule.
3. **1d, `tools/browser-gen`.** Its 439-entry intersection table has nothing to intersect
   against once the DOM surface is ours; replace its authority before removing it.
4. **`TR008` in the lib output.** The escape column of both surfaces is `any -> obj`
   (`lib.dom` 17,514 sites). Whether that is acceptable for a shipped binding is a decision,
   not a measurement.
5. Wave fourteen's six carried items, listed in `generator-wave-fifteen.md`.

## Clauses

Wave fifteen's hold. One is added: **a probe package lives outside the repository tree.** The
gate's format stage sweeps every `.fs` under the worktree, and a 330,000-line generated file
under `.probe/` fails it.
