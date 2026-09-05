---
category: Generator
audience: managing agent
title: Dispatch - generator wave seven
integration-branch: worktree-generator-wave-seven
---

# Generator wave seven — dispatch

The plan wave seven's worklist asked for. It triages `docs/.ai/plans/generator-wave-seven.md`,
states which items the wave takes, and carries the lane briefs and batch structure.

## The rebase precondition is already met

The worklist warned that this branch forks from wave six's tip and must be rebased once wave six
lands. Wave six has landed: `master` is at `67061fe`, and `master..worktree-generator-wave-seven`
contains the worklist commit alone. No rebase is required.

## What decided the batch structure

Four of the six candidate items resolve through `Shape/Spec.fs`, and three of them within one
120-line region:

| Item | Site |
| --- | --- |
| 1 — `Func<_,_>` becomes an F# function | `Spec.fs:1200-1225` (`signatureRef`), `Callbacks.fs:34` |
| 2 — `KVNamespace.get` overloads | `Spec.fs:540` (`TR006`), `Overloads.fs:69` |
| 4 — `MB001` on optional parameters | `Spec.fs:1220`, `:1315` (`isOptionalParam`), `:1645` |
| 5 — `TR033` absence alphabet | `Spec.fs:1259` (`unionRef`) |

`.claude/rules/generator-fixtures.md` sequences `Spec.fs` alone rather than concurrently with a
wave. Items 1 and 4 edit the same function and cannot run together; item 2 sits 700 lines away
from item 1 and can. So the worklist's own shape hint — "items 1 and 2 are consumer-facing and
independent of each other" — holds for item 2 and fails for item 4, which the hint placed as the
one change sharing no file with the rest. It shares `signatureRef` with the wave's largest lane.

Item 5 is a payload on an existing case, which `generator-fixtures.md` assigns to the managing
agent rather than a lane. It is delivered in the pre-dispatch commit, not dispatched.

## Baseline, measured on this branch at `91ed5be`

Corpus `exact 464, ergonomic 1506, widened 773, escape 191`.
Gate: 419 generator tests, 85 wire tests (1 skipped by design under `XANTHAM_TSGO_EXE`), run gate
160 checks. `dotnet fsi build.fsx -- test` exits 0.

## Pre-declared finding cases

Appended in the pre-dispatch commit, with `FindingCodes.table` rows and `Findings.test.fs`
snapshot lines. **A lane uses the cases listed against it and edits `Findings.fs` no further.** A
case a lane finds it needs beyond these is a request back to the managing agent.

| Code | Case | Tier | Owner |
| --- | --- | --- | --- |
| `TR055` | `TR.CallbackKeptAsDelegate of reason: string` | Ergonomic | AE |
| `TR056` | `TR.StringLiteralKeptForOverload of literal: string` | Exact | AF |
| `DO002` | `DO.OverloadsDistinguishedByLiteral of parameter: string` | Exact | AF |
| `SY005` | `SY.NameSanitisedForIdentifier of key: string * sanitised: string` | Ergonomic | AI |
| `MB006` | `MB.OptionalParameterFromUnion` | Ergonomic | AG |

`TR033` gains the payload `fromNull: bool * fromUndefined: bool * fromVoid: bool`, threaded from
`absenceAcross model hoisted` at the raise site. Item 5 is closed by that commit.

An Exact-tier case firing at every callback site was considered for AE and rejected: it would
write 1,245 rows into the corpus manifests to record the default. `TR055` fires only where the
delegate is retained.

---

## Batch 1

Four workstreams, dispatched together. File overlaps are `Model.fs` (AE at ~742, AI at 247-290)
and `Render.fs` (AE at 155-172, AI at ~126) — different functions, several hundred lines apart.

### Lane AE — callbacks emit as F# functions (item 1)

**Model: opus.** The wave's largest consumer-facing change, and the one that can fail its own
design gate.

`D5` in `docs/.ai/plans/generator-type-mapping.md:762` decided delegates by default and named
curried emission as "a candidate config toggle later". This lane is that reversal, so it argues
against a recorded decision and must produce the evidence D5 did not.

**Gate the design before converting anything.** Build `tests/fixtures/callback-function-lab` and
its run-gate checks first, and establish what Fable 5.2 does with an F# function type in each
position the corpus uses: a callback parameter, a callback in a `ParamObject`, a callback as an
interface member, a callback returned from a member, and a callback of arity 0, 1, 2 and 3. The
question is whether the value crossing the boundary is a JavaScript function of the declared
arity, or a curried chain. **If any position curries, stop and report — do not convert the
corpus.** A partial answer is worth more than a wrong conversion of 1,245 members.

- **Files:** `Shape/Spec.fs` (`signatureRef`, 1200-1225), `Shape/Callbacks.fs`, `Model.fs`
  (`FsDelegate`, ~742), `Render.fs` (`printTypeIn` delegate arms, 155-172), `Shape/Arity.fs:22`,
  `Shape/Overloads.fs:34`, `Shape/ParamObjects.fs:81`.
- **Do not touch:** `Model.fs` above line 400 (`Naming`, lane AI), `Render.fs` below line 140
  (lane AI), `dedupeOverloads` in `Shape/Overloads.fs` (lane AF).
- **Lab:** `tests/fixtures/callback-function-lab`, registered in `Pipeline.test.fs` and linked
  into the run gate.
- **Findings:** `TR055` only.
- **Report:** delegate members before and after (baseline 1,245: cloudflare 559, animejs 515,
  solid-js 169, type-fest 2), tier counts, and the run-gate answer per position.

### Lane AF — literal-typed parameters keep the overloads apart (item 2)

**Model: opus.**

`KVNamespace.get` declares five overloads separated by a string-literal `type` parameter.
`TR006` widens the lone literal to `string` at `Shape/Spec.fs:540`, the five signatures become
identical, and `dedupe-overloads` (`Shape/Overloads.fs:69`) drops four. `arrayBuffer`, `stream`
and `json` have no F# spelling as a result. `DO001` reports 19 dropped overloads over 5 owners,
16 of them on `KVNamespace.get` and `getWithMetadata`.

Retain the literal where it separates an overload set, so the signatures stay distinct. The
overloads then differ in a parameter type rather than in return type alone, which is legal F#.
Decide and state whether the retained literal renders as a StringEnum over the set or as the
literal type itself.

- **Files:** `Shape/Spec.fs` around 540 (`TR006`), `Shape/Overloads.fs` (`dedupeOverloads`).
- **Do not touch:** `Shape/Spec.fs` 1190-1330 (lanes AE and AG), `normalize` in
  `Shape/Overloads.fs` at line 34 (lane AE).
- **Lab:** `tests/fixtures/literal-overload-lab`.
- **Findings:** `TR056`, `DO002`.
- **Report:** `DO001` count before and after, `TR006` count before and after, tier counts, and
  whether `KVNamespace.get` reaches all four forms in the regenerated cloudflare golden.

### Lane AI — names an F# declaration admits (items 3 and 6)

**Model: opus.** Two naming changes in one lane because both land in `Naming` and
`Render.qualified`, and splitting them is a merge chosen for no reason.

**Item 3.** Six `Name2` residues — `DurableObject2`, `WorkflowDurationLabel2`,
`WorkflowSleepDuration2`, `ScriptVersion2`, `TailEvent2`, `TracePreviewInfo2` — are namespaced
declarations that nesting under a namespace module resolves. Lane AD measured this and made no
change; `docs/.ai/handovers/lane-ad.md` carries the detail. Total residue is 124, and the
remainder are overload parameters and union arms with no owner to nest under. Move the six; leave
the rest.

**Item 6.** A member key that is not a legal .NET type name emits `FS0883`. The reproducer:

```ts
export interface Registry {
  "@cf/meta": { model: string };
}
```

emits a type named `Registry@cf/meta` and fails to compile. Pre-existing, byte-identical under
both naming schemes, and unexercised by the corpus, so nothing gates it today. `Naming.nestable`
already decides which segments are identifier-shaped; sanitise the synthesized declaration name
against the same rule and report the substitution.

- **Files:** `Model.fs` `Naming` (247-290), `Render.fs` `qualified` and `nestedBlocks` (~126).
- **Do not touch:** `Model.fs` `FsDelegate` (~742) or `Render.fs` `printTypeIn` (155-172), both
  lane AE.
- **Labs:** extend `tests/fixtures/nested-name-lab` for item 3; new
  `tests/fixtures/key-sanitise-lab` for item 6, carrying the `@cf/meta` key above.
- **Findings:** `SY005` for item 6; item 3 raises the existing `SY004`.
- **Report:** `Name2`-style residue count before and after, goldens whose names moved, and tier
  counts. Names moving across the corpus is expected here; say how many.

### Lane AH — the run gate's probes become goldens (item 8)

**Model: sonnet.** Mechanical, and touches no generator source.

`tests/Xantham.Generator.RunGate/Probes.fs` is hand-written F# mirroring forms wave six then made
the generator emit. Its own header says it should shrink once lanes AA and AD landed. Both have.
Each probe whose form now comes out of a lab golden moves to that golden's checks and out of
`Probes.fs`; `hook-interface-lab` covers the optional-hook probes and `nested-name-lab` the
nested-name ones. A probe with no generated equivalent stays, with a header saying which.

- **Files:** `tests/Xantham.Generator.RunGate/Probes.fs`, `Program.fs`,
  `Xantham.Generator.RunGate.fsproj`.
- **Do not touch:** anything under `src/`.
- **Findings:** none.
- **Report:** run-gate check count before and after (baseline 160), lines removed from
  `Probes.fs`, and which probes stayed with the reason.

### Recon R1 — which discovery mode `workerd` performs (item 7)

**Model: sonnet.** No code change, and it blocks no lane.

Wave six's lane AA emits each optional hook on an entrypoint class as an opt-in interface. Lane Z
settled the F# and Fable half: the form is discoverable by access and by `in`, invisible to
`Object.keys` and `hasOwnProperty`, and Fable emits the members unmangled. The platform half is
outside this repository and remains assumed. Read `workerd`'s own source for how it reads a
handler off an entrypoint instance — by property access, or by enumerating own keys.

Answer with the source citation, not an inference. If it enumerates, say so plainly: lane AA's
form must then be revisited and no generator change repairs it.

- **Files:** `docs/.ai/handovers/recon-workerd.md` only.
- **Findings:** none.

---

## Batch 2, after batch 1 merges and gates

### Lane AG — `MB001` fires on an optional parameter (item 4)

**Model: opus.** Sequenced behind AE because it edits `signatureRef`, which AE rewrites.

The wire does not flag optionality on parameter symbols, so `p.Optional` is true only for
expanded tuple-rest elements, and `isOptionalParam` (`Shape/Spec.fs:1315`) infers optionality
back from the hoist. `f(x?: T)` and `f(x: T | undefined)` are therefore one signature and the `?`
is unrecoverable at parameter positions. The worklist prices this as a wire change rather than a
Shape change, and it is: carry the parameter's optionality through the wire, then let
`isOptionalParam` read it instead of inferring it.

`MB006` separates the two spellings once they are distinguishable — a parameter admitting
`undefined` through its declared type is not a `?`.

- **Files:** `src/Xantham.TypeScript.Wire/**`, `src/Xantham.Generator/Harvest.fs`,
  `Shape/Spec.fs` (`isOptionalParam` and its call sites).
- **Lab:** `tests/fixtures/optional-param-lab`, pinning `f(x?: T)` against `f(x: T | undefined)`.
- **Findings:** `MB006`; `MB001` already exists and should start firing.
- **Report:** `MB001` count before and after, `MB006` count, wire test count, and tier counts.

---

## Deferred, with the reason

- **Items 9, 10, 11** — the fidelity queue (`TR018`'s 77, `TR023`'s 38, `TR036`/`TR037`,
  `alignOperands`, the `FollowDepth` cutoff), wave five's open items, and positional `obj`
  provenance. Carried again: the wave is already four lanes wide on a file three of them share.
- Item 11 should wait for a consumer joining checker facts to body operations. When one arrives,
  the field is a Wire node handle Harvest already holds, not `file:line`.

## Dispatch rules every lane carries

Repeated into each brief rather than referenced, because a lane starts cold:

1. Read `.claude/rules/generator-fixtures.md` before anything else.
2. Verify the branch base before starting: fork from `worktree-generator-wave-seven` at the
   pre-dispatch commit, and confirm it.
3. Commit as the work becomes coherent, not once at the end.
4. Run the fast loop while iterating; run `dotnet fsi build.fsx -- test` before the final commit.
5. Write the full report to `docs/.ai/handovers/lane-<id>.md` and return at most fifteen lines.
6. Never `git push`, never open a PR, never merge into `master`.

---

# Outcomes

Recorded after integration. Every lane's handover sits in `docs/.ai/handovers/`.

Gate over the integrated tree: **456 generator tests, 90 wire tests, run gate 179 checks.**
Baseline was 419 / 85 / 160.

| Item | Lane | Outcome |
| --- | --- | --- |
| 1 | AE | **Refused on evidence.** D5 upheld. Corpus unchanged at 1,245 delegates. |
| 2 | AF | `DO001` 36 → 18, cloudflare 19 → 3. `KVNamespace.get` reaches all four forms. |
| 3, 6 | AI | Residue 125 → 118, seven names resolved. `FS0883` closed for type names. |
| 4 | AG | `MB001` 0 → 610, `MB006` 37. Wire carries parameter optionality. |
| 5 | — | Closed in the pre-dispatch commit. `TR033` carries its alphabet. |
| 7 | R1 | `workerd` reads hooks by property access. Lane AA's form stands. |
| 8 | AH | `Probes.fs` 83 → 43 lines, check count held at 160. |
| — | AJ | `FS0883` closed for union case names. Pre-existing, found through item 2. |

## What the wave established beyond its items

**Item 1's answer is the wave's most valuable result, and it is a refusal.** Under Fable 5.2 an
F# function type crosses the boundary correctly in parameter position at every arity, in a
`ParamObject`, and as a method return at arity 0 and 1. It fails as a method return at arity 2 or
more: `(factory.make 5.0) 1.0 2.0` compiles to `factory.make(5)(1)(2)` and raises `TypeError`. It
also fails when a function-typed member is read back, which hands F# a curry wrapper of length 1.
Both failures type-check and compile. `@cloudflare/workers-types` and `solid-js` both return
callbacks from methods, so the corpus reaches the failing position. D5's default was right and now
rests on measurement rather than on expectation.

Lane AE handed back a hybrid — function types in parameter and `ParamObject` position, delegates
retained on member returns and on function-typed members — with every position it needs already
measured green. It needs a nesting rule: `Factory.Create` in the lab golden synthesises
`Func<float, Func<float, float, string>>`. That is the shape of a wave eight lane.

**Two of item 3's six residues were the global declaration, not the namespaced one.** The
namespaced declaration is declared first in source and claims the bare name first, so `name-exports`
reads the whole claim list before granting any. Nesting at the collision would have moved the wrong
declaration.

**A parameter's `?` is syntactic and the checker discards it.** Measured: `SymbolFlags` and
`CheckFlags` are identical whether the `?` is present or not. It survives only as
`ParameterDeclaration.questionToken`, so Wire gained `NodeHandle` to follow a symbol's declaration
into the blob. Emission still collapses `f(x?: T)` and `f(x: T | undefined)` onto `?x: T`, because
F# has one form and it admits both calls; the `MB001`/`MB006` pair is the record that was missing.

## Carried into wave eight

1. **The hybrid callback emission** lane AE priced. Highest consumer value on the list.
2. **A retained literal derived from a URL makes a long name** — `"http://www.w3.org/1999/xhtml"`
   becomes `DrawableSVGGeometry.HttpWwwW3Org1999Xhtml` in animejs. Deterministic and compiling; a
   naming judgment rather than a defect.
3. **An anonymous union of literals at a distinguishing position still collides.** No single literal
   type stands for `"a" | "b"`. Pinned by `literal-overload-lab`'s `Choice`.
4. **Export-function overloads are uncovered by item 2's fix** — animejs's `$` and `mapRange`, and
   neither is literal-separated.
5. **A bare `x: null` records no absence fact.** It is not a union, never reaches `unionRef`, and
   widens to `obj`. Pinned as the negative in `absence-alphabet-lab`.
6. The queues items 9, 10 and 11 carried, untouched again.

## Two process facts worth carrying

**`build.fsx` exits 0 when its pipeline fails.** It prints `Error: Pipeline is failed because the
result is not indicating as successful` and then exits successfully. A wave that trusts the exit
code will report a broken tree as green; this one nearly did. Read the summary line.

**`build.fsx -- test` runs fantomas, which writes CRLF on Windows.** Every F# file then reports as
modified with an empty content diff, so `git status` cannot be used to see a lane's footprint and
`git merge` refuses to run until the tree is cleaned. Compare with `--ignore-cr-at-eol`.

---

# Item 1, reopened and settled

Lane AE's refusal was correct about the curried spelling and too broad as a conclusion. The
repository owner pointed out that a function type need not be curried - the parameters can be
tupled - and that a delegate can still be built wherever the function form cannot express the
signature. Lane AK measured that.

**Tupled is unsafe, and it fails more quietly than curried.** Three spellings, measured side by
side on `callback-function-lab` under Fable 5.2, reading `fn.length` and then whether the call
carried its arguments:

| Position | `Func`/`Action` | Curried | Tupled |
| --- | --- | --- | --- |
| parameter, arity 0/1 | ok | ok | same spelling as curried |
| parameter, arity 2/3; `ParamObject`; abbreviation | ok | ok | length 1, arguments read `undefined`, no throw |
| member read back, arity 2 | ok | length 1, arity lost | application passes one array |
| method return, arity 2/3 | ok | `TypeError` | application passes one array |
| `unit` return, arity 2 | ok | ok | length 1, arguments `NaN` |

Curried throws where it is wrong. Tupled returns wrong values and keeps going, which is the worse
failure for a consumer. Neither is safe above arity 1.

**What converted.** The arity-0 and arity-1 slice, where the curried and tupled spellings coincide
and both carry their argument. Lane AK also measured a rule neither earlier lane had: a function
type may not return a function type, because Fable flattens `A -> (B -> C)`, and this fails even
when both levels are unary. Delegates are retained above arity 1 and at nested returns, with
`TR055` naming the position.

Delegates fall from 1,615 to 365 counting `Action` and nesting - cloudflare 795 to 207, animejs 542
to 117, solid-js 271 to 41, type-fest 7 to 0. `TR055` reaches 330 symbols and is the only finding
that moved corpus-wide. Tiers 490/1525/783/193 to 479/1540/783/193.

So D5's default survives where it earns its place and yields where it does not. `D5a` in
`generator-type-mapping.md` carries the measured rule.

**Left for wave eight:** a function type inside `U2<...>` appears in the cloudflare golden and
compiles, but no run-gate check targets a union arm, so that position is emitted and unproven.
