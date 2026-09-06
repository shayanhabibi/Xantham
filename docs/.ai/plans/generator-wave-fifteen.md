---
category: Generator
audience: managing agent
title: Worklist - generator wave fifteen
integration-branch: worktree-generator-wave-fifteen
---

# Generator wave fifteen — the shipped compiler-lib package

An inventory, not a plan. Wave fourteen's record is
`docs/.ai/plans/generator-wave-fourteen-dispatch.md`; read its Outcomes and Carried forward
sections before pricing anything here.

Read `.claude/rules/generator-fixtures.md` first.

**This wave is directed rather than measured.** Waves eleven through fourteen chose their items
from residue counts. This one executes answers the user wrote into
`docs/.ai/fable-utility-types.md` and `docs/.ai/fable-binding-gaps.md`, confirmed point by point
before dispatch. Where a residue count appears below it sizes a decision already taken; it does
not reopen it.

## Baselines, measured on the integration branch at item zero

`worktree-generator-wave-fifteen` forks `master` `fd2a10b`. Item zero is `2e2e9b2`.

Gate: **522 generator tests, run gate 323 checks**, wire unchanged from wave fourteen's 90 (1
skipped by design under `XANTHAM_TSGO_EXE`). `build.fsx -- test` exits 0 and leaves the tree
clean.

Corpus, all 55 fixtures: `exact 543, ergonomic 1623, widened 793, escape 206` over 3,165 symbols
and 17,121 findings — identical to wave fourteen's close, because item zero changed spelling
only.

Residues from the committed `symbols.jsonl`:

`TR032` 5174, `MB003` 3827, `SP001` 1751, `SY004` 729, `MB001` 662, `SP002` 662, `TR008` 575,
`TP002` 373, `TR055` 308, `TR006` 304, `TR059` 223, `TR009` 219, `SA002` 197, `TR045` 185,
`TR024` 147, `SP003` 144, `TR023` 136, `SI003` 119, `SI004` 89, `SI005` 81, `SY006` 80,
`TR046` 79, `MB004` 63, `TR049` 58, `TR018` 53, `TR020` 53, `TR040` 48, `MB006` 43, `TR031` 35,
`TR056` 34, `SI006` 31, `TR050` 30, `TR047` 29, `RA003` 29, `TP008` 29, `TR025` 27, `TR035` 26,
`DT002` 26, then a tail below 26.

## Item zero, landed before dispatch

**The support package shadows into `Fable.Core.JS`.** `master` `fd2a10b` consolidated `Brand.fs`
and `Record.fs` into one `[<AutoOpen>]` module and dropped the `Xantham.Fable.Core` namespace;
nothing downstream followed, so every golden failed the compile gate. Landed at `2e2e9b2`:

- The support module is `Fable.Core.JS.JS`. The emitted header opens `Fable.Core.JS` and no
  longer opens `Xantham.Fable.Core`; `Record` and `ReadonlyRecord` are referenced bare.
  **Generated output carries no Xantham-named token.**
- `JS.Record` and Fable's own `JS.Promise` coexist under `open Fable.Core.JS` — verified against
  the compiler, because F# merges the shipped module with the namespace this assembly declares.
- `Naming.SupportBindings` names what the support package shadows. A package declaring one of
  those names takes the unqualified spelling and the support package's is reached as
  `JS.<name>`, repaired in `repair-arity` rather than widened to `obj`. `record-index-lab`
  exports its own `interface Record` and gates this.

**`SupportBindings` lists six names today and is the seam every item below widens.**

## The answers this wave executes

1. Shadowing is the delivery mechanism. Anything Fable ought to ship goes into
   `Xantham.Fable.Core` or `Xantham.Fable.Browser`, shadowed into `Fable.Core`,
   `Fable.Core.JsInterop`, `Fable.Core.JS` or `Fable.Browser.*` — never a `Xantham.*` namespace.
   Recorded as mishandled repeatedly; it is checked at merge rather than trusted to a lane.
2. We build our own Fable.Browser. One corpus in this repository generates the library bindings,
   we publish them, and every user generation binds against those.
3. The whole `lib.dom` and `lib.es*` surface, not the subset the corpus references today.
4. DOM in a new `src/Xantham.Fable.Browser`; `lib.es*` names in `Xantham.Fable.Core`.
5. Upstream `Fable.Browser.*` package references go away. Where our generation loses information
   against upstream's hand-written binding we copy upstream's code — MIT, so it needs attribution.
6. Union arity above nine is generated inline at the bottom of the file that needs it.
   **Not to be priced and not to be proven at the run gate.**
7. `NoInfer` gets a toggle in `xantham.json`, defaulting to emitting `NoInfer<T>`.

## The items

### 1. The shipped compiler-lib package

`GroupDisposition.Widen` is documented in `Model.fs` as the default for non-entry groups **until
the shipped compiler-lib package exists**. O7 left this hole deliberately. This item fills it,
and it is the wave.

Every other item on this list is smaller than this one and several are blocked behind it.

**1a. Recon: what does the generator produce when pointed at `typescript/lib`?** The generator
has never run with the compiler lib as an entry package. `lib.dom.d.ts` alone is an order of
magnitude larger than any fixture, the declarations are mutually recursive across files, and the
`Lib` config exists to *exclude* these names rather than to ship them. Unknowns that price
everything downstream: emitted size, whether grouping holds across the `lib.*.d.ts` set, whether
the naming walk terminates, and how long a run takes. **Nothing else in this item can be priced
before this is measured.**

**1b. The ECMAScript half into `Xantham.Fable.Core`, shadowing `Fable.Core.JS`.** Closes
`fable-binding-gaps.md` entries 1-4, answered "generate or hand roll our own bindings and ship
them with Xantham.Fable.Core". Named causes measured today: iteration protocol 29 sites
(`IterableIterator` 20, `Iterable` 9), typed arrays 5 (`BigUint64Array` 3, `Float16Array` 2).
`Fable.Core.JS` already binds the async half, so the shadow sits beside it rather than replacing
it.

**1c. The DOM half into `src/Xantham.Fable.Browser`, shadowing `Fable.Browser.*`.** Upstream's
naming scheme is preserved. Families measured today inside `TR023`'s 136: DOM geometry 20, DOM
collections and traversal 15, events and listeners 14, Web Animations 13, element options records
10, File System Access 3, CSS Typed OM 2, WebGPU 2, media 2.

**1d. `tools/browser-gen` rewritten.** Its whole current job is intersecting the pinned
`Fable.Browser.*` packages with the pinned compiler's `lib.*.d.ts` to emit a 439-entry table plus
the gate file that proves it. Under answer 5 there is nothing to intersect against. The table
becomes a table over names we emit, and **its generated gate proves less than today's until the
new project is itself gated** — the gate currently earns its authority from a package we did not
write. Say what replaces that authority before removing it.

**1e. `SupportBindings` grows to the whole shadowed surface.** Item zero's collision rule is
correct for six names. At several hundred it decides how a package that declares `Element`,
`Event` or `Response` is emitted — `@cloudflare/workers-types` declares many such names, which is
exactly why its `xantham.json` sets `Lib`. The rule may need to become the reserved-name rule
item zero rejected as too large to decide on two names. **Price this against the real name set,
not against the six.**

**1f. `typescript/lib` moves off `Widen`.** The disposition, the config surface that selects it,
and what a consumer's `xantham.json` says to bind against the shipped package.

### 2. A bound name that takes fewer type parameters than TypeScript gives it

`TR024`, graded ergonomic. **147 findings measured, against the 70 the utility-types document
enumerated** — `ArrayBufferView<TArrayBuffer>` 63, `DataView<TArrayBuffer>` 2, the
`AsyncIterable`/`AsyncIterator`/`AsyncIterableIterator` family 4, `ProgressEvent<T>` 1. The other
77 are unattributed and are the first thing a lane here measures; the distribution is
`@cloudflare/workers-types` 118, `type-fest` 21, `lib-lab` 7, `animejs` 1.

The answer: F# admits two types of the same name at different arities, so ship the arities
TypeScript declares and drop only typars that serve no purpose. **Implemented the same way as
items 1b and 1c**, stated explicitly in the answer, so this is not independent of item 1.

### 3. Erased-union arity above nine, emitted inline

`TR036`, 6 findings: `animejs` 4, `@cloudflare/workers-types` 1, `indexed-access-lab` 1. A file
whose package needs arity 12 carries a `U12` at its footer. The arity-9 cap is `ErasedUnionArity`
at `Shape/Spec.fs:2127`.

**Directive, quoted: do it, do not price it, and do not test erased-cast semantics. "If it does
not work, it is cheap for me to tell you to get rid of it. It is expensive to have a team cycling
on it."** Wave thirteen's recorded proof obligation is explicitly waived. Do not spend a lane
proving erasure.

### 4. `NoInfer` toggle in `xantham.json`

`GeneratorConfig` gains a field defaulting to emitting `NoInfer<T>`; set, the generator resolves
`NoInfer<T>` to `T` at the mapping site. `type NoInfer<'T> = 'T` already exists in the support
package. The generator has no `NoInfer` handling at all today. `--only schema` re-emits
`xantham.schema.json` from the record. `solid-js` is the whole corpus incidence: the idiom drives
about 20 of its `TR045` sites and about 19 of its 22 `TR020` sites.

## What not to chase

The first four are the user's own declines, recorded so they stop being re-raised.

- **`TR032` 5174 and `MB003` 3827.** `null`, `undefined` and an absent property are one F# value.
  Known limitations; the finding stays as it is and no work is assigned to resolving something
  that cannot be resolved.
- **Structural equality on a generated interface.** Same answer.
- **Erased-union arm selection, and the downcast to a generated interface** (`SI005` 81). A known
  limitation of erased unions, not ours to resolve. Helpers may come later.
- **`SA002` 197 and `TR045` 185.** Phantom computation and deferred conditionals, §4.11, 382
  findings and the largest single block in the utility-types document. Declined by waves twelve,
  thirteen and fourteen; the premise was not contested, and `NoInfer`, `Record` and the measures
  were carved out by hand instead. **The only thing that would reopen it is challenging the
  premise that F# cannot defer a type — not any Fable capability.**
- **`TR009` 219.** `unknown` maps to `obj` because that is what `unknown` is. D8.
- **`TR023` 136 as a mapping item.** Subsumed by item 1; it is no longer a question about the
  mapping but about which package ships the name.
- **Uncapping erased-union arity in `Fable.Core`.** Replaced by item 3. `U10`-`U100` in the
  support assembly was rejected in wave thirteen as D4 and stays rejected; the footer emission is
  per-file and pays only where a package needs it.
- **`TR008` 575.** Verified a floor by wave fourteen's lane CJ, all 575 attributed across eight
  clusters; 295 are one `animejs` alias whose return the library discards.
- **`TR006` 304.** Wave fourteen's lane CK found its literal-union half does not exist.

## Carried to wave sixteen

Wave fourteen's six, deferred again by decision rather than by oversight — item zero rewrites the
emitted header in all 55 goldens and item 1 will move a large number of `obj`s to real names, so
any lane taking these would have its goldens regenerated and reviewed twice.

1. `Invoke` lifts lane CL's named-declaration floor, which neither CL nor CR could see alone.
   Real trade: `type X = Func<...>` accepts a lambda literal and an `Invoke` interface does not.
2. Lane CL's 16 recoverable callback sites — `animejs`'s `LayoutAnimationParams` and
   `AutoLayoutParams`; restructures the shape-decision entry point.
3. A named alias whose whole definition is one string literal — `type DurableObjectRoutingMode =
   "primary-only"` widens to `string`. Hidden inside lane CK's bucket A of 271.
4. A discarded callback return reads `obj` rather than `unit`, 295 `animejs` sites. Ergonomics
   rather than fidelity.
5. The naming walk renamed two `@cloudflare` symbols. Deterministic, not unstable;
   `Shape/Ordering.fs` runs after naming, so pinning it is a collision-policy redesign — and
   item 1e may force that redesign first.
6. `unionRef`'s self-name `ConcreteBranch` (`Spec.fs:1946`).

## Clauses that change how this wave is dispatched

Wave fourteen's hold. Five are restated because each was paid for:

- **Put the change first in a brief and subordinate the analysis to it.** Wave fourteen's batch
  one led with the change and needed no second instruction on any of eight lanes; batch two led
  with "establish and report" and all three lanes stopped once the analysis was written, having
  built nothing. One of them, re-sent, overshot and committed to the integration branch.
- **Report the gross, not the net, with per-symbol attribution of every tier movement.** Lane CH
  in wave thirteen and lane CN in wave fourteen both netted a movement away and both were caught
  at merge rather than by the lane.
- **A tier column can rise because the mapping improved.** When a minted declaration stops being
  minted, the findings it owned reattach to every referencing site. Any lane that removes
  declarations says so explicitly. **Item 1 will do this at a scale no previous wave has seen.**
- **A completion notification is not evidence of completion.** Check that a lane branch carries
  commits before believing the lane landed.
- **A lane commits on its own branch.** Wave fourteen's lane CN committed to the integration
  branch and shipped no handover until asked.

Unchanged and non-negotiable: goldens are regenerated, never hand-merged. Batches are gated as
batches. The full gate, never `--quick` — wave fourteen let an unformatted file reach the
integration branch that way. Merge messages are written to a file and applied with
`git commit -F`. **Never merge into `master`** — the integration branch is handed to the user.

**Model constraint.** `sonnet` and `haiku` are the only guaranteed-compliant values of the Agent
tool's `model` enum: `opus` resolves to the parent session's context variant, and where the parent
runs a 1M-context model that breaches the standing constraint that no teammate exceeds Opus 5 on a
standard context window. Consulting Fable 5.1 requires the user's permission and a stated reason.

## Shape of the wave, if it helps

Item 1a gates 1b through 1f and there is no way around it: the generator has never been pointed at
the compiler lib, and every estimate downstream is a guess until it has been. Run it alone and
early, read it before briefing the rest.

Items 3 and 4 touch neither `Xantham.Fable.Core` nor the browser work and are small and
well-specified. They can run alongside 1a rather than waiting for it.

Item 2 is stated as sharing an implementation with 1b and 1c, so it is not a separate lane — it is
a property those lanes must carry.

Item 1e is the one place this wave can force a redesign it did not intend to buy. Measure the real
shadowed-name set against what the corpus declares before choosing between the collision rule item
zero shipped and the reserved-name rule it rejected.
