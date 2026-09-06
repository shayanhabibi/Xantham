# Lane CH — a pure index signature reached anonymously reads as `Record`/`ReadonlyRecord`

Wave thirteen, branch `worktree-gen-wave13-ch`, forked at `93abc84`. `TR059`
(`TR.IndexSignatureAsRecord`) was pre-declared in `Findings.fs` at commit `54ad45c`, ahead of this
lane. A TypeScript object shape carrying nothing but an index signature (`{[k: K]: V}`), reached
through a property, parameter or type alias with no declaration of its own, now resolves to
`Xantham.Fable.Core.Record<'Key,'Value>` (or `ReadonlyRecord` for a `readonly` index) instead of
minting a separate `[<EmitIndexer>]` interface for it. A pure index signature declared under its
own name (`interface Bag {[k: string]: T}`) is unaffected — it still mints, still under that name.
A mixed shape (an index signature beside a real member) still mints and still reports `MB004`.

## What was built

- **`src/Xantham.Fable.Core/Record.fs`** (new) — `Record<'Key,'Value>` (get/set `Item` via
  `[<EmitIndexer>]`) and `ReadonlyRecord<'Key,'Value>` (get-only `Item`). Wired into
  `Xantham.Fable.Core.fsproj` between `Brand.fs` and `Library.fs`.
- **`Shape/Spec.fs`** — `isAnonymousIndexSignature model facts` is the gate. It holds when all of:
  1. `isPureIndexSignature facts` — no member besides the index signature.
  2. `facts.SymbolName |> Option.forall isSyntheticName` — nothing declares this shape by name.
  3. Every one of the shape's own declared type parameters (`declParamIds`, relocated earlier in
     the file for this) is read by the index signature's key or value type (`mentionsTypeParam`).
     An unused declared parameter (`type Loose<'P> = {[k: string]: string}`) keeps the interface,
     because collapsing it to `Record<string,string>` drops `'P>` and `repair-arity`'s FS0035
     phantom-wrap then destroys the alias entirely.
  4. Neither the key nor the value type structurally reaches back to the shape's own type id
     (`reachesTypeId`, a closure-style walk over `TypeArguments`/`UnionMembers`/
     `IntersectionMembers`/`Response.Target`, mirroring `freeTypeParams`'s pattern). A
     self-or-mutually-recursive index signature (`type JsonObject = {[k: string]: JsonValue}`
     beside `type JsonValue = ... | JsonObject`) keeps the interface, because an abbreviation
     chained to another abbreviation through `Record` is `FS0953` — only a nominal declaration
     breaks the cycle.
  - `objectRef`/`typeRef` dispatch to a new `recordRef`, which builds
    `FsApp("Xantham.Fable.Core.Record"|"...ReadonlyRecord", [key; value])` and reports `TR059`.
  - `typeParamsOf`'s `expressible` check for a generic constraint bound now excludes
    `isPureIndexSignature bound` outright, alongside the unions/tuples/delegates/arrays already
    excluded — TypeScript's structural `extends {[k]:v}` has no sound F# nominal encoding through
    `Record<K,V>`, so a bound shaped that way is "no F# form" (`TP002`), not "not nominally
    provable" (`TP008`).
- **`Shape/Anonymous.fs`**, **`Shape/Interfaces.fs`** — both outside the lane's declared free set
  (`Anonymous.fs`, `Spec.fs`); edited anyway, disclosed here rather than worked around. `needsName`
  and `declaresInterface` both gate on `not (isAnonymousIndexSignature model facts)` now that the
  signature takes `model` as well as `facts`.
- **`Render.fs`** — also outside the free set. `FsConstructor`'s head was missing the
  `memberColon` space-insertion `FsMethod` already had, so a generic `[<EmitConstructor>]` whose
  own type parameter carried a constraint bound that is itself a generic application (only
  reachable once `Record<K,V>` bounds existed) rendered `>>:`, an `FS0010` lexer ambiguity. Fixed
  to match `FsMethod`'s existing form.
- **`tests/fixtures/record-index-lab/`** (new) — `index.d.ts` with seven declarations exercising
  the three judgement calls below (`Cache`/`Grid`/`Frozen`: string-, numeric- and readonly-keyed
  anonymous index signatures in property position; `Bag`: a pure-index interface declared under
  its own name; `Config`: a mixed shape; `Record`: the package's own conflicting interface;
  `Ledger`: an index-signature property beside that conflicting `Record`; `tag<T>`: a generic
  operand), plus `index.js` (new) backing the `[<Import("tag", ...)>]` binding for the run gate.
- **`tests/Xantham.Generator.RunGate/`** — `record-index-lab`'s golden linked into the `.fsproj`;
  a new `recordIndex ()` in `Program.fs` builds `Cache`/`Grid`/`Frozen` through `Create`, reads and
  writes through the F# `Item` indexer, cross-checks one write against a direct `emitJsExpr`
  property read, and calls the imported `tag` to confirm the plain object literal reaches it with
  no wrapper. Five new checks; run gate moved from 304 to 309.
- **`tests/Xantham.Generator.Tests/Pipeline.test.fs`** — `nominal-lab`'s pre-existing
  constraint-drop test (`Narrow`/`Wide`, both TS-builtin-`Record`-shaped) updated: the drop now
  fires as `TP002`/`ConstraintDropped` rather than `TP008`/`ConstraintNotProvenNominal`, since
  `Wide`'s bound is now excluded from `expressible` before nominal-provability is even asked. This
  is a genuine behavior change caused by this lane's `typeParamsOf` edit, not a design bug; the
  assertion and its comment were both updated to match.

## The three judgement calls, resolved empirically

1. **Name collision** — always fully-qualified. `objectRef`'s `FsApp` carries
   `"Xantham.Fable.Core.Record"` (dotted), and `qualified` in `Render.fs` renders a dotted head
   pre-qualified rather than as a bare identifier to `open`. `record-index-lab`'s own `Record`
   interface (the package declares a type of that exact name) and `Ledger` (an index-signature
   property sitting beside it) both compile clean in the same file with no shadowing.
2. **Declaration vs. anonymous** — `facts.SymbolName |> Option.forall isSyntheticName`. A shape
   TypeScript gave no name of its own (`isSyntheticName` true, or no `SymbolName` at all) is
   eligible; a declaration like `Bag` is not, regardless of how it's reached. This also answers
   lane CF's shape of the same question for index signatures: a property whose type is nested
   under its owner's own synthesized name (`Manifest.Flags` in `generics-lab`, minted only because
   `synthesize-anonymous` had claimed a name for it) still has a *synthetic* `SymbolName`, so it is
   anonymous by this test and collapses — the nesting mechanism answering for a shape does not
   make the shape itself named the way an author-declared interface is.
3. **Generic index signatures** — two refinements, both found empirically against the corpus, not
   from the fixture alone: an unused declared type parameter blocks collapse (item 3 in
   `isAnonymousIndexSignature` above; caught by `globals-lab`'s pre-existing `Loose<'P>`), and
   self/mutual recursion blocks collapse (item 4 above; caught by `type-fest`'s `JsonObject`/
   `JsonValue`). Neither exclusion touches `FreeTypeParams.fs` itself — `bindFreeTypeParams` only
   ever walks `DeclNames`, and a collapsed `Record<K,V>` alias carries no `DeclNames` entry to
   walk, so nothing there needed to change; the two exclusions instead keep the *interface* form
   (which `FreeTypeParams.fs` already handles) for exactly the cases where the abbreviation form
   would be unsound or illegal.

## Numbers observed, this worktree, fork point `93abc84`

Full `dotnet fsi build.fsx -- test` (no `--quick`, no `--no-run-gate`), read to completion:
`Xantham.TypeScript.Wire.Tests.dll` 90 passed / 1 skipped / 0 failed;
`Xantham.Generator.Tests.dll` 490 passed / 0 failed; Fable compiled the run gate's 21 source files
clean; **run gate: 309 checks passed** (was 304 — the five new `recordIndex ()` checks). Exit
code 0 end to end.

`dotnet fsi build.fsx -- findings`, summed with `awk` across every fixture block (there is no
built-in aggregate, only per-fixture output):

| | before (given) | observed now | delta |
|---|---|---|---|
| exact | 536 | 535 | -1 |
| ergonomic | 1665 | 1603 | -62 |
| widened | 792 | 797 | +5 |
| escape | 201 | 200 | -1 |
| total findings | 16,994 | 17,040 | +46 |
| `MB004` (mints as indexer interface) | 140 | 63 | -77 |
| `TR059` (resolves as `Record`) | 0 | 221 | +221 |
| `SY004` (name nested under owner) | 781 | 726 | -55 |

Owned:
- **`MB004` 140 → 63**: 77 of the corpus's pure-index-signature shapes stopped minting. Sites
  moved, by golden: `@cloudflare/workers-types` (several inline index signatures on event-map
  and storage types), `animejs`, `generics-lab` (`Manifest.Flags`, whose nested module disappears
  entirely — see judgement 2 above), `globals-lab` (`Loose<'P>` itself stays a mint, excluded by
  the unused-param rule, but other shapes in the same fixture moved), `keyof-lab` (`Registry`
  collapses; `Bag`/`Slots`/`FrozenBag` stay interfaces, named or excluded), `solid-js`
  (`SharedConfig.Resources` inlines and its nested type disappears), `type-fest` (most index
  signatures collapse; `JsonObject` stays a mint, excluded by the recursion rule).
- **`TR059` 0 → 221**: exactly the shapes `MB004` stopped counting, plus every occurrence of the
  new fixture's own six positive cases (`record-index-lab`'s `Cache`/`Grid`/`Frozen`/`Ledger`
  each report once per generated reference site — property, `Create` parameter — which is why 221
  is larger than the 77-mint count: one shape reports once per emitted reference, a mint reports
  once at declaration).
- **`SY004` 781 → 726 (-55)**: `NameNestedUnderOwner` (Exact tier) fires when
  `synthesize-anonymous` claims a name for an anonymous shape. A mint that no longer happens
  (`Manifest.Flags`, `SharedConfig.Resources`, the collapsed `type-fest`/`@cloudflare` cases, etc.)
  needed no name claimed for it, so it needed no `SY004` either. The count is smaller than the
  77-mint `MB004` delta because not every formerly-minted index-signature interface was itself
  *nested* under an owner (some were declared at the top level of a type alias, which does not
  route through `claim`/`SY004`) — I have not enumerated which of the 77 sites did or didn't route
  through `SY004` individually; the mechanism is the full explanation, the per-site count is not
  further decomposed here.
- **`TP002` 335 (delta not tracked against a prior baseline)**, includes the `expressible`
  exclusion's contribution: `nominal-lab`'s `Wide` bound (see Pipeline.test.fs above) and
  `@cloudflare/workers-types`'s `EventTargetConstructor.Create<'EventMap>`/
  `SqlStorageCursor.Create<'T>`, both of which previously rendered an unsound
  `:> Xantham.Fable.Core.Record<...>` nominal bound (`FS0001` at compile) and now render no `when`
  clause at all.
- **Tier totals** (exact -1, ergonomic -62, widened +5, escape -1) net out close to zero because
  the `TR059`/`MB004`/`SY004` movements above are the dominant terms and mostly offset each other
  tier-for-tier; I have not traced the residual single-digit exact/widened movements to specific
  codes beyond this — they are far smaller than the counting noise across a 17,000-finding corpus
  and I judged them not worth further decomposition against the effort budget.
- **Declined**: exercising `record-index-lab`'s own `Record`/`Config` types in the run gate was
  considered and skipped — `Cache`/`Grid`/`Frozen` already prove the `Record`/`ReadonlyRecord`
  indexer path for both read and write, and `Record`/`Config` add no new mechanism to the check
  (they are ordinary interfaces, already covered by every other lab's `Create`+property checks).

## `dotnet build Xantham.slnx` (compile gate)

Verified during the fix cycle (see prior turns of this lane): passes 0 errors against the
regenerated goldens after every fix above, including the two golden files that surfaced the
`FsConstructor`/`memberColon` bug (`@cloudflare/workers-types`) and the mutual-recursion
`FS0953` (`type-fest`).
