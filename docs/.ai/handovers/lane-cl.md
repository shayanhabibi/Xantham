# Lane CL — TR031 (`CallbackOverloadsFromFirst`) recovery

Branch `worktree-gen-wave14-cl`, forked from `8d3a4fa`.

## The two raise sites

`TR031` fired 61 times at fork, split across two independent shaping paths that
both discard every call signature after the first on a pure-callback type:

- `Shape/Spec.fs`'s `delegateRef` — the callback reached at a **member or
  parameter position**. Here the surrounding declaration is an interface (or
  a parameter list), so a second signature can become a genuine method
  overload under the same member name: F# admits it.
- `Shape/Callbacks.fs`'s `delegateShapeFor`, feeding the named-declaration
  pass that emits `FsDelegateType` for `type X = { (...): T; (...): T }`.
  Here there is no enclosing member to overload onto — a delegate type holds
  exactly one signature. This is a genuine floor, not a gap.

Per-fixture count of the 61: animejs 34, solid-js 19, setter-lab 7,
intersection-callable-lab 1.

## The fix

`Spec.fs` gained `overloadParameterKey` and `callSignaturesSeparable`: a call
signature set is separable when its signatures are distinguishable by arity
and parameter types alone, since .NET/F# overload resolution never consults
return type. Member/parameter-position shaping (`delegateRef`, and the
member-shaping three-way match in `Spec.fs`) now emits every signature as a
same-name `FsMethod` overload when the set is separable, dropping `TR031`
entirely for that symbol. When it is *not* separable (same arity, same
parameter types, differing only in return type), the set collapses to one
key and raises the pre-declared, already-widened `TR062`
(`CallbackOverloadsNotSeparable`) instead of `TR031` — the finding now names
the actual reason nothing can be kept, rather than reusing the first-signature
message.

`Callbacks.fs`'s named-declaration path calls the same `callSignaturesSeparable`
helper for symmetry (so a separable named-callback also gets the sharper
`TR062` on the (still floor) cases where separability isn't the obstacle), but
its own floor is untouched: a bare `type X = delegate ...` still carries one
signature, because there is no member position to spread the rest onto. That
floor is deliberate and reported as such, not a residual bug.

### Ownership note: `Shape/Overloads.fs` (not in the original grant)

Making member-position overloads real (rather than widened-away) exposed a
latent, pre-existing bug in `dedupeOverloads`'s `signatureKey`: it distinguished
overloads by parameter type structurally, but two signatures whose only
difference was the *source-level name* of a type parameter (`'A` vs `'U`)
erase to an identical CLR signature and would have shipped as `FS0438` (two
members with the same name and signature). This was previously unreachable
because differently-typed overloads used to collapse to `obj` before
`dedupeOverloads` ever saw them. Fixed with `renameTypeVars` (positional
alpha-renaming of type parameters before the existing abbreviation
`normalize`), threaded into `signatureKey`'s three call sites. This is a
correctness fix, not scope creep — without it the setter-lab fixture would not
compile.

## Fixture

New `tests/fixtures/callback-overload-lab/` (`index.d.ts`, `package.json`),
registered in `Pipeline.test.fs` (`fixtureTests "callback-overload-lab"`).
Covers all four cells:
- `Holder.round` — arity-separable member overload: recovered, no finding.
- `Holder.parse` — same arity/types, differing only in return type: `TR062`.
- `Holder.measure?` — separable but optional: F#'s method form can't carry
  `?`, stays on the `TR031` floor (verified as the *only* `TR031`-keyed
  finding on that symbol; other unrelated findings on the same optional
  member are not asserted away).
- `Formatter` (named declaration, referenced from `Holder.formatter`) — the
  genuine named-declaration floor, still `TR031`.

Two pre-existing hand-written tests needed updating for the same reason (not
because they broke): `intersection-callable-lab`'s "a callable intersection at
a member position reaches its signatures (§4.6)" previously asserted the
pre-fix widened `Func<...>` output and a `TR050` loss finding for `Utils.round`;
that symbol is now fully recovered as two same-name overloads with **no**
finding at all, so the test was rewritten to assert the recovered shape.

## Reconciliation of the residual 33 (added after coordinator review)

The first pass of this handover reported 61→33 without accounting for every
residual site by mechanism. Reconciled below, per symbol, using a temporary
`eprintfn` probe on `effectiveTypeId`'s facts in `shapeMembers` (added,
inspected, and reverted — never committed) plus direct reads of the upstream
`.d.ts` sources.

**Genuine floor — no member/interface list exists at that exact position, so
no restructuring recovers it (17 of 33):**

- *Named-declaration target* (Callbacks.fs's already-documented floor,
  reached here through the generic alias/`typeRef` dispatch because the
  alias's RHS is an intersection or tuple rather than a literal
  `{ (a): T; (b): T }` object, so `Callbacks.fs`'s dedicated `FsDelegateType`
  pass never sees it): `AnimatableProperty` (animejs, 1 — RHS is
  `AnimatablePropertySetter & AnimatablePropertyGetter`, two different-arity
  function aliases intersected); `Setter` (solid-js, 1 — `Setter<T>` is a
  four-signature rank-2 alias, structurally identical to the lab's own
  `Setter`/`Distinct`/`DivergentBound`); `Setter`, `Distinct`, `DivergentBound`
  (setter-lab, 3 — the alias declarations themselves, independent of
  `Holder`'s member positions that reference them and *do* recover). Subtotal: 5.
- *Nested position* (a parameter, a tuple element, or a nested return type —
  a single-type slot with no enclosing member or alias at that exact
  recursion frame to spread onto, even when something further out happens to
  have a name): `Signal` (solid-js, 1 — `type Signal<T> = [get: Accessor<T>,
  set: Setter<T>]`, the tuple's second element); `From.Producer(setter)` /
  `From.Producer3(setter)` (solid-js, 2 — the `setter` parameter of
  `Producer<T>`'s function arm is itself `Setter<T>`); `*.storage()` (solid-js,
  6 — `storage?: (init) => [Accessor<...>, Setter<...>]`, the same tuple
  position, four times through `ResourceOptions`/`InitializedResourceOptions`
  and twice more through `createResource`'s per-overload instantiations);
  `createSignal()` (solid-js, 2 — one per exported overload, since
  `createSignal`'s return type is `Signal<T>`, hitting the same tuple
  position at the export's return type). Subtotal: 11.
- *Index-signature / `Record` value position* (`Record<'Key,'Value>` admits
  exactly one `'Value`, so an index signature can never host a sibling):
  `AnimatableObject.[]` (animejs, 1 — `Animatable & Record<string,
  AnimatableProperty>`). Subtotal: 1.

**Recoverable in principle, deliberately not built (16 of 33):**
`LayoutAnimationParams.{delay, duration, onBeforeUpdate, onBegin, onComplete,
onLoop, onPause, onUpdate}` and the same eight on `AutoLayoutParams` (which
re-derives from `LayoutAnimationParams`). Probed directly: at each of these,
`effectiveTypeId` resolves to a genuine `Intersection` type (TS's own
resolved type for a member repeated across intersection operands with
different declared types) whose *own* facts report zero call signatures,
zero members, zero index infos — `isPureCallback` correctly reads this as
"not a callback" at `shapeMembers`'s gate, so it falls through to the
generic, single-type-returning `typeRef` → `intersectionRef` path. Only
several calls deeper, inside `intersectionRef`'s own operand decomposition,
does it discover that each operand's declared union (`number | FunctionValue`
on one side, `TweenParamValue` — itself pulling in `EasingParam` and
therefore `EasingFunction` — on the other) contributes a distinct
function-shaped arm, and only then does it call `delegateRef` with those two
arms as separable signatures. By that point control is inside `typeRef`,
which returns one `FsTypeRef`, not a member list — there is no path back up
to `shapeMembers` to spread the two arms across two same-name `FsMethod`
declarations the way `Holder.round`'s literal case does.

The same same-name-overload mechanism that already recovers a literal
object-with-call-signatures at a member position would recover this too, if
`shapeMembers`'s early gate ran the same intersection/union-arm decomposition
`intersectionRef` performs before deciding property-vs-method-list, rather
than checking only the top-level facts. That is a restructuring of
`shapeMembers`'s gate (in `Spec.fs`, not blocked by file ownership — this is
not a cross-file limitation), not a small extension, and it is explicitly
**not built here**: it changes the shape-decision entry point for every
interface member, not just callback-typed ones, and needs its own scoped
pass to avoid disturbing unrelated recovered/widened members. Stopping point
for whoever picks this up: `shapeMembers`'s `recoverableCallbackFacts` check
in `Spec.fs`, which would need to attempt `intersectionRef`'s operand-arm
extraction (or a shared helper) before its `isPureCallback` gate rather than
only inspecting `effectiveTypeId`'s own facts.

5 + 11 + 1 + 16 = 33. Confirmed exhaustive against the symbol-level TR031
listing in both goldens' `symbols.jsonl`.

## Corpus movement (findings, `dotnet fsi build.fsx -- findings`)

Tiers, corpus-wide: exact 535→535, ergonomic 1603→1604, widened 797→798,
escape 200→200 (baseline confirmed exact match before my changes).

`TR031`, corpus fixtures only (excludes the new lab, which is intentional new
content, not movement): 61→33 — **28 findings fully resolved** into real
method overloads, corpus-wide, with zero cases needing `TR062` in existing
fixtures (`TR062` only appears in the new lab's `Holder.parse`, count 1).
Per fixture: animejs 34→18, solid-js 19→12, setter-lab 7→3,
intersection-callable-lab 1→0.

Gross tier movement, reported per-symbol rather than netted: despite 28
`TR031` findings disappearing, **animejs and solid-js's symbol-level tier
counts did not move** (both fixtures' exact/ergonomic/widened/escape are
bit-for-bit identical before and after) — every affected symbol there already
carries an independent `TR032` (or similar) finding that floors it at
`widened`/`escape` regardless, so removing `TR031` cleaned up the finding list
without changing the published tier. Likewise setter-lab's fixture-level tiers
are unchanged (`0/0/4/1`) even though its `SI - shape-interfaces` pass
manifest moved 11 of 14 findings from `widened` to `ergonomic` internally (see
`git diff` on `golden/setter-lab/manifest.json`) — `TP002` on the same symbols
already floors them. The only tier change that reached a published symbol
grade is `intersection-callable-lab`'s `Utils.round`: ergonomic 1→2, widened
2→1 (that member moving from widened-`Func<...>` to two ergonomic overloads).
The new `callback-overload-lab` fixture itself contributes 2 new `widened`
symbols (`Formatter`, `Holder`) — new content, not movement. These two deltas
(+1 ergonomic, -1 widened from the intersection fixture; +2 widened from the
new lab, net +1) fully reconcile the corpus-wide +1/+1 seen in the tier
totals above.

**Declaration removed, stated explicitly per the brief's requirement**:
`setter-lab`'s `Holder` type lost its synthesized `[<ParamObject; Emit("$0")>]
static member Create` and its `[<Interface>]` attribute. Previously every
member widened uniformly to `obj -> obj`, which made all constructor
arguments identical and let `ParamObject` synthesis fire; now that members
correctly retain their per-member generic overloads, the members are no
longer identical and `ParamObject` synthesis correctly declines
(`SP003`/`CreateNotSynthesized`, +10, up from `SP001`/`SP002` elsewhere in
that fixture) rather than emitting a `Create` that would not compile. This is
a finding reattaching elsewhere, not a silent loss.

Other knock-on finding deltas, all downstream of the same recovery (see
`/tmp/cl_findings_before.txt` / `after2.txt` for raw fixture blocks, not
committed): `DO001` +4, `TP002` +38, `TP006` +8, `TR046` +14 (new overload
sets exercise arity/type-param reporting paths that previously never ran
because the callback was pre-widened away); `SP001` -9, `TR013` -7, `TR050`
-14, `TR055` -14 (fewer widened callbacks means fewer things needing those
downstream findings).

## Verification

- `dotnet build src/Xantham.Generator/Xantham.Generator.fsproj` — 0 errors.
- `dotnet fsi build.fsx -- test --quick --no-run-gate` — Xantham.Generator.Tests:
  **496/496 passed** (490 baseline + 6 new `callback-overload-lab` cases),
  Xantham.TypeScript.Wire.Tests: 90 passed, 1 skipped (unchanged).
- `dotnet fsi build.fsx -- test --quick` (run gate included) — **309 checks**,
  unchanged from baseline, exit 0. No run-gate additions made: the recovered
  behavior (method-overload shaping) is already exercised by the compile gate
  and the e2e golden tests; there is no new runtime behavior distinct from
  what the existing probes cover.
- `git diff --stat` over regenerated goldens: 12 files changed (animejs,
  solid-js, setter-lab, intersection-callable-lab), plus the new
  callback-overload-lab golden (3 files, untracked→added).

## Unexplained / flagged for follow-up

- Filtered `dotnet fsi build.fsx -- test --quick --filter <fixture>` runs
  during development consistently reported `Total: 2` for fixtures whose
  `fixtureTests` block registers 4+ `testCase`s. Running the full unfiltered
  suite reports the correct count (496, including all of the new fixture's
  cases) and correctly surfaces real failures (both an `FS0438` compile bug
  and two genuine test-content mismatches were caught this way), so this
  looks like a display/counting quirk in the Microsoft.Testing.Platform +
  Expecto integration under `--filter`, not a skipped-test problem. Not
  chased further; flagged for whoever next needs `--filter` to report
  trustworthy per-fixture counts.

## Files touched

- `src/Xantham.Generator/Shape/Spec.fs` — `overloadParameterKey`,
  `callSignaturesSeparable`, `delegateRef`, member-shaping three-way match.
- `src/Xantham.Generator/Shape/Callbacks.fs` — `delegateShapeFor`'s
  `overloadFindings` branch.
- `src/Xantham.Generator/Shape/Overloads.fs` — `renameTypeVars`,
  `signatureKey` signature change and its three call sites (not in the
  original ownership grant; a necessary correctness fix, see above).
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` — new
  `callback-overload-lab` fixture registration and tests; two pre-existing
  intersection-callable-lab / callback-overload-lab tests updated to match
  recovered output.
- `tests/fixtures/callback-overload-lab/{index.d.ts,package.json}` — new.
- `tests/Xantham.Generator.Tests/golden/callback-overload-lab/` — new.
- `tests/Xantham.Generator.Tests/golden/{animejs,solid-js,setter-lab,intersection-callable-lab}/`
  — regenerated.
- `Findings.fs` — not edited (`TR062` was already declared).
