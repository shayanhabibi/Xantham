# Lane CR — a callable-and-properties hybrid reaches its call signature through `Invoke`

Wave fourteen, branch `worktree-gen-wave14-cr`, forked at `5c827c4`. `SI008`
(`SI.HybridCallSignaturesAsInvoke`) was pre-declared in `Findings.fs` ahead of this lane. A hybrid
(an object, interface or class that is both directly invocable and carries properties) previously
lost its call signatures entirely (`SI001`). It now mints an `Invoke` member tagged
`[<Emit("$0($1...)")>]`, so `x.Invoke(a)` compiles to `x(a)` — mirroring `ConstructorObjects.fs`'s
`Create`/`[<EmitConstructor>]` for the construct side. `SI001` survives only when the hybrid
already declares its own member named `Invoke`.

## What was built

- **`Model.fs`** — new `FsInvoke of FsConstructorMember` case on `FsMember`, reusing the record
  shape `FsConstructor` already has.
- **`Shape/Interfaces.fs`** — where the pass previously raised `HybridLosesCallSignatures`
  unconditionally, it now shapes each of `facts.CallSignatures` into an `FsInvoke` member (via the
  existing `shapeSignature`) and appends them to `members`; it raises `SI008` when none of the
  already-shaped members is literally named `Invoke`, `SI001` otherwise (the emitted member is
  withheld in that case). One code path covers both a hybrid reached at a member position and one
  reached as a named declaration — both funnel through `declaresInterface`/this pass once a
  `DeclNames` entry exists.
- **`Render.fs`, `Shape/Arity.fs`, `Shape/Classes.fs`, `Shape/Overloads.fs`, `Shape/ParamObjects.fs`**
  — exhaustiveness and integration for the new DU case: rendering (`[<Emit("$0($1...)")>]` +
  `abstract Invoke: ...`), qualification, arity repair, `dedupe-overloads` (an `Invoke` arm keyed
  like `Create`'s, so two non-separating call signatures collide down to one survivor plus a
  `DO001`), and `ParamObjects` (an `Invoke` member is excluded from `Create`'s parameter binding —
  it doesn't share `Create`'s name, so full refusal isn't needed, only exclusion from the bound
  member list). `Classes.fs` needed only the two exhaustiveness arms: TypeScript's `class` syntax
  has no call-signature form, so a class itself never needs `Invoke` handling.
- **`tests/fixtures/callable-hybrid-lab/`** (new) — member position (`Widget.Handler`), named
  declaration (`Trigger`), two-signature overload (`Multi`), non-separating overload (`Ambiguous`,
  proves `dedupe-overloads` already handles this generically), a call signature with its own type
  parameter (`Identity`), a generic interface whose call signature reads the interface's own type
  parameter (`Boxed<T>`), and the negative (`Collides`, a member already named `Invoke`, stays
  `SI001`). `index.js` backs a real runtime.
- **`tests/Xantham.Generator.RunGate/`** — `callable-hybrid-lab`'s golden linked into the
  `.fsproj`; a new `callableHybrids ()` in `Program.fs` calls `widget.handler.Invoke "ping"` and
  gets `"handled:ping"` back from the real JS function — the one value in the lab reached at a
  member position, so the one case that still exposes an explicit `.Invoke` to call through. Every
  other export in the lab is itself the callable value, so the pre-existing `Exports` pass renders
  it as a direct static function rather than an object carrying `Invoke` (confirmed pre-existing
  and unrelated to this pass, not chased). Nine new checks; run gate moved from 309 to 318.
- **`Pipeline.test.fs`, `Shape.test.fs`** — new fixture tests for the lab (member position, named
  declaration, multi-arity, dedupe, both generic cases, the collision negative), plus the
  pre-existing `intersection-callable-lab` pinned test updated to assert `SI008` where it
  previously asserted `SI001` for `Timers.Schedule`, and exhaustive-match fixes for the new DU case.

## Decisions

- **Multiple call signatures**: no new machinery — `Invoke` overloads by arity work the same way
  `Create` overloads already do (`Multi`).
- **Non-separating overloads**: no new refusal logic — the existing `dedupe-overloads` pass already
  drops a colliding `Invoke` overload and reports `DO001`, exactly as it does for any other member
  (`Ambiguous.Invoke`).
- **Generic call signatures**: both sub-cases work unmodified through `shapeSignature`. A call
  signature's own type parameter on a non-generic interface produces `Invoke<'T>`. A generic
  interface's call signature reading its own type parameter (already threaded via `TypeVars`)
  produces `Invoke: unit -> 'T`. Neither triggers `Anonymous.fs`'s "no own type parameter" naming
  restriction — that restriction is specific to the `isPureCallback`/delegate route (a type alias
  can't be generic in that direction); `Invoke` is an abstract member, which can.
- **Member position vs. named declaration**: identical handling — one pass, no distinction needed.
- **Classes**: no Invoke-specific logic beyond the two exhaustiveness arms. TypeScript forbids a
  `class` declaration from carrying a call signature; `Classes.fs`'s existing `CallSignatures`
  reads are for method overloads, not class-level callability.
- **The residual case**: only a name collision (the hybrid already declares `Invoke`) keeps
  `SI001`. All 11 real-fixture `SI001` sites at baseline had no such collision.

## Findings before/after (`dotnet fsi build.fsx -- findings`)

Baseline (clean checkout, verified by stash): exact 535, ergonomic 1603, widened 797, escape 200,
`SI001` 11, `SI008` 0.

After (`solid-js`, `animejs`, `intersection-lab`, `intersection-callable-lab` regenerated, plus the
new `callable-hybrid-lab`): `SI001` 1 (the lab's deliberate `Collides` negative — zero residual
among the 11 real sites), `SI008` 17 (11 from the real sites + 6 new from the lab's `Widget`,
`Trigger`, `Multi`, `Ambiguous`, `Identity`, `Boxed`).

Gross per-symbol movement, all 11 real sites, `SI001` → `SI008`:
- `solid-js`: `Errored` (escape), `ChildrenReturn` (widened) → `ergonomic`; `Pending`, `Ready`,
  `Refreshing`, `Unresolved` (widened) — the `SI008` finding itself is `ergonomic`, but each
  symbol's aggregate tier is unchanged because other findings on the same symbol still hold it at
  its prior tier (e.g. `Errored` stays `escape`, `Pending`/`Ready`/`Refreshing`/`Unresolved` stay
  `widened`) — consistent with tier being the worst finding per symbol, not this one.
- `animejs`: `AutoLayoutParams.Delay`, `AutoLayoutParams.Delay2` (escape, unchanged for the same
  reason), `ChainableUtil` (widened → `ergonomic`, no other finding held it back).
- `intersection-lab`: `Cancelable` (widened → `ergonomic`).
- `intersection-callable-lab`: `Timers.Schedule` (widened → `ergonomic`).

Net tier movement across the 4 real fixtures (excluding the new lab): ergonomic +4, widened −4,
exact and escape unchanged — matching the 4 symbols above that had no other finding holding them
back, against the 7 that did.

## Metrics

- Generator tests: 490 → 499 (+9, all new `callable-hybrid-lab` cases). Wire tests: 90 (1 skipped
  by design), unchanged.
- Run gate: 309 → 318 checks (+9), exit 0, includes a real `widget.handler.Invoke "ping"` call
  against a hand-written JS runtime.
- Full gate (`dotnet fsi build.fsx -- test`, no flags): passed — restore, clean, format, npm
  install, fixtures, 499+90 tests, run gate all green.
- `git diff --stat`: 23 files changed, 357 insertions(+), 46 deletions(-), plus two new untracked
  directories (`tests/fixtures/callable-hybrid-lab/`, `tests/Xantham.Generator.Tests/golden/callable-hybrid-lab/`).
- Branch `worktree-gen-wave14-cr`, forked at `5c827c4`.

## Anything unexplained

Nothing outstanding. The one out-of-scope observation (already known pre-existing, not touched):
`Exports.fs` renders a top-level exported value of a hybrid type as a direct callable static
function, bypassing that type's own properties at the export site — pre-existing behavior shared
with `intersection-callable-lab`'s `roundPad`, unrelated to `shape-interfaces`.
