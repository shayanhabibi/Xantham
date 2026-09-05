# Lane AQ — two callback-union positions lane AM left unreached

Wave nine. Branch `worktree-gen-wave9-aq`, forked from `worktree-generator-wave-nine`
`f7d6c17`.

Lane AM measured every union-arm callback position it found, and flagged exactly two it had
not reached: a function-typed union arm one level deeper than a direct union child (behind an
array, behind an `option`), and a `U2<...>` nested inside a delegate's own type parameter. This
lane measured both.

**Both positions carry the arity JavaScript declared.** No `src/` change. `TR055` already fires
on every retained union arm reached; no new finding case was needed.

## Positions measured

### 1. A function-typed union arm behind an array or an `option`

`animejs`'s real occurrences:

- `U2<ScopeConstructorCallback, (Scope -> Tickable)>[] with get, set` (array member, `Scope`)
- `U2<bool, (ScrollObserver -> bool)> option with get, set` (optional member, `repeat`)

Reproduced with `ArrayUnionHandlers.steps: U2<string, (float -> string)>[]` and
`OptionUnionHandlers.step: U2<string, (float -> string)> option`, in every crossing direction the
lab already exercises for a direct union child: `ParamObject` literal (parameter direction) and
JS-built object (read-back direction), present and absent for the option.

| Position | Arity | `length` JavaScript saw | Arguments |
| --- | --- | --- | --- |
| array member, `ParamObject` literal | 1 | 1 | `1:js1:1\|text:plain` |
| array member, read back from JavaScript | 1 | 1 | `1:js1:1\|text:plain` |
| option member present, `ParamObject` literal | 1 | 1 | `1:js1:1` |
| option member absent, `ParamObject` literal | — | — | `none` |
| option member present, read back from JavaScript | 1 | 1 | `1:js1:1` |
| option member absent, read back from JavaScript | — | — | `none` |

The generator emits `U2<string, (float -> string)>[]` and `U2<string, (float -> string)> option`
unchanged from the direct-child shape, just wrapped one level. No new behaviour in `Shape/` was
needed; the array and option passes each already carry whatever type sits inside them, union or
not.

### 2. `U2<...>` nested inside a delegate's own type parameter

Real site: `@cloudflare/workers-types`, `Action<'Type, U2<(obj -> unit),
EventListenerObject<Event>>, ...>` on `EventTarget`'s `Create`.

Reproduced with `addListener(register: (kind: string, listener: ((x: number) => void) |
UnionListenerObject) => void): void`. `register` has arity 2 and returns `void`, so it converts
to `Action<string, U2<UnionListenerObject, (float -> unit)>>` by the arity rule lane AK
established — the outer delegate's composition with an inner union arm was exactly the
uncertainty lane AM flagged, and it holds: the union's own arm keeps the arity its declaration
gave it, independent of the delegate wrapping it.

| Position | Arity | `length` JavaScript saw | Arguments |
| --- | --- | --- | --- |
| function arm, nested inside `Action`'s own type parameter | 1 | 1 | `fn-arm:1` |
| object arm, nested the same way | 1 (`handleEvent`) | 1 | `obj-arm:obj:1` |

**One pre-existing limitation surfaced again, not a new one.** Discriminating the two arms with
an F# `match ... U2.Case1 | U2.Case2` inside the `register` lambda fails the same way lane AM
already recorded for `callUnionObject`/`fireUnion`: Fable's runtime type test for the interface
arm (`UnionListenerObject`) folds to `false` at compile time (`Cannot type test (evals to
false)`), so the match always took the function-arm branch — both calls, including the one
JavaScript made with the object arm, landed there, and `obj.handleEvent` on that mis-typed value
came back `undefined`. Rewriting the discrimination as `emitJsExpr listener "typeof $0 ===
\"function\""`, exactly the pattern the fixture's existing object-arm checks already use, gets
the correct arity for both arms. This is the erased-union limitation lane AM's own handover
already flagged as "not a callback fact" — it recurs here because it is the general shape, not
because nesting inside a delegate changes it.

## Files touched

- `tests/fixtures/callback-function-lab/index.d.ts`, `index.js` — `ArrayUnionHandlers`,
  `OptionUnionHandlers`, `UnionListenerObject`, `addListener`.
- `tests/Xantham.Generator.RunGate/Program.fs` — `callbackUnionNestingForms`, called from `main`
  after `callbackUnionArmForms`.
- `tests/Xantham.Generator.Tests/golden/callback-function-lab/` — regenerated
  (`CallbackFunctionLab.fs`, `manifest.json`, `symbols.jsonl`).

No `src/` change. No new finding case; `TR055` already raised on every retained union arm this
lane's new exports carry. The run gate needed no `.fsproj` edit (golden already linked by lane
AK) and no `Pipeline.test.fs` edit (`fixtureTests "callback-function-lab"` already registered) —
both confirmed still present before starting.

## Counts

Fast loop (`--quick --update --filter callback-function-lab`, run gate included per this item's
brief): **257 run gate checks, up from 249** (base `f7d6c17`), all green.

Corpus tiers, summed over every manifest (`dotnet fsi build.fsx -- findings`):

| | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| before | 488 | 1544 | 785 | 193 |
| after | 493 | 1548 | 785 | 193 |

Movement is entirely the lab's own new exports: `callback-function-lab` manifest goes from
18/13/0/0 to 23/17/0/0 (exact/ergonomic/widened/escape). No npm golden changed —
`git diff --numstat` against the prior commit lists nothing under `golden/@cloudflare`,
`golden/animejs`, `golden/solid-js`, or `golden/type-fest`.

`TR055` across the corpus, counted as occurrences in each fixture's `symbols.jsonl`: **360
before, 361 after** — the one increase is the lab's own `addListener`.

`git diff --numstat` (commit `2957af0` vs. base `f7d6c17`): 6 files changed, +242/-8:
`tests/fixtures/callback-function-lab/{index.d.ts,index.js}`,
`tests/Xantham.Generator.RunGate/Program.fs`, and the three regenerated golden files.

Full `dotnet fsi build.fsx -- test` (no fast-loop flags) was run before this handover was
written; see the fifteen-line report back to the manager for its result.

## Left undone

Nothing the brief asked for. One observation for whoever picks callbacks up next: the erased-
union F#-`match` limitation lane AM found is general to any `U2` with an interface arm, not
specific to a position — it will resurface at the next site with the same shape, and the
workaround is always the same `emitJsExpr "typeof $0 === ..."` substitution rather than a repair
in `Shape/`.
