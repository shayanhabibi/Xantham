# Lane AM — a function-typed arm of an erased union

Wave eight. Branch `worktree-gen-wave8-am`, forked from `worktree-generator-wave-eight` at
`9a97b95`.

Lane AK left one position emitted and unproven: a function type inside `U2<_,_>`. `U2` erases at
runtime, so the arm that crosses is the bare value, and nothing had measured whether the arity and
the arguments survive that wrapping and unwrapping. This lane measured it.

**The union arm carries arity and arguments intact in every position measured, and the rule lane
AK established already applies inside the arm.** No `src/` change. The delegate is already retained
at arity 2 inside a union arm, with `TR055` naming the position, because the callback pass reads
the arm's own type rather than the union around it.

## The shape the corpus contains

Grepped, not read. `U[0-9]<...->...>` occurs 133 times in the `@cloudflare/workers-types` golden,
61 in `animejs`, 28 in `solid-js`. The recurring forms:

- `U2<(obj -> unit), EventListenerObject<Event>>` — a converted arm of arity 1 beside an
  **interface** arm, in parameter position, and inside an `Action<_,_,_>` on a `Create`.
  `EventListenerOrEventListenerObject` is the named abbreviation of it.
- `U2<float, Func<Target, float, Target[], float>>` — a **retained delegate** arm beside a
  primitive arm, on a property with `get, set` and as a `Create` parameter (`animejs`).
- `U2<bool, (ScrollObserver -> bool)> option with get, set` — a converted arm on an optional
  property (`animejs`).
- `U2<('T -> unit), Observable.Subscribe.Observer<'T>>` in parameter position (`solid-js`).

A union of *two* function types does not occur. Every occurrence is a callback arm beside a
non-callback arm, which is the shape the lab reproduces.

## What the lab now declares

`tests/fixtures/callback-function-lab/index.d.ts` and `index.js` gain a union block. The runtime
reports the same pair as the rest of the lab — `fn.length` beside the result of calling the
function with all of its arguments at once — and reports the value itself when the non-callback arm
arrived, so a curried chain would be visible twice over.

| Export | Declared | Emitted |
| --- | --- | --- |
| `callUnionNone` | `(() => string) \| string` | `U2<string, (unit -> string)>` |
| `callUnionOne` | `((a: number) => string) \| string` | `U2<string, (float -> string)>` |
| `callUnionTwo` | `((a, b) => string) \| string` | `U2<string, Func<float, float, string>>`, `TR055` |
| `Listener` / `callUnionNamed` | named abbreviation of the arity-1 union | `type Listener = U2<string, (float -> string)>` |
| `makeUnionOne` | returns `((a) => string) \| string` | `U2<string, (float -> string)>` |
| `makeUnionTwo` | returns `((a, b) => string) \| string` | `U2<string, Func<float, float, string>>`, `TR055` |
| `UnionHandlers` | three union-typed members | `one`/`text` converted, `two` delegate, `TR055` |
| `unionHandlers` | the same object built in JavaScript | read-back position |
| `fireUnion` | reads a `UnionHandlers` built in F# | outward position |
| `callUnionObject` | `((a) => string) \| ListenerObject` | `U2<ListenerObject, (float -> string)>` |
| `objectUnion` | the same union as a `const` | read-back beside an object arm |

## Measurements

Every row below is a run-gate check that passed on `tests/Xantham.Generator.RunGate/Program.fs`'s
`callbackUnionArmForms`, against the generated golden rather than a hand-written mirror.

| Position | Arity | `length` JavaScript saw | Arguments |
| --- | --- | --- | --- |
| parameter, converted arm | 0 | 0 | result `none` |
| parameter, converted arm | 1 | 1 | `1` arrived |
| parameter, retained delegate arm | 2 | 2 | `1, 2` arrived |
| parameter, non-callback arm of the same union | — | — | `plain` arrived |
| parameter, named abbreviation | 1 | 1 | `1` arrived |
| return, converted arm | 1 | 1 | `one:5:1` |
| return, retained delegate arm | 2 | 2 | `two:5:1:2` |
| member read back, converted arm | 1 | 1 | `js1:1` |
| member read back, retained delegate arm | 2 | 2 | `js2:1:2` |
| member read back, non-callback arm | — | — | `plain` |
| `ParamObject` literal read outward | 0/1/2 mixed | 1 and 2 | `1:one:1\|2:two:1:2\|text:plain` |
| parameter, converted arm beside an object arm | 1 | 1 | `got:1` |
| parameter, object arm of the same union | 1 (`handleEvent`) | 1 | `obj:1` |
| `const` read back beside an object arm | 1 | 1 | `js:1` |

### Two facts worth carrying

**Read-back through a union member preserves the arity JavaScript holds.** Lane AK measured that
reading a function-typed member back hands F# a curry wrapper of length 1. That does not happen
here: the member's declared type is the union, so the read carries no function type and Fable
inserts no wrapper. The delegate arm reads back at length 2 and the converted arm at length 1, each
the arity JavaScript declared. Inside the converted slice the two behaviours coincide at length 1,
so no consumer sees a difference — but the union position is the weaker claim of the two, and it is
the one that holds.

**Discriminating a union whose other arm is an interface is the pre-existing erased-union
limitation, not a callback fact.** Where the non-callback arm is `string`, `match` reaches the right
arm: Fable compiles the test to `typeof $0 === "string"`. Where it is an interface,
`fable-workaround-lab` already measures that Fable folds the type test to `false` and the match
collapses. The object-arm checks therefore discriminate in JavaScript (`typeof $0 === "function"`)
rather than adding a third folding type test to the gate. Arity and argument passage are unaffected
either way; this is about which arm a consumer can name, and that question belongs to the erased
union, not to the callback rule.

## Files touched

- `tests/fixtures/callback-function-lab/index.d.ts`, `index.js` — the union block above.
- `tests/Xantham.Generator.RunGate/Program.fs` — `callbackUnionArmForms`, called from `main`.
- `tests/Xantham.Generator.Tests/golden/callback-function-lab/` — regenerated.

No `src/` change. No finding case added; `TR055` was already raised at every retained union arm.
The run gate needed no `.fsproj` edit — lane AK had already linked the golden — and no
`Pipeline.test.fs` edit, since `fixtureTests "callback-function-lab"` is registered at line 2774.

## Counts

Gate: **460 generator tests, 90 wire tests, run gate 249 checks**, up from 460 / 90 / 230. Compile
gate green. `git diff --numstat`: 6 files, +335 / -8.

Corpus tiers, summed over every manifest:

| | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| before | 479 | 1540 | 783 | 193 |
| after | 488 | 1544 | 783 | 193 |

The whole movement is the lab's own new exports: its manifest goes from 9/9/0/0 to 18/13/0/0. No
npm golden changed — `git status` lists no file under `golden/@cloudflare`, `golden/animejs`,
`golden/solid-js` or `golden/type-fest`.

`TR055` across the corpus, counted as occurrences in every `symbols.jsonl`: **357 before, 360
after**. The three are the lab's own `callUnionTwo`, `makeUnionTwo` and `UnionHandlers.two`. The npm
fixtures hold at cloudflare 180, animejs 120, solid-js 30, type-fest 0.

## Left undone

Nothing the brief asked for. Three observations for whoever picks callbacks up next:

- **A union arm was measured only where the arm is a direct child of the union.** `animejs` carries
  `U2<ScopeConstructorCallback, (Scope -> Tickable)>[]` — an array of unions — and
  `U2<bool, (ScrollObserver -> bool)> option`. Both compile; neither is run-gated. Erasure makes
  the arity claim the same one, but the claim is not measured through the array or the option.
- **The setter half of a union-typed property is unmeasured.** `fireUnion` covers "F# builds the
  object, JavaScript reads the members", which is where the setter's value lands, but no check
  writes to a union-typed `get, set` member after construction.
- **`U2<...>` inside a delegate's own type parameter** — cloudflare's
  `Action<'Type, U2<(obj -> unit), EventListenerObject<Event>>, ...>` on the `EventTarget` `Create`
  — is one nesting level deeper than anything here. The outer delegate carries its arity by lane
  AK's measurement and the inner arm by this one, so the composition is expected to hold, but the
  composition itself is not measured.
