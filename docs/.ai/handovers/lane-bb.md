# Lane BB — a retained callback is declared as a named delegate

Wave twelve, branch `worktree-gen-wave12-bb`, forked at `1a2f537`. Lane BA proved the Fable
boundary holds; this lane spends that proof.

A callback the arity rule keeps as a delegate is now a declaration of its own, carrying the
parameter names TypeScript spelled:

```fsharp
module CallTwo =
    type Handler = delegate of x: float * y: float -> string
```

`Func<float, float, string>` said only how many arguments there are. The corpus writes 108 such
declarations; positional `Func<…>`/`Action<…>` in the goldens falls from 411 sites to 230.

## What was built

- `Model.fs` — `FsDelegateParam`, `FsDelegateDecl` and the `FsDelegateType` case of `FsDecl`.
  `FsDelegate` is unchanged: it is still what a reference position without a type node to name
  writes, and `printTypeIn` still spells it `Func`/`Action` there.
- `Shape/Anonymous.fs` — `needsName` admits a pure callback the arity rule retains, so the name
  comes from the same `claim` every other hoisted shape uses: sanitised, nested under its owner
  (`Handlers.OnTick`, `Factory.Make.Result`), uniquified by suffix, recorded under `SY004`/`SY005`.
  Each mint reports `SY006` under the name it claimed.
- `Shape/Spec.fs` — `callbackRetainedAsDelegate`, which reads `callbackRef`'s rule before shaping:
  a callback is written as an F# function type at one argument or none with a return written the
  same way, and is retained everywhere else. A callback this run names is written as that name, so
  a callback returning one is retained.
- `Shape/Callbacks.fs` — writes `FsDelegateType` where the reference the arity rule chose is a
  delegate, and keeps the abbreviation where it is a function type (`type Formatter = (float ->
  string)`).
- `Shape/FreeTypeParams.fs` — `bind-free-type-params` now covers callbacks, so a delegate hoisted
  out of a generic scope is declared over the variables it reads (`Each.Props.Render<'T, 'U>`) and
  every reference applies them back. `aliasTypeParams` binds the same list on the head.
- `Render.fs` — `renderDelegate`, plus the `FsDelegateType` arm of `declName`, `underLeaf`,
  `qualifyDecl`, `declFiles` and `symbolTiers`; `Ordering.fs`, `Coverage.fs`, `Aliases.fs` and
  `Arity.fs` take the same arm.

## The three problems the brief named

1. **Declaration order within a file.** Every emitted module is `module rec`, so a delegate
   declared after the interface referencing it still binds. `claim` records `DeclOrders` and
   `order-declarations` sorts delegates with everything else, by source order then name.
2. **Placement across files.** A delegate declaration is placed by the rule every synthesized
   declaration already uses: `declOrigins` reads `DeclNames`, `emittingGroup` sends it to its own
   group where that group ships and to the entry package otherwise. Nothing new was decided, and
   the compile gate compiles every regenerated golden, `groups/` first.
3. **One declaration per shape.** The declaration is hash-consed on the checker's type id, exactly
   as `DeclNames` hash-conses every other shape, so a shape used at *n* sites is declared once.
   361 retained sites resolve to 108 declarations.

## What is left inline, and why

Three classes of retained callback keep `Func`/`Action`, because naming them costs more than it
buys:

- **A method member's own type** (80 of the 131 remaining sites, all `Create` overloads). A method
  is shaped from its signature, not from a reference to its type, so `Options.Create (transform:
  Func<float, float, string>)` has no reference position for a name to be written at. Closing this
  needs the ParamObject pass to carry a name it does not currently have.
- **An application of a generic callback alias** — `ExportedHandlerFetchHandler<Env, Cf, Props>` at
  a member of `ExportedHandler`. The head would bind the alias's parameters and a reference has
  nothing to apply them with; naming it widened all nine `ExportedHandler` members to `obj`
  (`RA003`), so it expands in place under the alias's own name, which *is* a named delegate.
- **A rank-2 signature** (`<T>(t: T) => T`), for the same reason: F# has no rank-2 form, and the
  hoist a name would force loses the variables the inline spelling keeps in scope.

## Measurements

| | before | after |
|---|---|---|
| tiers | exact 495, ergonomic 1552, widened 782, escape 193 | exact 513, ergonomic 1616, widened 790, escape 195 |
| `TR055` | 361 | 325 |
| `SY006` | 0 | 80 |
| delegate declarations | 0 | 108 |
| run gate | 283 | 297 |
| findings, all keys | 17947 | 17928 |

`dotnet fsi build.fsx -- test` exits 0; `dotnet build Xantham.slnx` succeeds, so the compile gate
compiles every regenerated golden. Regeneration is 21 golden files, +762 -334; the whole branch is
37 files, +1201 -386.

**Tier movement, cause.** 94 symbol rows are added and 2 removed, and 16 change tier. The
delegate declarations are symbols of their own, so a finding that used to sit on the owner now
sits on the extracted delegate: `solid-js`'s `For.Props` moves widened → ergonomic while the new
`For.Props.Children` is widened, and `ErrorBoundary.Props` moves escape → widened while the new
`ErrorBoundary.Props.Fallback` is escape. Every pair I checked is that shape - the loss is
re-attributed to a finer-grained symbol, not created. In `callback-function-lab` eight export rows
move ergonomic → exact for the same reason.

**Finding movement, cause.** `TR055` (−36) and `TR032` (−167) fall because a shape used at several
sites is now shaped once, at its declaration, rather than once per site. `SY004` (+68) is the
nested naming of the new declarations. `MB001` (+34) and `MB006` (+6) rise: they land entirely on
new delegate rows in `animejs`, `@cloudflare` and `solid-js` - `Utils.Stagger.Result` carries the
five optional parameters that `utils.stagger`'s four overloads previously wrote out in full at
each return position. `RA003`, `RA006` and `TR013` are back at their baselines after the alias and
rank-2 exclusions; nothing else moves by more than 8.

**`Pipeline.test.fs`'s exact `TR055` list is unchanged and still passes.** All three reasons still
occur in `callback-function-lab`: `callNestingOne`'s outer level supplies "its return is itself a
callback".

## Lab fixture and gates

`tests/fixtures/delegate-name-lab` pins the feature: two- and three-argument callbacks, a
unit-returning one, a named alias, two members of the same declared shape beside a nullary one, a
retained callback in property and return position, and the nesting rule. `Pipeline.test.fs`
registers it; `Program.fs`'s `generatedDelegateForms` runs 14 checks over its golden - parameter
position at arities 2 and 3, the unit-returning arm, the alias position, a `ParamObject` literal
carrying two delegates, read-back off an interface member and off a property, a delegate returned
from a method, a factory crossing outward, and the nesting rule - against
`tests/fixtures/delegate-name-lab/index.js`, which reports the arity JavaScript received.

Nothing unexplained.
