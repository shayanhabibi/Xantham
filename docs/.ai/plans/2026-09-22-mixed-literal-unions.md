# Mixed literal/typed unions as `[<Erase>]` DUs: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use `superpowers:subagent-driven-development`
> to implement this plan task by task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> REQUIRED READING before starting: `.claude/rules/generator-fixtures.md` (lab first, run
> everything, read almost none of it), `.claude/rules/comments.md`, and
> `docs/.ai/plans/generator-type-mapping.md` §D4/§D12 for how unions are mapped today.
>
> **Step 0 is a go/no-go gate. Do not skip it.** This plan may well conclude in "don't build
> this", and that is a successful outcome.

**Goal:** A TypeScript union mixing string literals with non-literal types —
`"center" | number | readonly number[]` — becomes a named F# DU that constructs directly and
matches correctly:

```fsharp
[<RequireQualifiedAccess>]
[<Erase(CaseRules.None)>]
type Origin =
    | center
    | Num of float
    | NumArray of float[]
```

**Why:** Today such a union falls through to `erasedUnionRef` and lands as `U2<float, float[]>`
with the literals widened to `string`, or widens to `obj`. Both lose the literal set. The DU
form keeps it, and `Origin.Num 3.5` is a direct application with no `!^` needed.

**Status:** design settled and empirically validated against Fable 5.13.0; not implemented.
Two review rounds and four compile probes went into it. Everything below marked *verified* was
observed in emitted JavaScript, not recalled.

## The constraint that shapes everything

An erased DU's arms are discriminated by **runtime type tests**. Fable compiles a `match` on
an `[<Erase>]` DU through `transformUnionCaseTest` (`FSharp2Fable.fs:494-565`): a fieldless
case becomes `x === "center"` equality, a single-field case becomes a `Fable.TypeTest` that
`transformTypeTest` (`Fable2Babel.fs:~485-539`) then lowers.

Two distinct failure modes follow, and **only one of them produces a diagnostic**:

| Failure | Fable's response | Result |
|---|---|---|
| Arm has no usable type test | `warning FABLE: Cannot type test (evals to false): T` | branch silently absent |
| Two arms share one type test | **nothing at all** | second branch silently dead |

Both compile. Both ship wrong answers. The second is invisible to any log-scraping check,
which is why condition 3 below exists and why verification needs a runtime round trip.

## Settled decisions

### One arm per non-string type, never a folded `NonString of U2<...>`

Verified. Folded constructs fine — `Folded.NonString !^ 3.5` erases to `3.5` — but `U2` is
itself erased, so its type test folds to `false`:

```
warning FABLE: Cannot type test (evals to false): Fable.Core.U2`2 (erased)
```
```js
export function describeB(x) { return "center"; }   // the payload branch is gone
```

Per-arm, same input, discriminates correctly:

```js
if (typeof x === "number") { ... } else if (isArrayLike(x)) { ... } else { return "center"; }
```

`!^` works in a union-case field position; it is simply not needed, and is not a reason to
prefer folded.

### Collisions resolve through the existing `uniqueCaseNames`

`Spec.fs:3163` already yields `Num`, `Num2`, `Num3` in member order and already serves
`LiteralUnions.fs` and `TaggedUnions.fs`. A literal `"Num"` plus a `number` arm gives
`| Num | Num2 of float`. Do **not** introduce a prime suffix (`Num'`) — it would put two
collision conventions in one generated file for one concept.

Feed literal cases first and payload arms last: the TS-derived literal then wins the
unsuffixed name, and stays winning when upstream adds a literal.

### No `CaseN` fallback, ever

Positional names break under the exact event bindings exist to survive: upstream inserting a
union member renames every later arm, silently changing the F# API across a package bump.
Regeneration tests would not catch it — output stays byte-reproducible for a fixed input —
but user code breaks. Arm names derive from the arm's type, which is insertion-stable.

### An unnameable or non-discriminable arm widens the whole union

Do not paper over one arm. Fall back to today's behaviour and record a finding.

## The discrimination table — verified row by row

Compiled one mixed union per row, using the payload forms Xantham actually emits.

| Arm payload | Emitted test | Verdict |
|---|---|---|
| `float` | `typeof x === "number"` | discriminable |
| `bigint` | `typeof x === "bigint"` | discriminable, distinct from `float` |
| `bool` | `typeof x === "boolean"` | discriminable |
| `float[]` **and** `(float * float)` | `isArrayLike` for both | **ambiguous** — `Fable2Babel.fs:491-492` sends `Fable.Array` and `Fable.Tuple` to one test |
| `System.Func<..>` **and** `(float -> float)` | `typeof x === "function"` for both | **ambiguous** — `Fable2Babel.fs:489-490`, does not subdivide by arity |
| an F# enum (`FsEnum`) | `typeof x === "number"` | discriminable, but **collides with a `float` arm** |
| a class bound with `[<Import>]` | `x instanceof Box` | discriminable |
| a plain interface | warning, branch absent | **not discriminable** |
| a `[<StringEnum>]` | `typeof x === "string"` | **catastrophic — see below** |
| a nested mixed `[<Erase>]` union | warning, branch absent | **not discriminable**; the feature does not nest |
| `U2`/`FsErasedUnion` | warning, branch absent | not discriminable |

Two rows deserve emphasis:

**A `StringEnum` arm does not merely collide with the literals — it kills them.** The emitted
body returned the payload branch for *any* string, leaving the fieldless literal case
unreachable. Classify `StringEnum` payloads as `Str`, which disqualifies.

**Plain interfaces are the common case in TypeScript definitions.** `Render.fs:756-767` emits
an `[<Import>]` binding only when the declaration is class-shaped; everything else has no JS
constructor, so `transformTypeTest` falls to `warnAndEvalToFalse`. Expect most interface arms
to disqualify their union. **This is what Step 0 measures.**

## `RequireQualifiedAccess` is mandatory, not stylistic

```
error FSHARP: Lowercase discriminated union cases are only allowed when using
RequireQualifiedAccess attribute (code 53)
```

TypeScript string literals are overwhelmingly lowercase and Xantham preserves them as case
names. Do **not** copy `renderStringEnum`'s conditional `qualified` logic
(`Render.fs:801-804`), which drops the attribute for single-case enums — that is a compile
error here whenever the sole literal is lowercase.

## Step 0 — go/no-go gate

- [ ] Wire the classifier from `mixed-union-classifier.fsx` (see "Prototype" below) into a
      throwaway counting pass. No model or render changes.
- [ ] Over Anime.js and two other packages, count: unions mixing string literals with
      non-literal members; and of those, how many pass every condition below.
- [ ] **If the second number is near zero, stop and write up the negative result.** The
      implementation cost is dominated by a tenth `FsDecl` case rippling through ~40
      exhaustive match sites. That is not worth paying for a handful of unions.

## Conditions to emit

All must hold; otherwise decline and let today's path run, with a finding.

1. Every literal is a **string** literal. Number/bool literals mixed with typed arms are out
   of scope — the `CompiledValue` story for erased unions is unverified.
2. Every non-literal member resolves to a nameable F# type.
3. Every non-literal member is discriminable, and **no two share a test class**. This is the
   check `erasedUnionRef`'s `List.distinct` does not do: it dedupes by `FsTypeRef` structural
   equality, and `float[]` vs `string[]` are distinct refs sharing one test.
4. No arm's test class is `Str` or `Opaque`.
Callback arms are **not** excluded. A callback arm discriminates fine (`typeof x ===
"function"`); the fact that arm overloads serve it better at parameter position is a reason
for a user to choose that style, not a reason for this pass to refuse. See "Relationship to
union arm overloads".

## Where it lands

| Concern | Site |
|---|---|
| Entry gate | `Shape/LiteralUnions.fs:58-64` — see the trap below |
| Fallback that runs today | `Shape/Spec.fs:2321` `erasedUnionRef`, result at `2340-2344` |
| Name uniquing | `Shape/Spec.fs:3163` `uniqueCaseNames` — reuse verbatim |
| New decl case | `Model.fs:1505-1515` `FsDecl` (9 cases today) |
| Clashing name | `Model.fs:1154` `FsTypeRef.FsErasedUnion` — use `FsMixedUnion` for the decl |
| Render | **no template exists**, see below |
| Findings | `Findings.fs:768` `type ClassifyLiteralUnions` |
| Pass order | `Shape/Passes.fs:9-31` |

### The `literals.Length < 2` trap

`LiteralUnions.fs:58-64` bails on three OR'd conditions:

```fsharp
if literals.Length < remaining.Length   // mixed union
   || literals.Length < 2               // fewer than two literals
   || isBooleanPair model remaining
then None
```

The second guard predates this feature and is **not** conditioned on the mixed case, so
`"center" | number | readonly number[]` — one literal — would never reach a branch added
naively alongside the first guard. The mixed path needs `literals.Length >= 1`, which means
restructuring this into an explicit three-way split (pure literal enum / mixed / decline)
rather than adding a disjunct.

**Invariant:** the existing D12 path must not change. Lines 58-150 already handle all-int
enums, string enums, and mixed *literal* unions (string + number/bool literals as one
`StringEnum` with `CompiledValue` cases). All three sit under
`literals.Length = remaining.Length` and belong to the "pure literal enum" leg. Regenerate and
diff to prove they are byte-identical after the restructure.

### Render: there is no template to copy

- `renderStringEnum` (`Render.fs:800`) emits attribute-only cases, no payload.
- `renderTaggedUnion` (`Render.fs:834`) looks closer and is a trap. Different attribute,
  different dispatch (tag-property equality, not a type test), and its cases carry a *named
  field list* (`FsTaggedCase.Fields`, `Model.fs:1406-1409`) erasing to
  `{ kind: "circle", radius: 2 }`. `Model.fs:1402-1405` documents this in the codebase's own
  words: carrying the arm type as a single payload field "does *not* work — Fable wraps it as
  `{ kind: "circle", Item: x }`, an object no TypeScript signature would accept."

`[<Erase(CaseRules.None)>]` with single **positional** payload cases has no renderer today.
Budget for writing one.

### The `FsDecl` ripple — the real cost

A tenth `FsDecl` case touches roughly forty exhaustive match sites across
`DeclarationCatalog.fs`, `Render.fs`, and the `Shape/` passes `Aliases`, `Arity`, `Coverage`,
`Ordering`, `Orphans`, `ExportCollisions`. Most is mechanical — the compiler finds exhaustive
matches.

- [ ] **Audit wildcard `_ ->` arms by hand first.** Those compile clean and silently route the
      new declaration into default handling. A mixed union falling into an `FsAbbrev`-shaped
      default in the catalogue or ordering pass would be a subtle, hard-to-trace bug.

## Model change

Add a decl type rather than widening `FsUnionCase`, leaving the StringEnum path untouched:

```fsharp
/// One case of an `[<Erase(CaseRules.None)>]` mixed union. A fieldless case is a string
/// literal, carrying `CompiledName` when the literal differs from the case name; a case with
/// `Payload` is a non-literal union member, discriminated at runtime by its own type.
type FsErasedCase =
    { Name: string; CompiledName: string option; Payload: FsTypeRef option }

type FsErasedUnionDecl =
    { Name: string; Docs: string; Tags: JSDocTagInfo list; Order: DeclOrder option
      Cases: FsErasedCase list }
```

`FsErasedCase`, `FsErasedUnionDecl` and `FsMixedUnion` are all free of collisions.

## Arm naming

Total function `armName : FsTypeRef -> string option`, `None` disqualifying the union:

- primitives from a fixed table — `float` → `Num`, `bool` → `Bool`, `bigint` → `BigInt`
- named types take their own name
- constructors get a mechanical suffix, not pluralisation — `FsArray FsFloat` (renders
  `float[]`, `Render.fs:152`) → `NumArray`. Pluralising reads better and is not total:
  irregulars and already-plural type names. Note Xantham never emits `ResizeArray`; `Array<T>`
  and `ReadonlyArray<T>` resolve through `arrayElement` (`Spec.fs:264-271`) to `FsArray`.
- anonymous object types never arrive here: `Passes.fs:9-31` runs `synthesizeAnonymous`
  (position 2) before `classifyLiteralUnions` (position 5), so the arm takes the hoisted name.
  That hoisted declaration is usually an interface, so it will disqualify on discrimination
  anyway.

Then `uniqueCaseNames (literalCaseNames @ armNames)`, literals first.

### Case order

Literals in source order first, payload arms after. Observed: payload tests become `if`
branches and literals the `else` fallback — but treat that as an observation, not a guarantee.
Per-case sequencing comes from FCS's decision-tree construction, not from anything Fable
decides, so no line can be cited. The design does not depend on it: condition 3 removes any
overlap, so no ordering can make a branch dead. Do not build anything that *relies* on it.

## Findings

On `type ClassifyLiteralUnions`:

```fsharp
| [<Ergonomic>] MixedUnionAsErasedDu of name: string * arms: int
| [<Widened>] MixedUnionArmUnnameable of name: string * arm: string
| [<Widened>] MixedUnionArmNotDiscriminable of name: string * arm: string
| [<Widened>] MixedUnionArmsAmbiguous of name: string * first: string * second: string
```

The three `Widened` findings make every fallback visible in the tier table rather than silent.

## Reuse at other reference sites

`namedUnionByMembers` (`Spec.fs:150-183`) is what makes a *reference* to the same member-id set
resolve to `FsNamed "Origin"` rather than re-collapsing through `erasedUnionRef`. Its
`List.isEmpty nullish || literalEnum` condition appears to admit mixed unions through the first
disjunct. **Confirm by test, not by reading:** if it does not hold, the generator emits a named
mixed union nothing refers to, while every use site still widens to `U2`/`obj`.

## Gating

Off by default for 0.1.x. Config flag reaching the pass through `Context` — mirror the existing
`groups`/`module` plumbing in `xantham.json`. **The exact config plumbing is the one thing in
this plan still unread.** Flipping the default later changes generated APIs for any package
with a mixed union, so it is a breaking change belonging to a minor release with the tier table
diffed.

## Verification

- [ ] **Gate holds.** Regenerate Anime.js with the flag off; `bindings/Animejs.fs` byte-identical.
- [ ] **D12 untouched.** Existing literal-union output byte-identical after the restructure.
- [ ] **Determinism.** Generate twice with the flag on; empty diff.
- [ ] **No non-discriminable arms.** Compile flag-on output and assert zero `Cannot type test`
      occurrences in the log.
- [ ] **No ambiguous arms — not greppable.** Two guards, both required:
      - a unit test asserting the test class is injective across every emitted union's arms;
      - a **runtime round trip**: construct each arm, match it, assert the branch taken, under
        Node. This is the only check that observes a dead branch.
- [ ] **Unit.** `armName`/`testClass` tables; `uniqueCaseNames` giving `| Num | Num2 of float`.
- [ ] **Tier table.** Baseline 129 exact / 129 ergonomic / 14 widened / 79 escape before;
      declined arms should appear as `Widened`, not vanish.

Do not rely on the log-scraping check to cover ambiguity. A plan that greps the build log and
calls it done ships the exact bug this feature exists to prevent.

## Prototype

A classifier sketch — `TestClass`, `testClass`, `armName`, `classifyMixed`, with every mapping
annotated by the probe or Fable source line that grounds it — was written alongside this plan
and is intended to be lifted into `Shape/Spec.fs`. It is scratch, not committed. Regenerate it
from the discrimination table above if it is gone.

## Relationship to union arm overloads

Paired record: `docs/.ai/plans/2026-09-22-union-arm-overloads.md`. Changes to arm composition
belong in both.

**These are two mapping styles for different contexts, not rivals.** A named DU preserves the
literal set and reads well anywhere a value is held, named, or matched. Arm overloads read
better at a call site, particularly for callbacks. Which is wanted is a property of the
consuming code, which the generator cannot see. It is a user choice expressed in config — the
same way `groups` dispositions already are — and our job is to pick defensible defaults and
document the interaction honestly, not to arbitrate per union.

Where both are enabled and could claim the same union, **precedence is structural and fixed**:
`classify-literal-unions` is `Passes.fs` position 5, `expand-union-arms` runs after
`dedupe-overloads`, so a union this pass converts to a named DU is no longer a `U_n` and arm
expansion never sees it. That is the same mechanism by which §4.5 preference 1 already removes
all-literal unions from that population. Nothing negotiates; the earlier pass wins.

Do **not** make either pass inspect the other's config flag or use sites. An earlier draft of
this plan proposed exactly that — declining callback-bearing unions so arm overloads could
claim them — and it was wrong twice over: with overloads disabled (the shipping default) it
loses the literal set to buy nothing, and it assumed an eligibility that pass does not
guarantee. A user who enables a combination we would not have chosen gets the documented
result of that combination.

### What informs the default

Xantham's tiers rank fidelity above convenience: a named DU keeps the literal set (exact),
while widening to `U_n` loses it. That argues for this pass claiming what it can by default
once Step 0 justifies building it, and for arm overloads serving the unions it declines —
which is, conveniently, what the structural precedence already does.

The one place that ordering costs something real is callbacks, and the cost is measured rather
than asserted:

### The two "distinctness" rules are different, and mine is stronger

Arm overloads require arms *pairwise distinct as F# signatures* (`U2<string, string>` is
refused). This pass requires arms *pairwise distinct as runtime test classes*. Test-class
distinctness implies signature distinctness but not the reverse — `float[]` and `string[]` are
distinct F# signatures that share one `isArrayLike` test.

So any union this pass accepts automatically satisfies the collapsing-arm exclusion, but not
vice versa. Neither rule substitutes for the other.

### Callback arms belong to arm overloads, not here

Measured, and it reverses the intuitive answer. A union-case constructor gets **no**
lambda-to-delegate coercion — that conversion applies at method-argument position only:

| Position | Bare lambda | Emitted JS |
|---|---|---|
| DU case, `System.Func<float,float,string>` arm | **FS0002** — needs `System.Func<_,_,_>(...)` explicitly | `(a, b) => ...` |
| DU case, curried `(float -> float -> string)` arm | compiles | `(a) => ((b) => ...)` — **curried, wrong arity at the JS boundary** |
| Method parameter carrying the arm type | compiles | `(a, b) => ...` correct |

Both DU forms are worse than an arm overload: one demands an explicit delegate construction,
the other silently emits a curried function where JavaScript expects two parameters. That
second row is the same hazard `Model.fs:1155-1158` already cites as the reason `FsDelegate`
exists "where `FsFunc` would lose arity".

Arm callability is the *load-bearing* benefit of the overload feature, per its own record, and
the DU route cannot reach it from either direction. So a user whose consuming code is
callback-heavy at call sites has a genuine reason to prefer that style and turn this pass off
for those packages. That is a real trade, and it is theirs to make.

What this pass owes them is an accurate description of what it costs, not a rule that decides
for them:

- a delegate arm in a DU case needs `Origin.Cb(System.Func<_,_,_>(fun a b -> ...))` — correct,
  and verbose;
- a curried-function arm accepts a bare lambda but emits `(a) => ((b) => ...)`, which is
  wrong at the JS boundary. **Prefer `FsDelegate` for callback arms in this decl** for exactly
  the reason `Model.fs:1155-1158` already gives.

Note the granularity difference when reading that trade: emitting the DU is a *declaration*-level
decision binding every use site, while arm expansion is a *member*-level decision binding one
parameter. A union appearing at one parameter and three property positions is served
differently at each, and no single setting is optimal at all four.

- [ ] **Step 0 should size the callback population** — how many mixed unions carry a callback
      arm, and at what positions — so the default is chosen against real numbers and the
      config documentation can say which packages it matters for.

### Step 0 must also count the overlap

The corpus numbers in the overload record — 484 imported static members, 59 with a union
parameter, 57 with exactly one — are measured against today's union population. Every union
this pass claims leaves that population. Step 0 should therefore report, alongside its own
counts, **how many of those 59 members carry a union this pass would claim**, so the overload
feature's cap analysis (the six-arm cluster carrying 132 of 239 overloads) can be recomputed
rather than silently invalidated.

## Out of scope

- Non-string literals in mixed unions (condition 1).
- Unions of named types with no literal at all — still `erasedUnionRef`'s job.
- Changing `erasedUnionRef` itself.
- Nesting. Not a limitation to fix later: it is a property of how Fable compiles the type test.
