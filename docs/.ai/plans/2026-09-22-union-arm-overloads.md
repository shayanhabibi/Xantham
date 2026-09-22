# Union arm overloads at parameter position

Status: design approved 2026-09-22, unimplemented.
Closes the deferred half of **D4** (`generator-type-mapping.md` §4.5).

`generator-architecture.md` records the state this design starts from: "`U_n` already satisfies
D4's position preference - `U2.Case1 x` constructs on input, DU matches on output - so only
overload expansion at input positions is still deferred." This is that expansion.

A member whose parameter is an erased union `U_n<A, B, C>` gains one synthesised overload per
arm, beside the union member rather than instead of it. `animate(targets: TargetsParam, …)`
keeps its signature and gains `animate(targets: string, …)`, `animate(targets: string[], …)`,
`animate(targets: HTMLElement, …)`.

## What the overloads buy

Two things, and the second is the load-bearing one.

An arm value reaches the member without `!^`. `Exports.animate("#field i", options)` compiles
where today it reads `Exports.animate(!^ "#field i", options)`.

**A delegate arm becomes callable.** Inside a `U_n`, a bare lambda has no target type to infer
against: `Exports.f(!^ (fun n -> "x"))` is FS0002 against `U2<string, System.Func<float, string>>`.
An arm overload gives the lambda its parameter types, and `Exports.f(fun a b -> "x")` compiles
unannotated at every arity measured. The Anime.js demo's hand-written five-to-four adapter
delegate exists because of this gap.

## What the overloads cost

`f(!^ x)` becomes FS0041 the moment arms exist. The cast has no unique target among the union
member and its arms, and no arrangement of the emitted overloads resolves it - an additive
overload set is what makes the target ambiguous. Published consumer code written in the `!^`
idiom requires either the cast deleted or the argument annotated.

This is why the feature ships disabled (§ Configuration). A consumer opts in, migrates the
affected call sites once, and keeps the union member for values already held at union type.

## Measured behaviour

Fable.Core 5.2.0, net8.0, F# compile probes. Recorded because several of these replaced an
assumption that pointed the other way.

| Shape | Call style | Result |
|---|---|---|
| union only | `f(!^ "x")` | compiles |
| union only | bare lambda into a delegate arm | **FS0002** |
| union + arms | plain arm value `f("x")` | compiles |
| union + arms | argument held at union type | compiles |
| union + arms | `f(!^ "x")` | **FS0041** |
| arms only, union member dropped | argument held at union type | FS0041 |
| union + arms, one arm `obj` | `f("x")` | compiles |
| union + arms, `float` and `int` arms | `f(1)` | compiles |
| union + arms, two arms one F# type | `f("q")` | FS0041 |
| union + arms, optional tail | either arity | compiles |
| union + arms, interface abstract member | `t.animate "q"` | compiles |
| union + arms | bare two-argument lambda | compiles |

The last three rows were probed as candidate exclusions and none of them earns one. The
optional tail resolves, and the interface member resolves. The `obj` and `float`/`int` rows are
doubly moot: arm collapsing already reduces a union with an `obj` arm to `obj` before this pass
runs, so such a union never arrives, and `number` maps to `float` with no `int` beside it. They
are recorded so a later reader does not re-probe them.

The two rules the table does justify are the collapsing-arm exclusion and the `!^` migration
note.

## Corpus shape

Across the 108 goldens: 484 imported static members, 59 with a union parameter, **57 of those
with exactly one**. Single-union expansion reaches 97% of the opportunity.

| arms | members | synthesised overloads |
|---|---|---|
| 2 | 18 | 36 |
| 3 | 1 | 3 |
| 4 | 12 | 48 |
| 5 | 4 | 20 |
| 6 | 22 | 132 |

Uncapped expansion adds 239 members. A cap of 4 admits 31 members for 87 overloads; the
six-arm row is a single cluster carrying 132 of the 239 on its own.

Most union parameters read as a named alias (`targets: TargetsParam`), not as an inline
`U6<…>`, so arm discovery resolves through `FsAbbrev`.

## The pass

`expand-union-arms`, in `Shape/`, **after `dedupe-overloads`**.

The order is the opposite of the intuitive one. `dedupe-overloads` drops overloads that widened
into one F# signature, and arm expansion produces exactly that collision. Expanding first would
have dedupe absorb synthesised arms and report `DO001` for them, which conflates two different
facts: that TypeScript declared overloads F# cannot separate, and that this pass invented a
clashing signature. Expanding second leaves expansion owning its collision check against a
settled signature set, under its own code.

The abbreviation map `dedupe-overloads` builds to see through `FsAbbrev` becomes a shared
helper, since both passes resolve the same names.

## Eligibility

A member expands when every condition holds:

- it is a module-level exported function member;
- exactly one parameter resolves to a `U_n`;
- that union's arm count is at most the configured cap;
- the arms are pairwise distinct as F# signatures after mapping;
- no synthesised signature collides with an existing member of the same name, including the
  TypeScript-declared overloads that survived dedupe.

A collapsing arm set disqualifies the whole member. `U2<string, string>` yields two identical
signatures and FS0041 at every call site, and a partial arm set would be an API whose shape
depends on which arms happened to survive.

## Configuration

```json
{ "unionArmOverloads": { "enabled": false, "maxArms": 4, "policy": "single" } }
```

`enabled` defaults to `false`. Every generated binding is a stable artefact, and enabling this
by default rewrites call resolution for 59 corpus members in one release; the default flips in a
later wave, once the lab fixtures and the compile gate have run against it.

`maxArms` defaults to 4.

`policy` accepts `"single"`. `"linear"` is reserved for the second landing and rejected until
then, so the later feature is a new case in an existing union rather than a schema change.

`Xantham.Cli`'s `schema` command regenerates `xantham.schema.json` from the config type.

## Findings

A new union `ExpandUnionArms` in `Findings.fs`, append-only like the rest, keyed by prefix and
case position. Cases cover the expansion itself (arm count, parameter) and each way a member is
refused: arms collapsed, cap exceeded, signature collided.

The refusal cases carry the weight. A consumer who enables the feature and finds `animate`
unchanged reads the manifest to learn that the union has six arms against a cap of four.

An expanded member keeps the grade its union already earned. Arm overloads add a call path
rather than recovering fidelity, and `Findings.test.fs` snapshots the table.

## Testing

`tests/fixtures/union-arm-overload-lab` carries one `.d.ts` exercising the four predicate
outcomes: a single-union export, a delegate arm, a collapsing arm set, an over-cap union.

Because the feature is disabled by default, the landing's golden diff is confined to the lab
that opts in. The compile gate proves the synthesised overload set compiles. The delegate arm
takes a `RunGate` check as well: that a lambda infers is a compile fact, and that the callback
is invoked at the right arity is not.

## Out of scope

Class methods, constructors, interface abstract members and synthesised `Create` factories stay
out of the first landing. The interface probe above is green, and one green probe is not a
guarantee across Fable's member-mangling configurations; each position earns its own lab.

Policy `"linear"` - expanding each union parameter in turn while holding the others at union
type - ships later, against the 2 corpus members that carry more than one union parameter.

## Interaction with mixed unions

Paired record: `docs/.ai/plans/2026-09-22-mixed-literal-unions.md`. Changes to arm composition
belong in both.

The mixed-union work sits directly upstream. §4.5 preference 1 sends an all-literal union to a
`StringEnum` before arm expansion ever sees it; the mixed-union pass extends that to unions
mixing literals with typed members, emitting a named `[<Erase(CaseRules.None)>]` DU.

**They are two mapping styles for different contexts, not rivals.** A named DU preserves the
literal set and reads well wherever a value is held, named, or matched; arm overloads read
better at a call site, especially for callbacks. Which is wanted is a property of the
consuming code, invisible to the generator — so it is a user choice expressed in config, like
`groups` dispositions already are. Our job is defensible defaults and an honest description of
the interaction, not per-union arbitration.

Where both are enabled and could claim one union, **precedence is structural and fixed**:
`classify-literal-unions` is `Passes.fs` position 5, ahead of this pass after
`dedupe-overloads`, so a union converted to a named DU is no longer a `U_n` and never arrives
here — the same mechanism that already keeps all-literal unions out. Nothing negotiates.

Neither pass should inspect the other's config flag or use sites. An earlier draft of the
mixed-union record proposed having it decline callback-bearing unions to feed this pass; that
was wrong in the shipping default (both features are `enabled: false`, so the decline would
lose the literal set to buy nothing) and it assumed an eligibility this pass does not
guarantee — six-arm unions exceed `maxArms`, and property or return positions are unreachable
from here at all. A user who enables a combination we would not have chosen gets the
documented result of that combination.

### Callbacks are where the choice actually bites

Measured 2026-09-22 (Fable 5.13.0), and it reverses the intuitive answer. A union-case
constructor gets no lambda-to-delegate coercion — that applies at method-argument position
only:

| Position | Bare lambda | Emitted JS |
|---|---|---|
| DU case, `System.Func<float,float,string>` arm | FS0002 — needs `System.Func<_,_,_>(...)` | `(a, b) => ...` |
| DU case, curried `(float -> float -> string)` arm | compiles | `(a) => ((b) => ...)` — curried, wrong arity |
| **Method parameter carrying the arm type (this feature)** | **compiles** | **`(a, b) => ...` correct** |

This is direct evidence for the claim in § *What the overloads buy* that delegate-arm
callability is the load-bearing benefit: the DU route cannot reach it from either direction.

It is therefore the clearest case for a user preferring this style: consuming code that passes
lambdas at call sites is materially better served here, and a user with a callback-heavy
package has good reason to leave the mixed-union pass off for it. The mixed-union pass does
**not** decline callback arms to make that happen — it documents the cost and lets the choice
be made in config. Both records should keep saying so; the temptation to encode a preference
in pass logic has already been written and removed once.

### The collapsing-arm exclusion is weaker than the mixed-union arm rule

This pass refuses arms that are not pairwise distinct *as F# signatures*. The mixed-union pass
refuses arms not pairwise distinct *as runtime test classes* — a strictly stronger condition,
since `float[]` and `string[]` are distinct signatures sharing one `isArrayLike` test. Anything
the mixed-union pass accepts would pass the collapsing check here; the reverse does not hold,
so neither rule substitutes for the other.

### Corpus numbers have a dependency — resolved 2026-09-22, no change

The mixed-union Step 0 gate ran and returned **zero** eligible unions across all 108 packages,
so that pass will not be built and claims nothing. The counts below stand as measured and the
`maxArms` analysis needs no recomputation. The paragraph that follows is kept for its reasoning;
its action item is discharged.



The counts in § *Corpus shape* — 484 imported static members, 59 with a union parameter, 57
with exactly one — are measured against today's union population. Every union the mixed-union
pass claims leaves that population, including some of the six-arm cluster that carries 132 of
the 239 uncapped overloads. Those figures, and the `maxArms` cap analysis resting on them,
must be recomputed once the mixed-union Step 0 measurement lands. Both features ship disabled,
so there is no release-ordering hazard — only a measurement one.
