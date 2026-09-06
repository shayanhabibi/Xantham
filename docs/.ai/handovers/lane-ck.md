# Lane CK — splitting `TR006`'s 302 before pricing either half

Read-only lane. No change under `src/` or `tests/`. Base: `8d3a4fa`.

## The counts

`TR006` (`TR.StringLiteralToString`) fires 302 times corpus-wide. It splits into three
buckets, not two — the dispatch's two-way split (single-literal member vs. literal-union
member) turns out to be the wrong shape for what actually reaches this code, and a third
bucket is real and non-trivial:

| Bucket | Count | What it is |
|---|---:|---|
| **A — single-literal site** | **268** | The referenced type is exactly one string literal, never a union, at a member or overload-parameter position. |
| **B — pure literal-union member** (`kind: "user" \| "system" \| "tool"`) | **0** | Confirmed absent. See mechanism below. |
| **C — literal(s) folded into a wider union** | **34** | A union containing one or more literal arms plus at least one non-literal arm (a real type, or the `T \| (string & {})` autocomplete idiom), or a pure literal union reached at a *top-level alias's own declaration*, outside the path that would otherwise name it. |

268 + 0 + 34 = 302.

Per-fixture assignment (all fixtures that carry `TR006`; `@cloudflare/workers-types` and
`shared-tag-lab` counted apart from each other per the dispatch's own caution):

| Fixture | Total | A | C |
|---|---:|---:|---:|
| `@cloudflare/workers-types` | 227 | 223 | 4 |
| `shared-tag-lab` | 27 | 27 | 0 |
| `animejs` | 20 | 0 | 20 |
| `literal-overload-lab` | 8 | 6 | 2 |
| `solid-js` | 6 | 5 | 1 |
| `type-fest` | 4 | 0 | 4 |
| `phase-b-lab` (dir name `lab`) | 2 | 2 | 0 |
| `intersection-empty-lab` | 2 | 0 | 2 |
| `keyof-lab` | 2 | 1 | 1 |
| `nominal-lab` | 2 | 2 | 0 |
| `flags-lab` | 1 | 1 | 0 |
| `inherit-lab` | 1 | 1 | 0 |

Of the `@cloudflare/workers-types` bucket-C 4, one (`ChatCompletionToolChoiceOption`) is
confirmed a mixed union (`U4<string, ChatCompletionToolChoiceAllowedTools,
ChatCompletionToolChoiceCustom, ChatCompletionToolChoiceFunction>` in the golden — a literal
arm collapsed alongside three distinct named types). The other three
(`DurableObjectRoutingMode`, `ToolChoiceOptions`, `WorkflowStepSensitivity`) widen to plain
`type X = string` in the golden, which is consistent with either a single literal (bucket A)
or a pure top-level literal-union alias missed by naming (the `ConcreteBranch` pattern below,
bucket C) — the golden's F# output cannot tell these apart, and the package's `.d.ts` is not
vendored in this repo to check directly. I have placed all three in bucket C on the strength
of the naming convention (`ToolChoiceOptions`, `WorkflowStepSensitivity` read as enumerations,
not single tags) but this is the one figure in this table I could not fully verify — hand it
back if it matters to the price.

## Why each site falls where it does

**The literal-union naming mechanism (why bucket B is empty).** A union whose every
non-nullish member is a string literal gets a synthesized name — either by matching an
existing named type via `namedUnionByMembers` (`src/Xantham.Generator/Shape/Spec.fs:144`), or
by `synthesize-anonymous` minting one nested under its owning declaration — *before* the
member or parameter position is shaped. Once named, the reference resolves through
`model.DeclNames` and never reaches the `TypeFlags.StringLiteral` branch that raises `TR006`
(`Shape/Spec.fs:1252-1256`); it becomes an `FsStringEnum` case elsewhere in
`Shape/LiteralUnions.fs`. `literal-overload-lab`'s `Choice` interface is the fixture's own
negative proof: `pick(kind: "a" | "b")` / `pick(kind: "c" | "d")` never appears in `TR006` at
all — its golden shows `SY.NameNestedUnderOwner` naming `Choice.Pick.Kind` and
`Choice.Pick.Kind2` at the `exact` tier instead. **Test for bucket B: is the reference a union
whose every non-nullish member is a string literal? If yes, it is not in this corpus's `TR006`
population at all** — go look at `SY.NameNestedUnderOwner` / the `LiteralUnions.fs` output
instead of at `TR006`.

**Bucket A — single-literal, no union anywhere in the reference.** The referenced type carries
exactly one literal and nothing else (`TypeFlags.StringLiteral`, no `TypeFlags.Union`
wrapper). Two shapes produce this in practice:
- A plain discriminant field on one variant of a union that never folded into a tagged union —
  `shared-tag-lab`'s 27 and `phase-b-lab`'s 2 are exactly this: each variant interface's own
  `type`/`kind`/`channel`/`state` field is a lone literal, independent of whether
  `TaggedUnions.fs`'s fold (`detect-tagged-unions`, `Shape/TaggedUnions.fs`) later succeeds for
  the union that owns it. `@cloudflare/workers-types`'s pattern (`.type`, `.role`,
  `.sensitive`, the `Rpc.___*_BRAND` markers) is the same shape at real-world scale — 216 of
  its 223 A-sites are single discriminant tags by this naming convention (spot-checked a
  sample; not read exhaustively, per the fixtures rule).
- A parameter position in an overload set where the literal fails the retention test below —
  `literal-overload-lab`'s `Solo.tag`, `Label.kind`, `Mixed.send` (×2), `emit` (×2), and
  `@cloudflare/workers-types`'s `BrowserRun.quickAction` (7 overloads, verified in the golden:
  9 signatures total, 2 keep a named literal type, 7 widen to plain `string` because each
  overload's *other* parameter also differs, so none of them collide once literals are
  erased — see `Test` below).

**Test for bucket A: strip every union wrapper from the reference — is what's left exactly one
literal?** If yes, and no sibling overload of the same member collides with it once literals
are erased (see the `TR056` mechanism), it is bucket A.

**Bucket C — a literal folded into a wider, non-pure union.** Two distinct sub-shapes, both
landing here because neither is a pure literal union and neither is a lone literal:
1. **The autocomplete/brand idiom**, `"a" | "b" | (string & {})`: the object operand of the
   intersection contributes nothing, so the reduction described in
   `intersection-empty-lab/index.d.ts` leaves a union of two literals plus bare `string`; every
   literal arm individually widens to `string` via the same `TR006` branch, and
   `erasedUnionRef`'s `List.distinct` (`Shape/Spec.fs:1971`) collapses the now-identical
   `string` arms into one. `type-fest`'s `LiteralUnion<T, string>` helper (referenced directly
   in that fixture's own comment) produces the same shape in `PackageJson.cpu` /
   `PackageJson.os` / `PackageJson.homepage` / `PackageJson.Funding.type`.
2. **A literal alongside a genuinely distinct other type**, not a brand: `animejs`'s
   `EasingParam` golden reads `U4<string, EasingFunction, Spring, TweakRegister>` — several
   named easing-preset literals collapsed to the one `string` arm, standing beside three
   distinct callable/class arms that keep their own identity. `StaggerParams.from`
   (`U3<float, string, float[]>`), `ScrambleTextParams.from` (`U2<float, string>`), and
   `ScrollObserverParams.axis` are the same pattern. `literal-overload-lab`'s `Blend.pick`
   (`"a" | "b" | (string & {})` in one overload, `"c" | "d" | (string & {})` in the other) is
   the lab's own instance of sub-shape 1, documented as: *"a union only where every non-nullish
   member is a literal [gets named]; this one keeps no name, widens to string at both
   positions."*
3. **A pure literal union at a top-level alias's own declaration**, missed by the naming
   mechanism that protects nested/member-position unions. `keyof-lab`'s `ConcreteBranch`
   (`Options extends { duration: number } ? "yes" : "no"`) is the confirmed instance: the
   conditional resolves to a two-member literal union, but — unlike `Choice`'s inline
   parameter-position union — this union is *itself* the alias's whole declared body, reached
   through the `shape-aliases` pass rather than through a member or parameter position. It does
   not self-name through `Map.tryFind facts.Response.Id model.DeclNames`
   (`Shape/Spec.fs:1946`), and nothing else offers it a name, so it falls to `erasedUnionRef`
   and widens like any other multi-arm union. I did not chase why the self-name lookup misses
   it — that is a question for whoever owns `Shape/Spec.fs`'s `unionRef`, not something this
   read-only lane should resolve by inference.

**Test for bucket C: strip nullability, then look at the remaining union's arms. If more than
one *distinct* F# type survives the arm collapse (including a bare `string` standing for one
or more collapsed literals), it is bucket C — regardless of whether the union is a top-level
alias or a member's type.**

## Pricing each half

**Bucket A (268 sites) is not a retention question at all — it should not be priced as one.**
A lone literal at a member or unmatched-overload position has no sibling to distinguish itself
from; there is nothing for a retention mechanism to *keep it apart from*. The only way to
"fix" these is to mint a one-case named type for every such site (a `[<StringEnum>] type Kind =
Case "user"` with a single case), which is a real F# shape but changes 268 sites' emitted
surface for a payoff of documentation rather than disambiguation — nothing currently written
against these members would behave differently. I read this as: **do not price bucket A as a
retention feature.** If the corpus's real complaint is "268 nearly-identical widened findings
is noisy," the cheaper fix is downgrading the *report*, not changing the *generator* — e.g.
folding lone-literal-member widenings into a single summary line per declaration the way
`shared-tag-lab`'s fold summarizes fold failures, rather than one `TR006` per field.

**Bucket C (34 sites) is a tagged-union/literal-union pass question, not a literal-retention
one — my judgement is it belongs to `Shape/LiteralUnions.fs` and `Shape/TaggedUnions.fs`, not
to a new retention mechanism next to `TR056`.** Reasoning:
- Sub-shape 1 (autocomplete/brand, `~26` sites: `animejs`'s 20 minus its non-brand arms,
  `type-fest`'s 4, `intersection-empty-lab`'s 2) is already a *named, documented, accepted*
  reduction — `docs/plans/generator-type-mapping.md` §4.6 and `intersection-empty-lab` exist
  specifically to pin this loss. Pricing it as new work would be re-opening a decision the
  corpus has already made, not discovering one. If it is ever revisited, `LiteralUnions.fs` is
  the right file: it already builds `StringEnum` DUs from literal arms, and would need to grow
  a case that keeps the enum plus adds a fallback string arm — a real `FsErasedUnion` change,
  not a `retainedLiteral`-style single-name lookup.
- Sub-shape 2 (literal beside a distinct other type, `~7` sites, mostly `animejs`) is a
  genuine information loss (the callback-vs-preset-name distinction), but it is exactly the
  discriminated/tagged-union shape `TaggedUnions.fs` targets when the arms are objects — this
  data is scalar-plus-callback, so it is closer to `LiteralUnions.fs`'s territory once that
  pass is taught to keep a literal enum arm inside an erased union rather than collapsing it to
  `string`.
- Sub-shape 3 (top-level pure literal-union alias missed by self-naming, `1` confirmed +
  `0-3` unverified in `@cloudflare/workers-types`) is the cheapest fix of the three, if it is a
  fix at all: it looks like a gap in `unionRef`'s existing self-name lookup rather than a new
  mechanism. Confirm the `ConcreteBranch` shape reproduces on a small lab fixture and hand it
  to whoever owns `Shape/Spec.fs` next — this is a two-line-diff-shaped bug, not a pass design
  question.

**What retention would cost if someone insists on it for bucket C anyway:** `LiteralUnions.fs`
would need an `FsErasedUnion` arm that is itself a `[<StringEnum>]`, plus a widening decision
per remaining non-literal arm (same as today). That's a new case in the union-shaping match,
not a new finding mechanism — the finding table doesn't need a new code, `TR006` on the
collapsed literal arms would simply stop firing for the sites that keep their enum.

## What `TR056`'s 34 sites establish

`TR056` (`retainedLiteral`, `Shape/Spec.fs:1033-1041`, fed by `literalOverloadSets`,
`Shape/Spec.fs:901-997`) keeps a literal only under three conditions, all evaluated **per
overload group of one member name**:
1. The member has more than one call signature (`memberFacts.CallSignatures.Length > 1`).
2. Those signatures fall into a group that collides once every literal is erased from every
   parameter (`literalErasedKey`) — i.e., a group F# would otherwise reject as a duplicate
   overload.
3. Within that colliding group, at least one parameter position has, on every signature, *at
   most one* literal (`lone`, `literals.Length <= 1` — this is what excludes a literal-union
   parameter: a union carries more than one literal, so `lone` is false and the whole position
   is skipped), and the signatures *disagree* on which literal it is (`distinct > 1`).

This mechanism is a poor fit for the two halves this lane split apart, and the fit is the
point:
- It cannot reach bucket A's non-overload sites at all — most of bucket A (plain discriminant
  fields, `Solo.tag` with a single signature) never has a second signature to collide with, so
  `literalOverloadSets` never considers it. Reusing `TR056`'s machinery for bucket A would mean
  discarding its central premise (a *pair* of signatures agreeing on shape and disagreeing on
  one literal) — cheaper to leave bucket A alone than to bend this mechanism to fit it.
- It explicitly **excludes** bucket B/C's literal-union parameters by construction (`lone`
  check) — this is not an oversight to patch, it is the documented boundary: *"A union of
  literals is left alone: one literal type stands for `\"a\"`, and for `\"a\" | \"b\"` there is
  none."* (`Shape/Spec.fs:943-944`). Reusing `TR056` for bucket C would mean weakening `lone`
  to admit unions, which changes what an overload-disambiguating position means and is exactly
  the kind of change that needs the manager's/`Spec.fs`-owner's sign-off, not a silent
  extension from a read-only lane.

**Conclusion: `TR056`'s mechanism does not generalise to either half. It solves a narrower
problem (a literal that tells apart two otherwise-identical overloads) than either "a lone
literal exists" (bucket A) or "a literal is one arm of a union" (bucket B/C). Reuse would cost
more than either half's own fix.**

## Reproducers

**Bucket A — single-literal member, no overload, nothing to disambiguate:**

```typescript
export interface Message {
  kind: "user";
}
```

**Bucket C — literal folded into a union carrying a non-literal arm (the accepted
autocomplete idiom, already pinned by `intersection-empty-lab`):**

```typescript
export type Ease = "in" | "out" | (string & {});
export declare const ease: Ease;
```

**Bucket C, sub-shape 3 — a pure literal union that misses the naming path because it is a
top-level alias's own body rather than a member/parameter reference:**

```typescript
export interface Options {
  duration: number;
}
export type ConcreteBranch = Options extends { duration: number } ? "yes" : "no";
```

## Unexplained, handed back

`unionRef`'s self-name lookup (`Map.tryFind facts.Response.Id model.DeclNames`,
`Shape/Spec.fs:1946`) does not find `ConcreteBranch`'s own declared name for its
conditionally-resolved literal union, even though the alias is a top-level named declaration.
I did not trace why — `Shape/Spec.fs` is not this lane's file, and this is a pointer, not a
fix. If bucket C's sub-shape 3 is worth pricing, start there.
