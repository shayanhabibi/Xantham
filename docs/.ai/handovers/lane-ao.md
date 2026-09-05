---
category: Generator
audience: managing agent
title: Lane AO - the literal-union collision is unreachable; an export drop is its own loss
branch: worktree-gen-wave8-ao
base: 50fb712eba0007a59b188c2213941366449054aa
---

# Lane AO - wave eight, items 2 and 3

Both items were dispatched as "measure first, then decide". Both measurements said the same
thing: the mechanism on offer would fire zero times on the corpus. Item 2 raises nothing and
pins the boundary in the lab. Item 3 records the loss under `DO004` rather than repairing it.

## Item 2 - a union of literals never distinguishes an overload set in this corpus

### The measurement

Every dropped overload reports `DO001` once, and `dedupe-overloads` is the only pass that drops
one, so the `DO001` set is the complete set of collisions. At the base commit it held nineteen
sites, and every one is accounted for below. None is separated by a union of literals.

A second, independent measurement covers the same ground from the other side. A temporary probe
in `Spec.literalOverloadSets` dumped, for every member of every named declaration carrying more
than one call signature, each signature's per-parameter `literalErasedKey` and the literals
`literalsCarried` reads out of it. Across the whole corpus that is **230 overloaded members**,
and exactly **one parameter position** takes two or more literals:

```
Choice.pick   kind=(literal,literal)['a', 'b']
Choice.pick   kind=(literal,literal)['c', 'd']
```

`Choice` is `literal-overload-lab`'s own boundary case, and `synthesize-anonymous` names both
arm-sets, so it comes out as `Choice.Pick.Kind` / `Choice.Pick.Kind2` and separates on its own.
The probe reads through union members and type arguments to depth four, so a literal union
arriving as a type argument (`ReadOptions<"a" | "b">`) is inside the count. The probe was removed
before the first commit.

### When `synthesize-anonymous` declines to name a union

`Anonymous.isLiteralUnion`, guarding the union arm of `needsName`. A union reached by the walk
takes a name where all four hold:

1. Two or more members remain after `splitNullish` hoists `null`, `undefined` and `void`.
2. Every remaining member carries a literal.
3. The remainder is not `true | false` (`isBooleanPair`, and the `TypeFlags.Boolean` guard above
   it).
4. No already-named union carries the same non-nullish member set.

Failing (1) leaves one literal, which is lane AF's territory and already repaired. Failing (3)
gives `bool`. Failing (4) resolves the reference to the existing name through
`Spec.namedUnionByMembers`, so the position still separates. **Only (2) leaves a union widened at
a parameter position**, and it is the condition item 2 needs: one member that is not a literal.
`"a" | "b" | string` cannot reach it, because the checker subsumes literals into the primitive;
`"a" | "b" | (string & {})` survives the checker and does.

### What is pinned

`literal-overload-lab` gains `Blend`:

```ts
export interface Blend {
  pick(kind: "a" | "b" | (string & {})): void;
  pick(kind: "c" | "d" | (string & {})): void;
}
```

Both positions widen to `string`, four `TR006` fire, and the second overload drops (`DO001`).
Beside `Choice`, which names both arm-sets and keeps both overloads, the pair fixes the boundary
exactly: a *pure* literal union is already separated, and a union with one non-literal member is
the loss a per-arm-set StringEnum would have to repair.

`DO003` stays at zero and `Findings.fs` is untouched. The lab's findings test asserts
`symbolsOf "DO003" = []`, so a later lane that raises it will see this assertion fail rather than
have to rediscover the boundary.

### Cost

`DO001` 19 to 20, the widened tier 783 to 784. Both are the new lab negative. No other fixture
moves.

## Item 3 - the two export-function drops are not literal-separated

### The split of the nineteen `DO001` sites at the base commit

| category | count | sites |
|---|---:|---|
| export function, non-literal | 2 | `animejs` `$`, `mapRange` |
| type parameter constrained by `keyof …TagNameMap` against `string` | 13 | `animejs` `DrawableSVGGeometry` - `closest` 2, `getElementsByTagName` 3, `matches` 2, `querySelector` 3, `querySelectorAll` 3 |
| non-literal parameter collision | 1 | `@cloudflare/workers-types` `Ai.run` |
| identical parameters, differing return type only | 1 | `@cloudflare/workers-types` `AutoRAG.aiSearch` |
| **string-literal separated, missed by retention** | 1 | `@cloudflare/workers-types` `BrowserRun.quickAction` |
| lab negative, deliberate | 1 | `literal-overload-lab` `Widen.scan` |

The thirteen `DrawableSVGGeometry` sites carry no literal at the colliding position at all - the
probe reports an empty literal list for every signature of all five members. `Ai.run` separates
on `AiOptions & { queueRequest: true }`, a *boolean* literal inside an intersection, and its
parameter positions likewise carry no string literal. `AutoRAG.aiSearch` declares two overloads
with identical parameter lists whose returns are `AutoRagAiSearchResponse` and
`AutoRagAiSearchResponse | Response`; F# has nothing to separate.

### The two export sites

- **`$`** is `registerTargets`, three overloads over `DOMTargetsParam`, `JSTargetsParam` and
  `TargetsParam`. `DOMTargetsParam` and `TargetsParam` both abbreviate `obj`, `normalize` sees
  through both, and the third drops. `JSTargetsParam` is a `U2` and survives.
- **`mapRange`** is `typeof numberUtils.mapRange & ChainedMapRange`, whose parameters are all
  `float`.

Neither carries a literal, so extending lane AF's retention to export functions repairs nothing.
**Option (b).**

### What landed

`Overloads.dedupeOverloads` reports an `ExportFunction` drop as `DO004`
(`DO.ExportFunctionOverloadDropped`, widened, no payload). An `ExportConstructor` drop keeps
`DO001`, which is what the pre-declared case name commits to; the corpus carries no export
constructor drop, so the choice moves no count today.

Evidence is a lab pair, both required because the two prove different halves:

- `literal-overload-lab` gains `export function emit(kind: "start"): void` against
  `emit(kind: "stop")`. A literal *does* tell these apart, and they still collide - which is the
  claim, since retention reads `facts.Members` of a declaration and an exported function has no
  declaration to read.
- A `Shape.test.fs` unit test over `dedupeOverloads` with an `ExportFunction` pair and an
  `ExportConstructor` pair, asserting `[Widened, "DO004", "emit"; Widened, "DO001", "make"]`.

### Cost

`DO001` 20 to 18, `DO004` 0 to 3 (`$`, `mapRange`, `emit`). The widened tier 784 to 785, which is
the lab's new `emit` symbol. `animejs` moves two lines of `symbols.jsonl` and nothing else;
`@cloudflare/workers-types`, `solid-js` and `type-fest` do not move at all.

## Measurements

Base `50fb712`, final `a66d512`.

| key | base | after item 2 | after item 3 |
|---|---:|---:|---:|
| `DO001` | 19 | 20 | **18** |
| `DO002` | 4 | 4 | 4 |
| `DO003` | 0 | 0 | **0** |
| `DO004` | 0 | 0 | **3** |
| `TR056` | 32 | 32 | 32 |

Tiers over the whole corpus:

| | base | final |
|---|---|---|
| exact | 488 | 488 |
| ergonomic | 1544 | 1544 |
| widened | 783 | **785** |
| escape | 193 | 193 |

Full `dotnet fsi build.fsx -- test` green at `a66d512`: **463 generator tests** (460 at base, plus
two `Pipeline.test.fs` cases and one `Shape.test.fs` case), **90 wire tests**, compile gate built,
Fable run gate **249 checks** (unchanged - neither item is a runtime claim, and the compile gate
already proves the form). `git status` clean after the run.

## One thing this lane found and did not fix

`@cloudflare/workers-types` `BrowserRun.quickAction` is a **genuine literal-separated loss that
lane AF's retention misses**, and it is repairable. Nine overloads, each `action: "<literal>"`
plus its own options type; eight survive and `markdown` drops. The cause:

```ts
type BrowserRunContentOptions  = BrowserRunCommonOptions & BrowserRunAlternateBackendOptions;
type BrowserRunMarkdownOptions = BrowserRunCommonOptions & BrowserRunAlternateBackendOptions;
```

The checker gives these **two distinct type ids** (1778 and 1780 in the run probed), so
`literalErasedKey` produces two different keys and `literalOverloadSets` never forms the
colliding group. `dedupe-overloads` collides them anyway, because `BrowserRunMarkdownOptions`
renders as `type BrowserRunMarkdownOptions = BrowserRunContentOptions` and `normalize` sees
through the abbreviation.

The gap is general: **the retention analysis groups by type id, and deduplication compares
normalized F# signatures.** Wherever two ids shape to one F# type, retention declines a
separation deduplication then performs. Repairing it means keying the group nearer the shaped
form, which is a change to lane AF's mechanism in `Shape/Spec.fs` and outside both of this lane's
items. Handing it over rather than taking it: it would repair one `@cloudflare/workers-types`
overload today and it moves `DO002`/`TR056` on a path this lane did not measure.

## Files touched

- `src/Xantham.Generator/Shape/Overloads.fs` - the export-member branch of `dedupeOverloads`.
- `tests/fixtures/literal-overload-lab/index.d.ts` - `Blend`, `emit`.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` - two cases, and the `DO001`/`DO003`
  assertions in the existing findings case.
- `tests/Xantham.Generator.Tests/Shape.test.fs` - one case.
- `tests/Xantham.Generator.Tests/golden/literal-overload-lab/`,
  `tests/Xantham.Generator.Tests/golden/animejs/symbols.jsonl`.

Untouched, as briefed: `Findings.fs`, `Render.fs`, `Model.fs`, `Shape/Callbacks.fs`,
`Shape/Spec.fs`. `TR057` left unraised.
