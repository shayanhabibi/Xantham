---
category: Generator
audience: managing agent
title: Lane AP - retention keyed by the F# type; a keyof bound is a loss of its own
branch: worktree-gen-wave9-ap
base: f7d6c170249950b1ab1f31a1b7bf2e121c66b2ea
---

# Lane AP - wave nine, items 1 and 2

Item 1 is a repair: the literal-retention analysis grouped by the checker's type id while
`dedupe-overloads` compares normalized F# signatures, so a separation was declined and then a
deduplication was performed on the same pair. Item 2 is a decision taken on three measurements,
and it goes to **(b)**: the thirteen `DrawableSVGGeometry` drops are an accepted loss and now
report `DO005`.

## Item 1 - `BrowserRun.quickAction` keeps all nine overloads

### The site, re-confirmed at the base commit

`@cloudflare/workers-types` `BrowserRun.quickAction` declares nine overloads, each
`action: "<literal>"` plus its own options type. At `f7d6c17` the golden carried eight, every
one written `action: string`, and `symbols.jsonl` reported one `DO001` against
`BrowserRun.quickAction`. Lane AO's account holds exactly.

### The mechanism

```ts
type BrowserRunContentOptions  = BrowserRunCommonOptions & BrowserRunAlternateBackendOptions;
type BrowserRunMarkdownOptions = BrowserRunCommonOptions & BrowserRunAlternateBackendOptions;
```

Both distribute into the same two-arm union, and the checker hands out two ids for it. The two
ids reach **one** F# type by two different routes, and `Spec.unionRef` /
`Shape/Aliases.fs` between them make it unavoidable:

- an id not in `DeclNames` resolves through `namedUnionByMembers`, which compares non-nullish
  member sets;
- an id that *is* in `DeclNames` is written by `shape-aliases` through `typeRefIgnoringSelf`,
  which takes the same route and emits `type BrowserRunMarkdownOptions = BrowserRunContentOptions`.

`dedupe-overloads` follows that abbreviation in `normalize` and collides the pair.
`literalErasedKey` bottomed out at `string typeId`, so the group never formed and the `action`
literal was widened away at every position.

### The repair

`Spec.shapedKey` replaces the `string typeId` leaf. A type carrying no literal keys by the F#
type it shapes to: a union of two or more non-nullish members keys by that member set, with a
marker for the hoisted nullish part, because that part is the `option` wrapper. Everything else
keys by id as before.

The key is exactly the equivalence `namedUnionByMembers` induces, so it is a refinement of the
old key rather than a coarsening: it never groups two positions that shape to different F#
types, and no fixture outside `@cloudflare/workers-types` moved.

### Cost

`DO001` 18 to 17, `DO002` 4 to 5, `TR056` 32 to 34, `TR006` 1208 to 1206. The exact tier 488 to
490, which is `BrowserRun.Content` and `BrowserRun.Markdown` - two single-case StringEnums. The
golden gains 172 lines, all of them the restored `markdown` overload and its documentation.

## Item 2 - the thirteen `DrawableSVGGeometry` drops are `DO005`

### What the bound actually is

The brief and lane AO both describe these as "constrained by `keyof …TagNameMap`". At the
shape, they are not. A probe over `closest` reported its four signatures binding type
parameters 1828-1831 whose constraints carry:

```
tp=1828 cons=2768 flags=Union
tp=1829 cons=2664 flags=Union
tp=1830 cons=2460 flags=Union
tp=1831 cons=137  flags=Object      // the `E extends Element` overload
```

The checker expands a `keyof` over a closed operand into its union of literal keys before the
run sees it. The three unions hold **112, 63 and 31** string literals - the HTML, SVG and MathML
tag-name maps - and the five-signature members carry a fourth of **29**, the deprecated HTML map.
That is why lane AO's probe reported an empty literal list: it read `literalsCarried` off the
*parameter* type, and every literal here sits one level away, on the type parameter's bound.

### Why the bound cannot separate the overloads

Two measurements, both negative.

1. **A constraint is not part of a method signature.** Compiled directly:

   ```fsharp
   type IProbe =
       abstract closest<'K when 'K :> System.IComparable> : selector: 'K -> obj option
       abstract closest<'K when 'K :> System.ICloneable> : selector: 'K -> obj option
   ```

   `error FS0438: Duplicate method`. Writing the bound moves nothing, whatever the bound is.

2. **`keyof<'T>` has no operand to stand over.** `Spec.keyOfRef` writes the support package's
   idiom over a type *variable* in scope. Here the operand is a lib.dom interface, and
   `BrowserBindingTable.generated.fs` carries no `TagNameMap` row among its 452 - so
   `HTMLElementTagNameMap` has no F# name at all.

### Scope, measured over the whole corpus

A probe listed every member of every named declaration binding two or more type parameters over
a literal-union bound. Across all fixtures it reports **five members of one declaration in one
package**: `DrawableSVGGeometry` in `animejs` - `closest`, `getElementsByTagName`, `matches`,
`querySelector`, `querySelectorAll`. Nothing else in the corpus is near this shape.

### The verdict, and the repair that does exist

**(b).** The overloads are an accepted loss and report `DO005`
(`DO.KeyofConstrainedOverloadDropped`, widened, carrying the parameter name).

There *is* a repair, and it is not this one. Substituting the bound's literal union for the
erased type parameter would separate the overloads, because it makes the parameter types differ:
`find(selector: HtmlTags.Keys)` against `find(selector: SvgTags.Keys)`. That is lane AO's
`DO003` mechanism - a StringEnum per literal arm-set - applied to a bound rather than to a
parameter, and lane AO deliberately left `DO003` unraised with an assertion in
`literal-overload-lab` that will fail when someone takes it up. Its cost here is measured:
**235 StringEnum cases across four bounds**, materialized into an animation library's binding,
for members `animejs` inherits from `SVGGeometryElement` and whose returns are `obj` either way.
That is a decision about `DO003`, and it belongs to whoever takes `DO003`.

### How the drop is recognised

Two halves, because the finding's name has to stay true.

- `Spec.keyBoundedOverloads` reads the wire model: a member whose call signatures bind two or
  more type parameters over a key set - an open `keyof` (`TypeFlags.Index`) or the union of
  string literals a closed one expands to.
- `Overloads.keyErasedParameter` reads the F# member: a parameter taking a type parameter that
  reached F# with no constraint written.

Both must hold. `Finder.pick` in the lab is the negative for the second: a nominal bound *is*
written (`'T :> Div`), the overloads collide anyway, and the drop stays `DO001`.

### Evidence

`tests/fixtures/keyof-overload-lab/`, registered in `Pipeline.test.fs`:

```ts
export interface Finder {
    find<K extends keyof HtmlTags>(selector: K): HtmlTags[K];
    find<K extends keyof SvgTags>(selector: K): SvgTags[K];
    find(selector: string): Div;

    pick<T extends Div>(value: T): void;
    pick<T extends Span>(value: T): void;
}
```

Its golden writes `abstract find<'K>: selector: 'K -> obj` beside
`abstract find: selector: string -> Div`, and the two test cases assert `DO005 = [Finder.find]`
against `DO001 = [Finder.pick]`.

### Cost

`DO001` 17 to 5, `DO005` 0 to 14 - thirteen `animejs` sites and the lab's `Finder.find`. The
`animejs` binding does not move at all; one line of its `symbols.jsonl` does, which is the
finding changing its name. The ergonomic tier 1544 to 1548 and the widened tier 785 to 786 are
the lab's own five declarations.

## Measurements

Base `f7d6c17`, after item 1 `1fbeef0`, final `HEAD` of `worktree-gen-wave9-ap`.

| key | base | after item 1 | after item 2 |
|---|---:|---:|---:|
| `DO001` | 18 | 17 | **5** |
| `DO002` | 4 | **5** | 5 |
| `DO004` | 3 | 3 | 3 |
| `DO005` | 0 | 0 | **14** |
| `TR056` | 32 | **34** | 34 |
| `TR006` | 1208 | **1206** | 1206 |

Tiers over the whole corpus:

| | base | after item 1 | final |
|---|---:|---:|---:|
| exact | 488 | **490** | 490 |
| ergonomic | 1544 | 1544 | **1548** |
| widened | 785 | 785 | **786** |
| escape | 193 | 193 | 193 |

Every move is accounted for: the exact pair is item 1's two retained literals, and the five
symbols added at item 2 are `keyof-overload-lab`'s own declarations - `Div`, `Span`, `HtmlTags`
and `SvgTags` ergonomic, `Finder` widened.

Full `dotnet fsi build.fsx -- test` green at both commits: **467 generator tests** (463 at base,
plus the lab's golden case and two `Pipeline.test.fs` cases), **90 wire tests**, compile gate
built, Fable run gate **249 checks** unchanged - the lab has no runtime behaviour to prove and
the compile gate already decides the form. `git status` clean after the run.

## Files touched

- `src/Xantham.Generator/Shape/Spec.fs` - `shapedKey`, `keyBoundedOverloads`.
- `src/Xantham.Generator/Shape/Overloads.fs` - `DO005` at the method drop.
- `tests/fixtures/keyof-overload-lab/` - new.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` - one `fixtureTests` block, appended after
  `literal-overload-lab`'s. This is the append point the fixtures rule names; keep both sides on
  a conflict.
- Goldens: `@cloudflare/workers-types`, `animejs/symbols.jsonl`, `keyof-overload-lab` (new).

Untouched, as briefed: `Findings.fs`, `tests/fixtures/callback-function-lab/`,
`tests/Xantham.Generator.RunGate/`.

## Nothing was left unexplained

No fixture moved that this lane did not target, and no count moved in a direction the sections
above do not account for.
