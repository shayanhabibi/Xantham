---
category: Generator
title: Plan - the @types/three rung
---

# `@types/three` as a litmus rung — reconnaissance

**Status (2026-09-02): do not land.** The generator survives `@types/three` and produces output,
but the output is 128 MB, is not byte-deterministic, does not compile, and 76.5% of it is the
product of an unbounded instantiation walk rather than the package's real surface. This document
records what was measured, reduces each blocker to the smallest `.d.ts` that reproduces it, and
orders the work that would have to land before the rung can.

`tests/fixtures/pins.json` is deliberately **not** amended. A pin is a promise that the fixture is
gated; nothing here is gateable yet. `tools/xantham-fixtures.fsx` already lists `@types/three`, so
the fixture installs unpinned for anyone who wants to reproduce this.

---

## 1. Provenance

Everything below came from a run, and every number came from a script over the artefacts rather
than from reading them. Neither the 482k-line rendered file nor the 367k-line manifest was opened
whole or paged through, per `.claude/rules/generator-fixtures.md`; the incantations used are in
[appendix A](#appendix-a--how-each-number-was-taken).

| | |
|---|---|
| package | `@types/three@0.185.4`, installed `--save-exact` under `tests/fixtures/@types/three/` |
| input size | 924 `.d.ts` files, 53,460 lines, 3.7 MB |
| entry | `index.d.ts` re-exporting `./src/Three.js` |
| config | `GeneratorConfig.Default` — no `xantham.json`, no `Lib`, no `Groups` |
| compiler | `node_modules/@typescript/typescript-win32-x64/lib/tsc.exe`, borrowed from the main checkout via `tools/workspace.fsx`; `XANTHAM_REQUIRE_TSC=1` |
| harness | scratch `.fsx` calling `Pipeline.generate config packageDir` twice in one process, outside the worktree, not committed |
| baseline | `master` @ 4f08945 |

`src/Xantham.Generator/` was not modified. Two source sites are cited below; neither was touched.

---

## 2. Does it survive the package?

**Yes.** Generation completes with no crash and no API-server refusal — the `type-fest`
`PositiveInfinity = 1e999` class of failure does not recur here. 1,469 symbols are harvested,
1,253 declarations are rendered, 51,033 findings are emitted.

The `unexplainedDrops` invariant would hold: `audit-coverage` emits exactly one finding (`AC001`),
against `EventListener`, and that symbol carries findings from other passes, so it is an explained
drop rather than a silent one.

Survival is the only one of the eight questions that comes back clean.

---

## 3. Where the time goes, and the depth cutoff

76,059 ms end to end. Per-tier, measured by calling `Bootstrap.start` and then `Pipeline.runTier`
tier by tier with a stopwatch around each, and compared against the two nearest existing rungs run
through the same harness:

| tier | `@types/three` | `@cloudflare/workers-types` (`lib=esnext`) | `type-fest` |
|---|---:|---:|---:|
| bootstrap | 129 ms | 99 ms | 108 ms |
| harvest | 81 ms | 82 ms | 52 ms |
| resolve | **17,082 ms** | 1,017 ms | 216 ms |
| shape | **57,069 ms** | 1,316 ms | 132 ms |
| render | 1,697 ms | 232 ms | 79 ms |
| **total** | **76,059 ms** | 2,746 ms | 587 ms |
| type-table entries | **71,833** | 9,693 | 1,800 |
| types not followed | **10,461** | 261 | 0 |
| declarations | 1,253 | 1,677 | 299 |
| findings | **51,033** | 9,563 | 1,670 |

Shape is 75% of wall clock and resolve is another 22%; render and the two front passes are noise.
The cost is not the package — `three` produces *fewer* declarations than `workers-types` from 30x
the source — it is the type table. 71,833 entries for 1,253 declarations is 57 resolved types per
emitted declaration, against 5.8 for `workers-types`.

**The depth cutoff fires hard.** `FollowDepth = 12` (`src/Xantham.Generator/Resolve.fs:14`), and
the single `RT001` finding recorded against `<type-table>` reads:

> `RT001 | widened | 10461 types not resolved: beyond the depth cutoff (12) - the frontier of
> instantiations still growing after that many generations`

10,461 against `workers-types`' 261. The frontier is not a tail here; it is the shape of the run.
Downstream, 5,698 `TR002` findings ("type not resolved (beyond the depth cutoff (12)); widened to
obj") land on real members — every one of them a member whose type was truncated by the cutoff.
`three` is the first and only fixture that exercises this code path at all: `TR002` is 0 on every
other rung.

The cutoff is doing its job. It is what stops the run from being unbounded. What it cannot do is
stop the *names* the frontier generates from reaching the output — see §5 and blocker 1.

---

## 4. Determinism: **no**

Two `Pipeline.generate` calls in one process, files compared byte for byte:

- `Types.Three.fs` — **identical**, 134,520,603 bytes both runs.
- `manifest.json` — **differs**. Same byte length both runs; first differing byte at offset
  **669,233**; **162 differing lines**, in 162 single-line hunks.

Every one of the 162 normalises to the same string:

```
"message": "type parameter #93151 has no name to write; its uses widen to obj"   (run A)
"message": "type parameter #93022 has no name to write; its uses widen to obj"   (run B)
```

The finding is `TP001`. Its message interpolates a checker-assigned type id
(`src/Xantham.Generator/Findings.fs:273`):

```fsharp
| UnnamedTypeParameter id -> $"type parameter #{id} has no name to write; its uses widen to obj"
```

emitted from `typeParamsOf` (`src/Xantham.Generator/Shape.fs:933`) on the branch where the
parameter's id is absent from `model.Types`:

```fsharp
match Map.tryFind id model.Types |> Option.bind _.SymbolName with
| Some name -> Some(id, name)
| None ->
    findings <- findings @ [ Finding.make owner (TypeParameters.UnnamedTypeParameter id) ]
    None
```

This is the same class as the already-fixed frontier failure: **a checker id reaches a rendered
string**. The ids are assigned by the type checker in traversal order and are not stable across
runs within the same process.

**Why the parameter has no name.** All 54 symbols that carry a `TP001` also carry a `TR002` — the
correlation is total, checked by set intersection over the manifest. The parameter's own type
response fell beyond the depth cutoff, so it never entered `model.Types`, so `SymbolName` is
unavailable and the id is all the finding has to say. `TP001` is therefore strictly downstream of
the runaway in §5; the reproducer for the runaway is the reproducer for this too. Two attempts at
an independent minimal `.d.ts` for `TP001` (a bare unnamed method-level parameter, and one behind
an alias) both produced zero `TP001` and were deterministic — the finding requires the depth
cutoff to have fired, which requires the runaway.

`TP001` is 162 on `three` and 0 on every other rung, so the determinism property has never had a
chance to catch this.

---

## 5. What the golden would cost

| | `@types/three` | `@cloudflare/workers-types` |
|---|---:|---:|
| rendered `.fs` | **482,524 lines / 134,520,603 bytes (128 MB)** | 30,580 lines |
| `manifest.json` | **367,273 lines / 46,538,136 bytes (44 MB)** | 78,740 lines |
| symbols | 1,469 | 1,714 |
| findings | 51,033 | 9,563 |
| exact / ergonomic / widened / escape | **101 / 257 / 1,038 / 73** | 260 / 967 / 380 / 107 |

`three` produces *fewer symbols* than `workers-types` and a golden **15.8x larger**. It is also
the first rung where the widened tier dominates: 71% of symbols are widened, against 22% for
`workers-types`.

The size is not distributed. It is one defect:

| measurement | value |
|---|---:|
| declaration blocks | 1,253 |
| blocks whose name contains a hoisted `…Result` segment | **518** |
| lines in those blocks | **369,116 of 482,508 — 76.5%** |
| blocks >= 1,000 lines | 95 |
| blocks >= 500 lines | 366 |
| largest block | `Exports`, 3,642 lines |
| name length p50 / p90 / p99 / max | 23 / 1,363 / 1,622 / **1,689 chars** |
| names > 200 chars | 425 |
| names > 1,000 chars | 271 |
| max `…Result` segments in one name | **135** |

95 declarations are 1,550 lines each and differ only in how many times `ToVarResult` is appended
to their name: `RenderOutputNodeToVarResult`, `RenderOutputNodeToVarResultToVarResult`,
`RenderOutputNodeToVarResultToVarResultToVarResult`, and so on. Strip those 518 declarations and
the binding is roughly 113k lines — still the largest rung by 3.7x, but a plausible artefact.

This is blocker 1; its reproducer is in §9.

---

## 6. The findings profile

Rolled up by key over the manifest (top 25 of 32 keys present):

```
21352 TR032    2097 TR031     541 TR006     205 SC003      27 TR014
 5698 TR002    2053 TR018     511 SI003     184 TR035      18 SC004
 4626 TR036    1751 TR001     345 TR007     162 TP001      17 DO001
 3593 TR009    1001 TR023     297 TR008      96 TR024      10 TR004
 2866 MB003     657 TP006     257 SP001      60 MB004       6 MB002
 2330 TP002
```

Tail: `TR013` 5, `LU001` 4, `TR015` 2, `TP004` 2, and one each of `TR033`, `RT001`, `AC001`,
`RA001`, `SA002`, `TR029`.

The same script over each committed `tests/Xantham.Generator.Tests/golden/*/manifest.json` gives
the cross-rung matrix. What matters is not that `three` is bigger — it is which keys are
*qualitatively* new:

| key | message | three | workers | type-fest | solid | animejs |
|---|---|---:|---:|---:|---:|---:|
| `TR002` | type not resolved (beyond the depth cutoff (12)) | **5698** | 0 | 0 | 0 | 0 |
| `TP001` | type parameter #N has no name to write | **162** | 0 | 0 | 0 | 0 |
| `SC004` | static dropped: class declares no instance members | **18** | 0 | 0 | 0 | 0 |
| `TR001` | type refers to itself through unnamed shapes | **1751** | 0 | 7 | 0 | 0 |
| `TR031` | callback with N overloads shaped from the first | **2097** | 0 | 0 | 19 | 3 |
| `TR036` | union of N distinct types widened to obj (D4 caps at 4) | **4626** | 16 | 2 | 4 | 11 |
| `TR009` | unknown maps to obj (D8) | **3593** | 130 | 7 | 3 | 0 |
| `TP002` | constraint has no F# form and is dropped | **2330** | 75 | 144 | 47 | 4 |
| `TP006` | type parameter erased: every use widened away | **657** | 12 | 0 | 8 | 0 |
| `TR018` | intersection over a non-object operand | **2053** | 84 | 14 | 25 | 106 |
| `SC003` | settable static emitted read-only | **205** | 1 | 0 | 0 | 0 |
| `SI002` | base members flattened, is-a relation not emitted | **254** | 77 | 1 | 7 | 4 |
| `SI003` | intersection of N object types flattened | **511** | 81 | 3 | 8 | 22 |

Read as a design document rather than a scoreboard, this says four things.

**The depth cutoff and the unnamed-parameter path have never been under test.** `TR002`, `TP001`
and `SC004` are 0 everywhere else. The determinism failure in §4 lives in code no existing rung
reaches.

**D4's union cap at 4 is the wrong shape for this package.** 4,626 `TR036` against 16 on the next
rung — 289x. `three`'s node graph unions run to 5, 6, 7, 10 and 13 arms (`union of 5 distinct
types` alone is 2,306 findings); every one collapses to `obj`. This is not a bug, it is D4 working
as specified, but `three` is the fixture that shows what the specification costs at scale.

**Structural intersection is the package's idiom.** `SI002` + `SI003` = 765, `TR018` = 2,053. The
TSL node system is written almost entirely as `A & B & C`, and each flatten emits a note that the
is-a relation is dropped — which is exactly what makes the output not compile (§7, blocker 3).

**Overload-bearing callbacks are common.** 2,097 `TR031`, against 0 on `workers-types`. Callback
parameters with 2, 3 and 4 overloads are shaped from the first, silently narrowing the callable.

`three` also uses the erased-phantom path barely at all (`SA002` = 1, against 185 on `type-fest`)
— the two large rungs stress disjoint parts of the shaper.

---

## 7. Would the compile gate accept it? **No**

Method: a scratch net8.0 project mirroring `tests/Xantham.Generator.CompileGate`'s package set —
`Fable.Core 5.2.0`, `Xantham.Fable.Core`, the 23 `Fable.Browser.*` pins — compiling the rendered
file. `--maxerrors:5000` was added via `OtherFlags` because fsc's default cap of 100 truncated the
error set. Errors were deduplicated by a script over the build log, normalising quoted identifiers.

The first run stopped in 5 s on two parse errors, hiding everything downstream. To see past them
the *artefact copy* was patched — two characters, in a scratch copy of the generated file, never in
the generator — and rebuilt. The patched file then type-checks to completion in **3 m 20 s** and
yields 337 distinct diagnostic sites:

| | count | first site | representative text |
|---|---:|---|---|
| `FS0010` | 2 | `399206,95` | `Unexpected identifier in member definition. Expected ':' or other token.` |
| `FS0001` | 328 | `898,139` | `The type 'NormalBufferAttributes' is not compatible with the type 'NormalOrGLBufferAttributes'` |
| `FS0043` | 1 | `480568,19` | `A type parameter is missing a constraint 'when 'TBufferGeometry :> Three.BufferGeometry<Three.NormalBufferAttributes,Three.BufferGeometryEventMap>'` |
| `FS0193` (warn) | 8 | `481902,266` | `A type parameter is missing a constraint 'when 'TGeometry :> BufferGeometry<NormalBufferAttributes,BufferGeometryEventMap>'` |

Three distinct causes, two of them blockers; `FS0043`/`FS0193` are the same nominal-constraint
cause as `FS0001`, surfacing at the `Exports` statics.

Two secondary observations worth recording. fsc does **not** OOM on a 482k-line file — the implicit
worry about single-file scale is unfounded at this size. But 3 m 20 s of type-checking, added to a
gate that runs on every `dotnet build Xantham.slnx`, is on its own disqualifying: the gate's value
is that it is cheap enough to always run.

---

## 8. Configuration

**No `lib` setting is needed, and that is the interesting answer.** `workers-types` needs
`Lib = Some [ "esnext" ]` because it *replaces* the DOM and its own declarations lose the name
collision against the default lib. `three` *extends* the DOM, so the question had to be checked
rather than assumed. It was: 25 names that collide with `lib.dom.d.ts` were probed against the
manifest —

`Event`, `EventListener`, `BaseEvent`, `EventDispatcher`, `Source`, `Path`, `Shape`, `Cache`,
`Box2`, `Box3`, `Line`, `Plane`, `Sphere`, `Spherical`, `Controls`, `Color`, `Clock`, `Group`,
`Layers`, `Uniform`, `Curve`, `Audio`, `AudioContext`, `AnimationAction`, `Font`

— and **24 of 25 are present**. The only absentee is `Font`, which lives in `examples/jsm` and is
not reachable from the entry point at all. `three` loses nothing to lib merging. Default config,
no `Groups` disposition, is correct.

**But there is a real configuration gap, and it is not `lib`.** The module renders as
`Types.Three` and all **737** `[<Import(…)>]` attributes carry the specifier `"@types/three"` — the
only distinct specifier in the file.

`@types/three` is a types-only package. It ships no JavaScript. Every one of those 737 imports
would resolve, at Fable output time, to a package with no runtime; the correct specifier is
`three`. `GeneratorConfig` (`src/Xantham.Generator/Model.fs:36`) has `ModuleName`, `Groups` and
`Lib`, and no key that overrides the runtime import specifier — it is derived from the package
name. **Every `@types/*` rung will hit this**, so it is a generator gap rather than a `three`
quirk: DefinitelyTyped is where the majority of remaining binding demand lives.

This is blocker 5. It is the cheapest of the five and the only one that is purely additive.

---

## 9. Blockers, ordered

Ordered by what unblocks what. 1 subsumes 2; 3, 4 and 5 are independent of both and of each other.

### Blocker 1 — the instantiation runaway (76.5% of the output, and the cause of blocker 2)

`three`'s TSL layer declares a polymorphic-`this` method whose return type is an intersection
*containing* `this`. Each application produces a strictly larger anonymous type; the shaper hoists
each one to a named declaration by appending the member name plus `Result`; that declaration's own
`toVar` produces another. The depth cutoff at 12 stops the walk, but only after it has minted 518
declarations and 369,116 lines, with names up to 1,689 characters and 135 `Result` segments.

Root cause in the package, `src/nodes/core/VarNode.d.ts`:

```ts
type VarNode<TNodeType, TNode> = Node<TNodeType> & VarNodeInterface<TNode>;
declare module "./Node.js" {
    interface NodeExtensions<TNodeType> {
        toVar: (name?: string | null) => VarNode<TNodeType, this>;
        toConst: (name?: string | null) => VarNode<TNodeType, this>;
        toVarIntent: () => VarNode<TNodeType, this>;
    }
}
```

**Reproducer** — 11 declarative lines, generates 31 symbols / 30 declarations / 317 lines, names
6 levels deep:

```ts
// `three` TSL shape, reduced: a polymorphic-`this` method whose return type is an
// intersection *containing* `this`, so every application produces a strictly larger
// anonymous type.
export interface NodeExtensions<TNodeType> {
    toVar: (name?: string | null) => VarNode<TNodeType, this>;
}
export type Node<TNodeType> = { readonly isNode: true } & NodeExtensions<TNodeType>;
export interface VarNodeInterface<TNode> {
    node: TNode;
    readonly isVarNode: true;
}
export type VarNode<TNodeType, TNode> = Node<TNodeType> & VarNodeInterface<TNode>;
export declare const seed: Node<number>;
```

Emitted symbol names, verbatim from the reproducer's manifest:

```
Node, NodeToVarResult, NodeToVarResultToVarResult, NodeToVarResultToVarResultToVarResult,
NodeToVarResultToVarResultToVarResultToVarResult, …                     (6 deep)
NodeExtensions, NodeExtensionsToVarResult, …                            (6 deep)
VarNode, VarNodeToVarResult, …                                          (6 deep)
Seed, SeedToVarResult, …                                                (6 deep)
```

Counts: `{"exact":1,"ergonomic":7,"widened":23,"escape":0}`, 31 symbols. The output **compiles
clean** — this is a size and naming defect, not a legality one.

What needs deciding is not "raise or lower the cutoff". Depth 12 already costs 128 MB; depth 6
would cost less and be equally arbitrary. The shaper needs to recognise that a hoisted anonymous
shape is an instantiation of an already-named declaration and emit a reference rather than a new
name — or refuse to hoist through `this` at all and widen, taking one finding instead of 518
declarations.

### Blocker 2 — non-deterministic manifest (`TP001` carries a checker id)

Covered in §4. 162 manifest lines differ between two in-process runs; each is a `TP001` message
interpolating a checker-assigned type id. The determinism property in `Pipeline.test.fs` would fail
on this rung.

Reproducer: the same file as blocker 1 — `TP001` only fires where the depth cutoff has already
fired, so it has no smaller independent reproduction. Two candidate minimal cases (an unnamed
method-level parameter; the same behind an alias) both produced zero `TP001` and were deterministic.

The fix is the same one applied to the frontier finding: **do not put a checker id in a rendered
string.** Either name the owner instead (the symbol is known — `Finding.make owner …`), or count
these the way `RT001` counts the frontier: one aggregate finding on `<type-table>` rather than 162
individually-keyed ones.

### Blocker 3 — structural `extends` becomes nominal `:>` (328 `FS0001`, 1 `FS0043`)

TypeScript's `extends` on a type parameter is structural; F#'s `:>` is nominal. When the constraint
and the default argument are two `[<EmitIndexer>]` interfaces with no `inherit` between them, the
default does not satisfy the constraint in F# even though it does in TypeScript. `SI002` already
records the cause — "the is-a relation is not emitted" — 254 times.

Root cause in the package, `src/core/BufferGeometry.d.ts`:

```ts
export type NormalBufferAttributes = Record<string, BufferAttribute | InterleavedBufferAttribute>;
export type NormalOrGLBufferAttributes = Record<
    string,
    BufferAttribute | InterleavedBufferAttribute | GLBufferAttribute
>;
// class BufferGeometry<Attributes extends NormalOrGLBufferAttributes = NormalBufferAttributes, …>
```

Every one of the 328 sites is a use of `BufferGeometry<NormalBufferAttributes, …>` — the first at
`Types.Three.fs:898`, inside a tupled member signature, the same text 326 further times, plus one
each for `PlaneGeometry` and `CylinderGeometry` not being nominally their flattened base.

**Reproducer** — 12 declarative lines:

```ts
// `three`'s BufferGeometry, reduced: a structural `extends` constraint whose default
// argument satisfies it structurally but not nominally.
export interface Attr { readonly kind: "attr" }
export interface GLAttr { readonly kind: "gl" }

export type Narrow = Record<string, Attr>;
export type Wide = Record<string, Attr | GLAttr>;

export class Geometry<Attributes extends Wide = Narrow> {
    attributes: Attributes;
}

export declare const g: Geometry;
```

Exact failure, from a scratch gate project over the rendered `NominalLab.fs`:

```
NominalLab.fs(44,22): error FS0001: The type 'Narrow' is not compatible with the type 'Wide'
```

at the rendered line

```fsharp
static member g: Geometry<Narrow> = jsNative
```

Two directions are open, and this is the one that needs an architecture decision rather than a
patch: emit `inherit` for declared bases so the nominal relation exists (which the
"inherit for declared bases" work in flight may already give), or drop `TP002`-style constraints
from the *rendered* head when the shaper cannot prove the nominal relation, accepting the looser
signature and taking the finding.

### Blocker 4 — a type-parameter head ending in `>` swallows the member's colon (2 `FS0010`)

A method-level type parameter whose constraint is itself a generic application renders a head
ending `>>`, immediately followed by the member's colon. F# lexes `>>:` as one token, and the parse
error kills the whole build in 5 s before any other diagnostic is produced.

```fsharp
abstract intersectObject<'TIntersected when 'TIntersected :> Object3D<Object3DEventMap>>: ``object``: Object3D<Object3DEventMap> * …
```

Isolated at the F# level, independent of the generator: `abstract m<'T when 'T :> Obj<Ev>>: p: X -> Y`
is `FS0010`; the identical line with **one space before the colon** —
`abstract m<'T when 'T :> Obj<Ev>> : p: X -> Y` — compiles. It is purely a renderer spacing bug.

Root cause in the package, `src/core/Raycaster.d.ts` (`intersectObject`, `intersectObjects`).

**Reproducer** — 9 declarative lines:

```ts
// `three`'s Raycaster.intersectObject, reduced: a method-level type parameter whose
// constraint is itself a generic application, so the rendered head ends `>>` and the
// member's colon is lexed as part of it.
export interface EventMap { readonly click: { at: number } }
export interface Object3D<TEventMap> { readonly id: number }
export interface Intersection<TIntersected> { readonly object: TIntersected }

export class Caster {
    intersectObject<TIntersected extends Object3D<EventMap>>(object: Object3D<EventMap>): Intersection<TIntersected>[];
}
```

Exact failure:

```
ParseLab.fs(37,87): error FS0010: Unexpected identifier in member definition. Expected ':' or other token.
ParseLab.fs(40,1): error FS0010: Unexpected symbol '[<' in implementation file
```

at the rendered line

```fsharp
abstract intersectObject<'TIntersected when 'TIntersected :> Object3D<EventMap>>: ``object``: Object3D<EventMap> -> Intersection<'TIntersected>[]
```

This is the cheapest fix in the document — a space in the renderer when a member's type-parameter
head ends in `>` — and the highest-leverage, because it is what stops the gate from seeing anything
else. It is not `three`-specific: any package with a generic constraint on a method-level parameter
hits it, which is why it should land as a lab fixture regardless of whether `three` ever does.

### Blocker 5 (not blocking `three`, blocking every `@types/*` rung) — import specifier

Covered in §8. 737 imports point at a package with no runtime. Needs a `GeneratorConfig` key —
`RuntimePackage: string option`, defaulting to the package name with a `@types/` prefix stripped —
plus a test that a types-only package renders its runtime specifier.

**Landed (wave two, lane D).** `RuntimePackage` is the `runtime` key of `xantham.json`; unset, it
derives the specifier from the package name, and the derivation had to be DefinitelyTyped's whole
convention rather than a prefix strip: DT publishes one flat `@types` scope, so it folds a scoped
package's scope into the name (`@types/babel__core` is the types of `@babel/core`), and a
`__` therefore unfolds back to `@scope/name`. Nothing in a DT manifest states the runtime name, so
the convention is the only evidence on disk; `runtime` overrides it for anything it cannot
describe. `tests/fixtures/types-only-lab` pins it, and `SE002` reports a derived specifier once
per run — the alternative, once per import, is the same sentence 737 times on `three`. No corpus
golden moved: not one rung is a `@types/*` package, and the derivation is the identity on every
other name.

---

## 10. Recommendation

**Do not land `@types/three` as a golden rung, and do not pin it.** Four independent reasons, any
one of which is sufficient:

1. A 128 MB `.fs` and a 44 MB `manifest.json` cannot be committed to this repository. Even if they
   could, 76.5% of the binding is an artefact.
2. It is not byte-deterministic. The run-twice property would fail.
3. It does not compile, and its compile gate would cost 3 m 20 s on every build even after the
   errors are fixed.
4. Its symbol set is not the package's real surface, so a golden over it would gate the runaway
   rather than the package.

**Land the small evidence first.** Three hand-authored lab fixtures, tracked in git, each the
fewest declarations that pin one construct under the live compiler, in the model of
`intersection-lab` and `statics-lab`:

| lab | pins | status of the reproducer |
|---|---|---|
| `tests/fixtures/parse-lab/` | generic constraint on a method-level type parameter; rendered head ends `>` | built, fails as quoted (blocker 4) |
| `tests/fixtures/nominal-lab/` | structural `extends` constraint with a structurally-but-not-nominally satisfying default | built, fails as quoted (blocker 3) |
| `tests/fixtures/chain-lab/` | polymorphic `this` inside an intersection return type | built; compiles, but 11 lines produce 30 declarations (blocker 1) |

Each is under 15 lines, each fails today for exactly one reason, and together they are the whole of
what `three` taught that is actionable. `chain-lab` in particular is a *size assertion* — a test
that 11 lines of TypeScript produce a bounded number of declarations is the regression guard the
runaway needs, and it costs 317 lines of golden instead of 482,524.

**Keep `three` as a diagnostic rung, not a gated one.** It is the only fixture that exercises the
depth cutoff (`TR002`, `RT001` at 10,461), the unnamed-parameter path (`TP001`), the union-arity
cap under real load (`TR036` at 289x the next rung), and polymorphic-`this`-in-an-intersection. It
belongs in `tools/xantham-fixtures.fsx` — where it already is — as something an agent can point the
generator at deliberately, with the numbers in this document as the baseline to compare against.

**Reconsider the rung when** blockers 1-4 are closed and a re-run reports: rendered file under
~50k lines, byte-identical across two in-process runs, zero compiler errors, and gate compile time
under ~30 s. Blocker 1 is the one that decides it; the other three are bounded work.

---

## Appendix A — how each number was taken

Per `.claude/rules/generator-fixtures.md`: run everything, read almost none of it. No rendered file
or manifest was opened whole; the largest single read of generated output anywhere in this work was
three individual lines extracted with `sed -n '<n>p'` to name a compiler-error site.

| number | how |
|---|---|
| tier timings, type-table size, notFollowed | scratch `.fsx` calling `Bootstrap.start` then `Pipeline.runTier` per tier with a stopwatch, printing counts only |
| line and byte counts | `wc -l`, `wc -c` on the written artefacts |
| tier counts, symbol count, findings total | `sed -n '1,/"symbols"/p' manifest.json` — the header, first ~87 lines |
| findings by key, and by key plus id-normalised message | a `python -c` one-liner building a `collections.Counter` over `(f['key'], f['message'])` for every finding of every symbol, printing `most_common(25)`; digits normalised to `N` for the rollup |
| cross-rung matrix | the same script over each `tests/Xantham.Generator.Tests/golden/*/manifest.json` |
| determinism | byte-level compare of the two runs' outputs; first differing offset, hunk count, and the differing lines normalised — never a dump |
| name-length percentiles, `Result`-segment histogram | `python` over the manifest's `symbols[].name` |
| per-declaration line counts | `awk` over the rendered file splitting on `^type ` / `^and `, emitting `lines<TAB>name` — never printing the file |
| import specifiers | `grep -oE 'Import\("[^"]*", "[^"]*"\)' … \| sort -u` |
| DOM-collision probe | set membership of 25 names against the manifest's symbol names |
| compiler errors | scratch net8.0 project mirroring `Xantham.Generator.CompileGate`'s package set, `--maxerrors:5000`; build log deduplicated by a `python` script keyed on `(line, col, code)` with quoted identifiers normalised |
