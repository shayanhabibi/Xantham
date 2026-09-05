---
category: Generator
audience: managing agent
title: Lane AF - literal-typed parameters keep overloads apart
branch: worktree-gen-wave7-af
base: f708e348ca9a2c434ac1f3b8e7b6641f8e1e964b
---

# Lane AF - literal-typed parameters keep overloads apart (wave seven, item 2)

## What landed

`TR006` widened every lone string literal to `string`, which made the fourteen `KVNamespace.get`
overloads read as six and `dedupe-overloads` drop the other eight. A generated `KVNamespace`
could read a KV key as text and by no other means: `arrayBuffer`, `stream` and `json` had no F#
spelling that reached them.

A string literal now keeps its own type at the parameter positions where that is what tells an
overload set apart. Everywhere else it still widens and still reports `TR006`.

## The shape of the decision

**One type per literal, not one StringEnum over the set.** §3.2 of the cloudflare recon proposed
"synthesizing a StringEnum over the literals across an overload set". That does not work: a
shared `[<StringEnum>] type KVGetType = Text | Json | ArrayBuffer | Stream` gives every overload
the *same* parameter type, so the five signatures stay identical and nothing is separated. Once
the literal is gone the overloads differ in their return types alone, which F# rejects, so the
**parameter** types are what has to differ - and that means one type per literal.

Each retained literal is written as a single-case `[<StringEnum>]` nested under the declaration
that reads it:

```fsharp
module KVNamespace =
    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Text =
        | [<CompiledName("text")>] Text
```

`RequireQualifiedAccess` is what `render-source` puts on every StringEnum it writes, so the call
site is `kv.get(key, KVNamespace.Text.Text)`. The doubled name is the price of that attribute;
changing it is a `Render.fs` decision and `Render.fs` belongs to other lanes this wave.

One declaration per literal **per owner**, shared across that owner's members: `get` and
`getWithMetadata` both keep `"text"` and both read `KVNamespace.Text`.

## How retention is scoped

`Shape.Spec.literalOverloads` answers the question from the type table alone, before any
reference is written:

1. For each declaration the run names, each member with two or more call signatures.
2. Group the signatures by their parameter types **with string literals erased**
   (`literalErasedKey`). A group of two or more is the collision F# would reject - the same
   collision `dedupe-overloads` prices as `DO001`.
3. Within a group, a parameter position keeps its literals where every signature there carries
   one literal at most and two of them disagree on which.

`typeRef` then writes `FsNamed <declaration>` instead of `FsString` at those positions and reports
`TR056` (Exact) instead of `TR006` (Widened). `dedupe-overloads` declares the types and reports
`DO002` (Exact) once per set it separated.

Three properties of that scoping are load-bearing:

- **Keyed by parameter position, not by type id.** The checker interns a literal type, so `"text"`
  is one id wherever it appears. Keying retention by id would have moved every property typed
  `"text"` as well. The key is the owner string `typeRef` already carries
  (`KVNamespace.get(type)`), so `Label.kind: "text"` is untouched. `literal-overload-lab` pins
  this with a `Label` interface carrying the same literal the `Store` overload set retains.
- **Reached through type arguments.** `KVNamespaceGetOptions<"text">` carries its literal two
  levels down, and the four `options?:` overloads collide for exactly that reason.
  `literalsCarried` reads through union members and type arguments to depth four.
- **Entry group only.** A synthesized name is not in `DeclNames`, so `Pipeline.declOrigins` cannot
  find its group and `groupModules` places it in the entry module. An owner in a shipped
  dependency group would therefore reference a type in a module it does not read, so owners
  outside the entry group are left alone (`ownedByEntry`). No corpus fixture reaches that branch;
  the guard is there so none can.

The analysis is a pure function of `model.Types` and `model.DeclNames`, both fixed for the whole
Shape tier, so it is memoized on `DeclNames` through a `ConditionalWeakTable` rather than
recomputed at every literal.

## Measurements

Baseline `f708e348`, after `3120e12`. Counts are over every committed `symbols.jsonl`.

| key | before | after |
|---|---:|---:|
| `DO001` corpus | 36 | 18 |
| `DO001` `@cloudflare/workers-types` | 19 | 3 |
| `DO001` `animejs` | 17 | 15 |
| `TR006` corpus | 1225 | 1197 |
| `TR006` `@cloudflare/workers-types` | 303 | 279 |
| `TR006` `animejs` | 654 | 651 |
| `TR056` corpus | 0 | 32 |
| `DO002` corpus | 0 | 3 |

Tier counts:

| fixture | before | after |
|---|---|---|
| `@cloudflare/workers-types` | exact 234, ergonomic 1089, widened 381, escape 110 | exact **238**, ergonomic 1089, widened 381, escape 110 |
| `animejs` | exact 72, ergonomic 82, widened 32, escape 52 | exact **75**, ergonomic 82, widened 32, escape 52 |

The widened tier does not move: `KVNamespace` and `DrawableSVGGeometry` each carry other widening
findings, so the symbols stay in the tier their worst finding puts them in. The finding count is
where this work shows.

`git diff --stat` over the feature commit: 15 files, 760 insertions, 24 deletions - 220 lines of
`Shape/Spec.fs`, 38 of `Shape/Overloads.fs`, 202 of tests, one new lab, and 96 lines of moved
golden across two fixtures. **`solid-js` and `type-fest` did not move at all**: both carry `TR006`
(213 and 43) and neither has an overload set a literal separates.

Full gate green at `3120e12`: `dotnet fsi build.fsx -- test` - 427 generator tests, 85 wire tests,
compile gate built, Fable run gate 160 checks (unchanged; this lane added no run-gate check
because the compile gate already proves the form and nothing here is a runtime claim).

## `KVNamespace.get` after the change

All fourteen `get` and all fourteen `getWithMetadata` overloads survive. Every form is reachable:

```fsharp
type KVNamespace<'Key> =
    abstract get: key: 'Key * ?options: KVNamespace.Get.Options -> JS.Promise<string option>
    abstract get: key: 'Key * ``type``: KVNamespace.Text -> JS.Promise<string option>
    abstract get<'ExpectedValue>: key: 'Key * ``type``: KVNamespace.Json -> JS.Promise<'ExpectedValue option>
    abstract get: key: 'Key * ``type``: KVNamespace.ArrayBuffer -> JS.Promise<JS.ArrayBuffer option>
    abstract get: key: 'Key * ``type``: KVNamespace.Stream -> JS.Promise<ReadableStream<obj> option>
    abstract get: key: 'Key * ?options: KVNamespaceGetOptions<KVNamespace.Text> -> JS.Promise<string option>
    ...
```

`DO002` fires twice on cloudflare (`KVNamespace.get`, `KVNamespace.getWithMetadata`) and `TR056`
twenty-four times.

## What else moved, and why

`animejs` `DrawableSVGGeometry.getElementsByTagNameNS` is the same construct in `lib.dom.d.ts`:
three overloads separated by the namespace URI literal, of which two were dropped. All three now
survive, reading `DrawableSVGGeometry.HttpWwwW3Org1999Xhtml`, `...Org2000Svg` and
`...Org1998MathMathML`. The names are long because `Naming.enumCaseOfString` is derived from the
literal, and the literals are URLs. They are deterministic and they compile; if the manager wants
them shorter that is a naming decision, not a mechanism one.

`DO001`'s remaining 18 are all non-literal collisions and are untouched by design:

- `animejs` 15: `DrawableSVGGeometry.querySelector`/`querySelectorAll`/`getElementsByTagName`/
  `closest`/`matches` are `K extends keyof HTMLElementTagNameMap` against a plain `string`
  overload - both widen, and no literal is party to it. `$` and `mapRange` are export functions,
  which this lane does not cover.
- `@cloudflare/workers-types` 3: `Ai.run`, `AutoRAG.aiSearch`, `BrowserRun.quickAction`.

## `tests/fixtures/literal-overload-lab`

Registered in `Pipeline.test.fs` with four assertions plus the two `fixtureTests` standards. The
positive is `Store.read`: six overloads over two colliding groups, three separated by a literal
`kind` parameter and two by the literal a `ReadOptions<...>` argument carries. Four negatives:

| negative | what it pins |
|---|---|
| `Solo.tag(name: "only")` | a lone signature has no sibling, so the literal still widens (`TR006`) |
| `Label.kind: "text"` | the same interned literal type at a non-overload position is untouched |
| `Mixed.send` | parameter lists that differ before the literal is read do not collide, so nothing is retained |
| `Choice.pick` | a *union* of literals per position is already a StringEnum of its own |
| `Widen.scan` | a collision no literal is party to still drops an overload (`DO001`) |

Per-pass unit tests in `Shape.test.fs` cover `Spec.typeRef` (retained at the separating position,
widened at another position over the same type id) and `Overloads.dedupeOverloads` (the
single-case StringEnum declarations and the `DO002` finding).

## Left undone

An anonymous **union** of literals at a distinguishing position still collides:
`pick(kind: "a" | "b")` against `pick(kind: "c" | "d")` both widen to whatever the union maps to.
In this corpus `synthesize-anonymous` already names such a union and it comes out as
`Choice.Pick.Kind` / `Choice.Pick.Kind2`, so the two do *not* collide there - but a union that
`synthesize-anonymous` does not name would. One literal type cannot stand for a union, so
repairing it means synthesizing a StringEnum per union arm-set, which is a different mechanism.
`literal-overload-lab`'s `Choice` pins the boundary.

Export functions (`ExportFunction` overloads) are not covered. Two `DO001` sites in `animejs`
(`$`, `mapRange`) sit there; neither is literal-separated, so nothing is lost today.

## Findings owned

`TR056` / `TR.StringLiteralKeptForOverload of literal: string` (Exact) and `DO002` /
`DO.OverloadsDistinguishedByLiteral of parameter: string` (Exact). Both were pre-declared; this
lane edited neither `Findings.fs` nor `Findings.test.fs`.

## Files touched

- `src/Xantham.Generator/Shape/Spec.fs` - the analysis block (after `typeSpelling`) and the
  `TypeFlags.StringLiteral` arm of `typeRefOnPath`.
- `src/Xantham.Generator/Shape/Overloads.fs` - `literalDecl`, and `dedupeOverloads` gains the
  declarations and the `DO002` findings.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs`, `Shape.test.fs`.
- `tests/fixtures/literal-overload-lab/`, `tests/Xantham.Generator.Tests/golden/literal-overload-lab/`.
- Regenerated goldens for `@cloudflare/workers-types` and `animejs`.

Untouched, as briefed: `Model.fs`, `Render.fs`, `Findings.fs`, `Shape/Spec.fs` 1190-1330,
`Shape/Overloads.fs`'s `normalize`, `Shape/Passes.fs` (no new pass was needed - the retention is
read where the reference is written, and the declarations are added by `dedupe-overloads` itself).

## Nothing unexplained

Every moved line in both large fixtures is accounted for above: the two literal-separated overload
sets in cloudflare, the one in animejs, and the StringEnum declarations they read.
