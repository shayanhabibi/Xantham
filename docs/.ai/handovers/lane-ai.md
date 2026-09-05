# Lane AI — the names an F# declaration admits

Wave seven, batch 1. Branch `worktree-gen-wave7-ai`, base `f708e34`, tip `3d194ba`.
Two commits, one per item.

| Commit | Item |
| --- | --- |
| `2aaa465` | 6 — a synthesized declaration name is spelled in the identifier shape |
| `3d194ba` | 3 — a namespace separates two declarations of one name |

## Item 3 — the six `Name2` residues

Lane AD measured six flat residues that a namespace could separate and changed nothing. All
six are gone. The corpus `Name2`-style residue is **125 → 118**, seven names rather than six:
`ambient-module-lab`'s `Session2` is the same construct, already in the corpus, and moved with
them.

| Residue | Now |
| --- | --- |
| `DurableObject2` | `CloudflareWorkersModule.DurableObject` |
| `WorkflowDurationLabel2` | the global one takes the bare name; `CloudflareWorkersModule.WorkflowDurationLabel` nests |
| `WorkflowSleepDuration2` | as above |
| `ScriptVersion2` | `TailStream.ScriptVersion` |
| `TailEvent2` | `TailStream.TailEvent` |
| `TracePreviewInfo2` | `TailStream.TracePreviewInfo` |
| `Session2` (`ambient-module-lab`) | `AmbientLabRuntime.Session` |

### Which declaration moved, and why the whole claim list is read

Two of the six are not the declaration a reader would guess. `CloudflareWorkersModule` is
declared at `index.d.ts:15601` and the global `WorkflowSleepDuration` at `:17397`, so the
*namespaced* declaration was the first claimant and the **global** one took the suffix. Nesting
at the moment of collision would have moved the global declaration under a namespace it is not
written in.

So `name-exports` now reads every export's claim before granting any of them: a name claimed
twice is contested, and a contested claimant that has a namespace nests. `synthesize-anonymous`
nests at the collision instead, which is where the other case lands — `TailStream.TailEvent` is
not exported by anything, it is reached through `ExportedHandlerTailStreamHandler`'s parameter,
and the global `TailEvent` class has already taken the name by then.

### Plumbing

| File | Change |
| --- | --- |
| `Harvest.fs` | `namespacesAmong`: the namespace symbols of the entry package, by id. `harvest-exports` merges the module's own exports with a `SymbolFlags.Module` scope query at the top of the entry file; `harvest-globals` does the scope query alone. |
| `Model.fs` | `HarvestModel.Namespaces: Map<int, string>`; `TypeFacts.SymbolParent: int option`. |
| `Resolve.fs` | One field, off the symbol response the object branch already had. No extra round trip. |
| `Shape/ExportNames.fs` | Rewritten as a full pass: claim list, contested set, nesting, `SY004`. |
| `Shape/Anonymous.fs` | `claim` takes an owner and nests instead of suffixing where the name is taken. |
| `Shape/Coverage.fs` | One clause — see below. |

Two facts made the plumbing cheap. `SymbolResponse.Parent` is already on every symbol the
resolve tier reads, so the owner is an id lookup rather than a `getParentOfSymbol` per type. And
a namespace of types alone (`TailStream`) is neither a `Type` nor a `Value`, so it arrives
under `SymbolFlags.Module` and nowhere else — one extra scope query per run.

The quoted-specifier trap: `getExportsOfModule` on `declare module "cloudflare:sockets"`
returns exports whose parent is the module symbol itself, and that symbol's name is
`"cloudflare:sockets"`. `namespacesAmong` filters on `Naming.isWritableTypeName`, so nothing
nests under a name that would be FS0883.

### `audit-coverage` gained a clause

A namespace that holds only types is an export the shape tier declares nothing for, so
`nested-name-lab`'s new `Cluster` case read as `AC001` — an unexplained drop — while `module
Cluster` was being emitted three lines away. `audit-coverage` now counts an export as
represented where a declaration is named under it.

Corpus escape count is **191 before and after**, and `AC001` stays at **3** (`setter-lab` 1,
`type-fest` 2). Nothing real was forgiven; the clause removed exactly the false negative this
lane introduced.

### Shadowing — the hazard did not fire

Lane AD flagged it: inside `module CloudflareWorkersModule`, a bare `WorkflowSleepDuration`
resolves to the nested declaration rather than the global one, and the two are identical type
aliases where `shape-aliases` abbreviates the second to the first. Had the *global* declaration
been the one to nest, the emission would have been
`module CloudflareWorkersModule = type WorkflowSleepDuration = WorkflowSleepDuration`, an
immediate cyclic abbreviation.

It reads the other way round, because the namespaced declaration is the earlier one and
therefore the one that moved:

```fsharp
module CloudflareWorkersModule =
    type WorkflowSleepDuration = U2<float, string>

type WorkflowSleepDuration = CloudflareWorkersModule.WorkflowSleepDuration
```

The compile gate is green over the whole corpus, so no reference inside `module TailStream` or
`module CloudflareWorkersModule` was captured by a sibling name either.

### Labs

`tests/fixtures/nested-name-lab` gained a namespace case: a global `Node`, a
`declare namespace Cluster` that declares a second `Node`, and an exported function reaching
the second. The namespace is deliberately *not* exported — that exercises the scope query
rather than the export list, which is the path a module package takes.
`ambient-module-lab` already carried the globals path (`AmbientLabRuntime.Session`) and gained
a test case rather than a fixture change.

Per-pass tests in `Shape.test.fs`: `synthesize-anonymous` nests a namespaced type, keeps the
number where the run has no module name for the namespace, and `name-exports` nests the
namespaced claimant of a contested name whether it claims first or second.

## Item 6 — FS0883 on a synthesized name

```ts
export interface Registry {
  "@cf/meta": { model: string };
}
```

emitted `type ``Registry@cf/meta`` =`. Verified against `net8.0` with a two-line project, since
`dotnet fsi` accepts it with FS1104 alone and only a compiled assembly refuses it:

```
L.fs(2,6): error FS0883: Invalid namespace, module, type or union case name    // Registry@cf/meta
L.fs(6,6): error FS0883: Invalid namespace, module, type or union case name    // Registry$ref
                                                                               // RegistryBeta channel compiles
```

A space is legal in a .NET type name and `$`, `/` and `@` are not, which corrects lane AD's
reading of its own `beta channel` case: that name was never broken, only ugly.

`Naming.identifierName` reduces a name to the plain identifier shape the way
`Naming.enumCaseOfString` already reduces a StringEnum case — every run of characters outside
the shape separates segments, and each following segment is capitalised. It is applied in
`synthesize-anonymous`'s `claim`, per dot-segment, and an identifier-shaped name is returned
unchanged, so the O7 naming contract moves nowhere.

| Input | Output |
| --- | --- |
| `Registry@cf/meta` | `RegistryCfMeta` |
| `Registry@cf/meta/llama-3` | `RegistryCfMetaLlama3` |
| `Registry$ref` | `RegistryRef` |
| `RegistryBeta channel` | `RegistryBetaChannel` |
| `Settings2fa` | unchanged — digit-led key, legal concatenation |
| `Widget_2'` | unchanged |

**Rejected:** sanitising the *segment*, before `nestUnder` decides whether the key opens a
module. It reads better (`Registry.CfMeta` rather than `RegistryCfMeta`) but raises a finding
for every dirty member key walked — `@cloudflare/workers-types` carries several hundred
`@cf/*` model keys, almost none of which name a declaration.

Corpus effect is one name: `nested-name-lab`'s `RegistryBeta channel`. No other synthesized
declaration name in the corpus was outside the identifier shape, which is why the defect was
never gated.

`tests/fixtures/key-sanitise-lab` pins `@cf/meta`, `@cf/meta/llama-3` and `$ref` against two
negatives: `"2fa"`, which is not nestable but concatenates into the legal `Settings2fa`, and
`timeouts`, which opens `Settings.Timeouts` as any identifier-shaped key does. It also pins the
member position keeping the key verbatim, and the sanitised name opening a module of its own
(`RegistryCfMeta.Limits`). Types only, no run gate: the F# name is erased and the JavaScript key
is unchanged, so there is no runtime behaviour the compile gate does not already cover.

## Measurements

### Tiers, whole corpus

| | exact | ergonomic | widened | escape |
| --- | --- | --- | --- | --- |
| Before | 464 | 1505 | 774 | 191 |
| After | **465** | **1515** | **775** | 191 |

The movement is the two new labs' own declarations plus `nested-name-lab`'s new case; no
existing symbol changed tier.

### Findings

| Key | Before | After |
| --- | --- | --- |
| `SY004` `SY.NameNestedUnderOwner` | 701 | **711** |
| `SY005` `SY.NameSanitisedForIdentifier` | 0 | **4** |
| `SP001` | 1659 | 1669 |
| everything else | unchanged | |

`SY004` by fixture after: `@cloudflare/workers-types` 556 (+6), `solid-js` 54, `type-fest` 50,
`animejs` 28, `nested-name-lab` 10, `generics-lab` 3, `key-sanitise-lab` 2,
`ambient-module-lab` 1, eight other labs 1 each.

`SY005` by fixture: `key-sanitise-lab` 3, `nested-name-lab` 1.

### `Name2`-style residue

Measured over every `symbols.jsonl` in the golden corpus: a declaration name of `Stem` +
number ≥ 2 where `Stem` is also a declared name.

| | Before | After |
| --- | --- | --- |
| Residue | **125** | **118** |
| …flat | 77 | 70 |
| …dotted | 48 | 48 |

By fixture after: `@cloudflare/workers-types` 98, `solid-js` 14, `animejs` 2, `type-fest` 2,
`nested-name-lab` 1 (`Choice.Either2`, deliberate), `statics-collision-lab` 1.

Lane AD reported 124; the baseline measured here is 125 on the pre-dispatch commit. One name
entered the residue between wave six's tip and `f708e34`, which is the `TR033` payload commit —
not investigated, and it is not one of the six.

### Golden churn

12 files, +338 / −170 over both commits.

| Fixture | .fs lines changed |
| --- | --- |
| `@cloudflare/workers-types` | 264 |
| `key-sanitise-lab` | 97 (new) |
| `nested-name-lab` | 29 |
| `ambient-module-lab` | 13 |

`animejs`, `solid-js`, `type-fest` and every other golden are byte-identical.

### Gates

`dotnet fsi build.fsx -- test` green: 85 wire tests, 432 generator tests, **160 run-gate checks
passed**. `dotnet build Xantham.slnx` green, which is the compile gate over every golden
including the new one.

## Left undone

- **`Shape/Spec.fs`'s `uniqueCaseNames`** mints StringEnum case names by the same route, and
  FS0883 names a union case as well as a type. A literal union member spelled `@cf/meta` would
  emit `` | ``@cf/meta`` `` and fail the same way. `Spec.fs` belongs to lanes AE/AF/AG this
  wave, so it was left alone; it is the same three-line fix.
- **`Shape/ConstructorObjects.fs`'s `claim`** names `{stem}Constructor` from a source type
  name, which is always an identifier, so it needs nothing today.
- **The remaining 118 residues** are what lane AD recorded: union arms and overload parameters
  reaching one path, and two exports of one name with no namespace between them. TypeScript
  itself separates overloads by order alone, so any scheme reintroduces a number under some
  other spelling.

## Nothing unexplained

Every golden that moved is accounted for by one of the two items. No count moved in a direction
the small evidence does not predict.
