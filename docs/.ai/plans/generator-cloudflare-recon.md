---
category: Generator
title: Plan - @cloudflare/workers-types reconnaissance
---

# `@cloudflare/workers-types` — reconnaissance

`@cloudflare/workers-types` is the primary litmus test. Its widened tier stands at **374 symbols**
and its escape tier at **115**, and neither has ever been attributed. This document attributes
every site of the eight widened keys at 15 sites or more that no existing recon covers, plus the
whole escape tier, to a named cause; reduces each cause to a reproducer of six declarative lines
or fewer; and says for each whether it is a defect or the mapping declining to lie.

**Nothing here is a code change.** Source sites are cited; none were touched.

Three headlines, each stated in full at §3:

1. **The escape tier is not a work queue.** 108 of the 115 escape symbols carry `TR008` and
   nothing else; the other 7 carry `HG001` and nothing else.
2. **`KVNamespace.get` binds only its first overload**, so the `arrayBuffer`, `stream` and `json`
   forms are unreachable from F#. This is `TR006` arriving at a user-facing consequence.
3. **`TR018` is 47 sites on this fixture and is deliberately out of scope.** It is priced in
   `docs/plans/generator-tr018-recon.md` and lane P is landing causes D, C and the
   identical-operand slice of A on `Shape/Spec.fs` now.

---

## 1. Provenance

| | |
|---|---|
| baseline | `worktree-gen-cloudflare-recon` @ `b993a75`, over `worktree-generator-wave-four` |
| corpus | `golden/**/manifest.json`, 28 fixtures; `@cloudflare/workers-types` read as the aggregate, never as its `symbols` array in a reading tool |
| population | 242 findings over eight keys, plus 115 escape-tier symbols |
| grades | 242 `widened`; escape tier 165 `TR008` findings and 7 `HG001` findings |
| owning passes | `shape-interfaces`, `shape-aliases`, `shape-callbacks`, `shape-exports` (all eight keys resolve through `Shape/Spec.fs`); `DO001` through `Shape/Overloads.fs` |
| labs | **none run.** Each reproducer below is reduced from the fixture's own `.d.ts` and its count is the manifest's count at the cited owner, not a lab measurement. A lane acting on any cause should build the lab first. |
| binding checks | `Fable.Core` 5.2.0 probed through `fslangmcp check` against `tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj` — the pinned reference set the compile gate uses |
| compiler | `node_modules/@typescript/typescript-win32-x64/lib/tsc.exe`, borrowed from the main checkout |

Tier counts at this baseline: exact 252, ergonomic 1,031, widened 374, escape 115, over 1,772
symbols.

---

## 2. Where the 242 are

Site counts on the fixture, and the same key corpus-wide, so wave five can price at either scope:

| key | case | fixture | corpus | verdict |
|---|---|---:|---:|---|
| `TR037` | `TemplateLiteralToString` | 56 | 68 | defect (mis-keyed) — 4 losses reported 14 times each |
| `TR023` | `NotAmongGeneratedDeclarations` | 43 | 143 | 37 the mapping declining to lie, 6 recoverable |
| `TR040` | `ObjectTypeToObj` | 40 | 45 | the mapping declining to lie — floor |
| `TR035` | `UnionWithObjArm` | 23 | 88 | cascade only; no independent site |
| `TR020` | `IndexedAccessNoForm` | 22 | 67 | defect (recoverable) — 17 of 22 |
| `TR047` | `ObjectWithoutMembers` | 22 | 27 | 16 floor, 6 defect (mis-keyed) |
| `DO001` | `OverloadDropped` | 19 | 36 | 5 losses reported 19 times; downstream of `TR006` |
| `TR036` | `UnionTooWide` | 17 | 35 | 9 recoverable by one constant, 7 by tagging, 1 floor |

`TR018` sits at 47 on this fixture — larger than `TR023` — and is excluded on purpose. See §3.3.

Two of the eight keys report far more findings than they describe losses. Distinct owners against
findings, for every key at 15 sites or more on the fixture:

| key | findings | distinct owners | inflation |
|---|---:|---:|---:|
| `TR037` | 56 | 4 | **14.00** |
| `DO001` | 19 | 5 | **3.80** |
| `TR018` | 47 | 28 | 1.68 |
| `TR006` | 297 | 219 | 1.36 |
| `TR036` | 17 | 13 | 1.31 |
| `TR009` | 156 | 133 | 1.17 |
| `TR020` | 22 | 20 | 1.10 |
| every other key at 15+ sites | | | ≤ 1.07 |

`TR023`, `TR040`, `TR035` and `TR047` sit at exactly 1.00.

### Symbol-level shape of the widened tier

A symbol lands in widened by carrying any widened finding, so the eight keys above account for a
smaller share of the 374 widened symbols than their finding counts suggest. Sole cause, per
symbol:

| key | symbols it is the sole widened cause of | disposition |
|---|---:|---|
| `TR006` | 151 | settled |
| `TR009` | 75 | settled (D8) |
| `TR040` | 33 | floor, §9.1 |
| `TR023` | 10 | §7 |
| `TR047` | 10 | floor, §9.2 |
| `TR036` | 9 | §5 |
| all others | 86 | |

**226 of the 374 widened symbols are sole-caused by a settled decision.** Wave five should price
the widened tier knowing that its majority is closed by prior ruling, and that the eight keys here
are the recoverable remainder.

---

## 3. Three conclusions that stand alone

### 3.1 The escape tier is not a work queue

115 escape symbols. Every one of them carries exactly one escape-tier key:

| key | escape symbols | escape findings |
|---|---:|---:|
| `TR008 AnyToObj` | 108 | 165 |
| `HG001 AmbientModuleDropped` | 7 | 7 |

There is no overlap: 108 symbols carry `TR008` alone and 7 carry `HG001` alone. The corpus
declares two further escape-tier keys, `AC001` (3 sites) and `RE001` (2), and both fire at zero
sites on this fixture. Corpus-wide the tier is `TR008` 550, `HG001` 8, `AC001` 3, `RE001` 2 —
the same shape at four times the size.

**94% of the escape tier is `TR008`, which is a settled decision.** `any` maps to `obj` and the
symbol grades escape because `obj` admits values the declaration excluded. That ruling puts 108
symbols in escape and will keep them there. The escape tier on the primary litmus test measures
how much `any` Cloudflare writes, and it answers a question open since the tier was introduced:
**escape is not where the generator's remaining work lives.**

The 7 `HG001` symbols are the residue, and they are a real capability hole rather than a grading
artefact:

```
"assets:*"  "cloudflare:email"  "cloudflare:node"  "cloudflare:pipelines"
"cloudflare:sockets"  "cloudflare:workers"  "cloudflare:workflows"
```

Each is a `declare module "…"` ambient module declaration whose members are importable from that
specifier. `import { DurableObject } from "cloudflare:workers"` is how a Worker is written, so
these seven are the package's primary import surface, and the generator emits nothing for them.
Seven symbols is a small count attached to a large consequence. Pricing it is a separate exercise
from anything in this document.

### 3.2 `KVNamespace.get` binds only its first overload

`DO001` reports 19 dropped overloads over 5 owners, and 16 of the 19 are `KVNamespace.get` and
`KVNamespace.getWithMetadata`. The declaration (`index.d.ts:2256`):

```ts
get(key: Key, options?: Partial<KVNamespaceGetOptions<undefined>>): Promise<string | null>;
get(key: Key, type: "text"): Promise<string | null>;
get<ExpectedValue = unknown>(key: Key, type: "json"): Promise<ExpectedValue | null>;
get(key: Key, type: "arrayBuffer"): Promise<ArrayBuffer | null>;
get(key: Key, type: "stream"): Promise<ReadableStream | null>;
```

The overloads are distinguished by the **string-literal type** of `type`, or by the literal
argument to `KVNamespaceGetOptions<…>`. `TR006` widens a lone string literal to `string`
(`Shape/Spec.fs:540`), so after widening every signature reads `get(key: string, type: string)`
and `dedupe-overloads` (`Shape/Overloads.fs:69`) drops eight of them as identical to the first.

**The consequence is user-facing.** The surviving binding returns `JS.Promise<string option>`. An
F# consumer of the generated `KVNamespace` can read a key as text and by no other means: the
`arrayBuffer`, `stream` and `json` forms have no F# spelling that reaches them. Retaining the
overloads is illegal F# — they differ in return type alone once the literal is gone — so the
recovery is to make the *parameter* types differ again, by synthesizing a StringEnum over the
literals across an overload set. That is a lane, and it is described at §11.

`DO001` is therefore not an independent key. It is the visible price of `TR006`, and its 19
findings describe 5 losses.

### 3.3 `TR018` is out of scope and already priced

`TR018` stands at 47 on this fixture, above `TR023`. It is excluded deliberately.
`docs/plans/generator-tr018-recon.md` attributes all 194 corpus sites to four causes and prices
them; on this fixture the split is cause A 31 and cause B 16. Lane P is landing cause D, cause C
and the identical-operand slice of cause A on `Shape/Spec.fs` at the time of writing.

This matters here because **`TR035` is entirely downstream of `TR018` and `TR023`** (§8). The 13
`TR035` sites that lane P will move are already counted in that recon's win, and counting them
again here would double-price the same work.

---

## 4. `TR037` — 4 losses, reported 56 times (56 sites, 4 owners)

### Mechanism

`erasedUnionRef` (`Shape/Spec.fs:1095`) deduplicates the union's **arms** and accumulates **every
arm's findings**:

```fsharp
let arms =
    memberIds
    |> List.map (fun id ->
        let reference, refFindings = typeRef ctx model self owner id
        findings <- findings @ refFindings
        reference)
    |> List.distinct
```

`List.distinct` at the end of the pipeline collapses arms that map to the same F# type. The
`findings <- findings @ refFindings` inside the lambda has already run once per arm. A union of
fourteen arms that all map to `string` renders as one `string` and carries fourteen `TR037`s.

The `TR037` itself is raised at `Shape/Spec.fs:557` and is correct: a template literal is a string
at runtime, and the pattern is what the mapping gives up.

### Reproducer

**Two lines, reduced from `experimental/index.d.ts:15928`:**

```ts
export type WorkflowDurationLabel = "second" | "minute" | "hour" | "day" | "week" | "month" | "year";
export type WorkflowSleepDuration = `${number} ${WorkflowDurationLabel}${"s" | ""}` | number;
```

The checker expands the template over the finite label union and the `"s" | ""` suffix into
**14 open template-literal types** — open, because `${number}` keeps each one from collapsing to a
string literal. Each takes the `TemplateLiteral` arm at `Spec.fs:557`. The arms deduplicate to
`[FsString; FsFloat]`, the rendered type is `U2<string, float>`, and the manifest records
**14 `TR037` findings** on the owner `WorkflowSleepDuration`.

### Where they are

Four owners, 14 findings each:

| owner | findings |
|---|---:|
| `WorkflowSleepDuration` | 14 |
| `WorkflowStepDoConfigRetries.delay()` | 14 |
| `WorkflowStepDoRollbackOptionsRollbackConfigRetries.delay` | 14 |
| `WorkflowStepDoRollbackOptionsRollbackConfigRetries.delay()` | 14 |

All four resolve to the same `WorkflowSleepDuration` declaration, reached at a member position and
at a call-signature position. Corpus-wide the key is 68 over 16 distinct owners; `flags-lab` (8),
`type-fest` (2), `animejs` (1) and `keyof-lab` (1) hold the remainder.

### Verdict

**Defect (mis-keyed — the behaviour is right, the message is wrong).** The rendered binding is
correct. 52 of the 56 findings are duplicate reports of 4 losses. Deduplicating findings by
(owner, message) inside `erasedUnionRef` removes them and changes no emitted line.

---

## 5. `TR036` — a threshold, a shared tag, and an inline union (17 sites)

### 5.1 Mechanism — the cap

`ErasedUnionArity` is `4` (`Shape/Spec.fs:310`). `erasedUnionRef` widens above it
(`Shape/Spec.fs:1116-1121`):

```fsharp
| arms when arms.Length <= ErasedUnionArity -> FsErasedUnion arms, findings
| arms ->
    FsObj,
    findings @ [ Finding.make owner (TypeReference.UnionTooWide(arms.Length, ErasedUnionArity)) ]
```

**`Fable.Core` 5.2.0 ships `U2` through `U9`.** Verified against the compile gate's own reference
set: `U9<int,int,int,int,int,int,int,int,string>` type-checks clean. The cap of 4 is D4's recorded
threshold, and the pinned package supports more than twice it.

The renderer needs no work. `Render.fs:154` already builds the name from the arity:

```fsharp
| FsErasedUnion arms ->
    let text = arms |> List.map (printTypeIn true) |> String.concat ", "
    $"U{arms.Length}<{text}>"
```

Raising the cap touches four sites: the constant at `Spec.fs:310`, the two doc comments that
restate the threshold (`Render.fs:152`, `Model.fs:622`), and the literal `UnionTooWide(5, 4)` in
`Findings.test.fs:149`. D4's record in `docs/plans/generator-type-mapping.md` §4.5 carries the
decision and would move with it.

### Reproducer

**Three lines, reduced from `experimental/index.d.ts:16866`:**

```ts
export interface Attribute {
  readonly value: string | string[] | boolean | boolean[] | number | number[] | bigint | bigint[];
}
```

Eight distinct arms, all primitive, none collapsing. At cap 4 this is **1 `TR036`** and
`Attribute.value: obj`. At cap 9 it is `U8<string, string[], bool, bool[], float, float[], bigint,
bigint[]>` and no finding.

### 5.2 Mechanism — a tag value shared by two arms

`taggedUnionShape` (`Shape/Spec.fs:195`) requires the tag values to be distinct
(`Shape/Spec.fs:233`):

```fsharp
if List.distinct values = values then
```

`TailStream.EventType` is a ten-arm union in which every arm carries `readonly type: "…"`. It
reports **11** arms, because one member is an intersection over a union:

```ts
type Log = { readonly type: "log"; readonly level: string } & (
  | { readonly message: object; readonly truncated?: false }
  | { readonly message: string; readonly truncated: true }
);
```

The checker distributes that intersection into two members, **both tagged `"log"`**. The
uniqueness test fails, `taggedUnionShape` returns `None`, and the union falls to `erasedUnionRef`
at 11 arms. The refusal is silent: `ArmNotPlainData` is raised only after `taggedUnionShape`
succeeds, so a `None` from the uniqueness test produces no finding at all. The manifest carries no
`DT` finding for `EventType`, and `EventType` appears under no name in the output.

### 5.3 Mechanism — an inline discriminated union

`detectTaggedUnions` (`Shape/TaggedUnions.fs`) iterates `model.DeclNames`. A discriminated union
written inline at a member position has no declaration name, so the pass is never offered it.

**Reproducer, three lines, reduced from `experimental/index.d.ts:16777`:**

```ts
interface FetchEventInfo { readonly type: "fetch"; readonly url: string; }
interface AlarmEventInfo { readonly type: "alarm"; readonly scheduledTime: Date; }
export interface Onset { readonly info: FetchEventInfo | AlarmEventInfo; }
```

At the fixture's full width `Onset.info` is a ten-arm union, uniformly tagged on `type`, and it
records **1 `TR036`** with `info: obj`. Under a declaration name the same union would be claimed
by `detect-tagged-unions` and graded exact.

### Where they are

| slice | sites | arity |
|---|---:|---|
| under the cap once it is 9 | 9 | 5, 6, 8 |
| `TailStream.EventType`, shared tag value | 6 | 11 |
| `Onset.info`, inline tagged union | 1 | 10 |
| `TraceItem.event` | 1 | 10 |

The nine that the cap recovers: `WebAssemblyInstanceImportsItem.[]` (5), `Attribute.value` (8),
`CryptoKey.algorithm` (6), `R2Bucket.put(value)` (5, twice), `R2MultipartUpload.uploadPart(value)`
(5), `ResponseInputItem` (6), `VectorizeVectorMetadata` (5), `VectorizeVectorMetadataFilter.[]`
(5).

The six `EventType` sites are `ExportedHandlerTailStreamHandler()(event)`,
`ExportedHandler.tailStream()(event)` and `WorkerEntrypoint.tailStream()(event)`, twice each.

`TraceItem.event` is ten arms of which two — `TraceItemConnectEventInfo` and
`TraceItemCustomEventInfo` — declare no members at all and so carry no tag. They appear in the
`TR047` list at §9.2 for the same reason.

### Verdict

**Nine sites: defect (recoverable), and the cheapest change in this document.** Seven sites:
defect (recoverable) in `detect-tagged-unions`, one of them worsened by a silent refusal that
should raise a finding either way. One site — `TraceItem.event` — is the mapping declining to lie:
ten arms, two untaggable, above any plausible cap.

---

## 6. `TR020` — the operand has to be a type variable (22 sites)

### Mechanism

`indexedAccessRef` (`Shape/Spec.fs:718-731`) resolves `T[K]` through one path and widens on every
other:

```fsharp
let objectName =
    facts.Response.ObjectType
    |> ValueOption.toOption
    |> Option.bind (fun id -> Map.tryFind id model.TypeVars)

match binding, objectName with
| Some(TypedKeyOf(operand, result)), Some name when operand = name -> FsTypeVar result, []
| _ -> FsObj, [ Finding.make owner TypeReference.IndexedAccessNoForm ]
```

`model.TypeVars` holds the type parameters the declaration being shaped bound. **A concrete named
type is never in it.** So `EventMap[Type]` inside a generic `EventTarget<EventMap>` resolves —
`EventTarget` gets `TP004` and `typekeyof<'EventMap,'R>` correctly — and the same access resolves
to `obj` at every declaration that instantiates `EventMap` with a concrete interface.

### Reproducer

**Five lines, reduced from `index.d.ts:395` and `:992`:**

```ts
interface WorkerGlobalScopeEventMap { fetch: FetchEvent; scheduled: ScheduledEvent; }
export declare function addEventListener<Type extends keyof WorkerGlobalScopeEventMap>(
  type: Type,
  handler: (event: WorkerGlobalScopeEventMap[Type]) => void,
): void;
```

The operand `WorkerGlobalScopeEventMap` is a concrete interface, `Map.tryFind` returns `None`, and
the access widens. The fixture records **2 `TR020`** on this declaration — one on the `handler`
parameter and one on the callback's own `event` parameter.

The honest upper bound is available at the site: the union of the map's value types. That is a
widening, and a far tighter one than `obj`.

### Where they are

| slice | sites | owners |
|---|---:|---|
| concrete operand | 16 | `WorkerGlobalScope`, `ServiceWorkerGlobalScope`, `WebSocket`, and the global `addEventListener` / `removeEventListener`, 4 each |
| index is not a bound key variable | 1 | `EventTarget.dispatchEvent(event)` |
| nested access | 5 | `Ai.run()` 1, `Ai.run(inputs)` 3, `AiRunInputs.requests` 1 |

`EventTarget.dispatchEvent(event: EventMap[keyof EventMap])` indexes by `keyof EventMap` rather
than by a bound variable, and the union-of-value-types technique lands it too — 17 of 22 on one
mechanism.

The five nested sites are `AiModelList[Name]["inputs"]` (`index.d.ts:11851`): an access over the
`'R` that `typekeyof` introduced, indexed by a string literal. `'R` is opaque, so nothing names
`"inputs"` on it.

### Verdict

**17 sites: defect (recoverable).** Resolving an indexed access whose object is a concrete object
type to the union of its value types recovers 16 directly and `dispatchEvent` by the same
technique. **5 sites: the mapping declining to lie** — a literal index into an opaque result
variable has no F# form.

---

## 7. `TR023` cause 2 — the compiler-lib binding gap (43 sites)

Causes 1A and 1B closed in wave three; see `docs/plans/generator-tr023-recon.md`. What remains on
this fixture is cause 2 alone.

### Mechanism

`objectRef`'s last arm (`Shape/Spec.fs:819`) reports a lib type that neither `LibBindings` nor
`BrowserBindings` answers for:

```fsharp
| (Ship | Widen), _ ->
    let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
    FsObj, [ Finding.make owner (TypeReference.NotAmongGeneratedDeclarations shown) ]
```

The table it consulted is `Model.LibBindings` (`Model.fs:232`).

### Where they are, by the name reported

| name | fixture | corpus | disposition |
|---|---:|---:|---|
| `IterableIterator` | 20 | 20 | declining to lie |
| `Module` | 8 | 8 | correct — declaration genuinely absent |
| `Iterable` | 7 | 9 | declining to lie |
| `Error` | 5 | 8 | **recoverable** |
| `BigUint64Array` | 1 | 3 | declining to lie |
| `AsyncIterableIterator` | 1 | 1 | **recoverable** |
| `Env` | 1 | 1 | correct — a declaration this run owes |

### The binding facts, verified

`Model.fs:276` states that the `seq`-shaped names are absent on purpose because Fable.Core binds
only the async ones. **That claim is correct against the pin.** Probed through `fslangmcp check`
against the compile gate's reference set:

```fsharp
let a: Fable.Core.JS.Iterable<int> = unbox ()   // FS0039: type 'Iterable' is not defined in 'Fable.Core.JS'
let b: Fable.Core.JS.Iterator<int> = unbox ()   // FS0039: type 'Iterator' is not defined in 'Fable.Core.JS'
```

while `JS.AsyncIterable<int>`, `JS.BigInt64Array` and `JS.IteratorResult<int>` all type-check
clean. `JS.BigUint64Array` is absent where `JS.BigInt64Array` is present, so that row is a real
gap in Fable rather than an omission in the table.

Two rows are recoverable:

- **`Error` to `exn`**, with a loss note. `Fable.Core.JS` declares no `Error`; F#'s `exn` is
  `System.Exception`, which Fable compiles to the JavaScript `Error`. The `TR023` recon already
  identified this row. 5 sites here, 8 corpus-wide.
- **`AsyncIterableIterator` to `JS.AsyncIterable`**, with a loss note. The table already carries
  `AsyncIterable`, `AsyncIterator` and `AsyncGenerator`; this is the missing fourth name of the
  same family. 1 site.

### Reproducer

**Three lines, reduced from `index.d.ts` `Headers`:**

```ts
export declare class Headers {
  entries(): IterableIterator<[string, string]>;
}
```

`IterableIterator` reaches `Spec.fs:819` and records **1 `TR023`** with `entries(): obj`.

### Verdict

**28 sites: the mapping declining to lie.** `Iterable`, `IterableIterator` and `BigUint64Array`
have no target in the pinned Fable surface, and the table exists to refuse rather than to guess.
**9 sites: correct** — `Module` is `WebAssembly.Module`, which nothing binds, and `Env` is a
declaration this run genuinely owes the reader. **6 sites: defect (recoverable)** — two table rows
in `Model.fs:234`.

The `TR023` recon's instruction stands: do not send an agent at "the compiler-lib binding gap". It
is two rows, and it rides on another lane.

---

## 8. `TR035` — cascade only (23 sites, 0 independent)

### Mechanism

`erasedUnionRef` (`Shape/Spec.fs:1115`) collapses the whole union when any arm reached `obj`:

```fsharp
| arms when arms |> List.contains FsObj ->
    FsObj, findings @ [ Finding.make owner TypeReference.UnionWithObjArm ]
```

The finding describes the collapse. It never describes why the arm was `obj`.

### Where they are

Every one of the 23 owners also carries the finding that produced the `obj` arm. There is no
residual:

| upstream key | sites |
|---|---:|
| `TR018` | 13 |
| `TR023` | 7 |
| `TR040` | 3 |

The 13 `TR018`-driven owners: `RoleScopedChatInput.role`, `AiTextGenerationToolInput.type`,
`AiTextGenerationToolLegacyInputParameters.type`,
`AiTextGenerationToolInputFunctionParameters.type`, the three `Ai_Cf_*.image` members, and
`EmailDestinations2-4` / `EmailMessageBuilder2-4` at `to`, `cc` and `bcc`.

The 7 `TR023`-driven owners: `TypedArray`, `HeadersInit`, `BodyInit`, `URLSearchParams(init)`,
`URLSearchParamsConstructor.Create(init)`, `WorkerLoaderWorkerCodeModules.[]`,
`WorkerLoaderModule.wasm`.

The 3 `TR040`-driven owners: `AiTextGenerationInput.tools`, `AIGatewayHeaders.[]`,
`AIGatewayUniversalRequestHeaders.[]`.

### Reproducer

**Two lines, reduced from `index.d.ts` `HeadersInit`:**

```ts
export type HeadersInit = Headers | Iterable<Iterable<string>> | Record<string, string>;
export declare const init: HeadersInit;
```

The `Iterable` arm reaches `Spec.fs:819` and becomes `obj`; the union then collapses. The fixture
records **1 `TR023` and 1 `TR035`** on `HeadersInit`.

### Verdict

**Cascade. Not a lane, at any price.** The 13 `TR018`-driven sites fall with lane P; counting them
in this document's win would double-price that recon's work. The 7 `TR023`-driven sites are
downstream of the `Iterable` refusal at §7 and stay. The 3 `TR040`-driven sites stay.

`TR035` earns its place as a downstream indicator: it says a union lost more than one arm's worth
of precision. It should never be staffed directly.

---

## 9. The two floors

### 9.1 `TR040` — TypeScript's `object` (40 sites, all floor)

`Spec.fs:565` maps the `NonPrimitive` flag:

```fsharp
elif has TypeFlags.NonPrimitive then
    FsObj, [ Finding.make owner TypeReference.ObjectTypeToObj ]
```

**Reproducer, two lines, from `index.d.ts:11797`:**

```ts
export interface AiOptions { extraHeaders?: object; }
export declare const options: AiOptions;
```

This records **1 `TR040`**.

All 40 sites are the literal `object` keyword: `extraHeaders?: object` (7 sites across the
`Ai*Options` family), `guided_json?: object` and `arguments: object` across the generated model
types, `cfJson?: object`, `body: object`, and `toJSON(): object` on the six `Performance*` classes.

F# has no type admitting every non-primitive and no primitive. `obj` is §4.1's mapping and it is
still a widening, which is exactly what the finding says.

**Verdict: the mapping declining to lie.** Floor. Keep the key; staff nothing.

### 9.2 `TR047` — member-less object types (22 sites, 16 floor)

Raised at `Spec.fs:816`, the arm wave three's lane H introduced to stop `{}` being reported as a
missing declaration.

**Reproducer, two lines, from `index.d.ts:495`:**

```ts
export interface TestController {}
export declare const controller: TestController;
```

This records **1 `TR047`**.

16 sites are genuinely member-less and correct: `interface TestController {}`,
`Cloudflare.Exports` (a declaration-merging extension point that `wrangler types` fills in,
reached at `ExecutionContext.exports`, `DurableObjectState.exports` and
`CloudflareWorkersModule.exports`), `TraceItemConnectEventInfo`, `TraceItemCustomEventInfo`,
`SqlStorageStatement`, the `logprobs` and `prompt_logprobs` members across eight `Ai_Cf_*` model
types, all spelled `{} | null`, and `grounding?: {}[]`.

**Six sites are a different construct wearing the same message** — a mapped type over an operand
the checker cannot enumerate, which has members the mapping cannot list rather than no members:

```ts
type Params<P extends string = any> = Record<P, string | string[]>;
type EventContext<Env, P extends string, Data> = { params: Params<P>; };
```

`Params<P>` is `Record<P, ...>` over an open key set. Four sites: `EventContext.params`,
`PagesFunctionContext.params`, `EventPluginContext.params`, `PagesPluginFunctionContext.params`.
Two more are `payload: Readonly<T>` at `WorkflowEntrypointRunEvent.payload` and
`WorkflowStepWaitForEventResultItem.payload`. `Readonly` erases at runtime, so the operand itself
is the mapping, on the same argument that already carries `ReadonlyMap` to `JS.Map` with a loss
note.

**Verdict: 16 the mapping declining to lie; 6 defect (mis-keyed).** The six want the `keyof` idiom
or a pass-through, and in either case a finding that names the construct.

---

## 10. Residual

Zero. Every one of the 242 findings is attributed:

| key | sites | attributed |
|---|---:|---|
| `TR037` | 56 | 4 losses + 52 duplicate reports |
| `TR023` | 43 | 28 declining to lie + 9 correct + 6 recoverable |
| `TR040` | 40 | 40 floor |
| `TR035` | 23 | 13 + 7 + 3, all cascade |
| `TR020` | 22 | 16 + 1 + 5 |
| `TR047` | 22 | 16 floor + 6 mis-keyed |
| `DO001` | 19 | 5 losses + 14 duplicate reports |
| `TR036` | 17 | 9 + 6 + 1 + 1 |

Escape tier: 115 symbols, 108 plus 7, no overlap.

---

## 11. Recommendation

One lane per recoverable cause, cheapest first.

### Lane Q1 — deduplicate findings in `erasedUnionRef`. **Cheapest, do first.**

- Recovers **66 findings** (52 `TR037`, 14 `DO001`) on this fixture; corpus-wide the same
  predicate reaches every key that accumulates per arm.
- Owns `Shape/Spec.fs`, `erasedUnionRef` only.
- Deduplicate by (owner, rendered message), never by owner alone: `BodyInit` carries four distinct
  `TR024` messages that all have to survive.
- Emits no golden line. The whole change is manifest-visible.

### Lane Q2 — `ErasedUnionArity` 4 to 9.

- Recovers **9 `TR036`** here, and re-prices 35 corpus-wide.
- Owns the constant at `Shape/Spec.fs:310`; touches `Render.fs:152`, `Model.fs:622`,
  `Findings.test.fs:149` and D4's record in `docs/plans/generator-type-mapping.md` §4.5.
- `U5`-`U9` are verified present in `Fable.Core` 5.2.0, but a nine-arm erased union in a nested
  position is a construct the corpus has never emitted. Regenerate and let the compile gate
  compile before reading anything.
- Reopens a recorded decision. That is the lane's real cost; the code change is one integer.

### Lane Q3 — indexed access over a concrete operand.

- Recovers **17 `TR020`** here, 67 corpus-wide to re-measure.
- Owns `indexedAccessRef` (`Shape/Spec.fs:718-731`).
- Resolves `Concrete[Type]` where `Type extends keyof Concrete` to the union of the operand's
  value types, and reaches `EventMap[keyof EventMap]` by the same route.
- An event map with more than nine entries produces a union above `ErasedUnionArity` and lands
  back on `obj` through `TR036` rather than `TR020` — a count that moves sideways rather than
  down. Sequence this after Q2.

### Lane Q4 — reach the two tagged unions `detect-tagged-unions` misses.

- Recovers **7 `TR036`**.
- Owns `Shape/TaggedUnions.fs` and `taggedUnionShape` (`Shape/Spec.fs:195-240`).
- Two changes: merge arms sharing a tag value rather than refusing the union, and raise a finding
  on the refusal path so a `None` from the uniqueness test stops being silent; and offer inline
  unions at member position to the pass.
- A DU case per arm where two arms share a tag is not a legal F# DU. The merge has to produce one
  case whose fields are the intersection of the sharing arms, or refuse loudly. Refusing loudly is
  an acceptable outcome for this lane; refusing silently is what it exists to end.

### Lane Q5 — two rows in `Model.LibBindings`. **A rider, not a lane.**

- Recovers **6 `TR023`**: `Error` reads `exn`, `AsyncIterableIterator` reads `JS.AsyncIterable`,
  both with loss notes. 9 corpus-wide.
- Owns `Model.fs:234`.
- An F# `exn` parameter accepts any exception, which is wider than the declaration. The loss note
  has to say so.

### Lane Q6 — a finding for mapped types over an open operand.

- Recovers **6 `TR047`** into a message that names the construct.
- Owns `Shape/Spec.fs:816` and one new case in `Findings.fs`.
- Lane H introduced `TR047` to separate `{}` from a missing declaration; a third split has to
  leave the 16 genuine sites where they are.

### Not a lane

- **`TR035`** (23). Cascade in full. Falls with lane P and with §7's upstream, or not at all.
- **`TR040`** (40) and 16 of **`TR047`**. The mapping declining to lie.
- **28 of `TR023`.** Verified absent from `Fable.Core` 5.2.0.
- **The escape tier.** 108 of 115 are `TR008`, a settled decision.
- **`DO001`'s 5 real losses.** They need a StringEnum synthesized across an overload set, which is
  a `TR006` lane and a wave in its own right. §3.2 is the case for opening it; it is not this
  document's recommendation.

---

## 12. The shape of the win

Cumulative, over the 242 findings in scope, on `@cloudflare/workers-types`:

| after | in-scope findings remaining |
|---|---:|
| today | 242 |
| Lane Q1 | 176 |
| + Lane Q2 | 167 |
| + Lane Q3 | 150 |
| + Lane Q4 | 143 |
| + Lane Q5 | 137 |
| + Lane Q6 | 131 |

**131 is the floor** without changing the pinned Fable surface or reopening `TR006`. It is:

| | sites |
|---|---:|
| `TR040`, TypeScript's `object` | 40 |
| `TR023`, no target in the pinned Fable packages | 37 |
| `TR035`, cascade from `TR018` and `TR023` | 23 |
| `TR047`, genuinely member-less | 16 |
| `TR020`, a literal index into an opaque variable | 5 |
| `DO001`, the price of `TR006` | 5 |
| `TR037`, template literal patterns | 4 |
| `TR036`, ten arms with two untaggable | 1 |

Lane P moves the 13 `TR018`-driven `TR035` sites separately, taking the floor to 118 without any
work priced here.

Tier movement is smaller than the finding movement. Q2 and Q6 move 19 symbols out of widened; Q1,
Q3, Q4 and Q5 move roughly 20 more. The widened tier goes to about 335 of 374, because 226 of
those symbols are sole-caused by `TR006`, `TR009` or `TR040` and no lane here touches them. **The
finding count is where this work shows; the tier count is not.**

---

## 13. What this document does not cover

- **`TR018`**, 47 sites on this fixture. Priced in `docs/plans/generator-tr018-recon.md`; lane P
  is landing it. Cited at §3.3 and §8, never re-attributed.
- **`TR032`** (3,529), **`MB003`** (3,045), **`SP001`** (1,137), **`TR006`** (297), **`TR008`**
  (165 as a defect), **`TR009`** (156), **`SI005`** (57). Settled decisions.
- **The ergonomic tier.** `TR024`, `SI003`, `SI004`, `MB004` and `TP002` are not losses.
- **`@types/three` and the rung question.** Deprioritised.
- **Pricing `HG001`.** Seven ambient module declarations carrying the package's primary import
  surface. §3.1 establishes that they are the whole of the escape tier outside `TR008` and that
  they are a capability hole rather than a grading artefact. What it costs to emit them is a
  separate exercise.
- **Lab evidence.** No lab was run. Every reproducer here is reduced from the fixture's own
  `.d.ts` and every count is the manifest's count at the cited owner. A lane acting on any cause
  should build the lab first and confirm the reproducer bounds where this document says it does.
- **`RT001` and `FollowDepth`.** Wave three flagged 1,772 truncated types on this fixture as thin
  headroom. Nothing here measures it.
- **The keys on this fixture below the 15-site threshold.** `RA003` (17), `TR048` (12), `SA002`
  (10), `MB002` (9), `TR026` (8), `TP006` (8) and the tail were not attributed.
