---
category: Generator
audience: managing agent
title: Plan - generator wave six
integration-branch: worktree-generator-wave-six
---

# Generator wave six — the consumer's edit

Wave five closed on master (`docs/.ai/plans/generator-wave-five.md`, batch 3 landed). The corpus
stands at 37 fixtures, `map` and multi-package `ship` are real dispositions, and a CLI exists.

Read `.claude/rules/generator-fixtures.md` first — this plan assumes its "For the managing agent"
protocol, and one clause drives the batch structure: finding codes are positional, so every case
this wave needs is appended by the managing agent in one pre-dispatch commit.

## What this wave is

Waves two through four bought scope against the finding aggregate. Wave five changed the subject to
multi-package. **This wave changes it again: every lane here is priced against a consumer's edit**,
not against a count. The evidence is a downstream Durable Object facade built on the
`@cloudflare/workers-types` golden, and the question each lane answers is whether the F# a consumer
must write to use the binding is the F# they would have written by hand.

Three of the four reports behind this wave compile cleanly. **The compile gate cannot see this
wave's work**, which is why lane Z exists and why two lanes are gated behind it.

Nothing here reduces a finding count as an objective. `TR018`'s remaining 77, `TR023` cause 2, the
cloudflare causes behind `TR036`/`TR037`, `alignOperands` and the `FollowDepth` cutoff all stand
where wave five left them, and `docs/.ai/plans/generator-cloudflare-recon.md` prices them already.
A lane may report a count that moved; no lane may take one as its objective.

## What `aca8953` already bought, and what it left

`feat(shape): a class an ambient module exports to be derived from is an F# class` selects a class
that is `abstract` or carries a base of its own *and* is exported by an ambient module, and renders
it `[<Import(name, specifier); AbstractClass>]` with a primary constructor. Seven classes in
`@cloudflare/workers-types`. Methods stay `abstract`, which is the slot a derived class fills.

That commit is why two of the four reports below are now *reachable* rather than refused. It also
draws the line this wave works along: the entrypoint class form exists, and the members that should
be filling its abstract slots are not.

## The four reports, triaged against master

| Report | Verdict | Lane |
|---|---|---|
| Lifecycle hooks are settable properties, not overridable | Confirmed, and sharper than reported | AA |
| `NonRetryableError` cannot be raised or caught — no `inherit exn` | Confirmed. **Now viable**, which it was not before `aca8953` | AB |
| `DurableObject2` — a class loses the bare name to a global interface | Confirmed, and the mechanism has spread | AD |
| The constructor widens `DurableObjectState<'Props>` to `<obj>` | Not a generator defect. Declined | — |

A fifth item raised during triage — that `name-exports` had no name dedupe and silently dropped the
second of two colliding exports — **is already fixed on master**. `ExportNames.fs:19-25` now claims
through a `taken` set, and its own doc comment cites `WorkflowSleepDuration`, the exact case. No
lane.

### The one refusal

**The `ctx` widening.** `@cloudflare/workers-types` declares `protected ctx:
DurableObjectState<Props>` and `constructor(ctx: DurableObjectState, env: Env)` — the type argument
is omitted upstream, and `DurableObjectState<Props = unknown>` defaults it. `Spec.fs` maps `unknown`
to `obj` under `TR009`, faithfully. `WorkerEntrypoint` shows the identical split for the identical
reason. Repairing it means emitting a binding tighter than the types it is generated from, which
the pipeline declines to do everywhere else.

## Before dispatch — done by the managing agent, in one commit

Cases appended, `Findings.test.fs` updated in the same commit. Each lane raises the cases named
below and edits `Findings.fs` no further.

| Case | Lane | Note |
|---|---|---|
| `TR.NullableHoistedToOption` | AC | Gains a payload. A payload change does not renumber |
| `MB.OptionalHookAsInterface` | AA | Append at the end of `Members` |
| `SC.EntrypointClassInheritsExn` | AB | Append at the end of the `shape-classes` union |
| `SY.NameNestedUnderOwner` | AD | Append at the end of the `synthesize-anonymous` union |

Lane Y changes how these keys are *derived*, so it lands before the pre-declare commit, not after.

## Batch 0 — one lane, alone

### Lane Y — finding keys become symbolic

`Findings.fs:22-28` derives a manifest key from a union's prefix plus the case's 1-based declaration
position, at the price of one rule: cases are append-only, and inserting or reordering renumbers
every key after the edit. Wave five paid that price in the open — `TR055`, `RT003` and `GE004` were
reserved before dispatch, measured unreachable, and retired only because each happened to be the
last case in its union. The wave-five record draws the right conclusion about reserving keys and
stops short of the one about deriving them.

The rule is also about to bind outside the repository. Numeric codes are cited in prose elsewhere,
and a downstream binding generator is going to dispatch on them.

- **The stable key is the union prefix plus the case name** (`TR.NullableHoistedToOption`), written
  into the manifest beside the numeric code. The numeric is derived from a committed name-to-code
  table pinned by a catalogue test, so a retirement or a reorder never moves the key an external
  consumer holds. The numeric stays, for grep and for brevity.
- Owns `Findings.fs`, `Findings.test.fs`, and the manifest emitter.
- **Rider, strike it if it crowds the lane:** split the manifest. The `symbols` array is the bulk of
  it and everything above is the aggregate. Emit `manifest.json` (aggregate plus a `schemaVersion`)
  and `symbols.jsonl`, one symbol per line. The fixtures rule currently asks agents not to read a
  file sitting in front of them; after the split the file they may read is small and the one they
  may not is line-addressable. Note master reverted a manifest-thinning attempt (`ff7e741`) —
  **read that revert before starting**; this is a different change, and the reasons it was reverted
  may or may not apply.
- Done: every manifest carries both keys, the table is committed and pinned, `build.fsx -- findings`
  reads the new shape, corpus finding counts unchanged.

## Batch 1 — three lanes, disjoint files

```
[ Z | AB | AC ]  →  merge  →  batch 2
```

### Lane Z — the run gate answers what reading cannot

No `src/` change. Lane AA's design and lane AD's rendering both rest on Fable and platform behaviour
that reading cannot settle, and `docs/.ai/fable5-workarounds.md` §3 is the in-repo precedent that the
assumption is unsafe: an F# object expression compiles to a class instance whose members sit on the
prototype, so everything reading *own* properties finds an empty object.

Three probes:

1. **Does a hook implemented as an F# interface member survive platform discovery?** Implement an
   interface on a class that inherits an entrypoint class, assert the method is present and
   dispatched; omit it, assert `typeof … === 'undefined'`. `typeof` walks the prototype chain, so
   this passes if the platform reads by access and fails if it enumerates own keys.
2. **Does Fable emit interface members on a class unmangled?** §3 measures object expressions, a
   different emission path. This is asserted downstream and never shown.
3. **Does Fable emit nested modules correctly?** `X.Options` is proven legal F# (lane AD); Fable is
   unproven.

- Owns `tests/Xantham.Generator.RunGate`.
- Done: three probes land as run-gate checks with recorded results. **Report to the managing agent
  before batch 2 dispatches.** A red probe 1 cancels lane AA; a red probe 3 cancels lane AD's
  nesting and falls back to flat path-derived names.

### Lane AB — an entrypoint class deriving from `Error` inherits `exn`

`NonRetryableError` is emitted on master, and the shape is exactly the reported one:

```fsharp
[<Import("NonRetryableError", "cloudflare:workflows"); AbstractClass>]
type NonRetryableError (message: string, ?name: string) =
```

No `inherit`. The declaration is `export class NonRetryableError extends Error`, and `Error` has no
row in `Naming.LibBindings` (32 rows) nor in `BrowserBindingTable.generated.fs`, so the base
resolves to `FsObj` and the heritage is dropped under `SI002`. An F# type that does not derive from
`System.Exception` cannot be `raise`d and cannot be caught by type, so the declaration is
unusable for the purpose it exists to serve.

**Before `aca8953` this was refused, and the refusal was correct at the time**: the declaration
rendered as an interface, and an interface admits no `inherit` at all. It now renders as an
`[<AbstractClass>]`, so `inherit exn` is expressible and the refusal is obsolete.

- Owns `Shape/Classes.fs`, `Render.fs`, and the `Naming.LibBindings` table in `Model.fs`. Lab:
  `error-class-lab`.
- Scope is **entrypoint classes only**. An `interface X extends Error` keeps its flattened members
  and its `SI002`; flattening is right for an interface and this lane does not touch it. That
  bounds the change to the entrypoint set rather than the 20 `SI002` sites in the fixture.
- The `Error` row targets `exn` with a loss note, as `generator-tr023-recon.md:263` proposes. Report
  whether adding the row moves `TR023` and by how much — it is a side effect, not the objective.
- Raises `SC.EntrypointClassInheritsExn`.
- Done: the lab pins an `Error`-derived entrypoint class that a consumer `raise`s and catches by
  type, **proven in the run gate, not only compiled**; the negative (a non-entrypoint `interface X
  extends Error`) pinned unchanged.

### Lane AC — the absence alphabet is four states collapsed to one

`Spec.fs:65-68`, unchanged since wave three:

```fsharp
let internal isNullish (facts: TypeFacts) =
    flag TypeFlags.Undefined facts
    || flag TypeFlags.Null facts
    || flag TypeFlags.Void facts
```

`TR032` is raised with no payload. So `null`, `undefined`, `void` and — via `MB003` — an absent
property all arrive at one `option`, and the manifest cannot tell a consumer which. TypeScript
distinguishes `x?: T` from `x: T | undefined` at the declaration, and the platforms distinguish them
at runtime: a KV `get` returns `null` on a miss, DO storage returns `undefined`, D1 carries SQL
`NULL` as `null`, JSON has `null` and no `undefined`. A downstream generator selecting an inbound
`Keep | Clear | Set 'T` union, or an outbound rule choosing between an omitted key, `null` and
`undefined`, needs the distinction per site.

There is a consumer on the F# track too: Fable erases `None` to `undefined`, so any API whose
semantics turn on null-means-clear is mis-served today, invisibly. The payload makes those sites
visible.

- Owns `Shape/Spec.fs`. Lab: `absence-alphabet-lab` — a declaration carrying each of `x?: T`,
  `x: T | undefined`, `x: T | null`, `x?: T | null`, and a `void`-returning member.
- `TR.NullableHoistedToOption` gains separate flags for null, undefined and void. `MB003` is left
  alone: it is the `?` fact and stays the `?` fact.
- **The rendered binding does not change.** This lane moves information into the manifest and
  nothing else.
- Done: the lab pins all five shapes with distinct payloads, every golden `.fs` byte-identical,
  `TR032` count unchanged.

## Batch 2 — one lane, alone

Gated on lane Z probe 3.

### Lane AD — a nested module replaces the numeric suffix

The `$"{preferred}{i + 2}"` rule now appears in **four** places — `Anonymous.fs:216`,
`ConstructorObjects.fs:39`, `ExportNames.fs:24` and `Spec.fs:1832`. It has spread since wave three,
and `generator-type-mapping.md:703-705` names the outcome and rejects it:

> deterministic disambiguation (`Name2` is unacceptable; path-derived names per the archive's
> `NamePath` scheme).

The live case: a global `interface DurableObject` is named by `name-exports`, and the class of the
same name nested in `declare namespace CloudflareWorkersModule` is reached later and becomes
`DurableObject2`. A namespaced class loses to a global interface by pass order alone. A consumer
writes `inherit DurableObject2<'Env,'Props>(ctx, env)`.

**For an anonymous inline shape the correct name is a nested module, not a concatenation.** Given
`type X { propA: string, options: { l: string, b: string } }`, the name is `X.Options`, not
`XOptions`.

Feasibility is settled — compiled, in a namespace, under the `module rec` header every golden
carries:

| Case | Result |
|---|---|
| Module before the type, two levels (`X.Options.Retry`) | compiles |
| Module emitted after the type | compiles under `module rec` |
| Nested type references its owner | compiles under `module rec` |
| Two owners with the same leaf name, bags cross-referencing | compiles |
| Without `[<CompilationRepresentation(ModuleSuffix)>]` | compiles |

Without `module rec`, ordering and cycles both fail FS0039. Every golden emits `module rec`, so both
constraints are dissolved and the attribute is not required.

- Owns `Shape/Anonymous.fs`, `Shape/ConstructorObjects.fs`, `Shape/ExportNames.fs`, `Render.fs`, and
  the `DeclNames` declaration in `Model.fs`. It also owns `Spec.fs:1832`, which is why no other lane
  holds `Spec.fs` this batch. Lab: `nested-name-lab`.
- Implementation shape: `DeclNames` maps a type id to a dotted path. **Reference sites need no
  change** — `FsNamed "X.Options"` already prints as `X.Options`. Only `Render.fs` changes, to group
  declarations sharing a prefix under a companion module.
- **Check the keyword-escape path in `Render.fs`**: escaping must run per segment, or `X.Options`
  renders backticked as a single name.
- **Two declarations colliding with no owner to nest under** — the `DurableObject` case — keep the
  numeric suffix. Report that residue as a count. Whether a namespaced class should instead nest
  under its namespace module is a question this lane answers with a number, not a change.
- Raises `SY.NameNestedUnderOwner`.
- Done: `Name2` gone from the corpus except the reported residue, count recorded, compile gate
  green, run gate green, golden churn reviewed in aggregate.

## Batch 3 — one lane, alone

Gated on lane Z probes 1 and 2, and on lane AD landing.

### Lane AA — an optional hook fills an abstract slot

`Spec.fs:1726-1732`, byte-identical to wave three:

```fsharp
let asMethod =
    if not (hasAny SymbolFlags.Method m.Symbol.Flags) then None
    else
        match Map.tryFind m.TypeId model.Types with
        | Some memberFacts when not memberFacts.CallSignatures.IsEmpty -> Some memberFacts
        | _ -> None
```

Under `strictNullChecks` the checker's type of an optional member is a union with `undefined`, and a
union carries no call signatures. So the second clause fails for **every optional method in the
corpus** and the member falls through to the property branch. `Classes.fs:112` carries the same gate.

`aca8953` makes this sharp. `DurableObject2` is now an `[<AbstractClass>]` whose methods are
`abstract` — the slot a derived class fills — and the hooks are the members that do not reach it:

```fsharp
type DurableObject2<'Env, 'Props> (ctx: DurableObjectState<obj>, env: 'Env) =
    member _.fetch
        with get (): Func<Request<…>, U2<JS.Promise<Response>, Response>> option = jsNative
        and set (_: …): unit = jsNative
```

Not `abstract`. A concrete instance member bound `jsNative`. A consumer assigns it in a constructor,
which dispatches at runtime and is invisible to everything static.

**The emitted form is one opt-in interface per optional method**, and the class does not carry the
member:

```fsharp
[<Interface>]
type IFetchHandler =
    abstract fetch: request: Request<…> -> U2<JS.Promise<Response>, Response>
```

A subclass implements the interfaces it provides, and `typeof instance.fetch === 'function'` holds
exactly when it does.

Two decisions inside that, both settled:

- **Per hook, never grouped.** A group is derivable from the primitives by interface inheritance; a
  grouped interface cannot be decomposed. Grouping is package knowledge — the `.d.ts` marks each `?`
  independently — and the generator would have to be told it per package, indefinitely.
- **Not a plain `abstract` member, and not `abstract … default`.** A plain `abstract` makes the hook
  mandatory, which the `?` denies. A default body makes it present, and a `fetch` that exists and
  returns 404 differs from no `fetch`.

- Predicate: `SymbolFlags.Method`, `m.Optional`, and the owner renders as an entrypoint class. All
  three are available at the gate. This deliberately leaves optional methods on plain interfaces as
  option properties — widening the predicate later is append-safe, narrowing it breaks generated
  API. `aca8953` selects seven classes in the fixture, so the change is bounded by construction.
- Owns `Shape/Spec.fs`, `Shape/Classes.fs`, `Shape/Interfaces.fs`. Lab: `hook-interface-lab`.
- Raises `MB.OptionalHookAsInterface`, so hooks stop sharing `MB003` with data members.
- **Rider, same file, own lab (`statics-collision-lab`):** `Shape/Classes.fs` keys its statics side
  table by the export name (`:264`, `fsName fallback export`) and joins it back by the F# declared
  name. They coincide only while an exported class is never renamed. Lane AD is about to change what
  declarations are named, so verify against the composed tree and resolve the key through
  `DeclNames` if it has diverged. Zero live incidence today.
- Done: the lab pins an implemented hook present and an omitted hook absent **in the run gate**,
  `MB.OptionalHookAsInterface` counted, `MB003` falls by that count, compile gate green.

## Deferred

- **Wave five's open items** — `inline` and demand-driven resolve, the `Fable.Core` binding gaps
  (`docs/.ai/fable-binding-gaps.md`), group sorting after a dependent, and the `EndOfStreamException`
  from a dying tsgo child. All independent of this wave.
- **The fidelity queue** — `TR018`'s 77, `TR023` cause 2, `TR036`/`TR037`, `alignOperands`, the
  `FollowDepth` cutoff. Priced in `generator-cloudflare-recon.md` and `generator-tr018-recon.md`.
- **Positional `obj` provenance in findings.** The per-symbol table is sufficient for dispatch
  today, because the manifest's symbol path already addresses every declaration site uniquely.
  Per-position provenance is needed only for a consumer joining checker facts to body operations. If
  a finding grows an optional field for it, the right field is a Wire node handle Harvest already
  holds, not `file:line`, which drifts.

## What wave seven should weigh

1. **`Func<_,_>` becomes `FSharpFunc<_,_>`.** Ordinary curried F# functions fit `FSharpFunc`, and
   every callback the generator emits obliges a consumer to wrap in `System.Func`. It interacts with
   lane AA — a hook that lands as an abstract method stops being a delegate at all — so the count to
   price it against is whatever the delegate members stand at after this wave.
2. **`KVNamespace.get` binds only the first overload** (`generator-cloudflare-recon.md` §3.2), which
   puts the `arrayBuffer`, `stream` and `json` forms out of reach from F#. A consumer-facing defect
   in the same family as this wave's, already reconnoitred.
3. Wave five's open items, carried above.
