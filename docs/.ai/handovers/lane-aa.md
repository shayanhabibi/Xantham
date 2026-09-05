# Lane AA — an optional hook fills an abstract slot

Wave six, batch 3. Branch `worktree-agent-a9ef93c259e16aead`, based on `worktree-generator-wave-six`
at `10f9af0`.

The worktree forked from `1fff741` (`feat(findings): pre-declare wave six's four cases`), three
commits behind the integration branch. Reset onto `worktree-generator-wave-six` before any work;
everything below is measured against `10f9af0`.

## What changed

An optional method declared by a class that renders as an `[<AbstractClass>]` entrypoint (§4.4) is
a *lifecycle hook*. It now becomes an interface of its own, nested under the owner, and the class
carries no member for it:

```fsharp
[<Import("DurableObject", "cloudflare:workers"); AbstractClass>]
type DurableObject2<'Env, 'Props> (ctx: DurableObjectState<obj>, env: 'Env) =
    member _.ctx with get (): DurableObjectState<'Props> = jsNative and set ...
    member _.env with get (): 'Env = jsNative and set ...

module DurableObject2 =
    [<Interface>]
    type IFetchHandler =
        abstract fetch: request: Request<obj, U2<RequestInitCfProperties, IncomingRequestCfProperties<obj>>> -> U2<JS.Promise<Response>, Response>
        [<ParamObject; Emit("$0")>]
        static member Create (fetch: Func<…>) : IFetchHandler = jsNative
```

A consumer writes `interface DurableObject2.IFetchHandler with member this.fetch req = …` and
`typeof instance.fetch === "function"` holds exactly where they did.

Three shaping-tier edits carry it:

- **`Shape/Spec.fs`** gains `callSignaturesOf` (a member's signatures, read off the non-nullish
  arms of the `undefined` union `strictNullChecks` produces — the gate at `:1726` that failed for
  every optional method in the corpus), `isOptionalHook`, `exportedClassSides`, and `shapeHook`.
  `isEntrypoint` and `typeVars` moved here from `Shape/Classes.fs` (`typeVars` merged into the
  pre-existing `typeVarsOf`). `shapeMembers` takes the hook names as a parameter and declares none
  of them.
- **`Shape/Interfaces.fs`** decides the hoist, because the finding has to be raised or withheld in
  the pass that shapes the member. The predicate is `isEntrypoint` over the exported class's
  constructor object, plus "this run emits no `inherit` line for the declaration" — the two
  conditions `admitEntrypoint` refuses on that are knowable there.
- **`Shape/Classes.fs`** keys its statics side table and its collision test by the *declared* name
  rather than the export name (see below).

### Naming

`I{Pascal(member)}Handler`, nested under the owner through `nestUnder`, so `fetch?` on
`WorkerEntrypoint` is `WorkerEntrypoint.IFetchHandler`. Flat names collide: `fetch` and `connect`
are hooks on both `WorkerEntrypoint` and `DurableObject2`. Nesting composes with lane AD's
`Render.fs` module grouping, which was already proven to emit `module X` beside `type X` (probe 3,
and `WorkflowDynamicDelayContext` in the committed golden).

A hook whose signature mentions the owner's type parameters takes the owner's whole head:
`Relay.IForwardHandler<'T>` in the lab. Zero incidence in the corpus.

### What was deliberately left alone

- **Optional methods on plain interfaces and on non-entrypoint classes stay option properties.**
  `DurableObject` (the *global interface*, distinct from the `cloudflare:workers` class),
  `Listener` and `Hub` in the lab. Widening the predicate later is append-safe.
- **`Shape/Classes.fs:112`'s `shapeStatic` carries the same union gate.** An optional *static*
  method still falls through to the property branch and reports `SC005`. Corpus incidence of
  `SC005` is zero, so this is measured rather than fixed.
- **No forwarding member on the entrypoint class.** A forwarder would put `fetch` on the base
  prototype and make `typeof instance.fetch === "function"` true for a subclass that declined the
  hook — the exact property the emission exists to provide. The cost is that a consumer cannot call
  `this.fetch` from their own F# without upcasting; the plan rules that accepted.
- **`ParamObjects` synthesizes a `Create` on each hook interface.** `IFetchHandler.Create(fetch =
  …)` is the object literal a handler map is, which is a real second form for the same declaration,
  so `Shape/ParamObjects.fs` was left untouched. It accounts for the `SP001`/`SP002` movement below.

## Measurements

Corpus tiers, `dotnet fsi build.fsx -- findings`:

| | exact | ergonomic | widened | escape |
|---|---|---|---|---|
| before | 462 | 1482 | 771 | 191 |
| after | 464 | 1506 | 773 | 191 |

`+2 exact / +9 ergonomic / +2 widened` are the two new labs' own symbols; the remaining
`+15 ergonomic` is `@cloudflare/workers-types`, one per hook interface.

`@cloudflare/workers-types`, by key:

| key | before | after |
|---|---|---|
| `MB003` `MB.OptionalMemberAsOption` | 3063 | 3048 |
| `MB005` `MB.OptionalHookAsInterface` | 0 | 15 |
| `TR032` `TR.NullableHoistedToOption` | 3551 | 3536 |
| `SP001` `SP.ParamObjectSynthesized` | 1292 | 1307 |
| `SP002` | 541 | 556 |

Every count moves by exactly 15 and in the direction the change predicts: fifteen members stop
being option properties (`MB003`, `TR032` fall), fifteen hook interfaces appear (`MB005`, `SP001`,
`SP002` rise). Tier counts `exact 234 / widened 381 / escape 110` are unmoved.

`MB005` total: 15 in `@cloudflare/workers-types`, 3 in `hook-interface-lab`. The fifteen are
`WorkerEntrypoint`'s `email fetch connect queue scheduled tail tailStream test trace` and
`DurableObject2`'s `alarm fetch connect webSocketMessage webSocketClose webSocketError`.
`aca8953` selects seven entrypoint classes; the other five declare no optional method.

Golden churn: one file. `Cloudflare.WorkersTypes.fs` +92/−45. Every other committed golden is
byte-identical.

## Labs

**`tests/fixtures/hook-interface-lab`** — an ambient module exporting `Station` (abstract; `run`
mandatory, `fetch?` and `alarm?` hooks, `tag?` an optional data member), `Relay<T>` (a hook reading
the class's type parameter), `Hub` (exported, neither abstract nor derived — the interface form, so
`probe?` stays an option), and `Annex extends Hub` (an entrypoint candidate this run gives an
`inherit` line, so it keeps the interface form and its inherited `probe?` with it). A global
`Listener` interface carries `ping?`, untouched.

Run gate, `optionalHooks ()`:

- an implemented hook is present by property access and dispatches to the subclass's implementation;
- a hook the same subclass declined reads `undefined` and fails `in`;
- a subclass implementing no interface carries neither hook, while the mandatory slot it overrode
  still dispatches and the imported base constructor's own member reads back;
- a hook interface carrying its owner's type parameter dispatches under the hook's name;
- the hook is no own property of the instance.

**`tests/fixtures/statics-collision-lab`** — the Rider question, answered with a reproducer rather
than a reading. `shape-classes` keyed its statics side table by the export name and joined it back
by the F# declared name; they diverge whenever a clash renames the class. The lab declares a global
interface `Depot` beside a `statics-lab:depot` class `Depot`, whose instance side is declared
`Depot2`. Before the fix the golden was:

```fsharp
type Depot =                                   // the global interface
    abstract slot: string with get, set
    [<Import("Depot.LIMIT", "statics-lab:depot")>]
    static member LIMIT: float = jsNative      // the class's static, on the wrong declaration
```

So the divergence is live, not latent — it was invisible only because no exported class in the
corpus is both renamed and carries statics. Fixed by resolving the key through `DeclNames`, the
same lookup `admitEntrypoint` already made. `SC002`/`SC004`/`SC005`/`SC006` are unmoved across the
corpus, and the run gate reads `Depot2.LIMIT` and `Depot2.``open``` back off the JavaScript.

Lane AD's nesting does not reach this: it renames anonymous shapes, and the statics key is an
exported class's name.

## Gates

- `dotnet build Xantham.slnx` — green. The compile gate compiles the new `module WorkerEntrypoint`
  / `module DurableObject2` blocks beside the generic classes of those names.
- `dotnet fsi build.fsx -- test` — green. 419 generator tests (10 new), 85 wire tests, run gate 160
  checks (11 new).

## Open

- The plan's premise that `workerd` discovers hooks *by access* rather than by own-key enumeration
  is still unverified in this repository. Probe 1 measured what Fable emits, not what the platform
  reads. Nothing in this lane's work bears on it, and no generator change would fix it if the
  premise is wrong.
- `admitEntrypoint`'s `FreeTypeParameter` refusal is the one admission condition
  `shape-interfaces` cannot reproduce, because it depends on shaping the construct signature under
  a type-variable scope `shape-classes` does not set. A candidate refused for that reason would
  keep the interface form while its hooks stay opt-in interfaces. `SC008` is 1 across the whole
  corpus and its reason is `InheritedBase`, so the case has no incidence; the lab covers the
  `InheritedBase` half through `Annex`.
