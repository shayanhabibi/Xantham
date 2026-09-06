# Lane CO — the resolve tier does not follow lib interface members: mechanism, attribution, decline

Wave fourteen, branch `worktree-gen-wave14-co`, forked at `35c42f9`. Read-only lane: no
behaviour change lands. `git status` verified clean before this file was written; nothing under
`src/` or `tests/` is touched or staged. This handover is the deliverable.

## 1. The mechanism

`src/Xantham.Generator/Resolve.fs:469`, function `deriveFacts`, `TypeFlags.Object` branch, the
`shapeName` block at lines 651–667:

```fsharp
let! shapeName =
    if
        GeneratorConfig.disposition ctx.Config origin = Ship
        || objectFlags.HasFlag ObjectFlags.Mapped
        || isMemberType
    then
        async.Return None
    else
        match named symbol with
        | Some name -> async.Return(Some name)
        | None ->
            async {
                let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                return named alias
            }

if shapeName.IsSome then
    // Identity only (O7): the shape tier renders references to this group by
    // templated name or widens them, and either way nothing reads its members.
    return
        { TypeFacts.shallow ty with
            Origin = origin
            SymbolName = shapeName
            TypeArguments = typeArguments |> List.map _.Id
            TupleElements = tupleElements
            AliasTypeArguments = aliasTypeArguments |> List.map _.Id },
        channel trace "type-arguments" typeArguments
        @ channel trace "alias-type-arguments" aliasTypeArguments
else
    // ... deriveStructure ctx trace ty, which populates Members / IndexInfos / CallSignatures ...
```

This is the O7 identity-only shortcut, not a distinct bug. Any object type whose group's
`disposition` (`Model.fs:238`, defaulting non-`Ship` groups to `Widen` until configured
otherwise) is not `Ship`, is not a mapped type, is not a member type, and carries a real declared
name gets returned with `Members = []`, `IndexInfos = []`, `CallSignatures = []`,
`ConstructSignatures = []` — deliberately, per the comment: the shape tier only ever needs the
group's identity (its templated module name or a widen-with-finding) for such a reference, so
reading its members would be a wasted round trip repeated across the whole `lib.dom`/dependency
graph.

**`FollowDepth` (`Resolve.fs:64`, the wave-eleven-pinned constant `20`) is not implicated.** The
shortcut fires at generation zero of the walk, on the type's own group and name, before depth is
consulted at all; `FollowDepth` only bounds the breadth-first frontier's *generation count* for
types that do get followed. A lib interface reached through this shortcut never reaches the
depth check because it never enters `walk`'s frontier as a member-bearing node in the first
place — it is answered and returned in the same `deriveFacts` call that discovered it.

Consequence for `TR020` (`TR.IndexedAccessNoForm`, `Findings.fs:103`, raised at
`Shape/Spec.fs:1507`): `indexedAccessRef`/`indexedAccessValues` (`Shape/Spec.fs:1176`, `:1505`)
need `operand.Members`/`operand.IndexInfos` to resolve what a `T[K]` access selects. When the
operand is a named, non-`Ship`, non-mapped interface — a `lib.dom`/`lib.es5` interface, or an
unconfigured dependency interface — those are empty, `indexedAccessValues` returns `None`, and
`TR020` fires with `FsObj`.

## 2. Attribution — all 53 sites, one by one

I read every site's declaring `.d.ts` source, not just the fixture totals. The dispatch's
carried-forward item assumed most of the 53 share the mechanism above; **they do not** — four
other, structurally distinct causes account for most of them.

| Fixture | Symbol | Cause | Shares the lib-follow mechanism (§1)? |
|---|---|---|---|
| `animejs` | `DrawableSVGGeometry.addEventListener(listener)(ev)` | `K extends keyof HTMLElementTagNameMap` → `HTMLElementTagNameMap[K]`; the map is a real named `lib.dom` interface under `Widen` | **Yes** |
| `animejs` | `DrawableSVGGeometry.closest()` (×2 overload instances) | same `HTMLElementTagNameMap`/`SVGElementTagNameMap`-keyed pattern | **Yes** |
| `animejs` | `DrawableSVGGeometry.querySelector()` (×3) | same | **Yes** |
| `animejs` | `DrawableSVGGeometry.querySelectorAll()` (×4) | same | **Yes** |
| `animejs` | `DrawableSVGGeometry.removeEventListener(listener)(ev)` | same | **Yes** |
| — | **animejs subtotal** | | **13 / 13 yes** |
| `type-fest` | `ArrayLength` (`T['length']`, `T extends readonly unknown[]`) | operand's bound resolves to `Array<T>`/`lib.es5`, a named interface under `Widen` | **Yes** |
| `type-fest` | `StringLength` (`StringToArray<S>['length']`) | same `Array`-shaped operand for the non-literal-string case | **Yes** |
| `type-fest` | `ArrayValues` (`T[number]`) | index is the primitive `number` type, not a string literal or `keyof`; `keySetOf` (`Shape/Spec.fs:1140`) has no case for a numeric index at all — following `Array`'s members would not help | **No** |
| `type-fest` | `ExtractRestElement` (`SplitOnRestElement<T>[1][number]`) | chained numeric-literal + numeric index over a deferred conditional utility (`SplitOnRestElement`) | **No** |
| `type-fest` | `TaggedUnion` (`{[Name in keyof UnionMembers]: ...}[keyof UnionMembers]`) | a **mapped type** (`ObjectFlags.Mapped`, exempt from the §1 shortcut) indexed by its own still-generic `keyof`; `deriveStructure` runs but the mapped type is abstract over a generic `UnionMembers`, so it has no concrete members to enumerate — the §4.11 phantom/deferred family, not lib-follow | **No** |
| `type-fest` | `GetTagMetadata` (`Type[typeof tag][TagName]`) | same generic-mapped self-index family as `TaggedUnion` | **No** |
| `type-fest` | `UnionLength` (`UnionToTuple<Union>['length']`) | operand is a deferred recursive conditional utility applied to a still-generic `Union`; no concrete members exist to read regardless of lib-follow | **No** |
| — | **type-fest subtotal** | | **2 / 8 yes** |
| `@cloudflare/workers-types` | `Ai.run()` / `Ai.run(inputs)` (×3 overload instances) | `AiModelList[Name]["inputs"]` is a **chained** indexed access: the outer operand is the *inner* `IndexedAccess` type itself (not a `TypeParameter`), so `operandShape` never reaches `Record`/`AiModelList` at all — the nested-access limitation, distinct from §1 | **No** |
| `@cloudflare/workers-types` | `Ai.Run.Inputs.requests` (×2) | same chained-access pattern inside the hoisted `{requests: AiModelList[Name]["inputs"][]}` object | **No** |
| — | **@cloudflare subtotal** | | **0 / 5 yes** |
| `indexed-access-lab` | `runModel()`, `runModel(input)` | same chained-access pattern, deliberately authored by this lab as its own documented floor (source comment: "the outer operand is `ModelMap[Name]`, which declares no keys of its own") | **No** |
| `indexed-access-lab` | `Feed.take(event)` | a literal index (`EventMap["fetch"]`) over a bound (`Record<string, WorkerEvent>`) that declares no keys of its own — same family, lab's own documented floor | **No** |
| — | **indexed-access-lab subtotal** | | **0 / 3 yes** |
| `array-shape-lab` | `TupleOf` (`[[], [Fill], [Fill,Fill]][Length]`, `Length extends 0\|1\|2`) | index is a **numeric-literal union**; `keySetOf` only handles `TypeFlags.StringLiteral`, not numeric literals — a real, narrow, separate gap | **No** |
| `keyof-lab` | `values()` (`T[keyof T]`, `T` unconstrained) | `operandShape` has no `Constraint` to fall back to on an unconstrained generic — genuinely no F# form is possible here, lib-follow or not | **No** |
| — | (same mechanism as `keyof-lab`, listed here for completeness) `type-fest`'s `ValueOf` above is counted in the type-fest row; both are the unconstrained-operand family | | |
| `solid-js` | `NoInfer` | `[T][T extends any ? 0 : never]` — the index is itself a deferred conditional type; `keySetOf` has no `Conditional` case | **No** |
| `solid-js` | `CreateResource.Options2.initialValue`, `.storage()()`, `.storage(init)`, `.OnHydrated.Info.value` (and the `Options4` mirror, same four) | all inline `NoInfer<T>` at the call site — same deferred-conditional family as `NoInfer` itself | **No** |
| `solid-js` | `createComputed(fn)(v)`, `createRenderEffect(fn)(v)`, `createEffect(fn)(v)`, `createMemo(fn)(v)` | `EffectFunction<undefined \| NoInfer<Next>, Next>` — same family | **No** |
| `solid-js` | `on()()` (×2), `on()(v)` (×2), `on(fn)(prev)` (×2) | `NoInfer<Prev>` / `NoInfer<Next>` in `on`'s two overloads — same family | **No** |
| `solid-js` | `SplitProps.Result.Item.Item.[]`, `For.Props.Children(item)`, `Index.Props.Children(item)()` | **not attributed within this pass** — flagged rather than guessed; these 3 did not resolve to a `NoInfer` site under inspection and need a separate look | **Unknown (3 sites)** |
| — | **solid-js subtotal** | | **0 / 22 yes, 3 unattributed** |

**Total: 15 of 53 confirmed to share the §1 mechanism** (`animejs` 13 + `type-fest`'s
`ArrayLength`/`StringLength` 2). 35 of the remaining 38 are attributed to four other,
structurally distinct causes (chained indexed access, numeric/numeric-literal index unsupported
by `keySetOf`, unconstrained-generic operand, deferred-conditional/mapped self-index). 3
`solid-js` sites are unattributed.

## 3. Headline correction

**15 of the 53 share the zero-member-lib-operand cause** (`animejs`'s 13, `type-fest`'s
`ArrayLength`/`StringLength` 2). The other ~38 split across four structurally distinct
mechanisms:

- **Chained indexed access** (`T[K1][K2]`, the outer operand is itself an `IndexedAccess` type,
  not a `TypeParameter`) — `@cloudflare/workers-types`'s 5, `indexed-access-lab`'s 3. Already a
  documented floor in `indexed-access-lab`'s own source comments.
- **Numeric-literal / `number` index unsupported by `keySetOf`** — `type-fest`'s `ArrayValues`,
  `array-shape-lab`'s `TupleOf`, and folded into `ExtractRestElement`.
- **Unconstrained-generic operand** (`T[keyof T]` with no `extends` on `T`) — `keyof-lab`'s
  `values`, `type-fest`'s `ValueOf`. Genuinely no F# form is possible here.
- **Deferred conditional / generic-mapped self-index**, the §4.11 family this wave already
  declines under `SA002`/`TR045` — `type-fest`'s `TaggedUnion`, `GetTagMetadata`, `UnionLength`,
  and the bulk of `solid-js`'s 22 (`NoInfer` and every call site that inlines it).

## 4. Cost reasoning

No live instrumented measurement was run (would need a `getPropertiesOfType`/
`getIndexInfosOfType` round trip added to the shortcut path and the harness re-run over the full
55-fixture corpus — out of scope for a recon pass, and the analytic case against it is already
strong):

- **Following broadly — dropping the §1 shortcut for every named non-`Ship` object — is the
  wrong shape of fix.** The shortcut exists specifically to keep `Widen`-group expansion bounded
  (`Resolve.fs:606`'s own comment: deferring to a name that does not exist "widens the whole
  expansion to `obj` and loses the operand"). Removing it for all named non-`Ship` types would
  pull `lib.dom`'s whole prototype graph — every DOM interface any `HTMLElementTagNameMap` value
  type transitively reaches (`HTMLAnchorElement`, `HTMLButtonElement`, ... down each one's own
  base types and members) — into the type table for every fixture that touches a DOM type at
  all, not just the 13 `animejs` sites that would benefit. That is a corpus-wide table-size and
  generation-count increase for a 13-site (soon 15-site) gain.
- **The 15 sites that would actually benefit are reachable by a narrow fix instead**: resolve a
  named object's members on demand, only when it is the operand `indexedAccessRef` is actually
  about to index, rather than widening the general §1 shortcut for the whole group. This bounds
  the cost to exactly the operands indexed, which is what "follow narrowly" (the coordinator's
  stated preference) means concretely here.
- The other ~38 sites gain nothing from either shape of fix — they need `keySetOf` support for
  numeric/numeric-literal indices, chained-indexed-access resolution, or are genuine floors
  (unconstrained generics, deferred conditionals) already declined elsewhere in this wave.

## 5. The decline, and what would close it

**Declined for this pass.** The mechanism is confirmed and the recoverable set is small and
precisely bounded (15 sites: `animejs`'s 13, `type-fest`'s `ArrayLength`/`StringLength`), but a
correct narrow fix needs plumbing that does not exist today: `Shape/` currently only ever reads
what `Resolve.fs`'s breadth-first walk already populated into `model.Types` — there is no path
for a `Shape/` pass to demand-resolve one specific operand id mid-shape.

What would close it:

1. Give the resolve tier a demand-resolve entry point — a function in `Resolve.fs` that, given a
   type id currently answered via the §1 shortcut, runs `deriveStructure` for it on demand and
   returns the populated `Members`/`IndexInfos` (bypassing the shortcut for that one id only,
   not its whole group).
2. Call it from `indexedAccessRef` (`Shape/Spec.fs:1505`) — specifically from
   `indexedAccessValues` (`Shape/Spec.fs:1176`) — at the point where `operandShape` returns a
   `TypeFacts` with an empty `Members`/`IndexInfos` pair for an object-flagged, non-mapped
   operand under a non-`Ship` disposition. That is the exact and only trigger condition; every
   other `TR020` cause in the table above would still correctly stay `None`.
3. Re-run the corpus and confirm the delta is exactly `animejs`'s 13 and `type-fest`'s 2 moving
   off `TR020` (to whichever tier the recovered value type resolves to), with zero movement
   anywhere else — that is the acceptance test for "narrow," not "broad."

This is new plumbing, not a one-line change, and its cost should be priced as its own lane rather
than folded into this recon.
