# Lane CF — a union written inline is offered a name, and claimed under it

Wave thirteen, branch `worktree-gen-wave13-cf`, forked at `522450b`. `detect-tagged-unions`
iterates `model.DeclNames`, so a discriminated union the author wrote inline at a member or a
parameter was never offered to the pass at all: `TailStream.Onset.info` is ten arms uniformly
tagged on `type` and read `obj`. `synthesize-anonymous` now mints a name for such a union through
the same `claim` every other hoisted shape uses, and the pass claims it from there.

One spelling of a union maps as the other does. `Onset.info` is a discriminated union in
TypeScript whether or not TypeScript gave it a name, and it is one in F# now.

## What was built

- **`Shape/Spec.fs`** — `taggedCaseFields` and `isTaggedCaseData`, lifted verbatim out of
  `detect-tagged-unions`' body so the naming pass and the declaring pass agree on which arms a DU
  case can bind. `taggedUnionShape` is unchanged.
- **`Shape/Anonymous.fs`** — `becomesTaggedUnion`, and the union arm of `needsName` widened from
  `isLiteralUnion facts` to `isLiteralUnion facts || becomesTaggedUnion facts`. Everything else is
  machinery that already existed: `claim` sanitises, nests under the owner, uniquifies and records
  `SY004`/`SY005`; `DeclOrders` and `order-declarations` place the declaration in source order;
  `declOrigins`/`emittingGroup` place it in the group its type came from. Nothing new was decided
  about ordering or placement, and the compile gate compiles every regenerated golden.
- **`tests/fixtures/shared-tag-lab/`** — extended with ten one-line arm interfaces and the four
  spellings below. Lane CB's six declarations are untouched.
- **Run gate** — `mintedTaggedCases ()`, four checks.

## The gate, and the classes left inline

`becomesTaggedUnion` claims a union when all three hold:

1. It hoists no nullish member. A nullable union stays an abbreviation carrying `option`, which is
   what `detect-tagged-unions` does with one, so a name minted for it would be declared nowhere.
2. `namedUnionByMembers` answers for it under no other name.
3. `taggedUnionShape` reports `Discriminated`, and every arm satisfies `isTaggedCaseData`.

Conditions 1 and 3 are hard: **a name no pass declares is written at every reference site and
declared at none**, which is a broken binding rather than a widened one. `TagCollides`, `Untagged`,
an arm carrying a method or a signature, and an arm above `TaggedCaseFieldBudget` all have to be
declined for that reason, not as a judgement.

Condition 2 is the judgement, and it came out of the corpus. Ungated, the run claimed
`ResponseCustomToolCallOutput.Output.Item` for a union `@cloudflare/workers-types` declares as
`ResponseInputContent` in both `index.d.ts` and `experimental/index.d.ts`: two type ids, one member
set. `ResponseInputContent` is already a DU, `namedUnionByMembers` already mapped the second id
onto it, and the mint replaced the author's own name with a path-derived one at two members. This
is lane BB's alias-application class in a different pass, and it is excluded the same way
`isLiteralUnion` excludes it.

Two further classes are declined by condition 3, and they are the recon's own sites:

- **`TraceItem.event`** — ten arms of which `TraceItemConnectEventInfo` and
  `TraceItemCustomEventInfo` declare no members, so they carry no tag and `taggedUnionShape` reads
  `Untagged`. It stays `obj` and stays a floor.
- **`TailStream.EventType`** — eleven arms, of which two are the halves of the `Log` intersection
  and carry `TypeFlags.Intersection` rather than `TypeFlags.Object`. `taggedUnionShape` never
  reaches its discriminant test, so the union is offered no name and the three
  `…tailStream()(event)` sites stay `obj`. **This is lane CB's open worklist item — admitting
  intersection-flagged arms in `isObjectMember` — and it is now the only thing between the fold
  and `EventType`.** The lab pins the spelling as a negative.

### The boundary was measured, and the width of the union is not it

The brief expected a class of inline union worth leaving inline, on lane BB's precedent.

Gating the mint on "the erased form would be `obj`" (`remaining.Length > ErasedUnionArity`) was
built and measured first. It claims exactly one union corpus-wide, `Onset.Info`: `DT002` +1,
`SY004` +1, `TR036` 9→8, `TR032` +2, `TR040` +1, tiers exact 530 / ergonomic 1633 / widened 777 /
escape 195, one golden touched. Every other claim in the corpus is a union of three or two arms
whose erased form is a `U3`/`U2` over arm interfaces this run declares, and no finding falls when
one of those is claimed.

What decides between them is the run gate's `workarounds`, item 1, which measures the erased form
directly:

> a U2 over two interfaces matches one arm whatever the value is

Fable type-tests a primitive alone, so an erased union over interface arms is written rather than
read: `match` reaches one arm for every value, and the workaround is a hand-written `"key" in $0`
test. D4 §4.5(2) grades the tagged form Exact and says to detect it aggressively; §4.5(4) grades
`U2`–`U9` "Exact writing; reading requires runtime tests the consumer writes"; §4.5(5) says an
output position should prefer the tagged form where it is detectable. Every claim this lane makes
is at a read position.

So the cap is not the boundary and the mint is not gated on it. `mintedTaggedCases ()` measures
both halves: a JavaScript-built `{ type: "jsrpc", methodName: "ping" }` reaches
`Wide.Event.Jsrpc "ping"` at ten arms, and `{ type: "email" }` reaches `Narrow.Event.Email` at
three.

**The narrow gate is one line and both measurements are here**, so reverting to it is cheap if the
wave wants only the `obj` recovery.

## Findings

No new case, and `Findings.fs` is untouched. Four existing codes carry the work: `DT002` on a
claimed union, `SY004` on the mint, `TR036` where the union is declined, and `TR040`/`TR032`
re-attributed onto the new declarations. No mint needed sanitising, so `SY005` is unchanged.

## Measurements

`dotnet fsi build.fsx -- findings`, over the whole corpus, before and after.

| | before | after |
|---|---|---|
| tiers | exact 530, ergonomic 1633, widened 776, escape 195 | exact 536, ergonomic 1637, widened 788, escape 195 |
| total findings | 16,870 | 16,910 |
| generator tests | 472 | 473 |
| wire tests | 90 | 90 |
| run-gate checks | 300 | 304 |

Per-key, whole corpus: `DT002` +7, `SP001` +13, `SY004` +6, `TR006` +10, `TR032` +3, `TR040` +1.
`TR036` reads 9 both times. Everything else is unchanged, including `DT001` 2, `DT003` 1, `DT004`
1, `TR018` 59, `TR020` 69, `TR023` 136, `TR037` 15.

Per fixture, which is where those totals resolve. No other fixture moved at all:

| fixture | tiers | keys |
|---|---|---|
| `@cloudflare/workers-types` | exact 240→241, ergonomic 1109→1110, widened 380→381 | `DT002` +3, `SY004` +3, `TR032` +3, `TR040` +1, **`TR036` 5→4** |
| `nominal-lab` | exact 3→4 | `DT002` +1, `SY004` +1 |
| `shared-tag-lab` | exact 3→7, ergonomic 3→6, widened 14→25 | `DT002` +3, `SY004` +2, `SP001` +13, `TR006` +10, `TR036` +1 |

**`TR036` corpus-wide is flat because the lab adds one on purpose.** The recovery is
`@cloudflare`'s 5→4; `shared-tag-lab`'s `OnEvent` is the `TailStream.EventType` spelling written as
a negative, and it records the `TR036` that leaves the total at 9. The remaining eight are
`@cloudflare`'s `TailStream.EventType` ×3 and `TraceItem.event`, and `animejs`' four
(`DurationKeyframes.Item.[]`, `Revertible`, `AnimationParams.[]`, `WAAPIAnimationParams.[]`, at
arity 10, 10, 14, 14 and none of them tagged).

The three claims in `@cloudflare`:

| site | before | after |
|---|---|---|
| `TailStream.Onset.info` | `obj`, `TR036` at 10 arms | `Onset.Info`, ten cases |
| `HibernatableWebSocketEventInfo.info` | `U3<…Close, …Error, …Message>` | `HibernatableWebSocketEventInfo.Info`, three cases |
| `ResponseOutputMessage.content` | `U2<ResponseOutputRefusal, ResponseOutputText>[]` | `ResponseOutputMessage.Content.Item[]` |

`nominal-lab`'s is `Record<string, Attr | GLAttr>`, whose value union becomes `Wide.Item` with two
tag-only cases.

**Tier movement, cause.** `Onset` stays widened on a `TR006` it already carried; the `TR036` it
carried is gone, and the new `Onset.Info` row is widened on a `TR040` — `cfJson: obj`, TypeScript's
`object`, the floor at recon §9.1 — which `FetchEventInfo` already carried. The loss is
re-attributed to a finer symbol, as in lane BB. The `TR032` +3 is `NullableHoistedToOption` on the
optional fields the new cases bind. `shared-tag-lab`'s `SP001` +13 and `TR006` +10 are the ten arm
interfaces the lab now declares and their `readonly` members.

`git diff --stat` over goldens: 9 files, +218 −30, of which `shared-tag-lab` is +158.

```
 @cloudflare/workers-types/Cloudflare.WorkersTypes.fs | 42 ++++++--
 @cloudflare/workers-types/manifest.json              | 22 ++--
 @cloudflare/workers-types/symbols.jsonl              |  5 +-
 nominal-lab/NominalLab.fs                            |  8 +-
 nominal-lab/manifest.json                            | 12 ++-
 nominal-lab/symbols.jsonl                            |  1 +
 shared-tag-lab/SharedTagLab.fs                       | 112 +++++++++++
 shared-tag-lab/manifest.json                         | 28 ++++--
 shared-tag-lab/symbols.jsonl                         | 18 ++++
```

Whole branch: 15 files, +442 −49.

`dotnet fsi build.fsx -- test` exits 0 with the run gate on; `dotnet build Xantham.slnx` succeeds,
so the compile gate compiles every regenerated golden.

## Lab fixture and gates

`tests/fixtures/shared-tag-lab/index.d.ts` gains ten arm interfaces and five declarations, one per
class:

| declaration | what it pins |
| --- | --- |
| `Wide.event` | ten arms inline at a member position: minted `Wide.Event`, `DT002` + `SY004`, no longer `obj` |
| `OnWide` | the same union at a callback parameter — one type id, one declaration, written at both |
| `Named` + `Alias.event` | the member set a declaration already answers for: reads `Named`, mints nothing |
| `Narrow.event` | three arms inline: claimed at any width, so the mapping does not turn on the spelling |
| `OnEvent` | nine arms and the `Log` intersection — `TailStream.EventType` verbatim, offered no name, `TR036` |

`Pipeline.test.fs`'s existing `shared-tag-lab` block gains one test asserting all five, so the
fixture needs no new registration. `Program.fs`'s `mintedTaggedCases ()` runs four checks over the
golden: a case with a field and a tag-only case each erase to the tagged object, and a
JavaScript-built object reaches the case its tag names at ten arms and at three.

## Worklist

- **Admit intersection-flagged arms in `isObjectMember`** (lane CB's item, unchanged in priority).
  It is now the whole of what stands between this pass and `TailStream.EventType`'s three sites:
  the naming half is landed, and the union is declined only because two of its arms carry
  `TypeFlags.Intersection`.
- **A claimed union restates arms this run declares.** `HibernatableWebSocketEventInfo.Info`
  carries the fields of three interfaces that remain declared beside it, and a value of one of
  those interfaces can no longer be passed where the union is expected. This is the price D4
  §4.5(2) already pays for every named discriminated union, and it is now paid for inline ones
  too. If the wave judges it too high for a union the erased form could have written, the gate to
  add back is `remaining.Length > ErasedUnionArity`, measured above at one claim corpus-wide.

Nothing unexplained.
