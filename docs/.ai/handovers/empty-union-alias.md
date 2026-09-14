# Erased unions over local object aliases

The actual Workers producer emitted TraceItem.event as an optional erased union over ten
event interfaces. Two of those interfaces are explicitly empty in workers-types: the
producer emitted named aliases for obj, while Containers reached them only through the
property and widened them directly. All other TraceItem API fields agreed. The strict
catalog API check correctly rejected this inconsistent output.

The minimal empty-union-alias lab contains Empty {}, Data { id:string }, and
Value { event: Empty | Data | null }. A separate consumer imports Value. Its registered
catalog test failed first with the Value API mismatch while 53 other catalog tests passed.
The fixture also covers a named alias chain and producer-only callable, indexed,
inherited, and generic empty interfaces.

The late normalize-obj-unions pass uses actual emitted local nongeneric FsAbbrev targets.
It applies the existing TR035 rule to erased unions containing an alias that reaches obj,
with a visited-name guard for cycles. It does not expand external/reference names or
generic phantoms, and interfaces/delegates/indexed records retain their contracts. The
existing declaration-reference traversal preserves option and other container wrappers,
updates construction helpers consistently, and runs before catalog authentication.
No finding cases or catalog source/API/arity/constraint guards changed.

Validation: all 54 focused catalog tests pass. The typed consumer passes shared values
and writes obj option through the shared property, exercises the alias chain, checks that
callable/indexed/inherited/generic controls still require erased unions, and rejects a
changed input. Fresh FCS checking reports zero errors and warnings. The complete regeneration and independent check phases each pass 896 Generator and
90 Wire tests. The compile gate and 461 Fable runtime checks pass. Of the 103 prior
golden trees, 102 remain byte-identical; only the Workers fixture changes.
The only existing golden change is Workers TraceItem: its event property and Create
parameter become obj, and the unused generated U10 helper disappears. Workers tier
counts change from 412/1072/369/111 to 412/1071/370/111 (exact/ergonomic/widened/escape),
with one additional TR035 finding. The added fixture has 2 exact, 1 ergonomic, 2 widened,
and 1 escape symbol; the escape is the existing GE004 group-module naming advisory.

Actual diagnostic artifacts live in CloudEdge under
artifacts/one-shot-20260913/core-catalogs-native-obj-union-normalized. The driver records
and verifies the copied tool payload hashes and unchanged source profile, uses the exact
net8.0 support pins, and gives Runtime.Workers sole ownership for Containers catalog reuse.

The actual native probe passes the former TraceItem mismatch, then reaches an independent
WorkerEntrypoint API mismatch. A diagnostic copy records the exact delta under
core-catalogs-native-entrypoint-api-diagnostic/entrypoint-api.diff: the producer emits the
abstract entrypoint constructor and strips optional hook properties, while the consumer
sees an ordinary interface. The entrypoint lane owns that follow-up; this commit does not
claim completed native generation or Request/switchPort composition.
