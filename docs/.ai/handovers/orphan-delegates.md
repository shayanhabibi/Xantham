# Orphan delegates handover — #84 follow-ups (83c8412, e24f896)

Branch `develop`, on top of the #84 merge `03bf57f`. Gated on the composed tree: `dotnet build Xantham.slnx`
0 errors, `dotnet fsi build.fsx -- test --update --run-gate` every stage ok, run gate 449 checks.

## #84 review gaps (83c8412)
- `Cli.test.fs:230` carried a `//FOR-REVIEW`, which trips the comment gate. Stripped; the test name ("a package with subpaths but no root export passes pre-flight") already carries it.
- `ExportLayout.test.fs:46` built its `@cloudflare/workers-types` path from `root` alone and errored in a fresh worktree. `mainCheckout` and the npm lookup moved to `tests/Xantham.Generator.Tests/Fixtures.fs`; `Pipeline.test.fs` delegates, so its call sites are unchanged.
- The conservative lifecycle-hook guard in `Shape/Anonymous.fs` stands, since the retraction pass makes the prediction unnecessary. Recorded in the subpath plan.

## `drop-orphan-delegates` (e24f896)
- `Shape/Orphans.fs`, pass 18, between `resolve-export-collisions` and `audit-coverage`. Collects every name read from a reference position across the finished declaration set and removes any `FsDelegateType` no declaration reads. Iterates to a fixed point, since dropping one delegate can leave another unread. Raises `DD001`.
- Inverts `a5a5f67`, which predicted in `synthesize-anonymous` which positions would read a callback. `shape-interfaces` builds the `inherit` graph and `interfaceNames` that decide the question, and both run after `synthesize-anonymous`, so the answer only exists later. `Inherited.dropInherited` is the precedent for reasoning over rendered decls.
- A delegate representing a harvested export is retained, even where the run reads it nowhere: an export's own declaration is read by consumers. `prune` checks this the way `audit-coverage` does — a candidate drops only when every harvested export stays represented without it. Failure mode to watch: a first version retained nothing and pushed `AC001` from 8 to 22; a second retained anything whose name fuzzily matched an export and dropped nothing corpus-wide.
- Four drops: `ExportedHandlerTailStreamHandler` (workers-types), `ResourceFetcher` and `Store.StoreSetter2` (solid-js), `OnFail` (the new lab case). `Store.StoreSetter2` is a `claim`-minted twin, so this covers part of the root-homing follow-up. solid-js widened 92 -> 91; every other tier and `AC001` flat; every package outside those three byte-identical.
- `orphan-callback-lab` gained `Wharf extends Error` — an entrypoint whose base this run leaves undeclared, the position `Anonymous.signatureShaped`'s `BaseTypes.IsEmpty` guard misses.

## Measured, not fixed: `solid-js/store`
- `AC001` is 7 on `master` and 8 now. #84 added `Store.SetStoreFunction`. Traced in the PR thread; the shaping gap predates #84, which routed an input into it for the first time.
- `Shape/Callbacks.fs` cannot represent an overloaded generic callable interface. `SetStoreFunction` has nine call signatures and no members, so `declaresInterface` (`Interfaces.fs:11`) excludes it, it becomes one delegate, `aliasTypeParams` (`Spec.fs:2411`) hoists all nine signatures' parameters onto one head, and `K1` lands twice under two bounds. `repair-arity` then drops it (`RA007`, added by `597f02f` for exactly this input).
- The damage is upstream of the drop: `TP002` x37, `TR020` x8, `TR031` x1 are all `shape-callbacks`, so the declaration was already `delegate of obj * ... -> unit` before `RA007` saw it.
- Consumer effect: `type StoreReturn<'T> = 'T * Action<obj, obj, obj, obj, obj, obj, obj, obj>` and `createStore<'T> : ... -> 'T * obj`.
- Fix: `docs/.ai/plans/2026-09-13-callable-interface-invoke.md`.
