# Orphan delegates and the #84 review gaps

Branch `develop`, on top of the #84 merge `03bf57f`. Gated on the composed tree:
`dotnet build Xantham.slnx` 0 errors, `dotnet fsi build.fsx -- test --update --run-gate` every stage ok,
run gate 449 checks.

## The three open items from the #84 review

- **`Cli.test.fs:230` carried a `//FOR-REVIEW`**, which trips the comment gate. Stripped; the test name
  ("a package with subpaths but no root export passes pre-flight") already carries it.
- **`ExportLayout.test.fs:46` built its `@cloudflare/workers-types` path from `root` alone**, so it errored
  in a fresh worktree. `mainCheckout` and the npm lookup moved to `tests/Xantham.Generator.Tests/Fixtures.fs`;
  `Pipeline.test.fs` delegates, so its call sites are unchanged.
- **The conservative lifecycle-hook guard in `Shape/Anonymous.fs`** is no longer worth closing: the retraction
  pass below makes the prediction unnecessary. Left as it stands, recorded in the subpath plan.

## `drop-orphan-delegates` (`Shape/Orphans.fs`)

Pass 18, between `resolve-export-collisions` and `audit-coverage`. Collects every name read from a reference
position across the finished declaration set and removes any `FsDelegateType` no declaration reads. Iterates to
a fixed point, since dropping one delegate can leave another unread. Raises `DD001`.

`a5a5f67` solved the same problem by predicting, in `synthesize-anonymous`, which positions would read a
callback. This inverts it: `shape-interfaces` builds the `inherit` graph and `interfaceNames` that decide the
question, and both come after `synthesize-anonymous`, so the answer only exists later. `Inherited.dropInherited`
is the precedent for reasoning over rendered decls.

**A delegate that represents a harvested export is retained**, even though the run reads it nowhere: an export's
own declaration is read by consumers. `prune` checks this the way `audit-coverage` does — it drops a candidate
only when every harvested export stays represented without it. Getting this wrong is the failure to watch for:
a first version retained nothing and pushed `AC001` from 8 to 22; a second retained anything whose name fuzzily
matched an export and dropped nothing corpus-wide.

Four drops: `ExportedHandlerTailStreamHandler` (workers-types), `ResourceFetcher` and `Store.StoreSetter2`
(solid-js), `OnFail` (the new lab case). `Store.StoreSetter2` is a `claim`-minted twin, so this also covers part
of the root-homing follow-up. solid-js widened 92 -> 91; every other tier and `AC001` flat; every package outside
those three byte-identical.

`orphan-callback-lab` gained `Wharf extends Error` — an entrypoint whose base this run leaves undeclared, which
is the position `Anonymous.signatureShaped`'s `BaseTypes.IsEmpty` guard misses.

## Measured, not fixed: `solid-js/store`

`AC001` is **7 on `master` and 8 now**. #84 added one: `Store.SetStoreFunction`. Traced in the PR thread; the
short form is that the shaping gap predates #84 and #84 routed an input into it for the first time.

`Shape/Callbacks.fs` cannot represent an overloaded generic callable interface. `SetStoreFunction` has nine call
signatures and no members, so `declaresInterface` (`Interfaces.fs:11`) excludes it, it becomes one delegate,
`aliasTypeParams` (`Spec.fs:2411`) hoists all nine signatures' parameters onto one head, and `K1` lands twice
under two bounds. `repair-arity` then drops it (`RA007`, added by `597f02f` for exactly this input). The damage is
upstream of the drop: `TP002` x37, `TR020` x8, `TR031` x1 are all `shape-callbacks`, so the declaration was
already `delegate of obj * ... -> unit` before `RA007` saw it.

Consumer effect: `type StoreReturn<'T> = 'T * Action<obj, obj, obj, obj, obj, obj, obj, obj>` and
`createStore<'T> : ... -> 'T * obj`.

The fix is `docs/.ai/plans/2026-09-13-callable-interface-invoke.md`.
