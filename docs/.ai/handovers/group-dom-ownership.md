# Grouped DOM alias ownership

Branch: `fix/group-dom-ownership`; isolated worktree: `.claude/worktrees/group-dom-ownership`.
Base verified as `ff35b89758881497e682f120d6385122bbc87fa8`.

## Reproducer

```ts
// index.d.ts
export declare function roundTrip(request: Request): Request;
// node_modules/worker-augmentation-lab/index.d.ts
interface Request { readonly cf: string; }
```

Use `lib=[esnext,dom]`, `types=[worker-augmentation-lab]`, ship the fake dependency, and enable a declaration catalog. No Cloudflare or Node package is required. The input is tracked as `grouped-dom-aliases-lab`.

The recorded pre-fix probe used immutable Xantham package `0.1.0-local.8826f584b6cda5986565` at a692a01. Generation succeeded: 7 exact / 0 ergonomic / 0 widened / 2 escape. Compiling group-before-root failed FS0039 at group line 20: `Probe.Root.RequestCache` was declared in the later root file. Root `roundTrip` simultaneously referenced the group-owned Request. The CLI build stopped at 15 errors; a fresh FCS project check reported 18 errors. Exact commands, package/compiler fingerprints, inputs, generated files and logs are preserved in the sibling CloudEdge repository under `artifacts/one-shot-20260913/group-dom-probe/initial/`.

Catalog mode matters: it recovers compiler-lib alias names such as RequestCache. The ordinary non-catalog fixture already nests the corresponding anonymous names under Request.Cache. The regression therefore includes a catalog-enabled producer and adapter, not only a golden generated in ordinary mode.

## Change

Semantic lookup traced `Render.renderSources` to `Pipeline.groupModulesForScope`, then its declaration origin/placement helpers. An unexported compiler-library literal alias had CompilerLib origin and fell back to EntryPackage even when its sole shipped dependency consumer was emitted in an earlier file.

`Pipeline.fs` now assigns such FsStringEnum/FsEnum declarations to their unique shipped dependency consumer, identified through the final F# declaration references. The existing `Shape.Orphans.readNames` visitor is exposed internally for reuse. Its pre-change FCS impact was two sites in one file; the new helper visibility adds no public signature.

Explicit exports, compiler-library shipping, entry-owned aliases, aliases read only by the root, and aliases read by several dependency groups preserve their previous ownership. Six focused cases enforce those boundaries. This change does not claim to solve ambiguous multi-group ownership or general package cycles.

The ordinary golden is compiled group-before-root by the repository gate, with a consumer reading Request.Cache and the added cf property. A separate declaration-catalog test generates a producer and adapter, authenticates catalog reuse, and compiles a consumer that passes the producer-owned Request through both root exports and reads its canonical RequestCache type.

## Validation

The full gate passed in 313 seconds: 866 generator tests and 90 Wire tests in both regeneration and independent checking, the compile gate, and 461 Fable runtime checks. Generator and Wire retain their two/one configured ignored cases. A fresh FCS generator check is clean. The eight-case grouped fixture/ownership subset also passed.

Existing golden F# files, manifests and finding counts are unchanged. The new golden has 17 root lines and 197 shipped-group lines, with 7 exact / 0 ergonomic / 0 widened / 2 escape symbols. A separate catalog-mode before/after measurement retains the same 7/0/0/2 tiers and finding-code counts. `Probe.Root.RequestCache` becomes `Probe.WorkerAugmentationLab.RequestCache`; its assembly/catalog owner remains `Probe.Root`. The catalog-enabled producer/adapter compile test proves reuse of that ownership.

Initial setup failures came from adding the compile consumer before seeding its golden, then omitting the typed fixture configuration. Both were corrected before the successful full-gate run. No existing large golden change required explanation or acceptance.

Commands/logs:

- Full gate: `XANTHAM_TSGO_EXE=/home/hhh/.cache/xantham/7.1.0-dev.20260902.1/node_modules/@typescript/typescript-linux-x64/lib/tsc XANTHAM_REQUIRE_TSC=1 dotnet fsi build.fsx -- test --update --run-gate`; log `/tmp/group-dom-full-configured.log`.
- Fresh generator FCS check: `/tmp/group-dom-final-check.jsonl`.
- Helper impact: `/tmp/group-dom-readnames-impact.jsonl`.
- Findings snapshots: `/tmp/group-dom-before-findings.log`, `/tmp/group-dom-after-findings.log`.
- Catalog-mode measurement: `/tmp/group-dom-catalog-after/manifest.json`, `/tmp/group-dom-catalog-after/declarations.json`.

No SDK sweep, deployment, package-pin change or catalog-profile relaxation was performed in this branch.
