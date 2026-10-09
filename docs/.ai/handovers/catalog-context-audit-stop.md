# Stopped implementation: Xantham → FSharp.CloudEdge contract preservation

**Status: interrupted, dirty, uncommitted, incomplete. User ordered STOP and handoff.**
No further implementation, builds, tests, regeneration, commits or pushes were performed after that order. Do not interpret this document as acceptance. The last production patch has not been built. A targeted test currently fails.

## Objective and scope

The deliverable is stricter **delivered FSharp.CloudEdge bindings**: preserve known TypeScript contracts in parameters and return types instead of widening them to `obj`. Myriad projections on top of already widened bindings do not meet that objective. Preserve genuine source-authored `any`/`unknown` honestly; distinguish those from compiler-error or unresolved types. The original integration motivation was Cloudflare Agents with Pi; other SDKs remain in scope where needed for ownership and reusable generator correctness.

The user accepts coordinated regeneration under an explicit shared owner. Do not weaken catalog validation or replace types with casts to make generation pass. No claim that remaining defects are unfixable has been established.

## Heads and working trees at stop

| Repository | Branch | HEAD | Tracked working diff |
|---|---|---|---|
| `/home/hhh/repos/Xantham` | `fix/myriad-early-projections` | `2ecbd73cff0ee95b46ebc68ade560a473307e974` | 60 files, +1619/−313 |
| `/home/hhh/repos/FSharp.CloudEdge` | `fix/myriad-early-projections` | `ae366aebc817282852f4c870faa543dd2133621e` | 115 files, +1875/−2818 |

No commits or pushes in this increment; `git diff --shortstat HEAD..HEAD` is empty in both. The table is the **uncommitted** diff, excluding untracked files. Xantham entered this increment with 60 files, +1333/−286. CloudEdge's tracked diff was unchanged by this increment. No fresh remote fetch was performed at stop.

Current increment production/test edits are confined to:

- `src/Xantham.Generator/Shape/Spec.fs`
- `src/Xantham.Generator/Shape/Anonymous.fs`
- `src/Xantham.Generator/DeclarationCatalog.fs`
- `tests/Xantham.Generator.Tests/DeclarationCatalog.test.fs`

Those files already contained inherited changes. Do not attribute their entire HEAD diff to this increment. Entry hashes are in `tests/.scratch/catalog-context-audit/entry-state.json` (hash inventory, not a backup of entry contents). **No Resolve.fs changes were made by this increment**; its dirty changes are inherited.

Preserve these untracked items:

- `docs/.ai/handovers/catalog-literal-union-ownership.md`: original user dossier, untouched.
- `docs/.ai/handovers/cloudedge-obj-elimination-takeover.md`: older historical takeover primer.
- `docs/.ai/handovers/cloudedge-obj-elimination-checkpoint.md`: appeared during this work; not authored or validated by this increment. Treat its claims independently.
- `tests/fixtures/generic-tag-lab/`, `tests/fixtures/unresolved-any-lab/`, and their corresponding golden directories: inherited labs, still untracked.

## Stop and process state

Agent inventory at stop: `catalog_review` completed; `cloudedge` and `obj_removal_lead` interrupted. No active child agent remains. Process inspection found no active task build, generator or `tsc` process. Existing idle reusable MSBuild nodes and the user's Ionide `fsautocomplete` remain; the editor was not stopped.

## Working constraints for successor

- Work only in the two primary checkouts above. No worktrees, clones, detached checkouts or copied repositories. Scratch stays under the repository's `tests/.scratch/`.
- **Targeted regressions first. Broad acceptance only after targeted fixes pass.** Do not rerun accepted baselines simply to establish them again.
- Do not invoke `boz`/Bozzetto for F#/Fable builds, analysis or leases. The user explicitly prohibited that. Retrieval helper scripts stored under a cache directory named bozzetto are separate from executing Bozzetto.
- Serialize builds sharing Generator/Core outputs; Fable restores can change Core assets.
- Read `docs/.ai/footguns.md` and applicable `.claude/rules/` before new generator changes. Extend the owning pass; avoid a second mechanism for an existing fact.
- Use one reviewer per increment. This increment used only `catalog_review` as reviewer; that agent performed no builds or edits.
- Do not spawn new agents without applicable authorization. Do not resume work merely because this handoff describes next steps.

## Retrieval and evidence binding

Retained retrieval schema: `tests/.scratch/catalog-context-audit/schema.response.json`, request beside it. Snapshot `0cbe94d87d33e4cd8d2f3f0437fa2b9860db04e798b93d6efa5d56ca09edcb97` was fresh/latest with matching snapshot IDs when obtained. It excluded Xantham, CloudEdge and Myriad. Therefore no `find → pgq → sources` sequence was possible for these repositories; bounded direct reads used the outside-index exception, and uncommitted work was inspected through git diff. Rediscover if index coverage changes.

No F# semantic MCP/SageFS tool was available in this session. Bounded source inspection and targeted compiler evidence were used; textual discovery is not semantic proof. A bounded LAN read is retained in `lan-review.txt`; its suggestions were not accepted as verification and included advice to weaken checks that was rejected.

Template consulted: `/home/hhh/repos/Bozzetto/docs/FFI_Increment_Handoff_Template.md`. Its clean/pushed/full-gate expectations are **not satisfied** by this interrupted handoff. All paths below are relative to `tests/.scratch/catalog-context-audit/` unless stated otherwise. Logs prove particular intermediate states, not the final unbuilt source tree.

## Implemented changes and targeted evidence

### A. Literal overloads on shipped dependencies

`Shape/Spec.fs`, `literalOverloadSets`: removed the entry-owned restriction. Named declarations shaped in shipped dependency groups now retain literal overload discriminator types. Existing group ownership carries nested declarations into the owner's group.

Reducer: owner Store has `get("text"): string` and `get("bytes"): number`; consumer reuses owner catalog. Test `shippedOverloadTests` compiles the distinct enum calls and rejects arbitrary string with FS0041. `overload-before.log` shows the catalog API mismatch; `overload-after.log` passes. `shipped-after.log` has **2 passing tests**, covering this and B. Reviewer accepted the pass change. Standalone shipped generation without references remains uncovered.

### B. Named alias intersection bases

`Shape/Anonymous.fs`, intersection traversal: follow an operand with a meaningful symbol name **or nonempty AliasDeclarations**. Named object aliases can have body symbol `__type`. Missing their alias anchor flattened consumer inheritance and changed the API.

Reducer: `Base = { name: string }`, `Result = Base & { id: number }`. Before: catalog API mismatch. After: consumer compilation verifies inherited Base and id. `intersection-before.log`, `shipped-after.log`. Reviewer accepted. Generic intersection variant remains uncovered.

An exploratory qualified-group test passed before any fix and was removed. **No canonicalNames/qualification fix was made.**

### C. Source-closure authentication

`DeclarationCatalog.fs`: authenticate producer Sources against actual current program inputs instead of requiring equality with the consumer's graph-derived closure. Every producer source must be present and equal; all current copies for the same package/version/file key must agree. Every producer handle must be anchored in producer Sources. Existing API/arity/constraint/conflict checks remain.

Positive regression adds an authentic extra source to the producer closure, accepts it, and rejects changed source bytes. `source-before.log` fails under old list-equality behavior; `source-after.log`: **2 pass** (JSON/Brotli). Existing and added mutations (`source-missing`, `source-removed`) in `source-negatives.log`: **26 pass**. This does not prove producer closure completeness.

**Current failing test:** `unchanged nested dependency versions remain separate inputs` was extended to reject same-version conflicting installations. `duplicate-source.log`: **2 failures, “Expected f to throw.”** TypeScript package-ID dedup likely prevents both physical conflicting files from entering the program. Next implementation must explicitly include both files via relative imports or triple-slash references and assert both differing Source records actually exist before asserting rejection. Do not weaken the production guard to satisfy a fixture that did not create the intended condition.

### D. Intrinsic empty object identity

`DeclarationCatalog.fs`: added structural role `empty-object` for checker intrinsic `{}` with no source handles. Guard requires Object, MembersResolved, no NotFollowed entry, not Mapped, Unclassified, no declaration file/aliases/arguments, no members/indexers/call/construct signatures/type arguments/response type parameters or target. MembersResolved alone is not proof local derivation succeeded.

Observed origin: `ObjectConstructor["keys"]` has an overload using checker intrinsic `{}` (type 38 in the captured program), synthetic `__type`, no declaration handle. Missing this identity cascaded into a private Util namespace. `empty-object.log` contains facts. Temporary production diagnostic instrumentation was removed.

Regression `privateNamespaceTests`, named `declaration catalog namespace values retain intrinsic empty object arguments`: producer/consumer and compiled string[] return. `namespace-privatekeys.log` is old failure; `namespace-after.log`: **1 pass**. Added mutation replacing the intrinsic with shallow facts + NotFollowed; `namespace-guards.log`: **1 pass**, before the final namespace patch.

**Pending interaction:** the final unbuilt namespace patch below can bypass child structural identities, so this negative test may no longer throw. Test design needs repair without discarding the empty-object safeguards. An alternative object-alias fixture was explored in `object-lab/`: `export type Util = { keys: ObjectConstructor["keys"] }`, imported as exported `helpers: Util`; it generates `Identity.Util.Helpers`. That alternative was **not applied to the test**.

### E. Open/closed generic union identity

`DeclarationCatalog.fs`: helper `boundParameters` combines declared/free parameter IDs but filters to actual TypeFlags.TypeParameter nodes. Used for identity normalization and closure binding. Concrete AliasTypeArguments had been normalized as binders and omitted from closure, collapsing open/closed types.

Regression `specializedUnionTests`, `declaration catalog distinguishes closed and open extracted generic unions`: dependency Outcome<R>/State<R>/Record<R> with Extract arms; consumer writes Record<string> and reads Record<R>. Compiles both arities. `pi-lab-before.log` has conflicting identity at arity 0/1; `pi-lab-after.log`: **1 pass**. Real Pi raw generation in `pi-fixed-probe.log`: **success**. Reviewer accepted binder filtering. Later helper refactor built in `guard-build.log`; do not claim the final tree was tested after the namespace edit.

Real collision before fix: `745b5d199ea16be896e2d4d3f5344b3190d5bd98b6965fb12f62d5647f5f54cc`, `StorageWrite4.Value2.State` arity 0 vs `Harness.GetTask.Result.Item2.State` arity 1. Concrete JsonValue vs R fields were confirmed. Reducer is `pi-lab/`; facts in `pi-shape.log`, `pi-parents.log`, `pi-lab-shape.log`.

### F. LAST PRODUCTION EDIT — namespace nominal identity, UNBUILT/UNTESTED

`DeclarationCatalog.fs`, inside typeIdentity: introduced `namespaceValue` when all actual-symbol declaration handles are ModuleDeclaration or SourceFile, declarations are nonempty, type is not Mapped/Instantiated, and type/alias/declaration arguments are empty. Excludes such namespace values from structural identity treatment.

Intent: genuine uninstantiated namespace/module values can use source-anchored nominal identity without canonicalizing every generic conditional member to build the identity. This is **not accepted or verified**. Reviewer discussed the direction and guards, not this final implementation. Evaluate Reference and generic/outer parameter safeguards. Retain the complete actual-symbol handle set, not merely export handles.

Required shape coverage remains: typeof module, re-exported namespace, mapped derivative, class/function namespace merge, augmentation negative. Reconcile D's negative regression. Latest `namespace-lab/` was overwritten to `namespace util { function check<T>(value:T): T extends string ? string : number }` and an exported `helpers: typeof util`; `namespace-private-conditional.log` captures failure with the previously built DLL. Earlier namespace logs used the Object.keys fixture, so mutable scratch contents do not reproduce all older logs verbatim.

## Actual SDK probes: results and limits

**These are scratch generation probes, not delivered CloudEdge acceptance.** They do not establish consumer project compilation, Myriad-projected Pi integration, runtime behavior, packaging, or final obj-count reductions. No tracked CloudEdge output was regenerated by this increment.

| Target | Most recent scoped result before final patch | Evidence |
|---|---|---|
| kvassethandler | Generated | `kvassethandler-fixed-probe.log` |
| computer--artifacts | Generated | `computer--artifacts-fixed-probe.log` |
| workersaiprovider | Generated against newly generated scratch Workers owner | `workers-new-producer.log`, `workerai-new-producer.log` |
| agents-pi | Raw generation succeeded, references removed for isolation | `pi-fixed-probe.log` |
| zod-v4 | Still fails stable identity for SourceFile namespace `HomeHhhReposFSharpCloudEdgeProfilesAgentsNodeModulesZodV4ClassicExternal` | `zod-fixed-probe.log` |
| chanfana | Still fails genuine AbortSignal F# API mismatch | `chanfana-fixed-probe.log` |

Initial stage configs failed inference-profile compatibility before reaching the reported issue. Scratch configs aligned lib/types/groups/resolveNoInfer with Workers to reproduce it; see `aligned-reproductions.json` and `*-aligned.log`. WorkersAI initially advanced to another mismatch against the retained old producer; regenerating only Workers in scratch with the new shaper resolved that. Do not remove profile compatibility checks.

Inputs are under CloudEdge profiles: `kv-asset-handler/node_modules/@cloudflare/kv-asset-handler`, `workers-ai-provider/node_modules/workers-ai-provider`, `computer/node_modules/@cloudflare/computer`, `chanfana/node_modules/chanfana`, `agents/node_modules/zod`, `agents-pi/node_modules/agents`. Scratch `pi-shape-config.json` uses public `./harness/pi` and `./models/pi-ai` with shipped Pi dependencies and no references. Zod isolation config: `/home/hhh/repos/FSharp.CloudEdge/tests/.scratch/obj-elimination/regen/zod-probe/norefs.json`.

## Remaining Chanfana defect: confirmed Resolve width loss, no fix implemented

`chanfana-unresolved.log`: AbortSignal's throwIfAborted/addEventListener/removeEventListener/dispatchEvent were NotFollowed **“beyond the frontier width cutoff (32768)”**. Consumer has no inherits and emits FsObj properties; producer inherits EventTarget<Record<string,Event>> and has typed methods. Same actual declaration handle `4238.264`. Catalog rejection is correctly exposing lost API.

Resolve currently uses FollowDepth 20 / FollowWidth 32768 and drops an entire oversized generation for determinism. Existing follow modes constrain expansion. No patch to this behavior was made here. Raising the cap, accepting an arbitrary ID prefix, or ignoring the mismatch is not a demonstrated solution.

Reviewer proposal, **unimplemented/unverified**:

1. At width overflow, distinguish intrinsic leaves and canonical source declaration forms from instantiated expansion.
2. Prove canonical form using the type at the original declaration node, compared with the candidate TypeId; cache proof by handle. Source handles alone are insufficient because instantiated generic methods retain them while cloning signature parameters.
3. Batch queries/derivation. Keep transformed applications on existing identity-only/deferred paths. Preserve NotFollowed diagnostics; remove stale entries if the same ID later derives successfully.
4. Targeted regressions: stranded ordinary method, generic method returning nested union/anonymous shape, expanding instantiated-method chain that terminates, reordered declarations preserving behavior.

Resolve already has typed source/NodeHandle navigation around the declaration handling code; Wire provides `getTypeAtLocation` and `getTypeAtLocations`. Confirm node/type-form semantics before using them. Do not assume symbol type and declaration instance/value type are interchangeable.

## Gate ledger and unfinished acceptance

No broad suite was run by this increment. Earlier inherited reports state generic-tag/Scheduled<T> and Any-provenance work passed a full gate and review; that evidence was accepted, not rerun. Final tree is **not** covered by those prior gates.

Focused commands used:

```sh
dotnet build tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj -c Release --no-restore -v q
dotnet tests/Xantham.Generator.Tests/bin/Release/net10.0/Xantham.Generator.Tests.dll --sequenced --filter-test-case '<exact case substring>'
# Theory parent filters require --filter-test-list, not --filter-test-case.
```

A first theory filter ran zero cases; it was corrected to `--filter-test-list 'incompatible producers fail before output is written'` for the recorded 26-case pass. Do not count zero-case output as evidence. `build.fsx -- test --quick --filter` still builds the solution and was avoided after the user's targeted-only instruction.

Scratch `generate.fsx` calls Pipeline.run against Release DLLs; `inspect-shape.fsx` inspects pass facts. For raw `@38` selection pass FSI `--` to prevent response-file parsing. Probes used `XANTHAM_TSGO_EXE=/home/hhh/repos/FSharp.CloudEdge/node_modules/@typescript/typescript-linux-x64/lib/tsc` where required. Freeze relevant sources/configs/DLL closure during gates; no gate presently binds to the final namespace patch.

Residual ledger:

- **R01:** Last namespace nominal patch unbuilt/untested; review its guards and mutation interaction.
- **R02:** Duplicate-copy regression currently fails twice; fixture must prove both sources enter the program.
- **R03:** Chanfana width-cutoff contract loss unresolved; proposed Resolve approach not implemented.
- **R04:** Zod SourceFile namespace failure unresolved in last executed probe.
- **R05:** Identity behavior changed but compatibility remains IdentityVersion 2 / ApiVersion 3. IdentityVersion bump and coherent owner regeneration are outstanding.
- **R06:** Architecture documentation still contains “Identity keys are unchanged,” now inaccurate for this increment. Current root changes are not documented in owning phase records/footguns.
- **R07:** Standalone shipped overload, generic intersection, namespace shape matrix and discriminating mutations incomplete.
- **R08:** CloudEdge final regeneration, actual API compile/runtime evidence, per-target widening attribution and delivered Myriad contract checks incomplete. Four successful raw probes are not completion.
- **R09:** Previously reported one-line tool-pack fix has not been independently located/verified by this increment.
- **R10:** No final broad gate, final review acceptance, commit or push.

## Inherited evidence and growth

Prior work: `tests/.scratch/obj-elimination/{scope,scope2,implement,review,fixup,takeover-audit}/`. Source-closure counterexample in `scope2/source-closure-mismatch/receipts.txt` showed all producer sources byte/manifest-equal in consumer inputs despite differing graph closures. CloudEdge's `tests/.scratch/obj-elimination/regen/gen-all-2.log` reports selected39/current14/generated5/failed6/blocked14; this is a different run/counting frame from the earlier 29/35 report. Do not blend them.

Earlier diagnostic totals TR023 7,875; TP006/TR013 3,173; TR008/TR009 7,130 are inherited measurements, **not final verified deltas**. Inherited Xantham work includes tagged/inline union generics, error Any separation TR063/TR064, apiVersion3 and Core.TS regeneration. Inherited CloudEdge work includes xantham.source migration, ownership config and pins.

No new production file or owner was introduced by this increment. Existing Shape/Resolve/catalog authorities remain owners. The new file in this handoff is documentation owned by `docs/.ai/handovers/`, justified by the user's explicit request for stopped-work transfer. Scratch runners are local diagnostic evidence, not new product authorities. The inherited new labs belong to generic union and Any provenance regressions. Retire this handoff by merging durable facts into phase records/footguns when work is actually accepted; preserve the original dossier until its own retirement conditions are met.

## Suggested order only if the user authorizes resumption

First reconcile the unbuilt namespace patch and failing duplicate-source fixture using focused checks. Then implement a bounded Resolve regression/fix for genuine width loss. Complete missing shape/mutation coverage and compatibility/docs changes. Regenerate only affected owners/consumers and verify their emitted public contracts. Only after targeted fixes pass, perform the requested broad acceptance and review, report measured delivered API changes, and commit/push as authorized. Do not declare completion while any of R01–R10 remains unaccounted for.
