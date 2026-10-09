# Xantham and CloudEdge migration audit

**Auditor return, 2026-10-09. Verdict: changes required for the proposed migration.**
The literal-union collision is confirmed. The existing producer catalog mechanism can
establish one owner, but consumer-only arbitration cannot repair independently emitted
F# enum types. The owner selected coordinated regeneration with an explicit shared owner
after reviewing the counterexamples. Implementation is on `fix/catalog-composition`,
based on `develop` `2a1aafe65e649b63de62dc021f166869d3fd09c5`.
The full integrated gate passes. On 2026-10-09 the owner explicitly requested that this
checkpoint be committed and pushed for Shayan's review, with the unexplained Core.TS
catalog identity drift (CORETS-1 below) retained as an open finding. That instruction
releases the earlier commit hold; it does not establish full migration acceptance.

**Integration scope.** The primary target is Cloudflare's
[durable Pi integration](https://developers.cloudflare.com/changelog/post/2026-10-02-pi-harness/):
Pi AI provides models, Pi Durable owns the agent loop and recovery, and `PiHarness`
connects that loop to Durable Object storage and lifecycle wake-ups. The documented
[Pi setup](https://developers.cloudflare.com/agents/harnesses/pi/) requires `agents`,
`@earendil-works/pi-durable` and `@earendil-works/pi-ai`.
TanStack AI remains a separate desired integration for the Partas.Solid work;
[@tanstack/ai-solid](https://tanstack.com/ai/latest/docs/api/ai-solid) provides upstream
Solid support, while F# composition still needs its own acceptance. OpenCode remains
an option under investigation. Neither is a prerequisite of the documented durable Pi
setup. Their inclusion in the broader audit must not turn their unresolved binding
issues into blockers for unrelated integration paths.

These findings concern Xantham's F# representations and composition, not demonstrated
failures of the upstream JavaScript libraries. Keep generator correctness defects
separate from F# expressibility limits; use each integration's actual typed and runtime
boundary to determine acceptance. Broad SDK export counts alone do not establish that
the useful integration surface has reached a translation limit.

**Original audit binding.** Xantham `4216cac45554e9ac66bd5242f819952f0317df3b` and CloudEdge
`3bf08618837910e433b779cf6c0396fc900a1080` equal their local origin refs. CloudEdge
is clean; Xantham originally had only the owner's untracked dossier. No remote-head
freshness is inferred from local refs. The new Pi/OpenCode names are scratch migration
outputs: committed CloudEdge still pins `agents@0.22.0`, while the candidate uses `0.27.0`.
CloudEdge's actual tool manifest pins `0.1.0-local.8e4c7b11b0ac90236c25`, not the alpha.7
pin attributed to it by the dossier. This does not explain the reproduced master failures.

The candidate closure pins `agents` 0.27.0, `@earendil-works/pi-ai` and `pi-durable`
1.1.0, `@opencode/sdk` and `@opencode/plugin` 2.0.26, `@tanstack/ai` 0.66.0,
MCP client/server 2.0.0 and SDK 1.30.0, Workers types 5.20261009.1, `ai` 7.0.93,
`zod` 4.1.12 and Node types 22.20.1. Its 569-package lockfile SHA-256 is
`a69090478022d8696cf6c70e2f6819901961e78eacd1a7c29c5d4b0de011a1a7`.
These are the probe's resolved inputs, not updated CloudEdge delivery pins.

**Development baseline correction.** The owner's recollection prompted checking the
nullable-alias repair [#109](https://github.com/shayanhabibi/Xantham/pull/109),
`1d2bd62`, on `develop` and absent from `master`. It gives recovered aliases wrapped in
`null`/`undefined` a `nullable-alias` identity, preserving the distinction between named
and anonymous literal types inside mapped options. Its existing typed regression is
preserved. The public NuGet `xantham` 0.1.0 package records commit `c68c15f`, predating
this repair; merged source and published tools are separate evidence.

After fetching both refs, the audit built clean `develop` and reran the literal kit,
TanStack reducer and real package, two Pi reducers, and both Pi producer profiles.
Every relevant failure reproduced with freshly generated develop catalogs. The literal
identity remains `46b8804e0130b515576e65acfa3f427ae369e0bb30bc51effbe7997903045b1c`.
The original Pi profile reaches Agents and fails at `ToolRegistration`; the shared
`ship` profile fails earlier in PiDurable at `Context`. No master catalogs were reused.
Receipts: `develop-literal/receipts.json`, `vendor/develop-baseline/{receipts,binding}.json`,
and the Pi lane's `develop-run/{matrix,binding,frozen-executable-verified}.json`.

Develop also includes schema 2 portable compatibility (#111) and Brotli transport (#112).
Schema 2 checks compatibility and compiler source identity; an unconditional identical
generator/compiler binary requirement is obsolete. Legacy/unverified cases retain
fingerprint checks. Plain JSON remains the default transport. Neither transport nor
compatibility metadata removes source, API, constraint or owner validation.

The owner's later recollection describes `U2<StringEnumDU, int>` emission from another
local branch. That branch is not available here; the owner confirmed it may be unpublished
on the maintainer's machine. Known mixed-literal support (`741dd6b`) and nullable-alias
recovery (`39a0123`) are already on both fetched heads. The local history search did not
identify the recalled newer change. This audit makes no claim about that unavailable branch.

**Findings.** E = executed, R = source read, I = inference. Source paths in this table
refer to the original audit revisions above unless explicitly marked otherwise. The
development reruns confirm the failures, not every historical line number or measurement.

| ID | Severity / grade | Evidence | Finding and required exit |
| --- | --- | --- | --- |
| CAT-1 | significant / E,R | Xantham `DeclarationCatalog.fs:328–365,744–761,1109–1117`; `catalog-design/*results.json` | Two independent producers publish the same structural identity under different F# owners. Reversing catalog order still exits 4. Coordinated B→A generation and typed consumption pass. Record the owner in the producer DAG and gate the typed diamond; preserve the rejection for incompatible independently emitted owners. |
| GEN-1 | blocker / E | Scratch vendor `minimal-shape-matrix-receipts.json`; Xantham `DeclarationCatalog.fs:1538–1566` | TanStack's within-run double claim reduces to three lines below. Nullable objects, promises, arrays and TanStack itself are unnecessary. Fix the constraint/default handling in its owning pass and retain compatible/incompatible controls. |
| PI-1 | blocker / E,I | Scratch `vendor/agents.log`, `pi-source-comparison.json`, `pi-source-disk-validation.json` | Fresh Pi producers still fail consumption at `ToolRegistration`; all 29 recorded source and package hashes match disk. A counterfactual consumer emits 9 matching sources and a different API. Program-dependent closure is implicated; source/API authentication must remain intact. Exit requires a reduced cause and a composed typed Pi consumer. |
| CE-1 | blocker for migration acceptance / R | CloudEdge `config/sdk-delivery.json:13–68,382–484`; `tests/SDKComposition/SDKComposition.fsproj:11–24` | Pi producer ownership and model/harness/skills composition are absent from the committed migration graph and gate. Extend the existing catalog-import/project graph and gate the actual Pi boundary. This is not a demonstrated regression in the existing release. |
| KIT-1 | significant / E,R | Scratch `catalog-audit/run.sh:22–23`, `vendor-case/chain.py:2,13–17` | `run.sh` logs generator exit 4 but itself exits 0. The vendor kit also retains session-specific paths and assumptions. Preserve real process status and provide reproducible in-repo inputs before using either as a gate. |
| COST-1 | significant / E | Scratch `vendor/measurements.json`; `opencode-compile/project-evidence.json` | Full-export cost does not establish that the adapter requires an Effect owner. Scoping to `./workerd` sharply reduces output, but its consumer compile fails with seven FS0039 errors for missing `FSharp.CloudEdge.Plugin`. Close the selected Plugin ownership dependency and compile the real adapter before accepting the scoped output. |

**Discriminating shape matrix.** These are focused audit probes, not an unfiltered gate.

| Shape / control | Observed result |
| --- | --- |
| Independent A and B, identical inline union, both catalogs | Generator exit 4; identity `46b8804e0130b515576e65acfa3f427ae369e0bb30bc51effbe7997903045b1c` |
| Catalogs reversed / identical A catalog repeated | Exit 4 / exit 0 |
| B regenerated against A; consumer references both | Generation and typed field transfer pass |
| Independent producer fields transferred | FS0193; whole-options control compiles |
| Consumer loads only A; parameter uses `BOptions["maxTokensField"]` | Generation passes, B field argument fails FS0193; coordinated B passes |
| Reduced constrained generic alias plus `U = never` consumer | Exit 4, incompatible declarations, constraints differ |
| Remove all defaults / equal defaults / remove constraints | Each generates successfully |
| Remove only alias default / remove only consumer default | Exit 4 / exit 0 |
| Pi fresh AI → Durable → agents | Producers succeed; consumer source mismatch |
| Pi consumer retains only AI catalog | Earlier/different `ModelCost` source mismatch |
| Mutate only ToolRegistration source set to the measured subset | Advances to `ModelCost` mismatch, still exit 4; not acceptance |

The independent reducer is:

```typescript
interface Def { id: string; }
export type Result<T extends Def = Def> = { definition: T };
export interface Middleware<U extends Def = never> { callback?: () => Result<U>; }
```

It uses `declarationCatalog: true`, `lib: ["esnext"]`, `types: []`; complete inputs and
exact commands are retained under `tests/.scratch/migration-audit/vendor/`.

**Measurements.** Full and scoped OpenCode were regenerated with the same compiler,
generator, dependency tree and inference profile. These replace the dossier's unbound
full-export figures for this audit.

| OpenCode surface | F# lines | Exact | Ergonomic | Widened | Escape | Effect textual mentions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Full export map | 128,837 | 51 | 409 | 3,170 | 3 | 11,215 |
| `./workerd` | 2,038 | 19 | 115 | 185 | 1 | 10 |

Fresh Pi.Ai matches the dossier: 5,643 lines, 147/206/187/11. Fresh Pi.Durable has
9,285 lines and 119/310/385/17, differing from its dossier row. Generation is not
compilation acceptance. The scoped OpenCode compile used Fable.Core 5.2.0 and the
existing local Core/TS Release DLLs, with unchanged input hashes and no source stubs.

**Rule controls and design.** Anonymous structural union identity is intentional
(`docs/.ai/plans/generator-architecture.md:1742–1758`). Named equal-valued aliases and
nominal TypeScript enums must remain distinct (`docs/.ai/footguns.md:33–36`). Consumer
preference cannot change an existing producer's nominal F# API. The selected producer
ownership policy must preserve source/API/arity/constraint checks, owner DAG acyclicity,
and reference-order invariance. The implementation controls below extend the existing
owning passes and catalog test suite.

**Acceptance boundary.** The existing CloudEdge Agents project's source, project,
configuration, bundle, sample and JavaScript adapter hashes still match its accepted
September 13 evidence. That baseline was authenticated, not rerun. It covers state,
restart and destruction through an adapter. The new gate must additionally exercise
`createModels`/registry → `Harness.open` → `PiHarness` → `lifecycle.use`, and the skills
`ToolRegistration` boundary, with typed F# and emitted runtime calls. This follows the
[official Pi integration](https://developers.cloudflare.com/agents/harnesses/pi/) and
[Pi model provider](https://developers.cloudflare.com/agents/models/pi-ai/) contracts.

**Implementation and rule controls.**

Schema 2 `identityVersion` and `apiVersion` advance to 2 for source-closure and emitted
constraint changes respectively. Earlier producer catalogs reject before substitution;
coordinated regeneration is required. Contract/inference/customization versions remain 1
because their policy/profile/customization semantics are unchanged. Existing version
negatives now compute a different value; a regression rejects both preceding contracts.

- CAT-1 uses the existing producer dependency mechanism: regenerate B against A, then
  consume both. The new lab covers opposite catalog orders, typed field transfer and
  indexed access. Equal-valued named aliases remain distinct; stale API/source inputs
  still reject. Removing B's dependency on A kills the regression. No consumer winner
  preference or second ownership authority was added.
- GEN-1 extends `Shape/Spec.fs` constraint proof: an uninhabited `never` default retains
  its nominal bound. Applied bounds such as `Def<string>` retain their complete F# type
  at argument sites, and equivalent emitted bounds compare independently of checker IDs.
  Nine focused tests cover default/nullability variants, valid consumption, recursive
  bounds, invalid generic arguments and catalog-constraint tampering. The initial
  never-only candidate introduced vendor compile errors and was rejected. The complete
  correction generates TanStack's catalog and reduces five distinct baseline FS0001
  sites to one shared `InputModalitiesTypes` error, with no introduced errors. Full vendor
  compilation therefore remains blocked. The old constraint proof fails two original
  controls; removal of the applied-bound correction is tested separately.
  Inhabited structural defaults retain the existing TP008 policy and its composition
  limitation; this is deliberately a `never` correction.
- PI-1 reduces in part to `Context { content: string | number }`, followed by a consumer
  exporting `Extra = Context['content']`. The compiler interns the anonymous content
  type; an export-only handle must not add the consumer file to Context's transitive
  declaration source closure. The bounded fix filters those handles in the existing
  catalog closure pass. Explicit aliases and source/API/input authentication remain.
  A separate six-line tagged-union reducer still fails API authentication: a producer
  selects a named alias while its consumer shapes an anonymous union. That residual
  prevents claiming a working composed Pi migration.

**Gates and limitations.** Clean develop Release CLI build passed, zero warnings/errors.
The actual compiler is native TypeScript `7.1.0-dev.20260902.1`; the root install was
reconciled with the lockfile before implementation acceptance. Artifact/compiler hashes
are in the receipts. `fslangmcp` is unavailable. Per the owner's correction, Bozzetto
is excluded from F#/Fable validation. Fable.SageFs at `53ed662` was used for an auxiliary
live Fable check: coordinated producer field transfer emitted JavaScript with zero
errors; independent producers returned two FSHARP193 diagnostics. Its Fable 5.16.2
compiler does not replace the repository's pinned Fable acceptance gate.

Core.TS was regenerated with `tools/fable-core-ts-input/xantham.json` plus
`declarationCatalog: true`, retaining the generated diagnostics and relocating the shipped
group through the existing compiler-lib layout. The ordinary compiler-lib stage omits
catalog mode and deletes diagnostics; that initial invocation's incidental output was
restored before the matching profile ran. The binding, manifest and all 4,289 diagnostic
records are byte-identical to develop. The catalog now has schema 2/current compatibility
and still has 3,288 declarations. Its previous provenance was
`@typescript/typescript-win32-x64@7.1.0-dev.20260913.1`; the repository pin used by this
gate is `@typescript/typescript-linux-x64@7.1.0-dev.20260902.1`. This explains substantial
catalog handle/source/hash churn without an emitted Core.TS API change.

**Integrated acceptance.** `dotnet fsi build.fsx -- test --run-gate` completed with exit 0:
1,197 generator tests passed (one Linux SolidJS golden exclusion), 99 Wire tests passed
(one ignored), 468 Fable runtime checks passed, and the Partas companion/plugin JSX gate
passed. The solution and compile gates passed; the site project emitted its NU1608
FSharp.Compiler.Service/FSharp.Core dependency warning. No tests were filtered. The first
attempt exposed six hardcoded-version mutation tests; those now mutate the emitted value,
and the full gate was rerun. Logs: `acceptance.log` (failed attempt),
`acceptance-final.log` and `acceptance-result.json` (accepted run, 154.324 seconds).
All nine recorded dependency hashes match before/after. Existing corpus findings are
unchanged; additions are confined to the three new labs. The applied-bound mutation
fails two of nine focused tests, and the source-closure and ownership mutations also fail
their respective controls. The acceptance run binds the complete working tree including
the regenerated catalog; it does not resolve the identity uncertainty described next.

**CORETS-1: unresolved catalog identity drift.** A fresh clean-develop generation with the same
compiler, inputs and catalog profile agrees with the integrated tool on all 3,288 names,
handles, source sets, roles, arities and constraints. It differs on five structural
identities (`String.Match.Matcher`, `String.Replace.SearchValue`, `SearchValue2`,
`String.Search.Searcher`, `String.Split.Splitter`) and the referencing `String` API digest.
These adapters carry symbol-keyed members dropped under MB002; their own API digests
and emitted F# bytes agree. A checker-generated symbol identifier entering a structural
key is a hypothesis, not an established cause. Exact records and comparison commands:
`vendor/core-ts-artifact-binding.{txt,json,py}`. The five historical constraint changes
normalize to the same named bounds; both historical role changes already occur with
clean develop. Those are explained separately from the five remaining identities.

The initial commit hold followed `.claude/rules/generator-fixtures.md`: “report it to
the user with the pointer and stop” on an unexplained large artifact change. After
receiving this finding, the owner requested committing and pushing the branch for
Shayan's review. The checkpoint includes the regenerated catalog with this uncertainty
documented. Branch publication does not resolve the metadata drift or certify a release.

**Context sources.** Fresh/latest retrieval snapshot
`0cbe94d87d33e4cd8d2f3f0437fa2b9860db04e798b93d6efa5d56ca09edcb97`, matching current
snapshot, no stale/pending sources. Xantham, CloudEdge and its two validation/scoping
siblings are outside that index; bounded direct reads use that exception. The handoff
template was retrieved with `schema → find → pgq → sources`: Bozzetto
`f7a742db01507d34dfd1d7d6d122be4cec5334c7`,
`docs/FFI_Increment_Handoff_Template.md:1–130`. Its earlier direct read preceded discovery
that Bozzetto is now indexed; retrieval subsequently verified the same text. LAN workers
provided bounded first-pass readings; their unverified suggestions were not accepted.

**Growth.** `git diff --shortstat 2a1aafe..HEAD`: 37 files changed, 223067 insertions(+), 221931 deletions(-).
CloudEdge `git diff --shortstat 3bf0861..HEAD` is also empty; it has no audit edits.
The Xantham checkpoint contains 37 affected files, including 26 new files. The
catalog alone accounts for +221,912/−221,898 lines, with no emitted Core.TS change.
Exact totals and every path are recorded in `growth.json`; the owner's original
untracked dossier is excluded and unchanged. Existing passes and test owners are extended;
no parallel ownership mechanism or new finding code was introduced.

New file inventory (brace lists enumerate every file):

| Files | Existing owner | Reason |
| --- | --- | --- |
| `docs/.ai/handovers/xantham-cloudedge-migration-audit.md` | Migration audit | Preserve unresolved, falsifiable findings; retire when this wave closes. |
| `tests/fixtures/catalog-generic-defaults-lab/{index.d.ts,package.json}` | Shape.Spec constraint mapping | Persist the default/applied/recursive-bound reducer and its compiler package identity. |
| `tests/fixtures/catalog-source-projection-lab/{index.d.ts,model.d.ts,named.d.ts,package.json,xantham.json}` | DeclarationCatalog source closure | Persist projections, explicit alias dependency and catalog profile. |
| `tests/fixtures/literal-union-ownership-lab/{index.d.ts,package.json,xantham.json}` | DeclarationCatalog producer ownership | Persist the consumer diamond and shared generation profile. |
| `tests/fixtures/literal-union-ownership-lab/node_modules/literal-owner-a-lab/{index.d.ts,package.json}` | Same ownership lab | Small independent producer A with structural and named literals. |
| `tests/fixtures/literal-union-ownership-lab/node_modules/literal-owner-b-lab/{index.d.ts,package.json}` | Same ownership lab | Producer B needed to distinguish independent from coordinated generation. |
| `tests/Xantham.Generator.Tests/golden/catalog-generic-defaults-lab/{CatalogGenericDefaultsLab.fs,manifest.json,symbols.jsonl}` | Pipeline golden corpus | Deterministic output and compile coverage for the constraint lab. |
| `tests/Xantham.Generator.Tests/golden/catalog-source-projection-lab/{CatalogSourceProjectionLab.fs,manifest.json,symbols.jsonl}` | Pipeline golden corpus | Deterministic output and compile coverage for the source lab. |
| `tests/Xantham.Generator.Tests/golden/literal-union-ownership-lab/{LiteralUnionOwnershipLab.fs,manifest.json,symbols.jsonl,groups/Identity.LiteralOwnerALab.fs,groups/Identity.LiteralOwnerBLab.fs}` | Pipeline golden corpus | Compile both independent ordinary producer shapes and the consumer. |

New lab tiers (Exact/Ergonomic/Widened/Escape): generic defaults 2/10/5/1,
source projection 9/8/0/0, literal ownership 2/4/0/2. Scratch files remain owned by this
audit for reproductions, hash binding and gate receipts; they introduce no product authority.

**Evidence packets and residuals.** `tests/.scratch/migration-audit/` contains
`catalog-report.txt`, `vendor-report.txt`, `cloudedge-report.txt`, command logs, JSON
measurements and hash receipts. PI-1 is reduced into a source-closure defect and a separate
unfixed named/anonymous tagged-union API mismatch. GEN-1's catalog failure is repaired;
TanStack still has one pre-existing `InputModalitiesTypes` consumer error and the general
inhabited-structural-default limitation remains. COST-1 lacks a closed Plugin dependency:
marking Plugin as `ship` exposes reciprocal OpenCode/Plugin F# references and still does
not compile. CE-1 lacks Pi acceptance. Validation and EmDash
siblings have no committed heads, so their local evidence is not revision-addressable.
These are explicit residuals, not accepted migration coverage.
