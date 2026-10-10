# Takeover primer: remove widened `obj` from FSharp.CloudEdge

Prepared 2026-10-09, America/New_York. **Work stopped at the user's request.**
The user explicitly instructed: leave unfinished changes in place, document them, and stop.
This document records state and proposed continuation; it is not an acceptance report.

## The actual assignment

The user's exact correction was: **“The point of all this work is to REMOVE THE WIDENED OBJ FROM FSHARP.CLOUDEDGE.”**

The purpose is constrained, contractual F# APIs that preserve TypeScript information through
generation. Myriad was introduced before Shape loses that information. The preceding agent
instead completed a reusable projection mechanism and four Pi operation wrappers, then repeatedly
called the assignment complete. Those completion claims were wrong. The published work is useful
groundwork; the broader assignment remains unfinished.

Do not count renamed `obj`, opaque wrappers over lost contracts, dropped APIs, selected examples,
or lower aggregate finding counts as completion. Cover the delivered public type graph, including
inputs, outputs, callbacks, properties, indexers, generic arguments and input/result relationships.
Distinguish source-authored `any`/`unknown` from generator-induced loss, with source evidence.
Checker `Any` flags alone are insufficient: error/unresolved types can also appear as Any.
If a real source contract cannot be preserved, present that concrete limitation and decision to the
user; do not silently narrow the objective or claim the remaining loss is acceptable.

The motivating application is Cloudflare Agents SDK integration with Pi in Durable Objects.
OpenCode and TanStack were investigated as related vendor dependencies/opportunities. The user
also values TanStack's compatibility with Shayan's Partas.Solid work and does not want options
arbitrarily closed. This does not mean all 214 configured candidate targets are already delivered.
Freeze the existing delivery inventory and do not shrink it to improve a metric.

## Stop state and repository locations

All delegated agents are stopped: `cloudedge` and `obj_removal_lead` were interrupted;
`vendor_failures` had completed; `catalog_design` was no longer active. No agent-owned build or
runtime process remained when checked. The user's Ionide `fsautocomplete` process was left alone.
No implementation, tests, commits or pushes were performed after stopping; only this handoff and
its scratch state/patch capture were prepared.

| Repository | Primary checkout | Branch / committed HEAD |
| --- | --- | --- |
| Xantham | `/home/hhh/repos/Xantham` | `fix/myriad-early-projections` / `2ecbd73cff0ee95b46ebc68ade560a473307e974` |
| FSharp.CloudEdge | `/home/hhh/repos/FSharp.CloudEdge` | `fix/myriad-early-projections` / `ae366aebc817282852f4c870faa543dd2133621e` |

Both commits were pushed; local origin tracking refs match these heads. Remotes are
`https://github.com/shayanhabibi/Xantham.git` and
`https://github.com/fsprojects/FSharp.CloudEdge.git`.

**Both working trees now contain unfinished, uncommitted edits. Preserve them.** Exact patches,
changed-file hashes and status were captured inside Xantham at:

```text
tests/.scratch/obj-elimination/takeover/state.json
tests/.scratch/obj-elimination/takeover/xantham-working.patch
tests/.scratch/obj-elimination/takeover/cloudedge-working.patch
```

These patches are evidence/backups of the current edits, not instructions to apply them again.
The capture predates adding this handoff file. This handoff is intentionally uncommitted.

## Non-negotiable working agreements

- **Primary checkouts only.** Never create/use secondary worktrees, temporary clones, detached
  checkouts or copied repositories. All agents must use the same primary checkouts. Scratch
  fixtures and build evidence belong under the repositories' documented `tests/.scratch/`.
- **Do not run Bozzetto or `boz` for F#/Fable**, request its leases, or start its daemon. The user
  explicitly prohibited that. A retrieval HTTP helper happens to live under a cache directory
  named `bozzetto`; using that helper is separate from running Bozzetto.
- Read Xantham `AGENTS.md`, `.claude/rules/generator-fixtures.md`, the other applicable rule files,
  and `docs/.ai/footguns.md`. User primary-checkout instructions supersede older worktree guidance.
  There was no `AGENTS.md` at the CloudEdge repository root when checked.
- Use retrieval first. Require schema freshness=true, latest mode, matching snapshot/current IDs;
  then find → pgq → sources pinned to that snapshot for indexed repositories. The last fresh
  snapshot was `0cbe94d87d33e4cd8d2f3f0437fa2b9860db04e798b93d6efa5d56ca09edcb97`.
  Xantham, CloudEdge and Myriad were absent, permitting bounded direct reads. Refresh for a new
  session. Uncommitted diffs are a separate permitted direct-read exception.
- Retrieval protocol/authentication instructions:
  `/home/hhh/.cache/bozzetto/evidence/ffi-correction-2026-10-03/tools/README.md`.
  Never print credentials. Fresh schema evidence is in Xantham `tests/.scratch/obj-elimination/`.
  Use LAN workers for bounded first-pass reading; verify their claims. Several LAN suggestions
  in this task were incorrect and were not accepted as findings.
- Do not scan repositories wholesale or load huge generated bindings/`symbols.jsonl` into context.
  Aggregate findings, then inspect bounded relevant snippets. F# semantic MCP tools were absent
  from this session; record that limitation and use actual compiler/runtime evidence.
- Extend existing owning passes. New files/owners need a one-line justification. Report per-repo
  `git diff --shortstat <base>..<head>` and new-file ownership. Use one independent reviewer;
  implementer runs the shape matrix and discriminating mutations. Freeze running gate inputs.
  Handoff template: `/home/hhh/repos/Bozzetto/docs/FFI_Increment_Handoff_Template.md`.
- Serialize builds sharing Xantham project outputs. CloudEdge Fable restoration previously
  replaced Core's assets with a single-framework restore, causing Xantham NETSDK1005 for net8.0.
  Do not run competing dotnet builds. Use Xantham's build script, **not `dotnet test`**.

The user authorized local branch-built tooling, coordinated regeneration under an explicit shared
owner, work on these branches, commits and pushes. No package publishing or merge into develop
was authorized. The latest instruction is to stop; a successor needs the user's instruction to resume.

## What is committed and validated — groundwork, not completion

Xantham history:

```text
45be2390733cd09856372480adffccf16725c3c8  catalog constraints/source ownership (#113), develop base
0cab4e95992d519a05b3cc43ef9e3c13dd2656df  early Myriad union projections
2ecbd73cff0ee95b46ebc68ade560a473307e974  resolved input contracts and typed SDK operations
```

`Customization.Contract`/`Semantics` expose an opaque resolved snapshot between Resolve and Shape.
The operation shape model preserves strings, numbers, booleans, string literals, null, undefined,
arrays, records and unions. `Resolved.tryFindParameter` / `tryFindParameterField`, `shape` and
`operation` select source occurrences. Resolve owns declaration spellings and defining-source
closure. Snapshot seals/fingerprints prevent replay or unauthenticated source claims. Pipeline
checks operation receivers against their final Shape/catalog-qualified owner.

`src/Xantham.Generator.Myriad/Operations.fs` implements `Operations.create` and `createShared`.
It emits ordinary F# DUs/records, private encoders and typed instance-method calls. `createShared`
aliases the first selection's types after exact shape agreement. Required literal tags are emitted
automatically; optional fields separate omission from present undefined; null remains distinct;
own-property writes handle `__proto__`; strings reject F# null at runtime. Existing finite-union
codecs/active patterns preserve null versus undefined and nominal declaration distinctions.

These operations currently select bounded nongeneric single instance-method signatures. Generic,
recursive, indexed, callable and other unsupported payload/source forms reject. They do not
automatically replace loose raw bindings, preserve all generic relationships or make all SDK
results precise. That is the central outstanding work.

`recursiveGroups` is an opt-in placement setting using the existing recursive-namespace renderer
for cyclic shipped package graphs. Entry/ambient export blocks retain their entry owner, even
when a dependency type shares a public subpath's prefix.

CloudEdge commit `ae366ae` added an explicit `agents-pi` profile: Agents 0.27.0,
Pi AI/Pi Durable/Chord 1.1.0, ai 7.0.93, workers-types 5.20261009.1 and Node types 22.20.1.
It ships reached dependency types in a closed owner, a typed Chord.Context factory, and four
shared-input operations: PiHarness.submit/prompt and PiSession.submit/prompt. It does not deliver
complete standalone PiAI/PiDurable value-export libraries. Those four raw inputs were already U2;
the wrappers improve construction/constraints but are **not an input-obj reduction**.

The committed source adapter under CloudEdge `generators/xantham/` was initially Pi-only. It
requires the exact clean Xantham revision and associates source/build-input hashes with compiled
payload hashes; stale `--no-build` reuse rejects. Default SDKComposition excludes Pi unless
`CloudEdgeIncludeAgentsPi=true`. The uncommitted migration below changes adapter scope.

Accepted evidence at the committed heads:

| Gate | Result |
| --- | --- |
| Xantham full `dotnet fsi build.fsx -- test --run-gate` | 1,233 generator passed / 1 existing Linux SolidJS ignore; 99 Wire passed / 1 baseline ignore; no failures |
| Fable / Partas | 525 runtime checks; Partas JSX and runtime gates passed |
| Xantham mutation evidence | 5 core + 3 operation-guard + 4 runtime mutants compiled and were killed |
| Xantham gate binding | 15,673 inputs unchanged; 42 changed paths match committed content |
| CloudEdge tooling | 184 passed |
| Real Pi integration | 11 F# → Fable 5.17.2 → workerd assertions, including malformed-image compile rejection, presence/null handling, typed calls, transcript/idempotency persistence across restart |

The real Pi test used the actual SDK/harness/SQLite Durable Object with a deterministic test
provider, not a remote paid model. These results qualify the preceding increment only.

Evidence locations:

```text
Xantham/docs/.ai/handovers/myriad-early-projections.md
Xantham/tests/.scratch/strict-operation-audit/acceptance.log
Xantham/tests/.scratch/strict-operation-audit/acceptance.{before,after,binding}.json
Xantham/tests/.scratch/strict-operation-audit/{changed-paths,commit-binding}.json
Xantham/tests/.scratch/strict-operation-audit/core-mutants/
Xantham/tests/.scratch/strict-operation-audit/operation-guard-mutants/receipt.json
Xantham/tests/.scratch/strict-operation-audit/runtime-mutants/results.json
FSharp.CloudEdge/docs/sdk-build.md
FSharp.CloudEdge/tests/.scratch/strict-operation-audit/final-binding.json
FSharp.CloudEdge/tests/.scratch/agents-pi/run-4ybpS0/report.json
FSharp.CloudEdge/artifacts/agents-pi/generation.json
```

Scratch artifacts are local and gitignored. Some build outputs have since been rebuilt by the
unfinished work. Historical acceptance does not authenticate the current dirty tree or a newly
overwritten DLL. Trust the recorded hashes, and do not weaken receipt checks to reuse stale output.

## Unfinished Xantham edits — intentionally left in place

Current delta: **2 files, +25/−1**, no production-generator implementation changes.

- `tests/fixtures/shared-tag-lab/index.d.ts` adds `Scheduled<T>` tagged cases with payload T,
  optional previous T, and `Scheduler.schedule<T>` / `current<T>` methods.
- `tests/Xantham.Generator.Tests/Pipeline.test.fs` adds a regression requiring the generated
  tagged union and inline-return union to bind/apply their type parameters, preserve the
  input/result relationship, and avoid TR013 for those payloads.

The regression **currently fails**. The new lead identified a likely owning-pass defect:
tagged-union IR lacks type parameters, payload shaping sees T out of scope, and references emit
bare named types. Model, union shaping/reference construction, rendering, catalog API and arity
handling were the proposed next implementation points. None of that fix was applied before stop.
Assess catalog compatibility versions when changing emitted generic arity/API.

Evidence under Xantham `tests/.scratch/obj-elimination/`:

- `tagged-before-actual.log`: one actual regression test, one expected failure.
- `tagged-before.log`: **zero tests selected; invalid evidence**, not a passing baseline.
- `lan-tagged-*`, `core-design/`: reading aids/hypotheses, not accepted implementation proof.

The actual test name is
`generator e2e.generic tagged payloads preserve declaration and method type parameters`.
A filter containing only `shared-tag-lab` selected zero tests in the attempted invocation.
Goldens were not updated for the new source declarations. Do not label the dirty tree green.

## Unfinished CloudEdge edits — intentionally left in place

Current delta: **10 files, +179/−70**:

```text
config/sdk-delivery.json
config/targets.json
docs/sdk-build.md
generators/xantham/Program.fs
scripts/generate.mjs
scripts/projects.mjs
scripts/tool-packages.mjs
tests/generate.test.mjs
tests/projects.test.mjs
tests/tool-packages.test.mjs
```

These move the adapter project/revision pin from Pi's projection configuration to shared
`xantham.source` in `config/targets.json`. The runner accepts ordinary generation without a
projection; generation/project verification use the same authenticated source adapter for all
configured targets, build it once per selected closure, and record/check sourceTool provenance.
The shared pin remains `2ecbd73cff0ee95b46ebc68ade560a473307e974`.

Focused evidence exists: **113 tooling tests passed**, the adapter built, and an ordinary
no-projection scratch smoke generated output. See CloudEdge:

```text
tests/.scratch/obj-elimination/source-tool-tests.log
tests/.scratch/obj-elimination/source-adapter-build.log
tests/.scratch/obj-elimination/source-adapter-smoke.json
tests/.scratch/obj-elimination/source-smoke/
```

**No delivered all-target regeneration, composed full acceptance, final review, commit or push
has occurred for these edits.** The changed config/tool fingerprints can deliberately invalidate
old generation receipts. The documentation changes describe intended migration behavior and must
not be mistaken for evidence that all delivered bindings already use it. Preserve and review the
edits; complete or revise them under the resumed task rather than assuming they are finished.

## Inventory of remaining loss

CloudEdge inventory currently covers **35 generated targets**: 31 default delivered SDK roots,
3 default generated support targets, and the explicit AgentsPi target. Broader inventory also
looked at 13 Hawaii-owned F# files and one handwritten Workers support file. The full semantic
public-position denominator is not yet implemented.

Artifacts under CloudEdge `tests/.scratch/obj-elimination/`:

| File | Purpose / limitation |
| --- | --- |
| `baseline.json` | ~9 MB per-symbol/finding baseline at ae366ae; source receipt hashes were current at inventory time. Aggregate it; do not dump it. |
| `summary.json` | Per-target counts and aggregate finding codes. |
| `driver-paths.json`, `catalog-candidates.json` | Candidate declaration paths and dependency ownership investigation. |
| `lexical-public-surface.json`, `obj-lines.jsonl` | Lexical candidate inventory, **not FCS semantic proof**; may count duplicate constructors/fields and private implementation uses. |
| `inventory.mjs`, `public-surface.py` | Scratch inventory drivers. |

Leading counts are **finding occurrences, not distinct widened public positions**:

| Finding | Count |
| --- | ---: |
| TR023 NotAmongGeneratedDeclarations | 7,875 |
| TR008 AnyToObj | 4,366 |
| TP006 TypeParameterErased | 2,983 |
| TR009 UnknownToObj | 2,764 |
| TR035 UnionWithObjArm | 1,293 |
| TR002 TypeNotResolved / depth cutoff | 998 |
| TR045 ConditionalTypeDeferred | 284 |
| TR018 IntersectionOverNonObject | 207 |
| TR013 TypeParameterOutOfScope | 190 |

The inventory lane also reported 72 indexed-access findings. Missing-name leaders included
JSONObject (722), ZodType (616), $ZodType/LazySchema/Schema (591 each), and Response (331).
Dependency/catalog ownership is therefore a major candidate lane, but these counts alone do not
prove the solution or justify simply shipping every dependency.

The Hawaii lexical pass found 217 `obj` tokens, all `obj.ReferenceEquals` in JSON converter
implementations, and no obj-bearing public signatures in those 13 files. This is qualified lexical
evidence, not a blanket semantic audit. Handwritten
`src/Support/FSharp.CloudEdge.Support.Workers/DurableObjects.fs` has actual public obj exposure
in `NativeRequest` generic arguments and `requireFetchTransport`; it needs source/contract review.

## Completion controls and proposed continuation

The independent reviewer identified `scripts/integration/contracts.mjs` as an existing owner for
public inventories and authenticated source/catalog/API evidence. Extend existing owners rather
than create a competing authority. Its `catalogContract` handling was reported as schema-1-only;
verify compatibility with current catalogs before relying on it.

`docs/SDK-LIBRARY.md` around lines 117–121 explicitly permits unsupported constructs/recursion
cutoffs and prioritizes selected composition over aggregate loss. The user's new objective
supersedes that policy; update the existing policy owner when implementing the stricter contract.

The fresh lead's proposed sequence, **not yet completed**, was:

1. Freeze all delivered roots and reachable public type positions, including support/Hawaii.
2. Finish the shared source-tool migration so generator fixes actually reach every delivered SDK.
3. Repair dependency/catalog closure and existing Resolve/Shape owners; preserve relationships
   before conversion. The failing generic-tagged-payload lab is one concrete first reducer.
4. Extend authenticated contract evidence to source → generated public-position precision.
5. Regenerate the complete closure; run compile, positive/negative consumer and real runtime
   gates; apply discriminating mutations; obtain one independent review; commit/push checkpoints.

Completion must keep the delivered API denominator intact. Follow aliases, containers, catalogs,
generic applications, callbacks and results transitively. TR008 is Escape and TR009 is Widened,
so neither total `obj` tokens nor tier budgets distinguish genuine source dynamism from lost
contracts. Dropped members, overloads and generic relations can hide loss without printing `obj`.
Private codec obj/unbox is legitimate only where source-bound shape/encoding proves the boundary;
an unconstrained public cast or generic opaque substitute is not a strict binding.

Useful commands after the user authorizes the successor to resume:

```sh
# Inspect and preserve current unfinished edits first, in each primary checkout.
git status --short --branch
git diff --stat
git diff

# Xantham focused work: verify the filter actually selects tests.
dotnet fsi build.fsx -- test --quick --filter "generator e2e.generic tagged payloads preserve declaration and method type parameters"
# After implementation and reviewed golden updates:
dotnet fsi build.fsx -- test --run-gate

# CloudEdge existing gates; current dirty migration needs coherent receipt regeneration first.
npm run test:tooling
npm run build:sdk -- agents-pi
npm run test:agents-pi
# Full existing project acceptance is broader; inspect package.json's test/build scripts.
```

Do not rerun an accepted baseline just to establish it again. New changes, stale bindings and
current failing regressions do require focused checks and subsequent composed acceptance.
Do not use `--update` to accept an unexplained loss or a large unexplained golden diff.

## Original dossier and unresolved historical hypotheses

The user's original untracked file is
`Xantham/docs/.ai/handovers/catalog-literal-union-ownership.md`. It remains unchanged and was not
included in either published commit. Its hash is captured in takeover/state.json.
Its runnable kit remains at `Xantham/tests/.scratch/catalog-audit/`, including `run.sh` and the
vendor-case driver. The old structural literal-union identity is
`46b8804e0130b515576e65acfa3f427ae369e0bb30bc51effbe7997903045b1c`.

That dossier describes master-era observations; do not treat its suggested work or line numbers
as current branch truth. The user subsequently directed work from develop and accepted coordinated
producer regeneration under an explicit owner. Catalog fixes are in the branch ancestry, and
current footguns document ownership rules. The old TanStack double-claim and Pi source-set
explanations were explicitly unminimized/unproven. The claim that OpenCode requires owning Effect
was not established by measuring the scoped `./workerd` entry. Preserve those qualifications.

Local Myriad was previously inspected at revision `bf274d16bec0d88eadf98e212933f03b6a59297c`;
the implementation uses published Myriad.Core 1.1.0. Treat that inspection revision as historical,
not a statement about its current checkout. Generated bindings target Fable 5.x only.

## Growth and custody

Accepted predecessor increments:

- Xantham `0cab4e9..2ecbd73`: **42 files, +2,943/−77**.
- CloudEdge `3bf0861..ae366ae`: **29 files, +15,616/−30**, mostly generated bindings and lockfile.
- Their tracked handoffs already list new-file owners/reasons.

Unfinished deltas at stop: Xantham **2 files +25/−1**; CloudEdge **10 files +179/−70**.
No new production file was added by the unfinished increments. This new handoff file belongs to
the existing AI-handover owner: its sole purpose is the user's requested transfer to a separate
agent. The scratch patch/state capture belongs to that same handoff evidence, not a new generator
authority. Neither it nor this primer should be described as completed implementation.

**Successor instruction:** independently assess the preserved changes and evidence. Carry out the
user's full objective when they resume the task. Do not trust earlier “complete” statements as
evidence, and do not repeat them while known-source public contracts still widen to obj.
