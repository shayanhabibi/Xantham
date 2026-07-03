# Xantham Execution Plan — Steps

*The steps document. [LANDSCAPE.md](LANDSCAPE.md) holds context/analysis, [GOALS.md](GOALS.md) the acceptance criteria (layer definitions L0–L3, phase exits), **[STATUS.md](STATUS.md) the dated state — generated, machine-sourced from the gate baselines; read progress THERE, not here.** This file holds the remaining steps and their closure criteria, plus a compressed dated record of closed steps. A step closes only when its criterion is met with evidence (a gate passing, an artifact existing, a number reported); the full engineering narratives live where the evidence lives — the dated comment blocks in `scripts/partition-gate.baseline` / `golden-gate.baseline` and git history — not here.*

Doer tags: **[owner]** = decision/markup only the owner can make. **[gen]** = generator-side work (this repo). **[both]** = gen produces, owner reviews/acks.

---

## Phase 0 — Declare the surface — CLOSED 2026-07-02

All nine steps closed 2026-07-02 (constellation survey; `cloudflare.pilot.toml` v0; multi-entry crawl seeding + per-entry provenance; recipe loader/entry resolution; first v0 crawl, 31 roots / 30,719 types; `status-report.sh` → generated STATUS.md with the L0 gate; re-baseline of all four gates; exit: unassigned = 0, every reached package policied). Owner recipe markup deferred by owner ("defaults stand, ratified by use"). Findings ledger (module-twin DECISION reversed by measurement; MISSREF crawl-gating deferred to Phase 4) — details in git history of this file and the baselines.

## Phase 1 — Partitioned emission + per-area L1 gates

**Exit criterion: every unit's own-file error count = 0 (compiles + produces its assembly). Progress: the L1 matrix in [STATUS.md](STATUS.md).**

Closed steps (dated; narratives in `scripts/partition-gate.baseline` unless noted):

- *2026-07-02:* recipe consumed by generation; area-assignment pass (monotone-lattice fixpoint; shared synthetics 40%); first partitioned emission (7 units, publish order, per-unit fsprojs); per-area L1 gate (`partition-gate.sh` + baseline).
- *2026-07-03:* zod opaque-handle policy end-to-end (332 machinery nodes unemitted, 564 refs collapsed onto the handle); the "Fantomas floor" resolved (doc-cascade was ours; wide-format emission) — Zod unit typechecks; monolith retired (units are the ONLY artifact; golden/arity gates re-aimed); type-granular shared placement (`SyntheticPlacementOrder`); chunk-provenance root fix (encoder FQNs); erased-dep enforcement machinery + enablement (advisory ledger, `Erased.*` aliases, forward-ref scrub — the DAG holds by construction); second-order burn 63→42.
- *2026-07-04:* coordinated phantom-typar arc 42→10 (case-twin dedup, opaque/erased prefix collapse, nested-child arm fix, def/ref-symmetry walk; three ref-side variants measured and REVERTED — pinned negative results); **ZOD AT 0** (home-child scrub + support library as project reference + function-param parens + duplicate-property dedup); Workers burn-down 86→8 (heritage scrubs, def/ref-closure scrub, abbreviation-legality verdict, Fable arity table, memoized per-export stores) → final-8→8-neutral composition fixes (five singletons killed).
- *2026-07-05:* **WORKERS AT 0** (rpcStub anchor/localise split → path-occupancy Case{n} re-homing, encoder verified clean → return-only overload unification + same-name method grouping + unit-setter drop); **PARTYSOCKET AT 0** (real, unblocked); **PARTYSERVER AT 0** (Function-arm anchor/localise split — the flagged latent twin; scrub-host correction catalogued to land WITH the cross-owner def/ref closure); Mcp first real count 26 → burn-down (holder-channel scrubs, Generator/AsyncGenerator arity, alias-arity recording gate removal).

Closed 2026-07-05 (continued): **MCP AT 0** (obj-alias→empty-interface verdict shape ×215 ledgered + 0-typar export-key arity recording + function-return parenthesization; fifth cell); **CODEMODE AT 0** (stdlib-ALIAS substitution at the remap mint — PropertyKey class; sixth cell); Agents first real count 4 → 2 (decorator-context types joined the obj map).

Open steps:

- **[gen]** Agents at 0 — THE LAST P1 CELL. The remaining 2 (`CreateMcpHandler`/`ExperimentalCreateMcpHandler`) are the CROSS-OWNER CACHE-HIT class: it closes via the DEFERRED-TOGETHER pair below, not another arm-local patch (a reRegisterStructuralLiterals wiring was measured 2026-07-05 — zero movement — and reverted; ref-side attempts are 4-for-4 negative on this family). *Closure: per-unit build exit 0; Phase-1 exit = the STATUS matrix all-zero.*
- **[gen]** DEFERRED-TOGETHER pair (measured 2026-07-05, evidence in the baseline; now the Agents blocker): scrub-host correction for holder params (surfaces real refs — fetch renders true `Request`/`URL` types instead of `obj`) + the cross-owner def/ref closure (cache-hit literals' defs materialize only at their first owner — the `ExportedHandler.Fetch` / 120B-`U2<obj,obj>` / Agents-`CreateMcpHandler` family). *Closure: Agents at 0; params scrub at the export-unit host with zero new dangles; ledgered obj-degradations on holder params burn down.*
- **[gen]** SharedLiterals homes placed by manifest classification (the designed fix for the literal-home dangle class). *Closure: dangle class = 0 in per-area gates.*
- **[gen]** Fidelity refinements catalogued during the burn-down, per-area after Phase 2: literal-discriminant widening (KV overloads become real again), never-arm strip for XOR expansions, `TypeReference` intermediate-alias arg preservation (the BaseToolCallback middle-arg drop). *Closure: each retires its advisory-ledger class.*

## Phase 2 — Ship the first slice (the near-term goal)

**Deployment model (owner directive 2026-07-05): NO WRANGLER/MINIFLARE.** Fidelity.CloudEdge's deployment side already exists and is largely solved: the SpeakEZ `clef-lang-site` repo's `cli` (Hawaii-generated Cloudflare management-API bindings, consumed as the published `Fidelity.CloudEdge.Management` nupkg). Its reusable primitives — generic `buildWorker` (dotnet fable → esbuild ESM bundle) + `deployWorker`/`UploadWorkerWithBindings` (multipart script upload with metadata/bindings, workers.dev subdomain enable), env-var auth (`CLOUDFLARE_API_TOKEN`/`CLOUDFLARE_ACCOUNT_ID`) — are the proof-run harness. The worker-side bindings that CLI's workers consume today are Glutinum-built (`Fidelity.CloudEdge/src/Runtime/*`); Xantham's units are their replacement. Worker entry shape in production there: `[<ExportDefault>] createObj [ "fetch" ==> fun req env ctx -> promise { ... } ]`.

- **[gen]** Pack the v0 libs as nupkgs (upstream-tracking versions, publish order). *Closure: `dotnet add package` works from a local feed.*
- **[gen]** HelloWorker: fetch handler + KV/R2/D1 round-trip, Fable-compiled through the Xantham units, deployed via the clef-lang-site CLI model (management-API upload), verified against the live workers.dev endpoint. *Closure: deploy succeeds + correct response bytes from the deployed worker.* **Unblocked NOW — needs only the Workers unit; the earliest signal on the runtime `Emit`/`Import` fidelity unknown (fable 5.0.0-rc.3 × Fable.Core 5.0.0-beta.4 never exercised on a generated unit). Do not gate on Agents.**
- **[gen]** Minimal Agent slice consumer: `Agent` subclass + `routeAgentRequest` + `getAgentByName` + state + `this.sql`, compiled through the bindings, deployed the same way. *Closure: Fable compile exit 0; deployed slice responds.* (Gated on Agents at 0.)
- **[owner]** zod/MCP policy proof-or-adjust: the opaque-handle builders suffice for `registerTool` authoring, or the policy is revised. *Closure: a tool-authoring sample compiles.*
- **[both]** Phase 2 exit = the "ecstatic" milestone: 1:1 import→export, a consumer builds something real without reading Xantham internals.

## Phase 3 — L2 conventions per area (after the milestone)

- **[gen]** 7-stage hook model implemented; inline policies migrated behind it.
- **[both]** Per-area conventions declared in the recipe; advisory ledger live; zero unacknowledged advisories on shipping areas.

## Phase 4 — L3 + steady-state operation

- **[gen]** Update run end-to-end: bump pin → crawl → gates → drift report → regenerate → changed-only republish. *Closure: one real upstream bump processed (agents 0.17.1→0.17.3 is queued as the first).*
- **[gen]** Extensions packages scaffolded; ported upstream test slice green.
- **[both]** Next-wave entries promoted by owner decision (oauth-provider, containers, sandbox).

---

**Standing rules:**
- Work lands against this plan; **progress is read in [STATUS.md](STATUS.md)** (generated from the gate baselines — regenerate with `scripts/status-report.sh` whenever a baseline moves). When a step closes, it moves to its phase's dated Closed list as ONE line; the narrative stays with the evidence (baseline comments, git history). When reality diverges from a step, the step is edited (with a dated note), never silently skipped.
- **Test-coverage parity (owner directive 2026-07-02):** every new code structure lands with its parallel unit + integration coverage in the same change, and coverage remains in relative parity to new code as it is ratified within the design's scaffolding. Coverage reporting is part of `status-report.sh` (per-module test counts). A structure without its coverage plane is not closed.
