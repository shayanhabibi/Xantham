# Xantham Execution Plan — Checklist

*The tracking document. [LANDSCAPE.md](LANDSCAPE.md) holds context/analysis, [GOALS.md](GOALS.md) the acceptance criteria, [STATUS.md](STATUS.md) the dated state; this file holds the steps and their closure. An item closes only with evidence (a gate passing, an artifact existing, a number reported) — the closure line records date + evidence. No item closes on "should work."*

Doer tags: **[owner]** = decision/markup only the owner can make. **[gen]** = generator-side work (this repo). **[both]** = gen produces, owner reviews/acks.

---

## Phase 0 — Declare the surface

- [x] **[gen]** Constellation survey: every installed package + npm family, entries, boundary leaks, publish order. — *Closed 2026-07-02: 6-agent workflow wf_83e8a5b6; distillate in memory + recipe; traps: root==oldest, Ai=37%, zod construction-position, chunk-shims, 5 tombstone entries.*
- [x] **[gen]** `cloudflare.pilot.toml` v0 draft (policy overlay: entries, dependency policies, ambient areas, packaging). — *Closed 2026-07-02: at repo root, DECISION lines marked for owner.*
- [ ] **[owner]** Recipe markup: the 4 `DECISION:` lines (module-twin crawl, Workers.Ai split, web-standard twins, agents entry list) + any entry-set edits. *Closure: owner says so / commits the recipe.*
- [ ] **[gen]** Multi-entry crawl seeding: `getAndPrepareExports` loops all `entryFiles`; per-entry `EntryExports` provenance in `EncodedResult` (additive, optional-decode); isolation tests (two entries sharing a chunk → one decl set, both attributions). *Closure: encoder test suite green incl. new tests.*
- [ ] **[gen]** Recipe loader + entry resolution in the CLI (`--recipe`): TOML parse, module entries resolved via the TS resolver (exports maps honored), ambient/module-twin roots joined; hardcoded dev entry block removed. *Closure: `xantham --recipe cloudflare.pilot.toml -o <path>` produces IR from the declared set.*
- [ ] **[gen]** First v0 crawl (defaults standing in for unmarked DECISIONs) → scratch IR + per-entry attribution report from `discover.sh`. *Closure: report posted with real numbers; committed IR/baselines untouched until re-baseline decision.*
- [ ] **[gen]** `scripts/status-report.sh` → `STATUS.md`: per-package × per-layer matrix; unassigned-decl count; per-package error attribution. *Closure: STATUS.md generated from the v0 IR.*
- [ ] **[both]** Phase 0 exit: every IR declaration attributable to a recipe entry + policy; unassigned = 0; owner acks the matrix as the standing signal. *Closure: gate script enforces unassigned=0.*

## Phase 1 — Partitioned emission + per-area L1 gates

- [ ] **[gen]** Emit one `.fs` unit per recipe lib (publish order: Workers, Zod, PartySocket, PartyServer, Mcp, CodeMode, Agents) + shared-root placement by type-dependency classification. *Closure: 7 units emitted; no `module rec` monolith.*
- [ ] **[gen]** SharedLiterals homes placed by manifest classification (the designed fix for the 227 literal-home dangles). *Closure: dangle class = 0 in per-area gates.*
- [ ] **[gen]** Per-area fsproj generation + all four gates re-scoped per area; whole-artifact 644 ratchet retired. *Closure: per-area baselines committed; old baseline deleted.*
- [ ] **[gen]** `Fidelity.CloudEdge.Workers` compiles at 0 errors as its own unit. *Closure: `dotnet build` exit 0 on the Workers unit.*
- [ ] **[gen]** Remaining v0 units: error counts per unit reported in STATUS.md, each with its own ratchet. *Closure: no unit regresses; Agents unit ≤ its ratchet.*

## Phase 2 — Ship the first slice (the near-term goal)

- [ ] **[gen]** Pack the v0 libs as nupkgs (upstream-tracking versions, publish order). *Closure: `dotnet add package` works from a local feed.*
- [ ] **[gen]** HelloWorker: fetch handler + KV/R2/D1 round-trip, Fable-compiled, runs under miniflare. *Closure: miniflare exit 0 + correct response bytes.*
- [ ] **[gen]** Minimal Agent slice consumer: `Agent` subclass + `routeAgentRequest` + `getAgentByName` + state + `this.sql`, compiled through the bindings. *Closure: Fable compile exit 0; runs under miniflare.*
- [ ] **[owner]** zod/MCP policy proof-or-adjust: the opaque-handle builders suffice for `registerTool` authoring, or the policy is revised. *Closure: a tool-authoring sample compiles.*
- [ ] **[both]** Phase 2 exit = the "ecstatic" milestone: 1:1 import→export, a consumer builds something real without reading Xantham internals.

## Phase 3 — L2 conventions per area (after the milestone)

- [ ] **[gen]** 7-stage hook model implemented; inline policies migrated behind it.
- [ ] **[both]** Per-area conventions declared in the recipe; advisory ledger live; zero unacknowledged advisories on shipping areas.

## Phase 4 — L3 + steady-state operation

- [ ] **[gen]** Update run end-to-end: bump pin → crawl → gates → drift report → regenerate → changed-only republish. *Closure: one real upstream bump processed (agents 0.17.1→0.17.3 is queued as the first).*
- [ ] **[gen]** Extensions packages scaffolded; ported upstream test slice green.
- [ ] **[both]** Next-wave entries promoted by owner decision (oauth-provider, containers, sandbox).

---

**Standing rule:** work lands against this checklist; status reports reference item numbers and closure evidence, not narrative. When reality diverges from an item, the item is edited (with a dated note), never silently skipped.
