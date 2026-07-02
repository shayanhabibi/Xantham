# Xantham Execution Plan — Checklist

*The tracking document. [LANDSCAPE.md](LANDSCAPE.md) holds context/analysis, [GOALS.md](GOALS.md) the acceptance criteria, [STATUS.md](STATUS.md) the dated state; this file holds the steps and their closure. An item closes only with evidence (a gate passing, an artifact existing, a number reported) — the closure line records date + evidence. No item closes on "should work."*

Doer tags: **[owner]** = decision/markup only the owner can make. **[gen]** = generator-side work (this repo). **[both]** = gen produces, owner reviews/acks.

---

## Phase 0 — Declare the surface

- [x] **[gen]** Constellation survey: every installed package + npm family, entries, boundary leaks, publish order. — *Closed 2026-07-02: 6-agent workflow wf_83e8a5b6; distillate in memory + recipe; traps: root==oldest, Ai=37%, zod construction-position, chunk-shims, 5 tombstone entries.*
- [x] **[gen]** `cloudflare.pilot.toml` v0 draft (policy overlay: entries, dependency policies, ambient areas, packaging). — *Closed 2026-07-02: at repo root, DECISION lines marked for owner.*
- [~] **[owner]** Recipe markup: the 4 `DECISION:` lines. — *Deferred by owner 2026-07-02 ("I don't want to hand-hold the TOML right now"); generator defaults stand and are ratified by use; owner may revisit any line at any time.*
- [x] **[gen]** Multi-entry crawl seeding: `getAndPrepareExports` loops all `entryFiles`; per-entry `EntryExports` provenance in `EncodedResult` (additive, optional-decode on the .NET side); union-interning remap covers the new field. — *Closed 2026-07-02: suite 445→467 green; provenance suite pins one-key-across-entries dedup + no-duplication-beyond-single-entry invariant (fixtures `TypeFiles/multi-entry/`).*
- [x] **[gen]** Recipe loader + entry resolution in the CLI (`--recipe`): typed model (`Recipe.fs` — untyped access confined to the TOML boundary), module entries via `ts.resolveModuleName` (Bundler, exports maps honored), ambient roots joined + existence-checked, error accumulation; hardcoded dev entry block removed from `Program.fs`. — *Closed 2026-07-02: 31/31 entries resolve; 16 loader tests (9 unit decode + 7 integration against pinned node_modules, incl. the PINNED mcp-sdk lying-root fixture).*
- [x] **[gen]** First v0 crawl → scratch IR + per-entry attribution report. — *Closed 2026-07-02: 31 crawl roots, 30,640 types (vs 14,704 single-entry), 1,519 attributed top-level exports, workers-types `latest` first-class (948 exports via the module twin); committed IR/baselines untouched. Findings: mcp-sdk declares an unshipped root export (recipe uses subpaths, test-pinned); `@types/json-schema` reached without a declared policy (added to recipe).*
- [x] **[gen]** `scripts/status-report.sh` → generated `STATUS.md`: per-package L0 matrix (EntryExports-primary attribution, FQN fallback), per-entry provenance, coverage counts per module; exits non-zero on missing policy. — *Closed 2026-07-02: generated from the v0 IR; L0 gate PASS (0 missing policies).*
- [x] **[gen]** Re-baseline staged for owner commit: IR re-crawled to `src/Xantham.Fable/output.json` (ambient-root revision — see finding below), all four gate baselines updated with dated in-file justifications (sentinel 13/36; golden callbacks 75/proto 4/smash 55/synth 1759/lines 46,538; arity per-name +2 known-class; conformance 1101). — *Closed 2026-07-02: sentinel PASS, arity PASS, determinism ×2 byte-identical, decoder 400/400, generator 509+2 pins, encoder 470.*
- [x] **[both]** Phase 0 exit: unassigned = 0 (gated); every reached package has a declared policy (gated); IR attribution owned by tooling. — *Closed 2026-07-02 with one honest carve-out: per-ERROR→per-area attribution arrives with Phase 1's partitioned units, where errors become per-unit natively instead of log-parsed.*

**Phase-0 findings ledger (dated, for the record):**
- *Module-twin DECISION reversed by measurement (2026-07-02):* the workers-types module twin omits the `declare module "cloudflare:*"` islands; third-party `import ... from "cloudflare:workers"` (DurableObject/WorkerEntrypoint/RpcTarget — the base-class spine) resolved to nothing → 6 dangling builders → decoder crash on compress. Ambient root (`latest/index.d.ts`) declares islands + globals as ONE symbol set. MISSREF 7→2 after the flip; the residual 2 are one characterized class (`declare const Cloudflare: Cloudflare` merged-twin, island `export =`) — logged, not chased.
- *Pipeline robustness note (deferred, deliberate):* unresolvable imports currently produce dangling IR keys that crash the decoder downstream. With the recipe fixed this state is unreachable, but a future SDK bump could reintroduce it mid-update-run; the crawl should gate on MISSREF count (fail loudly at the boundary). Scoped to the update-run work (Phase 4), not built speculatively now.

## Phase 1 — Partitioned emission + per-area L1 gates

- [x] **[gen]** Recipe consumed by generation — foundation: shared typed model (`Xantham.Common/Recipe.Model.fs`: entry kinds, `DependencyPolicy` DU with total parse, lib/depends-on/policy fields), Fable loader refactored onto it, .NET loader (`RecipeLoad.fs`, Tomlyn 2.9 boundary) mirroring the same decode contract; the committed recipe is a live fixture on BOTH boundaries. — *Closed 2026-07-02: encoder suite 474, generator suite 517 (+8), IR byte-identical after refactor.*
- [ ] **[gen]** Area-assignment pass: every IR type → owning lib, from EntryExports provenance + source-path mapping + the recipe's workers-types area table; synthetics by dependency classification (single-owner → that lib; shared → dependency-order root). *Closure: assignment total (0 unassigned types) + isolation tests.*
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

**Standing rules:**
- Work lands against this checklist; status reports reference item numbers and closure evidence, not narrative. When reality diverges from an item, the item is edited (with a dated note), never silently skipped.
- **Test-coverage parity (owner directive 2026-07-02):** every new code structure lands with its parallel unit + integration coverage in the same change, and coverage remains in relative parity to new code as it is ratified within the design's scaffolding. Coverage-analysis reporting is part of the Phase-0 `status-report.sh` scope (per-module test counts alongside the signal matrix); a structure without its coverage plane is not closed.
