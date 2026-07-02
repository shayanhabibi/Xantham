# Xantham Goals — Acceptance Criteria

*Single purpose: what "done" means, stated objectively so any claim can be checked against it. Context and analysis: [LANDSCAPE.md](LANDSCAPE.md). Execution steps and their closure: [PLAN.md](PLAN.md). Current state: [STATUS.md](STATUS.md).*

## The goal

Generate F# bindings for the Cloudflare SDK constellation that a consumer uses without reading Xantham internals: `dotnet add package`, write a Worker/Agent in F#, Fable-compile, run. Xantham itself is a general TypeScript→F#/Fable binding generator; no Cloudflare-specific logic in core — all target knowledge in the recipe (`cloudflare.pilot.toml`).

**Near-term milestone (1:1 import→export):** seven npm packages → seven F# libraries (`Fidelity.CloudEdge.{Workers, Zod, PartySocket, PartyServer, Mcp, CodeMode, Agents}`), sufficient to build a working Worker and a working Agent. Verification is by consumption, not inspection: the samples below compile and run.

**End state (steady-state operation):** an update run — pin bump → crawl → gates → drift report → regenerate → changed-only republish — processes each upstream Cloudflare release with human attention limited to the drift report and new advisories. Eventual shape: many SDK entries in, many namespaced libraries out, in one run.

## Per-layer definitions of done

Reported per area × per layer in a generated `STATUS.md` (`scripts/status-report.sh`); a completeness claim exists only per cell, never for the project.

- **Done(L0 — declared surface):** unassigned declarations = 0; every reached package has an explicit surface-or-dependency policy; the recipe is consumed by generation, not advisory.
- **Done(L1 — raw binding, per area):** Fable error count 0 for the area's own compilation unit; arity disagreements 0; identity conflations 0; every deliberate erasure counted, classed, and justified in the erasure ledger.
- **Done(L2 — conventions, per area):** semantic budgets met (callbacks-as-Invoke, synth-home penetration, smash names); zero unacknowledged advisories — each shape either has a declared convention or an explicit accept-passthrough mark.
- **Done(L3 — usable API, per area):** a sample consumer compiles (Fable exit 0), runs (miniflare/workerd), and the area's package packs. A running program, not a grep, proves usability.

## Phase exit criteria

- **Exit(P0):** unassigned declarations = 0; every reached package has a declared policy; error decomposition owned by tooling, not session analysis.
- **Exit(P1):** `Fidelity.CloudEdge.Workers` compiles at 0 errors as its own unit; every area has its own ratchet; the whole-artifact ratchet is retired.
- **Exit(P2):** an installable package exists; HelloWorker (fetch + KV/R2/D1 round-trip) runs under miniflare; the minimal Agent slice (subclass, `routeAgentRequest`, `getAgentByName`, state, `this.sql`) compiles and runs; a zod tool-authoring sample compiles (proves the opaque-handle policy) or the policy is revised.
- **Exit(P3):** shipping areas' semantic budgets green; zero unacknowledged advisories; MCP area at L1 = 0.
- **Exit(P4 — the goal):** every declared area at L1 = 0; one real sample per area compiling and running; ported upstream test slice green; packages published changelog-gated in dependency order; one real upstream version bump processed end-to-end as an update run.

## Standing rules

No whole-project completeness claims — cells only. Per-area ratchets never regress. Fix at the owning tier (identity/twins → encoder; resolution/keying → decoder; naming/paths → generator; missing or degraded surface → recipe gap, never a codegen patch). Every gate failure names `{area, owning layer, error class, fix location}`.
