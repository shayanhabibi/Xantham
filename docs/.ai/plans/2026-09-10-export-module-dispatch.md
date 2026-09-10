# Export layout: dispatch brief

**Status:** finalized for dispatch, 2026-09-10. Implementation has not started.

**Authority:** [implementation specification](2026-09-10-export-module-layout.md). This brief routes workers to that specification; it does not replace its contracts.

## Coordinator procedure

1. Read the specification once. Establish a local feature branch/worktree from the intended base, preserving existing user changes. A local integration target is required before merging; it is not needed to prepare feature work. Never land on master or touch remotes.
2. Dispatch task 1, then task 2. Review the types, naming allocator, provenance fields, and mechanical traversal migration before freezing the shared interface.
3. Start task 3 and task 4 independently. After task 3 commits, start task 5 on that lane while task 4 can continue. Maximum two workers concurrently.
4. Review and integrate tasks 4 and 5, then dispatch task 6. Keep intentionally failing intermediate commits on the feature branch.
5. Review consumer compilation, Fable runtime evidence, manifest changes, and unchanged type identity. Merge only into the specified local integration branch with a substantive local commit message.

Use the configured lower-capacity execution model for bounded workers. Coordinator owns semantic policy and integration decisions. This document does not prescribe an unavailable model identifier or authorize starting execution in the planning turn.

## Shared worker prompt

> Implement your assigned task from `docs/.ai/plans/2026-09-10-export-module-layout.md`. Follow repository AGENTS.md. Load only the specification headings listed in your assignment and the named source files. Preserve the frozen interface. Run targeted tests and commit your changes locally. Report commit SHA, owned files, produced interfaces, exact test commands/results, and unresolved failures in under 400 words. If the contract is insufficient, report the exact failing invariant and proposed contract correction to the coordinator before expanding scope.

Each assignment message also supplies: task number, worktree absolute path, feature branch, prerequisite commit SHA(s), and any reviewed interface delta. These are execution-time values supplied by the coordinator, not worker guesses.

Budget targets: assignment text under 2,000 tokens; initial source context under 8,000 tokens; no full chat history, whole generated Node file, or generated Wire source. Load additional context only for a concrete symbol or failed test. Use semantic outlines/check/find with explicit project paths; use textual search for this Markdown specification. All shell commands use RTK.

## Assignment 1 — regression

Load spec headings: `Public ownership`, `F# layout`, `Task 1`.

Deliver: deterministic lab package, focused Pipeline regression, actual Expecto selectors, and Node input/version evidence or explicit absence. Follow task 1's listed file ownership. This worker does not design the collision algorithm.

Seed for the ambient fixture (no top-level import/export):

```typescript
interface LayoutPayload { value: string; }
declare var sharedFlag: boolean;
declare module "layout-lab" {
    export function check(value: string): string;
    export const mode: string;
    export function echo(value: LayoutPayload): LayoutPayload;
    export function convert(value: string): string;
    export function convert(value: number): number;
    export function pick(value: string): string;
    export function pick(value: string): number;
}
declare module "layout-lab/strict" {
    export function check(value: string): string;
    export const mode: string;
    export function echo(value: LayoutPayload): LayoutPayload;
}
declare module "layout-lab/aliases" {
    export { check as renamedCheck } from "layout-lab";
    export type { check as typeOnlyCheck } from "layout-lab";
}
```

Use package runtime `layout-lab`, generated module `LayoutLab`. Required access paths: `LayoutLab.Exports.check`, `LayoutLab.Strict.Exports.check`, `LayoutLab.Aliases.Exports.renamedCheck`, and `LayoutLab.Globals.Exports.sharedFlag`. `convert` keeps two overloads; `pick` keeps `pick` and gains `pick_Overload2`; `typeOnlyCheck` produces no runtime member.

Review gate: failure comes from old layout/drop behavior, not skipped compiler or invalid fixture configuration.

## Assignment 2 — frozen model and layout

Load spec headings: `Public ownership`, `F# layout`, `Naming collisions`, `Type identity, qualification, and reporting`, `Task 2`.

Deliver: provenance-carrying model types, pure allocator, migration of existing constructors/traversals, append-only layout finding. Commit builds on task 1. Read the final-collision contract's provenance requirements only when updating model fields; the collision algorithm belongs to task 5.

Review gate: provenance survives until repaired declarations; no session IDs enter persistent output; path/name collision allocation is deterministic; no type-owner relocation. Coordinator publishes the frozen signature summary to subsequent workers.

## Assignment 3 — owner grouping and candidate preservation

Load spec headings: `Public ownership`, `F# layout`, `Task 3`; read the `Pass order` paragraph under `Final F# export collision contract`.

Deliver: independent containers with all candidates preserved through arity repair. Remove the existing owner-blind export filter while retaining non-export overload handling and literal synthesis. Own Shape files listed in task 3; renderer and final reconciliation belong to other workers.

Review gate: a shared origin exported from two public modules appears in both; a same-owner collision still reaches task 5. Showing only the first candidate is a failure even if compilation succeeds.

## Assignment 4 — rendering

Load spec headings: `F# layout`, `Naming collisions`, `Type identity, qualification, and reporting`, `Task 4`; read the rename/binding paragraph under `Final F# export collision contract`.

Deliver: nested rendering from synthetic containers, scoped type qualification, and runtime selector independence from F# member names. This task consumes task 2's frozen types and can proceed before task 3 finishes.

Review gate: constructor imports and mutable globals retain their JavaScript targets when F# names change; foreign-type references and helper declarations still compile. Shared type identity remains unchanged.

## Assignment 5 — final collision resolution

Load spec headings: `Final F# export collision contract`, `Task 5`, plus the frozen model summary. This is the largest task; execute it as two sequential checkpoints rather than a single exploratory session:

1. **Compiler semantics checkpoint:** complete task 5's compiler probes and structural normalization tests. Return the observed distinctions and canonicalization interface to the coordinator. Commit the tested helper/probe work. Coordinator reviews generic alias substitution, optional/rest behavior, and cycle termination before reconciliation proceeds.
2. **Reconciliation checkpoint:** complete consolidation, renaming, findings, late pass registration, and idempotence. A fresh worker may continue from the checkpoint commit and a short interface summary. It should not reread the compiler exploration transcript.

Keep both checkpoints on the same lane and avoid concurrent edits to the new collision module. This task starts after task 3 and may run alongside task 4. It does not edit rendering files.

Review gate: full provenance is required for duplicate proof; parameter-key equality alone never authorizes a drop; legal overloads survive; return-only/constraint/alias collapse keeps both names; suffixes avoid original members/accessors; late arity collisions are repaired; no new widening is introduced.

## Assignment 6 — acceptance and documentation

Load spec headings: `Task 6`, `Acceptance and distribution`, `Scope boundaries`. Consult naming/collision contracts only to interpret an unexpected golden difference.

Deliver: regenerated reviewed goldens, passing consumer compile/runtime gates, migration notes, architecture phase record, Node validation results bounded to the captured input. Run the full pipeline once after targeted checks, repeating only when a change/failure warrants it.

Runtime assertions for the seed fixture:

```text
LayoutLab.Exports.check("x")          => "root:x"
LayoutLab.Strict.Exports.check("x")   => "strict:x"
LayoutLab.Aliases.Exports.renamedCheck("x") => "root:x"
LayoutLab.Exports.mode                => "root"
LayoutLab.Strict.Exports.mode         => "strict"
LayoutLab.Globals.Exports.sharedFlag <- true
original globalThis.sharedFlag       => true
```

Add the task 6 renamed-overload/runtime cases separately. A return-only pair proves two typed F# entrypoints and unchanged target selection; it cannot prove contradictory TypeScript return promises are simultaneously true for one argument.

Review gate: full compiler/live/runtime evidence recorded, no unexplained declaration-catalog changes, no old export-drop findings for candidates now retained, existing losses remain visible. If Node input is absent or unrelated Node failures remain, report that limitation without relabeling the lab acceptance as a full Node success.

## Completion record

Maintain a concise task status beside each checkpoint in the coordinator's working notes: not started, active, committed, reviewed, integrated. The final report names the local integration commit, tests, remaining limitations, and migration impact. The specification is the single authority for behavioral decisions; update it once if review discovers a necessary contract correction.

## Local execution candidates — inspected 2026-09-10

OpenCode 1.14.19 is installed and configured for `ollama/qwen3-coder:30b` through `http://127.0.0.1:11434/v1`. Its `run` command supports explicit model, directory, attached files, JSON events, and pure mode. The inspected user configuration has no explicit model context limit or MCP configuration. Establish a worker-specific configuration and verify F# semantic-tool access before assigning repository implementation. Preserve the user's default configuration.

Hardware: RTX 5080, 16,303 MiB VRAM; approximately 62 GiB system RAM. Use one local inference worker at a time; the specification's second lane may use a hosted worker or wait.

Installed Ollama candidates: `qwen3-coder:30b` (30.5B Q4_K_M), `qwen3.5:latest` (9.7B Q4_K_M), `qwen3.8:latest` (27.3B Q4_K_M), `glm-4.7-flash:latest` (29.9B Q4_K_M), and `hf.co/deepreinforce-ai/Ornith-1.0-9B-GGUF:Q4_K_M`. Locally Uncensored also has local GGUF files including Granite 4.1 8B; its model directory overlaps much of this inventory. LM Studio currently lists an embedding model only.

Two short direct Ollama inference probes used 16,384-token context settings, temperature zero, and fewer than 100 output tokens. These are speed/response checks, not implementation benchmarks:

- `qwen3.5:latest`: 100% GPU placement, approximately 126 output tokens/second, 33.3 seconds total including 14.7 seconds reported model loading. Thinking was disabled.
- `qwen3-coder:30b`: 29% CPU / 71% GPU placement, approximately 84 output tokens/second, 27.2 seconds total including 21.3 seconds reported model loading. Its response identified return-only overload conflicts, separate module scopes, and renaming as remedies.

Ollama reports tool capability for both models. A real OpenCode editing/test loop and F# semantic-tool integration have not been validated. Short decode speed is not whole-task throughput; longer contexts may change memory placement and latency.

Trial routing: use the existing coder model for task 1's bounded fixture/test slice, subject to runner verification; compare the smaller Qwen if latency or memory pressure becomes material. Keep task 5 compiler-semantics decisions and final integration with the strongest proven coordinator. Allow one correction attempt, then escalate. Record accepted changes, correction effort, elapsed time and hosted review usage before claiming cost savings. No additional model download or persistent runtime reconfiguration is required for the initial comparison.
