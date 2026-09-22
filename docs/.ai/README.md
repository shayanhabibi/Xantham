# LLM documentation

No secret: xantham was built largely through guided LLMs. This directory is where agents leave
notes for the agents and people who come after them. It is not rendered to the site.

- `plans/` — one file per design, dated. A plan stays while its work is unlanded or its decisions
  are still cited; the record of what landed lives in `plans/generator-architecture.md` (phase
  and wave records) and `plans/generator-type-mapping.md` (per-construct mapping, decisions
  D1–D12). Those two are the living documents and are updated in the same commit as a behaviour
  change.
- `footguns.md` — facts that constrain future changes: compiler-API traps, declaration-catalog
  identity rules, pass invariants, and the threads left open when a lane closed.
- `fable5-workarounds.md`, `fable-binding-gaps.md`, `fable-utility-types.md` — what Fable 5 and
  `Fable.Core` cost a generated binding, measured against the repository's pins.
- `plans/tsgo-protocol.md`, `plans/wire-remaining-work.md` — the compiler protocol and the Wire's
  outstanding work.

Execution plans are deleted once their work ships; handovers are retired into the phase record
and `footguns.md` on merge (`.claude/rules/generator-fixtures.md`). A note kept past its use is a
note the next agent has to read before discovering it is spent.
