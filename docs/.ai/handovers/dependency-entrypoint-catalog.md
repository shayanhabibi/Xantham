# Dependency entrypoint catalog representation

Branch: `fix/dependency-entrypoint-catalog`; base `66da5c0`, with the already gated empty-union
fix `5fcbc94` composed locally as `bbbfd99` before this change.

Workers' `WorkerEntrypoint` is declared in a namespace re-exported by `cloudflare:workers`.
The producer harvested its constructor and emitted an abstract class with optional hook
interfaces. A dependency-only consumer reached its instance members, emitted an ordinary
interface, and failed strict catalog API authentication. The smallest reproducer uses:

```typescript
declare namespace EntrypointRuntime {
    export interface Branded { readonly brand: void; }
    export abstract class Actor<T = unknown> implements Branded {
        readonly brand: void;
        constructor(options: { seed: T });
        readonly seed: T;
        fetch?(value: T): T;
    }
}
declare module "entrypoint-lab:runtime" {
    export = EntrypointRuntime;
}
```

A second input references that declaration and exports `accept(actor: Actor<string>): Actor<string>`.
Before the fix its catalog comparison fails for `Actor`.

Harvest now retains ambient runtime class metadata separately from `Exports`. It reuses the
existing module export/alias/provenance resolver, including namespace `export =`, and admits
only runtime class exports in shipped groups. Resolve reads constructor signatures only when
the class itself is reached. Their parameter, generic and return dependencies join the normal
closure, with no synthetic constructor object or public export occurrence.

The common class-side view supplies existing hook naming, explicit-implements admission and
class shaping. Recovered dependency entrypoints retain the same import and constructor shape.
The existing declared/public class view takes precedence. Package and package-subpath imports
remain ordinary, in addition to the existing configured runtime/public-input check. This
additional package check is not the sole runtime classifier; declaration packages and runtime
aliases can differ.

The tracked lab tests a generic anonymous constructor argument, a shared generic hook interface,
and rejection of mismatched constructor arguments. It also verifies ordinary module and
ambient type-only classes retain optional-function properties and reject subclass constructors,
and that a package's own quoted public module retains the same ordinary form in both programs.
Four Fable checks cover the imported constructor, generic hook invocation, hook presence and
hook absence. FreeTypeParams needs no extra constructor traversal: the constructor's own
signature binds its parameters and the anonymous option shape retains its own referenced T.

Validation completed on 2026-09-13:

- Five focused tests passed in both write/check phases.
- Full `dotnet fsi build.fsx -- test --update --run-gate`, with the cached pinned native
  compiler and `XANTHAM_REQUIRE_TSC=1`, passed 901 Generator and 90 Wire tests in each phase,
  followed by 465 Fable runtime checks. Fixtures were installed fresh in this worktree.
- Fresh generator FCS check: zero errors and zero warnings. The full build retains the existing
  site NU1608 package warning, two intentional Fable type-test warnings and a fixture module warning.
- Existing golden files and aggregate finding counts are unchanged. The new lab contributes
  exact 3 / ergonomic 4 / widened 0 / escape 0, including the existing SI002 conformance limit.
- An earlier full run exposed a missing Unclassified case in the additional package boundary;
  it was corrected before the complete successful gate. No generator edits followed that gate.
- The frozen actual SDK probe now authenticates WorkerEntrypoint. Other reported native
  mismatches (generic marker and constraint metadata) are separate lanes; full native consumer
  compilation has not passed yet.
- Source/API catalog guards and entrypoint explicit-implements SI002 remain unchanged.

Logs: `/tmp/xantham-dependency-entrypoint-before.log`,
`/tmp/xantham-dependency-entrypoint-final-lab.log`; actual native diagnostics under CloudEdge
`artifacts/one-shot-20260913/core-catalogs-native-entrypoint-composed/`.

The runtime checks use the tracked local JavaScript base class. They establish imported base
construction and optional hook calling conventions, including absence of an unimplemented hook.
They do not establish Cloudflare Durable Object lifecycle, alarm delivery, actor ordering,
Facet supervision or persisted recovery. Those require the separate actual SDK consumer and
workerd acceptance runs; Conclave's application overlay remains outside this generator change.

Full gate: `/tmp/xantham-dependency-entrypoint-full-gate-final.log`.
Findings: `/tmp/xantham-dependency-entrypoint-findings-{before,after}.txt`.
FCS: `/tmp/xantham-dependency-entrypoint-fcs-final.jsonl`.
