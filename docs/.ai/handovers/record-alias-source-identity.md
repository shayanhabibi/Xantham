# Canonical sources for independent Record aliases

Two independent packages reproduce the Workers-to-Containers catalog rejection:

```ts
// producer/index.d.ts
export type PipelineRecord = Record<string, unknown>;
```

```ts
// consumer/index.d.ts
export type OutboundHandlerParams = Record<string, unknown>;
```

Generate each package with `lib: ["esnext"]`, `types: []`, and
`declarationCatalog: true`; give the consumer the producer's `declarations.json`
through `declarationReferences`. Tool revision `35feabd` rejects the consumer with
`source hash mismatch for OutboundHandlerParams (...PipelineRecord)`.

Both types have the canonical declaration identity of the same applied `Record`.
The old closure also added each occurrence's alias declarations, authenticating
different alias files as if they defined that shared identity. The closure now
follows the sources selected for canonical identities. Intrinsic argument aliases
have no independent declaration provenance; an alias selected as the identity's
owner still contributes its source. Catalog input hashes and rejection guards
are unchanged.

The tracked `record-alias-source-identity-lab` adds a model argument and an echo
function. Its catalog test compiles producer and consumer together, checks public
alias assignments and string-valued record indexing, and rejects changed reachable
alias inputs and changed model declarations. The standalone golden deliberately
widens unknown values and the separately owned model; it does not claim a typed
model-property surface.

`dotnet fsi build.fsx -- test --update --run-gate` passes with the pinned compiler:
872 generator and 90 Wire tests in each phase, the compile gate, and 461 runtime
checks. Existing ignored counts remain two generator and one Wire test. All 95
existing golden directories are byte-identical. The new lab has zero exact, zero
ergonomic, two widened, and zero escape findings by declaration. Fresh generator
FCS checking has zero errors and warnings. Existing site NuGet and Fable fixture
warnings remain in the full gate.

The actual SDK composition must be regenerated with a freshly packed tool after
integration. This source fix does not change F# API hashing and does not claim to
resolve the separately reported AI provider alias API mismatches.
