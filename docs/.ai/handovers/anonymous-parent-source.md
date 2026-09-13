# Anonymous parent roles and source ownership

The smallest reproducer uses separate packages:

```ts
// optional-array-shared-lab/index.d.ts
export interface Shared { values?: string[]; }
```

```ts
// optional-array-consumer-lab/index.d.ts
import { Shared } from "optional-array-shared-lab";
export interface Container { values?: string[]; cache: Shared; }
```

Generate the shared package with a declaration catalog, then generate the consumer
with that reference. Both use `lib: ["esnext"]`, `types: []`, and ship the shared
package. Revision `774aa68` rejects `Shared` with a source hash mismatch. The
registered catalog regression fails for the same reason before the fix.

TypeScript shares the anonymous `string[] | undefined` type between the two
properties. Parent-role inference assigns it one output identity, selected among
both parents. Using that completed map during source traversal lets the consumer's
property contribute its source file to the shared interface. Source ownership now
uses the immutable canonical map from before parent-role inference. Anonymous
output identities still use the completed map and retain their parent sources.

The actual Workers/Containers diagnostic established the same path:
`CacheContext → purge → CachePurgeOptions → string[] | undefined`, whose inferred
owner was Containers' `deniedHosts`. The consumer added only
`@cloudflare/containers@0.3.7/dist/lib/container.d.ts` to the producer's eleven
sources. Every common source and hash matched.

With the fix, actual Workers-to-Containers generation passes source authentication
and reaches an F# API mismatch for `RequestInitCfPropertiesImageDraw`. This is
separate from source ownership, so it remains for composed generator validation.
The diagnostic output is under CloudEdge's
`artifacts/one-shot-20260913/core-catalogs-canonical-snapshot`; it records the copied
CLI payload, exact generator diff, support pins, and unchanged input/tool hashes.

The catalog regression compiles a typed consumer that returns `Shared` through the
adapter and reads an optional string value. Changed shared input remains rejected.
No catalog authentication guard changes or new finding codes are introduced.

Full validation (`dotnet fsi build.fsx -- test --update --run-gate`) passes:
875 generator and 90 Wire tests in both phases, the compile gate, and 461 Fable
checks. Existing ignored counts remain two generator and one Wire test. All 97
existing golden trees are byte-identical. The new lab has one exact and two
ergonomic symbols, with zero widened and escape symbols. Fresh FCS checking reports
zero errors and warnings. The existing site NuGet and Fable fixture warnings remain.
