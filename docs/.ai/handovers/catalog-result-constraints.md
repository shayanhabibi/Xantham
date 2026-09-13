# Generic result declaration constraints

Branch: `fix/catalog-result-constraints`, based on `b2bf683`.

## Reproducer

The complete native comparison found two constraint mismatches with equal API hashes and arity:
`ReadableStreamBYOBReader.Read.Result.Item` and `ReadAtLeast.Result.Item`, both owned by
`Runtime.Workers.ReadableStreamReadResult2`. The owner has no generic bound; the consumer had
incorrectly attached `JS.ArrayBufferView` from the calling method.

```ts
export type ReadResult<R = any> =
    { done: false; value: R } | { done: true; value?: undefined };
export declare class Reader {
    read<T extends ArrayBufferView>(view: T): Promise<ReadResult<T>>;
    readAtLeast<T extends ArrayBufferView>(minimum: number, view: T): Promise<ReadResult<T>>;
}
```

A second package imports only `Reader` and exports `accept(reader: Reader): Reader`. The tracked
`catalog-result-constraints-lab` adds an explicitly constrained `BoundResult<R extends ArrayBufferView>`
control. Before the fix, its producer succeeds and the adapter fails at the existing constraint
comparison for `Reader.Read.Result.Item (Identity.Root.ReadResult2)`.

## Change

Resolve already retains canonical generic alias declarations and their original anonymous arms
in catalog runs. `bind-free-type-params` now considers those original anonymous declarations even
when only an application has a reachable generated name. Complete structural substitution still
requires a unique source declaration and consistent type-parameter arguments. The original arm
receives the first application's name and order, binds its own free parameters, and supplies the
canonical declaration for subsequent applications. Method bounds remain on method parameters;
actual declared bounds remain on result declarations. Ordinary generation retains its existing
selection of named canonical declarations.

The catalog source, API, arity, and constraint guards are unchanged. No new finding codes or Fable
runtime behavior are introduced.

## Validation

The focused catalog suite passes all 55 tests twice. Fresh generator FCS reports 0 errors and
0 warnings. `dotnet fsi build.fsx -- test --update --run-gate` passes both regeneration and
assertion phases: 899 Generator tests and 90 Wire tests each, plus the consumer compile gate and
all 461 Fable runtime checks. The existing two Generator and one Wire intentional ignores remain;
the solution reports its existing two site NU1608 warnings. The new test compiles typed producer/consumer use of both
read methods and permits `ReadResult2<string>`. It separately rejects `BoundResult2<string>` and
passing a string to `Reader.read`, checking the two independent constraint owners.

`dotnet fsi build.fsx -- findings` before/after shows no changes to existing fixture counts.
All 104 prior golden trees remain byte-identical. The new lab has 2 exact, 6 ergonomic, 0 widened,
and 0 escape symbols; its emitted F# bindings total 78 lines. The new lab adds these existing
finding keys only: TR024 (6), SP001 (6), SY004 (5), SP002 (3), MB003 (2), TR055 (1), and GE001 (1).

Evidence is retained in `/tmp/catalog-result-constraints-{before-focused,focused,full-gate}.log`,
`/tmp/catalog-result-constraints-findings-{before,after}.log`, and fresh FCS JSONL alongside them.
The frozen candidate and exact production patch used by the separate native composition probe
live under CloudEdge `artifacts/one-shot-20260913/catalog-read-result-constraints/candidate/`.

The composed actual SDK probe accepted both Workers and Containers generation with every normal
catalog guard enabled; source and tool hashes stayed unchanged. That closes both reported
`ReadableStreamBYOBReader` constraint mismatches. Its subsequent build has a separate, still
pending pair of FS0887 errors from consumer interfaces inheriting catalog-owned abstract classes;
this lane does not claim the complete native consumer builds. Evidence:
CloudEdge `artifacts/one-shot-20260913/core-catalogs-native-final-composition/results.json`.
