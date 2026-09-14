# Dependency generic marker ownership

The native compatibility diagnostic found exactly one remaining API mismatch after the
entrypoint fix: FacetStartupOptions.class is DurableObjectClass<T> in Workers and obj in
Containers. Its optional id property, arity, and constraints agree. DurableObjectClass is
an explicitly named generic empty interface, already emitted as a phantom by the producer.

The generic-marker-catalog lab reduces this to Marker<T> {}, Options<T> { marker:Marker<T> },
and a separate consumer importing only Options. Before the fix, both the immutable CLI
probe and the registered catalog regression fail with the Options API mismatch.

The fix lets shipped, explicitly named generic empty declarations claim their name in
Anonymous. The existing target-first traversal names the generic declaration before its
applications; existing alias shaping then preserves the producer phantom contract.
Nongeneric empty objects keep their prior representation. No generic type argument,
constraint, input hash, or catalog API guard is discarded.

The typed consumer passes Options<string>, reads and writes Marker<string>, rejects a
Marker<int> setter argument, accepts the nongeneric object control, and rejects a changed
shared input. All 55 focused catalog tests pass; fresh FCS checking reports zero errors
and warnings. The full gate passes 899 Generator and 90 Wire tests in both regeneration
and independent checking, the compile gate, and 461 Fable runtime checks. All 104 prior
golden trees remain byte-identical. The added fixture has 2 exact, 1 ergonomic, 2 widened, and 1 escape
symbol: existing SA002 for the phantom, TR023 for Loose, and GE004 for group naming.

The original complete shared-compatibility enumeration is preserved in CloudEdge under
artifacts/one-shot-20260913/core-catalogs-native-compatibility-enumeration. It records
Facet as the sole API mismatch and two same-family stream result constraint mismatches.
That diagnostic always fails before output; it is not acceptance evidence.

The composed actual-package diagnostic under
core-catalogs-native-marker-compatibility-enumeration proves Facet's producer and consumer
API hashes now agree exactly (22d3b219...). All shared API and arity comparisons pass;
only the two previously identified stream-result constraint mismatches remain. The
diagnostic still fails deliberately before redirects/output and later ownership checks,
so it does not establish accepted native generation or compilation. The source candidate,
diagnostic changes, payload hashes, and unchanged pinned profile are recorded separately.
