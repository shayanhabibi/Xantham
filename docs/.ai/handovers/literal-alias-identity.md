# Inline literal union ownership across catalogs

The Workers producer exposed `ImageCompositeMode`, while Containers reached the same
six literal values only through `RequestInitCfPropertiesImageDraw.composite`. Shape's
member-set fallback gave the producer property the named alias and the consumer
property an anonymous enum. Authenticated source inputs were identical; their generated
F# APIs disagreed because reachable declarations differed between compiler programs.

The minimal `literal-alias-identity-lab` puts `Value.kind?: "a" | "b"` beside named
`Mode` and reversed-order `ReversedMode` aliases and a nominal TypeScript enum. A
separate consumer imports only Value and Nominal and accepts `"b" | "a"`. Before the
fix the registered catalog test failed with a Value API mismatch (52 other tests passed).

The final fix keeps inline literal unions anonymous in catalog mode. Shape's existing
member-set fallback still handles nonliteral unions. Direct alias references and
recovered NonNullableAlias wrappers keep their named owners. DeclarationCatalog and
all source/API/arity/constraint checks are unchanged. The typed catalog regression
passes the producer Value and its nested enum into the consumer and preserves the
nominal enum boundary. Named aliases in opposite literal orders retain independent
identities. A changed dependency input remains rejected.

A rejected experiment collapsed named string-alias identities to their literal values.
Although its new test passed, nine existing tests rejected that behavior: named aliases
lost declaration handles, private same-valued aliases became the same F# type, and
alias declaration files disappeared from source closures. Those existing contracts
were retained; the experiment was removed. Its diagnostic patch and log are available
locally at `/tmp/xantham-literal-alias-global-collapse.patch` and
`/tmp/xantham-literal-alias-focused.log`.

Validation: all 53 focused catalog tests pass. The complete regeneration and independent
check phases each pass 887 Generator and 90 Wire tests; the compile gate and 461 Fable
runtime checks pass. Fresh FCS checking reports zero errors and warnings. All 100
pre-existing golden trees remain byte-identical. The added golden
has four exact, one ergonomic, zero widened, and one escape symbol. The escape is the
existing GE004 namespace-derived group-module naming advisory; catalog producer and
consumer module ownership is exercised separately by the typed test.

The immutable actual-package probe under CloudEdge
`artifacts/one-shot-20260913/core-catalogs-native-anonymous-literals` passes the former
ImageDraw mismatch and reaches a subsequent TraceItem API mismatch. It records all
copied CLI payload hashes, the exact source diff, unchanged input profile hashes,
net8.0 consumer settings, and exact pinned support-package versions. Actual native
catalog generation and typed Request/switchPort composition are not yet proven by
that probe.
