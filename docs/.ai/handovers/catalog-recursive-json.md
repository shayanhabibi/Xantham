# Catalog recursive JSON and NonNullable aliases

Base: `8163784` on `fix/catalog-alias-api`.

## Reproducer

Dependency:

```typescript
export type Value = string | number | null | Value[];
export type Output = { kind: "text"; text: string } | { kind: "result"; result: NonNullable<Value> };
```

Consumer:

```typescript
import { Output } from "recursive-json-owner-lab";
export declare function accept(output: Output): Output;
```

Both recursive and nonrecursive versions reproduce an API mismatch before this change.
The producer matched NonNullable's members to the nullable Value alias, reintroducing null.
The consumer expanded the same type and encountered a different recursive cutoff.

## Contract

Resolve recovers source names for canonical nongeneric union aliases in catalog mode;
checker alias-symbol identity alone is insufficient because transformed types can retain
that symbol. The checker-declared type must match the actual type being named. Shape names
canonical shipped union aliases at reference positions. Member-set matching preserves the
nullability boundary, with literal enum declarations retaining their existing value-only use.
Catalog API, source, constraint and arity guards are unchanged. No finding codes are added.

The producer now emits the same deliberately bounded recursive Value used by its consumer:

```fsharp
type Value = U3<string, float, obj[]> option
// Output.Result payload:
U3<string, float, Value[]>
```

The recursive alias's obj cutoff remains reported (TR001); NonNullable's result has no outer
option, and recursive array items can still be nullable. A typed catalog test constructs the
payload, passes it through the consumer function and reads the canonical producer type.
Two Shape tests separately protect heterogeneous alias nullability and nullable literal
union enum reuse.

## Actual SDK diagnostic evidence

All six projects generate with catalog references and compile together against the project's
exact support package pins on net8.0, with zero warnings and zero errors:

| Project | Catalog dependencies | Exact / ergonomic / widened / escape |
| --- | --- | --- |
| AI SDK Provider v3 (3.0.15) | none | 48 / 93 / 216 / 3 |
| AI SDK Provider v4 (4.0.10) | none | 61 / 189 / 415 / 3 |
| OpenAI Compatible | Provider v4 | 15 / 20 / 85 / 11 |
| AI Search provider (0.1.1) | Provider v3 | 3 / 5 / 58 / 2 |
| Workers AI provider (4.0.0), all 5 inputs | Provider v4 + OpenAI Compatible | 35 / 21 / 100 / 6 |
| AI Gateway provider (4.0.0), all 19 inputs | Provider v4 + OpenAI Compatible | 16 / 17 / 99 / 4 |

Diagnostic solution and machine-readable summary:
`tests/Xantham.Generator.Tests/obj/catalog-fixtures/ai-alias-api/AI.Probe.slnx` and
`generation-and-compile-summary.json` in the same directory.
Logs: `/tmp/ai-recursive-fixed-probe.log`, `/tmp/ai-recursive-leaves-probe.log`,
`/tmp/ai-recursive-compile.log`. The build took 17.77 seconds.
These are generation and compilation checks, not live inference or lifecycle acceptance.
The root delivery flow must regenerate catalogs with its own packed immutable tool;
worktree generator fingerprints are not interchangeable with the integration checkout.

## Golden measurement

Existing source differences are three type-fest indexers (six diff lines): nullable members
that additionally admit undefined now expand the non-null value members before applying one
option layer. They no longer wrap the already-nullable JsonValue/Jsonifiable alias in another
option. Recursive arrays refer to their canonical nullable value type. Existing finding counts
and all other existing golden files are unchanged. The new ordinary lab has 1 exact /
0 ergonomic / 3 widened / 1 escape; GE004 is existing dependency-module naming provenance.

## Regression validation

Fresh FCS: zero errors and warnings. Full regeneration and independent checking pass
878 Generator tests and 90 Wire tests; the compile gate and 461 Fable runtime checks pass.
Log: `/tmp/catalog-recursive-full.log`; FCS: `/tmp/catalog-recursive-fcs.jsonl`.

A subsequent full-sized typed consumer is being checked by the delivery lane. Its direct
WorkersAIChatLanguageModel → LanguageModelV4 and AISearchChatLanguageModel → LanguageModelV3
subtype constraints currently fail FS0193. The unified Gateway/OpenAICompatible factory path
compiles. This is a separate class/interface representation boundary; the six-project build
above must not be reported as proof of those subtype conversions.
