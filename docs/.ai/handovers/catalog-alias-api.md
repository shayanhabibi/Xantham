# Catalog nullable alias API compatibility

Base: `35feabd5a975f626215762fa3365416b778d4fcd`.
Branch: `fix/catalog-alias-api`.

## Change

Resolve recovers nullable nongeneric object union aliases in catalog mode. Both the source
union and recovered non-nullable alias must have no alias arguments for the new path;
existing literal-union behavior is retained. Shape reuses the option layer of a named nullable
heterogeneous alias, while literal unions still wrap their generated non-nullable enum.
No catalog comparison, identity key, source check, arity check or constraint check is relaxed.
No finding codes are added.

Dependency reproducer (`catalog-alias-api-lab`):

```typescript
export type Value = string | number | null;
export type Output = { type: "text"; value: string } | { type: "json"; value: Value };
export interface Part { output: Output; }
export type Choice = { type: "auto" } | { type: "tool"; toolName: string };
export type Options = { prompt: string; choice?: Choice; };
export interface Model { generate(options: Options): Promise<Part>; }
```

Consumer:

```typescript
import { Model, Part } from "alias-api-owner-lab";
export declare class Client {
    accept(part: Part): Part;
    generate(options: Parameters<Model["generate"]>[0]): Promise<Part>;
}
```

The producer exposes `Choice`; the consumer previously substituted an erased union of cases
at the same `Options.choice` site. Nullable `Value` was also wrapped a second time at tagged
payload fields. The new producer/consumer regression compiles direct reuse of Options/Part,
reads a canonical Choice, and pattern-matches JSON payloads as exactly one option layer.
A Shape unit test separates nullable aliases from nullable literal enums; the pre-existing
literal-union test remains unchanged.

## Measurement

Existing golden source changes are confined to type-fest: six reference sites (12 diff lines)
remove redundant `option` around already-nullable aliases: JsonObject, JsonArray,
StructuredCloneable indexers and Jsonifiable callbacks. Its line count is unchanged;
25 exact / 65 ergonomic / 205 widened / 2 escape becomes 26 / 64 / 205 / 2.
TR032 occurrences fall from 320 to 314 because those sites reuse the alias's nullable mapping.
All other existing golden bindings and manifests are unchanged.
The new ordinary lab is 1 exact / 4 ergonomic / 4 widened / 1 escape; its GE004 finding is
existing dependency-module naming provenance. Catalog generation has a separate typed test.

## Validation

The targeted declaration catalog suite passes 49 tests on writing and independent checking.
A fresh Generator FCS check is clean. Full regeneration and independent checking pass
873 Generator tests and 90 Wire tests; the compile gate and 461 Fable runtime checks pass.
Logs: `/tmp/catalog-alias-targeted.log`, `/tmp/catalog-alias-full-final.log`,
`/tmp/catalog-alias-fcs-recursion.jsonl`, `/tmp/catalog-alias-findings-before.log`.

## Actual SDK boundary

The original AI Search Options and AI Gateway/Workers AI ToolResultOutput mismatches are
resolved by the bounded change. Full AI catalog composition still stops at
LanguageModelV3Content / LanguageModelV4Content. Those declarations use
`NonNullable<JSONValue>` and expose a separate member-set alias match / recursive expansion
inconsistency. The catalog guard remains active. A two-line recursive JSON prototype is in
`tests/Xantham.Generator.Tests/obj/catalog-fixtures/recursive-json-prototype` and is assigned
as the next bounded fix; these results do not establish full AI catalog acceptance.
