---
paths:
    - src/Xantham.Fable/**
---

- `TypeScript.fs` is auto-generated via ts2fable — **do not edit**; extend via `Utils/TypeScript.Extensions.fs`
- TypeNodeKind (syntactic) ≠ TypeKind (semantic): `Foo<'A>` is `TypeReference` syntactically but may resolve to `TypeLiteral` after type-checking
- Stack-based traversal in `Read.fs` is intentional — no TCO in JS

#### XanthamTag invariant
`node[TRACER_TAG]` is always a wrapper `{ Value: XanTagKind, [TRACER_PROXY]: "XanTagKind" }` — never the DU directly. `tag.Value: XanTagKind`; `tag.Value.Value: U2<Ts.Type, Ts.Node>`. `Guard` points to the symbol/type/node where `GuardTracer` is stored under `TRACER_GUARD`.

#### Signal.fulfillWith self-reference hazard
`FulfillWith` runs the thunk immediately. Never fulfill a signal with a thunk that reads the same signal — it reads `ValueNone`, subscribes to its own `.Invalidated`, and stalls permanently. `store.Builder` and `GuardedData.AstNodeBuilder.getOrSetDefault tag` always refer to the same object when the TypeStore was created from that tag.

#### Shared-symbol hazard (`resolveTypeBase`)
Two distinct `Ts.Type` objects can share the same underlying symbol (e.g. `Array<string>` and `Array<T>`). `resolveTypeBase` always checks `signalCache.ContainsKey(guard.Value)` **regardless of Visited/Unvisited state**, but only when `guard <> xanTag.Guard` (prevents treating the type as a cached entry for itself). When `cachedKey = typ.TypeKey`, returns `TypeSignal.ofKey typ.target.TypeKey`; otherwise `TypeSignal.ofKey cachedKey`.

#### `Reading/TypeReference.fs` `fromType`
Never add a `signalCache.ContainsKey` guard in `fromType` — the tag was just registered by `tryGetOrRegisterStore` so the check is always true, creating a self-referential `fulfillWith` that permanently stalls the builder.

#### TypeLiteral signature handling
`Ts.Signature.getDeclaration()` can return `FunctionTypeNode` or `ConstructorTypeNode` — built inline in `TypeFlagObject.fs` (not Declaration nodes, cannot go through `RefMember.fromDeclaration`).

#### Read pipeline (current)
`runReader` (stack traversal + `Dispatcher.dispatch`) → `assembleResults` (groups by TypeKey, sorts by identity priority, splits non-dupes from dupes) → build `EncodedResult` → `trimTypeReferenceArrayTupleDuplicates` (collapse TypeRef vs Array/Tuple duplicate pairs) → `mergeExports` (merge Interface members + combine Function overloads via `TsOverloadableConstruct.Combine`) → `selectAndMergeWinnersInDuplicates` (promote `List.head` winner from each remaining duplicate group).

Identity priority (lower = higher priority): `DeclarationPosition (0) > Symbol (1) > AliasSymbol (2) > Id (3)`. `duplicates[0]` is always the winner.

#### Diagnostics
- Log markers: `[CIRCREF]` circular ref, `[MISSREF]` missing builder
- ~4 `Id N` entries with `ValueNone` builders are expected (anonymous types from complex generics) — not errors
- `findUnknownFields` false positives for `unknown[]` / `() → unknown` — `unknownKey` is also a legitimate type value
- `isFromEs5Lib` matches all `/lib/lib.*.d.ts`, not just `lib.es5.d.ts`
