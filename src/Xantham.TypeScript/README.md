# Xantham.TypeScript

This is the 'harness' for utilising the TypeScript compiler and providing a more focused experience for creating
extensions and exposing other internal methods/functions from the TypeScript compiler.

## Proofs

Several wrappers in this project perform operations the F# type system cannot make safe on its own:

- **`failwith` guards** in the `Source` / `ExternalModule` constructors (e.g. "a source file marked as an
  external module had no symbol"),
- **`.Value` field access** on `option`-typed package.json fields and on Fable DU payloads, and
- the **`Ignore` fall-through** in `XanTagKind.Create` and the `ContainsKey` predicates
  (`IsTopLevelStatementKind`, `IsMemberDeclarationKind`, …).

Each of these is sound only because of an *invariant* that holds for real-world TypeScript declarations. Those
invariants are not assumed — they are **proven** as executable tests in
[`tests/Xantham.TypeScript.Tests/Program.test.fs`](../../tests/Xantham.TypeScript.Tests/Program.test.fs), run with:

```bash
npm run test:typescript
```

Every proof carries a **stable ID** (`SF-n`, `XTK-n`, `ND-n`, `TC-n`, `OF-n`, `TF-n`, `EN-n`, `SY-n`, `TR-n`, `NW-n`) embedded in its test
name. Wrapper XML docs cite these IDs, so when a proof fails you can jump straight from the broken invariant to the
wrapper that relied on it (and vice versa).

### Corpus

Each proof is asserted over a corpus of real, published `.d.ts` packages (one full proof series per file):

`@cloudflare/ai-chat`, `@cloudflare/think`, `@cloudflare/dynamic-workflows`, `@cloudflare/workers-types`,
`@cloudflare/sandbox`, `@cloudflare/shell`, `@cloudflare/puppeteer`, `@cloudflare/voice`, `agents`,
`@types/three`, `solid-js`, `@types/d3`, `@types/node`, `@types/semver`, `ansi-regex`, `type-fest`,
`@types/lodash`, `animejs`, `typescript`.

These were chosen to span the constructs that stress the wrappers: ambient modules, global augmentation, deep
namespaces, heavy generics, mapped/conditional types, and mixed script/module files. The later additions widen the
extremes — `typescript` and `@types/node` are very large and script-heavy, `type-fest` leans on conditional/mapped
types, `@types/lodash`/`@types/d3` carry dense overload sets, and `ansi-regex` is a tiny single-export module.
Adding a fixture that breaks a proof is the intended way to discover an unsound wrapper path.

### SF · Source File Model

Invariants the TypeScript compiler guarantees about source files, which the `Source` model depends on.

| ID | Invariant | Backs |
|----|-----------|-------|
| SF-1  | The program under test contains source files (the corpus is non-empty). | quantification base for all proofs |
| SF-2  | A source file has a checker symbol **iff** it is an external module. | `ExternalModule.create` symbol guard |
| SF-3  | An external-module symbol resolves and exposes ≥1 module specifier. | `ExternalModule.create` `moduleSpecifierInvariant` (`NonEmptyArray`) |
| SF-4  | A script (non-module) source file has a `locals` map. | `Script` `Locals` |
| SF-5  | A script source file has **no** `exports` map (distinguishes script from module). | `SourceKind` discrimination |
| SF-6  | An external module has an `exports` map. | `ExternalModule.create` `symbolExports` guard |
| SF-7  | An external module also has a `locals` map. | `ExternalModule.create` `sourceFileLocals` |
| SF-8  | An external module resolves a `package.json` (its own or a nearest ancestor's). | `Source.create` `packageJsonFields.Value` |
| SF-9  | That resolved `package.json` exposes a `version`. | `Source.create` version `.Value` |
| SF-10 | A non default-lib script source also resolves a `package.json`. | `Source.create` (script path) |
| SF-11 | That `package.json` is both **named** and **versioned**. | `Source.create` `closestNamedAndVersionedPackageJsonFields.Value` |

### XTK · Wrapper Totality

Invariants that the classifier wrappers are *total* over real input — they never reach their failure / `Ignore` case.

| ID | Invariant | Backs |
|----|-----------|-------|
| XTK-1 | `Source.create` never throws on real input (exercises every SF-* guard end to end). | `Source.create` |
| XTK-2 | Every external-module export classifies as `TypeDeclaration` or `ModulesAndExports`. | `XanTagKind.Create` |
| XTK-3 | Every script top-level statement classifies the same way. | `XanTagKind.Create` |
| XTK-4 | Every class & interface member classifies. | `MemberDeclaration.Create` / `IsMemberDeclarationKind` |
| XTK-5 | Every symbol `escapedName` maps to a `SymbolName` (string or known `InternalSymbolName`). | `SymbolName.Create` |
| XTK-6 | Every symbol-table value declaration classifies (asserts no `Ignore`). | `XanTagKind.Create` |
| XTK-7 | Every top-level statement kind is recognised. | `TopLevelStatements.IsTopLevelStatementKind` |
| XTK-8 | Every exported-symbol declaration kind is recognised. | `TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind` |
| XTK-9 | Every local-symbol declaration kind is recognised. | `TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind` |

> Note: the DU `.Value` accessors (e.g. `XanTagKind.Value`, `TypeNode.Value`) rely on a *separate* invariant —
> Fable 5's internal single-payload DU layout (`fields[0]`) — which is documented inline at each accessor rather
> than proven by the corpus, since it is a property of the Fable compiler, not of the input `.d.ts`.

---

The proofs below (suite **"Node invariants"** in `Program.test.fs`) sit one level lower than SF/XTK: rather than the
`Source` model or the classifier wrappers, they pin down the **shape of the parsed AST and the answers the
`TypeChecker` gives** for the narrow dialect that appears in real `.d.ts` files. Many run via `testSyntaxKind`,
which **auto-skips** a fixture that contains no node of the relevant `SyntaxKind` — so a green run means "held
wherever the construct occurred", not "occurred in every fixture".

### ND · Declaration-File Node Shape

Which syntactic node, member, operator, and literal kinds actually occur in `.d.ts` corpora — the facts that let
the wrappers narrow TypeScript's full node union to a manageable subset.

| ID | Invariant | Backs |
|----|-----------|-------|
| ND-1  | Every `NumericLiteral` `text` parses as a JS `number`. | numeric literal wrapping |
| ND-2  | Every `BigIntLiteral` `text` (minus the `n` suffix) parses as a `BigInteger`. | bigint literal wrapping |
| ND-3  | Every `StringLiteral` has a non-null `text` (empty allowed). | string literal wrapping |
| ND-4  | Every `NoSubstitutionTemplateLiteral` has a non-null `text` (empty allowed). | template literal wrapping |
| ND-5  | A `PrefixUnaryExpression` operator in a `.d.ts` is only ever `MinusToken`. | literal-type sign handling |
| ND-6  | A `PrefixUnaryExpression` operand is always a `NumericLiteral`. | literal-type sign handling |
| ND-7  | A `LiteralTypeNode.literal` is one of: `null`/`true`/`false` keyword, numeric/string/bigint/template literal, or a prefix-unary expression. | `LiteralTypeNode` wrapping |
| ND-8  | Declaration files use only a known subset of `SyntaxKind`s. | `DeclarationFileNodes.IsKnownDeclarationFileNodeSyntaxKind` |
| ND-9  | `ClassDeclaration` members are only property/method/get/set/index-signature/constructor. | class-member subset |
| ND-10 | `InterfaceDeclaration` members are only property/method-signature/get/set/index-signature/call-signature/construct-signature. | interface-member subset |
| ND-11 | No node in a `.d.ts` carries decorators. | decorator-free assumption |
| ND-12 | A `TypeOperator` operator is only `keyof`, `readonly`, or `unique`. | `TypeOperatorNode` classification |
| ND-13 | A `TypeOperator`'s inner `.type` always parses as a `TypeNode`. | `TypeNode.IsTypeNodeKind` |

### TC · Type-Checker Resolution

What the `TypeChecker` resolves a node to — the type-level facts the wrappers depend on when they reach past syntax
into checked types.

| ID | Invariant | Backs |
|----|-----------|-------|
| TC-1 | A `ClassDeclaration` resolves to an object type carrying the `Class` flag. | class type resolution |
| TC-2 | An `InterfaceDeclaration` resolves to an object type carrying the `ClassOrInterface` flag. | interface type resolution |
| TC-3 | A non-optional `MethodDeclaration` resolves to an `Object` type. | method type resolution |
| TC-4 | An optional `MethodDeclaration` resolves to a `Union` type. | optional-method resolution |
| TC-5 | A non-optional `MethodSignature` resolves to an anonymous `Object` type. | method-signature resolution |
| TC-6 | An optional `MethodSignature` resolves to a 2-member `Union` of `undefined` and an anonymous object. | optional-signature resolution |
| TC-7 | A `TypeOperator` node has no symbol. | `TypeOperatorNode` (symbol-free) |
| TC-8 | A `readonly` `TypeOperator` resolves to the **same** type as its inner `.type`. | `readonly` operator transparency |
| TC-9 | A non-`readonly` `TypeOperator` resolves to a **different** type than its inner `.type`. | `keyof`/`unique` operator effect |
| TC-10 | Every `TypeNode` in the corpus resolves to a `Ts.Type` via the checker (`getTypeFromTypeNode` is `Some`). | `resolveTypeBase` syntax→type totality |

### OF · Object Flags

The type classifier in `XanTagKind.fs` resolves an object type's `ObjectFlags` through `typeFlagObjectKindSet`
via a **throwing `Array.find`**. These proofs pin that the flags are mutually exclusive (so the first matching
row is unambiguous) and that the `Reference`-keyed shape access is sound.

| ID | Invariant | Backs |
|----|-----------|-------|
| OF-1 | A `Class`/`Interface` object type carrying `Reference` exposes type parameters or a `thisType`. | `InterfaceType` `typeParameters`/`thisType` access |
| OF-2 | A `Class`/`Interface` object type **without** `Reference` has no type parameters and no `thisType`. | generic-vs-instantiated discrimination |
| OF-3 | An object type's `ObjectFlags` are mutually exclusive — at most one of the classifier's primary object kinds is set. | `typeFlagObjectKindSet` first-match is unambiguous |
| OF-4 | An object type carrying the `Tuple` flag always also carries `Reference`. | tuple sub-classification (`Tuple` ⇒ tuple reference) |
| OF-5 | The curated `ObjectFlags` exclusive/inclusive map holds for every object type in the corpus. | `typeFlagObjectKindSet` row disjointness |

### TF · Type Flags

`typeFlagPrimaryKindSet`/`typeFlagLiteralKindSet` likewise classify a `Ts.Type`'s `TypeFlags` by a **throwing
`Array.find`** with no `Ignore` fall-through. These proofs pin the flag algebra those tables assume.

| ID | Invariant | Backs |
|----|-----------|-------|
| TF-1 | A `Union`+`Boolean` type's members are exactly the `true` and `false` literal types. | boolean type modelling |
| TF-2 | A `Union`+`Boolean` type has exactly 2 union members. | boolean type modelling |
| TF-3 | A `Literal` flag can occur without `EnumLiteral` (the corpus exercises non-enum literals). | literal sub-classifier coverage |
| TF-4 | The curated `TypeFlags` exclusive/inclusive map holds for every type in the corpus. | `typeFlagPrimaryKindSet` row disjointness |
| TF-5 | A type's primary-kind `TypeFlags` are mutually exclusive — at most one is set. | `typeFlagPrimaryKindSet` first-match is unambiguous |

### EN · Enum Resolution

How `Enum`/`EnumLiteral`-flagged types resolve to a symbol and value declaration — what lets the classifier route
the whole-enum vs enum-member cases and read their declarations safely.

| ID | Invariant | Backs |
|----|-----------|-------|
| EN-1 | A type with the `Enum` flag always also carries `EnumLiteral` and `NumberLiteral`. | enum type classification |
| EN-2 | An `EnumLiteral` type does **not** always carry the `Enum` flag (observed) — separates whole-enum from enum-member. | `EnumLiteral` vs `Enum` discrimination |
| EN-3 | A type with the `Enum` flag resolves to a symbol whose `valueDeclaration` is an `EnumDeclaration`. | enum symbol value-declaration access |
| EN-4 | An `EnumLiteral` type also carrying `Enum`/`Union` resolves to a symbol with an `EnumDeclaration` `valueDeclaration`. | whole-enum literal resolution |
| EN-5 | An `EnumLiteral` type **without** `Enum`/`Union` resolves to a symbol whose `valueDeclaration` is an `EnumMember`. | enum-member literal resolution |

### SY · Symbols & Identity

Symbol presence, identity, and declaration backing — what lets the wrappers safely call `.Value`/`Option.get` on
symbol lookups and treat node/symbol ids as keys.

| ID | Invariant | Backs |
|----|-----------|-------|
| SY-1 | Every `InterfaceDeclaration` name resolves to a symbol. | interface symbol lookup |
| SY-2 | Every `ClassDeclaration` resolves to a symbol. | class symbol lookup |
| SY-3 | Every `ClassDeclaration` symbol has a `valueDeclaration`. | class value-declaration access |
| SY-4 | Every node has a positive id (`ts.getNodeId`). | node identity / keying |
| SY-5 | Every symbol has a positive id (`ts.getSymbolId`). | symbol identity / keying |
| SY-6 | Every **non-transient** symbol has ≥1 declaration. | declaration enumeration |
| SY-7 | A **transient** symbol may still carry declarations (observed) — transience alone does not imply zero declarations. | complement to SY-6: the declaration-presence guard keys on transience, not the converse |

### TR · Type References

How a `TypeReferenceNode` resolves and how its type arguments relate to the target's arity.

| ID | Invariant | Backs |
|----|-----------|-------|
| TR-1 | A `TypeReference.typeName` always resolves to a symbol. | type-reference resolution |
| TR-2 | `TypeReference.typeArguments` is either absent or non-empty (never an empty array). | type-argument handling |
| TR-3 | A `TypeReference.typeName` resolving to a **non-transient** symbol has ≥1 declaration. | type-reference declaration access |
| TR-4 | A `TypeReference` may supply **fewer** type arguments than the target's parameters; every omitted parameter has a default or a constraint (and it never supplies more). | type-argument arity reconciliation |

### NW · Node Wrappers

The wrapper constructors produce the expected wrapped shape over real input.

| ID | Invariant | Backs |
|----|-----------|-------|
| NW-1 | `TypeOperatorNode.Create` wraps every `TypeOperator` node (non-null result). | `TypeOperatorNode.Create` |
| NW-2 | `MethodDeclaration.Create` exposes an anonymous-object `.Type` and a `.Value` of kind `MethodDeclaration`. | `MethodDeclaration.Create` |
