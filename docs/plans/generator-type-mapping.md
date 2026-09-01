# TypeScript → F# type mapping for the Wire-based generator

Research document for phase 6 of `wire-remaining-work.md` ("it needs a direction before it
needs code"). This is the direction for the *type mapping* specifically: given what the
TypeScript 7 compiler tells us over the Wire, what F#/Fable construct does each TypeScript
construct become, and what do we trade away in each case. It is meant to be iterated on —
every mapping below records its alternatives and its open questions, and nothing here is
committed until the generator exists to test it.

Sources drawn on: the Wire's generated API surface (`ProtoApi.generated.fs`,
`Proto.generated.fs`), the archived pre-Wire pipeline (`.archive/src/Xantham.Fable.Core`
erased-idiom library, `.archive/src/Xantham.Generator` categorization/rendering,
`.archive/src/Xantham.Common` IR schema), the Glutinum heritage this repo forked from, and
ts2fable as the syntactic-only baseline to improve on.

---

## 1. Goals and the three-way tension

Every mapping decision trades between three properties:

- **Safe** — the F# type rejects at compile time what TypeScript would reject, and does not
  claim more than TypeScript proved. No silent `obj` where a real type was available; no
  representation whose misuse compiles but breaks at runtime.
- **Easy** — call sites read like idiomatic F#. A user configuring a library should write
  `{ opts with Strict = true }` or a `[<ParamObject>]` call, not build `JsonObject`s or
  thread `!^` casts through every line.
- **Flexible** — when the mapping is too tight or too loose for a consumer, there is a
  sanctioned escape hatch (widen to `obj`, cast with `!!`, drop to dynamic access), and the
  generator itself is configurable per construct rather than hard-coding one policy.

These conflict. `U9<...>` is safe and miserable to use; `obj` is easy and unsafe; a bespoke
erased wrapper per template-literal type is safe and flexible but explodes the surface. The
resolution proposed here: **every mapping is assigned a fidelity tier, the generator records
the tier in the emitted doc comment, and constructs where tiers genuinely compete become
generator options with a documented default.**

Fidelity tiers used throughout:

| Tier | Meaning |
|------|---------|
| **Exact** | Same set of values, same operations, statically enforced. |
| **Ergonomic** | Different shape, equivalent safety (e.g. options object → `[<ParamObject>]` overload). |
| **Widened** | Strictly more values accepted/produced than TS allows (e.g. template literal → `string`). Safe to *read*, lossy to *write*. |
| **Escape** | `obj` / dynamic. Present deliberately, marked, never silent. |

A hard lesson carried from the archive (`prevent-silent-drops-progress.md`): the pre-Wire
pipeline's worst failures were *silent* — a construct it did not understand was dropped and
nobody noticed until a consumer needed it. The generator must make "I widened this" and "I
dropped this" loud: tier annotations in doc comments, a per-run report of every Widened/Escape
emission, and ideally a diff-able manifest so upgrades show fidelity regressions.

---

## 2. What the compiler gives us

This is the decisive difference from every prior attempt (ts2fable, the archived Fable
extractor): the Wire exposes the **checker**, not just the syntax tree. We do not have to
reimplement TypeScript's type algebra — we can ask the compiler to run it.

### 2.1 The two layers and their crosslink

**Syntactic** — the binary AST read in place: `Node<'Tag>` over 351 kinds, full declaration
structure, modifiers, JSDoc, exact source text. This layer preserves what the *author wrote*:
names, aliases, parameter names, doc comments, module organisation.

**Semantic** — 142 checker methods. The core object is `TypeResponse`
(`Proto.generated.fs:534`): stable `Id`, `TypeFlags`, `ObjectFlags`, literal `Value`,
tuple data (`ElementFlags`, `FixedLength`, `Readonly`), indexed-access parts
(`ObjectType`/`IndexType`), conditional parts (`CheckType`/`ExtendsType`), substitution
parts, template-literal `Texts`, type-parameter data, intrinsic name, **and crucially
`AliasSymbol`/`AliasTypeArguments`** — a resolved type still remembers the alias it came
through. `SymbolResponse` carries name, flags, and `Declarations`/`ValueDeclaration` as node
handles.

The crosslink matters architecturally: symbol → declaration node handles → typed AST, and
node → `getTypeAtLocation`/`getSymbolAtLocation` → checker. The generator can walk
declarations for structure and drop into the checker at any node for meaning, then come back.

### 2.2 Checker calls the mapping relies on

| Need | Wire call(s) |
|------|-------------|
| Type of any declaration/expression | `getTypeAtLocation(s)`, `getTypeOfSymbol(s)`, `getTypeFromTypeNode` |
| Flatten an object/intersection to members | `getPropertiesOfType`, `getApparentPropertiesOfType`, `getApparentType`, `getReducedType` |
| Call/construct shapes, overloads | `getSignaturesOfType`, `getParametersOfSignature`, `getReturnTypeOfSignature`, `getResolvedSignature`, `getThisParameterOfSignature`, `getTypePredicateOfSignature` |
| Union/intersection members | `getTypeArguments` (for references), type `Id` graphs via `TypeResponse` fields |
| Generics | `getConstraintOfTypeParameter`, `getBaseConstraintOfType`, `getDefaultFromTypeParameter`, `getTypeParametersOfType` |
| Index signatures | `getIndexInfosOfType` (`KeyType`/`ValueType`/`IsReadonly` per info), `getIndexTypeOfType` |
| `keyof T` as a closed set | `getIndexTypeOfType` → union of string literals |
| Strip/hoist nullability | `getNonNullableType`, `getNonMissingTypeOfSymbol` |
| Literal widening control | `getWidenedType`, `getBaseTypeOfLiteralType`, `getFreshTypeOfType`/`getRegularTypeOfType` |
| Conditional types | `getCheckTypeOfType`, `getExtendsTypeOfType`, `getTrueTypeOfConditionalType`, `getFalseTypeOfConditionalType` |
| Heritage | `getBaseTypes`, `getBaseTypeOfType` |
| Exports & modules | `getExportsOfModule`, `getExportsOfSymbol`, `getMembersOfSymbol`, `getSymbolOfSourceFile`, `getFullyQualifiedName` |
| Docs | `getDocumentationComment`, `getJsDocTags` |
| Enum values | `getConstantValue` |
| Sanity checks on our own output decisions | `isTypeAssignableTo`, `typeToString`, `typeToTypeNode` |
| Alias identity | `getAliasSymbolOfType`, `getAliasTypeArgumentsOfType`, `getAliasedSymbol` |

Batching note: all of these route through the mailbox's `batchRequests`, so "ask the checker
a lot of small questions" is a viable per-declaration strategy, not a per-question round trip.

### 2.3 Declaration-driven skeleton, checker-resolved leaves

**Recommended architecture:** walk *exported declarations* (syntactic) to decide what F#
declarations exist and what they are named; resolve every *type reference position* through
the checker to decide what F# type is written there.

- Declaration-driven, because the checker's structural answers erase authorship: expanding
  `Partial<Options>` at every use site produces anonymous blobs, loses the alias name, and
  duplicates structurally-equal types. The syntax tree tells us "the author exported an
  interface named `Options` with these doc comments".
- Checker-resolved, because the syntax tree cannot evaluate `Omit<T, K>`, a conditional
  type, `keyof`, or a template literal — and the archive proved that reimplementing that
  algebra (2,287 lines of IR schema, a resolver, a categorizer) is a treadmill that tracks
  the language forever. The checker does it for free and is *always* right for the pinned
  compiler version.
- The `AliasSymbol` field arbitrates between the two: when a resolved type still carries an
  alias that we also generated a declaration for, emit the alias name, not the expansion.
  When the alias is not exported (or not representable), expand.

Memoize on `TypeResponse.Id`. Type ids are stable within a snapshot, which gives us
hash-consing of anonymous structural types (emit once, reference thereafter) and cycle
detection for free — the archived pipeline's stack overflows on `three`/`solid-js`/
`typescript` (`litmus-tests.md`) were exactly the cost of not having stable ids and doing
recursive descent over syntax.

---

## 3. The Fable target vocabulary

The toolbox we map *into*. Listed once here; the catalogue below refers to these by name.

| Tool | Semantics |
|------|-----------|
| `[<Erase>]` types / unions | Compile-time-only wrapper, erased to the payload in JS. The workhorse for zero-cost nominal safety. |
| `[<StringEnum>]` (+ `CaseRules`) | F# DU whose cases compile to string literals. |
| `U2<..>`…`U9<..>` | Erased untagged unions; `!^` upcast, pattern-match to read (unreliable at runtime — cases are untagged), implicit `op_ErasedCast`. |
| `[<TypeScriptTaggedUnion "tag">]` | Real pattern-matchable DU over a TS discriminated union; matches on the tag property. |
| Interfaces (`abstract` members) | The default object-type representation; erased, structural at runtime. Overloaded abstract members are legal. |
| Anonymous records `{| x: int |}` | Compile to POJOs; good for closed inline object types in *input* position. |
| `[<AllowNullLiteral>]` | Permits `null` for a class/interface type. |
| `[<Emit "...">]`, `[<EmitIndexer>]`, `[<EmitConstructor>]`, `[<EmitMethod>]`, `[<EmitProperty>]` | Inline JS templates; indexer/constructor/etc. sugar. |
| `[<Import(..)>]`, `[<ImportAll>]`, `[<ImportDefault>]`, `[<ImportMember>]`, `[<Global>]` | Module binding. |
| `[<ParamObject>]` | Named/optional F# arguments collapse into a JS options object at the call site. |
| `[<NamedParams>]` | Named arguments for plain calls. |
| `[<ParamArray>]` | Variadic tail. |
| `'T option` / `'T voption` | Erased to `x | undefined` at the boundary (non-nested). |
| Delegates `Func<..>`/`Action<..>` or F# lambdas | Callback representations; arity semantics differ (see §4.8). |
| `[<Mangle>]` / `[<AttachMembers>]` | Overload naming control on interfaces/classes. |
| `[<CompiledName>]`, backtick identifiers | Name fidelity when F# identifiers can't match JS ones. |
| `jsNative`, `!!`, `?`, `emitJsExpr` | The sanctioned escape hatches. |
| `[<Obsolete>]` | `@deprecated` JSDoc. |
| Units-of-measure-tagged primitives | e.g. `string<'U>` — the archive (`Common.Types.fs:4`) and Wire (`Measures.fs`) both already use measure-tagged primitives as zero-cost brands. |
| The archive's erased idioms | `keyof<'T>`, `typekeyof<'T,'R>`, `proptypekey`/`proptypelock`, `PropertyRecord<'T,'K>` (`.archive/src/Xantham.Fable.Core`) — resurrect as a small runtime-support package the generated code can reference. |

A standing decision to make early: **generated output should depend on a small
`Xantham.Fable.Core`-style support package** (erased keyof idioms, maybe brand helpers)
rather than emitting those definitions into every binding. The archive already designed this
library; it was sound and is worth reviving mostly as-is.

---

## 4. The mapping catalogue

Format per entry: recommended mapping, tier, alternatives, and what the Wire supplies.

### 4.1 Primitives and intrinsics

| TypeScript | F# | Tier | Notes |
|---|---|---|---|
| `string` | `string` | Exact | |
| `number` | `float` | Exact | `int` only when a JSDoc/config override says so; TS has one number type. |
| `boolean` | `bool` | Exact | |
| `bigint` | `bigint` | Exact | Fable compiles to JS BigInt. |
| `symbol` / `unique symbol` | `JS.Symbol` | Exact / Widened | unique symbols lose uniqueness; consider erased brand per unique symbol (open). |
| `object` | `obj` | Widened | |
| `unknown` | `obj` | Escape-ish | **Decided (D8):** `obj` for now. An erased `Unknown` wrapper forcing explicit narrowing may become a config toggle if it stays cheap to implement. |
| `any` | `obj` | Escape | Tier-marked in doc comment. |
| `void` (return) | `unit` | Exact | |
| `undefined` / `null` in unions | hoisted to `option` | Ergonomic | See §4.3. |
| `never` | — | — | In unions: dropped (identity). As return type: `'T` generic or `unit` + doc note; F# has no bottom type. |
| intrinsic via `IntrinsicName` | | | The wire names intrinsics explicitly — no guessing from syntax. |

### 4.2 Literal types and their unions

- Union of string literals → **`[<StringEnum>]` DU**, `RequireQualifiedAccess`. Exact.
  Case naming via `CaseRules` when the literals are camelCase; `[<CompiledName>]` per case
  when they are not identifier-shaped (`"utf-8"` → ``` ``utf-8`` ``` or `Utf8` + CompiledName).
- Union of numeric literals → F# **enum** (`type E = A = 1`) when the values are ints;
  erased type with static members when floats. Exact.
- Mixed literal unions (string + number) → erased type + static members, or `U2<StringPart, NumPart>`. Not yet decided — tracked in §6's live threads.
- Single literal type (e.g. `kind: "click"`) → the StringEnum/enum case type where one
  exists; else the literal's base primitive, Widened, with the literal recorded in the doc
  comment. In tagged-union positions the tag is consumed by `[<TypeScriptTaggedUnion>]`
  instead.
- `boolean` literals `true`/`false` → `bool`, Widened, doc-noted.

Wire: `TypeFlags` distinguishes every literal kind; `Value` carries the payload (bigint as
decimal string); `getFreshTypeOfType`/`getRegularTypeOfType` and `getBaseTypeOfLiteralType`
control widening; enum-literal membership comes via the symbol's parent.

**Reuse from the archive:** the categorization algorithm in
`ResolvedType.Categorization.fs` is the right shape for union handling and should be ported
conceptually: partition union members into *nullability* (hoisted out), *literal-like*,
*enum-like* (and reassemble a full enum when every case of one enum is present — the checker
hands unions of enums back as their member literals, so this reassembly is load-bearing),
*primitive-like*, and *other*, then choose the representation from the surviving buckets.

### 4.3 `null` / `undefined` / optional

Policy question with a recommended default:

- `T | undefined`, optional members `x?: T`, optional params → **`'T option`**
  (**Decided (D1):** `option`, not `voption`, repo-wide). Fable erases `Some`/`None` to
  `value | undefined` at the boundary. Ergonomic, and the F# idiom.
- `T | null` → also `option` by default (the archive's choice: one `Nullable` bit,
  `undefined`/`null`/`void`/missing all collapse). Widened-by-conflation: a consumer cannot
  *write* an explicit `null` distinct from "absent" through `option`.
- APIs where the null/undefined distinction is semantic (e.g. React refs, JSON round-trips,
  the Wire protocol itself learned this with virtual-FS callbacks) need an opt-out: keep
  `null` as `'T | null` via Fable's `Null` semantics or an erased `Nullable<'T>` wrapper.
  **Decided (D1):** generator config with a per-symbol override list; default = collapse.
- Nested options (`(T | undefined) | undefined` after alias expansion, optional element of
  optional…) — Fable's erased option is unsound when nested; the generator must flatten
  after checker resolution (checker usually already has).

### 4.4 Object types: interfaces, type literals, classes

- **Exported `interface`** → F# interface: `abstract` properties (`with get, set`;
  `readonly` → get-only), abstract methods, `[<AllowNullLiteral>]` per config
  (**Decided (D2):** default off — Glutinum defaulted on for compat, but off is safer). Exact.
- **Heritage** — `extends` of a named interface → F# `inherit` on the interface. Where TS
  heritage does surgery F# can't express (extending a mapped type, `Omit`-based heritage),
  flatten via `getApparentPropertiesOfType` and emit the full member set with a doc note.
- **Inline type literals** (in parameter/return positions) → hash-cons by type `Id`:
  first occurrence emits a generated-name interface (name derived from the path:
  `Foo.Bar.options`-style synthesis existed in the archive's `SyntheticPathAssignment.fs`
  and `NamePath.fs` — reuse the naming scheme, not the code); later occurrences reference it.
  **Decided (D3):** an inline type literal in *parameter position* is additionally flattened
  into the member's `[<ParamObject>]` argument list, with the F# parameter names matching
  the literal's member names (that is what `ParamObject` emits, so the call site reproduces
  the TS object literal exactly). Anonymous records are not the default representation.
- **Construction ergonomics** — for every "plain data" interface (no methods, no call
  signatures), also emit a `[<ParamObject>]` `Create` static or a companion `create`
  function so consumers don't hand-build objects via `createObj`. This is the single biggest
  "easy" win; the old Glutinum `jsOptions` approach is superseded by `ParamObject`.
- **`class`** → the ts2fable/Glutinum two-part pattern, which remains right:
  - instance side: F# interface (as above, plus `inherit` for base classes);
  - static side + constructor: members on an `Exports`-style erased type or a
    `[<Import>]`-bound `IClassNameStatic` interface with `[<EmitConstructor>]`.
  - `abstract`/`protected`/`private` members: private dropped (they are not API), protected
    dropped with doc note (open: emit under an attribute-gated interface?), abstract same as
    concrete on the instance interface.
- **Callable + properties hybrids** (function with attached props) → interface with an
  `[<Emit "$0($1...)">]` `Invoke` member plus the properties. Ergonomic.

### 4.5 Unions (non-literal)

Order of preference, decided per union after §4.2's categorization:

1. All-literal → StringEnum/enum (Exact).
2. Discriminated by a common literal tag property (checker: every member has property `k`
   with a unique literal type) → **`[<TypeScriptTaggedUnion>]`** (Exact, pattern-matchable —
   by far the best consumer experience; detect aggressively).
3. Nullability hoisted + a *single* remaining member → `option`-wrapped member.
4. Two-to-four heterogeneous members → `U2`–`U4` (Exact for writing; reading requires
   runtime tests the consumer writes). **Decided (D4):** the `U_n` threshold is 4.
5. Larger / open unions → dedicated erased union type with static constructors per case
   (`[<Erase>] type Shade = static member inline ofString (s:string) : Shade = !!s` …), or
   `obj` at the Escape tier.

**Decided (D4):** the same union is treated by position. *Input* (parameter) position:
prefer overloads and erased-union constructors. *Output* (return/property-read) position:
prefer tagged unions where detectable, and emit typed test helpers otherwise, since the
consumer must discriminate. The generator knows the position; use it.

### 4.6 Intersections

- Intersections of object types → flatten with `getApparentPropertiesOfType` → one emitted
  interface (Exact in members, loses the "is-a" relation to each operand — mitigate by
  also emitting F# interface inheritance when every operand is a named generated interface).
- Branding intersections (`string & { __brand: "UserId" }`) → **measure-tagged primitive or
  erased single-case wrapper** (`string<userId>` per the existing `Measures.fs` idiom, or
  `[<Erase>] type UserId = UserId of string`). Exact-in-spirit; this repo already lives this
  pattern. Detection: primitive & object-with-only-phantom-members.
- Nonsensical/`never` intersections → whatever the checker reduces them to (`getReducedType`).

### 4.7 Enums

- Numeric enum → F# `enum` with the checker's `getConstantValue` per member (Exact,
  including non-sequential and computed-but-constant values).
- String enum → StringEnum DU (Exact).
- Heterogeneous / non-constant computed members → erased type + static members (Ergonomic).
- `const enum` → same as above; values are known (`getConstantValue`), and the import may
  not exist at runtime — must inline values, never `Import` the enum object.

### 4.8 Functions, callbacks, overloads

- Top-level exported functions → `[<Import>]`-bound members on an `Exports` erased type
  (Glutinum convention) — keeps the module surface in one place.
- **Callback parameters** → the arity problem: F# curried lambdas auto-uncurry at the Fable
  boundary, and in Fable 5 this works well in practice. **Decided (D5):** default to
  **delegates** (`System.Func<..>`/`System.Action<..>`) anyway, for simplicity and
  guaranteed arity. Curried-lambda emission remains a candidate config toggle later.
- **Overloads** → overloaded abstract members / static members (legal in F#). Where
  overloads differ only in literal-typed params (the `addEventListener` pattern:
  `on(event: "click", cb: MouseCb)`), emit one member per literal with a synthesized name
  (`onClick`) *in addition to* the general form — Ergonomic tier, config-gated.
- Optional params → F# optional params (`?x: T`) on members; trailing-only, as in TS.
- Rest params → `[<ParamArray>]`.
- `this` parameter → `getThisParameterOfSignature`; usually drop (Fable binds `this` via the
  interface receiver); emit doc note when the declared `this` differs from the receiver.
- Type predicates (`x is Foo`) → return `bool`, Widened, doc-noted. (An erased
  `TypeGuard<'T>` that unlocks a typed read is possible future work.)
- Generators/iterables → `JS.Iterable` / `seq<'T>` (Fable understands both); async
  iterables → `JS.AsyncIterable`. Promises → `JS.Promise<'T>` (leave `Async` adaptation to
  the consumer or an opt-in wrapper layer; don't bake it into bindings).

### 4.9 Generics

- Type parameters → F# type parameters, names preserved.
- Constraints → F# constraints only when expressible: constraint is a generated nominal
  interface → `'T :> IThing`. Structural constraints (`T extends { id: string }`) →
  hash-cons the constraint object type into a named interface, then `:>` it; or drop the
  constraint (Widened) with doc note. `T extends keyof U` → `typekeyof`/`keyof` idioms
  (§4.10). `T extends string` → measure-tagged `string<'T>`? — open; probably drop.
- **Default type arguments** (`interface Foo<T = string>`) — F# permits same-name types with
  different generic arity: emit `type Foo<'T> = ...` *and* `type Foo = Foo<string>` (an
  abbreviation per defaulted suffix). Ergonomic, cheap, and exactly how consumers expect it
  to read.
- Variance annotations (`in`/`out`) → no F# equivalent on interfaces; ignore (F# generics
  are invariant; consumers use `!!` or flexible types `#I` where they need it). Doc-noted.

### 4.10 `keyof`, indexed access, mapped types — the erased-idiom zone

This is where the archived `Xantham.Fable.Core` work pays off. Two regimes:

- **Closed/concrete**: `keyof KnownInterface` is a *finished union of string literals* from
  the checker (`getIndexTypeOfType`). Emit it like any literal union → StringEnum. Exact.
  Same for concrete indexed access `Config["port"]` → the checker hands us the resolved
  type; nothing special to emit.
- **Open/generic**: `K extends keyof T`, `T[K]` inside generic signatures cannot be closed.
  Map to the support package: `keyof<'T>`, `typekeyof<'T,'R>`, `proptypekey`/`proptypelock`
  for `T[keyof T]`, `PropertyRecord<'T,'V>` for `{ [K in keyof T]: V }`. Ergonomic and
  genuinely safe — this was the point of that library, and its README's mapping table
  (keyof / K-extends-keyof + T[K] / T[keyof T] / index signature) is adopted as-is.

Mapped types over concrete operands (`Partial<Options>`, `Pick<X,"a"|"b">`,
`Record<string, T>`):

- The checker expands them (`getApparentPropertiesOfType`) — emit the expansion, hash-consed,
  *named by the alias* when `AliasSymbol` says `Partial<Options>`. **Decided (D6):** simple
  operand-first synthesis — `Partial<Options>` → `OptionsPartial` — accepted for now;
  revisit if collisions or readability demand. (A verbatim generic `Partial<'T>` cannot be
  expressed structurally in F#, so some synthesis is unavoidable.)
- `Record<string, V>` / `{ [key: string]: V }` (string index signature) → interface with
  `[<EmitIndexer>]` `Item: string -> 'V` (the `PropertyRecord` shape generalized); also
  worth a `JS.Map`-free plain `Dictionary`-like read helper. `getIndexInfosOfType` supplies
  exact key/value types including `number` keys and `readonly` flags.
- Generic mapped types in generic positions (a library exporting its own `DeepPartial<T>`
  used at unresolved `T`) → cannot expand; emit an erased phantom
  (`[<Erase>] type DeepPartial<'T> = ...`) whose only operations are casts. Widened, loud.

### 4.11 Conditional and template-literal types

- Concrete instantiation site → the checker already resolved it; we never see it.
- In a *generic* signature (unresolved): the wire hands us `CheckType`/`ExtendsType`/true/
  false branches. Options: (a) emit the union of both branches (Widened, often acceptable),
  (b) emit `obj` (Escape), (c) emit an erased phantom carrying the name. Recommend (a) when
  both branches resolve to representable types, else (c). Config-gated; always doc-noted.
- Template literal types (`` `on${Capitalize<E>}` ``) → `string` (Widened) by default;
  measure-tagged `string<brand>` per named alias as opt-in. Closed template literals over
  finite unions expand to literal unions in the checker → StringEnum path applies.

### 4.12 Tuples

Fable F# tuples *are* JS arrays — a happy exact match:

- Fixed tuple `[A, B, C]` → F# tuple `A * B * C`. Exact. (Labels are cosmetic → doc comment.)
- Optional tail elements (`ElementFlags.Optional`, `FixedLength`) → **Decided (D7):**
  `option`-typed tail components (which produce an `undefined` slot rather than a shorter
  array) — simplicity first. If a real API rejects the `undefined` slot, patch that case
  with shorter-array overloads or an erased carrier. The behavioral test (what TS APIs
  actually accept) stays on the list as a watch item.
- Rest elements (`[A, ...B[]]`) → no F# tuple form; erased type + indexer/head accessors, or
  `obj[]` Escape. Recommend the erased carrier with typed accessors.
- `readonly` tuples → same mapping; readonly-ness doc-noted.

### 4.13 Modules, namespaces, exports, declaration merging

- ES module / `.d.ts` file → F# module; nested `namespace` → nested module.
- Import binding: default export → `[<ImportDefault>]`, `export =` → `[<ImportAll>]`/
  `[<ImportDefault>]` per module kind (`SourceFileMetadata.ImpliedNodeFormat` and
  `PackageJsonType` are on the wire — use them instead of guessing), named exports →
  `[<ImportMember>]`. Ambient/global declarations → `[<Global>]`.
- **Declaration merging** (interface+interface, class+namespace, function+namespace) — the
  checker has already merged: `getExportsOfModule`/`getMembersOfSymbol` on the *symbol* see
  the union. Generate from symbols, not from individual declaration nodes, and merging costs
  nothing. (This bit ts2fable hard; it falls out for free here.)
- Re-exports / barrel files → follow `getAliasedSymbol` to the origin, emit once at the
  origin, alias elsewhere (F# module abbreviations / type abbreviations).

### 4.14 Names and docs

- Identifier fidelity: F# casing conventions vs JS (`camelCase` members) — keep source names
  verbatim by default (bindings track upstream docs); `[<CompiledName>]`/backticks for
  non-identifier names; config for PascalCasing members if a consumer wants .NET style.
  Reserved F# keywords → backticked.
- Collisions from case-insensitive filesystem/module flattening and from hash-consed
  synthetic names → deterministic disambiguation (`Name2` is unacceptable; path-derived
  names per the archive's `NamePath` scheme).
- JSDoc → XML doc comments (`getDocumentationComment`, `getJsDocTags`); `@deprecated` →
  `[<Obsolete>]`; `@see`/`@example` carried into `<remarks>`/`<example>`. The tier
  annotation from §1 also lands here.

---

## 5. Generator policy layer

Decisions that are not per-construct but shape everything:

1. **Configurable strategy surface.** A per-package config (file next to the generation
   input) selecting: option vs voption, AllowNullLiteral default, union thresholds,
   ParamObject emission, literal-overload synthesis, casing. Defaults chosen for safety;
   everything above marked "open" becomes a config knob with a default.
2. **Fidelity report.** Every Widened/Escape emission is logged with source location and
   reason; the run emits a manifest. Silent drops are a bug by definition.
3. **Determinism.** Same input ⇒ byte-identical output (stable ordering from the wire's
   stable ids and declaration order), so bindings diffs track upstream diffs.
4. **Support package.** Revive `Xantham.Fable.Core` (erased keyof idioms, PropertyRecord,
   brand helpers, maybe erased-union constructor helpers) as the one runtime-less dependency
   of generated output.
5. **Escape hatch is part of the API.** Every generated interface is erased; document (once,
   in the package README template) that `!!`, `unbox`, and dynamic access are sanctioned
   when the types pinch — safety here is a default, not a prison.
6. **Target: Fable-only (D10).** Generated bindings assume the Fable compiler; no
   dual-audience compile-for-JS-and-read-from-.NET constraints on attribute choice.

---

## 6. Decisions (2026-09-01) and remaining threads

The original eleven open questions, resolved in review. Referenced from the catalogue as
D1–D11.

1. **D1 — decided.** `option` (not `voption`) repo-wide. Null/undefined collapse into
   `option` by default; generator config carries a per-symbol override list for APIs where
   the distinction is semantic (§4.3).
2. **D2 — decided.** `[<AllowNullLiteral>]` default **off**; config toggle (§4.4).
3. **D3 — decided.** `[<ParamObject>]` is also applied to inline type literals in parameter
   position, with F# parameter names matching the literal's member names. Anonymous records
   are not the default (§4.4).
4. **D4 — decided.** By position: overloads and erased-union constructors for parameters;
   tagged unions and typed test helpers for returns. `U_n` threshold stays at 4 (§4.5).
5. **D5 — decided.** Delegates by default. Curried lambdas work well in Fable 5, so a
   curried-emission config toggle may come later, but delegates are the simple
   guaranteed-arity default (§4.8).
6. **D6 — decided.** Simple synthesis for checker-expanded aliases: `Partial<Options>` →
   `OptionsPartial`. Revisit if collisions or readability demand (§4.10).
7. **D7 — decided.** Optional tuple tails are `option`-typed components (`undefined` slot),
   simplicity first; patch to shorter-array overloads or erased carriers where a real API
   rejects the slot (§4.12). *Watch item:* the behavioral test of what TS APIs accept.
8. **D8 — decided.** `unknown` → `obj` for now; an erased `Unknown` wrapper becomes a
   config toggle only if it stays cheap to implement (§4.1).
9. **D9 — open by design.** Utility-type depth (type-fest grade) cannot be settled on
   paper: chase what we can, run the fixtures, and tune the phantom-erasure cutoff from the
   results.
10. **D10 — decided.** Generated bindings target **Fable-only**.
11. **D11 — decided.** Protected members, unique-symbol brands, and type predicates are
    dropped (with doc notes) now; revisit after the generator exists (§4.4, §4.1, §4.8).

Threads still live after these decisions:

- Mixed literal unions (string + number literals in one union): erased type with static
  members vs `U2` split — not yet decided (§4.2).
- D7's runtime acceptance test for `undefined` tuple slots.
- D9's calibration against the `type-fest`/`three`/`solid-js`/`typescript` fixtures.
- The D5 and D8 config toggles, if wanted later.

---

## 7. What we deliberately reuse vs. discard from the archive

**Reuse (as design, not code):** the `Xantham.Fable.Core` erased idiom library (near
verbatim); the union categorization algorithm (`ResolvedType.Categorization.fs`) as the
shape of §4.2/§4.5's classifier; the `NamePath`/synthetic-naming scheme; the litmus-test
methodology (drive the whole of `three`/`solid-js`/`typescript`/`type-fest` through and
demand zero silent drops).

**Discard:** the JSON IR and everything that existed to serve it (`Common.Types.fs`,
Decoder) — the Wire's checker access is the IR now; the syntax-driven type resolver — the
checker replaces it; the Fable-compiled extractor — Wire is .NET end to end.
