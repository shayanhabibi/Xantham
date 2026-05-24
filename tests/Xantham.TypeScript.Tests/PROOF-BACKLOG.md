# Proof Backlog — Xantham.TypeScript × Xantham.Fable

> Status: **proposals**. These are stress/invariant proofs worth adding to `Program.fs`. None are implemented yet.
> The catalogue of *implemented* proofs lives in [`src/Xantham.TypeScript/README.md`](../../src/Xantham.TypeScript/README.md)
> (`SF-*`, `XTK-*`, `ND-*`, `TC-*`, `SY-*`, `TR-*`, `NW-*`).

## Why this document exists

`Xantham.TypeScript` is a **typed F# façade over the TypeScript compiler API, compiled to JavaScript by Fable**.
That double nature is the source of every risk worth proving:

1. **The TypeScript side** — the compiler's runtime objects must have the shape the wrappers assume (a node of a
   given `SyntaxKind` really does carry the fields the matching DU case unwraps; a symbol really resolves; a
   `package.json` really exists up the tree). The existing `SF/XTK/ND/TC/SY/TR/NW` proofs cover much of this.
2. **The Fable side** — F# constructs only *behave* as written once Fable lowers them to JS. `unbox` is a no-op,
   so a mis-tagged node is never caught until a field read returns `undefined`; flag enums must compile to numbers
   that still answer `HasFlag` by bitwise math; single-payload DUs must keep their `fields[0]` layout; `option`
   must keep tracking JS `undefined`/`null` the way the source assumes.

The proofs below target the **gaps** — places where the wrappers do something the F# type system cannot make safe
*and* which the current corp us does not yet exercise. Each entry states, concretely, **what the test does** and
**what it proves**, plus why it matters to Xantham.Fable's operation (parse `.d.ts` → classify nodes → emit the
3-tier representation).

### Current state & how these were derived

`Xantham.TypeScript` and `Xantham.Fable` are **not wired together yet** — the bindings in `Xantham.TypeScript`
were extracted from the engine and have no consumer in this branch. So the proofs here are not "tests of the
integration"; they are **obligations the extracted bindings must keep** so that the engine stays sound *when it is
reconnected*. They were derived by reading how `Xantham.Fable` actually drives the compiler today:

- **`Read.fs` / `Reading/Entry.fs`** — `read` runs `SeedResolvedModules → CompilePackageCache → SeedExportPoints →
  getAndPrepareExports → runReader → assembleResults`. `getDeclarations` resolves a module file's symbol, calls
  `getExportsOfModule`, then walks **every** declaration of each export symbol (to catch value+type merges like
  `declare const Node` + `type Node`), and `failwith`s if the module file has no symbol. Ambient files instead
  filter `ts.isDeclarationStatement`.
- **`Reading/Dispatcher.fs`** — dispatch is a total `match` on `XanTagKind`; `Ignore` is a silent no-op, so any
  node the classifier mis-buckets to `Ignore` is *dropped from the output*, not error-flagged.
- **`.claude/rules/fable.md`** — records the load-bearing runtime contracts: the `node[TRACER_TAG]` wrapper shape,
  stack traversal (no TCO in JS), the shared-symbol hazard in `resolveTypeBase`, `IdentityKey` priority
  (`DeclarationPosition > Symbol > AliasSymbol > Id`), and `TypeNodeKind` (syntactic) ≠ `TypeKind` (semantic).
- **`TypeScript.fs` is ts2fable-generated and must not be edited** — every behavioural assumption about the
  compiler API therefore has to be pinned by a proof, not a type signature.

Priority key: **P0** = guards a path that throws/silently corrupts today with no proof; **P1** = strengthens a
known-load-bearing assumption; **P2** = regression net / nice-to-have.

---

## A · Lookup-map ↔ DU structural proofs (`LM-*`)

The classifier is a set of `Dictionary<Ts.SyntaxKind, obj -> 'DU>` maps in `XanTagKind.fs`'s `Internal` module
(`declarationFileNodes`, `typeNodeKindSetMap`, `memberDeclarationKindSetMap`, `typeDeclarationKindSetMap`,
`topLevelStatements`, `topLevelExportDeclarations`, `topLevelLocalDeclarations`, `moduleExportSetMap`,
`modifierSetMap`, `literalTokenNodeKindSet`, `jsDocKindSetMap`). These proofs are **corpus-independent** — they
hold by construction or they don't — so they fail fast and pinpoint a drift the moment a DU case is added without
its map row (or vice versa).

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| LM-1 | For each map, enumerate the DU's cases via reflection (or a hand-mirrored list) and assert every case name appears as a value-constructor in the map. | No DU case is unreachable — every variant the model exposes can actually be produced by `Create`. Without this, a case added to e.g. `TypeNode` but forgotten in `typeNodeKindSetMap` is dead, and the node it should match falls through to `Ignore`. |
| LM-2 | Assert each map's keys are **distinct** (no `SyntaxKind` mapped twice). | A `SyntaxKind` resolves to exactly one DU case; rules out a copy-paste double-entry silently shadowing one constructor. |
| LM-3 | Assert the key sets of `topLevelExportDeclarations` and `topLevelLocalDeclarations` are supersets of what `Source.create` actually feeds them (cross-check against XTK-8/XTK-9 input). | The export/local declaration maps stay aligned with the symbol-table walk that drives them; catches a `SyntaxKind` that the walk emits but the map omits before it reaches a user. |
| LM-4 | For maps that share a domain (`declarationFileNodes` should be a superset of every other node map's keys), assert containment. | `DeclarationFileNodes` remains the single widest node vocabulary; if a kind is classifiable as a `TypeNode`/`MemberDeclaration` but not present in `declarationFileNodes`, ND-8 would pass while a downstream consumer keyed on the master set breaks. |
| LM-5 | Apply each map's value-constructor to a node of its key kind drawn from the corpus and assert the result is the matching DU case (not just non-throwing). | The `unbox >> Case` wiring in `(>=>)` actually targets the intended case — a transposed pair (`StringKeyword >=> NumberKeyword`) compiles fine and only this catches it. |

---

## B · Type-flag resolution totality (`TF-*`)  — **P0**

Unlike the `SyntaxKind` maps (which use a `ContainsKey`/`Ignore` fall-through), the type-flag tables in
`XanTagKind.fs` (`typeFlagPrimaryKindSet`, `typeFlagLiteralKindSet`, `typeFlagObjectKindSet`, ~lines 344–411)
resolve a `Ts.Type` by **`Array.find`** over a list of `(flag predicate, constructor)` rows. `Array.find`
**throws** when no row matches — there is no `Ignore` safety net. Xantham.Fable's `Dispatcher.dispatch` routes
every checked type through this path (the `Type`/`TypeFlagPrimary` arm), so a single corpus type whose
`TypeFlags`/`ObjectFlags` combination is unlisted aborts the whole read. These proofs pin the tables as **total**
over the types the engine can actually reach.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| TF-1 | Walk every export/declaration the engine would feed the checker (mirror `getDeclarations`), call `getTypeAtLocation`, and assert `typeFlagPrimaryKindSet` finds a matching row for **every** resolved type — no `Array.find` throw. | The primary type-flag table is total over the corpus; the `Type` arm of `dispatch` cannot abort the read on an unclassifiable `TypeFlags`. |
| TF-2 | For every type that carries `TypeFlags.Object`, assert `typeFlagObjectKindSet` finds a row for its `ObjectFlags`. | The object sub-classifier covers every `ObjectFlags` combo real `.d.ts` types produce (anonymous, interface, reference, mapped, instantiated…), so object-type dispatch never throws. |
| TF-3 | For every type that carries a literal flag (`StringLiteral`/`NumberLiteral`/`BigIntLiteral`/`BooleanLiteral`/etc.), assert `typeFlagLiteralKindSet` finds a row. | The literal sub-classifier is total over the literal `TypeFlags` the corpus yields; rules out a freshly-added literal flag (e.g. a TS-version bump) silently throwing. |
| TF-4 | Assert the flag predicates are **mutually exclusive** for every corpus type — exactly one row matches each type (count matches, not just ≥1). | `Array.find` returning the *first* match is unambiguous: no type is classified by accident of row order. A type matching two rows would mean the table's meaning depends on ordering, which is fragile under edits. |
| TF-5 | Cross-check that union/intersection types (`TypeFlags.Union`/`Intersection`) resolve to the primary row the engine expects, then assert each constituent (`type.types`) itself resolves through TF-1. | The recursive descent the engine does into union members stays total — a union whose *member* is unclassifiable throws just as hard as a top-level one, and only descending catches it. |

---

## C · Classification totality over the engine's node feeders (`CT-*`)

`XTK-2`/`XTK-6` already prove `XanTagKind.Create` never reaches `Ignore` for *exports* and *symbol-table value
declarations*. But Xantham.Fable does not feed the classifier from one source: `getDeclarations` walks **every**
declaration of **every** export symbol (to catch value+type merges such as `declare const X` + `type X`),
`expandDeclarations` re-resolves Interface/Function symbols, and ambient files are filtered by
`ts.isDeclarationStatement` instead. Each feeder can hand the classifier a node shape the export-only proofs never
saw. Because `Dispatcher.dispatch` treats `Ignore` as a **silent drop**, a mis-bucket here is invisible — the node
just vanishes from `output.json`. These proofs assert totality over *each distinct feeder*.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| CT-1 | Mirror `getDeclarations` exactly (module file → `getExportsOfModule` → for each export symbol, every `symbol.declarations` entry) and assert each declaration classifies to a non-`Ignore` `XanTagKind`. | The full per-declaration walk (not just the first declaration per symbol) is covered — a value+type merge whose *second* declaration is unclassifiable would be silently dropped without this. |
| CT-2 | Mirror the ambient-file path (`sourceFile.statements |> filter ts.isDeclarationStatement`) over script/ambient fixtures and assert every surviving statement classifies non-`Ignore`. | The ambient branch the engine uses for non-module files is total; catches a declaration-statement kind that only appears in script files (e.g. `export =` / `import =` forms) reaching `Ignore`. |
| CT-3 | Mirror `expandDeclarations` (resolve the symbol behind each Interface/FunctionDeclaration, enumerate *its* declarations) and assert each classifies. | The second-pass expansion the engine does for merged interfaces/overload sets stays total — an overload signature kind missing from the member maps would be dropped here. |
| CT-4 | For every node any feeder classifies, assert `Create` returns the **same** `XanTagKind` case the corresponding lookup map predicts (`IsTopLevelStatementKind`/`IsMemberDeclarationKind`/…). | The fast `ContainsKey` predicates the engine uses for pre-filtering agree with the full `Create` classification — a kind the predicate accepts but `Create` buckets to `Ignore` (or vice versa) would desync filter from dispatch. |
| CT-5 | Assert no corpus node reachable from any feeder classifies to `Ignore`; if one does, fail with its `SyntaxKind` name and source location. | Directly guards the silent-drop hazard in `Dispatcher.dispatch`: every node the engine visits is either deliberately ignorable or surfaced. The failure message names exactly what would have vanished. |

---

## D · Identity & key stability (`ID-*`)

`assembleResults` groups every read node by **`TypeKey`** and sorts duplicates by **`IdentityKey`** priority
(`DeclarationPosition (0) > Symbol (1) > AliasSymbol (2) > Id (3)`; `duplicates[0]` is the winner). Dedup, caching
(`signalCache`), and winner-selection are all keyed on these being **stable and collision-free** within a single
read. `getNodeId`/`getSymbolId` are mutable counters the compiler assigns lazily; `DeclarationPosition` is
`(fileName, pos, end)`. These proofs pin the keys' determinism and disjointness.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| ID-1 | For every node, call `getNodeId` twice and assert equality; assert the id is positive (extends SY-4 to *stability*, not just presence). | Node identity is stable across reads within one program — the keying `assembleResults` relies on does not drift mid-pipeline as the compiler lazily assigns ids. |
| ID-2 | For every symbol, call `getSymbolId` twice; assert equal and positive; for **merged** symbols (same symbol reached via two export paths) assert the id is identical. | A value+type merge resolves to *one* symbol id, so the engine's symbol-keyed dedup collapses the merge into a single group instead of emitting it twice. |
| ID-3 | Build `DeclarationPosition` (`fileName`, `pos`, `end`) for every declaration and assert it is unique across the corpus — no two distinct declarations share a triple. | `DeclarationPosition`, the **highest-priority** identity key, never collides, so winner-selection in a duplicate group is deterministic and not decided by sort tie-break. |
| ID-4 | For each duplicate group the engine would form, assert sorting by `IdentityKey` priority yields a **unique** `duplicates[0]` (no two entries tie at the top priority). | "`duplicates[0]` is always the winner" is well-defined — there is a single highest-priority entry, so `selectAndMergeWinnersInDuplicates` promotes a deterministic winner. |
| ID-5 | Compute `TypeKey` for every resolved type twice (and for two `Ts.Type` objects sharing a symbol, e.g. `Array<string>` vs `Array<T>`) and assert determinism and that the shared-symbol pair gets **distinct** `TypeKey`s. | `TypeKey` discriminates the shared-symbol hazard from `fable.md` — caching keyed on `TypeKey` will not conflate `Array<string>` with `Array<T>` even though they share an underlying symbol. |

---

## E · Type-resolution / semantic invariants (`RS-*`)

The wrappers repeatedly cross from **syntax** (a `TypeNode`'s `SyntaxKind`) into **semantics** (the `Ts.Type` the
checker resolves it to). `fable.md` records the load-bearing gap: `TypeNodeKind` (syntactic) ≠ `TypeKind`
(semantic) — `Foo<'A>` is a `TypeReference` syntactically but may resolve to a `TypeLiteral` after checking. The
engine's `resolveTypeBase` also leans on `getTypeAtLocation` being **total** and on the shared-symbol guard. These
proofs pin the semantic answers the engine consumes.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| RS-1 | For every `TypeNode` in the corpus, call `getTypeAtLocation` and assert it returns a type (never `undefined`/throw). | The syntax→type crossing the engine does everywhere is total; no `TypeNode` shape leaves `resolveTypeBase` without a type to key on. |
| RS-2 | Find corpus `TypeReferenceNode`s and assert at least some resolve to a `TypeKind` that is **not** `TypeReference` (e.g. resolve to an object/literal/union), demonstrating the syntactic≠semantic divergence is real and handled. | The wrappers must not assume `TypeReference` syntax ⇒ reference semantics; this exhibits the divergence `fable.md` warns about on real input so a wrapper that conflates them is caught. |
| RS-3 | Enumerate pairs of distinct `Ts.Type` objects that share an underlying symbol (generic instantiations vs their generic), and assert `resolveTypeBase`'s guard condition (`guard <> xanTag.Guard` and `signalCache.ContainsKey(guard.Value)`) distinguishes them. | The shared-symbol hazard is exercised with concrete corpus types, proving the cache guard does not treat an instantiation as a cached entry for its generic (the permanent-stall bug `fable.md` documents). |
| RS-4 | For every symbol reached via an **alias** (`export { X as Y }`, re-exports), assert `getAliasedSymbol`/alias resolution yields a symbol and that its `IdentityKey` is `AliasSymbol`-priority, distinct from a direct `Symbol`. | Alias resolution — priority 2 in the identity ordering — produces a usable symbol, so re-exported declarations are keyed correctly rather than collapsing onto the direct export. |
| RS-5 | For `readonly` `TypeOperator` nodes, assert the resolved type is **referentially identical** to the inner `.type`'s resolved type (TC-8 strengthened to reference equality); for `keyof`/`unique`, assert non-identity. | `readonly` transparency holds at the *object-reference* level Fable compares, so the engine can pass a `readonly T` straight through without re-resolving, while `keyof`/`unique` are correctly treated as new types. |

---

## F · Module / package resolution (`MR-*`)

The read pipeline begins with `SeedResolvedModules → CompilePackageCache → SeedExportPoints`. Several of these
steps call into wrappers that `failwith` or `.Value` on the assumption that module resolution succeeded:
`getDeclarations` `failwith`s when a module file has no symbol; `ExternalModule.create` asserts a non-empty module
specifier; `Source.create` reads `packageJsonFields.Value` and a `version`. SF-2..11 prove these over the *whole
corpus as a program*; these proofs target the **resolution mechanics the engine drives directly**.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| MR-1 | For every module source file the engine would seed, assert `getSymbolAtLocation(sourceFile)` is `Some` (the exact condition `getDeclarations` `failwith`s on). | The `failwith "…no known declarations"` branch is unreachable for real module files, so `getDeclarations` never aborts the seed. |
| MR-2 | For every module symbol, assert `getExportsOfModule` returns an array (possibly empty is allowed) and that re-invoking it yields the same membership. | The export enumeration the reader iterates is well-defined and stable; the per-export declaration walk has a deterministic input set. |
| MR-3 | For every external module, assert `GetModuleSpecifier` yields ≥1 specifier (backs `ExternalModule.create`'s `NonEmptyArray` invariant) and that the specifier string is deterministic across calls. | Module-specifier keying in the package cache is stable and the `NonEmptyArray` constructor never throws on real modules. |
| MR-4 | For every source file, walk `GetClosestAncestorPackageJson` and assert it terminates (never loops past `node_modules`) and resolves a `package.json` with a `name` and `version` for non-default-lib files. | `CompilePackageCache` and `Source.create`'s `.Value` reads on package fields are sound; the ancestor walk has the `node_modules` stop the engine assumes and cannot spin. |
| MR-5 | For two source files in the **same** package, assert they resolve the **same** `package.json` object/identity. | The package cache keys collapse co-packaged files correctly — versions/names are not duplicated per-file, matching how `CompilePackageCache` memoises. |

---

## G · Fable runtime-semantics proofs (`FB-*`)

These hold (or fail) only *after Fable lowers F# to JavaScript* — the F# type system cannot make them safe, and
`TypeScript.fs` being ts2fable-generated means none can be pinned by a signature. They must run **in the Fable/JS
build** (via `npm run test:typescript`), not as a pure .NET unit test, because that is where the lowering they
assert actually happens.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| FB-1 | Read the numeric value of representative `Ts.SyntaxKind`/`TypeFlags`/`ObjectFlags` enum members in the Fable build and assert they equal the TypeScript compiler's runtime numbers. | The F# enum mirrors compile to the *same* integers the live `typescript` module emits — a lookup map keyed on `SyntaxKind.ClassDeclaration` actually matches the compiler's node. |
| FB-2 | Combine two flag values, then assert `HasFlag` answers by **bitwise** math in JS (e.g. `(Object ||| Anonymous).HasFlag Anonymous = true`, and a non-set flag is `false`). | Flag-enum `HasFlag`, used throughout the type-flag tables, lowers to bitwise-and in JS rather than reference/structural comparison — the predicates in TF-* mean what they read. |
| FB-3 | Construct a single-payload DU case (e.g. `XanTagKind.Type t`) and assert reading `.Value` returns the payload, confirming the `fields[0]` layout the accessors assume. | The DU `.Value` accessors (`XanTagKind.Value`, `TypeNode.Value`, …) read the right slot post-lowering — the invariant the README flags as Fable-internal rather than corpus-proven. |
| FB-4 | Round-trip `option` against JS `undefined` and `null`: assert `Some null`/`Some undefined` vs `None` behave as the wrappers expect when reading optional compiler fields (`symbol.valueDeclaration`, package.json fields). | `option`-typed reads still track JS abs/null correctly after lowering, so `.Value`/`Option.get` on optional compiler fields don't surface `undefined` as a spurious `Some`. |
| FB-5 | Take a node of one kind, `unbox` it to the wrong wrapper, and assert the mis-tag is **not** caught at the `unbox` site but **is** caught at first field read (returns `undefined`). | Documents the `unbox` no-op hazard concretely: it proves *why* LM-5/CT-4 matter — a transposed map row cannot be caught by type-checking, only by a field-shape proof. |
| FB-6 | Store a `XanTagKind` wrapper under the `TRACER_TAG` symbol key on a node, read it back, and assert the round-trip yields `{ Value: XanTagKind, [TRACER_PROXY]: "XanTagKind" }` (the shape `fable.md` pins). | Symbol-keyed property storage on compiler node objects survives Fable lowering, so the tracer tag the engine writes is the tracer tag it reads — not shadowed or coerced. |
| FB-7 | Resolve the same node/type twice and assert the wrapper returns reference-equal objects where the engine's caching assumes identity (e.g. `signalCache`/builder reuse keyed by reference). | Reference-equality stability the reactive `Signal`/`Builder` cache depends on holds in JS — re-resolving does not mint a new object that defeats the cache and re-runs builders. |

---

## H · Pipeline-level / no-throw stress (`PL-*`)

The categories above isolate one obligation each. These proofs run the **whole sequence** the engine runs — a
harness that mirrors `getDeclarations → classify → resolve type-flags → key → group` over the entire corpus —
and assert it completes without throwing or silently losing nodes. They are the integration net that catches an
interaction the unit-level proofs each pass individually.

| ID | What the test does | What it proves |
|----|--------------------|----------------|
| PL-1 | Drive every fixture through a harness that mirrors `getDeclarations` + `XanTagKind.Create` + type-flag resolution (TF-*) end to end, asserting **no exception** escapes for any node/type. | The composed read path is total over the corpus — the `failwith`/`Array.find`/`.Value` sites that each proof guards in isolation also never fire *in combination*. |
| PL-2 | During the PL-1 walk, count nodes classified to `Ignore` and assert the count equals the deliberately-ignorable set (with each unexpected `Ignore` reported by kind + location). | No node is silently dropped by `Dispatcher.dispatch` — the integration-level restatement of CT-5, catching drops that only appear when feeders interact. |
| PL-3 | Build the type-reference graph the engine traverses and assert `healthCheckType` (TypeKey-based circular-ref detection) terminates and flags only genuine cycles — run with the corpus's deepest generics (`type-fest`, `@types/three`). | Stack traversal (no TCO in JS) stays bounded on real deep/recursive types; the circular-ref guard does not false-positive on legitimate deep nesting nor miss an actual cycle. |
| PL-4 | Extract JSDoc comments/tags for every node carrying them (`jsDocKindSetMap` path) across the corpus and assert extraction is total and non-throwing. | The JSDoc arm of dispatch handles every JSDoc kind the corpus contains — comment/tag emission into the 3-tier representation never aborts on an unlisted JSDoc node. |
| PL-5 | Assert the harness output (grouped-by-`TypeKey`, winner-per-duplicate) is **deterministic** across two full runs over the same corpus — same groups, same winners, same order. | The full dedup/merge/winner-selection pipeline is reproducible, so `output.json` is stable run-to-run — a prerequisite for diffing engine output as a regression signal. |

---

## Suggested implementation order

1. **B (TF-*)** and **C (CT-*)** — these guard paths that throw / silently corrupt **today** with no proof (P0).
2. **F (MR-*)** and **D (ID-*)** — strengthen the resolution + keying assumptions the pipeline is built on (P1).
3. **G (FB-*)** — the Fable-lowering proofs; cheap to write, and they explain *why* A/C must exist (P1).
4. **E (RS-*)** — semantic-divergence and shared-symbol proofs (P1/P2).
5. **A (LM-*)** and **H (PL-*)** — structural nets and the full-pipeline stress that catch regressions once the
   above are green (P2).
