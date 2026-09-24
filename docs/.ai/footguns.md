# Footguns and carried-forward threads

Durable facts harvested from `docs/.ai/handovers/` before that directory was retired on
2026-09-22. The handovers were lane bookkeeping for landed work; what survives here is the part
that constrains future changes, plus the threads that were still open when their lane closed.

Log paths under `/tmp` and artefact paths in the sibling CloudEdge repository are not
reproducible from this checkout and were dropped rather than carried.

## Compiler API

**`getTypeAtLocation` is wrong for class heritage.** It adds an internal trailing `this`
argument, so `Model<string>` reads as two type arguments. Read heritage nodes and ask
`getTypeFromTypeNode` for each contract. `TypeFacts.ImplementedTypes` records explicit
`implements` contracts separately from `BaseTypes`.

**Checker alias-symbol identity alone does not name a type.** A transformed type retains the
symbol of the alias it came from, so the checker-declared type must equal the type being named
before a source name is recovered. `NonNullable<Value>` matched the members of the nullable
`Value` and reintroduced `null` on the strength of the shared symbol.

## Declaration catalogs

Each rule below was written against a catalog authentication failure between two real packages;
the corresponding lab under `tests/fixtures/` is the reducer.

- **Source ownership reads the canonical map from before parent-role inference.** TypeScript
  shares one anonymous type between properties of different packages, so using the completed map
  during source traversal lets a consumer's property contribute its source file to a shared
  interface, and the producer's hash stops matching.
- **The source closure follows the sources selected for canonical identities**, not the alias
  files of every occurrence. Two independent packages that both write `Record<string, unknown>`
  share a canonical identity and must not authenticate different alias files.
- **Named literal aliases keep independent identities at equal values.** Collapsing them by value
  was tried and reverted: named aliases lost their declaration handles, private same-valued
  aliases became one F# type, and alias declaration files vanished from source closures. Nine
  existing tests rejected the behaviour even though the new one passed.
- **An interface may not inherit a base the run emits as an abstract class** — FS0887. Class
  shaping removes inheritance edges whose targets the accepted entrypoint map emits as classes,
  before inherited-member deduplication so the members stay available. `SI006` records the
  omitted nominal relation.
- **A catalog-mode regression needs a catalog-enabled producer**, not only an ordinary golden.
  Catalog mode recovers compiler-library alias names that ordinary mode nests anonymously, so the
  two modes reach different declarations from the same input.
- **A diagnostic that fails deliberately before output is not acceptance evidence.** Several lanes
  recorded a probe reaching a *later* mismatch as proof of the earlier fix; that establishes the
  earlier guard passes and nothing about the run as a whole.

## Generator passes

**Harvest cannot distinguish a declaring path from an alias.** Same `SymbolId`, same `Order`, and
ambient modules are harvested alphabetically. "The declaring module wins" is unimplementable;
`Shape/Classes.fs` uses the class's own binding specifier instead, falling back to the first
harvested occurrence, and reports `SC010` per dropped member.

**`drop-orphan-delegates` has a failure mode on each side.** The first version retained nothing
and pushed `AC001` from 8 to 22; the second retained anything whose name fuzzily matched an export
and dropped nothing corpus-wide. The shipped rule checks representation the way `audit-coverage`
does: a candidate drops only where a harvested export stays represented without it.

**Exactly one pass may declare a callable name.** `hasIncompatibleOverloadedTypeParameters`
(`Shape/Spec.fs`) is read with opposite signs by `declaresInterface` (`Shape/Interfaces.fs`,
admits) and `shapeCallbacks` (`Shape/Callbacks.fs`, skips). Changing either sign alone mints a
duplicate or an unnamed type.

**A return position widens on a concrete application, not on a type parameter.** Applying a
declaration's own free parameter keeps the name; `Coalesce<string>` renders `obj -> obj` under
`TR031`/`TR013` where `Coalesce<'T>` does not.

**A pass that rewrites exports must run after `order-declarations`.** Exports are not an
`FsExports` decl until `order-declarations` builds one from `model.ExportMembers`. A pass sitting
earlier and mapping over `model.Decls` looking for `FsExports` matches nothing, silently produces
no output, and has no container name to key its findings on. `expand-union-arms` cost an
afternoon to this.

**A finding needs a row in `FindingCodes.table`, not just a registered union.** Registering the
kind in `FindingCatalogue` is enough to compile and enough for the pass to run; the missing code
only throws at manifest render, from `Finding.get_Key`, which reads as a render bug rather than a
missing table row.

## Fable runtime type tests

Measured against Fable 5.13.0 on 2026-09-22 while designing erased-DU mapping for mixed
literal/typed unions. These constrain any pass that puts a payload arm on an `[<Erase>]` union.
Full table and provenance: `docs/.ai/plans/2026-09-22-mixed-literal-unions.md`.

**Two arms that share a runtime test are a silent failure.** `float[]` and `(float * float)`
both reach `isArrayLike` (`Fable2Babel.fs:491-492` sends `Fable.Array _ | Fable.Tuple _` to one
test); `System.Func<…>` and `(float -> float)` both reach `typeof x === "function"` (`:489-490`).
The second arm compiles to a dead branch and **emits no diagnostic at all**. An arm with no
usable test — `U_n`, a nested erased union, a plain interface, a type parameter — does warn
(`Cannot type test`) and is greppable. Verifying only the warnings misses the first failure mode
entirely; it needs a runtime round-trip.

**A `[<StringEnum>]` arm makes the literal cases unreachable.** It tests `typeof x === "string"`
and so does every fieldless literal case, so the arm swallows them. An F# `enum` arm tests
`typeof x === "number"` and collides with a `float` arm the same way.

Distinctness must therefore be checked over *emitted test class*, not over `FsTypeRef` — which
is what `erasedUnionRef`'s `List.distinct` (`Shape/Spec.fs:2321`) does, and it is not sufficient
for this purpose.

**A lambda coerces to a delegate at method-argument position only.** A union-case constructor
gets no coercion: a bare lambda into a `System.Func<…>` arm is FS0002, and a curried
`(float -> float -> string)` arm accepts the lambda but emits curried JS of the wrong arity. The
same arm carried by a method parameter takes a bare lambda and emits `(a, b) => …`.

## Fable consumption of bindings

**`Xantham.Fable.Core.TS` reaches Fable as a DLL, never as source.** Fable merges a NuGet
package's sources only when the package ships `fable/*.fsproj`, and this package ships `lib/`
alone. The run gate reproduces that with `--exclude Xantham.Fable.Core.TS`, which also cut its
Fable compile from 22.7s to 6.3s (Fable 5.0.0, 2026-09-24). From the DLL, Fable resolves only
attributes (`Emit`, `Import`, `Global`, `Erase`, `ParamObject`, `CompiledValue`) and interfaces.
Every member of the bindings, generated or hand-written, must therefore be an attribute-carrying
member or an abstract one. An `inline` member or a member with a body fails at the consumer's
call site:

- under a `Fable.Core.*` namespace, as `… is not supported, try updating fable tool`;
- elsewhere, as `Cannot find the body of inline member`.

The run gate catches this only for members it calls, so a new hand-written member needs a check
in `tests/Xantham.Generator.RunGate/Program.fs` (`bindingExtensions`).

`Xantham.Fable.Core` is the exception: its `KeyOf`/`TypeKeyOf` helpers are `inline`, so it is
compiled from source, and excluding it fails with `Cannot find inline member`. Its package ships
its sources under `fable/` for the same reason; a package without them fails the same way at a
consumer's first helper call.

## Build and test environment

**Nested `dotnet build` inside a test stalls under `dotnet test`.** Idle MSBuild nodes hold the
test host's stdout, reproduced three times in one lane. `build.fsx` runs the Expecto executables
directly, and nested builds carry a `global.json` in a temp directory outside the repository so a
newer SDK stays eligible.

**`tools/workspace.fsx` exports `XANTHAM_TSGO_EXE` from any checkout**, not only a worktree, so
the catalog suites run in the main checkout.

## Open threads

Carried from lanes that closed without finishing these.

- **Mixed literal/typed unions: measured and declined.** `plans/2026-09-22-mixed-literal-unions.md`
  Step 0 found 18 such unions in the corpus and **0** emittable as erased DUs — two thirds are
  `"a" | "b" | string`, whose bare `string` arm swallows the literal cases. `U_n` is the right
  mapping here. Re-run the counting pass before reopening; do not re-argue the design.
- **Union arm overloads remain open and unimplemented**: `plans/2026-09-22-union-arm-overloads.md`.
  Ships disabled. Its corpus counts are now final (the mixed-union pass claims nothing).

- **`objectRef` ordering.** `Shape/Spec.fs`'s pure-callback branch sits ahead of its
  named-instantiation lookup, so `type StoreReturn<'T> = 'T * Action<obj, …>` while
  `createStore<'T>` returns `SetStoreFunction<'T>` from the same declaration. The candidate fix
  orders the named-instantiation lookup first. It reaches every reference to a pure callable in
  the corpus (`TR031` stood at 36), so it needs its own lab and a before/after `findings`
  measurement per `.claude/rules/generator-fixtures.md`.
- **`Exports` has no stable declaration parent role.** 22 of 38 catalog cases errored on this
  across the declaration-catalog, callable-signature, source-closure and anonymous-literal-union
  lists. Four cases stay disabled behind a TODO.
- **Entrypoint subclassability.** Generated `Container`/`ContainerProxy` are not F# subclassable;
  direct subclassing goes through the ambient runtime base. The ordinary SDK class
  interface-and-constructor policy was retained rather than revisited.
