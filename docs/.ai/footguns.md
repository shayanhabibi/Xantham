# Footguns and carried-forward threads

## Dependent application constraints (2026-10-26)

Shape generic bounds under the target declaration's binders, then substitute the supplied
argument references. Reading `T extends Internals<O, I>` directly in the caller's scope
turns out-of-scope `O` and `I` into `obj` and can emit an application that violates its own
constraint. Comparison binder names must avoid all existing `TypeVars`, including names
introduced by nested comparison scopes. Forward-dependent arguments are corrected first.

Compare complete applied nominal bases for dependent constraints; generic head equality
does not establish F# subtyping. Validate cyclic bounds after correction. `Self<any>` for
`T extends Box<T>` cannot be repaired as `Self<Box<obj>>`: the corrected argument would need
to inherit `Box<Box<obj>>`. `TR074` widens the entire unproven cyclic application. Valid
nominal self and mutual recursion retain their typed applications. Avoid a fixpoint loop
that grows a fresh nested generic application on each iteration.

Durable facts harvested from `docs/.ai/handovers/` before that directory was retired on
2026-09-22. The handovers were lane bookkeeping for landed work; what survives here is the part
that constrains future changes, plus the threads that were still open when their lane closed.

Log paths under `/tmp` and artefact paths in the sibling CloudEdge repository are not
reproducible from this checkout and were dropped rather than carried.

## Compiler API

**A global namespace can have a parent.** At TypeScript 7.1, a namespace inside `declare global`
is parented by the augmentation's `__global` symbol. A parentless-symbol test misses it and can
mistake module locals or an `export =` body for globals. `Ast.moduleAugmentations` identifies
augmentation **name nodes**, not module declarations: select the identifier `global`, then read
its parent module and body. The module's keyword is `ModuleKeyword`, not `GlobalKeyword`.

**An export alias has its own namespace.** `getAliasedSymbol` returns the target's parent.
Keep the harvested occurrence's `DeclaringNamespace` separately: `Pool.Depth = Pool.Inner.Deep`
declares `Depth` in `Pool` without changing the `Deep` symbol's identity or defining namespace.

**A symbol id alone does not identify a symbol.** At TypeScript 7.1 a symbol is a reference. A
file-owned one, which a declaration's symbol, a module's and its exports are, carries its source
file and an id unique within that file. A snapshot-owned one, a transient or merged symbol, carries
a project and an id unique within the snapshot, offset by 2^48. Key by `SymbolKey` (`SymbolId`,
owning file node id and `int64` id), and send a symbol back as its `SymbolResponse.Reference`; a
compact reference (`TypeResponse.Symbol`, `SymbolResponse.Parent`) names the file by node id only and
cannot be sent. A file-owned symbol names no project, so `Session.ForSymbol` leaves the session's.

**`getTypeAtLocation` is wrong for class heritage.** It adds an internal trailing `this`
argument, so `Model<string>` reads as two type arguments. Read heritage nodes and ask
`getTypeFromTypeNode` for each contract. `TypeFacts.ImplementedTypes` records explicit
`implements` contracts separately from `BaseTypes`.

**Checker alias-symbol identity alone does not name a type.** A transformed type retains the symbol of the alias it came from, so the checker-declared type must equal the type being named before a source name is recovered. `NonNullable<Value>` matched the members of the nullable `Value` and reintroduced `null` on the strength of the shared symbol. A union application of a generic tagged alias is recognised only where its arms correspond to the declared form's arm for arm. An arm that is an instantiation open over a signature parameter reaches the shape tier identity-only, with no members, so it corresponds by its target rather than by its tag.

**`Session.getConstraintOfType` is not the checker's `getConstraintOfType`.** It reads a
substitution type's preloaded constraint handle. Ask `getBaseConstraintOfType` for a type's
constraint. For a conditional over a bounded, naked type parameter it is the conditional
instantiated at the bound. Otherwise it is the union of both branches, which §4.11 rejects as a
mapping, and over a union bound it is the union of each member's branch.

**`TypeFlags.Any` is four intrinsic names.** `any` covers a written or omitted annotation, a shorthand ambient module and diagnosed circular return, accessor or import-alias annotations, so `TR008` is not proof an author wrote `any`; a circular type alias is the error type. `error` and `unresolved` stand for a reference the program leaves unresolved; `intrinsic` is the body of a compiler-implemented alias. Read `IntrinsicName`, never the flag alone. A union, an intersection or an optional position with an error constituent reduces to the nameless singleton error type, so its written name is lost; a qualified name survives only as the unresolved symbol's parent chain. Intrinsic type ids are singletons. Compiler-authenticated `NoInfer` declarations specialize by complete symbol identity before the shared intrinsic fallback, preserving their own binder and the configured support-alias policy. Other intrinsic declarations retain `TR064`. Compiler `NoInfer<T>` reference substitution types still use the existing fallback; the indexed-access alias reference policy is a separate mapping.

**`keyof` over a declared type reaches Shape as that type's key literals.** `K extends keyof
TagMap` arrives with its constraint resolved to `"div" | "canvas"`; only a `keyof` over a type
parameter stays an `Index` type with a target. Resolve reads the operand of a literal-bounded
type parameter off its constraint node into `TypeFacts.KeyOfOperand`; a pass that inspects the
constraint alone cannot tell `keyof TagMap` from a written literal union.

**An indexed-access type carries no alias arguments.** `type ValueOf<T, K extends keyof T =
keyof T> = T[K]` reaches Shape with no `AliasTypeArguments`, so `declTypeParams` sees no
parameters and the alias was emitted `type ValueOf = obj`. `shape-aliases` binds the operand a
`T[keyof T]` body reads from `ObjectTypeId`; any other indexed-access alias keeps no parameters.

**`getJsDocTags` merges every overload's tags.** Overloads share one symbol, so a
`@deprecated` written on one `createElement` declaration reaches all of them. Resolve reads each
overload's own leading JSDoc into `ResolvedSignature.OwnDoc`; `signatureTags` keeps the tags
that JSDoc writes, and an undocumented overload keeps the shared tags other than `@deprecated`
(solid-js documents only the first overload of `createSignal`).

**A nullable union hides the application it was written over.** `x?: Job<string>` reaches the shape tier as `Job<string>`'s arms beside `undefined`, and arms no argument re-instantiates are the declared form's own, so a member-set match lands on `Job<T>` outside its scope. Read `TypeFacts.NonNullableApplication` (the union origin's application, from `getNonNullableType`) and apply its arguments; a declared form's own parameters apply only where they are in scope. An alias that already includes `null` (`Maybe<T> | undefined`) leaves no single application in the origin, and its reference depends on the member-set match.

**Expecto's `--filter` is a hierarchy prefix.** A fixture's tests are named `generator e2e.<fixture> generates the committed goldens` and its extra cases `generator e2e.<case name>`, so a bare fixture name selects zero tests and reports success.

**The frontier width limit must not discard a source contract with an unrelated expansion.**
Resolve admits explicit seeds, primitive leaves, union enumeration, ordinary nominal references,
canonical source declarations and the nested contract edges of already-derived nominal owners.
It retains deferred responses so an owner discovered later can prove an earlier omitted edge.
Following an application's members again recreates substituted method binders indefinitely;
follow its arguments instead, and keep explicit structural promotion separate. A conditional
alias can reduce to an anonymous type literal carrying `Instantiated`; that flag alone does
not invalidate a nested contract reached from a canonical owner. Mapped computations remain
outside that traversal. Successful derivation removes both pending and `NotFollowed` entries. The
contract walk must stay linear in the table: memoise per type, never per path. Path-keyed
memoisation enumerates every path through an interface diamond, and declaration ancestry alone
admits one instantiation per permutation of mutually wrapping generic aliases. Moving a batch of
checker queries changes the order checker type ids are assigned in; Shape naming must not read
that order (see "A name, owner or placement must never depend on checker type-id order" under
Generator passes). Regenerate Core.TS after reordering session queries, and diff its symbol
names, not only its tiers.

**Core.TS `WebAssembly.*Constructor` ownership varies between runs.** With an unchanged
generator (abbdae07), `generate --only compiler-lib` places the ten constructor interfaces in
`Dom.WebAssembly` on some runs and in `Dom.GlobalThis.WebAssembly` on others. Regenerate more
than once before attributing that hunk to a change.

**An empty module export list can hide an `export =` variable root.**
`getExportsOfModule` lists namespace exports, while the direct export table retains the
`export=` assignment. Follow that assignment to its declaring value and bind the root through
the existing default-import policy. Symbol parents are absent for variable roots, so determine
locality from every typed declaration's ancestors. A `declare global` block keeps a value
global even inside an ambient module; one genuine global declaration preserves the global
binding. A scope query at file position zero can select the first ambient module's inner
scope and expose its local variable as a global candidate. The source-file node itself has
the global scope for a script. The ambient-root lab begins with its module to retain this trap.

## Declaration catalogs

**A complete constraint can disappear from naming without a resource omission.**
An object alias such as `Env` has a TypeLiteral symbol; a consumer that reaches it only through
a generic bound must retain the alias name. Catalog Resolve recovers the alias name only for
actual Constraint/Default targets whose source TypeLiteral and TypeAlias declarations are
authenticated and whose checker-declared alias type equals the bound type. Anonymous naming
follows declaration and signature type parameters, then their source-named bounds/defaults.
Inline structural bounds retain their existing constraint policy. A transformed application
that keeps an alias symbol cannot claim the original alias name through this recovery.
The bound-alias naming lab checks owner/consumer API agreement and imported nominal rejection.

**Anonymous intersections can inherit different reachable parents in different runs.**
`BotEnterprise.botManagement: BotBase & { ja3Hash: string }` is the same checker contract
when reached through `Combined = BotEnterprise & Other`, but a producer reaches Combined
while a consumer can reach only Enterprise. Parent-role minimum keys then disagree and change
Enterprise's canonical API even though the child's emitted API agrees. Declaration-less,
alias-less intersections with an actual compiler TypeLiteral constituent use ordered,
source-authenticated constituent contracts and their declaration handles before parent roles.
Every constituent must authenticate directly; incomplete or recursive evidence retains the
existing fallback. Applied arguments and independent written literal handles remain distinct.
The source-ownership snapshot still precedes parent-role inference. The
`census-owner-intersection-identity-lab` covers differing reachability, generic applications,
independent source contracts, consumer compilation and stale/API rejection. Identity contract 8
requires regenerating earlier catalogues; this change alone alters neither API nor inference policy.

**An anonymous union of nominal arms has a source contract independent of reachable parents.**
The `tlsClientAuth: Auth | Placeholder` member can acquire Combined's parent role in a producer
and Shield's in a consumer while both emit the same tagged union. Alias-less, declaration-less
unions of actual source-declared interface/class arms use sorted direct arm identity contracts
and normalized arm declaration handles. Two written anonymous unions over the same applied
nominal arms intentionally share this contract; the compiler exposes no independent union
occurrence handle. Distinct nominal source declarations, named aliases and concrete arguments
retain their identities. Every arm must authenticate directly before parent-role inference;
primitive, missing, cyclic or declaration-less arm evidence retains the existing fallback.
API and source checks still reject incompatible representations or stale contracts. The
`census-owner-union-identity-lab` covers reachability, order, repeated occurrences, independent
source arms, generic applications, aliases and conservative fallback. This correction belongs
to identity contract 8 alongside source-anchored intersections.

**TypeScript library ownership is logical, not a platform distribution.** Installed
`lib/lib.*.d.ts` files from `typescript` and recognized `@typescript/typescript-<rid>`
packages share package identity `typescript`, their package-relative library path and
exact source-byte hash. Their root manifest fingerprint covers canonical name, exact
version and normalized full gitHead; intervening manifests remain byte-authenticated.
Recognized compiler release metadata must match the library metadata. Binary fallback
permits an absent gitHead while retaining exact executable authentication. Other package
sources retain raw manifest hashes. Identity contract 4 rejects earlier platform-coloured
catalogs before reuse. Core.TS regeneration must match the committed semantic catalogue
on both operating systems; compiler/generator binary provenance may differ.

**Compiler-library shipping must not depend on module exports.** `harvestGlobals` also runs
when `typescript/lib` is `ship`. For module-only inputs, read global scope through a verified
default-library source, not the module entry: module locals can hide library names, and
module exports must not be harvested again as global values. Public global scripts retain
their own scope. Type-only global declarations can reuse Core.TS; new global values change
`typeof globalThis` and its transitive source closure, which remains authenticated.

**Computed-member escaped names contain compiler-session IDs.** A name such as
`__@match@1085` is not portable identity. Catalogue structural member keys and parent roles
use the full normalized declaration-handle set for computed keys, with a disjoint encoding
for ordinary names. Equal unique-symbol spellings from different declarations remain distinct;
computed keys without declaration evidence get no fabricated portable key. Identity contract 4
requires regenerating catalogues built before this correction. The small harvest lab locks the
scope boundary; the Core.TS entry matrix authenticates and compiles complete catalogue reuse.

**Intrinsic types are shared by unrelated exports.** A boolean value or alias cannot lend
its declaration handles to every alias of the checker's boolean type. Intrinsic alias identities
come from their own declarations, without type-wide export handles or generated parent roles.
Pure index signatures reference `JS.Record` / `JS.ReadonlyRecord` explicitly: a bare support name
can accidentally resolve to a shipped library phantom or a local type. Inference contract 2 and API contract 3
requires regeneration for this mapping correction.

**Brotli completion requires the decoder's Done status.** Catalogue file transport accepts
exactly one complete stream and checks buffered and subsequent bytes for trailing data.
A complete JSON value can precede the Brotli end marker. The shared stream reader counts actual
decoded UTF-8 bytes with an inclusive 128 MiB limit for JSON and Brotli. JsonDocument.Parse
reads to logical EOF; compatibility, declarations and variants use that shared document.
Compression belongs to Pipeline.run's disk boundary. Generation retains a declarations.json
text entry, output reports the actual filename, and switching formats retains old output.

**Schema 2 compatibility is an explicit maintenance contract.** Assess every change to
catalogue identity, source closure or ownership (`identityVersion`), canonical API/constraint
hashing or emitted ABI (`apiVersion`), inference/profile semantics (`inferenceVersion`), and
variant/customization semantics (`customizationVersion`). Bump the affected version in
`CatalogCompatibility.current` when an earlier producer must be rejected; bump
`contractVersion` for changes to the policy itself. All five versions require exact equality.
An assembly rebuild alone remains compatible. Producer compiler/generator SHA-256 fields remain
provenance; schema 1 still authenticates both hashes exactly.

**Compiler identity belongs to the executable Bootstrap launched.** Recognized TypeScript
wrapper/platform packages must agree on exact release and full gitHead, match the platform
dependency, and match the bounded `--version` probe. Portable identity also includes the binary
AST protocol. Missing package identity uses the executable hash; conflicting recognized metadata
fails. Keep the captured path and cached per-run discovery shared across customization passes.
Preserve the selected logical install path for linked platform packages; its expected executable
must match the actual physical executable beneath the resolved package directory. A file link to
another conventional install uses that physical install's metadata. An unrelated file link stays
binary. Resolving away the install path before locating the wrapper loses valid package-store pairs.
Decode compatibility and variants from the same JSON document and retain downstream source,
manifest, API, arity, constraint, owner and variant authentication.

**Constructor alias fallback identities retain their bound parameter contracts.** Two indexed
constructor aliases can have the same declaration and argument positions while binding different
nominal constraints. Include constructor parameter bounds in alias-fallback keys; an unavailable
bound cannot authenticate through the fallback. Alpha-renamed equivalent binders still share.
Ordinary alias applications retain declaration bounds rather than a caller's stronger constraint.

**SourceFile symbols do not provide public F# names.** A default namespace export can resolve
to a quoted absolute module path. Use the export's fallback name, not a sanitized checkout path.
Secondary aliases use the same `Naming.typeNameSegment` normalization as Shape when recovering
their source anchor, including names beginning with `$`.

Each rule below was written against a catalog authentication failure between two real packages;
the corresponding lab under `tests/fixtures/` is the reducer.

- **Source ownership reads the canonical map from before parent-role inference.** TypeScript
  shares one anonymous type between properties of different packages, so using the completed map
  during source traversal lets a consumer's property contribute its source file to a shared
  interface, and the producer's hash stops matching.
- **The source closure follows the sources selected for canonical identities**, not the alias
  files of every occurrence. Two independent packages that both write `Record<string, unknown>`
  share a canonical identity and must not authenticate different alias files.
- **Export-only handles do not anchor transitive source closures.** An indexed projection can
  export a compiler-interned anonymous type from a consumer file. Keep that handle for the
  projection's own identity, but traverse intrinsic type declarations when authenticating its
  owner's sources. Explicit named aliases still carry their declaration files.
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
- **Shared anonymous literals need a producer ownership decision.** Two independently generated
  producers can expose different F# enum types under the same structural catalog identity.
  Regenerate dependent producers against the selected owner's catalog; a consumer-only preference
  cannot change the enum types in their compiled APIs. The ownership lab compiles indexed field
  calls and transfers, not just whole options objects, and retains independent-ownership rejection.
- **A `never` generic default does not invalidate its nominal constraint.** Preserve the declared
  bound and apply the ordinary argument check at concrete uses. Dropping the bound on a callback's
  type parameter can give its result a different F# constraint from the exported generic alias
  with the same catalog identity. The generic-defaults lab covers this without nullable types.
- **A widen-mode consumer does not read the Core.TS catalogue.** `Xantham.Fable.Node` lists no
  `declarationReferences`, so its compiler library takes the default `widen` and
  `DeclarationCatalog.applyWithProducer` returns before loading any catalogue. A catalogue is
  accepted only under an equal inference profile (`lib`, `types`, `groups`, `resolveNoInfer`), and
  @types/node's program (es2020, through its own `/// <reference lib>`) differs from Core.TS's
  `esnext`/`dom` ship profile, so listing the catalogue alone fails validation. Under `widen`,
  `Shape.libBinding` is the only route from a compiler-lib type to a name: `Naming.LibBindings`
  first (`Promise`/`PromiseLike` → `JS.Promise`, `Error` → `exn`), then Core.TS by bare symbol
  name (`Fable.Core.TS.Dom` for DOM files and, since W3 #134, `Fable.Core.TS.Es` for ECMAScript
  files). Namespace members take their bare name, as Core.TS flattens them
  (`Intl.NumberFormatOptions` → `Fable.Core.TS.Es.NumberFormatOptions`). A name Core.TS renamed
  on collision (`Iterator2`) is reachable only under the name it kept, and the compile gate will
  not catch a same-arity wrong target. A `LibBindings` row splits a type across two families: a
  consumer's `U2<JS.ArrayBuffer, Es.SharedArrayBuffer>` was a different F# type from Core.TS's
  `ArrayBufferLike` until the row went. `Promise` and `PromiseLike` are bound under `ship` as
  well (`LibBindings.bindsWhenShipped`), so Core.TS's own members return `JS.Promise` and unify
  with a consumer's; Core.TS still declares `Es.Promise`, which nothing it emits references.

## Generator passes

**Harvest cannot distinguish a declaring path from an alias.** Same `SymbolId`, same `Order`, and
ambient modules are harvested alphabetically. "The declaring module wins" is unimplementable;
`Shape/Classes.fs` uses the class's own binding specifier instead, falling back to the first
harvested occurrence, and reports `SC010` per dropped member.

**A name, owner or placement must never depend on checker type-id order or scope enumeration
order.** Type ids follow the order of session queries, and `getSymbolsInScope` enumerates the
checker's symbol tables in an order that varies between runs, so `Intl.Locale` and
`globalThis.Intl.Locale` reach `harvest-order` in either order with an equal source position and
export name. A first-wins choice over `Map.toList` of a type-id map, or over harvest order with
an unbroken tie, moved sixteen `Intl.*Constructor` and `WebAssembly.*Constructor` declarations
under `GlobalThis` between identical runs. `orderExports` therefore breaks ties by the
declaring namespace's own path first, then ordinal path, leaving occurrences from different
origins in harvest-pass order (an origin-string key moves solid-js's `For.Props` under `Web`); and `name-constructor-objects`
descends its roots in source order, then by name. Order every root list that decides a claim by
`DeclOrders` and name; prove a change deterministic by regenerating compiler-lib three times and
comparing the `symbols.jsonl` hashes.

**A frontier generation must never read what the same generation writes.** `resolve-type-table`
derives a generation under `Async.Parallel`, and derivation both writes and reads the walk's
registers: a union registers its operands as structural, a signature registers its type
parameters, and an instantiation decides identity-versus-full derivation by reading both. With
live `ConcurrentDictionary` reads, an operand derived in the same generation as its union came
out identity or full depending on which answer arrived first, and solid-js's first
`FrontierTooWide` count drifted between identical runs (280392, 281116, 281110, 280874). Each
`Register` in `Resolve.fs` therefore answers from its state at the last `Commit`, and the walk
commits only between generations (and after `boundedFrontier`, which writes but does not derive).
A new register, or a new read of one, goes through `Register`; `XANTHAM_RESOLVE_COUNTERS=1`
prints `staged-reads`, the reads per generation that a live read would have answered by timing.

**`__@<name>@<id>` does not identify a well-known symbol.** A package's own `declare const
iterator: unique symbol` escapes to the same `__@iterator@<id>` as `[Symbol.iterator]`. Resolve
reads the declaration's computed name `Symbol.<name>` into `ResolvedMember.WellKnownSymbol`; a
pass that decides on the escaped name alone emits the impostor as `[Symbol.iterator]`. The
spelling `Symbol.<name>` is not proof either: a package's `declare global` augmentation of
`SymbolConstructor` (`Symbol.observable`) names a key that is `undefined` at runtime, so Resolve
also requires a `lib.*.d.ts` declaration of the property. The program's own lib files are not
enough either: `@types/node` augments `Symbol.asyncDispose` because its `lib` omits
`esnext.disposable`, so a key declared only by a package falls back to a second program over the
compiler's `esnext` lib. That program must have a root file; with none, `resolveName` finds no
global `Symbol`.

**F# admits one `seq` instantiation per interface.** `NodeListOf<'TNode>` inheriting
`NodeList`'s `seq<Node>` beside its own `seq<'TNode>` is FS3360 ("may unify"). Only
`settle-symbol-members` settles which `seq` a declaration keeps, bases first: the base's
wins, and a narrowed iterator becomes an Emit member. A pass that adds a `seq` inherit after it
reopens the error.

**A finding cannot be withdrawn.** The tier fold appends each pass's findings, so a member a
later pass removes keeps the findings raised while shaping it, and the manifest reports a
widening that is not emitted. Findings that depend on a later decision wait on the model, as
`ShapeModel.IteratorFindings` does for `[Symbol.iterator]` until `settle-symbol-members`.

**`drop-orphan-delegates` has a failure mode on each side.** The first version retained nothing
and pushed `AC001` from 8 to 22; the second retained anything whose name fuzzily matched an export
and dropped nothing corpus-wide. The shipped rule checks representation the way `audit-coverage`
does: a candidate drops only where a harvested export stays represented without it.

**Exactly one pass may declare a callable name.** `hasIncompatibleOverloadedTypeParameters`
(`Shape/Spec.fs`) is read with opposite signs by `declaresInterface` (`Shape/Interfaces.fs`,
admits) and `shapeCallbacks` (`Shape/Callbacks.fs`, skips). Changing either sign alone mints a
duplicate or an unnamed type.

**A declared callable application must precede callback expansion.** `objectRef` uses the
existing named instantiation and its checked arguments before `delegateRef`. Re-expanding
`SetStoreFunction<T>` inside `StoreReturn<T>` discarded its overload binders and emitted an
eight-argument `Action<obj, ...>` while direct uses referred to the existing interface. The
generic-callable-reference lab compiles and runs transfers through inputs, returns and tuples,
including a catalogue consumer; unnamed callbacks retain the ordinary fallback.

**A return position widens on a concrete application, not on a type parameter.** Applying a
declaration's own free parameter keeps the name; `Coalesce<string>` renders `obj -> obj` under
`TR031`/`TR013` where `Coalesce<'T>` does not.

**A pass that rewrites exports must run after `order-declarations`.** Exports are not an
`FsExports` decl until `order-declarations` builds one from `model.ExportMembers`. A pass sitting
earlier and mapping over `model.Decls` looking for `FsExports` matches nothing, silently produces
no output, and has no container name to key its findings on. `expand-union-arms` cost an
afternoon to this.

**`literalOverloadSets` and `shapeMembers` must read the same literal keys.** Both decide
through `Spec.isLiteralKeyed` and `Spec.literalKeys` whether a literal is a method name (W5) or
a retained single-case StringEnum. If they diverge, a StringEnum is declared that nothing reads,
or a literal parameter is both written into an Emit and kept. A callback-valued property is
tested on its declared type in `literalOverloadSets` and on the type its intersection operands
agree on in `shapeMembers`; where those differ, the literal widens rather than retains.

**`anyReturnAsUnit` reaches only sites that scope with `selectAnyReturn`.** `delegateRef` writes
`unit` for a callback's `any` return when its owner equals `ShapeModel.AnyReturnOwner`, which only
`Spec.selectAnyReturn` sets. A new property or value site that calls `typeRef` without it keeps
`obj` for a selected name. A stale owner affects only a callback whose owner string is identical.
The pattern is part of `DeclarationCatalog.profile` when set, so a catalog consumer whose pattern
differs from its producer's fails the inference-profile check. Every Core.TS catalog consumer sets
`^on`.

**A `[<ParamObject>]` Emit that spreads `$0` is parenthesised.** In statement position `{`
opens a block, so `{ kind: 'method', ...$0 }` fails with `SyntaxError: Unexpected token '...'`;
`({ kind: 'method', ...$0 })` works in every position (spec probe 10). A literal value written
into an Emit template escapes `$`, `{` and `}` as `\x24`, `\x7B` and `\x7D`, because Fable's Emit
macro reads them as argument holes; `Render.jsString` does both.

**A declared form's arguments are its enclosing declaration's parameters.** Written inside
`interface Grid<T>`, `Grid<T>` is the generic target itself (`ownArguments` non-empty), not an
instantiation. `freeTypeParams` reads a named declaration's arguments in both cases; a walk that
skips the declared form leaves a hoisted delegate without the owner's `'T`, and every use of it
widens with `TR012`/`TR013` (188 typed-array delegates in Core.TS before W7).

**A namespace member's symbol is an `Alias` when it is re-exported.** `export { f } from "./f"`
inside a namespace object gives `ResolvedMember.Symbol` the `Alias` flag, not `Function`.
Decide method form on `ResolvedMember.ResolvesToFunction` through `Spec.emitsAsMethod`, never on
`SymbolFlags` alone. `Customization/Semantics.fs` classifies through the same predicate
(`Spec.emitsAsMethodWith`), so a customization contract and the rendered member agree.

**A finding needs a row in `FindingCodes.table`, not just a registered union.** Registering the
kind in `FindingCatalogue` is enough to compile and enough for the pass to run; the missing code
only throws at manifest render, from `Finding.get_Key`, which reads as a render bug rather than a
missing table row.

- **Inline object literals never share a type id.** Two arms spelling `owner: { name: string }`
  identically carry two ids, so comparing member type ids calls them different. Compare with
  `Spec.sameShape` wherever "the same type" means "reads as one F# type".
- **A union case's XML doc documents all of its fields or none.** FS3390 checks `<param name>`
  on union cases against field names and requires every field once one is documented; an empty
  `<param>` satisfies it. A static member named like a case of its union is FS0023, which is why
  case helpers are `Create<Case>`.
- **A submodule spelled like a union case is unreachable from expressions.** With
  `type Recovery = | Aborted …` and `module Recovery = module Aborted = type Type`,
  `Recovery.Aborted.Type.User` resolves `Recovery.Aborted` to the case: FS0039, and FS1127 in a
  pattern. Type annotations still resolve, so a compile gate over declarations alone misses it.
  Case declarations nest under `Recovery.Cases.Aborted` instead.
- **Two `TypeScriptTaggedUnion` cases sharing a `CompiledName` need a `CaseTest`.** Without one,
  Fable compiles them with no warning and the later case owns the tag's `switch` slot: the first
  is unreachable, and a value of its arm matches the second (spike S1(e),
  `docs/.ai/plans/2026-10-15-pre158-findings.md`). `Spec.separateArms` gives every case of a
  shared tag value its test, or `detect-tagged-unions` declines the DU (`DT003`). Each test must
  be exclusive within its group, because a `match` checks cases in the order its clauses list
  them. The attribute exists only in the Fable fork's `Fable.Core` (`tools/fable-fork/fork.json`).
- **An arm interface drops only where its case carries the arm whole** (`Spec.caseCarriesArmWhole`):
  a symbol-keyed member or an index signature is no case field, so such an arm keeps its
  interface as the typed path to it. Such an arm is now an `[<InlineInterface>]` case, which
  reads the interface, so it stays declared through that read.
- **Only Fable checks an `[<InlineInterface>]` case.** The case must have one field whose type is
  an interface; .NET compiles any field, so the compile gate passes a case Fable rejects ("the
  case must have exactly one field, whose type is an interface"). A golden carrying a new inlined
  shape needs the run gate (`build.fsx -- test --run-gate`), or a Fable compile of it, as
  evidence.

## Fable runtime type tests

Measured against Fable 5.13.0 on 2026-09-22 while designing erased-DU mapping for mixed
literal/typed unions; they predate the repository's `fable` tool pin of 5.17.2. These constrain any pass that puts a payload arm on an `[<Erase>]` union.
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

**Imported class statics need native accessor metadata.** With Fickl 0.6.2, a type-level
`Import` bypasses an ordinary F# static member body at the call site. A property-level
`CompiledName` also leaves its allocated getter/setter spelling in the emitted JavaScript.
For an exact string key, attach the constructor `Import` and bracket `Emit` to each native
getter/setter, and to the native method. The backtick member lab proves reads, writes and
constructor receivers through both source bindings and a raw `Reference`/`HintPath` DLL.
An ambient entrypoint's abstract method needs separate override proof; retain `MB007` for
an impossible key until that contract is supported.

**`Xantham.Fable.Core.TS` reaches Fable as a DLL, never as source.** Fable merges a NuGet
package's sources only when the package ships `fable/*.fsproj`, and this package ships `lib/`
alone. The run gate reproduces that with `--exclude Xantham.Fable.Core.TS`, which also cut its
Fable compile from 22.7s to 6.3s (Fable 5.0.0, 2026-09-24). From the DLL, Fable resolves only
attributes (`Emit`, `Import`, `Global`, `Erase`, `ParamObject`, `CompiledValue`) and interfaces.
Every member of the bindings, generated or hand-written, must therefore be an attribute-carrying
member or an abstract one. The exception is `op_ErasedCast` on an `[<Erase>]` union: Fable
special-cases it, so the generated `static member op_ErasedCast(x) = CaseN x` bodies (199 in
`Xantham.Fable.Core.TS`) compile from the DLL. Any other `inline` member or member with a body
fails at the consumer's call site:

- under a `Fable.Core.*` namespace, as `… is not supported, try updating fable tool`;
- elsewhere, as `Cannot find the body of inline member`.

The run gate catches this only for members it calls, so a new hand-written member needs a check
in `tests/Xantham.Generator.RunGate/Program.fs` (`bindingExtensions`).

`Xantham.Fable.Core` is the exception: its `KeyOf`/`TypeKeyOf` helpers are `inline`, so it is
compiled from source, and excluding it fails with `Cannot find inline member`. Its package ships
its sources under `fable/` for the same reason; a package without them fails the same way at a
consumer's first helper call.

**Fable gives no Fable.Core `JS.*` type a mapping a binding can rely on.** Checked against
`fable-compiler/Fable` at tag 5.17.2 (the tool pin) on 2026-10-13. The compiler matches the
`Fable.Core.JS.` namespace as a whole: its entities resolve to the global of their display name
(`FSharp2Fable.Util.fs` `tryGlobalOrImportedAttributes`), are not replacement candidates, and
keep unmangled members. Core.TS declares interfaces, which Fable leaves unmangled, and reaches
values through attribute-carrying members, so it needs none of that.
The only per-type cases are `JS.Map`2` and `JS.Set`1` (`Fable2Babel.fs`), annotated as
fable-library's `IMap`/`ISet` in TypeScript output only, which `Dictionary`/`HashSet` implement at
runtime; F# type-checks no conversion from either. So a `JS.*` spelling buys a consumer nothing
the Core.TS declaration does not, and costs unification with every Core.TS API. `LibBindings`
keeps `Error` → `exn` (Fable compiles `System.Exception` to a JavaScript `Error`) and `Promise`
/`PromiseLike` → `JS.Promise`, because Fable.Promise's `promise { }` and
`Async.AwaitPromise`/`StartAsPromise` are written against `JS.Promise` (lead decision, W3 #134).
A future row needs a reason of one of those two kinds. The promise rows also bind under `ship`;
a row that binds only for consumers splits a type between Core.TS and its consumers.

**A same-named module loses qualified expression lookup to an interface member.** `module
Event` of `[<Literal>]` values beside `[<Interface>] type Event` in `module rec M` resolves the
fully qualified `M.Event.AT_TARGET` in an expression to the interface's instance property
(FS0806, "Property 'AT_TARGET' is not static"); a pattern resolves it to the literal. After
`open M`, the unqualified `Event.AT_TARGET` resolves to the literal in both positions. A module
abbreviation (`module EventConstants = M.Event`) names the module alone for qualified access.
`Event.Create` resolves to the type's static, since the module declares no `Create`; W6 keeps a
literal named `Create` out of the module.

## Build and test environment

**Nested `dotnet build` inside a test stalls under `dotnet test`.** Idle MSBuild nodes hold the
test host's stdout, reproduced three times in one lane. `build.fsx` runs the Expecto executables
directly, and nested builds carry a copy of the root `global.json` beside their scratch project
inside the repository (`.claude/rules/tests.md`).

**The CLI prefers its `xantham tsc init` cache to the parent walk.** Its compiler precedence is
`XANTHAM_TSGO_EXE`, then the cache, then the walk up from the package. A stale cached compiler
crashed on `type-fest` (`json: cannot marshal … +Inf` from `getDeclaredTypeOfSymbol`) while the
test suite passed. When measuring with `dotnet run --project src/Xantham.Cli -- generate`, set
`XANTHAM_TSGO_EXE` to the root install's `tsc.exe`, and check each off-mode `manifest.json`
against the committed one.

**`tools/workspace.fsx` exports `XANTHAM_TSGO_EXE` from any checkout**, not only a worktree, so
the catalog suites run in the main checkout. Only a worktree skips `npm install`: the main
checkout owns its install, so a `package.json` typescript pin bump reaches the exported path.

## Open threads

Carried from lanes that closed without finishing these.

- **Mixed literal/typed unions: reopened, decision pending.**
  `plans/2026-09-22-mixed-literal-unions.md` Step 0 found **11 eligible of 45** such unions,
  counted over raw `TypeFacts` by `Shape/MixedUnionCensus.fs`. An earlier run reported 0; it
  read `FsTypeRef`s after the literals had already widened to `string`, and its result is
  withdrawn. Whether 11 unions justify a new decl case is the open question.
- **Union arm overloads: implemented 2026-09-22, shipped disabled** behind
  `unionArmOverloads.enabled` in `xantham.json`: `plans/2026-09-22-union-arm-overloads.md`.

- **`Exports` has no stable declaration parent role.** 22 of 38 catalog cases errored on this
  across the declaration-catalog, callable-signature, source-closure and anonymous-literal-union
  lists. Four cases stay disabled behind a TODO.
- **Entrypoint subclassability.** Generated `Container`/`ContainerProxy` are not F# subclassable;
  direct subclassing goes through the ambient runtime base. The ordinary SDK class
  interface-and-constructor policy was retained rather than revisited.


## Early projections must precede loss (2026-10-09)

`Customization.Semantics.projectResolved` runs after Resolve, before Shape. Build companions
from its original arms; an emitted `string` or `option` cannot recover source literal membership
or the null/undefined distinction. Keep source tokens and companion plans snapshot-sealed;
equal checker IDs or equal arm sets do not merge named declaration identity.
A phantom alias such as `type Phantom<T> = "auto"` shares its checker type with a plain
literal alias; inspect its declaration parameters through Resolve rather than tagging that type. Cached tokens and
plans must fail on the next run, while nonces must never enter output provenance.

Projection companions do not alter the raw ABI. Keep their manifest provenance separate from
raw catalog variant policy and validate the complete emitted source before writing. The
Myriad adapter's `Decoded|Invalid` matches through a strict raw-value decoder into a normal DU.
It does not pattern-match an erased union or recover the originating alias from shared strings.
An optional-property presence test requires separate object facts; decoding its read value
cannot distinguish absence from an explicitly present `undefined` property.

**Typed operations must authenticate their receiver after placement.** Before-Shape method
facts carry their receiver token into `ProjectionCompanion.forOperation`. Pipeline checks its
final qualified type after catalog application. A caller-supplied F# name plus an `unbox` is
insufficient: another receiver can have the same method name with an incompatible argument.

**Array identity comes from the checker.** Global augmentation can classify the built-in
`Array<T>` under `@types/node`. Use `Session.isArrayType`, retaining the non-tuple/one-argument
guards. A local interface named `Array<T>` is not evidence of JavaScript array representation.

**Operation property keys come from typed declarations.** Checker symbol names escape leading
double underscores (`__proto__` becomes `___proto__`). Method and selected-field lookup must
compare recovered declaration spellings, retaining the final name authentication. Record projections recover the declaration
spelling and use allocated F# field names; computed/symbol keys reject. Emit own data properties
with `Object.defineProperty`: ordinary assignment to `__proto__` invokes the prototype setter.
Optional record and selected-field inputs have an outer option for omission, separate from
present undefined. Selected-field projections must reject required selected fields and required
siblings. The contract describes resolved values, not `exactOptionalPropertyTypes` writes.

**Shared operation input contracts require an explicit owner.** `Operations.createShared`
aliases the first selection's emitted types only after exact resolved-shape agreement.
Each method still authenticates its own occurrence and source closure. Automatic value-set
deduplication would silently merge independent contracts.

## Core.TS packed catalogue

Regenerate `compiler-lib` with its tracked `declarationCatalog: true` setting whenever changing
the TypeScript pin, binding configuration or generated producer. Keep committed declarations
and compiled bindings together. Pack creates a Brotli transport and version/ownership descriptor;
it does not regenerate producer facts. The generation stamp `catalogue-inputs.json` and the
assembly's build record must match current inputs before packing. The package consumer test and full-library regeneration
smoke are release evidence. Consumers need both the restored catalogue file and a PackageReference
to the same Core.TS package version; the descriptor alone supplies no binding implementation.

The isolated local package test packs both Core.TS and its `Xantham.Fable.Core` dependency
into its scratch feed. Its `PackageVersion` override propagates through project references,
so both test packages use that override. Packing Core.TS alone can appear green while a
published stable support version satisfies its prerelease range, then fail with NU1102 after
the next Core.TS bump. Downloaded-package verification still restores real published dependencies.

## Fickl must be last in the tool manifest

After a Fable/Fickl run, restore the solution before a quick .NET build: compilation can leave
the multi-targeted support project's assets restored for netstandard2.1 only. Run
`dotnet restore Xantham.slnx -v q` to restore every solution target.

With SDK 10.0.402, a cold Linux `dotnet tool restore` fails if another tool follows Fickl:
fsdocs-tool and Fantomas are reported as containing only the `fickl` command. The SDK's
`ToolPackageDownloaderBase` reuses its local assets directory without clearing
`project.assets.ridpackage.json`; `ToolPackageInstance` prefers that leftover file for the
next package. See the [SDK implementation at 10.0.402](https://github.com/dotnet/sdk/blob/v10.0.402/src/Cli/dotnet/ToolPackage/ToolPackageDownloaderBase.cs).

Keep Fickl last in `.config/dotnet-tools.json` until the SDK fixes this. `--disable-parallel`
does not fix a cold restore; warm-cache retries can conceal the problem. `tools/verify-build.mjs`
guards the ordering. The workaround was verified with separate empty `DOTNET_CLI_HOME` and
`NUGET_PACKAGES` directories on Linux, using the CI SDK version.

## Release markers after a squashed release

PR #117 was squashed into master at `500a04a`, which is the repository commit recorded by all
seven published 0.2.0/0.3.0 packages. The later history repair and release merge `78de2edf`
join that squash with the original develop history. Its published payload inputs (`src/`,
Directory.Build props/targets, SDK and npm pins) are unchanged from `500a04a`. A ShipIt baseline
on either pre-join history counts the other copy of already released feature commits again.
The release markers therefore use `78de2edf` as the verified joined-history baseline. Preview
after this correction advances only Core.TS and its Node consumer to 0.2.1 for catalogue
packaging. Preserve release merge commits to avoid recreating this duplicated history.

## Merged callable members retain declaration provenance (2026-10-25)

`Window & typeof globalThis` gives structuredClone a transient Property symbol whose Method and
Function flags are both absent. Its declarations retain MethodSignature and FunctionDeclaration;
its single generic signature retains one binder shared by input and return. Reading flags alone
sends that method through delegateRef and widens both positions with TR013. Resolve authenticates
the retained declarations, including ExportSpecifier aliases resolved to Function symbols.
A PropertySignature or PropertyDeclaration keeps the property form; required pure-callable
guards still apply in Spec and customization Semantics. The namespace intersection lab reproduces
the same compiler shape without importing the entire global scope.

## Source-identical overloads precede emitted signature collisions (2026-10-25)

Two source overloads with identical compiler parameter types and optionality select the first
return in TypeScript. For example `readlinkSync(path, options?: EncodingOption): string`
precedes an overload with the same inputs returning `string | Buffer`; unioning those returns
loses precision. Compare source identities before shaping, rather than only the compiled F#
parameter key. A receiver is separate from `getParametersOfSignature`: the compiler's
`SignatureResponse.ThisParameter` supplies `ResolvedSignature.HasThisParameter`. Generic,
rest and explicit-receiver signatures retain their existing collision policy. Assign expanded
signature ordinals before filtering, so a later retained overload keeps its authenticated
source ordinal rather than its new position.

## Declared generic alias operands survive frontier pressure (2026-10-25)

Catalogue discovery can add a non-exported generic alias's declared form after the initial
Resolve seeds. That form is a source contract even when its response carries alias arguments.
Protect its written operand closure only after authenticating actual generic TypeAliasDeclaration
nodes and exact equality with getDeclaredTypeOfSymbol. All-type-parameter arguments alone are
insufficient: a method's application has its own binders and is not the alias declaration.
Cache authentication per type and declared-type lookup per alias symbol. Applications retain
the existing ancestry/depth limits; unavailable source or compiler evidence grants no exemption.

The real Hono JSONRespondReturn canonical intersection lost its TypedResponse operand at width
32768 while complete method applications survived. Selecting a complete application as the
canonical form would change binder provenance. Keep the canonical source form instead; do not
raise the global width budget or relax catalogue API/source authentication. Simple reductions
with array or conditional arguments did not reproduce the cutoff: retained real-source RED/GREEN
evidence is required alongside the small width-one positive and bounded-expansion regressions.

## Selected conditional branches are protected contract edges (2026-10-26)

When Resolve has chosen Conditional.Branch for a protected source signature, the source-contract
walk must retain that target and its selected closure. HonoRequest.param's outer sole-inhabited
conditional was present in both producer and consumer, but the consumer's selected inner
conditional was width-deferred. That made Spec emit TR045/obj before overload deduplication,
while the producer followed the same branch policy to P2. Follow only the stored selected edge
inside the existing expand guard, with the same ancestry/depth/memoization; do not crawl both
raw branches or introduce a new conditional mapping policy to repair an incomplete closure.
