# Fidelity loop and generic callable references

> Execute inline with superpowers:executing-plans; independently review each completed PR slice.

**Goal:** Close avoidable fidelity losses, especially lost type parameters, obj reductions and
generated APIs that cannot be used together. Use Myriad and Fickl where they preserve the source
contract within F#'s capabilities. This is the user's October 24 Ralph-loop instruction.

**Architecture:** Prefer preserving a source declaration and its arguments over re-expanding its
shape. Use typed compiler-supported representations for runtime identity, and authenticated
Myriad projections for concrete computations that cannot be represented directly in F#.
Do not hide a real loss by relabeling findings, adding phantom parameters without semantics,
inventing runtime tags, or substituting casts for consumer type-checking.

**Tech stack:** Existing TypeScript compiler pin, F#, Fickl 0.6.0 and Myriad customization APIs.

## Current release and validation status

All 13 Fickl 0.6.0 packages are published and publicly indexed, built from merged upstream
commit b32f1707b130c588e2292b1912c8f15c0d0dd8c7. Slow uploads hit NuGet's gateway deadline;
the same unmodified artifacts succeeded through NuGet's other public TCP route, retaining
www.nuget.org TLS validation, SNI and Host. Historical blocked notes below describe earlier
attempts and are superseded by this completed publication and public-package validation.

With a fresh `tests/.scratch/nuget-public-release` cache and the unchanged public-feed
NuGet.Config, solution restore and both Core.TS package-consumer tests passed. The full .NET
run then passed 1,543 generator tests and 105 Wire tests, with one ignored in each suite.
Evidence is in public-restore.log, public-core-consumer.log and public-full-test.log under
tests/.scratch. A fresh DOTNET_CLI_HOME and public-only tool restore then passed all six callable
fixture tests, 648 runtime checks and the Partas gate (public-runtime.log). SHA-256 comparisons
confirmed the cached Fickl and Fickl.win-x64 packages exactly match fresh nuget.org downloads;
Fickl.Core's cache metadata records nuget.org as its source. Public dependency validation is
complete. The current branch is based directly on develop at merged PR #168 and is ready for
commit and PR submission after the completed independent source/generated-output reviews.

## Loop contract

- One active bottom PR slice at a time; gh stack manages subsequent slices.
- Reproduce each defect in a small lab, observe failure, implement, compile consumer usage,
  check runtime behavior where relevant, measure the selected corpus, then review and submit.
- Wait on completion events for long checks. No timed model polling or elapsed-time updates.
- Record irreducible F# limitations separately from missing implementation and compiler support.
- Existing untracked user decision notes and unrelated workspace changes remain user-owned.

## First slice: named generic callback identity

Files: Shape/Spec.fs, Shape.test.fs, Pipeline.test.fs, a new generic-callable-reference-lab,
the runtime gate and the mapping/architecture records.

- [x] Add PairCallback<T> with two arguments, an application inside Pair<T>, and value exports
  returning and accepting PairCallback<string>. Assert every reference uses the named application.
- [x] Add a pure shaping test: an object reference targeting a declared callback must become
  FsApp("PairCallback", [FsString]), not a fresh System.Func/delegate shape. Keep unnamed
  callbacks and tuple/array handling unchanged.
- [x] Run the focused tests and observe the expected failure.
- [x] In objectRef, prefer an authenticated named instantiation over the pure-callback fallback.
  Reuse appliedRefTo so declaration constraints remain checked.
- [x] Compile and run a consumer that obtains a callback from the tuple and passes it into the
  accepting export. Measure solid-js and animejs, particularly TR013/TR031 and arity findings.
- [x] Regenerate goldens, run complete gates and independently review.
- [x] Commit and submit through gh stack. PR #167 merged as f9660c86 after green CI.

## Remaining frontier to refresh after each slice

1. Generic calls and overloads currently hoisted into delegates: determine where interface
   Invoke methods retain method-level binders without pretending F# has rank-2 function types.
2. Missing owners and namespace/global augmentation: distinguish an absent binding from a
   fundamental type-language limitation, and authenticate catalogue ownership.
3. Required nullable construction: preserve the approved explicit-null overload direction;
   measure multi-field growth before choosing its API.
4. Conditional/indexed/mapped computations: use concrete, authenticated Myriad projections where
   useful; do not claim a general F# type-level evaluator or arbitrary structural constraints.

### Next candidate: callable-owned binders and overloads

The existing callable-overloads-lab deliberately records two losses: OneShot<T>'s method-owned
U is hoisted to the delegate head, and Multiplex loses its second non-generic call signature.
Fresh FCS inspection finds the shared routing decision in Shape/Callbacks.fs and
Shape/Interfaces.fs: only incompatible overloaded type-parameter bounds force a pure callable
into an interface today. Broaden that decision only after a focused experiment proves generic
Invoke methods and overloads can preserve both call-site behavior and construction ergonomics.
Include source and catalogue consumers, then runtime calls in both directions. In particular,
do not merely move the loss from reading a callback to constructing one, or treat F#'s lack of
a rank-2 function type as proof that a generic interface method cannot represent the contract.

Scratch experiment: tests/.scratch/callable-binders-spike (Fickl 0.5.4) compiles an Identity
interface with generic Invoke<'T>. An imported JavaScript identity function can be invoked at
int and string and passed back to JavaScript unchanged. However, implementing Identity with an
F# object expression produces `{ Invoke(value) { return value; } }`, not a callable function;
passing it to JavaScript throws `TypeError: callback is not a function`. Adding InlineInterface
does not change that outcome. A complete representation needs a proven typed construction path
or explicit compiler support; merely emitting the interface would leave that gap open.

A follow-up spike succeeds with a separate IdentityImplementation interface (generic method,
ordinary object semantics) and a typed adapter emitting `implementation.Invoke.bind(implementation)`.
All six import, construction, and roundtrip checks pass with number and string instantiations.
This proves construction is within reach without erasing the binder. It is not yet the chosen
generated API: duplicating every callable into an implementation interface has a usability cost,
and Fickl support could make an ordinary object expression produce the callable directly.

## Evidence

### Generic callable interfaces in progress

Fickl PR #8 adds CallableInterface support for a single generic Invoke contract, including
object-expression construction, rest arguments, recursion and method-owned TypeScript binders.
PR #9 merges the 0.6.0 version. Local validation passed 2,944 TypeScript tests, 149 compiler
tests and the complete JavaScript pipeline. Publication built and validated all 13 packages,
but the first NuGet upload returned HTTP 403; no 0.6.0 package was uploaded. A replacement
publication credential is pending. Public-package consumer evidence remains required.

Xantham branch feat/generic-callable-interfaces starts from merged #167. A focused shaping
test first failed because Identity's generic call became an alias. The shared routing predicate
now sends a single generic call to shape-interfaces and excludes it from shape-callbacks.
The focused test passes: Identity has no declaration parameters and Invoke owns T in both its
input and result. This is only shaping evidence. Remaining work includes authenticated named
references, anonymous callback lifting, inheritance, CallableInterface rendering, source/DLL
construction and runtime roundtrips, catalogue reuse, full fixture regeneration and review.

The live generic-callable-interface-lab exposed anonymous generic callbacks becoming
`obj -> obj`. Synthesize-anonymous now names single generic call contracts and keeps their
binders on Invoke. The five focused tests pass, including a catalogue consumer that constructs
Identity, invokes the same instance at int and string, and consumes PairWith<string> with an
independent int method argument. This is .NET compile evidence only; Fickl construction is still
pending. InheritedIdentity currently retains `inherit Identity`. Preserve that subtype relation
when completing compiler construction support: flattening the inherited signature alone would
make values cease to be assignable to Identity in F#.

Fickl PR #10 supplies inherited callable support: an explicitly attributed derived interface
may inherit one callable base with no added members. Validation recurses through bases;
source and DLL consumers retain generic substitution, base assignability and function identity.
The follow-up passed 2,947 TypeScript tests, 151 compiler tests and the full JavaScript pipeline
(2,995 .NET, 3,181 JavaScript, 3,206 standalone, plugin gate), plus independent spec/standards
review. The prebuilt 0.6.0 package files predate this follow-up and must be rebuilt from the
final release commit before publication. No successful publication has occurred yet.

Rendering now carries an explicit FsInterfaceDecl.Callable flag from Shape through inherited
member pruning and emits Fickl.Core.CallableInterface. A live source assertion first failed
without the attribute, then passed for both Identity and InheritedIdentity. The catalogue API
fingerprint includes the marker for callable contracts only, retaining ordinary-interface hashes.
Existing test model literals were updated with Callable=false using compiler diagnostic locations.
The new fixture goldens still predate attribute rendering; regenerate after the public package
restore is available. Do not cite the earlier catalogue compile result as validating the new
attribute-bearing output. Remaining edge cases include multiple callable bases, incompatible
overload inheritance, catalogue-supplied bases and runtime construction of generated anonymous
contracts. Production/package pins remain unchanged at Fickl0.5.4 pending publication.

Isolated local-compiler evidence now covers generated source and compiled bindings DLLs:
tests/.scratch/generated-callable-runtime runs 13 checks in each mode. Named, anonymous,
inherited and redundantly inherited contracts preserve callable construction, function identity,
base assignability and repeated generic invocations. PairWith<string> retains its independent
method binder. These use the local merged Fickl compiler and an explicit scratch DLL reference,
not a public-package restore; promote the checks into the permanent run gate after publication.

RedundantIdentity extends both Identity and InheritedIdentity. The live checker repeats the
same resolved call signature (same binder IDs and file-owned parameter identity), previously
causing a hoisted alias and an obj return. Interface routing/shaping now deduplicates identical
resolved signatures. A redundant non-generic base is omitted only when another declared base
already reaches it; the transitive F# subtype relation remains. This fixes the fixture to zero
widened/escape findings and passes both source/DLL runtime checks. Distinct generic applications
and unrelated multiple callable bases still need separate coverage; no structural equivalence
or generic argument unification is inferred by this change.

The permanent source-level catalogue inheritance test passes for Child extends Identity and
PairChild extends PairWith<string> from a producer catalogue: both preserve the attributed
qualified base reference without redeclaring the producer interface. Public NuGet recheck still
lists Fickl.Core only through 0.5.4.

Selected corpus measurement (CLI with the root compiler pin, same installed fixture versions):
SolidJS TR013 falls from 7 to 2, with remaining findings on NoInfer and Store.Store. TR031 stays
12; TP003 stays 1; RA003 stays 0. Symbol tiers change 71/189/81/1 to 71/192/81/1
(exact/ergonomic/widened/escape), including the new anonymous callable contracts. Animejs stays
126/188/26/6; TR013=0, TR031=18, TP003=0, RA003=0. Both full generated binding files compile
under .NET with zero warnings/errors and under the merged local Fickl compiler. Scratch outputs
and aggregate script live in tests/.scratch/callable-measure; production goldens are unchanged.

Independent review found two generic-inheritance defects, reproduced by StringPair and
RedundantPair: inherited member comparison had not substituted base arguments, and the earlier
name-only pruning handled no FsApp ancestors. Inherited.dropInherited now compares instantiated
members and prunes exact instantiated ancestor references. Reference substitution is shared with
SymbolMembers. Closed, renamed-outer and redundant-generic live declarations pass source checks;
17 local source/DLL runtime checks pass before adding the binder-collision regression.

Review also exposed binder capture. Comparison alpha-renames method binders to internal names
before outer substitution. Four unit cases cover closed arguments, method shadowing, distinct
return retention and capture (observed red then green). The live Collision<U> extends PairWith<U>
fixture revealed that shapeSignature already assigned the same written name to distinct outer
and method IDs. typeParamsOf now suffixes collisions against any different in-scope binder,
including unsanitised source names. This latest naming change requires renewed runtime/corpus
evidence and review; earlier selected corpus metrics predate generic inheritance substitution.

Final allocator verification preserves a prior allocation for the same type ID while suffixing
different IDs with colliding spellings. All 123 shape-pass tests, six type-parameter-scope tests,
and four inherited-callable generic tests pass. Regenerated bindings pass all 19 runtime checks
in source and DLL consumption with the local Fickl compiler; the DLL build has zero warnings
and errors. Spec re-review reports no further concrete findings. These remain local compiler
checks, not validation against published 0.6.0 packages.

The next surgical probe is tests/.scratch/callable-multiple-bases: First and Second each declare
the same generic identity call, and Both extends both unrelated interfaces. Current output
hoists Both to Both<'T> = ('T -> 'T), losing invocation-owned polymorphism and both nominal
base relations. The manifest reports one widened symbol (TP003/TP009/TR031), rather than an
unsupported CallableInterface marker. This is a confirmed remaining routing/overload gap;
do not close it by dropping a base or choosing a monomorphic alias. Fickl's current inherited
callable contract accepts only one attributed base, so faithful multiple-base support needs
compiler evidence as well as generator signature equivalence work.

Follow-up probe confirms the F# boundary: directly implementing Invoke for two unrelated
identical interface slots fails FS0361. Moving the method to a shared callable ancestor and
having both named interfaces inherit that ancestor compiles under F#, preserving both base
relations. Fickl rejected this valid diamond with its single-base restriction. The compiler
follow-up branch feat/callable-diamond is based on merged 03bc9111e and accepts only a shared
instantiated declaring slot (not unrelated structurally equal slots). Its 2,950 TypeScript tests
pass, including generic and DLL diamonds. All 153 compiler tests pass, followed by 2,998 .NET,
3,184 JavaScript, 3,209 standalone tests and the plugin gate. Independent spec and standards
reviews found no remaining defects. Fickl PR #11 was submitted with gh stack and merged as
b32f1707b130c588e2292b1912c8f15c0d0dd8c7. The single-layer stack was not registered as a
GitHub stack by submit, so merge used gh pr merge with the verified head SHA. Actions remain
disabled for the repository; no CI wait is armed. The compiler worktree is clean on
feat/callable-diamond at 3b03f62a. Public publication remains pending; previously packed 0.6.0
artifacts predate both the inherited-interface and diamond fixes and must be rebuilt.
Generator canonicalization of equivalent callable signatures is still unimplemented and needs
catalogue/ownership design before moving declarations to shared ancestors.

Catalogue inspection confirms that per-run signature coalescing cannot by itself solve unrelated
base declarations from independently compiled producers: their nominal ancestor ownership is
already fixed. Shared-root generation remains an open design task, not an implemented guarantee.

A separate exported-value regression is now fixed: `const identity: Identity` previously emitted
a generic static method, so `let imported: Identity = Exports.identity` failed F# with FS0002.
shape-exports keeps non-function callable values on the invocation-owned interface route as
typed properties. Function declarations retain methods. The lab's source assertion and catalogue
inheritance test pass, and fresh source/DLL scratch runs pass 22 runtime checks, including native
import identity and repeated int/string invocations. This uses local Fickl, not released-package
validation. NuGet still lists Fickl.Core only through 0.5.4. The correct test prefix is
`generic callable interfaces.generic calls retain`; filters with an extra fixture segment or
only the leaf name select zero tests and provide no evidence.

Review and a live anonymous-const probe found that export shaping alone would introduce
`obj -> obj`: anonymous naming previously excluded every exported value as signature-shaped.
Spec.isPolymorphicCallableValue now supplies the same classification to export shaping and
anonymous naming. The permanent lab adds anonymousIdentity; fresh source and DLL consumers
pass 25 runtime checks including repeated anonymous export calls and identity. The separate
overloaded-const probe retains the existing dropped-bound/overload findings, but no longer
introduces TR013. Broader corpus verification and review of the naming change remain required.

The follow-up review found a shared-ID edge: a function export and a const of `typeof` that
function contribute the same callable type ID. The permanent lab now includes declaredIdentity
and identityAlias; its no-obj assertion failed before subtracting retained value IDs from the
complete signatureShaped exclusion set. This latest alias change still needs refreshed runtime
and selected corpus evidence; the 25 runtime checks above predate it.

Fresh verification after the shared-ID fix passes 28 runtime checks in source and DLL modes,
including identityAlias at int/string and reference equality with declaredIdentity. The generated
lab reports 9 exact, 13 ergonomic, zero widened/escape. SolidJS and Animejs regenerate and compile
under .NET (zero warnings/errors) and local Fickl. SolidJS is 71/193/81/1 with TR013=2,
TR031=12, TP003=1, RA003=0; Animejs remains 126/188/26/6 with TR013=0, TR031=18.

The two remaining SolidJS TR013 findings are isolated under tests/.scratch/identity-alias-probe.
`type Store<T> = T` emits `type Store = obj`; `type NoInfer<T extends any> =
[T][T extends any ? 0 : never]` emits `type NoInfer = JS.NoInfer<obj>`. Function uses of both
retain their method binder. This confirms an alias-declaration scope defect independent of
generic callable lowering; the reproduction reports two widened declarations and two TR013s.
It is the next concrete binder-loss candidate, not an established F# representational limit.

F# directly accepts `type Store<'T> = 'T` (tests/.scratch/identity-alias-probe/Check.fsx).
The declaration path in shape-aliases obtains binders through declTypeParams, which reads
TypeResponse.ParameterTypeIds, AliasTypeArguments and hoisted DeclParams. These do not retain
the source alias's erased binder list. Resolve.declarationHasTypeParameters already reads
TypeAliasDeclaration.typeParameters from an authenticated symbol declaration for customization;
extend that declaration-owned path rather than mutating shared TypeFacts or inferring binders
from body traversal. The latter loses unused parameters and their original order (footguns:
Early projections must precede loss). ExportTypeIds/ResolveModel and ShapeModel currently have
no declaration-owned binder list. Any model change needs an FCS impact check first.

Added permanent identity-alias-lab and the focused identity-alias declaration regression in
Pipeline.test.fs. This is intentionally red until the resolver/shaper fix lands; no golden has
been generated. Do not claim the working branch's full suite is green. The callable slice's
runtime/corpus evidence remains as recorded above.

Implemented declaration-owned alias metadata: ExportTypeIds.AliasParameters is optional ordered
parameter IDs. FCS impact reported 43 sites in 18 generator files, not public API. The new
resolve-alias-parameters pass runs after exported type discovery and before type-table derivation;
it reads the typed alias AST, asks the checker for each parameter, and seeds those responses into
the ordinary frontier. ShapeAliases uses the defining export's parameter list. Missing source or
invalid declaration handles produce the existing unresolved-facet finding. Synthetic test export
records use None (28 Shape records and four Ownership records).

The identity-alias source regression is now green. A consumer test checks Store<string>,
Store<int> and NoInfer<int> against their underlying F# types; its first attempt had an incorrect
fully qualified support name (NoInfer is Fable.Core.JS.JS.NoInfer), corrected to assert the actual
underlying int contract. Still required: source-order/unused-parameter/shared-body negatives,
catalogue ownership, selected corpus, resolver suite, review and complete gate. No new golden
has been generated and this incremental implementation is uncommitted.

Expanded the alias lab with Plain/Phantom<T> sharing a literal body, reordered tuple binders,
and a const Store sharing the type alias's symbol. Both source and compiled-consumer tests pass.
Secondary aliases not already declared by an earlier shape pass now read their own source
parameters and body instead of inheriting the primary alias's arity; unused binders receive the
existing RA006 erased-phantom treatment. Reverse declaration order and catalogue reuse still
need explicit coverage.

Follow-up coverage now includes reverse declaration order and a compiled catalogue consumer.
Review found ExistingPhantom<T> = Existing still lost T when Existing was already shaped;
the regression failed first, then passed after applying the alias-owned parameters regardless
of the primary declaration's shape status. Named carriers retain the primary reference.
The reverse-order case also exposed a needless obj carrier in the existing phantom path;
that path now preserves its computed carrier. Three focused alias tests and 186 shaping tests
pass. Independent review found no further concrete issue in these changes. Full regeneration,
broader gates and released-Fickl validation remain outstanding; changes are uncommitted.

The alias increment is now isolated in `.claude/worktrees/alias-declaration-binders`, branch
`feat/alias-declaration-binders`, based on merged PR #167. This is the active bottom slice;
the callable-interface work stays in this original worktree awaiting published Fickl 0.6.0.
The isolated lab passed all five tests on released dependencies, and a direct shape-pass
regression covers absent, empty and populated declaration-owned binder lists. Independent
review confirmed the split has no dependency on CallableInterface.

The first full write/check/runtime command stopped during its write pass: 1,532 generator
tests ran (1,527 passed, one ignored, four failed, one errored). Three expectations still
described now-fixed binder losses (array-shape, tag-map, type-scope); these were corrected.
The two catalogue checks exposed stale Core.TS API hashes. Regeneration also revealed phantom
secondary aliases moving out of compiler-library ownership: Pipeline.secondaryAliasOrder now
accepts FsPhantom's preserved source order, with a direct regression. Compiler-lib regeneration
and renewed checks are required before committing. The original mixed worktree does not yet
contain these later ownership/test updates; merge/rebase from the completed bottom slice.

The independent bottom slice is committed as `4f16f4cec1b197504dd8f64f5931cdb2a9847b30`
and submitted with gh stack as PR #168. All 142 golden tests passed. After the ownership fix
and formatted Core.TS regeneration, the full gate passed 1,533 generator tests, 105 Wire tests
(one ignored in each), 620 runtime checks and Partas. Source and generated diffs passed review.
CI is pending; gh stack's initial title was corrected to the conventional breaking-fix title.
Do not claim the callable-interface increment is part of this PR or publicly released.

PR #168 passed all CI shards and cross-platform catalogue checks, then merged as
`9e278bf2c76d5012971f536ab2654a7d0e7816be`. gh stack merge did not recognise the single-PR
stack, so gh pr merge used the reviewed head SHA. The current callable work now lives in
`.claude/worktrees/generic-callable-interfaces` on `feat/polymorphic-callable-interfaces`,
based directly on that merge. The original d2 worktree remains an untouched source snapshot
for the transfer. Three-way application retained the merged alias fixes; the new alias unit
test's interface literal needed Callable=false. All 187 shaping tests and the callable source
regression pass on the new base. Fresh NuGet inspection still lists Fickl.Core only through
0.5.4. Published dependency validation, complete callable goldens/runtime gates and commit/PR
remain outstanding. The old scratch runtime evidence predates this transfer.

Fresh runtime proof on the transferred branch regenerates the callable lab with zero widened
or escaped symbols (9 exact, 13 ergonomic). All 28 checks pass from generated source and all
28 pass from a bindings DLL using the local Fickl 0.6.0 compiler. The scratch projects now
reference this worktree's support DLLs, not the original d2 worktree. Permanent runtime-gate
registration and published-package validation remain to be completed.

Fickl release artifacts have now been rebuilt from merged main
`b32f1707b130c588e2292b1912c8f15c0d0dd8c7` on local branch `release/repack-0.6.0`
in the Fable erased-case-tests worktree. `publish --pack-only` succeeded and validated all
13 packages under `temp/release-0.6.0`; representative Core, portable-tool and win-x64 nuspecs
all record that commit. This replaces the stale pre-inheritance payloads. Nothing was uploaded;
publishing still requires valid NuGet credentials. The package-only log is `temp/repack-0.6.0.log`.

Permanent GenericCallableInterfaces runtime registration now contributes all 28 checks to the
normal gate. Tool/Core pins are updated to 0.6.0 in this uncommitted branch. The scratch script
`tests/.scratch/run-package-preflight.ps1` adds the staged local feed only inside try/finally,
uses `tests/.scratch/nuget-package-preflight` as an isolated cache, and restores NuGet.Config
byte-for-byte afterwards. Six callable fixture tests passed in write/check phases, 648 runtime
checks and Partas passed. All 13 tested package hashes are in fickl-preflight-packages.json.

Full preflight regenerated formatted Core.TS and all 143 goldens. Core.TS's ClassDecorator and
MethodDecorator retain invocation generics; removed inherited members remain available through
their instantiated bases. Independent review of Core.TS and the other changed goldens found
no actionable loss. The full generator suite ran 1,543 tests: 1,538 passed, one ignored, five
failed. Four were old-output expectations in callable-overloads, setter and type-scope labs;
the strengthened replacements and neighbours pass in 11 focused tests after a second successful
143-golden pass. The remaining fresh Core.TS package-consumer test deliberately restores from
its own feed configuration and cannot obtain unpublished Fickl.Core 0.6.0. It was not bypassed.
The full gate therefore remains unproven until publication and a clean rerun.

Consumer docs now explain generic callable values and construction. SolidJS's current counts
relative to merged #168 are 72/190/79/1 -> 72/194/79/1; Animejs remains 126/188/26/6.
Callable-overloads moves 4/5/2/0 -> 6/4/1/0 and setter-lab 0/0/5/0 -> 0/2/4/0.
No jobs remain running. NuGet.Config has no diff. Do not commit or create the callable PR until
the published dependency gate can pass; the earlier alias PR #168 is already merged.

Blocked audit: the publication dependency has persisted across more than three consecutive
goal turns while the available implementation, package preparation and local validation were
completed. A fresh NuGet flat-container lookup still lists only 0.5.2/0.5.3/0.5.4, and
FICKL_NUGET_KEY is absent from both process and user environments. The previous rejected key
was not retried. The current slice cannot complete its fresh-package gate or merge without
publication; do not bypass that gate or move attention to a higher PR slice.

Resume after valid publish credentials are available, or after Fickl 0.6.0 is published.
Validate the staged package hashes, publish through Fickl.Build when credentials are supplied,
confirm NuGet availability, and run the complete Xantham gate with public feeds and a fresh
package cache. Then review, commit, submit through gh stack, monitor only that PR and merge
after green checks. Current source, fixtures, generated output and docs remain uncommitted in
generic-callable-interfaces. No background processes or monitors are running.

Resume attempt: the user supplied a key and reactivated the goal. It was passed through a
masked interactive prompt into a process-only environment variable, never saved in a file.
The publication pipeline rebuilt/validated packages, then NuGet rejected the first upload,
Fickl.AST.0.6.0, with HTTP 403 (invalid, expired, or insufficient package permission). The
pipeline stopped; no package was uploaded. A fresh public Core index still ends at 0.5.4.
The process secret was cleared in finally. Do not retry this key without evidence of corrected
permissions or a replacement. This is the first blocked-audit turn after explicit resumption.
The sanitized failure is in Fable's temp/publish-0.6.0-attempt.log; packages were repacked, so
refresh the staged archive checksums before the next authorized publication attempt.

SolidJS exposed Transition as a merged value/type symbol. The resolver now filters declaration
handles by the typed AST's actual TypeAliasDeclaration kind before requiring one alias. Metadata
failures return Error through the existing finding path (attempt catches wire failures, not generic
failwith). The merged-symbol regression was red before this fix. Fresh SolidJS counts are
72 exact, 194 ergonomic, 79 widened, 1 escape; TR013 is zero, with no new RE001. Animejs's earlier
regeneration in this increment remained 126/188/26/6. Final ownership review remains pending.

PR #166 merged as d7cfebe after all CI checks passed. Fresh generator FCS check is clean.
The existing footguns record identifies objectRef's callback-before-instantiation ordering;
current source confirms it. Solid-js's Store.StoreReturn repeats callback shaping and TR013,
while its named callback declaration already exists. Baseline findings are captured in
tests/.scratch/fidelity-solid-before.log in the main checkout.

The first full gate exposed stale Core.TS catalogue API hashes for Transformer and UnderlyingSink.
Regenerating compiler-lib updates those hashes and named callback references in DOM stream
interfaces. Independent review found no actionable issue with that additional generated diff;
the complete rerun passed 1,526 generator tests, 105 Wire tests (one ignored in each suite),
620 runtime checks and the Partas gate, with pipeline exit code zero.
