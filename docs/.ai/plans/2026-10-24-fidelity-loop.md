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

**Tech stack:** Existing TypeScript compiler pin, F#, Fickl 0.5.4 and Myriad customization APIs.

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
- [ ] Commit and submit through gh stack.

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
