# W9 (#140): resolving deferred conditionals early — findings

Experiment record for workstream W9 of
[`2026-10-13-widening-escape-reduction-design.md`](2026-10-13-widening-escape-reduction-design.md).
Base: `agent/w3-node-catalog-reuse` at 7d4fa2f (develop plus format, W2, W4 and W3). Branch:
`agent/w9-early-resolve-experiment`. Finding code taken: `TR068`
(`TR.ConditionalResolvedToConstraint`, Widened).

## Question

W9.1 asks whether a pass before Resolve can obtain the resolved type of a reference whose alias
is generic and whose arguments are concrete (`InstanceType<typeof IncomingMessage>`), so that
later passes read `IncomingMessage` instead of the `obj` that `TR045` reports. The deliverable
is a verdict on whether the approach holds, what it costs and what it risks.

## Premise check

1. **A concrete application never reaches the shaper as a conditional.** The checker resolves
   it eagerly. `conditional-lab` already pins `type Decided = string extends string ? number :
   boolean` arriving as `float` with no finding (`generator-type-mapping.md` §4.11).
   `early-resolve-lab` adds `type ConcreteListener = (req: InstanceType<typeof
   IncomingMessage>) => void`, which renders `(IncomingMessage -> unit)` with no finding under
   either setting. Testcase (b) pins it with the flag off.
2. **The `InstanceType` sites are applications over a bounded type parameter.** All 130
   `InstanceType` `TR045` findings in `src/Xantham.Fable.Node/symbols.jsonl` (90 distinct
   symbols, in `http`, `https` and `http2`) are `InstanceType<Request>`, where the parameter is
   declared as `Request extends typeof IncomingMessage = typeof IncomingMessage`
   (`@types/node/http.d.ts`). The argument is the bounded parameter, not a concrete type.
3. **Why the existing logic leaves them deferred.** Resolve's conditional derivation asks
   `isTypeAssignableTo(check, extends)`. `InstanceType`'s extends type contains `infer R`, so
   the answer is false, and the false branch is `any`, not `never`. No branch is taken, and
   `Spec.conditionalRef` writes `obj` under `TR045`.

W9.1 as written therefore targets an empty set. The question that does have sites is: can a
conditional deferred over a **bounded** type parameter be written at that bound?

## Approach

The checker answers this through `getBaseConstraintOfType`. For a conditional whose check type
is a naked, constrained type parameter, the checker's distributive constraint substitutes the
constraint for the parameter and re-instantiates the conditional:
`InstanceType<Request>` gives `IncomingMessage`. Without a distributive constraint it falls back
to the default constraint, which is the union of both branches. §4.11 rejects that union.

The read sits inside Resolve's existing conditional derivation (`deriveFacts`), not in a
separate pass before Resolve. This deviates from W9.1's wording for two reasons:

- The conditional types are found by Resolve's frontier walk. A separate pass would have to
  repeat that walk to find them.
- `getTypeAtLocation` on the written reference returns the same deferred conditional that
  Resolve already holds. A pass before Resolve learns nothing new from it.

The prototype is opt-in: `resolveConditionalConstraints` in `xantham.json`, `false` by
default. When the flag is set, Resolve records `ConditionalFacts.Constraint` if all of these
hold:

- either no branch is taken, or the taken branch is an `infer` variable (`TypeParameter`);
- the peeled check type is a type parameter that `getConstraintOfTypeParameter` bounds;
- the conditional has no alias, or its alias's declared type is a different type (an alias's
  own declaration body stays deferred);
- `getBaseConstraintOfType` returns a type that is not the conditional itself, either branch,
  `any`, `unknown`, `never`, another conditional, or a union other than `boolean`.

`conditionalRef` tries the taken branch first, then the constraint. A constraint that maps to
`obj` falls back to `TR045`. A mapped constraint reports `TR068` (Widened). The flag joins the
catalogue inference profile when set, and only then, so an unset profile hashes as it did
before the flag existed.

## Results

Every run below generates with the CLI into `tests/.scratch/w9/`, with `XANTHAM_TSGO_EXE` set to
the main checkout's compiler. Each off-mode `manifest.json` is byte-identical to the committed
golden or package manifest, which is the control. "Strict" is the committed guard set.
"Permissive" is an earlier guard that refused a union only when it contained both written
branch ids. Symbol tiers (exact / ergonomic / widened / escape) did not move in any run.

| Package | TR045 off | TR045 on (strict) | TR068 on (strict) | Permissive TR045 / TR068 | Binding diff (strict) |
| --- | --- | --- | --- | --- | --- |
| `early-resolve-lab` | 5 `InstanceType` | 1 (`Exports.either`, union bound) | 4 | — | 4 `obj` → `IncomingMessage` sites |
| `@types/node` | 224 (130 `InstanceType`) | 93 (0 `InstanceType`) | 160 (159 `InstanceType`, 1 inline) | 43 / 210 | 101 lines replaced; compiles |
| `type-fest` | 154 (152 alias bodies) | 152 | 2 (inline, inside alias bodies) | 152 / 2 | none |
| `solid-js` | 56 (51 references) | 56 | 0 | 56 / 0 | none |
| Core.TS (T5, scratch) | 25 (`Awaited` 14) | 25 | 0 | — | none |

Notes on the rows:

- **`@types/node`.** All 130 `InstanceType` findings move to `TR068`. The extra 29 `TR068`
  findings are nested applications. `InstanceType<Http2Response>` resolves to
  `Http2ServerResponse<InstanceType<Http2Request>>`, and the inner application resolves in turn,
  so it reports a second `TR068` at the same symbol. The distinct symbols are the same 90. The
  inline site is `Test.MockFunctionContext.calls`. Two rendered samples:
  - `type RequestListener<'Request, 'Response when 'Request :> IncomingMessageConstructor> =
    delegate of req: IncomingMessage * res: obj -> unit` (was `req: obj`);
  - `type OnRequestHandler2<…> = delegate of request: Http2.Http2ServerRequest * response:
    Http2.Http2ServerResponse<Http2.Http2ServerRequest> -> unit` (was `obj * obj`).

  The strict-mode `Node.fs` compiles against Core.TS and Fable.Core 5.2.0 through a scratch
  project (`tests/.scratch/w9/nodegate`).
- **`res` stays `obj`.** `InstanceType<Response> & { req: InstanceType<Request> }` is an
  intersection with a conditional operand. The intersection pass widens it (`TR018`) before
  any operand is mapped, and the conditional's constraint is never consulted.
  `early-resolve-lab` pins the three `TR018` sites.
- **`type-fest`.** The two moved findings sit inside `Replace` and `StringRepeat`, whose bodies
  chain through nested conditionals. The alias body is emitted as an erased phantom (`SA002`)
  whatever the reference maps to, so the rendered binding is byte-identical and only the
  manifest moves.
- **`solid-js`.** Every conditional reference is over an **unbounded** parameter
  (`CustomPartial<T>`, 20 sites, and 26 inline conditionals). There is no distributive
  constraint, so nothing moves.
- **Alias-body mode** (the guard removed, measurement only). `type-fest` moves 2 more findings,
  `Join` and `Includes`, to `TR068`, with no `.fs` change: `shape-aliases` still emits the
  phantom, so no phantom becomes a parameter-dropping abbreviation. `RA001` stays 0, `SA002`
  stays 170, and the tiers are unchanged. `solid-js` and `@types/node` are identical to
  references mode. Only 4 of `type-fest`'s 152 alias bodies have a bounded, naked check type
  with a usable constraint. The rest test `IsAny<T> extends true` and similar, or test over an
  unbounded parameter.
- **Permissive mode.** It additionally resolves `Listener` (20), `PipelinePromise` (15),
  `PipelineCallback` (10), `Args` (2) and `ImplicitArrayBuffer` (1) in Node. Each result is a
  union, for example `listener: U2<(obj -> unit), (obj[] -> unit)>` and `__promisify__ … ->
  U2<JS.Promise<obj>, JS.Promise<unit>>`. Over a union bound, the distributive constraint is
  the union of each member's branch, and the written branches are generic, so their ids never
  match the instantiated members. That is the branch union §4.11 rejects, so the committed
  guard refuses every non-`boolean` union. The cost is `ImplicitArrayBuffer`'s
  `Buffer<U2<ArrayBuffer, SharedArrayBuffer>>` (the bound itself, `ArrayBufferLike`), which
  stays `Buffer<obj>`.

## Cost

- **Round trips.** Only conditional types with no usable branch pay, and only when the flag is
  set. Each pays one `getConstraintOfTypeParameter` (plus `getBaseTypeOfType` per substitution
  layer peeled). An aliased conditional with a bound adds one `getDeclaredTypeOfSymbol`. A
  conditional that passes the guards adds one `getBaseConstraintOfType`.
- **Table size.** The resolve counters are identical off and on for every package: Node 29,918
  types (widest generation 7,441), `type-fest` 3,757 (684), `solid-js` 18,372 (2,307). Each
  accepted constraint was already in the table.
- **Wall time** (CLI, strict guard, two runs each, off / on): Node 22.9 s, 19.5 s / 21.3 s,
  21.2 s; `type-fest` 4.1 s, 2.5 s / 3.4 s, 3.0 s; `solid-js` 6.4 s, 7.0 s / 6.5 s, 6.8 s. An
  earlier session with the permissive guard measured Node 21.4 s, 23.6 s / 25.1 s, 30.0 s. The
  spread between repeated runs is larger than any off/on difference, so no per-alias cache was
  added.

## Risks

- **The branch union.** For a conditional with no distributive constraint, the checker's
  constraint is the union of both branches. Over a union bound, the distributive constraint is
  the per-member branch union. Both look precise and are not. Refusing every non-`boolean`
  union covers both cases. Refusing either branch's own id covers the single-branch default
  constraint.
- **Alias bodies.** No phantom became an abbreviation in the measurement. `shape-aliases`
  decides the phantom independently of the reference. The guard stays: an alias body that did
  resolve could lose a parameter to arity repair (`RA001`), as `ExcludeStrict` did under the
  branch rung (§4.11).
- **The frontier width cutoff.** A constraint id missing from the table maps to `obj` through
  `typeRef`, and `conditionalRef` then reports `TR045`, not `TR003`.
- **The catalogue inference profile.** The flag is part of the profile when set, so a catalogue
  consumer must match its producer. Core.TS does not set it.
- **Lost parameter link (Widened).** `RequestListener<CustomRequest>` still reads `req:
  IncomingMessage`. A consumer passing a subclass of `IncomingMessage` must downcast. The bound
  is a sound upper bound in a covariant position (a value handed to a listener). In a
  contravariant position (a value the consumer hands in) it admits arguments TypeScript would
  reject.
- **Nested applications** resolve in turn and report one `TR068` each, so `TR068` counts
  findings and not symbols. Node: 159 findings over 90 symbols.

## Recommendation

**Keep it opt-in and enable it for `@types/node` only.** The mechanism holds for the sites W9
targeted:

- every `InstanceType` `TR045` in Node is removed;
- Node's binding compiles;
- the table, the tiers and the wall time do not move;
- the guards keep `type-fest` and `solid-js` byte-identical even with the flag on.

Do not make it the default yet. The selected goldens show that W9 cannot reduce `type-fest`'s
or `solid-js`'s `TR045`. Their conditionals are alias bodies or sit over unbounded parameters,
and no checker query answers those without the branch union. Enabling it for Node means adding
`"resolveConditionalConstraints": true` to `src/Xantham.Fable.Node/xantham.json` and
regenerating Node at integration. Node is not a catalogue consumer, so the profile does not
matter there.

Follow-ups this experiment surfaced, out of W9's scope:

- `TR018` intersections with a conditional operand (`res: InstanceType<Response> & {…}`, 3 lab
  sites and most of Node's `res` parameters) widen before the conditional is consulted.
- The CLI resolves its compiler as `XANTHAM_TSGO_EXE`, then the `xantham tsc init` cache, then
  the parent walk. In this worktree the cached compiler crashed on `type-fest` (`json: cannot
  marshal … +Inf` from `getDeclaredTypeOfSymbol`). Measurements must pin `XANTHAM_TSGO_EXE`.

## Alias-body measurement patch

Applied temporarily to `src/Xantham.Generator/Resolve.fs` for the alias-body rows, then
reverted:

```diff
                                             async {
                                                 let! declared = ctx.Session.getDeclaredTypeOfSymbol alias.Id
-                                                return declared.Id = ty.Id
+                                                return false && declared.Id = ty.Id
                                             }
```
