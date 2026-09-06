---
category: Generator
audience: managing agent
title: Handover - lane CJ, wave fourteen (TR008 reachability recon)
---

# Lane CJ — how much of TR008 is reachable

Read-only. Base `8d3a4fa`, branch `worktree-gen-wave14-cj`. No files under `src/` or `tests/`
touched.

## Verdict: essentially none of the 574. The tier is real `any`, not a mapping gap.

The raise site is one line, `Shape/Spec.fs:1292-1293`, inside `typeRefOnPath`: whenever a type's
own `TypeFlags` has `Any` set, the function returns `FsObj` and raises `TR.AnyToObj`
unconditionally — no other fact in scope (siblings, generics, index signatures) is consulted at
that arm. So the question is entirely upstream: at how many of the 574 call sites does the TS
checker hand `typeRefOnPath` an `Any`-flagged type *because* something else in the same
declaration already pins a better one nearby. Working fixture by fixture, by finding count
(animejs 350, `@cloudflare/workers-types` 166, solid-js 42, labs 16 — sums to 574, confirmed
against `symbols.jsonl` per fixture):

| cluster | sites | recoverable? |
| --- | ---: | --- |
| 1. animejs — `Callback<T>`'s extracted-method return `any` | 295 | declined |
| 2. animejs — plain untyped JS surfaced through tsc's declaration emit | 53 | declined |
| 3. animejs — union collapsed by an unresolved ambient/lib member | 2 | flagged, not chased |
| 4. cloudflare — "accepts anything" stdlib idiom (variadic rest, `reason`/`error`, plain `any` fields) | 157 | declined |
| 5. cloudflare — generic left at its own `<R = any>` default | 6 | declined |
| 6. cloudflare — call-signature residue with no visible `any` in either overload | 3 | flagged, not chased |
| 7. solid-js — internal reactive-graph state (`Computation<any>`, `context: any \| null`, …) | 42 | declined |
| 8. labs — same three shapes as clusters 1/4, pinned in miniature | 16 | declined |
| **total** | **574** | **0 recoverable** |

## 1. animejs — `Callback<T>`'s extracted-method return, 295 sites. Declined.

`animejs`'s `.d.ts` is tsc-emitted from JSDoc, and every lifecycle callback (`onBegin`,
`onBeforeUpdate`, `onUpdate`, `onLoop`, `onPause`, `onComplete`, `onRender`) is typed through one
alias:

```ts
export type Callback<T> = {
    method(self: T): any;
}["method"];
export type TickableCallbacks<T> = {
    onBegin?: Callback<T>;
    onUpdate?: Callback<T>;
    onComplete?: Callback<T>;
};
```

The return position is `any` by construction — the author's own signal that a callback's return
value is discarded. `TickableCallbacks<T>` is intersected onto `AnimatableParams`,
`AnimationParams`, `AutoLayoutParams`, `JSAnimation`, `Timer`, `Timeline`, and more, so **one
authoring decision is what the corpus counts 295 times**, once per `(declaring type, callback
name)` pair. Nothing else in any of those declarations disagrees with `any` — there is no second
arm, no `keyof`, no call signature elsewhere pinning the return. Declined: this is `any` as
written, not `any` as a fallback.

**Repro (7 lines):**

```ts
export type Callback<T> = {
    method(self: T): any;
}["method"];
export type TickableCallbacks<T> = {
    onComplete?: Callback<T>;
};
```

Price if the manager still wants to move it: this is a policy call, not a constraint recovery —
"a callback's own return position maps to `unit` rather than `obj`" for the specific shape
`{ method(...): any }["method"]`. That would touch `Shape/Callbacks.fs` (or wherever the
extracted-method form is unwrapped before `typeRef` sees it) and moves all 295 sites plus every
future fixture that uses the same TS idiom at once.

## 2. animejs — plain untyped JS, 53 sites. Declined.

The rest of animejs's `any`s are literal, unpaired, and untyped in the same declaration:
`DOMProxy.el`/`.parentElement`/`.x`/`.y`/`.width`/`.height` (getter and setter both `any`),
`DOMProxy.GetBoundingClientRect.Result.{top,right,bottom,left}`, adapter-registry callback
parameters (`(t: any) => boolean`), `Globals.Editor._head`/`_tail`, `Scope.data:
Record<string, any>`, `TweakRegister.defaultValue`, `Tween._value: string | number | any` (the
checker itself collapses this union to `Any` before the generator ever sees it — the `string |
number` siblings are gone by the time `typeRefOnPath` runs), and similar. None carry a sibling
overload, `keyof`, or neighbouring member that types the same value differently.

**Repro (5 lines, the class-field shape):**

```ts
declare class DOMProxy {
    el: any;
    set x(v: any);
    get x(): any;
}
```

Declined — this is a plain-JS library's tsc-emitted output, no second source of truth exists.

## 3. animejs — unresolved ambient member, 2 sites. Flagged, not chased.

`Engine.reqId: number | NodeJS.Immediate` and `DrawableSVGGeometry` (`SVGGeometryElement &
{...}`, reaching `className`) both read as `Any` — the first because `@types/node` is absent from
this fixture's `node_modules` so `NodeJS.Immediate` cannot resolve, the second because
`className` comes from `SVGGeometryElement`, a DOM-lib interface member. Both point at the same
mechanism as batch two's deferred item 2, "resolve tier does not follow lib interface members"
(owned by lane CO next batch) — not a TR008-specific defect, and I did not chase it further per
the fixture rule.

## 4. cloudflare — "accepts anything" stdlib idiom, 157 sites. Declined.

`Console.log/.warn/.error/...(...data: any[])`, `AbortSignal`/`AbortController`/`DurableObjectFacets`
`.abort(reason: any)`, `WritableStreamDefaultController.error(reason: any)`,
`KVNamespacePutOptions.metadata: any | null`, `DiagnosticChannelEvent.message: any`, and a long
tail of one-off plain fields (`ContainerExecOptions.stdin`, `Body.body`, `Buffer`,
`AiModelListType`, …) plus `Table.get(index): any` / `Global.valueOf(): any` /
`EventTargetHandlerObject.handleEvent(): (event: Event) => any | undefined` /
`WebSocket.deserializeAttachment(): any | null` (verified: each of these last four has no other
overload of the same name in scope — the `any` is the whole signature).

**Repro (4 lines):**

```ts
interface Console {
  log(...data: any[]): void;
}
```

Declined — deliberate "this API takes anything" idiom, no sibling constrains any of it.

## 5. cloudflare — generic left at its own default, 6 sites. Declined.

`Blob.stream()`, `File.stream()` (inherited, unoverridden), and four `KVNamespace.get()` /
`.getWithMetadata()` overload arms for the `"stream"` variant all return `ReadableStream` (or
wrap it), and `ReadableStream<R = any>` — the type parameter's own default is `any`. The call
sites supply no type argument, so the default is what reaches `typeRefOnPath`.

**Repro (6 lines):**

```ts
interface ReadableStream<R = any> {}
interface Blob {
  stream(): ReadableStream;
}
```

Declined for the same reason as cluster 1: the default lives on `ReadableStream`'s own
declaration, not on any of these six call sites, so there is nothing local to substitute. Note for
whoever eventually prices `ReadableStream<R = any>` itself: fixing the default's binding (`R`
should probably stay a bound type parameter rather than resolve through its default at every
unparameterized use) is a `Model.fs`/lib-binding question, out of this lane's scope, and would
move this exact cluster wherever `ReadableStream` is referenced bare across every fixture, not
only these six.

## 6. cloudflare — unexplained call-signature residue, 3 sites. Flagged, not chased.

`AiSearchInstance.chatCompletions()`, `AiSearchNamespace.chatCompletions()`, and `Ai.run()` are
`TR.AnyToObj` findings I could not trace to a visible `any` in either overload I found under
those names — `chatCompletions`'s two overloads return `Promise<ReadableStream>` and
`Promise<AiSearchChatCompletionsResponse>` (no bare `any`), and the `run(...)` I found nearest the
symbol (`AiGateway.run`, `WorkflowEntrypoint.run`) does not match the flagged owner at all —
`@cloudflare/workers-types` almost certainly declares a distinct `Ai.run` overload set elsewhere
in the ~30k-line file that I did not locate without a wider read. Per the fixture rule, writing
this pointer down rather than loading more of the file to chase it: **someone should re-grep
`symbols.jsonl` for the exact owner id these three findings carry and locate the matching
declaration before assuming this is the same generic-default mechanism as cluster 5.**

## 7. solid-js — internal reactive-graph state, 42 sites. Declined.

`Owner.owned: Computation<any>[] | null`, `Memo.observers`, `TransitionState.{sources,effects,
promises,disposed}`, `*.context: any | null`, `SharedConfig.{effects,resources}`, and the
`Callback<T>`-shaped return positions on `WriteSignal`, `ExternalSource.track`, `MapArray.Options.
fallback`, etc. `Computation<Init, Next extends Init = Init>` is itself generic, and every one of
these fields instantiates it as `Computation<any>` — solid's own choice to type its internal
scheduler loosely rather than thread the value type through. No sibling in any of these
declarations types the same slot more precisely.

**Repro (4 lines):**

```ts
export interface Computation<Init> {}
export interface Owner {
    owned: Computation<any>[] | null;
}
```

Declined — same reasoning as cluster 2, applied to a different library's internals.

## 8. labs — 16 sites, three already-known shapes. Declined.

- `uninhabited-intersection-lab` (10): every site is `Timer.then(callback?: (self: ...) => any):
  Promise<any>` — cluster 1's shape, hand-written rather than tsc-emitted. Confirms cluster 1's
  read is right: the lab's own comment calls this a deliberate pin, not a defect.
- `inherit-lab` (2) and `member-shape-lab` (2): `Deferred.then`/`.catch(onrejected: any)` —
  `Promise`'s own standard-lib rejection-handler idiom, cluster 4's shape.
- `optional-param-lab` (1): `markedAny(a: string, b?: any): string` — a lone unpaired `any`
  parameter, purpose-built to pin something else (marking optional `any` params), not a
  constraint-recovery candidate.
- `literal-overload-lab` (1): `Widen.scan` is overloaded as `scan(input: unknown)` and
  `scan(input: any)` — the closest thing in the corpus to "a sibling overload exists." The lab's
  own comment calls this **a deliberate negative**: "a collision no literal is party to, which
  drops an overload as it always did." The `any` arm is the one that survives collapsing today,
  and the lab exists to pin that outcome, not to flag it as recoverable. Left as designed.

## What I could not settle

- Cluster 6's three cloudflare sites (`Ai.run()`, both `chatCompletions()`s) — owner declaration
  not located without a wider read of a 30k-line file; see §6 for the exact re-grep to run.
- Cluster 3's two sites overlap batch two's deferred lib-interface-following item; not this
  lane's to fix, noted so lane CO's pricing accounts for it if relevant.

No implementation lane is opened by this recon. The corpus-wide read: `TR.AnyToObj` is almost
entirely real, author-intended `any` (callback returns nobody reads, stdlib "accepts anything"
signatures, and generic defaults), not a local-information gap the generator is failing to use.
