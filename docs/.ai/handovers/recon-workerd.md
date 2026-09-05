---
category: Generator
audience: managing agent
title: Recon R1 — how workerd discovers entrypoint handler methods
---

# Recon R1 — discovery mode `workerd` performs

**Answer: property access, not key enumeration.** `workerd` reads each optional
hook (`fetch`, `alarm`, `connect`, `scheduled`, `queue`, `tail`, `trace`,
`tailStream`, `test`, `webSocketMessage`, `webSocketClose`, `webSocketError`,
`email`, …) off the constructed handler object with V8's named-property `Get`
(`in->Get(context, fieldName)`), one call per known field name. This walks the
JS prototype chain exactly as `obj.fetch` would in JS — it does not call
`Object.keys`, `Object.getOwnPropertyNames`, or `hasOwnProperty` anywhere in
this path. Lane AA's opt-in-interface form is therefore compatible with the
runtime as emitted: a hook implemented via an F# interface member lands on the
class prototype, unmangled (per lane Z), and `in->Get` finds it there the same
way it finds a hand-written class method.

## Provenance

Read live via `gh api` against `cloudflare/workerd` (no clone left in the
worktree). Repository default branch tip at time of read:
`dd8133e9b9656fb39f1434247a80aa7a249ee204` (`main`). Files fetched at that
ref's content (unpinned `contents` API call resolves to the default branch
head, i.e. this commit):

- `src/workerd/api/global-scope.h` (blob defining `ExportedHandler`)
- `src/workerd/jsg/struct.h` (`JSG_STRUCT` unwrap machinery)
- `src/workerd/io/worker.c++` (entrypoint-class construction path)
- `src/workerd/io/worker.h` (`EntrypointClass` type alias)

## Citations

**1. The struct every handler funnels through** — `src/workerd/api/global-scope.h:495-522`:

```cpp
struct ExportedHandler {
  using FetchHandler = jsg::Promise<jsg::Ref<api::Response>>(jsg::Ref<api::Request> request,
      jsg::Value env,
      jsg::Optional<jsg::Ref<ExecutionContext>> ctx);
  jsg::LenientOptional<jsg::Function<FetchHandler>> fetch;
  ...
  using AlarmHandler = kj::Promise<void>(jsg::Ref<AlarmInvocationInfo> alarmInfo);
  jsg::LenientOptional<jsg::Function<AlarmHandler>> alarm;
  ...
```

Every hook is a `jsg::LenientOptional<jsg::Function<...>>` field on one
`JSG_STRUCT`, `ExportedHandler`. This struct is the shared target for both the
old module-format `export default { fetch(){} }` object and the newer
class-based entrypoints (see citation 3) — there is exactly one discovery
mechanism, not one per era or per entrypoint kind.

**2. How a struct field is read off a JS object** — `src/workerd/jsg/struct.h:133-155`
(`FieldWrapper::unwrap`, the per-field routine `JSG_STRUCT` generates for each
member such as `fetch`/`alarm`):

```cpp
Type unwrap(TypeWrapper& wrapper,
    v8::Isolate* isolate,
    v8::Local<v8::Context> context,
    v8::Local<v8::Object> in) {
  static_assert(NotV8Local<Type>);
  auto& js = Lock::from(isolate);
  auto fieldName = nameHandle.Get(isolate);
  v8::Local<v8::Value> jsValue = v8::Undefined(isolate);
  if (!js.isJavascriptExecutionDisallowed()) {
    jsValue = check(in->Get(context, fieldName));
  } else {
    // Safe path to get a v8::Value under the `DisallowJavascriptExecution` scope without
    // walking the prototype chain, hence skipping any Object.prototype getters
    if (check(in->HasRealNamedProperty(context, fieldName))) {
      jsValue = check(in->GetRealNamedProperty(context, fieldName));
    }
  }
  return wrapper.template unwrap<Type>(
      js, context, jsValue, TypeErrorContext::structField(typeid(Struct), exportedName), in);
}
```

The normal-execution path is `in->Get(context, fieldName)` — a literal
named-property `Get`, one call per field name (`"fetch"`, `"alarm"`, ...),
which walks the prototype chain like ordinary `obj.fetch` access. The
`DisallowJavascriptExecution` fallback (`HasRealNamedProperty` /
`GetRealNamedProperty`) is also name-keyed, not an enumeration; it exists only
to avoid running user getters during that restricted scope. Neither branch
calls `Object.keys`, `Object.getOwnPropertyNames`, or `hasOwnProperty`.

**3. Class-based entrypoints go through the same struct, uniformly across all
three kinds** — `src/workerd/io/worker.h:76-78`:

```cpp
using ExecutionContextOrState =
    kj::OneOf<jsg::Ref<api::ExecutionContext>, jsg::Ref<api::DurableObjectState>>;
using EntrypointClass =
    jsg::Constructor<api::ExportedHandler(ExecutionContextOrState ctx, jsg::Value env)>;
```

`EntrypointClass` is one alias used for `WorkerEntrypoint`, `DurableObject`,
and `WorkflowEntrypoint` alike (`EntrypointClasses` in the same header lists
`workerEntrypoint`, `durableObject`, `workflowEntrypoint` as parallel
`jsg::JsObject` constructor handles, all consumed as `EntrypointClass`). Its
return type is `api::ExportedHandler` — the JSG constructor machinery runs the
user's class constructor, then unwraps the produced instance into
`ExportedHandler` via the exact `FieldWrapper::unwrap` routine quoted above.
Confirmed at the call site, `src/workerd/io/worker.c++:4004-4023`
(`Worker::Actor::ensureConstructedImpl`, the Durable Object construction path):

```cpp
auto handler =
    info.cls(lock, ctx.addRef(), KJ_ASSERT_NONNULL(lock.getWorker().impl->env).addRef(js));
...
handler.env = js.v8Ref(js.v8Undefined());
handler.ctx = kj::none;
handler.missingSuperclass = info.missingSuperclass;
...
impl->classInstance = kj::mv(handler);
```

`info.cls(...)` is the `EntrypointClass` constructor call; its result,
`handler`, is already a plain `api::ExportedHandler` — the class instance was
converted into that struct (per-field `Get`) inside the constructor call
itself. The stateless-class path (`WorkerEntrypoint`/`WorkflowEntrypoint`) is
the same shape at `worker.c++:2449-2464`
(`getHandlerFromEntrypointClass`): `cls(js, ..., env)` again produces an
`api::ExportedHandler` directly.

There is no branch anywhere in this call graph that special-cases
`DurableObject` vs. `WorkerEntrypoint` vs. `WorkflowEntrypoint`, or that
special-cases the class-constructor path vs. the plain default-export-object
path — both terminate in the identical `ExportedHandler` struct-unwrap.

## What this means for lane AA

Lane AA's form works as emitted. A hook implemented as an F#/Fable interface
member (unmangled, per lane Z's half of this question) is an ordinary property
on the transpiled class's prototype (or own property, for arrow-style
assignment — either is found by `Get`, which walks the prototype chain).
`workerd`'s handler discovery is `in->Get(context, "fetch")` /
`in->Get(context, "alarm")` / etc. — literal property reads by name, identical
in effect to `"fetch" in obj` or `obj.fetch` in JS, and blind to whatever
`Object.keys`/`hasOwnProperty` would or would not report. No generator change
is required; the platform-side assumption the wave-six handover flagged as
open is settled in the favorable direction.

This holds uniformly across `WorkerEntrypoint`, `DurableObject`, and
`WorkflowEntrypoint` (one shared `EntrypointClass` alias, one shared
`ExportedHandler` unwrap), and uniformly across the older
`export default { fetch(){} }` module-format object and the newer class style
(both terminate in the same `ExportedHandler` `JSG_STRUCT`). Lane AA emits the
class style; that is the style directly exercised by citation 3, but the
discovery mechanism does not differ by style.

## What could not be established

- This traces the C++ discovery path in the `workerd` runtime source; it was
  not confirmed against a deployed Workers probe (task instructions permitted
  either; source reading was sufficient to answer with a direct citation, and
  no repository clone or deployed sandbox was available from this lane).
- `jsg::Function<T>`'s own unwrap (converting the `v8::Value` pulled by `Get`
  into a callable) was not separately audited; it is out of scope for the
  access-vs-enumeration question, since the field is already selected by name
  before that conversion runs.
- Whether V8's `Get` on the field name can be intercepted by a `Proxy` or a
  custom getter that itself inspects `Object.keys` internally was not
  investigated — this only matters for handwritten JS entrypoints doing
  something unusual, not for Fable-emitted classes, which is the case lane AA
  and this recon are about.
