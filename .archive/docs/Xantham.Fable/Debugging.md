---
title: Debugging
category: Xantham.Fable
categoryindex: 1
index: 6
---

# XanthamTag Debug Capabilities

Xantham.Fable ships with a per-tag opt-in debug-tracing system designed for
diagnosing the AST traversal pipeline without drowning the console in noise.
Every `XanthamTag` carries a `Debug` flag and a stable `DebugId`; tracing
helpers are no-ops unless `Debug` is true *and* the build was compiled with the
`DEBUG` symbol.

> All helpers in this document live in
> `module XanthamTag` (`src/Xantham.Fable/Types/XanTag.fs`) and are guarded by
> `#if DEBUG`. In release builds they compile away to passthroughs returning
> the tag unchanged.

## Why a per-tag flag?

The reader processes thousands of nodes per `.d.ts`. A blanket trace dump is
unreadable. Instead, you "tag" a single node of interest (typically the one
producing a bug) and let the system propagate the `Debug` flag along the
relationships you care about &mdash; child traversals, shared symbols,
re-entries through caches. From that tag's seed point you see only the
processing it touches, with a colourised tracking ID linking every line of
output back to its origin.

## Core surface

### `Debug` and `DebugId`

```fsharp
type XanthamTag with
    member val Debug   : bool   // mutable, defaults to false
    member val DebugId : int    // stable counter, assigned at tag creation
```

`DebugId` comes from a process-wide counter (`Tracer.getDebugId`) and is the
short identifier rendered in yellow brackets in every trace line, e.g.
`[42]`. Two distinct tags never share an id within a run.

> **API note.** As of the structured-logging refactor, every debug
> helper is an **instance method on `XanthamTag`**. The previous
> module-level curried functions (`XanthamTag.setDebug tag`,
> `XanthamTag.debugLocation loc tag`, ...) have been removed; the
> equivalents are `tag.setDebug(...)`, `tag.debugMessage(...)`, etc.
> Callers no longer need to thread location strings through &mdash;
> `[<CallerFilePath>]` / `[<CallerLineNumber>]` are captured on every
> helper, so `file:line` lands in the JSON log automatically.

### Turning tracing on

```fsharp
type XanthamTag with
    member setDebug             : condition:(XanthamTag -> bool) -> XanthamTag
    member setDebugForReason    : reason:string -> (XanthamTag -> bool) -> XanthamTag
    member withDebugForReason   : reason:string -> (XanthamTag -> bool) -> XanthamTag
    member setDebugForReasonOr  : onFail:string -> reason:string -> (XanthamTag -> bool) -> XanthamTag
    member withDebugForReasonOr : onFail:string -> reason:string -> (XanthamTag -> bool) -> XanthamTag
```

When the condition lambda returns true:

* `setDebug` flips the flag silently.
* `setDebugForReason` / `withDebugForReason` flip the flag *and* emit a
  `TRACKING` event through the structured logger (level `Debug`) the
  first time they are asked to track that tag. The line carries
  `debugId`, `event = "TRACKING"`, `tagType` (`TYPE` or `DECLARATION`),
  the tag's type-flags or syntactic kind, `tagIdentityKey`, and the
  free-form `trackingReason` &mdash; all as named JSON fields so a
  follow-up `jq '.data.trackingReason'` is trivial.
* `setDebugForReasonOr` / `withDebugForReasonOr` are safe for paths that
  may be reached more than once: the first request logs `trackingReason`;
  subsequent requests log `onFail` instead, with `firstTrack` recording
  whether the tag was already in debug mode. Use this anywhere the seed
  predicate may match across re-entrant code &mdash; you'll see which
  call site *would* have started tracking had it been first.

The `with*`-prefixed variants return the tag for piping; the `set*`-prefixed
variants discard the result so they compose cleanly inside imperative
blocks.

```fsharp
let tagState, _ = XanthamTag.Create(node, checker)
let tag = TagState.value tagState
tag.setDebugForReason "User reported missing TsType for Promise<T>" (fun _ -> true)
```

> The public API forces predicate usage so CI can automatically detect
> commits that include calls to `setDebug*` &mdash; build with the
> `FAIL_ON_DEBUG_TRACKING` symbol and any call site throws regardless of
> the condition.

### Conditional execution

```fsharp
type XanthamTag with
    member withDebug        : (XanthamTag -> unit) -> XanthamTag        // returns tag
    member doWithDebug      : (XanthamTag -> unit) -> unit              // discards result
    member withDebugOneShot : key:string -> (XanthamTag -> unit) -> XanthamTag
    member doDebugOneShot   : key:string -> (XanthamTag -> unit) -> unit
```

* `withDebug` / `doWithDebug` run `fn` only when `tag.Debug` is true;
  otherwise the tag passes through untouched.
* `withDebugOneShot` / `doDebugOneShot` are the same but de-duplicate by
  `key` &mdash; the body fires at most once per `(tag, key)` pair, even
  if the surrounding code is re-entered. Useful in cache lookups and
  fixed-point loops where the same tag may be revisited many times.
  De-duplication state is stored on the tag itself under the
  `DebugOneShots` slot, so it lives exactly as long as the tag does.

### Logging helpers

There is no longer a `debugLocation` / `debugComment` family &mdash; the
caller location is captured automatically, so a single helper is enough:

```fsharp
type XanthamTag with
    member debugMessage   : message:string -> XanthamTag    // returns tag
    member doDebugMessage : message:string -> unit          // discards result

    /// Escape hatch for emitting a structured line that needs access to
    /// the underlying logger (e.g. multi-arg printf templates). Only
    /// runs in DEBUG builds when the tag's Debug flag is set.
    member trace : (Utils.Logging.Log -> int (* traceId *) -> unit) -> unit
```

`debugMessage` and `doDebugMessage` emit a single `Debug`-level line with
`debugId`, `event = "EMIT"`, and the supplied `message`; `file:line` of
the call site is attached automatically via the caller-info attributes.
For richer payloads, `trace` hands you the per-tag `Log` and a stable
`TraceId` so you can call `log.logfd "%s{phase} %A{key}" "merge" key`
directly.

Old call sites such as:

```fsharp
XanthamTag.debugLocationAndForget "TypeReference.resolveTypeBase | cache hit" tag
```

become:

```fsharp
tag.doDebugMessage "TypeReference.resolveTypeBase | cache hit"
```

The `file:line` part of the old "location" string is now redundant
(it's already captured in `at`), so the convention is to drop it and
keep just the semantic suffix.

### Propagating debug across relationships

```fsharp
type XanthamTag with
    member chainDebug : parent:XanthamTag -> XanthamTag
```

`tag.chainDebug(parent)` activates tracing on `tag` whenever `parent` is
being traced, and emits a `TRACKING` line with
`trackingReason = "Parent [N] chained debug"` so the parent/child link is
visible in the log. This is the primary way the seed flag spreads
through the pipeline. Idiomatic use lives in `Reading/TypeReference.fs`:

```fsharp
typeReferenceArgs
|> Array.iter (fun arg ->
    let child = TagState.value (fst (XanthamTag.Create(arg, checker)))
    child.chainDebug(parentTag)
    |> stackPushAndThen ctx)
```

## File logging

As of the structured-logging refactor every reader run also writes a
**file-backed JSON log** alongside the synthesized `temp.d.ts`. This is the
primary diagnostics surface; the console is now warning-only by default.

### Where logs live

`TypeScriptReader.createWithLogger[For]` provisions two sinks:

| Sink              | Default level | Format        | Lifetime                                        |
|-------------------|---------------|---------------|-------------------------------------------------|
| `ConsoleLogger`   | `Warning`     | colourised    | stderr/stdout of the running process            |
| `TextWriterLogger`| `Trace`       | JSON-per-line | `.xantham/log_<ts>.txt` (sibling of `run_*/`)   |

The log file is written to `.xantham/` itself, **not** inside the
per-invocation `run_<ts>/` directory. This is deliberate &mdash; the run
directory is destroyed at the end of every non-`--debug` invocation, but
the log file is what you want to keep, so it lives one level up and
persists regardless of the `--debug` flag.

(In `#if DEBUG` builds the console is bumped to `Debug` so traces remain
visible at the REPL.) Both sinks are wrapped by a single
`Utils.Logging.CombinedLogger`, exposed on the reader as
`reader.Log` / `reader.logger`, so a single call site emits to both.

### Driving from the CLI

`Program.fs` selects the logger based on build symbols and CLI flags:

```text
xantham <input> [--debug] [-o <output>]
```

* In non-RELEASE builds the file-backed logger is always installed.
* In RELEASE builds it is installed **only** when `--debug` is passed.
* The per-run scratch directory is preserved iff `--debug` was passed;
  otherwise `Temp.Directory.closeRunDirectory` deletes it after `read`.

```text
# Inspect previous logs (one per invocation, newest last)
ls .xantham/log_*.txt
```

### Line shape

Each line is a single JSON object emitted by `formatEntryAsJson`:

```json
{"level":"Debug","format":"TypeReference.resolveTypeBase | Shared symbol: [42]","at":"src/Xantham.Fable/Reading/TypeReference.fs:118"}
{"level":"Trace","format":"Dispatcher.dispatch | {kind} {key}","at":"src/Xantham.Fable/Reading/Dispatcher.fs:24","data":{"kind":"TypeDeclaration","key":"Symbol(Promise)"}}
{"level":"Warning","format":"[CIRCREF] | {key}","at":"src/Xantham.Fable/Read.fs:36","data":{"key":"Id 17"}}
```

* `level` &mdash; `Trace | Debug | Information | Warning | Error | Critical`.
* `format` &mdash; the printf/Serilog-style template with `{name}` holes left
  in place; named holes follow Serilog conventions (the `@` sigil is stripped
  from the key but preserved structurally).
* `at` &mdash; `file:line` of the call site captured by `[<CallerFilePath>]` /
  `[<CallerLineNumber>]`. Paths are relativised to the repo root.
* `data` &mdash; nested object containing every argument by name (unnamed
  printf specifiers are keyed `arg0`, `arg1`, ...). Omitted when there are
  none, keeping the top-level schema stable for `jq` / LLM consumption.

### Logging helpers

The reader and tags both expose printf-style entry points. Prefer these over
raw `printfn` so output stays structured:

| Helper       | Level        | Notes                                      |
|--------------|--------------|--------------------------------------------|
| `log.logft`  | `Trace`      | per-dispatch traces                        |
| `log.logfd`  | `Debug`      | targeted diagnostics                       |
| `log.logfi`  | `Information`| run-level milestones                       |
| `log.logfw`  | `Warning`    | console-visible by default                 |
| `log.logfe`  | `Error`      | recoverable failures                       |
| `log.logfc`  | `Critical`   | unrecoverable failures                     |

Tags carry a reference to the active logger via `GuardedData.Log` so the
`XanthamTag.debug*` helpers route through the same JSON pipeline as the
reader; tracing emitted from a debug-seeded tag therefore lands in the run
log alongside the dispatcher's own traces, correlated by the yellow `[id]`.

### Dispatcher trace

Every `Dispatcher.dispatch` invocation now emits a `Trace`-level line
identifying the tag's kind and identity key. With file logging on (the
default in non-RELEASE), the entire traversal is recoverable from
`log_*.txt` without seeding any tag &mdash; the per-tag `Debug` flag remains
the right tool when you want a *focused* slice, but the dispatcher trace is
the catch-all backbone.

## Output format

A traced run produces JSON lines like:

```json
{"level":"Debug","format":"[42] [TRACKING] [TYPE] [\"Object\"] Tracking Symbol(Promise): User reported missing TsType for Promise<T>.","at":"src/Xantham.Fable/Reading/TypeReference.fs:118","data":{"debugId":42,"event":"TRACKING","tagType":"TYPE","tagTypeDiscriminators":["Object"],"tagIdentityKey":"Symbol(Promise)","trackingReason":"User reported missing TsType for Promise<T>"}}
{"level":"Trace","format":"Dispatcher.dispatch | {kind} {key}","at":"src/Xantham.Fable/Reading/Dispatcher.fs:24","data":{"kind":"TypeReference","key":"Symbol(Promise)"}}
{"level":"Debug","format":"[42] [EMIT] TypeReference.resolveTypeBase | Shared symbol","at":"src/Xantham.Fable/Reading/TypeReference.fs:142","data":{"debugId":42,"event":"EMIT","message":"TypeReference.resolveTypeBase | Shared symbol"}}
{"level":"Debug","format":"[43] [TRACKING] [false] [TYPE] [\"TypeParameter\"] Tracking Symbol(T): Parent [42] chained debug.","at":"src/Xantham.Fable/Reading/TypeReference.fs:121","data":{"debugId":43,"event":"TRACKING","firstTrack":false,"tagType":"TYPE","tagTypeDiscriminators":["TypeParameter"],"tagIdentityKey":"Symbol(T)","trackingReason":"Parent [42] chained debug"}}
```

Reading order:

1. `event = "TRACKING"` lines &mdash; *who* is being followed and *why*
   (`tagIdentityKey`, `trackingReason`).
2. `event = "EMIT"` lines &mdash; *what* the reader did with that tag,
   correlated by `debugId`.
3. `Dispatcher.dispatch` traces interleave to show traversal order.
4. Subsequent `TRACKING` lines whose `trackingReason` starts with
   `Parent [N] chained debug` reveal the propagation graph.

`jq` recipes:

```bash
# Every line a single tag touched
jq -c 'select(.data.debugId == 42)' .xantham/log_*.txt

# Just the TRACKING banners (who got seeded, and why)
jq -c 'select(.data.event == "TRACKING")' .xantham/log_*.txt

# Everything the dispatcher saw, in order
jq -c 'select(.format | startswith("Dispatcher.dispatch"))' .xantham/log_*.txt
```

## Practical workflow

### 1. Seed at a known entry point

When you have a reproducer (a specific symbol or `.d.ts` declaration that
mis-extracts), add a one-line `setDebugForReason` near the top of the
relevant `Reading/*.fs` handler:

```fsharp
// in Reading/TypeReference.fs
let xanTag, guard = XanthamTag.Create(typ, ctx.Checker)
let xanTag = xanTag |> TagState.value
xanTag.setDebugForReason
    "Repro for #42: Promise<T> missing"
    (fun t -> t.IdentityKey.ToString().Contains("Promise"))
```

Remove the predicate once the bug is fixed &mdash; the helper itself is a
no-op in release, but the seed condition is real code that survives
compilation.

### 2. Follow the chain

Verify that downstream call sites you care about already use
`chainDebug` / `withDebug`. The `Reading/` modules do this consistently;
when you add a new handler, mirror the pattern:

```fsharp
let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) (node: MyNode) =
    tag.doDebugMessage "MyNode.dispatch"
    let childTag = ...
    childTag.chainDebug(tag)
    |> ignore
    ...
```

### 3. Use one-shots for hot paths or effects

Cache lookups and the `runReader` stack-pop loop visit the same tag many
times. Use `withDebugOneShot` / `doDebugOneShot` with a stable key to fire
the body only the *first* time a tag passes through a given site:

```fsharp
tag.doDebugOneShot
    "TypeReference.resolveTypeBase:cache-hit"
    (fun t -> t.doDebugMessage "TypeReference.resolveTypeBase | cache hit")
```

For richer payloads, drop into `trace` to use the logger directly:

```fsharp
tag.doDebugOneShot "first-pass:register" (fun t ->
    Signal.effect (fun () ->
        GuardedData.TypeSignal.getOrDefault t
        |> _.Value
        |> fun key ->
            t.trace (fun log id ->
                log.logfd "[%i{debugId}] [%s{event}] %A{typeSignal}"
                    id "SIGNAL" key))
    |> ignore)
```

### 4. Cross-reference with the standard log markers

The reader emits the build-wide markers `[CIRCREF]` (circular ref) and
`[MISSREF]` (missing builder) regardless of `Debug`. They're plain
`Warning`-level entries in the JSON log, so a `jq 'select(.format |
startswith("[CIRCREF]"))'` will surface every one. When a traced tag
correlates with a `[CIRCREF]` line, the surrounding `debugId = N` lines
usually point at the offending push.

### 5. Strip seeds before commit

`setDebug*` calls land actual logging side-effects in DEBUG builds. Remove
ad-hoc seed predicates before committing; leave `chainDebug` and the
`debugMessage` / `doDebugMessage` calls in place &mdash; they are
zero-cost when no tag is seeded.



## Build configuration

The per-tag helpers are bracketed by `#if DEBUG`. The Fable extractor
compiles in `Debug` configuration through the default `npm run prestart` /
`npm run watch` paths, so traces are available out of the box during
development. The Fable test target is also Debug:

```bash
npm run pretest   # dotnet fable -c Debug --cwd tests/Xantham.Fable.Tests -o dist/tests
```

For a non-tracing run (e.g. perf measurement) compile with `-c Release`
**and** invoke the CLI without `--debug` &mdash; the tag still carries
`Debug`/`DebugId` members, all `XanthamTag.*` helpers degrade to identity
functions, `setDebug*` does not mutate the flag, and no
`TextWriterLogger` is attached so no file log is produced. Passing
`--debug` to a release build re-enables the file logger (but the per-tag
helpers are still inert &mdash; you only get the dispatcher trace and any
`logf*` calls outside `#if DEBUG`).

## Reference

All helpers are now **instance methods on `XanthamTag`**. `with*`
variants return the tag; `set*` / `do*` variants discard the result.
`file:line` of the caller is captured automatically.

| Method                                          | Side effect when `Debug` is on                            | Returns |
|-------------------------------------------------|------------------------------------------------------------|---------|
| `tag.setDebug(cond)`                            | flips flag if `cond tag`                                   | tag     |
| `tag.setDebugForReason(reason)(cond)`           | flips flag + emits `TRACKING` line with `trackingReason`   | tag     |
| `tag.withDebugForReason(reason)(cond)`          | same as above (alias kept for symmetry with `with*` style) | tag     |
| `tag.setDebugForReasonOr(onFail)(reason)(cond)` | first call logs `reason`, later calls log `onFail`         | tag     |
| `tag.withDebugForReasonOr(onFail)(reason)(cond)`| same as above (return value variant)                       | tag     |
| `tag.withDebug(fn)`                             | `fn tag`                                                   | tag     |
| `tag.doWithDebug(fn)`                           | `fn tag`                                                   | unit    |
| `tag.withDebugOneShot(key, fn)`                 | `fn tag` once per `(tag, key)`                             | tag     |
| `tag.doDebugOneShot(key, fn)`                   | `fn tag` once per `(tag, key)`                             | unit    |
| `tag.chainDebug(parent)`                        | propagates flag + emits `chained debug` line               | tag     |
| `tag.debugMessage(msg)`                         | emits `EMIT` line with `message = msg`                     | tag     |
| `tag.doDebugMessage(msg)`                       | emits `EMIT` line with `message = msg`                     | unit    |
| `tag.trace(fn)`                                 | `fn tag.Logger tag.TraceId` (escape hatch)                 | unit    |

All helpers are `inline` and elided entirely in release. The output of
every helper lands in `.xantham/log_<ts>.txt` as a structured JSON line;
see **File logging** above for the schema and recommended `jq` recipes.
