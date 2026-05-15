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

### Turning tracing on

```fsharp
XanthamTag.setDebug              : condition:(XanthamTag -> bool) -> XanthamTag -> XanthamTag
XanthamTag.setDebugForReason     : reason:string -> condition:(XanthamTag -> bool) -> XanthamTag -> XanthamTag
XanthamTag.setDebugForReasonOr   : onFail:string -> reason:string -> condition:(XanthamTag -> bool) -> XanthamTag -> XanthamTag
```

When the condition lambda returns true:

* `setDebug` flips the flag silently.
* `setDebugForReason` flips the flag *and* emits a one-line `[TRACKING]`
  banner (white `[TRACKING]` + identity key + italicised reason + yellow
  `[DebugId]`) the first time it is asked to track that tag.
* `setDebugForReasonOr` is the safe variant for code paths that may be
  reached more than once: it logs the reason on the first request and the
  `onFail` message on subsequent requests, so you can tell which call site
  *would* have started tracking had it been first.

All three return the tag, so they compose into pipelines:

```fsharp
node
|> XanthamTag.Create checker
|> fst
|> TagState.value
|> XanthamTag.setDebugForReason "User reported missing TsType for Promise<T>" (fun _ -> true)
```

> The public API forces predicate usage so that CI can automatically detect
> if a commit included usage of `setDebug-` variants using a preprocessor
> condition which fails when the function is called (regardless of the condition).
> 
> This simply requires building and usage of the library with the `FAIL_ON_DEBUG_TRACKING` 
> property set.

### Conditional execution

```fsharp
XanthamTag.withDebug         : (XanthamTag -> unit) -> XanthamTag -> XanthamTag
XanthamTag.withDebugOneShot  : key:string -> (XanthamTag -> unit) -> XanthamTag -> XanthamTag
```

* `withDebug` runs `fn` only when `tag.Debug` is true; otherwise the tag
  passes through untouched.
* `withDebugOneShot` is the same but de-duplicates by `key` &mdash; the body
  fires at most once per `(tag, key)` pair, even if the surrounding code is
  re-entered. Useful in cache lookups and fixed-point loops where the same
  tag may be revisited many times in one read.

### Logging helpers

All helpers below are built on `withDebug` and emit through the project's
chalk-coloured `Log.debug`. They share a common shape: a yellow `[DebugId]`,
a yellow location label, and a free-form comment.

```fsharp
XanthamTag.debugLocation                    : location:string -> XanthamTag -> XanthamTag
XanthamTag.debugComment                     : comment:string  -> XanthamTag -> XanthamTag
XanthamTag.debugLocationAndComment          : location:string -> comment:string -> XanthamTag -> XanthamTag

// Same as above but discard the result; convenient inside imperative blocks.
XanthamTag.debugLocationAndForget           : location:string -> XanthamTag -> unit
XanthamTag.debugCommentAndForget            : comment:string  -> XanthamTag -> unit
XanthamTag.debugLocationAndCommentAndForget : location:string -> comment:string -> XanthamTag -> unit
```

Convention used throughout `Reading/`: pass the fully-qualified F# location
(`"TypeReference.resolveTypeBase | Shared symbol"`) rather than a free-form
sentence, so output greps cleanly.

### Propagating debug across relationships

```fsharp
XanthamTag.chainDebug : parent:XanthamTag -> child:XanthamTag -> XanthamTag
```

`chainDebug` activates tracing on `child` whenever `parent` is being traced,
and emits a `[TRACKING] ... chained debug` banner so the parent / child link
is visible in the log. This is the primary way the seed flag spreads through
the pipeline. Idiomatic uses live in `Reading/TypeReference.fs`:

```fsharp
typeReferenceArgs
|> Array.iter (fun arg ->
    arg
    |> Tag.Create checker
    |> fst |> TagState.value
    |> stackPushAndThen ctx (XanthamTag.chainDebug parentTag))
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

A traced run produces lines like:

```
[TRACKING] Tracking Symbol(Promise)
           Tracking Reason: User reported missing TsType for Promise<T>
           Tracking Id: [42]
[DEBUG] TypeReference.resolveTypeBase | Shared symbol: [42]
[DEBUG] [42]: Pushing two argument tags
[TRACKING] Tracking Symbol(T)
           Tracking Reason: Parent [42] chained debug
           Tracking Id: [43]
[DEBUG] Prelude.MemberStore.Parameter.create: [43] Creating
```

Reading order:

1. `[TRACKING]` banner &mdash; *who* is being followed and *why*.
2. `[DEBUG] location: [id]` lines &mdash; *what* the reader did with that tag.
3. Subsequent `[TRACKING]` banners with `Parent [N] chained debug` show the
   propagation graph.

## Practical workflow

### 1. Seed at a known entry point

When you have a reproducer (a specific symbol or `.d.ts` declaration that
mis-extracts), add a one-line `setDebugForReason` near the top of the
relevant `Reading/*.fs` handler:

```fsharp
// in Reading/TypeReference.fs
let xanTag, guard = XanthamTag.Create(typ, ctx.Checker)
let xanTag = xanTag |> TagState.value
XanthamTag.setDebugForReason
  "Repro for #42: Promise<T> missing"
  _.IdentityKey.ToString().Contains("Promise")
  xanTag
|> ignore
```

Remove the conditional once the bug is fixed &mdash; the helper itself is a
no-op in release, but the seed predicate is real code.

### 2. Follow the chain

Verify that downstream call sites you care about already use
`chainDebug` / `withDebug`. The `Reading/` modules do this consistently;
when you add a new handler, mirror the pattern:

```fsharp
let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) (node: MyNode) =
    XanthamTag.debugLocationAndForget "MyNode.dispatch" tag
    let childTag = ... |> XanthamTag.chainDebug tag
    ...
```

### 3. Use one-shots for hot paths or effects

Cache lookups and the `runReader` stack-pop loop visit the same tag many
times. Use `withDebugOneShot` with a stable key to log the *first* visit
under a given condition only:

```fsharp
tag
|> XanthamTag.withDebugOneShot
    "TypeReference.resolveTypeBase:cache-hit"
    (fun t ->
        XanthamTag.debugLocationAndForget
            "TypeReference.resolveTypeBase | cache hit"
            t)
```

You can also subscribe to changes in signals via effects:

```fsharp
tag
|> XanthamTag.withDebugOneShot
    "first-pass:register"
    (fun _ ->
        Signal.effect (fun () ->
            GuardedData.TypeSignal.getOrDefault tag
            |> _.Value
            |> sprintf "Type signal set: %A"
            |> XanthamTag.debugLocationAndForget
            |> funApply tag)
        |> ignore)
```

### 4. Cross-reference with the standard log markers

The reader emits the build-wide markers `[CIRCREF]` (circular ref) and
`[MISSREF]` (missing builder) regardless of `Debug`. When a traced tag
correlates with a `[CIRCREF]` line, the chained `[DEBUG] ... [42]` lines
above it usually point at the offending push.

### 5. Strip seeds before commit

`setDebug*` calls land actual logging side-effects in DEBUG builds. Remove
ad-hoc seed predicates before committing; leave `chainDebug` and the
`debugLocation*` calls in place &mdash; they are zero-cost when no tag is
seeded.



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

| Helper                              | Side effect when `Debug` is on                  | Returns          |
|-------------------------------------|--------------------------------------------------|------------------|
| `setDebug`                          | flips flag                                       | tag              |
| `setDebugForReason`                 | flips flag + `[TRACKING]` banner                 | tag              |
| `setDebugForReasonOr`               | banner on first call, `onFail` line on retries   | tag              |
| `withDebug fn`                      | `fn tag`                                         | tag              |
| `withDebugOneShot key fn`           | `fn tag` once per `(tag, key)`                   | tag              |
| `chainDebug parent child`           | propagates flag + chain banner                   | child            |
| `debugLocation loc`                 | `[DEBUG] loc: [id]`                              | tag              |
| `debugComment cmt`                  | `[DEBUG] [id]: cmt`                              | tag              |
| `debugLocationAndComment loc cmt`   | `[DEBUG] loc: [id] cmt`                          | tag              |
| `*AndForget` variants               | as above                                         | `unit`           |

All helpers are `inline` and elided entirely in release.
