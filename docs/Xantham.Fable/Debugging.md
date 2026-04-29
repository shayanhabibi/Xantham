---
title: Debugging
category: Xantham.Fable
categoryindex: 2
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
XanthamTag.setDebug              : XanthamTag -> XanthamTag
XanthamTag.setDebugForReason     : reason:string -> XanthamTag -> XanthamTag
XanthamTag.setDebugForReasonOr   : onFail:string -> reason:string -> XanthamTag -> XanthamTag
```

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
|> XanthamTag.setDebugForReason "User reported missing TsType for Promise<T>"
```

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
if (xanTag.IdentityKey.ToString()).Contains "Promise" then
    XanthamTag.setDebugForReason "Repro for #42: Promise<T> missing" xanTag |> ignore
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

### 3. Use one-shot for hot paths

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

The helpers are bracketed by `#if DEBUG`. The Fable extractor compiles in
`Debug` configuration through the default `npm run prestart` /
`npm run watch` paths, so traces are available out of the box during
development. The Fable test target is also Debug:

```bash
npm run pretest   # dotnet fable -c Debug --cwd tests/Xantham.Fable.Tests -o dist/tests
```

For a non-tracing run (e.g. perf measurement) compile with
`-c Release` &mdash; the tag still carries `Debug`/`DebugId` members, but
all helpers degrade to identity functions and `setDebug*` does not
mutate the flag.

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
