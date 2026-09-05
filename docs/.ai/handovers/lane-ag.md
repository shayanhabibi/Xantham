---
category: Generator
audience: managing agent
title: Lane AG - MB001 fires on an optional parameter
branch: worktree-gen-wave7-ag
base: 44507724b441c5b965bd20fef87038e8cd9b605d
---

# Lane AG — wave seven, item 4

`MB001` (`MB.OptionalParameterAsOption`) fired nowhere in the corpus. The parameter's `?` reaches
the generator now, `MB001` fires 610 times, and `MB006` separates the other spelling.

## What the compiler actually sends

Measured against the live compiler before anything was written, with a five-line probe:

```
param a  flags=FunctionScopedVariable check=None decls=[|"9.170.<path>"|]
param b  flags=FunctionScopedVariable check=None decls=[|"12.170.<path>"|]   // declared b?: string
```

A parameter symbol carries `SymbolFlags.FunctionScopedVariable` and `CheckFlags.None` whether or
not it is declared `?`, and that holds for `b?: any` and `b?: number | undefined` too. So:

- `SymbolFlags.Optional` states the fact **for a property** (the binder adds it in
  `bindPropertyWorker`, from the property's own `questionToken`). It is never added for a
  parameter.
- `CheckFlags.OptionalParameter` marks the synthetic parameters of a combined signature, not a
  declared one.
- No protocol method answers it either: the 142-method surface has no `isOptionalParameter` and
  no `getMinArgumentCount`, and `SignatureResponse` carries no minimum arity.
- `getNonMissingTypeOfSymbol` does not separate the spellings: upstream's `removeMissingType`
  gates on `exactOptionalPropertyTypes` *and* on `SymbolFlags.Optional`, so on a parameter it is
  the identity.

The fact exists only as syntax: `ParameterDeclaration.questionToken`. The symbol's `Declarations`
already point at that node.

## The wire change

`NodeHandle` in `src/Xantham.TypeScript.Wire/Library.fs`, public, beside `Ast`:

```fsharp
[<Struct>] type NodeHandle = { Index: int; Kind: SyntaxKind; Path: string }
NodeHandle.parse  : string -> NodeHandle voption
NodeHandle.format : NodeHandle -> string
```

`docs/wire-navigation.md` had anticipated exactly this ("no handle helper yet … when a second
caller needs them, they belong beside `Ast.read` in `Library.fs`"). It is updated: the two
hand-rolled snippets are replaced by the calls, and the closing paragraph now says why a handle
is load-bearing rather than that there is no helper for it.

`parse` splits on the first two dots only, so a Windows drive colon and a dotted package
directory survive. A malformed string is `ValueNone` rather than an exception —
`Grouping.declOrder` in `Model.fs` still hand-parses handles for its source-order key and was
left alone to keep the diff off another lane's file; folding it onto `NodeHandle.parse` is a
one-line follow-up.

## The generator change

`src/Xantham.Generator/Resolve.fs` — **not** on the brief's file list, but it is where a
`ResolvedMember`'s `Optional` is decided, and no other wave-seven lane is in it:

- `resolveMember`'s flag is renamed `readOnlyRelevant` → `isProperty`, because it now selects
  two questions rather than one: `readonly` for a property, the `?` token for a parameter.
- `declaresQuestionToken` follows the parameter symbol's first declaration handle into the blob,
  checks the node's kind against the blob rather than trusting the handle, and reads
  `ParameterDeclaration.questionToken`.
- Blobs are cached in a `ConditionalWeakTable` keyed on the `TscMailbox`, with
  `(snapshot, project, path)` inside it. Per-connection rather than process-global: two
  concurrently generating fixtures cannot collide on a snapshot id, and a finished generation
  drops its blobs with the compiler process that served them. The value is a
  `Lazy<Task<_>>`, so a file is requested once however many parameters name it.

`src/Xantham.Generator/Shape/Spec.fs`, two edits, both under line 1900 (lane AJ's region above
2000 untouched):

- `isOptionalParam`'s doc no longer says the wire cannot carry the marker. The disjunction itself
  is unchanged and deliberately so — see below.
- `shapeSignature` raises `MB006` (`MB.OptionalParameterFromUnion`) where a parameter reads as
  `option` without a `?`. `MB001` was already raised on `p.Optional` and starts firing now that
  `p.Optional` is a fact.

### Why the emission still collapses the two spellings

`f(x?: T)` and `f(x: T | undefined)` still reach one F# signature, and that is correct rather
than residual: F# has one spelling for an optional parameter, and `?x: T` admits both `f()` and
`f(Some v)`. The defect was that the manifest recorded no difference. It records one now — the
pair `MB001` / `MB006` at the parameter's own owner string — which is the same pair `MB003` and
`TR032` already gave a property. `MB006` is tiered Ergonomic and its message says "reads as
option", matching that reading.

What did change in emission is the case the hoist could never see: `?p: any`. `any` absorbs
`undefined`, so nothing hoisted and the parameter rendered required. 39 lines of `@cloudflare/workers-types` were wrong that way
and are now `?p: obj`.

## Measurements

Baseline is `44507724b441c5b965bd20fef87038e8cd9b605d` (batch 1 already merged).

| | before | after |
| --- | --- | --- |
| `MB001` corpus-wide | 0 | 610 |
| `MB006` corpus-wide | 0 (case unused) | 37 |
| wire tests | 85 passed, 1 skipped | 90 passed, 1 skipped |
| generator tests | 444 | 450 |
| run-gate checks | 179 | 179 |

`MB001` by fixture: cloudflare 349, animejs 212, solid-js 21, member-shape-lab 6, inherit-lab 4,
phase-b-lab 4, error-class-lab 3, literal-overload-lab 3, optional-param-lab 3, and 1 each in
ansi-regex, brand-lab, generics-lab, globals-lab, absence-alphabet-lab.

`MB006` by fixture: animejs 17, cloudflare 11, solid-js 4, member-shape-lab 2,
optional-param-lab 2, absence-alphabet-lab 1.

**Tier counts did not move anywhere.** cloudflare stays `exact 237 / ergonomic 1089 / widened 382
/ escape 110`; animejs, solid-js and type-fest are unchanged too. Every symbol that gained an
`MB001` or an `MB006` was already Ergonomic or worse, almost always through the `TR032` hoist on
the same parameter.

**No other finding key moved by one.** Verified mechanically: for every changed `symbols.jsonl`,
the per-key counts at HEAD and in the tree differ only by the two new keys appearing.

### Golden movement

Two binding files changed, 40 lines:

- `@cloudflare/workers-types` — 39 lines: 22 member declarations and the 17 `Create`
  overloads whose delegate positions carry the same parameters. Every one is `?p: any` becoming
  optional:
  `console.dir`, `console.table`, `AbortSignal.abort`, `AbortController.abort`,
  `ReadableStream*.cancel`, `WritableStream*.abort`, `WritableStreamDefaultController.error`,
  `WebAssembly.Global`/`Table` constructors and `Table.grow`/`set`, `Container.destroy`,
  `MessagePort.postMessage`, `EventCounts.forEach`, `Flagship.get`, `DigestStream.abort`. The
  delegate positions beside them move with the same fact — `Action<obj>` → `Action<obj option>`,
  since `delegateRef` reads `isOptionalParam` too.
- `solid-js` — 1 line: `createContext(defaultValue?: undefined, …)` was required, because
  `undefined` alone shapes to `unit` rather than to an option. Now `?defaultValue: unit`.

Everything else in the diff is `manifest.json` and `symbols.jsonl`.

## Evidence

- `tests/fixtures/optional-param-lab` — the three spellings of presence (`b?: T`,
  `b: T | undefined`, `b: T`) at a bare function and at a method, plus `b?: any` for the case the
  hoist cannot see. Registered in `Pipeline.test.fs` with four assertions: the exact `MB001` and
  `MB006` symbol lists, the required parameter carrying neither, both optional spellings
  rendering as `?b: string`, and `markedAny` rendering `?b: obj`. Not linked into the run gate —
  its behaviour (an omitted optional argument) is already proven by `globals-lab`.
- `tests/Xantham.TypeScript.Wire.Tests/NodeHandle.test.fs` — five tests. Two pure (a Windows path
  with a dotted directory round-trips; five malformed strings are refused). Three live, and they
  are the regression guard for the whole lane: a parameter symbol's `Flags` are exactly
  `FunctionScopedVariable` and its `CheckFlags` exactly `None` whether or not it is `?`; the
  marker is on the declaration node the handle names; the same fact is on a *property's* symbol
  as `SymbolFlags.Optional`. Fixture `fixtures/optional.d.ts`, added to the fixture `tsconfig`'s
  `files`.
- Lane AC's pinned defect assertion in `Pipeline.test.fs` (`absence-alphabet-lab`,
  `withOptional(fallback)`) asserted `(false, [ "fromUndefined" ])` under a comment saying the
  wire cannot carry the marker. It now asserts `(true, [ "fromUndefined" ])`. The neighbouring
  `withNullable(fallback)` assertion is unchanged and still `(false, [ "fromNull" ])` — `null` is
  not `?`, and `absenceAt`'s marker reads `MB001`/`MB003` only, so `MB006` does not disturb it.

## Gate

`dotnet fsi build.fsx -- test` with no flags, over the composed tree: wire 90/1 skipped,
generator 450, compile gate built, Fable run gate 179 checks. Green.

## One thing to know at integration

`dotnet fsi build.fsx -- test` runs `dotnet fantomas .` as its `format` stage, and on Windows
fantomas writes CRLF. Every F# file in the tree therefore reports as modified in `git status`
after a full run, while `git diff` shows nothing — `.gitattributes` normalises them back to LF on
staging, so they do not enter a commit. It is noise, not a change, but it makes `git status`
useless for spotting what a lane touched until the files are staged. Commit 67061fe pinned the
line endings; the fantomas rewrite is what still triggers the report.
