# An F# client for the tsgo unstable API

Status: working proof of concept. `Xantham.TsGo.slnx` builds and 31/31 tests pass, 8 of them
against a live native compiler.

Since TypeScript 7, tsgo *is* TypeScript: it ships from `microsoft/TypeScript` as the `typescript`
npm package, the binary is called `tsc`, and the separate `@typescript/native-preview` preview
package is history. The client was verified against upstream `tsc` 7.0.2 and needed no protocol
change - see §8.

> **Source-of-truth note (2026-08-31).** `microsoft/typescript-go` is archived and its local
> checkout has been deleted. Do not clone or cite it. All compiler research goes against
> `microsoft/TypeScript@main` or the installed `typescript` 7.x npm package - see the
> "TypeScript 7 compiler sources" section of `AGENTS.md` for the name and path translations.

This document records why the client is shaped the way it is, how to use it, and where to extend
it. Two companion documents hold the underlying research:

- [`tsgo-protocol.md`](tsgo-protocol.md) - the wire protocol, byte by byte, with live traces.
- [`tsgo-native-route.md`](tsgo-native-route.md) - the c-shared DLL route that was investigated
  and rejected.

Where this document and those disagree, this one wins: it records what the running code actually
does, and a few of their claims did not survive contact with the server (see *Corrections* below).

---

## 1. What was built

| Project | Purpose |
|---|---|
| `src/Xantham.TsGo` | The client library. No dependencies beyond the BCL. |
| `tests/Xantham.TsGo.Tests` | Expecto suite: protocol unit tests plus live end-to-end tests. |
| `Xantham.TsGo.slnx` | Its own solution, deliberately separate from `Xantham.slnx`. |

Five files, in compile order:

| File | Responsibility |
|---|---|
| `Wtf8.fs` | Decodes the AST string table. |
| `Msgpack.fs` | The frame envelope: read and write one 3-tuple. |
| `Channel.fs` | Owns the `tsgo` process; one serial request/response pump. |
| `Ast.fs` | Reads the binary AST blob. |
| `Session.fs` | The method surface: snapshots, source files, checker queries. |

The separation is worth preserving. `Msgpack` and `Ast` are pure functions over bytes and are
tested without a server; `Channel` is the only part that owns a process; `Session` is the only
part that knows method names, which is where all the version drift lives.

---

## 2. Why IPC rather than a native DLL

The original question was whether to speak the same IPC protocol the TypeScript client uses, or to
build a Go glue layer and call into typescript-go through a DLL. The second option was investigated
seriously - a `c-shared` build was produced and a C driver exercised parse, AST encoding,
diagnostics, a full `Program` from a `tsconfig.json`, and a checker query, all through the DLL.

It was rejected on measurements, not taste:

- **Per-call cost.** A no-op cgo export costs **828.8 ns**; the same no-op into a plain C DLL costs
  **1.3 ns**. The boundary, not the work, dominates. Over 29,676 nodes, per-node FFI measured
  24.6 ms against 2.7 ms for a single bulk transfer.
- **The premise was wrong.** IPC is not chatty. `getSourceFile` returns the entire file's AST in one
  blob, after which every node, position and identifier is a local byte-array read. Round trips are
  per *file*, and only checker queries cost more. Both routes therefore want the same bulk design,
  and the DLL only buys a memcpy instead of a pipe write.
- **Fork cost.** typescript-go has 105 Go package directories and exactly one outside `internal/`:
  `./cmd/tsgo`, a `main`. Parser, AST, checker and encoder are all unimportable from an external
  module, so the DLL route requires a fork. Upstream is closed and **archives September 2026**, with
  development moving back to `microsoft/TypeScript`.

The spike survives in `tmp/tsgo-native/` if the decision is ever revisited. Its three ABI hazards are
recorded in the native-route document; the important one is that a Go panic unwinding into C kills
the process outright, so every export needs a hand-written `recover()` barrier.

---

## 3. How the protocol works, in one page

`tsc --api --cwd <dir>` is a long-lived, **strictly serial** RPC server on stdio. Every frame,
both directions, is a MessagePack 3-tuple:

```
0x93                  fixarray, length 3
<type>                1=Request 2=CallResponse 3=CallError 4=Response 5=Error 6=Call
<bin: method name>    0xC4 u8 | 0xC5 u16-BE | 0xC6 u32-BE, then bytes
<bin: payload>        same encoding
```

There are **no request ids**. The method name is the correlation key, and at most one request is in
flight. Payloads are UTF-8 JSON, except `getSourceFile`, whose response is the raw binary AST.

The JS client wraps this in `Atomics.wait`, Windows named pipes and `readSync` loops. None of that is
protocol - it exists because Node cannot do synchronous I/O on its own stdio. .NET's
`Process.StandardInput.BaseStream` is an ordinary blocking stream, so the client just uses it. Do not
reach for the `StreamReader`/`StreamWriter` wrappers: they apply text translation and corrupt the
stream.

---

## 4. Using it

```fsharp
open System.IO
open Xantham.TsGo

let exe = TsGo.locate repoRoot |> Option.get      // or set XANTHAM_TSGO_EXE
use channel = new TsGoChannel(exe, projectDir)

let session = Session channel
session.Initialize()

let snapshot = session.OpenProjects [ Path.Combine(projectDir, "tsconfig.json") ]
let project = List.head snapshot.Projects

// The whole file's AST in one round trip.
let file = session.GetSourceFile(snapshot, project, Path.Combine(projectDir, "main.ts"))

for index in Ast.descendants file Ast.Root do
    if Ast.kind file index = 79u then                     // Identifier
        printfn $"{Ast.pos file index}..{Ast.endPos file index}"

// Checker queries do cost a round trip each.
match session.GetTypeAtPosition(snapshot, project, mainTs, position) with
| ValueSome node -> printfn $"{node}"
| ValueNone -> ()

session.Release snapshot
```

`position` is an ordinary .NET string index - see §6.1. Disposing the channel closes stdin and waits,
then kills the process if it has not exited.

The compiler is **not vendored**. `TsGo.locate` checks `XANTHAM_TSGO_EXE`, then walks up from the
given directory looking for a platform package, trying two layouts in order:

| Layout | Path under `node_modules` |
| --- | --- |
| TypeScript 7 and later | `@typescript/typescript-<rid>/lib/tsc(.exe)` |
| `native-preview`, superseded | `@typescript/native-preview-<rid>/lib/tsgo(.exe)` |

`tests/Xantham.TsGo.Tests/package.json` pins the compiler for the live tests; run `npm install`
there to enable them. They skip rather than fail when it is missing.

---

## 5. Extending it

### 5.1 Adding a method

Almost every method is a scoped JSON request. The shape is uniform enough that adding one is three
lines on `Session`:

```fsharp
member this.GetSymbolsInScope(snapshot, project, file: string, position: int, meaning: int) =
    let payload = this.Scoped(snapshot, project)
    payload["file"] <- normalize file
    payload["position"] <- position
    payload["meaning"] <- meaning
    request "getSymbolsInScope" payload
```

Two rules that are easy to get wrong:

- **Normalize paths.** tsgo rejects anything not absolute and forward-slashed, with a bare error
  string and no code. `normalize` handles it; use it on every path parameter.
- **Check the shipped build, not upstream.** `probe4.mjs` in `tmp/tsgo-probe/` enumerates which
  methods actually exist - 115 shipped against 137 on `main`. Writing against upstream docs produces
  methods that fail at runtime.

### 5.2 Typing the results

Checker results are `JsonNode` on purpose. The method surface drifts between builds and pinning F#
records to it now would mean rewriting them next release - `snapshot` is an integer, diagnostics use
`text` and `code` rather than `message`, and the reports disagreed with the server on both.

When typing does happen, do it **per method at the point of use**, not as one exhaustive model, and
keep the types tolerant: ignore unknown fields and treat missing ones as optional. The parts worth
typing first are the ones already stable and already exercised: diagnostics
(`fileName`, `pos`, `end`, `code`, `category`, `text`) and the symbol record
(`id`, `name`, `flags`, `checkFlags`, `declarations`, `valueDeclaration`).

### 5.3 Navigating the AST by property name

`Ast.childAtOrder` takes a numeric slot. Mapping slot numbers to property names
(`declarationList`, `initializer`, ...) requires the per-kind table generated in
`dist/api/node/node.generated.js` as `childProperties`. That table is **not ported** - it is large
and machine-generated, and the PoC did not need it.

To port it, transcribe `childProperties` into an F# array indexed by `SyntaxKind`, then:

```fsharp
let childByName file index name =
    childProperties[int (Ast.kind file index)]
    |> Array.tryFindIndex ((=) name)
    |> Option.bind (fun order -> Ast.childAtOrder file index order |> ValueOption.toOption)
```

Generating it from the JS rather than hand-writing it is strongly preferable; it changes with the
compiler.

### 5.4 Extended and structured data

`Ast.data` returns `Extended index` for nodes whose payload lives in the extended-data array, and the
blob has a structured-data section encoded with general MessagePack (arrays, uints, strings, bools).
Neither is decoded yet - `Msgpack` implements only the frame envelope, not the general reader.

The visible consequence is that `SourceFile` extended data - which holds the file text, file name and
path indices - is unread, so the client cannot yet recover node text from the blob alone. The tests
work around this by reading the fixture from disk and slicing it with `pos`/`end`, which is sound
because positions are UTF-16 (§6.1) but does mean the blob is not self-contained in F# yet. Decoding
the twelve `RemoteSourceFile` fields from `dist/api/node/node.js:264-297` is the natural next step.

### 5.5 Virtual filesystem callbacks

`TsGoChannel` takes an optional callback dictionary and passes `--callbacks=<names>`. The server then
sends `MSG_CALL` frames mid-request, which the pump answers inline before the response arrives. The
mechanism is implemented and the frames are handled, but **no live test exercises it** - treat it as
unverified.

Two traps if you turn it on: the argument arrives **JSON-encoded** (a path is `"C:/..."` with quotes,
so it must be parsed, not used raw), and `null` is not the same as an absent property - the
distinction changes module resolution.

### 5.6 The async channel

Not implemented, and probably not wanted. It swaps the envelope for JSON-RPC 2.0 against the same
serial server, so it buys correlation machinery for no concurrency, and it base64s the AST blob -
33% more bytes plus a decode pass per file. It is worth building only to multiplex the API over an
existing LSP connection (`custom/initializeAPISession`) or to sit on a socket.

---

## 6. Traps

### 6.1 Two coordinate systems in one blob

**Node `pos`/`end` and the `position` request parameter are UTF-16 code units** - the same units as a
.NET string index, so they pass through with no conversion. This is verified end to end, not assumed:
the `unicode.ts` fixture is 31 UTF-16 code units and 34 UTF-8 bytes, and the test asserts the
SourceFile ends at 31, that `pos`/`end` slice the .NET string correctly past a 4-byte character, and
that `getSymbolAtPosition` finds `yy` at 23 but nothing at 26.

**String-table offsets in the same blob are UTF-8 byte offsets.** The two agree for ASCII, which is
exactly what makes mixing them up survive casual testing. `Ast.getString` handles it; anything new
that reads the string table must too.

### 6.2 Endianness flips mid-message

The msgpack envelope is **big-endian**; the AST inside it is **little-endian**. A wrong `bin16`
length desynchronises the stream, and the symptom is a hang, not an exception - the reader blocks
waiting for bytes that will never come. The unit tests pin both header shapes for this reason.

### 6.3 WTF-8

String-table strings are WTF-8: an unpaired UTF-16 surrogate is encoded as `ED A0-BF 80-BF`, which
strict UTF-8 rejects. `Encoding.UTF8.GetString` replaces those with U+FFFD and corrupts the
identifier silently. TypeScript permits them, so `Wtf8.decode` exists and has a fast path for the
common case where no `0xED` appears at all.

### 6.4 Process hygiene

- **stderr must be drained** or the pipe fills and the child blocks forever. `TsGoChannel` reads it
  on a background task and exposes it as `Diagnostics`, which is the first thing to look at when a
  call fails.
- **There is no cancellation.** A long `getSemanticDiagnostics` can only be escaped by killing the
  process. Any timeout policy has to be built above the channel.
- **Handles are snapshot-scoped** with no per-object release, so a long-lived session that queries
  many types grows until `Release`. `NodeHandle` is resolved client-side against the cached blob;
  it means nothing once that blob is gone, and the server will not tell you.

### 6.5 Serial by construction

One request in flight, correlated by method name. `TsGoChannel.Request` is not thread-safe and should
not be made to look concurrent - the server is not. If parallelism is ever needed, run several
processes.

---

## 7. Tests

`tests/Xantham.TsGo.Tests` splits deliberately:

- **`Protocol.fs`** needs no server. Frame round-trips across every `bin` width boundary
  (0/1/255/256/65535/65536), all six message types, the two big-endian header shapes asserted as
  literal bytes, and the WTF-8 cases including one that asserts `Encoding.UTF8` disagrees - the bug
  the decoder exists to prevent.
- **`Live.fs`** starts a real `tsc` against `fixtures/`. It skips cleanly when the binary is absent
  - so check the counts, not just the colour: 31 tests and 0 ignored means the live tests ran, 23
  and 1 ignored means they did not.

The fixtures earn their keep. `main.ts` carries a deliberate type error (TS2322) so diagnostics have
something to find, and `unicode.ts` is constructed so UTF-16 and UTF-8 offsets disagree - without a
character outside the BMP, every position test passes under either interpretation.

Each live test gets a fresh session. Sharing one would be faster, but a failing test would then
poison its successors through snapshot state.

---

## 8. Version pinning

Verified against `typescript@7.0.2` (`tsc --version` reports `Version 7.0.2`). Originally built
against `@typescript/native-preview@7.0.0-dev.20260707.2`.

**The move upstream cost nothing.** Everything this client depends on was checked against 7.0.2 and
is byte-identical: the `[type, method, payload]` framing, the six message type numbers, the spawn
arguments (`--api --cwd`, `--callbacks=`), AST protocol version 5 with the same header offsets, node
length and masks, and the parameter names of every method `Session.fs` sends. The preview build's
method deficit is closed - it is now the same surface, because it is now the same repository. Only
the package name and executable name changed, which is why the only code change was `TsGo.locate`.

**Still pin it.** This remains the *unstable* API: the method surface grows between builds and
parameter names have moved before. The AST blob carries a protocol version in the top byte of its
header and `Ast.read` rejects anything but 5, so a format change fails loudly. The JSON method
surface has no such guard and will fail as a missing field or a null.

To see what a version bump changed, diff the method census:

```bash
grep -oh 'apiRequest("[a-zA-Z]*"' node_modules/typescript/dist/api/sync/*.js   | sed 's/.*apiRequest("//;s/"//' | sort -u
```

The parameter names are worth a second look on any bump - they are literal object keys at the call
sites in `dist/api/sync/api.js`, and a renamed one fails as a null result rather than an error.

---

## 9. Corrections to the research documents

Found by the tests failing against the real server:

1. **Children are not contiguous.** `tsgo-protocol.md` sketches child lookup as
   `firstChildIndex + rank`. The real implementation walks the `next` sibling pointer `rank` times
   from `index + 1`, because a child's own subtree sits between it and its next sibling. The rank
   arithmetic is equivalent; the indexing is not. Contiguity returns plausible wrong nodes rather
   than failing.
2. **`snapshot` is an integer**, not a string.
3. **Diagnostics carry `text` and `code`**, not `message`.

---

## 10. Next steps

Roughly in order of value:

1. Decode `SourceFile` extended data (§5.4) so node text comes from the blob rather than a separate
   disk read. This is what makes the client self-contained.
2. Port `childProperties` (§5.3) so the AST can be navigated by property name.
3. Type the two stable payloads - diagnostics and symbols (§5.2).
4. Exercise the FS callbacks (§5.5), which are implemented but unverified.
5. Benchmark against a real project. Nothing here has been measured at scale; the performance
   argument in §2 rests on the native-route agent's numbers, and no IPC baseline was ever timed.
