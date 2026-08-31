# tsgo "unstable API" — protocol specification

> **Packaging note (2026-08-30).** This document was researched against the
> `@typescript/native-preview` preview package. TypeScript 7 has since shipped the Go compiler from
> `microsoft/TypeScript` itself: the npm package is `typescript`, the platform packages are
> `@typescript/typescript-<rid>`, and the executable is named `tsc` rather than `tsgo`. Everything
> below about the *protocol* was re-verified against `typescript@7.0.2` and still holds byte for
> byte; only the package and executable names have changed. Read `tsgo`/`tsgo.exe` below as
> `tsc`/`tsc.exe`, and see §8 of [`tsgo-fsharp-client.md`](tsgo-fsharp-client.md).

> **Source-of-truth note (2026-08-31).** `microsoft/typescript-go` is archived and its local
> checkout has been deleted. Do not clone or cite it. All compiler research goes against
> `microsoft/TypeScript@main` or the installed `typescript` 7.x npm package - see the
> "TypeScript 7 compiler sources" section of `AGENTS.md` for the name and path translations.

Target: writing a **pure .NET / F# client** against the `tsgo` API server, without Node.

**Ground truth for this document**

- Installed npm package `@typescript/native-preview@7.0.0-dev.20260707.2`
  at `C:\Users\shaya\RiderProjects\Xantham\tmp\tsgo-probe\node_modules\@typescript\native-preview\`
- Native binary `...\node_modules\@typescript\native-preview-win32-x64\lib\tsgo.exe`
- Live byte traces captured with `probe*.mjs` in `C:\Users\shaya\RiderProjects\Xantham\tmp\tsgo-probe\`
  (see the *Evidence index* at the end — each probe script is reusable by the implementer).
- Upstream Go source (`microsoft/typescript-go`, branch `main`) consulted via `gh api` for
  `internal/api/protocol_msgpack.go`, `internal/api/proto.go`, `internal/api/session.go`.
  Upstream `main` is **ahead** of the shipped build; where they differ the shipped build wins and
  the drift is flagged.

Statements are file:line-cited. Anything not directly verified is marked **UNVERIFIED** or **TODO**.

---

## 1. Executive summary

`tsgo.exe` is a single native executable that, given `--api`, becomes a **long-lived, strictly serial
RPC server on stdio**. Two wire protocols come out of the same server:

| Mode | Flag | Envelope | Used by |
|---|---|---|---|
| sync | `--api` | custom **MessagePack 3-tuple** frames | `@typescript/native-preview/unstable/sync` |
| async | `--api --async` | **LSP-style JSON-RPC 2.0** (`Content-Length` headers) | `.../unstable/async` |

In **both** protocols the actual request/response payload is **UTF-8 JSON**; MessagePack is only the
envelope. The one exception is binary responses (the encoded AST), which are raw bytes in the msgpack
payload slot and base64 inside a JSON object under JSON-RPC.

There is **no non-IPC route** in the shipped artifact: no DLL, no `c-shared` library, no exported ABI.
The only shipped native thing is `tsgo.exe`. (A second route exists in principle — `tsgo --lsp` exposes
a `custom/initializeAPISession` request that hands out an API channel — but it is still IPC.)

Verdict up front: **a pure .NET client is easy**, and it should target the **sync (MessagePack) channel**
over plain redirected stdio. All the awkward machinery in the JS client (`Atomics.wait`, Windows named
pipes, `readSync` loops) exists solely because Node cannot do synchronous I/O on its own stdio pipes.
.NET can, so none of it is needed. See §10.

---

## 2. Launch

### 2.1 Executable resolution

`lib/getExePath.js` resolves `@typescript/native-preview-${process.platform}-${process.arch}/lib/tsgo`
(`.exe` on win32). A .NET client would ship or locate the binary itself; there is nothing
protocol-relevant here.

### 2.2 Authoritative flag list

`tsgo.exe --api --help`:

```
Usage api:
  -async
        use the JSON-RPC protocol instead of MessagePack (for the async API)
  -callbacks string
        comma-separated list of FS callbacks to enable
        (readFile,fileExists,directoryExists,getAccessibleEntries,realpath)
  -cwd string
        current working directory (default <cwd of process>)
  -pipe string
        use a named pipe or Unix domain socket for communication instead of stdio
  -timing
        collect per-request server processing time, folded into the client's timing snapshot
```

`tsgo.exe --lsp --help` offers `-pipe`, `-pprofDir`, `-socket`, `-stdio`.

### 2.3 Sync launch (recommended for .NET)

`dist/api/sync/client.js:9-54`:

```js
const args = ["--api", "--cwd", cwd];
if (enabledCallbacks.length > 0) args.push(`--callbacks=${enabledCallbacks.join(",")}`);
if (collectTiming) args.push("--timing");
const channel = new SyncRpcChannel(resolveExePath(options), args, collectTiming);
```

- argv: `tsgo.exe --api --cwd <abs path> [--callbacks=a,b,c] [--timing]`
- cwd: anything; `--cwd` is what the server uses for path resolution, not the process cwd.
- stdio: stdin and stdout redirected as **raw binary** — no text translation, no encoding conversion,
  no `StreamReader`. stderr can be inherited or captured; it carries panics/log noise, not protocol.
- The process is **long-lived**: one process serves the whole session. Requests are strictly serial —
  the method name doubles as the correlation ID, so there is at most one in-flight request.
- The sync client **throws** `"Socket connections not yet supported in sync client"` if given socket
  options (`dist/api/sync/client.js`).

Shutdown: close stdin / kill the process. The async client sends `SIGTERM`
(`dist/api/async/client.js`). There is no protocol-level `shutdown` method in the shipped build.

### 2.4 Async launch

`dist/api/async/client.js:32-72`:

```js
["--api", "--async", "--cwd", options.cwd ?? process.cwd()]
```

with `StreamMessageReader(process.stdout)` / `StreamMessageWriter(process.stdin)` from vendored
`vscode-jsonrpc`, or `net.createConnection(options.pipe)` when a pipe is supplied.

### 2.5 `--pipe` is a Node workaround, not a requirement

`dist/api/syncChannel.js:89-123` builds `\\.\pipe\tsgo-sync-<pid>-<Date.now()>` on Windows and passes
`--pipe <name>`, then retries the connect ~500 times with 10 ms sleeps. This exists because Node cannot
`readSync`/`writeSync` a libuv stdio pipe on Windows. **`probe6.mjs` proves the server speaks the
identical msgpack protocol over plain redirected stdio on Windows**, so a .NET client should use stdio
and skip `--pipe` entirely.

---

## 3. Transport and framing

### 3.1 Sync protocol — MessagePack 3-tuple frames

Defined in `dist/api/syncChannel.js` (framing, lines 284-337 `writeTuple`, 344-388 `readTuple`/`readBin`)
plus `dist/api/node/msgpack.js` (primitives), and confirmed byte-for-byte against upstream
`internal/api/protocol_msgpack.go`.

Every frame, in both directions, is exactly:

```
0x93                      fixarray, length 3
<message type>            positive fixint (0x00-0x7F) or 0xCC <u8>
<bin: method name>        0xC4 <u8 len> | 0xC5 <u16be len> | 0xC6 <u32be len>, then len bytes
<bin: payload>            same bin encoding
```

**Critical detail:** msgpack length prefixes are **big-endian**. The binary AST format in §5 is
**little-endian**. Do not mix them up.

`dist/api/node/msgpack.js:5-38`:

```js
export const MSGPACK_FIXARRAY3 = 0x93;
export const MSGPACK_BIN8  = 0xc4;
export const MSGPACK_BIN16 = 0xc5;
export const MSGPACK_BIN32 = 0xc6;
export const MSGPACK_UINT8 = 0xcc;
export function binHeaderSize(len) { if (len < 0x100) return 2; if (len < 0x10000) return 3; return 5; }
```

The same file also has a general `MsgpackWriter`/`MsgpackReader` (array header `0x90|n` / `0xdc` / `0xdd`;
uint `0xcc`/`0xcd`/`0xce`; str `0xa0|n` / `0xd9` / `0xda` / `0xdb`; bool `0xc2`/`0xc3`). Those are used
**only** for the *structured data* section inside the binary AST (§5.5), never for the frame envelope.

### 3.2 Message types

`dist/api/syncChannel.js:17-23`, matching upstream `MessageType*` constants:

| Value | Name | Direction | Meaning |
|---|---|---|---|
| 0 | Unknown | — | invalid |
| 1 | `MSG_REQUEST` | client → server | invoke an API method |
| 2 | `MSG_CALL_RESPONSE` | client → server | successful reply to a server callback |
| 3 | `MSG_CALL_ERROR` | client → server | failed reply to a server callback |
| 4 | `MSG_RESPONSE` | server → client | successful method result |
| 5 | `MSG_ERROR` | server → client | method failed |
| 6 | `MSG_CALL` | server → client | server invokes a *client* callback (virtual FS) |

### 3.3 Correlation

There are **no request IDs**. The method-name slot is the correlation key and the channel is strictly
serial: send one `MSG_REQUEST`, then read frames until you get `MSG_RESPONSE`/`MSG_ERROR`. Any `MSG_CALL`
frames arriving in between are FS callbacks that must be answered inline (with `MSG_CALL_RESPONSE` /
`MSG_CALL_ERROR` carrying the **same method name as the call**) before the response arrives.
`dist/api/syncChannel.js:213-249` implements exactly this loop; anything else is a protocol error.

Upstream `protocol_msgpack.go` notes the server tracks pseudo-IDs internally; nothing is on the wire.

### 3.4 Async protocol — JSON-RPC 2.0

Standard LSP framing: `Content-Length: <n>\r\n\r\n<json>`. Method names and params objects are identical
to the sync protocol. Errors come back as JSON-RPC errors — observed code `-32603` (internal error) for
an unknown method (`probe3.mjs`). Binary results are wrapped as `{"data": "<base64>"}` and decoded with
`Buffer.from(response.data, "base64")` in `dist/api/async/client.js`. Server → client callbacks are
JSON-RPC requests in the reverse direction.

### 3.5 Payload encoding

- Request payload: `JSON.stringify(params)` as UTF-8 bytes. For no params, the literal 4 bytes `null`.
- Response payload: UTF-8 JSON, **or** raw binary for AST-returning methods (`getSourceFile`).
- An empty response payload means `undefined` (`dist/api/sync/client.js`: `apiRequest` returns
  `undefined` when the result string is empty).
- **Callback payloads are JSON-encoded too** — the argument to `readFile` arrives as `"C:/..."`
  *with quotes*, i.e. it must be `JSON.parse`d. Verified in `probe5.mjs`.

---

## 4. Worked example — annotated hex

All captured live (`probe.mjs`, `probe6.mjs`, `probe7.mjs`).

### 4.1 `initialize`

Request (20 bytes):

```
93                                   array(3)
01                                   MSG_REQUEST
c4 0a 69 6e 69 74 69 61 6c 69 7a 65  bin8(10) "initialize"
c4 04 6e 75 6c 6c                    bin8(4)  "null"
```

Response:

```
93
04                                   MSG_RESPONSE
c4 0a "initialize"
c4 72 {"useCaseSensitiveFileNames":false,"currentDirectory":"C:\\Users\\..."}
```

### 4.2 Error frame

```
93
05                                   MSG_ERROR
c4 0c "nosuchmethod"
c4 37 api: invalid request: unknown API method "nosuchmethod"
```

The error payload is a **bare UTF-8 string, not JSON**. See §8.

### 4.3 Server callback round trip (`--callbacks=readFile,...`)

```
server -> 93 06 c4 08 "readFile" c4 46 "C:/.../tsconfig.json"     MSG_CALL, JSON-encoded string arg
client -> 93 02 c4 08 "readFile" c4 24 {"content":"..."}          MSG_CALL_RESPONSE
```

### 4.4 `getSourceFile` → 652-byte binary AST

Source file `src/a.ts` containing exactly:

```ts
export const x: number = 1;
```

(28 bytes including the trailing newline.) Full annotated dump produced by `probe7.mjs`.

**Header (44 bytes, all little-endian u32):**

```
offset  value       meaning
+0      00000005    metadata: PROTOCOL_VERSION(5) << 24 ... see note below
+4      eb20fc4f    hash lo0
+8      39ac8c2d    hash lo1
+12     212f18e3    hash hi0
+16     0990bf7c    hash hi1        -> content hash 7cbf9009 e3182f21 2d8cac39 4ffc20eb
+20     00000000    parse options key
+24     2c000000    string table offsets @ 44
+28     54000000    string table @ 84
+32     e8000000    extended data @ 232
+36     20010000    structured data @ 288
+40     20010000    nodes @ 288
```

Note: `encoder.js:263` writes `const metadata = PROTOCOL_VERSION << 24;` — the observed `00000005`
read as LE u32 is `0x05000000`, i.e. version 5 in the high byte. Read the version as
`(u32_le >> 24) & 0xFF`.

**String table offsets** (10 u32s, pairs `[start, end)` interleaved as `offsets[i]`, `offsets[i+1]`
for string index `i` = 0, 2, 4, 6, 8):

```
00000000 1c000000   [0,28)   -> "export const x: number = 1;\n"       (whole file text)
1c000000 58000000   [28,88)  -> "C:/Users/.../src/a.ts"               (fileName)
58000000 94000000   [88,148) -> "c:/users/.../src/a.ts"               (path, lowercased)
0d000000 0e000000   [13,14)  -> "x"                                   (identifier text)
19000000 1a000000   [25,26)  -> "1"                                   (numeric literal text)
```

Note strings 0-2 are *stored* in the string table; strings 3-4 are slices back into string 0
(the file text) — the offsets are into the same byte array.

**Extended data (u32 array):**

```
index 0..11  SourceFile record:
  00000000  textIndex        = 0
  02000000  fileNameIndex    = 2
  04000000  pathIndex        = 4
  00000000  languageVariant  = 0
  03000000  scriptKind       = 3
  03000000 x6                NO_STRUCTURED_DATA (0xFFFFFFFF)? see §5.6 TODO
  03000000
index 48     NumericLiteral record: [textIndex = 8, tokenFlags = 0]
```

**UNVERIFIED:** the exact SourceFile extended-data field order above is reconstructed from
`dist/api/node/node.js:264-297` (`RemoteSourceFile` reads +0 +4 +8 +12 +16 +20 +24 +28 +32 +36 +40 +44)
and should be re-derived from that file before implementing.

**Nodes (28 bytes each: kind u32, pos u32, end u32, next u32, parent u32, data u32, flags u32):**

```
idx  kind                        pos  end  next  parent  data        flags
1    307 SourceFile                0   28     0       0  0x80000000
2    NodeList (0xFFFFFFFF)         0   27    12       1  0x00000001
3    244 VariableStatement         0   27     0       2  0x00000003
4    NodeList                      0    6     6       3  0x00000001
5     94 ExportKeyword             0    6     0       4  0
6    262 VariableDeclarationList   6   26     0       3  0x00000001  0x2
7    NodeList                     12   26     0       6  0x00000001
8    261 VariableDeclaration      12   26     0       7  0x0000000d
9     79 Identifier               12   14    10       8  0x40000006
10   150 NumberKeyword            15   22    11       8  0
11     8 NumericLiteral           24   26     0       8  0x80000030
12     1 EndOfFileToken           27   28     0       1  0
```

Index 0 is reserved/absent; node indices are 1-based into the nodes section.

---

## 5. The binary AST encoding (protocol version 5)

Only `getSourceFile` returns it (server → client), and only `printNode` accepts it (client → server,
via `Emitter.printNode`, which base64s an `encodeNode` result). Everything else is JSON.

### 5.1 Layout constants

`dist/api/node/protocol.js`:

```js
export const PROTOCOL_VERSION = 5;
export const HEADER_OFFSET_METADATA              = 0;
export const HEADER_OFFSET_HASH_LO0              = 4;
export const HEADER_OFFSET_HASH_LO1              = 8;
export const HEADER_OFFSET_HASH_HI0              = 12;
export const HEADER_OFFSET_HASH_HI1              = 16;
export const HEADER_OFFSET_PARSE_OPTIONS         = 20;
export const HEADER_OFFSET_STRING_TABLE_OFFSETS  = 24;
export const HEADER_OFFSET_STRING_TABLE          = 28;
export const HEADER_OFFSET_EXTENDED_DATA         = 32;
export const HEADER_OFFSET_STRUCTURED_DATA       = 36;
export const HEADER_OFFSET_NODES                 = 40;
export const HEADER_SIZE = 44;

export const NODE_LEN = 28;
export const NODE_OFFSET_KIND   = 0;
export const NODE_OFFSET_POS    = 4;
export const NODE_OFFSET_END    = 8;
export const NODE_OFFSET_NEXT   = 12;
export const NODE_OFFSET_PARENT = 16;
export const NODE_OFFSET_DATA   = 20;
export const NODE_OFFSET_FLAGS  = 24;

export const KIND_NODE_LIST = 0xFFFFFFFF;
export const NODE_DATA_TYPE_CHILDREN = 0x00000000;
export const NODE_DATA_TYPE_STRING   = 0x40000000;
export const NODE_DATA_TYPE_EXTENDED = 0x80000000;
export const NODE_STRING_INDEX_MASK  = 0x00FFFFFF;
export const NODE_EXTENDED_DATA_MASK = 0x00FFFFFF;
```

`dist/api/node/node.infrastructure.js:7-10`: `NODE_DATA_TYPE_MASK = 0xc0000000`, `NODE_CHILD_MASK = 0xff`.

**All multi-byte integers in this format are little-endian.**

### 5.2 Header

44 bytes as above. `metadata` carries the protocol version in its top byte; a client **must** reject
anything other than 5. The four hash words are a content hash used for cache validation. `parseOptions`
is a key identifying the parse options used.

### 5.3 String table

`stringTableOffsets` is a u32 array of `[start, end)` pairs; string index `i` uses `offsets[i]` and
`offsets[i+1]` (i.e. entries are consumed pairwise, so a "string index" as stored in node data is an
index *into the offsets array*, stepping by 2). Bytes live in the string table section and are
**WTF-8** (§5.7), not plain UTF-8.

### 5.4 Node records

28 bytes each; index 1 is the SourceFile. `next` is the next sibling within a node list (0 = end).
`parent` is a node index. `kind` is the TypeScript `SyntaxKind` numeric value, except `0xFFFFFFFF`
which marks a synthetic **node list**.

`data` is a tagged word: the top 2 bits (`0xc0000000`) select
`CHILDREN` (0x0), `STRING` (0x40000000) or `EXTENDED` (0x80000000).

- **STRING**: `data & 0x00FFFFFF` is a string-table index.
- **EXTENDED**: `data & 0x00FFFFFF` is an index into the extended-data u32 array.
- **CHILDREN**: `data` is a **child mask** — a bitmap over the node's declared child slots.
  `dist/api/node/node.generated.js:265-320` computes a child's node index by popcounting the mask below
  the requested slot:

  ```js
  // conceptually
  const bit = 1 << slot;
  if ((mask & bit) === 0) return undefined;
  const rank = popcount(mask & (bit - 1));
  return firstChildIndex + rank;
  ```

  with the byte-wise popcount `((0xff & ...) ...)` sequence in that file. **Reimplement popcount
  directly in .NET (`BitOperations.PopCount`)**; the JS byte-splitting is an artifact of JS bitwise ops.

`node.generated.js:240-248` reads the string table with `index*4` / `(index+1)*4` and the next node with
`(index+1)*NODE_LEN + NODE_OFFSET_PARENT`.

### 5.5 Structured data

Section starting at `HEADER_OFFSET_STRUCTURED_DATA`, encoded with the **general MessagePack**
reader in `dist/api/node/msgpack.js` (arrays/uints/strings/bools). `NO_STRUCTURED_DATA = 0xFFFFFFFF`
marks its absence for a given slot. In the worked example the structured-data offset equals the nodes
offset, i.e. the section is empty.

### 5.6 SourceFile extended data

`dist/api/node/node.js:264-297` — `RemoteSourceFile` reads twelve u32 fields at +0 … +44 of its extended
data record: text index, fileName index, path index, languageVariant, scriptKind, and six
structured-data slots (each possibly `NO_STRUCTURED_DATA`), plus one more. **TODO:** transcribe the exact
field names from that file into this table before implementing; the worked-example dump above shows the
shape but the names are partly inferred.

### 5.7 WTF-8 strings

`dist/api/node/wtf8.js`. Strings are UTF-8 **except** that unpaired UTF-16 surrogates are encoded as
3-byte sequences `ED A0-BF 80-BF` (which is illegal in strict UTF-8). The decoder special-cases a leading
`0xED` and reconstructs the code unit as:

```js
String.fromCharCode(0xD000 | ((b1 & 0x3F) << 6) | (b2 & 0x3F))
```

Everything else goes through a normal `TextDecoder`.

**.NET impact:** `Encoding.UTF8.GetString` will replace those sequences with U+FFFD, silently corrupting
identifiers that contain lone surrogates. This is rare but real (TypeScript permits them in string
literals). A correct client needs its own WTF-8 decoder; a pragmatic one can scan for `0xED` and fall
back to UTF-8 otherwise.

### 5.8 Position encoding — VERIFIED

**Node `pos`/`end`, and the `position` parameter of checker requests, are UTF-16 code units.**
**String table offsets are UTF-8 byte offsets.** These are two different coordinate systems in the
same blob.

Fixture `u/b.ts` (`probe8.mjs`), chosen so the two disagree:

```ts
const s = "é😀"; const yy = 2;
```

UTF-16 length 31, UTF-8 length 34. `yy` begins at UTF-16 index 23 / UTF-8 byte 26.

Result:

```
node[1]  kind=307 SourceFile      pos=0  end=31     <- 31 = UTF-16 length, not 34
node[13] kind=79  Identifier "yy" pos=22 end=25     <- 22 includes leading trivia; 25 = 23+2
getSymbolAtPosition(position=23) => {"id":1,...,"name":"yy",...}
getSymbolAtPosition(position=26) => null
```

Both axes agree: the server speaks UTF-16 code units on the RPC surface and in the node table. This is
exactly what .NET wants — a `string` index is directly usable as a `position`, and `pos`/`end` slice a
.NET string correctly with no conversion.

The upstream ambiguity (typescript-go is UTF-8 internally; `FormatNodeForInsertionParams` in `proto.go`
documents `Position` as "UTF-16 code-unit offset") resolves in favour of the documented UTF-16.

### 5.9 The coordinate-system trap

`probe9.mjs` dumps the string table of the same file:

```
stringTableOffsets: [0,34, 34,92, 92,150, 6,7, 11,17, 26,28, 31,32]

str[0]  [0,34)   = "const s = \"é😀\"; const yy = 2;\n"    (whole file text)
str[2]  [34,92)  = "C:/Users/.../u/b.ts"                     (fileName)
str[4]  [92,150) = "c:/users/.../u/b.ts"                     (path)
str[6]  [6,7)    = "s"
str[8]  [11,17)  = "é😀"
str[10] [26,28)  = "yy"
str[12] [31,32)  = "2"
```

Note `str[10] = [26,28)` — the **UTF-8 byte** offsets of `yy`, while the corresponding
`Identifier` node reports `pos=22 end=25` in **UTF-16 code units**. Both refer to the same two
characters.

Consequences for a .NET implementation:

- Keep the string table as a `byte[]` and decode slices with `[start, end)` **byte** ranges. Do not
  index it with a node's `pos`/`end`.
- Keep the file text as a decoded .NET `string` and index it with `pos`/`end`. Do not index it with
  string-table offsets.
- A node's text is available two ways — via its string-table index (bytes) or by slicing the file text
  with `pos`/`end` (UTF-16). They agree in content; they must not share an offset variable.
- This also confirms §5.3: the value in a `NODE_DATA_TYPE_STRING` node's data field is an index directly
  into the offsets array (here `0x4000000a` → index 10 → `offsets[10], offsets[11]` = `[26,28)` = `yy`),
  not a pair index.

### 5.10 Why this matters for cost

`getSourceFile` returns the **entire file's AST in one blob**. Every node is then materializable locally
with **zero further round-trips** — child navigation, positions, and node text are all pure reads over
the byte array. Exhaustive AST traversal over IPC is therefore economical. Only *checker* queries
(types, symbols) cost round-trips.

---

## 6. Object model and handles

### 6.1 Identifier types

From upstream `internal/api/proto.go` and `dist/api/proto.d.ts`:

| Type | Wire form | Scope |
|---|---|---|
| `SnapshotID` | string | session |
| `ProjectID` | string | snapshot |
| `SymbolID` | number (opaque integer) | snapshot |
| `TypeID` | number | project (within snapshot) |
| `SignatureID` | number | project (within snapshot) |
| `NodeHandle` | string `"index.kind.path"` | resolved **client-side** |

`DocumentIdentifier` = `{ fileName }` or a `lsproto.DocumentUri`.

### 6.2 NodeHandle is client-side

`dist/api/node/node.generated.js:156-158`:

```js
get id() { return `${this.index}.${this.kind}.${this.sourceFile.path}`; }
```

and `dist/api/node/node.js:264-297` parses `"index.kind.path"` back by looking up the cached binary AST
for `path` and indexing it. **The server never resolves a NodeHandle by walking anything** — it is a
pointer into the client's own copy of the AST plus enough redundancy to detect staleness. A .NET client
must keep the AST blob alive for as long as it hands out node handles.

### 6.3 Lifetime and release

- **Snapshots** are the unit of lifetime. `updateSnapshot` produces a new `SnapshotID`;
  `release { snapshot }` frees it server-side (`Snapshot.dispose()` in `dist/api/sync/api.js` is exactly
  `client.apiRequest("release", { snapshot: this.id })`).
- **There is no per-object release.** Symbols, types and signatures are freed wholesale when their
  snapshot (or project) goes away. `Checker.dispose()` only clears the *client-side* registry — no
  request is sent.
- Client-side caches in the JS client, which a .NET client will want to mirror:
  - `SourceFileCache` keyed by `(path, parseOptionsKey, contentHash)`
  - `(snapshot, project)` reference counting
  - `SnapshotObjectRegistry` (symbols) and `ProjectObjectRegistry` (types, signatures)
- Invalidation: everything keyed to a snapshot is invalid once that snapshot is released. Handles are
  **not** validated cheaply by the server; using a stale ID is an error (§8).

### 6.4 Snapshot update

`updateSnapshot` ← `{ openProjects?, closeProjects?, openFiles?, closeFiles?, fileChanges? }`
→ `{ snapshot, projects[], changes? }`.

This is the single mutation point: file edits, project open/close, everything.

### 6.5 Lazy handle resolution

Types/symbols/signatures come back as bare integers. The JS client resolves them lazily via
`fetchType`/`fetchSymbol`/`fetchSignature`, and navigates with `{ snapshot, project, objectId }` methods
(the `...Of...` family listed in §7).

---

## 7. Method surface

The shipped build (`7.0.0-dev.20260707.2`) exposes **115** methods; upstream `main` has **137**.
`probe4.mjs` established this by probing method existence — an unknown method produces the distinctive
`unknown API method "..."` error, so existence is cheaply testable. 10 methods present on `main` were
unknown to the shipped build.

### 7.1 Session / lifecycle

| Method | Params | Result |
|---|---|---|
| `initialize` | `null` | `{ useCaseSensitiveFileNames, currentDirectory }` |
| `updateSnapshot` | `{ openProjects?, closeProjects?, openFiles?, closeFiles?, fileChanges? }` | `{ snapshot, projects[], changes? }` |
| `release` | `{ snapshot }` | `true` |
| `ping` | — | — (used as a liveness check in `probe7.mjs`) |

**TODO:** confirm `ping`'s exact params/result shape; it was exercised but not documented here.

### 7.2 Source files and printing

| Method | Params | Result |
|---|---|---|
| `getSourceFile` | `{ snapshot, project, file }` | **binary** AST (§5) |
| `printNode` | `{ data: <base64 AST>, preserveSourceNewlines?, neverAsciiEscape?, terminateUnterminatedLiterals? }` | string |

`printNode` is the **only** client→server AST encode path (`Emitter.printNode`, `encodeNode` +
`uint8ArrayToBase64`).

### 7.3 Diagnostics

Params shape in the shipped build: `{ snapshot, project, ...(file !== undefined ? { file } : {}) }`.

**DRIFT WARNING:** upstream `main`'s `GetDiagnosticsParams` uses `Files []DocumentIdentifier` (plural).
The shipped build uses singular `file`. A client must match the binary it launches.

Methods include `getSyntacticDiagnostics`, `getSemanticDiagnostics`, `getSuggestionDiagnostics`
(**UNVERIFIED:** exact set — re-enumerate from `dist/api/sync/api.d.ts`).

### 7.4 Checker queries

Shape: `{ snapshot, project, location | type | symbol | signature, ... }`. Examples:
`getSymbolAtPosition`, `getTypeAtPosition`, `getTypeOfSymbol`, `getSymbolAtLocation`,
`getTypeOfExpression`.

### 7.5 Handle navigation (the `...Of...` family)

All take `{ snapshot, project, objectId }` (the id being a `SymbolID`, `TypeID` or `SignatureID`):

Symbols: `getParentOfSymbol`, `getMembersOfSymbol`, `getExportsOfSymbol`, `getExportSymbolOfSymbol`.

Types: `getSymbolOfType`, `getAliasSymbolOfType`, `getTargetOfType`, `getFreshTypeOfType`,
`getRegularTypeOfType`, `getTypesOfType`, `getTypeParametersOfType`, `getOuterTypeParametersOfType`,
`getLocalTypeParametersOfType`, `getAliasTypeArgumentsOfType`, `getObjectTypeOfType`,
`getIndexTypeOfType`, `getCheckTypeOfType`, `getExtendsTypeOfType`, `getBaseTypeOfType`,
`getConstraintOfType`, `getTrueTypeOfConditionalType`, `getFalseTypeOfConditionalType`.

Signatures: `getTypeParametersOfSignature`, `getParametersOfSignature`, `getThisParameterOfSignature`,
`getTargetOfSignature`.

### 7.6 Compiler-level, non-session

`transpileModule`, `transpileModuleFromFile`, `transpileDeclaration`, `transpileDeclarationFromFile`
(present on `main`; **UNVERIFIED** in the shipped build — `probe4.mjs` can settle it).

### 7.7 Client callbacks (server → client, `MSG_CALL`)

Enabled per-process with `--callbacks=`. The full set: `readFile`, `fileExists`, `directoryExists`,
`getAccessibleEntries`, `realpath`.

`dist/api/fs.d.ts` defines the virtual FS surface. Semantics detail that matters:
a `null` result means **"absent"** (the file does not exist), while `undefined` means **"fall back to the
real filesystem"**. Getting this backwards silently changes module resolution.

---

## 8. Error model

### 8.1 Sync

`MSG_ERROR` (5) with the same method name and a **bare UTF-8 string** payload — not JSON, not a
structured object:

```
api: invalid request: unknown API method "nosuchmethod"
```

The JS client throws an `Error` with that string as the message. So: no error codes, no error data, no
way to distinguish "bad handle" from "unknown method" other than by string matching.

### 8.2 Async

JSON-RPC error objects; observed `-32603` (internal error) for an unknown method. The rich JSON-RPC error
structure is not actually populated with anything more useful than the sync string.

### 8.3 Callback failures

`MSG_CALL_ERROR` (3), method name + error string payload.

### 8.4 Fatal

A Go panic goes to **stderr** and the process dies; the pending request never completes. A .NET client
must watch for process exit and fail outstanding requests, and should capture stderr for diagnostics.

---

## 9. Versioning and stability

- **The protocol is unversioned.** Only the binary AST format carries a version number
  (`PROTOCOL_VERSION = 5`) and that covers only the AST payload, not the RPC surface.
- The package name is `@typescript/native-preview` and the entry points are literally under
  `unstable/`. Upstream is explicit that both sides must be built from the same tree.
- Observed drift between the shipped `7.0.0-dev.20260707.2` build and upstream `main` at the time of
  writing: **115 methods vs 137**, and `file` vs `files` in diagnostics params. That is meaningful drift
  in a few weeks.
- Mitigation for an F# client:
  1. Pin the `@typescript/native-preview` version and treat the exe as a versioned artifact.
  2. Call `initialize` first and assert the AST protocol version on the first `getSourceFile`.
  3. Probe method existence at startup for anything optional (the `unknown API method` error makes this
     cheap — see `probe4.mjs`).
  4. Keep the JSON payload types tolerant (ignore unknown fields; treat missing fields as optional).

---

## 10. Assessment for a .NET / F# client

### 10.1 Target the sync (MessagePack) channel

Reasons:

1. **The framing is trivial** — one array header byte, one type byte, two length-prefixed byte strings.
   It is perhaps 80 lines of F#. No msgpack library is needed for the envelope; a general msgpack reader
   is needed only for the AST structured-data section (§5.5), and even that is a small subset.
2. **No Node pathologies apply.** `Atomics.wait`, `readSync` loops, `--pipe` on Windows: all of it exists
   because Node's stdio is non-blocking libuv pipes. .NET's `Process.StandardInput.BaseStream` /
   `StandardOutput.BaseStream` are ordinary blocking `Stream`s. `probe6.mjs` demonstrates the server is
   perfectly happy on plain redirected stdio on Windows.
3. **The serial, ID-less protocol matches a synchronous client naturally.** With JSON-RPC you would have
   to implement correlation, cancellation and out-of-order delivery for no benefit — the server is serial
   anyway.
4. **Binary AST without base64.** The sync channel delivers the AST blob raw; the async channel base64s
   it, costing 33% bandwidth and a decode pass on every source file.

The async channel is worth having only if the client must multiplex the API with an LSP connection
(`API.fromLSPConnection` / `custom/initializeAPISession`), or if it wants to sit on a socket.

### 10.2 Implementation sketch

- `Process` with `RedirectStandardInput/Output = true`, **`StandardInputEncoding`/`StandardOutputEncoding`
  irrelevant** — use `BaseStream` and never touch the `StreamReader`/`StreamWriter` wrappers.
- A `Request(method, payloadBytes) -> payloadBytes` primitive that writes one tuple and then loops
  reading tuples, dispatching `MSG_CALL` to a registered FS callback table and returning on
  `MSG_RESPONSE`/`MSG_ERROR`.
- `System.Text.Json` for payloads. Everything is plain JSON objects.
- A `ReadOnlyMemory<byte>`-backed AST reader; `BinaryPrimitives.ReadUInt32LittleEndian` throughout;
  `BitOperations.PopCount` for the child mask.
- A custom WTF-8 decoder (§5.7).

### 10.3 Blockers and nasty surprises

1. **Endianness flip.** msgpack lengths are big-endian; the AST is little-endian. Easy to get wrong,
   and the failure mode for a wrong `bin16` length is a desynchronised stream that looks like a hang.
2. **Callbacks are JSON-encoded strings.** `readFile`'s argument arrives as `"C:/..."` with quotes.
   Missing that produces paths with literal quote characters and mystifying resolution failures.
3. **`null` vs `undefined` in FS callbacks** changes module resolution semantics (§7.7). .NET's JSON
   serialisers make this distinction annoying — you must emit *absent property* vs *explicit null*
   deliberately.
4. **WTF-8** will silently corrupt strings if you use `Encoding.UTF8` (§5.7).
5. **Two coordinate systems in one AST blob** — node `pos`/`end` are UTF-16 code units, string-table
   offsets are UTF-8 bytes (§5.9). Mixing them yields text that is right for ASCII files and subtly
   wrong for everything else, which is the worst possible failure mode.
6. **`NodeHandle` is only meaningful while you hold the AST blob** it points into. Releasing the source
   file cache invalidates handles with no server-side error.
7. **No per-object release** — memory is only reclaimed at snapshot granularity. A long-lived session
   that queries many types will grow until `release`.
8. **stderr must be drained.** If the child writes a lot to stderr and nothing reads it, the pipe fills
   and the child blocks — the classic redirected-process deadlock. Read it on a background task.
9. **No cancellation.** There is no cancel message in the shipped build. A long `getSemanticDiagnostics`
   can only be escaped by killing the process.
10. **Upstream drift** (§9) is fast and unannounced. Pin the version.

### 10.4 RESOLVED — position encoding is UTF-16, but string-table offsets are UTF-8 bytes

**Verified by `probe8.mjs` and `probe9.mjs`.** See §5.9 for the full result. Summary:

- Node `pos`/`end` and the `position` request parameter are **UTF-16 code units** — the same units as
  a .NET `string` index. No translation needed on that axis.
- **String table offsets are UTF-8 byte offsets** into the string-table byte array. The two coordinate
  systems coexist in the same blob.

This is the nastiest trap in the format. See §5.9.

---

## 11. Non-IPC routes

None in the shipped artifact. The npm package contains:

- `tsgo.exe` (the only native file, in `@typescript/native-preview-win32-x64/lib/`)
- JavaScript under `dist/` and `lib/`

There is **no** `.dll`, no `c-shared` export table, no documented ABI. `package.json` exports:

```json
"exports": {
  ".": "./lib/version.cjs",
  "./unstable/sync":  "./dist/api/sync/api.js",
  "./unstable/async": "./dist/api/async/api.js",
  "./unstable/fs":    "./dist/api/fs.js",
  "./unstable/proto": "./dist/api/proto.js",
  "./unstable/ast":   "./dist/ast/index.js"
},
"imports": {
  "#getExePath": "./lib/getExePath.js",
  "#vscode-jsonrpc/node": "./vendor/vscode-jsonrpc/lib/node/main.js"
}
```

Note `dist/api/syncChannel.js` is deliberately **not** exported — importing it by package specifier
fails with `ERR_PACKAGE_PATH_NOT_EXPORTED`; the probes import it by relative path.

The only alternative transport is `tsgo --lsp`, whose server implements a custom
`custom/initializeAPISession` request (string present in the binary) that hands out an API channel —
matching `API.fromLSPConnection` and `ClientSocketOptions` in the JS. Still IPC.

(A separate investigation covers whether typescript-go can be *built* as a c-shared library; that is out
of scope here — this section reports only what the shipped artifact offers.)

---

## 12. Evidence index — probe scripts

All in `C:\Users\shaya\RiderProjects\Xantham\tmp\tsgo-probe\`. They import `syncChannel.js` by relative
path (`./node_modules/@typescript/native-preview/dist/api/syncChannel.js`) because it is not exported.
Fixtures created alongside them: `src/a.ts`, `tsconfig.json`, `u/b.ts`, `u/tsconfig.json`.

| Script | Demonstrates |
|---|---|
| `probe.mjs` | Baseline round trip and the unknown-method error frame. Source of the §4.1/§4.2 hex. |
| `probe2.mjs` | Payloads are UTF-8 JSON in the msgpack `bin` slot. |
| `probe3.mjs` | The async path: `--api --async`, JSON-RPC `Content-Length` framing, `-32603` for unknown methods. |
| `probe4.mjs` | Method-existence probing. Established 115 shipped methods vs 137 on `main`, 10 unknown. Reusable to detect drift. |
| `probe5.mjs` | Virtual FS callbacks (`--callbacks=...`): `MSG_CALL` / `MSG_CALL_RESPONSE` trace, and that callback arguments are **JSON-encoded**. |
| `probe6.mjs` | **The key one for .NET:** msgpack over plain redirected stdio on Windows, spawned the way a non-Node parent would. Proves `--pipe` is unnecessary. |
| `probe7.mjs` | Fully annotated binary AST dump (header, string table, extended data, all 13 nodes) plus `ping`. Source of §4.4. Effectively a reference decoder. |
| `probe8.mjs` | **RUN — decisive.** Settles `pos`/`end`/`position` as **UTF-16 code units** using a fixture with a 2-byte and a 4-byte UTF-8 character (§5.8). |
| `probe9.mjs` | **RUN.** Dumps the string table of the same fixture, showing its offsets are **UTF-8 bytes** — the coordinate-system trap (§5.9). Also confirms the node string-index encoding. |

---

## 13. Status of this document

Transport, framing, error model, handle model and the binary AST format are verified against live byte
traces and are safe to implement from. Outstanding items are refinements, not unknowns:

- [x] Position encoding resolved: **UTF-16 code units** on the RPC surface and in node `pos`/`end`;
      **UTF-8 bytes** for string-table offsets (§5.8, §5.9). `probe8.mjs` / `probe9.mjs`.
- [ ] Transcribe exact `RemoteSourceFile` extended-data field names from `dist/api/node/node.js:264-297`
      into §5.6.
- [ ] Enumerate the full 115-method list with param/result types from `dist/api/sync/api.d.ts`
      (§7 currently groups them by area and names the important ones).
- [ ] Confirm `ping` params/result (§7.1) and whether `transpile*` exists in the shipped build (§7.6).
