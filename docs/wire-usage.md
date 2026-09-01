# Using Xantham.TypeScript.Wire

How to consume the package: install it, get a compiler talking, call the API, read what comes
back. For *navigating* the AST once you have it, see [`wire-navigation.md`](wire-navigation.md).

## Install

```bash
dotnet add package Xantham.TypeScript.Wire
npm install typescript@7.1.0-dev.20260830.1
```

The compiler is **not bundled**. Wire runs the Go `tsc` binary shipped in the `typescript` npm
package as `tsc --api`. The protocol is unversioned, so the npm pin must match the version the
package was generated against — mismatches surface as decode failures, not as a version error.

Targets `net10.0`, `net8.0`, `netstandard2.1`. Only dependency: `System.Text.Json`.

## Locate the executable

```fsharp
open Xantham.TypeScript.Wire

// Walks up from the directory looking for node_modules/@typescript/typescript-<rid>/lib/tsc.
let exe = (Tsc.locate "./my-project").Value
```

`Tsc.locate` returns `string option`. `XANTHAM_TSGO_EXE`, if set to an existing file, wins over
the search. Treat `None` as "the caller has not run `npm install`" and say so — it is the single
most common setup failure.

## Two clients

| | Use | Concurrency |
|---|---|---|
| `TscChannel` | synchronous `Api.*` | **one request at a time**, not thread-safe |
| `TscMailbox` | `Async` `AsyncApi.*`, batches overlapping calls | safe from many callers; 2.1–2.3× under pressure |

Both own a child process and both are `IDisposable` — `use`, don't `let`. Disposal closes stdin,
which is the documented shutdown; a leaked instance leaks a `tsc` process.

## The lifecycle

`initialize` once, then get a **snapshot** and a **project**. Every subsequent request is
addressed by that pair.

### From a tsconfig

```fsharp
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

use channel = new TscChannel(exe, "./my-project")
Api.initialize channel |> ignore

let snapshot =
    Api.updateSnapshot channel
        { UpdateSnapshotParams.Default with
            OpenProjects = ValueSome [| DocumentIdentifier.FileName "./my-project/tsconfig.json" |] }

let project = snapshot.Projects[0].Id
```

### From root files, no tsconfig

```fsharp
let program =
    Api.createProgram channel
        { CreateProgramParams.Default with
            RootFiles = ValueSome [| DocumentIdentifier.FileName "./my-project/index.d.ts" |] }

let snapshot, project = program.Snapshot, program.Project.Value.Id
```

Opens are ref-counted and persist across snapshots. `Api.release channel { Snapshot = id }` drops
one when you are done with it; `updateSnapshot` with `CloseProjects`/`CloseFiles` unwinds the
opens themselves.

Either way you end up holding the pair. A [session](#a-session-binds-the-snapshot-and-the-project)
binds it once so no later call has to repeat it.

## Three ways to call the same method

All 142 methods exist in each form. Pick one per codebase and stay with it.

```fsharp
// 1. Module function, explicit record. Most greppable; what the docs use.
Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file }

// 2. Extension member, record. No channel threading.
channel.getSourceFile { Snapshot = snapshot.Snapshot; Project = project; File = file }

// 3. Extension member, fields spread as named arguments. Optional fields are optional arguments.
channel.getSourceFile(snapshot = snapshot.Snapshot, project = project, file = file)
```

`AsyncApi.*` mirrors `Api.*` over `TscMailbox`, same names and types wrapped in `Async`, with the
same three forms. The one exception is `batchRequests`, which has no async counterpart — the
mailbox *is* the batcher.

There is a fourth form, the session, which drops the two arguments that never vary; it gets its
own section below.

### Parameter records

Every field the schema marks optional is `voption` and every all-optional record carries a
`Default`. Copy-update it rather than writing the fields out:

```fsharp
{ GetDiagnosticsParams.Default with Files = ValueSome [| file "main.ts" |] }
```

`DocumentIdentifier` is `FileName of string` or `Uri of string`; paths may be relative to the
channel's cwd.

### Result contract

- `'T` — the schema declares a result, and a missing one raises.
- `'T voption` — the schema permits null. **`ValueNone` is an answer, not a failure**: no symbol
  at that position, no such file in this project.
- Server-side failures raise `TsGoError(method, message)`.
- `channel.Diagnostics` returns everything the process has written to stderr — panics and log
  noise. Include it when reporting a failure; it is usually the only explanation.

## A session binds the snapshot and the project

126 of the 142 methods lead with the same two arguments — the snapshot and the project — because
that pair is what the compiler resolves everything else against. `Session<'T>` holds the pair and
re-exposes those methods with the two arguments removed, in the named-argument form:

```fsharp
let session = channel.Session program    // the createProgram response already carries the pair

let symbol = session.getSymbolAtPosition(file "main.ts", 42)
let diagnostics = session.getSemanticDiagnostics(files = [| file "main.ts" |])
```

`channel.Session(...)` and `mailbox.Session(...)` each accept the `createProgram` response, an
`updateSnapshot` response — naming the project when the snapshot holds more than one — or a raw
snapshot id and project id. `Session<TscChannel>` answers synchronously and `Session<TscMailbox>`
in `Async`, under the same member names, so a call site changes transport by changing how the
session was built.

The pair is data, not identity. `WithSnapshot` and `WithProject` rebind one half, and
`ForSymbol symbol` retargets to the project a symbol was first observed in:

```fsharp
let updated = session.Sessionless.updateSnapshot(openFiles = [| file "main.ts" |])
let session = session.WithSnapshot updated.Snapshot
```

The 16 methods that take neither argument — `initialize`, `updateSnapshot`, `createProgram`, the
`transpile*` and config-parsing family — precede any snapshot, so they hang off
`session.Sessionless` rather than being absent. Handles are valid for exactly the pair a session
holds; [`wire-navigation.md`](wire-navigation.md#a-session-is-the-snapshot-and-the-project-bound-once)
covers that scope and where the layer is generated from.

## Common patterns

### Diagnostics for a file

```fsharp
match Api.getSemanticDiagnostics channel
          { Snapshot = snapshot.Snapshot; Project = project; Files = ValueSome [| file "main.ts" |] } with
| ValueNone -> []
| ValueSome diagnostics -> [ for d in diagnostics -> d.Code, d.Text ]
```

Assert on `Code` (`2322`), not on `Text` — the prose changes upstream, the code does not.
`getSyntacticDiagnostics` takes the same record.

### Read a file's AST

```fsharp
match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" } with
| ValueNone -> failwith "not in this project"
| ValueSome ast ->
    printfn $"%d{ast.NodeCount} nodes in %s{Ast.fileName ast}"
    Node.root ast   // Node<SourceFile> - continue in wire-navigation.md
```

One request brings the whole file: every node is readable from the blob with no further round
trips. `Api.getSourceFileNames` lists what the project contains.

### Ask the checker about a node

Checker requests take a `Location` — a **node handle**, the string `index.kind.path`:

```fsharp
let handle = $"{Node.index node}.{uint32 node.Kind}.{Ast.path (Node.file node)}"
let ty = Api.getTypeAtLocation channel { Snapshot = snapshot.Snapshot; Project = project; Location = handle }
```

Handles are valid only within the snapshot and project that produced them. Going the other way —
a handle from `symbol.Declarations` back to a node — is in
[`wire-navigation.md`](wire-navigation.md#example-a-symbols-declarations).

### Enumerate a module's exports

```fsharp
match Api.getSymbolOfSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "index.d.ts" } with
| ValueNone -> [||]
| ValueSome moduleSymbol ->
    Api.getExportsOfModule channel { Snapshot = snapshot.Snapshot; Project = project; Symbol = moduleSymbol.Id }
    |> ValueOption.defaultValue [||]
```

`SymbolResponse.Flags` and `.CheckFlags` are typed enums — read them by name
(`symbol.Flags.HasFlag SymbolFlags.Property`), never by bit.

### Concurrent calls, batched

```fsharp
use mailbox = new TscMailbox(exe, "./my-project")
Async.RunSynchronously(AsyncApi.initialize mailbox) |> ignore

let! results =
    files
    |> Array.map (fun f -> mailbox.getSourceFile(snapshot = snapshot, project = project, file = f))
    |> Async.Parallel
```

Whatever queues while a batch is in flight goes out in the next `batchRequests` round trip. A
lone caller never batches and pays nothing for the mailbox.

### Sources that exist only in memory

```fsharp
let fs =
    { VirtualFileSystem.Default with
        ReadFile =
            ValueSome(fun path -> if path = "/virtual.ts" then Content "export const a = 1" else FallBack)
        FileExists = ValueSome(fun path -> if path = "/virtual.ts" then ValueSome true else ValueNone) }

use channel = new TscChannel(exe, cwd, VirtualFileSystem.callbacks fs)
```

Only the members you set are registered, and only those are ever asked about. The three-way
`FileRead` matters:

| Reply | Meaning |
|---|---|
| `Content text` | this is the file, `Content ""` included |
| `Missing` | the file does not exist; **resolution stops here** |
| `FallBack` (or `ValueNone`) | not answered; the server reads the real filesystem |

`Missing` where you meant `FallBack` silently changes module resolution.

For a one-off edit to a file that does exist, `Api.updateTemporarySnapshot` is cheaper than a
virtual filesystem:

```fsharp
let edited =
    Api.updateTemporarySnapshot channel
        { Snapshot = snapshot.Snapshot; File = file "main.ts"; NewText = "export const a = 1" }
```

### Transpile without a project

```fsharp
let output =
    Api.transpileModule channel
        { Input = "const a: number = 1"
          Options = { TranspileOptions.Default with ReportDiagnostics = ValueSome true } }
output.OutputText
```

`transpileModule`/`transpileDeclaration` need no snapshot or project — only `initialize`.

## Gotchas

- **Positions are UTF-16 code units.** They index straight into `Ast.sourceText ast`; no
  conversion, and no byte offsets.
- **Node text is cooked, not the source spelling.** `0x2a` reads back `"42"`. For the spelling,
  slice `Ast.sourceText` between `node.Pos` and `node.End`.
- **Absent and empty are indistinguishable** in the AST blob: an empty collection is written the
  same as a missing one.
- **Never write a kind or flag ordinal.** They are positional upstream and move.
  `SyntaxKind.StringLiteral`, not `11u`.
- **A `TscChannel` serialises requests.** Sharing one across threads corrupts the stream; use a
  `TscMailbox`, or a channel per thread.
- **`initialize` first**, before any other method, on both clients.

## Where to go next

- [Navigating the AST](wire-navigation.md) — sessions in depth, `Node<'Tag>`, views, accessors,
  node handles.
- [The hand-written register](wire-hand-written.md) — the facts transcribed from upstream rather
  than derived, and how to update them.
- [The wire protocol](plans/tsgo-protocol.md) — framing, errors, the binary AST format.
- [Remaining work](plans/wire-remaining-work.md) — what is still outstanding.
