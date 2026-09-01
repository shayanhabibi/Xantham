# Xantham.TypeScript.Wire

An F# client for the TypeScript 7 compiler's API server — the Go `tsc` binary run as
`tsc --api`. It speaks the compiler's own msgpack protocol over stdio, and reads the binary AST
it returns without going through JSON.

- **The full API surface**, generated from the compiler's shipped schema: 142 synchronous calls,
  the same set again as `Async`, and typed records for every parameter and response.
- **The binary AST**, read in place. A node is a struct over the blob and an index, not an object
  graph, and the typed layer gives each one a tag — `Node<FunctionDeclaration>` — so narrowing is
  a compile-time question. Generated from `ast.json`, so the 351 kinds and their child slots come
  from the compiler rather than from hand-written constants.
- **A batching mailbox** that collects overlapping calls into one `batchRequests` round trip.
  Under load it runs 2.1–2.3× the serial path; a lone caller pays nothing for it.
- **A virtual filesystem**, so the compiler can be pointed at sources that exist only in memory.

Targets `net10.0`, `net8.0` and `netstandard2.1`.

## Getting started

The compiler is not bundled — install the `typescript` npm package whose version matches this
one, since the protocol is unversioned and both sides must come from the same tree.

```fsharp
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

// Finds the platform's tsc executable under a directory containing node_modules.
let exe = (Tsc.locate "./my-project").Value

use channel = new TscChannel(exe, "./my-project")
Api.initialize channel |> ignore

// Records whose fields are all optional carry a `Default` - copy-update the ones you mean.
let program =
    Api.createProgram channel
        { CreateProgramParams.Default with
            RootFiles = ValueSome [| DocumentIdentifier.FileName "./my-project/index.d.ts" |] }

let project = program.Project.Value.Id

match Api.getSourceFile channel { Snapshot = program.Snapshot; Project = project; File = DocumentIdentifier.FileName "./my-project/index.d.ts" } with
| ValueSome ast -> printfn $"%d{ast.NodeCount} nodes"
| ValueNone -> ()
```

Every method is also an extension member on the channel, with a second overload taking the
parameter record's fields directly, so the channel need not be threaded through by hand:

```fsharp
let ast =
    channel.getSourceFile(
        snapshot = program.Snapshot,
        project = project,
        file = DocumentIdentifier.FileName "./my-project/index.d.ts")
```

`TscMailbox` is the same surface asynchronously, and batches whatever overlaps:

```fsharp
use mailbox = new TscMailbox(exe, "./my-project")
Async.RunSynchronously(AsyncApi.initialize mailbox) |> ignore

let! names = mailbox.getSourceFileNames(snapshot, project)
```

## Reading the AST

Navigate through the typed layer rather than raw indexes — see `docs/wire-navigation.md`:

```fsharp
open Xantham.TypeScript.Wire.Patterns

for statement in SourceFile.statements (Node.root ast) do
    match statement with
    | FunctionDeclaration declaration ->
        // `name` is a Node<Identifier> voption - a function declaration need not have one.
        match FunctionDeclaration.name declaration with
        | ValueSome name -> printfn $"function %A{Identifier.text name}"
        | ValueNone -> ()
    | _ -> ()
```

A `Node<'Tag>` is a struct over the blob and an index. The tags inherit each other exactly when
one's kinds are a subset of the other's, so `'Tag :> Expression` is the compile-time form of
`isExpression`; narrowing is a `[<return: Struct>]` active pattern that allocates nothing.

## Serving files from memory

```fsharp
let fs =
    { VirtualFileSystem.Default with
        ReadFile = ValueSome(fun path -> if path = "/virtual.ts" then Content "export const a = 1" else FallBack)
        FileExists = ValueSome(fun path -> if path = "/virtual.ts" then ValueSome true else ValueNone) }

use channel = new TscChannel(exe, cwd, VirtualFileSystem.callbacks fs)
```

`FallBack` and `ValueNone` mean "not answered, read the real filesystem"; `Missing` means the file
does not exist and resolution stops there. The distinction changes module resolution, so it is in
the types rather than in a comment.

## Documentation

- `docs/wire-navigation.md` — the typed layer, tags, views and the escape hatches.
- `docs/wire-hand-written.md` — every fact in the pipeline transcribed from upstream rather than
  derived from its schema, with how to update each one.
- `docs/plans/tsgo-protocol.md` — the wire protocol itself, verified against live byte traces.

Licensed under Apache-2.0.
