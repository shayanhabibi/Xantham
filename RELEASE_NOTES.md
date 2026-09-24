# Release Notes

## Unreleased

### Xantham.TypeScript.Wire

- A frame split across several pipe reads is read to its end rather than failing as a short read.
- Every transport failure raises an `IOException` carrying the server's exit code and stderr, and
  faults the channel: later requests fail with the same error.
- A batch the server refuses as a whole replays only its read-only members. Methods that change
  server state (`Batch.sideEffectingMethods`) fail with the batch error and are not sent twice.
- `TscMailbox.Dispose` fails every queued request with `ObjectDisposedException`.

### xantham

- `unionArmOverloads` in `xantham.json`: a module-level export whose sole parameter is an erased
  union gains one overload per arm. Off by default, because the overloads make existing
  `f(!^ x)` calls ambiguous. An arm that would make a declared overload's call ambiguous is
  declined.
- `dedupe-overloads` keeps overloads that differ after their third parameter; solid-js's
  `SetStoreFunction.Invoke` regains four.
- Declaration catalogs hash every member and type in full. Interfaces that differed only after
  their hundredth member used to hash alike. Existing catalogs rehash once.
- `RT002`, `TR003` and `TP001` no longer quote checker type ids, so the manifest is the same from
  run to run.
- Exported type names containing `$` (zod v4's `$ZodType`) declare under their identifier shape.
- A refused `xantham.json` exits with the configuration code, and an unknown option is reported
  instead of being read as the package directory.
- `tsc version --json` writes valid JSON for Windows paths and fails when no compiler is cached;
  a failed `tsc init` exits with npm's message.
- The package ships a README.

### Xantham.Fable.Core

- The package ships its F# sources under `fable/`, so Fable can compile the `inline`
  `KeyOf`/`TypeKeyOf` helpers from a NuGet reference.

### Xantham.Fable.Core.TS

- `TSExtensions.Utils.toJSFunc` is an `[<Emit>]` member. As an `inline` member it failed when
  Fable consumed the package as a DLL.

### Xantham.Fable.Node

- `README.md` and `xantham.json` are no longer delivered to consumers as content files; the
  README is the package readme.

## 0.1.0

First release of `Xantham.TypeScript.Wire`.

- The compiler's API surface, generated from the schema shipped in `typescript@7.1.0-dev`: 142
  synchronous calls, the same set as `Async`, and typed records for every parameter and response.
- The binary AST, read in place: `SyntaxKind`, child slots, node and file accessors, and a typed
  layer of tags and views generated from `ast.json`.
- `TscChannel` for serial use and `TscMailbox`, which batches whatever calls overlap.
- `VirtualFileSystem`, for compiling sources that exist only in memory.
- Records whose fields are all optional carry a `Default` to copy-update from.
- Targets `net10.0`, `net8.0` and `netstandard2.1`.
