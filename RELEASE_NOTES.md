# Release Notes

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
