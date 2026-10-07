# Xantham.Fable.Node

Generated Fable 5 bindings for `@types/node` 22.20.2, the version pinned in the repository's
`package.json`.

```text
dotnet add package Xantham.Fable.Node
```

The bindings reference `Xantham.Fable.Core.TS` for the ECMAScript and DOM types that Node's
declarations use, and `Xantham.Fable.Core` for the support types. Both arrive as package
dependencies. Every declaration sits under the top-level `Node` module:

```fsharp
open Fable.Core.TS
open Node
```

The source file is generated from `node_modules/@types/node` with the settings in
`xantham.json`; regenerate it with:

```text
dotnet fsi build.fsx -- generate --only node-lib
```
