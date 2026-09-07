# Support helper globals runtime repro

This opt-in repro references `src/Xantham.Fable.Core` directly. It needs the
repository's .NET SDK and Node.js, with no CloudEdge checkout or npm dependencies.
Its local tool manifest pins Fable 5.13.0; Fable.Core is pinned to 5.2.0.
It is a small standalone companion to the support-helper checks in the default
runtime gate.

From the Xantham repository root:

```sh
cd tests/repros/support-helper-globals
dotnet tool restore
dotnet build SupportHelperGlobals.fsproj --nologo
dotnet tool run fable -- SupportHelperGlobals.fsproj --outDir fable-out --noCache
node --test check.mjs
```

The .NET and Fable compilations succeed, and all five Node tests now pass.
Before the helper namespace correction, Node exited with code 1: the erased
key's `Value` member passed, while the other four cases failed with undefined
`TypeKeyOf`, `KeyOf`, or `Brand` JavaScript globals. Each assertion runs
independently, so the first failure cannot hide the other helpers. The item
tests use literal erased keys to avoid depending on `TypeKeyOf.create`.

The erased support types remain in `Fable.Core.JS.JS`. Fable's
`tryGlobalOrImportedAttributes` in `src/Fable.Transforms/FSharp2Fable.Util.fs`
treats entity names starting with `Fable.Core.JS.` as JavaScript globals before
expanding inline calls. Executable helpers now belong to the global auto-open
module `XanthamFableCore`, allowing ordinary calls to inline. Inspect
`fable-out/Helpers.js` for the resulting property accesses and erased casts.
Successful F# type checking alone does not exercise them at runtime.
