# Support helper globals runtime repro

This opt-in repro references `src/Xantham.Fable.Core` directly. It needs the
repository's .NET SDK and Node.js, with no CloudEdge checkout or npm dependencies.
Its local tool manifest pins Fable 5.13.0; Fable.Core is pinned to 5.2.0.
It is separate from the default test gate.

From the Xantham repository root:

```sh
cd tests/repros/support-helper-globals
dotnet tool restore
dotnet build SupportHelperGlobals.fsproj --nologo
dotnet tool run fable -- SupportHelperGlobals.fsproj --outDir fable-out --noCache
node --test check.mjs
```

The .NET and Fable compilations succeed. The Node tests currently exit with code
1: the erased key's `Value` member passes, while the four helper cases fail with
undefined `TypeKeyOf`, `KeyOf`, or `Brand` JavaScript globals. Each assertion runs
independently, so the first failure cannot hide the other helpers. The item
tests use literal erased keys to avoid depending on `TypeKeyOf.create`.
These assertions check the intended results and should pass after a fix.

The support source declares `[<AutoOpen>] module Fable.Core.JS.JS`. Fable's
`tryGlobalOrImportedAttributes` in `src/Fable.Transforms/FSharp2Fable.Util.fs`
treats entity names starting with `Fable.Core.JS.` as JavaScript globals.
Inspect `fable-out/Helpers.js` for the resulting bare helper calls; successful
F# type checking alone does not exercise them at runtime.
