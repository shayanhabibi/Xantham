# ConstrainFunction feasibility — 2026-09-10

The current helper cannot replace every emitted `:> JS.Function` constraint.
These probes use the actual `ConstrainFunction` in `Fable.Core.TSExtensions`,
with .NET SDK 10.0.401 and F# preview syntax enabled.

Build the supporting assembly, then select a case:

```powershell
rtk dotnet build src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj -c Debug
rtk dotnet fsi --langversion:preview --define:INTERFACE docs/.ai/probes/constrain-function/Probe.fsx
```

- `INTERFACE` and `METHOD`: fail FS0670. The SRTP constraint cannot be generalized
  on a generic interface or abstract method.
- `STATIC_INLINE`: fails FS0043, with FS0064 specialization to an F# function.
- `ANNOTATION`: passes for an F# lambda and reports FS0064. Its parameter specializes
  to `'a -> 'b`; it is not an arbitrary callable type parameter.
- `JS_FUNCTION`, `TS_FUNCTION`, and `DELEGATE`: fail FS0001. The annotation does not
  admit `JS.Function`, Core.TS's `Function`, or `System.Func<int,int>`.
- `NONFUNCTION`: fails FS0001, as expected for a non-callable value.

The helper's single `Utils.toJSFunc` overload accepts `FSharpFunc<_,_>`.
This is useful as a function-shaped parameter annotation, but neither that overload
nor adding `inline` provides a universal replacement for nominal generic constraints.
Generated function constraints remain unchanged. Supporting more callable representations
would need a separate API design and Fable runtime tests.

The extension module was moved from `Fable.Core.TS.Extensions` to
`Fable.Core.TSExtensions`: the former makes `Fable.Core.TS` both a namespace and
a module in one assembly (FS0247). Its `[<AutoOpen>]` keeps the helper available
after `open Fable.Core`.
