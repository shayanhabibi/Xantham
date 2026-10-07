# Xantham.Generator

The TypeScript-to-F# Fable bindings generator as a .NET library. Use it to embed the
generation pipeline in your own applications and customisations. Targets .NET 10;
generated bindings target Fable 5.x. Depends on `Xantham.TypeScript.Wire` and requires
the TypeScript 7 compiler used by Xantham.

F# consumers supply `FSharp.Core` 9.0.100 or newer. The package keeps its build reference
private rather than imposing the build SDK's FSharp.Core version on your project.

> [!WARNING]
> New releases are temporarily hosted on [Cloudsmith](https://app.cloudsmith.com/shayanhabibi/r/xantham)
> while NuGet package ownership is transferred back to the author. This warning will
> be removed when the transfer is complete.

```bash
dotnet nuget add source https://nuget.cloudsmith.io/shayanhabibi/xantham/v3/index.json --name xantham-temporary
dotnet add package Xantham.Generator
```

See the [Xantham documentation](https://shayanhabibi.github.io/Xantham) and
[source](https://github.com/shayanhabibi/Xantham) for the pipeline and configuration.
