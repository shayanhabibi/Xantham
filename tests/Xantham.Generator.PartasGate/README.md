# Generator customization acceptance gate

Run from the repository root:

```powershell
rtk node tests/Xantham.Generator.PartasGate/verify.mjs
```

The same gate runs under `build.fsx -- test --run-gate`. It generates companions through the
public extension API, compiles a property-free component with the real Partas Fable plugin,
compares its JSX with the accepted fixture, renders it with Solid's browser runtime, and checks
direct property access from generated source and an excluded binding DLL. Missing JSX fails.
The ordinary Expecto customization suite separately proves readonly assignments fail.

`partas-source.zip` is an MIT-licensed source snapshot of Partas.Solid 3.0.0 from the local
working tree based on commit `0d43cb5e3d9de95c74e92c89547a1b4439a32979`. That checkout had
uncommitted work; the base commit alone does not reproduce the tested source. `partas-pin.json`
authenticates the archive and every extracted source. The gate needs no sibling checkout.
The snapshot's project files target the installed .NET 10 SDK for the library and .NET 6 for
the plugin, pin Fable.Core 5.2.0/Fable.AST 5.0.0, and reference the current Xantham support project
through an MSBuild environment property. Framework source code is preserved in the archive.
To update the pin, replace the archive from reviewed source and update all checksums together.

The gate restores its exact npm lockfile, including Solid 2.0.0-rc.9 and the real JSX compiler.
Generated projects and output live in `tests/.scratch/partas-gate/`. Each invocation recreates
that directory. A network connection is needed on first restore or when the pins change.
