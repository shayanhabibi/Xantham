---
title: Using bindings
description: Add generated bindings to a Fable project and call the package.
order: 3
---

<p class="xantham-lead">Generated bindings provide the F# surface. Your app still uses the original JavaScript package at runtime.</p>

## Add the support packages

In your Fable project directory:

```bash frame=terminal
dotnet add package Fable.Core --version 5.2.0
dotnet add package Xantham.Fable.Core
dotnet add package Xantham.Fable.Core.TS
```

Use Xantham support packages matching your generator release. For prerelease
packages, add `--prerelease` to the corresponding install commands.

Bindings target **Fable 5.x**. Use **net8.0 or later** for the F# project:
generated object builders can use static interface members with bodies.

## Include files in order

Add the output to your `.fsproj` before the application files that use it.
For the [quick start](../index.md):

```xml title="Project file"
<ItemGroup>
  <Compile Include="out/AnsiRegex.fs" />
  <Compile Include="Program.fs" />
</ItemGroup>
```

When generation emits `groups/`, include those dependencies before the package
binding. For bindings composed from several runs, compile producer bindings
before their consumers.

## Call an export

The quick-start binding exposes the default import through `Exports`:

```fsharp title="Program.fs"
let pattern = AnsiRegex.Exports.ansiRegex()
```

The generated signature is the source of truth for argument and return types.
In this example the default mapping widens the returned regular expression
to `obj`; review the finding before using members on the result.

Generated option objects provide a `Create` member where supported:

```fsharp
let options = AnsiRegex.Options.Create(onlyFirst = true)
let firstPattern = AnsiRegex.Exports.ansiRegex(options = options)
```

Keep `ansi-regex` installed in your application's npm project. Fable emits an
import from that package; the binding does not include its JavaScript implementation.

## Find types and values

A binding is rooted in a generated F# module.

- **Value exports** are members of `Exports`, such as `MyPackage.Exports.connect`.
- **Types** appear under their owning module.
- **Subpaths** become nested modules: `./client/deep` becomes `Client.Deep`.
- **Ambient modules** follow their import paths: `inspector/promises` becomes
  `Inspector.Promises`.
- **Anonymous option objects and callbacks** are named under the member that uses them.

The same type exported through several paths has a shared canonical location.
Use the emitted signature to find it.

## Build and run

Run your application's normal .NET build and Fable build. A successful .NET build
checks the F# types; running the Fable app checks the JavaScript imports and behavior.

If a generated JavaScript property needs a backticked F# name, you may need to
suppress `FS1104` in the consumer project:

```xml title="Project file"
<PropertyGroup>
  <NoWarn>$(NoWarn);FS1104</NoWarn>
</PropertyGroup>
```

## Keep your custom code separate

Place convenience wrappers and application-specific conversions in their own files.
Regenerate the binding when the npm package changes, then review the diff and
the APIs your app calls.

<div class="xantham-grid xantham-next">
<a class="card xantham-card" href="../packages/"><h3>Understand the support packages</h3><p>Utility types, standard libraries, and Node bindings.</p><span class="xantham-card__link">Support packages →</span></a>
<a class="card xantham-card" href="../troubleshooting/"><h3>Review mapping quality</h3><p>Find the symbols that widen or need manual handling.</p><span class="xantham-card__link">Read the report →</span></a>
</div>

