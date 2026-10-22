---
title: Using bindings
description: Add generated bindings to a Fable project and call the package.
order: 3
---

<p class="xantham-lead">Generated bindings provide the F# surface. Your app still uses the original JavaScript package at runtime.</p>

## Add the support packages

In your Fable project directory:

```bash frame=terminal
dotnet add package Fable.Core --version 5.5.0
dotnet add package Xantham.Fable.Core
dotnet add package Xantham.Fable.Core.TS
```

Use Xantham support packages matching your generator release. For prerelease
packages, add `--prerelease` to the corresponding install commands.

Bindings target **Fable 5.x**. Use **net8.0 or later** for the F# project:
generated object builders can use static interface members with bodies.

Compile bindings that carry `[<Fickl.Core.InlineInterface>]` with **Fickl 0.5.1**.
Xantham's support package supplies that attribute temporarily until Fickl.Core is published.
`CaseTest` comes from Fable.Core 5.5.0.

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
- **Constants**: a required member typed by one string or numeric literal, readonly or writable
  (`readonly AT_TARGET: 2`) is also a `[<Literal>]` in a module of the type's name, so the
  pattern in
  `match e.eventPhase with | Event.AT_TARGET -> ...` compiles to `e.eventPhase === 2`.
  `Create` sets these members itself and takes no argument for them. After you open the
  binding module, `Event.AT_TARGET` reads the value in an expression too. Fully qualified
  (`MyBinding.Event.AT_TARGET`), an expression names the interface's instance property
  instead; read the value there through a module abbreviation
  (`module EventConstants = MyBinding.Event`, then `EventConstants.AT_TARGET`).
- **Literal overloads**: an overload selected by a literal argument is its own method, named
  after the literal, with the literal written in: `store.read(key, "text")` binds as
  `store.readText key` and still calls `read(key, 'text')` in JavaScript.
- **Symbol members**: an iterable (`[Symbol.iterator]`) inherits `seq<'T>`, so
  `for item in items do` needs no cast. Other well-known symbols are named members, such as
  `resource.SymbolDispose()` and `value.SymbolToStringTag`.
- **Tag maps**: a parameter typed by a key of a declared map takes a `typekeyof` key, and the
  key fixes the result type: `document.createElement (typekeyof<HTMLElementTagNameMap, _> _.div)`
  returns `HTMLDivElement`. The key reaches JavaScript as the bare string `"div"`.
- **Functions of a namespace object** (`export * as utils`) and global functions are
  methods: `MyPackage.Exports.utils.clamp(5., 0., 1.)`. TypeScript does not allow
  assigning to them, so the binding has no setter; reassigning one needs dynamic access
  (`?` or `!!`).
- **A type parameter used only in a return**, such as `querySelectorAll<'E>`, is inferred
  from how the result is used inside a function. A module-level binding states it:
  `let items: NodeListOf<HTMLElement> = document.querySelectorAll "li"`.

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

