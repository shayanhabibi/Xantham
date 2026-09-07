---
title: Getting Started
order: 0
---

:::warning
`xantham` is in early development.
:::

## CLI

Download `xantham` via dotnet as a global tool.

```bash title="xantham" frame=terminal
dotnet tool install -g xantham 

# xantham may still be in prerelease:
dotnet tool install -g xantham --prerelease
```

Point it at a package, and generate!

```bash frame=terminal
xantham generate ./fixtures/solid-js/node_modules/solid-js
```

By default, `xantham` generates to `./xantham-out`. You can change this by passing the `-o <dir>` flag.

:::filetree
- xantham-out
  - **SolidJs.fs**
  - manifest.json
  - symbols.jsonl
:::

For further usage and configuration, [see here](configuration.md).

## TSC

`xantham` is built and pinned against a specific version of the `typescript` compiler. Due to the current
api for the compiler being in prerelease, any change in the format of the api bytecode responses will invalidate
our usage.

For this reason, you are intended to use the `typescript` compiler that `xantham` ships with. You can install
this with `xantham tsc init`{bash}.

#### Install TSC for xantham

```bash frame=terminal
xantham tsc init
```

## Xantham.Fable.Core

`xantham` tries to cover as much of the `typescript` surface as possible by default. This is not possible without
the utility types that we provide through `Xantham.Fable.Core`{fsharp}.

Once you install the package, it will shadow the `Fable.Core.JS` and inject some utility helpers into the global namespace.


#### Reference Xantham.Fable.Core in your bindings 

```bash frame=terminal
dotnet add package Xantham.Fable.Core

# While xantham is in prerelease, so is its utility pkg
dotnet add package Xantham.Fable.Core --prerelease
```

## Missing Lib types/dependencies

If you notice the generated content is missing `lib` types/functions or other, then this is due to the narrow api
that the `Fable.Core.JS` and `Fable.Browser` packages can meaningfully cover.

You are able to generate the full `lib` packages with `xantham`.

:::info
The lib bindings will likely be published alongside `Xantham.Fable.Core`{fsharp}.
:::