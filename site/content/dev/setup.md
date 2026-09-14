---
title: Local setup
description: Build Xantham, run the tests, and preview the documentation.
order: 1
---

<p class="xantham-lead">Run commands from the repository root. The build pipeline installs the pinned compiler and fixture dependencies.</p>

## Prerequisites

Install the .NET 10 SDK, Node.js, npm, and Git.

```bash frame=terminal
git clone https://github.com/shayanhabibi/Xantham.git
cd Xantham
npm ci
dotnet build Xantham.slnx
```

The root `package.json` pins the TypeScript compiler used by the generator and
live compiler tests.

## Run the test pipeline

```bash frame=terminal
dotnet fsi build.fsx -- test
```

For Fable runtime checks as well:

```bash frame=terminal
dotnet fsi build.fsx -- test --run-gate
```

<div class="xantham-grid">
<div class="card xantham-card"><h3>Expecto suites</h3><p>Check the compiler client, generator passes, and generated golden output.</p></div>
<div class="card xantham-card"><h3>Compile and run gates</h3><p>Compile generated F# against the consumer dependencies and exercise selected bindings through Fable.</p></div>
</div>

The compile-gate projects are ordinary projects: a solution build compiles their
goldens. The run gate validates selected JavaScript behavior.

## Run the local CLI

```bash frame=terminal
dotnet run --project src/Xantham.Cli -- generate ./node_modules/your-package -o ./scratch/out
```

Replace the input with an installed package directory containing `package.json`.

## Preview the website

```bash frame=terminal
dotnet run --project site/site.fsproj -- watch
```

Open `http://localhost:8080/Xantham/`.

For a static build:

```bash frame=terminal
dotnet run --project site/site.fsproj -- build
```

Pages are written to `output/`. API reference pages use available Release
assemblies; build those before checking reference changes.

## Working in a worktree

The build pipeline can borrow the main checkout's compiler installation.
Fixture dependencies are installed separately in the worktree:

```bash frame=terminal
dotnet fsi tools/xantham-fixtures.fsx -- init
```

Use the pipeline for tests so compiler discovery and fixture setup follow the
repository's conventions.

<a class="card xantham-card xantham-next" href="../generator/">
    <h3>Make a generator change</h3>
    <p>Start with a minimal fixture and a focused test run.</p>
    <span class="xantham-card__link">Generator workflow →</span>
</a>
