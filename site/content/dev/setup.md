---
title: Local setup
description: Build Xantham, run the tests, and preview the documentation.
order: 1
---

<p class="xantham-lead">Run commands from the repository root. The build pipeline installs the pinned compiler and fixture dependencies.</p>

## Prerequisites

Install the .NET SDK selected by `global.json`, Node.js 24 (see `.node-version`), npm, and Git.

```bash frame=terminal
git clone https://github.com/shayanhabibi/Xantham.git
cd Xantham
npm ci
dotnet build Xantham.slnx
```

The root `package.json` pins the TypeScript compiler used by the generator and
live compiler tests. The pipeline installs the committed npm lockfile with `npm ci`.

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

`--quick` reuses existing dependencies and build setup. `--no-format` skips local
formatting while retaining setup; CI checks formatting. Use `--explain` to inspect
the resolved stages without running them.

## CI, packages and golden provenance

The Test workflow runs the complete suite and runtime gates. Each successful run
uploads the committed corpus as `golden/<fixture>/...` in an artifact alongside
`provenance.json`: the checked-out source commit and SHA-256 of every golden file.
Golden contents retain their exact bytes. The artifact contains the checked,
committed corpus; it does not regenerate or rewrite goldens. The known Linux
Solid golden differences remain reported by the suite.

After a successful master push, the Tag goldens workflow creates an annotated
`goldens/<full-commit-hash>` Git tag pointing to the tested source commit. Its
annotation records the run and artifact digest. Reruns preserve an existing tag.
Artifacts are retained for 90 days; tags remain in Git. Pull-request runs upload
provenance artifacts but do not create tags.

The Publish workflow runs `pack --ci --run-gate`, transfers the verified packages
to a separate job, then runs `publish artifacts --ci`. The NuGet key is exposed
only to that final upload step. Local `publish` still runs the complete pipeline.
The artifact command checks package filenames against selected project versions
and rejects missing or unexpected packages before uploading.

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
