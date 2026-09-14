---
title: Reports and troubleshooting
description: Read generation findings and resolve common setup and binding problems.
order: 7
---

<p class="xantham-lead">Start with the diagnostic or symbol you need. The output report tells you where a TypeScript API changes shape.</p>

## Read the generation report

`manifest.json` contains aggregate counts. `symbols.jsonl` contains one JSON
record per symbol, including its findings.

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">Exact</span><h3>Direct representation</h3><p>The F# binding preserves the TypeScript construct directly.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Ergonomic</span><h3>Adapted for F#</h3><p>Meaning is preserved with a more idiomatic representation, such as an option.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Widened</span><h3>Less type information</h3><p>The F# type loses precision, for example by becoming <code>obj</code>.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Escape</span><h3>Manual handling needed</h3><p>The construct is not represented by the generated binding.</p></div>
</div>

Search `symbols.jsonl` for the TypeScript name or finding code.
Review the APIs your app calls, then compile and run a small example.
Counts describe mapping quality; they do not establish runtime correctness.

## The tool or compiler is missing

If the shell cannot find `xantham`, check the .NET tools PATH in
[Installation](installation.md#check-the-setup).

If Xantham cannot find its matching compiler:

```bash frame=terminal
xantham tsc init
xantham tsc version
```

Check that Node.js and npm are on PATH and that npm installation completed.

## TypeScript cannot resolve a module or provider

Install the missing dependency where the input package can resolve it.
For `TS2688`, check the configured `types` list and install the named provider.

Use `"types": []` when automatic ambient providers are unwanted.
Explicit imports still need their dependencies.

## No declaration entry is selected

Check the package's `types`, `typings`, and `exports` declarations.
A blocked or absent public root may require an explicit `entry` or `publicInputs`.

For a subpath, set the matching JavaScript `runtime` import.
See [selecting declarations](usage.md#choose-declarations).

## A type became obj

Find the symbol's finding code. Common causes include an unlisted dependency group,
a construct with no precise F# equivalent, or the resolution depth limit (`RT001`).

Configure a [group disposition](dependencies.md#set-group-dispositions) if a dependency
has a usable binding. For a remaining widened API, keep conversions in a small
application wrapper and verify its runtime behavior.

## The F# output does not compile

Check these first:

- Use Fable 5.x and `Fable.Core` 5.2.0.
- Reference `Xantham.Fable.Core` and `Xantham.Fable.Core.TS`.
- Compile shipped groups and catalog producers before their consumers.
- Use a consumer target framework of net8.0 or later.
- Check custom mappings for the correct F# name and generic arity.

If it still fails, report the first distinct compiler error and the smallest
declaration that reproduces it.

## The JavaScript import fails

The runtime package must be installed in the app's npm project.
Check that `runtime` names an import the package actually exports and that the
selected declarations match your browser, Node, or worker environment.

## Release expectations

Xantham 0.1.x is an early release. Generated names and shapes may change before 1.0.
Some TypeScript constructs widen or escape, and successful generation alone does
not guarantee that every API compiles or behaves correctly.

Pin versions, keep the reports, and validate the APIs your application uses.

<a class="card xantham-card xantham-next" href="/Xantham/dev/#report-a-problem">
    <h3>Report a reproducible problem</h3>
    <p>Include the input declaration, configuration, versions, and diagnostic.</p>
    <span class="xantham-card__link">What to include →</span>
</a>
