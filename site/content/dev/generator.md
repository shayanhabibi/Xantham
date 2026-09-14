---
title: Generator workflow
description: Understand the generator pipeline and validate a mapping change.
order: 2
---

<p class="xantham-lead">Prove a mapping on a small declaration first, then run the full fixture corpus.</p>

## Follow the pipeline

`src/Xantham.Generator` runs a sequence of passes through `Pipeline.fs`:

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">01 · Harvest</span><h3>Collect declarations</h3><p>Identify the package’s public TypeScript surface.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">02 · Resolve</span><h3>Resolve types</h3><p>Follow type information and package boundaries through the compiler.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">03 · Shape</span><h3>Form the F# API</h3><p>Apply the passes that choose F# declarations and member shapes.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">04 · Render</span><h3>Write the binding</h3><p>Emit F# files and the reports of per-symbol findings.</p></div>
</div>

Source order is pass order. Read the relevant phase decisions in
`docs/.ai/plans/generator-architecture.md` and `generator-type-mapping.md`.
Update the affected phase record with a behavior change.

## Add a minimal fixture

Create `tests/fixtures/<feature>-lab/` with a `package.json` and the few declarations
needed to demonstrate the behavior. The `-lab` suffix keeps the fixture tracked.

Register it with a `fixtureTests` block in
`tests/Xantham.Generator.Tests/Pipeline.test.fs`.
Add focused per-pass tests where they explain the mapping.

The compile gate picks up generated goldens. Add a run-gate check when the feature
has JavaScript behavior to verify.

## Iterate on the suite

```bash frame=terminal
dotnet fsi build.fsx -- test --quick --update --filter "<suite>"
```

`--quick` skips setup. `--update` rewrites goldens, then runs the suite again to check
them. Replace `<suite>` with the affected suite name.

Review the golden diff before committing. Fix the generator and regenerate;
generated bindings and manifests are derived artifacts.

## Measure the findings

```bash frame=terminal
dotnet fsi build.fsx -- findings
dotnet fsi build.fsx -- findings --fixture animejs
dotnet fsi build.fsx -- findings --key TR014
```

Compare the finding counts before and after the change.
For large fixtures, inspect aggregate reports and targeted symbols rather than
reading the whole generated binding.

Finding codes are a published contract. Preserve existing codes and update the
code table and its snapshot tests when adding a finding.

## Run the complete checks

```bash frame=terminal
dotnet fsi build.fsx -- test
```

Include `--run-gate` when checking runtime behavior.
Report whether the output compiles, whether runtime checks pass, and which
finding counts changed.

## Consumer compatibility

Compile against `Fable.Core` **5.2.0**, `Xantham.Fable.Core`, and
`Xantham.Fable.Core.TS` generated from the root TypeScript pin.
Bindings target Fable 5.x.

A compile gate is useful only when it uses the dependencies a consumer will use.

