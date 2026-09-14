---
title: Generated sources
description: Refresh the compiler client, standard-library bindings, and schema.
order: 3
---

<p class="xantham-lead">Change the generator or its inputs, then regenerate the artifact you need.</p>

## Compiler client layers

```bash frame=terminal
dotnet fsi build.fsx -- generate
```

Select a layer for a focused refresh:

```bash frame=terminal
dotnet fsi build.fsx -- generate --only ast
dotnet fsi build.fsx -- generate --only proto
dotnet fsi build.fsx -- generate --only session
```

The tools under `tools/tsc-ast`, `tools/proto-gen`, and `tools/session-gen`
generate the typed AST, protocol records, and session API.

## Update vendored compiler inputs

```bash frame=terminal
dotnet fsi build.fsx -- generate --sync
```

`--sync` refreshes vendored upstream sources and requires network access.
Review the pin, checksum, and generated diffs together.

Use the installed `node_modules/typescript` package as the first source of truth,
then `tools/tsc-ast/upstream/`. The upstream compiler repository is
`microsoft/TypeScript`, with compiler sources under `tsc/`.

Read `.claude/rules/upstream.md` before changing the vendored inputs.

## Standard-library binding

```bash frame=terminal
dotnet fsi build.fsx -- generate --only compiler-lib
```

This explicitly refreshes `src/Xantham.Fable.Core.TS/Fable.Core.TS.fs` and its report.
The large consumer artifact is excluded from the default generation command.

Compile the consumer gates against the refreshed binding.

## Configuration schema

```bash frame=terminal
dotnet fsi build.fsx -- generate --only schema
```

Refresh the schema when generator configuration changes and check the documented
examples against it.

## Facts maintained by hand

Some protocol details come from upstream prose or hand-written declarations.
Their sources and update procedures are recorded in the hand-written register.

<a class="card xantham-card xantham-next" href="../hand-written/">
    <h3>The hand-written register</h3>
    <p>Know which facts need manual review during a compiler update.</p>
    <span class="xantham-card__link">Read the register →</span>
</a>
