---
title: Configuration
description: Configure modules, compiler libraries, and declaration inputs.
order: 5
---

<p class="xantham-lead">Start with the defaults. Add settings for the names, environment, and package boundaries your binding needs.</p>

## A minimal configuration

```json title="xantham.json"
{
  "module": "MyApp.Bindings",
  "lib": ["esnext", "dom"],
  "types": []
}
```

Pass the file with `--config ./xantham.json`. All properties are optional.
The [repository schema](https://raw.githubusercontent.com/shayanhabibi/Xantham/master/xantham.schema.json)
lists the available settings; use the schema from your Xantham version when pinning a build.

## Name the F# API

- **`module`** sets the generated F# module. The default is derived from the npm name:
  `@scope/pkg-name` becomes `Scope.PkgName`.
- **`namespace`** names a related package family. Use the same namespace when generating
  packages that reference each other.
- **`autoOpenExports`** marks generated `Exports` types as auto-open.
  It defaults to `false`; value exports then use explicit `Exports.member` access.
- **`runtime`** overrides the JavaScript import path. The default is the package name,
  with the `@types/` naming convention undone.

## Select the environment

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">Built-in declarations</span><h3>lib</h3><p>Select TypeScript libraries, such as <code>esnext</code> and <code>dom</code>. Omitting this uses the compiler default, which includes DOM.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Installed declarations</span><h3>types</h3><p>Select ambient providers. Omit for automatic discovery, use an empty array to disable it, or list installed providers such as <code>node</code>.</p></div>
</div>

For a worker environment that supplies its own globals, use the libraries its
type package recommends. Loading DOM alongside a provider that redeclares the
same globals can cause conflicts.

Explicit imports and reference directives still resolve their dependencies when
`types` is empty. A configured provider must be installed where the input package
can resolve it.

## Select public inputs

Use `subpaths` to select concrete keys from the package's `exports` map:

```json title="Select subpaths"
{
  "subpaths": ["./client", "./server"]
}
```

Use `publicInputs` to choose exact public keys and declaration files:

```json title="Select declarations explicitly"
{
  "publicInputs": {
    ".": "dist/index.d.ts",
    "./client": "dist/client.d.ts"
  }
}
```

Only the listed public inputs are generated. Include `"."` to select the root.
Keys must be exact public paths; expand wildcard exports into concrete keys.

`publicInputs` is mutually exclusive with `entry` and `subpaths`.
Each declaration path must name an existing TypeScript file inside the input package.

Use separate configurations for browser and worker variants with different globals.

## Configure package boundaries

`groups` decides whether a dependency is generated, referenced, mapped to an existing
F# type, or widened. See [Dependencies and shared types](dependencies.md) for examples.

## Resolve NoInfer

`resolveNoInfer: true` maps TypeScript's `NoInfer<T>` directly to `T`.
The default preserves `NoInfer<T>` through the support library.

## Generate your own standard library

`compilerLib` controls the layout when generating a combined TypeScript library binding:

```json title="Custom compiler-library layout"
{
  "compilerLib": {
    "module": "MyBindings.TypeScript",
    "esModule": "Es",
    "domModule": "Dom",
    "autoOpenEs": false,
    "autoOpenDom": false
  }
}
```

Defaults are `TypeScript.Lib`, `Es`, and `Dom`, with both auto-open flags off.
The child module names must be distinct F# identifiers. ES and DOM are emitted together
in one recursive module file.

Most consumers can use [Xantham.Fable.Core.TS](packages.md#standard-libraries).

<div class="xantham-grid xantham-next">
<a class="card xantham-card" href="../dependencies/"><h3>Compose package bindings</h3><p>Ship dependencies or reuse types from another generated binding.</p><span class="xantham-card__link">Dependencies →</span></a>
<a class="card xantham-card" href="../troubleshooting/"><h3>Understand the result</h3><p>Findings, diagnostics, and common fixes.</p><span class="xantham-card__link">Troubleshooting →</span></a>
</div>
