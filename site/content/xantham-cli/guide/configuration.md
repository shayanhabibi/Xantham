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

## Emit declaration catalogues

`"declarationCatalog": true` emits `declarations.json`. The default is `false`.
Use the object form for compressed output:

```json title="Brotli catalogue output"
{
  "declarationCatalog": { "enabled": true, "compression": "brotli" }
}
```

This writes `declarations.json.br`. The object requires a boolean `enabled`;
`compression` accepts `none` or `brotli` and defaults to `none`. With
`enabled: false`, neither catalogue format is emitted.

The .NET generation API still returns JSON text under `declarations.json` in
`RenderModel.Files`. `Pipeline.run` applies compression when writing to disk,
and `RunReport.OutputFiles` contains the actual written filename.
Switching formats preserves an existing alternate-format file; remove old output
explicitly if needed.

For .NET callers, `GeneratorConfig.DeclarationCatalog` remains a boolean.
The new `DeclarationCatalogCompression` field defaults to
`CatalogCompression.Uncompressed`; set `CatalogCompression.Brotli` for compressed
disk output. Callers constructing every record field must add this field.
Record updates from `GeneratorConfig.Default` keep their previous behavior.

## Resolve NoInfer

`resolveNoInfer: true` maps TypeScript's `NoInfer<T>` directly to `T`.
The default preserves `NoInfer<T>` through the support library.

## Resolve conditionals at a bound (experimental)

`resolveConditionalConstraints: true` writes a conditional type that stays open over a bounded
type parameter as its value at that bound. `@types/node` writes a request listener's argument as
`InstanceType<Request>`, where `Request extends typeof IncomingMessage`; with the key set, the
argument binds as `IncomingMessage` instead of `obj`.

```json title="Resolve conditionals at a bound"
{
  "resolveConditionalConstraints": true
}
```

Each rewritten reference is reported as `TR068`, graded Widened: a listener declared for a
subclass of the bound still reads the bound. A conditional over an unbounded parameter or a
union bound, one whose value at the bound is `any`, `unknown`, `never`, another conditional or
one of its own branches, and an alias's own declaration all stay `obj` under `TR045`. The key
is part of a catalogue's inference profile when set. The default is `false`.

## Write handler returns as unit

A DOM-style handler declared `onclick: ((ev: MouseEvent) => any) | null` binds as
`(MouseEvent -> obj) option`, so every handler body has to end in a value. `anyReturnAsUnit` is
a .NET regular expression over property and value names; a callback typed directly at a matching
name returns `unit` where its source return is `any`.

```json title="Write on* handler returns as unit"
{
  "anyReturnAsUnit": "^on"
}
```

The match is case-sensitive and unanchored unless the pattern anchors itself. Each rewritten
return is reported as `TR065`. These keep `obj`: method returns, a callback nested inside a
selected one, a named callback alias, an anonymous callback of two or more arguments (declared as
a delegate) and a stated return type. The compiler-library binding `Xantham.Fable.Core.TS`
selects `^on`, so a configuration that reuses its declaration catalogue sets
`"anyReturnAsUnit": "^on"` too. The pattern is part of a catalogue's inference profile, so any
other value fails the inference-profile check.

## Add overloads per union arm

`unionArmOverloads` synthesizes one overload per arm beside a member whose parameter is an
erased union, so `animate(targets: TargetsParam)` also gains `animate(targets: string)` and
`animate(targets: string[])`. The feature is opt-in: enabling it makes existing `!^` casts at
those call sites ambiguous (FS0041).

```json title="Enable union arm overloads"
{
  "unionArmOverloads": {
    "enabled": true,
    "maxArms": 4,
    "policy": "single"
  }
}
```

| Field | Default | Meaning |
|-------|---------|---------|
| `enabled` | `false` | Synthesize one overload per arm beside the union member. |
| `maxArms` | `4` | The largest arm count that expands, at least 2. A union with more arms keeps the union member alone and reports why. |
| `policy` | `"single"` | Which union parameters expand. Only `"single"` is accepted: a member with exactly one union parameter. `"linear"` is reserved and rejected. |

Every expanded or declined member is reported under the `UA` finding codes, so a member left
unchanged names the condition it failed. See [Troubleshooting](troubleshooting.md) for reading
findings.

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
