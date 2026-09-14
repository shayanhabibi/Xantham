---
title: Generate bindings
description: Select an input package and generate its F# bindings.
order: 2
---

<p class="xantham-lead">Point Xantham at a package directory. Its declarations and installed dependencies supply the input.</p>

## Install the input

Use an npm project with the package you want to bind installed:

```bash frame=terminal
npm install --save-exact animejs@4.5.0
xantham generate ./node_modules/animejs -o ./out
```

The input directory must contain `package.json`. For a package whose declarations
come from DefinitelyTyped, install the corresponding `@types` package and generate
from its directory. Keep the JavaScript package installed for runtime use.

## Set the output directory

```bash frame=terminal
xantham generate ./node_modules/animejs --out ./bindings
```

The default is `./xantham-out`, relative to your working directory.
Output includes F# files, `manifest.json`, and `symbols.jsonl`.
Dependencies configured as `"ship"` are emitted under `groups/`.

## Add configuration

Keep your configuration outside `node_modules` so a reinstall preserves it:

```json title="xantham.json"
{
  "module": "MyApp.Anime",
  "types": []
}
```

```bash frame=terminal
xantham generate ./node_modules/animejs --config ./xantham.json -o ./out
```

All settings are optional. JSON comments and trailing commas are accepted.
Without `--config`, Xantham looks for `xantham.json` in the input package directory.

<div class="xantham-callout">
<p><strong>Paths have two different bases.</strong> The CLI’s <code>--config</code> and <code>--out</code> paths are relative to your working directory. Declaration paths inside the configuration are relative to the input package directory.</p>
</div>

## Choose declarations

By default, Xantham selects the package's public declarations and generates
concrete subpaths from its `exports` map as nested F# modules.

For one declaration file, set `entry`:

```json title="xantham.json"
{
  "entry": "dist/adapter.d.ts",
  "runtime": "example-package/adapter",
  "module": "Example.Adapter"
}
```

`entry` selects a file inside the input package and disables subpath enumeration.
`runtime` specifies the JavaScript import used by the binding.
Use the package's public import path, which may differ from the declaration file path.

For several selected exports, conditional declarations, or wildcard expansion,
see [Configuration](configuration.md#select-public-inputs).

## Control terminal output

- `--quiet` prints the file list without the findings summary.
- `--json` selects JSON output.
- `--color auto|always|never` controls terminal colors.
- `--banner auto|always|never` controls the CLI banner.

Run `xantham generate --help` for the options in your installed version.

## Regenerate

Keep the npm lockfile, Xantham version, and configuration with your project.
Regenerate when the package changes, then review the F# diff and findings.

Put application-specific wrappers in a separate F# file so regeneration preserves them.

<div class="xantham-grid xantham-next">
<a class="card xantham-card" href="../bindings/"><h3>Compile and use the result</h3><p>Add the generated files to your Fable project.</p><span class="xantham-card__link">Using bindings →</span></a>
<a class="card xantham-card" href="../configuration/"><h3>Adjust the mapping</h3><p>Modules, libraries, ambient types, and public inputs.</p><span class="xantham-card__link">Configuration →</span></a>
</div>
