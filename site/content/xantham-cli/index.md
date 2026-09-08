---
title: Quick Start
order: 0
---

:::warning
`xantham` is in early development.
:::

Download `xantham` via dotnet as a global tool.

```bash title="xantham" frame=terminal
dotnet tool install -g xantham 

# xantham may still be in prerelease:
dotnet tool install -g xantham --prerelease
```
Ensure you have the typescript compiler version compatible installed, 
point it at a package, and generate!

```bash frame=terminal
xantham tsc init
xantham generate ./fixtures/solid-js/node_modules/solid-js
```

:::filetree
- xantham-out
  - **SolidJs.fs**
  - manifest.json
  - symbols.jsonl
:::

:::info
For further usage and configuration, [see here](guide/usage.md).
:::

## Where to go next

- [Installation](guide/installation.md)
- [Usage and configuration](guide/usage.md)