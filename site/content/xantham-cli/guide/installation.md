---
title: Setting up
order: 0
---

Install the tool from, install the pinned compiler version, write `xantham.json`, 
run `xantham generate`{bash}.

For the compiler client `xantham` is built on, see 
[the compiler client documentation](/content/wire/index.md).

:::warning
The generator is in **alpha**.
:::

## Install the tool

```bash frame=terminal title=xantham
dotnet tool install --global xantham --prerelease
```

The package install the command `xantham` and targets `net10.0`.

`xantham --version`{bash} prints the installed version, and 
`xantham --help`{bash} the whole command line.

## Pin the compiler for the tool

:::warning
You must have `node`/`npm` installed on PATH.
:::

```bash frame=terminal title=xantham
xantham tsc version
# ^7.1.0-dev.20260902.1 not found in cache. Run `xantham tsc init`
xantham tsc init
```

The native Go `tsc` binary shipped inside the `typescript` npm package
must match the client protocol that `xantham` shipped with, especially
as there is no version handshake protocol in `tsc --api` at the moment.

