---
title: Installation
description: Install Xantham and its matching TypeScript compiler.
order: 1
---

<p class="xantham-lead">Set up the command-line tool once, then generate bindings from your installed npm packages.</p>

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">For the tool</span><h3>.NET 10 SDK</h3><p>The Xantham CLI targets .NET 10.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">For the compiler</span><h3>Node.js and npm</h3><p>Keep both on PATH. Xantham uses npm to install its matching TypeScript compiler.</p></div>
</div>

## Install Xantham

```bash frame=terminal
dotnet tool install --global xantham
xantham --version
```

For a prerelease build, add `--prerelease` to the install command.

## Set up TypeScript

```bash frame=terminal
xantham tsc init
xantham tsc version
```

`tsc init` caches the compiler version expected by your installed Xantham tool.
`tsc version` shows the expected version and whether it is available.

:::note
Xantham uses TypeScript 7's native compiler API. Run `xantham tsc init` after updating
the tool to install its matching compiler.
:::

## Update the tool

```bash frame=terminal
dotnet tool update --global xantham
xantham tsc init
```

## Check the setup

```bash frame=terminal
dotnet --version
node --version
npm --version
xantham --help
```

If `xantham` is unavailable after installation, add the .NET tools directory to PATH:
`$HOME/.dotnet/tools` on macOS/Linux or `%USERPROFILE%\.dotnet\tools` on Windows.
Open a new terminal after changing PATH.

<div class="xantham-grid xantham-next">
<a class="card xantham-card" href="../../"><h3>Generate your first binding</h3><p>A small package, from install to F# output.</p><span class="xantham-card__link">Quick start →</span></a>
<a class="card xantham-card" href="../troubleshooting/"><h3>Something failed?</h3><p>Compiler setup, missing declarations, and generation reports.</p><span class="xantham-card__link">Troubleshooting →</span></a>
</div>

