---
title: Quick start
description: Generate your first Fable binding with Xantham.
order: 0
---

<div class="xantham-welcome">
<div class="xantham-welcome__copy"><p class="xantham-lead">Turn an installed npm package’s TypeScript declarations into F# bindings for your Fable app.</p></div>
<span class="xantham-mascot-tile xantham-poses-floating" aria-hidden="true"></span>
</div>

## 1. Install the tool

You need the .NET 10 SDK, Node.js, and npm. See [Installation](guide/installation.md)
for setup and updates.

```bash frame=terminal
dotnet tool install --global xantham
xantham tsc init
```

## 2. Install a package

Start with a small package that includes its own declarations:

```bash frame=terminal
mkdir my-bindings
cd my-bindings
npm init -y
npm install --save-exact ansi-regex@6.3.0
```

Keep the npm version pinned so later runs use the same declarations.

## 3. Generate the binding

```bash frame=terminal
xantham generate ./node_modules/ansi-regex -o ./out
```

:::filetree
- out
  - AnsiRegex.fs
  - manifest.json
  - symbols.jsonl
:::

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">Your binding</span><h3>AnsiRegex.fs</h3><p>The F# types and imports to add to your Fable project.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Your report</span><h3>manifest.json + symbols.jsonl</h3><p>A summary of the mapping quality, plus findings for individual symbols.</p></div>
</div>

## 4. Use it in Fable

Add the support packages and include the generated file before code that uses it.
Continue with [Using bindings](guide/bindings.md) for the project setup.

:::note
Bindings target **Fable 5.x**. Review the report for APIs you intend to use:
some TypeScript types need a wider F# representation.
:::

<div class="xantham-grid xantham-next">
<a class="card xantham-card" href="guide/bindings/"><h3>Use the generated F#</h3><p>Package references, file order, and calling exports.</p><span class="xantham-card__link">Using bindings →</span></a>
<a class="card xantham-card" href="guide/usage/"><h3>Generate your own package</h3><p>Choose declarations, output paths, and configuration.</p><span class="xantham-card__link">Generation guide →</span></a>
</div>
