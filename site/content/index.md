---
title: Xantham
description: Generate F# bindings for Fable from TypeScript declarations.
layout: splash
---

<div class="xantham-home">
<div class="xantham-banner" role="img" aria-label="Xantham"></div>
<section class="xantham-home__intro" aria-labelledby="xantham-heading">
<div>
<span class="xantham-hero__eyebrow">TypeScript → F# · Fable 5</span>
<h1 id="xantham-heading">Your next Fable binding starts here.</h1>
<p class="xantham-lead">Generate F# bindings from TypeScript declarations. Bring npm packages into your Fable project, with a report of the types that need your attention.</p>
<div class="xantham-home__actions">
<a href="xantham-cli/" class="btn btn-primary">Get started →</a>
<a href="https://github.com/shayanhabibi/Xantham" class="btn btn-outline">View on GitHub</a>
</div>
</div>
<div class="xantham-home__art" aria-hidden="true"><span class="xantham-mascot-tile xantham-actions-wave"></span></div>
</section>

<section class="xantham-home__section" aria-labelledby="generate-heading">
<span class="xantham-hero__eyebrow">The generator</span>
<h2 id="generate-heading">Declarations in. F# out.</h2>
<p>Install the tool, cache its matching TypeScript compiler, and point it at an installed package.</p>

```bash frame=terminal title="Get generating"
dotnet tool install --global xantham
xantham tsc init
xantham generate ./node_modules/your-package
```

<div class="xantham-grid">
<a class="card xantham-card" href="xantham-cli/guide/bindings/">
<span class="xantham-card__label">Use the result</span>
<h3>From generated files to your app</h3>
<p>Add the support packages, put files in compile order, and call the package from F#.</p>
<span class="xantham-card__link">Using bindings →</span>
</a>
<a class="card xantham-card" href="xantham-cli/guide/troubleshooting/">
<span class="xantham-card__label">Know what changed</span>
<h3>See where types lose precision</h3>
<p>The generation report identifies widened and unsupported types so you can review the APIs you use.</p>
<span class="xantham-card__link">Reading the report →</span>
</a>
</div>
</section>

<section class="xantham-home__section" aria-labelledby="packages-heading">
<span class="xantham-hero__eyebrow">Support packages</span>
<h2 id="packages-heading">The types behind your bindings</h2>
<p>Use the shared support libraries alongside the generated F#.</p>
<div class="xantham-grid">
<a class="card xantham-card" href="xantham-cli/guide/packages/#utility-types">
<span class="xantham-card__label">TypeScript utility types</span>
<h3>Xantham.Fable.Core</h3>
<p>F# representations for property keys, indexed access, and nominal brands. Erased by Fable at runtime.</p>
<span class="xantham-card__link">Explore the support library →</span>
</a>
<a class="card xantham-card" href="xantham-cli/guide/packages/#standard-libraries">
<span class="xantham-card__label">ECMAScript &amp; DOM</span>
<h3>Xantham.Fable.Core.TS</h3>
<p>Generated bindings for TypeScript’s standard libraries, including promises, collections, and browser APIs.</p>
<span class="xantham-card__link">Use the standard libraries →</span>
</a>
</div>
<p>Working with Node.js? See the <a href="xantham-cli/guide/packages/#node-bindings">Node bindings and their current status</a>.</p>
</section>

<section class="xantham-home__section" aria-labelledby="wire-heading">
<div class="xantham-welcome">
<span class="xantham-mascot-tile xantham-actions-explore" aria-hidden="true"></span>
<div class="xantham-welcome__copy">
<span class="xantham-hero__eyebrow">Build with the compiler</span>
<h2 id="wire-heading">TypeScript.Wire</h2>
<p>The .NET client for TypeScript 7’s compiler API. Read syntax trees, query types, and inspect diagnostics from your own tools.</p>
<div class="xantham-home__actions"><a class="btn btn-outline" href="wire/">Explore TypeScript.Wire →</a></div>
</div>
</div>
</section>
<section class="xantham-home__section" aria-labelledby="blog-heading">
<h2 id="blog-heading">Behind the bindings</h2>
<a class="card xantham-card" href="blog/15092026-xantham/">
<span class="xantham-card__label">15 September 2026</span>
<h3>How Xantham relates to Glutinum</h3>
<p>Shared roots, package graphs, and reusable compiler access from .NET.</p>
<span class="xantham-card__link">Read the post →</span>
</a>
</section>
<section class="xantham-home__section" aria-labelledby="contribute-heading">
<h2 id="contribute-heading">Help shape Xantham</h2>
<p>A small declaration that reproduces a problem, a clearer example, or a generator improvement makes a useful contribution.</p>
<a href="dev/" class="btn btn-outline">Contributor guide →</a>
</section>
</div>
