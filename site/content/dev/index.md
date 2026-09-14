---
title: Contributing
description: Help improve Xantham with documentation, bug reports, and generator changes.
order: 0
---

<div class="xantham-welcome">
<span class="xantham-mascot-tile xantham-actions-create" aria-hidden="true"></span>
<div class="xantham-welcome__copy"><span class="xantham-hero__eyebrow">DEV · Contributor guide</span><p class="xantham-lead">Help improve the bindings, the compiler client, or the next person’s first run.</p></div>
</div>

<div class="xantham-grid">
<a class="card xantham-card" href="setup/"><span class="xantham-card__label">Start here</span><h3>Build the repository</h3><p>Install dependencies, run the tests, and preview the docs.</p><span class="xantham-card__link">Local setup →</span></a>
<a class="card xantham-card" href="generator/"><span class="xantham-card__label">Change a mapping</span><h3>Work on the generator</h3><p>Follow the pipeline and prove changes with a small declaration fixture.</p><span class="xantham-card__link">Generator workflow →</span></a>
<a class="card xantham-card" href="generated-sources/"><span class="xantham-card__label">Maintain the sources</span><h3>Regenerate artifacts</h3><p>Refresh compiler layers, standard libraries, and the schema.</p><span class="xantham-card__link">Regeneration guide →</span></a>
<a class="card xantham-card" href="/Xantham/wire/"><span class="xantham-card__label">Understand the client</span><h3>Explore TypeScript.Wire</h3><p>Learn the compiler sessions and typed AST used by the generator.</p><span class="xantham-card__link">Wire documentation →</span></a>
</div>

## Report a problem

Include:

- Your Xantham version, npm package version, and platform.
- The command and `xantham.json` used.
- The smallest TypeScript declaration that reproduces the problem.
- The diagnostic or finding code and the F# shape you expected.

For a runtime issue, include the Fable version and a small JavaScript/F# example.
For a Wire process failure, include its stderr diagnostics.

[Open an issue](https://github.com/shayanhabibi/Xantham/issues).

## Improve the docs

The website's pages live under `site/content/`; shared styles live under `site/css/`.
Keep consumer instructions task-focused, with runnable examples.
Contributor procedures belong in this DEV section.

Preview changes in light and dark mode and at a narrow window width.
Check links, code blocks, and any cards you add.

## Submit a change

Keep the change focused and explain the behavior it improves.
For generator changes, include the small fixture, regenerated output, and relevant
findings. Run the full test pipeline before submitting.

Read the repository's `AGENTS.md` and applicable `.claude/rules/` files for
current project conventions.
