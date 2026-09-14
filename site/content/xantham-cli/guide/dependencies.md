---
title: Dependencies and shared types
description: Control dependency output and reuse types between generated bindings.
order: 6
---

<p class="xantham-lead">Choose what happens when a declaration refers to a type from another package.</p>

## Set group dispositions

`groups` is keyed by npm package name. Use `typescript/lib` for TypeScript's
standard libraries.

<div class="xantham-grid">
<div class="card xantham-card"><span class="xantham-card__label">Generate it</span><h3>ship</h3><p>Emit the dependency’s declarations under <code>groups/</code>. Compile them before the binding that uses them.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Reuse it</span><h3>reference</h3><p>Use the group’s F# module name. Supply a separately generated binding with matching names.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Redirect it</span><h3>map</h3><p>Map named TypeScript types to existing F# types. Names missing from the map widen.</p></div>
<div class="card xantham-card"><span class="xantham-card__label">Accept a wider type</span><h3>widen</h3><p>Render the reference as <code>obj</code> and report a finding. This is the default for unlisted groups.</p></div>
</div>

```json title="xantham.json"
{
  "namespace": "MyBindings",
  "groups": {
    "shared-models": "ship",
    "another-package": "reference"
  }
}
```

Configure the same namespace for related generation runs.

## Map an existing type

A string destination takes no type arguments. Use the object form to state
the destination's generic arity:

```json title="xantham.json"
{
  "groups": {
    "typescript/lib": {
      "map": {
        "RegExp": "System.Text.RegularExpressions.Regex",
        "WeakRef": { "name": "System.WeakReference", "arity": 1 }
      }
    }
  }
}
```

Choose mappings whose Fable behavior matches the JavaScript type.
A mismatched arity widens and reports finding `TR053`.

## Share declaration identities

When several bindings must reuse the same types, emit a declaration catalog
from the producer:

```json title="Producer configuration"
{
  "module": "Example",
  "declarationCatalog": true
}
```

The run writes `declarations.json` beside its F# output. Reference it from a later run:

```json title="Consumer configuration"
{
  "module": "Example.Adapter",
  "entry": "adapter.d.ts",
  "runtime": "example/adapter",
  "declarationReferences": ["/bindings/root/declarations.json"]
}
```

Catalog paths are absolute or relative to the input package directory.
Replace the example path with the producer's output path.

The consumer reuses the producer's F# identities while retaining its own imports.
Compile producers first, following the catalog's ordered `owners` list.

## Keep catalogs compatible

Generate related bindings with the same Xantham build and compatible `lib`,
`types`, group dispositions, and inference options.
Keep separate catalogs for environments that need different globals.

Catalogs check declaration identity, package/source hashes, and F# API compatibility.
If a catalog is rejected, regenerate the related bindings together and inspect
the diagnostic. Some entry-dependent or generic shapes still cannot share an
emitted identity; those combinations require separate bindings or a mapping change.

<div class="xantham-callout">
<p><strong>A reference needs an implementation.</strong> Both <code>reference</code> groups and declaration catalogs require the producer’s generated F# or compiled assembly in the consumer project.</p>
</div>

