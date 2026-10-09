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

## Ship mutually dependent groups

Set `recursiveGroups` when the entry package and shipped dependencies refer back to
one another. Their modules share a `namespace rec` source at
`groups/<namespace>.fs`, which compiles the complete cycle together.

```json title="xantham.json"
{
  "module": "MyBindings.Client",
  "namespace": "MyBindings",
  "recursiveGroups": true,
  "groups": { "shared-models": "ship" }
}
```

The namespace must be explicit, and each emitted package module must be its immediate
child. Compiler-library output keeps its configured layout. The default remains
separate files. Declaration identities and qualified type names stay the same.

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

For Brotli output, configure the producer with
`"declarationCatalog": { "enabled": true, "compression": "brotli" }`
and reference `declarations.json.br`:

```json title="Compressed catalogue reference"
{
  "declarationReferences": ["/bindings/root/declarations.json.br"]
}
```

A `.br` filename suffix selects Brotli, case-insensitively. Other filenames
are read as JSON, including custom filenames. References may mix formats in
their existing order. Invalid, incomplete or trailing compressed data fails
generation before consumer files are written.

Both formats have an inclusive limit of 128 MiB (134,217,728 bytes) of decoded
JSON. This measures the uncompressed catalogue's UTF-8 bytes, regardless of its
compressed size. The reader streams these bytes into the JSON decoder; the
JSON document and authenticated catalogue model still occupy memory.
Switching producer formats preserves any old alternate-format file in the
output directory; reference the file selected for the current run.

The consumer reuses the producer's F# identities while retaining its own imports.
Compile producers first, following the catalog's ordered `owners` list.

## Choose a shared owner before generating related libraries

Identical anonymous literal unions share a structural catalog identity. Two independent
producers can emit that identity as different F# enum types; a consumer referencing both
catalogs rejects the conflicting owners. Catalog order does not choose a winner.

Choose the owner explicitly in the generation graph. Generate producer A first, then put
A's `declarations.json` in producer B's `declarationReferences` and regenerate B. Consumers
can then reference both catalogs. B's generated API uses A's enum, and B's catalog records
its dependency on A. Compile and package that dependency as part of B's public API.

Make this ownership choice consistently across a binding family and regenerate its affected
producers together. Selecting a preferred catalog only in the final consumer cannot change
the distinct enum types already compiled into the producers. Named literal aliases retain
their own declaration identities even when their values match.

## Keep catalogs compatible

New catalogs use schema 2. Related bindings can reuse catalogs across Windows and Linux
when the installed TypeScript packages have the exact same release and source revision,
the AST protocol matches, and Xantham's identity, API, inference, customization, and policy
contracts match. Rebuilding Xantham with the same contracts remains compatible.
Custom compiler executables without verifiable package metadata require identical compiler
binary hashes. Matching TypeScript major/minor versions alone is insufficient.

Keep `lib`, `types`, group dispositions, and inference options compatible.
Use separate catalogs for environments that need different globals.

Upgrade consumers before regenerating producers: older Xantham builds reject schema 2.
Schema 1 catalogs retain exact compiler and generator binary checks. Regenerate their
producers with the upgraded generator to obtain portable catalogs. Continue supplying
explicit `declarationReferences` paths as shown above.

Both schemas check declaration identity, package/source hashes, owner dependencies,
generic arity and constraints, F# APIs, and customization variants. Schema 2 retains
compiler and generator fingerprints as producer provenance.
JSON and Brotli carry the same schema and pass the same authentication checks.
If a catalog is rejected, regenerate the related bindings together and inspect
the diagnostic. Some entry-dependent or generic shapes still cannot share an
emitted identity; those combinations require separate bindings or a mapping change.

<div class="xantham-callout">
<p><strong>A reference needs an implementation.</strong> Both <code>reference</code> groups and declaration catalogs require the producer’s generated F# or compiled assembly in the consumer project.</p>
</div>

