---
title: Support packages
description: Choose the shared libraries used by generated Fable bindings.
order: 4
---

<div class="xantham-welcome">
<span class="xantham-mascot-tile xantham-poses-with-coffee" aria-hidden="true"></span>
<div class="xantham-welcome__copy"><p class="xantham-lead">Shared utility types and standard-library bindings keep generated packages working with the same F# types.</p></div>
</div>

Use these alongside `Fable.Core` **5.2.0**, with Xantham package versions matching
your generator release.

## Utility types

**Xantham.Fable.Core**

```bash frame=terminal
dotnet add package Xantham.Fable.Core
```

The support library represents TypeScript constructs such as property keys,
indexed access, and nominal brands. Its utility types erase at runtime.

<div class="xantham-grid">
<div class="card xantham-card"><h3>Property keys</h3><p><code>keyof&lt;'T&gt;</code> carries a property name. <code>typekeyof&lt;'T, 'Value&gt;</code> also retains its value type.</p></div>
<div class="card xantham-card"><h3>Indexed values</h3><p>Property locks, keys, and record helpers represent access to TypeScript object properties.</p></div>
</div>

```fsharp
open Fable.Core.JS

type Config = { Timeout: int }

let timeoutKey = typekeyof<Config, int> _.Timeout
let timeout = TypeKeyOf.item timeoutKey { Timeout = 30 }
```

Open `Fable.Core.JS` for the erased types. Helper functions are available from
the auto-opened `XanthamFableCore` module.

See the <a href="/Xantham/reference/xantham-fable-core/">API reference</a> for the full surface.

## Standard libraries

**Xantham.Fable.Core.TS**

```bash frame=terminal
dotnet add package Xantham.Fable.Core.TS
```

Generated bindings for TypeScript's `esnext` and DOM libraries.
Add this package to the consumer project alongside `Xantham.Fable.Core`.

```fsharp
open Fable.Core.TS

type PendingRequest = {
    Completion: Promise<string>
    Target: Fable.Core.TS.Dom.EventTarget
}
```

The `Es` module is auto-opened from `Fable.Core.TS`.
DOM types stay under `Fable.Core.TS.Dom`.

Custom [dependency mappings](dependencies.md) can use a separately generated
standard library instead. Adding an assembly reference does not change the
generator's configured group dispositions.

See the <a href="/Xantham/reference/xantham-fable-core-ts/">standard-library reference</a>.

## Node bindings

**Xantham.Fable.Node**

Bindings for `@types/node` are under development. The package is currently marked
as a stub in the repository; treat it as work in progress for this release.

For a specific Node package, generate from its installed declarations and review
the resulting findings and compile errors.

## TypeScript.Wire is separate

`Xantham.TypeScript.Wire` is a .NET compiler client for tools that inspect TypeScript.
A Fable app using generated bindings does not need it.

<a class="card xantham-card xantham-next" href="/Xantham/wire/">
    <h3>Build tools with TypeScript.Wire</h3>
    <p>Read ASTs, query the type checker, and request diagnostics.</p>
    <span class="xantham-card__link">Compiler client guide →</span>
</a>
