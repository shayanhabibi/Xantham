---
title: Generator customization
description: Register F# extensions to annotate bindings and generate component properties.
order: 7
---

Use `Xantham.Generator.Customization` when a package needs additional member attributes or a
component-facing API. Extensions are ordinary F# values registered with the generator library.
`Pipeline.generateWith extensions config input` returns output; `Pipeline.runWith extensions
config input output` writes it. The existing `generate` and `run` functions retain their behavior.
An empty registration produces the same sources, manifest and declaration catalog.

## Add an attribute

```fsharp
open Xantham.Generator
open Xantham.Generator.Customization

let attributes : GeneratorExtension =
    { Identity = { Id = "my-package.attributes"; Version = "1"; Configuration = Map.empty }
      Transform = fun snapshot ->
          match Semantic.tryFind "customization-lab" ["Input"] snapshot with
          | None -> Ok Edits.empty
          | Some input ->
              let property =
                  Semantic.properties input snapshot
                  |> List.find (fun p -> Semantic.jsName p snapshot = "disabled")
              let attribute = Attribute.create "System.Obsolete" [AttributeValue.String "Use enabled"]
              Ok (Semantic.outputTargets property snapshot
                  |> List.fold (fun edits target -> Edits.addAttribute target attribute edits) Edits.empty) }
```

Attribute arguments support strings, booleans, integers, resolved type references, enum values
and arrays. `Attribute.onGetter` and `onSetter` select an accessor. Identical attributes coalesce;
distinct attributes retain registration order. Attributes that change Fable interop require
`Edits.replaceInterop target (Interop.property "javascript-key")`, an explicit Escape edit.

`Semantic.members snapshot` includes instance methods and concrete exported functions/values,
so their `outputTargets` can receive member attributes too. `Semantic.properties source snapshot`
selects only properties for the companion adapter. Accessor attributes and property interop
edits require a property target. Type-valued arguments use the final producer-qualified names.

Every callback reads the original immutable snapshot. Declaration selection uses canonical
package identity and path. `Semantic.tryFind "typescript/lib" ["HTMLElement"] snapshot` selects
the compiler's DOM declaration. `Semantic.descendants root snapshot` follows explicit heritage,
including the root, aliases and instantiated bases. A structural lookalike or an `implements`
clause does not establish this relationship. `Semantic.implementedTypes` is a separate query.
Missing source metadata is available through `Semantic.diagnostics`; ambiguous selection fails.

## Generate component properties

`Companion.create namespace marker source snapshot` emits an independent marker interface and
an auto-open module of optional extension properties. Ordinary bindings keep their abstract
members. Property types include inherited generic substitutions and optionality; readonly
properties receive a getter. `Companion.withProperties` chooses a subset and `withBases` adds
explicit marker references from `Companion.reference`. Dependencies are emitted before users.

The default accessor mode uses erased stubs for a framework plugin.
`Companion.directProperties` uses Emit accessors to read and write the original JavaScript keys,
including keys requiring escaping. Those accessors work when the bindings are consumed as a DLL.
Methods and indexers are excluded from this property adapter and recorded in findings.

The executable example selects a real HTMLElement descendant and emits `value`, inherited
`title`, and readonly `tagName`:

```powershell
rtk dotnet run --project tools/customization-example -- tests/fixtures/customization-dom-lab tests/.scratch/my-components
```

Its generated marker is `Partas.Solid.CustomizationAcceptance.InputProperties`. Compile the
generated companion file before this consumer, alongside Partas.Solid and its actual Fable plugin:

```fsharp
namespace Partas.Solid.CustomizationProbe
open Partas.Solid
open Fable.Core
open Partas.Solid.CustomizationAcceptance

[<Erase>]
type CustomInput() =
    interface HtmlElement
    interface InputProperties
    [<SolidTypeComponent>]
    member props.View = input (value = props.value, title = props.title)

module App =
    [<SolidComponent>]
    let view () = CustomInput(value = "custom value", title = "custom title")
```

The tested Partas plugin recognizes extensions under `Partas.Solid`; this component implements
`HtmlElement`. The acceptance gate verifies the emitted JSX and Solid runtime properties against
a checksummed Partas 3.0.0 source snapshot. Erased framework companions require the framework's
plugin/source packaging. Direct Emit companions can be distributed as a binding DLL.

## Verify a customization

From a configured checkout, run the focused generator tests and the framework acceptance gate:

```powershell
rtk dotnet fsi build.fsx -- test --quick --filter customization --run-gate
```

The gate generates the HTMLElement companion through the public API, compiles a component with
no abstract property implementations using the real Partas plugin, checks the emitted JSX,
and renders it with Solid to verify the input's `value` and `title`. It also executes direct
getters/setters against JavaScript objects from both generated source and a compiled binding DLL,
including inherited properties and escaped JavaScript keys. The generator tests separately
compile attributed interfaces, constructor-backed classes, exported functions/values, and
referenced-producer consumers; assigning a readonly companion property must fail compilation.

For the complete regression suite, use `rtk dotnet fsi build.fsx -- test --run-gate`.
The Partas source snapshot and npm lockfile are checked in under
`tests/Xantham.Generator.PartasGate`; the gate needs no sibling Partas checkout.

## Replace a declaration explicitly

`Replacement.marker` replaces an owned interface with a marker, preserving its binding name.
`Replacement.properties members snapshot` selects resolved abstract property contracts;
`Replacement.withBases` declares their dependencies. Apply either with `Edits.replaceDeclaration`.
A changed interface with heritage consumers is rejected. Replacement is always reported as
Escape, since the original TypeScript contract has changed.
Selected signatures cannot introduce type variables absent from the replacement's original head.

`Replacement.raw source ["BindingName"] dependencies` is an explicit source escape for a
non-generic interface. It requires `Pipeline.generateValidatedWith compiler extensions config
input`, where `Compiler.dotnet scratchDirectory assemblyReferences` runs an actual .NET build
against Fable.Core 5.2.0 and the supplied consumer DLLs. Include the current Xantham support DLLs
and any referenced producer assemblies. Validation checks the complete output and a witness for
the declared export before returning it. Raw replacements are rejected for catalog production.
The validation directory is caller-owned scratch space; repository tests use `tests/.scratch`.

## Ownership, conflicts and provenance

Referenced declarations remain available for semantic selection and companion generation.
Mutation requires an owned output target. Duplicate extension IDs, duplicate companion names,
dependency cycles, competing replacements, and edits to members removed by replacement fail
before output is written. Getter/setter attributes must match the property's accessor contract.

Manifests retain source fidelity findings and add `CU` findings for attributes, companions,
omissions and replacements. They record extension identity, version, canonically sorted
configuration and companion artifact hashes. Companions have their own identities. Customized
producer catalogs carry a variant fingerprint alongside the authenticated original API;
companion-only edits leave base-binding fingerprints intact. Consumers reject conflicting
versions/configurations for the same registered extension ID and authenticate source hashes.
Catalog reference reuse rejects a variant whose replacement changed the original contract;
a marker cannot authenticate the former abstract interface's API.

Registration is explicit F# code. The JSON configuration and CLI do not load extension assemblies.
