---
title: Generator customization
description: Register F# extensions to annotate bindings and generate component properties.
order: 7
---

<p class="xantham-lead">
Use `Xantham.Generator.Customization` when a package needs additional member attributes, or a component-facing API.
</p>


Extensions are ordinary F# values registered with the generator library.
`Pipeline.generateWith extensions config input` returns output; `Pipeline.runWith extensions
config input output` writes it. 

The existing `generate` and `run` functions retain their behavior.
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
                  |> List.find (fun p ->
                      Semantic.jsName p snapshot = "disabled")
              let attribute =
                  Attribute.create
                      "System.Obsolete"
                      [AttributeValue.String "Use enabled"]
              Ok (Semantic.outputTargets property snapshot
                  |> List.fold (fun edits target ->
                      Edits.addAttribute target attribute edits)
                      Edits.empty) }
```

* Attribute arguments support:

    * strings
    * booleans
    * integers
    * resolved type references
    * enum values
    * arrays
* `Attribute.onGetter` and `onSetter` select an accessor. 
* Identical attributes coalesce; distinct attributes retain registration order. 
* Attributes that change Fable interop require `Edits.replaceInterop target (Interop.property "javascript-key")`, 
an explicit Escape edit.
* `Semantic.members snapshot` includes instance methods and concrete exported functions/values,
so their `outputTargets` can receive member attributes too.
* `Semantic.properties source snapshot` selects only properties for the companion adapter. 
* Accessor attributes and property interop edits require a property target. 
* Type-valued arguments use the final producer-qualified names.
* Every callback reads the original immutable snapshot. 
* Declaration selection uses canonical package identity and path. 
* `Semantic.tryFind "typescript/lib" ["HTMLElement"] snapshot` selects
the compiler's DOM declaration. 
* `Semantic.descendants root snapshot` follows explicit heritage,
including the root, aliases and instantiated bases. 
* A structural lookalike or an `implements` clause does not establish this relationship.
* `Semantic.implementedTypes` is a separate query.
* Missing source metadata is available through `Semantic.diagnostics`; ambiguous selection fails.

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

For example, suppose the input package is named `customization-dom-lab` and exports:

```typescript
export interface Input extends HTMLInputElement {
    custom?: string;
}
```

This F# generator selects `Input` through the compiler's actual HTMLElement ancestry and emits
only `value`, inherited `title`, and readonly `tagName`:

```fsharp
open Xantham.Generator
open Xantham.Generator.Customization

let domProperties : GeneratorExtension =
    { Identity =
        { Id = "example.dom-properties"
          Version = "1"
          Configuration = Map.ofList ["owner", "customization-dom-lab"] }
      Transform = fun snapshot ->
          let input =
              Semantic.tryFind "typescript/lib" ["HTMLElement"] snapshot
              |> Option.bind (fun root ->
                  Semantic.descendants root snapshot
                  |> List.tryFind (fun source ->
                      Semantic.package source snapshot = "customization-dom-lab"
                      && Semantic.path source snapshot = ["Input"]))

          match input with
          | None ->
              Error [Diagnostic.create "example/missing-input"
                         "Expected Input to inherit from HTMLElement." None]
          | Some input ->
              let properties =
                  Semantic.properties input snapshot
                  |> List.filter (fun property ->
                      List.contains (Semantic.jsName property snapshot)
                          ["value"; "title"; "tagName"])
              let companion =
                  Companion.create "Partas.Solid.CustomizationAcceptance"
                      "InputProperties" input snapshot
                  |> Companion.withProperties properties
              Ok (Edits.empty |> Edits.emitCompanion companion) }

[<EntryPoint>]
let main arguments =
    Pipeline.runWith [domProperties] GeneratorConfig.Default arguments[0] arguments[1]
    |> Async.RunSynchronously
    |> ignore
    0
```

Put this in a console project referencing `Xantham.Generator` and pass the input and output
directories. A runnable version is checked in under `tools/customization-example`:

```bash
dotnet run --project tools/customization-example -- tests/fixtures/customization-dom-lab tests/.scratch/my-components
```

The generated companion contains an empty marker and optional extension properties:

```fsharp
namespace Partas.Solid.CustomizationAcceptance
open Fable.Core
open Fable.Core.JsInterop

[<Interface>]
type InputProperties = interface end

[<AutoOpen>]
module InputPropertiesExtensions =
    type InputProperties with
        [<Erase>]
        member _.tagName: string = jsNative

        [<Erase>]
        member _.title
            with get (): string = jsNative
            and set (value: string) = ()

        [<Erase>]
        member _.value
            with get (): string = jsNative
            and set (value: string) = ()
```

An implementing component supplies no bodies for those properties. For direct JavaScript
access instead of Partas plugin stubs, pipe the companion through `Companion.directProperties`
before `Edits.emitCompanion`; the runnable example exposes that mode with `--direct`.

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

```bash
dotnet fsi build.fsx -- test --quick --filter customization --run-gate
```

The gate generates the HTMLElement companion through the public API, compiles a component with
no abstract property implementations using the real Partas plugin, checks the emitted JSX,
and renders it with Solid to verify the input's `value` and `title`. It also executes direct
getters/setters against JavaScript objects from both generated source and a compiled binding DLL,
including inherited properties and escaped JavaScript keys. The generator tests separately
compile attributed interfaces, constructor-backed classes, exported functions/values, and
referenced-producer consumers; assigning a readonly companion property must fail compilation.

For the complete regression suite, use `dotnet fsi build.fsx -- test --run-gate`.
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

## Project unions before widening

Reference `Xantham.Generator.Myriad` to generate companion APIs from resolved TypeScript
facts before Shape maps them to F# types. The adapter uses Myriad.Core 1.1.0; it does not
need a sibling Myriad checkout. Select exported declarations explicitly:

```fsharp
open Xantham.Generator.Myriad

let projections =
    LiteralUnions.create
        { Id = "example.choices"; Version = "1"; Configuration = Map.empty }
        "tests/.scratch/myriad-inputs"
        [{ Package = "projection-lab"
           Path = ["Choice"]
           ModuleName = "Projected.Choice"
           TypeName = "Value" }]

let compiler = Compiler.dotnet "tests/.scratch/myriad-compile" supportAssemblyPaths
let report =
    Pipeline.runProjectedWith compiler [projections] [] GeneratorConfig.Default input output
    |> Async.RunSynchronously
```

Supply the current Xantham support DLLs and referenced producer DLLs in
`supportAssemblyPaths`, as for raw replacement validation above. The runner compiles every
output source plus witnesses for the declared companion types before returning or writing.
Module and type names use unquoted ASCII identifiers; the type name starts with an uppercase letter.
Existing `GeneratorExtension` registrations can be passed as the second extension list.
An empty projection list leaves the ordinary output unchanged.

For `"auto" | "manual" | number | null | undefined`, the companion contains a normal DU:

```fsharp
type Value = Auto | Manual | Number of float | Null | Undefined
```

The generated module provides `encode : Value -> obj`,
`decode : obj -> Result<Value, string>`, and a two-case active pattern. Match a raw JavaScript
value through the decoder:

```fsharp
open Projected.Choice

let describe (raw: obj) =
    match raw with
    | Decoded Value.Null -> "explicit null"
    | Decoded Value.Undefined -> "undefined"
    | Decoded (Value.Number value) -> string value
    | Decoded Value.Auto -> "automatic"
    | Decoded Value.Manual -> "manual"
    | Invalid reason -> reason
```

`null` and `undefined` use separate strict JavaScript predicates. Strings outside the literal
set and values of other kinds return `Error`. The number arm preserves `NaN`, infinities and
negative zero. These codecs target Fable; their JavaScript operations are not .NET runtime APIs.
Decode before an `option` conversion can erase the distinction. An absent property read and a
present property containing `undefined` both yield the same JavaScript value; this API does not
inspect property presence.

The generated DU is a companion representation. Call `encode` at an outgoing JavaScript boundary
and `decode` on incoming raw values. Ordinary generated signatures retain their existing types;
this increment does not automatically replace or wrap those signatures. Equal string sets still
produce distinct F# types when selected separately. Convert through `encode`/`decode` when needed.
Overlapping sets may accept the same primitive, so decoding proves membership, not which source
union produced it. Preserve an outer application DU if that provenance matters.

The first projection contract supports string literals with `number`, `null` and `undefined`.
Boolean and numeric literals, generic aliases, broad strings, object arms and incomplete resolved
facts return `projection/unsupported-union` or `projection/incomplete-union` diagnostics.
No companion is produced for a rejected selection. Existing source widening findings remain in
the manifest. `CU006` marks the raw-source extension boundary as Escape; the source, artifact
hashes, extension identity and selection configuration are recorded under `projections`.
Projection metadata does not change raw declaration catalogs or require ordinary consumers to
register the same companion policy.

The checked-in example accepts one selected union and caller-owned scratch/reference paths:

```bash
dotnet run --project tools/customization-example -- \
  tests/fixtures/projection-lab tests/.scratch/projected-output \
  --union Choice --workspace tests/.scratch/projected-work \
  --reference src/Xantham.Fable.Core/bin/Release/net8.0/Xantham.Fable.Core.dll \
  --reference src/Xantham.Fable.Core.TS/bin/Release/net8.0/Xantham.Fable.Core.TS.dll
```

It emits `Projected.Choice.Value`. To verify the early-source matrix and companion goldens,
run `dotnet fsi build.fsx -- test --quick --filter projection`. The complete
`--run-gate` acceptance command also executes the generated active patterns, codecs, equal and
overlapping union cases, and an imported JavaScript echo function.

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
