# Generator customization design

Status: design agreed in conversation on 2026-10-06; implementation has not started.
Companion execution plan: [implementation](2026-10-06-generator-customization.md).

## Purpose

Allow an F# library author to customize generated bindings without maintaining a fork of
Xantham. Prove the interface with two distinct extensions: targeted member attributes and
Partas.Solid component properties generated from DOM declarations.

The motivating transformation moves properties from an abstract implementation contract to
extension members. A component implements a marker and obtains typed property syntax without
implementing every DOM property. Ordinary TypeScript bindings remain available alongside the
component-facing companions. Explicit replacement remains available for other extensions.

## Agreed scope

- Extensions are reusable F# code registered by a small generator executable. JSON plugin
  discovery, NuGet resolution, and an extension marketplace are outside this iteration.
- Xantham owns a supported semantic view and structured output operations. Internal pass
  records, the live compiler session, and arbitrary pass insertion are not the public contract.
- The framework extension owns companion names, receiver types, attributes, and DSL conventions.
  Xantham has no Partas.Solid or Oxpecker dependency.
- Selection includes the real compiler-library `HTMLElement` and its explicit transitive
  descendants, including interface heritage. Structural resemblance and names alone do not
  select a type. Explicit `implements` relationships remain separately queryable.
- Referenced declarations are selectable when their source declarations and binding identities
  are available. Their companions have separate ownership; their bindings are not rewritten.
- Initial companion generation covers mutable and readonly properties, inherited properties,
  substituted generic types, and the declaring identity of each member. Methods, indexers,
  event DSLs, and comprehensive DOM-to-JSX policy are later work. Report excluded member kinds.
- Attribute additions are structured and support custom attribute types. Interop changes
  require an explicit replacement operation. Diagnose incompatible attributes.
- Registration order is explicit. Additive changes compose; competing replacements and
  generated name collisions fail instead of taking the last writer.
- Preserve existing fidelity findings. Attribute involvement is recorded; semantic replacement
  cannot silently retain an Exact claim. Omission and widening are explicit findings.
- Extension identity, version, canonical configuration, and order participate in provenance.
  API-affecting transformations participate in declaration compatibility. Companion output has
  its own generated API identity and references the authenticated base API.
- Acceptance includes a working Partas.Solid component, F# compile checks, generated JSX
  checks, and Fable runtime checks. A successful print or an early catalog error is insufficient.

## Repository evidence

Evidence was read from the working trees, including existing uncommitted changes, on 2026-10-06.

- `src/Xantham.Generator/Pipeline.fs`: `generate` executes Harvest, Resolve, Shape,
  `DeclarationCatalog.apply`, source rendering, then manifest rendering. `run` writes only
  after `generate` returns. Public `runTier` is not an extension installation contract.
- `src/Xantham.Generator/Model.fs`: the generator already carries type facts and heritage
  alongside shaped output, but member records have no arbitrary F# attribute collection.
- `docs/.ai/footguns.md`: exports materialize during `order-declarations`; changing them before
  that stage can silently do nothing. Coverage, catalog identity, declaration ordering, and
  callable ownership are invariants a customization must preserve.
- `site/content/xantham-cli/guide/dependencies.md`: catalogs authenticate source hashes and
  generated APIs. Referenced F# identities still require consumer assembly references.
- Sibling `../Partas.Solid/src/Partas.Solid/HtmlAttributes.fs:12`: an AutoOpen module extends
  marker interfaces with erased getters/setters. `HTMLAttributes` has no abstract members in
  the public API projection. This is a concrete precedent for the requested representation.
- Sibling `../Partas.Solid/AGENTS.md`: `SolidTypeComponent` has namespace/member-shape
  requirements, and the Fable plugin rewrites properties into JSX. DOM property forwarding
  alone does not prove compatibility with this transformation.
- Sibling `../Partas.Solid/tests/Partas.Solid.Tests.Plugin/Compiled/SolidCases/Tag Extensions/`
  demonstrates erased marker-implementing components and expected JSX.
- Sibling Partas.Solid project currently declares version 3.0.0, Fable.Core 5.2.0 and
  Xantham.Fable.Core.TS 0.1.0-alpha.3. These are local source facts, not a verified published
  package selection. The acceptance harness must use the current Xantham support project and
  a reproducibly pinned Partas source revision rather than silently mixing support versions.

Fresh fslangmcp project checks: Xantham.Generator had zero errors/warnings; Partas.Solid had
zero errors and 14 warnings. These establish readable semantic context, not feature validation.

## Proposed seam

Expose one immutable semantic snapshot per run and let registered extensions return ordered
edit batches. Each extension observes the same original snapshot. The engine validates all
batches together and applies them transactionally to the output model. Extensions do not depend
on accidentally observing another extension's partial output.

The snapshot carries stable source and member handles, resolved property types, declaring and
effective receiver identities, explicit heritage, original JS property keys, readonly status,
binding identities, and inherited findings. Handles are opaque; TypeScript checker IDs remain
run-local implementation details and never appear in fingerprints.

The engine supports adding attributes, explicitly replacing interop, emitting companion
declarations, and explicitly replacing owned declarations. Structured replacement keeps the
original binding identity and must pass dependency/heritage checks. Raw replacement is a marked
escape with a declared public contract; it requires compile validation and is excluded from
authenticated catalog production unless that contract can be authenticated. It must not forge
compatibility by copying the old declaration's fingerprint.

Keep existing `Pipeline.generate` and `Pipeline.run` signatures. Add `generateWith` and
`runWith`; their empty-extension behavior must equal the existing operations. Keep extension
configuration out of the serializable `GeneratorConfig` in this iteration.

## Pipeline placement

1. Resolve the selected source surface, retaining facts for referenced declarations needed by
   the extension even when those declarations are omitted from ordinary output.
2. Complete ordinary Shape, including export materialization. Build the semantic snapshot and
   an output projection with stable declaration/member targets.
3. Evaluate extensions, validate conflicts, apply edits, and finalize ordering, names,
   references, and coverage. Extract a reusable finalization step where necessary; do not rerun
   the entire Shape pipeline or assume a second run of its passes is idempotent.
4. Authenticate reference catalogs and customized owned APIs. Bind provisional references to
   their authenticated producer identities before exposing final output. Companion ownership
   must never overwrite the source declaration's canonical identity.
5. Render source, catalog, and manifest from the same accepted model. An extension error occurs
   before `runWith` writes files.

Catalog handling may need separate reference-authentication and producer-serialization steps.
The implementation must preserve existing source authentication while making transformed output
available to API hashing. Blindly moving `DeclarationCatalog.apply` across all extension work
would leave one of these requirements unproven.

## Concrete acceptance behavior

The first Partas example generates a distinct marker for `HTMLInputElement` properties and an
AutoOpen extension module. A component implements that marker and Partas's existing node marker.
It accesses generated `value` and inherited `title` without declaring either property. The
framework adapter emits Partas-compatible erased accessor stubs. Readonly properties, such as
`tagName`, expose a getter only. A negative compile test attempts a readonly assignment.

The first executable probe determines and pins the exact optional-extension syntax and plugin
recognition for a separately generated marker. It must use the real Partas plugin, not a mock
that accepts arbitrary erased properties. If the plugin cannot recognize that shape, record
the failing JSX/diagnostic and isolate the required adapter or plugin change before freezing
the public extension contract. This is an implementation risk, not a proven feature today.

A second, framework-independent adapter emits getter/setter access to the original JavaScript
property key on an existing object. Its Fable check proves actual reads/writes and correct
escaping of unusual keys. It makes no claim that a component's props object is a DOM element.

DOM property types and JSX props are not interchangeable in general. The Partas acceptance
selects `value` and `title`; it does not infer event or attribute renaming policy from all DOM
members. Readonly getter behavior is tested on a supplied object, not assumed to be populated
on every component props object.

## Delivery and exclusions

Implement sequential vertical slices: real consumer probe; semantic selection; attribute edits;
companions; replacements/conflicts; findings/catalog provenance; reproducible framework gate.
Each slice carries tests through the supported interface. Framework code lives in an example
generator/harness. A consumer guide explains normal generation, extension registration, failure
behavior, and the packaging requirements for Fable source versus DLL consumption.

The iteration does not promise arbitrary F# AST construction, arbitrary compiler queries,
custom pass scheduling, sandboxed third-party code, automatic discovery of every installed DOM
declaration, or a generated replacement for the full Partas HTML/ARIA/SVG library. An explicit
source entry controls the selection universe; missing source metadata is a diagnostic.
