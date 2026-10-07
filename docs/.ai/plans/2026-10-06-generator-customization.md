# Generator Customization Implementation Plan

> **For agentic workers:** Use superpowers:executing-plans for sequential implementation.
> Use superpowers:subagent-driven-development only if the user chooses delegated execution.
> Steps use checkbox syntax for tracking. Implementation has not started.

**Goal:** Support reusable generator customizations for member attributes, component-facing
property companions, and explicit representation replacement, with validated output and provenance.

**Architecture:** Extensions read an immutable semantic snapshot and return structured edits.
An internal adapter validates and applies them after ordinary shaping, then finalizes output
before catalog serialization and rendering. Framework policies live outside the generator.

**Tech Stack:** F#/.NET 10 generator, Fable 5.x, Fable.Core 5.2.0, the root TypeScript 7.x pin,
Expecto, compile/run gates, and the real Partas.Solid Fable plugin.

**Spec:** [Agreed design and repository evidence](2026-10-06-generator-customization-design.md).

## Global constraints

- Generated bindings target **Fable 5.x only**.
- Compile against **Fable.Core 5.2.0**, current `src/Xantham.Fable.Core.TS`, and the root
  `package.json` TypeScript 7.x compiler/library pin.
- Prefix shell commands with `rtk`; run suites through `build.fsx`, never `dotnet test`.
- Read `docs/.ai/footguns.md` and applicable `.claude/rules/` before implementation.
- Run fslangmcp `check` before semantic `find`; run `fcs_refactor_impact` before changing public
  signatures. Preserve existing entry points by adding functions.
- Scratch projects live under `tests/.scratch/`; use the existing `Scratch.directory` helper.
- Existing unrelated edits belong to the user. Execution begins from an explicitly recorded
  working-tree baseline; a worktree based only on HEAD will omit substantial current changes.
- Update architecture/type-mapping phase records in the same commit as behavior changes.
- Empty extension registration must preserve existing generated source and findings.

## Review focus

1. Aliased or same-named `HTMLElement` impostors: select by authenticated source identity (Task 2).
2. Diamond inheritance, overrides, and instantiated generic bases: retain effective types and
   emit each property once (Tasks 2 and 4).
3. Accessors consumed from DLLs: prove attribute-only access works without unavailable inline
   bodies; prove framework consumption in its actual source/package mode (Tasks 1 and 7).
4. Referenced producer variants and companion ownership: reject stale customized APIs without
   contaminating unchanged base-binding fingerprints (Task 6).
5. Extension exceptions, conflicts, and repeat runs: produce deterministic diagnostics and leave
   destination files unchanged on generation failure (Tasks 3, 5, and 6).

## Proposed public interface

These are new API names, not claims about existing code. Keep them in
`src/Xantham.Generator/Customization/Contract.fs` and expose them through a signature file.
Public handles and output types have private representations and builder/query functions;
their implementations may use current internal IR without exporting it.

```fsharp
type ExtensionIdentity = {
    Id: string
    Version: string
    Configuration: Map<string, string>
}

// Opaque supported types: SemanticSnapshot, EditBatch, ExtensionDiagnostic.
// A batch owns ordered edits and per-source diagnostics.
type GeneratorExtension = {
    Identity: ExtensionIdentity
    Transform: SemanticSnapshot -> Result<EditBatch, ExtensionDiagnostic list>
}

// Added to Pipeline; existing generate/run delegate with an empty list.
val generateWith:
    GeneratorExtension list -> GeneratorConfig -> string -> Async<RenderModel>
val runWith:
    GeneratorExtension list -> GeneratorConfig -> string -> string -> Async<RunReport>
```

Define the following opaque types in that same contract: `SourceType`, `SourceMember`,
`OutputTarget`, `BindingType`, `AttributeSpec`, `InteropSpec`, `CompanionSpec`, and
`ReplacementSpec`. They represent, respectively, source identity, member identity, an owned
output target, a resolved F# type, an attribute application, accessor interop behavior, a
companion declaration, and an explicit replacement declaration/contract.

Supported operations, split into modules in the contract:

```fsharp
// Semantic
val types: SemanticSnapshot -> SourceType list
val tryFind: string -> string list -> SemanticSnapshot -> SourceType option
val descendants: SourceType -> SemanticSnapshot -> SourceType list // includes root
val properties: SourceType -> SemanticSnapshot -> SourceMember list
val bindingType: SourceMember -> SemanticSnapshot -> BindingType
val declaringType: SourceMember -> SemanticSnapshot -> SourceType
val jsName: SourceMember -> SemanticSnapshot -> string
val isReadOnly: SourceMember -> SemanticSnapshot -> bool
val outputTargets: SourceMember -> SemanticSnapshot -> OutputTarget list

// Edits (each function returns a new batch; all validation is centralized)
val empty: EditBatch
val addAttribute: OutputTarget -> AttributeSpec -> EditBatch -> EditBatch
val replaceInterop: OutputTarget -> InteropSpec -> EditBatch -> EditBatch
val emitCompanion: CompanionSpec -> EditBatch -> EditBatch
val replaceDeclaration: OutputTarget -> ReplacementSpec -> EditBatch -> EditBatch
```

`tryFind` takes canonical package identity and declaration path, so the HTMLElement seed is
`"typescript/lib"`, `["HTMLElement"]` in the DOM profile; verify that mapping in Task 2.
Return deterministic ambiguity diagnostics when a canonical declaration cannot be selected.
Do not expose raw checker integer IDs, textual F# type snippets, or output-path strings as
substitutes for source identity.

The public output builders are frozen after Task 1's probe. Their required capabilities are
precise: named attributes with typed constant arguments and explicit member/getter/setter target;
separate getter/setter interop; marker interfaces with base markers; optional extension modules;
resolved property signatures; and replacement declarations with explicit dependency references.
Reuse the internal type printer through `BindingType`; do not build a second F# type mapper.

## Task 1: Pin the actual consumer shape

**Create:** `tests/fixtures/customization-lab/{package.json,index.d.ts,index.js}`;
`tests/Xantham.Generator.Tests/Customization.test.fs`;
`tests/Xantham.Generator.CompileGate/CustomizationConsumer.fs`.
**Modify:** tests project compile list and `Pipeline.test.fs` lab registration.
**Produces:** a tiny accepted F# companion sample, expected Partas JSX, and the accessor/attribute
builder requirements; it does not modify generator behavior yet.

- [ ] Record baseline `rtk git status --short` and `rtk dotnet fsi build.fsx -- findings`.
- [ ] Create the surgical fixture below and register it using
  `fixtureTests "customization-lab" (handFixture "customization-lab")`.

```typescript
export interface Properties<T> { value: T; readonly stamp: string }
export interface Named { title: string }
export interface Input extends Properties<string>, Named { disabled: boolean }
export interface OddKeys { "aria-label": string; "quote\"key": string }
```

- [ ] Use `tests/.scratch/` for a hand-authored probe of marker interfaces plus optional
  extension properties. Compile a concrete marker implementer with no property bodies; compile
  a separate negative consumer assigning `stamp` and require a readonly/member-setter diagnostic.
- [ ] Compile attribute-driven access through Fable against a JS object with `value`, `stamp`,
  `title`, and unusual keys. Assert readback after setting `value`, unchanged readonly data,
  and exact JS key selection. Run one variant from a compiled binding DLL.
- [ ] Compile a Partas component using a separate generated-style marker and erased extensions,
  following `../Partas.Solid/src/Partas.Solid/HtmlAttributes.fs`. Use the actual plugin and its
  namespace/member-shape rules. Assert generated JSX includes the selected prop exactly once
  and that the component has no abstract property implementations.
- [ ] Save the successful F# and JSX as acceptance fixtures under
  `tests/Xantham.Generator.PartasGate/fixtures/`. If this probe fails, resolve the smallest
  framework adapter/plugin incompatibility before freezing the builders or proceeding.

## Task 2: Project stable semantics for local and referenced types

**Create:** `Customization/Contract.fsi`, `Customization/Contract.fs`,
`Customization/Semantics.fs` under `src/Xantham.Generator/`.
**Modify:** generator fsproj compile order; `Resolve.fs` only where reference pruning removes
required facts; `Customization.test.fs`; add `tests/fixtures/customization-dom-lab/` and register it.
**Consumes:** current Resolve and Shape facts. **Produces:** semantic snapshot and queries above.

- [ ] Add live-compiler tests for a true `HTMLElement` descendant, a grandchild, an alias to the
  true root, an unrelated exported type named HTMLElement, and a structural lookalike.
- [ ] Test `Properties<string>` inheritance, a diamond, and a property override. Expected:
  effective type is string; one inherited property per effective key; declaring identity stays
  available. Readonly and optionality must be preserved independently.
- [ ] Add a referenced-producer test selecting a declaration omitted from ordinary output.
  Missing source metadata must yield a named diagnostic, not an empty match.
- [ ] Run `rtk dotnet fsi build.fsx -- test --quick --filter customization` and observe failing
  expectations before implementing the projection.
- [ ] Implement snapshot projection from existing facts and canonical identities. Query explicit
  heritage transitively with a visited set; keep `ImplementedTypes` separate. Build effective
  properties from checker-resolved instantiations, retaining original JS names and mapped types.
- [ ] Run the same filter; assert stable projected identities across two fresh compiler sessions.
  Keep checker IDs and absolute checkout paths out of serialized output.

## Task 3: Add structured attributes through the supported entry point

**Create:** `Customization/Apply.fs`, `Customization/Output.fs`.
**Modify:** `Pipeline.fs`, `Model.fs`, `Render.fs`, generator fsproj, `Render.test.fs`,
`Customization.test.fs`, `docs/.ai/plans/generator-architecture.md`.
**Consumes:** snapshot, ordered extensions. **Produces:** `generateWith`/`runWith` and validated
attribute edits with correct property/accessor placement.

- [ ] Add tests for an Obsolete attribute on exactly one selected property, a custom attribute
  with a quoted string argument, an exported member materialized by ordering, and empty registration.
  Compare every file and finding in the empty-registration run with ordinary `generate`.
- [ ] Add the exact public entry points and private attribute builder representations. Represent
  constants structurally (string, bool, integral value, type reference, enum value and arrays);
  validate representable arguments and target sites before printing.
- [ ] Add attribute metadata to the output representation and route current generated attributes
  through a common validation/rendering path so conflicting interop is visible to the engine.
  Preserve existing attribute order and spelling for empty registration.
- [ ] Apply edits after exports exist; finalize output before catalog serialization. Use immutable
  original snapshots for every callback. Duplicate extension IDs are errors.
- [ ] Add a sentinel file in a scratch output directory, run an extension that throws, and assert
  the sentinel is unchanged and no generated files were written. Preserve exception context in
  diagnostics while excluding stacks/absolute paths from deterministic manifests.
- [ ] Run focused tests and compile the attributed consumer. Arbitrary custom attribute definitions
  belong in the consumer test assembly; generator core has no reference to them.

## Task 4: Generate property companions

**Modify:** `Customization/Output.fs`, `Customization/Apply.fs`, `Render.fs`,
`Shape/Ordering.fs` or an extracted finalization module, `Customization.test.fs`;
compile/run gate project lists and consumer checks.
**Produces:** marker declarations and extension modules using the shape proven in Task 1.

- [ ] Write a generator test that selects Input, emits an independent marker and property
  extensions, and leaves the original abstract interface unchanged.
- [ ] Add compile tests for an implementer supplying zero property implementations, mutable access,
  inherited `title`, generic substitution, and a failing readonly setter. Assert original binding
  values still cross an ordinary producer/consumer boundary with their original F# type.
- [ ] Implement companion builders using opaque `BindingType` references, explicit marker bases,
  source-linked property specs, and separate getter/setter behavior. Emit no setter for readonly
  properties. A framework can choose erased stubs or attribute-driven direct access.
- [ ] Extract final declaration validation/order so companions enter dependency sorting and name
  collision checks. Reject ambiguous inherited member signatures instead of dropping one.
- [ ] Add the framework-independent read/write checks to the existing RunGate. Link the generated
  companion files explicitly and verify source and DLL consumption modes.
- [ ] Run `rtk dotnet fsi build.fsx -- test --quick --filter customization --run-gate`.

## Task 5: Validate overlapping edits and explicit replacement

**Modify:** `Customization/Apply.fs`, `Customization/Output.fs`, `Customization.test.fs`.
**Produces:** deterministic conflict rules and the replacement escape hatch.

- [ ] Test two additive attributes, duplicate identical attributes, competing interop replacements,
  competing declaration replacements, duplicate companion names, and an edit to a referenced
  declaration. Expected: identical attribute applications coalesce; distinct additions retain
  registration order; conflicts report both extension IDs and source targets; external mutation fails.
- [ ] Treat a declaration replacement and edits to one of its removed members as a conflict.
  Validate getter/setter attributes against existing Fable interop; require `replaceInterop`
  rather than a generic attribute append for known interop-changing attributes.
- [ ] Implement structured replacement with explicit dependency references and preserved binding
  identity. Validate every use site and heritage edge affected by the replacement.
- [ ] Implement raw replacement only as an explicit Escape contract: source text plus declared
  exports/dependencies, compile validation, and rejection from catalog production when its API
  cannot be authenticated. A marker replacement is not assignable evidence for the original TS API.
- [ ] Test a local structured replacement, a dependent type that becomes invalid, and rejected raw
  catalog output. Assert replacement never retains an unqualified Exact finding.
- [ ] Run the focused suite twice and compare outputs and diagnostic ordering byte for byte.

## Task 6: Findings, extension provenance, and catalogs

**Modify:** `Findings.fs`, `Findings.test.fs`, `Render.fs`, `DeclarationCatalog.fs`,
`DeclarationCatalog.test.fs`, `Customization.test.fs`, architecture/type-mapping phase records.
**Produces:** authenticated customized APIs and separately identified companion APIs.

- [ ] Allocate the next unused finding codes at execution time using `FindingCodes.table`;
  retain retired rows. Add cases for customization involvement, omitted member, widening,
  interop change, and replacement. Keep plugin-local diagnostic IDs namespaced by extension ID.
- [ ] Preserve source findings in companions. A successful compilation does not erase Widened or
  Escape findings. Record excluded method/indexer/event members for the initial property adapter.
- [ ] Serialize identities in registration order with canonically sorted configuration keys.
  Include API-contract version and all API-affecting emitted attribute/accessor information in
  canonical hashing. No timestamps, process IDs, checker IDs or machine-local paths.
- [ ] Separate catalog reference authentication from customized producer serialization as needed.
  Test that a companion-only run reuses the unchanged base API, and that customized producer
  declarations carry their own fingerprint. Generated companions need their own stable identity,
  not a second claim to the base TypeScript declaration's canonical identity.
- [ ] Add producer/consumer tests: matching customized producer succeeds end to end; changing
  extension version/configuration or interop invalidates the affected variant; changing only a
  companion leaves the base reference valid; changed source hashes still fail.
- [ ] Test stable bytes across fresh runs and reordered configuration-map construction. Reversing
  extension registration order intentionally changes provenance. Preserve legacy empty-extension
  manifest output where the schema permits; document any deliberate catalog format migration.
- [ ] Run focused customization and declaration-catalog suites. Require a compiled successful
  consumer as positive evidence, alongside each rejection test.

## Task 7: Ship a reproducible Partas example and gate

**Create:** `tools/customization-example/{CustomizationExample.fsproj,Program.fs,PartasDom.fs}`;
`tests/Xantham.Generator.PartasGate/{README.md,PartasGate.fsproj,Consumer.fs,verify.mjs}`.
**Modify:** `build.fsx` and `.claude/rules/build.md` following their stage DSL;
`site/content/xantham-cli/guide/customization.md`; architecture/type-mapping records.
**Consumes:** the supported extension API. **Produces:** working example and automated acceptance.

- [ ] Implement the example executable with explicit F# registration and command arguments for
  input/output paths. The adapter chooses the real HTMLElement root and descendant subset,
  emits independent markers, preserves resolved property types, and chooses Partas erased accessors.
- [ ] Generate `value`, inherited `title`, and readonly `tagName` for the small input-element
  example. Assert that the emitted member inventory matches the selected source inventory and
  that exclusions appear in diagnostics. Do not generate all DOM types during the inner loop.
- [ ] Establish a pinned Partas.Solid source revision/build artifact, recording content hashes if
  the tested local source is uncommitted. Local sibling paths may bootstrap the probe but cannot
  be the CI dependency. Align the consumer with the current Xantham support project and
  Fable.Core 5.2.0; package availability/version is verified at implementation time.
- [ ] Compile Consumer.fs with the real Partas plugin. Compare JSX to Task 1's accepted fixture.
  Render a component in the Partas runtime harness and assert `value`/`title` on the resulting
  input element. Test readonly getter separately against an object supplying that property.
- [ ] Make `verify.mjs` fail if the expected JSX file is missing, then assert the JSX properties
  and runtime behavior. Wire this gate into the existing `--run-gate` build path. Avoid a gate
  that passes by skipping when the sibling checkout is absent.
- [ ] Document extension registration, supported semantic queries, edit conflicts, referenced
  companion generation, fidelity/provenance, and required consumer assembly/source packaging.
  Include the actual generated F# consumer and executable command verified by this gate.

## Task 8: Regression measurement and handoff

**Modify:** generated lab goldens and final documentation only as justified by measurements.

- [ ] Regenerate focused lab goldens, inspect their diffs, and verify compile-gate inclusion.
- [ ] Run the repository's complete evidence set:

```powershell
rtk dotnet build Xantham.slnx
rtk dotnet fsi build.fsx -- test --run-gate
rtk dotnet fsi build.fsx -- findings
rtk git diff --stat
rtk git diff --check
```

- [ ] Compare finding counts to Task 1's baseline. Existing no-extension bindings must remain
  byte-identical except for a separately explained catalog schema migration. Inspect bounded
  representative hunks; follow the fixture rule for unexplained large-package changes.
- [ ] Re-run fslangmcp check and compare the generator public API to its baseline. Verify that
  internal pass types did not enter the new supported signature surface.
- [ ] Report the actual Partas pin, compile result, JSX result, runtime result, finding deltas,
  no-extension compatibility, and any explicitly deferred member kinds. Commit only owned
  changes once the full gate passes; do not include unrelated working-tree edits.

## Plan review

Coverage: attributes (Task 3); inheritance/references (Task 2); companion representation (Task 4);
replacement/conflicts (Task 5); findings/provenance/catalogs (Task 6); real framework acceptance
(Tasks 1 and 7); regression and documentation (Tasks 7 and 8).

The first consumer probe deliberately precedes freezing the output builders. The local Partas
pattern is strong evidence, but the exact generated optional-extension shape has not yet been
compiled with its plugin. Do not report the plan's proposed syntax or runtime behavior as tested.
