# Declaration catalog handover — 2026-09-06

The opt-in declaration catalog stage passed the composed repository gate. It preserves
same-source type identity across separately generated entries when provenance and the canonical
F# API agree. This is a bounded generator change; complete CloudEdge SDK identity is unfinished.

## Contract and implementation

`declarationCatalog: true` writes `declarations.json`. `declarationReferences` accepts producer
catalog paths relative to the package input directory or absolute paths. Catalogs retain
inherited ownership; the current owner's dependencies contain actually reused producers.
Compile according to the ordered owner DAG. Runtime/module/entry names can differ.

Resolve retains actual symbol declaration handles and concrete alias arguments. Identity uses
the complete normalized declaration-handle set, package versions, generic argument positions,
and stable anonymous/synthetic roles. Structural signature keys include method generic bounds
and defaults. No checker IDs are persisted. Package manifests, source inventories, per-type
source closures, compiler/generator binary fingerprints, inference profiles, arity, constraints,
and canonical F# member APIs are checked before reuse. Nested versions of one package remain
separate inputs. Unsuppressed TS2307/TS2688 diagnostics refuse catalog generation; TypeScript's
suppression directives remain effective.

Private generic aliases now apply their declared alias parameters at reference positions.
Contextual generic argument bounds do not alter a declaration's source closure. Each canonical
F# declaration's constraints and member surface are checked independently. This also improves
ordinary SolidJS resource aliases and Workers Pages/workflow aliases previously widened to obj.
Their goldens were regenerated and reviewed.

A public class whose instance type was already owned retains a type alias plus a separately
imported constructor/static value. Its helper interface preserves overloaded methods, generic
methods, mutable properties, and constructor signatures. The constructor object receives its
actual source declaration identity. Render owns emission and qualification of FsAbbrevDecl.Value.

## Initial catalog validation (`452c2a4`)

- Declaration catalog tests: 16 cases, including compiling the root/adapter/next consumer,
  rejecting a Left argument to Callback<Right>, private constrained generic imports, static
  overloads/accessors/constructor use, nested package versions, and provenance conflict cases.
- Renderer tests: 24 cases, including the four imported alias value binding variants.
- Private generic alias reference unit regression added to Shape.test.fs.
- New declaration-identity-lab golden/determinism pair passed.
- SolidJS and Workers Types golden/determinism pairs passed and diffs reviewed: generic alias
  parameters now reach previously obj-typed positions, with corresponding finding reductions.
- Final `dotnet build Xantham.slnx`: zero warnings and errors.
- Final `dotnet fsi build.fsx -- test`: 658 generator tests, 90 wire tests, and 327 Fable
  runtime checks passed. The one wire test skipped checks the default executable location;
  this run explicitly selected the pinned native compiler below. Logs:
  `/tmp/clef-xantham-evaluation-build.log` and `/tmp/clef-xantham-evaluation-test.log`.

The pinned native compiler used for these probes was
`/tmp/clef-cloudflare-xantham-20260906/node_modules/@typescript/typescript-linux-x64/lib/tsc`,
version 7.1.0-dev.20260902.1. Logs and repros are under
`/tmp/clef-cloudedge-sdk-probes-20260906`; independent review repros are under
`/tmp/xantham-catalog-review`.

## Explicit remaining boundaries

Inference profiles must match exactly, including lib/types and group dispositions. Worker and
DOM profiles are intentionally not combined. Cross-profile shared ownership is unfinished.

Full Agents 0.22.0 root/MCP catalog composition remains unfinished. The initial probe refused
MCP reuse because Agent connection parameters and WSMessage had different F# shapes between
entries. The closing alias-recovery change below preserves root connection parameters as
`Connection<obj>`, but catalog ownership still needs to distinguish a generic declaration from
its concrete applications. Named aliases versus equivalent inline forms also need a principled
API comparison. Keep the guards while these cases are resolved. Earlier root/MCP compile
evidence predates the full API guard and does not certify complete SDK identity.

Regenerate all producer and consumer catalogs with the same final build; a Debug/Release or
binary change correctly invalidates older catalogs. The root/MCP scratch output directory can
contain an older MCP file after refusal, so its presence is not successful current generation.

## Closing alias recovery

The final slice preserves compiler-reported alias arguments before attempting structural
recovery. When an imported alias has no argument metadata, recovery is limited to two forms
already identified by the compiler as the same alias. It requires a consistent binding for
every parameter. A transformed fragment can supply no binding; contradictory bindings and
unrecovered parameters still refuse recovery. A default `unknown` argument can therefore
remain `Connection<obj>` when it absorbs part of a callback union.

`ShapeModel.AliasApplications` records an application's declaration owner explicitly. Interface
and alias declaration passes exclude these reference sites, preserving the generic declaration
independently of compiler ID ordering. The conditional-alias golden now exposes
`condSeed: CondNode<float>` while retaining `CondNode<'T>`.

The `default-intersection-lab` fixture covers local and imported default arguments. Its compiled
consumer passes `Connection<obj>` callbacks to both generated Agent forms. Unit tests also
cover contradictory and missing bindings, plus application IDs ordered before or after their
declaration. The top-level README and documentation index link the entry, ambient provider,
and catalog contracts for evaluation.

This is the closing scope for Shayan's handoff. Further catalog ownership and cross-profile
work belongs to a later change, with the remaining boundaries above retained for review.
