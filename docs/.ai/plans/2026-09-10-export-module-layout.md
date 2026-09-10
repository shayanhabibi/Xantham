# Public-module export layout and signature collision implementation plan

> Dispatch-ready specification, finalized 2026-09-10 following agreement on owner-scoped exports and a final F# signature-collision pass. This turn finalizes planning only; it does not start workers. At execution, use superpowers:executing-plans, or superpowers:subagent-driven-development when delegation is authorized. Read the shared contract and only your assigned task. See `2026-09-10-export-module-dispatch.md` for assignment instructions.

**Goal:** Emit functions and variables in a separate `Exports` type for each public JavaScript module, consolidate proven equivalent duplicate occurrences, retain legal overloads, and give incompatible F# signature collisions distinct names without changing runtime bindings.

**Architecture:** Keep Harvest → Resolve → Shape → Render. Preserve export origin and occurrence provenance through shape, allocate F# module paths once, and render named export containers through existing nested-module machinery. Resolve export collisions after arity repair and before coverage. Public export ownership and canonical type ownership remain separate concepts.

**Tech stack:** F#, Expecto, TypeScript 7 Wire, Fable 5, Fable.Core 5.2.0, generated Xantham.Fable.Core.TS.

**Spec:** The design contract in this document is the specification. Proposed new names and signatures below are implementation contracts, not claims that those declarations already exist.

## Evidence and limits

Inspected on 2026-09-10:

- `Harvest.harvestAmbientModule` already records `FromAmbientModule specifier` and consults `ExportProvenance.reader` for value reachability.
- `Shape.Spec.bindingOf` distinguishes globals, ordinary module exports, and ambient-module imports. This information must survive unchanged.
- `Shape.Exports.shapeExports` collects indexed export members into one list.
- `Shape.Ordering.orderDeclarations` turns that list into a single `FsExports` declaration.
- `Render.renderBody` sends unnamed declarations to the empty nesting path; `FsExports` currently uses that path.
- `Shape.ExportNames.declarationExports` deliberately shares canonical declarations across exports of the same type. Replacing this with per-import type copies would undo existing ownership work.
- `GeneratorConfig.Entry` and `RuntimePackage` explicitly support one selected declaration entry per invocation. Grouping discovered exports does not discover every package subpath.
- Existing ambient-module fixture and Fable RunGate exercise ambient imports, globals, classes, and module-body aliases.
- Workspace semantic check was **unknown**, with four project timeouts. Generator, generator tests, and ordinary compile gate were analyzed with no error diagnostics. This is not a passing whole-solution build. Scoped semantic lookup of `FsExports` found 21 sites in the generator.
- `@types/node` is not in the inspected fixture pin list. Its concrete failing package/version and baseline must be captured during task 1; this plan does not claim a reproduced Node failure.
- Finalization inspection: `Shape.Overloads.dedupeOverloads` currently filters `ExportMembers` with a single owner-insensitive set and drops later collisions with a finding. This must stop before grouping can solve the problem. Preserve its separate interface-member deduplication and literal-discrimination work.
- `Shape.Passes` currently ends with overload deduplication → ordering → arity repair → coverage. Arity repair can change signature types; the new export collision pass belongs after it.
- A fresh generator-project semantic check during finalization was clean (zero errors/warnings, five informational diagnostics). Implementation and runtime verification remain execution tasks.

## Design contract

### Public ownership

An export occurrence is identified by public owner plus exported name, not by original symbol ID alone. One symbol exported from two public modules produces two accessible value surfaces. Multiple overloads within one owner remain overloads within that owner's container.

An internal import does not itself create a public API. A root re-export remains in the root module's `Exports`; its import attribute names the public root, not the declaration's source file. Do not traverse imports to invent containers.

Use these logical owners:

```fsharp
type ExportOwner =
    | EntryModule
    | AmbientModule of specifier: string
    | GlobalScope

type OwnedExportMember =
    { Owner: ExportOwner
      HarvestIndex: int
      ExportName: string
      SourceSymbolId: int
      SignatureOrdinal: int option
      Member: FsExportMember }

type FsExportContainer =
    { Name: string
      Owner: ExportOwner
      Members: OwnedExportMember list }
```

`Name` is a fully allocated dotted name relative to the generated group, normally ending in `Exports` (the documented reserved-name fallback may suffix the leaf). Change the `FsExports` payload to `FsExportContainer`; change `ShapeModel.ExportMembers` to `OwnedExportMember list`. Retain the existing member and binding representations. `ExportName` is the original public name and never changes when `Member.Name` is renamed. `SourceSymbolId` is the harvested origin symbol ID, used only for in-session duplicate proof; `SignatureOrdinal` identifies the overload in the resolved signature list (`None` for values). Neither enters emitted names, findings identities, catalogs, or stable keys. Owner selection uses harvest origin, never parsing generated import strings. Public/internal visibility follows surrounding model conventions; run semantic refactor-impact before changing exposed types.

### F# layout

The configured/generated root module is already the package container. Do not add a duplicate package module around it.

For an illustrative runtime package `node` and root module `Node`:

```fsharp
module rec Node

type Exports =
    // members imported from "node"

module Strict =
    type Exports =
        // members imported from "node/strict"
```

This example describes layout only; it does not assert that these are actual Node built-in specifiers. Use the installed Node declaration strings in regression tests.

- Entry-module exports use root `Exports`.
- Ambient owner equal to the effective entry runtime specifier uses the same logical container as entry exports. Normalize this identity before grouping.
- An ambient specifier beginning with the effective runtime specifier followed by `/` uses the remaining slash-separated segments: `pkg/strict` → `Strict.Exports`.
- Other ambient specifiers use their full path. Split `/` and `:` into hierarchy segments; remove a leading scope marker `@`, and apply existing Pascal-segment naming to each segment. Thus `cloudflare:email` → `Cloudflare.Email.Exports`. Do not erase arbitrary package-name prefixes merely because they resemble the configured F# root.
- Globals retain root `Exports` if there is no entry-runtime owner in that generated group. If both occur, globals use `Globals.Exports`. Their bindings stay global. A shared pure layout function makes this rule explicit rather than depending on traversal order.
- No `AutoOpen` on these modules and no root forwarding aliases. The purpose is distinct qualified surfaces.
- Emit no empty export containers.
- Existing type/interface/class placement remains unchanged in this change. Consumers may use `Strict.Exports.parse` returning a root-declared type. Imported class entrypoints keep their current import behavior.

### Naming collisions

Path normalization is presentation, never identity. `foo-bar`, `foo_bar`, `foo/bar`, different case spellings, and colon-prefixed aliases must not silently merge. Preserve each exact specifier in its import attributes. Alias equivalence is not inferred from equal signatures or equal origin symbols.

Allocate paths against existing declarations and modules before rendering. Existing declaration names win. A module may be shared as a parent, but its leaf `Exports` must not conflict with a declaration already there. Reserve `Exports` in every export-container scope and `Globals` when needed.

For conflicting requested module paths, append `_` plus the first 12 lowercase hex characters of SHA-256 of the exact UTF-8 owner key to the conflicting segment. Extend the digest until unique if necessary. Use tagged owner keys (`ambient:` plus specifier, `entry`, `global`) rather than session IDs or process hashes. Sort conflicting owners ordinally. Root `Exports` conflicting with an existing declaration uses `Exports_<digest>` as the container leaf. Test the unusual fallback explicitly.

Record name changes through a new append-only finding case with an Ergonomic tier and a message containing the exact owner and allocated path. Do not renumber finding unions. Names are deterministic for the same complete input; adding a new colliding owner can rename existing containers. Document that limitation instead of promising cross-version name stability.

### Type identity, qualification, and reporting

Keep `DeclNames`, declaration catalogs, producer/reference ownership, and alias resolution keyed as today. A public value surface is not a second type declaration.

All traversals of `FsExports` must visit each `container.Members` item's `Member`, retaining its occurrence metadata, including dependency grouping, qualification, arity/footer analysis, and coverage. Nested export signatures must use the same scoped reference qualification as named declarations so local modules cannot shadow type names accidentally.

Findings about export members must be distinguishable by public owner plus exported name. Keep session-local symbol IDs out of persistent identities. Do not change the manifest JSON schema merely to achieve qualification: use existing symbol naming fields where sufficient and pin expected output. Declaration catalog contents should remain unchanged unless an existing bug is exposed; unexplained catalog churn fails review.

### Final F# export collision contract

The scope is members of generated `Exports` containers, including callable exports and constructor helpers placed there. Existing instance-method/interface/class-static overload behavior is unchanged; broadening that policy requires a separate design because renaming abstract members has different runtime-binding requirements.

**Pass order:** keep literal-discrimination synthesis and existing non-export overload handling in `Overloads.dedupeOverloads`; remove only its export filtering. Keep ordering and arity repair, then add `ExportCollisions.resolveExportCollisions`, then coverage. The final pass consumes `model.Decls`, not the now-cleared `ExportMembers`. No later pass may rewrite export signature types without rerunning collision resolution. Do not add a second early export deduper.

**Resolve these cases in order, inside each container only:**

1. Repeated occurrence: consolidate only if normalized owner, original exported name, source symbol ID, signature ordinal, exact binding, full mapped signature, and mutability agree. Keep the earliest occurrence in existing stable harvest order. Preserve distinct documentation/tag blocks in stable first-seen order and keep all prior loss findings. Emit an Exact consolidation finding, so coverage does not mistake consolidation for an unexplained drop.
2. Legal overloads: retain unchanged when their compiled parameter signatures are distinct and focused consumer call probes can select them. Do not rename every overload of a function merely because one pair conflicts.
3. Colliding or ambiguous mapped overloads: retain every distinct candidate under different F# names. This includes return-only differences, erased constraints, mapped literals that still collapse, different declarations mapped to the same signature, method/property name conflicts, and optional/rest overloads whose overlapping call forms cannot be selected reliably. Distinct original signatures are not proven equivalent merely because their mapped return types also agree.
4. Widening: introduce **no additional widening in this pass**. Renaming supplies access without further type loss. Existing mapping widenings retain their original findings; a rename finding is not a claim that the widened signature became Exact. Unioning return types, dropping candidates, or introducing an `obj` fallback is outside this implementation contract.

**Signature comparison:** inspect emitted F# semantics, not `FsParam` record equality. Alpha-normalize method type variables by position and include generic arity; ignore parameter names, return type, generic constraints, and attributes that do not distinguish CLR signatures. Normalize aliases recursively, including type-argument substitution for applied generic abbreviations; traverse tuples, arrays, options, functions, delegates, branded types, and nested applications. Preserve distinctions the compiler actually retains. Optional parameters use the type the renderer emits; `ParamArray` uses its emitted array type, not a distinct signature because the Rest flag differs. Treat zero-argument syntax according to emitted methods. Compile probes pin each of these decisions; do not claim the existing limited `signatureKey` is sufficient. Alias cycles must terminate with a stable symbolic cycle marker, not loop or silently normalize to `obj`.

Use two comparisons: a parameter/compiled-name key for declaration collisions, and full alpha-normalized mapped semantics (result, constraints, defaults/optionality, parameter names for named calls, mutability and binding) plus provenance for duplicate proof. An undecidable duplicate is retained and renamed, never discarded. For optional/rest ambiguity, conservatively rename connected overload sets with overlapping callable arities and compatible shared parameter prefixes; probe omitted arguments, empty rest calls, and named arguments. Arbitrary subtype/inference ambiguity across otherwise legal overloads is not solved here and must not trigger a rewrite of the type checker.

**Rename allocation:** preserve the earliest member's original F# name in a conflicting set; allocate later candidates as `<name>_Overload2`, `<name>_Overload3`, and so on in existing stable harvest/signature order. Skip names already occupied by any original member in the container, including properties, emitted accessors (`get_`/`set_`), and other generated helpers. Allocate against the whole container, then recheck the final set. Preserve legal overload sharing of the base name outside the collision set. Numbers are intentionally mechanical: this phase does not invent semantic labels from widened types. They are deterministic for the same source but may shift when overloads are inserted.

Renaming changes only `Member.Name`. Keep import selector, specifier, constructor emit, GlobalName, setter target, docs and type signatures. Audit renderers for any place they derive a runtime property from the F# name, especially mutable globals; decouple that use to the retained binding/original export name. An identifier rename alone does not repair lost literal constraints.

Append `ExportOccurrenceConsolidated` (Exact) and `ExportMemberRenamed` (Ergonomic) cases to the existing `DedupeOverloads` finding union, with message arms and snapshot updates. Preserve all old case positions and old codes; old export-drop codes remain defined even if unused by the new export path. Include owner, original export name, final member name and reason in findings. Distinguish return-only, constraint/alias collapse, ambiguous call form, and member-kind collision in messages. Manifest entries for renamed overloads must be individually attributable; tests use qualified container/member names and verify prior Widened/Escape evidence is retained.

### Scope boundaries

Included: functions, constants, variables, callable exports, namespace-object values, default value exports, `export =` value surfaces already harvested, type-only exclusion, exact import paths, deterministic container naming, duplicate proof, final export collision resolution, compile/runtime coverage.

Excluded: relocating all types and classes, automatic package `exports` map enumeration, discovery of ambient augmentations currently outside harvest coverage, compiler/Wire changes, alias-canonicalization configuration, compatibility forwarding APIs, changing instance/interface/class-static overload policies, extra widening, fixing all unrelated Node mapping failures. Report a discovered harvest gap separately; do not expand this patch silently.

## Benefits, costs, and alternatives

Recommended: owner-aware value containers. Prevents cross-module signature conflicts, improves discovery, preserves public import semantics, and concentrates layout in one pure module. Cost: breaking qualified F# value names, extra qualification, and careful renderer/grouping changes.

Alternative: rename conflicting root members only. Smaller apparent diff, but import paths remain hidden and names depend on unrelated exports. Reject as the primary design; use renaming only for residual collisions inside an owner's container.

Alternative: move every declaration into its public module. More visually uniform, but a type re-exported through several modules has one identity and several public paths. This broadens the work to aliases, class statics, recursive references, dependency naming, and catalogs. Defer until there is a separate requirement for type relocation.

The recommended design intentionally permits root `Exports` for actual root exports. Eliminating those would move an ambiguity rather than resolve it.

## Execution rules and context budget

- Read repository AGENTS.md, the dispatch brief's shared rules, and the specific contract sections assigned to your task. Do not load the chat, this entire plan, or every prior task report. The coordinator reads the whole specification once.
- Use RTK for commands. Use fslangmcp for semantic navigation, check first, and pass explicit project paths. Treat incomplete checks and negative lookups cautiously.
- Start with a targeted file outline or named symbol, then read its implementation. Do not load generated Wire/AST files or the entire Node golden.
- Coordinator owns interface decisions, shared model changes, integration, and full-suite runs. Workers do not improvise alternate owner identities or naming policies.
- Aim for a task packet under 2,000 tokens, an initial source working set under 8,000 tokens, and a completion report under 400 words. These are working limits, not permission to skip needed evidence. Escalate a concrete interface question before spending a second broad exploration pass.
- Handoff: commit SHA, owned files, produced interfaces, commands and results, remaining failures. Never return raw logs or a narrative of every tool call.
- Implement locally. No fetch, push, PR, or master merge. The integration branch must be specified before landing. Worktree jobs commit their work and keep their branch refs.
- In worktrees borrow the compiler through the build scripts; do not root-install npm. Install pinned fixtures locally through the fixture script. A skipped live compiler suite is a failed verification.

## Task 1 — establish the regression and baseline

**Read:** `tests/fixtures/ambient-module-lab/index.d.ts`, `tests/Xantham.Generator.Tests/Pipeline.test.fs` fixture registration, `Render.test.fs` existing export tests, RunGate `register.mjs`, Node binding README/config. No generator implementation edits.

**Own:** new `tests/fixtures/export-layout-lab/` package; focused tests in new `tests/Xantham.Generator.Tests/ExportLayout.test.fs`; test-project registration. Leave broad golden and RunGate changes to task 6.

- [ ] Create a global-script declaration fixture with ambient `layout-lab` and `layout-lab/strict`, both exporting `check(value: string): string` and `mode: string`. Add a global `sharedFlag: boolean` and a shared interface used by both modules.
- [ ] Add a third owner that re-exports `check` under a different name, plus `export type` of a value-bearing declaration. Assert that type-only export does not become a member.
- [ ] Add same-owner overloads `convert(value: string): string` and `convert(value: number): number` (remain legal), plus `pick(value: string): string` and `pick(value: string): number` (require separate F# names). Create runtime-friendly collapsed-literal overloads with distinct results, and record current export-drop findings as baseline. Synthetic shape models, not invented TypeScript syntax, test impossible TS declaration combinations such as property/method collision.
- [ ] Use existing Pipeline test helpers to assert qualified container names, exact import strings, and absence of duplicate root members. Check the failure is the intended old layout, not fixture discovery or compiler absence.
- [ ] Record the actual Node input/version/config and smallest colliding pair of imports from the user's generated input. If unavailable locally, use the deterministic lab regression as the acceptance fixture and report Node validation as pending input; do not choose a random latest package version.
- [ ] Commit the regression with its known failure clearly recorded. This task runs on the feature branch, not the integration branch.

**Produces:** reproducible failing lab, test selectors from the actual Expecto list, concise baseline note. No interface choices delegated to this task.

## Task 2 — model ownership and pure path allocation

**Read:** Model export representations; `Shape/Spec.fs` binding/name helpers; `Shape/ExportNames.fs`; existing naming helpers. Use semantic impact for `FsExports` and `ExportMembers` before edits.

**Own:** `Model.fs`, new `ExportLayout.fs`, generator project compile order, new pure layout tests in `ExportLayout.test.fs`. Coordinate test-file ownership after task 1 commits.

**Produces:** the three proposed types above and these pure functions in module `ExportLayout`:

```fsharp
val ownerOf : runtimePackage: string -> origin: ExportOrigin -> ExportOwner
val allocate : runtimePackage: string -> declaredNames: string list -> owners: ExportOwner list -> Map<ExportOwner, string list>
```

`ExportOrigin` is the existing union containing `FromModule`, `FromGlobal`, `FromAmbientModule`; do not invent a second origin union. `allocate` returns container module segments, excluding the `Exports` leaf; additionally allocate the leaf at container construction when it conflicts with declarations. Keep leaf naming in the same module, exposed as `containerName : declaredNames: string list -> owner: ExportOwner -> path: string list -> string`. Expose `preferredPath : runtimePackage: string -> hasEntryOwner: bool -> owner: ExportOwner -> string list` for comparing requested paths with allocations when emitting collision findings.

- [ ] Pin pure tests for root, child, unrelated ambient, scoped package, colon path, global-only, mixed global/module, reserved declaration, and normalization collisions. Reverse owner input order and require equal maps.
- [ ] Implement identity normalization and naming exactly as the contract. Test equal effective runtime/ambient owners normalize before allocation.
- [ ] Migrate every model constructor and union match mechanically so the solution type-checks. Preserve old output temporarily by using root container names until task 3; do not implement grouping in renderer. Populate `ExportName`, `SourceSymbolId`, and `SignatureOrdinal` at shaping sites now, retaining the provenance through every later copy-update. Test two aliases to one source overload and two different overload ordinals.
- [ ] Add append-only naming finding and snapshot if the allocator exposes a collision. Keep finding production separate from the pure returned path map; caller compares requested and allocated names through a helper in this module.
- [ ] Run focused tests and semantic check. Commit.

**Review gate:** coordinator freezes actual signatures and supplies only this contract delta to tasks 3 and 4. Do not run those tasks against guessed model types.

## Task 3 — shape one container per public owner

**Depends on:** task 2. **Read/own:** `Shape/Exports.fs`, `Shape/Ordering.fs`, export-filter section of `Shape/Overloads.fs`; scoped traversal updates in Shape; relevant `Shape.test.fs` assertions.

- [ ] Replace indexed tuples with `OwnedExportMember` using `ownerOf` on each harvest origin. Keep `bindingOf`, `HasValueExport`, docs, tags, mutability, overloads, and type references intact.
- [ ] Remove the `seenExports`/`List.filter` export-drop branch from `Overloads.dedupeOverloads`. Preserve literal declarations and the separate interface deduplication logic. Assert both colliding candidates survive through ordering and arity repair; do not silently retain only the first while reporting successful grouping.
- [ ] Group by normalized owner in ordering; allocate names once against declared names; retain harvest ordering within each container. Append nonempty containers in ordinal allocated-path order after ordinary declarations.
- [ ] Test same name/signature in two owners yields two containers, multiple overloads in one owner stay together, a shared symbol exported from two owners is not deduplicated, and entry re-exports remain entry-owned.
- [ ] Verify default, `export =`, global writable value, namespace-object value, and type-only behavior using existing helpers. Qualify emitted member findings by owner without changing fidelity grading.
- [ ] Run focused shape tests; commit. Existing old-layout goldens may still fail until task 6; list only expected cases. Intermediate duplicate signatures in a container are expected until task 5, and must not reach the integration branch prematurely.

**Produces:** complete owned `FsExports` containers ready for rendering; no new compiler calls.

## Task 4 — render nested containers and preserve grouping

**Depends on:** task 2 interface freeze; may run alongside task 3 in an isolated worktree. **Read/own:** `Render.fs`, `Render.test.fs`; use semantic FsExports sites to inspect every traversal. Do not edit Shape files.

- [ ] Treat container `Name` as a declaration name for nesting and leaf replacement; render the allocated leaf instead of hardcoded `Exports`.
- [ ] Route through existing `nestedBlocks`; ensure one block can contain both parent Exports and child module blocks without reopening invalid F# modules.
- [ ] Apply foreign and scoped qualification to all container member signatures. Preserve owner through grouping even when member types belong to another shipped package.
- [ ] Test synthetic owned containers directly, independent of task 3: root and child, siblings with identical methods, reserved name collision, cross-group return type, nested shadowing, footer union arity, empty container omission.
- [ ] Assert exact import selector and specifier strings in emitted output. Do not replace named imports with a namespace import merely to implement nesting.
- [ ] Render a synthetic member whose `Name` differs from its retained public name and Binding. Cover named/default imports, constructor helper, global function, and a settable global. Prove JavaScript target spelling is derived from binding metadata even after a F# rename. Include property accessor names in collision probe output for task 5.
- [ ] Run renderer tests and compile a focused generated sample against consumer dependencies. Commit.

**Produces:** owner-aware nested output from synthetic model input. Report any shared-model correction to the coordinator instead of editing it concurrently.

## Task 5 — final signature comparison, consolidation, and renaming

**Depends on:** task 3 and task 2 frozen types. Runs alongside task 4 without shared writes. **Read:** final collision contract; `Shape/Overloads.fs` existing normalization/literal behavior; `Shape/Arity.fs` late transformations; renderer parameter spelling identified by task 4. **Own:** new `Shape/ExportCollisions.fs`, `Shape/Passes.fs`, generator compile registration, `Findings.fs`/`Findings.test.fs` after task 2, new `ExportCollisions.test.fs` and its test registration. Do not change renderer or instance-member policy.

**Interface:** `ExportCollisions.resolveExportCollisions : Pass<ShapeModel>`. Pure implementation consumes repaired `FsExports` declarations and returns the same model with reconciled containers plus findings. No Wire requests, name rendering, filesystem access, or model fields added by this worker.

- [ ] Write compiler-backed focused probes using existing consumer dependencies: aliases including generic applications; alpha-renamed generic parameters; different generic arity; constraints-only and return-only differences; optional versus explicit option; rest versus array; zero arguments; property/method/accessor collisions. Assert the intended F# compiler outcome before encoding a comparison rule. Keep probes small and record the actual test selectors.
- [ ] Implement total structured canonicalization over `FsTypeRef` and alias substitution with cycle detection. Use structural keys, not `.ToString()` or hash equality as duplicate proof. Write tests for nested generic aliases and recursive aliases.
- [ ] Implement provenance-backed duplicate consolidation. Same origin/overload/binding and full mapped signature consolidates; differing owner, exported alias name, ordinal, result, constraint, parameter-name contract, mutability, or binding does not. Merge documentation deterministically and keep prior findings.
- [ ] Implement collision sets and stable suffix allocation from the contract. Reserve all original member/accessor/helper names before assigning suffixes. Tests include a user export already named `pick_Overload2`, three-way collisions, legal overloads adjacent to a conflicting pair, and method/property conflict.
- [ ] Test omission/empty-rest/named-call ambiguity. When canonicalization cannot prove distinct declarations safe, retain each under a distinct name; never drop an unproven duplicate.
- [ ] Add the two append-only finding cases and snapshots. Assert consolidation, renaming, and retained widening are all attributable; do not emit the old export-drop finding for retained candidates.
- [ ] Register after `Arity.repairArity` and before `Coverage.auditCoverage`. Regression: two different pre-repair parameter references become the same repaired type, then receive distinct names. Test pass idempotence: a second run does not add suffixes or duplicate rename findings.
- [ ] Run focused pass tests and sample compilation. Commit. Send renderer any required binding invariant as a concise contract clarification, without editing its files.

**Produces:** no unresolved known F# declaration collisions in export containers, proven duplicates consolidated, legal overloads retained, all incompatible candidates callable through distinct F# names. The final runtime acceptance belongs to task 6.

## Task 6 — end-to-end, runtime, and migration gates

**Depends on:** tasks 4 and 5 integrated. **Read/own:** Pipeline regression, affected goldens, CompileGate project links, RunGate project links and `register.mjs`, new RunGate `ExportLayout.fs`, affected existing RunGate call sites, consumer migration documentation and architecture phase record.

- [ ] Give the lab runtimes different results: root `check("x")` returns `"root:x"`, strict returns `"strict:x"`; `mode` also differs. Register exact specifiers in the existing resolver pattern. Both functions must execute in one Fable program.
- [ ] Add consumer calls to both qualified Exports types. Assert both values and calls resolve to their own runtime. Include shared type arguments/returns so relocation mistakes fail compilation.
- [ ] Call both retained legal overloads and every renamed member of a collapsed-literal pair with matching runtime arguments; assert distinct results and identical original JavaScript selector. For return-only overloads, compile both typed F# entrypoints and inspect emitted target identity, but do not claim one runtime input can satisfy contradictory return promises. Add a renamed mutable-global getter/setter runtime check to prove assignments retain the original target.
- [ ] Assert duplicate consolidation preserves one callable member, documentation, and findings; renamed candidates have distinct qualified manifest identities. Assert existing instance/interface overload goldens change only for intentional incidental name qualification, not a new overload policy.
- [ ] Refresh only affected goldens through the existing update mechanism (`XANTHAM_UPDATE_GOLDEN=1`), review diff, and unset the variable before verification. No handwritten generated fixes.
- [ ] Update existing ambient-module RunGate calls for the new module paths. Confirm global calls retain global behavior.
- [ ] Run `rtk dotnet build Xantham.slnx` and `rtk dotnet fsi build.fsx -- test`. In the full pipeline verify live suites ran and Fable runtime checks completed. Capture pre-existing failures separately; do not label a failed full pipeline passing.
- [ ] Generate Node using the captured input/config if available. Compare the selected collision pair and import paths, then compile its output. Existing unrelated Node failures must be listed; the lab compile/runtime gates remain independently required.
- [ ] Compare declaration catalogs and reference fixtures. Expected change is value-container paths; unexplained shared-type identity changes block acceptance.
- [ ] Update `docs/.ai/plans/generator-architecture.md` phase record and add consumer migration notes under `docs/`: before/after qualified calls, root re-export retention, unchanged type locations, path collision behavior, one-entry discovery scope, `_OverloadN` names, stability limits, preserved literal/constraint losses, and why no extra widening is performed.
- [ ] Commit the integrated result with reasons, chosen approach, deferred full-type relocation, verification, and remaining Node limitations. Land only on the user-specified local integration branch.

## Acceptance and distribution

Required: lab fails before the change; passes after it; identical methods and variables coexist under distinct owner paths; generated imports retain exact specifiers; root re-exports survive; globals remain globals; type-only paths produce no runtime member; deterministic collision handling; unchanged shared declaration identity; proven equivalent occurrences consolidate; distinguishable overloads retain their names; incompatible candidates are retained under distinct names; collisions introduced by arity repair are resolved; no extra widening or silent drops; findings preserve prior losses; consumer compile gate and Fable runtime gate pass.

Dependency graph: 1 → 2; then 3 → 5 on one lane, and 4 on a second lane; join 4 + 5 → 6. Maximum useful concurrency is two workers. Task 5 starts only after task 3 commits and does not edit renderer files. Tasks 2 and 5 own shared project/finding files at different times. Do not distribute by package or individual FsExports match: that duplicates context and causes shared-file conflicts.

For lower-capacity models, split each task into the checkboxes above inside the same focused session. Use a stronger coordinator for the interface freeze, collision-policy review, and final integration. A worker must stop with the precise unresolved symbol or failing invariant when the frozen contract is insufficient; it must not redesign the pipeline.
