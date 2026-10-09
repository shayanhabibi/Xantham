# Portable Catalogue Compatibility Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans for native execution, or superpowers:subagent-driven-development if the user selects delegation. Track each checkbox below.

**Goal:** Permit declaration catalogue reuse across equivalent Windows/Linux TypeScript toolchains and Xantham assembly rebuilds while retaining all existing semantic authentication.

**Architecture:** Add an internal compatibility policy and compiler identity discovery module before DeclarationCatalog.fs in compile order. Preserve public records by keeping compatibility metadata in JSON and compiler-session provenance inside Bootstrap. Decode catalogue metadata and variants together, then run the existing declaration authentication and ownership pipeline.

**Tech Stack:** F#/.NET 10, System.Text.Json, System.Diagnostics.Process, ConditionalWeakTable, Expecto, the pinned TypeScript 7 native compiler, GitHub Actions.

**Spec:** [2026-10-09-portable-catalogue-design.md](2026-10-09-portable-catalogue-design.md).

## Global constraints

- All shell commands use `rtk`; use `rtk proxy` for unfiltered passthrough.
- Preserve the public `DeclarationCatalog.Catalog` and `Context` record shapes and existing public function signatures.
- Newly generated catalogues use schema 2; schema 1 retains exact binary fingerprint checks.
- Contract, identity, API, inference, and customization versions start at 1 and require exact equality.
- Portable compiler identity requires exact TypeScript release, lowercase full gitHead, and `Ast.ProtocolVersion` equality.
- A custom/unidentified executable requires binary-hash equality; different identity kinds reject reuse.
- The version probe has a five-second timeout and terminates a timed-out child process.
- All source, owner, API, arity, constraint, inference, and variant checks remain active.
- Scratch stays under `tests/.scratch/`; use `Scratch.directory` in Expecto.
- Consumer compile gates use the repository's pinned Fable.Core 5.2.0 and Core.TS.
- Use fslangmcp `check` before semantic `find`; inspect impact before public signature changes.
- Read the fixture, comment, style, and test rules before their corresponding edits.
- Brotli, NuGet discovery/descriptors, packing helpers, and package adoption are subsequent work.

## Review focus

- A compiler override pointing at another installed package uses that executable's package identity; it cannot borrow the nearest consumer package's identity. Task 2 tests both paths.
- Symlinked package directories preserve a verifiable wrapper/platform pairing, while an unrelated copied executable receives binary identity. Task 2 tests discovery with injected physical-path resolution.
- JSON duplicate critical fields cannot select a weaker policy through parser last-value behavior. Task 1 rejects repeated schema/compatibility fields and repeated fields inside compatibility metadata.
- Customization's preliminary/final catalogue passes reuse the same discovered compiler identity and version probe. Task 3 checks discovery count and variant preservation.
- An exchanged catalogue cannot silently skip its live compiler gate or alter source bytes between Windows/Linux. Task 5 requires artifacts and checks fixture hashes before consumption.

## Files and interfaces

Create `src/Xantham.Generator/CatalogCompatibility.fs` and
`src/Xantham.Generator/CatalogCompiler.fs`, both internal modules. Compile them before
DeclarationCatalog.fs. Add matching `CatalogCompatibility.test.fs` and
`CatalogCompiler.test.fs` before DeclarationCatalog.test.fs in the tests project; tests are
discovered by the existing assembly runner.

The policy uses these types and entry points:

```fsharp
type CompilerIdentity =
    | TypeScriptPackage of version: string * revision: string * astProtocolVersion: uint32
    | Binary of astProtocolVersion: uint32

type Contract =
    {
        ContractVersion: int
        IdentityVersion: int
        ApiVersion: int
        InferenceVersion: int
        CustomizationVersion: int
        Compiler: CompilerIdentity
    }

type Producer =
    {
        Compiler: string
        Generator: string
        InferenceProfile: string
        Contract: Contract
    }

// CatalogCompatibility, internal:
// current : CompilerIdentity -> Contract
// read : path:string -> root:JsonElement -> Contract option
// write : Contract -> JsonNode
// validate : path:string -> schema:int -> expected:Producer ->
//            compilerHash:string -> generatorHash:string -> inferenceProfile:string ->
//            actual:Contract option -> unit
// CatalogCompiler, internal:
// discover : executable:string -> Async<CompilerIdentity>
// Bootstrap, internal:
// compilerPath : Context -> string
// DeclarationCatalog, internal:
// createProducer : Context -> Async<Producer>
// cacheProducer : Async<Producer> -> (unit -> Async<Producer>)
// applyWithProducer takes (unit -> Async<Producer>) followed by the existing applyWith arguments.
```

`read` returns None for schema 1, requires metadata for schema 2, and rejects any other schema.
It checks integer kinds, positive versions, supported identity kinds, required string fields,
valid revisions, and duplicate policy fields. `validate` produces the path/component/expected/
actual diagnostic and keeps legacy message fragments where tests rely on them.

### Task 1: Schema and compatibility policy

**Files:** create CatalogCompatibility.fs and CatalogCompatibility.test.fs; modify both .fsproj
compile lists. This task has no pipeline behavior change.

- [ ] Write policy tests using JSON documents for schema 1 and schema 2, both compiler kinds,
  malformed/missing/duplicate metadata, unsupported schemas, and each mismatched component.
  Include acceptance when portable provenance hashes differ and rejection when binary hashes differ.

```fsharp
testCase "portable identity accepts different producer binaries" <| fun _ ->
    let identity = TypeScriptPackage("7.1.0-dev.20260902.1", String.replicate 40 "a", 8u)
    let expected =
        { Compiler = "consumer-compiler"; Generator = "consumer-generator"
          InferenceProfile = "profile"; Contract = current identity }
    validate "producer.json" 2 expected "producer-compiler" "producer-generator"
        "profile" (Some(current identity))
```

- [ ] Run `rtk dotnet fsi build.fsx -- test --quick --filter "catalog compatibility"` and
  record the intended missing-module/behavior failure.
- [ ] Implement `current`, JSON encoding/decoding, duplicate-field checking, and `validate`.
  Use explicit branches for schema 1 versus schema 2; match each component individually to
  retain actionable diagnostics. Emit compiler.kind as `typescript-package` or `binary`.

```fsharp
match schema, actual with
| 1, _ ->
    requireEqual "compiler" expected.Compiler compilerHash
    requireEqual "generator" expected.Generator generatorHash
| 2, Some contract ->
    compareContract expected.Contract contract
    match expected.Contract.Compiler, contract.Compiler with
    | Binary _, Binary _ -> requireEqual "compiler fingerprint" expected.Compiler compilerHash
    | TypeScriptPackage _, TypeScriptPackage _ -> ()
    | _ -> mismatch "compiler identity kind"
| _ -> unsupportedOrMissingMetadata ()
requireEqual "inference profile" expected.InferenceProfile inferenceProfile
```

  Here `requireEqual`, `compareContract`, `mismatch`, and `unsupportedOrMissingMetadata` are
  private helpers in this task: all raise `declaration catalog: <path>: <component> ...` with
  expected and actual values. Compare all five versions and all compiler identity fields.
- [ ] Re-run the focused suite and check the Generator project semantically.
- [ ] Commit only this task's files as `feat(generator): define portable catalogue compatibility policy`.

### Task 2: Discover the compiler actually launched

**Files:** create CatalogCompiler.fs and CatalogCompiler.test.fs; modify Bootstrap.fs,
Bootstrap.test.fs, and both .fsproj files. Public Bootstrap.start remains unchanged.

- [ ] Add discovery tests over scratch npm installs, using injected process probing and
  physical-path resolution to avoid fake executables. Cover valid metadata, differing wrapper/
  platform releases and gitHeads, dependency disagreement, invalid revision, missing metadata,
  unrelated paths, recognized override paths, symlinks, nonzero probe exit, bad version output,
  and timeout termination. Include a smoke test against the real selected compiler.

```fsharp
testCase "unrelated executable cannot inherit package identity" <| fun _ ->
    use scratch = Scratch.directory "catalog-compiler"
    let executable = Path.Combine(scratch.Path, "custom", "tsc.exe")
    Directory.CreateDirectory(Path.GetDirectoryName executable) |> ignore
    File.WriteAllText(executable, "unidentified compiler")
    let unexpectedProbe _ = async { return failwith "binary discovery must not probe" }
    let identity = discoverWith unexpectedProbe id executable |> Async.RunSynchronously
    Expect.equal identity (Binary Ast.ProtocolVersion) "unidentified executable remains binary"
```

  `discoverWith` is an internal
  test seam in CatalogCompiler: `(string -> Async<string>) -> (string -> string) -> string ->
  Async<CompilerIdentity>`. The probe returns the version output only on a successful exit.
- [ ] Run `rtk dotnet fsi build.fsx -- test --quick --filter "catalog compiler"` and record red.
- [ ] Implement metadata discovery from the executable path and wrapper/platform pairing.
  Validate recognized package names against supported TypeScript platform suffixes; compare exact
  versions and lowercase gitHeads. Missing identity information returns Binary; conflicting
  recognized metadata raises a toolchain diagnostic.
- [ ] Implement the real probe with ProcessStartInfo.ArgumentList.Add("--version"), redirected
  stdout/stderr, asynchronous draining, timeout cancellation, Kill(entireProcessTree=true),
  disposal, and successful-exit/output validation. Add a process-level timeout test using a
  repository scratch helper process rather than a system temp file.
- [ ] Retain the captured executable for every successful Bootstrap.start using a private
  `ConditionalWeakTable<Context, string>`. Register the final Context with the selected path
  before returning it. `compilerPath ctx` returns that path or raises an explicit missing-session-
  provenance error for an unregistered Context. Weak keys allow disposed runs to be collected.

```fsharp
let private compilerPaths = ConditionalWeakTable<Context, string>()
// In start, after constructing the final Context:
compilerPaths.Add(ctx, exe)
return mailbox, ctx
```

- [ ] Verify Bootstrap tests, discovery tests, and Generator `check`. Confirm existing public
  signatures are preserved; run impact analysis if implementation needs a public change.
- [ ] Commit as `feat(generator): capture and identify catalogue compiler toolchains`.

### Task 3: Integrate schema 2 without weakening reuse checks

**Files:** modify DeclarationCatalog.fs, Pipeline.fs, DeclarationCatalog.test.fs,
Customization.test.fs; create `tests/fixtures/catalog-portability-lab/` with package.json,
index.d.ts, xantham.json, and a tracked `node_modules/catalog-portability-owner-lab/` dependency;
register the lab in Pipeline.test.fs.

- [ ] Write the tiny fixture before implementation. The dependency exports
  `export interface Box<T> { readonly value: T; }`; the consumer imports Box and exports
  `export function accept(value: Box<string>): Box<string>;`. Both manifests have stable names
  and versions. The root config ships the dependency and enables declarationCatalog.
- [ ] Add a producer/consumer test that changes only portable provenance hashes in the producer
  JSON, generates the consumer, and uses the existing compileConsumer helper to verify its API.
  Add schema 1 tests by removing metadata and setting schemaVersion=1 on a current producer.
  Test both legacy hash mismatch guards, all portable contract mismatch diagnostics, and absence
  of consumer output after rejection.

```fsharp
let document = JsonNode.Parse(File.ReadAllText catalogPath)
document["compiler"] <- JsonValue.Create("other-platform-fingerprint")
document["generator"] <- JsonValue.Create("equivalent-build-fingerprint")
File.WriteAllText(catalogPath, document.ToJsonString())
// Generate consumer with DeclarationReferences=[catalogPath], then compile producer and consumer.
```

- [ ] Run the focused declaration-catalog suite to establish the expected strict-hash failure.
- [ ] Implement DeclarationCatalog.createProducer using the existing profile function, compiler
  and generator SHA-256 helpers, Bootstrap.compilerPath, and CatalogCompiler.discover. Implement
  cacheProducer as a lazy task factory so the operation executes once when first requested.
- [ ] Add a lazy per-run Producer context in Pipeline after Bootstrap.start, created only when
  catalogues are enabled/referenced. Cache its async discovery result once; both preliminary and
  final customization authentication use it. Avoid `Lazy<Async<_>>` alone, which can rerun the
  async body; force an Async.StartAsTask once and await the cached task.
- [ ] Add the internal DeclarationCatalog.applyWithProducer entry point taking that cached operation.
  Preserve existing apply/applyWith wrappers and signatures; their discovery uses Bootstrap's
  captured path. Catalogue-disabled calls return before evaluating the producer operation.
- [ ] Decode Catalog, Contract, and Variant metadata from one JsonDocument per reference.
  Retain the current private Variant record; remove the separate second file read for variants.
  Validate schema/policy before declaration merging. Leave downstream reuse checks in their
  existing order and emit compatibility metadata together with variants at schema 2.
- [ ] Add a customization regression asserting a single probe/discovery per run and retained
  variants after serialization. Exercise source/manifest/API/arity/constraint/owner failures
  under schema 2 using the existing tests and explicit focused negatives where coverage is absent.
- [ ] Exercise producer→adapter→consumer ownership chaining, including an accepted schema 1 input.
- [ ] Regenerate only the new lab golden using the fixture workflow, inspect its small diff, and
  run catalogue/customization suites and compile gates before committing this coherent behavior.

### Task 4: Migration and contract maintenance

**Files:** modify site/content/xantham-cli/guide/dependencies.md, docs/.ai/footguns.md,
docs/.ai/plans/generator-architecture.md, docs/.ai/README.md, and this execution checklist.

- [ ] Document upgrading consumers before regenerating producers; schema 1 retains strict hashes
  and schema 2 uses explicit contracts. Explain exact TypeScript release/revision compatibility,
  binary fallback for custom toolchains, and unchanged explicit file configuration.
- [ ] Add the durable version-bump contract to footguns and the catalogue phase record. Describe
  identity/API/inference/customization changes that require assessment and retain producer
  fingerprint provenance. Record the implementation date and measured validation, rather than
  pre-claiming cross-platform results.
- [ ] Review documentation links and `rtk git diff --check`. Commit the consumer and durable
  records with the behavior change; retain this plan while work is unlanded under the repo policy.

### Task 5: Real Windows/Linux portability gate and final verification

**Files:** create tools/catalog-portability.fsx and .github/workflows/catalog-portability.yml;
use existing setup action, compileConsumer conventions, and the Task 3 lab.

- [ ] Add a script with `produce <artifactDir>` and `consume <artifactDir> <scratchDir>` modes.
  Reference the built Generator and Wire assemblies from the checkout and use Pipeline.run;
  all outputs live under tests/.scratch. Producer mode writes the tiny binding, declarations.json,
  and a source-byte SHA-256 manifest. Consumer mode verifies all source hashes, loads the foreign
  catalogue, generates a consumer, and builds a scratch consumer project using the repository's
  pinned support assemblies. Fail if the compiler is absent or a generation/compile step fails.
- [ ] Run local produce→consume as the same-platform smoke gate and corrupt the source manifest
  once to verify the gate fails for the intended hash mismatch.
- [ ] Add Windows/Linux producer jobs and opposing consumer jobs. Each uses checkout and setup,
  installs pinned npm dependencies, builds the required projects, uploads/downloads the small lab
  artifact, and invokes the script. Use immutable action pins; resolve an existing download-artifact
  pin `actions/download-artifact@3e5f45b2cfb9172054b4087a40e8e0b5a5461e7c` already used by release.yml.
  Configure LF for tracked source fixture
  checkout, and use recorded hashes to detect any byte changes in artifacts or inputs.
- [ ] Trigger on the same PR/push branches as test.yml and workflow_dispatch. Keep read-only
  contents/actions permissions, bounded job timeouts, required artifact downloads, and both
  direction-specific job names. Run the repository's CI policy tests against the workflow.
- [ ] Run required local gates once after the integrated change:

```powershell
rtk dotnet fsi build.fsx -- test
rtk dotnet build Xantham.slnx
rtk dotnet fsi build.fsx -- test --quick --run-gate
rtk dotnet fsi build.fsx -- findings
rtk git diff --check
rtk git diff --stat
```

- [ ] Compare aggregate findings and golden source hashes with the pre-implementation baseline.
  Catalogue JSON changes and the new lab are expected; explain any other generated F# or finding
  changes using the fixture rules. Stop and report unexplained corpus movement.
- [ ] Perform a whole-branch review through the selected execution skill's workflow, fix verified
  issues, and rerun only the affected checks. Record exact local results and distinguish pending
  remote Windows↔Linux jobs from completed local verification.
- [ ] Commit the CI gate and report the branch, commits, tests, and any outstanding remote evidence.

## Execution readiness

The spec is approved. This plan is ready for user review and execution-method selection. Native
execution is recommended because the policy, compiler discovery, and pipeline integration share
one small sequence of internal interfaces. The current checkout is on develop, with unrelated
untracked user files; an isolated feature worktree is recommended before implementation.
Worktree consent and plan review remain prerequisites of the selected skill workflow.

Before code edits, record the catalogue test baseline and `rtk dotnet fsi build.fsx -- findings`
output in the selected workspace. Run setup through build.fsx; a linked worktree borrows the main
compiler install through tools/workspace.fsx and initializes its own fixture dependencies.
