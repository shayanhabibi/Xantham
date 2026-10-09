# Brotli Catalogue Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Emit and consume Brotli declaration catalogue files with unchanged compatibility and authentication guarantees.

**Architecture:** An internal transport module owns strict stream decompression, byte limits, and compressed writing. The existing catalogue decoder consumes a streamed JsonDocument. Pipeline compresses only catalogue disk output; generation continues to return text.

**Tech Stack:** F#, .NET 10 System.IO.Compression and System.Text.Json, Expecto, pinned TypeScript 7 compiler, existing Fable 5 compile/run gates.

**Spec:** [Approved design](2026-10-09-brotli-catalogue-design.md).

## Global Constraints

- Keep `GeneratorConfig.DeclarationCatalog: bool`; add explicit compression with default none.
- Boolean JSON configuration and string file references remain supported.
- Object form requires `enabled`; compression accepts `none` or `brotli` and defaults to `none`.
- `.br` suffix selects Brotli case-insensitively; other filenames remain JSON. Never sniff or retry.
- Maximum decoded input is 134,217,728 bytes for both transports, inclusive.
- Reject incomplete, invalid, trailing, and concatenated Brotli streams.
- Do not materialize an intermediate uncompressed file or full JSON string on catalogue reads.
- Keep schema versions, compatibility versions, authentication, inference, and customization semantics unchanged.
- Preserve RenderModel.Files text and actual RunReport output filenames; no alternate-file deletion.
- Use framework compression only; no new dependencies, package discovery, or packing.
- Prefix commands with `rtk`; use `rtk proxy` for arbitrary commands. Scratch files stay under tests/.scratch.
- Read docs/.ai/footguns.md and applicable .claude/rules before changes. Use fresh fslangmcp check and semantic impact tools for F# public changes.
- Do not read large golden source or symbols files. Use hashes and aggregate findings.

## Review Focus

1. Short reads and decoder output-buffer exhaustion must preserve compressed input and make progress (Task 1).
2. A complete JSON value without Brotli's end marker must still fail (Task 1).
3. A configuration record copy must retain compression without changing the inference fingerprint (Tasks 2 and 3).
4. Switching output formats in an existing directory must report only current output and preserve old files (Task 3).
5. Corruption in a later mixed-format reference must fail before any consumer output (Task 3).

## File boundaries and preparation

- Create src/Xantham.Generator/CatalogTransport.fs for stream and disk transport only.
- Create tests/Xantham.Generator.Tests/CatalogTransport.test.fs for decoder and limit tests.
- Modify Model.fs, DeclarationCatalog.fs, Pipeline.fs and generator fsproj for integration.
- Modify src/Xantham.Cli/Schema.fs and regenerate xantham.schema.json for nested options.
- Modify Bootstrap.test.fs / Cli.test.fs for parser and schema contracts, CatalogPortability.test.fs and DeclarationCatalog.test.fs / Customization.test.fs for both-format authentication.
- Modify tools/catalog-portability.fsx, tools/catalog-portability.test.mjs and .github/workflows/catalog-portability.yml for JSON plus Brotli exchange.
- Modify site/content/xantham-cli/guide/configuration.md and dependencies.md, docs/.ai/plans/generator-architecture.md and docs/.ai/footguns.md for durable behavior documentation.

- [ ] At execution time use using-git-worktrees to create .claude/worktrees/brotli-catalogue on the existing agent/brotli-catalogue branch. Main checkout compiler install is borrowed by build.fsx; do not install another compiler. Preserve unrelated untracked files.
- [ ] Read spec, this plan, footguns and build/comments/style/tests/generator-fixtures rules. Run baseline solution build and focused catalogue tests; record existing golden F#/manifest hashes and `build.fsx -- findings` aggregate under tests/.scratch.
- [ ] Run fslangmcp check, then fcs_refactor_impact(kind=signature, symbol=GeneratorConfig) with explicit projectPath; inspect solution-wide callers before adding the field. No public Catalog or RenderModel signature changes.

## Task 1: Strict bounded stream transport

**Files:** New CatalogTransport.fs and CatalogTransport.test.fs; both project compile lists (transport before DeclarationCatalog.fs; tests before Main.fs).

**Interfaces:** Internal `CatalogTransport.readJsonWithLimit : int64 -> string -> JsonDocument`, `readJson : string -> JsonDocument`, and `writeBrotli : string -> string -> unit`. Returned document belongs to the caller. Stream wrappers remain private and synchronous because JsonDocument.Parse(Stream) uses synchronous reads.

- [ ] Write independent fixtures using framework BrotliStream and UTF-8 bytes, with Scratch.directory. Establish JSON/custom-extension and .BR equivalence and small injected limit boundaries:

```fsharp
let json = "{\"value\":\"é\"}"
let bytes = Text.Encoding.UTF8.GetBytes json
File.WriteAllBytes(plain, bytes)
use target = File.Create compressed
use writer = new IO.Compression.BrotliStream(target, IO.Compression.CompressionLevel.SmallestSize)
writer.Write(bytes, 0, bytes.Length)
// Dispose both streams before opening compressed for read.
use document = CatalogTransport.readJsonWithLimit (int64 bytes.Length) plain
Expect.equal (document.RootElement.GetProperty("value").GetString()) "é" "UTF-8 boundary"
Expect.throws (fun () ->
    use _document = CatalogTransport.readJsonWithLimit (int64 bytes.Length - 1L) plain
    ()) "one byte above limit fails"
```

Repeat boundary assertions for compressed input. Include empty and malformed JSON, missing file, garbage bytes, every strict prefix of a small valid compressed fixture, appended byte, and concatenated streams. Error assertions include path and substantive transport reason. Verify files can be reopened exclusively after every failure.

- [ ] Run `rtk dotnet fsi build.fsx -- test --quick --filter "catalog transport"`; confirm missing module/behavior is the expected RED, not infrastructure failure.
- [ ] Implement a private bounded read-only stream. Cap each read request at remaining allowance plus one byte; throw InvalidDataException on actual count exceeding limit. Zero-count reads return zero without consuming or declaring EOF. Unsupported seek/write operations throw NotSupportedException. Dispose owned underlying streams.

```fsharp
[<Literal>]
let MaxJsonBytes = 134217728L

let readJson path = readJsonWithLimit MaxJsonBytes path

let writeBrotli (path: string) (content: string) =
    use file = File.Create path
    use compressed = new IO.Compression.BrotliStream(file, IO.Compression.CompressionLevel.SmallestSize)
    use writer = new StreamWriter(compressed, Text.UTF8Encoding(false))
    writer.Write content
```

- [ ] Implement strict incremental BrotliDecoder stream with bounded input/output buffers and retained input offsets. It must not read compressed data into a whole-file array. Use Decompress's consumed/written counts to advance offsets. Handle statuses explicitly:

```fsharp
match status with
| Buffers.OperationStatus.Done ->
    // Reject unread bytes in the input buffer and any byte subsequently read from the file.
    // Mark completion only after both checks; return produced output normally.
    finishExactlyOneStream ()
| Buffers.OperationStatus.NeedMoreData ->
    // Preserve any unconsumed input; refill. EOF before Done is truncated input.
    refillOrRejectTruncated ()
| Buffers.OperationStatus.DestinationTooSmall ->
    // Return produced bytes; retain compressed remainder and decoder state for next Read.
    retainForNextRead ()
| _ -> raise (InvalidDataException "invalid Brotli stream")
```

The named operations above are private helpers within this stream, not cross-task APIs. Test status transitions through externally observable reads, including a test stream returning one byte per read and a payload larger than the output buffer. Dispose the mutable decoder exactly once.

- [ ] Implement readJsonWithLimit using File.OpenRead, suffix selection, strict decoder, bounded stream, and JsonDocument.Parse. Wrap only file/transport/JSON failures with path and inner exception. Always dispose stream stack. JsonDocument.Parse must read to logical EOF so decoder completion and trailing-byte validation happen before returning; explicitly drain/check if observed parser behavior does not guarantee this.
- [ ] Run focused transport tests GREEN. Confirm no complete-JSON truncation is accepted and test both zero-byte read and short-read behavior. Add focused source comments explaining completion and byte-limit invariants; format production source with Fantomas.
- [ ] Run full `rtk dotnet fsi build.fsx -- test` before coherent task commit, following repository gate rules. Commit transport and tests together.

## Task 2: Configuration and compressed producer output

**Files:** Model.fs, Pipeline.fs, Schema.fs, xantham.schema.json, Bootstrap.test.fs, Cli.test.fs, CatalogTransport.test.fs and CatalogPortability.test.fs.

**Interfaces:** Public `[<RequireQualifiedAccess>] type CatalogCompression = Uncompressed | Brotli`; public `GeneratorConfig.DeclarationCatalogCompression : CatalogCompression`, default Uncompressed. `parseDeclarationCatalog : JsonElement -> bool * CatalogCompression` is private. Existing generation/run signatures stay unchanged.

- [ ] Add RED parser cases for omitted/false/true, object with omitted compression, both named compressions, disabled Brotli, missing/nonboolean enabled, null/numeric options, and unknown/nonstring compression. Use configuration files in Scratch.directory and GeneratorConfig.loadFile:

```fsharp
File.WriteAllText(path, """{"declarationCatalog":{"enabled":true,"compression":"brotli"}}""")
let config = GeneratorConfig.loadFile path
Expect.isTrue config.DeclarationCatalog "emission enabled"
Expect.equal config.DeclarationCatalogCompression CatalogCompression.Brotli "explicit compression"
let copied = { config with ModuleName = Some "Transport.Copy" }
Expect.equal copied.DeclarationCatalogCompression config.DeclarationCatalogCompression "copy preserves output"
```

- [ ] Add RED schema assertions: declarationCatalog has boolean/object oneOf, object required enabled, compression enum none/brotli, and no standalone declarationCatalogCompression property. Adjust reflection parity tests so the two public fields are explicitly accounted for by one JSON property; every other field still needs a key-table mapping.
- [ ] Add RED real small-lab producer output assertions. Run once plain and once Brotli with identical semantic configuration. Decompress using independent framework BrotliStream and compare bytes to plain file. Assert returned filenames, unchanged .fs/manifest bytes, generation still returns a declarations.json text entry, and disabled output emits neither catalogue.
- [ ] Implement enum/record/default/parser. Replace the old declarationCatalog boolean read with the parsed pair and bind both record fields. Object enabled is required, compression defaults Uncompressed; unknown names or wrong types fail with nested option name. Apply existing config policy to extra keys consistently with schema. Add semantic public API change documentation.
- [ ] Special-case DeclarationCatalog schema as a oneOf boolean/object. Explicitly skip DeclarationCatalogCompression as an internal-to-JSON representation field and retain exhaustive reflection validation for all other fields. Generate via `rtk dotnet fsi build.fsx -- generate --only schema`; inspect only expected schema diff.
- [ ] Implement output-boundary mapping and preserve reported actual names:

```fsharp
let outputName name =
    if name = "declarations.json" && config.DeclarationCatalogCompression = CatalogCompression.Brotli then
        name + ".br"
    else name

for name, content in rendered.Files do
    let writtenName = outputName name
    let path = Path.Combine(outDir, writtenName)
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    if writtenName <> name then CatalogTransport.writeBrotli path content
    else File.WriteAllText(path, content, utf8NoBom)
// In RunReport: OutputFiles = rendered.Files |> List.map (fst >> outputName)
```

- [ ] Prove output compression does not enter inferenceProfile: run a consumer with an otherwise identical record whose compression differs and require successful authentication. Do not change profile serialization or compatibility versions.
- [ ] Update configuration docs in this task with both JSON forms and public record field migration. Focused config/CLI/transport/portability tests GREEN; required full test gate before commit. Commit configuration and output as one deliverable.

## Task 3: Shared stream loading and authentication parity

**Files:** DeclarationCatalog.fs, CatalogPortability.test.fs, DeclarationCatalog.test.fs, Customization.test.fs, dependencies.md, generator-architecture.md, footguns.md.

**Interfaces:** Replace only file acquisition inside private load; `use document = CatalogTransport.readJson path`. Existing producer cache, compatibility decoder, loaded model, apply/applyWith, and variants decoder signatures remain unchanged.

- [ ] Add RED real consumer of producer-generated .br; assert canonical producer identities and compile producer+adapter using DeclarationCatalogTests.compileConsumer. Run transport formats through the existing portability tests by enumerating formats, decompressing mutations independently in test helpers, recompressing to the same path, and selecting the matching filename.

```fsharp
for compression in [CatalogCompression.Uncompressed; CatalogCompression.Brotli] do
    let suffix = if compression = CatalogCompression.Brotli then ".br" else ""
    // withProducer accepts compression; producer record selects it explicitly.
    // catalogue path = Path.Combine(root, "declarations.json" + suffix).
    // Tests mutate JSON through independent framework streams, not CatalogTransport.readJson.
    // Preserve each existing assertion and include format in the test name.
    ()
```

- [ ] Extend existing authentication cases in DeclarationCatalog.test.fs and Customization.test.fs to both formats. Keep assertions for source/manifest hashes, conflicting owners, arity, constraints, canonical API, customized variants, stale ownership and adapter dependency chains. Avoid duplicating large fixtures or rewriting goldens. Preserve plain JSON direct serialization tests where transport is not exercised.
- [ ] Add duplicate/malformed root and nested compatibility metadata cases for both transports, legacy exact-fingerprint acceptance/rejection, and mixed-format chains in both directions. Add a valid first reference followed by corrupt .br; assert output directory does not exist after failure.
- [ ] Replace File.ReadAllText + JsonDocument.Parse in load with the shared reader. Preserve CatalogCompatibility.read, JsonSerializer.Deserialize of the root, duplicate checks, variants and all later authentication unchanged. Keep transport exception wrapping outside semantic checks so authentication reasons survive.
- [ ] Add format-switch tests using an existing producer output directory: write JSON then Brotli and reverse; assert old file still exists, only selected current catalogue appears in RunReport, and no unrelated file changes. Reopen rejected payload files exclusively to prove no leaked handles.
- [ ] Run focused catalogue/customization tests GREEN, then full tests. Compare existing golden F#/manifest hashes and finding aggregates to baseline. Update dependency docs with file examples, 128 MiB limit, .br selection, unchanged guards, and stale alternate-output behavior. Update architecture and footguns with stream completion and output/API separation. Commit reader, parity tests and docs together.

## Task 4: Cross-platform exchange and final verification

**Files:** tools/catalog-portability.fsx, tools/catalog-portability.test.mjs, .github/workflows/catalog-portability.yml; this plan's execution evidence.

**Interfaces:** Preserve existing `produce <artifactDir>` and `consume <artifactDir> <scratchDir>` commands. One exchanged artifact contains producer .fs plus both JSON and Brotli payloads, each included in payload.json hashes. Consumer creates separate JSON and Brotli subdirectories and compiles both.

- [ ] Add RED tooling assertions for declarations.json.br artifact hashing, both-format consumer paths, and unchanged scratch-path and source-hash guards. Use `rtk proxy node --test tools/catalog-portability.test.mjs`.
- [ ] Produce JSON normally, then produce Brotli using identical config in a separate tests/.scratch directory and copy the compressed catalogue into the artifact. Check portable metadata through framework decompression for compressed data; keep artifact hashes over actual transferred bytes. No NuGet descriptor is created.
- [ ] Loop consumption over both exact filenames; use separate scratch directories and existing compile function. Require both source and artifact hashes before generating either consumer. Retain opposing Windows/Linux workflow artifact exchange; ensure artifact includes .br and both consumers run on each target OS.
- [ ] Run Node tooling tests GREEN and local Release build. Run produce/consume exchange locally using fresh paths under tests/.scratch; report local evidence as local, not cross-platform evidence.
- [ ] Run `rtk dotnet build Xantham.slnx` and `rtk dotnet fsi build.fsx -- test --run-gate`. Complete production formatting checks. Compare findings aggregates and hashes; inspect unexpected changes rather than updating goldens to mask them. Run `rtk git diff --check`.
- [ ] Commit coherent tooling change after required gates. Use requesting-code-review for a fresh whole-branch review under the selected execution method; resolve Critical/Important findings with tests and rerun affected gates. Record actual commands/results here and remove transient execution ledgers after completion.
- [ ] Before publishing, fetch remote and check PR #111. Once its squash merge is on develop, reconcile only next-wave commits onto that develop in the isolated branch. Do not force-update remote develop or duplicate the portable wave in the next PR. Publishing/push follows the user's applicable authorization; this plan itself does not create a PR.

## Handoff

Recommended execution is native in this session with one fresh final reviewer: the four tasks share the transport interface and catalogue helpers, so sequential implementation keeps those contracts consistent. Existing session preferences selected native execution for the prior wave; confirm this plan before coding and preserve that method if it remains the user's preference.

## Execution evidence

No product implementation or validation results yet. Fill this section with actual RED/GREEN commands, full gate results, unchanged corpus measures, review resolutions, and separately observed cross-platform CI results during execution.
