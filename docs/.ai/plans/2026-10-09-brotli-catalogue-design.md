# Brotli declaration catalogue file transport

Status: written spec approved for implementation planning on 2026-10-09.
Implements the explicit-file Brotli transport slice of
[issue #110](https://github.com/shayanhabibi/Xantham/issues/110).

## Outcome and scope

Producers can emit a compressed declaration catalogue and consumers can reference it using
the existing file-reference configuration. JSON and Brotli carry the same catalogue schema,
compatibility metadata, provenance, declarations, owners, and customization variants. Transport
does not weaken any compatibility or authentication check from the portable catalogue wave.

This wave includes configuration, file output, bounded stream loading, diagnostics, tests,
and documentation. NuGet descriptors, payload checksums in descriptors, project.assets.json
discovery, package packing, build targets, and adoption by binding packages remain later waves.

## Configuration and public interfaces

The existing boolean `declarationCatalog` remains supported: false disables emission and true
emits `declarations.json`. Add the object form:

```json
{
  "declarationCatalog": { "enabled": true, "compression": "brotli" },
  "declarationReferences": ["./producer/declarations.json.br"]
}
```

The object requires `enabled` and accepts `compression` values `none` and `brotli`; omitted
compression means `none`. Unknown compression values and malformed option types produce
configuration errors. With enabled false, neither format is emitted. String references keep
their current path resolution and ordering. Package reference objects are not introduced.

Keep the public GeneratorConfig.DeclarationCatalog boolean and add an explicit compression
setting with a default of none. Do not store configuration in hidden side tables: normal record
copies must preserve the requested output. Existing callers using the defaults plus record
updates retain their behavior; callers constructing every field of the public record must add
the new field. Document this source compatibility impact and inspect its semantic use sites
before implementation.

Keep RenderModel.Files and the generation API's text representation. Catalogue content returned
by generation remains JSON text. Compression belongs to Pipeline's disk-output boundary:
compressed output is named `declarations.json.br`, uses UTF-8 without a BOM, and decompresses
to the same bytes as plain JSON output for that generation. RunReport.OutputFiles reports the
actual filename written. Other generated files retain their existing writing behavior.

Emit only the requested catalogue format. Do not delete a pre-existing alternate-format file
from an output directory; explain that switching formats does not clean old generated outputs.

## Shared reader and transport boundary

An explicit reference whose filename ends in `.br`, compared case-insensitively, selects Brotli.
All other paths retain the existing plain JSON interpretation, preserving references with custom
filenames. Do not sniff content or fall back to JSON when Brotli decoding fails.

Open each reference as a file stream. Feed JSON bytes from that stream, or a Brotli decompression
stream, into one shared JSON decoder. Remove File.ReadAllText from catalogue loading. Do not
create an intermediate uncompressed file or full JSON string. A JsonDocument and the decoded
catalogue model may still be materialized: metadata validation, duplicate-field checks, and
customization variants require the shared structured decoder. Streaming transport does not
promise constant-memory semantic validation.

Preserve the existing decoder's duplicate-field and malformed metadata rejection. Both transports
then follow the same schema 1 strict policy, schema 2 portable policy, source and manifest
authentication, ownership checks, canonical API checks, arity and constraint checks, and variant
validation. Compression does not change schema or compatibility contract versions.

The decompressed JSON limit is 128 MiB (134,217,728 bytes), applied to both transports before
unbounded JSON parsing. This leaves room above the roughly 26 MiB Core.TS catalogue recorded in
the issue. Count actual bytes read, not file length or compression ratio. Exactly the limit is
accepted; an additional byte fails. Keep the limit internal and documented, without a bypass
configuration flag. Expose a smaller internal test limit if needed to test boundaries cheaply.

Reject invalid or incomplete Brotli streams even when a truncated payload happens to contain a
complete JSON value. Reject trailing compressed garbage and concatenated Brotli streams. Verify
the actual end-of-stream behavior of the chosen .NET decoder; do not assume that a zero-byte read
establishes a complete compressed stream. If BrotliStream alone cannot establish completion,
use a focused stream adapter backed by BrotliDecoder's completion status. Keep format selection,
byte limiting, and strict decompression inside an internal transport module rather than the
declaration authentication module.

Dispose file and decompression streams on success and failure. A failed reference must fail
the run before generated output is written. Compression uses the framework's Brotli support,
with no new third-party dependency.

## Diagnostics

Errors identify the reference path and distinguish missing/unreadable files, invalid or truncated
Brotli, decompressed-size overflow, and malformed JSON. Existing compatibility and authentication
errors retain their substantive reason for both formats. Preserve underlying exceptions as inner
exceptions where the existing error model permits it. Never retry failed compressed data as JSON.

## Verification and documentation

Use the existing small catalogue portability lab and focused internal transport tests. Verify:

- Boolean and object configuration defaults, disabled emission, both compression values, invalid
  values/types, and generated configuration schema parity.
- JSON output remains unchanged; Brotli output has the correct filename, reported output path,
  and decompressed UTF-8 bytes. Generation's text API remains usable.
- A real producer and consumer work with either format and compile against the producer bindings.
- Existing catalogue authentication and compatibility rejection cases exercise both formats,
  including schema 1 and schema 2, malformed/duplicate metadata, source and ownership changes,
  canonical API/arity/constraint failures, and customization variants.
- Corrupt data, empty input, truncated Brotli at multiple boundaries, a truncated stream containing
  complete JSON, trailing garbage, concatenated streams, malformed JSON, and missing files fail.
- Both transports enforce the byte limit at the boundary and dispose streams after rejection.
- Mixed JSON/Brotli references and producer/adapter/consumer chaining retain reference ordering
  and conflict detection. Transport failures leave no newly generated consumer output.

Run focused suites during implementation, then the required full repository test/build compile
gates and Fable run gate. Check that existing generated F# and findings remain unchanged; this
wave changes transport, not binding semantics. Extend the cross-platform catalogue exchange to
exercise compressed transport while retaining the JSON path. Report local and CI evidence
separately rather than claiming remote checks have run.

Update the consumer configuration/dependency documentation, generated JSON schema, catalogue
architecture notes, and relevant footguns in the same behavior changes. Explain how to generate
and reference each format, the size limit, text-generation API behavior, and output-directory
format switching. No package-discovery claims belong in these docs yet.

## Alternatives considered

Compressing only at the disk boundary retains the existing text-generation interface and keeps
this slice focused. Extending RenderModel.Files to binary artifacts would change unrelated
generation consumers and is unnecessary for file transport. Keeping text-based catalogue loading
would be simpler but retain an avoidable full JSON string and fail the streaming-read requirement.

## Review boundary

Approval of this written spec permits implementation planning. Product changes start after the
implementation plan is reviewed and its execution method selected. The local next-wave branch
contains the portable wave; reconcile it with remote develop after PR #111's squash merge before
publishing the next PR, preserving next-wave commits and avoiding duplicate portable changes.
