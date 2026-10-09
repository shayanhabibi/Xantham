# Portable declaration catalogue compatibility

Status: proposed for written-spec review. Implements the compatibility slice of
[issue #110](https://github.com/shayanhabibi/Xantham/issues/110).

## Outcome and scope

Bindings generated with equivalent supported Windows and Linux toolchains can reuse each
other's declaration catalogues. Rebuilding Xantham without changing its compatibility contract
also permits reuse. Existing declaration, source, ownership, F# API, arity, constraint,
inference, and customization validation continues to apply.

This slice adds portable validation to explicit JSON file references. Brotli transport,
package descriptors, project.assets.json discovery, NuGet owner mapping, packing targets,
and adoption by Core.TS are separate slices. Passing this slice does not complete issue #110.

The user selected portable compatibility first and approved the design direction on
2026-10-09. This document defines the concrete contract for review before implementation.

## Existing behavior

`DeclarationCatalog.Catalog` is a public record. Schema 1 carries SHA-256 fingerprints of
the native compiler executable and generator assembly, plus a configuration-derived inference
profile. `DeclarationCatalog.load` requires equality of all three before reuse.

`applyWith` subsequently authenticates sources and manifests, merges declarations and owner
graphs, checks arity, constraints, and canonical F# APIs, and validates customization variants.
Variants are JSON metadata outside the public Catalog record. Canonical API printing is already
explicit and independent of F# pretty-printer output.

`Tsc.locate` selects an existing `XANTHAM_TSGO_EXE` override before walking npm install roots.
The root TypeScript pin has matching wrapper/platform package versions and gitHead values.
`Ast.ProtocolVersion` is the authority for binary AST protocol compatibility.

## Format and migration

Keep the public `Catalog` record's fields and signatures unchanged. Add internal metadata types
and JSON encoding/decoding for a required `compatibility` object in schema 2. Keep `variants`
alongside it. Retain the existing `compiler` and `generator` hashes as producer provenance.

All newly generated catalogues use schema 2. Schema 1 loading keeps its exact compiler hash,
generator hash, and inference checks. Schema 1 catalogues gain portability by regenerating their
producer with the new generator. Old generators reject schema 2 through their existing schema
guard; upgrade the consumer before regenerating the producer.

The proposed portable metadata is:

```json
{
  "schemaVersion": 2,
  "compatibility": {
    "contractVersion": 1,
    "identityVersion": 1,
    "apiVersion": 1,
    "inferenceVersion": 1,
    "customizationVersion": 1,
    "compiler": {
      "kind": "typescript-package",
      "version": "7.1.0-dev.20260902.1",
      "revision": "43a90f4c105bc9db7cb7aa299beddafbabe1d23e",
      "astProtocolVersion": 8
    }
  }
}
```

The TypeScript identity above illustrates the current install, rather than pinning the contract
to that release forever. These fields are serialized with stable camelCase names.

`contractVersion` versions metadata interpretation and compatibility decisions.
`identityVersion` covers normalized handles, source ownership/closure, roles, type arguments,
literal identities, and nullable alias identity rules. `apiVersion` covers canonical constraint
and F# API encoding, normalization, ownership redirects, and emitted Fable 5 binding semantics.
`inferenceVersion` covers the
inference profile's meaning and normalization. `customizationVersion` covers variant API and
extension-profile interpretation.

Version 1 describes the repository's current rules. Every field requires exact equality in this
initial implementation. Accepting a range or translating contracts requires a subsequent explicit
design and tests. An unknown version fails even when producer hashes match.

## Compiler identity

Resolve identity from the executable selected for generation. Portable identity is available
when that executable belongs to an installed `@typescript/typescript-<platform>` package:

1. Resolve the selected executable's physical path and containing platform package.
2. Validate the platform package name, expected executable location, nonempty exact version,
   and full hexadecimal gitHead revision.
3. Locate its corresponding `typescript` wrapper in the same install, validate its name,
   version, gitHead, and declared dependency on that platform package at that exact version.
4. Probe the selected executable with `--version`, using an argument list and a five-second timeout;
   require its reported version to match the package metadata. Cache this result per run.
5. Record the lowercase revision, exact version, and the Wire AST protocol version. Parse the
   probe's single `Version <version>` line; require successful exit and terminate a timed-out probe.

The compiler path used here must be the path selected to launch the generator's compiler
session. Capture it once for that run; resolving a different compiler during catalogue handling
is an error. Any necessary internal plumbing must preserve public record/signature compatibility
where possible and receive a semantic impact check before public API changes.

Windows/Linux platform package names, executable hashes, and absolute install paths are
provenance rather than portable compatibility keys. The full TypeScript release version and
source revision must match; matching major/minor versions or protocol numbers alone is insufficient.

A custom executable or an install without verifiable package metadata uses a schema 2 compiler
identity with `kind: "binary"` and `astProtocolVersion`, with the existing top-level `compiler`
SHA-256 as its compatibility key. Binary identity requires exact executable-hash equality.
Two compiler identities of different kinds are incompatible. An environment override pointing
to a recognized installed package can receive portable identity; an unrelated override cannot
borrow identity from the consumer's nearest wrapper package.

Incomplete metadata falls back to binary identity during identity discovery. Metadata that
claims a recognized install but contradicts the selected executable's version or package
pairing fails with an actionable toolchain error. Generation without catalogue production or
references retains its existing behavior and incurs no identity-probe cost.

This is a compatibility contract for locally installed toolchains, not a signature or authenticity
scheme for adversarial package contents. Producer fingerprint changes are allowed only after the
supported compatibility checks pass. Package provenance and NuGet payload integrity remain
responsibilities of subsequent issue slices.

## Loader and validation

Parse the schema and metadata before selecting a validation policy. Schema 1 retains legacy
guards. Schema 2 requires well-formed compatibility metadata and compares each contract field,
compiler identity, and the existing inference-profile hash. Missing or malformed schema 2
metadata fails closed; it cannot fall back to the schema 1 policy.

For schema 2, generator assembly hashes are retained for diagnostics and cease to be equality
guards. Portable compiler identities permit different executable hashes; binary identities keep
the hash guard. Both paths retain inference-profile equality and all existing downstream checks.

Validate every explicit reference before merging declarations. Each inherited catalogue must
be compatible with the consumer's contract, and existing owner dependency ordering, missing-owner,
cycle, duplicate identity, source/manifest, arity, constraint, API, and variant checks remain active.
New producer output records the current contract and incorporates only successfully validated
references. The existing owner DAG is retained; mapping its owners to resolved NuGet dependencies
belongs to the discovery slice.

The loader should return the decoded catalogue and its metadata together internally, so policy
selection cannot lose metadata. A shared decoding path should retain variant metadata and permit
future stream-based transport without coupling compatibility decisions to a file extension.
This slice may retain the current string output API; adding streaming output belongs to Brotli work.

Errors identify the reference path, incompatible component, expected value, and actual value.
Distinguish unsupported schema, missing/malformed contract, compiler identity/version/revision/
protocol mismatch, binary fingerprint mismatch, and inference mismatch. Existing downstream
diagnostics remain recognizable. Recommend regeneration or selecting the matching toolchain;
offer no bypass flag.

## Contract maintenance

Place contract constants beside the compatibility policy, with one authoritative definition per
component. A behavior change affecting an encoded rule updates its component version in the same
commit unless tests demonstrate that the contract is unchanged. A JSON format change updates
schemaVersion; a policy/metadata interpretation change updates contractVersion.

Changing output formatting or rebuilding the assembly alone requires no contract bump. Changing
identity, canonical API, inference, customization authentication, or owner interpretation requires
an explicit compatibility assessment. Record this requirement in footguns.md and the catalogue
phase record when implementation lands, and explain migration in the consumer dependency guide.

## Acceptance evidence

Add a small `catalog-portability-lab` producer/consumer using a shared generic interface and a
source dependency. Register the lab through the existing fixture mechanism. Test policy decisions
directly with internal metadata models, and exercise accepted and rejected catalogues through
the real Pipeline and live pinned compiler.

Required cases:

- Matching portable contracts permit changed generator and compiler provenance hashes; a consumer
  successfully compiles against the generated producer API.
- Each contract version, compiler release, revision, protocol, and inference mismatch rejects reuse.
- Missing, null, malformed, unsupported, and inconsistent metadata rejects reuse before output.
- Binary identities accept identical executable hashes and reject different hashes and kinds.
- Schema 1 accepts matching legacy fingerprints and rejects changed compiler/generator fingerprints.
- Compiler discovery covers nearest installs, recognized environment overrides, unrelated overrides,
  missing metadata, inconsistent wrapper/platform packages, and a failed/timed-out version probe.
- Changed source bytes, manifest ownership, arity, constraints, canonical APIs, conflicting owner
  graphs, and incompatible customization variants still reject reuse under schema 2.
- Producer/adapter/consumer chaining retains owner ordering and compatibility validation for every
  reference, including legacy references accepted through the strict policy.
- Existing plain JSON configuration and public Catalog record use sites remain compatible.

Real portability evidence requires a catalogue generated on Windows to be consumed on Linux and
the reverse with the same pinned TypeScript release, revision, and source bytes. Exchange a small
lab catalogue and its producer binding through CI artifacts; preserve the source fixture's bytes
across checkout/transfer. Compile the resulting consumer on each platform. Local metadata mutation
tests establish policy behavior, but alone do not establish cross-platform compatibility. Report
cross-platform evidence separately if the local environment cannot run it.

Run focused catalogue/customization suites during development, then the repository's required full
test, build/compile gates, and Fable run gate before implementation completion. Catalogue metadata
changes are expected; generated F# and findings should remain unchanged. Review any unexpected
corpus changes under the generator-fixture rules.

## Delivery boundaries

The implementation updates catalogue policy/serialization, the internal compiler identity path,
small lab and regression tests, consumer migration documentation, and the existing catalogue
architecture/footgun records. CI artifact exchange provides the portability gate. It adds no
configuration switch to weaken validation and performs no package restore/download.

The subsequent Brotli slice can serialize this schema unchanged into compressed payloads. NuGet
descriptors can reference these contract fields while adding payload integrity and binding package
identity. Their final layout and dependency-owner mapping require their own design review.
