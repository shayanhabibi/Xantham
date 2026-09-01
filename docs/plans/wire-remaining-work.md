# Xantham.TypeScript.Wire — remaining work

Baseline at the time of writing: the library builds clean on `net10.0`, `net8.0` and
`netstandard2.1` with zero warnings, and the 44 tests in
`tests/Xantham.TypeScript.Wire.Tests` pass against the live `tsc` binary. Nothing below is a
repair; it is completion, ergonomics, packaging and coverage.

Phases are ordered by what unblocks what. Phase 1 gates the library's usability, phases 2-4 are
independent of each other and of phase 1, phase 6 depends on phase 1.

**Status (2026-09-01).** Phases 1, 2, 3, 4, 5, 7 and 8 are done; the suite is 74 tests, all passing
against the live compiler, and the library builds on all three target frameworks with no warnings.
Phase 6 is the only one left, and it is not a task so much as a project: nothing live generates
bindings today, and what a Wire-based generator should emit has not been decided. It needs a
direction before it needs code.

---

## Phase 1 — Record defaults (blocks every consumer) — DONE

**Problem.** `tests/Test.fsx` is the evidence: to call `createProgram` a caller must write out
all ~110 fields of `CompilerOptions` (`Proto.generated.fs:1550`) as `ValueNone` by hand. There is
no `Default`, `empty` or equivalent anywhere in the assembly, and `Paths` is a non-optional
`JsonObject` so it cannot even be omitted. Outside of a generator that writes the records for
you, the sync and async APIs are unusable.

**Work.**
1. In `tools/proto-gen/generate.mjs`, emit a `Default` static member on every record whose
   fields are *all* optional (`voption`, or a reference type the schema marks optional).
   The member is a value, not a function, so `{ CompilerOptions.Default with Strict = ValueSome true }`
   is the idiom.
2. Decide the treatment of non-optional `JsonObject` fields (`Paths`, and any sibling). Either
   the schema genuinely requires them — in which case `Default` supplies an empty `JsonObject`
   — or they are optional upstream and the generator's type mapping is wrong. Check
   `dist/api/sync/api.d.ts` before choosing; record the answer in `docs/wire-hand-written.md`
   if it turns out to be a transcribed fact rather than a derived one.
3. Regenerate (`dotnet fsi tools/generate-wire.fsx generate proto`) and rewrite `tests/Test.fsx`
   against the new form — it collapses from ~110 lines to one.
4. A test that `CompilerOptions.Default` serialises to `{}` (every field is
   `JsonIgnoreCondition.WhenWritingDefault`), so a default record is a no-op on the wire.

**Done when** `tests/Test.fsx` constructs a program in a single expression and the wire payload
for a default record is empty.

**Outcome.** 15 of the 105 records now carry a `Default`, by a least-fixpoint rule: a record is
defaultable when every field is optional *or* is itself a defaultable record. That closure is
what gives `CreateProgramOptions` a `Default` even though the schema requires its
`compilerOptions`. `tests/Test.fsx` went from 137 lines to 34 and now runs end to end against
`@cloudflare/workers-types`: 83 source files, a 43,954-node AST.

Two findings worth keeping:

- **`Default` cannot be a `static member val`.** Static fields initialise in file order, the
  module is in schema order, and `CreateProgramOptions` (line 1596) names
  `CompilerOptions.Default` (line ~2100). The eager form captured the field before it was
  assigned and `CreateProgramOptions.Default.CompilerOptions` read back as a silent `null`. The
  generator emits a private `lazy` behind the property instead, which defers the body past the
  file's initialiser and still builds the value once. The nested-identity test in `Json.fs` is
  what catches a regression here.
- **`paths` is not a type-mapping bug.** The schema declares `paths?: Record<string, string[]>`
  and the generator maps bare `JsonNode`/`JsonObject` fields to nullable references rather than
  value options, so its absent form is `null`, not `ValueNone`. Nothing to change, and nothing
  to add to `docs/wire-hand-written.md`: it is derived, not transcribed.

---

## Phase 2 — Untested surface — DONE

Four holes, in descending order of risk:

1. **Virtual-FS callbacks** — `TsGoCallback` (`Library.fs:47`) and the `--callbacks=` path
   through `TscChannel`/`TscMailbox` have zero tests; "callback" does not appear in the test
   project. Per `docs/plans/tsgo-protocol.md` (probe5) the arguments arrive JSON-encoded and an
   explicit `null` is not the same as absent — it changes module resolution. Needs a test that
   serves a file from memory and asserts both the encoding and the null/absent distinction.
2. **WTF-8 decoding** — `Wtf8.decode` (`Library.fs:55`) exists precisely for lone surrogates,
   and no test feeds it one. `unicode.ts` is currently only used for UTF-16 position checks
   (`Live.fs:62`). Add a fixture carrying an unpaired surrogate in an identifier and a string
   literal, and assert the decoded string round-trips.
3. **Typed layer** — 6,977 generated lines behind 152 lines of tests (8 cases). Because the
   layer is generated, the valuable test is generated too: a round-trip over every tag
   asserting `retag`/`ofNode`/view agreement, rather than more hand-written cases.
4. **Msgpack `ContentMapperSourceFileInfo`** — the six virtual-file-only fields (`spanMap`,
   `contentMapper`, `virtualFileName`, `supplementalSourceFileNames`,
   `canonicalSourceFileName`, `diagnosticDirectives`) are never exercised, because no fixture
   is a virtual file. Depends on phase 2.1, which is what creates one — but see the outcome:
   it does not, because nothing the API server exposes does.

**Outcome (2.1, 2.2).** `VirtualFileSystem` in `Library.fs` gives the callbacks a typed surface -
one member per callback, each `ValueNone` when unanswered so a partial filesystem falls through to
the real one, and `FileRead` separating `Content`/`Missing`/`FallBack` because the server's answer
to a wrong reply shape is a Go panic rather than an error frame. `Callbacks.test.fs` compiles a
file that exists only in memory, pins every reply shape without a server, and records that
`MSG_CALL_ERROR` is unrecoverable: the channel is spent, not just the request. `surrogate.ts` and
a test in `Live.fs` feed `Wtf8.decode` a lone surrogate - built from `char 0xD800` rather than an
F# escape, since F# lowers a U+D800 escape in its own source to U+FFFD and would have hidden a bad
decode.

**Outcome (2.3).** The typed layer's test is generated from the same schema the layer is:
`tools/tsc-ast/generate-ast.mts` now also writes
`tests/Xantham.TypeScript.Wire.Tests/Typed.table.generated.fs`, a table of every tag with its
kind set, which `Typed.test.fs` runs over. Three claims come out of it that no hand-written case
was making: every node accepts `Node.ofIndex` at the tags whose kind set contains its kind and at
no others, tag inheritance agrees with kind-set inclusion across all pairs, and every alias has an
`ofNode`. The nodes it runs over are one per kind across the whole program rather than the six
fixture files — 107 distinct kinds, against 69 for `lib.es5.d.ts` alone — since the fixtures are a
narrow slice of the language. The remaining kinds need JSX, JSDoc or a syntax error to appear at
all.

**Outcome (2.4).** The dependency on 2.1 was wrong: a virtual file is not something a client can
ask for. `dist/api/node/node.d.ts` exposes a `contentMapper` getter and no method that accepts
one, and `encoder.go:675-695` writes the six words out of a Go `SourceFile` that only the
compiler's own content-mapping path populates, so no sequence of requests produces a blob with
them set. `VirtualFile.test.fs` tests the decoding instead of the production: it fetches a real
blob, asserts an ordinary file carries none of the six, then appends three hand-written msgpack
values to the structured-data section and points the root's extended record at them. That is the
encoder's side of the format written by hand, which is the point — the tests that catch a
misreading are the ones asserting field order within a span-map segment and that a segment written
without the optional sixth element reads as `ValueNone`, both of which fail loudly when the
accessors are mutated.

---

## Phase 3 — Packaging — DONE

`dotnet pack` succeeds, but the nuspec is bare: `<description>Package Description</description>`,
`<authors>Xantham.TypeScript.Wire</authors>`, no tags, no README, no release notes. Also
`Directory.Build.props` links `FsDocsReleaseNotesLink` at a `RELEASE_NOTES.md` that does not
exist in the repo root, and `Version` is `0.1.0` while `AssemblyVersion` is `0.0.0.0`.

**Work.** Set `Description`, `Authors`, `PackageTags`, `PackageReadmeFile` (a Wire-specific
README, not the repo root one, which is about the old pipeline) and `PackageReleaseNotes`;
create `RELEASE_NOTES.md`; settle the assembly-version story.

**Outcome.** Done, bar the assembly-version story, which is left as it was: the packed nuspec now
carries the authors, description, tags and `README.md`, and packs with 0 warnings.

---

## Phase 4 — Documentation — DONE

`README.md` and `docs/index.md` do not mention the Wire at all: there is no "how do I read a
TypeScript AST from F#" entry point. `docs/wire-navigation.md` and `docs/wire-hand-written.md`
are good but unlinked from the index, and `docs/Documentation/` is empty. Wire them into the
index and add a short quick-start that matches what phase 1 makes possible.

**Outcome.** `src/Xantham.TypeScript.Wire/README.md` is the quick-start, with every snippet
compile-checked against the live compiler, and it ships in the package. Both `README.md` and
`docs/index.md` gained a Wire row in the module and status tables and a section linking
`wire-navigation.md`, `wire-hand-written.md`, `plans/tsgo-protocol.md` and this plan. Neither
index was otherwise rewritten: they still describe the Fable/Decoder pipeline, which is accurate
until phase 6 says otherwise. `docs/Documentation/` is still empty - it is fsdocs' own directory,
not ours.

---

## Phase 5 — Protocol document's own open items — DONE

From `docs/plans/tsgo-protocol.md:843-847`, plus one discrepancy found since:

- Transcribe the exact `RemoteSourceFile` extended-data field names from
  `dist/api/node/node.js:264-297` into §5.6.
- Enumerate the full method list with param/result types from `dist/api/sync/api.d.ts`.
- **Method-count discrepancy:** the generator emits 142 sync and 141 async methods, against the
  document's "115 shipped vs 137 on main". Calling a `main`-only method fails at runtime, so
  reconcile the two and, if the generator is emitting methods the shipped build lacks, make the
  generator filter or mark them.
- Confirm `ping`'s params/result, and whether the four `transpile*` methods the generator emits
  exist in the shipped build.

**Outcome.** All four settled against `typescript@7.1.0-dev.20260830.1`, and `docs/plans/tsgo-protocol.md`
now says so with its checklist ticked.

- §5.6 carries the whole nineteen-word record, names and offsets, from the
  `sourceFileExtendedDataOffsets` table in `dist/api/node/node.js` — the same table
  `tools/tsc-ast/record.mts` generates `SourceFileRecord` from, so the doc and the code cannot
  disagree without the generator noticing. The document was written against protocol version 5,
  whose record was twelve words; that is flagged rather than silently rewritten.
- **There is no method-count discrepancy any more.** The "115 shipped vs 137 on `main`" figure was
  the old `@typescript/native-preview` preview build. In the pinned build the schema declares 142
  and the binary answers all 142: `tools/probe-method-existence.mjs` (new, and reusable after an
  npm bump) probes every method the schema declares and reports zero unknown, zero fatal. So the
  generator has nothing to filter, and §7.8 now lists all 142 with their params and result types.
- `ping` is `null` in, the JSON string `"pong"` out. All four `transpile*` methods exist.

Two corrections fell out of the reading, both now in the document and one in the library:

- §5.3 described the string-table offsets as consumed *pairwise*, "stepping by 2". They are
  cumulative: string `i` runs from `offsets[i]` to `offsets[i + 1]`. The decoder was always right;
  the document was not.
- A span-map segment written with five elements instead of six means `SpanMapFeature.All` in the
  reference client, not "no features". `Ast.SpanMapSegment.Features` still reports the wire fact as
  a `voption` — the two forms are worth telling apart — but its doc comment now says what `ValueNone`
  means, because the natural reading of it is exactly backwards.

---

## Phase 6 — Generator integration (depends on phase 1)

The pre-Wire generator and the Fable/Decoder extraction path it sat on have been retired to
`.archive/` — see `.archive/README.md`. Nothing live generates bindings today. Building a
generator on the Wire's typed layer is the outstanding architectural step, and it starts from
scratch rather than from the archived `Xantham.Generator`.
`tests/Test.fsx` — the cloudflare `workers-types` dogfood script — is the current scratch work
toward it.

---

## Phase 7 — CI check — DONE

`build.fsx` runs `npm install` in the test project and then `dotnet test`, and CI runs on
`ubuntu-latest`, so the live tests should be exercising the linux branch of `Tsc.locate`
(`Library.fs:312`). Confirm they actually run rather than `skiptest` on the runner: a green
build with every live test skipped looks identical to a green build that tested something.

**Outcome.** The linux path is right and the CI could not have exercised it. Three findings:

- `Tsc.locate` builds `node_modules/@typescript/typescript-linux-x64/lib/tsc`, and that is exactly
  what npm writes: the platform tarball for `7.1.0-dev.20260830.1` contains `package/lib/tsc`, no
  extension. Verified by fetching the linux-x64 tarball and listing it, since a Windows working copy
  never installs it.
- **A skipped run is no longer green.** `Tsc.test.fs` fails when `XANTHAM_REQUIRE_TSC` is set and no
  compiler was found, and the workflow sets it. Checked both ways by moving `node_modules/@typescript`
  aside: red with the variable, ignored-and-green without it, which is what a working copy with no
  `npm install` should still get. A second case pins the located path to a platform package named for
  the running platform, so a rid typo cannot degrade quietly into a skip.
- **The workflow would not have got that far.** `actions/setup-dotnet@v3` was pinned to no version
  and there is no `global.json`, so the runner would have built with its preinstalled SDK — older
  than the `net10.0` the library targets. Now pinned to `10.0.x`.

---

## Phase 8 — Extension members on `TscChannel` (added after the fact) — DONE

The mailbox had extension members (`mailbox.getSourceFile parameters`, plus a flattened overload
per parameter record) and the channel did not, so the synchronous surface was the only one that
had to be threaded by hand. The emitter in `tools/proto-gen/generate.mjs` is now a single
`extensions()` function called twice - once for `TscChannel`/`Api` and once for
`TscMailbox`/`AsyncApi` - so the two sets cannot drift, and regenerating leaves
`ProtoAsync.generated.fs` byte-identical. `ProtoApi.generated.fs` gains a `TscChannelExtensions`
block: 142 members, 140 of them with a flattened overload as well. Two tests in `Live.fs` check
that the free function, the record overload and the flattened one reach the same server, that an
omitted optional argument is absent rather than null, and that the binary methods come back
decoded through the member too.

