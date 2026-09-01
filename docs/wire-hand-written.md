# The hand-written register

Almost everything in `src/Xantham.TypeScript.Wire` is generated from a vendored upstream file.
This document is the exception list: every fact in the pipeline that was **transcribed or decided
by a person** rather than derived from the schema.

Each entry says what it reflects, how it was arrived at, and what to do when upstream moves. They
are ordered by consequence — the first few fail *silently* if they go stale, the last few fail
loudly at build time.

Upstream is `microsoft/TypeScript`, pinned in `tools/tsc-ast/upstream.lock.json`. Check the pin
with `dotnet fsi tools/generate-wire.fsx sync tsc-ast --check`, and re-fetch with the same command
without `--check`.

## How to use this list

Two commands regenerate everything derived:

```
dotnet fsi tools/generate-wire.fsx generate ast     # Enums, Ast, AstNode, Typed
dotnet fsi tools/generate-wire.fsx generate proto   # Proto, ProtoApi, ProtoAsync
```

Both fail with a non-zero exit and a `problems` list rather than emitting something wrong, so an
entry below that has a machine check is one you will hear about. The entries marked **unchecked**
are the ones a compiler upgrade can invalidate without anyone noticing.

---

## 1. Extended-record offsets per kind — unchecked

**Where:** `src/Xantham.TypeScript.Wire/Library.fs`, in `Ast.text` / `rawText` / `tokenFlags` /
`templateFlags` (the `extendedWord file index N<byteOffset>` calls).

**What it reflects:** the layout of the small extended-data record a literal node points at.
Unlike the `SourceFile` record (entry 5), this layout is neither in `ast.json` nor written down in
prose — it is inline in upstream's emitter.

| Kinds | offset 0 | offset 4 | offset 8 |
|---|---|---|---|
| `StringLiteral`, `NumericLiteral`, `BigIntLiteral`, `RegularExpressionLiteral` | text index | `tokenFlags` | |
| `TemplateHead`, `TemplateMiddle`, `TemplateTail` | text index | raw text index | `templateFlags` |
| `NoSubstitutionTemplateLiteral` | text index | | |

**How it was derived:** read off `tools/scripts/tsc/generate-encoder.ts:1897-1955` at the pinned
commit, by hand. The *classification* of which kinds carry which shape is generated
(`AstKind.hasStringText` / `hasExtendedText`, from upstream's `classifyDataType`); only the byte
offsets within each shape are transcribed.

**Failure mode if stale:** silent. A shifted offset reads a neighbouring word — a plausible-looking
string index or flag word — and nothing throws.

**To update:** re-read `recordExtendedData` in `generate-encoder.ts` at the new pin and compare its
appends against the table above. The live test `the data word decodes to literal text, flags and
commonData bits` asserts a hex literal's `tokenFlags` is exactly `TokenFlags.HexSpecifier` and a
template head's raw text, which is the tripwire worth keeping.

## 2. The msgpack tag subset — loud, and incomplete by design

**Where:** `Library.fs`, the buffer cursor inside `module Msgpack`.

**What it reflects:** the structured-data section of the blob is msgpack, but the writer only ever
emits eleven tags: `fixarray`/`array16`/`array32`, positive fixint/`uint8`/`uint16`/`uint32`,
`fixstr`/`str8`/`str16`/`str32`, and `true`/`false`.

**How it was derived:** by enumerating the write calls in the vendored
`tsc/internal/api/encoder/encoder.go:849-891`. The subset is deliberate: a general msgpack decoder
would accept forms the encoder never produces, and would therefore not notice if the encoder
started producing them.

**Failure mode:** loud. Any other tag throws `expected a msgpack array header, got 0x..`.

**To update:** if that throw fires in the field, the encoder gained a form. Add it to the reader.
Do not add a catch-all skip — skipping an unknown value silently drops data.

This is not the same msgpack as the transport envelope, which is also in `module Msgpack` in the
same file. The envelope is a stream and is written; this is a buffer and is only read.

## 3. `FIELD_ENUMS`, the proto field-to-enum table — checked for staleness, not for correctness

**Where:** `tools/proto-gen/generate.mjs`, `const FIELD_ENUMS`.

**What it reflects:** the wire schema types every enum as a bare `number`, because the Go side
serialises them as integers. Upstream's own typed JavaScript wrapper does name them, and this table
transcribes that naming into F#.

| Key | F# type | Transcribed from |
|---|---|---|
| `SymbolResponse.flags` | `SymbolFlags` | `dist/api/async/api.d.ts`, `readonly flags: SymbolFlags` |
| `SymbolResponse.checkFlags` | `CheckFlags` | same, `readonly checkFlags: CheckFlags` |
| `TypeResponse.flags` | `TypeFlags` | `dist/api/async/types.d.ts`, `readonly flags: TypeFlags` |
| `TypeResponse.objectFlags` | `ObjectFlags` | same, `readonly objectFlags: ObjectFlags` |
| `TypeResponse.elementFlags` | `ElementFlags[]` | same, `readonly elementFlags: readonly ElementFlags[]` |
| `SignatureResponse.flags` | `SignatureFlags` | name correspondence only — see below |
| `TypePredicateResponse.kind` | `TypePredicateKind` | `dist/api/async/types.d.ts`, `readonly kind: TypePredicateKind` |
| `GetSignaturesOfTypeParams.kind` | `SignatureKind` | `dist/api/sync/api.d.ts`, `getSignaturesOfType(type, kind: SignatureKind)` |
| `ResolveNameParams.meaning` | `SymbolFlags` | same, `resolveName(name, meaning: SymbolFlags, ...)` |
| `GetSymbolsInScopeParams.meaning` | `SymbolFlags` | same, `getSymbolsInScope(location, meaning: SymbolFlags)` |
| `SignatureToSignatureDeclarationParams.kind` | `SyntaxKind` | same, `signatureToSignatureDeclaration(signature, kind: SyntaxKind, ...)` |
| `SignatureToSignatureDeclarationParams.flags` | `NodeBuilderFlags` | same, `..., flags?: NodeBuilderFlags` |

`SignatureResponse.flags` is the one entry with weaker authority: upstream exports `SignatureFlags`
but never declares a field at that type, so the claim rests on the name and on the enum's members
(`HasRestParameter`, `Construct`, `Abstract`) being what a signature carries. It is the first entry
to re-check on an upgrade.

**Why explicit rather than inferred from the field name:** `TypeToTypeNodeParams.flags` is the
counter-example. `typeToTypeNode` and `typeToString` share that one parameter record, and upstream
types the argument `NodeBuilderFlags` for the first and `TypeFormatFlags` for the second. One field
cannot be both, so it stays `int`. A rule keyed on names ending in "flags" would have picked one
arbitrarily.

**Failure mode:** half-loud. A key that no longer matches any field in the schema is a `problems`
entry and fails the run. A key that still matches but whose meaning changed upstream is silent.

**To update:** after a compiler upgrade, re-read the declarations named in the last column, under
`node_modules/typescript/dist/api/`. Add or correct entries, then regenerate proto.

## 4. `HAND_WRITTEN_NARROWINGS` — one entry, unchecked

**Where:** `tools/tsc-ast/generate-ast.mts`, `const HAND_WRITTEN_NARROWINGS`.

```
JsxTagNamePropertyAccess -> PropertyAccessExpression
```

**What it reflects:** a few upstream types are declared in hand-written `ast.ts` rather than in
`ast.json`, and are narrower than any set of kinds can express — a `JsxTagNamePropertyAccess` is a
`PropertyAccessExpression` whose expression is itself a tag name, which is a constraint on a child,
not on a kind. The generated guard admits the whole kind and says so.

**How it was derived:** it is the residue. Every type the schema resolver could not turn into a
kind set was reported as a problem; this was the only one that was a real type rather than a bug in
the resolver.

**Failure mode:** a *new* such type is loud — it fails resolution and stops the run. The existing
entry going stale is silent, but it is one line and its widening is documented where it is used.

**To update:** if a run reports an unresolvable type, decide deliberately whether it belongs here.
Adding an entry widens a guard, so it is a decision rather than a formality.

## 5. `FIELD_NAMES`, the `SourceFile` record's field names — loud

**Where:** `tools/tsc-ast/record.mts`, `const FIELD_NAMES`, nineteen pairs in record order.

**What it reflects:** the `SourceFile` extended-data record is 19 uint32 words, and its layout is
documented in a **markdown table inside a Go comment** — `tsc/internal/api/encoder/encoder.go`,
lines 72-200. This is the one place in the pipeline where the authority is prose.

The *offsets* are parsed out of that table and generated into `SourceFileRecord` in
`Ast.generated.fs`. Only the mapping from upstream's spelling to the F# name is written down:
`text -> Text`, `referencedFiles -> ReferencedFiles`, and so on.

**How it was derived:** the table was checked against the code of `recordExtendedData_SourceFile`
(`encoder.go:662`) field by field; the two agree word for word. The names are then just
pascal-cased, but they are *listed* rather than computed so that a reworded row is a failed run
rather than a renamed — or worse, reordered — accessor.

**Failure mode:** loud, three ways. The parser requires the section heading; requires every row to
be `| n-m | uint32 | ... |` with contiguous, non-overlapping 4-byte spans starting at 0; and
cross-checks the row count and each row's name against `FIELD_NAMES` and against the argument count
of the `appendUint32s(...)` call.

**To update:** if a run reports that the record's fields have moved, read the new table, confirm it
against `recordExtendedData_SourceFile`, and edit `FIELD_NAMES` to match — in record order.

## 6. `VENDORED`, the enums we emit — loud on a bad file, silent on an omission

**Where:** `tools/tsc-ast/enums.mts`, `const VENDORED` (20 names), mirrored by the fourth `sources`
group in `tools/tsc-ast/upstream.json`.

**What it reflects:** upstream's `packages/typescript/src/enums/` holds far more enums than Wire
needs. This is the chosen subset: the nine the checker answers with, the three the blob carries,
`nodeBuilderFlags` for the one request that takes it, and the seven small ones the `SourceFile`
record and the span map use.

`syntaxKind.enum.ts` is vendored but deliberately **absent from `VENDORED`** — it is fetched as an
oracle (entry 9), not emitted, because `SyntaxKind` is already derived from `ast.json` and two of
them would be two sources of truth.

**Why this source and not the Go:** each of these files carries the banner `Code generated by
Herebyfile.mjs generate:enums from tsc/internal/....go. DO NOT EDIT.` — upstream runs the
Go-to-TypeScript enum generator itself and commits the result, with the Go name prefix stripped.
Reading these costs a small const-expression evaluator; reading the Go directly would cost a Go
parser and a 64 KB `types.go` in the vendor tree.

**Failure mode:** a vendored file that stops parsing is loud — the reader is strict (banner,
declaration, one member per line, close) and throws with `file:line`. An enum that should have been
added and was not is silent: the field simply stays an `int`.

**To update:** add the file name to both `upstream.json` and `VENDORED`, run `sync tsc-ast`, then
`generate ast`. The two lists are separate on purpose, since `upstream.json` also fetches the
oracle.

## 7. The flag-set rule, `isFlagSet` — unchecked, and cosmetic

**Where:** `tools/tsc-ast/generate-ast.mts`, in the `Enums.generated.fs` block.

```js
const isFlagSet = (type) => type.members.filter(member => member.bit).length > 1;
```

**What it reflects:** which enums get `[<System.Flags>]`. A member counts as defining a bit when
upstream spelled it literally `1 << n`; more than one such member makes the enum a bitfield.

**How it was derived:** empirically, after a value-based rule — "every standalone member is a power
of two" — got it wrong. `SymbolFlags.All = (1 << 30) - 1` is neither a power of two nor obviously
composite, so the value rule dropped `[<System.Flags>]` from the largest flag enum in the set. The
syntactic rule reads upstream's intent rather than its arithmetic, and gets 11 of 20, including
`SpanMapFeature` — a bitfield whose name does not say so — and correctly excluding `ScriptKind`.

The same `bit` test does double duty: a member it recognises is emitted in upstream's own shift
form, `JSDocPublic = (1u <<< 23)` rather than `8388608u`, which F# folds to the same constant.
Composite members are not enum cases at all — see entry 13.

**Failure mode:** cosmetic and silent. `[<System.Flags>]` affects `ToString()` and nothing else; a
wrong answer produces an ugly diagnostic string, never a wrong value. Losing the shift form is
equally cosmetic: the values are computed by the evaluator either way.

**To update:** nothing routine. If upstream starts spelling bits as hex literals this rule stops
recognising them and every enum loses the attribute at once, which is visible in the generator's
summary line (`enums 20 over 448 members (11 flag sets)`).

## 8. `ANCHORS`, the kind-numbering tripwire — loud

**Where:** `tools/tsc-ast/kinds.mts`, `const ANCHORS`: `Unknown = 0`, `EndOfFile = 1`,
`Identifier = 79`, `SourceFile = 307`.

**What it reflects:** `SyntaxKind` values are *derived*, by numbering `kinds.elements` in
declaration order and skipping comment-only entries, exactly as upstream's `generate-go-ast.ts`
does with `iota`. These four were the hand-verified anchors on that derivation, and `79` and `307`
were the magic numbers hard-coded in the live tests before phase 1 removed them.

Superseded in practice by entry 9. They are kept because they are cheap, and because they record
why those two numbers ever mattered.

## 9. The `SyntaxKind` oracle — loud, and total

**Where:** `tools/tsc-ast/generate-ast.mts`, the check after the `ANCHORS` loop.

Not hand-written at all. It is listed here because it *retires* a hand-written fact. Every derived
kind and every marker is compared against upstream's published `syntaxKind.enum.ts`: all 351
derived values match, zero missing and zero mismatched, and the only unaccounted entry on the
oracle side is `Count`, which is asserted rather than assumed.

This turns the positional numbering rule from a spot-check into a checked invariant. If upstream
changes how kinds are numbered, the run fails with a list of the values that moved.

## 10. `rawJsonFields` — loud

**Where:** `tools/proto-gen/generate.mjs`, `const rawJsonFields`: `BatchRequest.params` and
`BatchResponse.result`.

**What it reflects:** two schema-untyped payloads that are only ever spliced into, or lifted out
of, a larger document. The shape of each depends on the sibling `method` string, which the schema
does not relate to them. They are emitted as raw UTF-8 JSON (`byte[]` plus `RawJsonConverter`)
rather than as a `JsonNode` DOM, so callers can reuse `ProtoJson.serialize`/`deserialize`
unchanged.

**Failure mode:** loud on absence. A field that stops existing stops matching, and the batch tests
in `Json.test.fs` fail on the round trip.

## 11. F# keyword lists — loud

**Where:** `KEYWORDS` in both `tools/proto-gen/generate.mjs` and `tools/tsc-ast/fsharp.mjs`.

Identifiers that need backtick-escaping in F#. Two lists rather than one because unifying them
means regenerating the proto output from a shared helper, and that change is worth making against a
byte-identical diff rather than alongside another one.

**Failure mode:** loud. A missing keyword is a compile error in the generated file, immediately.

## 12. Blob layout constants — loud for the version, unchecked for the masks

**Where:** `Library.fs`, `module Ast`: `ProtocolVersion = 8u`, `HeaderSize = 44`, `NodeLen = 28`,
and the data-word masks `DataTypeMask = 0xC0000000u`, `ChildMask = 0x000000FFu`,
`StringIndexMask` / `ExtendedDataMask = 0x00FFFFFFu`, `CommonDataMask = 0x3F000000u`,
`CommonDataShift = 24`.

**What it reflects:** the binary format's fixed frame. Transcribed from the vendored
`packages/typescript/src/api/node/node.infrastructure.ts`, the reader side of the same format,
which carries these as named constants — that file is vendored specifically so these stop being
folklore.

**Failure mode:** loud for the version. `Ast.read` rejects any blob whose protocol version is not
8, naming both numbers. The masks are unchecked, but a wrong one breaks every test at once rather
than subtly.

**To update:** on a protocol bump, diff `node.infrastructure.ts` against these and bump
`ProtocolVersion` last, so the version guard keeps failing until the layout is actually right.

## 13. `FSHARP_OPERATORS` and the parenthesising renderer — checked, by a test

**Where:** `tools/tsc-ast/enums.mts`, `const FSHARP_OPERATORS` and `render`.

**What it reflects:** how upstream's value expressions are re-spelled as F#. Composite members —
the 122 that combine other members, `ClassExcludes = (Value | Type) & ~(ValueModule | Interface |
Function)` and the like — are emitted into a companion module as `[<Literal>]`s computed from the
cases, rather than as enum cases carrying an evaluated number:

```fsharp
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SymbolFlags =

    /// <summary><code>Value | Type</code></summary><returns><c>900095u</c></returns>
    [<Literal>]
    let EnumMemberExcludes = (Value ||| Type)
```

They live in the module because **an enum case may not name another case of its own enum**
(`FS0039`). The module shares the enum's name, so `SymbolFlags.Property` and
`SymbolFlags.EnumMemberExcludes` both resolve and a caller cannot tell which half a name is in.
A composite that mixes in a bare integer — `SymbolFlags.All = (1 << 30) - 1` — has no enum type to
be written at, so `render` returns `null` for it and it stays a case with its evaluated value.
There are two such members across the twenty enums.

**How it was derived:** two facts, both established by probe rather than by reading a spec.

The operator map is direct (`|` → `|||`, `&` → `&&&`, `^` → `^^^`, `<<` → `<<<`, `>>` → `>>>`,
`~` → `~~~`). The parenthesisation is not. **F# puts `|||` and `&&&` at the same precedence and
associates left**, where TypeScript binds `&` tighter than `|`. So upstream's `A | B & C` means
`A | (B & C)`, and the same characters in F# mean `(A ||| B) &&& C` — a different number, with no
warning. `render` therefore parenthesises *every* node it emits, and takes its structure from the
parser in the same file rather than from upstream's text.

A reference to an earlier literal in the same module has to be written **bare** (`Value`, not
`SymbolFlags.Value`): inside the module the type wins name resolution, and the qualified form is
`FS0039`. References to enum cases must be qualified. `render` takes a `spell` callback so the
emitter, which knows which bucket a name landed in, makes that call.

**Failure mode:** would be silent — a wrong number, compiling — which is why it is the one entry
here with a runtime test rather than a generator check. `Enums.test.fs` reads every literal's
value back out of the built assembly and requires it to equal the value the generator's own
evaluator recorded in the `<returns>` of the doc comment above it. That is also why the doc
carries the number at all: the F# expression deliberately does not state one, so the comment is
the other half of the comparison rather than decoration. Expressions are XML-escaped on the way
in — `(1 << 30) - 1` inside a `<code>` element would otherwise open a tag that never closes, and
F# warns on malformed doc XML (FS3390). Those two numbers are computed by different
languages from the same tree, so agreement is evidence and not a tautology. All 122 agree.

**To update:** nothing routine; the map covers every operator the vendored files use. A new
operator upstream is loud — `parse` throws on a token it does not know, and `render` returns
`null` for an operator missing from the map, which demotes the member to a plain case rather than
emitting something wrong.

## 14. The FS callback reply shapes — checked, by tests

**Where:** `Library.fs`, `module VirtualFileSystem`: the encoder for each of the six callbacks the
server may invoke (`readFile`, `fileExists`, `directoryExists`, `getAccessibleEntries`,
`realpath`, `writeFile`).

**What it reflects:** the wire form of a `MSG_CALL` reply, which the schema does not describe at
all. Transcribed from `dist/api/sync/client.js:35-56`, the JS client's own adapter, and typed
against `dist/api/fs.d.ts`:

| Callback | Argument | Reply |
| --- | --- | --- |
| `readFile` | JSON string path | `{"content": <string>}`, `{"content": null}` for absent, or empty for fall-back |
| `fileExists`, `directoryExists` | JSON string path | `true` / `false`, or empty |
| `getAccessibleEntries` | JSON string path | `{"files": [...], "directories": [...]}`, or empty |
| `realpath` | JSON string path | a JSON string, or empty |
| `writeFile` | `{"path": ..., "data": ...}` | empty |

Two distinctions carry meaning and are the reason `readFile` answers an object rather than a
string. `{"content": null}` means the file does not exist and resolution stops; an **empty reply**
means "not answered", and the server reads the real filesystem instead. The F# type says this out
loud - `Content` / `Missing` / `FallBack` - because the two are one character apart on the wire
and swapping them changes module resolution without erroring.

**Failure mode:** was the worst kind, which is why the typed layer exists. A wrong reply shape is
not an error frame: it is a Go panic - `json: unable to unmarshal JSON string into Go struct
{ Content *string }` - that kills the process mid-request, so the caller sees only
`tsgo closed the pipe mid-frame` with the real message on stderr. A failed callback is equally
terminal by design: the server treats `MSG_CALL_ERROR` as unrecoverable and exits, so a raising
callback spends the channel.

**To update:** re-read `dist/api/sync/client.js` on a compiler upgrade, and `fsCallbackNames` in
`dist/api/fs.js` for the list itself. `Callbacks.test.fs` pins every reply shape without a server,
and compiles a file that exists only in memory with one, so a changed shape fails there rather
than at a user's process exit.

## What is *not* on this list

Everything else is derived, and worth naming so the boundary is clear: kind ordinals and their 34
markers and guards, the 413 child slots and their order, the `commonData` bit layout, the 305 tags
and 404 inheritances of the typed layer, the 73 node-alias guards, all 448 enum members, the
`SourceFile` record's offsets and size, and every proto record, method and async overload.

Three filters in the generators are *ported* rather than invented, and are the reason the derived
numbers are right. They are documented at their definitions in `generate-ast.mts` rather than here,
since none of them is a value anyone would transcribe:

- the child-slot filter `isChild() && !noTS && !noGo`, from `generate-encoder.ts:65-67,285`, without
  which seven function-like nodes shift every slot after `FullSignature`;
- the `commonData` member filter `!noTS && !noGo && !isChild() && !isKindParam()` plus the
  `NodeFlags`-member exclusion, from `generate-encoder.ts:284`;
- `HAND_WRITTEN_COMMON_DATA = ["SyntheticExpression"]`, the one node whose bit layout is delegated
  to a Go function that is not vendored. It is asserted to remain the only one; a second fails the
  run.
