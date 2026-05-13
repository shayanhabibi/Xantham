# Progress Report — `prevent-silent-drops-in-export-map`

> Handover note. Picks up where `docs/plans/post-pr1-progress.md` left
> off (after commit `33cc56f`, which added that template). This branch
> targets PR #2 (`speakeztech/Xantham#2`) and addresses the largest
> structural issue identified in the post-PR1 report: silent export
> drops in `interner.ExportMap`, plus the `Record<K, V>` / mapped-type
> collapse that compounded it.

- **Branch:** `prevent-silent-drops-in-export-map`
- **Base:** `33cc56f` (post-pr1-progress.md doc) on top of PR #1
- **HEAD:** `1472e24`
- **Target PR:** https://github.com/speakeztech/Xantham/pull/2
- **Status:** in-flight; `2cc1645` (the structural fix) lands but test
  fixtures and the consuming Fidelity.CloudEdge driver still need
  migration to consume the new shapes.

## Why this branch exists

`post-pr1-progress.md` identified three structural issues blocking the
Cloudflare SDK closeout:

1. **`ExportMap` silently dropping exports** at `Arena.Interner.fs:989`
   via `Map.ofSeq` last-wins on duplicate `Source` keys
   (e.g. `zod/v3/types.d.ts`: 71 exports → 1 surviving entry).
2. **`Record<K, V>` body collapse** in the encoder — every
   `Record<string, X>` resolved to a single shared `ResolvedType` for
   `{[key: string]: any}` (`Type: -7`), so 19+ aliases shared one body
   identity and the generator's render cache let one alias win the
   race for all of them.
3. **Synthetic literal anchor bug in the `Literal` prerender arm** of
   `RenderScope.Prelude.fs` — 518+ FS0039 errors against
   `_LitN.{Type,Code,Method,Item,…}` paths.

(3) was partially fixed at the end of the post-PR1 work (commit
`dc7a650` on the prior branch, documented inline in
`post-pr1-progress.md`). This branch finishes (1) and (2) as one
encoder+decoder change, fixes a typar-order regression that surfaced
once the bigger fixes landed, and refactors the encoder entry point so
the driver no longer has to point at a specific `.d.ts` inside a
package.

## What landed (12 commits since `33cc56f`)

Listed in commit order, base → HEAD.

### Documentation patches to `post-pr1-progress.md`

* `6bd27fb` — note `Record<K, V>` collapse issue
* `eda01e9` — added WASM notes
* `4b570a4` — update for map vs multimap
* `1ba4737` — update for issue with Synthetic Paths

Four commits expanding the post-PR1 doc with deeper analysis of issues
(1), (2), the `SyntheticPathAssignment` content-vs-reference identity
gap, and the surface trade-offs (FS0039→FS0887 if you papered over the
collapse with substitutions). No code change; they sharpen the design
input that informs `2cc1645` below.

### `952ef12` — graft `scope.PathContext` in Literal prerender arm

The "synthetic inner-literal anchor" fix described in the post-PR1
doc, applied to the `ResolvedType.Literal` arm of `prerender` in
`Generator/RenderScope.Prelude.fs`. Mirrors what the `TypeLiteral`
and `Union LiteralLike` branches already did:

```fsharp
Root = rootPath |> TypeLikePath.create |> ValueSome
Render = lazy Literal.render ctx childScope tsLiteral |> Render.create ref
TransientChildren = ValueSome childScope
```

vs. the bare `TransientTypePath.Anchored` / `scope` it used before.
Pre-fix this produced two colliding anchors at the parent record's
path (`_Lit70` and `_Lit70.Method`); post-fix the inner literal anchors
correctly as `_Lit70` sibling module.

This is mostly a re-land of the work documented in `post-pr1-progress`
under "Synthetic inner-literal anchor — graft `scope.PathContext` in
the `Literal` branch". The empirical impact table there still applies
(FS0039 `'Type'`/`'Code'`/`'Method'` family from ~518 → ~200 in agents).

### `03d4eaf` — preserve typar declaration order in
`renderTypeParametersIntoPostfixList`

Bug in `Generator/TypeRender.Render.fs:387` exposed once more types
were reaching the generator (i.e. surfaced by the multi-map fix).
`renderTypeParameter` was applied via `List.fold (fun acc x -> x :: acc …)`
which reverses the input list — a TS declaration `<O, I, Internals>`
was emitting `<'Internals, 'I, 'O>`, breaking every call site that
passed type arguments positionally with FS0001 cascades.

Fix swaps `List.fold` for `List.foldBack` to preserve order. Inline
comment notes the reason.

### `2cc1645` — fix(types): MappedType / ReverseMappedType + silent export drops

The main commit of this branch. Three intertwined changes that
together close issues (1) and (2) from `post-pr1-progress`.

**Decoder side — `Arena.Interner.fs:469, 999`:**

```fsharp
// before
ExportMap: Map<Xantham.Source, LazyResolvedExport>
// after
ExportMap: Map<Xantham.Source, LazyResolvedExport list>
```

The construction step at line ~989 now does
`|> Seq.groupBy fst |> Seq.map (fun (s, vs) -> s, vs |> Seq.map snd |> List.ofSeq) |> Map.ofSeq`
instead of letting `Map.ofSeq` last-wins drop duplicates. Doc-comment
notes: *"Lists should be singletons where `Xantham.Source.IsPackage`"* —
multiple entries only legitimately appear under `PackageInternal` and
`LibEs` buckets. Sorts on `Union`/`Intersection` element lists in
`Utils.compressWithMap` (lines ~305, ~310) so the literal-union element
ordering pathology called out in `post-pr1-progress` under
"SyntheticPathAssignment coverage gap" no longer produces two distinct
`ResolvedType` identities for `"text" | "audio"` vs `"audio" | "text"`.

**Encoder side — `Reading/Dispatch/TypeFlagObject.fs:217`:**

`TypeFlagObject.Mapped` no longer collapses generic mapped types to
`{[key: string]: any}`. The fallback path (when `getPropertiesOfType`
returns 0 props — i.e. the mapped type is over a generic key set
rather than a known property union) now reads:

```fsharp
let keyType =
    mappedType.nameType
    |> Option.orElse mappedType.constraintType
    |> Option.orElse (mappedType.typeParameter |> Option.bind _.getConstraint())
    |> Option.filter (_.TypeKey >> (<>) mappedType.TypeKey)
    |> Option.map (pushTypeToStack ctx >> _.TypeSignal)
let valueType =
    mappedType.templateType
    |> Option.filter (_.TypeKey >> (<>) mappedType.TypeKey)
    |> Option.map (pushTypeToStack ctx >> _.TypeSignal)
let isReadOnly = decl.readonlyToken.IsSome
let isOptional = decl.questionToken.IsSome
```

The emitted IndexSignature now carries the real key and value types
(`Type = K`, `Type = V`) rather than `string`/`any`, with the readonly
and optional modifiers from the declaration's `readonlyToken` /
`questionToken`. The encoder transmits faithfully and lets the
decoder/generator decide what to do with it (the design rule the PR
comment thread settled on: *"encoder transmits, doesn't decide"*).

`Record<string, ImportValue>` and `Record<string, unknown>` now intern
to distinct `ResolvedType` identities. The render-cache race that
mis-targeted `inherit Record<...>` to `WebAssembly.ModuleImports` is
eliminated at its source.

**Encoder — rename + new typed wrappers:**

Was: `TypeFlagObject.Instantiated of Ts.ObjectType` bound to
`Ts.ObjectFlags.Instantiated` in `typeFlagObjectKindSet`. Both were
wrong — the dispatch was clearly meant for *reverse-mapped types*
(which is what `Ts.ObjectFlags.ReverseMapped` represents). Renamed
to `TypeFlagObject.ReverseMapped of ReverseMappedType` and the bit
flag corrected.

New typed extension wrappers in `Utils/TypeScript.Extensions.fs`:

* `TypeMapKind` / `TypeMapper` (TS internal type-mapper discriminator
  via `[<TypeScriptTaggedUnion>]`).
* `MappedType` — wraps `Ts.ObjectType` with strong-typed access to
  `declaration: Ts.MappedTypeNode`, `typeParameter`, `constraintType`,
  `nameType`, `templateType`.
* `ReverseMappedType` — wraps with `mappedType: MappedType` and
  `constraintType: Ts.IndexType`.

These replace raw `Ts.ObjectType` everywhere the dispatch needed
mapped-type fields. `Mapped` and `ReverseMapped` arms in
`TypeFlagObject` now carry these typed wrappers.

**Encoder — commutative vs ordered union/intersection builders:**

`Types/ReactiveBuilders.fs:491` — `STypeUnionBuilder` /
`STypeIntersectionBuilder` annotations updated to document the
commutative-vs-ordered contract. The decoder-side `compressWithMap`
sort (above) is the runtime enforcement; the builder annotation is
the documentation.

**Decoder — declaration-context invariant log:**

`Reading/Dispatch/TypeDeclaration.fs:420` — when a declaration is
neither lib-es, nor has an export collection, nor has a submodule id,
the previous silent fallback to `Source.LibEs` now logs an
`Log.error` first. Marker for the residual cases we still want to
hear about.

**Common — `PackageId` / `SubModuleId` ergonomics:**

`Common/Common.Types.fs:64` — added `Name`/`Version`/`PackageId`/
`SubModuleName` member accessors so call sites don't have to pattern-
match the DU constructor.

### `95bbbb8` — feat(encoder): generate a temporary .d.ts file when starting program

The encoder driver previously required a path-to-a-specific-`.d.ts`
file. Most packages have multi-file shapes (main + supplementary
declarations) and the consumer wants to feed *"the package"* rather
than know which `.d.ts` inside the package to use.

New module `Xantham.Fable.Temp.Directory` (`Utils/Xantham.Directory.fs`)
creates a per-run scratch directory under `.xantham/run_<id>/temp.d.ts`
containing import statements that pull in all of a package's
declaration files. The TS compiler reads that scratch root and walks
out into the package via normal module resolution. The CLI now accepts
a package name (e.g. `xantham solid-js`) and resolves entry from
`package.json` + `node_modules`.

`TypeScriptReader` gained a `TempFilePath: string` field; program
construction returns `{ TempFilePath; Program }` so the reader holds
on to the path for the duration of its lifetime. `--clean` flag
removes stale `.xantham/run_*` directories at end of run.

Knock-on `Read.fs` change: the root file gathers its own source
attribution rather than assuming the entry file is the canonical
root (it's the temp scratch file, not part of the package).

### `ebd0728` — feat(core): clean up temp directories and enhance multi-file package handling

Builds on `95bbbb8`. Test fixtures migrated from a flat
`TypeFiles/packages/<name>/` layout to `TypeFiles/node_modules/<name>/`
with proper `package.json` files. Each package's `package.json`
points entry to the package's actual entry (`types/index.d.ts`,
`dist/index.d.ts`, etc.). Tests now consume packages by *name*, the
same way the production driver does.

Removed obsolete utility methods (`isFromEs5Lib`, `trySetSourceForTag`,
`setSourceForTag`) from earlier source-attribution scaffolding that
the `Source` DU made unnecessary.

### `1472e24` — fix: prevent parallel testing causing races in cleanup 'stale' temp dirs

Final tightening: `closeRunDirectory` and `cleanupXanthamDirectory`
guard against ENOENT and EACCES errors from concurrent test workers
racing on the same `.xantham/run_*` paths. Each cleanup step wrapped
in `try …/with _ -> ()` and writability is probed before unlinking.
Tests can now run in parallel without one worker's cleanup pulling
the rug from another.

## Empirical effect (qualitative)

The post-PR1 doc projected this fix would *increase* the surface error
count before reducing it, because making the dropped exports visible
exposes latent bugs in the rendering of their inner literals
(Interface/Class bodies whose string-union literals weren't covered by
the earlier `Literal`-arm fix). Both prior generator-side attempts
documented in the post-PR1 doc (full iterate-all: +136; two-pass
supplement: +1) hit exactly that cascade.

We have not re-run the three-SDK verify pipeline against this branch
yet (fixtures still need to be regenerated and the Fidelity.CloudEdge
driver still needs to consume the new package-name entry shape — see
"Migration work remaining" below). Decoder tests (`Identifier` filter:
13/13) and the local Fable.Tests suite pass against the migrated
fixture layout.

Expected effect when verify runs:

* `'ZodType'` FS0033 family in agents (104 errors against
  `Zod.Decoder.…ZodV3Types.ZodType<a,b,c>`) — should go to **zero**
  once the multi-map allows all 71 zod V3 exports to render rather
  than 1 winning the `Map.ofSeq` race.
* Workers-types `'ModuleImports'` ×134 + `'WebAssembly'` ×134 — should
  go to zero now that `Record<string, X>` aliases intern to distinct
  bodies (encoder side), removing the render-cache race premise.
* `'JSONSchema'` ×54, `'StandardSchemaV1'`, `'ZodType'` declaration-merge
  buckets — same expected resolution; declaration-merged augmentations
  no longer get squashed onto one bucket entry.
* New errors will surface from the unmasked Interface/Class inner-
  literal anchor cascade that defeated the previous attempts. Anchor
  fix may need to be lifted out of the `Literal`/`TypeLiteral`/`Union`
  arms specifically and applied across all body kinds that can contain
  string-union literals. This is the next-most-likely follow-up.

## Migration work remaining

These are the open items before this branch is mergeable.

### 1. Regenerate decoder test fixtures

Five fixture `output.json` files have local modifications in the
working tree (`agents`, `dynamic-workflows`, `solid-js`, `three`,
`workers-types`). These need regeneration against the current encoder
and committed alongside the encoder changes. The `2cc1645` commit
message notes *"TODO: Tests still need migrating"* — this is what it
refers to.

To regenerate: run `Xantham.Fable` on each fixture's `node_modules`
package (now possible directly by package name post-`95bbbb8`) and
write to `tests/Xantham.Decoder.Tests/fixtures/<name>/output.json`.

### 2. Migrate Fidelity.CloudEdge driver

`generators/xantham/Driver.fsproj` and `Program.fs` (52 lines, per
post-PR1 doc) still call `TypeScriptReader.create` with an explicit
path into a package. The driver needs to either:

* switch to package-name entry (preferred — matches the new CLI
  ergonomic) by passing the package name and letting the encoder
  resolve via `node_modules`, OR
* keep an explicit-path code path but consume the new
  `{ TempFilePath; Program }` shape from program construction.

Either way, the driver's output emission consumes the multi-map
`ExportMap` so any place that did `Map.iter` over it needs review.
The generator's `processExports` was the one significant consumer
identified in the post-PR1 doc; check whether it iterates over the
`list` correctly or needs `List.concat` first.

### 3. Re-run the three-SDK verify pipeline

After (1) and (2), uncapped builds of
`Verify.{DynamicWorkflows,WorkersTypes,Agents}.fsproj` will tell us
the new error landscape. Expected to reveal a fresh wave of
Interface/Class inner-literal anchor errors (per the prior iterate-all
and two-pass-supplement regressions). Those become the next branch.

### 4. Decision: residual `Source.LibEs` fallback in TypeDeclaration

The `Log.error "Invariant: …"` in `TypeDeclaration.fs:420` is loud-
fail-soft. Any actual hits in the verify run mean a declaration
exists that we don't have a `SourceTag` for and isn't a lib type —
likely a project-reference or ambient module that the
`SeedExportPoints` / `GetExportCollection` extensions in
`TypeScript.Extensions.fs` aren't reaching. Need to either widen
those extensions or convert the fallback to an explicit Source DU
case for "unknown-but-declared."

### 5. Generator-side: lift inner-literal anchor across all body kinds

The `Literal` and `TypeLiteral` arms of `prerender` now graft
`scope.PathContext`. `Union LiteralLike` already does. The remaining
gap is `Interface` and `Class` bodies that contain string-union
literals as property types — the post-PR1 iterate-all attempt
regressed precisely because their inner literals lacked anchors. The
shape of the fix is the same as `952ef12`; just needs applying to the
two body kinds that the multi-map now sends through the renderer.

## Outstanding questions for review

Carried over from `post-pr1-progress.md` — most are addressed by the
work above; one new one:

* **(addressed)** Map vs MultiMap for `interner.ExportMap`. Resolved
  by switching to `Map<Source, LazyResolvedExport list>`. The
  alternative (`Source * Name` finer-grained key) was rejected
  because it would break the symmetry between `Source.IsPackage`
  (one bucket per package, semantically) and `Source.LibEs` /
  `Source.PackageInternal` (multiple buckets, semantically). The
  list-valued map preserves that distinction in the doc-comment
  invariant: *"Lists should be singletons where
  `Xantham.Source.IsPackage`"*.

* **(addressed)** `Record<K, V>` collapse. Resolved encoder-side per
  the post-PR1 doc's preferred fix shape. PR #2 thread captured the
  rationale: encoder transmits faithfully; the IndexSignature
  Parameter Type slot can carry any TsType (including template-literal
  types and `K extends U ? never : K` conditional filters); decisions
  about what to do with non-trivial key shapes belong downstream.

* **(addressed)** `SyntheticPathAssignment` content-vs-reference gap.
  Resolved by sorting union/intersection elements in
  `compressWithMap` so `"text" | "audio"` and `"audio" | "text"` no
  longer intern to distinct identities — eliminating the duplicate-
  identity premise. Option (1) from the post-PR1 doc (encoder-side
  normalization).

* **(addressed)** `TypeAlias` identity in `ResolvedType`. Not
  modified in this branch — the registration-site workaround in
  `prerenderTypeAliases` stands as the intended pattern.

* **(new)** Mapped-type key emission strategy for the renderer.
  The encoder now transmits `nameType` (template-literal types,
  conditional filters) faithfully in the IndexSignature Parameter
  Type slot. The PR #2 thread sketched four tiers of treatment the
  generator could apply — pure key transformation
  (`as \`get${Capitalize<K>}\``), `never`-filtering
  (`as K extends U ? never : K`), combined transform+filter, and
  unsupported-bail-to-`obj`. None of those tiers are wired up yet;
  the generator currently passes through to `obj` for everything
  except plain `Record`-shape mappings. The four-tier approach is
  worth picking up as a follow-up — most Cloudflare SDK mapped types
  fall in tier 1 (`Record`/`Pick`/`Omit`/`Partial` shapes), but the
  agents SDK has at least one `as` clause that would benefit from
  tier 2.

## Reproduction

```pwsh
# Build Xantham
dotnet build src\Xantham.Fable\Xantham.Fable.fsproj
dotnet build src\Xantham.Decoder\Xantham.Decoder.fsproj

# Regenerate a fixture (post-95bbbb8: by package name)
dotnet run --project src\Xantham.Fable -- solid-js `
    -o tests\Xantham.Decoder.Tests\fixtures\solid-js\output.json

# Decoder tests
dotnet run --project tests\Xantham.Decoder.Tests -- --colours 0

# Fable encoder tests
dotnet run --project tests\Xantham.Fable.Tests
```
