# Task 2 report: co-locate compiler-library families

## Files changed

- `src/Xantham.Generator/Render.fs`
  - Added the compact `CompilerLibFamily` marker on `GroupModule`.
  - Added the dedicated combined compiler-library renderer: one `module rec` root, configured ES/DOM children, independent auto-open attributes, canonical cross-family qualification, and one shared footer.
  - Kept ordinary module and namespace renderers unchanged.
- `src/Xantham.Generator/Pipeline.fs`
  - Marks the existing compiler-library ES and DOM placements with the new renderer metadata. Configuration is still consumed through `CompilerLibLayout.create`, rather than reimplemented here.
- `tests/Xantham.Generator.Tests/Render.test.fs`
  - Added a hand-built two-family renderer test using `Fable.Core.TS`, ES-only auto-open, and bidirectional ES/DOM references.

## TDD evidence

The initial focused test was added before production changes. Its first runnable failure was:

```text
Expected ... `namespace rec Fable.Core.TS` ... to contain `module rec Fable.Core.TS`.
```

This demonstrated the old namespace grouping behavior rather than the required combined recursive module. (The first attempt needed a test-only record type annotation; no production code was changed before the behavioral red run.)

After the minimal renderer and placement metadata changes:

```text
rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj --filter "FullyQualifiedName~compiler library families share one recursive root module"
ok dotnet test: 1 tests passed, 0 warnings in 1 projects
```

## Verification

- `rtk dotnet test tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj` — passed.
- `mcp__fslangmcp__check` for `src/Xantham.Generator/Xantham.Generator.fsproj` — clean, 0 errors, 0 warnings.
- `mcp__fslangmcp__check` for `tests/Xantham.Generator.Tests/Xantham.Generator.Tests.fsproj` — clean, 0 errors, 0 warnings.
- `rtk git diff --check` — clean.

## Decisions

- The only new routing metadata is `CompilerLibFamily = Es | Dom` on `GroupModule`; it does not introduce a general module-path tree.
- `GroupModule.Module` remains a `string`. The compiler renderer derives its effective canonical module from Task 1's `CompilerLibLayout` at render time.
- Ownership/qualification uses the effective ES/DOM qualified modules, so references never rely on either child being auto-opened.
- Compiler-family declarations aggregate footers before rendering, producing exactly one footer in the combined file.

## Commit

`feat(generator): co-locate compiler-lib families`

## Concerns

None. The focused test exercises the configured dotted root and asymmetric auto-open policy; ordinary module and namespace rendering remains on its existing path.
