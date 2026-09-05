---
category: Generator
audience: managing agent
title: Lane P1 - packaging, consumer documentation, ignore rule
branch: worktree-gen-wave10-p1
base: cb6256b
---

# Lane P1 - wave ten, the alpha release surface

Three items, all landed in one commit. The packaging half turned up one live defect the brief
did not price: `pack` fails on `Xantham.Fable.Core` for a reason that only appears once the
project is multi-targeted *and* packed after the Fable run gate. It is fixed here.

## Item 1 - the publishable set

`Spec.publishable` in `build.fsx` names the three projects explicitly, and both sites that
filtered on `_.Name.EndsWith("Wire")` (`Options.projects`'s default, and its empty-selection
fallback) read it. `Options.projects` feeds `build`, `pack`, `publish` and `bump`, so all four
now default to the same three.

| Package | Version | Was |
| --- | --- | --- |
| `Xantham.TypeScript.Wire` | `0.2.0` | `0.2.0`, unchanged |
| `Xantham.Fable.Core` | `0.1.0-alpha.1` | no package identity at all |
| `Xantham.Cli` | `0.1.0-alpha.1` | identity complete, `Version` absent |

`Xantham.Fable.Core` gained `PackageId`, `Version`, `AssemblyVersion 0.0.0.0`, `Authors`,
`Description`, `PackageTags` and `PackageReadmeFile`, and its `README.md` moved from
`Content Include` to `None … Pack="true" PackagePath="\"`. `Xantham.Cli` gained `Version`,
`Authors` and `PackageTags` - `Authors` because it otherwise defaults to the assembly name
`xantham`, which is what a consumer meets first on nuget.org. Flag it if you want it dropped.

### `Xantham.Generator` needs no package - confirmed from the `.nupkg`

`Xantham.Cli.0.1.0-alpha.1.nupkg` carries, under `tools/net10.0/any/`: `xantham.dll`,
`Xantham.Generator.dll`, `Xantham.TypeScript.Wire.dll`, `FSharp.Core.dll` and its resource
satellites. The `PackAsTool` claim holds.

### `Xantham.Fable.Core` packs the way the Fable ecosystem packs

`Fable.Core` 5.2.0 itself ships `lib/netstandard2.0` and no sources - no `fable/` folder, and
`contentFiles` carrying only a changelog. `Fable.Browser.Dom` 2.20.0 ships `lib` alone.
`Xantham.Fable.Core` ships `lib/netstandard2.1` + `lib/net8.0` + `README.md` at the root, which
matches. Nothing here needs a source-shipping item group.

### The pack defect

`pack-Xantham.Fable.Core` failed with `NETSDK1005`: the assets file had a target for
`netstandard2.1` and none for `net8.0`. The cause is an ordering one. `dotnet fable`, run by the
run gate inside the `test` stage, restores the support package for `netstandard2.1` alone and
rewrites `src/Xantham.Fable.Core/obj/project.assets.json` to that single framework. `pack` then
ran `--no-build --no-restore`, and `--no-build` implies `--no-restore` regardless, so the flag
was not even the lever. Each pack stage now runs `dotnet restore {project.Path} -v q` first.

This never fired before because the one packed project was not one the run gate compiles.

### `bump` reaches all three

Run and reverted. `dotnet fsi build.fsx -- bump` printed:

```
src/Xantham.Cli/Xantham.Cli.fsproj: 0.1.0-alpha.1 -> 0.1.0
src/Xantham.Fable.Core/Xantham.Fable.Core.fsproj: 0.1.0-alpha.1 -> 0.1.0
src/Xantham.TypeScript.Wire/Xantham.TypeScript.Wire.fsproj: 0.2.0 -> 0.2.1
```

No project is skipped. **Two behaviours for the user to know before the next bump**: a `Patch`
bump on a prerelease resolves to the release (`0.1.0-alpha.1 -> 0.1.0`) rather than to
`-alpha.2`, so bumping this alpha needs the version edited by hand; and the helper rewrites the
`<Version>`/`<AssemblyVersion>` pair onto one line with CRLF endings, which is the shape
`Xantham.TypeScript.Wire.fsproj` already carries in the tree.

## Item 2 - `docs/generator-usage.md`

New page, `category: Generator`, `title: Usage`, `index: 0`, following `wire-usage.md`'s shape.
It is registered in `Xantham.slnx` beside the other four `docs/` pages and linked from the
`### Documentation` list in both `README.md` and `docs/index.md`.

It covers: `dotnet tool install --global Xantham.Cli --version 0.1.0-alpha.1`; the
`typescript@7.1.0-dev.20260902.1` install **in the consumer's own project**, called out as the
step that fails as a missing executable rather than a missing `npm install`, with
`XANTHAM_TSGO_EXE` as the CI escape; the package-directory layout and `--save-exact`;
`xantham.json` with `xantham schema -o …` as the way to get the schema rather than a
transcription of it, plus the four dispositions; `xantham generate`, its three options and all
five exit codes; the four output files and the four tiers; and an `.fsproj` for the consuming
side carrying `Fable.Core` 5.2.0, `Xantham.Fable.Core` 0.1.0-alpha.1, `groups/` before the root
module, `NoWarn FS1104`, the `net8.0` floor for the `ParamObject` static interface members, and
a pointer at `Xantham.Generator.CompileGate.fsproj` for the `Fable.Browser.*` pins.

The boundary section states the five gated rungs, `@types/three` and `typescript` as still
ahead with the `@types/three` measurement cited, the corpus tiers `495 / 1552 / 786 / 193`,
`RT001` as bounded resolution depth, and `DO005` as the dropped `keyof`-bound overloads.

Both status tables (`README.md:82-84`, `docs/index.md:106-108`) now read `Alpha` with the
package id and version, replacing "Not yet packaged".

## Item 3 - the ignore rule

`.gitignore` gained `xantham-out/`, placed with `.xantham/` above the `tests/fixtures/` block so
that the `*lab` re-includes below are untouched. Verified both ways:
`git check-ignore -v src/Xantham.Cli/xantham-out/manifest.json` reports the new rule, and
`git check-ignore tests/fixtures/statics-lab/package.json` still exits 1.

The rule is unanchored rather than `src/Xantham.Cli/xantham-out/`: `-o` defaults to
`./xantham-out` relative to wherever the command runs, so the directory can land anywhere.

## Gates

- `dotnet fsi build.fsx -- pack` **exit 0**. `bin/` holds exactly three:
  `Xantham.Cli.0.1.0-alpha.1.nupkg`, `Xantham.Fable.Core.0.1.0-alpha.1.nupkg`,
  `Xantham.TypeScript.Wire.0.2.0.nupkg`.
- `dotnet fsi build.fsx -- test` **exit 0**.
- 467 generator tests, 90 wire tests (1 skipped by design), run gate 257 checks - the
  baseline exactly.
- Nothing published. `bin/*.nupkg` is ignored by `bin/` at `.gitignore:1`.

`git diff --numstat` against `cb6256b`: 8 files, +277 -7, of which `docs/generator-usage.md` is
+237. No generator source was touched, so no golden, tier count or finding count moves.

## Handed back

- **The user's `git clean`.** `src/Xantham.Generator/Measures.fs` and
  `src/Xantham.Cli/xantham-out/` are untracked in the main checkout, invisible from here. The
  ignore rule stops the second recurring; neither file is removed by this lane.
- **`0.1.0-alpha.1` is the manager's assumption.** A different number is one line in each of the
  two `.fsproj` files and four mentions in `docs/generator-usage.md`, `README.md` and
  `docs/index.md`.
- **Pre-existing broken links, not touched.** `README.md` and `docs/index.md` link
  `docs/plans/…` and `plans/…` for `tsgo-protocol.md`, `wire-remaining-work.md`,
  `generator-architecture.md` and `generator-type-mapping.md`. All four live under
  `docs/.ai/plans/`. Outside the brief, so left as found; one lane could repoint eight links.
