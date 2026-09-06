---
category: Generator
title: Usage
index: 0
---

# Generating F# bindings with `xantham`

How to take an npm package of TypeScript declarations to an F# binding a Fable project compiles:
install the tool, pin a compiler in the package you generate from, write `xantham.json`, run
`xantham generate`, reference the output. For the compiler client the generator is built on, see
[`wire-usage.md`](wire-usage.md).

The generator is **alpha**. [What the alpha does not do](#what-the-alpha-does-not-do) states the
boundary, so it arrives here rather than in your build.

## Install the tool

```bash
dotnet tool install --global Xantham.Cli --version 0.1.0-alpha.1
```

The package installs the command `xantham` and targets `net10.0`. `xantham --version` prints the
installed version and `xantham --help` the whole command line.

## Pin the compiler in your own project

```bash
cd my-bindings
npm install --save-dev typescript@7.1.0-dev.20260902.1
```

**This step is yours, and skipping it fails as something else.** Generation drives the Go `tsc`
binary shipped inside the `typescript` npm package as `tsc --api`, and locates it by walking up
from the package directory for `node_modules/@typescript/typescript-<rid>/lib/tsc`. Where no
install sits anywhere above the package, the walk reaches the filesystem root and the run reports
a missing executable rather than a missing `npm install`.

`XANTHAM_TSGO_EXE`, set to an existing file, wins over the search. That is how a CI job points at
a compiler installed outside the tree.

The protocol carries no version handshake, so the npm pin has to match the version the tool was
built against. This alpha carries `7.1.0-dev.20260902.1`.

## Lay out the package directory

`xantham generate` reads a directory holding a `package.json` and the `node_modules` its
declarations resolve through — an ordinary npm project with the package you are binding installed:

```bash
mkdir my-bindings && cd my-bindings
npm init -y
npm install --save-exact animejs@4.5.0
npm install --save-dev typescript@7.1.0-dev.20260902.1
```

Point the generator at the installed package rather than at the project root:

```bash
xantham generate ./node_modules/animejs
```

Pin with `--save-exact`. A binding is generated against exactly the declarations installed, so a
floating range turns the next regeneration's diff into a question about which side moved.

## Write `xantham.json`

`xantham.json` sits beside the package's own `package.json`, or anywhere `--config` names. It is
JSONC: comments and trailing commas are accepted. Every key is optional, and a package with no
`xantham.json` generates under the defaults.

```jsonc
{
  "$schema": "./xantham.schema.json",
  // The F# namespace the family is written under. The entry package takes it; a shipped group
  // takes <namespace>.<Leaf>.
  "namespace": "FSharp.Anime",
  // The compiler's `lib`, as tsconfig.json spells it. A global type library that redeclares DOM
  // names sets this to what its own README prescribes.
  "lib": ["esnext"],
  // What happens at each package boundary the declarations reach. An unlisted group widens.
  "groups": {
    "typescript/lib": {
      "map": {
        "RegExp": "System.Text.RegularExpressions.Regex",
        "WeakRef": { "name": "System.WeakReference", "arity": 1 }
      }
    }
  }
}
```

Write the schema out and let the editor do the rest:

```bash
xantham schema -o xantham.schema.json
```

The schema is emitted from the generator's own configuration record, so it describes the keys the
installed tool reads. A copy generated from `master` is committed at the repository root as
[`xantham.schema.json`](https://github.com/shayanhabibi/Xantham/blob/master/xantham.schema.json).

### Select a declaration entry

Each invocation generates from one TypeScript input. By default, the generator selects the
manifest's `types`, then `typings`, then the first `types` string under the root export's
conditions, then `index.d.ts`. In an `exports` map, the root is `"."`; named subpaths such as
`"./adapter"` are separate inputs. A map without a root, an empty map, or an explicitly blocked
`".": null` requires an explicit `entry`, even when `types`, `typings` or `index.d.ts` exists.
An absent or top-level `null` `exports` field uses the default lookup.

Use `entry` to select a particular declaration file, including a condition-specific `.d.mts`
or `.d.cts` file. Choose the environment or import/require variant explicitly; the default
root-condition scan is a declaration lookup. The path is relative to the package directory passed
to `generate`, including when `--config` points elsewhere. It must be nonempty, remain within that
directory, and name an existing TypeScript file (`.ts`, `.tsx`, `.mts` or `.cts`, including declarations).

```jsonc
{
  "entry": "dist/adapter.d.ts",
  "runtime": "example-package/adapter",
  "module": "Example.Adapter"
}
```

`runtime` controls the public JavaScript import used in generated `Import` attributes; `module`
controls the F# module name. A supplied `runtime` must be a nonempty string; omitting it keeps
the derived package import. Set these names for the selected entry. The declaration path is a file
inside the installed package; the runtime import is the package's public module specifier.
For a package with no public root, supply `runtime` when generating value imports: an explicit
declaration input still leaves the default runtime import at the package root.
Generate other public entries with separate configurations and output directories. Conditional
npm resolution and automatic generation of every subpath are outside this selection mechanism.

### The four group dispositions

`groups` is keyed by npm name, with the compiler's own library as `typescript/lib`.

| Disposition | What a reference to the group's types renders as |
|---|---|
| `"ship"` | The group's declarations are emitted, under `groups/` in the output. |
| `"reference"` | The module name that group templates to. A separate `ship` run of that group — yours or anyone's — supplies it. |
| `{ "map": { … } }` | The destination each named type is redirected to. A name outside the table widens. |
| `"widen"` | `obj`, with a finding. The default for every group left unlisted. |

A `map` destination spelled as a bare string takes no type arguments; the object form states its
`arity`, and a reference applying any other number widens with finding `TR053`.

## Run it

```bash
xantham generate ./node_modules/animejs -o ./out
```

| Option | |
|---|---|
| `-o`, `--out <dir>` | Where the output is written. Default `./xantham-out`. |
| `--config <path>` | The `xantham.json` to read, or a directory holding one. Default: the package directory. |
| `--quiet` | Write the file list alone, dropping the findings summary. |

Standard output carries one absolute path per file written and the findings summary goes to
standard error, so `xantham generate … > files.txt` keeps the two apart.

| Exit | |
|---|---|
| 0 | The binding and its manifest were written. |
| 1 | The command line is not one `xantham` accepts. |
| 2 | The path holds no package the generator can read. |
| 3 | `xantham.json` was refused. |
| 4 | Generation itself failed. |

## What a run writes

| File | |
|---|---|
| `<Module>.fs` | The entry package's binding. |
| `groups/<Module>.fs` | One file per further shipped group. |
| `manifest.json` | The aggregate: the package, the tier counts, the per-pass tallies. A page long for any package. |
| `symbols.jsonl` | One line per symbol, carrying that symbol's findings. Thousands of lines for a large package; grep it. |

Every symbol is graded into one of four tiers, and a symbol's tier is the worst tier among its
findings:

| Tier | |
|---|---|
| `exact` | The F# type accepts and rejects exactly what TypeScript does. |
| `ergonomic` | Meaning preserved, spelling made idiomatic — `null \| undefined` hoisted to `option`. |
| `widened` | Information TypeScript had was dropped — a union collapsed to `obj`. |
| `escape` | The construct is not represented; the consumer is on their own. |

`manifest.json` is the file to read. Every widened and every escaped site is named there and in
`symbols.jsonl` with the finding code accounting for it, so a binding's losses are enumerable
before you build on it.

## Compile the output

Add the generated files to a Fable project. `groups/` compiles first: a group's module is written
before the module naming its types.

```xml
<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <TargetFramework>net8.0</TargetFramework>
        <!-- FS1104: a JavaScript property name is not an F# identifier, and a backticked
             member is the only way to write one. -->
        <NoWarn>$(NoWarn);FS1104</NoWarn>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="out/groups/*.fs" />
        <Compile Include="out/*.fs" />
    </ItemGroup>

    <ItemGroup>
        <PackageReference Include="Fable.Core" Version="5.2.0" />
        <PackageReference Include="Xantham.Fable.Core" Version="0.1.0-alpha.1" />
    </ItemGroup>

</Project>
```

- **`Fable.Core` 5.2.0.** Generated bindings target **Fable 5.x only**.
- **`Xantham.Fable.Core`.** Every generated file opens it. It carries erased F# forms for the
  TypeScript idioms F# has no spelling for — `keyof`, indexed access, `T[keyof T]`,
  index-signature objects and nominal brands — and each one erases at runtime. Its
  [README](https://github.com/shayanhabibi/Xantham/blob/master/src/Xantham.Fable.Core/README.md)
  documents them.
- **`net8.0` or later.** The generated `ParamObject` `Create` members are static interface members
  with bodies, which need default-interface-member runtime support to type-check. Fable erases
  them; the target framework only has to let the F# compiler accept them.
- **The `Fable.Browser.*` family**, where the package's declarations reach DOM names. A generated
  reference to `HTMLElement` resolves against `Fable.Browser.Dom`, and the rest of the family
  covers the other DOM areas.
  [`Xantham.Generator.CompileGate.fsproj`](https://github.com/shayanhabibi/Xantham/blob/master/tests/Xantham.Generator.CompileGate/Xantham.Generator.CompileGate.fsproj)
  carries the whole set at the pins the generator's own binding table was reflected over; take the
  entries your binding needs.

Regenerate rather than editing. Every generated file opens with an `<auto-generated>` header, and
generation is deterministic: the same package at the same version under the same `xantham.json`
produces the same bytes.

## What the alpha does not do

The generator is gated on five pinned npm rungs — `ansi-regex`, `animejs`, `type-fest`, `solid-js`
and `@cloudflare/workers-types` — beside a corpus of hand-authored fixtures. Every committed
golden is compiled as F# on each build, so what ships compiles. Beyond that:

- **The ladder's top two rungs are outstanding.** `@types/three` and `typescript` are still ahead.
  `@types/three` was measured and refused: the run produces 128 MB of output, not
  byte-deterministic, which does not compile, and 76.5% of it is an unbounded instantiation walk
  rather than the package's real surface. The measurement is in
  [`generator-three-rung.md`](https://github.com/shayanhabibi/Xantham/blob/master/docs/.ai/plans/generator-three-rung.md).
- **Roughly a third of graded symbols widen or escape.** Over the whole 50-fixture corpus the
  tiers read `exact 495, ergonomic 1552, widened 786, escape 193`. A large package widens more
  than a small one.
- **Resolution follows a bounded depth.** A type past that bound widens to `obj` under finding
  `RT001`.
- **Overloads separated only by a `keyof`-constrained type parameter are dropped** (`DO005`):
  every overload in such a set maps to one F# parameter type.
- **A binding is a starting point rather than a contract.** The package identities and the shape
  of the output can move before `1.0`.

Read `manifest.json` for the package in front of you rather than generalising from these numbers.

## Where to go next

- [Using Xantham.TypeScript.Wire](wire-usage.md) — the compiler client the generator drives.
- [Navigating the AST](wire-navigation.md) — sessions, `Node<'Tag>`, views, accessors.
