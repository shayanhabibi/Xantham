# Combined Compiler-Library Binding Design

## Goal

Generate the TypeScript compiler's complete ECMAScript and DOM library declarations as a first-class Fable binding artifact in one F# source file so cyclic ES-to-DOM and DOM-to-ES references compile, while preserving distinct, configurable child modules for each family.

## Scope

This design applies only when the compiler-library group (`"typescript/lib"`) is shipped. It does not alter ordinary entry-package or dependency group layout, their existing `module`/`namespace` configuration, or reference and map dispositions.

## Configuration

Add an optional `compilerLib` object to `xantham.json`:

```jsonc
{
  "groups": { "typescript/lib": "ship" },
  "compilerLib": {
    "module": "MyCompany.TypeScript.Lib",
    "esModule": "Core",
    "domModule": "Browser",
    "autoOpenEs": true,
    "autoOpenDom": false
  }
}
```

Every member is optional. Its effective defaults preserve the current output:

```jsonc
{
  "module": "TypeScript.Lib",
  "esModule": "Es",
  "domModule": "Dom",
  "autoOpenEs": false,
  "autoOpenDom": false
}
```

`module` is the single shared root and may use the repository's existing dotted F# module spelling. `esModule` and `domModule` are single F# module identifiers, not dotted paths. Each must be nonempty, valid in an F# module declaration, and distinct from the other. A malformed value is a configuration refusal (CLI exit code 3), before compiler work begins.

## Generated layout and references

The shipped compiler library renders to exactly one `groups/<root-module>.fs` source file:

```fsharp
module rec MyCompany.TypeScript.Lib

[<AutoOpen>]
module Core =
    // ES-family declarations

module Browser =
    // DOM-family declarations
```

`module rec` covers the root and both nested modules, enabling cross-family cycles in a single F# compilation unit. `autoOpenEs` and `autoOpenDom` independently control whether the corresponding child module receives `[<AutoOpen>]`.

The generator itself always renders ownership-crossing references in canonical fully qualified form:

`<root-module>.<family-module>.<declaration-name>`.

Auto-open attributes are consumer ergonomics only; emitted binding correctness must not depend on them. Family routing remains unchanged: declarations whose source belongs to `Grouping.libFamily` `"Es"` go to the ES child; all `"Dom"` families (DOM, worker, and script-host) go to the DOM child.

## First-class artifact

Ship the initial generated binding as a new `src/Xantham.Fable.Core.TS` project. Its generated source is `Fable.Core.TS.fs`, and its public module name is fixed by its checked-in generation configuration:

```jsonc
{
  "lib": ["esnext", "dom"],
  "groups": { "typescript/lib": "ship" },
  "compilerLib": {
    "module": "Fable.Core.TS",
    "esModule": "Es",
    "domModule": "Dom",
    "autoOpenEs": true,
    "autoOpenDom": false
  }
}
```

Thus consumers write `open Fable.Core.TS` and receive the ECMAScript surface without an additional `open Es`; DOM declarations remain explicitly scoped under `Fable.Core.TS.Dom`. The project references Fable.Core 5.2.0 and the existing `Xantham.Fable.Core` support project, matching the generated binding contract. The build pipeline owns regeneration from the pinned root TypeScript 7.x package and fails when regeneration produces an uncommitted diff.

## Architecture

Introduce one focused configuration module/value, `CompilerLibLayout`, that owns defaults, JSON parsing, validation, source-file name derivation, the two fully qualified module names, and the auto-open choices. `Pipeline.groupModulesForScope` consults this module for compiler-library placement instead of the fixed `Naming.CompilerLib*` constants.

Keep the existing string-based `GroupModule.Module` representation. The constrained one-segment child names mean the existing namespaced renderer does not need a general module-path tree. General package groups continue to use their current rendering path.

Adapt the compiler-library rendering branch to write `module rec <root>` and child `module <Es>` / `module <Dom>` declarations, rather than `namespace rec <root>`. The ordinary namespace renderer remains responsible for dependency namespaces. The compiler-library branch owns its own one-file grouping, header, child-module attributes, deterministic ordering, footer aggregation, and foreign-reference map.

## Error handling

Configuration parsing rejects a non-object `compilerLib`, non-string module names, non-boolean auto-open flags, empty values, invalid child identifiers, and equal ES/DOM child names. Omitted fields are never errors. Generation rejects no additional TypeScript inputs beyond current behavior.

## Verification

- Unit-test defaulting, partial overrides, valid dotted roots, and every configuration-refusal case.
- Test layout planning and reference qualification using a non-default root plus custom ES and DOM child names.
- Test each auto-open flag independently, proving only the selected child has the attribute.
- Update `lib-ship-lab` golden assertions to verify a single compiler-library file headed by `module rec`, the two configured child modules, and bidirectional fully qualified cross-family references.
- Add `Xantham.Fable.Core.TS`, its checked-in generation configuration, and a build stage that regenerates the pinned `esnext` + `dom` closure into its one committed source file. Compile the project as the artifact's gate, and include it in the solution build.
- Run generator tests with `XANTHAM_REQUIRE_TSC=1`, build `Xantham.slnx`, and inspect the generated-file diff and manifest counts for determinism.

## Non-goals

- General nested module-path configuration for ES or DOM children.
- Changing ordinary package/dependency namespace behavior.
- Relying on `[<AutoOpen>]` to resolve generated references.
- Supporting Fable versions other than Fable 5.x.
