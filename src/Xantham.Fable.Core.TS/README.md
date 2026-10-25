# Xantham.Fable.Core.TS

Generated Fable 5 bindings for TypeScript's `esnext` and DOM standard-library declarations.

## Reuse the packaged declaration catalogue

The NuGet package contains `xantham/declarations.json.br` and `xantham/catalogue.json`.
Restore `Xantham.Fable.Core.TS` in the consumer project, then pass the restored catalogue's
absolute path in the input package's generator configuration:

```json
{
  "module": "MyBindings",
  "lib": ["esnext", "dom"],
  "declarationReferences": ["/path/to/packages/xantham.fable.core.ts/<version>/xantham/declarations.json.br"]
}
```

Replace `<version>` and the package-cache path with the version actually restored. The
consumer must compile against that same package version. MSBuild's
`GeneratePathProperty="true"` on the PackageReference exposes its restored directory as
`$(PkgXantham_Fable_Core_TS)` for build integrations.

The descriptor maps the authenticated catalogue owner `FableCoreTsInput` to the package ID
and version. It records compatibility contracts, ordered owners and SHA-256 hashes of the
compressed catalogue, compiled assembly, binding sources and generation configuration.
Source and configuration hashes normalize CRLF to LF so they agree across checkouts.
Xantham reads the `.br` file directly; the descriptor is package metadata for integrations.
The catalogue retains qualified names and arities, including
`Fable.Core.TS.Dom.HTMLInputElement`, `Fable.Core.TS.Dom.FileList` and
`Fable.Core.TS.Es.Map<'K, 'V>`. Reuse requires the exact TypeScript release/source revision
and compatible generation settings recorded in the catalogue.

Namespace members now retain their TypeScript namespace: for example,
`Fable.Core.TS.Es.NumberFormatOptions` moves to `Fable.Core.TS.Es.Intl.NumberFormatOptions`.
Regenerate dependent bindings and update uses of the former flat names. Catalogue API
contract 9 rejects earlier catalogues; consumers and the producer must use regenerated
catalogues together.

## Regenerate

The source file is generated from `tools/fable-core-ts-input`; regenerate it with:

```text
dotnet fsi build.fsx -- generate --only compiler-lib
```

Regeneration updates `catalogue-inputs.json` alongside the producer. Packing checks that
the catalogue, sources and configuration still match this generation record. A build records
the compiled assembly and its binding inputs; `pack --no-build` rejects a stale assembly.
