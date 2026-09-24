# xantham

Command line driver for Xantham, a TypeScript-to-F# bindings generator. It reads a package's
`.d.ts` declarations through the TypeScript 7 compiler and emits Fable 5 bindings plus a
`manifest.json` that grades every symbol's mapping.

```bash
dotnet tool install --global xantham
```

The tool targets `net10.0`. `xantham --help` prints the whole command line.

## Cache the compiler

```bash
xantham tsc init
```

Installs the TypeScript compiler version the tool was built against into
`~/.cache/xantham/<version>`; `generate` uses the cached compiler when present. The protocol
carries no version handshake, so the compiler has to match the tool.
`xantham tsc version` reports the cached compiler and `xantham tsc clean` removes the cache.

## Generate a binding

```bash
xantham generate node_modules/some-package -o bindings --config xantham.json
```

The argument is a package directory holding a `package.json` and the `node_modules` its
declarations resolve through. `xantham schema --output xantham.schema.json` writes the JSON
Schema for the configuration file.

Documentation: <https://shayanhabibi.github.io/Xantham/xantham-cli/>
