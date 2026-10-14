---
paths: "**/*.fs"
---

# Style

- Follow fantomas 8.0.0-beta-001 defaults
- Exceptions:
  - Generated *.fs files
    - `.fantomasignore` lists the `*.generated.fs` layers, the generated bindings
      (`src/Xantham.Fable.Core.TS/Fable.Core.TS.fs`, `src/Xantham.Fable.Node/Node.fs`) and the
      golden corpus. On CI the `format` stage runs `fantomas --check`, so a generated file that
      is not listed fails the build
    - `build.fsx -- generate` formats the wire layers and the generated bindings it writes
      (`Generated.format`), unless `--no-format` is given. Commit them formatted; a generator
      that writes another committed `.fs` file adds it there
    - The golden corpus stays unformatted: it is the generator's raw output
  - Expecto test files